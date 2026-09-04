// SPDX-License-Identifier: GPL-2.0-only
#include <linux/debugfs.h>
#include <linux/delay.h>
#include <linux/device.h>
#include <linux/fs.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/rmi.h>
#include <linux/slab.h>
#include <linux/uaccess.h>
#include <linux/vmalloc.h>

#define RMI_FN_MAX_IRQS 6
#define F54_GET_REPORT BIT(0)
#define F54_NORMALIZED_16BIT_IMAGE 2
#define F54_REPORT_CHUNK 32
#define F54_REPORT_TIMEOUT_MS 1000
#define F54_CAP_IMAGE16 BIT(6)
#define F54_QUERY_BYTES 6

struct rmi_function {
    struct rmi_function_descriptor fd;
    struct rmi_device *rmi_dev;
    struct device dev;
    struct list_head node;
    unsigned int num_of_irqs;
    int irq[RMI_FN_MAX_IRQS];
    unsigned int irq_pos;
    unsigned long irq_mask[];
};

struct rmi_function_handler {
    struct device_driver driver;
    u8 func;
    int (*probe)(struct rmi_function *fn);
    void (*remove)(struct rmi_function *fn);
    int (*config)(struct rmi_function *fn);
    int (*reset)(struct rmi_function *fn);
    irqreturn_t (*attention)(int irq, void *ctx);
    int (*suspend)(struct rmi_function *fn);
    int (*resume)(struct rmi_function *fn);
};

extern int __rmi_register_function_handler(struct rmi_function_handler *,
                                            struct module *, const char *);
extern void rmi_unregister_function_handler(struct rmi_function_handler *);

struct health_data {
    struct rmi_function *fn;
    struct dentry *file;
    struct mutex lock;
};

static int rmi_read_block(struct health_data *data, u16 address, void *buffer,
                          size_t length)
{
    struct rmi_transport_dev *xport = data->fn->rmi_dev->xport;

    return xport->ops->read_block(xport, address, buffer, length);
}

static int rmi_write_block(struct health_data *data, u16 address,
                           const void *buffer, size_t length)
{
    struct rmi_transport_dev *xport = data->fn->rmi_dev->xport;

    return xport->ops->write_block(xport, address, buffer, length);
}

static int capture_normalized_image(struct health_data *data, u8 **image,
                                    size_t *image_size)
{
    struct rmi_function_descriptor *fd = &data->fn->fd;
    u8 query[F54_QUERY_BYTES];
    u8 command;
    u8 previous_report_type;
    u8 report_type = F54_NORMALIZED_16BIT_IMAGE;
    unsigned long deadline;
    size_t offset;
    size_t size;
    u8 *buffer;
    int error;

    error = rmi_read_block(data, fd->query_base_addr, query, sizeof(query));
    if (error < 0)
        return error;

    size = (size_t)query[0] * query[1] * sizeof(u16);
    if (!(query[2] & F54_CAP_IMAGE16))
        return -EOPNOTSUPP;
    if (!size || size > 128 * 128 * sizeof(u16))
        return -EINVAL;

    error = rmi_read_block(data, fd->data_base_addr,
                           &previous_report_type, 1);
    if (error < 0)
        return error;

    error = rmi_read_block(data, fd->command_base_addr, &command, 1);
    if (error < 0)
        return error;
    if (command)
        return -EBUSY;

    error = rmi_write_block(data, fd->data_base_addr, &report_type, 1);
    if (error < 0)
        return error;

    command = F54_GET_REPORT;
    error = rmi_write_block(data, fd->command_base_addr, &command, 1);
    if (error < 0)
        goto restore_report_type;

    deadline = jiffies + msecs_to_jiffies(F54_REPORT_TIMEOUT_MS);
    do {
        msleep(10);
        error = rmi_read_block(data, fd->command_base_addr, &command, 1);
        if (error < 0)
            goto restore_report_type;
        if (!(command & F54_GET_REPORT))
            break;
    } while (time_before(jiffies, deadline));

    if (command & F54_GET_REPORT) {
        error = -ETIMEDOUT;
        goto restore_report_type;
    }

    buffer = vmalloc(size);
    if (!buffer) {
        error = -ENOMEM;
        goto restore_report_type;
    }

    for (offset = 0; offset < size; offset += F54_REPORT_CHUNK) {
        u8 fifo[] = { offset, offset >> 8 };
        size_t count = min_t(size_t, F54_REPORT_CHUNK, size - offset);

        error = rmi_write_block(data, fd->data_base_addr + 1,
                                fifo, sizeof(fifo));
        if (error < 0)
            goto free_buffer;

        error = rmi_read_block(data, fd->data_base_addr + 3,
                               buffer + offset, count);
        if (error < 0)
            goto free_buffer;
    }

    *image = buffer;
    *image_size = size;
    error = 0;
    goto restore_report_type;

free_buffer:
    vfree(buffer);
restore_report_type:
    if (rmi_write_block(data, fd->data_base_addr,
                        &previous_report_type, 1) < 0 && !error)
        error = -EIO;
    return error;
}

static ssize_t normalized_image_read(struct file *file, char __user *buffer,
                                     size_t count, loff_t *position)
{
    struct health_data *data = file->private_data;
    u8 *image;
    size_t image_size;
    ssize_t result;

    if (*position)
        return 0;

    mutex_lock(&data->lock);
    result = capture_normalized_image(data, &image, &image_size);
    if (!result) {
        result = simple_read_from_buffer(buffer, count, position,
                                         image, image_size);
        vfree(image);
    }
    mutex_unlock(&data->lock);

    return result;
}

static int normalized_image_open(struct inode *inode, struct file *file)
{
    file->private_data = inode->i_private;
    return 0;
}

static const struct file_operations normalized_image_fops = {
    .owner = THIS_MODULE,
    .open = normalized_image_open,
    .read = normalized_image_read,
    .llseek = noop_llseek,
};

static int health_probe(struct rmi_function *fn)
{
    struct health_data *data;

    data = devm_kzalloc(&fn->dev, sizeof(*data), GFP_KERNEL);
    if (!data)
        return -ENOMEM;

    data->fn = fn;
    mutex_init(&data->lock);
    data->file = debugfs_create_file("rmi4_touchpad_normalized_image", 0400,
                                     NULL, data, &normalized_image_fops);
    if (IS_ERR_OR_NULL(data->file))
        return data->file ? PTR_ERR(data->file) : -ENOMEM;

    dev_set_drvdata(&fn->dev, data);
    return 0;
}

static void health_remove(struct rmi_function *fn)
{
    struct health_data *data = dev_get_drvdata(&fn->dev);

    debugfs_remove(data->file);
}

static struct rmi_function_handler health_handler = {
    .driver.name = "rmi4_touchpad_health",
    .func = 0x54,
    .probe = health_probe,
    .remove = health_remove,
};

static int __init health_init(void)
{
    return __rmi_register_function_handler(&health_handler, THIS_MODULE,
                                            KBUILD_MODNAME);
}

static void __exit health_exit(void)
{
    rmi_unregister_function_handler(&health_handler);
}

module_init(health_init);
module_exit(health_exit);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("Synaptics RMI4 F54 touchpad health sampler");
