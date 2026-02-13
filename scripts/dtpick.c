/*
 * dtpick — 快速日期时间选择器 (X11/Xft)
 *
 * 键位:
 *   w e r       跳转到 年/月/日
 *   s d f       跳转到 时/分/秒
 *   h / l       左/右移动槽位
 *   j / k       当前字段 +1 / -1 (循环)
 *   0-9         数字输入 (智能歧义等待)
 *   Enter       确认输出 ISO 时间并保存
 *   Esc/Ctrl+]  取消，无输出
 *   BackSpace   清除当前输入缓冲
 *
 * 持久化: ~/.cache/dtpick_last (第一行 ISO 时间, 第二行 last_edited slot)
 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xft/Xft.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pwd.h>

#define NUM_SLOTS 6
#define FONT_NAME "monospace:size=18"
#define BG_COLOR  "#1d1f21"
#define FG_COLOR  "#c5c8c6"
#define HL_BG     "#FFB300"
#define HL_FG     "#1d1f21"
#define HINT_COLOR "#888888"

typedef struct {
    char key;           /* jump key */
    int  width;         /* digit width (4 or 2) */
    char value[8];      /* confirmed value string */
    char buffer[8];     /* input buffer */
    int  buflen;
} Slot;

static Slot slots[NUM_SLOTS];
static int active = 0;
static int last_edited = 0;

static const char slot_keys[NUM_SLOTS] = {'w','e','r','s','d','f'};
static const int  slot_widths[NUM_SLOTS] = {4,2,2,2,2,2};
static const char *separators[NUM_SLOTS] = {"","-","-"," ",":",":" };

static char cache_path[512];

static void init_cache_path(void) {
    const char *home = getenv("HOME");
    if (!home) home = getpwuid(getuid())->pw_dir;
    snprintf(cache_path, sizeof(cache_path), "%s/.cache/dtpick_last", home);
}

static int days_in_month(int y, int m) {
    if (m == 2) return (y%4==0 && (y%100!=0 || y%400==0)) ? 29 : 28;
    if (m==4||m==6||m==9||m==11) return 30;
    return 31;
}

static void slot_range(int idx, int *lo, int *hi) {
    switch (idx) {
        case 0: *lo=1; *hi=9999; break;
        case 1: *lo=1; *hi=12; break;
        case 2: *lo=1; *hi=days_in_month(atoi(slots[0].value), atoi(slots[1].value)); break;
        case 3: *lo=0; *hi=23; break;
        default: *lo=0; *hi=59; break;
    }
}

static int validate(int idx, const char *buf) {
    int n = atoi(buf), lo, hi;
    slot_range(idx, &lo, &hi);
    return n >= lo && n <= hi;
}

static int can_extend(char digit, int idx) {
    int lo, hi, tens;
    slot_range(idx, &lo, &hi);
    tens = (digit - '0') * 10;
    return tens <= hi && tens + 9 >= lo;
}

static void commit_buffer(void) {
    Slot *s = &slots[active];
    if (s->buflen > 0) {
        char buf[8];
        int pad = s->width - s->buflen;
        memset(buf, '0', pad);
        memcpy(buf + pad, s->buffer, s->buflen);
        buf[s->width] = 0;
        if (validate(active, buf)) {
            memcpy(s->value, buf, s->width + 1);
            last_edited = active;
        }
        s->buflen = 0;
    }
}

static void input_digit(char ch) {
    Slot *s = &slots[active];
    s->buffer[s->buflen++] = ch;
    s->buffer[s->buflen] = 0;

    if (s->buflen == 1 && s->width == 2) {
        if (can_extend(ch, active)) return; /* wait for 2nd digit */
        char buf[4] = {'0', ch, 0};
        if (validate(active, buf)) {
            memcpy(s->value, buf, 3);
            last_edited = active;
            s->buflen = 0;
            if (active < NUM_SLOTS - 1) active++;
        } else {
            s->buflen = 0;
        }
    } else if (s->buflen >= s->width) {
        char buf[8];
        memcpy(buf, s->buffer, s->width);
        buf[s->width] = 0;
        if (validate(active, buf)) {
            memcpy(s->value, buf, s->width + 1);
            last_edited = active;
            s->buflen = 0;
            if (active < NUM_SLOTS - 1) active++;
        } else {
            s->buflen = 0;
        }
    }
}

static void adjust(int delta) {
    Slot *s = &slots[active];
    s->buflen = 0;
    int lo, hi;
    slot_range(active, &lo, &hi);
    int n = atoi(s->value) + delta;
    if (n > hi) n = lo;
    else if (n < lo) n = hi;
    snprintf(s->value, sizeof(s->value), "%0*d", s->width, n);
    last_edited = active;
}

static void move_slot(int delta) {
    commit_buffer();
    active += delta;
    if (active < 0) active = 0;
    if (active >= NUM_SLOTS) active = NUM_SLOTS - 1;
}

static void jump_slot(int idx) {
    commit_buffer();
    active = idx;
    slots[active].buflen = 0;
}

static void to_iso(char *out, size_t sz) {
    snprintf(out, sz, "%s-%s-%s %s:%s:%s",
             slots[0].value, slots[1].value, slots[2].value,
             slots[3].value, slots[4].value, slots[5].value);
}

static void load_last(void) {
    FILE *f = fopen(cache_path, "r");
    if (f) {
        char line[64];
        if (fgets(line, sizeof(line), f)) {
            /* parse "2026-02-12 08:54:44" */
            int y,M,d,h,m,s;
            if (sscanf(line, "%d-%d-%d %d:%d:%d", &y,&M,&d,&h,&m,&s) == 6) {
                snprintf(slots[0].value, 8, "%04d", y);
                snprintf(slots[1].value, 8, "%02d", M);
                snprintf(slots[2].value, 8, "%02d", d);
                snprintf(slots[3].value, 8, "%02d", h);
                snprintf(slots[4].value, 8, "%02d", m);
                snprintf(slots[5].value, 8, "%02d", s);
            }
        }
        if (fgets(line, sizeof(line), f)) {
            int a = atoi(line);
            if (a >= 0 && a < NUM_SLOTS) {
                active = a;
                last_edited = a;
            }
        }
        fclose(f);
        return;
    }
    /* fallback: current time */
    time_t t = time(NULL);
    struct tm *tm = localtime(&t);
    snprintf(slots[0].value, 8, "%04d", tm->tm_year + 1900);
    snprintf(slots[1].value, 8, "%02d", tm->tm_mon + 1);
    snprintf(slots[2].value, 8, "%02d", tm->tm_mday);
    snprintf(slots[3].value, 8, "%02d", tm->tm_hour);
    snprintf(slots[4].value, 8, "%02d", tm->tm_min);
    snprintf(slots[5].value, 8, "%02d", tm->tm_sec);
}

static void save_last(void) {
    /* ensure ~/.cache exists */
    char dir[512];
    snprintf(dir, sizeof(dir), "%s/.cache", getenv("HOME") ? getenv("HOME") : ".");
    mkdir(dir, 0755);

    FILE *f = fopen(cache_path, "w");
    if (f) {
        char iso[32];
        to_iso(iso, sizeof(iso));
        fprintf(f, "%s\n%d\n", iso, last_edited);
        fclose(f);
    }
}

static void init_slots(void) {
    for (int i = 0; i < NUM_SLOTS; i++) {
        slots[i].key = slot_keys[i];
        slots[i].width = slot_widths[i];
        slots[i].buflen = 0;
    }
    load_last();
}

/* ---- X11 rendering ---- */

static XftColor xft_alloc(Display *dpy, Colormap cmap, Visual *vis, const char *name) {
    XftColor c;
    XftColorAllocName(dpy, vis, cmap, name, &c);
    return c;
}

int main(void) {
    init_cache_path();
    init_slots();

    Display *dpy = XOpenDisplay(NULL);
    if (!dpy) { fprintf(stderr, "Cannot open display\n"); return 1; }

    int scr = DefaultScreen(dpy);
    Visual *vis = DefaultVisual(dpy, scr);
    Colormap cmap = DefaultColormap(dpy, scr);

    XftFont *font = XftFontOpenName(dpy, scr, FONT_NAME);
    if (!font) { fprintf(stderr, "Cannot open font\n"); return 1; }

    int fh = font->ascent + font->descent;
    int pad = 12;
    /* "2026-02-12 08:54:44" = 19 chars + hint line */
    int text_w = 19 * (font->max_advance_width > 0 ? font->max_advance_width : fh * 2 / 3);
    int win_w = text_w + pad * 2;
    int win_h = fh * 2 + pad * 3;

    /* center on screen */
    int sw = DisplayWidth(dpy, scr);
    int sh = DisplayHeight(dpy, scr);
    int wx = (sw - win_w) / 2;
    int wy = (sh - win_h) / 2;

    XSetWindowAttributes swa;
    swa.override_redirect = True;
    swa.event_mask = ExposureMask | KeyPressMask;
    Window win = XCreateWindow(dpy, RootWindow(dpy, scr), wx, wy, win_w, win_h, 0,
                               CopyFromParent, InputOutput, vis,
                               CWOverrideRedirect | CWEventMask, &swa);

    /* set window title */
    XStoreName(dpy, win, "dtpick");

    /* grab keyboard with retry (another grab may briefly hold it) */
    XMapRaised(dpy, win);
    XSetInputFocus(dpy, win, RevertToParent, CurrentTime);
    for (int i = 0; i < 50; i++) {
        if (XGrabKeyboard(dpy, win, True, GrabModeAsync, GrabModeAsync, CurrentTime) == GrabSuccess)
            break;
        usleep(10000); /* 10ms */
    }

    XftDraw *xd = XftDrawCreate(dpy, win, vis, cmap);
    XftColor col_bg = xft_alloc(dpy, cmap, vis, BG_COLOR);
    XftColor col_fg = xft_alloc(dpy, cmap, vis, FG_COLOR);
    XftColor col_hl_bg = xft_alloc(dpy, cmap, vis, HL_BG);
    XftColor col_hl_fg = xft_alloc(dpy, cmap, vis, HL_FG);
    XftColor col_hint = xft_alloc(dpy, cmap, vis, HINT_COLOR);

    int confirmed = 0;
    int running = 1;

    while (running) {
        /* draw */
        XClearWindow(dpy, win);
        XftDrawRect(xd, &col_bg, 0, 0, win_w, win_h);

        /* compute total content width for centering */
        char full[32];
        to_iso(full, sizeof(full));
        XGlyphInfo ext;
        XftTextExtentsUtf8(dpy, font, (FcChar8*)full, strlen(full), &ext);
        int content_w = ext.xOff;
        int x_start = (win_w - content_w) / 2;

        int x = x_start;
        int y_val = pad + font->ascent;
        int y_key = pad + fh + pad / 2 + font->ascent;

        for (int i = 0; i < NUM_SLOTS; i++) {
            /* separator */
            int sep_len = strlen(separators[i]);
            if (sep_len) {
                XftTextExtentsUtf8(dpy, font, (FcChar8*)separators[i], sep_len, &ext);
                XftDrawStringUtf8(xd, &col_fg, font, x, y_val, (FcChar8*)separators[i], sep_len);
                x += ext.xOff;
            }

            /* slot text */
            char text[8];
            if (i == active && slots[i].buflen > 0) {
                memcpy(text, slots[i].buffer, slots[i].buflen);
                for (int j = slots[i].buflen; j < slots[i].width; j++) text[j] = '_';
            } else {
                memcpy(text, slots[i].value, slots[i].width);
            }
            text[slots[i].width] = 0;

            XftTextExtentsUtf8(dpy, font, (FcChar8*)text, slots[i].width, &ext);
            int tw = ext.xOff;

            if (i == active) {
                XftDrawRect(xd, &col_hl_bg, x - 2, pad, tw + 4, fh);
                XftDrawStringUtf8(xd, &col_hl_fg, font, x, y_val, (FcChar8*)text, slots[i].width);
            } else {
                XftDrawStringUtf8(xd, &col_fg, font, x, y_val, (FcChar8*)text, slots[i].width);
            }

            /* hint key below, centered */
            char key[2] = {slots[i].key, 0};
            XGlyphInfo kext;
            XftTextExtentsUtf8(dpy, font, (FcChar8*)key, 1, &kext);
            int kx = x + (tw - kext.xOff) / 2;
            XftColor *kcol = (i == active) ? &col_hl_bg : &col_hint;
            XftDrawStringUtf8(xd, kcol, font, kx, y_key, (FcChar8*)key, 1);

            x += tw;
        }

        XFlush(dpy);

        /* event loop */
        XEvent ev;
        XNextEvent(dpy, &ev);
        if (ev.type == Expose) continue;
        if (ev.type != KeyPress) continue;

        KeySym ksym;
        char buf[8];
        int len = XLookupString(&ev.xkey, buf, sizeof(buf), &ksym, NULL);

        if (ksym == XK_Escape || (ev.xkey.state & ControlMask && ksym == XK_bracketright)) {
            running = 0;
            continue;
        }
        if (ksym == XK_Return) {
            commit_buffer();
            confirmed = 1;
            running = 0;
            continue;
        }
        if (ksym == XK_BackSpace) {
            slots[active].buflen = 0;
            continue;
        }
        if (len == 1) {
            char c = buf[0];
            for (int i = 0; i < NUM_SLOTS; i++) {
                if (c == slot_keys[i]) { jump_slot(i); goto next; }
            }
            if (c == 'h') { move_slot(-1); continue; }
            if (c == 'l') { move_slot(1); continue; }
            if (c == 'j') { adjust(1); continue; }
            if (c == 'k') { adjust(-1); continue; }
            if (c >= '0' && c <= '9') { input_digit(c); continue; }
        }
        next:;
    }

    XUngrabKeyboard(dpy, CurrentTime);
    XftDrawDestroy(xd);
    XftFontClose(dpy, font);
    XDestroyWindow(dpy, win);
    XCloseDisplay(dpy);

    if (confirmed) {
        save_last();
        char iso[32];
        to_iso(iso, sizeof(iso));
        printf("%s\n", iso);
        return 0;
    }
    return 1;
}
