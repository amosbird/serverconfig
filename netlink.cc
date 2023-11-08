#include <cstring>
#include <dlfcn.h>
#include <ifaddrs.h>
#include <iostream>
#include <map>
#include <net/if.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/types.h>

#define QWE(name)                                                                                  \
    static decltype(&name) orig_##name;                                                            \
    if (!orig_##name && !(orig_##name = (decltype(&name))dlsym(RTLD_NEXT, #name))) {               \
        abort();                                                                                   \
    }

extern "C" {

/// g++ -std=c++2a -pthread -fpic -fpermissive -shared netlink.cc -o netlink.so -ldl -D_GNU_SOURCE

// std::string card = "wlp0s20f3";
// int getifaddrs(struct ifaddrs** ifap) {
//     *ifap = NULL;
//     return 0;
//     QWE(getifaddrs);
//     return orig_getifaddrs(ifap);
//     auto ret = orig_getifaddrs(ifap);
//     if (ret == -1)
//         return -1;

//     int family, s;
//     char host[NI_MAXHOST];

//     // ifaddrs* card = NULL;
//     using namespace std::literals;
//     for (ifaddrs* ifa = *ifap; ifa != NULL; ifa = ifa->ifa_next) {
//         if (card != ifa->ifa_name) {
//             ifa->ifa_addr = NULL;
//             // continue;
//         }

//         // card = ifa;
//         // card->ifa_next = NULL;
//         // ifaddr_map.emplace(card, *ifap);
//         // *ifap = card;
//     }

//     return ret;
// }

int prlimit64(pid_t pid, enum __rlimit_resource resource, const struct rlimit64* new_limit,
    struct rlimit64* old_limit) {
    if (resource == RLIMIT_CORE)
        return 0;

    QWE(prlimit64);
    return orig_prlimit64(pid, resource, new_limit, old_limit);
}

// int socket(int family, int type, int protocol) {
//     QWE(socket);
//     struct ifreq interface;
//     int fd = orig_socket(family, type, protocol);
//     int errorCode;
//     strcpy(interface.ifr_name, card.c_str());
//     errorCode = setsockopt(fd, SOL_SOCKET, SO_BINDTODEVICE, &interface, sizeof(interface));
//     if (errorCode < 0) {
//         perror("setsockopt");
//         errno = EINVAL;
//         return -1;
//     };

//     return fd;
// }

// void freeifaddrs(struct ifaddrs* ifa) {
//     QWE(freeifaddrs);
//     auto it = ifaddr_map.find(ifa);
//     if (it != ifaddr_map.end())
//         orig_freeifaddrs(it->second);
//     else
//         orig_freeifaddrs(ifa);
// }
}
