#ifndef itoa_hpp_INCLUDED
#define itoa_hpp_INCLUDED

#include <stdint.h>

const char gDigitsLut[200] = {
    '0','0','0','1','0','2','0','3','0','4','0','5','0','6','0','7','0','8','0','9',
    '1','0','1','1','1','2','1','3','1','4','1','5','1','6','1','7','1','8','1','9',
    '2','0','2','1','2','2','2','3','2','4','2','5','2','6','2','7','2','8','2','9',
    '3','0','3','1','3','2','3','3','3','4','3','5','3','6','3','7','3','8','3','9',
    '4','0','4','1','4','2','4','3','4','4','4','5','4','6','4','7','4','8','4','9',
    '5','0','5','1','5','2','5','3','5','4','5','5','5','6','5','7','5','8','5','9',
    '6','0','6','1','6','2','6','3','6','4','6','5','6','6','6','7','6','8','6','9',
    '7','0','7','1','7','2','7','3','7','4','7','5','7','6','7','7','7','8','7','9',
    '8','0','8','1','8','2','8','3','8','4','8','5','8','6','8','7','8','8','8','9',
    '9','0','9','1','9','2','9','3','9','4','9','5','9','6','9','7','9','8','9','9'
};

#define BEGIN2(n) \
    do { \
        int t = (n); \
        if(t < 10) *p++ = '0' + t; \
        else { \
            t *= 2; \
            *p++ = gDigitsLut[t]; \
            *p++ = gDigitsLut[t + 1]; \
        } \
    } while(0)
#define MIDDLE2(n) \
    do { \
        int t = (n) * 2; \
        *p++ = gDigitsLut[t]; \
        *p++ = gDigitsLut[t + 1]; \
    } while(0)
#define BEGIN4(n) \
    do { \
        int t4 = (n); \
        if(t4 < 100) BEGIN2(t4); \
        else { BEGIN2(t4 / 100); MIDDLE2(t4 % 100); } \
    } while(0)
#define MIDDLE4(n) \
    do { \
        int t4 = (n); \
        MIDDLE2(t4 / 100); MIDDLE2(t4 % 100); \
    } while(0)
#define BEGIN8(n) \
    do { \
        uint32_t t8 = (n); \
        if(t8 < 10000) BEGIN4(t8); \
        else { BEGIN4(t8 / 10000); MIDDLE4(t8 % 10000); } \
    } while(0)
#define MIDDLE8(n) \
    do { \
        uint32_t t8 = (n); \
        MIDDLE4(t8 / 10000); MIDDLE4(t8 % 10000); \
    } while(0)
#define MIDDLE16(n) \
    do { \
        uint64_t t16 = (n); \
        MIDDLE8(t16 / 100000000); MIDDLE8(t16 % 100000000); \
    } while(0)

inline void u32toa(uint32_t x, char* p) {

    if(x < 100000000) BEGIN8(x);
    else { BEGIN2(x / 100000000); MIDDLE8(x % 100000000); }
    *p = 0;
}

inline void i32toa(int32_t x, char* p) {

    uint64_t t;
    if(x >= 0) t = x;
    else *p++ = '-', t = -uint32_t(x);
    u32toa(t, p);
}

inline void u64toa(uint64_t x, char* p) {

    if(x < 100000000) BEGIN8(x);
    else if(x < 10000000000000000) { BEGIN8(x / 100000000); MIDDLE8(x % 100000000); }
    else { BEGIN4(x / 10000000000000000); MIDDLE16(x % 10000000000000000); }
    *p = 0;
}

inline void i64toa(int64_t x, char* p) {

    uint64_t t;
    if(x >= 0) t = x;
    else *p++ = '-', t = -uint64_t(x);
    u64toa(t, p);
}

#endif // itoa_hpp_INCLUDED
