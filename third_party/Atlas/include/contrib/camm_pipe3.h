#include "camm_util.h"

#ifndef N
#error N must be defined in camm_pipe3.h
#endif
#ifndef KB
#error KB must be defined in camm_pipe3.h
#endif

#undef p1
#define p1(a_)         Mjoin(p1_4_,N)(a_)
#undef p2
#define p2(a_)         Mjoin(p1_2_,N)(a_)
#undef p4
#define p4(a_)         Mjoin(p1_,N)(a_)
#undef load_pipe
#define load_pipe(a_)  Mjoin(lp,N)(a_)
#undef drain_pipe
#define drain_pipe(a_) Mjoin(dp,N)(a_)
#undef pipe_len
#define pipe_len       Mjoin(pl,N)

#undef p8
#if pipe_len > 4
#define p8(a_)         Mjoin(p2_,N)(a_)
#else
#define p8(a_)         p4(a_)   p4(SS(a_,16))
#endif

#undef p16
#if pipe_len > 8
#define p16(a_)        Mjoin(p4_,N)(a_)
#else
#define p16(a_)        p8(a_)   p8(SS(a_,32))
#endif

#undef p32
#if pipe_len > 16
#define p32(a_)        Mjoin(p8_,N)(a_)
#else
#define p32(a_)        p16(a_)   p16(SS(a_,64))
#endif

#undef p64
#if pipe_len > 32
#define p64(a_)        Mjoin(p16_,N)(a_)
#else
#define p64(a_)        p32(a_)   p32(SS(a_,128))
#endif

#undef p128
#if pipe_len > 64
#define p128(a_)       Mjoin(p32_,N)(a_)
#else
#define p128(a_)       p64(a_)   p64(SS(a_,256))
#endif

#undef p256
#if pipe_len > 128
#define p256(a_)       Mjoin(p64_,N)(a_)
#else
#define p256(a_)       p128(a_)   p128(SS(a_,512))
#endif

#if KB < pipe_len
#undef pipe_len
#define pipe_len 0
#undef load_pipe
#define load_pipe(a_)
#undef drain_pipe
#define drain_pipe(a_)
#endif


#undef MKB
/* #ifdef SREAL */
#define MKB KB
/* #elif defined (DCPLX) */
/* #define MKB ( KB * 4 ) */
/* #else */
/* #define MKB ( KB * 2 ) */
/* #endif */

#if MKB >= 512
#error MKB must be less than 512
#endif

#undef x0
#undef o0
#define x0 load_pipe(0)
#define o0 0

#undef MKBB
#define MKBB ( MKB - pipe_len )

#undef xx1
#undef oo1
#if MKBB >= 256
#define xx1 x0 p256(o0)
#define oo1 SS(1024,o0)
#else
#define xx1 x0
#define oo1 o0
#endif

#undef xx1a
#undef oo1a
#if pipe_len == 256
#define xx1a xx1 drain_pipe(oo1)
#define oo1a SS(1024,oo1)
#undef MKBB
#define MKBB MKB
#else
#define xx1a xx1
#define oo1a oo1
#endif

#undef x1
#undef o1
#if ( MKBB / 128 ) % 2
#define x1 xx1a p128(oo1a)
#define o1 SS(512,oo1a)
#else
#define x1 xx1a
#define o1 oo1a
#endif

#undef x1a
#undef o1a
#if pipe_len == 128
#define x1a x1 drain_pipe(o1)
#define o1a SS(512,o1)
#undef MKBB
#define MKBB MKB
#else
#define x1a x1
#define o1a o1
#endif

#undef x2
#undef o2
#if ( MKBB / 64 ) % 2
#define x2  x1a p64(o1a)
#define o2 SS(256,o1a)
#else
#define x2 x1a
#define o2 o1a
#endif

#undef x2a
#undef o2a
#if pipe_len == 64
#define x2a x2 drain_pipe(o2)
#define o2a SS(256,o2)
#undef MKBB
#define MKBB MKB
#else
#define x2a x2
#define o2a o2
#endif

#undef x3
#undef o3
#if ( MKBB / 32 ) % 2
#define x3  x2a p32(o2a)
#define o3 SS(128,o2a)
#else
#define x3 x2a
#define o3 o2a
#endif

#undef x3a
#undef o3a
#if pipe_len == 32
#define x3a x3 drain_pipe(o3)
#define o3a SS(128,o3)
#undef MKBB
#define MKBB MKB
#else
#define x3a x3
#define o3a o3
#endif

#undef x4
#undef o4
#if ( MKBB / 16 ) % 2
#define x4 x3a p16(o3a)
#define o4 SS(64,o3a)
#else
#define x4 x3a
#define o4 o3a
#endif

#undef x4a
#undef o4a
#if pipe_len == 16
#define x4a x4 drain_pipe(o4)
#define o4a SS(64,o4)
#undef MKBB
#define MKBB MKB
#else
#define x4a x4
#define o4a o4
#endif

#undef x5
#undef o5
#if ( MKBB / 8 ) % 2
#define x5  x4a p8(o4a)
#define o5 SS(32,o4a)
#else
#define x5 x4a
#define o5 o4a
#endif

#undef x5a
#undef o5a
#if pipe_len == 8
#define x5a x5 drain_pipe(o5)
#define o5a SS(32,o5)
#undef MKBB
#define MKBB MKB
#else
#define x5a x5
#define o5a o5
#endif

#undef x6
#undef o6
#if ( MKBB / 4 ) % 2
#define x6  x5a p4(o5a)
#define o6 SS(16,o5a)
#else
#define x6 x5a
#define o6 o5a
#endif

#undef x6a
#undef o6a
#if pipe_len == 4
#define x6a x6 drain_pipe(o6)
#define o6a SS(16,o6)
#undef MKBB
#define MKBB MKB
#else
#define x6a x6
#define o6a o6
#endif

#undef x7
#undef o7
#if ( MKB / 2 ) % 2
#define x7  x6a p2(o6a)
#define o7 SS(8,o6a)
#else
#define x7 x6a
#define o7 o6a
#endif

#undef x7a
#undef o7a
#if pipe_len == 2
#define x7a x7 drain_pipe(o7)
#define o7a SS(8,o7)
#undef MKBB
#define MKBB MKB
#else
#define x7a x7
#define o7a o7
#endif

#undef x8
#undef o8
#if ( MKB / 1 ) % 2
#define x8 x7a p1(o7a)
#define o8 SS(4,o7a)
#else
#define x8 x7a
#define o8 o7a
#endif

#undef x8a
#undef o8a
#if pipe_len == 1
#define x8a x8 drain_pipe(o8)
#define o8a SS(4,o8)
#undef MKBB
#define MKBB MKB
#else
#define x8a x8
#define o8a o8
#endif

#undef KB_block
#define KB_block x8a
