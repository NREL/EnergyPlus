#ifndef CAMM_UTIL_H
#define CAMM_UTIL_H    /*+ To stop multiple inclusions. +*/

#ifndef ATL_SSE1
   #error "This routine requires SSE1!"
#endif
typedef struct {
  float r,i;
} Complex;

typedef struct {
  double r,i;
} Dcomplex;

#undef str
#define str(a_) xstr(a_)
#undef xstr
#define xstr(a_) #a_

#undef val
#define val(a_) xval(a_)
#undef xval
#define xval(a_) a_

#ifndef Mjoin
#define Mjoin(a,b) mjoin(a,b)
#ifdef mjoin
   #undef mjoin
#endif
#define mjoin(a,b) a ## b
#endif

#undef VOLATILE
#define VOLATILE __volatile__
#undef ASM
#define ASM __asm__ VOLATILE

#ifdef BETA0
#undef BL
#define BL b0
#endif
#ifdef BETA1
#undef BL
#define BL b1
#endif
#ifdef BETAX
#undef BL
#define BL bX
#endif
#ifdef BETAXI0
#undef BL
#define BL bXi0
#endif

#ifdef NO_TRANSPOSE
#ifdef GER
#ifdef Conj_
#undef FEXT
#define FEXT Gc
#else
#undef FEXT
#define FEXT Gu
#endif
#else
#ifdef Conj_
#undef FEXT
#define FEXT Nc
#else
#undef FEXT
#define FEXT N
#endif
#endif
#else
#ifdef Conj_
#undef FEXT
#define FEXT C
#else
#undef FEXT
#define FEXT T
#endif
#endif

#undef BLC
#define BLC Mjoin(FEXT,BL)

#ifdef __GNUC__
#undef NO_INLINE
#define NO_INLINE  double sq(double x) {return x*x;}
#else
#undef NO_INLINE
#define NO_INLINE
#endif

#undef lab
#define lab(a_)     "\n" str(MY_FUNCTION)  "_" str(N) "_" str(a_) ":\n\t"
#undef jmp
#define jmp(a_)     "jmp " str(MY_FUNCTION) "_" str(N) "_" str(a_) "\n\t"
#undef je
#define je(a_)      "je " str(MY_FUNCTION) "_" str(N) "_" str(a_) "\n\t"
#undef jge
#define jge(a_)     "jge " str(MY_FUNCTION) "_" str(N) "_" str(a_) "\n\t"
#undef jle
#define jle(a_)     "jle " str(MY_FUNCTION) "_" str(N) "_" str(a_) "\n\t"
#undef jl
#define jl(a_)      "jl " str(MY_FUNCTION) "_" str(N) "_" str(a_) "\n\t"
#undef jne
#define jne(a_)     "jne " str(MY_FUNCTION) "_" str(N) "_" str(a_) "\n\t"
#undef align
#define align       ".align 16\n\t"
#undef test
#define test(a_,b_) "testl $" str(a_) ",%%e" str(b_) "\n\t"
#undef and
#define and(a_,b_)  "andl $" str(a_) ",%%e" str(b_) "\n\t"
#undef sub
#define sub(a_,b_)  "subl $" str(a_) ",%%e" str(b_) "\n\t"
#undef SS
#define SS(a_,b_)   a_ + b_
#undef MM
#define MM(a_,b_)   a_ * b_
#undef E4
#define E4(a_)      (( a_ >> 2 ) << 2 )

#undef TYPE
#undef SCALAR
#undef PREC
#undef CSHUF
#undef LSHUF
#undef HSHUF
#undef ISHUF
#undef RSHUF
#undef SINGLE
#undef REAL
#undef DIV

#ifdef SCPLX
#define TYPE Complex
#define SCALAR Complex *
#define PREC c
#define CSHUF 177
#define LSHUF 160
#define HSHUF 245
#define ISHUF 13*17
#define RSHUF 8*17
#define SINGLE
#define DIV 2
/* #ifdef Conj_ */
/*   static const TYPE signd[2]={{-1.0,1.0},{-1.0,1.0}}; */
/* #else */
  static const TYPE signd[2]={{1.0,-1.0},{1.0,-1.0}};
/* #endif */
#endif

#ifdef SREAL
#define TYPE float
#define SCALAR float
#define PREC s
#define SINGLE
#define REAL
#define DIV 1
#endif

#ifdef DREAL
#define TYPE double
#define SCALAR double
#define PREC d
#define REAL
#define DIV 2
#endif

#ifdef DCPLX
#define TYPE Dcomplex
#define SCALAR Dcomplex *
#define PREC z
#define CSHUF 1
#define LSHUF 0
#define HSHUF 3
#define ISHUF 3
#define RSHUF 0
#define DIV 4
/* #ifdef Conj_ */
/*   static const TYPE signd[1]={{-1.0,1.0}}; */
/* #else */
  static const TYPE signd[1]={{1.0,-1.0}};
/* #endif */
#endif

#undef M11
#define M11 0
#undef M12
#define M12 1
#undef M13
#define M13 2
#undef M14
#define M14 3
#undef M15
#define M15 4
#undef M16
#define M16 5
#undef M17
#define M17 6
#undef M18
#define M18 7

#undef M23
#define M23 1
#undef M24
#define M24 2
#undef M25
#define M25 3
#undef M26
#define M26 4
#undef M27
#define M27 5
#undef M28
#define M28 6

#undef M33
#define M33 0
#undef M34
#define M34 1
#undef M35
#define M35 2
#undef M36
#define M36 3
#undef M37
#define M37 4
#undef M38
#define M38 5

#undef P10
#define P10 1
#undef P11
#define P11 2
#undef P12
#define P12 3
#undef P13
#define P13 4
#undef P14
#define P14 5
#undef P15
#define P15 6
#undef P16
#define P16 7

#undef XM
#define XM(a_,b_)     M ## b_ ## a_
#undef M
#define M(a_,b_)      XM(a_,b_)

#undef XP
#define XP(a_,b_)     P ## b_ ## a_
#undef P
#define P(a_,b_)      XP(a_,b_)

#undef mex
#define mex(a_)       str(%%e ## a_)
#undef msx
#define msx(a_)       "%%st(" str(a_) ")"

#undef cmp
#define cmp(a_,b_)    "cmp " mex(a_) "," mex(b_) "\n\t"
#undef icmpr
#define icmpr(a_,b_)    "cmp " mex(a_) ",(" mex(b_) ")\n\t"
#undef f
#define f(a_,b_,c_)   "prefetch" str(a_) " " str(b_) "(%%e" #c_ ")\n\t"
#undef pfx
#define pfx(a_,b_,c_,d_,e_)   "prefetch" str(a_) " " str(b_) "(%%e" #c_ ",%%e" #d_ "," str(e_) ")\n\t"
#undef a
#define a(a_,b_)      "addl $" str(a_) "," mex(b_) "\n\t"
#undef m
#define m(a_,b_)      "imul $" str(a_) "," mex(b_) "\n\t"
#undef pop
#define pop(a_)       "popl %%e" str(a_) "\n\t"
#undef push
#define push(a_)      "pushl %%e" str(a_) "\n\t"
#undef d
#define d(a_,b_)      "idiv $" str(a_) "," mex(b_) "\n\t"
#undef shl
#define shl(a_,b_)    "shl $" str(a_) "," mex(b_) "\n\t"
#undef shr
#define shr(a_,b_)    "shr $" str(a_) "," mex(b_) "\n\t"
#undef mm
#define mm(a_,b_)     "mov $" str(a_) "," mex(b_) "\n\t"
#undef ra
#define ra(a_,b_)     "addl %%e" str(a_) "," mex(b_) "\n\t"
#undef rs
#define rs(a_,b_)     "subl %%e" str(a_) "," mex(b_) "\n\t"

#undef fl
#define fl(a_,b_)     "fldl " str(a_) "(" mex(b_) ")\n\t"
#undef fp
#define fp(a_,b_)     "fstpl " str(a_) "(" mex(b_) ")\n\t"
#undef fd
#define fd(a_)        "fld " msx(a_) "\n\t"
#undef fap
#define fap(a_,b_)    "faddp " msx(a_) "," msx(b_) "\n\t"
/* #define fsp(a_)       fx(a_) "fsubp %%st," msx(a_) "\n\t" */
#undef fsp
#define fsp(a_)       "fsubrp %%st," msx(a_) "\n\t"
#undef fmp
#define fmp(a_,b_)    "fmulp " msx(a_) "," msx(b_) "\n\t"
#undef fa
#define fa(a_,b_)     "fadd " msx(a_) "," msx(b_) "\n\t"
#undef fm
#define fm(a_,b_)     "fmul " msx(a_) "," msx(b_) "\n\t"
#undef faa
#define faa(a_,b_)    "faddl " str(a_) "(" mex(b_) ")\n\t"
#undef fma
#define fma(a_,b_)    "fmull " str(a_) "(" mex(b_) ")\n\t"
#undef fz
#define fz            "fldz\n\t"
#undef fx
#define fx(a_)        "fxch " msx(a_) "\n\t"
#undef fx1
#define fx1           "fxch\n\t"
#undef fc
#define fc(a_)        "fstp " msx(a_) "\n\t"


#ifndef ATHLON


#if defined(DREAL) || defined(DCPLX)
#undef SSESUF
#define SSESUF "d "
#undef RS4
#define RS4 16
#undef RS
#define RS 4
#else
#undef SSESUF
#define SSESUF "s "
#undef RS4
#define RS4 16
#undef RS
#define RS  4
#endif

#undef mxx
#define mxx(a_)        str(%%xmm ## a_)
#undef prp
#define prp(a_,b_)     "rcpp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef prps
#define prps(a_,b_)    "rcps" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pann
#define pann(a_,b_)    "andnp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef psqs
#define psqs(a_,b_)    "sqrts" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef por
#define por(a_,b_)     "orp"   SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pan
#define pan(a_,b_)     "andp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pcm
#define pcm(a_,b_,c_)  "cmpp" SSESUF " $" str(a_) "," mxx(b_) "," mxx(c_) "\n\t"
#undef pcms
#define pcms(a_,b_,c_) "cmps" SSESUF " $" str(a_) "," mxx(b_) "," mxx(c_) "\n\t"
#undef pax
#define pax(a_,b_)     "maxp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef paxs
#define paxs(a_,b_)    "maxs" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pd
#define pd(a_,b_)      "divp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pdsr
#define pdsr(a_,b_)    "divs" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pxx
#define pxx(a_,b_)     "xorp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef px
#define px(a_)         "xorp" SSESUF mxx(a_) "," mxx(a_) "\n\t"
#undef pm
#define pm(a_,b_)      "mulp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pa
#define pa(a_,b_)      "addp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pmm
#define pmm(a_,b_,c_)  "mulp" SSESUF str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pam
#define pam(a_,b_,c_)  "addp" SSESUF str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pl
#define pl(a_,b_,c_)   "movup" SSESUF str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pla
#define pla(a_,b_,c_)  "movap" SSESUF str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pu
#define pu(a_,b_,c_)   "movup" SSESUF mxx(a_) "," str(b_) "(" mex(c_) ")\n\t"
#undef punt
#define punt(a_,b_,c_) "movntp" SSESUF mxx(a_) "," str(b_) "(" mex(c_) ")\n\t"
#undef pua
#define pua(a_,b_,c_)  "movap" SSESUF mxx(a_) "," str(b_) "(" mex(c_) ")\n\t"
#undef pud
#define pud(a_,b_,c_)  "movlp" SSESUF mxx(a_) "," str(b_) "(" mex(c_) ")\n\t"
#undef pudr
#define pudr(a_,b_)    "movlp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pc
#define pc(a_,b_)      "movap" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef ps
#define ps(a_,b_,c_)   "shufp" SSESUF " $" str(a_) "," mxx(b_) "," mxx(c_) "\n\t"
#undef phl
#define phl(a_,b_)     "movhlp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pus
#define pus(a_,b_,c_)  "movs" SSESUF mxx(a_) "," str(b_) "(" mex(c_) ")\n\t"
#undef pls
#define pls(a_,b_,c_)  "movs" SSESUF str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pld
#define pld(a_,b_,c_)  "movlp" SSESUF str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef plh
#define plh(a_,b_)     "movlhp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pas
#define pas(a_,b_,c_)  "adds" SSESUF str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pms
#define pms(a_,b_,c_)  "muls" SSESUF str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pcs
#define pcs(a_,b_)     "movs" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pasr
#define pasr(a_,b_)    "adds" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pmsr
#define pmsr(a_,b_)    "muls" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef pul
#define pul(a_,b_)     "unpcklp" SSESUF mxx(a_) "," mxx(b_) "\n\t"
#undef puh
#define puh(a_,b_)     "unpckhp" SSESUF mxx(a_) "," mxx(b_) "\n\t"

#undef plsx
#define plsx(a_,b_,c_,d_,e_) \
                       "movs" SSESUF str(a_) "(" mex(b_) "," mex(c_) "," #d_ ")," mxx(e_) "\n\t"
#undef plx
#define plx(a_,b_,c_,d_,e_) \
                       "movup" SSESUF str(a_) "(" mex(b_) "," mex(c_) "," #d_ ")," mxx(e_) "\n\t"
#undef plax
#define plax(a_,b_,c_,d_,e_) \
                       "movap" SSESUF str(a_) "(" mex(b_) "," mex(c_) "," #d_ ")," mxx(e_) "\n\t"
#undef pasx
#define pasx(a_,b_,c_,d_,e_) \
                       "adds" SSESUF str(a_) "(" mex(b_) "," mex(c_) "," #d_ ")," mxx(e_) "\n\t"
#undef pusx
#define pusx(a_,b_,c_,d_,e_) \
                       "movs" SSESUF mxx(a_) "," str(b_) "(" mex(c_) "," mex(d_) "," #e_ ")\n\t"
#undef pux
#define pux(a_,b_,c_,d_,e_) \
                       "movup" SSESUF mxx(a_) "," str(b_) "(" mex(c_) "," mex(d_) "," #e_ ")\n\t"
#undef puax
#define puax(a_,b_,c_,d_,e_) \
                       "movap" SSESUF mxx(a_) "," str(b_) "(" mex(c_) "," mex(d_) "," #e_ ")\n\t"
#undef pudx
#define pudx(a_,b_,c_,d_,e_) \
                       "movlp" SSESUF mxx(a_) "," str(b_) "(" mex(c_) "," mex(d_) "," #e_ ")\n\t"

#undef pldx
#define pldx(a_,b_,c_,d_,e_) \
                       "movlp" SSESUF str(a_) "(" mex(b_) "," mex(c_) "," #d_ ")," mxx(e_) "\n\t"

#else

#undef RS4
#define RS4 8
#undef RS
#define RS  2

#undef mxx
#define mxx(a_)       str(%%mm ## a_)
#undef pul
#define pul(a_,b_)    "punpckldq " mxx(a_) "," mxx(b_) "\n\t"
#undef puh
#define puh(a_,b_)    "punpckhdq " mxx(a_) "," mxx(b_) "\n\t"

#undef px
#define px(a_)        "pxor " mxx(a_) "," mxx(a_) "\n\t"
#undef pm
#define pm(a_,b_)     "pfmul " mxx(a_) "," mxx(b_) "\n\t"
#undef pa
#define pa(a_,b_)     "pfadd " mxx(a_) "," mxx(b_) "\n\t"
#undef pac
#define pac(a_,b_)    "pfacc " mxx(a_) "," mxx(b_) "\n\t"
#undef pmm
#define pmm(a_,b_,c_) "pfmul " str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pam
#define pam(a_,b_,c_) "pfadd " str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pl
#define pl(a_,b_,c_)  "movq " str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pla
#define pla(a_,b_,c_) "movq " str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"
#undef pu
#define pu(a_,b_,c_)  "movq " mxx(a_) "," str(b_) "(" mex(c_) ")\n\t"
#undef pc
#define pc(a_,b_)     "movq " mxx(a_) "," mxx(b_) "\n\t"
#undef ps
#define ps(a_,b_,c_)  "pswapd " mxx(b_) "," mxx(c_) "\n\t"
#undef phl
#define phl(a_,b_)    "punpckhdq " mxx(a_) "," mxx(b_) "\n\t"
#undef plh
#define plh(a_,b_)    "punpckldq " mxx(a_) "," mxx(b_) "\n\t"
#undef pus
#define pus(a_,b_,c_) "movd " mxx(a_) "," str(b_) "(" mex(c_) ")\n\t"
#undef pls
#define pls(a_,b_,c_) "movd " str(a_) "(" mex(b_) ")," mxx(c_) "\n\t"

#undef plsx
#define plsx(a_,b_,c_,d_,e_) \
                      "movd " str(a_) "(" mex(b_) "," mex(c_) "," #d_ ")," mxx(e_) "\n\t"
#undef plx
#define plx(a_,b_,c_,d_,e_)  \
                      "movq " str(a_) "(" mex(b_) "," mex(c_) "," #d_ ")," mxx(e_) "\n\t"
#undef pasx
#define pasx(a_,b_,c_,d_,e_) \
                      "addss " str(a_) "(" mex(b_) "," mex(c_) "," #d_ ")," mxx(e_) "\n\t"
#undef pusx
#define pusx(a_,b_,c_,d_,e_) \
                      "movd " mxx(a_) "," str(b_) "(" mex(c_) "," mex(d_) "," #e_ ")\n\t"
#undef pux
#define pux(a_,b_,c_,d_,e_)  \
                      "movq " mxx(a_) "," str(b_) "(" mex(c_) "," mex(d_) "," #e_ ")\n\t"
#endif

#endif /* CAMM_UTIL_H */
