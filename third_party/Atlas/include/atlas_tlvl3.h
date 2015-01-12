#ifndef atlas_tlvl3_H
   #define atlas_tlvl3_H

#include "atlas_threads.h"
#ifdef TYPE
   #include "atlas_lvl3.h"
#endif
#ifndef ATL_XOVER_L3
   #ifdef TREAL
      #define ATL_XOVER_L3 2   /* number of NBxNB blocks */
   #else
      #define ATL_XOVER_L3 1
   #endif
#endif

#ifndef ATL_TGEMM_XOVER
   #define ATL_TGEMM_XOVER ATL_XOVER_L3
#endif
#ifndef ATL_TGEMM_ADDP
   #define ATL_TGEMM_ADDP 1
#endif
/*
 * Number of blocks per proc for GEMM to divide M only
 */
#ifndef ATL_TMMMINMBLKS
   #define ATL_TMMMINMBLKS 4
#endif
#ifndef ATL_TGEMM_THRESH_MF
   #define ATL_TGEMM_THRESH_MF  ((((2.0*(ATL_TGEMM_XOVER))*MB)*NB)*KB)
#endif
/*
 * This is the minimal number of flops each thread requires once THRESH
 * is exceeded
 */
#ifndef ATL_TGEMM_PERTHR_MF
   #define ATL_TGEMM_PERTHR_MF  ((((2.0*ATL_TGEMM_ADDP)*MB)*NB)*KB)
#endif
/*
 * For debugging, can define ATL_SERIAL_COMBINE, and then it any required
 * workspaces of C will be allocated before beginning parallel operations,
 * and all required combined will happen after parallel operations are
 * done.
 */
// #define ATL_SERIAL_COMBINE
#ifdef ATL_SERIAL_COMBINE
typedef struct ATL_CombNode ATL_combnode_t;
struct ATL_CombNode
{
   ATL_INT M, N, ldw, ldd;
   void *W, *D;                 /* Work and Destination */
   ATL_combnode_t *next;
};
#endif
/*
 * The array Cinfp holds C partitioning information.  This array holds a
 * list of pointers to nodes whose data I have not been able to combine
 * with my native C partition.  The first nCw entries contain the pointers
 * to the MMNODE of allocated C workspaces that I have not been able to
 * combine.  If my node has C in workspace, I am the first entry in this array.
 * Sometimes, a child thread has been combined with me that owned a piece of
 * the original C.  These values do not need to be combined (they were written
 * to the original C), but we need to combine the range of "owned" workspaces
 * so that we know when it is legal for a parent node to add into the space.
 * The final nCp entries of Cinfp entries of Cinfp hold these original pieces
 * that need to be combined to create larger owned partitions (starting from
 * the end of the array).  If the C ptr is NULL, that means that entry has
 * been subsumed into a new entry.
 */
typedef struct ATL_TMMNode ATL_TMMNODE_t;
struct ATL_TMMNode
{
   ATL_TMMNODE_t *Cinfp[ATL_NTHREADS];
   void (*gemmK)(ATL_CINT, ATL_CINT, ATL_CINT, const void*, const void *,
                 ATL_CINT, const void*, ATL_CINT, const void*, void*, ATL_CINT);
   const void *A, *B;
   void *C, *Cw;
   void *alpha, *beta;
   void *zero, *one;
   ATL_INT ldcw, M, N, K, lda, ldb, ldc;
   int mb, nb, kb;
   int eltsz, eltsh; /* element size, and shift (eg. log_2(eltsz)) */
   int rank;         /* the rank of my thread ([0,P-1]) */
   int nCw;          /* # of workspace entries in 1st nCw elts of Cinfp array */
   int nCp;          /* # of orig. C pieces last nCp elts of Cinfp */
   int ownC;         /* do I own my piece of C, or only wrkspace? */
};
/*
 * This data structure used for dynamically scheduled rank-K update
 * It is needed only by routines that are typed, and thus define TYPE
 */
#ifdef TYPE
typedef struct
{
   void *aNcnt;           /* count on col-panels of C */
   void *aMcnt;           /* count row-panels of A */
   void **aMcnts;         /* P-len array of counts on row-blks of C */
   void **Mlocks;         /* mutexes protecting init of aMcnts */
   int *Js;               /* current C col for each node */
   int Sync0;             /* 0: no sync at end; else thr 0 waits til all done */
   volatile int *chkin;   /* ATL_NTHREAD-len checkin array */
   TYPE **Bws;            /* preallocated thread copy areas */
   TYPE *Aw;              /* workspace for common A */
   const TYPE *A, *B;     /* original input matrices */
   TYPE *C;               /* original output matrix */
   #ifdef TREAL
      TYPE alpha;
      TYPE beta;
   #else
      const TYPE *alpha;
      const TYPE *beta;
   #endif
   ATL_INT nKb, kr, kr8;
   ATL_INT nMb, mr, nNb, nr;
   ATL_INT M, N, K, lda, ldb, ldc;
   enum ATLAS_TRANS TA, TB;
} ATL_TGEMM_RKK_t;
#endif

/*
 * This data structure is used when we split K for SYRK
 */
typedef struct ATL_SyrkK ATL_TSYRK_K_t;
struct ATL_SyrkK
{
   ATL_TSYRK_K_t *Cinfp[ATL_NTHREADS];
   void (*gemmT)(const enum ATLAS_TRANS, const enum ATLAS_TRANS,
                  ATL_CINT, ATL_CINT, ATL_CINT, const void *,
                  const void *, ATL_CINT, const void *, ATL_CINT,
                  const void *, void *, ATL_CINT);
   void (*tvsyrk)(const enum ATLAS_UPLO, const enum ATLAS_TRANS, ATL_CINT,
                  ATL_CINT, const void*, const void*, ATL_CINT, const void*,
                  void*, ATL_CINT);
   const void *A;
   void *C, *Cw;
   void *DoComb;
   ATL_LAUNCHSTRUCT_t *lp;
   const void *alpha, *beta;
   const void *zero, *one;
   ATL_INT ldcw, N, K, nb, lda, ldc;
   int eltsh, rank, nCw;
   enum ATLAS_UPLO Uplo;
   enum ATLAS_TRANS Trans, TB;
};
/*
 * This data structure used when we divide N only, and NTHREAD is a power of 2
 */
typedef struct
{
   void (*gemmK)(ATL_CINT, ATL_CINT, ATL_CINT, const void*, const void *,
                 ATL_CINT, const void*, ATL_CINT, const void*, void*, ATL_CINT);
   void (*tvsyrk)(const enum ATLAS_UPLO, const enum ATLAS_TRANS, ATL_CINT,
                  ATL_CINT, const void*, const void*, ATL_CINT, const void*,
                  void*, ATL_CINT);
   void *T;             /* Triangular matrix to do SYRK into*/
   void *C;             /* rect matrix to do GEMM into */
   const void *A0;      /* input matrix for syrk, */
   const void *A;       /* 1st input matrix for GEMM */
   const void *B;       /* 2nd input matrix for GEMM */
   const void *alpha, *beta;
   ATL_INT M;           /* size of SYRK and 1st dim of GEMM */
   ATL_INT N;           /* size of 2nd dim of N */
   ATL_INT K;           /* K of original problem */
   ATL_INT lda, ldc;
   int nb, eltsh;       /* shift to do equivalant of *sizeof */
   enum ATLAS_UPLO Uplo;
   enum ATLAS_TRANS TA, TB;
} ATL_TSYRK_M_t;


typedef struct
{
   const void *A, *alpha;
   void *B;
   ATL_INT M, N, lda, ldb;
   enum ATLAS_SIDE side;
   enum ATLAS_UPLO uplo;
   enum ATLAS_TRANS TA;
   enum ATLAS_DIAG  diag;
} ATL_TTRSM_t;

typedef struct
{
   const void *A, *B, *alpha, *beta;
   void *C;
   ATL_INT M, N, lda, ldb, ldc, nb;
   enum ATLAS_SIDE side;
   enum ATLAS_UPLO uplo;
} ATL_TSYMM_t;
typedef struct
{
   const void *alpha, *alpha2, *beta, *one, *zero;
   void (*tvgemm)(const enum ATLAS_TRANS, const enum ATLAS_TRANS,
                  ATL_CINT, ATL_CINT, ATL_CINT, const void *,
                  const void *, ATL_CINT, const void *, ATL_CINT,
                  const void *, void *, ATL_CINT);
   void (*tvApAt)(const enum ATLAS_UPLO, ATL_CINT, const void *, ATL_CINT,
                  const void *, void *, ATL_CINT);

   ATL_INT K, lda, ldb, ldc;
   int nb, eltsh;
   enum ATLAS_UPLO Uplo;
   enum ATLAS_TRANS trans, TA, TB,  /* trans for syr2k, TA,TB are for GEMM */
                    TA2, TB2;       /* transpose of TA,TB */
} ATL_SYR2K_t;

/*
 * =============================================================================
 * Function prototypes
 * =============================================================================
 */
void ATL_EnforceNonPwr2LO(ATL_TMMNODE_t *ptmms, const int P);
int Mjoin(PATL,threadMM)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                         size_t M, size_t N, size_t K);
void ATL_tvsyr2k_rec(ATL_SYR2K_t *syp, ATL_CINT Nblks, ATL_CINT nr,
                     const void *A, const void *B, void *C);
#ifdef TYPE
void Mjoin(PATL,tsyrk)
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
    ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
#ifdef TCPLX
void Mjoin(PATL,therk)
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans, ATL_CINT N,
    ATL_CINT K, const TYPE alpha, const TYPE *A, ATL_CINT lda,
    const TYPE beta, TYPE *C, ATL_CINT ldc);
#endif
void Mjoin(PATL,tsymm)
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc);
#ifdef TCPLX
void Mjoin(PATL,themm)
   (const enum ATLAS_SIDE Side, const enum ATLAS_UPLO Uplo,
    ATL_CINT M, ATL_CINT N, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc);
#endif
void Mjoin(PATL,tsyr2k)
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsyr2k)
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const SCALAR beta, TYPE *C, ATL_CINT ldc);
#ifdef TCPLX
void Mjoin(PATL,ther2k)
   (const enum ATLAS_UPLO Uplo, const enum ATLAS_TRANS Trans,
    ATL_CINT N, ATL_CINT K, const SCALAR alpha, const TYPE *A, ATL_CINT lda,
    const TYPE *B, ATL_CINT ldb, const TYPE beta, TYPE *C, ATL_CINT ldc);
#endif

void Mjoin(PATL,ttrsm)(const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
                       const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
                       ATL_CINT M, ATL_CINT N, const SCALAR alpha,
                       const TYPE *A, ATL_CINT lda, TYPE *B, ATL_CINT ldb);
void Mjoin(PATL,ttrmm)(const enum ATLAS_SIDE side, const enum ATLAS_UPLO uplo,
                       const enum ATLAS_TRANS TA, const enum ATLAS_DIAG diag,
                       ATL_CINT M, ATL_CINT N, const SCALAR alpha,
                       const TYPE *A, ATL_CINT lda, TYPE *B, ATL_CINT ldb);
void Mjoin(PATL,tgemm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                       ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
                       const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
                       const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tvgemm)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                        ATL_CINT M, ATL_CINT N, ATL_CINT K, const void *alpha,
                        const void *A, ATL_CINT lda, const void *B,ATL_CINT ldb,
                        const void *beta, void *C, ATL_CINT ldc);
int Mjoin(PATL,tgemm_bigMN_Kp)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
int Mjoin(PATL,tgemm_rkK)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
int Mjoin(PATL,tgemm_rec)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
#ifdef TCPLX
void Mjoin(PATL,tgemmCC)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsvgemmCC)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc);
#endif  /* end ifdef TCPLX */
#ifdef TCPLX
void Mjoin(PATL,tgemmCN)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsvgemmCN)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc);
#endif  /* end ifdef TCPLX */
#ifdef TCPLX
void Mjoin(PATL,tgemmCT)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsvgemmCT)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc);
#endif  /* end ifdef TCPLX */
#ifdef TCPLX
void Mjoin(PATL,tgemmNC)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsvgemmNC)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc);
#endif  /* end ifdef TCPLX */
void Mjoin(PATL,tgemmNN)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsvgemmNN)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc);
void Mjoin(PATL,tgemmNT)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsvgemmNT)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc);
#ifdef TCPLX
void Mjoin(PATL,tgemmTC)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsvgemmTC)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc);
#endif  /* end ifdef TCPLX */
void Mjoin(PATL,tgemmTN)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsvgemmTN)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc);
void Mjoin(PATL,tgemmTT)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *B, ATL_CINT ldb,
    const SCALAR beta, TYPE *C, ATL_CINT ldc);
void Mjoin(PATL,tsvgemmTT)
   (ATL_CINT M, ATL_CINT N, ATL_CINT K, const void* alpha,
    const void *A, ATL_CINT lda, const void *B, ATL_CINT ldb,
    const void *beta, void *C, ATL_CINT ldc);
#endif  /* end ifdef TYPE */

#endif
