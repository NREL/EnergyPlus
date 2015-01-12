#include "atlas_misc.h"
#include "atlas_tst.h"
#include "atlas_f77.h"

#if defined(NoChange)
   #define F77TRTRI Mjoin(PRE,trtri)
#elif defined (UpCase)
   #define F77TRTRI Mjoin(PREU,TRTRI)
#elif defined (Add_) || defined(Add__)
   #define F77TRTRI Mjoin(PRE,trtri_)
#endif
#define f77trtri Mjoin(PATL,f77trtri)

int f77trtri(const enum ATLAS_UPLO Uplo, const enum ATLAS_DIAG Diag,
	     const int N, TYPE *A, const int lda)
{
   #if defined(StringSunStyle)
      #if defined(ATL_FunkyInts)
         F77_INTEGER ONE=1;
      #else
         int ONE=1;
      #endif
   #elif defined(StringStructVal) || defined(StringStructPtr) || defined(StringCrayStyle)
      F77_CHAR fuplo;
      F77_CHAR fdiag;
   #endif
   #ifdef ATL_FunkyInts
      const F77_INTEGER F77N=N, F77lda=lda;
      F77_INTEGER info;
   #else
      int info;
      #define F77N N
      #define F77lda lda
   #endif
   char cuplo;
   char cdiag;

   if (Uplo == AtlasUpper) cuplo = 'U';
   else cuplo = 'L';
   if (Diag == AtlasUnit) cdiag = 'U';
   else cdiag = 'N';
   #if defined(StringSunStyle)
      F77TRTRI(&cuplo, &cdiag, &F77N, A, &F77lda, &info, ONE);
   #elif defined(StringCrayStyle)
      fuplo = ATL_C2F_TransChar(cuplo);
      fdiag = ATL_C2F_TransChar(cdiag);
      F77TRTRI(fuplo, fdiag, &F77N, A, &F77lda, &info);
   #elif defined(StringStructVal)
      fuplo.len = 1;
      fdiag.len = 1;
      fuplo.cp = &cuplo;
      fdiag.cp = &cdiag;
      F77TRTRI(fuplo, fdiag, &F77N, A, &F77lda, &info);
   #elif defined(StringStructPtr)
      fuplo.len = 1;
      fdiag.len = 1;
      fuplo.cp = &cuplo;
      fdiag.cp = &cdiag;
      F77TRTRI(&fuplo, &fdiag, &F77N, A, &F77lda, &info);
   #else
      fprintf(stderr, "\n\nF77/C interface not defined!!\n\n");
      exit(-1);
   #endif
   return(info);
}
