#include "atlas_misc.h"
#include "atlas_tlvl3.h"

/*
 * ====================================================================
 * This function will eventually be generated, but for now just written
 * ====================================================================
 */
int Mjoin(PATL,tNumGemmThreads)(ATL_CINT M, ATL_CINT N, ATL_CINT K)
/*
 * RETURNS : estimate of how many threads will be used to run the problem,
 *           assuming we will actually do threading (i.e. THRESH is exceeded)
 *           0 is returned if this number is 1 or less.
 */
{
   double flops;
   int np;
   if (M < 4 || N < 4 || K < 4)
      return(0);
   flops = ((2.0*M)*N)*K;
   np = flops / ATL_TGEMM_PERTHR_MF;
   np = (np <= 1) ? 0 : np;
   return(np >= ATL_NTHREADS ? ATL_NTHREADS : np);
}

int Mjoin(PATL,GemmWillThread)(ATL_CINT M, ATL_CINT N, ATL_CINT K)
/*
 * Returns: 0 if threshold FLOPS not achieved, rough # of threads used else
 */
{
   if (((2.0*M)*N)*K < ATL_TGEMM_THRESH_MF)
      return(0);
   return(Mjoin(PATL,tNumGemmThreads)(M,N,K));
}
