#if !defined(ATL_USEPTHREADS) || (defined(ATL_NCPU) && ATL_NCPU > 1)
#include "atlas_lapack.h"
#include "atlas_level3.h"

#ifdef ATL_USEPTHREADS
   #include "atlas_stGetNB_gelqf.h"
   #ifdef ATL_stGetNB_gelqf
      #define ATL_sGetNB_gelqf ATL_stGetNB_gelqf
   #endif
   #include "atlas_stGetNB_geqlf.h"
   #ifdef ATL_stGetNB_geqlf
      #define ATL_sGetNB_geqlf ATL_stGetNB_geqlf
   #endif
   #include "atlas_stGetNB_gerqf.h"
   #ifdef ATL_stGetNB_gerqf
      #define ATL_sGetNB_gerqf ATL_stGetNB_gerqf
   #endif
   #include "atlas_stGetNB_geqrf.h"
   #ifdef ATL_stGetNB_geqrf
      #define ATL_sGetNB_geqrf ATL_stGetNB_geqrf
   #endif
   #ifndef ATL_ilaenv
      #define ATL_ilaenv ATL_itlaenv
   #endif
#else
   #include "atlas_sGetNB_gelqf.h"
   #include "atlas_sGetNB_geqlf.h"
   #include "atlas_sGetNB_gerqf.h"
   #include "atlas_sGetNB_geqrf.h"
#endif
/*
 * See if any QR rout has been tuned, and if so, set it to default QR tune
 */
#if defined(ATL_sGetNB_geqrf) && !defined(ATL_sGetNB_QR)
   #define ATL_sGetNB_QR ATL_sGetNB_geqrf
#endif
#if defined(ATL_sGetNB_geqlf) && !defined(ATL_sGetNB_QR)
   #define ATL_sGetNB_QR ATL_sGetNB_geqlf
#endif
#if defined(ATL_sGetNB_gerqf) && !defined(ATL_sGetNB_QR)
   #define ATL_sGetNB_QR ATL_sGetNB_gerqf
#endif
#if defined(ATL_sGetNB_gelqf) && !defined(ATL_sGetNB_QR)
   #define ATL_sGetNB_QR ATL_sGetNB_gelqf
#endif
/*
 * Setup individual QR tunes if they haven't been indiviually tuned
 */
#ifndef ATL_sGetNB_geqrf
   #ifdef ATL_sGetNB_QR
      #define ATL_sGetNB_geqrf ATL_sGetNB_QR
   #else
      #define ATL_sGetNB_geqrf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_sGetNB_geqlf
   #ifdef ATL_sGetNB_QR
      #define ATL_sGetNB_geqlf ATL_sGetNB_QR
   #else
      #define ATL_sGetNB_geqlf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_sGetNB_gerqf
   #ifdef ATL_sGetNB_QR
      #define ATL_sGetNB_gerqf ATL_sGetNB_QR
   #else
      #define ATL_sGetNB_gerqf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_sGetNB_gelqf
   #ifdef ATL_sGetNB_QR
      #define ATL_sGetNB_gelqf ATL_sGetNB_QR
   #else
      #define ATL_sGetNB_gelqf(n_, nb_) (nb_) = 0;
   #endif
#endif

#ifdef ATL_USEPTHREADS
   #include "atlas_dtGetNB_gelqf.h"
   #ifdef ATL_dtGetNB_gelqf
      #define ATL_dGetNB_gelqf ATL_dtGetNB_gelqf
   #endif
   #include "atlas_dtGetNB_geqlf.h"
   #ifdef ATL_dtGetNB_geqlf
      #define ATL_dGetNB_geqlf ATL_dtGetNB_geqlf
   #endif
   #include "atlas_dtGetNB_gerqf.h"
   #ifdef ATL_dtGetNB_gerqf
      #define ATL_dGetNB_gerqf ATL_dtGetNB_gerqf
   #endif
   #include "atlas_dtGetNB_geqrf.h"
   #ifdef ATL_dtGetNB_geqrf
      #define ATL_dGetNB_geqrf ATL_dtGetNB_geqrf
   #endif
   #ifndef ATL_ilaenv
      #define ATL_ilaenv ATL_itlaenv
   #endif
#else
   #include "atlas_dGetNB_gelqf.h"
   #include "atlas_dGetNB_geqlf.h"
   #include "atlas_dGetNB_gerqf.h"
   #include "atlas_dGetNB_geqrf.h"
#endif
/*
 * See if any QR rout has been tuned, and if so, set it to default QR tune
 */
#if defined(ATL_dGetNB_geqrf) && !defined(ATL_dGetNB_QR)
   #define ATL_dGetNB_QR ATL_dGetNB_geqrf
#endif
#if defined(ATL_dGetNB_geqlf) && !defined(ATL_dGetNB_QR)
   #define ATL_dGetNB_QR ATL_dGetNB_geqlf
#endif
#if defined(ATL_dGetNB_gerqf) && !defined(ATL_dGetNB_QR)
   #define ATL_dGetNB_QR ATL_dGetNB_gerqf
#endif
#if defined(ATL_dGetNB_gelqf) && !defined(ATL_dGetNB_QR)
   #define ATL_dGetNB_QR ATL_dGetNB_gelqf
#endif
/*
 * Setup individual QR tunes if they haven't been indiviually tuned
 */
#ifndef ATL_dGetNB_geqrf
   #ifdef ATL_dGetNB_QR
      #define ATL_dGetNB_geqrf ATL_dGetNB_QR
   #else
      #define ATL_dGetNB_geqrf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_dGetNB_geqlf
   #ifdef ATL_dGetNB_QR
      #define ATL_dGetNB_geqlf ATL_dGetNB_QR
   #else
      #define ATL_dGetNB_geqlf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_dGetNB_gerqf
   #ifdef ATL_dGetNB_QR
      #define ATL_dGetNB_gerqf ATL_dGetNB_QR
   #else
      #define ATL_dGetNB_gerqf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_dGetNB_gelqf
   #ifdef ATL_dGetNB_QR
      #define ATL_dGetNB_gelqf ATL_dGetNB_QR
   #else
      #define ATL_dGetNB_gelqf(n_, nb_) (nb_) = 0;
   #endif
#endif

#ifdef ATL_USEPTHREADS
   #include "atlas_ctGetNB_gelqf.h"
   #ifdef ATL_ctGetNB_gelqf
      #define ATL_cGetNB_gelqf ATL_ctGetNB_gelqf
   #endif
   #include "atlas_ctGetNB_geqlf.h"
   #ifdef ATL_ctGetNB_geqlf
      #define ATL_cGetNB_geqlf ATL_ctGetNB_geqlf
   #endif
   #include "atlas_ctGetNB_gerqf.h"
   #ifdef ATL_ctGetNB_gerqf
      #define ATL_cGetNB_gerqf ATL_ctGetNB_gerqf
   #endif
   #include "atlas_ctGetNB_geqrf.h"
   #ifdef ATL_ctGetNB_geqrf
      #define ATL_cGetNB_geqrf ATL_ctGetNB_geqrf
   #endif
   #ifndef ATL_ilaenv
      #define ATL_ilaenv ATL_itlaenv
   #endif
#else
   #include "atlas_cGetNB_gelqf.h"
   #include "atlas_cGetNB_geqlf.h"
   #include "atlas_cGetNB_gerqf.h"
   #include "atlas_cGetNB_geqrf.h"
#endif
/*
 * See if any QR rout has been tuned, and if so, set it to default QR tune
 */
#if defined(ATL_cGetNB_geqrf) && !defined(ATL_cGetNB_QR)
   #define ATL_cGetNB_QR ATL_cGetNB_geqrf
#endif
#if defined(ATL_cGetNB_geqlf) && !defined(ATL_cGetNB_QR)
   #define ATL_cGetNB_QR ATL_cGetNB_geqlf
#endif
#if defined(ATL_cGetNB_gerqf) && !defined(ATL_cGetNB_QR)
   #define ATL_cGetNB_QR ATL_cGetNB_gerqf
#endif
#if defined(ATL_cGetNB_gelqf) && !defined(ATL_cGetNB_QR)
   #define ATL_cGetNB_QR ATL_cGetNB_gelqf
#endif
/*
 * Setup individual QR tunes if they haven't been indiviually tuned
 */
#ifndef ATL_cGetNB_geqrf
   #ifdef ATL_cGetNB_QR
      #define ATL_cGetNB_geqrf ATL_cGetNB_QR
   #else
      #define ATL_cGetNB_geqrf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_cGetNB_geqlf
   #ifdef ATL_cGetNB_QR
      #define ATL_cGetNB_geqlf ATL_cGetNB_QR
   #else
      #define ATL_cGetNB_geqlf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_cGetNB_gerqf
   #ifdef ATL_cGetNB_QR
      #define ATL_cGetNB_gerqf ATL_cGetNB_QR
   #else
      #define ATL_cGetNB_gerqf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_cGetNB_gelqf
   #ifdef ATL_cGetNB_QR
      #define ATL_cGetNB_gelqf ATL_cGetNB_QR
   #else
      #define ATL_cGetNB_gelqf(n_, nb_) (nb_) = 0;
   #endif
#endif

#ifdef ATL_USEPTHREADS
   #include "atlas_ztGetNB_gelqf.h"
   #ifdef ATL_ztGetNB_gelqf
      #define ATL_zGetNB_gelqf ATL_ztGetNB_gelqf
   #endif
   #include "atlas_ztGetNB_geqlf.h"
   #ifdef ATL_ztGetNB_geqlf
      #define ATL_zGetNB_geqlf ATL_ztGetNB_geqlf
   #endif
   #include "atlas_ztGetNB_gerqf.h"
   #ifdef ATL_ztGetNB_gerqf
      #define ATL_zGetNB_gerqf ATL_ztGetNB_gerqf
   #endif
   #include "atlas_ztGetNB_geqrf.h"
   #ifdef ATL_ztGetNB_geqrf
      #define ATL_zGetNB_geqrf ATL_ztGetNB_geqrf
   #endif
   #ifndef ATL_ilaenv
      #define ATL_ilaenv ATL_itlaenv
   #endif
#else
   #include "atlas_zGetNB_gelqf.h"
   #include "atlas_zGetNB_geqlf.h"
   #include "atlas_zGetNB_gerqf.h"
   #include "atlas_zGetNB_geqrf.h"
#endif
/*
 * See if any QR rout has been tuned, and if so, set it to default QR tune
 */
#if defined(ATL_zGetNB_geqrf) && !defined(ATL_zGetNB_QR)
   #define ATL_zGetNB_QR ATL_zGetNB_geqrf
#endif
#if defined(ATL_zGetNB_geqlf) && !defined(ATL_zGetNB_QR)
   #define ATL_zGetNB_QR ATL_zGetNB_geqlf
#endif
#if defined(ATL_zGetNB_gerqf) && !defined(ATL_zGetNB_QR)
   #define ATL_zGetNB_QR ATL_zGetNB_gerqf
#endif
#if defined(ATL_zGetNB_gelqf) && !defined(ATL_zGetNB_QR)
   #define ATL_zGetNB_QR ATL_zGetNB_gelqf
#endif
/*
 * Setup individual QR tunes if they haven't been indiviually tuned
 */
#ifndef ATL_zGetNB_geqrf
   #ifdef ATL_zGetNB_QR
      #define ATL_zGetNB_geqrf ATL_zGetNB_QR
   #else
      #define ATL_zGetNB_geqrf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_zGetNB_geqlf
   #ifdef ATL_zGetNB_QR
      #define ATL_zGetNB_geqlf ATL_zGetNB_QR
   #else
      #define ATL_zGetNB_geqlf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_zGetNB_gerqf
   #ifdef ATL_zGetNB_QR
      #define ATL_zGetNB_gerqf ATL_zGetNB_QR
   #else
      #define ATL_zGetNB_gerqf(n_, nb_) (nb_) = 0;
   #endif
#endif
#ifndef ATL_zGetNB_gelqf
   #ifdef ATL_zGetNB_QR
      #define ATL_zGetNB_gelqf ATL_zGetNB_QR
   #else
      #define ATL_zGetNB_gelqf(n_, nb_) (nb_) = 0;
   #endif
#endif

static int ATL_IEEECHK(int DOALL, float zero, float one)
/*
 * Direct translation of LAPACK/SRC/IEEECK into C
 * RETURN: 0 if arithmetic produces wrong answer, else 1
 */
{
   float nan1, nan2, nan3, nan4, nan5, nan6, neginf, negzro, newzro, posinf;

   posinf = one / zero;
   if (posinf <= one)
      return(0);
   neginf = -one / zero;
   if (neginf >= zero)
      return(0);
   negzro = one / (neginf+one);
   if (negzro != zero)
      return(0);
   neginf = one / negzro;
   if (neginf >= zero)
      return(0);
   newzro = negzro + zero;
   if (newzro != zero)
      return(0);
   posinf = one / newzro;
   if (posinf <= one)
      return(0);
   neginf = neginf*posinf;
   if (neginf >= zero)
      return(0);
   posinf = posinf*posinf;
   if (posinf <= one)
      return(0);
/*
 * Check NaN if all checks should be done
 */
   if (DOALL)
   {
      nan1 = posinf + neginf;
      nan2 = posinf / neginf;
      nan3 = posinf / posinf;
      nan4 = posinf * zero;
      nan5 = neginf * negzro;
      nan6 = nan5 * 0.0;
      if (nan1 == nan1)
         return(0);
      if (nan2 == nan2)
         return(0);
      if (nan3 == nan3)
         return(0);
      if (nan4 == nan4)
         return(0);
      if (nan5 == nan5)
         return(0);
      if (nan6 == nan6)
         return(0);
   }
   return(1);
}

static int ilg2Floor(unsigned int val)
{
   int i;

   for (i=30; i >= 0; i--)
      if ( ((1<<i) | val) == val)
         return(i);
   return(0);
}

int ATL_ilaenv(enum ATL_ISPEC ISPEC, enum ATL_LAROUT ROUT, unsigned int OPTS,
               int N1, int N2, int N3, int N4)
/*
 *  NOTE: the following comments only slightly modified from the original
 *        LAPACK/SRC/ilaenv.f
 *  Purpose
 *  =======
 *
 *  ILAENV is called from the LAPACK routines to choose problem-dependent
 *  parameters for the local environment.  See ISPEC for a description of
 *  the parameters.
 *
 *  This version is tuned only for those cases where the option is satisfied
 *  by a invocation of an optimized CPP macro provided by tuned header files;
 *  otherwise, it can almost certainly be further tuned by the user.
 *
 *  ILAENV returns an INTEGER
 *  if ILAENV >= 0: ILAENV returns the value of the parameter specified by ISPEC
 *  if ILAENV < 0:  if ILAENV = -k, the k-th argument had an illegal value.
 *
 *  Arguments
 *  =========
 *
 *  ISPEC   (input) enum ATL_ISPEC (defined in atlas_lapack.h)
 *          Specifies the parameter to be returned as the value of
 *          ILAENV.
 *     LAIS_MSQRXOVER=8:
 *     LAIS_OPT_NB=1   : the optimal blocksize; if this value is 1,
                         an unblocked algorithm will give the best performance.
 *     LAIS_MIN_NB=2   : the minimum block size for which the block routine
 *                       should be used; if the usable block size is less than
 *                       this value, an unblocked routine should be used.
 *     LAIS_NBXOVER=3  : the crossover point (in a block routine, for N less
 *                       than this value, an unblocked routine should be used)
 *                       i.e. LAIS_MIN_NB is for NB, but this is for the
 *                       unconstrained dimension (or MIN of them).
 *     LAIS_NEIGSHFT=4 : the number of shifts, used in the nonsymmetric
 *                       eigenvalue routines (DEPRECATED)
 *     LAIS_MINCSZ=5   : the minimum column dimension for blocking to be used;
 *                       rectangular blocks must have dimension at least k by m,
 *                       where k is given by ILAENV(2,...) and m by ILAENV(5,..)
 *     LAIS_SVDXOVER=6 : the crossover point for the SVD (when reducing an MxN
 *                       matrix to bidiagonal form, if max(M,N)/min(M,N) exceeds
 *                       this value, a QR factorization is used first to reduce
 *                       the matrix to a triangular form.)
 *     LAIS_NPROC=7    : the number of processors
 *     LAIS_MSQRXOVER=8: the crossover point for the multishift QR method
 *                       for nonsymmetric eigenvalue problems (DEPRECATED)
 *     LAIS_MAXDCSPSZ=9: maximum size of the subproblems at the bottom of the
 *                       computation tree in the divide-and-conquer algorithm
 *                       (used by xGELSD and xGESDD)
 *     LAIS_NTNAN=10   : ieee NaN arithmetic can be trusted not to trap
 *     LAIS_NTINF=11   : infinity arithmetic can be trusted not to trap
 *          12 <= ISPEC <= 16:
 *               xHSEQR or one of its subroutines,
 *               see IPARMQ for detailed explanation
 *
 *  ROUT    (input) enum ATL_LAROUT
 *          Enumerated type indicating the  LAPACK routine query is about
 *
 *  OPTS    (input) integer
 *          Bitmap of all options to routine, as indicated from ATL_LAFLG
 *          enum (defined in atlas_lapack.h), inlucing the data type/precision.
 *          Options are:
 *          LAUpper=1, LALower=2, LARight=4, LALeft=8, LAUnit=16, LANonunit=32,
 *          LASreal=(1<<28), LADreal=(1<<29), LAScplx=(1<<30), LADcplx=(1<<31).
 *
 *  N1      (input) INTEGER
 *  N2      (input) INTEGER
 *  N3      (input) INTEGER
 *  N4      (input) INTEGER
 *          Problem dimensions for the LAPACK routine ROUT; these may not all
 *          be required.
 *
 *  Further Details
 *  ===============
 *
 *  The following conventions have been used when calling ILAENV from the
 *  LAPACK routines:
 *  1)  OPTS is a concatenation of all of the options to the LAPACK ROUT
 *      even if they are not used in determining
 *      the value of the parameter specified by ISPEC.
 *  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
 *      that they appear in the argument list for NAME.  N1 is used
 *      first, N2 second, and so on, and unused problem dimensions are
 *      passed a value of -1.
 *  3)  The parameter value returned by ILAENV is checked for validity in
 *      the calling subroutine.  For example, ILAENV is used to retrieve
 *      the optimal blocksize for STRTRI as follows:
 *
 *      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
 *      IF( NB.LE.1 ) NB = MAX( 1, N )
 *
 *  =====================================================================
 */
{
   int nb, mindim, ns, nh;

   switch(ISPEC)
   {
   case LAIS_OPT_NB:
      nb = 0;
      if (N2 == -1)
         mindim = N1;
      else
      {
         mindim = Mmin(N1, N2);
         if (N3 > 0)
         {
            mindim = Mmin(mindim, N3);
            if (N4 > 0)
               mindim = Mmin(mindim, N4);
         }
      }
/*
 *    Present code treates ORMQR like GEQRF, even though ORMQR might want
 *    larger NB w/o QR2 dragging it down.  This is just to save install time.
 */
      if (ROUT & (LAgeqrf | LAormqr))
      {
         if (OPTS & LADreal)
         {
            if (OPTS & LARight)  /* R/L on right */
            {
               if (OPTS & LALower) /* QL */
               {
                  ATL_dGetNB_geqlf(mindim, nb);
               }
               else                /* QR */
               {
                  ATL_dGetNB_geqrf(mindim, nb);
               }
            }
            else if (OPTS & LALower) /* LQ */
            {
               ATL_dGetNB_gelqf(mindim, nb);
            }
            else /* RQ */
            {
               ATL_dGetNB_gerqf(mindim, nb);
            }
         }
         else if (OPTS & LASreal)
         {
            if (OPTS & LARight)  /* R/L on right */
            {
               if (OPTS & LALower) /* QL */
               {
                  ATL_sGetNB_geqlf(mindim, nb);
               }
               else                /* QR */
               {
                  ATL_sGetNB_geqrf(mindim, nb);
               }
            }
            else if (OPTS & LALower) /* LQ */
            {
               ATL_sGetNB_gelqf(mindim, nb);
            }
            else /* RQ */
            {
               ATL_sGetNB_gerqf(mindim, nb);
            }
         }
         else if (OPTS & LADcplx)
         {
            if (OPTS & LARight)  /* R/L on right */
            {
               if (OPTS & LALower) /* QL */
               {
                  ATL_zGetNB_geqlf(mindim, nb);
               }
               else                /* QR */
               {
                  ATL_zGetNB_geqrf(mindim, nb);
               }
            }
            else if (OPTS & LALower) /* LQ */
            {
               ATL_zGetNB_gelqf(mindim, nb);
            }
            else /* RQ */
            {
               ATL_zGetNB_gerqf(mindim, nb);
            }
         }
         else if (OPTS & LAScplx)
         {
            if (OPTS & LARight)  /* R/L on right */
            {
               if (OPTS & LALower) /* QL */
               {
                  ATL_cGetNB_geqlf(mindim, nb);
               }
               else                /* QR */
               {
                  ATL_cGetNB_geqrf(mindim, nb);
               }
            }
            else if (OPTS & LALower) /* LQ */
            {
               ATL_cGetNB_gelqf(mindim, nb);
            }
            else /* RQ */
            {
               ATL_cGetNB_gerqf(mindim, nb);
            }
         }
/*
 *       For ORMQR, do not except really small nb, since it doesn't have
 *       cost of QR2 dragging it down
 */
         if (ROUT & LAormqr)
         {
            if (OPTS & LADreal)
               ns = ATL_dGetNB();
            else if (OPTS & LASreal)
               ns = ATL_sGetNB();
            else if (OPTS & LAScplx)
               ns = ATL_cGetNB();
            else
               ns = ATL_zGetNB();
            if (nb < ns)
               nb = 0;
         }
      }
/*
 *    If we know nothing else, tell routine to use ATLAS's GEMM blocking factor,
 *    unless it is an unknown routine, in which case LAPACK returns NB=1
 */
      if (!nb)
      {
         if (OPTS & LADreal)
            nb = ATL_dGetNB();
         else if (OPTS & LASreal)
            nb = ATL_sGetNB();
         else if (OPTS & LADcplx)
            nb = ATL_zGetNB();
         else
            nb = ATL_cGetNB();
         if (mindim < 8)
            nb = 1;
         else if (nb < (mindim>>1))
            nb = (mindim>>1);
/*
 *       Routines that do extra flops based on NB need to constrain NB
 */
         if ((LAgeqrf | LAormqr | LAgehrd | LAgebrd |
              LAsytrd | LAhetrd | LArorgen | LAcungen) & ROUT)
         {
            if (OPTS & LADreal)
            {
               if (nb > 64)
                 nb = 60;
            }
            else if (OPTS & LASreal)
            {
               if (nb > 80)
                  nb = 80;
            }
            else
            {
               if (nb > 64)
                  nb = 40;
            }
         }
      }
      else if (ROUT & LAstebz)
         nb = 1;
      return(nb);
   case LAIS_MIN_NB:  /* changed from LAPACK to require 4 rather than 2 cols */
      nb = 4;    /* most GEMMs need at least one MU/NU of 4 */
      if (ROUT & LAsytrf)
         nb = 8;
      return(nb);
   case LAIS_NBXOVER:  /* unchanged from LAPACK */
      nb = 0;
      if (ROUT & (LAgeqrf | LAormqr | LAgehrd | LAgebrd))
         nb = 128;
      else if (ROUT & (LAsytrd | LAhetrd))
         nb = 32;
      else if (ROUT & (LArorgen | LAcungen))
         nb = 128;
      return(nb);
   case LAIS_NEIGSHFT:  /* unchanged from LAPACK */
      return(6);
   case LAIS_MINCSZ:   /* unchanged from LAPACK, says not used */
      return(2);
   case LAIS_SVDXOVER: /* unchanged from LAPACK */
      return((int)(Mmin(N1,N2)*1.6e0));
   case LAIS_NPROC:
      #ifdef ATL_NPROC
         return(ATL_NPROC);
      #else
         return(1);
      #endif
   case LAIS_MSQRXOVER:  /* unchanged from LAPACK */
      return(8);
   case LAIS_MAXDCSPSZ:  /* unchanged from LAPACK */
      return(25);
/*
 * These following two commands are swapped in lapack3.1.1, fixed here
 */
   case LAIS_NTNAN:     /* unchanged from LAPACK, except for fix */
      return(ATL_IEEECHK(1, 0.0, 1.0));
   case LAIS_NTINF:     /* unchanged from LAPACK, except for fix */
      return(ATL_IEEECHK(0, 0.0, 1.0));
/*
 * Cases 12-13 come from IPARMQ; unchanged from lapack :
 *   ILAENV = IPARMQ( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
 *   -> N=N1, ILO=N2, IHI=N3, LWORK=N4
 */
   case 13:  /* INWIN */
   case 15:  /* ISHFTS */
   case 16:  /* IACC22 */
      nh = N3 - N2 + 1;
      ns = 2;
      if (nh >= 6000)
         ns = 256;
      else if (nh >= 3000)
         ns = 128;
      else if (nh >= 590)
        ns = 64;
      else if (nh >= 150)
      {
/*       ns = nh / ((int)( log(nh)/log(2.0) )); */
         ns = nh / ilg2Floor(nh);
         ns = (10 >= ns) ? 10 : ns;
/*
 *       NS must always be even and >= 2 for all nh, but this is only nh
 *       where that is not known by assignment, so ensure it here
 */
         ns = (ns>>1)<<1;
         ns = (ns >= 2) ? ns : 2;
/*
         ns = ns - ns%2;
         ns = (ns >= 2) ? ns : 2;
 */
      }
      else if (nh >= 60)
         ns = 10;
      else if (nh >= 30)
         ns = 4;
      if (ISPEC == 16)
#if 1
         return((ns >= 14) ? 2 : 0);
#else
      {
         nh = 0;
         if (ns >= 14)   /* NOTE: IPARMQ's KACMIN=14 */
            nh = 1;
         if (ns >= 14)   /* NOTE IPARMQ's K22MIN=14 */
            nh = 2;
         return(nh);
      }
      else if (ISPEC == 13)
         return( (nh <= 500) ? ns : ((3*ns)>>1) );
#endif
      return(ns);
      break;
   case 12:  /* INMIN */
      return(75);
   case 14:  /* INIBL */
      return(14);
   default:
      exit(ISPEC);
   }
   return(0);
}

#endif  /* end protection compiling threaded ilaenv on serial machine */
