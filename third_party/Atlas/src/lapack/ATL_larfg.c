/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009 Siju Samuel
 *
 * Code contributers : Siju Samuel, Anthony M. Castaldo, R. Clint Whaley
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
 * This is the C translation of the standard LAPACK Fortran routine:
 *     SUBROUTINE DLARFG( N, ALPHA, X, INCX, TAU )
 *
 * ATL_larfg.c :
 * void ATL_larfg( const int N, TYPE *ALPHA, TYPE *X, int INCX, TYPE *TAU)
 *        NOTE : ATL_larfg.c  will get compiled to 4 precisions
 *               single precision real,      double precision real
 *               single precision complex,   double precision complex
 *  Purpose
 *  =======
 *
 *  Real Precision
 *  --------------
 *
 *  ATL_larfg generates a real/complex  elementary reflector H of order n, such
 *  that
 *
 *        H * ( alpha ) = ( beta ),   H' * H = I.
 *            (   x   )   (   0  )
 *
 *  where alpha and beta are scalars, and x is an (n-1)-element real
 *  vector. H is represented in the form
 *
 *        H = I - tau * ( 1 ) * ( 1 v' ) ,                 (Real Precisions)
 *                      ( v )
 *
 *        H = I - tau * ( 1 ) * ( 1 conjugate(v)' ) ,     (Complex Precisions)
 *                      ( v )
 *
 *  where tau is a real/complex scalar and v is a real/complex (n-1)-element
 *  vector.
 *
 *  If the elements of x are all zero, then tau = 0 and H is taken to be
 *  the unit matrix.
 *
 *  Otherwise  1 <= tau <= 2.
 *
 *
 *  Arguments
 *  =========
 *
 *  N       (input) INTEGER
 *          The order of the elementary reflector.
 *
 *  ALPHA   (input/output)
 *          On entry, the value alpha.
 *          On exit, it is overwritten with the value beta.
 *
 *  X       (input/output)   array pointer, dimension
 *                         (1+(N-2)*abs(INCX))
 *          On entry, the vector x.
 *          On exit, it is overwritten with the vector v.
 *
 *  INCX    (input) INTEGER
 *          The increment between elements of X. INCX > 0.
 *
 *  TAU     (output)
 *          value of tau.
 */
#include "atlas_misc.h"
#include <math.h>
#include "cblas.h"
#include "atlas_lapack.h"
#include "atlas_lamch.h"

/*----------------------------------------------------------------------------*/
/* HighLevel Logic : * Real Precision                                         */
/* ---------------------------------------------------------------------------*/
/* On entry from ATL_geqr2(or any other variant lq, ql, rq) ,                 */
/* *Alpha points at A[i,i] and *X points  at A[i+1, i].                       */
/* If N==1, cblas_nrm2 returns zero. The norm is actually                     */
/* found in two parts; XNORM is all of the column except for A[i,i], and if   */
/* that is zero, we return TAU of zero (so H = I).                            */
/* Otherwise, we combine XNORM and A[i,i] into BETAp using lapy2. So BETAp    */
/* is the actual norm2 of A[i:m, i].                                          */
/* We set BETA to BETAp but with the opposite sign as A[i,i]. This is done    */
/* to ensure that TAU is in [1,2].                                            */
/* We set TAU to (BETA-A[i,i])/BETA, and scale A[i+1:m,i] by 1/(A[i,i]-BETA). */
/* Finally, we replace A[i,i] with BETA.                                      */
/* Note that |A[i,i]-BETA| > |A[i,i]|. Treat the column as X=[x1, x2, x3],    */
/* and assume x1 > 0. Then we return X' (prime, not transpose) as:            */
/* x1' = -||X||  (The element actually on the diagonal, and part of R.)       */
/* x2' = x2/(x1+||X||)                                                        */
/* x3' = x3/(x1+||X||)                                                        */
/* TAU = (-||X||-x1)/-||X|| = (x1+||X||)/||X||                                */
/*                                                                            */
/* This is NOT a textbook Householder reflection, because [1 v]^T does not    */
/* have a norm of 1. The purpose seems to be to save the first storage        */
/* location; the '1' is not stored anywhere. The choice of whether to make    */
/* A[i,i] either +/- ||X|| is strictly to control the range of TAU, but has   */
/* the added value of making the Householder matrix unique for any given A.   */
/* Note that |x1| < | (x1+||X||) |, i.e. the magnitude is always increased,   */
/* so the magnitude of xi/(x1+||X||) is always decreased. I'm not sure if     */
/* that is intended to reduce error or not.                                   */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/* Complex Precisions                                                         */
/*----------------------------------------------------------------------------*/
/* The  highlevel logic for Complex precision remains same as that of Real    */
/* precisions. The difference are                                             */
/*    BETAp is calculated using real part of A[i,i], imaginary part of A[i,i] */
/*    and XNORM using lapy3.                                                  */
/*                                                                            */
/*    ATL_ladiv is called to apply complex number devision before             */
/*    performing  the scaling operation for A[i+1:m,i] by 1/(A[i,i]-BETA).    */
/*                                                                            */
/*   NOTE :                                                                   */
/*    For Real precision and Complex precision, the codes are kept seperately */
/*    for clarity. ( Many code might be similar)                              */
/*----------------------------------------------------------------------------*/


void ATL_larfg(ATL_CINT N, TYPE *ALPHA, TYPE *X, ATL_CINT INCX, TYPE *TAU)
{

#ifdef TREAL
      TYPE ONE=1.0, ZERO=0.0, BETA, BETAp, RSAFMN, SAFMAX, XNORM;
      int    j, KNT;

      if (N < 1)
      {
         *TAU = ZERO;
         return;
      }

      XNORM = cblas_nrm2(N-1, X, INCX);     /* Get the norm2 .                */

      if (XNORM == ZERO)
      {
/*
 *        H  =  I
 */
         *TAU = ZERO;
      } else
      {
         BETAp = ATL_lapy2((*ALPHA), XNORM);/* Get sqrt(a^2+b^2)              */
         BETA = BETAp;                      /* Assume ALPHA < 0               */
         if ((*ALPHA) > 0) BETA = 0.-BETAp; /* Change if assumed wrong.       */

         if (BETAp < ATL_laSAFMIN)
         {
/*
 *           XNORM, BETA may be inaccurate; scale X and recompute them
 */
            RSAFMN = ONE / ATL_laSAFMIN;    /* Set a maximum                  */
            KNT = 0;

            while (BETAp < ATL_laSAFMIN)
            {
               KNT++;
               cblas_scal(N-1, RSAFMN, X, INCX);
               BETA *= RSAFMN;
               BETAp *= RSAFMN;
               *ALPHA *= RSAFMN;
            }
/*
 *          New BETA is at most 1, at least SAFMIN
 */
            XNORM = cblas_nrm2(N-1, X, INCX);
            BETA = ATL_lapy2((*ALPHA), XNORM);   /* Will always be positive   */
            if ((*ALPHA) > 0) BETA = -BETA; /* -SIGN(BETA, ALPHA)             */
            *TAU = (BETA-(*ALPHA)) / BETA;
            cblas_scal(N-1, ONE/((*ALPHA)-BETA), X, INCX);

/*
 *          If ALPHA is subnormal, it may lose relative accuracy
 */
            *ALPHA = BETA;
            for (j=0; j<KNT; j++)
            {
               (*ALPHA) *= ATL_laSAFMIN;
            }
         } else                             /* General case                   */
         {
            *TAU = (BETA-(*ALPHA)) / BETA;
            cblas_scal(N-1, ONE / ((*ALPHA)-BETA), X, INCX);
            *ALPHA = BETA;
         }
      }
      return;
/*
 *     End of  Real Precision ATL_larfg
 */
#else
/*
 *     Beginning of  Complex  Precision ATL_larfg
 */
      TYPE ONE=1.0, ZERO=0.0, BETA, BETAp, RSAFMN, SAFMAX, XNORM, ALPHI, ALPHR;
      TYPE ONEVAL[2] =  {ATL_rone, ATL_rzero};
      int j, KNT;

      if ( N < 0)
      {
/*
 *        H  =  I
 */
         *(TAU)  = 0.0;
         *(TAU + 1) = 0.0;
         return;
      }

      XNORM = cblas_nrm2(N-1, X, INCX);     /* Get the nrm2                   */

      ALPHR = *( ALPHA) ;
      ALPHI = *( ALPHA + 1) ;

      if ( (XNORM == ZERO) &&  (ALPHI == ZERO)  )
      {
/*
 *        H  =  I
 */
        *(TAU)  = 0.0;
        *(TAU + 1) = 0.0;
      }
      else
      {

         BETAp = ATL_lapy3(ALPHR, ALPHI, XNORM); /* Get sqrt(a^2 + b^2 + c^2) */
         BETA = BETAp;                      /* Assume ALPHA < 0               */
         if ( (*ALPHA) > 0) BETA = 0. - BETAp;   /* Change if assumed wrong   */

         RSAFMN = ONE / ATL_laSAFMIN ;

         if ( BETAp  <  ATL_laSAFMIN )
         {
/*
 *           XNORM, BETA may be inaccurate; scale X and recompute them
 */
            KNT = 0;
            while ( BETAp < ATL_laSAFMIN )
            {
               KNT++;
               #ifdef DCPLX
                  cblas_zdscal(N-1, RSAFMN, X, INCX);
               #else
                  cblas_csscal(N-1, RSAFMN, X, INCX);
               #endif
               BETA *= RSAFMN;
               BETAp *= RSAFMN;
               ALPHI = ALPHI*RSAFMN;
               ALPHR = ALPHR*RSAFMN;
            }

/*
 *           New BETA is at most 1, at least SAFMIN
 */
            XNORM = cblas_nrm2(N-1, X, INCX);
            *(ALPHA) = ALPHR;
            *(ALPHA + 1) = ALPHI;

            BETA = ATL_lapy3(ALPHR, ALPHI,
                                          XNORM);/* Will always be positive   */
            if (ALPHR > 0) BETA = -BETA;    /* -SIGN(BETA, ALPHR)             */
            *(TAU) = ( BETA-ALPHR ) / BETA ;
            *(TAU + 1) =  (-1.0 * ALPHI) / BETA ;
/*          Modify alpha   to alpha - beta,  which is equal to alphar -beta   */
            *(ALPHA) = *(ALPHA) - BETA;
/*          Perform complex division before scaling the X vector              */
            ATL_ladiv( ONEVAL,  ALPHA, ALPHA);   /* ALPHA will have the result*/
            cblas_scal(N-1, ALPHA, X, INCX);
/*
 *           If ALPHA is subnormal, it may lose relative accuracy
 */
            *(ALPHA) = BETA;                /* Real Part of alpha             */
            for (j=0; j<KNT; j++)
            {
               (*ALPHA) *= ATL_laSAFMIN;
            }
            *(ALPHA + 1) = 0.0;             /* Set Imaginary part to Zero     */
        }
        else                                /* BETA > SAFMIN                  */
        {

            *(TAU) = ( BETA-ALPHR ) / BETA ;
            *(TAU + 1) =  (-1.0 * ALPHI) / BETA ;

/*          Modify alpha   to alpha - beta, which is equal to alphar -beta    */
            *(ALPHA) = *(ALPHA) - BETA ;
/*          Perform complex division before scaling the X vector              */
            ATL_ladiv( ONEVAL,  ALPHA, ALPHA);
            cblas_scal(N-1, ALPHA, X, INCX);
            *(ALPHA) = BETA;                /* Real Part of alpha             */
            *(ALPHA + 1) = 0.0;             /* Set Imaginary part to Zero     */
           }
      }
      return;
#endif
}                                           /* END ATL_larfg                  */

