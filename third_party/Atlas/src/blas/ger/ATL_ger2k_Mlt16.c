#include "atlas_misc.h"
#include "atlas_lvl2.h"

typedef void (*ger2k_t)
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, ATL_CINT incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda);

#ifdef TREAL

static void ATL_ger2k_Meq1
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq2
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq3
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq4
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq5
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq6
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq7
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b, x6a, x6b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
   x6b = -Xb[6*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
   x6a = -Xa[6*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
      A[6] += x6a * y0a + x6b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   y0a = alp_b;
   x6b = Xb[6*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   x6b = Xb[6*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   y0a = alp_a;
   x6a = Xa[6*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   x6a = Xa[6*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq8
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b, x6a, x6b, x7a, x7b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
   x6b = -Xb[6*incXb];
   x7b = -Xb[7*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
   x6a = -Xa[6*incXa];
   x7a = -Xa[7*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
      A[6] += x6a * y0a + x6b * y0b;
      A[7] += x7a * y0a + x7b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   y0a = alp_b;
   x6b = Xb[6*incXb] * y0a;
   y0a = alp_b;
   x7b = Xb[7*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   x6b = Xb[6*incXb];
   x7b = Xb[7*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   y0a = alp_a;
   x6a = Xa[6*incXa] * y0a;
   y0a = alp_a;
   x7a = Xa[7*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   x6a = Xa[6*incXa];
   x7a = Xa[7*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq9
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b, x6a, x6b, x7a, x7b, x8a, x8b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
   x6b = -Xb[6*incXb];
   x7b = -Xb[7*incXb];
   x8b = -Xb[8*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
   x6a = -Xa[6*incXa];
   x7a = -Xa[7*incXa];
   x8a = -Xa[8*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
      A[6] += x6a * y0a + x6b * y0b;
      A[7] += x7a * y0a + x7b * y0b;
      A[8] += x8a * y0a + x8b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   y0a = alp_b;
   x6b = Xb[6*incXb] * y0a;
   y0a = alp_b;
   x7b = Xb[7*incXb] * y0a;
   y0a = alp_b;
   x8b = Xb[8*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   x6b = Xb[6*incXb];
   x7b = Xb[7*incXb];
   x8b = Xb[8*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   y0a = alp_a;
   x6a = Xa[6*incXa] * y0a;
   y0a = alp_a;
   x7a = Xa[7*incXa] * y0a;
   y0a = alp_a;
   x8a = Xa[8*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   x6a = Xa[6*incXa];
   x7a = Xa[7*incXa];
   x8a = Xa[8*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq10
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b, x6a, x6b, x7a, x7b, x8a, x8b, x9a, x9b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
   x6b = -Xb[6*incXb];
   x7b = -Xb[7*incXb];
   x8b = -Xb[8*incXb];
   x9b = -Xb[9*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
   x6a = -Xa[6*incXa];
   x7a = -Xa[7*incXa];
   x8a = -Xa[8*incXa];
   x9a = -Xa[9*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
      A[6] += x6a * y0a + x6b * y0b;
      A[7] += x7a * y0a + x7b * y0b;
      A[8] += x8a * y0a + x8b * y0b;
      A[9] += x9a * y0a + x9b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   y0a = alp_b;
   x6b = Xb[6*incXb] * y0a;
   y0a = alp_b;
   x7b = Xb[7*incXb] * y0a;
   y0a = alp_b;
   x8b = Xb[8*incXb] * y0a;
   y0a = alp_b;
   x9b = Xb[9*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   x6b = Xb[6*incXb];
   x7b = Xb[7*incXb];
   x8b = Xb[8*incXb];
   x9b = Xb[9*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   y0a = alp_a;
   x6a = Xa[6*incXa] * y0a;
   y0a = alp_a;
   x7a = Xa[7*incXa] * y0a;
   y0a = alp_a;
   x8a = Xa[8*incXa] * y0a;
   y0a = alp_a;
   x9a = Xa[9*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   x6a = Xa[6*incXa];
   x7a = Xa[7*incXa];
   x8a = Xa[8*incXa];
   x9a = Xa[9*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq11
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b, x6a, x6b, x7a, x7b, x8a, x8b, x9a, x9b, x10a, x10b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
   x6b = -Xb[6*incXb];
   x7b = -Xb[7*incXb];
   x8b = -Xb[8*incXb];
   x9b = -Xb[9*incXb];
   x10b = -Xb[10*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
   x6a = -Xa[6*incXa];
   x7a = -Xa[7*incXa];
   x8a = -Xa[8*incXa];
   x9a = -Xa[9*incXa];
   x10a = -Xa[10*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
      A[6] += x6a * y0a + x6b * y0b;
      A[7] += x7a * y0a + x7b * y0b;
      A[8] += x8a * y0a + x8b * y0b;
      A[9] += x9a * y0a + x9b * y0b;
      A[10] += x10a * y0a + x10b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   y0a = alp_b;
   x6b = Xb[6*incXb] * y0a;
   y0a = alp_b;
   x7b = Xb[7*incXb] * y0a;
   y0a = alp_b;
   x8b = Xb[8*incXb] * y0a;
   y0a = alp_b;
   x9b = Xb[9*incXb] * y0a;
   y0a = alp_b;
   x10b = Xb[10*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   x6b = Xb[6*incXb];
   x7b = Xb[7*incXb];
   x8b = Xb[8*incXb];
   x9b = Xb[9*incXb];
   x10b = Xb[10*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   y0a = alp_a;
   x6a = Xa[6*incXa] * y0a;
   y0a = alp_a;
   x7a = Xa[7*incXa] * y0a;
   y0a = alp_a;
   x8a = Xa[8*incXa] * y0a;
   y0a = alp_a;
   x9a = Xa[9*incXa] * y0a;
   y0a = alp_a;
   x10a = Xa[10*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   x6a = Xa[6*incXa];
   x7a = Xa[7*incXa];
   x8a = Xa[8*incXa];
   x9a = Xa[9*incXa];
   x10a = Xa[10*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq12
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b, x6a, x6b, x7a, x7b, x8a, x8b, x9a, x9b, x10a, x10b;
   register TYPE x11a, x11b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
   x6b = -Xb[6*incXb];
   x7b = -Xb[7*incXb];
   x8b = -Xb[8*incXb];
   x9b = -Xb[9*incXb];
   x10b = -Xb[10*incXb];
   x11b = -Xb[11*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
   x6a = -Xa[6*incXa];
   x7a = -Xa[7*incXa];
   x8a = -Xa[8*incXa];
   x9a = -Xa[9*incXa];
   x10a = -Xa[10*incXa];
   x11a = -Xa[11*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
      A[6] += x6a * y0a + x6b * y0b;
      A[7] += x7a * y0a + x7b * y0b;
      A[8] += x8a * y0a + x8b * y0b;
      A[9] += x9a * y0a + x9b * y0b;
      A[10] += x10a * y0a + x10b * y0b;
      A[11] += x11a * y0a + x11b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   y0a = alp_b;
   x6b = Xb[6*incXb] * y0a;
   y0a = alp_b;
   x7b = Xb[7*incXb] * y0a;
   y0a = alp_b;
   x8b = Xb[8*incXb] * y0a;
   y0a = alp_b;
   x9b = Xb[9*incXb] * y0a;
   y0a = alp_b;
   x10b = Xb[10*incXb] * y0a;
   y0a = alp_b;
   x11b = Xb[11*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   x6b = Xb[6*incXb];
   x7b = Xb[7*incXb];
   x8b = Xb[8*incXb];
   x9b = Xb[9*incXb];
   x10b = Xb[10*incXb];
   x11b = Xb[11*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   y0a = alp_a;
   x6a = Xa[6*incXa] * y0a;
   y0a = alp_a;
   x7a = Xa[7*incXa] * y0a;
   y0a = alp_a;
   x8a = Xa[8*incXa] * y0a;
   y0a = alp_a;
   x9a = Xa[9*incXa] * y0a;
   y0a = alp_a;
   x10a = Xa[10*incXa] * y0a;
   y0a = alp_a;
   x11a = Xa[11*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   x6a = Xa[6*incXa];
   x7a = Xa[7*incXa];
   x8a = Xa[8*incXa];
   x9a = Xa[9*incXa];
   x10a = Xa[10*incXa];
   x11a = Xa[11*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq13
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b, x6a, x6b, x7a, x7b, x8a, x8b, x9a, x9b, x10a, x10b;
   register TYPE x11a, x11b, x12a, x12b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
   x6b = -Xb[6*incXb];
   x7b = -Xb[7*incXb];
   x8b = -Xb[8*incXb];
   x9b = -Xb[9*incXb];
   x10b = -Xb[10*incXb];
   x11b = -Xb[11*incXb];
   x12b = -Xb[12*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
   x6a = -Xa[6*incXa];
   x7a = -Xa[7*incXa];
   x8a = -Xa[8*incXa];
   x9a = -Xa[9*incXa];
   x10a = -Xa[10*incXa];
   x11a = -Xa[11*incXa];
   x12a = -Xa[12*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
      A[6] += x6a * y0a + x6b * y0b;
      A[7] += x7a * y0a + x7b * y0b;
      A[8] += x8a * y0a + x8b * y0b;
      A[9] += x9a * y0a + x9b * y0b;
      A[10] += x10a * y0a + x10b * y0b;
      A[11] += x11a * y0a + x11b * y0b;
      A[12] += x12a * y0a + x12b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   y0a = alp_b;
   x6b = Xb[6*incXb] * y0a;
   y0a = alp_b;
   x7b = Xb[7*incXb] * y0a;
   y0a = alp_b;
   x8b = Xb[8*incXb] * y0a;
   y0a = alp_b;
   x9b = Xb[9*incXb] * y0a;
   y0a = alp_b;
   x10b = Xb[10*incXb] * y0a;
   y0a = alp_b;
   x11b = Xb[11*incXb] * y0a;
   y0a = alp_b;
   x12b = Xb[12*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   x6b = Xb[6*incXb];
   x7b = Xb[7*incXb];
   x8b = Xb[8*incXb];
   x9b = Xb[9*incXb];
   x10b = Xb[10*incXb];
   x11b = Xb[11*incXb];
   x12b = Xb[12*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   y0a = alp_a;
   x6a = Xa[6*incXa] * y0a;
   y0a = alp_a;
   x7a = Xa[7*incXa] * y0a;
   y0a = alp_a;
   x8a = Xa[8*incXa] * y0a;
   y0a = alp_a;
   x9a = Xa[9*incXa] * y0a;
   y0a = alp_a;
   x10a = Xa[10*incXa] * y0a;
   y0a = alp_a;
   x11a = Xa[11*incXa] * y0a;
   y0a = alp_a;
   x12a = Xa[12*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   x6a = Xa[6*incXa];
   x7a = Xa[7*incXa];
   x8a = Xa[8*incXa];
   x9a = Xa[9*incXa];
   x10a = Xa[10*incXa];
   x11a = Xa[11*incXa];
   x12a = Xa[12*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq14
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b, x6a, x6b, x7a, x7b, x8a, x8b, x9a, x9b, x10a, x10b;
   register TYPE x11a, x11b, x12a, x12b, x13a, x13b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
   x6b = -Xb[6*incXb];
   x7b = -Xb[7*incXb];
   x8b = -Xb[8*incXb];
   x9b = -Xb[9*incXb];
   x10b = -Xb[10*incXb];
   x11b = -Xb[11*incXb];
   x12b = -Xb[12*incXb];
   x13b = -Xb[13*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
   x6a = -Xa[6*incXa];
   x7a = -Xa[7*incXa];
   x8a = -Xa[8*incXa];
   x9a = -Xa[9*incXa];
   x10a = -Xa[10*incXa];
   x11a = -Xa[11*incXa];
   x12a = -Xa[12*incXa];
   x13a = -Xa[13*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
      A[6] += x6a * y0a + x6b * y0b;
      A[7] += x7a * y0a + x7b * y0b;
      A[8] += x8a * y0a + x8b * y0b;
      A[9] += x9a * y0a + x9b * y0b;
      A[10] += x10a * y0a + x10b * y0b;
      A[11] += x11a * y0a + x11b * y0b;
      A[12] += x12a * y0a + x12b * y0b;
      A[13] += x13a * y0a + x13b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   y0a = alp_b;
   x6b = Xb[6*incXb] * y0a;
   y0a = alp_b;
   x7b = Xb[7*incXb] * y0a;
   y0a = alp_b;
   x8b = Xb[8*incXb] * y0a;
   y0a = alp_b;
   x9b = Xb[9*incXb] * y0a;
   y0a = alp_b;
   x10b = Xb[10*incXb] * y0a;
   y0a = alp_b;
   x11b = Xb[11*incXb] * y0a;
   y0a = alp_b;
   x12b = Xb[12*incXb] * y0a;
   y0a = alp_b;
   x13b = Xb[13*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   x6b = Xb[6*incXb];
   x7b = Xb[7*incXb];
   x8b = Xb[8*incXb];
   x9b = Xb[9*incXb];
   x10b = Xb[10*incXb];
   x11b = Xb[11*incXb];
   x12b = Xb[12*incXb];
   x13b = Xb[13*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   y0a = alp_a;
   x6a = Xa[6*incXa] * y0a;
   y0a = alp_a;
   x7a = Xa[7*incXa] * y0a;
   y0a = alp_a;
   x8a = Xa[8*incXa] * y0a;
   y0a = alp_a;
   x9a = Xa[9*incXa] * y0a;
   y0a = alp_a;
   x10a = Xa[10*incXa] * y0a;
   y0a = alp_a;
   x11a = Xa[11*incXa] * y0a;
   y0a = alp_a;
   x12a = Xa[12*incXa] * y0a;
   y0a = alp_a;
   x13a = Xa[13*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   x6a = Xa[6*incXa];
   x7a = Xa[7*incXa];
   x8a = Xa[8*incXa];
   x9a = Xa[9*incXa];
   x10a = Xa[10*incXa];
   x11a = Xa[11*incXa];
   x12a = Xa[12*incXa];
   x13a = Xa[13*incXa];
   goto XA_IS_LOADED;
}
static void ATL_ger2k_Meq15
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,
    const TYPE *Ya, const int incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, const int lda)
{
   register int j;
   register TYPE y0a, y0b, x0a, x0b, x1a, x1b, x2a, x2b, x3a, x3b, x4a, x4b;
   register TYPE x5a, x5b, x6a, x6b, x7a, x7b, x8a, x8b, x9a, x9b, x10a, x10b;
   register TYPE x11a, x11b, x12a, x12b, x13a, x13b, x14a, x14b;

   if (alp_b == ATL_rone) goto ALP_B_1;
   else if (alp_b != ATL_rnone) goto ALP_B_X;
   x0b = -Xb[0*incXb];
   x1b = -Xb[1*incXb];
   x2b = -Xb[2*incXb];
   x3b = -Xb[3*incXb];
   x4b = -Xb[4*incXb];
   x5b = -Xb[5*incXb];
   x6b = -Xb[6*incXb];
   x7b = -Xb[7*incXb];
   x8b = -Xb[8*incXb];
   x9b = -Xb[9*incXb];
   x10b = -Xb[10*incXb];
   x11b = -Xb[11*incXb];
   x12b = -Xb[12*incXb];
   x13b = -Xb[13*incXb];
   x14b = -Xb[14*incXb];
XB_IS_LOADED:
   if (alp_a == ATL_rone) goto ALP_A_1;
   else if (alp_a != ATL_rnone) goto ALP_A_X;
   x0a = -Xa[0*incXa];
   x1a = -Xa[1*incXa];
   x2a = -Xa[2*incXa];
   x3a = -Xa[3*incXa];
   x4a = -Xa[4*incXa];
   x5a = -Xa[5*incXa];
   x6a = -Xa[6*incXa];
   x7a = -Xa[7*incXa];
   x8a = -Xa[8*incXa];
   x9a = -Xa[9*incXa];
   x10a = -Xa[10*incXa];
   x11a = -Xa[11*incXa];
   x12a = -Xa[12*incXa];
   x13a = -Xa[13*incXa];
   x14a = -Xa[14*incXa];
XA_IS_LOADED:

   for (j=0; j < N; j++, A += lda)
   {
      y0a = Ya[incYa*j];
      y0b = Yb[incYb*j];
      A[0] += x0a * y0a + x0b * y0b;
      A[1] += x1a * y0a + x1b * y0b;
      A[2] += x2a * y0a + x2b * y0b;
      A[3] += x3a * y0a + x3b * y0b;
      A[4] += x4a * y0a + x4b * y0b;
      A[5] += x5a * y0a + x5b * y0b;
      A[6] += x6a * y0a + x6b * y0b;
      A[7] += x7a * y0a + x7b * y0b;
      A[8] += x8a * y0a + x8b * y0b;
      A[9] += x9a * y0a + x9b * y0b;
      A[10] += x10a * y0a + x10b * y0b;
      A[11] += x11a * y0a + x11b * y0b;
      A[12] += x12a * y0a + x12b * y0b;
      A[13] += x13a * y0a + x13b * y0b;
      A[14] += x14a * y0a + x14b * y0b;
   }
   return;

ALP_B_X:
   y0a = alp_b;
   x0b = Xb[0*incXb] * y0a;
   y0a = alp_b;
   x1b = Xb[1*incXb] * y0a;
   y0a = alp_b;
   x2b = Xb[2*incXb] * y0a;
   y0a = alp_b;
   x3b = Xb[3*incXb] * y0a;
   y0a = alp_b;
   x4b = Xb[4*incXb] * y0a;
   y0a = alp_b;
   x5b = Xb[5*incXb] * y0a;
   y0a = alp_b;
   x6b = Xb[6*incXb] * y0a;
   y0a = alp_b;
   x7b = Xb[7*incXb] * y0a;
   y0a = alp_b;
   x8b = Xb[8*incXb] * y0a;
   y0a = alp_b;
   x9b = Xb[9*incXb] * y0a;
   y0a = alp_b;
   x10b = Xb[10*incXb] * y0a;
   y0a = alp_b;
   x11b = Xb[11*incXb] * y0a;
   y0a = alp_b;
   x12b = Xb[12*incXb] * y0a;
   y0a = alp_b;
   x13b = Xb[13*incXb] * y0a;
   y0a = alp_b;
   x14b = Xb[14*incXb] * y0a;
   goto XB_IS_LOADED;
ALP_B_1:
   x0b = Xb[0*incXb];
   x1b = Xb[1*incXb];
   x2b = Xb[2*incXb];
   x3b = Xb[3*incXb];
   x4b = Xb[4*incXb];
   x5b = Xb[5*incXb];
   x6b = Xb[6*incXb];
   x7b = Xb[7*incXb];
   x8b = Xb[8*incXb];
   x9b = Xb[9*incXb];
   x10b = Xb[10*incXb];
   x11b = Xb[11*incXb];
   x12b = Xb[12*incXb];
   x13b = Xb[13*incXb];
   x14b = Xb[14*incXb];
   goto XB_IS_LOADED;
ALP_A_X:
   y0a = alp_a;
   x0a = Xa[0*incXa] * y0a;
   y0a = alp_a;
   x1a = Xa[1*incXa] * y0a;
   y0a = alp_a;
   x2a = Xa[2*incXa] * y0a;
   y0a = alp_a;
   x3a = Xa[3*incXa] * y0a;
   y0a = alp_a;
   x4a = Xa[4*incXa] * y0a;
   y0a = alp_a;
   x5a = Xa[5*incXa] * y0a;
   y0a = alp_a;
   x6a = Xa[6*incXa] * y0a;
   y0a = alp_a;
   x7a = Xa[7*incXa] * y0a;
   y0a = alp_a;
   x8a = Xa[8*incXa] * y0a;
   y0a = alp_a;
   x9a = Xa[9*incXa] * y0a;
   y0a = alp_a;
   x10a = Xa[10*incXa] * y0a;
   y0a = alp_a;
   x11a = Xa[11*incXa] * y0a;
   y0a = alp_a;
   x12a = Xa[12*incXa] * y0a;
   y0a = alp_a;
   x13a = Xa[13*incXa] * y0a;
   y0a = alp_a;
   x14a = Xa[14*incXa] * y0a;
   goto XA_IS_LOADED;
ALP_A_1:
   x0a = Xa[0*incXa];
   x1a = Xa[1*incXa];
   x2a = Xa[2*incXa];
   x3a = Xa[3*incXa];
   x4a = Xa[4*incXa];
   x5a = Xa[5*incXa];
   x6a = Xa[6*incXa];
   x7a = Xa[7*incXa];
   x8a = Xa[8*incXa];
   x9a = Xa[9*incXa];
   x10a = Xa[10*incXa];
   x11a = Xa[11*incXa];
   x12a = Xa[12*incXa];
   x13a = Xa[13*incXa];
   x14a = Xa[14*incXa];
   goto XA_IS_LOADED;
}

void Mjoin(PATL,ger2k_Mlt16)
   (const int M, const int N, const SCALAR alp_a, const TYPE *Xa,
    const int incXa, const TYPE *Ya, const int incYa, const SCALAR alp_b,
    const TYPE *Xb, const int incXb, const TYPE *Yb, const int incYb,
    TYPE *A, const int lda)
/*
 * ATLAS's normal GER2 kernels are optimized for long-M, and loop over rows in
 * the inner loop.  To avoid this killing us on short, wide matrices, have
 * special case code for M < 16.  This also allows kernels to assume M >= 16.
 */
{
   static ger2k_t ger2ks[15]={ATL_ger2k_Meq1, ATL_ger2k_Meq2, ATL_ger2k_Meq3,
                              ATL_ger2k_Meq4, ATL_ger2k_Meq5, ATL_ger2k_Meq6,
                              ATL_ger2k_Meq7, ATL_ger2k_Meq8, ATL_ger2k_Meq9,
                              ATL_ger2k_Meq10, ATL_ger2k_Meq11, ATL_ger2k_Meq12,
                              ATL_ger2k_Meq13, ATL_ger2k_Meq14,
                              ATL_ger2k_Meq15};
   if (M < 1 || N < 1 || (SCALAR_IS_ZERO(alp_a) && SCALAR_IS_ZERO(alp_b)))
      return;
   #ifdef ATL_GAS_x8664
   if (M > 14)
   #elif defined(ATL_GAS_x8632)
   if (M > 6)
   #else
   if (M > 15)
   #endif
   {
      Mjoin(PATL,ger2k_Nlt8)(M, N, alp_a, Xa, incXa, Ya, incYa, alp_b,
                             Xb, incXb, Yb, incYb, A, lda);
      return;
   }
   ger2ks[M-1](M, N, alp_a, Xa, incXa, Ya, incYa,
               alp_b, Xb, incXb, Yb, incYb, A, lda);
}

#else /* complex type */
#ifdef Conj_
void Mjoin(PATL,ger2ck_Mlt16)
#else
void Mjoin(PATL,ger2k_Mlt16)
#endif
   (ATL_CINT M, ATL_CINT N, const SCALAR alp_a, const TYPE *Xa, ATL_CINT incXa,     const TYPE *Ya, ATL_CINT incYa, const SCALAR alp_b, const TYPE *Xb,
    ATL_CINT incXb, const TYPE *Yb, ATL_CINT incYb, TYPE *A, ATL_CINT lda)
{
/*
 * For now, complex simply calls loop-based routine.
 */
#ifdef Conj_
   Mjoin(PATL,ger2ck_Nlt8)(M, N, alp_a, Xa, incXa, Ya, incYa,
                           alp_b, Xb, incXb, Yb, incYb, A, lda);
#else
   Mjoin(PATL,ger2k_Nlt8)(M, N, alp_a, Xa, incXa, Ya, incYa,
                          alp_b, Xb, incXb, Yb, incYb, A, lda);
#endif
}
#endif
