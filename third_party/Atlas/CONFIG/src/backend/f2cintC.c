#include <stdio.h>
#include <stdlib.h>
#if defined(Add_) || defined(Add__)
   #define f2cintc f2cintc_
#elif defined(UpCase)
   #define f2cintc F2CINTC
#endif
void f2cintc(void *vp, double *d)
{
   int *ip=vp;
   long *lp=vp;
   long long *llp=vp;
   short *sp=vp;

   *d = 0.0;
   if ( (sizeof(long) != sizeof(int)) && (*lp == 1) )
      *d = 2.0;
   else if (*ip == 1)  *d = 1.0;
   else if (*lp == 1)  *d = 2.0;
   else if (*llp == 1) *d = 3.0;
   else if (*sp == 1)  *d = 4.0;
}
