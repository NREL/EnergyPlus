#include <stdio.h>
#include <stdlib.h>
#ifdef NoChange
   #define flibchk_ flibchk
#elif defined(UpCase)
   #define flibchk_ FLIBCHK
#endif
int main(int nargs, char **args)
{
   void flibchk_(double*);
   double A[6];
   A[0] = A[1] = A[2] = A[3] = A[5] = 0.0;
   A[4] = 1.0;
   flibchk_(A);
   return(0);
}
