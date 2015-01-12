#include <cblas.h>
#include <clapack.h>
#ifdef NoChange
   #define dgeqrf_ dgeqrf
#elif defined(UpCase)
   #define dgeqrf_ DGEQRF
#endif
main(int nargs, char **args)
{
   extern void dgeqrf_(F77_INTEGER*,F77_INTEGER*,double*,F77_INTEGER*,
                       double*,double*,F77_INTEGER*,F77_INTEGER*);
   double A[1]={1.0}, b[1]={1.0}, c[1];
   F77_INTEGER N=1, info;
   dgeqrf_(&N, &N, A, &N, b, c, &N, &info);
}
