#include <cblas.h>
#include <clapack.h>
main(int nargs, char **args)
{
   double A[1]={1.0}, b[1]={1.0};
   int ipiv[1];
   clapack_dgesv(CblasColMajor, 1, 1, A, 1, ipiv, b, 1);
}
