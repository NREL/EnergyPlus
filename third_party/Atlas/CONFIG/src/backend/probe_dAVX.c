#include <stdio.h>
#include <stdlib.h>
int main(int nargs, char **args)
{
   double *x, *y, *z, ans[4];
   void *vp;
   void do_vmacc(double *z, double *x, double *y);
   int i;

   vp = malloc(4*3*sizeof(double) + 32);
   x = (double*) ( 32 + ((((size_t)(vp))>>5)<<5) );
   y = x + 4;
   z = y + 4;
   x[0] = 1.0; x[1] = 2.0; x[2] = 4.0; x[3] = 8.0;
   y[0] = -4.0; y[1] = -8.0; y[2] = -16.0; y[3] = -32.0;
   z[0] = 0.0; z[1] = 1.0; z[2] = -1.0; z[3] = -4.0;
   for (i=0; i < 4; i++)
      ans[i] = z[i] + x[i] * y[i];
   do_vmacc(z, x, y);   /* z += x * y */
   if (z[0] != ans[0] || z[1] != ans[1] || z[2] != ans[2] || z[3] != ans[3])
   {
      fprintf(stderr,
              "wanted={%.2f,%.2f,%.2f,%.2f}, got={%.2f,%.2f,%.2f,%.2f}\n",
              ans[0], ans[1], ans[2], ans[3], z[0], z[1], z[2], z[3]);
      printf("FAILURE\n");
      free(vp);
      exit(1);
   }
   printf("SUCCESS\n");
   free(vp);
   return(0);
}
