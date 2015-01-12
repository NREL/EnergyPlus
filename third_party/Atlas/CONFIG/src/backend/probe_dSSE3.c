#include <stdio.h>
#include <stdlib.h>
int main(int nargs, char **args)
{
   double *x, *y, *z, ans[2];
   void *vp;
   void do_vsum(double *z, double *x, double *y);

   vp = malloc(2*3*sizeof(double) + 16);
   x = (double*) ( 16 + ((((size_t)(vp))>>4)<<4) );
   y = x + 2;
   z = y + 2;
   x[0] = 1.0; x[1] = 2.0;
   y[0] = 4.0; y[1] = 8.0;
   ans[0] = x[0] + x[1];
   ans[1] = y[0] + y[1];
   do_vsum(z, x, y);   /* z[0]=x[0]+x[1], z[1]=y[0]+y[1]; */
   if (z[0] != ans[0] || z[1] != ans[1])
   {
      fprintf(stderr, "wanted={%.2f,%.2f}, got={%.2f,%.2f}\n",
              ans[0], ans[1], z[0], z[1]);
      printf("FAILURE\n");
      free(vp);
      exit(1);
   }
   printf("SUCCESS\n");
   free(vp);
   return(0);
}
