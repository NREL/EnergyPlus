#include <stdio.h>
#include <stdlib.h>
int main(int nargs, char **args)
{
   float *x, *y, *z, ans[4];
   void *vp;
   void do_vsum(float *, float *, float *);

   vp = malloc(4*3*sizeof(float) + 16);
   x = (float*) ( 16 + ((((size_t)(vp))>>4)<<4) );
   y = x + 4;
   z = y + 4;
   x[0] = 8.0;  x[1] = 4.0;  x[2] = 2.0;  x[3] = 1.0;
   y[0] = 16.0; y[1] = 32.0; y[2] = 64.0; y[3] = 128.0;
   ans[0] = x[0] + y[0];
   ans[1] = x[1] + y[1];
   ans[2] = x[2] + y[2];
   ans[3] = x[3] + y[3];
   do_vsum(z, x, y);
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
