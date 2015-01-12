#include <stdio.h>
#include <stdlib.h>
int main(int nargs, char **args)
{
   int i;
   int c2cslave(char, int, double, float);
   i = c2cslave('y', 2, 2.0, 3.0);
   if (i != -2)
   {
      printf("FAILURE\n");
      exit(1);
   }
   printf("SUCCESS\n");
   return(0);
}
