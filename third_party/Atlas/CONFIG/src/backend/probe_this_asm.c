#include <stdio.h>
#include <stdlib.h>

int main(int nargs, char **args)
{
   int asm_probe(int i);
   int i;
   i = asm_probe(7);
   if (i != 21)
   {
      fprintf(stdout, "FAILURE, i=%d!!\n", i);
      fprintf(stderr, "FAILURE, i=%d!!\n", i);
      exit(-1);
   }
   fprintf(stdout, "SUCCESS\n");
   return(0);
}
