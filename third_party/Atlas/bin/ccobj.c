#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#define Mstr2(m) # m
#define Mstr(m) Mstr2(m)
void PrintUsage(char *nam)
{
   fprintf(stderr, "USAGE: %s -DBETA[0,1,X,XI0] -o <outfile> --name <name> --objdir <objdir>\n", nam);
   exit(-1);
}
void GetFlags(int nargs, char **args, char *outfile, char *objdir, char *name,
              char **beta, char **conj, char *OS, char *ARCH)
{
   int i, j;
   char *sp;

   *beta = "1";
   *conj = "";
   objdir[0] = name[0] = outfile[0] = ARCH[0] = OS[0] = '\0';
   #ifdef TOPDIR
      strcpy(objdir, Mstr(TOPDIR));
      strcat(objdir, "/tune/blas/gemm/CASES/objs");
   #endif
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] == '-')
      {
         if (args[i][1] == 'o' && args[i][2] == '\0')
            strcpy(outfile, args[++i]);
         else if (!strcmp(args[i], "--name")) strcpy(name, args[++i]);
         else if (!strcmp(args[i], "--objdir")) strcpy(objdir, args[++i]);
         else if (args[i][1] == 'D') /* macro definitions */
         {
            if (!strncmp(args[i], "-DBETA", 6))
            {
               if (args[i][6] == '1') *beta = "1";
               else if (args[i][6] == '0') *beta = "0";
               else if (args[i][6] == 'X')
               {
                  if (args[i][7] == 'I') *beta = "Xi0";
                  else *beta = "X";
               }
            }
            else if (!strncmp(args[i], "-DATL_BETA=", 11)) /* alternate form */
            {
               if (args[i][11] == '0') *beta = "0";
               else if (args[i][11] == '1') *beta = "1";
               else if (args[i][11] == 'X')
               {
                  if (args[i][12] == 'I') *beta = "Xi0";
                  else *beta = "X";
               }
            }
            else if (!strncmp(args[i], "-DConj_", 7)) *conj="c";
            else if (!strncmp(args[i], "-DATL_ARCH_", 11))
               strcpy(ARCH, args[i]+11);
            else if (!strncmp(args[i], "-DATL_OS_", 9)) strcpy(OS, args[i]+9);
         }
      }
      else if (strstr(args[i], ".c"))  /* is the file being "compiled" */
      {
         if (outfile[0] == '\0') /* ignore if we have explicit -o */
         {
            for(j=(int)(strstr(args[i], ".c") - args[i]); j; j--)
            {
                if (args[i][j] == '/' || args[i][j] == '\\')
                {
                   if (j) j--;
                   break;
                }
            }
            strcpy(outfile, args[i]+j);
            sp = strstr(outfile, ".c");
            sp[1] = 'o';
         }
      }
   }
   if (objdir[0] == '\0' || name[0] == '\0' || *beta == '\0' ||
       outfile[0] == '\0')
   {
      fprintf(stderr, "objdir='%s', name='%s', beta=%c, outfile='%s'\n",
              objdir, name, *beta, outfile);
      PrintUsage(args[0]);
   }
   for (i=0; OS[i]; i++) OS[i] = tolower(OS[i]);
   for (i=0; ARCH[i]; i++) ARCH[i] = tolower(ARCH[i]);
}

void OSSysHack(char *name, char *OS, char *ARCH)
{
   if (strstr(name, "julian")) /* julian's Athlon kernel */
   {
      if (ARCH[0]) if (!strstr(ARCH, "athlon")) exit(-1); /* fake failure */
      if (strstr(OS, "win")) strcat(name, "_win");
   }
}
int main(int nargs, char **args)
/*
 * This guy fakes compilation by yanking .o out of
 *    ATLAS/tune/blas/gemm/CASES/objs (objdir)
 * and sticking it in local dir.  Given -D[BETAX,BETA0,BETA1] indicating the
 * beta case, and --name <name> indicating the base name, it copies the file
 *    ATLAS/tune/blas/gemm/CASES/objs/<name>_b[X,0,1].o
 * to the output file.  The output file can be selected by -o <outfile>, and
 * defaults to the name of the argument that ends in .c
 * This is a kludge of such magnitude that I'm almost proud to have written it.
 * To add to the kludge, it now grabs OS and ARCH from the arg list if they
 * are supplied.  Use OS to determine if we need windows version, and suffix
 * --name appropraitely, and use ARCH to see if we want to pretend to have
 * a compiler failure to avoid timing kernels that won't work for this OS.
 */
{
   char ln[1024], outfile[512], objdir[512], name[512], OS[128], ARCH[256];
   char *beta, *conj;
   GetFlags(nargs, args, outfile, objdir, name, &beta, &conj, OS, ARCH);
   OSSysHack(name, OS, ARCH);
   sprintf(ln, "cp -f %s/%s%s_b%s.o %s\n", objdir, name, conj, beta, outfile);

   fprintf(stderr, "%s", ln);
   return(system(ln));
}
