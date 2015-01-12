#include "atlconf.h"

int probe_name(char *targarg, int verb, char **usrcomps)
{
   char *frm = "make IRunF2C_name %s F77=\"%s\" F77FLAGS=\"%s\" CC=\"%s\" CCFLAGS=\"%s\" | fgrep 'F2C name'";
   char *cmnd, *res;
   enum F2CNAME f2cname = f2c_NamErr;
   int i;

   i = 1 + strlen(frm) + strlen(targarg) + strlen(usrcomps[F77_]) +
       strlen(usrcomps[F77_+NCOMP]) + strlen(usrcomps[ICC_]) +
       strlen(usrcomps[ICC_+NCOMP]);
   cmnd = malloc(i*sizeof(char));
   assert(cmnd);
   sprintf(cmnd, frm, targarg, usrcomps[F77_], usrcomps[F77_+NCOMP],
           usrcomps[ICC_], usrcomps[ICC_+NCOMP]);
   if (verb > 1)
      fprintf(stderr, "cmnd = '%s'\n", cmnd);
   res = atlsys_1L(NULL, cmnd, verb, 0);
   free(cmnd);
   if (res)
   {
      if (verb > 1)
         fprintf(stderr, "res = '%s'\n", res);
      if (strstr(res, "Add__"))
         f2cname = f2c_Add__;
      else if (strstr(res, "Add_"))
         f2cname = f2c_Add_;
      else if (strstr(res, "NoChange"))
         f2cname = f2c_NoChange;
      else if (strstr(res, "UpCase"))
         f2cname = f2c_UpCase;
      free(res);
   }
   if (verb)
      printf("F2C Name Decoration = %s\n", f2c_namestr[f2cname]);
   return(f2cname);
}

int probe_int(char *targarg, int verb, char **usrcomps, int f2cname)
{
   char *frm="make IRunF2C_int %s F77=\"%s\" F77FLAGS=\"%s\" CC=\"%s\" CCFLAGS=\"-D%s %s\" | fgrep 'F2C int'";
   char *cmnd, *res;
   enum F2CINT f2c_int = f2c_IntErr;
   int i;
   i = 1 + strlen(frm) + strlen(targarg) + strlen(usrcomps[F77_])
       + strlen(usrcomps[F77_+NCOMP]) + strlen(usrcomps[ICC_])
       + strlen(f2c_namestr[f2cname]) + strlen(usrcomps[ICC_+NCOMP]);
   cmnd = malloc(i*sizeof(char));
   assert(cmnd);
   sprintf(cmnd, frm, targarg, usrcomps[F77_], usrcomps[F77_+NCOMP],
           usrcomps[ICC_], f2c_namestr[f2cname], usrcomps[ICC_+NCOMP]);
   if (verb > 1)
      fprintf(stderr, "cmnd = '%s'\n", cmnd);
   res = atlsys_1L(NULL, cmnd, verb, 0);
   free(cmnd);
   if (res)
   {
      if (verb > 1)
         fprintf(stderr, "res = '%s'\n", res);
      if (strstr(res, " C int"))
         f2c_int = FintCint;
      else if (strstr(res, " C long long"))
         f2c_int = FintClonglong;
      else if (strstr(res, " C long"))
         f2c_int = FintClong;
      else if (strstr(res, " C short"))
         f2c_int = FintCshort;
      free(res);
   }
   if (verb)
      printf("F2C int = %s\n", f2c_intstr[f2c_int]);
   return(f2c_int);
}

int probe_str(char *targarg, int verb, char **usrcomps, int f2cname, int f2cint)
{
   char *frm = "make IRunF2C_str %s F77=\"%s\" F77FLAGS=\"%s\" CC=\"%s\" CCFLAGS=\"-D%s -D%s -DString%s %s\" | fgrep 'F2C string'";
   enum F2CSTRING f2cstr = f2c_StrErr;
   int i, len;

   len = 1 + strlen(frm) + strlen(targarg) + strlen(usrcomps[F77_]) +
        strlen(usrcomps[F77_+NCOMP]) + strlen(usrcomps[ICC_]) +
        strlen(f2c_namestr[f2cname]) + strlen(f2c_intstr[f2cint]) +
        strlen(usrcomps[ICC_+NCOMP]);
   for (i=1; i < 5; i++)
   {
      char *cmnd, *res;
      int k;
      k = len + strlen(f2c_strstr[i]);
      cmnd = malloc(k*sizeof(char));
      assert(cmnd);
      sprintf(cmnd, frm, targarg, usrcomps[F77_], usrcomps[F77_+NCOMP],
              usrcomps[ICC_], f2c_namestr[f2cname], f2c_intstr[f2cint],
              f2c_strstr[i], usrcomps[ICC_+NCOMP]);
      if (verb > 1)
         fprintf(stderr, "cmnd = '%s'\n", cmnd);
      res = atlsys_1L(NULL, cmnd, verb, 0);
      free(cmnd);
      if (res)
      {
         if (verb > 1)
            fprintf(stderr, "res = '%s'\n", res);
         free(res);
         f2cstr = i;
         break;
      }
   }
   if (verb)
      printf("F2C string = %s\n", f2c_strstr[f2cstr]);
   return(f2cstr);
}

void PrintUsage(char *name, int iarg, char *arg)
{
   fprintf(stderr, "\nERROR around arg %d (%s).\n", iarg,
           arg ? arg : "unknown");
   fprintf(stderr, "USAGE: %s [flags] where flags are:\n", name);
   fprintf(stderr, "   -v <verb> : verbosity level\n");
   fprintf(stderr, "   -C [xc,ic,if,sk,dk,sm,dm,al,ac] <compiler>\n");
   fprintf(stderr, "   -F [xc,ic,if,sk,dk,sm,dm,al,ac,gc] '<comp flags>'\n");
   fprintf(stderr,    /* HERE */
           "   -Fa [xc,ic,if,sk,dk,sm,dm,al,ac,gc] '<comp flags to append>'\n");
   fprintf(stderr, "        al: append flags to all compilers\n");
   fprintf(stderr, "        ac: append flags to all C compilers\n");
   fprintf(stderr,
      "   -T <targ> : ssh target for cross-compilation (probably broken)\n");
   fprintf(stderr,
      "NOTE: enum #s can be found by : make xprint_enums ; ./xprint_enums\n");
   exit(iarg);
}

void GetFlags(int nargs,                /* nargs as passed into main */
              char **args,              /* args as passed into main */
              int *verb,                /* verbosity setting */
              char **comps,
              char **targ             /* mach to ssh to*/
             )
{
   int i, k, k0, kn, DoInt;
   char *sp, *sp0;

   *verb = 0;
   *targ = NULL;
   for (k=0; k < NCOMP*3; k++)
      comps[k] = NULL;

   *verb = 0;
   for (i=1; i < nargs; i++)
   {
      if (args[i][0] != '-')
         PrintUsage(args[0], i, args[i]);
      switch(args[i][1])
      {
      case 'v':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *verb = atoi(args[i]);
         break;
      case 'T':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         *targ = args[i];
         break;
      case 'C':
      case 'F':
         if (++i >= nargs)
            PrintUsage(args[0], i, "out of arguments");
         sp = args[i];
         k = -1;
         if (*sp == 'i' && sp[1] == 'c') k = ICC_;
         else if (*sp == 'g' && sp[1] == 'c') k = GCC_;
         else if (*sp == 'i' && sp[1] == 'f') k = F77_;
         else if (*sp == 's' && sp[1] == 'k') k = SKC_;
         else if (*sp == 'd' && sp[1] == 'k') k = DKC_;
         else if (*sp == 's' && sp[1] == 'm') k = SMC_;
         else if (*sp == 'd' && sp[1] == 'm') k = DMC_;
         else if (*sp == 'x' && sp[1] == 'c') k = XCC_;
         if (*sp == 'a' && (sp[1] == 'l' || sp[1] == 'c'))
         {  /* only appended flags can be applied to all compilers */
            const int SKIPGCC=(sp[2] != 'g'), SKIPF=(sp[1] == 'c');
            if (args[i-1][1] == 'F')
            {
               if (args[i-1][2] == 'a')
               {
                  k0 = NCOMP+NCOMP;
                  kn = k0 + NCOMP;
               }
               else
               {
                  k0 = NCOMP;
                  kn = NCOMP+NCOMP;
               }
            }
            else
            {
               k0 = 0;
               kn = NCOMP;
            }
            if (++i >= nargs)
               PrintUsage(args[0], i, "out of arguments");
            for (k=k0; k < kn; k++)
               if ((!SKIPF || k-k0 != F77_) && (!SKIPGCC || k-k0 != GCC_))
                  comps[k] = args[i];
         }
         else
         {
            if (k < 0) PrintUsage(args[0], i, args[i]);
            if (args[i-1][1] == 'F')
            {
               k += NCOMP;
               if (args[i-1][2] == 'a')
                  k += NCOMP;
            }
            if (++i >= nargs)
               PrintUsage(args[0], i, "out of arguments");
            comps[k] = args[i];
         }
         break;
      default:
         PrintUsage(args[0], i, args[i]);
      }
   }
/*
 * allocate these strings ourselves so we can free them later if necessary
 */
   for (i=0; i < 3*NCOMP; i++)
   {
      if (comps[i])
      {
         if (!strcmp(comps[i], "default"))
            comps[i] = NULL;
         else
         {
            sp = malloc(sizeof(char)*(strlen(comps[i])+1));
            strcpy(sp, comps[i]);
            comps[i] = sp;
         }
      }
   }
}
int main (int nargs, char **args)
/*
 * This probe discovers the details of how fortran should call C for the
 * given compilers.  In particular, it discovers:
 *    (1) Name decoration C rout should do to be callable from fortran
 *    (2) What intergral type F77 integer corresponds to
 *    (3) How fortran strings are passed
 */
{
   int verb;
   int f2cname, f2cint, f2cstr;
   char *usrcomps[3*NCOMP];
   char *targ, *targarg;
   int i, ierr = 0;

   GetFlags(nargs, args, &verb, usrcomps, &targ);
   if (targ)
   {
      targarg = malloc(sizeof(char)*(strlen(targ)+24));
      assert(targarg);
      sprintf(targarg, "atlrun=atlas_runX targ=%s", targ);
   }
   else
      targarg = "";
   f2cname = probe_name(targarg, verb, usrcomps);
   if (f2cname)
   {
      f2cint = probe_int(targarg, verb, usrcomps, f2cname);
      f2cstr = probe_str(targarg, verb, usrcomps, f2cname, f2cint);
      printf("F2C=(%d,%d,%d)\n", f2cname, f2cint, f2cstr);
   }
   else
   {
      ierr = 1;
      if (verb)
         fprintf(stderr, "Cannot determine f2cname, quitting f2c probe!\n");
   }
   if (targ)
      free(targarg);
   for (i=0; i < 3*NCOMP; i++)
      if (usrcomps[i])
         free(usrcomps[i]);
   return(ierr);
}
