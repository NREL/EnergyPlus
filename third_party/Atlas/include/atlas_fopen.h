#ifndef ATLAS_FOPEN_H
#define ATLAS_FOPEN_H

static int FileExists(const char *path)
{
   FILE *fp;
   int iret=0;
   fp = fopen(path, "r");
   if (fp)
   {
      fclose(fp);
      iret = 1;
   }
   return(iret);
}

#ifdef ATL_FOPENDELAY
static FILE *ATL_fopen(const char *path, const char *mode)
/*
 * Overload fopen so it waits for NFS propogation upon first read failure
 */
{
   FILE *fp;
   char ln[256];

   fp = fopen(path, mode);
   if (fp == NULL)
   {
      if (*mode == 'r') /* give NFS time to produce file */
      {
         sprintf(ln, "make waitfile waitfile=%s\n", path);
         if (system(ln) == 0) fp = fopen(path, mode);
      }
   }
   return(fp);
}
#define fopen ATL_fopen
#endif

#endif
