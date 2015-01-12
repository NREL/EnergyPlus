#include "atlas_misc.h"
#include "atlas_f77.h"
#include "atlas_lapack.h"
#include "atlas_lamch.h"

#if defined(NoChange)
   #define F77lamch Mjoin(PRE,lamch)
#elif defined(UpCase)
   #define F77lamch Mjoin(PREU,LAMCH)
#elif defined(Add__)
   #define F77lamch Mjoin(PRE,lamch_)
#else /* Add_ */
   #define F77lamch Mjoin(PRE,lamch_)
#endif

#if defined(StringSunStyle)
TYPE F77lamch(char *f77what, F77_INTEGER len)
#elif defined(StringStructPtr)
TYPE F77lamch(F77_CHAR *f77what)
#else
TYPE F77lamch(F77_CHAR f77what)
#endif
{
   char cwhat;

   cwhat = ATL_F2C_TransChar(f77what);
   switch(cwhat)
   {
   case 'r':
   case 'R':
      return(ATL_laROUND);
   case 's':
   case 'S':
      return(ATL_laSAFMIN);
   case 'o':
   case 'O':
      return(ATL_laOVERTHRESH);
   case 'u':
   case 'U':
      return(ATL_laUNDERTHRESH);
   case 'l':
   case 'L':
      return(ATL_laMAXEXP);
   case 'm':
   case 'M':
      return(ATL_laMINEXP);
   case 'n':
   case 'N':
      return(ATL_laMANTDIG);
   case 'p':
   case 'P':
      return(ATL_laPRECISION);
   case 'b':
   case 'B':
      return(ATL_laBASE);
   case 'e':
   case 'E':
      return(ATL_laEPSILON);
   default:
     return(0.0);
   }
   return(0.0);
}
