#include "atlas_lapack.h"
#include "atlas_lamch.h"

#ifdef TREAL
TYPE ATL_lamch(char what)
{
   switch(what)
   {
   case 'r':
   case 'R':
      return(Mjoin(PATL,laROUND));
   case 's':
   case 'S':
      return(Mjoin(PATL,laSAFMIN));
   case 'o':
   case 'O':
      return(Mjoin(PATL,laOVERTHRESH));
   case 'u':
   case 'U':
      return(Mjoin(PATL,laUNDERTHRESH));
   case 'l':
   case 'L':
      return(Mjoin(PATL,laMAXEXP));
   case 'm':
   case 'M':
      return(Mjoin(PATL,laMINEXP));
   case 'n':
   case 'N':
      return(Mjoin(PATL,laMANTDIG));
   case 'p':
   case 'P':
      return(Mjoin(PATL,laPRECISION));
   case 'b':
   case 'B':
      return(Mjoin(PATL,laBASE));
   case 'e':
   case 'E':
      return(Mjoin(PATL,laEPSILON));
   default:
      return(0.0);
   }
   return(0.0);
}
#endif
