#include <stdio.h>
#include "atlas_buildinfo.h"
#include "atlas_cacheedge.h"

void ATL_buildinfo(void)
{
   printf("ATLAS version %s built by %s on %s:\n",
          ATL_VERS, ATL_UNAM, ATL_DATE);
   printf("   UNAME    : %s\n", ATL_SYSINFO);
   printf("   INSTFLG  : %s\n", ATL_INSTFLAGS);
   printf("   ARCHDEFS : %s\n", ATL_ARCHDEFS);
   printf("   F2CDEFS  : %s\n", ATL_F2CDEFS);
   #ifdef CacheEdge
      printf("   CACHEEDGE: %d\n", CacheEdge);
   #else
      printf("   CACHEEDGE: UNDEFINED\n");
   #endif
   printf("   %s : %s, version %s\n", "F77     ", ATL_F77, ATL_F77VERS);
   printf("   F77FLAGS : %s\n", ATL_F77FLAGS);
   printf("   %s : %s, version %s\n", "SMC     ", ATL_SMC, ATL_SMCVERS);
   printf("   SMCFLAGS : %s\n", ATL_SMCFLAGS);
   printf("   %s : %s, version %s\n", "SKC     ", ATL_SKC, ATL_SKCVERS);
   printf("   SKCFLAGS : %s\n", ATL_SKCFLAGS);
}
