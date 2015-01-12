#include "atlas_misc.h"

void *ATL_SetAtomicCount(int cnt)
{
   int *ip;

   ip = malloc(260); /* make false sharing unlikely by */
   ATL_assert(ip);   /* putting a 128 byte guard on */
   ip[32] = cnt;     /* both sides of counter */
   return((void*)ip);
}
