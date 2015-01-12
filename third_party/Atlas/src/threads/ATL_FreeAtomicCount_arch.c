#include <stdlib.h>
void ATL_FreeAtomicCount(void *vp)
{
   free(vp);   /* could just do #define ATL_FreeAtomicCount free */
}              /* but do this so compiles same as _mut version */
