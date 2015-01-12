#include "atlas_misc.h"
#include "atlas_threads.h"
#ifndef ATL_WINTHREADS
   #include <sched.h>
#endif
void ATL_thread_yield(void)
{
   #ifdef ATL_WINTHREADS
      Sleep(0);
   #else
      sched_yield();
   #endif
}
