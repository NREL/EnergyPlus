#ifndef EppPerformance_utility_hh_INCLUDED
#define EppPerformance_utility_hh_INCLUDED

//Geof Sawaya, 2014, LBL & DOE
// This is the routines for detecting data cache L1 size and number
// of physical processors.  It should be portable on all platforms with
// AMD and Intel processors.

namespace EppPerformance
{
  extern const int Perf_Thread_Count;
  extern const long L1_DCache_L_Size;
}

#endif
