#ifndef EppPerformance_utility_hh_INCLUDED
#define EppPerformance_utility_hh_INCLUDED

//Geof Sawaya, 2014, LBL & DOE
//This file has helper utilities -- currently
//this includes getCacheLineSize and 
//getNumProcElements -- both will only work
//in Unix environment . . .  can add support for
//Mac/Win later



namespace EppPerformance
{
  extern const int Perf_Thread_Count;
class Utility
{
public:
  static long getL1CacheLineSize();
  static int getProcElementCount();
  static void doDataDump();
};
}

#endif
