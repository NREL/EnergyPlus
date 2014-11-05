#ifndef ENERGYPLUS_LIB_ENERGYPLUSAPI_HPP
#define ENERGYPLUS_LIB_ENERGYPLUSAPI_HPP

#if _WIN32 || _MSC_VER
 #ifdef EnergyPlusLib_EXPORTS
  #define ENERGYPLUSLIB_API __declspec(dllexport)
 #else
  #define ENERGYPLUSLIB_API __declspec(dllimport)
 #endif
#else
 #define ENERGYPLUSLIB_API
#endif

#endif
