#ifndef __PLATFORM_CONFIG__
#define __PLATFORM_CONFIG__

// Due to problems with the NLOPT config script on windows, we've
// set up a file which will use our correctly windows config script
// Or, in the event of Linux/Mac, use a generated config.h file
// Modified 3/25/2019
#ifdef _WIN32
#include "config-windows.h"
#else
#include "config.h"
#endif 

#endif
