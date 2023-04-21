/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
 */

// This work was supported by the Assistant Secretary for Energy Efficiency 
// and Renewable Energy, Office of Building Technologies, 
// Building Systems and Materials Division of the 
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

/*
NOTICE: The Government is granted for itself and others acting on its behalf 
a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce, 
prepare derivative works, and perform publicly and display publicly. 
Beginning five (5) years after (date permission to assert copyright was obtained),
subject to two possible five year renewals, the Government is granted for itself 
and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
license in this data to reproduce, prepare derivative works, distribute copies to 
the public, perform publicly and display publicly, and to permit others to do so. 
NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL 
LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY 
INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE 
WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
*/

#include <string>
#include "DElightAPI.h"

void writewndo(const std::string instring, std::string sfpflg);

extern "C" DllExport void delightdaylightcoefficients(double dBldgLat, 
                                                      int* piErrorFlag); 

extern "C" DllExport void delightelecltgctrl(int iNameLength,
								   char* cZoneName, 
								   double dBldgLat, 
								   double dHISKF, 
								   double dHISUNF, 
								   double dCloudFraction, 
								   double dSOLCOSX, 
								   double dSOLCOSY, 
								   double dSOLCOSZ,
								   double* pdPowerReducFac,
                                   int* piErrorFlag);

extern "C" DllExport void delightfreememory();

extern "C" DllExport void delightoutputgenerator(int iOutputFlag);

