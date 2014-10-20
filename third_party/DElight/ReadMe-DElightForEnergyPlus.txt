***********************************
DElight2 Build Notes for EnergyPlus
Rob Hitchcock
February 27, 2009
***********************************

DElight2 is built as a DLL for integration with EnergyPlus.  As of the date of these notes the DElight
source code files on Starteam have been built within the Microsoft Visual Studio development platform 
in both VS2005 and VS2008 versions.

The DElight2.dll and DElight2.lib files checked into Starteam were built using VS2005 and tested with
an EnergyPlus build using the Intel(R) Fortran Compiler Integration for Microsoft Visual Studio 2005, 
Version 9.1.3291.2005.

***********************************
Source Code Notes
***********************************

There are multiple entry points into DElight, and divergent paths through DElight subroutine calls,
 depending on the program DElight is being integrated with (EnergyPlus, Energy-10, Standalone Interface)

The following flow sequence is the entry point and high-level subroutine call path for EnergyPlus to DElight.

The primary modules of interest include:
	EnergyPlus Module
		DElightManagerF.f90
	DElight Modules
		DElightManagerC.cpp
		EPlus_DElight.cpp
		EPlus_Loaddata.cpp
		EPlus_Geom.cpp
		DFcalcs.cpp
		EPlus_ECM.cpp
		ECM.cpp

=================================================
DElightManagerF.f90 [EnergyPlus interface module]
=================================================
	// DElight Input File Creation from Transformation of EnergyPlus Input
	SUBROUTINE DElightInputGenerator
	
	// Daylight Factor Preprocessing call to DElight DLL
	INTERFACE SUBROUTINE DElightDaylightCoefficients (dBldgLat, iErrorFlag)
	
	// Timestep Reference Point Illuminance and Electric Lighting Power Reduction Calculations call to DElight DLL
    INTERFACE SUBROUTINE DElightElecLtgCtrl (iNameLength, cZoneName, dBldgLat, &
                            dHISKF, dHISUNF, dCloudFraction, dSOLCOSX, dSOLCOSY, dSOLCOSZ, &
                            pdPowerReducFac, iErrorFlag)

=================================================
DElightManagerC.cpp [DElight interface module]
=================================================
	// Daylight Factor Preprocessing call from EnergyPlus
	extern "C" DllExport void delightdaylightcoefficients(double dBldgLat, 
															int* piErrorFlag)  // return Error Flag from DElight to EPlus

		calls DElightDaylightFactors4EPlus(...) [EPlus_DElight.cpp]
			calls LoadDataFromEPlus(...) [EPlus_Loaddata.cpp]
			calls LoadLibDataFromEPlus(...) [EPlus_Loaddata.cpp]
			calls CalcGeomFromEPlus(...) [EPlus_Geom.cpp]
			calls CalcDFs(...) [DFcalcs.cpp]
	
	// Timestep Reference Point Illuminance and Electric Lighting Power Reduction Calculations call from EnergyPlus
	extern "C" DllExport void delightelecltgctrl(int iNameLength,
                                    char* cZoneName, 
                                    double dBldgLat, 
                                    double dHISKF, 
                                    double dHISUNF, 
                                    double dCloudFraction, 
                                    double dSOLCOSX, 
                                    double dSOLCOSY, 
                                    double dSOLCOSZ,
                                    double* pdPowerReducFac,	// return value for calculated Zone Elec Ltg Power Reduction Factor
                                    int* piErrorFlag)  // return Error Flag from DElight to EPlus

		calls DElightElecLtgCtrl4EPlus(...) [EPlus_DElight.cpp]
			calls CalcInterpolationVars(...) [EPlus_ECM.cpp]
			calls CalcZoneInteriorIllum(...) [EPlus_ECM.cpp]
			calls dltsys(...) [ECM.cpp]
