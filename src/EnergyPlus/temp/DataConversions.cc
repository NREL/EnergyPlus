// EnergyPlus Headers
#include <DataConversions.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataConversions {

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   October 1998
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for conversion unit variables which
	// strictly "should not be needed" in EnergyPlus but may be used in a few
	// places (e.g. Conduction Transfer Functions).  We have adopted the "international
	// table" for conversions.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// Original Reference: DARCOM P 706-470, Engineering Design Handbook,
	// Metric Conversion Guide, July 1976.
	// Federal Standard 376B, January 27, 1993.  Preferred metric units
	// for general use by the Federal Government.
	// ASHRAE has similar recommendations.

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	//REAL(r64), PARAMETER:: CFC     =4.184D0            ! Specific Heat:  BTU/(LB*R) * CFC = KJ/(KG*K)
	Real64 const CFC( 4.1868 ); // Specific Heat:  BTU/(LB*R) * CFC = KJ/(KG*K)
	//  above is listed in July 1976 publication as "International Table"
	Real64 const CFL( 0.3048 ); // Length:         FT * CFL = M
	Real64 const CFM( 0.45359237 ); // Mass:           LB * CFM = KG
	//REAL(r64), PARAMETER:: CFP     =249.082D0          ! Pressure:       IN-H2O * CFP = N/M**2
	// above is listed in July 1976 publication as in-water at 39.2 deg F
	Real64 const CFP( 248.84 ); // Pressure:       IN-H2O * CFP = N/M**2
	//  above is listed in July 1976 publication as in-water at 60 deg F
	Real64 const DELTMP( -32.0 ); // Temperature:    (F + DELTMP) * CFT = C
	Real64 const CFA( CFL * CFL ); // Area:           FT**2 * CFA = M**2
	Real64 const CFT( 5.0 / 9.0 ); // Temperature:    R * CFT = K
	Real64 const CFV( CFA * CFL ); // Volume:         FT**3 * CFV = M**3
	Real64 const CFE( CFC * CFM * CFT / 3.6 ); // Energy:         BTU * CFE = W-HR
	Real64 const CFD( CFM / CFV ); // Density:        LB/FT**3 * CFD = KG/M**3
	Real64 const CFH( CFC * CFT ); // Enthalpy:       BTU/LB * CFH = J/KG
	Real64 const CFK( CFE / ( CFL * CFT ) ); // Conductivity:   BTU/(HR*FT*R) * CFK = W/(M*K)
	Real64 const CFMF( CFM / 3600.0 ); // Mass Flow:      LB/HR * CFMF = KG/SEC
	Real64 const CFQ( CFE ); // Power:          BTU/HR * CFQ = W
	Real64 const CFU( CFK / CFL ); // U-Value:        BTU/(HR*FT**2*R) * CFU = W/(M**2*K)
	// Note:  R-Value = 1/U-Value
	Real64 const CFS( CFL / 60.0 ); // Speed:          FT/MIN * CFS = M/SEC
	Real64 const CFVF( CFV / 60.0 ); // Volume Flow:    FT**3/MIN * CFVF = M**3/SEC
	Real64 const CFHF( CFQ / CFA ); // Heat Flux:      BTU/(HR*FT**2) * CFHF = W/M**2
	Real64 const CFTMP( DELTMP ); // Temperature:    Same as DELTMP

	// DERIVED TYPE DEFINITIONS
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	//     NOTICE
	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.
	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.
	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.
	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // DataConversions

} // EnergyPlus
