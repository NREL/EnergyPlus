// EnergyPlus Headers
#include <DataComplexFenestration.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataComplexFenestration {
	// MODULE INFORMATION:
	//       AUTHOR         Simon Vidanovic
	//       DATE WRITTEN   January 2012
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module contains data necessary for complex fenestration calculations

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// Parameters for complex shade
	int const csVenetian( 1 );
	int const csWoven( 2 );
	int const csPerforated( 3 );
	int const csOtherShadingType( 4 );
	int const csBSDF( 5 );

	// Parameters for gas definitions
	int const GasCoeffsCustom( 0 );
	int const GasCoeffsAir( 1 );
	int const GasCoeffsArgon( 2 );
	int const GasCoeffsKrypton( 3 );
	int const GasCoeffsXenon( 4 );

	// Parameters for Thermal Algorithm
	//INTEGER, PARAMETER :: taTarcog = 0
	//INTEGER, PARAMETER :: taWinkelmann = 1

	// Parameters for calculation standard
	int const csISO15099( 1 );
	int const csEN673Declared( 2 );
	int const csEN673Design( 3 );

	// Parameters for thermal model
	int const tmISO15099( 0 );
	int const tmScaledCavityWidth( 1 );
	int const tmConvectiveScalarModel_NoSDThickness( 2 );
	int const tmConvectiveScalarModel_WithSDThickness( 3 );

	// Parameters for deflection model
	int const dmNoDeflection( 0 );
	int const dmTemperatureAndPressureInput( 1 );
	int const dmMeasuredDeflection( 2 );

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

} // DataComplexFenestration

} // EnergyPlus
