// C++ Headers
#include<memory>
#include<vector>

// ObjexxFCL Headers

// EnergyPlus Headers
#include <GroundTempsManager.hh>

namespace EnergyPlus {

namespace GroundTemps {

	Real64
	KusudaGroundTempsModel::getGroundTemp(
		Real64 const z, // Depth
		Real64 const diffusivityGround, // Ground props
		Real64 const simTimeInSeconds // Simulation time
	)
	{
		// AUTHOR         Matt Mitchell
		// DATE WRITTEN   June 2015
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Returns a ground temperature

		// METHODOLOGY EMPLOYED:
		// Kusuda and Achenbach correlation is used

		//Kusuda and Achenbach
		// Using/Aliasing
		using DataGlobals::SecsInDay;
		using DataGlobals::Pi;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 term1;
		Real64 term2;
		Real64 secsInYear;

		secsInYear = SecsInDay * 365.0;

		term1 = -z * std::sqrt( Pi / ( secsInYear * diffusivityGround ) );
		term2 = ( 2 * Pi / secsInYear ) * ( simTimeInSeconds - phaseShiftInSecs - ( z / 2 ) * std::sqrt( secsInYear / ( Pi * diffusivityGround ) ) );

		return aveGroundTemp - aveGroundTempAmplitude * std::exp( term1 ) * std::cos( term2 );
	}


	//******************************************************************************

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

}	// GroundTemps

}	// EnergyPlus
