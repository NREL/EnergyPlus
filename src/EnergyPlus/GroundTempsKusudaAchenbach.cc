// C++ Headers
#include<memory>

// ObjexxFCL Headers

// EnergyPlus Headers
#include <GroundTempsManager.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

namespace GroundTemps {

	Real64
	KusudaGroundTempsModel::getGroundTemp()
	{
		// AUTHOR         Matt Mitchell
		// DATE WRITTEN   June 2015
		// MODIFIED       na
		// RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Returns a ground temperature

		// METHODOLOGY EMPLOYED:
		// Kusuda and Achenbach correlation is used

		// Using/Aliasing
		using DataGlobals::SecsInDay;
		using DataGlobals::Pi;

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 term1;
		Real64 term2;
		Real64 secsInYear;

		secsInYear = SecsInDay * 365.0;

		term1 = -depth * std::sqrt( Pi / ( secsInYear * groundThermalDiffisivity ) );
		term2 = ( 2 * Pi / secsInYear ) * ( simTimeInSeconds - phaseShiftInSecs - ( depth / 2 ) * std::sqrt( secsInYear / ( Pi * groundThermalDiffisivity ) ) );

		return aveGroundTemp - aveGroundTempAmplitude * std::exp( term1 ) * std::cos( term2 );
	}

	//******************************************************************************

	Real64
	KusudaGroundTempsModel::getGroundTempAtTimeInSeconds(
		Real64 const depthOfTemp,
		Real64 const seconds
	)
	{	
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       Matt Mitchell, Summer 2015
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns the ground temperature when input time is in seconds

		// Set depth of temperature
		depth = depthOfTemp;

		// Set sim time in seconds
		simTimeInSeconds = seconds;

		// Get and return ground temperature
		return getGroundTemp();
	}

	//******************************************************************************

	Real64
	KusudaGroundTempsModel::getGroundTempAtTimeInMonths(
		Real64 const depth,
		int const month
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       Matt Mitchell, Summer 2015
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns the ground temperature when input time is in months

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 const aveSecondsInMonth = ( 365 / 12 ) * ( 3600 * 24 );

		// Convert months to seconds. Puts 'seconds' time in middle of specified month
		if ( month >= 1 && month <= 12 ) {
			simTimeInSeconds = aveSecondsInMonth * ( ( month - 1 ) + 0.5 );
		} else {
			ShowFatalError("KusudaGroundTempsModel: Invalid month passed to ground temperature model");
		}
		
		// Get and return ground temperature
		return getGroundTemp();
	}

	//******************************************************************************

	//     NOTICE

	//     Copyright (c) 1996-2015 The Board of Trustees of the University of Illinois
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
