// C++ Headers
#include<memory>

// ObjexxFCL Headers

// EnergyPlus Headers
#include <DataIPShortCuts.hh>
#include <GroundTemperatureModeling/KusudaAchenbachGroundTemperatureModel.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

	//******************************************************************************

	// Kusuda model factory
	std::shared_ptr< KusudaGroundTempsModel > 
	KusudaGroundTempsModel::KusudaGTMFactory( 
		int objectType, 
		std::string objectName,
		Real64 groundThermalDiffusivity
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads input and creates instance of Kusuda ground temps model

		// USE STATEMENTS:
		using DataGlobals::SecsInDay;
		using namespace DataIPShortCuts;
		using namespace GroundTemperatureManager;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:	
		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;
		bool ErrorsFound = false;

		// New shared pointer for this model object
		std::shared_ptr< KusudaGroundTempsModel > thisModel( new KusudaGroundTempsModel() );

		std::string const cCurrentModuleObject = "Site:GroundTemperature:Undisturbed:KusudaAchenbach";
		int numCurrModels = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );

		for ( int modelNum = 1; modelNum <= numCurrModels; ++modelNum ) {

			InputProcessor::GetObjectItem( cCurrentModuleObject, modelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

			if ( objectName == cAlphaArgs( 1 ) ) {

				// Read input into object here
				thisModel->objectName = cAlphaArgs( 1 );
				thisModel->objectType = objectType;
				thisModel->aveGroundTemp = rNumericArgs( 1 );
				thisModel->aveGroundTempAmplitude = rNumericArgs( 2 );
				thisModel->phaseShiftInSecs = rNumericArgs( 3 ) * SecsInDay;
				thisModel->groundThermalDiffisivity = groundThermalDiffusivity;

				// Putting this here for now. Need to implement functionality allowing parameters to be generated from Site:GroundTemperature:Shallow if KA object not present

				//int monthsInYear( 12 );
				//int avgDaysInMonth( 30 );
				//int monthOfMinSurfTemp( 0 );
				//Real64 averageGroundTemp( 0 );
				//Real64 averageGroundTempAmplitude( 0 );
				//Real64 phaseShiftOfMinGroundTempDays( 0 );
				//Real64 minSurfTemp( 100 ); // Set high month 1 temp will be lower and actually get updated

				//InputProcessor::GetObjectItem( cCurrentModuleObject, modelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

				//thisModel->objectName = "Site:GroundTemperature:Shallow";

				//thisModel->objectType = objectType;

				//// Calculate Average Ground Temperature for all 12 months of the year:
				//for ( int monthIndex = 1; monthIndex <= monthsInYear; ++monthIndex ) {
				//	averageGroundTemp += PubGroundTempSurface( monthIndex );
				//}
				//averageGroundTemp /= monthsInYear;
				//
				//thisModel->aveGroundTemp = averageGroundTemp;

				//// Calculate Average Amplitude from Average:;
				//for ( int monthIndex = 1; monthIndex <= monthsInYear; ++monthIndex ) {
				//	averageGroundTempAmplitude += std::abs( PubGroundTempSurface( monthIndex ) - averageGroundTemp );
				//}
				//averageGroundTempAmplitude /= monthsInYear;
				//
				//thisModel->aveGroundTempAmplitude = averageGroundTempAmplitude;

				//// Also need to get the month of minimum surface temperature to set phase shift for Kusuda and Achenbach:
				//for ( int monthIndex = 1; monthIndex <= monthsInYear; ++monthIndex ) {
				//	if ( PubGroundTempSurface( monthIndex ) <= minSurfTemp ) {
				//		monthOfMinSurfTemp = monthIndex;
				//		minSurfTemp = PubGroundTempSurface( monthIndex );
				//	}
				//}
				//
				//phaseShiftOfMinGroundTempDays = monthOfMinSurfTemp * avgDaysInMonth;

				//// Unit conversion
				//thisModel->phaseShiftInSecs = phaseShiftOfMinGroundTempDays * SecsInDay;


				found = true;
				break;
			}
		}

		if ( found && !ErrorsFound ) {
			groundTempModels.push_back( thisModel );
			return thisModel;
		} else {
			ShowFatalError( "Site:GroundTemperature:Undisturbed:KusudaAchenbach--Errors getting input for ground temperature model");
			return nullptr;
		}
	}

	//******************************************************************************

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

}	// EnergyPlus
