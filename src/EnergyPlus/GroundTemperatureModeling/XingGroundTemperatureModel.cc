// C++ Headers
#include <memory>

// EnergyPlus headers
#include <DataGlobals.hh>
#include <DataIPShortCuts.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <GroundTemperatureModeling/XingGroundTemperatureModel.hh>
#include <InputProcessor.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

	//******************************************************************************

	// Xing model factory
	std::shared_ptr< XingGroundTempsModel > 
	XingGroundTempsModel::XingGTMFactory( 
		int objectType, 
		std::string objectName
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads input and creates instance of Xing ground temps model

		// USE STATEMENTS:
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
		std::shared_ptr< XingGroundTempsModel > thisModel( new XingGroundTempsModel() );

		std::string const cCurrentModuleObject = CurrentModuleObjects( objectType_XingGroundTemp );
		int numCurrModels = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );

		for ( int modelNum = 1; modelNum <= numCurrModels; ++modelNum ) {

			InputProcessor::GetObjectItem( cCurrentModuleObject, modelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

			if ( objectName == cAlphaArgs( 1 ) ) {
				// Read input into object here

				thisModel->objectName = cAlphaArgs( 1 );
				thisModel->objectType = objectType;
				thisModel->groundThermalDiffisivity = rNumericArgs( 1 ) / ( rNumericArgs( 2 ) * rNumericArgs( 3 ) );
				thisModel->aveGroundTemp = rNumericArgs( 4 );
				thisModel->surfTempAmplitude_1 = rNumericArgs( 5 );
				thisModel->surfTempAmplitude_2 = rNumericArgs( 6 );
				thisModel->phaseShift_1 = rNumericArgs( 7 );
				thisModel->phaseShift_2 = rNumericArgs( 8 );
				
				found = true;
				break;
			}
		}

		if ( found && !ErrorsFound ) {
			groundTempModels.push_back( thisModel );
			return thisModel;
		} else {
			ShowFatalError( "Site:GroundTemperature:Undisturbed:Xing--Errors getting input for ground temperature model");
			return nullptr;
		}
	}

	//******************************************************************************

	Real64 XingGroundTempsModel::getGroundTemp()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns the ground temperature for the Site:GroundTemperature:Undisturbed:Xing

		// USE STATEMENTS:
		using DataGlobals::Pi;
		using WeatherManager::NumDaysInYear;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int n;
		Real64 static tp( NumDaysInYear ); // Period of soil temperature cycle
		Real64 Ts_1; // Amplitude of surface temperature
		Real64 Ts_2; // Amplitude of surface temperature
		Real64 PL_1; // Phase shift of surface temperature
		Real64 PL_2; // Phase shift of surface temperature

		Real64 term1;
		Real64 term2;
		Real64 term3;
		Real64 term4;

		Real64 retVal;
		Real64 summation;

		// Inits
		Ts_1 = surfTempAmplitude_1;
		PL_1 = phaseShift_1;
		Ts_2 = surfTempAmplitude_2;
		PL_2 = phaseShift_2;

		n = 1;
		term1 = -depth * std::sqrt( ( n * Pi ) / ( groundThermalDiffisivity * tp ) );
		term2 = ( 2 * Pi * n ) / tp * ( simTimeInDays - PL_1 ) - depth * std::sqrt( ( n * Pi ) / ( groundThermalDiffisivity * tp ) );

		n = 2;
		term3 = -depth * std::sqrt( ( n * Pi ) / ( groundThermalDiffisivity * tp ) );
		term4 = ( 2 * Pi * n ) / tp * ( simTimeInDays - PL_2 ) - depth * std::sqrt( ( n * Pi ) / ( groundThermalDiffisivity * tp ) );

		summation = std::exp( term1 ) * Ts_1 * std::cos( term2 ) + std::exp( term3 ) * Ts_2 * std::cos( term4 );

		retVal = aveGroundTemp - summation;

		return retVal;
	}

	//******************************************************************************

	Real64 XingGroundTempsModel::getGroundTempAtTimeInMonths(
		Real64 _depth,
		int _month
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns ground temperature when input time is in months

		// USE STATEMENTS:
		using WeatherManager::NumDaysInYear;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 const aveDaysInMonth = NumDaysInYear / 12;

		depth = _depth;
	
		// Set month
		if ( _month >= 1 && _month <= 12 ) {
			simTimeInDays = aveDaysInMonth * ( ( _month - 1 ) + 0.5 );
		} else {
			int monthIndex = remainder( _month, 12 );
			simTimeInDays = aveDaysInMonth * ( ( monthIndex - 1 ) + 0.5 );
		}

		// Get and return ground temp
		return getGroundTemp();
	}

	//******************************************************************************

	Real64 XingGroundTempsModel::getGroundTempAtTimeInSeconds(
		Real64 _depth,
		Real64 seconds
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Returns ground temperature when time is in seconds

		// USE STATEMENTS:
		using DataGlobals::SecsInDay;
		using WeatherManager::NumDaysInYear;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		depth = _depth;

		simTimeInDays = seconds / SecsInDay;

		if ( simTimeInDays >  NumDaysInYear ) {
			simTimeInDays = remainder( simTimeInDays, NumDaysInYear );
		}

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

}	// EnergyPlus namespace
