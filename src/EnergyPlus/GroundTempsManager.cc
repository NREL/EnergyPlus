// C++ Headers
#include<memory>
#include<vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <DataEnvironment.hh>
#include <DataIPShortCuts.hh>
#include <GroundTempsManager.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

namespace GroundTemps {
			
	using DataEnvironment::PubGroundTempSurfFlag;
	using DataEnvironment::PubGroundTempSurface;
	using InputProcessor::GetObjectDefMaxArgs;

	// Object Data
	int objectType_Kusuda( 1 );
	int objectType_FiniteDiff( 2 );
	int objectType_ShallowGroundTemp( 3 );
	std::vector < std::shared_ptr < BaseGroundTempsModel > > groundTempModels;

	//******************************************************************************

	// Kusuda model factory
	std::shared_ptr< KusudaGroundTempsModel > 
	KusudaGTMFactory( int objectType, std::string objectName ){

		using namespace DataIPShortCuts;
		using DataGlobals::SecsInDay;
		
		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;
		bool ErrorsFound = false;

		// New shared pointer for this model object
		std::shared_ptr< KusudaGroundTempsModel > thisModel( new KusudaGroundTempsModel() );

		// Search through Kusuda models here
		std::string const cCurrentModuleObject = "Site:GroundTemperature:Undisturbed:KusudaAchenbach";
		int numCurrModels = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );
			for ( int modelNum = 1; modelNum <= numCurrModels; ++modelNum ) {

				int static totalArgs;
				int static numAlphas;
				int static numNumbers;

				InputProcessor::GetObjectItem( cCurrentModuleObject, modelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

				if ( objectName == cAlphaArgs( 1 ) ) {
					// Read input into object here

					thisModel->objectName = cAlphaArgs( 1 );

					thisModel->objectType = objectType_Kusuda;

					thisModel->aveGroundTemp = rNumericArgs( 1 );

					thisModel->aveGroundTempAmplitude = rNumericArgs( 2 );

					thisModel->phaseShiftInSecs = rNumericArgs( 3 ) * SecsInDay;

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

	// Finite difference model factory
	std::shared_ptr< FiniteDiffGroundTempsModel > 
	FiniteDiffGTMFactory( int objectType, std::string objectName ){

		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;
		bool ErrorsFound = false;

		// New shared pointer for this model object
		std::shared_ptr< FiniteDiffGroundTempsModel > thisModel( new FiniteDiffGroundTempsModel() );

		// Search through finite diff models here
		std::string const cCurrentModuleObject = "Site:GroundTemperature:Undisturbed:FiniteDifference";
		int numCurrModels = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );
			for ( int modelNum = 1; modelNum <= numCurrModels; ++modelNum ) {

				Array1D_string cAlphaArgs;
				Array1D< Real64 > rNumericArgs;

				InputProcessor::GetObjectItem( cCurrentModuleObject, modelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

				if ( objectName == cAlphaArgs( 1 ) ) {
					// Read input into object here

					thisModel->objectType = objectType_FiniteDiff;

					found = true;
					break;
				}
			}

		if ( found && !ErrorsFound ) {
			groundTempModels.push_back( thisModel );
			return thisModel;
		} else {
			ShowFatalError( "Site:GroundTemperature:Undisturbed:FiniteDifference--Errors getting input for ground temperature model" );
			return nullptr;
		}
	}

	//******************************************************************************

	// Site:GroundTemperature:Shallow model factory
	std::shared_ptr< KusudaGroundTempsModel > 
	ShallowGTMFactory( int objectType, std::string objectName ){

		using namespace DataIPShortCuts;
		using DataGlobals::SecsInDay;
		
		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;
		bool ErrorsFound = false;

		// New shared pointer for this model object
		std::shared_ptr< KusudaGroundTempsModel > thisModel( new KusudaGroundTempsModel() );

		// Search through Kusuda models here
		std::string const cCurrentModuleObject = "Site:GroundTemperature:Shallow";
		int numCurrModels = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );
			for ( int modelNum = 1; modelNum <= numCurrModels; ++modelNum ) {

				int static totalArgs;
				int static numAlphas;
				int static numNumbers;

				InputProcessor::GetObjectItem( cCurrentModuleObject, modelNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

				thisModel->objectName = "Site:GroundTemperature:Shallow";

				thisModel->objectType = objectType_ShallowGroundTemp;

				//// Calculate Average Ground Temperature for all 12 months of the year:
				////	PipingSystemDomains( DomainCtr ).Farfield.AverageGroundTemperature = 0.0;
				////	for ( MonthIndex = 1; MonthIndex <= MonthsInYear; ++MonthIndex ) {
				////		PipingSystemDomains( DomainCtr ).Farfield.AverageGroundTemperature += PubGroundTempSurface( MonthIndex );
				////	}
				////	PipingSystemDomains( DomainCtr ).Farfield.AverageGroundTemperature /= MonthsInYear;

				////	 Calculate Average Amplitude from Average:
				////	PipingSystemDomains( DomainCtr ).Farfield.AverageGroundTemperatureAmplitude = 0.0;
				////	for ( MonthIndex = 1; MonthIndex <= MonthsInYear; ++MonthIndex ) {
				////		PipingSystemDomains( DomainCtr ).Farfield.AverageGroundTemperatureAmplitude += std::abs( PubGroundTempSurface( MonthIndex ) - PipingSystemDomains( DomainCtr ).Farfield.AverageGroundTemperature );
				////	}
				////	PipingSystemDomains( DomainCtr ).Farfield.AverageGroundTemperatureAmplitude /= MonthsInYear;

				////	 Also need to get the month of minimum surface temperature to set phase shift for Kusuda and Achenbach:
				////	Domain( ZoneCoupledDomainCtr ).MonthOfMinSurfTemp = 0;
				////	Domain( ZoneCoupledDomainCtr ).MinSurfTemp = LargeNumber; // Set high month 1 temp will be lower and actually get updated
				////	for ( MonthIndex = 1; MonthIndex <= MonthsInYear; ++MonthIndex ) {
				////		if ( PubGroundTempSurface( MonthIndex ) <= Domain( ZoneCoupledDomainCtr ).MinSurfTemp ) {
				////			Domain( ZoneCoupledDomainCtr ).MonthOfMinSurfTemp = MonthIndex;
				////			Domain( ZoneCoupledDomainCtr ).MinSurfTemp = PubGroundTempSurface( MonthIndex );
				////		}
				////	}
				////	PipingSystemDomains( DomainCtr ).Farfield.PhaseShiftOfMinGroundTempDays = Domain( ZoneCoupledDomainCtr ).MonthOfMinSurfTemp * AvgDaysInMonth;
				////}

				//// Unit conversion
				////PipingSystemDomains( DomainCtr ).Farfield.PhaseShiftOfMinGroundTemp = PipingSystemDomains( DomainCtr ).Farfield.PhaseShiftOfMinGroundTempDays * SecsInDay;



				thisModel->aveGroundTemp = 15;

				thisModel->aveGroundTempAmplitude = 5;

				thisModel->phaseShiftInSecs = 250000;

				found = true;
				break;

			}

		if ( found && !ErrorsFound ) {
			groundTempModels.push_back( thisModel );
			return thisModel;
		} else {
			ShowFatalError( "Site:GroundTemperature:Shallow--Errors getting input for ground temperature model");
			return nullptr;
		}
	}

	//******************************************************************************

	std::shared_ptr< BaseGroundTempsModel >
	GetGroundTempModelAndInit(
		std::string const objectType_str,
		std::string const objectName
	)
	{
		int objectType( 0 );

		std::string objectType_Kusdua_str = "SITE:GROUNDTEMPERATURE:UNDISTURBED:KUSUDAACHENBACH";
		std::string objectType_FiniteDiff_str = "SITE:GROUNDTEMPERATURE:UNDISTURBED:FINITEDIFFERENCE";
		std::string objectType_ShallowGroundTemp_str = "SITE:GROUNDTEMPERATURE:SHALLOW";

		int numGTMs = groundTempModels.size();
	
		// Set object types
		if ( objectType_str == objectType_Kusdua_str ) {
			objectType = objectType_Kusuda;
		} else if ( objectType_str == objectType_FiniteDiff_str ) {
			objectType = objectType_FiniteDiff;
		} else if ( objectType_str == objectType_ShallowGroundTemp_str ){
			objectType = objectType_ShallowGroundTemp;
			if ( !PubGroundTempSurfFlag ) {
				ShowSevereError( "Input problem for " + objectType_str );
				ShowContinueError( "No Site:GroundTemperature:Shallow object found in the input file" );
			}

		} else {
			// Error out if no ground temperature object types recognized
			ShowFatalError( "GetGroundTempsModelAndInit: Undisturbed Ground Temperature Object Type Not Recognized" );
		}

		// Check if this instance of this model has already been retrieved
		for ( int i = 1; i <= numGTMs; ++i ) {
			auto & currentModel( groundTempModels[i] );
			// Check if the type and name match
			if ( objectType == currentModel->objectType && objectName == currentModel->objectName) {
				return groundTempModels[i];
			}
		}

		// If not found, create new instance of the model
		if ( objectType == objectType_Kusuda ) {
			return KusudaGTMFactory( objectType, objectName );
		} else if ( objectType == objectType_FiniteDiff ) {
			return FiniteDiffGTMFactory( objectType, objectName );
		} else if ( objectType == objectType_ShallowGroundTemp ) {
			return ShallowGTMFactory( objectType, objectName );
		} else {
			// Error
			return nullptr; 
		}
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
