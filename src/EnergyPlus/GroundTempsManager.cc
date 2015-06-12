// C++ Headers
#include<memory>
#include<vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <DataIPShortCuts.hh>
#include <GroundTempsManager.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

namespace GroundTemps {

	using InputProcessor::GetObjectDefMaxArgs;

	// Object Data
	std::vector < std::shared_ptr < BaseGroundTempsModel > > groundTempModels;

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
			ShowFatalError( "GetGroundTempsModelInput: Errors getting input for ground temperature model");
			return nullptr;
		}
	}

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
					found = true;
					break;
				}
			}

		if ( found && !ErrorsFound ) {
			groundTempModels.push_back( thisModel );
			return thisModel;
		} else {
			ShowFatalError( "GetGroundTempsModelInput: Errors getting input for ground temperature model" );
			return nullptr;
		}
	}

	std::shared_ptr< BaseGroundTempsModel >
	GetGroundTempModelAndInit(
		std::string const objectType_str,
		std::string const objectName
	)
	{
		int objectType( 0 );
		int objectType_Kusuda( 1 );
		int objectType_FiniteDiff( 2 );

		std::string objectType_Kusdua_str = "SITE:GROUNDTEMPERATURE:UNDISTURBED:KUSUDAACHENBACH";
		std::string objectType_FiniteDiff_str = "SITE:GROUNDTEMPERATURE:UNDISTURBED:FINITEDIFFERENCE";

		int numGTMs = groundTempModels.size();
	

		if ( objectType_str == objectType_Kusdua_str ) {
			objectType = objectType_Kusuda;
		} else if ( objectType_str == objectType_FiniteDiff_str ) {
			objectType = objectType_FiniteDiff;
		} else {
			ShowFatalError( "GetGroundTempsModelInput: Undisturbed Ground Temperature Object Type Not Recognized" );
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
