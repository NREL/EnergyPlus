// C++ Headers
#include<memory>
#include<vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <GroundTempsManager.hh>
#include <InputProcessor.hh>

namespace EnergyPlus {

namespace GroundTempsManager {

	// Object Data
	std::vector< std::shared_ptr < BaseGroundTempsModel > > groundTempModels;

	// Kusuda model factory
	std::shared_ptr< KusudaGroundTempsModel > 
	KusudaGTMFactory( int objectType, std::string objectName ){

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

		// Search through Kusuda models here
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
			ShowFatalError( "GetGroundTempsModelInput: Errors getting input for ground temperature model");
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

}	// GroundTempsManger

}	// EnergyPlus
