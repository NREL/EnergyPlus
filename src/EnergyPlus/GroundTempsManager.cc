// C++ Headers
#include<memory>
#include<vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <DataEnvironment.hh>
#include <DataIPShortCuts.hh>
#include <GroundTempsManager.hh>
#include <InputProcessor.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

namespace GroundTemps {
			
	using InputProcessor::GetObjectDefMaxArgs;

	// Object Data
	std::vector < std::shared_ptr < BaseGroundTempsModel > > groundTempModels;

	static gio::Fmt fmtA( "(A)" );
	static gio::Fmt fmtAN( "(A,$)" );

	//******************************************************************************

	// Kusuda model factory
	std::shared_ptr< KusudaGroundTempsModel > 
	KusudaGTMFactory( 
	int objectType, 
	std::string objectName,
	Real64 groundThermalDiffusivity
	)
	{
		
		using DataGlobals::SecsInDay;
		using namespace DataIPShortCuts;
		
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

					thisModel->objectType = objectType;

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

	// Site:GroundTemperature:Shallow factory
	std::shared_ptr< ShallowGroundTemps > 
	ShallowGTMFactory( int objectType ){

		using DataEnvironment::GroundTemp_SurfaceObjInput;
		using DataGlobals::OutputFileInits;
		using DataGlobals::SecsInDay;
		using namespace DataIPShortCuts;
		using namespace ObjexxFCL::gio;
		
		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;

		// New shared pointer for this model object
		std::shared_ptr< ShallowGroundTemps > thisModel( new ShallowGroundTemps() );

		// Search through Kusuda models here
		std::string const cCurrentModuleObject = "Site:GroundTemperature:Shallow";
		int numCurrObjects = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );

		if ( numCurrObjects == 1 ) {

			//Get the object names for each construction from the input processor
			InputProcessor::GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

			if ( NumNums < 12 ) {
				ShowSevereError( cCurrentModuleObject + ": Less than 12 values entered." );
				thisModel->errorsFound = true;
			}

			//Assign the ground temps to the variable
			for ( int i = 1; i <= 12; ++i ) {
				thisModel->surfaceGroundTemps( i ) = rNumericArgs( i );
			}

			GroundTemp_SurfaceObjInput = true;

		} else if ( numCurrObjects > 1 ) {
			ShowSevereError( cCurrentModuleObject + ": Too many objects entered. Only one allowed." );
			thisModel->errorsFound = true;
		} else {
			thisModel->surfaceGroundTemps = 13.0;
		}

		// Write Final Ground Temp Information to the initialization output file
		gio::write( OutputFileInits, fmtA ) << "! <Site:GroundTemperature:Shallow>, Months From Jan to Dec {C}";
		gio::write( OutputFileInits, fmtAN ) << " Site:GroundTemperature:Shallow";
		for ( int i = 1; i <= 12; ++i ) gio::write( OutputFileInits, "(', ',F6.2,$)" ) << thisModel->surfaceGroundTemps( i ); gio::write( OutputFileInits );

		found = true;

		if ( found && !thisModel->errorsFound ) {
			groundTempModels.push_back( thisModel );
			return thisModel;
		} else {
			ShowContinueError( "Site:GroundTemperature:Shallow--Errors getting input for ground temperature model");
			return nullptr;
		}
	}

	//******************************************************************************

	// Site:GroundTemperature:BuildingSurface factory
	std::shared_ptr< BuildingSurfaceGroundTemps > 
	BuildingSurfaceGTMFactory( int objectType ){

		using DataEnvironment::GroundTempObjInput;
		using DataGlobals::OutputFileInits;		
		using DataGlobals::SecsInDay;
		using namespace DataIPShortCuts;
		using namespace ObjexxFCL::gio;
		
		bool found = false;
		bool genErrorMessage = false;
		int NumNums;
		int NumAlphas;
		int IOStat;

		// New shared pointer for this model object
		std::shared_ptr< BuildingSurfaceGroundTemps > thisModel( new BuildingSurfaceGroundTemps() );

		// Search through Kusuda models here
		std::string const cCurrentModuleObject = "Site:GroundTemperature:BuildingSurface";
		int numCurrObjects = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );

		if ( numCurrObjects == 1 ) {

			//Get the object names for each construction from the input processor
			InputProcessor::GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

			if ( NumNums < 12 ) {
				ShowSevereError( cCurrentModuleObject + ": Less than 12 values entered." );
				thisModel->errorsFound = true;
			}

			//Assign the ground temps to the variable
			for ( int i = 1; i <= 12; ++i ) {
				thisModel->buildingSurfaceGroundTemps( i ) = rNumericArgs( i );
				if ( thisModel->buildingSurfaceGroundTemps( i ) < 15.0 || thisModel->buildingSurfaceGroundTemps( i ) > 25.0 ) genErrorMessage = true;
			}

			GroundTempObjInput = true;

			if ( genErrorMessage ) {
			ShowWarningError( cCurrentModuleObject + ": Some values fall outside the range of 15-25C." );
			ShowContinueError( "These values may be inappropriate.  Please consult the Input Output Reference for more details." );
			}

		} else if ( numCurrObjects > 1 ) {
			ShowSevereError( cCurrentModuleObject + ": Too many objects entered. Only one allowed." );
			thisModel->errorsFound = true;
		} else {
			thisModel->buildingSurfaceGroundTemps = 18.0;
		}

		// Write Final Ground Temp Information to the initialization output file
		gio::write( OutputFileInits, fmtA ) << "! <Site:GroundTemperature:BuildingSurface>, Months From Jan to Dec {C}";
		gio::write( OutputFileInits, fmtAN ) << " Site:GroundTemperature:BuildingSurface";
		for ( int i = 1; i <= 12; ++i ) gio::write( OutputFileInits, "(', ',F6.2,$)" ) << thisModel->buildingSurfaceGroundTemps( i ); gio::write( OutputFileInits );

		found = true;

		if ( found && !thisModel->errorsFound ) {
			groundTempModels.push_back( thisModel );
			return thisModel;
		} else {
			ShowContinueError( "Site:GroundTemperature:BuildingSurface--Errors getting input for ground temperature model");
			return nullptr;
		}
	}

	//******************************************************************************

	// Site:GroundTemperature:FCFactorMethod factory
	std::shared_ptr< FCFactorGroundTemps > 
	FCFactorGTMFactory( int objectType ){

		using DataEnvironment::FCGroundTemps;
		using DataGlobals::OutputFileInits;
		using DataGlobals::SecsInDay;
		using WeatherManager::wthFCGroundTemps;
		using namespace DataIPShortCuts;
		using namespace ObjexxFCL::gio;

		
		
		bool found = false;
		bool genErrorMessage = false;
		int NumNums;
		int NumAlphas;
		int IOStat;

		// New shared pointer for this model object
		std::shared_ptr< FCFactorGroundTemps > thisModel( new FCFactorGroundTemps() );

		// Search through Kusuda models here
		std::string const cCurrentModuleObject = "Site:GroundTemperature:FCFactorMethod";
		int numCurrObjects = InputProcessor::GetNumObjectsFound( cCurrentModuleObject );

		if ( numCurrObjects == 1 ) {

			//Get the object names for each construction from the input processor
			InputProcessor::GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

			if ( NumNums < 12 ) {
				ShowSevereError( cCurrentModuleObject + ": Less than 12 values entered." );
				thisModel->errorsFound = true;
			}

			// overwrite values read from weather file for the 0.5m set ground temperatures
			for ( int i = 1; i <= 12; ++i ) {
				thisModel->fcFactorGroundTemps( i ) = rNumericArgs( i );
			}

			FCGroundTemps = true;

		} else if ( numCurrObjects > 1 ) {
			ShowSevereError( cCurrentModuleObject + ": Too many objects entered. Only one allowed." );
			thisModel->errorsFound = true;

		} else if ( wthFCGroundTemps ) {
			FCGroundTemps = true;
		}

		// Write Final Ground Temp Information to the initialization output file
		gio::write( OutputFileInits, fmtA ) << "! <Site:GroundTemperature:FCfactorMethod>, Months From Jan to Dec {C}";
		gio::write( OutputFileInits, fmtAN ) << " Site:GroundTemperature:FCfactorMethod";
		for ( int i = 1; i <= 12; ++i ) gio::write( OutputFileInits, "(', ',F6.2,$)" ) << thisModel->fcFactorGroundTemps( i ); gio::write( OutputFileInits );

		found = true;

		if ( found && !thisModel->errorsFound ) {
			groundTempModels.push_back( thisModel );
			return thisModel;
		} else {
			ShowContinueError( "Site:GroundTemperature:FCFactorMethod--Errors getting input for ground temperature model");
			return nullptr;
		}
	}

	//******************************************************************************

	std::shared_ptr< BaseGroundTempsModel >
	GetGroundTempModelAndInit(
		std::string const objectType_str,
		std::string const objectName,
		Real64 const groundThermalDiffusivity
	)
	{
		int objectType( 0 );
		int objectType_KusudaGroundTemp( 1 );
		int objectType_FiniteDiffGroundTemp( 2 );
		int objectType_SiteBuildingSurfaceGroundTemp( 3 );
		int objectType_SiteShallowGroundTemp( 4 );
		int objectType_SiteDeepGroundTemp( 5 );
		int objectType_SiteFCFactorMethodGroundTemp( 6 );

		std::string objectType_KusudaGroundTemp_str = "SITE:GROUNDTEMPERATURE:UNDISTURBED:KUSUDAACHENBACH";
		std::string objectType_FiniteDiffGroundTemp_str = "SITE:GROUNDTEMPERATURE:UNDISTURBED:FINITEDIFFERENCE";
		std::string objectType_SiteBuildingSurfaceGroundTemp_str = "SITE:GROUNDTEMPERATURE:BUILDINGSURFACE";
		std::string objectType_SiteShallowGroundTemp_str = "SITE:GROUNDTEMPERATURE:SHALLOW";
		std::string objectType_SiteDeepGroundTemp_str = "SITE:GROUNDTEMPERATURE:DEEP";
		std::string objectType_SiteFCFactorMethodGroundTemp_str = "SITE:GROUNDTEMPERATURE:FCFACTORMETHOD";
	
		// Set object type
		if ( objectType_str == objectType_KusudaGroundTemp_str ) {
			objectType = objectType_KusudaGroundTemp;
		} else if ( objectType_str == objectType_FiniteDiffGroundTemp_str ) {
			objectType = objectType_FiniteDiffGroundTemp;
		} else if ( objectType_str == objectType_SiteBuildingSurfaceGroundTemp_str ) {
			objectType = objectType_SiteBuildingSurfaceGroundTemp;
		} else if ( objectType_str == objectType_SiteShallowGroundTemp_str ){
			objectType = objectType_SiteShallowGroundTemp;
		} else if ( objectType_str == objectType_SiteDeepGroundTemp_str ) {
			objectType = objectType_SiteDeepGroundTemp;
		} else if ( objectType_str == objectType_SiteFCFactorMethodGroundTemp_str ) {
			objectType = objectType_SiteFCFactorMethodGroundTemp;
		} else {
			// Error out if no ground temperature object types recognized
			ShowFatalError( "GetGroundTempsModelAndInit: Ground temperature object " + objectType_str + " not recognized." );
		}

		int numGTMs = groundTempModels.size();

		// Check if this instance of this model has already been retrieved
		for ( int i = 0; i < numGTMs; ++i ) {
			auto & currentModel( groundTempModels[i] );
			// Check if the type and name match
			if ( objectType == currentModel->objectType && objectName == currentModel->objectName) {
				return groundTempModels[i];
			}
		}

		// If not found, create new instance of the model
		if ( objectType == objectType_KusudaGroundTemp ) {
			return KusudaGTMFactory( objectType, objectName, groundThermalDiffusivity );
		} else if ( objectType == objectType_FiniteDiffGroundTemp ) {
			return FiniteDiffGTMFactory( objectType, objectName );
		} else if ( objectType == objectType_SiteBuildingSurfaceGroundTemp ) {
			return BuildingSurfaceGTMFactory( objectType );
		} else if ( objectType == objectType_SiteShallowGroundTemp ) {
			return ShallowGTMFactory( objectType );
		} else if ( objectType == objectType_SiteDeepGroundTemp ) {
			return 0;
		} else if ( objectType == objectType_SiteFCFactorMethodGroundTemp ) {
			return FCFactorGTMFactory( objectType );
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
