// C++ Headers
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <DataEnvironment.hh>
#include <DataIPShortCuts.hh>
#include <GroundTemperatureModeling/GroundTemperatureModelManager.hh>
#include <GroundTemperatureModeling/SiteShallowGroundTemperatures.hh>
#include <InputProcessor.hh>
#include <WeatherManager.hh>

namespace EnergyPlus {

	static gio::Fmt fmtA( "(A)" );
	static gio::Fmt fmtAN( "(A,$)" );

	//******************************************************************************

	// Site:GroundTemperature:Shallow factory
	std::shared_ptr< SiteShallowGroundTemps > 
	SiteShallowGroundTemps::ShallowGTMFactory()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Matt Mitchell
		//       DATE WRITTEN   Summer 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads input and creates instance of Site:GroundDomain:Shallow object

		// USE STATEMENTS:
		using DataEnvironment::GroundTemp_SurfaceObjInput;
		using DataGlobals::OutputFileInits;
		using DataGlobals::SecsInDay;
		using namespace DataIPShortCuts;
		using namespace GroundTemperatureManager;
		using namespace ObjexxFCL::gio;

		// Locals
		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		bool found = false;
		int NumNums;
		int NumAlphas;
		int IOStat;

		// New shared pointer for this model object
		std::shared_ptr< SiteShallowGroundTemps > thisModel( new SiteShallowGroundTemps() );

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

	Real64
	SiteShallowGroundTemps::getGroundTemp()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   Summer 2011
		//       MODIFIED       Matt Mitchell, Summer 2015
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Return the ground temperature from Site:GroundTemperature:Shallow

		return surfaceGroundTemps( timeOfSimInMonths );
	}

	//******************************************************************************

	Real64
	SiteShallowGroundTemps::getGroundTempAtTimeInSeconds(
		Real64 const depth,
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

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 secPerMonth = 365 * 3600 * 24 / 12;
		int month;

		// Convert secs to months
		if ( seconds > 0.0 && seconds <= ( secPerMonth * 12 ) ) {
			month = ceil( seconds / ( secPerMonth * 12 ) );
		} else if ( seconds > ( secPerMonth * 12 ) ) {
			month = ceil( seconds / (secPerMonth * 12.0 ) );
			month = remainder( month, 12 );
		} else {
			ShowFatalError("Site:GroundTemperature:Shallow--Invalid time passed to ground temperature model");
		}

		timeOfSimInMonths = month;

		// Get and return ground temp
		return getGroundTemp();
	}

	//******************************************************************************

	Real64
	SiteShallowGroundTemps::getGroundTempAtTimeInMonths(
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

		timeOfSimInMonths = month;

		// Get and return ground temp
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
