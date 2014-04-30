// EnergyPlus Headers
#include <DElightManagerF.hh>
#include <DataDElight.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DElightManagerF {

	// MODULE INFORMATION
	//       AUTHOR         Robert J. Hitchcock
	//       DATE WRITTEN   August 2003
	//       MODIFIED       January 2004
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:

	// Do nothing except show fatal error when calls are made to DElight subroutines.
	// Fatal errors will only be generated when a user include DElight objects in the input IDF.
	// This module replaces DElightManagerF.f90 when building EnergyPlus without the DElight LIB and DLL.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES:

	// USE STATEMENTS:
	// <use statements for data only modules>
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataDElight;

	// MODULE VARIABLE DECLARATIONS:

	// SUBROUTINE SPECIFICATIONS FOR MODULE DElightManagerF
	// MODULE SUBROUTINES:

	// Functions

	void
	GenerateDElightDaylightCoefficients(
		Real64 & dBldgLat,
		int & iErrorFlag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// The purpose of this subroutine is to provide an envelop to the DElightDaylightCoefficients
		// routine (and remove IEEE calls from main EnergyPlus core routines).

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		ShowFatalError( "DElight is not available in this version" );

	}

	void
	DElightDaylightCoefficients(
		Real64 & dBldgLat,
		int & iErrorFlag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   January 2004
		//       MODIFIED       February 2004 - Remove ProgramPath StringLength arguments
		//                       RJH - Jul 2004 - Add error flag parameter to match new DElight call
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does nothing but generate an error message when calls are made.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		ShowFatalError( "DElight is not available in this version" );

	}

	void
	DElightElecLtgCtrl(
		int const iNameLength,
		std::string const & cZoneName,
		Real64 const dBldgLat,
		Real64 const dHISKF,
		Real64 const dHISUNF,
		Real64 const dCloudFraction,
		Real64 const dSOLCOSX,
		Real64 const dSOLCOSY,
		Real64 const dSOLCOSZ,
		Real64 const pdPowerReducFac,
		int const iErrorFlag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   January 2004
		//       MODIFIED       RJH - Jul 2004 - Add error flag parameter to match new DElight call
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does nothing but generate an error message when calls are made.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		//    REAL(r64) rBldgLat

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		ShowFatalError( "DElight is not available in this version" );

	}

	void
	DElightFreeMemory()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does nothing but generate an error message when calls are made.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	}

	void
	DElightOutputGenerator( int & iOutputFlag )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does nothing but generate an error message when calls are made.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	}

	void
	DElightInputGenerator()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   January 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does nothing but generate an error message when calls are made.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		ShowFatalError( "DElight is not available in this version" );

	}

	void
	SetupDElightOutput4EPlus()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   February 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does nothing but generate an error message when calls are made.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		ShowFatalError( "DElight is not available in this version" );

	}

	std::string
	ReplaceBlanksWithUnderscores( std::string const & InputString ) // Input String
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Robert J. Hitchcock
		//       DATE WRITTEN   August 2003
		//       MODIFIED       From MakeUPPERCase function by Linda K. Lawrie
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataGlobals;

		// Return value
		std::string ResultString; // Result String

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		ResultString = InputString;

		return ResultString;

	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // DElightManagerF

} // EnergyPlus
