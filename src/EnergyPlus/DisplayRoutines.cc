// C++ Headers
#include <iostream>
#include <sstream>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <DisplayRoutines.hh>
#include <DataGlobals.hh>
#include <DataSystemVariables.hh>

namespace EnergyPlus {

void
DisplayString( std::string const & String ) // String to be displayed
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   Version 1.0
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine provides a call to display strings during program execution.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// na

	// Using/Aliasing
	using DataGlobals::KickOffSimulation;
	using DataSystemVariables::DeveloperFlag;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	// na

	if ( KickOffSimulation && ! DeveloperFlag ) return;
	std::cout << String << '\n';

}

void
DisplayString( char const * String ) // String to be displayed
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   Version 1.0
	//       MODIFIED       na
	//       RE-ENGINEERED  Overload to avoid std::string creation overhead

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine provides a call to display strings during program execution.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// na

	// Using/Aliasing
	using DataGlobals::KickOffSimulation;
	using DataGlobals::fMessagePtr;
	using DataSystemVariables::DeveloperFlag;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	// na

	if ( fMessagePtr ) fMessagePtr( String );

	if ( KickOffSimulation && ! DeveloperFlag ) return;
	std::cout << String << '\n';

}

void
DisplayNumberAndString(
	int const Number, // number to be displayed
	std::string const & String // String to be displayed
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   Version 1.0
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine provides a call to display (at set point on screen for screen positioning models) card images
	// during program parsing.

	// METHODOLOGY EMPLOYED:
	// usage:= call DisplayNumberAndString(numbr,string)

	// REFERENCES:
	// na

	// Using/Aliasing
	using DataGlobals::KickOffSimulation;
	using DataSystemVariables::DeveloperFlag;
	using DataGlobals::fMessagePtr;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	std::stringstream sstm;
	sstm << String << ' ' << Number;
	if ( fMessagePtr ) fMessagePtr( sstm.str() );

	if ( KickOffSimulation && ! DeveloperFlag ) return;
	std::cout << String << ' ' << Number << '\n';
}

void
DisplaySimDaysProgress( // This doesn't do anything!
	int const CurrentSimDay, // Current Simulation Day
	int const TotalSimDays // Total number of Simulation Days
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   Version 1.0
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS SUBROUTINE:
	// This subroutine provides a call for "progress" during simulation.
	// Progress is percent of current days vs total days.

	// METHODOLOGY EMPLOYED:
	// Needs description, as appropriate.

	// REFERENCES:
	// na

	// Using/Aliasing
	using DataGlobals::KickOffSimulation;
	using DataGlobals::fProgressPtr;
	using DataSystemVariables::DeveloperFlag;

	// Locals
	// SUBROUTINE ARGUMENT DEFINITIONS:

	// SUBROUTINE PARAMETER DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	static int percent( 0 ); // Current percent progress

	if ( KickOffSimulation && ! DeveloperFlag ) return;
	if ( TotalSimDays > 0 ) {
		percent = nint( ( ( float ) CurrentSimDay / ( float ) TotalSimDays ) * 100.0 );
		percent = min( percent, 100 );
	} else {
		percent = 0;
	}

	if ( fProgressPtr ) fProgressPtr( percent );

}

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


} // EnergyPlus
