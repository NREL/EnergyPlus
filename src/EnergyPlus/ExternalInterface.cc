// EnergyPlus Headers
#include <ExternalInterface.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace ExternalInterface {

	// Module containing the routines dealing with the BCVTB interface

	// MODULE INFORMATION:
	//       AUTHOR         Michael Wetter
	//       DATE WRITTEN   5Jan2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Do nothing except show fatal error when an instance is made of an ExternalInterface
	// object.
	// This module replaces ExternalInterface.f90 when building EnergyPlus without the bcvtb LIB and DLL.
	// This should only be done during development. The official EnergyPlus release needs to be
	// compiled with ExternalInterface.f90, and linked to a dummy bcvtb LIB and DLL.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// http://simulationresearch.lbl.gov/bcvtb

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// na

	// Data
	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	bool haveExternalInterfaceBCVTB( false );

	// SUBROUTINE SPECIFICATIONS FOR MODULE ExternalInterface:
	int NumExternalInterfaces( 0 ); // Number of ExternalInterface objects

	//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	// Functions

	void
	ExternalInterfaceExchangeVariables()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   5Jan2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Exchanges variables between EnergyPlus and the BCVTB socket.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;

		// Locals
		static int NumExternalInterfaces( 0 ); // Number of ExternalInterface objects

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"

		if ( GetInputFlag ) {
			NumExternalInterfaces = GetNumObjectsFound( "ExternalInterface" );
			GetInputFlag = false;
			if ( NumExternalInterfaces > 0 ) {
				ShowFatalError( "ExternalInterface is not available in this version." );
			}
		}

	}

	void
	CloseSocket( int const FlagToWriteToSocket ) // flag to write to the socket
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Michael Wetter
		//       DATE WRITTEN   5Jan2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does nothing, but it is needed since EnergyPlus
		// may call CloseSocket when it terminates.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Locals
		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

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

} // ExternalInterface

} // EnergyPlus
