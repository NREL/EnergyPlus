// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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
	std::cout << String << std::endl;

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
	std::cout << String << std::endl;

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
	std::cout << String << ' ' << Number << std::endl;
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


} // EnergyPlus
