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

// EnergyPlus Headers
#include <DataHVACControllers.hh>

namespace EnergyPlus {

namespace DataHVACControllers {

	// MODULE INFORMATION:
	//       AUTHOR         Dimitri Curtil
	//       DATE WRITTEN   February 2006
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for all variables used by the HVAC controllers.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// None!--This module is USEd by all other modules; it should not USE anything.

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	int const ControllerSimple_Type( 1 );
	Array1D_string const ControllerTypes( 1, std::string( "Controller:WaterCoil" ) );

	// Controller action used in modules HVACControllers and ZoneControllers
	int const iNoAction( 0 );
	int const iReverseAction( 1 );
	int const iNormalAction( 2 );
	Array1D_string const ActionTypes( {0,2}, { "No action", "Reverse action", "Normal action" } );

	// Controller mode used in modules HVACControllers and ZoneControllers
	int const iModeWrongAction( -2 ); // Controller error. E.g., bad action
	int const iModeNone( -1 ); // Controller mode not yet determined
	int const iModeOff( 0 ); // Controller off (no air flow in loop)
	int const iModeInactive( 1 ); // Controller inactive (equip not available for current step)
	int const iModeActive( 2 ); // Controller active (schedule>0 and min<actuated<max)
	int const iModeMinActive( 3 ); // Controller active and min-constrained (equip available and actuated=min)
	int const iModeMaxActive( 4 ); // Controller active and max-constrained (equip available and actuated=max)

	int const iFirstMode( iModeWrongAction ); // First operating mode in range
	int const iLastMode( iModeMaxActive ); // Last operating mode in range
	Array1D_string const ControllerModeTypes( {-2,4}, { "Wrong action mode", "No controller mode", "Off controller mode", "Inactive controller mode", "Active unconstrained controller mode", "Active min-constrained controller mode", "Active max-constrained controller mode" } );

	// Controller operation used in module HVACControllers
	int const iControllerOpColdStart( 1 ); // Reset for cold start
	int const iControllerOpWarmRestart( 2 ); // Reset for warm restart with previous solution
	int const iControllerOpIterate( 3 ); // Check convergence and estimate next guess if needed
	int const iControllerOpEnd( 4 ); // Check convergence only and trace

	// Controller restart flag used in module HVACControllers
	int const iControllerWarmRestartNone( -1 ); // Indicates that warm restart was not attempted
	int const iControllerWarmRestartFail( 0 ); // Indicates that warm restart failed
	int const iControllerWarmRestartSuccess( 1 ); // Indicates that warm restart was successful

	// DERIVED TYPE DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

} // DataHVACControllers

} // EnergyPlus
