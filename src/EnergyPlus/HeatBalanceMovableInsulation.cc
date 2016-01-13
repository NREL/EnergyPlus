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

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <HeatBalanceMovableInsulation.hh>
#include <DataHeatBalance.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace HeatBalanceMovableInsulation {

	// Module containing the routines dealing with the HeatBalanceMovableInsulation

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   December 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to contain all of the routines associated with
	// movable and transparent insulation.

	// METHODOLOGY EMPLOYED:
	// See individual routines

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataHeatBalance::Material;
	using DataSurfaces::Surface;

	// Use statements for access to subroutines in other modules
	using ScheduleManager::GetCurrentScheduleValue;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	// SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceMovableInsulation

	// Functions

	void
	EvalOutsideMovableInsulation(
		int const SurfNum, // DO loop counter for surfaces
		Real64 & HMovInsul, // Resistance or "h" value of movable insulation
		int & RoughIndexMovInsul, // Roughness index of movable insulation
		Real64 & AbsExt // Absorptivity of outer most layer
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   March 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines whether or not outside movable insulation
		// on opaque surfaces is present at the current time.

		// METHODOLOGY EMPLOYED:
		// The SurfNum is passed in and then the rest of the parameters are set
		// if movable insulation is present.  If it is not present, then
		// HMovInsul is set to zero.

		// REFERENCES:
		// (I)BLAST legacy routine OMVINS

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 MovInsulSchedVal; // Value of the movable insulation schedule for current time

		// FLOW:
		MovInsulSchedVal = GetCurrentScheduleValue( Surface( SurfNum ).SchedMovInsulExt );

		if ( MovInsulSchedVal <= 0.0 ) { // Movable insulation not present at current time

			HMovInsul = 0.0;
			AbsExt = 0.0;

		} else { // Movable insulation present-->calculate output parameters

			// Double check resistance and conductivity to avoid divide by zero problems
			if ( ( Material( Surface( SurfNum ).MaterialMovInsulExt ).Resistance ) <= 0.0 ) {
				if ( ( Material( Surface( SurfNum ).MaterialMovInsulExt ).Conductivity ) > 0.0 ) {
					Material( Surface( SurfNum ).MaterialMovInsulExt ).Resistance = Material( Surface( SurfNum ).MaterialMovInsulExt ).Thickness / Material( Surface( SurfNum ).MaterialMovInsulExt ).Conductivity;
				} else {
					ShowFatalError( "EvalOutsideMovableInsulation: No resistance or conductivity found for material " + Material( Surface( SurfNum ).MaterialMovInsulExt ).Name );
				}
			}

			HMovInsul = 1.0 / ( MovInsulSchedVal * Material( Surface( SurfNum ).MaterialMovInsulExt ).Resistance );
			RoughIndexMovInsul = Material( Surface( SurfNum ).MaterialMovInsulExt ).Roughness;
			AbsExt = max( 0.0, 1.0 - Material( Surface( SurfNum ).MaterialMovInsulExt ).Trans - Material( Surface( SurfNum ).MaterialMovInsulExt ).ReflectSolBeamFront );

		}

	}

	void
	EvalInsideMovableInsulation(
		int const SurfNum, // DO loop counter for surfaces
		Real64 & HMovInsul, // Resistance or "h" value of movable insulation
		Real64 & AbsInt // Inside solar absorptance of movable insulation
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   March 1998
		//       MODIFIED       Nov. 1999, FW, add AbsInt; change MaterialMovInsulExt to
		//                      MaterialMovInsulInt
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines whether or not inside movable insulation
		// is present at the current time.

		// METHODOLOGY EMPLOYED:
		// The SurfNum is passed in and then the rest of the parameters are set
		// if movable insulation is present.  If it is not present, then
		// HMovInsul is set to zero.

		// REFERENCES:
		// (I)BLAST legacy routine IMVINS

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MovInsulSchedVal; // Value of the movable insulation schedule for current time

		// FLOW:
		MovInsulSchedVal = GetCurrentScheduleValue( Surface( SurfNum ).SchedMovInsulInt );

		if ( MovInsulSchedVal <= 0.0 ) { // Movable insulation not present at current time

			HMovInsul = 0.0;
			AbsInt = 0.0;

		} else { // Movable insulation present-->calculate output parameters

			if ( ( Material( Surface( SurfNum ).MaterialMovInsulInt ).Resistance ) <= 0.0 ) {
				if ( Material( Surface( SurfNum ).MaterialMovInsulInt ).Conductivity > 0.0 && Material( Surface( SurfNum ).MaterialMovInsulInt ).Thickness > 0.0 ) {
					Material( Surface( SurfNum ).MaterialMovInsulInt ).Resistance = Material( Surface( SurfNum ).MaterialMovInsulInt ).Thickness / Material( Surface( SurfNum ).MaterialMovInsulExt ).Conductivity;
				} else {
					ShowFatalError( "EvalInsideMovableInsulation: No resistance found for material " + Material( Surface( SurfNum ).MaterialMovInsulInt ).Name );
				}
			}

			HMovInsul = 1.0 / ( MovInsulSchedVal * Material( Surface( SurfNum ).MaterialMovInsulInt ).Resistance );
			AbsInt = Material( Surface( SurfNum ).MaterialMovInsulInt ).AbsorpSolar;

		}

	}

} // HeatBalanceMovableInsulation

} // EnergyPlus
