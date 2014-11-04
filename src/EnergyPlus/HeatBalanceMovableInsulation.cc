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

} // HeatBalanceMovableInsulation

} // EnergyPlus
