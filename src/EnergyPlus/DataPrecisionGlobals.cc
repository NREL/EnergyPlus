// ObjexxFCL Headers
#include <ObjexxFCL/numeric.hh>

// EnergyPlus Headers
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataPrecisionGlobals {

	// Module containing the routines dealing with the precision of data in EnergyPlus

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   January 2008
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module allows for setting the default precision to "double precision" using
	// F95 KIND and parameters.  Should it ever be necessary to try a higher precision, it
	// will be easy to switch for testing.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// na

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const i32( selected_int_kind( 6 ) ); // 6 digits
	int const i64( selected_int_kind( 12 ) ); // 12 digits
	int const r32( kind( 1.0 ) );
	int const r64( kind( 1.0 ) );
	int const default_prec( r64 );
	Real64 const constant_zero( 0.0 );
	Real64 const constant_one( 1.0 );
	Real64 const constant_minusone( -1.0 );
	Real64 const constant_twenty( 20.0 );
	Real64 const constant_pointfive( 0.5 );
	Real64 const EXP_LowerLimit( -20.0 ); // In IVF=2.061153622438558d-009 - used 20
	// because it's already used in other parts of the code
	Real64 const EXP_UpperLimit( 40.0 ); // In IVF=2.353852668370200d+017

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE:
	// na

	//     NOTICE
	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

} // DataPrecisionGlobals

} // EnergyPlus
