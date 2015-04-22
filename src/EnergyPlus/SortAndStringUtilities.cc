// C++ Headers
#include <utility>

// EnergyPlus Headers
#include <SortAndStringUtilities.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SortAndStringUtilities {

	// Module containing the routines dealing with Sorting

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   March 2009
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// <description>

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// <use statements for data only modules>
	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE SortUtilities

	// Functions

	void
	SetupAndSort(
		Array1S_string Alphas, // Alphas to be sorted
		Array1S_int iAlphas // Indexes of sorted array
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Set up and call sort routine for Alphas

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		for ( int Loop = 1, Loop_end = Alphas.u(); Loop <= Loop_end; ++Loop ) {
			iAlphas( Loop ) = Loop;
		}

		QsortC( Alphas, iAlphas );

	}

	void
	QsortC(
		Array1S_string Alphas, // Alphas to be sorted
		Array1S_int iAlphas // Indexes of sorted array
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Make sort order for an Alpha Array but store the pointers in an
		// accompanying integer array which must be filled prior to the first call
		// as this routine is recursive and called from within.

		// METHODOLOGY EMPLOYED:
		// recursion and quick-sort methodology

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		if ( Alphas.size() > 1 ) {
			int iq;
			QsortPartition( Alphas, iAlphas, iq );
			QsortC( Alphas( {_,iq-1} ), iAlphas( {_,iq-1} ) );
			QsortC( Alphas( {iq,_} ), iAlphas( {iq,_} ) );
		}

	}

	void
	QsortPartition(
		Array1S_string Alphas, // Alphas to be sorted
		Array1S_int iAlphas, // Indexes of sorted array
		int & marker
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		std::string const & cpivot( Alphas( 1 ) );
		int i = 0;
		int j = Alphas.isize() + 1;

		while ( true ) {
			--j;
			while ( true ) {
				if ( Alphas( j ) <= cpivot ) break;
				--j;
			}
			++i;
			while ( true ) {
				if ( Alphas( i ) >= cpivot ) break;
				++i;
			}
			if ( i < j ) { // Swap the strings at index i and j
				Alphas( i ).swap( Alphas( j ) );
				std::swap( iAlphas( i ), iAlphas( j ) );
			} else if ( i == j ) {
				marker = i + 1;
				return;
			} else {
				marker = i;
				return;
			}
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

} // SortAndStringUtilities

} // EnergyPlus
