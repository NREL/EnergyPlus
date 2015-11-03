#ifndef SortAndStringUtilities_hh_INCLUDED
#define SortAndStringUtilities_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace SortAndStringUtilities {

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
	);

	void
	QsortC(
		Array1S_string Alphas, // Alphas to be sorted
		Array1S_int iAlphas // Indexes of sorted array
	);

	void
	QsortPartition(
		Array1S_string Alphas, // Alphas to be sorted
		Array1S_int iAlphas, // Indexes of sorted array
		int & marker
	);

} // SortAndStringUtilities

} // EnergyPlus

#endif
