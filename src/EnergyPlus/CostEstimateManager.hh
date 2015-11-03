#ifndef CostEstimateManager_hh_INCLUDED
#define CostEstimateManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace CostEstimateManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Functions

	void
	SimCostEstimate();

	void
	GetCostEstimateInput();

	void
	CheckCostEstimateInput( bool & ErrorsFound ); // Set to true if errors in input, fatal at end of routine

	void
	CalcCostEstimate();

} // CostEstimateManager

} // EnergyPlus

#endif
