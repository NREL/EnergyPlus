// EnergyPlus Headers
#include <DataCostEstimate.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataCostEstimate {
	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for Cost Estimation variables which are considered
	// to be "global" in nature in EnergyPlus.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	// CurntBldg holds results for current bldg. cost estimate
	// RefrnceBldg holds user input for comparison.

	int NumLineItems( 0 ); // number of cost estimate line items
	bool DoCostEstimate( false ); // set to true if any cost estimating needed

	int numMonetaryUnit( 0 );
	int selectedMonetaryUnit( 0 );

	// Object Data
	Array1D< CostLineItemStruct > CostLineItem;
	CostAdjustmentStruct CurntBldg( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ); // holds total from line item cost calculations | holds user-defined constant cost model | holds user-defined fraction for design fees | holds user-defined fraction for contractor fees | holds user-defined fraction for contingencies | holds user-defined fraction for bonding costs | holds user-defined fraction for commissioning costs | holds user-defined multiplier to account for regional diffs | the Grand Total of all line items plus all other costs
	CostAdjustmentStruct RefrncBldg( 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 ); // holds total from line item cost calculations | holds user-defined constant cost model | holds user-defined fraction for design fees | holds user-defined fraction for contractor fees | holds user-defined fraction for contingencies | holds user-defined fraction for bonding costs | holds user-defined fraction for commissioning costs | holds user-defined multiplier to account for regional diffs | the Grand Total of all line items plus all other costs
	Array1D< monetaryUnitType > monetaryUnit;

} // DataCostEstimate

} // EnergyPlus
