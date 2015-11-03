// EnergyPlus Headers
#include <DataViewFactorInformation.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataViewFactorInformation {

	// Module containing the data dealing with view factor information for ScriptF
	// and Diffuse Solar distribution calculations

	// MODULE INFORMATION:
	//       AUTHOR         Rob Hitchcock
	//       DATE WRITTEN   September 2007; Moved from HeatBalanceIntRadExchange
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
	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	// Object Data
	Array1D< ZoneViewFactorInformation > ZoneInfo;

} // DataViewFactorInformation

} // EnergyPlus
