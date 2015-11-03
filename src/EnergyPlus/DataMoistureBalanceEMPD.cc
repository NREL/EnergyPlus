// EnergyPlus Headers
#include <DataMoistureBalanceEMPD.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataMoistureBalanceEMPD {

	// MODULE INFORMATION:
	//       AUTHOR         Muthusamy V. Swami and Lixing Gu
	//       DATE WRITTEN   Aug. 2000
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module should contain the information that is needed to calculate
	// moisture level at interior surfaces

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS

	// Parameters for the definition and limitation of arrays:

	Real64 const Lam( 2500000.0 ); // heat of adsorption for building materials

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	// Variables that are used in both the Surface Heat Balance and the Moisture Balance
	Array1D< Real64 > MoistEMPDOld; // Moisture level at interior surfaces at previous time step
	Array1D< Real64 > MoistEMPDInt; // Moisture level at interior surfaces at previous interation
	// and current time step
	Array1D< Real64 > MoistEMPDNew; // Moisture level at interior surfaces at current interation
	// and current time step
	Array1D< Real64 > MoistEMPDFlux; // Moisture flux at interior surfaces [W]

} // DataMoistureBalanceEMPD

} // EnergyPlus
