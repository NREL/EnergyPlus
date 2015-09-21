#ifndef DataMoistureBalanceEMPD_hh_INCLUDED
#define DataMoistureBalanceEMPD_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataMoistureBalanceEMPD {

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS

	// Parameters for the definition and limitation of arrays:

	extern Real64 const Lam; // heat of adsorption for building materials

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	// Variables that are used in both the Surface Heat Balance and the Moisture Balance
	extern Array1D< Real64 > RVSurfaceOld; // Moisture level at interior surfaces at previous time step
	extern Array1D< Real64 > RVSurface; // Moisture level at interior surfaces at current interation
	// and current time step
	extern Array1D< Real64 > HeatFluxLatent; // Moisture flux at interior surfaces [W]
	extern Array1D< Real64 > RVSurfLayerOld;
	extern Array1D< Real64 > RVdeepOld;
	extern Array1D< Real64 > RVSurfLayer;
	extern Array1D< Real64 > RVDeepLayer;
	extern Array1D< Real64 > RVwall;

} // DataMoistureBalanceEMPD

} // EnergyPlus

#endif
