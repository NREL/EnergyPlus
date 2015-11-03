#ifndef HeatBalanceSurfaceManager_hh_INCLUDED
#define HeatBalanceSurfaceManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace HeatBalanceSurfaceManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Initialization routines for module

	// Algorithms for the module
	// These old external subroutines have been moved into the namespace and are no longer externals
	// CalcHeatBalanceOutsideSurf  ! The heat balance routines are now public because the
	//  CalcHeatBalanceInsideSurf   ! radiant systems need access to them in order to simulate

	// Record Keeping/Utility Routines for Module

	// Reporting routines for module

	// Functions
	void
	clear_state();

	void
	ManageSurfaceHeatBalance();

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitSurfaceHeatBalance();

	void
	GatherForPredefinedReport();

	void
	AllocateSurfaceHeatBalArrays();

	void
	InitThermalAndFluxHistories();

	void
	InitSolarHeatGains();

	void
	InitIntSolarDistribution();

	void
	ComputeIntThermalAbsorpFactors();

	void
	ComputeIntSWAbsorpFactors();

	void
	ComputeDifSolExcZonesWIZWindows( int const NumberOfZones ); // Number of zones

	void
	InitEMSControlledSurfaceProperties();

	void
	InitEMSControlledConstructions();

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	// Beginning of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	void
	UpdateFinalSurfaceHeatBalance();

	void
	UpdateThermalHistories();

	void
	CalculateZoneMRT( Optional_int_const ZoneToResimulate = _ ); // if passed in, then only calculate surfaces that have this zone

	// End of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the HB Module
	// *****************************************************************************

	void
	ReportSurfaceHeatBalance();

	// End of Reporting subroutines for the HB Module
	// *****************************************************************************



// *****************************************************************************
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************

// Formerly EXTERNAL SUBROUTINES (heavily related to HeatBalanceSurfaceManager) but now moved into namespace HeatBalanceSurfaceManager

void
CalcHeatBalanceOutsideSurf( Optional_int_const ZoneToResimulate = _ ); // if passed in, then only calculate surfaces that have this zone

void
CalcHeatBalanceInsideSurf( Optional_int_const ZoneToResimulate = _ ); // if passed in, then only calculate surfaces that have this zone

void
CalcOutsideSurfTemp(
	int const SurfNum, // Surface number DO loop counter
	int const ZoneNum, // Zone number the current surface is attached to
	int const ConstrNum, // Construction index for the current surface
	Real64 const HMovInsul, // "Convection" coefficient of movable insulation
	Real64 const TempExt // Exterior temperature boundary condition
);

void
CalcExteriorVentedCavity( int const SurfNum ); // index of surface

void
GatherComponentLoadsSurfAbsFact();

} // HeatBalanceSurfaceManager
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************


} // EnergyPlus

#endif
