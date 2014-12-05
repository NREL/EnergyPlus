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
	// These are now external subroutines
	//PUBLIC  CalcHeatBalanceOutsideSurf  ! The heat balance routines are now public because the
	//PUBLIC  CalcHeatBalanceInsideSurf   ! radiant systems need access to them in order to simulate

	// Record Keeping/Utility Routines for Module

	// Reporting routines for module

	// Functions

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

} // HeatBalanceSurfaceManager

// *****************************************************************************
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************

// EXTERNAL SUBROUTINES (heavily related to HeatBalanceSurfaceManager)

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

// *****************************************************************************
// *****************************************************************************
// *****************************************************************************
// *****************************************************************************

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


} // EnergyPlus

#endif
