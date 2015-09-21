#ifndef MoistureBalanceEMPDManager_hh_INCLUDED
#define MoistureBalanceEMPDManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace MoistureBalanceEMPDManager {

	// Data
	// MODULE VARIABLE and Function DECLARATIONs

	struct EMPDReportVarsData {
		Real64 rv_surface;
		Real64 RH_surface_layer;
		Real64 RH_deep_layer;
		Real64 w_surface_layer;
		Real64 w_deep_layer;
		Real64 mass_flux_zone;
		Real64 mass_flux_deep;
		Real64 u_surface_layer;
		Real64 u_deep_layer;

		// Default constructor
		EMPDReportVarsData() :
		rv_surface( 0.015 ),
		RH_surface_layer( 0.0 ),
		RH_deep_layer( 0.0 ),
		w_surface_layer( 0.015 ),
		w_deep_layer( 0.015 ),
		mass_flux_zone( 0.0 ),
		mass_flux_deep( 0.0 ),
		u_surface_layer( 0.0 ),
		u_deep_layer( 0.0 )
		{}
	};

	extern Array1D< EMPDReportVarsData > EMPDREportVars; // Array of structs that hold the empd report vars data, one for each surface.

	// SUBROUTINE SPECIFICATION FOR MODULE MoistureBalanceEMPDManager

	// Functions

	void
	GetMoistureBalanceEMPDInput();

	void
	InitMoistureBalanceEMPD();

	void
	CalcMoistureBalanceEMPD(
		int const SurfNum,
		Real64 const TempSurfIn, // INSIDE SURFACE TEMPERATURE at current time step
		Real64 const TempZone, // Zone temperature at current time step.
		Real64 & TempSat // Satutare surface temperature.
	);

	void
	CloseMoistureBalanceEMPD();

	void
	UpdateMoistureBalanceEMPD( int const SurfNum ); // Surface number

	void
	ReportMoistureBalanceEMPD();

	//     NOTICE

	//     Copyright (c) 1996-2015 The Board of Trustees of the University of Illinois
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

} // MoistureBalanceEMPDManager

} // EnergyPlus

#endif
