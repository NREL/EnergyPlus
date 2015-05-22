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
	extern Array1D< Real64 > RhoVapEMPD; // Inside Surface Vapor Density Reporting variable
	extern Array1D< Real64 > WSurfEMPD; // Inside Surface Humidity Ratio Reporting variable
	extern Array1D< Real64 > RHEMPD; // Inside Surface Relative Humidity Reporting variable

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
		Real64 const TempSurfInOld, // INSIDE SURFACE TEMPERATURE at previous time step.
		Real64 const TempZone, // Zone temperature at current time step.
		Real64 & TempSat // Satutare surface temperature.
	);

	void
	SolverMoistureBalanceEMPD(
		Real64 & VARNEW, // Value at current time step
		Real64 const VAROLD, // Value at previous time step
		Real64 const A, // Coefficient of time derivative in AdV/dt+BV=C
		Real64 const B, // Coefficienct of variable
		Real64 const C // Constant
	);

	void
	CloseMoistureBalanceEMPD();

	void
	UpdateMoistureBalanceEMPD( int const SurfNum ); // Surface number

	void
	ReportMoistureBalanceEMPD();

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

} // MoistureBalanceEMPDManager

} // EnergyPlus

#endif
