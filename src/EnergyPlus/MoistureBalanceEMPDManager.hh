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

} // MoistureBalanceEMPDManager

} // EnergyPlus

#endif
