#ifndef HVACManager_hh_INCLUDED
#define HVACManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace HVACManager {

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// na

	//MODULE VARIABLE DECLARATIONS:

	extern int HVACManageIteration; // counts iterations to enforce maximum iteration limit
	extern int RepIterAir;

	//SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
	// and zone equipment simulations

	// Functions
	void
	clear_state();

	void
	ManageHVAC();

	void
	SimHVAC();

	void
	SimSelectedEquipment(
		bool & SimAirLoops, // True when the air loops need to be (re)simulated
		bool & SimZoneEquipment, // True when zone equipment components need to be (re)simulated
		bool & SimNonZoneEquipment, // True when non-zone equipment components need to be (re)simulated
		bool & SimPlantLoops, // True when the main plant loops need to be (re)simulated
		bool & SimElecCircuits, // True when electic circuits need to be (re)simulated
		bool & FirstHVACIteration, // True when solution technique on first iteration
		bool const LockPlantFlows
	);

	void
	ResetTerminalUnitFlowLimits();

	void
	ResolveAirLoopFlowLimits();

	void
	ResolveLockoutFlags( bool & SimAir ); // TRUE means air loops must be (re)simulated

	void
	ResetHVACControl();

	void
	ResetNodeData();

	void
	UpdateZoneListAndGroupLoads();

	void
	ReportAirHeatBalance();

	void
	SetHeatToReturnAirFlag();

	void
	UpdateZoneInletConvergenceLog();

} // HVACManager

} // EnergyPlus

#endif
