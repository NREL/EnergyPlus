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

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

} // HVACManager

} // EnergyPlus

#endif
