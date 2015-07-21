#ifndef SimAirServingZones_hh_INCLUDED
#define SimAirServingZones_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace SimAirServingZones {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// coil operation
	extern int const CoilOn; // normal coil operation
	extern int const CoilOff; // signal coil shouldn't run
	extern int const BeforeBranchSim;
	extern int const AfterBranchSim;
	// CompType numerics -- for this module
	// component types addressed by this module
	extern int const OAMixer_Num;
	extern int const Fan_Simple_CV;
	extern int const Fan_Simple_VAV;
	extern int const WaterCoil_SimpleCool;
	extern int const WaterCoil_Cooling;
	extern int const WaterCoil_SimpleHeat;
	extern int const SteamCoil_AirHeat;
	extern int const WaterCoil_DetailedCool;
	extern int const Coil_ElectricHeat;
	extern int const Coil_GasHeat;
	extern int const WaterCoil_CoolingHXAsst;
	extern int const DXCoil_CoolingHXAsst;
	extern int const Coil_DeSuperHeat;
	extern int const DXSystem;
	extern int const HeatXchngr;
	extern int const Desiccant;
	extern int const Unglazed_SolarCollector;
	extern int const EvapCooler;
	extern int const UnitarySystem;
	extern int const Furnace_UnitarySys;
	extern int const Humidifier;
	extern int const Duct;
	extern int const UnitarySystem_BypassVAVSys;
	extern int const UnitarySystem_MSHeatPump;
	extern int const Fan_ComponentModel; // cpw22Aug2010 (new)
	extern int const DXHeatPumpSystem;
	extern int const CoilUserDefined;

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern bool GetAirLoopInputFlag; // Flag set to make sure you get input once
	extern int NumOfTimeStepInDay; // number of zone time steps in a day

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Simulation subroutines for the module

	// Functions

	void
	ManageAirLoops(
		bool const FirstHVACIteration, // TRUE if first full HVAC iteration in an HVAC timestep
		bool & SimAir, // TRUE means air loops must be (re)simulated
		bool & SimZoneEquipment // TRUE means zone equipment must be (re) simulated
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetAirPathData();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitAirLoops( bool const FirstHVACIteration ); // TRUE if first full HVAC iteration in an HVAC timestep

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimAirLoops(
		bool const FirstHVACIteration,
		bool & SimZoneEquipment
	);

	void
	SimAirLoop(
		bool const FirstHVACIteration,
		int const AirLoopNum,
		int const AirLoopPass,
		int & AirLoopIterMax,
		int & AirLoopIterTot,
		int & AirLoopNumCalls
	);

	void
	SolveAirLoopControllers(
		bool const FirstHVACIteration,
		int const AirLoopPass,
		int const AirLoopNum,
		bool & AirLoopConvergedFlag,
		int & IterMax,
		int & IterTot,
		int & NumCalls
	);

	void
	ReSolveAirLoopControllers(
		bool const FirstHVACIteration,
		int const AirLoopPass,
		int const AirLoopNum,
		bool & AirLoopConvergedFlag,
		int & IterMax,
		int & IterTot,
		int & NumCalls
	);

	void
	SimAirLoopComponents(
		int const AirLoopNum, // Index of the air loop being currently simulated
		bool const FirstHVACIteration // TRUE if first full HVAC iteration in an HVAC timestep
	);

	void
	SimAirLoopComponent(
		std::string const & CompName, // the component Name
		int const CompType_Num, // numeric equivalent for component type
		bool const FirstHVACIteration, // TRUE if first full HVAC iteration in an HVAC timestep
		int const AirLoopNum, // Primary air loop number
		int & CompIndex // numeric pointer for CompType/CompName -- passed back from other routines
	);

	void
	UpdateBranchConnections(
		int const AirLoopNum, // primary air system number
		int const BranchNum, // branch reference number
		int const Update // 1=BeforeBranchSim; 2=AfterBranchSim
	);

	void
	ResolveSysFlow(
		int const SysNum, // the primary air system number
		bool & SysReSim // Set to TRUE if mass balance fails and resimulation is needed
	);

	void
	SizeAirLoops();

	void
	SizeAirLoopBranches(
		int const AirLoopNum,
		int const BranchNum
	);

	void
	SetUpSysSizingArrays();

	void
	UpdateSysSizing( int const CallIndicator );

	void
	UpdateSysSizingForScalableInputs( int const AirLoopNum );

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the SimAir Module
	// *****************************************************************************

	//        End of Reporting subroutines for the SimAir Module
	// *****************************************************************************

	//        Utility Subroutines for the SimAir Module
	// *****************************************************************************

	//        End of Utility subroutines for the SimAir Module
	// *****************************************************************************

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

} // SimAirServingZones

} // EnergyPlus

#endif
