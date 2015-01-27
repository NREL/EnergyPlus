#ifndef PlantCondLoopOperation_hh_INCLUDED
#define PlantCondLoopOperation_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace PlantCondLoopOperation {

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const HeatingOp; // Constant for Heating Operation
	extern int const CoolingOp; // Constant for Cooling Operation
	extern int const DualOp; // Constant for Cooling or Heating Operation

	extern bool const TurnItemOn; // Convenient for calling TurnPlantItemOnOff instead of hardwired true/false
	extern bool const TurnItemOff; // Convenient for calling TurnPlantItemOnOff instead of hardwired true/false

	//MODULE VARIABLE DECLARATIONS:

	//SUBROUTINE SPECIFICATIONS FOR MODULE  !SUBROUTINE SPECIFICATIONS FOR MODULE
	//Driver Routines
	//Get Input Routines
	//Initialization Routines
	//Load Distribution/Calculation Routines

	//ON/OFF Utility Routines

	//PLANT EMS Utility Routines

	// Functions

	void
	ManagePlantLoadDistribution(
		int const LoopNum, // PlantLoop data structure loop counter
		int const LoopSideNum, // PlantLoop data structure LoopSide counter
		int const BranchNum, // PlantLoop data structure branch counter
		int const CompNum, // PlantLoop data structure component counter
		Real64 & LoopDemand,
		Real64 & RemLoopDemand,
		bool const FirstHVACIteration,
		bool & LoopShutDownFlag, // EMS flag to tell loop solver to shut down pumps
		Optional_bool LoadDistributionWasPerformed = _
	);

	// Beginning of GetInput subroutines for the Module
	//******************************************************************************

	void
	GetPlantOperationInput( bool & GetInputOK );

	void
	GetOperationSchemeInput();

	void
	FindRangeBasedOrUncontrolledInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	void
	FindDeltaTempRangeInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	void
	LoadEquipList(
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		int const ListNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	void
	FindCompSPInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	void
	GetUserDefinedOpSchemeInput(
		std::string & CurrentModuleObject, // for ease in renaming
		int const NumSchemes, // May be set here and passed on
		int const LoopNum, // May be set here and passed on
		int const SchemeNum, // May be set here and passed on
		bool & ErrorsFound // May be set here and passed on
	);

	// End of GetInput subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Plant Loop Module
	//******************************************************************************

	void
	InitLoadDistribution( bool const FirstHVACIteration );

	// End Initialization Section of the Plant Loop Module
	//******************************************************************************

	// Begin Load Calculation/Distribution Section of the Plant Loop Module
	//******************************************************************************

	void
	DistributePlantLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const CurSchemePtr, // use as index in PlantLoop()OpScheme() data structure
		int const ListPtr, // use as index in PlantLoop()OpScheme() data structure
		Real64 const LoopDemand,
		Real64 & RemLoopDemand
	);

	void
	AdjustChangeInLoadForLastStageUpperRangeLimit(
		int const LoopNum, // component topology
		int const CurOpSchemePtr, // currect active operation scheme
		int const CurEquipListPtr, // current equipment list
		Real64 & ChangeInLoad // positive magnitude of load change
	);

	void
	AdjustChangeInLoadByHowServed(
		int const LoopNum, // component topology
		int const LoopSideNum, // component topology
		int const BranchNum, // component topology
		int const CompNum, // component topology
		Real64 & ChangeInLoad // positive magnitude of load change
	);

	void
	FindCompSPLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		int const OpNum // index for Plant()%LoopSide()%Branch()%Comp()%OpScheme()
	);

	void
	DistributeUserDefinedPlantLoad(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		int const CurCompLevelOpNum, // index for Plant()%LoopSide()%Branch()%Comp()%OpScheme()
		int const CurSchemePtr,
		Real64 const LoopDemand,
		Real64 & RemLoopDemand
	);

	// End Load Calculation/Distribution Section of the Plant Loop Module
	//******************************************************************************

	//********************************

	Real64
	FindRangeVariable(
		int const LoopNum, // PlantLoop data structure loop counter
		int const CurSchemePtr, // set by PL()%LoopSide()%Branch()%Comp()%OpScheme()%OpSchemePtr
		int const CurSchemeType // identifier set in PlantData
	);

	//********************************

	// Begin Plant Loop ON/OFF Utility Subroutines
	//******************************************************************************

	void
	TurnOnPlantLoopPipes(
		int const LoopNum,
		int const LoopSideNum
	);

	void
	TurnOffLoopEquipment( int const LoopNum );

	void
	TurnOffLoopSideEquipment(
		int const LoopNum,
		int const LoopSideNum
	);

	// End Plant Loop ON/OFF Utility Subroutines
	//******************************************************************************

	// Begin Plant EMS Control Routines
	//******************************************************************************

	void
	SetupPlantEMSActuators();

	void
	ActivateEMSControls(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		bool & LoopShutDownFlag
	);

	void
	AdjustChangeInLoadByEMSControls(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		Real64 & ChangeInLoad // positive magnitude of load change
	);

	//*END PLANT EMS CONTROL ROUTINES!
	//******************************************************************************

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

} // PlantCondLoopOperation

} // EnergyPlus

#endif
