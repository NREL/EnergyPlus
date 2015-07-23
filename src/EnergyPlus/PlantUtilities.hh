#ifndef PlantUtilities_hh_INCLUDED
#define PlantUtilities_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace PlantUtilities {

	// Functions

	void
	InitComponentNodes(
		Real64 const MinCompMdot,
		Real64 const MaxCompMdot,
		int const InletNode, // component's inlet node index in node structure
		int const OutletNode, // component's outlet node index in node structure
		int const LoopNum, // plant loop index for PlantLoop structure
		int const LoopSideNum, // Loop side index for PlantLoop structure
		int const BranchIndex, // branch index for PlantLoop
		int const CompIndex // component index for PlantLoop
	);

	void
	SetComponentFlowRate(
		Real64 & CompFlow, // [kg/s]
		int const InletNode, // component's inlet node index in node structure
		int const OutletNode, // component's outlet node index in node structure
		int const LoopNum, // plant loop index for PlantLoop structure
		int const LoopSideNum, // Loop side index for PlantLoop structure
		int const BranchIndex, // branch index for PlantLoop
		int const CompIndex // component index for PlantLoop
	);

	void
	SetActuatedBranchFlowRate(
		Real64 & CompFlow,
		int const ActuatedNode,
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		bool const ResetMode // flag to indicate if this is a real flow set, or a reset flow setting.
	);

	Real64
	RegulateCondenserCompFlowReqOp(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		Real64 const TentativeFlowRequest
	);

	void
	UpdatePlantMixer(
		int const LoopNum,
		int const LoopSideNum,
		int const MixNum
	);

	bool
	AnyPlantSplitterMixerLacksContinuity();

	void
	CheckPlantMixerSplitterConsistency(
		int const LoopNum,
		int const LoopSideNum,
		int const SplitNum,
		int const MixNum,
		bool const FirstHVACIteration
	);

	void
	CheckForRunawayPlantTemps(
		int const LoopNum,
		int const LoopSideNum
	);

	void
	UpdatePlantSplitter(
		int const LoopNum,
		int const LoopSideNum,
		int const SplitNum
	);

	void
	SetAllFlowLocks( int const Value );

	void
	ResetAllPlantInterConnectFlags();

	void
	PullCompInterconnectTrigger(
		int const LoopNum, // component's loop index
		int const LoopSide, // component's loop side number
		int const BranchNum, // Component's branch number
		int const CompNum, // Component's comp number
		int & UniqueCriteriaCheckIndex, // An integer given to this particular check
		int const ConnectedLoopNum, // Component's interconnected loop number
		int const ConnectedLoopSide, // Component's interconnected loop side number
		int const CriteriaType, // The criteria check to use, see DataPlant: SimFlagCriteriaTypes
		Real64 const CriteriaValue // The value of the criteria check to evaluate
	);

	void
	UpdateChillerComponentCondenserSide(
		int const LoopNum, // component's loop index
		int const LoopSide, // component's loop side number
		int const TypeOfNum, // Component's type index
		int const InletNodeNum, // Component's inlet node pointer
		int const OutletNodeNum, // Component's outlet node pointer
		Real64 const ModelCondenserHeatRate, // model's heat rejection rate at condenser (W)
		Real64 const ModelInletTemp, // model's inlet temperature (C)
		Real64 const ModelOutletTemp, // model's outlet temperature (C)
		Real64 const ModelMassFlowRate, // model's condenser water mass flow rate (kg/s)
		bool const FirstHVACIteration
	);

	void
	UpdateComponentHeatRecoverySide(
		int const LoopNum, // component's loop index
		int const LoopSide, // component's loop side number
		int const TypeOfNum, // Component's type index
		int const InletNodeNum, // Component's inlet node pointer
		int const OutletNodeNum, // Component's outlet node pointer
		Real64 const ModelRecoveryHeatRate, // model's heat rejection rate at recovery (W)
		Real64 const ModelInletTemp, // model's inlet temperature (C)
		Real64 const ModelOutletTemp, // model's outlet temperature (C)
		Real64 const ModelMassFlowRate, // model's condenser water mass flow rate (kg/s)
		bool const FirstHVACIteration
	);

	void
	UpdateAbsorberChillerComponentGeneratorSide(
		int const LoopNum, // component's loop index
		int const LoopSide, // component's loop side number
		int const TypeOfNum, // Component's type index
		int const InletNodeNum, // Component's inlet node pointer
		int const OutletNodeNum, // Component's outlet node pointer
		int const HeatSourceType, // Type of fluid in Generator loop
		Real64 const ModelGeneratorHeatRate, // model's generator heat rate (W)
		Real64 const ModelMassFlowRate, // model's generator mass flow rate (kg/s)
		bool const FirstHVACIteration
	);

	void
	InterConnectTwoPlantLoopSides(
		int const Loop1Num,
		int const Loop1LoopSideNum,
		int const Loop2Num,
		int const Loop2LoopSideNum,
		int const PlantComponentTypeOfNum,
		bool const Loop1DemandsOnLoop2
	);

	void
	ShiftPlantLoopSideCallingOrder(
		int const OldIndex,
		int const NewIndex
	);

	void
	RegisterPlantCompDesignFlow(
		int const ComponentInletNodeNum, // the component's water inlet node number
		Real64 const DesPlantFlow // the component's design fluid volume flow rate [m3/s]
	);

	void
	SafeCopyPlantNode(
		int const InletNodeNum,
		int const OutletNodeNum,
		Optional_int_const LoopNum = _,
		Optional< Real64 const > OutletTemp = _ // set on outlet node if present and water.
	);

	Real64
	BoundValueToNodeMinMaxAvail(
		Real64 const ValueToBound,
		int const NodeNumToBoundWith
	);

	void
	TightenNodeMinMaxAvails(
		int const NodeNum,
		Real64 const NewMinAvail,
		Real64 const NewMaxAvail
	);

	Real64
	BoundValueToWithinTwoValues(
		Real64 const ValueToBound,
		Real64 const LowerBound,
		Real64 const UpperBound
	);

	bool
	IntegerIsWithinTwoValues(
		int const ValueToCheck,
		int const LowerBound,
		int const UpperBound
	);

	void
	LogPlantConvergencePoints( bool const FirstHVACIteration );

	bool
	CheckPlantConvergence(
		int const ThisLoopNum,
		int const ThisLoopSide,
		bool const FirstHVACIteration
	);

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

} // PlantUtilities

} // EnergyPlus

#endif
