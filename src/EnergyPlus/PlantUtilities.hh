// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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
	clear_state();

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

} // PlantUtilities

} // EnergyPlus

#endif
