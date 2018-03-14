// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

#ifndef PlantLoopSolver_hh_INCLUDED
#define PlantLoopSolver_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <Plant/LoopSide.hh>

namespace EnergyPlus {

namespace PlantLoopSolver {

	// MODULE VARIABLE DEFINITIONS
	extern Real64 InitialDemandToLoopSetPoint;
	extern Real64 CurrentAlterationsToDemand;
	extern Real64 UpdatedDemandToLoopSetPoint;
	extern Real64 LoadToLoopSetPointThatWasntMet; // Unmet Demand
	extern Real64 InitialDemandToLoopSetPointSAVED;
	extern int RefrigIndex; // Index denoting refrigerant used (possibly steam)

	struct Location
	{
		// Members
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		Location() :
				LoopNum( 0 ),
				LoopSideNum( 0 ),
				BranchNum( 0 ),
				CompNum( 0 )
		{}

	};

	struct PlantLoopSolverClass {

		struct m_FlowControlValidator {
			// Members
			bool Valid; // Assume true
			Location ErrorPoint; // Branch where the error was thrown
			std::string Reason; // Brief description of error

			// Default Constructor
			m_FlowControlValidator() :
					Valid(true) {}

		};

		m_FlowControlValidator
		ValidateFlowControlPaths(
				int const LoopNum,
				int const LoopSideNum
		);

		Real64
		SetupLoopFlowRequest(
				int const LoopNum,
				int const ThisSide,
				int const OtherSide
		);

		Real64
		DetermineLoopSideFlowRate(
				int LoopNum,
				int ThisSide,
				int ThisSideInletNode,
				Real64 ThisSideLoopFlowRequest
		);

		void
		SimulateAllLoopSideBranches(
				int const LoopNum,
				int const LoopSideNum,
				Real64 const ThisLoopSideFlow,
				bool const FirstHVACIteration,
				bool & LoopShutDownFlag
		);

		void
		SimulateLoopSideBranchGroup(
				int const LoopNum,
				int const LoopSideNum,
				int const FirstBranchNum,
				int const LastBranchNum,
				Real64 const FlowRequest,
				bool const FirstHVACIteration,
				bool & LoopShutDownFlag,
				bool const StartingNewLoopSidePass = false
		);

		void
		SimulateAllLoopSidePumps(
				int const LoopNum,
				int const ThisSide,
				Optional< Location const > SpecificPumpLocation = _,
				Optional< Real64 const > SpecificPumpFlowRate = _
		);

		Real64
		CalcOtherSideDemand(
				int const LoopNum,
				int const ThisSide
		);

		void
		DisableAnyBranchPumpsConnectedToUnloadedEquipment(
				int LoopNum,
				int ThisSide
		);

		void
		TurnOnAllLoopSideBranches(
				DataPlant::HalfLoopData & loop_side
		);

		void
		DoFlowAndLoadSolutionPass(
				int LoopNum,
				int ThisSide,
				int OtherSide,
				int ThisSideInletNode,
				bool FirstHVACIteration
		);

		Real64
		EvaluateLoopSetPointLoad(
				int const LoopNum,
				int const LoopSideNum,
				int const FirstBranchNum,
				int const LastBranchNum,
				Array1S_int LastComponentSimulated
		);

		void
		UpdateAnyLoopDemandAlterations(
				int const LoopNum,
				int const LoopSideNum,
				int const BranchNum,
				int const CompNum
		);

		void
		ResolveParallelFlows(
				int const LoopNum, // plant loop number that we are balancing flow for
				int const LoopSideNum, // plant loop number that we are balancing flow for
				Real64 const ThisLoopSideFlow, // [kg/s]  total flow to be split
				bool const FirstHVACIteration // TRUE if First HVAC iteration of Time step
		);

		Real64
		DetermineBranchFlowRequest(
				int const LoopNum,
				int const LoopSideNum,
				int const BranchNum
		);

		void
		UpdateLoopSideReportVars(
				int const LoopNum,
				int const LoopSide,
				Real64 const OtherSideDemand, // This is the 'other side' demand, based on other side flow
				Real64 const LocalRemLoopDemand // Unmet Demand after equipment has been simulated (report variable)
		);

		void
		PushBranchFlowCharacteristics(
				int const LoopNum,
				int const LoopSideNum,
				int const BranchNum,
				Real64 const ValueToPush,
				bool const FirstHVACIteration // TRUE if First HVAC iteration of Time step
		);

		void
		CalcUnmetPlantDemand(
				int const LoopNum,
				int const LoopSideNum
		);

		void
		CheckLoopExitNode(
				int const LoopNum, // plant loop counter
				bool const FirstHVACIteration // TRUE if First HVAC iteration of Time step
		);

		void
		AdjustPumpFlowRequestByEMSControls(
				int const LoopNum,
				int const LoopSideNum,
				int const BranchNum,
				int const CompNum,
				Real64 & FlowToRequest
		);

	};

	void
	clear_state();

	void
	PlantHalfLoopSolver(
		bool const FirstHVACIteration, // TRUE if First HVAC iteration of Time step
		int const LoopSideNum,
		int const LoopNum,
		bool & ReSimOtherSideNeeded
	);


} // PlantLoopSolver

} // EnergyPlus

#endif
