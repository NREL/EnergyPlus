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

#ifndef PlantOperationEquipAndOperations_hh_INCLUDED
#define PlantOperationEquipAndOperations_hh_INCLUDED

#include <PlantComponent.hh>

namespace EnergyPlus {
	namespace DataPlant {

		struct EquipListPtrData {
			// Members
			int ListPtr; // points to List on OpScheme on plant loop:
			// PlantLoop(LoopNum)%OpScheme(Optschemeptr)%List(ListPtr)...
			int CompPtr; // points to this component on List on OpScheme on plant loop:
			// PlantLoop(LoopNum)%OpScheme(Optschemeptr)%List(ListPtr)%Comp(compPtr)

			// Default Constructor
			EquipListPtrData() :
					ListPtr(0),
					CompPtr(0) {}

		};

		struct OpSchemePtrData {
			// Members
			int OpSchemePtr; // DSU points to OpScheme on plant loop:
			// PlantLoop(LoopNum)%OpScheme(Optschemeptr)...
			int NumEquipLists; // DSU ALLOCATABLE to the schedule (for valid schedules)
			Array1D <EquipListPtrData> EquipList; // DSU Component  list

			// Default Constructor
			OpSchemePtrData() :
					OpSchemePtr(0),
					NumEquipLists(0) {}

		};

		struct EquipListCompData {
			// Members
			std::string Name; // The name of each item in the list
			std::string TypeOf; // The name of each item in the list
			int TypeOf_Num;
			std::string CtrlType; // CoolingOp, HeatingOp, DualOp
			int CtrlTypeNum; // CoolingOp, HeatingOp, DualOp
			int LoopNumPtr; // pointer to the comp location in the data structure
			int LoopSideNumPtr; // pointer to the comp location in the data structure
			int BranchNumPtr; // pointer to the comp location in the data structure
			int CompNumPtr; // pointer to the comp location in the data structure
			Real64 SetPointFlowRate; // COMP SETPOINT CTRL ONLY--load calculation comp flow rate
			std::string DemandNodeName; // COMP SETPOINT CTRL ONLY--The name of each item in the list
			int DemandNodeNum; // COMP SETPOINT CTRL ONLY--The 'keyWord' identifying each item in list
			std::string SetPointNodeName; // COMP SETPOINT CTRL ONLY--The name of each item in the list
			int SetPointNodeNum; // COMP SETPOINT CTRL ONLY--The 'keyWord' identifying each item in list
			Real64 EMSIntVarRemainingLoadValue; // EMS internal variable remaining load, neg cooling [W]
			Real64 EMSActuatorDispatchedLoadValue; // EMS actuator for dispatched load, neg= cooling [W]

			// Default Constructor
			EquipListCompData() :
					TypeOf_Num(0),
					SetPointFlowRate(0.0),
					EMSIntVarRemainingLoadValue(0.0),
					EMSActuatorDispatchedLoadValue(0.0) {}

		};

		struct EquipOpList // DSU
		{
			// Members
			std::string Name; // The name of each item in the list
			Real64 RangeUpperLimit; // for range based controls
			Real64 RangeLowerLimit; // for range based controls
			int NumComps; // ALLOCATABLE to the schedule (for valid schedules)
			Array1D <EquipListCompData> Comp; // Component type list

			// Default Constructor
			EquipOpList() :
					RangeUpperLimit(0.0),
					RangeLowerLimit(0.0),
					NumComps(0) {}

		};

		struct OperationData // DSU
		{
			// Members
			std::string Name; // The name of each item in the list
			std::string TypeOf; // The 'keyWord' identifying each item in the list
			int OpSchemeType; // Op scheme type (from keyword)
			std::string Sched; // The name of the schedule associated with the list
			int SchedPtr; // ALLOCATABLE to the schedule (for valid schedules)
			bool Available; // TRUE = designated component or operation scheme available
			int NumEquipLists; // number of equipment lists
			int CurListPtr; // points to the current equipment list
			Array1D <EquipOpList> EquipList; // Component type list
			int EquipListNumForLastStage; // points to the equipment list with the highest upper limit
			std::string ReferenceNodeName; // DELTA CTRL ONLY--for calculation of delta Temp
			int ReferenceNodeNumber; // DELTA CTRL ONLY--for calculation of delta Temp
			int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
			int ErlInitProgramMngr; // EMS:ProgramManager to run when this model is initialized and setup
			Real64 EMSIntVarLoopDemandRate; // EMS internal variable for loop-level demand rate, neg cooling [W]
			bool MyEnvrnFlag;

			// Default Constructor
			OperationData() :
					OpSchemeType(0),
					SchedPtr(0),
					Available(false),
					NumEquipLists(0),
					CurListPtr(0),
					EquipListNumForLastStage(0),
					ErlSimProgramMngr(0),
					ErlInitProgramMngr(0),
					EMSIntVarLoopDemandRate(0.0),
					MyEnvrnFlag(true) {}

		};
	}
}

#endif
