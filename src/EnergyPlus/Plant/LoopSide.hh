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

#ifndef PlantTopologyLoopSide_hh_INCLUDED
#define PlantTopologyLoopSide_hh_INCLUDED

#include <DataLoopNode.hh>
#include <Plant/Branch.hh>
#include <Plant/ConnectedLoopData.hh>
#include <Plant/LoopSidePumpInformation.hh>
#include <Plant/MixerData.hh>
#include <Plant/PlantConvergencePoint.hh>
#include <Plant/SplitterData.hh>

namespace EnergyPlus {
namespace DataPlant {

    struct HalfLoopData
    {
        // Members
        bool SimLoopSideNeeded;       // Determine whether or not to re-simulate this plant LoopSide
        bool SimZoneEquipNeeded;      // Plant requests resimulate zone HVAC equipment
        bool SimAirLoopsNeeded;       // Plant requests resimulate air loop HVAC equipment
        bool SimNonZoneEquipNeeded;   // Plant requests resimulate non zone Equip
        bool SimElectLoadCentrNeeded; // Plant requests resimulate generators
        bool OncePerTimeStepOperations;
        Real64 TimeElapsed;            // store time for dynamic updates for last time
        Real64 FlowRequest;            // Flow request in the half loop
        Real64 FlowRequestTemperature; // Average Flow request outlet Temp in the half loop
        // It's necessary to hold the values here since AIR and GROUND SPs aren't associated with either a node or a SP manager
        Real64 TempSetPoint;            // Loop temperature setpoint
        Real64 TempSetPointHi;          // High Loop temperature setpoint
        Real64 TempSetPointLo;          // Low Loop temperature setpoint
        Real64 TempInterfaceTankOutlet; // Used by interface manager in common pipe simulation
        // This is the temperature at the loop outlet linterface
        // with half-loop capacitance and pump heat accounted for.
        Real64 LastTempInterfaceTankOutlet;
        std::string BranchList;  // Branch list name for the half loop
        std::string ConnectList; // Connector list name for the half loop
        int TotalBranches;       // Total number of branches on the half loop
        int NodeNumIn;           // Node number for the inlet to this loop
        std::string NodeNameIn;  // Node name for the inlet to this loop
        int NodeNumOut;          // Node number for the outlet to this loop
        std::string NodeNameOut; // Node name for the outlet to this loop
        int NumSplitters;        // Number of splitters in the half loop
        int NumMixers;           // Number of mixers in the half loop
        bool SplitterExists;     // Logical Flag indication splitter exists in the half loop
        bool MixerExists;        // Logical Flag indication mixer exists in the half loop
        int TotalPumps;          // total number of pumps on the half loop
        bool BranchPumpsExist;   // logical flag indication branch pumps exist on half loop
        Array1D<LoopSidePumpInformation> Pumps;
        Real64 TotalPumpHeat; // [W] total heat addition by the pumps to place in "tank"
        bool BypassExists;
        bool InletNodeSetPt;
        bool OutletNodeSetPt;
        bool EMSCtrl;
        Real64 EMSValue;
        bool FlowRestrictionFlag; // Max available flow at the outlet of the half loop
        // is less than max available flow at inlet
        int FlowLock;                         // DSU
        int TotalConnected;                   // total number of other loops connected to this loop side
        Array1D<ConnectedLoopData> Connected; // DSU Other loops connected to this Loop side
        Array1D<BranchData> Branch;           // Branch data
        Array1D<SplitterData> Splitter;       // Data for splitter on branch (if any)
        Array1D<MixerData> Mixer;             // Data for splitter on branch (if any)
        bool HasPressureComponents;
        bool HasParallelPressComps;
        Real64 PressureDrop;
        Real64 PressureEffectiveK;
        int errCount_LoadWasntDist;
        int errIndex_LoadWasntDist;
        int errCount_LoadRemains;
        int errIndex_LoadRemains;
        Real64 LoopSideInlet_TankTemp;
        Real64 LoopSideInlet_MdotCpDeltaT;
        Real64 LoopSideInlet_McpDTdt;
        Real64 LoopSideInlet_CapExcessStorageTime;
        Real64 LoopSideInlet_CapExcessStorageTimeReport;
        Real64 LoopSideInlet_TotalTime;
        PlantConvergencePoint InletNode;
        PlantConvergencePoint OutletNode;

        // Default Constructor
        HalfLoopData()
            : SimLoopSideNeeded(true), SimZoneEquipNeeded(true), SimAirLoopsNeeded(true), SimNonZoneEquipNeeded(true), SimElectLoadCentrNeeded(true),
              OncePerTimeStepOperations(true), TimeElapsed(0.0), FlowRequest(0.0), FlowRequestTemperature(0.0),
              TempSetPoint(DataLoopNode::SensedNodeFlagValue), TempSetPointHi(DataLoopNode::SensedNodeFlagValue),
              TempSetPointLo(DataLoopNode::SensedNodeFlagValue), TempInterfaceTankOutlet(0.0), LastTempInterfaceTankOutlet(0.0), TotalBranches(0),
              NodeNumIn(0), NodeNumOut(0), NumSplitters(0), NumMixers(0), SplitterExists(false), MixerExists(false), TotalPumps(0),
              BranchPumpsExist(false), TotalPumpHeat(0.0), BypassExists(false), InletNodeSetPt(false), OutletNodeSetPt(false), EMSCtrl(false),
              FlowRestrictionFlag(false), FlowLock(0), TotalConnected(0), HasPressureComponents(false), HasParallelPressComps(false),
              PressureDrop(0.0), PressureEffectiveK(0.0), errCount_LoadWasntDist(0), errIndex_LoadWasntDist(0), errCount_LoadRemains(0),
              errIndex_LoadRemains(0), LoopSideInlet_TankTemp(0.0), LoopSideInlet_MdotCpDeltaT(0.0), LoopSideInlet_McpDTdt(0.0),
              LoopSideInlet_CapExcessStorageTime(0.0), LoopSideInlet_CapExcessStorageTimeReport(0.0), LoopSideInlet_TotalTime(0.0),
              InletNode(0.0, 0.0), OutletNode(0.0, 0.0)
        {
        }
    };
} // namespace DataPlant
} // namespace EnergyPlus

#endif
