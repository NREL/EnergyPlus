// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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

#ifndef HVACInterfaceManager_hh_INCLUDED
#define HVACInterfaceManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Plant/Enums.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace HVACInterfaceManager {

    // Common Pipe Recirc Flow Directions
    constexpr int NoRecircFlow(0);
    constexpr int PrimaryRecirc(1);   // flow from Supply-outlet/Demand-inlet to Supply-inlet/demand-outlet
    constexpr int SecondaryRecirc(2); // flow from Supply-inlet/Demand-outlet to Supply-outlet/demand-inlet

    enum class FlowType
    {
        Unassigned = -1,
        Constant,
        Variable
    };

    struct CommonPipeData
    {
        // Members
        DataPlant::iCommonPipeType CommonPipeType; // type of common pipe used if any
        FlowType SupplySideInletPumpType;
        FlowType DemandSideInletPumpType;
        // Following report variables are used in uncontrolled common pipe
        int FlowDir; // Direction in which flow is in Common Pipe
        Real64 Flow; // Flow in the Common Pipe
        Real64 Temp;
        // Following report variables are used in two way common pipe
        Real64 SecCPLegFlow;     // Mass flow in the secondary side Common pipe leg
        Real64 PriCPLegFlow;     // Mass flow in the primary side Common pipe leg
        Real64 SecToPriFlow;     // Mass flow in the pipe from Secondary to primary side
        Real64 PriToSecFlow;     // Mass flow in the pipe from primary to Secondary side
        Real64 PriInTemp;        // Temperature at primary inlet node
        Real64 PriOutTemp;       // Temperature at primary outlet node
        Real64 SecInTemp;        // Temperature at secondary inlet node
        Real64 SecOutTemp;       // Temperature at secondary outlet node
        Real64 PriInletSetPoint; // Setpoint at Primary inlet node
        Real64 SecInletSetPoint; // Setpoint at Secondary inlet node
        bool PriInletControlled; // True if Primary inlet node is controlled
        bool SecInletControlled; // True if secondary inlet is controlled
        Real64 PriFlowRequest;   // total flow request on supply side.

        // Default Constructor
        CommonPipeData()
            : CommonPipeType(DataPlant::iCommonPipeType::No), SupplySideInletPumpType(FlowType::Unassigned),
              DemandSideInletPumpType(FlowType::Unassigned), FlowDir(0), Flow(0.0), Temp(0.0), SecCPLegFlow(0.0), PriCPLegFlow(0.0),
              SecToPriFlow(0.0), PriToSecFlow(0.0), PriInTemp(0.0), PriOutTemp(0.0), SecInTemp(0.0), SecOutTemp(0.0), PriInletSetPoint(0.0),
              SecInletSetPoint(0.0), PriInletControlled(false), SecInletControlled(false), PriFlowRequest(0.0)
        {
        }
    };

    // Functions

    void UpdateHVACInterface(EnergyPlusData &state,
                             int AirLoopNum, // airloop number for which air loop this is
                             DataConvergParams::iCalledFrom CalledFrom,
                             int OutletNode,          // Node number for the outlet of the side of the loop just simulated
                             int InletNode,           // Node number for the inlet of the side that needs the outlet node data
                             bool &OutOfToleranceFlag // True when the other side of the loop need to be (re)simulated
    );

    //***************

    void UpdatePlantLoopInterface(EnergyPlusData &state,
                                  int LoopNum,                // The 'inlet/outlet node' loop number
                                  int ThisLoopSideNum,        // The 'outlet node' LoopSide number
                                  int ThisLoopSideOutletNode, // Node number for the inlet of the side that needs the outlet node data
                                  int OtherLoopSideInletNode, // Node number for the outlet of the side of the loop just simulated
                                  bool &OutOfToleranceFlag,   // True when the other side of the loop need to be (re)simulated
                                  DataPlant::iCommonPipeType CommonPipeType);

    //***************

    void UpdateHalfLoopInletTemp(EnergyPlusData &state, int LoopNum, int TankInletLoopSide, Real64 &TankOutletTemp);

    void
    UpdateCommonPipe(EnergyPlusData &state, int LoopNum, int TankInletLoopSide, DataPlant::iCommonPipeType CommonPipeType, Real64 &MixedOutletTemp);

    void ManageSingleCommonPipe(EnergyPlusData &state,
                                int LoopNum,            // plant loop number
                                int LoopSide,           // plant loop side number
                                Real64 TankOutletTemp,  // inlet temperature to the common pipe passed in from the capacitance calculation
                                Real64 &MixedOutletTemp // inlet temperature to the common pipe passed in from the capacitance calculation
    );

    void ManageTwoWayCommonPipe(EnergyPlusData &state, int LoopNum, int LoopSide, Real64 TankOutletTemp);

    void SetupCommonPipes(EnergyPlusData &state);

} // namespace HVACInterfaceManager

struct HVACInterfaceManagerData : BaseGlobalStruct
{

    bool CommonPipeSetupFinished = false;
    Array1D<HVACInterfaceManager::CommonPipeData> PlantCommonPipe;
    Array1D_bool MyEnvrnFlag_SingleCommonPipe;
    Array1D_bool MyEnvrnFlag_TwoWayCommonPipe;
    bool OneTimeData_SingleCommonPipe = true;
    bool OneTimeData_TwoWayCommonPipe = true;
    Array1D<Real64> TmpRealARR = Array1D<Real64>(DataConvergParams::ConvergLogStackDepth); // Tuned Made static

    void clear_state() override
    {
        this->CommonPipeSetupFinished = false;
        this->PlantCommonPipe.deallocate();
        this->MyEnvrnFlag_SingleCommonPipe.deallocate();
        this->MyEnvrnFlag_TwoWayCommonPipe.deallocate();
        this->OneTimeData_SingleCommonPipe = true;
        this->OneTimeData_TwoWayCommonPipe = true;
    }
};

} // namespace EnergyPlus

#endif
