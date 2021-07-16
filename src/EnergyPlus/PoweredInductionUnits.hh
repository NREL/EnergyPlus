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

#ifndef PoweredInductionUnits_hh_INCLUDED
#define PoweredInductionUnits_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace PoweredInductionUnits {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS
    // coil types in this module
    enum class iHCoilType
    {
        Unassigned,
        Gas,
        Electric,
        SimpleHeating,
        SteamAirHeating,
    };

    struct PowIndUnitData
    {
        // Members
        // input data
        std::string Name;                                  // name of unit
        std::string UnitType;                              // type of unit
        DataDefineEquip::iZnAirLoopEquipType UnitType_Num; // index for type of unit
        std::string Sched;                                 // availability schedule
        int SchedPtr;                                      // index to schedule
        Real64 MaxTotAirVolFlow;                           // m3/s  (series)
        Real64 MaxTotAirMassFlow;                          // kg/s  (series)
        Real64 MaxPriAirVolFlow;                           // m3/s
        Real64 MaxPriAirMassFlow;                          // kg/s
        Real64 MinPriAirFlowFrac;                          // minimum primary air flow fraction
        Real64 MinPriAirMassFlow;                          // kg/s
        Real64 PriDamperPosition;                          // primary air damper position
        Real64 MaxSecAirVolFlow;                           // m3/s (parallel)
        Real64 MaxSecAirMassFlow;                          // kg/s (parallel)
        Real64 FanOnFlowFrac;                              // frac of primary air flow at which fan turns on (parallel)
        Real64 FanOnAirMassFlow;                           // primary air mass flow rate at which fan turns on (parallel)
        int PriAirInNode;                                  // unit primary air inlet node number
        int SecAirInNode;                                  // unit secondary air inlet node number
        int OutAirNode;                                    // unit air outlet node number
        int HCoilInAirNode;                                // unit mixed air node number
        int ControlCompTypeNum;
        int CompErrIndex;
        std::string MixerName;    // name of air mixer component
        int Mixer_Num;            // index for type of mixer
        std::string FanName;      // name of fan component
        int Fan_Num;              // index for fan type
        int Fan_Index;            // store index for this fan
        int FanAvailSchedPtr;     // index to fan availability schedule
        std::string HCoilType;    // type of heating coil component
        iHCoilType HCoilType_Num; // index for heating coil type
        int HCoil_PlantTypeNum;
        std::string HCoil; // name of heating coil component
        int HCoil_Index;   // index to this heating coil
        int HCoil_FluidIndex;
        Real64 MaxVolHotWaterFlow; // m3/s
        Real64 MaxVolHotSteamFlow; // m3/s
        Real64 MaxHotWaterFlow;    // kg/s
        Real64 MaxHotSteamFlow;    // kg/s
        Real64 MinVolHotWaterFlow; // m3/s
        Real64 MinHotSteamFlow;    // kg/s
        Real64 MinVolHotSteamFlow; // m3/s
        Real64 MinHotWaterFlow;    // kg/s
        int HotControlNode;        // hot water control node
        int HotCoilOutNodeNum;     // outlet of coil
        Real64 HotControlOffset;   // control tolerance
        int HWLoopNum;             // index for plant loop with hot plant coil
        int HWLoopSide;            // index for plant loop side for hot plant coil
        int HWBranchNum;           // index for plant branch for hot plant coil
        int HWCompNum;             // index for plant component for hot plant coil
        int ADUNum;                // index of corresponding air distribution unit
        bool InducesPlenumAir;     // True if secondary air comes from the plenum
        // Report data
        Real64 HeatingRate;        // unit heat addition rate to zone [W]
        Real64 HeatingEnergy;      // unit heat addition to zone [J]
        Real64 SensCoolRate;       // unit sensible heat removal rate from zone [W]
        Real64 SensCoolEnergy;     // unit sensible heat removal from zone [J]
        int CtrlZoneNum;           // index to control zone
        int ctrlZoneInNodeIndex;   // index to the control zone inlet node
        int AirLoopNum;            // index for the air loop that this terminal is connected to.
        Real64 OutdoorAirFlowRate; // zone outdoor air volume flow rate

        // Default Constructor
        PowIndUnitData()
            : UnitType_Num(DataDefineEquip::iZnAirLoopEquipType::Unassigned), SchedPtr(0), MaxTotAirVolFlow(0.0), MaxTotAirMassFlow(0.0),
              MaxPriAirVolFlow(0.0), MaxPriAirMassFlow(0.0), MinPriAirFlowFrac(0.0), MinPriAirMassFlow(0.0), PriDamperPosition(0.0),
              MaxSecAirVolFlow(0.0), MaxSecAirMassFlow(0.0), FanOnFlowFrac(0.0), FanOnAirMassFlow(0.0), PriAirInNode(0), SecAirInNode(0),
              OutAirNode(0), HCoilInAirNode(0), ControlCompTypeNum(0), CompErrIndex(0), Mixer_Num(0), Fan_Num(0), Fan_Index(0), FanAvailSchedPtr(0),
              HCoilType_Num(iHCoilType::Unassigned), HCoil_PlantTypeNum(0), HCoil_Index(0), HCoil_FluidIndex(0), MaxVolHotWaterFlow(0.0),
              MaxVolHotSteamFlow(0.0), MaxHotWaterFlow(0.0), MaxHotSteamFlow(0.0), MinVolHotWaterFlow(0.0), MinHotSteamFlow(0.0),
              MinVolHotSteamFlow(0.0), MinHotWaterFlow(0.0), HotControlNode(0), HotCoilOutNodeNum(0), HotControlOffset(0.0), HWLoopNum(0),
              HWLoopSide(0), HWBranchNum(0), HWCompNum(0), ADUNum(0), InducesPlenumAir(false), HeatingRate(0.0), HeatingEnergy(0.0),
              SensCoolRate(0.0), SensCoolEnergy(0.0), CtrlZoneNum(0), ctrlZoneInNodeIndex(0), AirLoopNum(0), OutdoorAirFlowRate(0.0)
        {
        }

        void CalcOutdoorAirVolumeFlowRate(EnergyPlusData &state);
    };

    void SimPIU(EnergyPlusData &state,
                std::string_view CompName, // name of the PIU
                bool FirstHVACIteration,   // TRUE if first HVAC iteration in time step
                int ZoneNum,               // index of zone served by PIU
                int ZoneNodeNum,           // zone node number of zone served by PIU
                int &CompIndex             // PIU Index in PIU names
    );

    void GetPIUs(EnergyPlusData &state);

    void InitPIU(EnergyPlusData &state,
                 int PIUNum,             // number of the current fan coil unit being simulated
                 bool FirstHVACIteration // TRUE if first zone equip this HVAC step
    );

    void SizePIU(EnergyPlusData &state, int PIUNum);

    void CalcSeriesPIU(EnergyPlusData &state,
                       int PIUNum,             // number of the current PIU being simulated
                       int ZoneNum,            // number of zone being served
                       int ZoneNode,           // zone node number
                       bool FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
    );

    void CalcParallelPIU(EnergyPlusData &state,
                         int PIUNum,             // number of the current PIU being simulated
                         int ZoneNum,            // number of zone being served
                         int ZoneNode,           // zone node number
                         bool FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
    );

    void ReportPIU(EnergyPlusData &state, int PIUNum); // number of the current fan coil unit being simulated

    // ===================== Utilities =====================================

    bool PIUnitHasMixer(EnergyPlusData &state, std::string_view CompName); // component (mixer) name

    void PIUInducesPlenumAir(EnergyPlusData &state, int NodeNum); // induced air node number

} // namespace PoweredInductionUnits

struct PoweredInductionUnitsData : BaseGlobalStruct
{

    Array1D_bool CheckEquipName;
    bool GetPIUInputFlag = true;
    bool MyOneTimeFlag = true;
    bool ZoneEquipmentListChecked = false;
    int NumPIUs = 0;
    int NumSeriesPIUs = 0;
    int NumParallelPIUs = 0;
    Array1D<PoweredInductionUnits::PowIndUnitData> PIU;
    std::unordered_map<std::string, std::string> PiuUniqueNames;
    Array1D_bool MyEnvrnFlag;
    Array1D_bool MySizeFlag;
    Array1D_bool MyPlantScanFlag;

    void clear_state() override
    {
        this->CheckEquipName.deallocate();
        this->GetPIUInputFlag = true;
        this->MyOneTimeFlag = true;
        this->ZoneEquipmentListChecked = false;
        this->NumPIUs = 0;
        this->NumSeriesPIUs = 0;
        this->NumParallelPIUs = 0;
        this->PIU.deallocate();
        this->PiuUniqueNames.clear();
        this->MyEnvrnFlag.clear();
        this->MySizeFlag.clear();
        this->MyPlantScanFlag.clear();
    }
};

} // namespace EnergyPlus

#endif
