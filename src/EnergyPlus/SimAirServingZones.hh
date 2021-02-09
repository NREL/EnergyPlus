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

#ifndef SimAirServingZones_hh_INCLUDED
#define SimAirServingZones_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACSystems.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace SimAirServingZones {

    // coil operation
    constexpr int CoilOn(1);  // normal coil operation
    constexpr int CoilOff(0); // signal coil shouldn't run

    constexpr int BeforeBranchSim(1);
    constexpr int AfterBranchSim(2);

    // CompType numerics -- for this module
    // component types addressed by this module
    constexpr int OAMixer_Num(1);
    constexpr int Fan_Simple_CV(2);
    constexpr int Fan_Simple_VAV(3);
    constexpr int WaterCoil_SimpleCool(4);
    constexpr int WaterCoil_Cooling(5);
    constexpr int WaterCoil_SimpleHeat(6);
    constexpr int SteamCoil_AirHeat(7);
    constexpr int WaterCoil_DetailedCool(8);
    constexpr int Coil_ElectricHeat(9);
    constexpr int Coil_GasHeat(10);
    constexpr int WaterCoil_CoolingHXAsst(11);
    constexpr int DXCoil_CoolingHXAsst(12);
    constexpr int Coil_DeSuperHeat(13);
    constexpr int DXSystem(14);
    constexpr int HeatXchngr(15);
    constexpr int Desiccant(16);
    constexpr int Unglazed_SolarCollector(17);
    constexpr int EvapCooler(18);
    constexpr int Furnace_UnitarySys_HeatOnly(19);
    constexpr int Furnace_UnitarySys_HeatCool(20);
    constexpr int Humidifier(21);
    constexpr int Duct(22);
    constexpr int UnitarySystem_BypassVAVSys(23);
    constexpr int UnitarySystem_MSHeatPump(24);
    constexpr int Fan_ComponentModel(25);
    constexpr int DXHeatPumpSystem(26);
    constexpr int CoilUserDefined(27);
    constexpr int Fan_System_Object(28);
    constexpr int UnitarySystemModel(29);
    constexpr int ZoneVRFasAirLoopEquip(30);

    void ManageAirLoops(EnergyPlusData &state, bool FirstHVACIteration, // TRUE if first full HVAC iteration in an HVAC timestep
                        bool &SimAir,                  // TRUE means air loops must be (re)simulated
                        bool &SimZoneEquipment         // TRUE means zone equipment must be (re) simulated
    );

    void GetAirPathData(EnergyPlusData &state);

    void InitAirLoops(EnergyPlusData &state, bool FirstHVACIteration); // TRUE if first full HVAC iteration in an HVAC timestep

    void ConnectReturnNodes(EnergyPlusData &state);

    void SimAirLoops(EnergyPlusData &state, bool FirstHVACIteration, bool &SimZoneEquipment);

    void SimAirLoop(EnergyPlusData &state,
        bool FirstHVACIteration, int AirLoopNum, int AirLoopPass, int &AirLoopIterMax, int &AirLoopIterTot, int &AirLoopNumCalls);

    void SolveAirLoopControllers(EnergyPlusData &state,
        bool FirstHVACIteration, int AirLoopNum, bool &AirLoopConvergedFlag, int &IterMax, int &IterTot, int &NumCalls);

    void SolveWaterCoilController(EnergyPlusData &state, bool FirstHVACIteration,
                                  int AirLoopNum,
                                  std::string const &CompName,
                                  int &CompIndex,
                                  std::string const &ControllerName,
                                  int ControllerIndex,
                                  bool HXAssistedWaterCoil);

    void ReSolveAirLoopControllers(EnergyPlusData &state,
        bool FirstHVACIteration, int AirLoopNum, bool &AirLoopConvergedFlag, int &IterMax, int &IterTot, int &NumCalls);

    void SimAirLoopComponents(EnergyPlusData &state, int AirLoopNum,         // Index of the air loop being currently simulated
                              bool FirstHVACIteration // TRUE if first full HVAC iteration in an HVAC timestep
    );

    void SimAirLoopComponent(EnergyPlusData &state, std::string const &CompName,   // the component Name
                             int CompType_Num,        // numeric equivalent for component type
                             bool FirstHVACIteration, // TRUE if first full HVAC iteration in an HVAC timestep
                             int AirLoopNum,          // Primary air loop number
                             int &CompIndex,                // numeric pointer for CompType/CompName -- passed back from other routines
                             HVACSystemData *CompPointer);

    void UpdateBranchConnections(EnergyPlusData &state,
                                 int AirLoopNum, // primary air system number
                                 int BranchNum,  // branch reference number
                                 int Update      // 1=BeforeBranchSim; 2=AfterBranchSim
    );

    void ResolveSysFlow(EnergyPlusData &state,
                        int SysNum, // the primary air system number
                        bool &SysReSim    // Set to TRUE if mass balance fails and re-simulation is needed
    );

    void SizeAirLoops(EnergyPlusData &state);

    void SizeAirLoopBranches(EnergyPlusData &state, int AirLoopNum, int BranchNum);

    void SetUpSysSizingArrays(EnergyPlusData &state);

    void SizeSysOutdoorAir(EnergyPlusData &state);

    void UpdateSysSizing(EnergyPlusData &state, DataGlobalConstants::CallIndicator CallIndicator);

    void UpdateSysSizingForScalableInputs(EnergyPlusData &state, int AirLoopNum);

    Real64 GetHeatingSATempForSizing(EnergyPlusData &state, int IndexAirLoop);

    Real64 GetHeatingSATempHumRatForSizing(EnergyPlusData &state, int IndexAirLoop);

    void LimitZoneVentEff(EnergyPlusData &state,
                          Real64 Xs,              // ratio of uncorrected system outdoor air flow rate to the design system supply flow rate
                          Real64 Voz,             // corrected (divided by distribution efficiency) zone outside air flow rate [m3/s]
                          int CtrlZoneNum,        // controlled zone number
                          Real64 &SystemCoolingEv // system ventilation efficiency
    );

    void CheckWaterCoilIsOnAirLoop(EnergyPlusData &state, int CoilTypeNum, std::string const &CompType, std::string const &CompName, bool &WaterCoilOnAirLoop);

    bool CheckWaterCoilOnPrimaryAirLoopBranch(EnergyPlusData &state, int CoilTypeNum, std::string const &CompName);

    bool CheckWaterCoilOnOASystem(EnergyPlusData &state, int CoilTypeNum, std::string const &CompName);

    bool CheckWaterCoilSystemOnAirLoopOrOASystem(EnergyPlusData &state, int CoilTypeNum, std::string const &CompName);

} // namespace SimAirServingZones

struct SimAirServingZonesData : BaseGlobalStruct {

    bool GetAirLoopInputFlag = true;        // Flag set to make sure you get input once
    int NumOfTimeStepInDay = 0;             // number of zone time steps in a day
    bool InitAirLoopsOneTimeFlag = true;
    int TestUniqueNodesNum = 0;
    bool SizeAirLoopsOneTimeFlag = true;
    bool InitAirLoopsBranchSizingFlag = true;
    bool OutputSetupFlag = false;
    bool MyEnvrnFlag = true;

    void clear_state() override
    {
        this->GetAirLoopInputFlag = true;
        this->NumOfTimeStepInDay = 0;
        this->InitAirLoopsOneTimeFlag = true;
        this->TestUniqueNodesNum = 0;
        this->SizeAirLoopsOneTimeFlag = true;
        this->InitAirLoopsBranchSizingFlag = true;
        this->OutputSetupFlag = false;
        this->MyEnvrnFlag = true;
    }
};

} // namespace EnergyPlus

#endif
