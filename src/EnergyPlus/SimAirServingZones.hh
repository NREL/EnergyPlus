// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // coil operation
    extern int const CoilOn;  // normal coil operation
    extern int const CoilOff; // signal coil shouldn't run
    extern int const BeforeBranchSim;
    extern int const AfterBranchSim;
    // CompType numerics -- for this module
    // component types addressed by this module
    extern int const OAMixer_Num;
    extern int const Fan_Simple_CV;
    extern int const Fan_Simple_VAV;
    extern int const Fan_System_Object;
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
    extern int const Furnace_UnitarySys_HeatOnly;
    extern int const Furnace_UnitarySys_HeatCool;
    extern int const Humidifier;
    extern int const Duct;
    extern int const UnitarySystem_BypassVAVSys;
    extern int const UnitarySystem_MSHeatPump;
    extern int const Fan_ComponentModel; // cpw22Aug2010 (new)
    extern int const DXHeatPumpSystem;
    extern int const CoilUserDefined;
    extern int const UnitarySystemModel;
    extern int const ZoneVRFasAirLoopEquip;

    extern bool GetAirLoopInputFlag; // Flag set to make sure you get input once
    extern int NumOfTimeStepInDay;   // number of zone time steps in a day

    void clear_state();

    void ManageAirLoops(EnergyPlusData &state, bool FirstHVACIteration, // TRUE if first full HVAC iteration in an HVAC timestep
                        bool &SimAir,                  // TRUE means air loops must be (re)simulated
                        bool &SimZoneEquipment         // TRUE means zone equipment must be (re) simulated
    );

    void GetAirPathData();

    void InitAirLoops(EnergyPlusData &state, bool FirstHVACIteration); // TRUE if first full HVAC iteration in an HVAC timestep

    void ConnectReturnNodes();

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

    void SizeAirLoops();

    void SizeAirLoopBranches(EnergyPlusData &state, int AirLoopNum, int BranchNum);

    void SetUpSysSizingArrays();

    void SizeSysOutdoorAir();

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

    void CheckWaterCoilIsOnAirLoop(EnergyPlusData &state, int CoilTypeNum, std::string CompType, std::string CompName, bool &WaterCoilOnAirLoop);

    bool CheckWaterCoilOnPrimaryAirLoopBranch(EnergyPlusData &state, int CoilTypeNum, std::string CompName);

    bool CheckWaterCoilOnOASystem(EnergyPlusData &state, int CoilTypeNum, std::string CompName);

    bool CheckWaterCoilSystemOnAirLoopOrOASystem(EnergyPlusData &state, int CoilTypeNum, std::string CompName);

} // namespace SimAirServingZones

struct SimAirServingZonesData : BaseGlobalStruct {

    void clear_state() override
    {

    }
};

} // namespace EnergyPlus

#endif
