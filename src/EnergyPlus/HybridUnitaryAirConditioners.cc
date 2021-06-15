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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/HybridEvapCoolingModel.hh>
#include <EnergyPlus/HybridUnitaryAirConditioners.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#define TEMP_CURVE 0
#define W_CURVE 1
#define POWER_CURVE 2

namespace EnergyPlus::HybridUnitaryAirConditioners {

// Begin routines for zone HVAC Hybrid Evaporative cooler unit
void SimZoneHybridUnitaryAirConditioners(EnergyPlusData &state,
                                         std::string_view CompName,    // name of the packaged terminal heat pump
                                         int const ZoneNum,              // number of zone being served
                                         Real64 &SensibleOutputProvided, // sensible capacity delivered to zone cooling is negative
                                         Real64 &LatentOutputProvided,   // Latent add/removal  (kg/s), dehumid = negative
                                         int &CompIndex                  // index to zone hvac unit
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Spencer Maxwell Dutton
    //       DATE WRITTEN   October 2017
    //       MODIFIED
    //       RE-ENGINEERED  na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CompNum;
    bool errorsfound = false;
    if (state.dataHybridUnitaryAC->GetInputZoneHybridEvap) {
        GetInputZoneHybridUnitaryAirConditioners(state, errorsfound);
        state.dataHybridUnitaryAC->GetInputZoneHybridEvap = false;
    }

    if (CompIndex == 0) {
        CompNum = UtilityRoutines::FindItemInList(CompName, state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner);
        if (CompNum == 0) {
            ShowFatalError(state, "SimZoneHybridUnitaryAirConditioners: ZoneHVAC:HybridUnitaryHVAC not found.");
        }
        CompIndex = CompNum;
    } else {
        CompNum = CompIndex;
        if (CompNum < 1 || CompNum > state.dataHybridUnitaryAC->NumZoneHybridEvap) {
            ShowFatalError(state,
                           format("SimZoneHybridUnitaryAirConditioners: Invalid CompIndex passed={}, Number of units ={}, Entered Unit name = {}",
                                  CompNum,
                                  state.dataHybridUnitaryAC->NumZoneHybridEvap,
                                  CompName));
        }
        if (state.dataHybridUnitaryAC->CheckZoneHybridEvapName(CompNum)) {
            if (CompName != state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(CompNum).Name) {
                ShowFatalError(
                    state,
                    format("SimZoneHybridUnitaryAirConditioners: Invalid CompIndex passed={}, Unit name={}, stored unit name for that index={}",
                           CompNum,
                           CompName,
                           state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(CompNum).Name));
            }
            state.dataHybridUnitaryAC->CheckZoneHybridEvapName(CompNum) = false;
        }
    }
    try {
        InitZoneHybridUnitaryAirConditioners(state, CompNum, ZoneNum);
    } catch (int e) {
        ShowFatalError(state,
                       format("An exception occurred in InitZoneHybridUnitaryAirConditioners{}, Unit name={}, stored unit name for that "
                              "index={}. Please check idf.",
                              CompNum,
                              CompName,
                              state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(CompNum).Name));
        return;
    }
    try {
        CalcZoneHybridUnitaryAirConditioners(state, CompNum, ZoneNum, SensibleOutputProvided, LatentOutputProvided);
    } catch (int e) {
        ShowFatalError(state,
                       format("An exception occurred in CalcZoneHybridUnitaryAirConditioners{}, Unit name={}, stored unit name for that "
                              "index={}. Please check idf.",
                              CompNum,
                              CompName,
                              state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(CompNum).Name));
        return;
    }
    try {
        ReportZoneHybridUnitaryAirConditioners(state, CompNum);
    } catch (int e) {
        ShowFatalError(state,
                       format("An exception occurred in ReportZoneHybridUnitaryAirConditioners{}, Unit name={}, stored unit name for that "
                              "index={}. Please check idf.",
                              CompNum,
                              CompName,
                              state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(CompNum).Name));
        return;
    }
}

void InitZoneHybridUnitaryAirConditioners(EnergyPlusData &state,
                                          int const UnitNum, // unit number
                                          int const ZoneNum  // number of zone being served
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Spencer Maxwell Dutton
    //       DATE WRITTEN   October 2017
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    //

    // METHODOLOGY EMPLOYED:
    //

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace Psychrometrics;
    auto &ZoneComp = state.dataHVACGlobal->ZoneComp;
    using DataZoneEquipment::CheckZoneEquipmentList;
    using DataZoneEquipment::ZoneHybridEvaporativeCooler_Num;

    // Locals
    int Loop;
    auto &MySizeFlag = state.dataHybridUnitaryAC->MySizeFlag;
    auto &MyEnvrnFlag = state.dataHybridUnitaryAC->MyEnvrnFlag;
    auto &MyFanFlag = state.dataHybridUnitaryAC->MyFanFlag;
    auto &MyZoneEqFlag = state.dataHybridUnitaryAC->MyZoneEqFlag; // used to set up zone equipment availability managers

    int InletNode;

    if (state.dataHybridUnitaryAC->HybridCoolOneTimeFlag) {
        MySizeFlag.dimension(state.dataHybridUnitaryAC->NumZoneHybridEvap, true);
        MyEnvrnFlag.dimension(state.dataHybridUnitaryAC->NumZoneHybridEvap, true);
        MyFanFlag.dimension(state.dataHybridUnitaryAC->NumZoneHybridEvap, true);
        MyZoneEqFlag.allocate(state.dataHybridUnitaryAC->NumZoneHybridEvap);
        MyZoneEqFlag = true;
        state.dataHybridUnitaryAC->HybridCoolOneTimeFlag = false;
    }
    if (!state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).Initialized) {
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).Initialize(ZoneNum);
    }
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).RequestedLoadToHeatingSetpoint = 0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).RequestedLoadToCoolingSetpoint = 0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).RequestedHumdificationMass = 0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).RequestedHumdificationLoad = 0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).RequestedHumdificationEnergy = 0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).RequestedDeHumdificationMass = 0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).RequestedDeHumdificationLoad = 0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).RequestedDeHumdificationEnergy = 0;

    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingRate = 0.0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).UnitTotalCoolingEnergy = 0.0;

    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingRate = 0.0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).UnitSensibleCoolingEnergy = 0.0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).UnitLatentCoolingRate = 0.0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).UnitLatentCoolingEnergy = 0.0;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).AvailStatus = 0;

    // set the availability status based on the availability manager list name
    if (allocated(ZoneComp)) {
        if (MyZoneEqFlag(UnitNum)) { // initialize the name of each availability manager list and zone number
            ZoneComp(ZoneHybridEvaporativeCooler_Num).ZoneCompAvailMgrs(UnitNum).AvailManagerListName =
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).AvailManagerListName;
            ZoneComp(ZoneHybridEvaporativeCooler_Num).ZoneCompAvailMgrs(UnitNum).ZoneNum = ZoneNum;
            MyZoneEqFlag(UnitNum) = false;
        }
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).AvailStatus =
            ZoneComp(ZoneHybridEvaporativeCooler_Num).ZoneCompAvailMgrs(UnitNum).AvailStatus;
    }

    // need to check all zone outdoor air control units to see if they are on Zone Equipment List or issue warning
    if (!state.dataHybridUnitaryAC->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataHybridUnitaryAC->ZoneEquipmentListChecked = true;
        for (Loop = 1; Loop <= state.dataHybridUnitaryAC->NumZoneHybridEvap; ++Loop) {
            if (CheckZoneEquipmentList(state, "ZoneHVAC:HybridUnitaryHVAC", state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(Loop).Name)) {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(Loop).ZoneNodeNum = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ZoneNode;
            } else {
                ShowSevereError(state,
                                "InitZoneHybridUnitaryAirConditioners: ZoneHVAC:HybridUnitaryHVAC = " +
                                    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(Loop).Name +
                                    ", is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }
    }

    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InitializeModelParams();
    // Do the following initializations (every time step): This should be the info from
    // the previous components outlets or the node data in this section.

    // Transfer the node data to EvapCond data structure
    InletNode = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletNode;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;

    // Set the inlet volumetric flow rate from the mass flow rate
    if (state.dataEnvrn->StdRhoAir > 1) {
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletVolumetricFlowRate =
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate / state.dataEnvrn->StdRhoAir;
    } else {
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletVolumetricFlowRate =
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate / 1.225;
    }

    // Set all of the inlet state variables from the inlet nodes
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp = state.dataLoopNodes->Node(InletNode).Temp;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat = state.dataLoopNodes->Node(InletNode).HumRat;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletEnthalpy = state.dataLoopNodes->Node(InletNode).Enthalpy;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure = state.dataLoopNodes->Node(InletNode).Press;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletRH =
        PsyRhFnTdbWPb(state,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure,
                      "InitZoneHybridUnitaryAirConditioners");

    // Set default outlet state to inlet states, just to be safe
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletEnthalpy;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletPressure =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletPressure;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletRH =
        PsyRhFnTdbWPb(state,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletPressure,
                      "InitZoneHybridUnitaryAirConditioners");
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate;

    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp =
        state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Temp;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletHumRat =
        state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).HumRat;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletEnthalpy =
        state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Enthalpy;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletPressure =
        state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Press;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletRH =
        PsyRhFnTdbWPb(state,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletHumRat,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletPressure,
                      "InitZoneHybridUnitaryAirConditioners");
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletMassFlowRate =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SupplyVentilationAir;

    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecOutletTemp =
        state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Temp;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecOutletHumRat =
        state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).HumRat;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecOutletEnthalpy =
        state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Enthalpy;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecOutletPressure =
        state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Press;
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecOutletRH =
        PsyRhFnTdbWPb(state,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletHumRat,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletPressure,
                      "InitZoneHybridUnitaryAirConditioners");
}

void CalcZoneHybridUnitaryAirConditioners(EnergyPlusData &state,
                                          int const UnitNum,              // unit number
                                          int const ZoneNum,              // number of zone being served
                                          Real64 &SensibleOutputProvided, // sensible capacity delivered to zone cooling negitive
                                          Real64 &LatentOutputProvided    // Latent add/removal  (kg/s), dehumid = negative
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Spencer Maxwell Dutton
    //       DATE WRITTEN   October 2017
    //       MODIFIED
    //       RE-ENGINEERED  na

    using namespace DataLoopNode;
    using namespace Psychrometrics;

    Real64 EnvDryBulbT, AirTempRoom, EnvRelHumm, RoomRelHum, DesignMinVR;

    Real64 ZoneCoolingLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum)
                                 .RemainingOutputReqToCoolSP; // Remaining load required to meet cooling setpoint (<0 is a cooling load)
    Real64 ZoneHeatingLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum)
                                 .RemainingOutputReqToHeatSP; // Remaining load required to meet heating setpoint (>0 is a heating load)
    Real64 OutputRequiredToHumidify =
        state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum)
            .OutputRequiredToHumidifyingSP; // Load required to meet humidifying setpoint (>0 = a humidify load) [kgWater/s]

    Real64 OutputRequiredToDehumidify =
        state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ZoneNum)
            .OutputRequiredToDehumidifyingSP; // Load required to meet dehumidifying setpoint (<0 = a dehumidify load)  [kgWater/s]

    SensibleOutputProvided = 0;
    LatentOutputProvided = 0;
    // taking class members out of the object and then using them in the calcualtion is odd but its for clarity with unit testing.
    EnvDryBulbT = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp; // degrees C
    AirTempRoom = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp;    // degrees C
    EnvRelHumm = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletRH;    // RH
    RoomRelHum = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletRH;       // RH

    bool UseOccSchFlag = true;
    bool UseMinOASchFlag = true;

    using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;
    DesignMinVR = CalcDesignSpecificationOutdoorAir(state,
                                                    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OARequirementsPtr,
                                                    ZoneNum,
                                                    UseOccSchFlag,
                                                    UseMinOASchFlag); //[m3/s]
    Real64 DesignMinVRMassFlow = 0;
    if (state.dataEnvrn->StdRhoAir > 1) {
        DesignMinVRMassFlow = DesignMinVR * state.dataEnvrn->StdRhoAir;
    } else {
        DesignMinVRMassFlow = DesignMinVR * 1.225;
    }
    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).doStep(
        state, ZoneCoolingLoad, ZoneHeatingLoad, OutputRequiredToHumidify, OutputRequiredToDehumidify, DesignMinVRMassFlow);
    SensibleOutputProvided = -state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).QSensZoneOut; // cooling negative

    LatentOutputProvided = -state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).QLatentZoneOutMass; // dehumidification negative kg/s
}

void ReportZoneHybridUnitaryAirConditioners(EnergyPlusData &state, int const UnitNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Spencer Maxwell Dutton
    //       DATE WRITTEN   October 2017
    //       MODIFIED
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using namespace DataLoopNode;
    using namespace Psychrometrics;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNodeNum;
    ZoneNodeNum = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).ZoneNodeNum;

    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).PrimaryMode =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).PrimaryMode;

    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletNode).Temp =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletTemp;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletNode).HumRat =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletHumRat;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletNode).MassFlowRate =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletMassFlowRate;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletNode).Enthalpy =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).InletEnthalpy;

    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).Temp =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletTemp;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).HumRat =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletHumRat;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).MassFlowRate =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletMassFlowRate;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletNode).Enthalpy =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).OutletEnthalpy;

    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Temp =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletTemp;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).HumRat =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletHumRat;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).Enthalpy =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletEnthalpy;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryInletNode).MassFlowRate =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecInletMassFlowRate;

    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryOutletNode).Temp =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecOutletTemp;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryOutletNode).HumRat =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecOutletHumRat;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryOutletNode).Enthalpy =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecOutletEnthalpy;
    state.dataLoopNodes->Node(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecondaryOutletNode).MassFlowRate =
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitNum).SecOutletMassFlowRate;
}

void GetInputZoneHybridUnitaryAirConditioners(EnergyPlusData &state, bool &Errors)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Spencer Maxwell Dutton
    //       DATE WRITTEN   October 2017
    //       MODIFIED
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using BranchNodeConnections::TestCompSet;
    using namespace ScheduleManager;
    using BranchNodeConnections::SetUpCompSets;
    using NodeInputManager::GetOnlySingleNode;
    using namespace DataLoopNode;
    std::string cCurrentModuleObject; // Object type for getting and error messages
    Array1D_string Alphas;            // Alpha items for object
    Array1D<Real64> Numbers;          // Numeric items for object
    Array1D_string cAlphaFields;      // Alpha field names
    Array1D_string cNumericFields;    // Numeric field names
    Array1D_bool lAlphaBlanks;        // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;      // Logical array, numeric field input BLANK = .TRUE.
    int NumAlphas;                    // Number of Alphas for each GetObjectItem call
    int NumNumbers;                   // Number of Numbers for each GetObjectItem call
    int NumFields;                    // Total number of fields in object
    int IOStatus;                     // Used in GetObjectItem
    bool ErrorsFound(false);          // Set to true if errors in input, fatal at end of routine
    bool IsNotOK;                     // Flag to verify name
    bool IsBlank;                     // Flag for blank name
    int UnitLoop;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetInputZoneHybridUnitaryAirConditioners: ");
    cCurrentModuleObject = "ZoneHVAC:HybridUnitaryHVAC";
    state.dataHybridUnitaryAC->NumZoneHybridEvap = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, NumFields, NumAlphas, NumNumbers);
    int MaxNumbers = max(0, NumNumbers); // Maximum number of numeric fields in all objects
    int MaxAlphas = max(0, NumAlphas);   // Maximum number of alpha fields in all objects
    Alphas.allocate(MaxAlphas);
    Numbers.dimension(MaxNumbers, 0.0);
    cAlphaFields.allocate(MaxAlphas);
    cNumericFields.allocate(MaxNumbers);
    lAlphaBlanks.dimension(MaxAlphas, true);
    lNumericBlanks.dimension(MaxNumbers, true);
    std::vector<std::string> test;
    std::vector<bool> blanks;

    if (state.dataHybridUnitaryAC->NumZoneHybridEvap > 0) {
        state.dataHybridUnitaryAC->CheckZoneHybridEvapName.dimension(state.dataHybridUnitaryAC->NumZoneHybridEvap, true);
        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner.allocate(state.dataHybridUnitaryAC->NumZoneHybridEvap);

        for (UnitLoop = 1; UnitLoop <= state.dataHybridUnitaryAC->NumZoneHybridEvap; ++UnitLoop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     UnitLoop,
                                                                     Alphas,
                                                                     NumAlphas,
                                                                     Numbers,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     lNumericBlanks,
                                                                     lAlphaBlanks,
                                                                     cAlphaFields,
                                                                     cNumericFields);

            IsNotOK = false;
            IsBlank = false;
            UtilityRoutines::VerifyName(state,
                                        Alphas(1),
                                        state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner,
                                        UnitLoop - 1,
                                        IsNotOK,
                                        IsBlank,
                                        cCurrentModuleObject + " Name");

            // A1, \field Name
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name = Alphas(1);
            // A2, \field Availability Schedule Name
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Schedule = Alphas(2);
            if (lAlphaBlanks(2)) {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SchedPtr = GetScheduleIndex(state, Alphas(2));
                if (state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SchedPtr == 0) {
                    ShowSevereError(state, "Invalid " + cAlphaFields(2) + '=' + Alphas(2));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + Alphas(1));
                    ErrorsFound = true;
                }
            }
            // A3, \field Availability Manager List Name
            if (!lAlphaBlanks(3)) {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).AvailManagerListName = Alphas(3);
            }

            // A4, \field Minimum Supply Air Temperature Schedule Named
            if (!lAlphaBlanks(4)) {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).TsaMin_schedule_pointer = GetScheduleIndex(state, Alphas(4));
                if (state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).TsaMin_schedule_pointer == 0) {
                    ShowSevereError(state, "Invalid " + cAlphaFields(4) + '=' + Alphas(4));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + Alphas(1));
                    ErrorsFound = true;
                }
            }
            // A5, \field Maximum Supply Air Temperature Schedule Name
            if (!lAlphaBlanks(5)) {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).TsaMax_schedule_pointer = GetScheduleIndex(state, Alphas(5));
                if (state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).TsaMax_schedule_pointer == 0) {
                    ShowSevereError(state, "Invalid " + cAlphaFields(5) + '=' + Alphas(5));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + Alphas(1));
                    ErrorsFound = true;
                }
            }
            // A6, \field Minimum Supply Air Humidity Ratio Schedule Name
            if (!lAlphaBlanks(6)) {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RHsaMin_schedule_pointer = GetScheduleIndex(state, Alphas(6));
                if (state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RHsaMin_schedule_pointer == 0) {
                    ShowSevereError(state, "Invalid " + cAlphaFields(6) + '=' + Alphas(6));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + Alphas(1));
                    ErrorsFound = true;
                }
            }
            // A7, \field Maximum Supply Air Humidity Ratio Schedule Name
            if (!lAlphaBlanks(7)) {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RHsaMax_schedule_pointer = GetScheduleIndex(state, Alphas(7));
                if (state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RHsaMax_schedule_pointer == 0) {
                    ShowSevereError(state, "Invalid " + cAlphaFields(7) + '=' + Alphas(7));
                    ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + Alphas(1));
                    ErrorsFound = true;
                }
            }

            // A8, \field Method to Choose Value of Controlled Inputs

            // A9, \field Return Air Node Name
            // A10, \field Outdoor Air Node Name
            // A11, \field Supply Air Node Name
            // A12, \field Relief Node Name
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).InletNode =
                GetOnlySingleNode(state,
                                  Alphas(9),
                                  ErrorsFound,
                                  cCurrentModuleObject,
                                  Alphas(1),
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::Inlet,
                                  1,
                                  ObjectIsNotParent);
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryInletNode =
                GetOnlySingleNode(state,
                                  Alphas(10),
                                  ErrorsFound,
                                  cCurrentModuleObject,
                                  Alphas(1),
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::OutsideAir,
                                  1,
                                  ObjectIsNotParent);
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OutletNode =
                GetOnlySingleNode(state,
                                  Alphas(11),
                                  ErrorsFound,
                                  cCurrentModuleObject,
                                  Alphas(1),
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::Outlet,
                                  1,
                                  ObjectIsNotParent);
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryOutletNode =
                GetOnlySingleNode(state,
                                  Alphas(12),
                                  ErrorsFound,
                                  cCurrentModuleObject,
                                  Alphas(1),
                                  DataLoopNode::NodeFluidType::Air,
                                  DataLoopNode::NodeConnectionType::ReliefAir,
                                  1,
                                  ObjectIsNotParent);
            TestCompSet(state, cCurrentModuleObject, Alphas(1), Alphas(9), Alphas(11), "Hybrid Evap Air Zone Nodes");
            TestCompSet(state, cCurrentModuleObject, Alphas(1), Alphas(10), Alphas(12), "Hybrid Evap Air Zone Secondary Nodes");

            // N1, \field System Maximum Supply AirFlow Rate
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemMaximumSupplyAirFlowRate = Numbers(1);

            // N2, \field External Static Pressure at System Maximum Supply Air Flow Rate

            // In each time step, the result for system power, fan power, gas use, water user, or supply airflow rate will be determined as :
            // TableValue * SysMaxSupply * ScalingFactor
            // A13, \field Fan Heat Included in Lookup Tables
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FanHeatGain = false;
            if (!lAlphaBlanks(13)) {
                if (UtilityRoutines::SameString(Alphas(13), "Yes")) {
                    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FanHeatGain = false;
                } else if (UtilityRoutines::SameString(Alphas(13), "No")) {
                    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FanHeatGain = true;
                } else {
                    ShowSevereError(state, cCurrentModuleObject + " = " + state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
                    ShowContinueError(state, "Illegal " + cAlphaFields(13) + " = " + Alphas(13));
                    ErrorsFound = true;
                }
            }
            // A14, \field Fan Heat Gain Location
            if (!lAlphaBlanks(14)) {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FanHeatGainLocation = Alphas(14);
            }
            // N3, \field Fan Heat in Air Stream Fraction
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FanHeatInAirFrac = Numbers(3);
            // N4, \field Scaling Factor
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ScalingFactor = Numbers(4);
            // the two numbers above are used to generate a overall scaling factor
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ScaledSystemMaximumSupplyAirVolumeFlowRate = Numbers(1) * Numbers(4);
            if (state.dataEnvrn->StdRhoAir > 1) {
                // SystemMaximumSupplyAirFlowRate*ScalingFactor*AirDensity;
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ScaledSystemMaximumSupplyAirMassFlowRate =
                    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ScaledSystemMaximumSupplyAirVolumeFlowRate *
                    state.dataEnvrn->StdRhoAir;
            } else {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ScaledSystemMaximumSupplyAirMassFlowRate =
                    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ScaledSystemMaximumSupplyAirVolumeFlowRate * 1.225;
            }

            // N5, \field Minimum Time Between Mode Change
            // A15, \field First fuel type
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FirstFuelType = Alphas(15);
            // A16, \field Second fuel type
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecondFuelType = Alphas(16);
            // A17, \field Third fuel type
            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ThirdFuelType = Alphas(17);
            // A18, \field Objective Function Minimizes

            // A19, \ OA requirement pointer
            if (!lAlphaBlanks(19)) {
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OARequirementsPtr =
                    UtilityRoutines::FindItemInList(Alphas(19), state.dataSize->OARequirements);
                if (state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OARequirementsPtr == 0) {
                    ShowSevereError(state, std::string{RoutineName} + cCurrentModuleObject + " = " + Alphas(1) + " invalid data");
                    ShowContinueError(state, "Invalid-not found " + cAlphaFields(19) + "=\"" + Alphas(19) + "\".");
                    ErrorsFound = true;
                } else {
                    state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OutdoorAir = true;
                }
            }

            int FirstModeAlphaNumber = 20;
            int NumberOfAlphasPerMode = 9;
            int Numberofoperatingmodes = 0;
            for (int i = FirstModeAlphaNumber; i <= NumAlphas; i = i + NumberOfAlphasPerMode) {
                if (!lAlphaBlanks(i)) {
                    ++Numberofoperatingmodes;
                } else {
                    break;
                }
            }

            for (int modeIter = 0; modeIter <= Numberofoperatingmodes - 1; ++modeIter) {
                ErrorsFound = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ParseMode(
                    state, Alphas, cAlphaFields, Numbers, cNumericFields, lAlphaBlanks, cCurrentModuleObject);
                if (ErrorsFound) {
                    ShowFatalError(state, std::string{RoutineName} + "Errors found parsing modes");
                    ShowContinueError(state, "... Preceding condition causes termination.");
                    break;
                }
            }
            // add the ZoneHVAC:HybridUnitaryHVAC Scaled Maximum Supply Air Volume Flow Rate to the Component Sizing Report Summary
            BaseSizer::reportSizerOutput(
                state,
                cCurrentModuleObject,
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                "Scaled Maximum Supply Air Volume Flow Rate [m3/s]",
                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ScaledSystemMaximumSupplyAirVolumeFlowRate);
        }
    }

    // setup output variables
    for (UnitLoop = 1; UnitLoop <= state.dataHybridUnitaryAC->NumZoneHybridEvap; ++UnitLoop) {

        SetUpCompSets(state,
                      cCurrentModuleObject,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                      cCurrentModuleObject,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                      state.dataLoopNodes->NodeID(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).InletNode),
                      state.dataLoopNodes->NodeID(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OutletNode));

        SetUpCompSets(state,
                      cCurrentModuleObject,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                      cCurrentModuleObject,
                      state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                      state.dataLoopNodes->NodeID(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryInletNode),
                      state.dataLoopNodes->NodeID(state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryOutletNode));

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemTotalCoolingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemTotalCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                            _,
                            "ENERGYTRANSFER",
                            "COOLINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemSensibleCoolingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemSensibleCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemLatentCoolingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemLatentCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalCoolingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitSensibleCoolingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitSensibleCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitLatentCoolingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitLatentCoolingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Total Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemTotalHeatingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Total Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemTotalHeatingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                            _,
                            "ENERGYTRANSFER",
                            "HeatingCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Sensible Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemSensibleHeatingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Sensible Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemSensibleHeatingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemLatentHeatingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC System Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SystemLatentHeatingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Total Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalHeatingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Total Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitTotalHeatingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Sensible Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitSensibleHeatingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Sensible Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitSensibleHeatingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitLatentHeatingRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Zone Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitLatentHeatingEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Predicted Sensible Load to Setpoint Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedLoadToCoolingSetpoint,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Error Code",
                            OutputProcessor::Unit::None,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ErrorCode,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Supply Air Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OutletTemp,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Return Air Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).InletTemp,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Outdoor Air Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecInletTemp,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Relief Air Temperature",
                            OutputProcessor::Unit::C,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecOutletTemp,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Supply Air Humidity Ratio",
                            OutputProcessor::Unit::kgWater_kgDryAir,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OutletHumRat,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Return Air Humidity Ratio",
                            OutputProcessor::Unit::kgWater_kgDryAir,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).InletHumRat,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Outdoor Air Humidity Ratio",
                            OutputProcessor::Unit::kgWater_kgDryAir,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecInletHumRat,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Relief Air Humidity Ratio",
                            OutputProcessor::Unit::kgWater_kgDryAir,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecOutletHumRat,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Supply Air Relative Humidity",
                            OutputProcessor::Unit::Perc,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OutletRH,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Return Air Relative Humidity",
                            OutputProcessor::Unit::Perc,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).InletRH,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Outdoor Air Relative Humidity",
                            OutputProcessor::Unit::Perc,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecInletRH,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Relief Air Relative Humidity",
                            OutputProcessor::Unit::Perc,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecOutletRH,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Supply Air Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OutletMassFlowRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Supply Air Standard Density Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).OutletVolumetricFlowRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Return Air Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).InletMassFlowRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Return Air Standard Density Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).InletVolumetricFlowRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Relief Air Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecOutletMassFlowRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Relief Air Standard Density Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecOutletVolumetricFlowRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Ventilation Air Standard Density Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SupplyVentilationVolume,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FinalElectricalPower,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FinalElectricalEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                            _,
                            "Electricity",
                            "Cooling",
                            "Hybrid HVAC Cooling",
                            "System");

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Requested Outdoor Air Ventilation Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).MinOA_Msa,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Ventilation Air Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SupplyVentilationAir,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Availability Status",
                            OutputProcessor::Unit::None,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).UnitOn,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Outdoor Air Fraction",
                            OutputProcessor::Unit::None,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).averageOSAF,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Dehumidification Load to Humidistat Setpoint Moisture Transfer Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedDeHumdificationMass,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Dehumidification Load to Humidistat Setpoint Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedDeHumdificationLoad,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC DehumidificationLoad to Humidistat Setpoint Heat Tansfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedDeHumdificationEnergy,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Humidification Load to Humidistat Setpoint Moisture Transfer Rate",
                            OutputProcessor::Unit::kg_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedHumdificationMass,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Humidification Load to Humidistat Setpoint Heat Transfer Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedHumdificationLoad,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Humidification Load to Humidistat Setpoint Heat Tansfer Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).RequestedHumdificationEnergy,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Supply Fan Electricity Rate",
                            OutputProcessor::Unit::W,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SupplyFanElectricPower,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Supply Fan Electricity Energy",
                            OutputProcessor::Unit::J,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SupplyFanElectricEnergy,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                            _,
                            "Electricity",
                            "Fans",
                            "Hybrid HVAC Fans",
                            "System");
        if (state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecondFuelType != "NONE") {
            SetupOutputVariable(state,
                                "Zone Hybrid Unitary HVAC Secondary Fuel Consumption Rate",
                                OutputProcessor::Unit::W,
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryFuelConsumptionRate,
                                "System",
                                "Average",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
            SetupOutputVariable(state,
                                "Zone Hybrid Unitary HVAC Secondary Fuel Consumption",
                                OutputProcessor::Unit::J,
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecondaryFuelConsumption,
                                "System",
                                "Sum",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                                _,
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).SecondFuelType,
                                "Cooling",
                                "Hybrid HVAC Cooling",
                                "System");
        }
        if (state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ThirdFuelType != "NONE") {
            SetupOutputVariable(state,
                                "Zone Hybrid Unitary HVAC Third Fuel Consumption Rate",
                                OutputProcessor::Unit::W,
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ThirdFuelConsumptionRate,
                                "System",
                                "Average",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
            SetupOutputVariable(state,
                                "Zone Hybrid Unitary HVAC Third Fuel Consumption",
                                OutputProcessor::Unit::J,
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ThirdFuelConsumption,
                                "System",
                                "Sum",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                                _,
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ThirdFuelType,
                                "Cooling",
                                "Hybrid HVAC Cooling",
                                "System");
        }
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Water Consumption Rate",
                            OutputProcessor::Unit::kgWater_s,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).WaterConsumptionRate,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC Water Consumption",
                            OutputProcessor::Unit::m3,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).WaterConsumption,
                            "System",
                            "Sum",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name,
                            _,
                            "Water",
                            "Cooling",
                            "Hybrid HVAC Cooling",
                            "System");
        SetupOutputVariable(state,
                            "Zone Hybrid Unitary HVAC External Static Pressure",
                            OutputProcessor::Unit::Pa,
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).ExternalStaticPressure,
                            "System",
                            "Average",
                            state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);

        if (state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FanHeatGain) {
            SetupOutputVariable(state,
                                "Zone Hybrid Unitary HVAC Fan Rise in Air Temperature",
                                OutputProcessor::Unit::deltaC,
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).FanHeatTemp,
                                "System",
                                "Average",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
            SetupOutputVariable(state,
                                "Zone Hybrid Unitary HVAC Fan Heat Gain to Air",
                                OutputProcessor::Unit::W,
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).PowerLossToAir,
                                "System",
                                "Average",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
        }

        int index = 0;

        for (auto &thisSetting : state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).CurrentOperatingSettings) {
            SetupOutputVariable(state,
                                format("Zone Hybrid Unitary HVAC Runtime Fraction in Setting {}", index),
                                OutputProcessor::Unit::None,
                                thisSetting.Runtime_Fraction,
                                "Zone",
                                "Average",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
            SetupOutputVariable(state,
                                format("Zone Hybrid Unitary HVAC Mode in Setting {}", index),
                                OutputProcessor::Unit::None,
                                thisSetting.Mode,
                                "Zone",
                                "Average",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
            SetupOutputVariable(state,
                                format("Zone Hybrid Unitary HVAC Outdoor Air Fraction in Setting {}", index),
                                OutputProcessor::Unit::kg_s,
                                thisSetting.Outdoor_Air_Fraction,
                                "Zone",
                                "Average",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
            SetupOutputVariable(state,
                                format("Zone Hybrid Unitary HVAC Supply Air Mass Flow Rate in Setting {}", index),
                                OutputProcessor::Unit::kg_s,
                                thisSetting.Unscaled_Supply_Air_Mass_Flow_Rate,
                                "Zone",
                                "Average",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
            SetupOutputVariable(state,
                                format("Zone Hybrid Unitary HVAC Supply Air Mass Flow Rate Ratio in Setting {}", index),
                                OutputProcessor::Unit::None,
                                thisSetting.Supply_Air_Mass_Flow_Rate_Ratio,
                                "Zone",
                                "Average",
                                state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(UnitLoop).Name);
            index++;
        }
    }
    Errors = ErrorsFound;
    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in getting input.");
        ShowContinueError(state, "... Preceding condition causes termination.");
    }
}
int GetHybridUnitaryACOutAirNode(EnergyPlusData &state, int const CompNum)
{
    bool errorsfound = false;
    if (state.dataHybridUnitaryAC->GetInputZoneHybridEvap) {
        GetInputZoneHybridUnitaryAirConditioners(state, errorsfound);
        state.dataHybridUnitaryAC->GetInputZoneHybridEvap = false;
    }

    int GetHybridUnitaryACOutAirNode;
    GetHybridUnitaryACOutAirNode = 0;
    if (CompNum > 0 && CompNum <= state.dataHybridUnitaryAC->NumZoneHybridEvap) {
        GetHybridUnitaryACOutAirNode = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(CompNum).SecondaryInletNode;
    }

    return GetHybridUnitaryACOutAirNode;
}

int GetHybridUnitaryACZoneInletNode(EnergyPlusData &state, int const CompNum)
{
    bool errorsfound = false;
    if (state.dataHybridUnitaryAC->GetInputZoneHybridEvap) {
        GetInputZoneHybridUnitaryAirConditioners(state, errorsfound);
        state.dataHybridUnitaryAC->GetInputZoneHybridEvap = false;
    }

    int GetHybridUnitaryACZoneInletNode;
    GetHybridUnitaryACZoneInletNode = 0;
    if (CompNum > 0 && CompNum <= state.dataHybridUnitaryAC->NumZoneHybridEvap) {
        GetHybridUnitaryACZoneInletNode = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(CompNum).OutletNode;
    }

    return GetHybridUnitaryACZoneInletNode;
}

int GetHybridUnitaryACReturnAirNode(EnergyPlusData &state, int const CompNum)
{
    bool errorsfound = false;
    if (state.dataHybridUnitaryAC->GetInputZoneHybridEvap) {
        GetInputZoneHybridUnitaryAirConditioners(state, errorsfound);
        state.dataHybridUnitaryAC->GetInputZoneHybridEvap = false;
    }

    int GetHybridUnitaryACReturnAirNode;
    GetHybridUnitaryACReturnAirNode = 0;
    if (CompNum > 0 && CompNum <= state.dataHybridUnitaryAC->NumZoneHybridEvap) {
        GetHybridUnitaryACReturnAirNode = state.dataHybridUnitaryAC->ZoneHybridUnitaryAirConditioner(CompNum).InletNode;
    }

    return GetHybridUnitaryACReturnAirNode;
}

//*****************************************************************************************

} // namespace EnergyPlus::HybridUnitaryAirConditioners
