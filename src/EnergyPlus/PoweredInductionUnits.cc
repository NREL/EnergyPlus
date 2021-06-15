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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixerComponent.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>

namespace EnergyPlus::PoweredInductionUnits {

// Module containing routines dealing with Series and Parallel fan powered terminal boxes

// MODULE INFORMATION:
//       AUTHOR         Fred Buhl
//       DATE WRITTEN   August 2000
//       MODIFIED       Brent Griffith, Sept 2010, plant upgrades, fluid properties
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms needed to simulate Series and Parallel
// fan powered induction terminal boxes.

// METHODOLOGY EMPLOYED:
// The terminal boxes are modeled as a collection of components: air mixer,
// fan, and heating coil plus an integrated control
// algorithm that adjusts the primary air flow and the heating coil output
// to meet the zone load.

// Using/Aliasing
using namespace DataLoopNode;
using DataHVACGlobals::SingleCoolingSetPoint;
using DataHVACGlobals::SingleHeatingSetPoint;
using DataHVACGlobals::SmallAirVolFlow;
using DataHVACGlobals::SmallLoad;
using DataHVACGlobals::SmallMassFlow;
using DataHVACGlobals::SmallTempDiff;
using namespace ScheduleManager;
using Psychrometrics::PsyCpAirFnW;
using Psychrometrics::PsyHFnTdbW;
using Psychrometrics::PsyRhoAirFnPbTdbW;
using SteamCoils::SimulateSteamCoilComponents;
using namespace FluidProperties;

constexpr const char *fluidNameSteam("STEAM");
constexpr const char *fluidNameWater("WATER");

void SimPIU(EnergyPlusData &state,
            std::string_view CompName,   // name of the PIU
            bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
            int const ZoneNum,             // index of zone served by PIU
            int const ZoneNodeNum,         // zone node number of zone served by PIU
            int &CompIndex                 // PIU Index in PIU names
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   March 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Manages the simulation of a fan powered induction terminal unit.
    // Called from SimZoneAirLoopEquipmentin module ZoneAirLoopEquipmentManager.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PIUNum; // index of powered induction unit being simulated

    // First time SimPIU is called, get the input for all the fan coil units
    if (state.dataPowerInductionUnits->GetPIUInputFlag) {
        GetPIUs(state);
        state.dataPowerInductionUnits->GetPIUInputFlag = false;
    }

    // Get the powered induction unit index
    if (CompIndex == 0) {
        PIUNum = UtilityRoutines::FindItemInList(CompName, state.dataPowerInductionUnits->PIU);
        if (PIUNum == 0) {
            ShowFatalError(state, "SimPIU: PIU Unit not found=" + std::string{CompName});
        }
        CompIndex = PIUNum;
    } else {
        PIUNum = CompIndex;
        if (PIUNum > state.dataPowerInductionUnits->NumPIUs || PIUNum < 1) {
            ShowFatalError(state,
                           format("SimPIU: Invalid CompIndex passed={}, Number of PIU Units={}, PIU Unit name={}",
                                  CompIndex,
                                  state.dataPowerInductionUnits->NumPIUs,
                                  CompName));
        }
        if (state.dataPowerInductionUnits->CheckEquipName(PIUNum)) {
            if (CompName != state.dataPowerInductionUnits->PIU(PIUNum).Name) {
                ShowFatalError(state,
                               format("SimPIU: Invalid CompIndex passed={}, PIU Unit name={}, stored PIU Unit Name for that index={}",
                                      CompIndex,
                                      CompName,
                                      state.dataPowerInductionUnits->PIU(PIUNum).Name));
            }
            state.dataPowerInductionUnits->CheckEquipName(PIUNum) = false;
        }
    }

    state.dataSize->CurTermUnitSizingNum =
        state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(PIUNum).ADUNum).TermUnitSizingNum;
    // initialize the unit
    InitPIU(state, PIUNum, FirstHVACIteration);

    state.dataSize->TermUnitPIU = true;

    // Select the correct unit type
    {
        auto const SELECT_CASE_var(state.dataPowerInductionUnits->PIU(PIUNum).UnitType_Num);

        if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat) { //  'AirTerminal:SingleDuct:SeriesPIU:Reheat'

            CalcSeriesPIU(state, PIUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);

        } else if (SELECT_CASE_var ==
                   DataDefineEquip::iZnAirLoopEquipType::SingleDuct_ParallelPIU_Reheat) { // 'AirTerminal:SingleDuct:ParallelPIU:Reheat'

            CalcParallelPIU(state, PIUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);

        } else {
            ShowSevereError(state, "Illegal PI Unit Type used=" + state.dataPowerInductionUnits->PIU(PIUNum).UnitType);
            ShowContinueError(state, "Occurs in PI Unit=" + state.dataPowerInductionUnits->PIU(PIUNum).Name);
            ShowFatalError(state, "Preceding condition causes termination.");
        }
    }

    state.dataSize->TermUnitPIU = false;

    // Update the current unit's outlet nodes
    // no update needed: reheat coil updates outlet node; inlet nodes' mass flow rate set by Calc routine

    // Fill the report variables
    ReportPIU(state, PIUNum);
}

void GetPIUs(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Obtains input data for powered induction unit terminal boxes and stores it
    // in PIU data structures

    // METHODOLOGY EMPLOYED:
    // Uses "Get" routines to read in data.

    // Using/Aliasing
    using BranchNodeConnections::SetUpCompSets;
    using BranchNodeConnections::TestCompSet;
    using DataPlant::TypeOf_CoilSteamAirHeating;
    using DataPlant::TypeOf_CoilWaterSimpleHeating;
    using FluidProperties::FindRefrigerant;
    using NodeInputManager::GetOnlySingleNode;
    using SteamCoils::GetCoilSteamInletNode;
    using WaterCoils::GetCoilWaterInletNode;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PIUIndex;            // loop index
    int PIUNum;              // current fan coil number
    int NumAlphas;           // Number of Alpha input fields for each GetObjectItem call
    int NumNumbers;          // Number of Numeric input fields for each GetObjectItem call
    int IOStatus;            // Used in GetObjectItem
    bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
    bool IsNotOK;            // Flag to verify name
    int CtrlZone;            // controlled zome do loop index
    int SupAirIn;            // controlled zone supply air inlet index
    bool AirNodeFound;
    int ADUNum;
    static constexpr std::string_view RoutineName("GetPIUs: "); // include trailing blank space
    bool SteamMessageNeeded;
    int FanType_Num; // integer representation of fan type

    // find the number of each type of fan coil unit
    SteamMessageNeeded = true;
    state.dataPowerInductionUnits->NumSeriesPIUs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:SeriesPIU:Reheat");
    state.dataPowerInductionUnits->NumParallelPIUs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:ParallelPIU:Reheat");
    state.dataPowerInductionUnits->NumPIUs = state.dataPowerInductionUnits->NumSeriesPIUs + state.dataPowerInductionUnits->NumParallelPIUs;
    // allocate the data structures
    state.dataPowerInductionUnits->PIU.allocate(state.dataPowerInductionUnits->NumPIUs);
    state.dataPowerInductionUnits->PiuUniqueNames.reserve(static_cast<unsigned>(state.dataPowerInductionUnits->NumPIUs));
    state.dataPowerInductionUnits->CheckEquipName.dimension(state.dataPowerInductionUnits->NumPIUs, true);
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    // loop over Series PIUs; get and load the input data
    for (PIUIndex = 1; PIUIndex <= state.dataPowerInductionUnits->NumSeriesPIUs; ++PIUIndex) {

        cCurrentModuleObject = "AirTerminal:SingleDuct:SeriesPIU:Reheat";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 PIUIndex,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        PIUNum = PIUIndex;
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataPowerInductionUnits->PiuUniqueNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        state.dataPowerInductionUnits->PIU(PIUNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataPowerInductionUnits->PIU(PIUNum).UnitType = cCurrentModuleObject;
        state.dataPowerInductionUnits->PIU(PIUNum).UnitType_Num = DataDefineEquip::iZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat;
        state.dataPowerInductionUnits->PIU(PIUNum).Sched = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr =
                GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2)); // convert schedule name to pointer
            if (state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                    " entered =" + state.dataIPShortCut->cAlphaArgs(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                    state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
        }

        state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow = state.dataIPShortCut->rNumericArgs(1);
        state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow = state.dataIPShortCut->rNumericArgs(2);
        state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac = state.dataIPShortCut->rNumericArgs(3);

        state.dataPowerInductionUnits->PIU(PIUNum).HCoilType = state.dataIPShortCut->cAlphaArgs(9); // type (key) of heating coil
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:WATER")) {
            state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num = iHCoilType::SimpleHeating;
            state.dataPowerInductionUnits->PIU(PIUNum).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:FUEL")) {
            state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num = iHCoilType::Gas;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:STEAM")) {
            state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num = iHCoilType::SteamAirHeating;
            state.dataPowerInductionUnits->PIU(PIUNum).HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
            state.dataPowerInductionUnits->PIU(PIUNum).HCoil_FluidIndex = FindRefrigerant(state, "Steam");
            if (state.dataPowerInductionUnits->PIU(PIUNum).HCoil_FluidIndex == 0) {
                ShowSevereError(state, std::string{RoutineName} + "Steam Properties for " + state.dataIPShortCut->cAlphaArgs(1) + " not found.");
                if (SteamMessageNeeded) ShowContinueError(state, "Steam Fluid Properties should have been included in the input file.");
                ErrorsFound = true;
                SteamMessageNeeded = false;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:ELECTRIC")) {
            state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num = iHCoilType::Electric;
        } else {
            ShowSevereError(state, "Illegal " + state.dataIPShortCut->cAlphaFieldNames(9) + " = " + state.dataIPShortCut->cAlphaArgs(9));
            ShowContinueError(state, "Occurs in " + cCurrentModuleObject + " = " + state.dataPowerInductionUnits->PIU(PIUNum).Name);
            ErrorsFound = true;
        }

        state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode = GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(3),
                                                                                    ErrorsFound,
                                                                                    state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                                    1,
                                                                                    ObjectIsParent,
                                                                                    state.dataIPShortCut->cAlphaFieldNames(3));

        state.dataPowerInductionUnits->PIU(PIUNum).SecAirInNode = GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(4),
                                                                                    ErrorsFound,
                                                                                    state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                                    1,
                                                                                    ObjectIsParent,
                                                                                    state.dataIPShortCut->cAlphaFieldNames(4));

        state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode = GetOnlySingleNode(state,
                                                                                  state.dataIPShortCut->cAlphaArgs(5),
                                                                                  ErrorsFound,
                                                                                  state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                                                  DataLoopNode::NodeFluidType::Air,
                                                                                  DataLoopNode::NodeConnectionType::Outlet,
                                                                                  1,
                                                                                  ObjectIsParent,
                                                                                  state.dataIPShortCut->cAlphaFieldNames(5));

        state.dataPowerInductionUnits->PIU(PIUNum).HCoilInAirNode = GetOnlySingleNode(state,
                                                                                      state.dataIPShortCut->cAlphaArgs(6),
                                                                                      ErrorsFound,
                                                                                      state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                      DataLoopNode::NodeConnectionType::Internal,
                                                                                      1,
                                                                                      ObjectIsParent,
                                                                                      state.dataIPShortCut->cAlphaFieldNames(6));
        // The reheat coil control node is necessary for hot water reheat, but not necessary for
        // electric or gas reheat.
        if (state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num == iHCoilType::SimpleHeating) {
            state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode =
                GetCoilWaterInletNode(state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), ErrorsFound);
        }
        if (state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num == iHCoilType::SteamAirHeating) {
            state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode =
                GetCoilSteamInletNode(state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), ErrorsFound);
        }
        state.dataPowerInductionUnits->PIU(PIUNum).MixerName = state.dataIPShortCut->cAlphaArgs(7); // name of zone mixer object
        state.dataPowerInductionUnits->PIU(PIUNum).FanName = state.dataIPShortCut->cAlphaArgs(8);   // name of fan object

        // find fan type
        // test if Fan:SystemModel fan of this name exists
        if (HVACFan::checkIfFanNameIsAFanSystem(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName)) {
            state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num = DataHVACGlobals::FanType_SystemModelObject;
            state.dataHVACFan->fanObjs.emplace_back(
                new HVACFan::FanSystem(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName)); // call constructor
            state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index =
                HVACFan::getFanObjectVectorIndex(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName);
            state.dataPowerInductionUnits->PIU(PIUNum).FanAvailSchedPtr =
                state.dataHVACFan->fanObjs[state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index]->availSchedIndex;
        } else {
            bool isNotOkay(false);
            ValidateComponent(state, "FAN:CONSTANTVOLUME", state.dataPowerInductionUnits->PIU(PIUNum).FanName, isNotOkay, "GetPIUs");
            if (isNotOkay) {
                ShowContinueError(
                    state, "In " + state.dataPowerInductionUnits->PIU(PIUNum).UnitType + " = " + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                ErrorsFound = true;
            }
            state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num = DataHVACGlobals::FanType_SimpleConstVolume;
            Fans::GetFanType(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName, FanType_Num, ErrorsFound);
            state.dataPowerInductionUnits->PIU(PIUNum).FanAvailSchedPtr = Fans::GetFanAvailSchPtr(
                state, DataHVACGlobals::cFanTypes(FanType_Num), state.dataPowerInductionUnits->PIU(PIUNum).FanName, ErrorsFound);
        }

        state.dataPowerInductionUnits->PIU(PIUNum).HCoil = state.dataIPShortCut->cAlphaArgs(10); // name of heating coil object
        ValidateComponent(state,
                          state.dataPowerInductionUnits->PIU(PIUNum).HCoilType,
                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                          IsNotOK,
                          cCurrentModuleObject + " - Heating Coil");
        if (IsNotOK) {
            ShowContinueError(state, "In " + cCurrentModuleObject + " = " + state.dataPowerInductionUnits->PIU(PIUNum).Name);
            ErrorsFound = true;
        }
        state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow = state.dataIPShortCut->rNumericArgs(4);
        state.dataPowerInductionUnits->PIU(PIUNum).MinVolHotWaterFlow = state.dataIPShortCut->rNumericArgs(5);
        state.dataPowerInductionUnits->PIU(PIUNum).HotControlOffset = state.dataIPShortCut->rNumericArgs(6);
        // Set default convergence tolerance
        if (state.dataPowerInductionUnits->PIU(PIUNum).HotControlOffset <= 0.0) {
            state.dataPowerInductionUnits->PIU(PIUNum).HotControlOffset = 0.001;
        }

        // Add fan to component sets array
        SetUpCompSets(state,
                      state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                      state.dataPowerInductionUnits->PIU(PIUNum).Name,
                      "UNDEFINED",
                      state.dataIPShortCut->cAlphaArgs(8),
                      "UNDEFINED",
                      state.dataIPShortCut->cAlphaArgs(6));

        // Add reheat coil to component sets array
        SetUpCompSets(state,
                      state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                      state.dataPowerInductionUnits->PIU(PIUNum).Name,
                      state.dataIPShortCut->cAlphaArgs(9),
                      state.dataIPShortCut->cAlphaArgs(10),
                      state.dataIPShortCut->cAlphaArgs(6),
                      state.dataIPShortCut->cAlphaArgs(5));

        // Register component set data
        TestCompSet(state,
                    state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                    state.dataPowerInductionUnits->PIU(PIUNum).Name,
                    state.dataLoopNodes->NodeID(state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode),
                    state.dataLoopNodes->NodeID(state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode),
                    "Air Nodes");

        AirNodeFound = false;
        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode == state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                state.dataPowerInductionUnits->PIU(PIUNum).ADUNum = ADUNum;
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum = state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataPowerInductionUnits->PIU(PIUNum).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for PIU = [" + state.dataPowerInductionUnits->PIU(PIUNum).UnitType +
                                ',' + state.dataPowerInductionUnits->PIU(PIUNum).Name + "].");
            ShowContinueError(state,
                              "...should have outlet node = " + state.dataLoopNodes->NodeID(state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the supply air inlet node number of this unit.
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                            state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode;
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                            state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode;
                        state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(PIUNum).ADUNum).TermUnitSizingNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                        state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(PIUNum).ADUNum).ZoneEqNum = CtrlZone;
                        AirNodeFound = true;
                        state.dataPowerInductionUnits->PIU(PIUNum).CtrlZoneNum = CtrlZone; // fill index for later use in finding air loop index
                        state.dataPowerInductionUnits->PIU(PIUNum).ctrlZoneInNodeIndex = SupAirIn;
                        break;
                    }
                }
            }
            if (!AirNodeFound) {
                ShowSevereError(
                    state, "The outlet air node from the " + cCurrentModuleObject + " Unit = " + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                ShowContinueError(state, "did not have a matching Zone Equipment Inlet Node, Node = " + state.dataIPShortCut->cAlphaArgs(5));
                ErrorsFound = true;
            }
        }
    }

    for (PIUIndex = 1; PIUIndex <= state.dataPowerInductionUnits->NumParallelPIUs; ++PIUIndex) {

        cCurrentModuleObject = "AirTerminal:SingleDuct:ParallelPIU:Reheat";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 PIUIndex,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNumbers,
                                                                 IOStatus,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);

        PIUNum = PIUIndex + state.dataPowerInductionUnits->NumSeriesPIUs;
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataPowerInductionUnits->PiuUniqueNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        state.dataPowerInductionUnits->PIU(PIUNum).Name = state.dataIPShortCut->cAlphaArgs(1);
        state.dataPowerInductionUnits->PIU(PIUNum).UnitType = cCurrentModuleObject;
        state.dataPowerInductionUnits->PIU(PIUNum).UnitType_Num = DataDefineEquip::iZnAirLoopEquipType::SingleDuct_ParallelPIU_Reheat;
        state.dataPowerInductionUnits->PIU(PIUNum).Sched = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr =
                GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2)); // convert schedule name to pointer
            if (state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr == 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + cCurrentModuleObject + ": invalid " + state.dataIPShortCut->cAlphaFieldNames(2) +
                                    " entered =" + state.dataIPShortCut->cAlphaArgs(2) + " for " + state.dataIPShortCut->cAlphaFieldNames(1) + '=' +
                                    state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
        }
        state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow = state.dataIPShortCut->rNumericArgs(1);
        state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow = state.dataIPShortCut->rNumericArgs(2);
        state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac = state.dataIPShortCut->rNumericArgs(3);
        state.dataPowerInductionUnits->PIU(PIUNum).FanOnFlowFrac = state.dataIPShortCut->rNumericArgs(4);
        state.dataPowerInductionUnits->PIU(PIUNum).HCoilType = state.dataIPShortCut->cAlphaArgs(9); // type (key) of heating coil
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:WATER")) {
            state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num = iHCoilType::SimpleHeating;
            state.dataPowerInductionUnits->PIU(PIUNum).HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:FUEL")) {
            state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num = iHCoilType::Gas;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:STEAM")) {
            state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num = iHCoilType::SteamAirHeating;
            state.dataPowerInductionUnits->PIU(PIUNum).HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating;
            state.dataPowerInductionUnits->PIU(PIUNum).HCoil_FluidIndex = FindRefrigerant(state, "Steam");
            if (state.dataPowerInductionUnits->PIU(PIUNum).HCoil_FluidIndex == 0) {
                ShowSevereError(state, std::string{RoutineName} + "Steam Properties for " + state.dataIPShortCut->cAlphaArgs(1) + " not found.");
                if (SteamMessageNeeded) ShowContinueError(state, "Steam Fluid Properties should have been included in the input file.");
                ErrorsFound = true;
                SteamMessageNeeded = false;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:ELECTRIC")) {
            state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num = iHCoilType::Electric;
        } else {
            ShowSevereError(state, "Illegal " + state.dataIPShortCut->cAlphaFieldNames(9) + " = " + state.dataIPShortCut->cAlphaArgs(9));
            ShowContinueError(state, "Occurs in " + cCurrentModuleObject + " = " + state.dataPowerInductionUnits->PIU(PIUNum).Name);
            ErrorsFound = true;
        }

        state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode = GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(3),
                                                                                    ErrorsFound,
                                                                                    cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                                    1,
                                                                                    ObjectIsParent,
                                                                                    state.dataIPShortCut->cAlphaFieldNames(3));

        state.dataPowerInductionUnits->PIU(PIUNum).SecAirInNode = GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(4),
                                                                                    ErrorsFound,
                                                                                    cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                                    1,
                                                                                    ObjectIsParent,
                                                                                    state.dataIPShortCut->cAlphaFieldNames(4));

        state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode = GetOnlySingleNode(state,
                                                                                  state.dataIPShortCut->cAlphaArgs(5),
                                                                                  ErrorsFound,
                                                                                  cCurrentModuleObject,
                                                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                                                  DataLoopNode::NodeFluidType::Air,
                                                                                  DataLoopNode::NodeConnectionType::Outlet,
                                                                                  1,
                                                                                  ObjectIsParent,
                                                                                  state.dataIPShortCut->cAlphaFieldNames(5));

        state.dataPowerInductionUnits->PIU(PIUNum).HCoilInAirNode = GetOnlySingleNode(state,
                                                                                      state.dataIPShortCut->cAlphaArgs(6),
                                                                                      ErrorsFound,
                                                                                      cCurrentModuleObject,
                                                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                                                      DataLoopNode::NodeFluidType::Air,
                                                                                      DataLoopNode::NodeConnectionType::Internal,
                                                                                      1,
                                                                                      ObjectIsParent,
                                                                                      state.dataIPShortCut->cAlphaFieldNames(6));
        // The reheat coil control node is necessary for hot water reheat, but not necessary for
        // electric or gas reheat.
        //  IF (PIU(PIUNum)%HCoilType_Num .EQ. HCoilType_Gas .OR. PIU(PIUNum)%HCoilType_Num .EQ. HCoilType_Electric) THEN
        //    IF(state.dataIPShortCut->cAlphaArgs(11) /= '') THEN
        //      CALL ShowWarningError(state, 'In '//TRIM(cCurrentModuleObject)//' = ' // TRIM(PIU(PIUNum)%Name) &
        //                             // ' the '//TRIM(cAlphaFieldNames(11))//' is not needed and will be ignored.')
        //      CALL ShowContinueError(state, '  It is used for hot water reheat coils only.')
        //    END IF
        //  ELSE
        //    IF(state.dataIPShortCut->cAlphaArgs(11) == '') THEN
        //      CALL ShowSevereError(state, 'In '//TRIM(cCurrentModuleObject)//' = ' // TRIM(PIU(PIUNum)%Name) &
        //                           // ' the '//TRIM(cAlphaFieldNames(11))//' is undefined.')
        //      ErrorsFound=.TRUE.
        //    END IF
        //    PIU(PIUNum)%HotControlNode  = &
        //      GetOnlySingleNode(state, state.dataIPShortCut->cAlphaArgs(11),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
        //                        DataLoopNode::NodeFluidType::Water,DataLoopNode::NodeConnectionType::Actuator,1,ObjectIsParent)
        //  END IF
        if (state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num == iHCoilType::SimpleHeating) {
            state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode =
                GetCoilWaterInletNode(state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), ErrorsFound);
        }
        if (state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num == iHCoilType::SteamAirHeating) {
            state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode =
                GetCoilSteamInletNode(state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), ErrorsFound);
        }
        state.dataPowerInductionUnits->PIU(PIUNum).MixerName = state.dataIPShortCut->cAlphaArgs(7); // name of zone mixer object
        state.dataPowerInductionUnits->PIU(PIUNum).FanName = state.dataIPShortCut->cAlphaArgs(8);   // name of fan object
        // find fan type
        // test if Fan:SystemModel fan of this name exists
        if (HVACFan::checkIfFanNameIsAFanSystem(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName)) {
            state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num = DataHVACGlobals::FanType_SystemModelObject;
            state.dataHVACFan->fanObjs.emplace_back(
                new HVACFan::FanSystem(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName)); // call constructor
            state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index =
                HVACFan::getFanObjectVectorIndex(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName);
            state.dataPowerInductionUnits->PIU(PIUNum).FanAvailSchedPtr =
                state.dataHVACFan->fanObjs[state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index]->availSchedIndex;
        } else {
            bool isNotOkay(false);
            ValidateComponent(state, "FAN:CONSTANTVOLUME", state.dataPowerInductionUnits->PIU(PIUNum).FanName, isNotOkay, "GetPIUs");
            if (isNotOkay) {
                ShowContinueError(
                    state, "In " + state.dataPowerInductionUnits->PIU(PIUNum).UnitType + " = " + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                ErrorsFound = true;
            }
            state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num = DataHVACGlobals::FanType_SimpleConstVolume;
            Fans::GetFanType(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName, FanType_Num, ErrorsFound);
            state.dataPowerInductionUnits->PIU(PIUNum).FanAvailSchedPtr = Fans::GetFanAvailSchPtr(
                state, DataHVACGlobals::cFanTypes(FanType_Num), state.dataPowerInductionUnits->PIU(PIUNum).FanName, ErrorsFound);
        }
        state.dataPowerInductionUnits->PIU(PIUNum).HCoil = state.dataIPShortCut->cAlphaArgs(10); // name of heating coil object
        ValidateComponent(state,
                          state.dataPowerInductionUnits->PIU(PIUNum).HCoilType,
                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                          IsNotOK,
                          cCurrentModuleObject + " - Heating Coil");
        if (IsNotOK) {
            ShowContinueError(state, "In " + cCurrentModuleObject + " = " + state.dataPowerInductionUnits->PIU(PIUNum).Name);
            ErrorsFound = true;
        }
        state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow = state.dataIPShortCut->rNumericArgs(5);
        state.dataPowerInductionUnits->PIU(PIUNum).MinVolHotWaterFlow = state.dataIPShortCut->rNumericArgs(6);
        state.dataPowerInductionUnits->PIU(PIUNum).HotControlOffset = state.dataIPShortCut->rNumericArgs(7);
        // Set default convergence tolerance
        if (state.dataPowerInductionUnits->PIU(PIUNum).HotControlOffset <= 0.0) {
            state.dataPowerInductionUnits->PIU(PIUNum).HotControlOffset = 0.001;
        }

        // Add fan to component sets array
        SetUpCompSets(state,
                      state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                      state.dataPowerInductionUnits->PIU(PIUNum).Name,
                      "UNDEFINED",
                      state.dataIPShortCut->cAlphaArgs(8),
                      state.dataIPShortCut->cAlphaArgs(4),
                      "UNDEFINED");

        // Add reheat coil to component sets array
        SetUpCompSets(state,
                      state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                      state.dataPowerInductionUnits->PIU(PIUNum).Name,
                      state.dataIPShortCut->cAlphaArgs(9),
                      state.dataIPShortCut->cAlphaArgs(10),
                      state.dataIPShortCut->cAlphaArgs(6),
                      state.dataIPShortCut->cAlphaArgs(5));

        // Register component set data
        TestCompSet(state,
                    state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                    state.dataPowerInductionUnits->PIU(PIUNum).Name,
                    state.dataLoopNodes->NodeID(state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode),
                    state.dataLoopNodes->NodeID(state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode),
                    "Air Nodes");

        AirNodeFound = false;
        for (ADUNum = 1; ADUNum <= state.dataDefineEquipment->NumAirDistUnits; ++ADUNum) {
            if (state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode == state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                //      AirDistUnit(ADUNum)%InletNodeNum = PIU(PIUNum)%InletNodeNum
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum = state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode;
                state.dataPowerInductionUnits->PIU(PIUNum).ADUNum = ADUNum;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (state.dataPowerInductionUnits->PIU(PIUNum).ADUNum == 0) {
            ShowSevereError(state,
                            std::string{RoutineName} + "No matching Air Distribution Unit, for PIU = [" + state.dataPowerInductionUnits->PIU(PIUNum).UnitType +
                                ',' + state.dataPowerInductionUnits->PIU(PIUNum).Name + "].");
            ShowContinueError(state,
                              "...should have outlet node = " + state.dataLoopNodes->NodeID(state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the supply air inlet node number of this unit.
            AirNodeFound = false;
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode =
                            state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode;
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode =
                            state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode;
                        state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(PIUNum).ADUNum).TermUnitSizingNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                        state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(PIUNum).ADUNum).ZoneEqNum = CtrlZone;
                        state.dataPowerInductionUnits->PIU(PIUNum).CtrlZoneNum = CtrlZone;
                        state.dataPowerInductionUnits->PIU(PIUNum).ctrlZoneInNodeIndex = SupAirIn;
                        AirNodeFound = true;
                    }
                }
            }
            if (!AirNodeFound) {
                ShowSevereError(
                    state, "The outlet air node from the " + cCurrentModuleObject + " Unit = " + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                ShowContinueError(state, "did not have a matching Zone Equipment Inlet Node, Node = " + state.dataIPShortCut->cAlphaArgs(5));
                ErrorsFound = true;
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in getting input.  Preceding conditions cause termination.");
    }

    for (PIUNum = 1; PIUNum <= state.dataPowerInductionUnits->NumPIUs; ++PIUNum) {
        // Setup Report variables for the PIUs
        SetupOutputVariable(state,
                            "Zone Air Terminal Primary Damper Position",
                            OutputProcessor::Unit::None,
                            state.dataPowerInductionUnits->PIU(PIUNum).PriDamperPosition,
                            "System",
                            "Average",
                            state.dataPowerInductionUnits->PIU(PIUNum).Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Heating Rate",
                            OutputProcessor::Unit::W,
                            state.dataPowerInductionUnits->PIU(PIUNum).HeatingRate,
                            "System",
                            "Average",
                            state.dataPowerInductionUnits->PIU(PIUNum).Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Heating Energy",
                            OutputProcessor::Unit::J,
                            state.dataPowerInductionUnits->PIU(PIUNum).HeatingEnergy,
                            "System",
                            "Sum",
                            state.dataPowerInductionUnits->PIU(PIUNum).Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            state.dataPowerInductionUnits->PIU(PIUNum).SensCoolRate,
                            "System",
                            "Average",
                            state.dataPowerInductionUnits->PIU(PIUNum).Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            state.dataPowerInductionUnits->PIU(PIUNum).SensCoolEnergy,
                            "System",
                            "Sum",
                            state.dataPowerInductionUnits->PIU(PIUNum).Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Outdoor Air Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            state.dataPowerInductionUnits->PIU(PIUNum).OutdoorAirFlowRate,
                            "System",
                            "Average",
                            state.dataPowerInductionUnits->PIU(PIUNum).Name);
    }
}

void InitPIU(EnergyPlusData &state,
             int const PIUNum,             // number of the current fan coil unit being simulated
             bool const FirstHVACIteration // TRUE if first zone equip this HVAC step
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for initializations of the powered induction unit
    // terminal boxe.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger initializations.

    // Using/Aliasing
    using DataPlant::TypeOf_CoilSteamAirHeating;
    using DataPlant::TypeOf_CoilWaterSimpleHeating;
    using DataZoneEquipment::CheckZoneEquipmentList;
    using PlantUtilities::InitComponentNodes;
    using PlantUtilities::ScanPlantLoopsForObject;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("InitPIU");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PriNode;    // primary air inlet node number
    int SecNode;    // secondary air inlet node number
    int HotConNode; // hot water control node number in PIU
    int OutletNode; // unit air outlet node number
    Real64 RhoAir;  // air density at outside pressure and standard temperature and humidity
    auto &MyEnvrnFlag = state.dataPowerInductionUnits->MyEnvrnFlag;
    auto &MySizeFlag = state.dataPowerInductionUnits->MySizeFlag;
    auto &MyPlantScanFlag = state.dataPowerInductionUnits->MyPlantScanFlag;
    int Loop;   // Loop checking control variable
    Real64 rho; // local plant fluid density
    bool errFlag;

    // Do the one time initializations
    if (state.dataPowerInductionUnits->MyOneTimeFlag) {

        MyEnvrnFlag.allocate(state.dataPowerInductionUnits->NumPIUs);
        MySizeFlag.allocate(state.dataPowerInductionUnits->NumPIUs);
        MyPlantScanFlag.allocate(state.dataPowerInductionUnits->NumPIUs);
        MyEnvrnFlag = true;
        MySizeFlag = true;
        MyPlantScanFlag = true;
        state.dataPowerInductionUnits->MyOneTimeFlag = false;
    }

    if (MyPlantScanFlag(PIUNum) && allocated(state.dataPlnt->PlantLoop)) {
        if ((state.dataPowerInductionUnits->PIU(PIUNum).HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating) ||
            (state.dataPowerInductionUnits->PIU(PIUNum).HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating)) {
            errFlag = false;
            ScanPlantLoopsForObject(state,
                                    state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                    state.dataPowerInductionUnits->PIU(PIUNum).HCoil_PlantTypeNum,
                                    state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum,
                                    state.dataPowerInductionUnits->PIU(PIUNum).HWLoopSide,
                                    state.dataPowerInductionUnits->PIU(PIUNum).HWBranchNum,
                                    state.dataPowerInductionUnits->PIU(PIUNum).HWCompNum,
                                    errFlag,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _);
            if (errFlag) {
                ShowFatalError(state, "InitPIU: Program terminated due to previous condition(s).");
            }
            state.dataPowerInductionUnits->PIU(PIUNum).HotCoilOutNodeNum =
                state.dataPlnt->PlantLoop(state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum)
                    .LoopSide(state.dataPowerInductionUnits->PIU(PIUNum).HWLoopSide)
                    .Branch(state.dataPowerInductionUnits->PIU(PIUNum).HWBranchNum)
                    .Comp(state.dataPowerInductionUnits->PIU(PIUNum).HWCompNum)
                    .NodeNumOut;
        }
        MyPlantScanFlag(PIUNum) = false;
    } else if (MyPlantScanFlag(PIUNum) && !state.dataGlobal->AnyPlantInModel) {
        MyPlantScanFlag(PIUNum) = false;
    }

    if (!state.dataPowerInductionUnits->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataPowerInductionUnits->ZoneEquipmentListChecked = true;
        // Check to see if there is a Air Distribution Unit on the Zone Equipment List
        for (Loop = 1; Loop <= state.dataPowerInductionUnits->NumPIUs; ++Loop) {
            if (state.dataPowerInductionUnits->PIU(Loop).ADUNum == 0) continue;
            if (CheckZoneEquipmentList(state,
                                       "ZoneHVAC:AirDistributionUnit",
                                       state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(Loop).ADUNum).Name))
                continue;
            ShowSevereError(state,
                            "InitPIU: ADU=[Air Distribution Unit," +
                                state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(Loop).ADUNum).Name +
                                "] is not on any ZoneHVAC:EquipmentList.");
            ShowContinueError(state,
                              "...PIU=[" + state.dataPowerInductionUnits->PIU(Loop).UnitType + ',' + state.dataPowerInductionUnits->PIU(Loop).Name +
                                  "] will not be simulated.");
        }
    }

    if (!state.dataGlobal->SysSizingCalc && MySizeFlag(PIUNum) && !MyPlantScanFlag(PIUNum)) {

        SizePIU(state, PIUNum);

        HotConNode = state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode;
        if (HotConNode > 0) {
            // plant upgrade note? why no separate handling of steam coil? add it ?
            rho = GetDensityGlycol(state,
                                   state.dataPlnt->PlantLoop(state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum).FluidName,
                                   DataGlobalConstants::HWInitConvTemp,
                                   state.dataPlnt->PlantLoop(state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum).FluidIndex,
                                   RoutineName);

            state.dataPowerInductionUnits->PIU(PIUNum).MaxHotWaterFlow = rho * state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow;
            state.dataPowerInductionUnits->PIU(PIUNum).MinHotWaterFlow = rho * state.dataPowerInductionUnits->PIU(PIUNum).MinVolHotWaterFlow;
            InitComponentNodes(state,
                               state.dataPowerInductionUnits->PIU(PIUNum).MinHotWaterFlow,
                               state.dataPowerInductionUnits->PIU(PIUNum).MaxHotWaterFlow,
                               state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode,
                               state.dataPowerInductionUnits->PIU(PIUNum).HotCoilOutNodeNum,
                               state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum,
                               state.dataPowerInductionUnits->PIU(PIUNum).HWLoopSide,
                               state.dataPowerInductionUnits->PIU(PIUNum).HWBranchNum,
                               state.dataPowerInductionUnits->PIU(PIUNum).HWCompNum);
        }

        MySizeFlag(PIUNum) = false;
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(PIUNum)) {
        RhoAir = state.dataEnvrn->StdRhoAir;
        PriNode = state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode;
        SecNode = state.dataPowerInductionUnits->PIU(PIUNum).SecAirInNode;
        OutletNode = state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode;
        // set the mass flow rates from the input volume flow rates
        if (state.dataPowerInductionUnits->PIU(PIUNum).UnitType == "AirTerminal:SingleDuct:SeriesPIU:Reheat") {
            // series
            state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow = RhoAir * state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow;
            state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow = RhoAir * state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow;
            state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirMassFlow =
                RhoAir * state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac * state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow;
            state.dataLoopNodes->Node(PriNode).MassFlowRateMax = state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow;
            state.dataLoopNodes->Node(PriNode).MassFlowRateMin = state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirMassFlow;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow;
        } else {
            // parallel
            state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow = RhoAir * state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow;
            state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirMassFlow =
                RhoAir * state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac * state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow;
            state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirMassFlow = RhoAir * state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow;
            state.dataPowerInductionUnits->PIU(PIUNum).FanOnAirMassFlow =
                RhoAir * state.dataPowerInductionUnits->PIU(PIUNum).FanOnFlowFrac * state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow;
            state.dataLoopNodes->Node(PriNode).MassFlowRateMax = state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow;
            state.dataLoopNodes->Node(PriNode).MassFlowRateMin = state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirMassFlow;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow;
        }

        if (((state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num == iHCoilType::SimpleHeating) ||
             (state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num == iHCoilType::SteamAirHeating)) &&
            !MyPlantScanFlag(PIUNum)) {
            InitComponentNodes(state,
                               state.dataPowerInductionUnits->PIU(PIUNum).MinHotWaterFlow,
                               state.dataPowerInductionUnits->PIU(PIUNum).MaxHotWaterFlow,
                               state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode,
                               state.dataPowerInductionUnits->PIU(PIUNum).HotCoilOutNodeNum,
                               state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum,
                               state.dataPowerInductionUnits->PIU(PIUNum).HWLoopSide,
                               state.dataPowerInductionUnits->PIU(PIUNum).HWBranchNum,
                               state.dataPowerInductionUnits->PIU(PIUNum).HWCompNum);
        }

        if (state.dataPowerInductionUnits->PIU(PIUNum).AirLoopNum == 0) { // fill air loop index
            if (state.dataPowerInductionUnits->PIU(PIUNum).CtrlZoneNum > 0 && state.dataPowerInductionUnits->PIU(PIUNum).ctrlZoneInNodeIndex > 0) {
                state.dataPowerInductionUnits->PIU(PIUNum).AirLoopNum =
                    state.dataZoneEquip->ZoneEquipConfig(state.dataPowerInductionUnits->PIU(PIUNum).CtrlZoneNum)
                        .InletNodeAirLoopNum(state.dataPowerInductionUnits->PIU(PIUNum).ctrlZoneInNodeIndex);
                state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(PIUNum).ADUNum).AirLoopNum =
                    state.dataPowerInductionUnits->PIU(PIUNum).AirLoopNum;
            }
        }

        MyEnvrnFlag(PIUNum) = false;
    } // end one time inits

    if (!state.dataGlobal->BeginEnvrnFlag) {
        MyEnvrnFlag(PIUNum) = true;
    }

    PriNode = state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode;
    SecNode = state.dataPowerInductionUnits->PIU(PIUNum).SecAirInNode;

    // Do the start of HVAC time step initializations
    if (FirstHVACIteration) {
        // check for upstream zero flow. If nonzero and schedule ON, set primary flow to max
        if (GetCurrentScheduleValue(state, state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr) > 0.0 &&
            state.dataLoopNodes->Node(PriNode).MassFlowRate > 0.0) {
            if (state.dataPowerInductionUnits->PIU(PIUNum).UnitType == "AirTerminal:SingleDuct:SeriesPIU:Reheat") {
                state.dataLoopNodes->Node(PriNode).MassFlowRate = state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow;
                state.dataLoopNodes->Node(SecNode).MassFlowRate = max(
                    0.0, state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow - state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow);
            } else {
                state.dataLoopNodes->Node(PriNode).MassFlowRate = state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow;
                state.dataLoopNodes->Node(SecNode).MassFlowRate = state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirMassFlow;
            }
        } else {
            state.dataLoopNodes->Node(PriNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(SecNode).MassFlowRate = 0.0;
        }
        // reset the max and min avail flows
        if (GetCurrentScheduleValue(state, state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr) > 0.0 &&
            state.dataLoopNodes->Node(PriNode).MassFlowRateMaxAvail > 0.0) {
            if (state.dataPowerInductionUnits->PIU(PIUNum).UnitType == "AirTerminal:SingleDuct:SeriesPIU:Reheat") {
                state.dataLoopNodes->Node(PriNode).MassFlowRateMaxAvail = state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow;
                state.dataLoopNodes->Node(PriNode).MassFlowRateMinAvail = state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirMassFlow;
                state.dataLoopNodes->Node(SecNode).MassFlowRateMaxAvail = max(
                    0.0, state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow - state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirMassFlow);
                state.dataLoopNodes->Node(SecNode).MassFlowRateMinAvail = max(
                    0.0, state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow - state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow);
            } else {
                state.dataLoopNodes->Node(PriNode).MassFlowRateMaxAvail = state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow;
                state.dataLoopNodes->Node(PriNode).MassFlowRateMinAvail = state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirMassFlow;
                state.dataLoopNodes->Node(SecNode).MassFlowRateMaxAvail = state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirMassFlow;
                state.dataLoopNodes->Node(SecNode).MassFlowRateMinAvail = 0.0;
            }
        } else {
            state.dataLoopNodes->Node(PriNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(PriNode).MassFlowRateMinAvail = 0.0;
            state.dataLoopNodes->Node(SecNode).MassFlowRateMaxAvail = 0.0;
            state.dataLoopNodes->Node(SecNode).MassFlowRateMinAvail = 0.0;
        }
    }

    // Do the following initializations every time step

    // None needed
}

void SizePIU(EnergyPlusData &state, int const PIUNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   January 2002
    //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing PIU terminal units for which flow rates have not been
    // specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the zone or system sizing arrays.

    // Using/Aliasing
    using namespace DataSizing;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using SteamCoils::GetCoilSteamInletNode;
    using SteamCoils::GetCoilSteamOutletNode;
    using WaterCoils::GetCoilWaterInletNode;
    using WaterCoils::GetCoilWaterOutletNode;
    using WaterCoils::SetCoilDesFlow;

    using PlantUtilities::MyPlantSizingIndex;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SizePIU");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PltSizHeatNum; // index of plant sizing object for 1st heating loop
    Real64 CoilInTemp;
    Real64 CoilOutTemp;
    Real64 CoilOutHumRat;
    Real64 DesCoilLoad;
    Real64 DesMassFlow;

    Real64 TempSteamIn;
    Real64 EnthSteamInDry;
    Real64 EnthSteamOutWet;
    Real64 LatentHeatSteam;
    Real64 SteamDensity;
    int CoilWaterInletNode(0);
    int CoilWaterOutletNode(0);
    int CoilSteamInletNode(0);
    int CoilSteamOutletNode(0);
    bool ErrorsFound;
    Real64 rho;
    Real64 Cp;
    int DummyWaterIndex(1);
    bool IsAutoSize;               // Indicator to autosize
    Real64 MaxPriAirVolFlowDes;    // Autosized maximum primary air flow for reporting
    Real64 MaxPriAirVolFlowUser;   // Hardsized maximum primary air flow for reporting
    Real64 MaxTotAirVolFlowDes;    // Autosized maximum air flow for reporting
    Real64 MaxTotAirVolFlowUser;   // Hardsized maximum air flow for reporting
    Real64 MaxSecAirVolFlowDes;    // Autosized maximum secondary air flow for reporting
    Real64 MaxSecAirVolFlowUser;   // Hardsized maximum secondary air flow for reporting
    Real64 MinPriAirFlowFracDes;   // Autosized minimum primary air flow fraction for reporting
    Real64 MinPriAirFlowFracUser;  // Hardsized minimum primary air flow fraction for reporting
    Real64 FanOnFlowFracDes;       // Autosized fan on flow fraction for reporting
    Real64 FanOnFlowFracUser;      // Hardsized fan on flow fraction for reporting
    Real64 MaxVolHotWaterFlowDes;  // Autosized maximum hot water flow for reporting
    Real64 MaxVolHotWaterFlowUser; // Hardsized maximum hot water flow for reporting
    Real64 MaxVolHotSteamFlowDes;  // Autosized maximum hot steam flow for reporting
    Real64 MaxVolHotSteamFlowUser; // Hardsized maximum hot steam flow for reporting

    PltSizHeatNum = 0;
    DesMassFlow = 0.0;
    DesCoilLoad = 0.0;
    ErrorsFound = false;
    IsAutoSize = false;
    MaxPriAirVolFlowDes = 0.0;
    MaxPriAirVolFlowUser = 0.0;
    MaxTotAirVolFlowDes = 0.0;
    MaxTotAirVolFlowUser = 0.0;
    MaxSecAirVolFlowDes = 0.0;
    MaxSecAirVolFlowUser = 0.0;
    MinPriAirFlowFracDes = 0.0;
    MinPriAirFlowFracUser = 0.0;
    FanOnFlowFracDes = 0.0;
    FanOnFlowFracUser = 0.0;
    MaxVolHotWaterFlowDes = 0.0;
    MaxVolHotWaterFlowUser = 0.0;
    MaxVolHotSteamFlowDes = 0.0;
    MaxVolHotSteamFlowUser = 0.0;

    auto &TermUnitSizing(state.dataSize->TermUnitSizing);
    auto &CurTermUnitSizingNum(state.dataSize->CurTermUnitSizingNum);

    if (state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "User-Specified Maximum Primary Air Flow Rate [m3/s]",
                                             state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow);
            }
        } else {
            CheckZoneSizing(state, state.dataPowerInductionUnits->PIU(PIUNum).UnitType, state.dataPowerInductionUnits->PIU(PIUNum).Name);
            MaxPriAirVolFlowDes = max(state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow,
                                      state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow);
            if (MaxPriAirVolFlowDes < SmallAirVolFlow) {
                MaxPriAirVolFlowDes = 0.0;
            }

            if (IsAutoSize) {
                state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow = MaxPriAirVolFlowDes;
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "Design Size Maximum Primary Air Flow Rate [m3/s]",
                                             MaxPriAirVolFlowDes);
            } else {
                if (state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow > 0.0 && MaxPriAirVolFlowDes > 0.0) {
                    MaxPriAirVolFlowUser = state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                 "Design Size Maximum Primary Air Flow Rate [m3/s]",
                                                 MaxPriAirVolFlowDes,
                                                 "User-Specified Maximum Primary Air Flow Rate [m3/s]",
                                                 MaxPriAirVolFlowUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxPriAirVolFlowDes - MaxPriAirVolFlowUser) / MaxPriAirVolFlowUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePIU: Potential issue with equipment sizing for " + state.dataPowerInductionUnits->PIU(PIUNum).UnitType +
                                            ' ' + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                            ShowContinueError(state, format("User-Specified Primary Air Flow Rate of {:.5R} [m3/s]", MaxPriAirVolFlowUser));
                            ShowContinueError(state, format("differs from Design Size Primary Air Flow Rate of {:.5R} [m3/s]", MaxPriAirVolFlowDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    IsAutoSize = false;
    if (state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "User-Specified Maximum Air Flow Rate [m3/s]",
                                             state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow);
            }
        } else {
            CheckZoneSizing(state, state.dataPowerInductionUnits->PIU(PIUNum).UnitType, state.dataPowerInductionUnits->PIU(PIUNum).Name);
            MaxTotAirVolFlowDes = max(state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow,
                                      state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow);
            if (MaxTotAirVolFlowDes < SmallAirVolFlow) {
                MaxTotAirVolFlowDes = 0.0;
            }
            if (IsAutoSize) {
                state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow = MaxTotAirVolFlowDes;
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "Design Size Maximum Air Flow Rate [m3/s]",
                                             MaxTotAirVolFlowDes);
            } else {
                if (state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow > 0.0 && MaxTotAirVolFlowDes > 0.0) {
                    MaxTotAirVolFlowUser = state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                 "Design Size Maximum Air Flow Rate [m3/s]",
                                                 MaxTotAirVolFlowDes,
                                                 "User-Specified Maximum Air Flow Rate [m3/s]",
                                                 MaxTotAirVolFlowUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxTotAirVolFlowDes - MaxTotAirVolFlowUser) / MaxTotAirVolFlowUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePIU: Potential issue with equipment sizing for " + state.dataPowerInductionUnits->PIU(PIUNum).UnitType +
                                            ' ' + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                            ShowContinueError(state, format("User-Specified Maximum Air Flow Rate of {:.5R} [m3/s]", MaxTotAirVolFlowUser));
                            ShowContinueError(state, format("differs from Design Size Maximum Air Flow Rate of {:.5R} [m3/s]", MaxTotAirVolFlowDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    IsAutoSize = false;
    if (state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "User-Specified Maximum Secondary Air Flow Rate [m3/s]",
                                             state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow);
            }
        } else {
            CheckZoneSizing(state, state.dataPowerInductionUnits->PIU(PIUNum).UnitType, state.dataPowerInductionUnits->PIU(PIUNum).Name);
            MaxSecAirVolFlowDes = max(state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow,
                                      state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow);
            if (MaxSecAirVolFlowDes < SmallAirVolFlow) {
                MaxSecAirVolFlowDes = 0.0;
            }
            if (IsAutoSize) {
                state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow = MaxSecAirVolFlowDes;
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "Design Size Maximum Secondary Air Flow Rate [m3/s]",
                                             MaxSecAirVolFlowDes);
            } else {
                if (state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow > 0.0 && MaxSecAirVolFlowDes > 0.0) {
                    MaxSecAirVolFlowUser = state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                 "Design Size Maximum Secondary Air Flow Rate [m3/s]",
                                                 MaxSecAirVolFlowDes,
                                                 "User-Specified Maximum Secondary Air Flow Rate [m3/s]",
                                                 MaxSecAirVolFlowUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxSecAirVolFlowDes - MaxSecAirVolFlowUser) / MaxSecAirVolFlowUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePIU: Potential issue with equipment sizing for " + state.dataPowerInductionUnits->PIU(PIUNum).UnitType +
                                            ' ' + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                            ShowContinueError(state, format("User-Specified Maximum Secondary Air Flow Rate of {:.5R} [m3/s]", MaxSecAirVolFlowUser));
                            ShowContinueError(
                                state, format("differs from Design Size Maximum Secondary Air Flow Rate of {:.5R} [m3/s]", MaxSecAirVolFlowDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    IsAutoSize = false;
    if (state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "User-Specified Minimum Primary Air Flow Fraction",
                                             state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac);
            }
        } else {
            CheckZoneSizing(state, state.dataPowerInductionUnits->PIU(PIUNum).UnitType, state.dataPowerInductionUnits->PIU(PIUNum).Name);
            if (state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow >= SmallAirVolFlow &&
                state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).MinOA >= SmallAirVolFlow) {
                MinPriAirFlowFracDes =
                    state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).MinOA / state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow;
            } else {
                MinPriAirFlowFracDes = 0.0;
            }
            if (IsAutoSize) {
                state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac = MinPriAirFlowFracDes;
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "Design Size Minimum Primary Air Flow Fraction",
                                             MinPriAirFlowFracDes);
            } else {
                if (state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac > 0.0 && MinPriAirFlowFracDes > 0.0) {
                    MinPriAirFlowFracUser = state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                 "Design Size Minimum Primary Air Flow Fraction",
                                                 MinPriAirFlowFracDes,
                                                 "User-Specified Minimum Primary Air Flow Fraction",
                                                 MinPriAirFlowFracUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MinPriAirFlowFracDes - MinPriAirFlowFracUser) / MinPriAirFlowFracUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePIU: Potential issue with equipment sizing for " + state.dataPowerInductionUnits->PIU(PIUNum).UnitType +
                                            ' ' + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                            ShowContinueError(state, format("User-Specified Minimum Primary Air Flow Fraction of {:.1R}", MinPriAirFlowFracUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Minimum Primary Air Flow Fraction of {:.1R}", MinPriAirFlowFracDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    if (CurTermUnitSizingNum > 0) {
        {
            auto const SELECT_CASE_var(state.dataPowerInductionUnits->PIU(PIUNum).UnitType_Num);
            if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat) {
                TermUnitSizing(CurTermUnitSizingNum).AirVolFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirVolFlow;
            } else if (SELECT_CASE_var == DataDefineEquip::iZnAirLoopEquipType::SingleDuct_ParallelPIU_Reheat) {
                TermUnitSizing(CurTermUnitSizingNum).AirVolFlow =
                    state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirVolFlow +
                    state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac * state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirVolFlow;
            }
        }
    }

    IsAutoSize = false;
    if (state.dataPowerInductionUnits->PIU(PIUNum).FanOnFlowFrac == AutoSize) {
        IsAutoSize = true;
    }
    if (state.dataSize->CurZoneEqNum > 0) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (state.dataPowerInductionUnits->PIU(PIUNum).FanOnFlowFrac > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "User-Specified Fan On Flow Fraction",
                                             state.dataPowerInductionUnits->PIU(PIUNum).FanOnFlowFrac);
            }
        } else {
            CheckZoneSizing(state, state.dataPowerInductionUnits->PIU(PIUNum).UnitType, state.dataPowerInductionUnits->PIU(PIUNum).Name);
            FanOnFlowFracDes = state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac;
            if (IsAutoSize) {
                state.dataPowerInductionUnits->PIU(PIUNum).FanOnFlowFrac = FanOnFlowFracDes;
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "Design Size Fan On Flow Fraction",
                                             FanOnFlowFracDes);
            } else {
                if (state.dataPowerInductionUnits->PIU(PIUNum).FanOnFlowFrac > 0.0 && FanOnFlowFracDes > 0.0) {
                    FanOnFlowFracUser = state.dataPowerInductionUnits->PIU(PIUNum).FanOnFlowFrac;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                 "Design Size Fan On Flow Fraction",
                                                 FanOnFlowFracDes,
                                                 "User-Specified Fan On Flow Fraction",
                                                 FanOnFlowFracUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(FanOnFlowFracDes - FanOnFlowFracUser) / FanOnFlowFracUser) > state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePIU: Potential issue with equipment sizing for " + state.dataPowerInductionUnits->PIU(PIUNum).UnitType +
                                            ' ' + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                            ShowContinueError(state, format("User-Specified Fan On Flow Fraction of {:.1R}", FanOnFlowFracUser));
                            ShowContinueError(state, format("differs from Design Size Fan On Flow Fraction of {:.1R}", FanOnFlowFracDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    IsAutoSize = false;
    if (state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow == AutoSize) { //.or.()) THEN
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "User-Specified Maximum Reheat Water Flow Rate [m3/s]",
                                             state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow);
            }
        } else {
            CheckZoneSizing(state, state.dataPowerInductionUnits->PIU(PIUNum).UnitType, state.dataPowerInductionUnits->PIU(PIUNum).Name);
            if (UtilityRoutines::SameString(state.dataPowerInductionUnits->PIU(PIUNum).HCoilType, "Coil:Heating:Water")) {

                CoilWaterInletNode =
                    GetCoilWaterInletNode(state, "Coil:Heating:Water", state.dataPowerInductionUnits->PIU(PIUNum).HCoil, ErrorsFound);
                CoilWaterOutletNode =
                    GetCoilWaterOutletNode(state, "Coil:Heating:Water", state.dataPowerInductionUnits->PIU(PIUNum).HCoil, ErrorsFound);
                if (IsAutoSize) {
                    PltSizHeatNum = MyPlantSizingIndex(state,
                                                       "Coil:Heating:Water",
                                                       state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                                       CoilWaterInletNode,
                                                       CoilWaterOutletNode,
                                                       ErrorsFound);
                    if (PltSizHeatNum > 0) {

                        if (state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatMassFlow >= SmallAirVolFlow) {
                            CoilInTemp = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU *
                                             state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac +
                                         state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak *
                                             (1.0 - state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac);
                            CoilOutTemp = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesTemp;
                            CoilOutHumRat = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesHumRat;
                            DesMassFlow = state.dataEnvrn->StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                            DesCoilLoad = PsyCpAirFnW(CoilOutHumRat) * DesMassFlow * (CoilOutTemp - CoilInTemp);

                            rho = GetDensityGlycol(state,
                                                   state.dataPlnt->PlantLoop(state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum).FluidName,
                                                   DataGlobalConstants::HWInitConvTemp,
                                                   state.dataPlnt->PlantLoop(state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum).FluidIndex,
                                                   RoutineName);
                            Cp = GetSpecificHeatGlycol(state,
                                                       state.dataPlnt->PlantLoop(state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum).FluidName,
                                                       DataGlobalConstants::HWInitConvTemp,
                                                       state.dataPlnt->PlantLoop(state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum).FluidIndex,
                                                       RoutineName);

                            MaxVolHotWaterFlowDes = DesCoilLoad / (state.dataSize->PlantSizData(PltSizHeatNum).DeltaT * Cp * rho);
                        } else {
                            MaxVolHotWaterFlowDes = 0.0;
                        }
                    } else {
                        ShowSevereError(state, "Autosizing of water flow requires a heating loop Sizing:Plant object");
                        ShowContinueError(state,
                                          "Occurs in" + state.dataPowerInductionUnits->PIU(PIUNum).UnitType +
                                              " Object=" + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                        ErrorsFound = true;
                    }
                }
                if (IsAutoSize) {
                    state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow = MaxVolHotWaterFlowDes;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                 "Design Size Maximum Reheat Water Flow Rate [m3/s]",
                                                 MaxVolHotWaterFlowDes);
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                 "Design Size Reheat Coil Inlet Air Temperature [C]",
                                                 state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU);
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                 "Design Size Reheat Coil Inlet Air Humidity Ratio [kgWater/kgDryAir]",
                                                 state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInHumRatTU);
                } else { // Hardsize with sizing data
                    if (state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow > 0.0 && MaxVolHotWaterFlowDes > 0.0) {
                        MaxVolHotWaterFlowUser = state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                     state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                     "Design Size Maximum Reheat Water Flow Rate [m3/s]",
                                                     MaxVolHotWaterFlowDes,
                                                     "User-Specified Maximum Reheat Water Flow Rate [m3/s]",
                                                     MaxVolHotWaterFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser) / MaxVolHotWaterFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizePIU: Potential issue with equipment sizing for " +
                                                state.dataPowerInductionUnits->PIU(PIUNum).UnitType + ' ' +
                                                state.dataPowerInductionUnits->PIU(PIUNum).Name);
                                ShowContinueError(state,
                                                  format("User-Specified Maximum Reheat Water Flow Rate of {:.5R} [m3/s]", MaxVolHotWaterFlowUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Maximum Reheat Water Flow Rate of {:.5R} [m3/s]", MaxVolHotWaterFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            } else {
                state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow = 0.0;
            }
        }
    }

    IsAutoSize = false;
    if (state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotSteamFlow == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow > 0.0) {
                BaseSizer::reportSizerOutput(state,
                                             state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                             state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                             "User-Specified Maximum Reheat Steam Flow Rate [m3/s]",
                                             state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow);
            }
        } else {
            if (UtilityRoutines::SameString(state.dataPowerInductionUnits->PIU(PIUNum).HCoilType, "Coil:Heating:Steam")) {

                CoilSteamInletNode =
                    GetCoilSteamInletNode(state, "Coil:Heating:Steam", state.dataPowerInductionUnits->PIU(PIUNum).HCoil, ErrorsFound);
                CoilSteamOutletNode =
                    GetCoilSteamOutletNode(state, "Coil:Heating:Steam", state.dataPowerInductionUnits->PIU(PIUNum).HCoil, ErrorsFound);
                if (IsAutoSize) {
                    PltSizHeatNum = MyPlantSizingIndex(state,
                                                       "Coil:Heating:Steam",
                                                       state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                                       CoilSteamInletNode,
                                                       CoilSteamOutletNode,
                                                       ErrorsFound);
                    if (PltSizHeatNum > 0) {

                        if (state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatMassFlow >= SmallAirVolFlow) {
                            CoilInTemp = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU *
                                             state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac +
                                         state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak *
                                             (1.0 - state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac);
                            CoilOutTemp = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesTemp;
                            CoilOutHumRat = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesHumRat;
                            DesMassFlow = state.dataEnvrn->StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                            DesCoilLoad = PsyCpAirFnW(CoilOutHumRat) * DesMassFlow * (CoilOutTemp - CoilInTemp);
                            TempSteamIn = 100.00;
                            EnthSteamInDry = GetSatEnthalpyRefrig(
                                state, fluidNameSteam, TempSteamIn, 1.0, state.dataPowerInductionUnits->PIU(PIUNum).HCoil_FluidIndex, RoutineName);
                            EnthSteamOutWet = GetSatEnthalpyRefrig(
                                state, fluidNameSteam, TempSteamIn, 0.0, state.dataPowerInductionUnits->PIU(PIUNum).HCoil_FluidIndex, RoutineName);
                            LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                            SteamDensity = GetSatDensityRefrig(
                                state, fluidNameSteam, TempSteamIn, 1.0, state.dataPowerInductionUnits->PIU(PIUNum).HCoil_FluidIndex, RoutineName);
                            Cp = GetSpecificHeatGlycol(
                                state, fluidNameWater, state.dataSize->PlantSizData(PltSizHeatNum).ExitTemp, DummyWaterIndex, RoutineName);
                            MaxVolHotSteamFlowDes =
                                DesCoilLoad / (SteamDensity * (LatentHeatSteam + state.dataSize->PlantSizData(PltSizHeatNum).DeltaT * Cp));
                        } else {
                            MaxVolHotSteamFlowDes = 0.0;
                        }
                    } else {
                        ShowSevereError(state, "Autosizing of Steam flow requires a heating loop Sizing:Plant object");
                        ShowContinueError(state,
                                          "Occurs in" + state.dataPowerInductionUnits->PIU(PIUNum).UnitType +
                                              " Object=" + state.dataPowerInductionUnits->PIU(PIUNum).Name);
                        ErrorsFound = true;
                    }
                }
                if (IsAutoSize) {
                    state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotSteamFlow = MaxVolHotSteamFlowDes;
                    BaseSizer::reportSizerOutput(state,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                 state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                 "Design Size Maximum Reheat Steam Flow [m3/s]",
                                                 MaxVolHotSteamFlowDes);
                } else {
                    if (state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotSteamFlow > 0.0 && MaxVolHotSteamFlowDes > 0.0) {
                        MaxVolHotSteamFlowUser = state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotSteamFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                                     state.dataPowerInductionUnits->PIU(PIUNum).Name,
                                                     "Design Size Maximum Reheat Steam Flow [m3/s]",
                                                     MaxVolHotSteamFlowDes,
                                                     "User-Specified Maximum Reheat Steam Flow [m3/s]",
                                                     MaxVolHotSteamFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser) / MaxVolHotSteamFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizePIU: Potential issue with equipment sizing for " +
                                                state.dataPowerInductionUnits->PIU(PIUNum).UnitType + ' ' +
                                                state.dataPowerInductionUnits->PIU(PIUNum).Name);
                                ShowContinueError(state, format("User-Specified Maximum Reheat Steam Flow of {:.5R} [m3/s]", MaxVolHotSteamFlowUser));
                                ShowContinueError(
                                    state, format("differs from Design Size Maximum Reheat Steam Flow of {:.5R} [m3/s]", MaxVolHotSteamFlowDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            } else {
                state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotSteamFlow = 0.0;
            }
        }
    }

    if (CurTermUnitSizingNum > 0) {
        TermUnitSizing(CurTermUnitSizingNum).MinFlowFrac = state.dataPowerInductionUnits->PIU(PIUNum).MinPriAirFlowFrac;
        TermUnitSizing(CurTermUnitSizingNum).MaxHWVolFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotWaterFlow;
        TermUnitSizing(CurTermUnitSizingNum).MaxSTVolFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxVolHotSteamFlow;
        TermUnitSizing(CurTermUnitSizingNum).DesHeatingLoad = DesCoilLoad; // coil report
        TermUnitSizing(CurTermUnitSizingNum).InducesPlenumAir = state.dataPowerInductionUnits->PIU(PIUNum).InducesPlenumAir;
        if (state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num == iHCoilType::SimpleHeating) {
            SetCoilDesFlow(state,
                           state.dataPowerInductionUnits->PIU(PIUNum).HCoilType,
                           state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                           TermUnitSizing(CurTermUnitSizingNum).AirVolFlow,
                           ErrorsFound);
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

void CalcSeriesPIU(EnergyPlusData &state,
                   int const PIUNum,             // number of the current PIU being simulated
                   int const ZoneNum,            // number of zone being served
                   int const ZoneNode,           // zone node number
                   bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Simulate a series powered induction unit; adjust its primary air flow
    // and reheat coil output to match the zone load.

    // METHODOLOGY EMPLOYED:
    // If unit is on and there is a cooling load:
    // (1) simulates mixer and fan at max secondary air flow and heating coil
    //     off. Obtains fan temperature increase.
    // (2) Calculates primary and secomdary air flow to meet zone load and
    //     resimulates mixer, fan, and (off) coil.
    // If unit is on and there is a heating load
    // (1) sets primary air flow to a minimum.
    // (2) simulates mixer and fan
    // (3) if reheat is hot water, calls ControlCompOutput to simulate hot
    //     water coil and adjust water flow to match coil output to the zone load.
    // (4) if reheat is electric or gas calls SimulateHeatingCoilComponents to
    //     simulate coil at coil output that matches the zone load

    // REFERENCES:
    // na

    // Using/Aliasing
    using namespace DataZoneEnergyDemands;
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSpecificHeatGlycol;
    using HeatingCoils::SimulateHeatingCoilComponents;
    using MixerComponent::SimAirMixer;
    using PlantUtilities::SetComponentFlowRate;
    using SteamCoils::SimulateSteamCoilComponents;
    using WaterCoils::SimulateWaterCoilComponents;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:

    // INTERFACE BLOCK SPECIFICATIONS

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 QZnReq;                // heating or cooling needed by zone [Watts]
    Real64 QToHeatSetPt;          // [W]  remaining load to heating setpoint
    Real64 QActualHeating;        // the heating load seen by the reheat coil [W]
    Real64 PowerMet;              // power supplied
    bool UnitOn(true);            // TRUE if unit is on
    bool PriOn(true);             // TRUE if primary air available
    bool HCoilOn(true);           // TRUE if heating coil is on
    int ControlNode(0);           // the hot water or cold water inlet node
    Real64 ControlOffset;         // tolerance for output control
    Real64 MaxWaterFlow;          // maximum water flow for heating or cooling [kg/s]
    Real64 MinWaterFlow;          // minimum water flow for heating or cooling [kg/s]
    int OutletNode;               // unit air outlet node
    int PriNode;                  // unit primary air inlet node
    int SecNode;                  // unit secondary air inlet node
    int HCoilInAirNode;           // air inlet node of reheat coil
    Real64 QCoilReq;              // required heating coil outlet to meet zone load
    Real64 PriAirMassFlow;        // primary air mass flow rate [kg/s]
    Real64 PriAirMassFlowMax;     // max primary air mass flow rate [kg/s]
    Real64 PriAirMassFlowMin;     // min primary air mass flow rate [kg/s]
    Real64 SecAirMassFlow;        // secondary air mass flow rate [kg/s]
    Real64 CpAirZn;               // zone air specific heat [J/kg-C]
    Real64 FanDeltaTemp(0.0);     // fan temperature rise [C]
    Real64 OutletTempNeeded(0.0); // unit outlet temperature needed to meet cooling load
    Real64 MixTempNeeded(0.0);    // mixer outlet temperature needed to meet cooling load
    Real64 MinSteamFlow;
    Real64 MaxSteamFlow;
    Real64 mdot; // local plant fluid flow rate kg/s
    // Initialize local fan flags to global system flags
    bool PIUTurnFansOn =
        (state.dataHVACGlobal->TurnFansOn || state.dataHVACGlobal->TurnZoneFansOnlyOn); // If True, overrides fan schedule and cycles PIU fan on
    bool PIUTurnFansOff = state.dataHVACGlobal->TurnFansOff; // If True, overrides fan schedule and PIUTurnFansOn and cycles PIU fan off

    // initialize local variables
    ControlOffset = state.dataPowerInductionUnits->PIU(PIUNum).HotControlOffset;
    OutletNode = state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode;
    PriNode = state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode;
    SecNode = state.dataPowerInductionUnits->PIU(PIUNum).SecAirInNode;
    HCoilInAirNode = state.dataPowerInductionUnits->PIU(PIUNum).HCoilInAirNode;
    ControlNode = state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode;
    PriAirMassFlow = state.dataLoopNodes->Node(PriNode).MassFlowRate;
    PriAirMassFlowMax = state.dataLoopNodes->Node(PriNode).MassFlowRateMaxAvail;
    PriAirMassFlowMin = state.dataLoopNodes->Node(PriNode).MassFlowRateMinAvail;
    SecAirMassFlow = state.dataLoopNodes->Node(SecNode).MassFlowRate;
    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
    QToHeatSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);

    // On the first HVAC iteration the system values are given to the controller, but after that
    // the demand limits are in place and there needs to be feedback to the Zone Equipment
    if (ControlNode > 0) {
        if (FirstHVACIteration) {
            MaxWaterFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxHotWaterFlow;
            MinWaterFlow = state.dataPowerInductionUnits->PIU(PIUNum).MinHotWaterFlow;
            MaxSteamFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxHotWaterFlow; // Need TO change THESE******************************
            MinSteamFlow = state.dataPowerInductionUnits->PIU(PIUNum).MinHotWaterFlow;
        } else {
            MaxWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
            MinWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
            MaxSteamFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
            MinSteamFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
        }
    }
    if (GetCurrentScheduleValue(state, state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr) <= 0.0) UnitOn = false;
    if ((GetCurrentScheduleValue(state, state.dataPowerInductionUnits->PIU(PIUNum).FanAvailSchedPtr) <= 0.0 || PIUTurnFansOff) && !PIUTurnFansOn)
        UnitOn = false;
    if (PriAirMassFlow <= SmallMassFlow || PriAirMassFlowMax <= SmallMassFlow) PriOn = false;
    // Set the mass flow rates
    if (UnitOn) {
        // unit is on
        if (!PriOn) {
            // no primary air flow
            PriAirMassFlow = 0.0;
            // PIU fan off if there is no heating load, also reset fan flag if fan should be off
            if (QZnReq <= SmallLoad) {
                SecAirMassFlow = 0.0;
                PIUTurnFansOn = false;
            } else {
                SecAirMassFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow;
            }
        } else if (state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) || std::abs(QZnReq) < SmallLoad) {
            // in deadband or very small load: set primary air flow to the minimum
            PriAirMassFlow = PriAirMassFlowMin;
            SecAirMassFlow = max(0.0, state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow - PriAirMassFlow);
        } else if (QZnReq > SmallLoad) {
            // heating: set primary air flow to the minimum
            PriAirMassFlow = PriAirMassFlowMin;
            SecAirMassFlow = max(0.0, state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow - PriAirMassFlow);
        } else {
            // cooling: set the primary air flow rate to meet the load.
            // First calculate the fan temperature rise
            // use only secondary air for this calculation
            state.dataLoopNodes->Node(PriNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(SecNode).MassFlowRate = state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow;
            SimAirMixer(
                state, state.dataPowerInductionUnits->PIU(PIUNum).MixerName, state.dataPowerInductionUnits->PIU(PIUNum).Mixer_Num); // fire the mixer
            if (state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataHVACFan->fanObjs[state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index]->simulate(
                    state, _, PIUTurnFansOn, PIUTurnFansOff, _);
            } else if (state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                Fans::SimulateFanComponents(state,
                                            state.dataPowerInductionUnits->PIU(PIUNum).FanName,
                                            FirstHVACIteration,
                                            state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index,
                                            _,
                                            PIUTurnFansOn,
                                            PIUTurnFansOff); // fire the fan
            }

            FanDeltaTemp = state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(SecNode).Temp;
            // using the required zone load, calculate the air temperature needed to meet the load
            // PIU(PIUNum)%MaxTotAirMassFlow * CpAirZn * (OutletTempNeeded - state.dataLoopNodes->Node(ZoneNodeNum)%Temp) = QZnReq
            OutletTempNeeded =
                state.dataLoopNodes->Node(ZoneNode).Temp + QZnReq / (state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow * CpAirZn);
            MixTempNeeded = OutletTempNeeded - FanDeltaTemp;
            if (MixTempNeeded <= state.dataLoopNodes->Node(PriNode).Temp) {
                PriAirMassFlow = PriAirMassFlowMax;
            } else if (MixTempNeeded >= state.dataLoopNodes->Node(PriNode).Temp && MixTempNeeded >= state.dataLoopNodes->Node(SecNode).Temp) {
                PriAirMassFlow = PriAirMassFlowMin;
            } else {
                PriAirMassFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow *
                                 (state.dataLoopNodes->Node(SecNode).Temp - MixTempNeeded) /
                                 max(SmallTempDiff, state.dataLoopNodes->Node(SecNode).Temp - state.dataLoopNodes->Node(PriNode).Temp);
                PriAirMassFlow = min(max(PriAirMassFlow, PriAirMassFlowMin), PriAirMassFlowMax);
            }
            SecAirMassFlow = max(0.0, state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow - PriAirMassFlow);
        }
    } else {
        // unit is off ; no flow
        PriAirMassFlow = 0.0;
        SecAirMassFlow = 0.0;
    }
    // Set inlet node flowrates
    state.dataLoopNodes->Node(PriNode).MassFlowRate = PriAirMassFlow;
    state.dataLoopNodes->Node(SecNode).MassFlowRate = SecAirMassFlow;
    if (PriAirMassFlowMax == 0) {
        state.dataPowerInductionUnits->PIU(PIUNum).PriDamperPosition = 0;
    } else {
        state.dataPowerInductionUnits->PIU(PIUNum).PriDamperPosition = PriAirMassFlow / PriAirMassFlowMax;
    }
    // now that inlet airflows have been set, the terminal bos components can be simulated.

    // fire the mixer
    SimAirMixer(state, state.dataPowerInductionUnits->PIU(PIUNum).MixerName, state.dataPowerInductionUnits->PIU(PIUNum).Mixer_Num);
    // fire the fan
    if (state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
        state.dataHVACFan->fanObjs[state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index]->simulate(state, _, PIUTurnFansOn, PIUTurnFansOff, _);
    } else if (state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
        Fans::SimulateFanComponents(state,
                                    state.dataPowerInductionUnits->PIU(PIUNum).FanName,
                                    FirstHVACIteration,
                                    state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index,
                                    _,
                                    PIUTurnFansOn,
                                    PIUTurnFansOff); // fire the fan
    }
    // check if heating coil is off
    QActualHeating = QToHeatSetPt - state.dataLoopNodes->Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                        (state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
    if ((!UnitOn) || (QActualHeating < SmallLoad) || (state.dataHeatBalFanSys->TempControlType(ZoneNum) == SingleCoolingSetPoint) ||
        (PriAirMassFlow > PriAirMassFlowMin)) {
        HCoilOn = false;
    }
    // fire the heating coil

    {
        auto const SELECT_CASE_var(state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num);

        if (SELECT_CASE_var == iHCoilType::SimpleHeating) { // COIL:WATER:SIMPLEHEATING
            if (!HCoilOn) {
                // call the reheat coil with the NO FLOW condition
                mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HotCoilOutNodeNum,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HWLoopSide,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HWBranchNum,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HWCompNum);

                SimulateWaterCoilComponents(state,
                                            state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                            FirstHVACIteration,
                                            state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index);
            } else {
                // control water flow to obtain output matching QZnReq
                ControlCompOutput(state,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                  state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index,
                                  FirstHVACIteration,
                                  QActualHeating,
                                  ControlNode,
                                  MaxWaterFlow,
                                  MinWaterFlow,
                                  ControlOffset,
                                  state.dataPowerInductionUnits->PIU(PIUNum).ControlCompTypeNum,
                                  state.dataPowerInductionUnits->PIU(PIUNum).CompErrIndex,
                                  HCoilInAirNode,
                                  OutletNode,
                                  _,
                                  _,
                                  _,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HWLoopSide,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HWBranchNum);
            }
        } else if (SELECT_CASE_var == iHCoilType::SteamAirHeating) { // COIL:STEAM:AIRHEATING
            if (!HCoilOn) {
                QCoilReq = 0.0;
            } else {
                QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                              (state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
            }
            SimulateSteamCoilComponents(state,
                                        state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                        FirstHVACIteration,
                                        state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index,
                                        QCoilReq);

        } else if (SELECT_CASE_var == iHCoilType::Electric) { // COIL:ELECTRIC:HEATING
            if (!HCoilOn) {
                QCoilReq = 0.0;
            } else {
                QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                              (state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
            }
            SimulateHeatingCoilComponents(state,
                                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                          FirstHVACIteration,
                                          QCoilReq,
                                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index);

        } else if (SELECT_CASE_var == iHCoilType::Gas) { // COIL:GAS:HEATING
            if (!HCoilOn) {
                QCoilReq = 0.0;
            } else {
                QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                              (state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
            }
            SimulateHeatingCoilComponents(state,
                                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                          FirstHVACIteration,
                                          QCoilReq,
                                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index);
        }
    }

    PowerMet = state.dataLoopNodes->Node(OutletNode).MassFlowRate *
               (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat) -
                PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat));
    state.dataPowerInductionUnits->PIU(PIUNum).HeatingRate = max(0.0, PowerMet);
    state.dataPowerInductionUnits->PIU(PIUNum).SensCoolRate = std::abs(min(DataPrecisionGlobals::constant_zero, PowerMet));
    if (state.dataLoopNodes->Node(OutletNode).MassFlowRate == 0.0) {
        state.dataLoopNodes->Node(PriNode).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(SecNode).MassFlowRate = 0.0;
    }
    if (state.dataPowerInductionUnits->PIU(PIUNum).InducesPlenumAir) {
        state.dataHVACGlobal->PlenumInducedMassFlow = state.dataLoopNodes->Node(SecNode).MassFlowRate;
    } else {
        state.dataHVACGlobal->PlenumInducedMassFlow = 0.0;
    }
    state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(PIUNum).ADUNum).MassFlowRatePlenInd =
        state.dataHVACGlobal->PlenumInducedMassFlow;
    state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = state.dataPowerInductionUnits->PIU(PIUNum).MaxTotAirMassFlow;
}

void CalcParallelPIU(EnergyPlusData &state,
                     int const PIUNum,             // number of the current PIU being simulated
                     int const ZoneNum,            // number of zone being served
                     int const ZoneNode,           // zone node number
                     bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2000
    //       MODIFIED       September 2016, March 2017

    // PURPOSE OF THIS SUBROUTINE:
    // Simulate a parallel powered induction unit; adjust its primary air flow
    // and reheat coil output to match the zone load.

    // METHODOLOGY EMPLOYED:
    // If unit is on and there is a cooling load:
    // (1) simulate fan at max secondary air flow and heating coil
    //     off. Obtains fan temperature increase.
    // (2) Calculates primary and secomdary air flow to meet zone load.
    //     (a) Assume fan is off and calculate primary air flow to meet cooling load.
    //     (b1) If calculated primary air flow is above the fan turn on ratio, fan is off.
    //         Otherwise fan is on; calculate mixed secondary and primary air flow that
    //         will meet the zone load
    //     (b2) If the fan turn on ratio is zero, then the fan is on only if reheat is needed.
    //  (3) Simulate fan, mixer, and (off) heating coil to obtain zone inlet conditions.
    // If unit is on and there is a heating load
    // (1) sets primary air flow to a minimum.
    // (2) simulates fan and mixer
    // (3) if reheat is hot water, calls ControlCompOutput to simulate hot
    //     water coil and adjust water flow to match coil output to the zone load.
    // (4) if reheat is electric or gas calls SimulateHeatingCoilComponents to
    //     simulate coil at coil output that matches the zone load

    using namespace DataZoneEnergyDemands;
    using HeatingCoils::SimulateHeatingCoilComponents;
    using MixerComponent::SimAirMixer;
    using PlantUtilities::SetComponentFlowRate;
    using SteamCoils::SimulateSteamCoilComponents;
    using WaterCoils::SimulateWaterCoilComponents;

    Real64 QZnReq;            // heating or cooling needed by zone [Watts]
    Real64 QToHeatSetPt;      // [W]  remaining load to heating setpoint
    Real64 QActualHeating;    // the heating load seen by the reheat coil [W]
    Real64 PowerMet;          // power supplied
    bool UnitOn(true);        // TRUE if unit is on
    bool PriOn(true);         // TRUE if primary air available
    bool HCoilOn(true);       // TRUE if heating coil is on
    int ControlNode(0);       // the hot water or cold water inlet node
    Real64 ControlOffset;     // tolerance for output control
    Real64 MaxWaterFlow;      // maximum water flow for heating or cooling [kg/s]
    Real64 MinWaterFlow;      // minimum water flow for heating or cooling [kg/s]
    int OutletNode;           // unit air outlet node
    int PriNode;              // unit primary air inlet node
    int SecNode;              // unit secondary air inlet node
    int HCoilInAirNode;       // air inlet node of reheat coil
    Real64 QCoilReq;          // required heating coil outlet to meet zone load
    Real64 PriAirMassFlow;    // primary air mass flow rate [kg/s]
    Real64 PriAirMassFlowMax; // max primary air mass flow rate [kg/s]
    Real64 PriAirMassFlowMin; // min primary air mass flow rate [kg/s]
    Real64 SecAirMassFlow;    // secondary air mass flow rate [kg/s]
    Real64 CpAirZn;           // zone air specific heat [J/kg-C]
    Real64 FanDeltaTemp(0.0); // fan temperature rise [C]
    Real64 mdot;              // local fluid flow rate kg/s

    // initialize local variables
    ControlOffset = state.dataPowerInductionUnits->PIU(PIUNum).HotControlOffset;
    OutletNode = state.dataPowerInductionUnits->PIU(PIUNum).OutAirNode;
    PriNode = state.dataPowerInductionUnits->PIU(PIUNum).PriAirInNode;
    SecNode = state.dataPowerInductionUnits->PIU(PIUNum).SecAirInNode;
    HCoilInAirNode = state.dataPowerInductionUnits->PIU(PIUNum).HCoilInAirNode;
    ControlNode = state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode;
    PriAirMassFlow = state.dataLoopNodes->Node(PriNode).MassFlowRate;
    PriAirMassFlowMax = state.dataLoopNodes->Node(PriNode).MassFlowRateMaxAvail;
    PriAirMassFlowMin = state.dataLoopNodes->Node(PriNode).MassFlowRateMinAvail;
    SecAirMassFlow = state.dataLoopNodes->Node(SecNode).MassFlowRate;
    QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired;
    QToHeatSetPt = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
    CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);
    // Initialize local fan flags to global system flags
    bool PIUTurnFansOn =
        (state.dataHVACGlobal->TurnFansOn || state.dataHVACGlobal->TurnZoneFansOnlyOn); // If True, overrides fan schedule and cycles PIU fan on
    bool PIUTurnFansOff = state.dataHVACGlobal->TurnFansOff; // If True, overrides fan schedule and PIUTurnFansOn and cycles PIU fan off

    // On the first HVAC iteration the system values are given to the controller, but after that
    // the demand limits are in place and there needs to be feedback to the Zone Equipment
    if (ControlNode > 0) {
        if (FirstHVACIteration) {
            MaxWaterFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxHotWaterFlow;
            MinWaterFlow = state.dataPowerInductionUnits->PIU(PIUNum).MinHotWaterFlow;
        } else {
            MaxWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMaxAvail;
            MinWaterFlow = state.dataLoopNodes->Node(ControlNode).MassFlowRateMinAvail;
        }
    }
    if (GetCurrentScheduleValue(state, state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr) <= 0.0) UnitOn = false;
    if (PriAirMassFlow <= SmallMassFlow || PriAirMassFlowMax <= SmallMassFlow) PriOn = false;
    // Set the mass flow rates
    if (UnitOn) {
        // unit is on
        // Calculate if reheat is needed
        bool ReheatRequired = false;
        Real64 qMinPrimary =
            PriAirMassFlowMin * (CpAirZn * min(-SmallTempDiff, (state.dataLoopNodes->Node(PriNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp)));
        if (qMinPrimary < QToHeatSetPt) ReheatRequired = true;

        if (!PriOn) {
            // no primary air flow
            PriAirMassFlow = 0.0;
            // PIU fan off if there is no heating load, also reset fan flag if fan should be off
            if (QZnReq <= SmallLoad) {
                SecAirMassFlow = 0.0;
                PIUTurnFansOn = false;
            } else {
                SecAirMassFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirMassFlow;
                PIUTurnFansOn = (state.dataHVACGlobal->TurnFansOn || state.dataHVACGlobal->TurnZoneFansOnlyOn);
            }
        } else if (state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) || std::abs(QZnReq) < SmallLoad) {
            // in deadband or very small load: set primary air flow to the minimum
            PriAirMassFlow = PriAirMassFlowMin;
            // PIU fan off if reheat is not needed, also reset fan flag if fan should be off
            if (ReheatRequired) {
                SecAirMassFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirMassFlow;
                PIUTurnFansOn = true;
            } else {
                SecAirMassFlow = 0.0;
                PIUTurnFansOn = false;
            }
        } else if (QZnReq > SmallLoad) {
            // heating: set primary air flow to the minimum
            PriAirMassFlow = PriAirMassFlowMin;
            SecAirMassFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirMassFlow;
        } else {
            // cooling: set the primary air flow rate to meet the load.
            // First calculate the fan temperature rise
            state.dataLoopNodes->Node(SecNode).MassFlowRate = state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirMassFlow;
            state.dataLoopNodes->Node(SecNode).MassFlowRateMaxAvail = state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirMassFlow;
            state.dataLoopNodes->Node(PriNode).MassFlowRate = 0.0;

            if (state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataHVACFan->fanObjs[state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index]->simulate(
                    state, _, PIUTurnFansOn, PIUTurnFansOff, _);
            } else if (state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                Fans::SimulateFanComponents(state,
                                            state.dataPowerInductionUnits->PIU(PIUNum).FanName,
                                            FirstHVACIteration,
                                            state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index,
                                            _,
                                            PIUTurnFansOn,
                                            PIUTurnFansOff); // fire the fan
            }
            SimAirMixer(
                state, state.dataPowerInductionUnits->PIU(PIUNum).MixerName, state.dataPowerInductionUnits->PIU(PIUNum).Mixer_Num); // fire the mixer
            FanDeltaTemp = state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(SecNode).Temp;
            // Assuming the fan is off, calculate the primary air flow needed to meet the zone cooling demand.
            // CpAir*PriAirMassFlow*(Node(PriNode)%Temp - Node(ZoneNodeNum)%Temp) = QZnReq
            PriAirMassFlow =
                QZnReq / (CpAirZn * min(-SmallTempDiff, (state.dataLoopNodes->Node(PriNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp)));
            PriAirMassFlow = min(max(PriAirMassFlow, PriAirMassFlowMin), PriAirMassFlowMax);
            // check for fan on or off
            if ((PriAirMassFlow > state.dataPowerInductionUnits->PIU(PIUNum).FanOnAirMassFlow) && !ReheatRequired) {
                SecAirMassFlow = 0.0; // Fan is off unless reheat is required; no secondary air; also reset fan flag
                PIUTurnFansOn = false;
            } else {
                // fan is on; recalc primary air flow
                // CpAir*PriAirMassFlow*(Node(PriNode)%Temp - Node(ZoneNodeNum)%Temp) +
                //   CpAir*SecAirMassFlow*(Node(SecNode)%Temp + FanDeltaTemp - Node(ZoneNodeNum)%Temp) = QZnReq
                PriAirMassFlow =
                    (QZnReq -
                     CpAirZn * SecAirMassFlow * (state.dataLoopNodes->Node(SecNode).Temp + FanDeltaTemp - state.dataLoopNodes->Node(ZoneNode).Temp)) /
                    (CpAirZn * min(-SmallTempDiff, (state.dataLoopNodes->Node(PriNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp)));
                PriAirMassFlow = min(max(PriAirMassFlow, PriAirMassFlowMin), PriAirMassFlowMax);
                SecAirMassFlow = state.dataPowerInductionUnits->PIU(PIUNum).MaxSecAirMassFlow;
            }
        }
    } else {
        // unit is off; no flow
        PriAirMassFlow = 0.0;
        SecAirMassFlow = 0.0;
    }
    // Set inlet node flowrates
    state.dataLoopNodes->Node(PriNode).MassFlowRate = PriAirMassFlow;
    state.dataLoopNodes->Node(SecNode).MassFlowRate = SecAirMassFlow;
    state.dataLoopNodes->Node(SecNode).MassFlowRateMaxAvail = SecAirMassFlow;
    if (PriAirMassFlowMax == 0) {
        state.dataPowerInductionUnits->PIU(PIUNum).PriDamperPosition = 0;
    } else {
        state.dataPowerInductionUnits->PIU(PIUNum).PriDamperPosition = PriAirMassFlow / PriAirMassFlowMax;
    }
    // now that inlet airflows have been set, the terminal box components can be simulated.
    // fire the fan

    if (state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
        state.dataHVACFan->fanObjs[state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index]->simulate(state, _, PIUTurnFansOn, PIUTurnFansOff, _);
    } else if (state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
        Fans::SimulateFanComponents(state,
                                    state.dataPowerInductionUnits->PIU(PIUNum).FanName,
                                    FirstHVACIteration,
                                    state.dataPowerInductionUnits->PIU(PIUNum).Fan_Index,
                                    _,
                                    PIUTurnFansOn,
                                    PIUTurnFansOff); // fire the fan
    }
    // fire the mixer
    SimAirMixer(state, state.dataPowerInductionUnits->PIU(PIUNum).MixerName, state.dataPowerInductionUnits->PIU(PIUNum).Mixer_Num);
    // check if heating coil is off
    QActualHeating = QToHeatSetPt - state.dataLoopNodes->Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                        (state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
    if ((!UnitOn) || (QActualHeating < SmallLoad) || (state.dataHeatBalFanSys->TempControlType(ZoneNum) == SingleCoolingSetPoint) ||
        (PriAirMassFlow > PriAirMassFlowMin)) {
        HCoilOn = false;
    }
    // fire the heating coil
    {
        auto const SELECT_CASE_var(state.dataPowerInductionUnits->PIU(PIUNum).HCoilType_Num);

        if (SELECT_CASE_var == iHCoilType::SimpleHeating) { // COIL:WATER:SIMPLEHEATING
            if (!HCoilOn) {
                // call the reheat coil with the NO FLOW condition
                mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HotControlNode,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HotCoilOutNodeNum,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HWLoopSide,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HWBranchNum,
                                     state.dataPowerInductionUnits->PIU(PIUNum).HWCompNum);
                SimulateWaterCoilComponents(state,
                                            state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                            FirstHVACIteration,
                                            state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index);
            } else {
                // control water flow to obtain output matching QZnReq
                ControlCompOutput(state,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                  state.dataPowerInductionUnits->PIU(PIUNum).UnitType,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index,
                                  FirstHVACIteration,
                                  QActualHeating,
                                  ControlNode,
                                  MaxWaterFlow,
                                  MinWaterFlow,
                                  ControlOffset,
                                  state.dataPowerInductionUnits->PIU(PIUNum).ControlCompTypeNum,
                                  state.dataPowerInductionUnits->PIU(PIUNum).CompErrIndex,
                                  HCoilInAirNode,
                                  OutletNode,
                                  _,
                                  _,
                                  _,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HWLoopNum,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HWLoopSide,
                                  state.dataPowerInductionUnits->PIU(PIUNum).HWBranchNum);
            }
        } else if (SELECT_CASE_var == iHCoilType::SteamAirHeating) { // COIL:STEAM:AIRHEATING
            if (!HCoilOn) {
                QCoilReq = 0.0;
            } else {
                QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                              (state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
            }
            SimulateSteamCoilComponents(state,
                                        state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                        FirstHVACIteration,
                                        state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index,
                                        QCoilReq);
        } else if (SELECT_CASE_var == iHCoilType::Electric) { // COIL:ELECTRIC:HEATING
            if (!HCoilOn) {
                QCoilReq = 0.0;
            } else {
                QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                              (state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
            }
            SimulateHeatingCoilComponents(state,
                                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                          FirstHVACIteration,
                                          QCoilReq,
                                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index);

        } else if (SELECT_CASE_var == iHCoilType::Gas) { // COIL:GAS:HEATING
            if (!HCoilOn) {
                QCoilReq = 0.0;
            } else {
                QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(HCoilInAirNode).MassFlowRate * CpAirZn *
                                              (state.dataLoopNodes->Node(HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
            }
            SimulateHeatingCoilComponents(state,
                                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil,
                                          FirstHVACIteration,
                                          QCoilReq,
                                          state.dataPowerInductionUnits->PIU(PIUNum).HCoil_Index);
        }
    }
    PowerMet = state.dataLoopNodes->Node(OutletNode).MassFlowRate *
               (PsyHFnTdbW(state.dataLoopNodes->Node(OutletNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat) -
                PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat));
    state.dataPowerInductionUnits->PIU(PIUNum).HeatingRate = max(0.0, PowerMet);
    state.dataPowerInductionUnits->PIU(PIUNum).SensCoolRate = std::abs(min(DataPrecisionGlobals::constant_zero, PowerMet));
    if (state.dataLoopNodes->Node(OutletNode).MassFlowRate == 0.0) {
        state.dataLoopNodes->Node(PriNode).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(SecNode).MassFlowRate = 0.0;
    }
    if (state.dataPowerInductionUnits->PIU(PIUNum).InducesPlenumAir) {
        state.dataHVACGlobal->PlenumInducedMassFlow = state.dataLoopNodes->Node(SecNode).MassFlowRate;
    } else {
        state.dataHVACGlobal->PlenumInducedMassFlow = 0.0;
    }
    state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(PIUNum).ADUNum).MassFlowRatePlenInd =
        state.dataHVACGlobal->PlenumInducedMassFlow;
    state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = state.dataPowerInductionUnits->PIU(PIUNum).MaxPriAirMassFlow;
}

void ReportPIU(EnergyPlusData &state, int const PIUNum) // number of the current fan coil unit being simulated
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Fills some of the report variables for the PIU terminal boxes

    // METHODOLOGY EMPLOYED:
    // NA

    // REFERENCES:
    // na

    // Using/Aliasing
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:

    // SUBROUTINE PARAMETER DEFINITIONS:
    // na

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // DERIVED TYPE DEFINITIONS
    // na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    state.dataPowerInductionUnits->PIU(PIUNum).HeatingEnergy =
        state.dataPowerInductionUnits->PIU(PIUNum).HeatingRate * TimeStepSys * DataGlobalConstants::SecInHour;
    state.dataPowerInductionUnits->PIU(PIUNum).SensCoolEnergy =
        state.dataPowerInductionUnits->PIU(PIUNum).SensCoolRate * TimeStepSys * DataGlobalConstants::SecInHour;

    // set zone OA Volume flow rate
    state.dataPowerInductionUnits->PIU(PIUNum).CalcOutdoorAirVolumeFlowRate(state);
}

// ===================== Utilities =====================================

bool PIUnitHasMixer(EnergyPlusData &state, std::string_view CompName) // component (mixer) name
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   September 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Given a mixer name, this routine determines if that mixer is found on
    // PIUnits.

    // Return value
    bool YesNo; // True if found

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int ItemNum;

    if (state.dataPowerInductionUnits->GetPIUInputFlag) {
        GetPIUs(state);
        state.dataPowerInductionUnits->GetPIUInputFlag = false;
    }

    YesNo = false;
    if (state.dataPowerInductionUnits->NumPIUs > 0) {
        ItemNum = UtilityRoutines::FindItemInList(CompName, state.dataPowerInductionUnits->PIU, &PowIndUnitData::MixerName);
        if (ItemNum > 0) YesNo = true;
    }

    return YesNo;
}

void PIUInducesPlenumAir(EnergyPlusData &state, int const NodeNum) // induced air node number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   January 2012
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Marks a PIU air terminal unit as obtaining its induced air from
    // a plenum.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PIUIndex;

    if (state.dataPowerInductionUnits->GetPIUInputFlag) {
        GetPIUs(state);
        state.dataPowerInductionUnits->GetPIUInputFlag = false;
    }

    for (PIUIndex = 1; PIUIndex <= state.dataPowerInductionUnits->NumPIUs; ++PIUIndex) {
        if (NodeNum == state.dataPowerInductionUnits->PIU(PIUIndex).SecAirInNode) {
            state.dataPowerInductionUnits->PIU(PIUIndex).InducesPlenumAir = true;
            break;
        }
    }
}

void PowIndUnitData::CalcOutdoorAirVolumeFlowRate(EnergyPlusData &state)
{
    // calculates zone outdoor air volume flow rate using the supply air flow rate and OA fraction
    if (this->AirLoopNum > 0) {
        this->OutdoorAirFlowRate = (state.dataLoopNodes->Node(this->PriAirInNode).MassFlowRate / state.dataEnvrn->StdRhoAir) *
                                   state.dataAirLoop->AirLoopFlow(this->AirLoopNum).OAFrac;
    } else {
        this->OutdoorAirFlowRate = 0.0;
    }
}

} // namespace EnergyPlus::PoweredInductionUnits
