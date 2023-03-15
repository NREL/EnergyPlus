// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

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
using DataHVACGlobals::SmallAirVolFlow;
using DataHVACGlobals::SmallLoad;
using DataHVACGlobals::SmallMassFlow;
using DataHVACGlobals::SmallTempDiff;
using namespace ScheduleManager;
using Psychrometrics::PsyCpAirFnW;
using Psychrometrics::PsyHFnTdbW;
using SteamCoils::SimulateSteamCoilComponents;
using namespace FluidProperties;

constexpr const char *fluidNameSteam("STEAM");
constexpr const char *fluidNameWater("WATER");

void SimPIU(EnergyPlusData &state,
            std::string_view CompName,     // name of the PIU
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
    int PIUNum = 0; // index of powered induction unit being simulated

    // First time SimPIU is called, get the input for all the fan coil units
    if (state.dataPowerInductionUnits->GetPIUInputFlag) {
        GetPIUs(state);
        state.dataPowerInductionUnits->GetPIUInputFlag = false;
    }

    // Get the powered induction unit index
    if (CompIndex == 0) {
        PIUNum = UtilityRoutines::FindItemInList(CompName, state.dataPowerInductionUnits->PIU);
        if (PIUNum == 0) {
            ShowFatalError(state, format("SimPIU: PIU Unit not found={}", CompName));
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
    switch (state.dataPowerInductionUnits->PIU(PIUNum).UnitType_Num) {

    case DataDefineEquip::ZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat: { //  'AirTerminal:SingleDuct:SeriesPIU:Reheat'

        CalcSeriesPIU(state, PIUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
        break;
    }
    case DataDefineEquip::ZnAirLoopEquipType::SingleDuct_ParallelPIU_Reheat: { // 'AirTerminal:SingleDuct:ParallelPIU:Reheat'

        CalcParallelPIU(state, PIUNum, ZoneNum, ZoneNodeNum, FirstHVACIteration);
        break;
    }
    default:
        ShowSevereError(state, format("Illegal PI Unit Type used={}", state.dataPowerInductionUnits->PIU(PIUNum).UnitType));
        ShowContinueError(state, format("Occurs in PI Unit={}", state.dataPowerInductionUnits->PIU(PIUNum).Name));
        ShowFatalError(state, "Preceding condition causes termination.");
        break;
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

    using FluidProperties::FindRefrigerant;
    using NodeInputManager::GetOnlySingleNode;
    using SteamCoils::GetCoilSteamInletNode;
    using WaterCoils::GetCoilWaterInletNode;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas = 0;                                          // Number of Alpha input fields for each GetObjectItem call
    int NumNumbers = 0;                                         // Number of Numeric input fields for each GetObjectItem call
    int IOStatus = 0;                                           // Used in GetObjectItem
    bool ErrorsFound(false);                                    // Set to true if errors in input, fatal at end of routine
    static constexpr std::string_view RoutineName("GetPIUs: "); // include trailing blank space
    bool SteamMessageNeeded = true;

    // find the number of each type of fan coil unit
    state.dataPowerInductionUnits->NumSeriesPIUs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:SeriesPIU:Reheat");
    state.dataPowerInductionUnits->NumParallelPIUs =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirTerminal:SingleDuct:ParallelPIU:Reheat");
    state.dataPowerInductionUnits->NumPIUs = state.dataPowerInductionUnits->NumSeriesPIUs + state.dataPowerInductionUnits->NumParallelPIUs;

    if (state.dataPowerInductionUnits->NumPIUs > 0) {
        // GetZonePlenumInput might call this routine before the AirDistUnit has been populated
        if (state.dataZoneAirLoopEquipmentManager->GetAirDistUnitsFlag) {
            ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment(state);
            state.dataZoneAirLoopEquipmentManager->GetAirDistUnitsFlag = false;
        }
    }

    // allocate the data structures
    state.dataPowerInductionUnits->PIU.allocate(state.dataPowerInductionUnits->NumPIUs);
    state.dataPowerInductionUnits->PiuUniqueNames.reserve(static_cast<unsigned>(state.dataPowerInductionUnits->NumPIUs));
    state.dataPowerInductionUnits->CheckEquipName.dimension(state.dataPowerInductionUnits->NumPIUs, true);
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    // loop over Series PIUs; get and load the input data
    for (int PIUIndex = 1; PIUIndex <= state.dataPowerInductionUnits->NumSeriesPIUs; ++PIUIndex) {

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

        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataPowerInductionUnits->PiuUniqueNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        auto &thisPIU = state.dataPowerInductionUnits->PIU(PIUIndex);
        thisPIU.Name = state.dataIPShortCut->cAlphaArgs(1);
        thisPIU.UnitType = cCurrentModuleObject;
        thisPIU.UnitType_Num = DataDefineEquip::ZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat;
        thisPIU.Sched = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            thisPIU.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            thisPIU.SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2)); // convert schedule name to pointer
            if (thisPIU.SchedPtr == 0) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaFieldNames(2),
                                       state.dataIPShortCut->cAlphaArgs(2),
                                       state.dataIPShortCut->cAlphaFieldNames(1),
                                       state.dataIPShortCut->cAlphaArgs(1)));
                ErrorsFound = true;
            }
        }

        thisPIU.MaxTotAirVolFlow = state.dataIPShortCut->rNumericArgs(1);
        thisPIU.MaxPriAirVolFlow = state.dataIPShortCut->rNumericArgs(2);
        thisPIU.MinPriAirFlowFrac = state.dataIPShortCut->rNumericArgs(3);

        thisPIU.HCoilType =
            static_cast<HtgCoilType>(getEnumerationValue(HCoilNamesUC, UtilityRoutines::MakeUPPERCase(state.dataIPShortCut->cAlphaArgs(9))));
        switch (thisPIU.HCoilType) {
        case HtgCoilType::SimpleHeating: {
            thisPIU.HCoil_PlantType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
            break;
        }
        case HtgCoilType::Electric:
        case HtgCoilType::Gas: {
            break;
        }
        case HtgCoilType::SteamAirHeating: {
            thisPIU.HCoil_PlantType = DataPlant::PlantEquipmentType::CoilSteamAirHeating;
            thisPIU.HCoil_FluidIndex = FindRefrigerant(state, "Steam");
            if (thisPIU.HCoil_FluidIndex == 0) {
                ShowSevereError(state, format("{}Steam Properties for {} not found.", RoutineName, state.dataIPShortCut->cAlphaArgs(1)));
                if (SteamMessageNeeded) {
                    ShowContinueError(state, "Steam Fluid Properties should have been included in the input file.");
                }
                ErrorsFound = true;
                SteamMessageNeeded = false;
            }
            break;
        }
        default: {
            ShowSevereError(state, format("Illegal {} = {}", state.dataIPShortCut->cAlphaFieldNames(9), state.dataIPShortCut->cAlphaArgs(9)));
            ShowContinueError(state, format("Occurs in {} = {}", cCurrentModuleObject, thisPIU.Name));
            ErrorsFound = true;
        }
        }

        thisPIU.PriAirInNode = GetOnlySingleNode(state,
                                                 state.dataIPShortCut->cAlphaArgs(3),
                                                 ErrorsFound,
                                                 DataLoopNode::ConnectionObjectType::AirTerminalSingleDuctSeriesPIUReheat,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 DataLoopNode::NodeFluidType::Air,
                                                 DataLoopNode::ConnectionType::Inlet,
                                                 NodeInputManager::CompFluidStream::Primary,
                                                 ObjectIsParent,
                                                 state.dataIPShortCut->cAlphaFieldNames(3));

        thisPIU.SecAirInNode = GetOnlySingleNode(state,
                                                 state.dataIPShortCut->cAlphaArgs(4),
                                                 ErrorsFound,
                                                 DataLoopNode::ConnectionObjectType::AirTerminalSingleDuctSeriesPIUReheat,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 DataLoopNode::NodeFluidType::Air,
                                                 DataLoopNode::ConnectionType::Inlet,
                                                 NodeInputManager::CompFluidStream::Primary,
                                                 ObjectIsParent,
                                                 state.dataIPShortCut->cAlphaFieldNames(4));

        thisPIU.OutAirNode = GetOnlySingleNode(state,
                                               state.dataIPShortCut->cAlphaArgs(5),
                                               ErrorsFound,
                                               DataLoopNode::ConnectionObjectType::AirTerminalSingleDuctSeriesPIUReheat,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               DataLoopNode::NodeFluidType::Air,
                                               DataLoopNode::ConnectionType::Outlet,
                                               NodeInputManager::CompFluidStream::Primary,
                                               ObjectIsParent,
                                               state.dataIPShortCut->cAlphaFieldNames(5));

        thisPIU.HCoilInAirNode = GetOnlySingleNode(state,
                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                   ErrorsFound,
                                                   DataLoopNode::ConnectionObjectType::AirTerminalSingleDuctSeriesPIUReheat,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   DataLoopNode::NodeFluidType::Air,
                                                   DataLoopNode::ConnectionType::Internal,
                                                   NodeInputManager::CompFluidStream::Primary,
                                                   ObjectIsParent,
                                                   state.dataIPShortCut->cAlphaFieldNames(6));
        // The reheat coil control node is necessary for hot water reheat, but not necessary for
        // electric or gas reheat.
        if (thisPIU.HCoilType == HtgCoilType::SimpleHeating) {
            thisPIU.HotControlNode =
                GetCoilWaterInletNode(state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), ErrorsFound);
        }
        if (thisPIU.HCoilType == HtgCoilType::SteamAirHeating) {
            thisPIU.HotControlNode =
                GetCoilSteamInletNode(state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), ErrorsFound);
        }
        thisPIU.MixerName = state.dataIPShortCut->cAlphaArgs(7); // name of zone mixer object
        thisPIU.FanName = state.dataIPShortCut->cAlphaArgs(8);   // name of fan object

        // find fan type
        // test if Fan:SystemModel fan of this name exists
        if (HVACFan::checkIfFanNameIsAFanSystem(state, thisPIU.FanName)) {
            thisPIU.Fan_Num = DataHVACGlobals::FanType_SystemModelObject;
            state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, thisPIU.FanName)); // call constructor
            thisPIU.Fan_Index = HVACFan::getFanObjectVectorIndex(state, thisPIU.FanName);
            thisPIU.FanAvailSchedPtr = state.dataHVACFan->fanObjs[thisPIU.Fan_Index]->availSchedIndex;
        } else {
            bool isNotOkay(false);
            ValidateComponent(state, "FAN:CONSTANTVOLUME", thisPIU.FanName, isNotOkay, "GetPIUs");
            if (isNotOkay) {
                ShowContinueError(state, format("In {} = {}", thisPIU.UnitType, thisPIU.Name));
                ErrorsFound = true;
            }
            thisPIU.Fan_Num = DataHVACGlobals::FanType_SimpleConstVolume;
            int FanType_Num = 0;
            Fans::GetFanType(state, thisPIU.FanName, FanType_Num, ErrorsFound);
            thisPIU.FanAvailSchedPtr = Fans::GetFanAvailSchPtr(state, DataHVACGlobals::cFanTypes(FanType_Num), thisPIU.FanName, ErrorsFound);
        }

        thisPIU.HCoil = state.dataIPShortCut->cAlphaArgs(10); // name of heating coil object
        bool IsNotOK = false;
        ValidateComponent(state, HCoilNamesUC[static_cast<int>(thisPIU.HCoilType)], thisPIU.HCoil, IsNotOK, cCurrentModuleObject + " - Heating Coil");
        if (IsNotOK) {
            ShowContinueError(state, format("In {} = {}", cCurrentModuleObject, thisPIU.Name));
            ErrorsFound = true;
        }
        thisPIU.MaxVolHotWaterFlow = state.dataIPShortCut->rNumericArgs(4);
        thisPIU.MinVolHotWaterFlow = state.dataIPShortCut->rNumericArgs(5);
        thisPIU.HotControlOffset = state.dataIPShortCut->rNumericArgs(6);
        // Set default convergence tolerance
        if (thisPIU.HotControlOffset <= 0.0) {
            thisPIU.HotControlOffset = 0.001;
        }

        // Add fan to component sets array
        SetUpCompSets(state,
                      thisPIU.UnitType,
                      thisPIU.Name,
                      "UNDEFINED",
                      state.dataIPShortCut->cAlphaArgs(8),
                      "UNDEFINED",
                      state.dataIPShortCut->cAlphaArgs(6));

        // Add reheat coil to component sets array
        SetUpCompSets(state,
                      thisPIU.UnitType,
                      thisPIU.Name,
                      state.dataIPShortCut->cAlphaArgs(9),
                      state.dataIPShortCut->cAlphaArgs(10),
                      state.dataIPShortCut->cAlphaArgs(6),
                      state.dataIPShortCut->cAlphaArgs(5));

        // Register component set data
        TestCompSet(state,
                    thisPIU.UnitType,
                    thisPIU.Name,
                    state.dataLoopNodes->NodeID(thisPIU.PriAirInNode),
                    state.dataLoopNodes->NodeID(thisPIU.OutAirNode),
                    "Air Nodes");

        for (int ADUNum = 1; ADUNum <= (int)state.dataDefineEquipment->AirDistUnit.size(); ++ADUNum) {
            if (thisPIU.OutAirNode == state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                thisPIU.ADUNum = ADUNum;
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum = thisPIU.PriAirInNode;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (thisPIU.ADUNum == 0) {
            ShowSevereError(state, format("{}No matching Air Distribution Unit, for PIU = [{},{}].", RoutineName, thisPIU.UnitType, thisPIU.Name));
            ShowContinueError(state, format("...should have outlet node = {}", state.dataLoopNodes->NodeID(thisPIU.OutAirNode)));
            ErrorsFound = true;
        } else {

            bool AirNodeFound = false;
            // Fill the Zone Equipment data with the supply air inlet node number of this unit.
            for (int CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) {
                    continue;
                }
                for (int SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (thisPIU.OutAirNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = thisPIU.PriAirInNode;
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = thisPIU.OutAirNode;
                        state.dataDefineEquipment->AirDistUnit(thisPIU.ADUNum).TermUnitSizingNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                        state.dataDefineEquipment->AirDistUnit(thisPIU.ADUNum).ZoneEqNum = CtrlZone;
                        AirNodeFound = true;
                        thisPIU.CtrlZoneNum = CtrlZone; // fill index for later use in finding air loop index
                        thisPIU.ctrlZoneInNodeIndex = SupAirIn;
                        break;
                    }
                }
            }
            if (!AirNodeFound) {
                ShowSevereError(state, format("The outlet air node from the {} Unit = {}", cCurrentModuleObject, thisPIU.Name));
                ShowContinueError(state, format("did not have a matching Zone Equipment Inlet Node, Node = {}", state.dataIPShortCut->cAlphaArgs(5)));
                ErrorsFound = true;
            }
        }
    }

    for (int PIUIndex = 1; PIUIndex <= state.dataPowerInductionUnits->NumParallelPIUs; ++PIUIndex) {

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

        auto &thisPIU = state.dataPowerInductionUnits->PIU(PIUIndex + state.dataPowerInductionUnits->NumSeriesPIUs);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 state.dataPowerInductionUnits->PiuUniqueNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        thisPIU.Name = state.dataIPShortCut->cAlphaArgs(1);
        thisPIU.UnitType = cCurrentModuleObject;
        thisPIU.UnitType_Num = DataDefineEquip::ZnAirLoopEquipType::SingleDuct_ParallelPIU_Reheat;
        thisPIU.Sched = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            thisPIU.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            thisPIU.SchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2)); // convert schedule name to pointer
            if (thisPIU.SchedPtr == 0) {
                ShowSevereError(state,
                                format("{}{}: invalid {} entered ={} for {}={}",
                                       RoutineName,
                                       cCurrentModuleObject,
                                       state.dataIPShortCut->cAlphaFieldNames(2),
                                       state.dataIPShortCut->cAlphaArgs(2),
                                       state.dataIPShortCut->cAlphaFieldNames(1),
                                       state.dataIPShortCut->cAlphaArgs(1)));
                ErrorsFound = true;
            }
        }
        thisPIU.MaxPriAirVolFlow = state.dataIPShortCut->rNumericArgs(1);
        thisPIU.MaxSecAirVolFlow = state.dataIPShortCut->rNumericArgs(2);
        thisPIU.MinPriAirFlowFrac = state.dataIPShortCut->rNumericArgs(3);
        thisPIU.FanOnFlowFrac = state.dataIPShortCut->rNumericArgs(4);
        if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:WATER")) {
            thisPIU.HCoilType = HtgCoilType::SimpleHeating;
            thisPIU.HCoil_PlantType = DataPlant::PlantEquipmentType::CoilWaterSimpleHeating;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:FUEL")) {
            thisPIU.HCoilType = HtgCoilType::Gas;
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:STEAM")) {
            thisPIU.HCoilType = HtgCoilType::SteamAirHeating;
            thisPIU.HCoil_PlantType = DataPlant::PlantEquipmentType::CoilSteamAirHeating;
            thisPIU.HCoil_FluidIndex = FindRefrigerant(state, "Steam");
            if (thisPIU.HCoil_FluidIndex == 0) {
                ShowSevereError(state, format("{}Steam Properties for {} not found.", RoutineName, state.dataIPShortCut->cAlphaArgs(1)));
                if (SteamMessageNeeded) {
                    ShowContinueError(state, "Steam Fluid Properties should have been included in the input file.");
                }
                ErrorsFound = true;
                SteamMessageNeeded = false;
            }
        } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(9), "COIL:HEATING:ELECTRIC")) {
            thisPIU.HCoilType = HtgCoilType::Electric;
        } else {
            ShowSevereError(state, format("Illegal {} = {}", state.dataIPShortCut->cAlphaFieldNames(9), state.dataIPShortCut->cAlphaArgs(9)));
            ShowContinueError(state, format("Occurs in {} = {}", cCurrentModuleObject, thisPIU.Name));
            ErrorsFound = true;
        }

        thisPIU.PriAirInNode = GetOnlySingleNode(state,
                                                 state.dataIPShortCut->cAlphaArgs(3),
                                                 ErrorsFound,
                                                 DataLoopNode::ConnectionObjectType::AirTerminalSingleDuctParallelPIUReheat,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 DataLoopNode::NodeFluidType::Air,
                                                 DataLoopNode::ConnectionType::Inlet,
                                                 NodeInputManager::CompFluidStream::Primary,
                                                 ObjectIsParent,
                                                 state.dataIPShortCut->cAlphaFieldNames(3));

        thisPIU.SecAirInNode = GetOnlySingleNode(state,
                                                 state.dataIPShortCut->cAlphaArgs(4),
                                                 ErrorsFound,
                                                 DataLoopNode::ConnectionObjectType::AirTerminalSingleDuctParallelPIUReheat,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 DataLoopNode::NodeFluidType::Air,
                                                 DataLoopNode::ConnectionType::Inlet,
                                                 NodeInputManager::CompFluidStream::Primary,
                                                 ObjectIsParent,
                                                 state.dataIPShortCut->cAlphaFieldNames(4));

        thisPIU.OutAirNode = GetOnlySingleNode(state,
                                               state.dataIPShortCut->cAlphaArgs(5),
                                               ErrorsFound,
                                               DataLoopNode::ConnectionObjectType::AirTerminalSingleDuctParallelPIUReheat,
                                               state.dataIPShortCut->cAlphaArgs(1),
                                               DataLoopNode::NodeFluidType::Air,
                                               DataLoopNode::ConnectionType::Outlet,
                                               NodeInputManager::CompFluidStream::Primary,
                                               ObjectIsParent,
                                               state.dataIPShortCut->cAlphaFieldNames(5));

        thisPIU.HCoilInAirNode = GetOnlySingleNode(state,
                                                   state.dataIPShortCut->cAlphaArgs(6),
                                                   ErrorsFound,
                                                   DataLoopNode::ConnectionObjectType::AirTerminalSingleDuctParallelPIUReheat,
                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                   DataLoopNode::NodeFluidType::Air,
                                                   DataLoopNode::ConnectionType::Internal,
                                                   NodeInputManager::CompFluidStream::Primary,
                                                   ObjectIsParent,
                                                   state.dataIPShortCut->cAlphaFieldNames(6));
        if (thisPIU.HCoilType == HtgCoilType::SimpleHeating) {
            thisPIU.HotControlNode =
                GetCoilWaterInletNode(state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), ErrorsFound);
        }
        if (thisPIU.HCoilType == HtgCoilType::SteamAirHeating) {
            thisPIU.HotControlNode =
                GetCoilSteamInletNode(state, state.dataIPShortCut->cAlphaArgs(9), state.dataIPShortCut->cAlphaArgs(10), ErrorsFound);
        }
        thisPIU.MixerName = state.dataIPShortCut->cAlphaArgs(7); // name of zone mixer object
        thisPIU.FanName = state.dataIPShortCut->cAlphaArgs(8);   // name of fan object
        // find fan type
        // test if Fan:SystemModel fan of this name exists
        if (HVACFan::checkIfFanNameIsAFanSystem(state, thisPIU.FanName)) {
            thisPIU.Fan_Num = DataHVACGlobals::FanType_SystemModelObject;
            state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, thisPIU.FanName)); // call constructor
            thisPIU.Fan_Index = HVACFan::getFanObjectVectorIndex(state, thisPIU.FanName);
            thisPIU.FanAvailSchedPtr = state.dataHVACFan->fanObjs[thisPIU.Fan_Index]->availSchedIndex;
        } else {
            bool isNotOkay(false);
            ValidateComponent(state, "FAN:CONSTANTVOLUME", thisPIU.FanName, isNotOkay, "GetPIUs");
            if (isNotOkay) {
                ShowContinueError(state, format("In {} = {}", thisPIU.UnitType, thisPIU.Name));
                ErrorsFound = true;
            }
            thisPIU.Fan_Num = DataHVACGlobals::FanType_SimpleConstVolume;
            int FanType_Num = 0;
            Fans::GetFanType(state, thisPIU.FanName, FanType_Num, ErrorsFound);
            thisPIU.FanAvailSchedPtr = Fans::GetFanAvailSchPtr(state, DataHVACGlobals::cFanTypes(FanType_Num), thisPIU.FanName, ErrorsFound);
        }
        thisPIU.HCoil = state.dataIPShortCut->cAlphaArgs(10); // name of heating coil object
        bool IsNotOK = false;
        ValidateComponent(state, HCoilNamesUC[static_cast<int>(thisPIU.HCoilType)], thisPIU.HCoil, IsNotOK, cCurrentModuleObject + " - Heating Coil");
        if (IsNotOK) {
            ShowContinueError(state, format("In {} = {}", cCurrentModuleObject, thisPIU.Name));
            ErrorsFound = true;
        }
        thisPIU.MaxVolHotWaterFlow = state.dataIPShortCut->rNumericArgs(5);
        thisPIU.MinVolHotWaterFlow = state.dataIPShortCut->rNumericArgs(6);
        thisPIU.HotControlOffset = state.dataIPShortCut->rNumericArgs(7);
        // Set default convergence tolerance
        if (thisPIU.HotControlOffset <= 0.0) {
            thisPIU.HotControlOffset = 0.001;
        }

        // Add fan to component sets array
        SetUpCompSets(state,
                      thisPIU.UnitType,
                      thisPIU.Name,
                      "UNDEFINED",
                      state.dataIPShortCut->cAlphaArgs(8),
                      state.dataIPShortCut->cAlphaArgs(4),
                      "UNDEFINED");

        // Add reheat coil to component sets array
        SetUpCompSets(state,
                      thisPIU.UnitType,
                      thisPIU.Name,
                      state.dataIPShortCut->cAlphaArgs(9),
                      state.dataIPShortCut->cAlphaArgs(10),
                      state.dataIPShortCut->cAlphaArgs(6),
                      state.dataIPShortCut->cAlphaArgs(5));

        // Register component set data
        TestCompSet(state,
                    thisPIU.UnitType,
                    thisPIU.Name,
                    state.dataLoopNodes->NodeID(thisPIU.PriAirInNode),
                    state.dataLoopNodes->NodeID(thisPIU.OutAirNode),
                    "Air Nodes");

        for (int ADUNum = 1; ADUNum <= (int)state.dataDefineEquipment->AirDistUnit.size(); ++ADUNum) {
            if (thisPIU.OutAirNode == state.dataDefineEquipment->AirDistUnit(ADUNum).OutletNodeNum) {
                //      AirDistUnit(ADUNum)%InletNodeNum = PIU(PIUNum)%InletNodeNum
                state.dataDefineEquipment->AirDistUnit(ADUNum).InletNodeNum = thisPIU.PriAirInNode;
                thisPIU.ADUNum = ADUNum;
            }
        }
        // one assumes if there isn't one assigned, it's an error?
        if (thisPIU.ADUNum == 0) {
            ShowSevereError(state, format("{}No matching Air Distribution Unit, for PIU = [{},{}].", RoutineName, thisPIU.UnitType, thisPIU.Name));
            ShowContinueError(state, format("...should have outlet node = {}", state.dataLoopNodes->NodeID(thisPIU.OutAirNode)));
            ErrorsFound = true;
        } else {

            // Fill the Zone Equipment data with the supply air inlet node number of this unit.
            bool AirNodeFound = false;
            for (int CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) {
                    continue;
                }
                for (int SupAirIn = 1; SupAirIn <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++SupAirIn) {
                    if (thisPIU.OutAirNode == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(SupAirIn)) {
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).InNode = thisPIU.PriAirInNode;
                        state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).OutNode = thisPIU.OutAirNode;
                        state.dataDefineEquipment->AirDistUnit(thisPIU.ADUNum).TermUnitSizingNum =
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZone).AirDistUnitCool(SupAirIn).TermUnitSizingIndex;
                        state.dataDefineEquipment->AirDistUnit(thisPIU.ADUNum).ZoneEqNum = CtrlZone;
                        thisPIU.CtrlZoneNum = CtrlZone;
                        thisPIU.ctrlZoneInNodeIndex = SupAirIn;
                        AirNodeFound = true;
                    }
                }
            }
            if (!AirNodeFound) {
                ShowSevereError(state, format("The outlet air node from the {} Unit = {}", cCurrentModuleObject, thisPIU.Name));
                ShowContinueError(state, format("did not have a matching Zone Equipment Inlet Node, Node = {}", state.dataIPShortCut->cAlphaArgs(5)));
                ErrorsFound = true;
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, format("{}Errors found in getting input.  Preceding conditions cause termination.", RoutineName));
    }

    for (int PIUNum = 1; PIUNum <= state.dataPowerInductionUnits->NumPIUs; ++PIUNum) {
        auto &thisPIU = state.dataPowerInductionUnits->PIU(PIUNum);

        // Setup Report variables for the PIUs
        SetupOutputVariable(state,
                            "Zone Air Terminal Primary Damper Position",
                            OutputProcessor::Unit::None,
                            thisPIU.PriDamperPosition,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisPIU.Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Heating Rate",
                            OutputProcessor::Unit::W,
                            thisPIU.HeatingRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisPIU.Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Heating Energy",
                            OutputProcessor::Unit::J,
                            thisPIU.HeatingEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            thisPIU.Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            thisPIU.SensCoolRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisPIU.Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            thisPIU.SensCoolEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            thisPIU.Name);
        SetupOutputVariable(state,
                            "Zone Air Terminal Outdoor Air Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            thisPIU.OutdoorAirFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisPIU.Name);
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

    using DataZoneEquipment::CheckZoneEquipmentList;
    using PlantUtilities::InitComponentNodes;
    using PlantUtilities::ScanPlantLoopsForObject;

    auto &thisPIU = state.dataPowerInductionUnits->PIU(PIUNum);
    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("InitPIU");

    // Do the one time initializations
    if (state.dataPowerInductionUnits->MyOneTimeFlag) {
        state.dataPowerInductionUnits->MyEnvrnFlag.dimension(state.dataPowerInductionUnits->NumPIUs, true);
        state.dataPowerInductionUnits->MySizeFlag.dimension(state.dataPowerInductionUnits->NumPIUs, true);
        state.dataPowerInductionUnits->MyPlantScanFlag.dimension(state.dataPowerInductionUnits->NumPIUs, true);
        state.dataPowerInductionUnits->MyOneTimeFlag = false;
    }

    if (state.dataPowerInductionUnits->MyPlantScanFlag(PIUNum) && allocated(state.dataPlnt->PlantLoop)) {
        if ((thisPIU.HCoil_PlantType == DataPlant::PlantEquipmentType::CoilWaterSimpleHeating) ||
            (thisPIU.HCoil_PlantType == DataPlant::PlantEquipmentType::CoilSteamAirHeating)) {
            bool errFlag = false;
            ScanPlantLoopsForObject(state, thisPIU.HCoil, thisPIU.HCoil_PlantType, thisPIU.HWplantLoc, errFlag, _, _, _, _, _);
            if (errFlag) {
                ShowFatalError(state, "InitPIU: Program terminated due to previous condition(s).");
            }
            thisPIU.HotCoilOutNodeNum = DataPlant::CompData::getPlantComponent(state, thisPIU.HWplantLoc).NodeNumOut;
        }
        state.dataPowerInductionUnits->MyPlantScanFlag(PIUNum) = false;
    } else if (state.dataPowerInductionUnits->MyPlantScanFlag(PIUNum) && !state.dataGlobal->AnyPlantInModel) {
        state.dataPowerInductionUnits->MyPlantScanFlag(PIUNum) = false;
    }

    if (!state.dataPowerInductionUnits->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataPowerInductionUnits->ZoneEquipmentListChecked = true;
        // Check to see if there is a Air Distribution Unit on the Zone Equipment List
        for (int Loop = 1; Loop <= state.dataPowerInductionUnits->NumPIUs; ++Loop) {
            if (state.dataPowerInductionUnits->PIU(Loop).ADUNum == 0) {
                continue;
            }
            if (CheckZoneEquipmentList(state,
                                       "ZoneHVAC:AirDistributionUnit",
                                       state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(Loop).ADUNum).Name)) {
                continue;
            }
            ShowSevereError(state,
                            format("InitPIU: ADU=[Air Distribution Unit,{}] is not on any ZoneHVAC:EquipmentList.",
                                   state.dataDefineEquipment->AirDistUnit(state.dataPowerInductionUnits->PIU(Loop).ADUNum).Name));
            ShowContinueError(state,
                              format("...PIU=[{},{}] will not be simulated.",
                                     state.dataPowerInductionUnits->PIU(Loop).UnitType,
                                     state.dataPowerInductionUnits->PIU(Loop).Name));
        }
    }

    if (!state.dataGlobal->SysSizingCalc && state.dataPowerInductionUnits->MySizeFlag(PIUNum) &&
        !state.dataPowerInductionUnits->MyPlantScanFlag(PIUNum)) {

        SizePIU(state, PIUNum);

        // If there's a hot water control node number defined in PIU
        if (thisPIU.HotControlNode > 0) {
            // plant upgrade note? why no separate handling of steam coil? add it ?
            // local plant fluid density
            Real64 const rho = GetDensityGlycol(state,
                                                state.dataPlnt->PlantLoop(thisPIU.HWplantLoc.loopNum).FluidName,
                                                DataGlobalConstants::HWInitConvTemp,
                                                state.dataPlnt->PlantLoop(thisPIU.HWplantLoc.loopNum).FluidIndex,
                                                RoutineName);

            thisPIU.MaxHotWaterFlow = rho * thisPIU.MaxVolHotWaterFlow;
            thisPIU.MinHotWaterFlow = rho * thisPIU.MinVolHotWaterFlow;
            InitComponentNodes(state, thisPIU.MinHotWaterFlow, thisPIU.MaxHotWaterFlow, thisPIU.HotControlNode, thisPIU.HotCoilOutNodeNum);
        }

        state.dataPowerInductionUnits->MySizeFlag(PIUNum) = false;
    }

    int const PriNode = thisPIU.PriAirInNode;
    int const SecNode = thisPIU.SecAirInNode;

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && state.dataPowerInductionUnits->MyEnvrnFlag(PIUNum)) {
        Real64 const RhoAir = state.dataEnvrn->StdRhoAir;
        int const OutletNode = thisPIU.OutAirNode;
        // set the mass flow rates from the input volume flow rates
        if (thisPIU.UnitType == "AirTerminal:SingleDuct:SeriesPIU:Reheat") {
            // series
            thisPIU.MaxTotAirMassFlow = RhoAir * thisPIU.MaxTotAirVolFlow;
            thisPIU.MaxPriAirMassFlow = RhoAir * thisPIU.MaxPriAirVolFlow;
            thisPIU.MinPriAirMassFlow = RhoAir * thisPIU.MinPriAirFlowFrac * thisPIU.MaxPriAirVolFlow;
            state.dataLoopNodes->Node(PriNode).MassFlowRateMax = thisPIU.MaxPriAirMassFlow;
            state.dataLoopNodes->Node(PriNode).MassFlowRateMin = thisPIU.MinPriAirMassFlow;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = thisPIU.MaxTotAirMassFlow;
        } else {
            // parallel
            thisPIU.MaxPriAirMassFlow = RhoAir * thisPIU.MaxPriAirVolFlow;
            thisPIU.MinPriAirMassFlow = RhoAir * thisPIU.MinPriAirFlowFrac * thisPIU.MaxPriAirVolFlow;
            thisPIU.MaxSecAirMassFlow = RhoAir * thisPIU.MaxSecAirVolFlow;
            thisPIU.FanOnAirMassFlow = RhoAir * thisPIU.FanOnFlowFrac * thisPIU.MaxPriAirVolFlow;
            state.dataLoopNodes->Node(PriNode).MassFlowRateMax = thisPIU.MaxPriAirMassFlow;
            state.dataLoopNodes->Node(PriNode).MassFlowRateMin = thisPIU.MinPriAirMassFlow;
            state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = thisPIU.MaxPriAirMassFlow;
        }

        if (((thisPIU.HCoilType == HtgCoilType::SimpleHeating) || (thisPIU.HCoilType == HtgCoilType::SteamAirHeating)) &&
            !state.dataPowerInductionUnits->MyPlantScanFlag(PIUNum)) {
            InitComponentNodes(state, thisPIU.MinHotWaterFlow, thisPIU.MaxHotWaterFlow, thisPIU.HotControlNode, thisPIU.HotCoilOutNodeNum);
        }

        if (thisPIU.AirLoopNum == 0) { // fill air loop index
            if (thisPIU.CtrlZoneNum > 0 && thisPIU.ctrlZoneInNodeIndex > 0) {
                thisPIU.AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(thisPIU.CtrlZoneNum).InletNodeAirLoopNum(thisPIU.ctrlZoneInNodeIndex);
                state.dataDefineEquipment->AirDistUnit(thisPIU.ADUNum).AirLoopNum = thisPIU.AirLoopNum;
            }
        }

        state.dataPowerInductionUnits->MyEnvrnFlag(PIUNum) = false;
    } // end one time inits

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataPowerInductionUnits->MyEnvrnFlag(PIUNum) = true;
    }

    // Do the start of HVAC time step initializations
    if (FirstHVACIteration) {
        // check for upstream zero flow. If nonzero and schedule ON, set primary flow to max
        if (GetCurrentScheduleValue(state, thisPIU.SchedPtr) > 0.0 && state.dataLoopNodes->Node(PriNode).MassFlowRate > 0.0) {
            if (thisPIU.UnitType == "AirTerminal:SingleDuct:SeriesPIU:Reheat") {
                state.dataLoopNodes->Node(PriNode).MassFlowRate = thisPIU.MaxPriAirMassFlow;
                state.dataLoopNodes->Node(SecNode).MassFlowRate = max(0.0, thisPIU.MaxTotAirMassFlow - thisPIU.MaxPriAirMassFlow);
            } else {
                state.dataLoopNodes->Node(PriNode).MassFlowRate = thisPIU.MaxPriAirMassFlow;
                state.dataLoopNodes->Node(SecNode).MassFlowRate = thisPIU.MaxSecAirMassFlow;
            }
        } else {
            state.dataLoopNodes->Node(PriNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(SecNode).MassFlowRate = 0.0;
        }
        // reset the max and min avail flows
        if (GetCurrentScheduleValue(state, thisPIU.SchedPtr) > 0.0 && state.dataLoopNodes->Node(PriNode).MassFlowRateMaxAvail > 0.0) {
            if (thisPIU.UnitType == "AirTerminal:SingleDuct:SeriesPIU:Reheat") {
                state.dataLoopNodes->Node(PriNode).MassFlowRateMaxAvail = thisPIU.MaxPriAirMassFlow;
                state.dataLoopNodes->Node(PriNode).MassFlowRateMinAvail = thisPIU.MinPriAirMassFlow;
                state.dataLoopNodes->Node(SecNode).MassFlowRateMaxAvail = max(0.0, thisPIU.MaxTotAirMassFlow - thisPIU.MinPriAirMassFlow);
                state.dataLoopNodes->Node(SecNode).MassFlowRateMinAvail = max(0.0, thisPIU.MaxTotAirMassFlow - thisPIU.MaxPriAirMassFlow);
            } else {
                state.dataLoopNodes->Node(PriNode).MassFlowRateMaxAvail = thisPIU.MaxPriAirMassFlow;
                state.dataLoopNodes->Node(PriNode).MassFlowRateMinAvail = thisPIU.MinPriAirMassFlow;
                state.dataLoopNodes->Node(SecNode).MassFlowRateMaxAvail = thisPIU.MaxSecAirMassFlow;
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
    bool IsAutoSize = false;           // Indicator to autosize
    bool IsMaxPriFlowAutoSize = false; // Indicate if the maximum terminal flow is autosize
    int SysSizNum = 0;                 // System sizing number
    Real64 DesCoilLoad = 0.0;
    bool ErrorsFound = false;

    auto &TermUnitSizing(state.dataSize->TermUnitSizing);
    auto &CurTermUnitSizingNum(state.dataSize->CurTermUnitSizingNum);
    auto &thisPIU = state.dataPowerInductionUnits->PIU(PIUNum);

    if (thisPIU.MaxPriAirVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (thisPIU.MaxPriAirVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, thisPIU.UnitType, thisPIU.Name, "User-Specified Maximum Primary Air Flow Rate [m3/s]", thisPIU.MaxPriAirVolFlow);
            }
        } else {
            CheckZoneSizing(state, thisPIU.UnitType, thisPIU.Name);
            // Autosized maximum primary air flow for reporting
            Real64 MaxPriAirVolFlowDes = max(state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow,
                                             state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow);
            if (MaxPriAirVolFlowDes < SmallAirVolFlow) {
                MaxPriAirVolFlowDes = 0.0;
            }

            if (IsAutoSize) {
                thisPIU.MaxPriAirVolFlow = MaxPriAirVolFlowDes;
                IsMaxPriFlowAutoSize = true;
                BaseSizer::reportSizerOutput(
                    state, thisPIU.UnitType, thisPIU.Name, "Design Size Maximum Primary Air Flow Rate [m3/s]", MaxPriAirVolFlowDes);
            } else {
                if (thisPIU.MaxPriAirVolFlow > 0.0 && MaxPriAirVolFlowDes > 0.0) {
                    // Hardsized maximum primary air flow for reporting
                    Real64 const MaxPriAirVolFlowUser = thisPIU.MaxPriAirVolFlow;
                    BaseSizer::reportSizerOutput(state,
                                                 thisPIU.UnitType,
                                                 thisPIU.Name,
                                                 "Design Size Maximum Primary Air Flow Rate [m3/s]",
                                                 MaxPriAirVolFlowDes,
                                                 "User-Specified Maximum Primary Air Flow Rate [m3/s]",
                                                 MaxPriAirVolFlowUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxPriAirVolFlowDes - MaxPriAirVolFlowUser) / MaxPriAirVolFlowUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, format("SizePIU: Potential issue with equipment sizing for {} {}", thisPIU.UnitType, thisPIU.Name));
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
    if (thisPIU.MaxTotAirVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (thisPIU.MaxTotAirVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, thisPIU.UnitType, thisPIU.Name, "User-Specified Maximum Air Flow Rate [m3/s]", thisPIU.MaxTotAirVolFlow);
            }
        } else {
            CheckZoneSizing(state, thisPIU.UnitType, thisPIU.Name);
            // Autosized maximum air flow for reporting
            Real64 MaxTotAirVolFlowDes = max(state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow,
                                             state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow);
            if (MaxTotAirVolFlowDes < SmallAirVolFlow) {
                MaxTotAirVolFlowDes = 0.0;
            }
            if (IsAutoSize) {
                thisPIU.MaxTotAirVolFlow = MaxTotAirVolFlowDes;
                BaseSizer::reportSizerOutput(state, thisPIU.UnitType, thisPIU.Name, "Design Size Maximum Air Flow Rate [m3/s]", MaxTotAirVolFlowDes);
            } else {
                if (thisPIU.MaxTotAirVolFlow > 0.0 && MaxTotAirVolFlowDes > 0.0) {
                    // Hardsized maximum air flow for reporting
                    Real64 const MaxTotAirVolFlowUser = thisPIU.MaxTotAirVolFlow;
                    BaseSizer::reportSizerOutput(state,
                                                 thisPIU.UnitType,
                                                 thisPIU.Name,
                                                 "Design Size Maximum Air Flow Rate [m3/s]",
                                                 MaxTotAirVolFlowDes,
                                                 "User-Specified Maximum Air Flow Rate [m3/s]",
                                                 MaxTotAirVolFlowUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxTotAirVolFlowDes - MaxTotAirVolFlowUser) / MaxTotAirVolFlowUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, format("SizePIU: Potential issue with equipment sizing for {} {}", thisPIU.UnitType, thisPIU.Name));
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

    // if a sizing run has been done, check if system sizing has been done for this system
    bool SizingDesRunThisAirSys = false;
    if (state.dataSize->SysSizingRunDone) {
        int const AirLoopNum = state.dataZoneEquip->ZoneEquipConfig(thisPIU.CtrlZoneNum).InletNodeAirLoopNum(thisPIU.ctrlZoneInNodeIndex);
        if (AirLoopNum > 0) {
            CheckThisAirSystemForSizing(state, AirLoopNum, SizingDesRunThisAirSys);
        }

        // get system sizing id if a sizing run has been done for this system
        if (SizingDesRunThisAirSys) {
            SysSizNum = UtilityRoutines::FindItemInList(
                state.dataSize->FinalSysSizing(AirLoopNum).AirPriLoopName, state.dataSize->SysSizInput, &SystemSizingInputData::AirPriLoopName);
            if (SysSizNum == 0) {
                SysSizNum = 1; // use first when none applicable
            }
        }
    }

    IsAutoSize = false;
    if (thisPIU.MaxSecAirVolFlow == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (thisPIU.MaxSecAirVolFlow > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, thisPIU.UnitType, thisPIU.Name, "User-Specified Maximum Secondary Air Flow Rate [m3/s]", thisPIU.MaxSecAirVolFlow);
            }
        } else {
            CheckZoneSizing(state, thisPIU.UnitType, thisPIU.Name);
            // Autosized maximum secondary air flow for reporting
            Real64 MaxSecAirVolFlowDes = max(state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesCoolVolFlow,
                                             state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatVolFlow);
            if (MaxSecAirVolFlowDes < SmallAirVolFlow) {
                MaxSecAirVolFlowDes = 0.0;
            }
            if (IsAutoSize) {
                thisPIU.MaxSecAirVolFlow = MaxSecAirVolFlowDes;
                BaseSizer::reportSizerOutput(
                    state, thisPIU.UnitType, thisPIU.Name, "Design Size Maximum Secondary Air Flow Rate [m3/s]", MaxSecAirVolFlowDes);
            } else {
                if (thisPIU.MaxSecAirVolFlow > 0.0 && MaxSecAirVolFlowDes > 0.0) {
                    // Harsized maximum secondary air flow for reporting
                    Real64 const MaxSecAirVolFlowUser = thisPIU.MaxSecAirVolFlow;
                    BaseSizer::reportSizerOutput(state,
                                                 thisPIU.UnitType,
                                                 thisPIU.Name,
                                                 "Design Size Maximum Secondary Air Flow Rate [m3/s]",
                                                 MaxSecAirVolFlowDes,
                                                 "User-Specified Maximum Secondary Air Flow Rate [m3/s]",
                                                 MaxSecAirVolFlowUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxSecAirVolFlowDes - MaxSecAirVolFlowUser) / MaxSecAirVolFlowUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, format("SizePIU: Potential issue with equipment sizing for {} {}", thisPIU.UnitType, thisPIU.Name));
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
    if (thisPIU.MinPriAirFlowFrac == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (thisPIU.MinPriAirFlowFrac > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, thisPIU.UnitType, thisPIU.Name, "User-Specified Minimum Primary Air Flow Fraction", thisPIU.MinPriAirFlowFrac);
            }
        } else {
            CheckZoneSizing(state, thisPIU.UnitType, thisPIU.Name);
            // Autosized minimum primary air flow fraction for reporting
            Real64 MinPriAirFlowFracDes = 0.0;
            if (thisPIU.MaxPriAirVolFlow >= SmallAirVolFlow &&
                state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).MinOA >= SmallAirVolFlow) {
                MinPriAirFlowFracDes = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).MinOA / thisPIU.MaxPriAirVolFlow;
            }
            if (SizingDesRunThisAirSys) {
                if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SysOAMethod::SP) { // 62.1 simplified procedure
                    if (thisPIU.MaxPriAirVolFlow > 0.0) {
                        MinPriAirFlowFracDes = 1.5 *
                                               max(state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).VozClgByZone,
                                                   state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).VozHtgByZone) /
                                               thisPIU.MaxPriAirVolFlow;

                        // adjust maximum flow rate
                        if (MinPriAirFlowFracDes > 1.0 && IsMaxPriFlowAutoSize) {
                            thisPIU.MaxPriAirVolFlow *= MinPriAirFlowFracDes;
                            MinPriAirFlowFracDes = 1.0;
                            ShowWarningError(state,
                                             format("SingleDuctSystem:SizeSys: Autosized maximum air flow rate for {} was increased to meet the zone "
                                                    "primary air flow determined according to the ASHRAE Standard 62.1 Simplified Procedure.",
                                                    thisPIU.Name));
                        } else if (MinPriAirFlowFracDes > 1.0) {
                            ShowWarningError(
                                state,
                                format("SingleDuctSystem:SizeSys: Maximum primary air flow rate for {} is potentially too low.", thisPIU.Name));
                            ShowContinueError(state,
                                              "The flow is lower than the minimum primary air flow rate calculated following the ASHRAE Standard "
                                              "62.1 Simplified Procedure:");
                            ShowContinueError(state, format(" User-specified maximum primary air flow rate: {:.3R} m3/s.", thisPIU.MaxPriAirVolFlow));
                            ShowContinueError(
                                state,
                                format(" Calculated minimum primary air flow rate: {:.3R} m3/s.", thisPIU.MaxPriAirVolFlow * MinPriAirFlowFracDes));
                            MinPriAirFlowFracDes = 1.0;
                        }
                    }
                }
            }
            if (IsAutoSize) {
                if (SizingDesRunThisAirSys) {
                    if (state.dataSize->SysSizInput(SysSizNum).SystemOAMethod == SysOAMethod::SP) {
                        state.dataSize->TermUnitFinalZoneSizing(state.dataSize->CurTermUnitSizingNum).VpzMinByZoneSPSized = true;
                    }
                }
                thisPIU.MinPriAirFlowFrac = MinPriAirFlowFracDes;
                BaseSizer::reportSizerOutput(
                    state, thisPIU.UnitType, thisPIU.Name, "Design Size Minimum Primary Air Flow Fraction", MinPriAirFlowFracDes);
            } else {
                if (thisPIU.MinPriAirFlowFrac > 0.0 && MinPriAirFlowFracDes > 0.0) {
                    // Hardsized minimum primary air flow fraction for reporting
                    Real64 const MinPriAirFlowFracUser = thisPIU.MinPriAirFlowFrac;
                    BaseSizer::reportSizerOutput(state,
                                                 thisPIU.UnitType,
                                                 thisPIU.Name,
                                                 "Design Size Minimum Primary Air Flow Fraction",
                                                 MinPriAirFlowFracDes,
                                                 "User-Specified Minimum Primary Air Flow Fraction",
                                                 MinPriAirFlowFracUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MinPriAirFlowFracDes - MinPriAirFlowFracUser) / MinPriAirFlowFracUser) >
                            state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, format("SizePIU: Potential issue with equipment sizing for {} {}", thisPIU.UnitType, thisPIU.Name));
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

        if (thisPIU.UnitType_Num == DataDefineEquip::ZnAirLoopEquipType::SingleDuct_SeriesPIU_Reheat) {
            TermUnitSizing(CurTermUnitSizingNum).AirVolFlow = thisPIU.MaxTotAirVolFlow;
        } else if (thisPIU.UnitType_Num == DataDefineEquip::ZnAirLoopEquipType::SingleDuct_ParallelPIU_Reheat) {
            TermUnitSizing(CurTermUnitSizingNum).AirVolFlow = thisPIU.MaxSecAirVolFlow + thisPIU.MinPriAirFlowFrac * thisPIU.MaxPriAirVolFlow;
        }
    }

    IsAutoSize = false;
    if (thisPIU.FanOnFlowFrac == AutoSize) {
        IsAutoSize = true;
    }
    if (state.dataSize->CurZoneEqNum > 0) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (thisPIU.FanOnFlowFrac > 0.0) {
                BaseSizer::reportSizerOutput(state, thisPIU.UnitType, thisPIU.Name, "User-Specified Fan On Flow Fraction", thisPIU.FanOnFlowFrac);
            }
        } else {
            CheckZoneSizing(state, thisPIU.UnitType, thisPIU.Name);
            // Autosized fan on flow fraction for reporting
            Real64 FanOnFlowFracDes = thisPIU.MinPriAirFlowFrac;
            if (IsAutoSize) {
                thisPIU.FanOnFlowFrac = FanOnFlowFracDes;
                BaseSizer::reportSizerOutput(state, thisPIU.UnitType, thisPIU.Name, "Design Size Fan On Flow Fraction", FanOnFlowFracDes);
            } else {
                if (thisPIU.FanOnFlowFrac > 0.0 && FanOnFlowFracDes > 0.0) {
                    // Hardsized fan on flow fraction for reporting
                    Real64 const FanOnFlowFracUser = thisPIU.FanOnFlowFrac;
                    BaseSizer::reportSizerOutput(state,
                                                 thisPIU.UnitType,
                                                 thisPIU.Name,
                                                 "Design Size Fan On Flow Fraction",
                                                 FanOnFlowFracDes,
                                                 "User-Specified Fan On Flow Fraction",
                                                 FanOnFlowFracUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(FanOnFlowFracDes - FanOnFlowFracUser) / FanOnFlowFracUser) > state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state, format("SizePIU: Potential issue with equipment sizing for {} {}", thisPIU.UnitType, thisPIU.Name));
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
    if (thisPIU.MaxVolHotWaterFlow == AutoSize) { //.or.()) THEN
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (thisPIU.MaxVolHotWaterFlow > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, thisPIU.UnitType, thisPIU.Name, "User-Specified Maximum Reheat Water Flow Rate [m3/s]", thisPIU.MaxVolHotWaterFlow);
            }
        } else {
            CheckZoneSizing(state, thisPIU.UnitType, thisPIU.Name);
            if (UtilityRoutines::SameString(HCoilNamesUC[static_cast<int>(thisPIU.HCoilType)], "Coil:Heating:Water")) {

                int const CoilWaterInletNode = GetCoilWaterInletNode(state, "Coil:Heating:Water", thisPIU.HCoil, ErrorsFound);
                int const CoilWaterOutletNode = GetCoilWaterOutletNode(state, "Coil:Heating:Water", thisPIU.HCoil, ErrorsFound);

                // Autosized maximum hot water flow for reporting
                Real64 MaxVolHotWaterFlowDes = 0.0;

                if (IsAutoSize) {
                    int const PltSizHeatNum =
                        MyPlantSizingIndex(state, "Coil:Heating:Water", thisPIU.HCoil, CoilWaterInletNode, CoilWaterOutletNode, ErrorsFound);
                    if (PltSizHeatNum > 0) {

                        if (state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatMassFlow >= SmallAirVolFlow) {
                            Real64 const CoilInTemp =
                                state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU * thisPIU.MinPriAirFlowFrac +
                                state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak * (1.0 - thisPIU.MinPriAirFlowFrac);
                            Real64 const CoilOutTemp = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesTemp;
                            Real64 const CoilOutHumRat = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesHumRat;
                            Real64 const DesMassFlow = state.dataEnvrn->StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                            DesCoilLoad = PsyCpAirFnW(CoilOutHumRat) * DesMassFlow * (CoilOutTemp - CoilInTemp);

                            Real64 const rho = GetDensityGlycol(state,
                                                                state.dataPlnt->PlantLoop(thisPIU.HWplantLoc.loopNum).FluidName,
                                                                DataGlobalConstants::HWInitConvTemp,
                                                                state.dataPlnt->PlantLoop(thisPIU.HWplantLoc.loopNum).FluidIndex,
                                                                RoutineName);
                            Real64 const Cp = GetSpecificHeatGlycol(state,
                                                                    state.dataPlnt->PlantLoop(thisPIU.HWplantLoc.loopNum).FluidName,
                                                                    DataGlobalConstants::HWInitConvTemp,
                                                                    state.dataPlnt->PlantLoop(thisPIU.HWplantLoc.loopNum).FluidIndex,
                                                                    RoutineName);

                            MaxVolHotWaterFlowDes = DesCoilLoad / (state.dataSize->PlantSizData(PltSizHeatNum).DeltaT * Cp * rho);
                        } else {
                            MaxVolHotWaterFlowDes = 0.0;
                        }
                    } else {
                        ShowSevereError(state, "Autosizing of water flow requires a heating loop Sizing:Plant object");
                        ShowContinueError(state, format("Occurs in{} Object={}", thisPIU.UnitType, thisPIU.Name));
                        ErrorsFound = true;
                    }
                }
                if (IsAutoSize) {
                    thisPIU.MaxVolHotWaterFlow = MaxVolHotWaterFlowDes;
                    BaseSizer::reportSizerOutput(
                        state, thisPIU.UnitType, thisPIU.Name, "Design Size Maximum Reheat Water Flow Rate [m3/s]", MaxVolHotWaterFlowDes);
                    BaseSizer::reportSizerOutput(state,
                                                 thisPIU.UnitType,
                                                 thisPIU.Name,
                                                 "Design Size Reheat Coil Inlet Air Temperature [C]",
                                                 state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU);
                    BaseSizer::reportSizerOutput(state,
                                                 thisPIU.UnitType,
                                                 thisPIU.Name,
                                                 "Design Size Reheat Coil Inlet Air Humidity Ratio [kgWater/kgDryAir]",
                                                 state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInHumRatTU);
                } else { // Hardsize with sizing data
                    if (thisPIU.MaxVolHotWaterFlow > 0.0 && MaxVolHotWaterFlowDes > 0.0) {
                        // Hardsized maximum hot water flow for reporting
                        Real64 const MaxVolHotWaterFlowUser = thisPIU.MaxVolHotWaterFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     thisPIU.UnitType,
                                                     thisPIU.Name,
                                                     "Design Size Maximum Reheat Water Flow Rate [m3/s]",
                                                     MaxVolHotWaterFlowDes,
                                                     "User-Specified Maximum Reheat Water Flow Rate [m3/s]",
                                                     MaxVolHotWaterFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser) / MaxVolHotWaterFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            format("SizePIU: Potential issue with equipment sizing for {} {}", thisPIU.UnitType, thisPIU.Name));
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
                thisPIU.MaxVolHotWaterFlow = 0.0;
            }
        }
    }

    IsAutoSize = false;
    if (thisPIU.MaxVolHotSteamFlow == AutoSize) {
        IsAutoSize = true;
    }
    if ((state.dataSize->CurZoneEqNum > 0) && (CurTermUnitSizingNum > 0)) {
        if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
            if (thisPIU.MaxVolHotWaterFlow > 0.0) {
                BaseSizer::reportSizerOutput(
                    state, thisPIU.UnitType, thisPIU.Name, "User-Specified Maximum Reheat Steam Flow Rate [m3/s]", thisPIU.MaxVolHotWaterFlow);
            }
        } else {
            if (UtilityRoutines::SameString(HCoilNames[static_cast<int>(thisPIU.HCoilType)], "Coil:Heating:Steam")) {

                int const CoilSteamInletNode = GetCoilSteamInletNode(state, "Coil:Heating:Steam", thisPIU.HCoil, ErrorsFound);
                int const CoilSteamOutletNode = GetCoilSteamOutletNode(state, "Coil:Heating:Steam", thisPIU.HCoil, ErrorsFound);
                Real64 MaxVolHotSteamFlowDes = 0.0; // Autosized maximum hot steam flow for reporting

                if (IsAutoSize) {
                    int const PltSizHeatNum =
                        MyPlantSizingIndex(state, "Coil:Heating:Steam", thisPIU.HCoil, CoilSteamInletNode, CoilSteamOutletNode, ErrorsFound);
                    if (PltSizHeatNum > 0) {
                        if (state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatMassFlow >= SmallAirVolFlow) {
                            Real64 const CoilInTemp =
                                state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).DesHeatCoilInTempTU * thisPIU.MinPriAirFlowFrac +
                                state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).ZoneTempAtHeatPeak * (1.0 - thisPIU.MinPriAirFlowFrac);
                            Real64 const CoilOutTemp = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesTemp;
                            Real64 const CoilOutHumRat = state.dataSize->TermUnitFinalZoneSizing(CurTermUnitSizingNum).HeatDesHumRat;
                            Real64 const DesMassFlow = state.dataEnvrn->StdRhoAir * TermUnitSizing(CurTermUnitSizingNum).AirVolFlow;
                            DesCoilLoad = PsyCpAirFnW(CoilOutHumRat) * DesMassFlow * (CoilOutTemp - CoilInTemp);
                            Real64 constexpr TempSteamIn = 100.00;
                            Real64 const EnthSteamInDry =
                                GetSatEnthalpyRefrig(state, fluidNameSteam, TempSteamIn, 1.0, thisPIU.HCoil_FluidIndex, RoutineName);
                            Real64 const EnthSteamOutWet =
                                GetSatEnthalpyRefrig(state, fluidNameSteam, TempSteamIn, 0.0, thisPIU.HCoil_FluidIndex, RoutineName);
                            Real64 const LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                            Real64 const SteamDensity =
                                GetSatDensityRefrig(state, fluidNameSteam, TempSteamIn, 1.0, thisPIU.HCoil_FluidIndex, RoutineName);
                            int DummyWaterIndex = 1;
                            Real64 const Cp = GetSpecificHeatGlycol(
                                state, fluidNameWater, state.dataSize->PlantSizData(PltSizHeatNum).ExitTemp, DummyWaterIndex, RoutineName);
                            MaxVolHotSteamFlowDes =
                                DesCoilLoad / (SteamDensity * (LatentHeatSteam + state.dataSize->PlantSizData(PltSizHeatNum).DeltaT * Cp));
                        } else {
                            MaxVolHotSteamFlowDes = 0.0;
                        }
                    } else {
                        ShowSevereError(state, "Autosizing of Steam flow requires a heating loop Sizing:Plant object");
                        ShowContinueError(state, format("Occurs in{} Object={}", thisPIU.UnitType, thisPIU.Name));
                        ErrorsFound = true;
                    }
                }
                if (IsAutoSize) {
                    thisPIU.MaxVolHotSteamFlow = MaxVolHotSteamFlowDes;
                    BaseSizer::reportSizerOutput(
                        state, thisPIU.UnitType, thisPIU.Name, "Design Size Maximum Reheat Steam Flow [m3/s]", MaxVolHotSteamFlowDes);
                } else {
                    if (thisPIU.MaxVolHotSteamFlow > 0.0 && MaxVolHotSteamFlowDes > 0.0) {
                        Real64 const MaxVolHotSteamFlowUser = thisPIU.MaxVolHotSteamFlow;
                        BaseSizer::reportSizerOutput(state,
                                                     thisPIU.UnitType,
                                                     thisPIU.Name,
                                                     "Design Size Maximum Reheat Steam Flow [m3/s]",
                                                     MaxVolHotSteamFlowDes,
                                                     "User-Specified Maximum Reheat Steam Flow [m3/s]",
                                                     MaxVolHotSteamFlowUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser) / MaxVolHotSteamFlowUser) >
                                state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            format("SizePIU: Potential issue with equipment sizing for {} {}", thisPIU.UnitType, thisPIU.Name));
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
                thisPIU.MaxVolHotSteamFlow = 0.0;
            }
        }
    }

    if (CurTermUnitSizingNum > 0) {
        TermUnitSizing(CurTermUnitSizingNum).MinFlowFrac = thisPIU.MinPriAirFlowFrac;
        TermUnitSizing(CurTermUnitSizingNum).MaxHWVolFlow = thisPIU.MaxVolHotWaterFlow;
        TermUnitSizing(CurTermUnitSizingNum).MaxSTVolFlow = thisPIU.MaxVolHotSteamFlow;
        TermUnitSizing(CurTermUnitSizingNum).DesHeatingLoad = DesCoilLoad; // coil report
        TermUnitSizing(CurTermUnitSizingNum).InducesPlenumAir = thisPIU.InducesPlenumAir;
        if (thisPIU.HCoilType == HtgCoilType::SimpleHeating) {
            SetCoilDesFlow(state,
                           HCoilNamesUC[static_cast<int>(thisPIU.HCoilType)],
                           thisPIU.HCoil,
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
    bool UnitOn(true);  // TRUE if unit is on
    bool PriOn(true);   // TRUE if primary air available
    bool HCoilOn(true); // TRUE if heating coil is on

    Real64 QCoilReq;     // required heating coil outlet to meet zone load
    Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/s]
    Real64 MinWaterFlow; // minimum water flow for heating or cooling [kg/s]
    Real64 MinSteamFlow; // TODO: this is unused
    Real64 MaxSteamFlow; // TODO: this is unused

    // Initialize local fan flags to global system flags
    bool PIUTurnFansOn =
        (state.dataHVACGlobal->TurnFansOn || state.dataHVACGlobal->TurnZoneFansOnlyOn); // If True, overrides fan schedule and cycles PIU fan on
    bool const PIUTurnFansOff = state.dataHVACGlobal->TurnFansOff; // If True, overrides fan schedule and PIUTurnFansOn and cycles PIU fan off

    // initialize local variables
    auto &thisPIU = state.dataPowerInductionUnits->PIU(PIUNum);

    Real64 PriAirMassFlow = state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRate;                  // primary air mass flow rate [kg/s]
    Real64 const PriAirMassFlowMax = state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRateMaxAvail; // max primary air mass flow rate [kg/s]
    Real64 const PriAirMassFlowMin = state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRateMinAvail; // min primary air mass flow rate [kg/s]
    Real64 SecAirMassFlow = state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate;                  // secondary air mass flow rate [kg/s]
    Real64 const QZnReq =
        state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired; // heating or cooling needed by zone [Watts]
    Real64 const QToHeatSetPt =
        state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP; // [W]  remaining load to heating setpoint
    Real64 const CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);          // zone air specific heat [J/kg-C]

    // On the first HVAC iteration the system values are given to the controller, but after that
    // the demand limits are in place and there needs to be feedback to the Zone Equipment
    if (thisPIU.HotControlNode > 0) {
        if (FirstHVACIteration) {
            MaxWaterFlow = thisPIU.MaxHotWaterFlow;
            MinWaterFlow = thisPIU.MinHotWaterFlow;
            MaxSteamFlow = thisPIU.MaxHotWaterFlow; // TODO: Need TO change THESE******************************
            MinSteamFlow = thisPIU.MinHotWaterFlow;
        } else {
            MaxWaterFlow = state.dataLoopNodes->Node(thisPIU.HotControlNode).MassFlowRateMaxAvail;
            MinWaterFlow = state.dataLoopNodes->Node(thisPIU.HotControlNode).MassFlowRateMinAvail;
            MaxSteamFlow = state.dataLoopNodes->Node(thisPIU.HotControlNode).MassFlowRateMaxAvail;
            MinSteamFlow = state.dataLoopNodes->Node(thisPIU.HotControlNode).MassFlowRateMinAvail;
        }
    }
    if (GetCurrentScheduleValue(state, thisPIU.SchedPtr) <= 0.0) {
        UnitOn = false;
    }
    if ((GetCurrentScheduleValue(state, thisPIU.FanAvailSchedPtr) <= 0.0 || PIUTurnFansOff) && !PIUTurnFansOn) {
        UnitOn = false;
    }
    if (PriAirMassFlow <= SmallMassFlow || PriAirMassFlowMax <= SmallMassFlow) {
        PriOn = false;
    }
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
                SecAirMassFlow = thisPIU.MaxTotAirMassFlow;
            }
        } else if (state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) || std::abs(QZnReq) < SmallLoad) {
            // in deadband or very small load: set primary air flow to the minimum
            PriAirMassFlow = PriAirMassFlowMin;
            SecAirMassFlow = max(0.0, thisPIU.MaxTotAirMassFlow - PriAirMassFlow);
        } else if (QZnReq > SmallLoad) {
            // heating: set primary air flow to the minimum
            PriAirMassFlow = PriAirMassFlowMin;
            SecAirMassFlow = max(0.0, thisPIU.MaxTotAirMassFlow - PriAirMassFlow);
        } else {
            // cooling: set the primary air flow rate to meet the load.
            // First calculate the fan temperature rise
            // use only secondary air for this calculation
            state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate = thisPIU.MaxTotAirMassFlow;
            SimAirMixer(state, thisPIU.MixerName, thisPIU.Mixer_Num); // fire the mixer
            if (thisPIU.Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataHVACFan->fanObjs[thisPIU.Fan_Index]->simulate(state, _, PIUTurnFansOn, PIUTurnFansOff, _);
            } else if (thisPIU.Fan_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                Fans::SimulateFanComponents(state,
                                            thisPIU.FanName,
                                            FirstHVACIteration,
                                            thisPIU.Fan_Index,
                                            _,
                                            PIUTurnFansOn,
                                            PIUTurnFansOff); // fire the fan
            }

            // fan temperature rise [C]
            Real64 const FanDeltaTemp = state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(thisPIU.SecAirInNode).Temp;
            // using the required zone load, calculate the air temperature needed to meet the load
            // PIU(PIUNum)%MaxTotAirMassFlow * CpAirZn * (OutletTempNeeded - state.dataLoopNodes->Node(ZoneNodeNum)%Temp) = QZnReq
            Real64 const OutletTempNeeded = state.dataLoopNodes->Node(ZoneNode).Temp + QZnReq / (thisPIU.MaxTotAirMassFlow * CpAirZn);
            // mixer outlet temperature needed to meet cooling load
            Real64 const MixTempNeeded = OutletTempNeeded - FanDeltaTemp;
            if (MixTempNeeded <= state.dataLoopNodes->Node(thisPIU.PriAirInNode).Temp) {
                PriAirMassFlow = PriAirMassFlowMax;
            } else if (MixTempNeeded >= state.dataLoopNodes->Node(thisPIU.PriAirInNode).Temp &&
                       MixTempNeeded >= state.dataLoopNodes->Node(thisPIU.SecAirInNode).Temp) {
                PriAirMassFlow = PriAirMassFlowMin;
            } else {
                PriAirMassFlow =
                    thisPIU.MaxTotAirMassFlow * (state.dataLoopNodes->Node(thisPIU.SecAirInNode).Temp - MixTempNeeded) /
                    max(SmallTempDiff, state.dataLoopNodes->Node(thisPIU.SecAirInNode).Temp - state.dataLoopNodes->Node(thisPIU.PriAirInNode).Temp);
                PriAirMassFlow = min(max(PriAirMassFlow, PriAirMassFlowMin), PriAirMassFlowMax);
            }
            SecAirMassFlow = max(0.0, thisPIU.MaxTotAirMassFlow - PriAirMassFlow);
        }
    } else {
        // unit is off ; no flow
        PriAirMassFlow = 0.0;
        SecAirMassFlow = 0.0;
    }
    // Set inlet node flowrates
    state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRate = PriAirMassFlow;
    state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate = SecAirMassFlow;
    if (PriAirMassFlowMax == 0) {
        thisPIU.PriDamperPosition = 0;
    } else {
        thisPIU.PriDamperPosition = PriAirMassFlow / PriAirMassFlowMax;
    }
    // now that inlet airflows have been set, the terminal bos components can be simulated.

    // fire the mixer
    SimAirMixer(state, thisPIU.MixerName, thisPIU.Mixer_Num);
    // fire the fan
    if (thisPIU.Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
        state.dataHVACFan->fanObjs[thisPIU.Fan_Index]->simulate(state, _, PIUTurnFansOn, PIUTurnFansOff, _);
    } else if (thisPIU.Fan_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
        Fans::SimulateFanComponents(state, thisPIU.FanName, FirstHVACIteration, thisPIU.Fan_Index, _, PIUTurnFansOn,
                                    PIUTurnFansOff); // fire the fan
    }
    // the heating load seen by the reheat coil [W]
    Real64 const QActualHeating =
        QToHeatSetPt - state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).MassFlowRate * CpAirZn *
                           (state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
    // check if heating coil is off
    if ((!UnitOn) || (QActualHeating < SmallLoad) ||
        (state.dataHeatBalFanSys->TempControlType(ZoneNum) == DataHVACGlobals::ThermostatType::SingleCooling) ||
        (PriAirMassFlow > PriAirMassFlowMin)) {
        HCoilOn = false;
    }
    // fire the heating coil

    switch (thisPIU.HCoilType) {

    case HtgCoilType::SimpleHeating: { // COIL:WATER:SIMPLEHEATING
        if (!HCoilOn) {
            // call the reheat coil with the NO FLOW condition
            Real64 mdot = 0.0;
            SetComponentFlowRate(state, mdot, thisPIU.HotControlNode, thisPIU.HotCoilOutNodeNum, thisPIU.HWplantLoc);

            SimulateWaterCoilComponents(state, thisPIU.HCoil, FirstHVACIteration, thisPIU.HCoil_Index);
        } else {
            // control water flow to obtain output matching QZnReq
            ControlCompOutput(state,
                              thisPIU.HCoil,
                              thisPIU.UnitType,
                              thisPIU.HCoil_Index,
                              FirstHVACIteration,
                              QActualHeating,
                              thisPIU.HotControlNode,
                              MaxWaterFlow,
                              MinWaterFlow,
                              thisPIU.HotControlOffset,
                              thisPIU.ControlCompTypeNum,
                              thisPIU.CompErrIndex,
                              thisPIU.HCoilInAirNode,
                              thisPIU.OutAirNode,
                              _,
                              _,
                              _,
                              thisPIU.HWplantLoc);
        }
        break;
    }
    case HtgCoilType::SteamAirHeating: { // COIL:STEAM:AIRHEATING
        if (!HCoilOn) {
            QCoilReq = 0.0;
        } else {
            QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).MassFlowRate * CpAirZn *
                                          (state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
        }
        SimulateSteamCoilComponents(state, thisPIU.HCoil, FirstHVACIteration, thisPIU.HCoil_Index, QCoilReq);

        break;
    }
    case HtgCoilType::Electric: { // COIL:ELECTRIC:HEATING
        if (!HCoilOn) {
            QCoilReq = 0.0;
        } else {
            QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).MassFlowRate * CpAirZn *
                                          (state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
        }
        SimulateHeatingCoilComponents(state, thisPIU.HCoil, FirstHVACIteration, QCoilReq, thisPIU.HCoil_Index);

        break;
    }
    case HtgCoilType::Gas: { // COIL:GAS:HEATING
        if (!HCoilOn) {
            QCoilReq = 0.0;
        } else {
            QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).MassFlowRate * CpAirZn *
                                          (state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
        }
        SimulateHeatingCoilComponents(state, thisPIU.HCoil, FirstHVACIteration, QCoilReq, thisPIU.HCoil_Index);
        break;
    }
    default:
        break;
    }

    // Power supplied
    Real64 const PowerMet = state.dataLoopNodes->Node(thisPIU.OutAirNode).MassFlowRate *
                            (PsyHFnTdbW(state.dataLoopNodes->Node(thisPIU.OutAirNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat) -
                             PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat));
    thisPIU.HeatingRate = max(0.0, PowerMet);
    thisPIU.SensCoolRate = std::abs(min(DataPrecisionGlobals::constant_zero, PowerMet));
    if (state.dataLoopNodes->Node(thisPIU.OutAirNode).MassFlowRate == 0.0) {
        state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate = 0.0;
    }
    if (thisPIU.InducesPlenumAir) {
        state.dataHVACGlobal->PlenumInducedMassFlow = state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate;
    } else {
        state.dataHVACGlobal->PlenumInducedMassFlow = 0.0;
    }
    state.dataDefineEquipment->AirDistUnit(thisPIU.ADUNum).MassFlowRatePlenInd = state.dataHVACGlobal->PlenumInducedMassFlow;
    state.dataLoopNodes->Node(thisPIU.OutAirNode).MassFlowRateMax = thisPIU.MaxTotAirMassFlow;
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

    bool UnitOn(true);  // TRUE if unit is on
    bool PriOn(true);   // TRUE if primary air available
    bool HCoilOn(true); // TRUE if heating coil is on

    Real64 QCoilReq;     // required heating coil outlet to meet zone load
    Real64 MaxWaterFlow; // maximum water flow for heating or cooling [kg/s]
    Real64 MinWaterFlow; // minimum water flow for heating or cooling [kg/s]

    // initialize local variables
    auto &thisPIU = state.dataPowerInductionUnits->PIU(PIUNum);

    Real64 PriAirMassFlow = state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRate;                  // primary air mass flow rate [kg/s]
    Real64 const PriAirMassFlowMax = state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRateMaxAvail; // max primary air mass flow rate [kg/s]
    Real64 const PriAirMassFlowMin = state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRateMinAvail; // min primary air mass flow rate [kg/s]
    Real64 SecAirMassFlow = state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate;                  // secondary air mass flow rate [kg/s]
    Real64 const QZnReq =
        state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputRequired; // heating or cooling needed by zone [Watts]
    Real64 const QToHeatSetPt =
        state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP; // [W]  remaining load to heating setpoint
    Real64 const CpAirZn = PsyCpAirFnW(state.dataLoopNodes->Node(ZoneNode).HumRat);          // zone air specific heat [J/kg-C]

    // Initialize local fan flags to global system flags
    bool PIUTurnFansOn =
        (state.dataHVACGlobal->TurnFansOn || state.dataHVACGlobal->TurnZoneFansOnlyOn); // If True, overrides fan schedule and cycles PIU fan on
    bool const PIUTurnFansOff = state.dataHVACGlobal->TurnFansOff; // If True, overrides fan schedule and PIUTurnFansOn and cycles PIU fan off

    // On the first HVAC iteration the system values are given to the controller, but after that
    // the demand limits are in place and there needs to be feedback to the Zone Equipment
    if (thisPIU.HotControlNode > 0) {
        if (FirstHVACIteration) {
            MaxWaterFlow = thisPIU.MaxHotWaterFlow;
            MinWaterFlow = thisPIU.MinHotWaterFlow;
        } else {
            MaxWaterFlow = state.dataLoopNodes->Node(thisPIU.HotControlNode).MassFlowRateMaxAvail;
            MinWaterFlow = state.dataLoopNodes->Node(thisPIU.HotControlNode).MassFlowRateMinAvail;
        }
    }
    if (GetCurrentScheduleValue(state, thisPIU.SchedPtr) <= 0.0) {
        UnitOn = false;
    }
    if (PriAirMassFlow <= SmallMassFlow || PriAirMassFlowMax <= SmallMassFlow) {
        PriOn = false;
    }
    // Set the mass flow rates
    if (UnitOn) {
        // unit is on
        // Calculate if reheat is needed
        bool ReheatRequired = false;
        Real64 const qMinPrimary =
            PriAirMassFlowMin *
            (CpAirZn * min(-SmallTempDiff, (state.dataLoopNodes->Node(thisPIU.PriAirInNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp)));
        if (qMinPrimary < QToHeatSetPt) {
            ReheatRequired = true;
        }

        if (!PriOn) {
            // no primary air flow
            PriAirMassFlow = 0.0;
            // PIU fan off if there is no heating load, also reset fan flag if fan should be off
            if (QZnReq <= SmallLoad) {
                SecAirMassFlow = 0.0;
                PIUTurnFansOn = false;
            } else {
                SecAirMassFlow = thisPIU.MaxSecAirMassFlow;
                PIUTurnFansOn = (state.dataHVACGlobal->TurnFansOn || state.dataHVACGlobal->TurnZoneFansOnlyOn);
            }
        } else if (state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) || std::abs(QZnReq) < SmallLoad) {
            // in deadband or very small load: set primary air flow to the minimum
            PriAirMassFlow = PriAirMassFlowMin;
            // PIU fan off if reheat is not needed, also reset fan flag if fan should be off
            if (ReheatRequired) {
                SecAirMassFlow = thisPIU.MaxSecAirMassFlow;
                PIUTurnFansOn = true;
            } else {
                SecAirMassFlow = 0.0;
                PIUTurnFansOn = false;
            }
        } else if (QZnReq > SmallLoad) {
            // heating: set primary air flow to the minimum
            PriAirMassFlow = PriAirMassFlowMin;
            SecAirMassFlow = thisPIU.MaxSecAirMassFlow;
        } else {
            // cooling: set the primary air flow rate to meet the load.
            // First calculate the fan temperature rise
            state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate = thisPIU.MaxSecAirMassFlow;
            state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRateMaxAvail = thisPIU.MaxSecAirMassFlow;
            state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRate = 0.0;

            if (thisPIU.Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
                state.dataHVACFan->fanObjs[thisPIU.Fan_Index]->simulate(state, _, PIUTurnFansOn, PIUTurnFansOff, _);
            } else if (thisPIU.Fan_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                Fans::SimulateFanComponents(state,
                                            thisPIU.FanName,
                                            FirstHVACIteration,
                                            thisPIU.Fan_Index,
                                            _,
                                            PIUTurnFansOn,
                                            PIUTurnFansOff); // fire the fan
            }
            SimAirMixer(state, thisPIU.MixerName, thisPIU.Mixer_Num); // fire the mixer
            // fan temperature rise [C]
            Real64 const FanDeltaTemp = state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(thisPIU.SecAirInNode).Temp;
            // Assuming the fan is off, calculate the primary air flow needed to meet the zone cooling demand.
            // CpAir*PriAirMassFlow*(Node(thisPIU.PriAirInNode)%Temp - Node(ZoneNodeNum)%Temp) = QZnReq
            PriAirMassFlow =
                QZnReq /
                (CpAirZn * min(-SmallTempDiff, (state.dataLoopNodes->Node(thisPIU.PriAirInNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp)));
            PriAirMassFlow = min(max(PriAirMassFlow, PriAirMassFlowMin), PriAirMassFlowMax);
            // check for fan on or off
            if ((PriAirMassFlow > thisPIU.FanOnAirMassFlow) && !ReheatRequired) {
                SecAirMassFlow = 0.0; // Fan is off unless reheat is required; no secondary air; also reset fan flag
                PIUTurnFansOn = false;
            } else {
                // fan is on; recalc primary air flow
                // CpAir*PriAirMassFlow*(Node(thisPIU.PriAirInNode)%Temp - Node(ZoneNodeNum)%Temp) +
                //   CpAir*SecAirMassFlow*(Node(thisPIU.SecAirInNode)%Temp + FanDeltaTemp - Node(ZoneNodeNum)%Temp) = QZnReq
                PriAirMassFlow =
                    (QZnReq - CpAirZn * SecAirMassFlow *
                                  (state.dataLoopNodes->Node(thisPIU.SecAirInNode).Temp + FanDeltaTemp - state.dataLoopNodes->Node(ZoneNode).Temp)) /
                    (CpAirZn *
                     min(-SmallTempDiff, (state.dataLoopNodes->Node(thisPIU.PriAirInNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp)));
                PriAirMassFlow = min(max(PriAirMassFlow, PriAirMassFlowMin), PriAirMassFlowMax);
                SecAirMassFlow = thisPIU.MaxSecAirMassFlow;
            }
        }
    } else {
        // unit is off; no flow
        PriAirMassFlow = 0.0;
        SecAirMassFlow = 0.0;
    }
    // Set inlet node flowrates
    state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRate = PriAirMassFlow;
    state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate = SecAirMassFlow;
    state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRateMaxAvail = SecAirMassFlow;
    if (PriAirMassFlowMax == 0) {
        thisPIU.PriDamperPosition = 0;
    } else {
        thisPIU.PriDamperPosition = PriAirMassFlow / PriAirMassFlowMax;
    }
    // now that inlet airflows have been set, the terminal box components can be simulated.
    // fire the fan

    if (thisPIU.Fan_Num == DataHVACGlobals::FanType_SystemModelObject) {
        state.dataHVACFan->fanObjs[thisPIU.Fan_Index]->simulate(state, _, PIUTurnFansOn, PIUTurnFansOff, _);
    } else if (thisPIU.Fan_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
        Fans::SimulateFanComponents(state, thisPIU.FanName, FirstHVACIteration, thisPIU.Fan_Index, _, PIUTurnFansOn,
                                    PIUTurnFansOff); // fire the fan
    }
    // fire the mixer
    SimAirMixer(state, thisPIU.MixerName, thisPIU.Mixer_Num);
    // the heating load seen by the reheat coil [W]
    Real64 const QActualHeating =
        QToHeatSetPt - state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).MassFlowRate * CpAirZn *
                           (state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
    // check if heating coil is off
    if ((!UnitOn) || (QActualHeating < SmallLoad) ||
        (state.dataHeatBalFanSys->TempControlType(ZoneNum) == DataHVACGlobals::ThermostatType::SingleCooling) ||
        (PriAirMassFlow > PriAirMassFlowMin)) {
        HCoilOn = false;
    }
    // fire the heating coil
    switch (thisPIU.HCoilType) {

    case HtgCoilType::SimpleHeating: { // COIL:WATER:SIMPLEHEATING
        if (!HCoilOn) {
            // call the reheat coil with the NO FLOW condition
            Real64 mdot = 0.0;
            SetComponentFlowRate(state, mdot, thisPIU.HotControlNode, thisPIU.HotCoilOutNodeNum, thisPIU.HWplantLoc);
            SimulateWaterCoilComponents(state, thisPIU.HCoil, FirstHVACIteration, thisPIU.HCoil_Index);
        } else {
            // control water flow to obtain output matching QZnReq
            ControlCompOutput(state,
                              thisPIU.HCoil,
                              thisPIU.UnitType,
                              thisPIU.HCoil_Index,
                              FirstHVACIteration,
                              QActualHeating,
                              thisPIU.HotControlNode,
                              MaxWaterFlow,
                              MinWaterFlow,
                              thisPIU.HotControlOffset,
                              thisPIU.ControlCompTypeNum,
                              thisPIU.CompErrIndex,
                              thisPIU.HCoilInAirNode,
                              thisPIU.OutAirNode,
                              _,
                              _,
                              _,
                              thisPIU.HWplantLoc);
        }
        break;
    }
    case HtgCoilType::SteamAirHeating: { // COIL:STEAM:AIRHEATING
        if (!HCoilOn) {
            QCoilReq = 0.0;
        } else {
            QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).MassFlowRate * CpAirZn *
                                          (state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
        }
        SimulateSteamCoilComponents(state, thisPIU.HCoil, FirstHVACIteration, thisPIU.HCoil_Index, QCoilReq);
        break;
    }
    case HtgCoilType::Electric: { // COIL:ELECTRIC:HEATING
        if (!HCoilOn) {
            QCoilReq = 0.0;
        } else {
            QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).MassFlowRate * CpAirZn *
                                          (state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
        }
        SimulateHeatingCoilComponents(state, thisPIU.HCoil, FirstHVACIteration, QCoilReq, thisPIU.HCoil_Index);

        break;
    }
    case HtgCoilType::Gas: { // COIL:GAS:HEATING
        if (!HCoilOn) {
            QCoilReq = 0.0;
        } else {
            QCoilReq = QToHeatSetPt - state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).MassFlowRate * CpAirZn *
                                          (state.dataLoopNodes->Node(thisPIU.HCoilInAirNode).Temp - state.dataLoopNodes->Node(ZoneNode).Temp);
        }
        SimulateHeatingCoilComponents(state, thisPIU.HCoil, FirstHVACIteration, QCoilReq, thisPIU.HCoil_Index);
        break;
    }
    default:
        break;
    }
    // Power supplied
    Real64 const PowerMet = state.dataLoopNodes->Node(thisPIU.OutAirNode).MassFlowRate *
                            (PsyHFnTdbW(state.dataLoopNodes->Node(thisPIU.OutAirNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat) -
                             PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNode).Temp, state.dataLoopNodes->Node(ZoneNode).HumRat));
    thisPIU.HeatingRate = max(0.0, PowerMet);
    thisPIU.SensCoolRate = std::abs(min(DataPrecisionGlobals::constant_zero, PowerMet));
    if (state.dataLoopNodes->Node(thisPIU.OutAirNode).MassFlowRate == 0.0) {
        state.dataLoopNodes->Node(thisPIU.PriAirInNode).MassFlowRate = 0.0;
        state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate = 0.0;
    }
    if (thisPIU.InducesPlenumAir) {
        state.dataHVACGlobal->PlenumInducedMassFlow = state.dataLoopNodes->Node(thisPIU.SecAirInNode).MassFlowRate;
    } else {
        state.dataHVACGlobal->PlenumInducedMassFlow = 0.0;
    }
    state.dataDefineEquipment->AirDistUnit(thisPIU.ADUNum).MassFlowRatePlenInd = state.dataHVACGlobal->PlenumInducedMassFlow;
    state.dataLoopNodes->Node(thisPIU.OutAirNode).MassFlowRateMax = thisPIU.MaxPriAirMassFlow;
}

void ReportPIU(EnergyPlusData &state, int const PIUNum) // number of the current fan coil unit being simulated
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Fills some report variables for the PIU terminal boxes

    // Using/Aliasing
    Real64 TimeStepSysSec = state.dataHVACGlobal->TimeStepSysSec;

    auto &thisPIU = state.dataPowerInductionUnits->PIU(PIUNum);
    thisPIU.HeatingEnergy = thisPIU.HeatingRate * TimeStepSysSec;
    thisPIU.SensCoolEnergy = thisPIU.SensCoolRate * TimeStepSysSec;

    // set zone OA Volume flow rate
    thisPIU.CalcOutdoorAirVolumeFlowRate(state);
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
    bool YesNo = false; // True if found

    if (state.dataPowerInductionUnits->GetPIUInputFlag) {
        GetPIUs(state);
        state.dataPowerInductionUnits->GetPIUInputFlag = false;
    }

    if (state.dataPowerInductionUnits->NumPIUs > 0) {
        int const ItemNum = UtilityRoutines::FindItemInList(CompName, state.dataPowerInductionUnits->PIU, &PowIndUnitData::MixerName);
        if (ItemNum > 0) {
            YesNo = true;
        }
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

    if (state.dataPowerInductionUnits->GetPIUInputFlag) {
        GetPIUs(state);
        state.dataPowerInductionUnits->GetPIUInputFlag = false;
    }

    for (int PIUIndex = 1; PIUIndex <= state.dataPowerInductionUnits->NumPIUs; ++PIUIndex) {
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
