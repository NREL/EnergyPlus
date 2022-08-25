// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/CoolingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/CoolingCapacitySizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>

namespace EnergyPlus::EvaporativeCoolers {
// Module containing the EvaporativeCoolers simulation routines

// MODULE INFORMATION:
//       AUTHOR         Richard J. Liesen
//       DATE WRITTEN   Oct 2000
//       MODIFIED       BG July 2003 ResearchSpecial Indirect
//                      BG Febraury 2007 outside air nodes
//                      BG March 2009 ResearchSpecial Direct
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required for
// Evaporative Coolers Components for use in mechanical air systems

// provide models for evaporative coolers as zone forced air units.

// METHODOLOGY EMPLOYED:
// various evaporative component models in this module
//   different models share common module level data structure.

constexpr std::array<std::string_view, static_cast<int>(EvapCoolerType::Num)> evapCoolerTypeNamesUC = {"EVAPORATIVECOOLER:DIRECT:CELDEKPAD",
                                                                                                       "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD",
                                                                                                       "EVAPORATIVECOOLER:INDIRECT:WETCOIL",
                                                                                                       "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL",
                                                                                                       "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL"};
constexpr std::array<std::string_view, static_cast<int>(EvapCoolerType::Num)> evapCoolerTypeNames = {"EvaporativeCooler:Direct:CelDekPad",
                                                                                                     "EvaporativeCooler:Indirect:CelDekPad",
                                                                                                     "EvaporativeCooler:Indirect:WetCoil",
                                                                                                     "EvaporativeCooler:Indirect:ResearchSpecial",
                                                                                                     "EvaporativeCooler:Direct:ResearchSpecial"};

void SimEvapCooler(EnergyPlusData &state, std::string_view CompName, int &CompIndex, Real64 const ZoneEvapCoolerPLR)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard Liesen
    //       DATE WRITTEN   October 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages EvapCooler component simulation.
    // It is called from the SimAirLoopComponent
    // at the system time step.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int EvapCoolNum; // The EvapCooler that you are currently loading input into

    auto &EvapCond(state.dataEvapCoolers->EvapCond);

    // Obtains and Allocates EvapCooler related parameters from input file
    if (state.dataEvapCoolers->GetInputEvapComponentsFlag) { // First time subroutine has been entered
        GetEvapInput(state);
        state.dataEvapCoolers->GetInputEvapComponentsFlag = false;
    }

    // Find the correct EvapCoolNumber
    if (CompIndex == 0) {
        EvapCoolNum = UtilityRoutines::FindItemInList(CompName, EvapCond, &EvapConditions::Name);
        if (EvapCoolNum == 0) {
            ShowFatalError(state, "SimEvapCooler: Unit not found=" + std::string{CompName});
        }
        CompIndex = EvapCoolNum;
    } else {
        EvapCoolNum = CompIndex;
        if (EvapCoolNum > state.dataEvapCoolers->NumEvapCool || EvapCoolNum < 1) {
            ShowFatalError(state,
                           format("SimEvapCooler:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                  EvapCoolNum,
                                  state.dataEvapCoolers->NumEvapCool,
                                  CompName));
        }
        if (state.dataEvapCoolers->CheckEquipName(EvapCoolNum)) {
            if (CompName != EvapCond(EvapCoolNum).Name) {
                ShowFatalError(state,
                               format("SimEvapCooler: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                      EvapCoolNum,
                                      CompName,
                                      EvapCond(EvapCoolNum).Name));
            }
            state.dataEvapCoolers->CheckEquipName(EvapCoolNum) = false;
        }
    }

    // With the correct EvapCoolNum Initialize
    InitEvapCooler(state, EvapCoolNum); // Initialize all related parameters

    switch (EvapCond(EvapCoolNum).evapCoolerType) {
    case EvapCoolerType::DirectCELDEKPAD: {
        CalcDirectEvapCooler(state, EvapCoolNum, ZoneEvapCoolerPLR);
    } break;
    case EvapCoolerType::IndirectCELDEKPAD: {
        CalcDryIndirectEvapCooler(state, EvapCoolNum, ZoneEvapCoolerPLR);
    } break;
    case EvapCoolerType::IndirectWETCOIL: {
        CalcWetIndirectEvapCooler(state, EvapCoolNum, ZoneEvapCoolerPLR);
    } break;
    case EvapCoolerType::IndirectRDDSpecial: {
        CalcResearchSpecialPartLoad(state, EvapCoolNum);
        CalcIndirectResearchSpecialEvapCooler(state, EvapCoolNum, ZoneEvapCoolerPLR);
    } break;
    case EvapCoolerType::DirectResearchSpecial: {
        CalcResearchSpecialPartLoad(state, EvapCoolNum);
        CalcDirectResearchSpecialEvapCooler(state, EvapCoolNum, ZoneEvapCoolerPLR);
    } break;
    default:
        break;
    }
    // Update the current Evap Cooler to the outlet nodes
    UpdateEvapCooler(state, EvapCoolNum);

    // Report the current Evap Cooler
    ReportEvapCooler(state, EvapCoolNum);
}

void GetEvapInput(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   Oct 2000
    //       MODIFIED       BTG,  adding in EVAPCOOLER:INDIRECT:RDDSPECIAL
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is the main routine to call other input routines and Get routines

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumDirectEvapCool;                // The number of Direct CelDek EvapCooler in this simulation
    int NumDryInDirectEvapCool;           // The number of dry indirect evap coolers
    int NumWetInDirectEvapCool;           // The number of wet indirect evap coolers
    int NumRDDEvapCool;                   // the number of special research indirect evap coolers
    int NumDirectResearchSpecialEvapCool; // the number of special research direct evap coolers

    int NumAlphas;
    int NumNums;
    int IOStat;
    bool ErrorsFound(false);

    auto &EvapCond(state.dataEvapCoolers->EvapCond);
    auto &UniqueEvapCondNames(state.dataEvapCoolers->UniqueEvapCondNames);

    state.dataEvapCoolers->GetInputEvapComponentsFlag = false;
    // Start getting the input data
    NumDirectEvapCool = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "EvaporativeCooler:Direct:CelDekPad");
    NumDryInDirectEvapCool = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "EvaporativeCooler:Indirect:CelDekPad");
    NumWetInDirectEvapCool = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "EvaporativeCooler:Indirect:WetCoil");
    NumRDDEvapCool = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "EvaporativeCooler:Indirect:ResearchSpecial");
    NumDirectResearchSpecialEvapCool =
        state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "EvaporativeCooler:Direct:ResearchSpecial");

    // Sum up all of the Evap Cooler Types
    state.dataEvapCoolers->NumEvapCool =
        NumDirectEvapCool + NumDryInDirectEvapCool + NumWetInDirectEvapCool + NumRDDEvapCool + NumDirectResearchSpecialEvapCool;

    if (state.dataEvapCoolers->NumEvapCool > 0) {
        EvapCond.allocate(state.dataEvapCoolers->NumEvapCool);
        UniqueEvapCondNames.reserve(state.dataEvapCoolers->NumEvapCool);
    }
    state.dataEvapCoolers->CheckEquipName.dimension(state.dataEvapCoolers->NumEvapCool, true);
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "EvaporativeCooler:Direct:CelDekPad";

    for (int EvapCoolNum = 1; EvapCoolNum <= NumDirectEvapCool; ++EvapCoolNum) {
        auto &thisEvapCooler = EvapCond(EvapCoolNum);
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 EvapCoolNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 _,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 UniqueEvapCondNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        thisEvapCooler.Name = state.dataIPShortCut->cAlphaArgs(1);
        thisEvapCooler.evapCoolerType = EvapCoolerType::DirectCELDEKPAD;

        thisEvapCooler.Schedule = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            thisEvapCooler.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            thisEvapCooler.SchedPtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (thisEvapCooler.SchedPtr == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
        }

        thisEvapCooler.InletNode = GetOnlySingleNode(state,
                                                     state.dataIPShortCut->cAlphaArgs(3),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::EvaporativeCoolerDirectCelDekPad,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     DataLoopNode::ObjectIsNotParent);

        thisEvapCooler.OutletNode = GetOnlySingleNode(state,
                                                      state.dataIPShortCut->cAlphaArgs(4),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::EvaporativeCoolerDirectCelDekPad,
                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      DataLoopNode::ObjectIsNotParent);

        BranchNodeConnections::TestCompSet(state,
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(3),
                                           state.dataIPShortCut->cAlphaArgs(4),
                                           "Evap Air Nodes");

        thisEvapCooler.EvapControlType = state.dataIPShortCut->cAlphaArgs(5);

        // input the numerical data
        thisEvapCooler.PadArea = state.dataIPShortCut->rNumericArgs(1);
        thisEvapCooler.PadDepth = state.dataIPShortCut->rNumericArgs(2);
        thisEvapCooler.RecircPumpPower = state.dataIPShortCut->rNumericArgs(3);

        SetupOutputVariable(state,
                            "Evaporative Cooler Wet Bulb Effectiveness",
                            OutputProcessor::Unit::None,
                            thisEvapCooler.SatEff,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);

        // A6 ; \Field Name of Water Supply Storage Tank
        thisEvapCooler.EvapWaterSupplyName = state.dataIPShortCut->cAlphaArgs(6);
        if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromMains;
        } else {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromTank;
            WaterManager::SetupTankDemandComponent(state,
                                                   thisEvapCooler.Name,
                                                   cCurrentModuleObject,
                                                   thisEvapCooler.EvapWaterSupplyName,
                                                   ErrorsFound,
                                                   thisEvapCooler.EvapWaterSupTankID,
                                                   thisEvapCooler.EvapWaterTankDemandARRID);
        }

    } // end Number of EvapCooler Loop

    //**************************************************************
    // This is the start of the Dry Indirect Evap Cooler Loop
    cCurrentModuleObject = "EvaporativeCooler:Indirect:CelDekPad";

    for (int IndEvapCoolNum = 1; IndEvapCoolNum <= NumDryInDirectEvapCool; ++IndEvapCoolNum) {
        int EvapCoolNum = NumDirectEvapCool + IndEvapCoolNum;
        auto &thisEvapCooler = EvapCond(EvapCoolNum);
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 IndEvapCoolNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 _,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 UniqueEvapCondNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        thisEvapCooler.Name = state.dataIPShortCut->cAlphaArgs(1);
        thisEvapCooler.evapCoolerType = EvapCoolerType::IndirectCELDEKPAD; //'EvaporativeCooler:Indirect:CelDekPad'

        thisEvapCooler.Schedule = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            thisEvapCooler.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            thisEvapCooler.SchedPtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (thisEvapCooler.SchedPtr == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
        }

        thisEvapCooler.InletNode = GetOnlySingleNode(state,
                                                     state.dataIPShortCut->cAlphaArgs(3),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectCelDekPad,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     DataLoopNode::ObjectIsNotParent);

        thisEvapCooler.OutletNode = GetOnlySingleNode(state,
                                                      state.dataIPShortCut->cAlphaArgs(4),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectCelDekPad,
                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      DataLoopNode::ObjectIsNotParent);

        BranchNodeConnections::TestCompSet(state,
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(3),
                                           state.dataIPShortCut->cAlphaArgs(4),
                                           "Evap Air Nodes");

        thisEvapCooler.EvapControlType = state.dataIPShortCut->cAlphaArgs(5);

        // input the numerical data
        thisEvapCooler.IndirectPadArea = state.dataIPShortCut->rNumericArgs(1);
        thisEvapCooler.IndirectPadDepth = state.dataIPShortCut->rNumericArgs(2);
        thisEvapCooler.IndirectRecircPumpPower = state.dataIPShortCut->rNumericArgs(3);
        thisEvapCooler.IndirectVolFlowRate = state.dataIPShortCut->rNumericArgs(4);
        thisEvapCooler.IndirectFanEff = state.dataIPShortCut->rNumericArgs(5);
        thisEvapCooler.IndirectFanDeltaPress = state.dataIPShortCut->rNumericArgs(6);
        thisEvapCooler.IndirectHXEffectiveness = state.dataIPShortCut->rNumericArgs(7);

        SetupOutputVariable(state,
                            "Evaporative Cooler Wetbulb Effectiveness",
                            OutputProcessor::Unit::None,
                            thisEvapCooler.SatEff,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);
        SetupOutputVariable(state,
                            "Evaporative Cooler Total Stage Effectiveness",
                            OutputProcessor::Unit::None,
                            thisEvapCooler.StageEff,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);

        // A6 ; \Field Name of Water Supply Storage Tank
        thisEvapCooler.EvapWaterSupplyName = state.dataIPShortCut->cAlphaArgs(6);
        if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromMains;
        } else {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromTank;
            WaterManager::SetupTankDemandComponent(state,
                                                   thisEvapCooler.Name,
                                                   cCurrentModuleObject,
                                                   thisEvapCooler.EvapWaterSupplyName,
                                                   ErrorsFound,
                                                   thisEvapCooler.EvapWaterSupTankID,
                                                   thisEvapCooler.EvapWaterTankDemandARRID);
        }

        // A7 ; \field Secondary Outside Air Inlet node.
        if (state.dataIPShortCut->lAlphaFieldBlanks(7)) {
            thisEvapCooler.SecondaryInletNode = 0;
        } else {
            thisEvapCooler.SecondaryInletNode = GetOnlySingleNode(state,
                                                                  state.dataIPShortCut->cAlphaArgs(7),
                                                                  ErrorsFound,
                                                                  DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectCelDekPad,
                                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                                  DataLoopNode::NodeFluidType::Air,
                                                                  DataLoopNode::ConnectionType::OutsideAirReference,
                                                                  NodeInputManager::CompFluidStream::Primary,
                                                                  DataLoopNode::ObjectIsNotParent);
            if (!OutAirNodeManager::CheckOutAirNodeNumber(state, thisEvapCooler.SecondaryInletNode)) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                // TODO rename point
                ShowContinueError(state, "Node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                ErrorsFound = true;
            }
        }

    } // end Number of Dry Indirect EvapCooler Loop

    //**************************************************************
    // This is the start of the WetIndirect Evap Cooler Loop
    cCurrentModuleObject = "EvaporativeCooler:Indirect:WetCoil";
    for (int IndEvapCoolNum = 1; IndEvapCoolNum <= NumWetInDirectEvapCool; ++IndEvapCoolNum) {
        int EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + IndEvapCoolNum;
        auto &thisEvapCooler = EvapCond(EvapCoolNum);
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 IndEvapCoolNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 _,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 UniqueEvapCondNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        thisEvapCooler.Name = state.dataIPShortCut->cAlphaArgs(1);
        thisEvapCooler.evapCoolerType = EvapCoolerType::IndirectWETCOIL; //'EvaporativeCooler:Indirect:WetCoil'

        thisEvapCooler.Schedule = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            thisEvapCooler.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            thisEvapCooler.SchedPtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (thisEvapCooler.SchedPtr == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
        }

        thisEvapCooler.InletNode = GetOnlySingleNode(state,
                                                     state.dataIPShortCut->cAlphaArgs(3),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectWetCoil,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     DataLoopNode::ObjectIsNotParent);

        thisEvapCooler.OutletNode = GetOnlySingleNode(state,
                                                      state.dataIPShortCut->cAlphaArgs(4),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectWetCoil,
                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      DataLoopNode::ObjectIsNotParent);

        BranchNodeConnections::TestCompSet(state,
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(3),
                                           state.dataIPShortCut->cAlphaArgs(4),
                                           "Evap Air Nodes");

        thisEvapCooler.EvapControlType = state.dataIPShortCut->cAlphaArgs(5);

        // input the numerical data
        thisEvapCooler.WetCoilMaxEfficiency = state.dataIPShortCut->rNumericArgs(1);
        thisEvapCooler.WetCoilFlowRatio = state.dataIPShortCut->rNumericArgs(2);
        thisEvapCooler.IndirectRecircPumpPower = state.dataIPShortCut->rNumericArgs(3);
        thisEvapCooler.IndirectVolFlowRate = state.dataIPShortCut->rNumericArgs(4);
        thisEvapCooler.IndirectFanEff = state.dataIPShortCut->rNumericArgs(5);
        thisEvapCooler.IndirectFanDeltaPress = state.dataIPShortCut->rNumericArgs(6);

        SetupOutputVariable(state,
                            "Evaporative Cooler Total Stage Effectiveness",
                            OutputProcessor::Unit::None,
                            thisEvapCooler.StageEff,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);

        //  A6 ; \Field Name of Water Supply Storage Tank
        thisEvapCooler.EvapWaterSupplyName = state.dataIPShortCut->cAlphaArgs(6);
        if (state.dataIPShortCut->lAlphaFieldBlanks(6)) {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromMains;
        } else {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromTank;
            WaterManager::SetupTankDemandComponent(state,
                                                   thisEvapCooler.Name,
                                                   cCurrentModuleObject,
                                                   thisEvapCooler.EvapWaterSupplyName,
                                                   ErrorsFound,
                                                   thisEvapCooler.EvapWaterSupTankID,
                                                   thisEvapCooler.EvapWaterTankDemandARRID);
        }

        // A7 ; \field Secondary Outside Air Inlet node.
        if (state.dataIPShortCut->lAlphaFieldBlanks(7)) {
            thisEvapCooler.SecondaryInletNode = 0;
        } else {
            thisEvapCooler.SecondaryInletNode = GetOnlySingleNode(state,
                                                                  state.dataIPShortCut->cAlphaArgs(7),
                                                                  ErrorsFound,
                                                                  DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectWetCoil,
                                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                                  DataLoopNode::NodeFluidType::Air,
                                                                  DataLoopNode::ConnectionType::OutsideAirReference,
                                                                  NodeInputManager::CompFluidStream::Primary,
                                                                  DataLoopNode::ObjectIsNotParent);
            if (!OutAirNodeManager::CheckOutAirNodeNumber(state, thisEvapCooler.SecondaryInletNode)) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                // TODO rename point
                ShowContinueError(state, "Node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                ErrorsFound = true;
            }
        }

    } // end Number of Wet Coil Indirect EvapCooler Loop
    //**************************************************************
    // This is the start of the Indirect Research Special Evap Cooler
    cCurrentModuleObject = "EvaporativeCooler:Indirect:ResearchSpecial";
    for (int IndEvapCoolNum = 1; IndEvapCoolNum <= NumRDDEvapCool; ++IndEvapCoolNum) {
        int EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + NumWetInDirectEvapCool + IndEvapCoolNum;
        auto &thisEvapCooler = EvapCond(EvapCoolNum);
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 IndEvapCoolNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 UniqueEvapCondNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        thisEvapCooler.Name = state.dataIPShortCut->cAlphaArgs(1);
        thisEvapCooler.evapCoolerType = EvapCoolerType::IndirectRDDSpecial; //'EvaporativeCooler:Indirect:ResearchSpecial'

        thisEvapCooler.Schedule = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            thisEvapCooler.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            thisEvapCooler.SchedPtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (thisEvapCooler.SchedPtr == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
        }

        thisEvapCooler.InletNode = GetOnlySingleNode(state,
                                                     state.dataIPShortCut->cAlphaArgs(7),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectResearchSpecial,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     DataLoopNode::ObjectIsNotParent);

        thisEvapCooler.OutletNode = GetOnlySingleNode(state,
                                                      state.dataIPShortCut->cAlphaArgs(8),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectResearchSpecial,
                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      DataLoopNode::ObjectIsNotParent);

        BranchNodeConnections::TestCompSet(state,
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(7),
                                           state.dataIPShortCut->cAlphaArgs(8),
                                           "Evap Air Nodes");

        if (state.dataIPShortCut->lAlphaFieldBlanks(9)) {
            thisEvapCooler.SecondaryInletNode = 0;
        } else {
            thisEvapCooler.SecondaryInletNode = GetOnlySingleNode(state,
                                                                  state.dataIPShortCut->cAlphaArgs(9),
                                                                  ErrorsFound,
                                                                  DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectResearchSpecial,
                                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                                  DataLoopNode::NodeFluidType::Air,
                                                                  DataLoopNode::ConnectionType::Inlet,
                                                                  NodeInputManager::CompFluidStream::Secondary,
                                                                  DataLoopNode::ObjectIsNotParent);
        }

        if (state.dataIPShortCut->lAlphaFieldBlanks(10)) {
            thisEvapCooler.SecondaryOutletNode = 0;
        } else {
            thisEvapCooler.SecondaryOutletNode = GetOnlySingleNode(state,
                                                                   state.dataIPShortCut->cAlphaArgs(10),
                                                                   ErrorsFound,
                                                                   DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectResearchSpecial,
                                                                   state.dataIPShortCut->cAlphaArgs(1),
                                                                   DataLoopNode::NodeFluidType::Air,
                                                                   DataLoopNode::ConnectionType::Outlet,
                                                                   NodeInputManager::CompFluidStream::Secondary,
                                                                   DataLoopNode::ObjectIsNotParent);
        }

        thisEvapCooler.EvapControlNodeNum = GetOnlySingleNode(state,
                                                              state.dataIPShortCut->cAlphaArgs(11),
                                                              ErrorsFound,
                                                              DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectResearchSpecial,
                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                              DataLoopNode::NodeFluidType::Air,
                                                              DataLoopNode::ConnectionType::Sensor,
                                                              NodeInputManager::CompFluidStream::Primary,
                                                              DataLoopNode::ObjectIsNotParent);

        thisEvapCooler.TertiaryInletNode = GetOnlySingleNode(state,
                                                             state.dataIPShortCut->cAlphaArgs(12),
                                                             ErrorsFound,
                                                             DataLoopNode::ConnectionObjectType::EvaporativeCoolerIndirectResearchSpecial,
                                                             state.dataIPShortCut->cAlphaArgs(1),
                                                             DataLoopNode::NodeFluidType::Air,
                                                             DataLoopNode::ConnectionType::Inlet,
                                                             NodeInputManager::CompFluidStream::Tertiary,
                                                             DataLoopNode::ObjectIsNotParent);

        thisEvapCooler.EvapWaterSupplyName = state.dataIPShortCut->cAlphaArgs(13);
        if (state.dataIPShortCut->lAlphaFieldBlanks(13)) {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromMains;
        } else {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromTank;
            WaterManager::SetupTankDemandComponent(state,
                                                   thisEvapCooler.Name,
                                                   cCurrentModuleObject,
                                                   thisEvapCooler.EvapWaterSupplyName,
                                                   ErrorsFound,
                                                   thisEvapCooler.EvapWaterSupTankID,
                                                   thisEvapCooler.EvapWaterTankDemandARRID);
        }

        // input the numerical data
        thisEvapCooler.WetCoilMaxEfficiency = state.dataIPShortCut->rNumericArgs(1);
        if (state.dataIPShortCut->lNumericFieldBlanks(2)) {
            thisEvapCooler.DryCoilMaxEfficiency = 0.0;
        } else {
            thisEvapCooler.DryCoilMaxEfficiency = state.dataIPShortCut->rNumericArgs(2);
        }
        thisEvapCooler.IndirectRecircPumpPower = state.dataIPShortCut->rNumericArgs(3);
        thisEvapCooler.RecircPumpSizingFactor = state.dataIPShortCut->rNumericArgs(4);
        thisEvapCooler.IndirectVolFlowRate = state.dataIPShortCut->rNumericArgs(5);
        thisEvapCooler.IndirectVolFlowScalingFactor = state.dataIPShortCut->rNumericArgs(6);
        thisEvapCooler.IndirectFanPower = state.dataIPShortCut->rNumericArgs(7);
        thisEvapCooler.FanSizingSpecificPower = state.dataIPShortCut->rNumericArgs(8);
        thisEvapCooler.DesVolFlowRate = state.dataIPShortCut->rNumericArgs(9);
        thisEvapCooler.DPBoundFactor = state.dataIPShortCut->rNumericArgs(10);
        if (state.dataIPShortCut->lNumericFieldBlanks(11)) {
            thisEvapCooler.DriftFraction = 0.0;
        } else {
            thisEvapCooler.DriftFraction = state.dataIPShortCut->rNumericArgs(11);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(12)) {
            thisEvapCooler.BlowDownRatio = 0.0;
        } else {
            thisEvapCooler.BlowDownRatio = state.dataIPShortCut->rNumericArgs(12);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(2) || state.dataIPShortCut->lNumericFieldBlanks(13) ||
            state.dataIPShortCut->lNumericFieldBlanks(14) || state.dataIPShortCut->lNumericFieldBlanks(15)) {
            thisEvapCooler.EvapCoolerOperationControlFlag = false;
        } else {
            if (!state.dataIPShortCut->lNumericFieldBlanks(2) && !state.dataIPShortCut->lNumericFieldBlanks(13) &&
                !state.dataIPShortCut->lNumericFieldBlanks(14) && !state.dataIPShortCut->lNumericFieldBlanks(15)) {
                thisEvapCooler.EvapCoolerOperationControlFlag = true;
                thisEvapCooler.MinOATDBEvapCooler = state.dataIPShortCut->rNumericArgs(13);
                thisEvapCooler.MaxOATWBEvapCooler = state.dataIPShortCut->rNumericArgs(14);
                thisEvapCooler.MaxOATDBEvapCooler = state.dataIPShortCut->rNumericArgs(15);
            } else {
                thisEvapCooler.EvapCoolerOperationControlFlag = false;
            }
        }
        thisEvapCooler.WetbulbEffecCurveIndex = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(3));
        thisEvapCooler.DrybulbEffecCurveIndex = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(4));
        thisEvapCooler.PumpPowerModifierCurveIndex = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(5));
        thisEvapCooler.FanPowerModifierCurveIndex = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(6));

        SetupOutputVariable(state,
                            "Evaporative Cooler Total Stage Effectiveness",
                            OutputProcessor::Unit::None,
                            thisEvapCooler.StageEff,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);
        SetupOutputVariable(state,
                            "Evaporative Cooler Part Load Ratio",
                            OutputProcessor::Unit::None,
                            thisEvapCooler.PartLoadFract,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);

        SetupOutputVariable(state,
                            "Evaporative Cooler Dewpoint Bound Status",
                            OutputProcessor::Unit::None,
                            thisEvapCooler.DewPointBoundFlag,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);
        SetupOutputVariable(state,
                            "Evaporative Cooler Operating Mode Status",
                            OutputProcessor::Unit::None,
                            thisEvapCooler.IECOperatingStatus,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);

    } // end of Indirect Research Special cooler input loop

    cCurrentModuleObject = "EvaporativeCooler:Direct:ResearchSpecial";
    for (int DirectEvapCoolNum = 1; DirectEvapCoolNum <= NumDirectResearchSpecialEvapCool; ++DirectEvapCoolNum) {
        int EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + NumWetInDirectEvapCool + NumRDDEvapCool + DirectEvapCoolNum;
        auto &thisEvapCooler = EvapCond(EvapCoolNum);
        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 cCurrentModuleObject,
                                                                 DirectEvapCoolNum,
                                                                 state.dataIPShortCut->cAlphaArgs,
                                                                 NumAlphas,
                                                                 state.dataIPShortCut->rNumericArgs,
                                                                 NumNums,
                                                                 IOStat,
                                                                 state.dataIPShortCut->lNumericFieldBlanks,
                                                                 state.dataIPShortCut->lAlphaFieldBlanks,
                                                                 state.dataIPShortCut->cAlphaFieldNames,
                                                                 state.dataIPShortCut->cNumericFieldNames);
        GlobalNames::VerifyUniqueInterObjectName(state,
                                                 UniqueEvapCondNames,
                                                 state.dataIPShortCut->cAlphaArgs(1),
                                                 cCurrentModuleObject,
                                                 state.dataIPShortCut->cAlphaFieldNames(1),
                                                 ErrorsFound);
        thisEvapCooler.Name = state.dataIPShortCut->cAlphaArgs(1);
        thisEvapCooler.evapCoolerType = EvapCoolerType::DirectResearchSpecial;

        thisEvapCooler.Schedule = state.dataIPShortCut->cAlphaArgs(2);
        if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
            thisEvapCooler.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
        } else {
            thisEvapCooler.SchedPtr = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
            if (thisEvapCooler.SchedPtr == 0) {
                ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(2) + '=' + state.dataIPShortCut->cAlphaArgs(2));
                ShowContinueError(state, "Entered in " + cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
        }

        thisEvapCooler.InletNode = GetOnlySingleNode(state,
                                                     state.dataIPShortCut->cAlphaArgs(5),
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::EvaporativeCoolerDirectResearchSpecial,
                                                     state.dataIPShortCut->cAlphaArgs(1),
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     DataLoopNode::ObjectIsNotParent);

        thisEvapCooler.OutletNode = GetOnlySingleNode(state,
                                                      state.dataIPShortCut->cAlphaArgs(6),
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::EvaporativeCoolerDirectResearchSpecial,
                                                      state.dataIPShortCut->cAlphaArgs(1),
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      DataLoopNode::ObjectIsNotParent);

        BranchNodeConnections::TestCompSet(state,
                                           cCurrentModuleObject,
                                           state.dataIPShortCut->cAlphaArgs(1),
                                           state.dataIPShortCut->cAlphaArgs(5),
                                           state.dataIPShortCut->cAlphaArgs(6),
                                           "Evap Air Nodes");

        thisEvapCooler.EvapControlNodeNum = GetOnlySingleNode(state,
                                                              state.dataIPShortCut->cAlphaArgs(7),
                                                              ErrorsFound,
                                                              DataLoopNode::ConnectionObjectType::EvaporativeCoolerDirectResearchSpecial,
                                                              state.dataIPShortCut->cAlphaArgs(1),
                                                              DataLoopNode::NodeFluidType::Air,
                                                              DataLoopNode::ConnectionType::Sensor,
                                                              NodeInputManager::CompFluidStream::Primary,
                                                              DataLoopNode::ObjectIsNotParent);

        thisEvapCooler.EvapWaterSupplyName = state.dataIPShortCut->cAlphaArgs(8);

        if (state.dataIPShortCut->lAlphaFieldBlanks(8)) {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromMains;
        } else {
            thisEvapCooler.EvapWaterSupplyMode = WaterSupply::FromTank;
            WaterManager::SetupTankDemandComponent(state,
                                                   thisEvapCooler.Name,
                                                   cCurrentModuleObject,
                                                   thisEvapCooler.EvapWaterSupplyName,
                                                   ErrorsFound,
                                                   thisEvapCooler.EvapWaterSupTankID,
                                                   thisEvapCooler.EvapWaterTankDemandARRID);
        }
        thisEvapCooler.DirectEffectiveness = state.dataIPShortCut->rNumericArgs(1);

        thisEvapCooler.DesVolFlowRate = state.dataIPShortCut->rNumericArgs(2);
        thisEvapCooler.RecircPumpPower = state.dataIPShortCut->rNumericArgs(3);
        thisEvapCooler.RecircPumpSizingFactor = state.dataIPShortCut->rNumericArgs(4);
        if (state.dataIPShortCut->lNumericFieldBlanks(5)) {
            thisEvapCooler.DriftFraction = 0.0;
        } else {
            thisEvapCooler.DriftFraction = state.dataIPShortCut->rNumericArgs(5);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(6)) {
            thisEvapCooler.BlowDownRatio = 0.0;
        } else {
            thisEvapCooler.BlowDownRatio = state.dataIPShortCut->rNumericArgs(6);
        }
        if (state.dataIPShortCut->lNumericFieldBlanks(7) || state.dataIPShortCut->lNumericFieldBlanks(8) ||
            state.dataIPShortCut->lNumericFieldBlanks(9)) {
            thisEvapCooler.EvapCoolerOperationControlFlag = false;
        } else {
            if (!state.dataIPShortCut->lNumericFieldBlanks(7) && !state.dataIPShortCut->lNumericFieldBlanks(8) &&
                !state.dataIPShortCut->lNumericFieldBlanks(9)) {
                thisEvapCooler.EvapCoolerOperationControlFlag = true;
                thisEvapCooler.MinOATDBEvapCooler = state.dataIPShortCut->rNumericArgs(7);
                thisEvapCooler.MaxOATWBEvapCooler = state.dataIPShortCut->rNumericArgs(8);
                thisEvapCooler.MaxOATDBEvapCooler = state.dataIPShortCut->rNumericArgs(9);
            } else {
                thisEvapCooler.EvapCoolerOperationControlFlag = false;
            }
        }
        thisEvapCooler.WetbulbEffecCurveIndex = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(3));
        thisEvapCooler.PumpPowerModifierCurveIndex = CurveManager::GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(4));

        SetupOutputVariable(state,
                            "Evaporative Cooler Stage Effectiveness",
                            OutputProcessor::Unit::None,
                            thisEvapCooler.StageEff,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);
    }

    if (ErrorsFound) {
        ShowFatalError(state, "Errors found in processing input for evaporative coolers");
    }

    for (int EvapCoolNum = 1; EvapCoolNum <= state.dataEvapCoolers->NumEvapCool; ++EvapCoolNum) {
        auto &thisEvapCooler = EvapCond(EvapCoolNum);
        // Setup Report variables for the Evap Coolers
        SetupOutputVariable(state,
                            "Evaporative Cooler Electricity Energy",
                            OutputProcessor::Unit::J,
                            thisEvapCooler.EvapCoolerEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            thisEvapCooler.Name,
                            _,
                            "Electricity",
                            "Cooling",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Evaporative Cooler Electricity Rate",
                            OutputProcessor::Unit::W,
                            thisEvapCooler.EvapCoolerPower,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisEvapCooler.Name);
        // this next report variable is setup differently depending on how the water should be metered here.
        if (thisEvapCooler.EvapWaterSupplyMode == WaterSupply::FromMains) {
            SetupOutputVariable(state,
                                "Evaporative Cooler Water Volume",
                                OutputProcessor::Unit::m3,
                                thisEvapCooler.EvapWaterConsump,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                thisEvapCooler.Name,
                                _,
                                "Water",
                                "Cooling",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Evaporative Cooler Mains Water Volume",
                                OutputProcessor::Unit::m3,
                                thisEvapCooler.EvapWaterConsump,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                thisEvapCooler.Name,
                                _,
                                "MainsWater",
                                "Cooling",
                                _,
                                "System");

        } else if (thisEvapCooler.EvapWaterSupplyMode == WaterSupply::FromTank) {
            SetupOutputVariable(state,
                                "Evaporative Cooler Storage Tank Water Volume",
                                OutputProcessor::Unit::m3,
                                thisEvapCooler.EvapWaterConsump,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                thisEvapCooler.Name,
                                _,
                                "Water",
                                "Cooling",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Evaporative Cooler Starved Water Volume",
                                OutputProcessor::Unit::m3,
                                thisEvapCooler.EvapWaterStarvMakup,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                thisEvapCooler.Name,
                                _,
                                "Water",
                                "Cooling",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Evaporative Cooler Starved Mains Water Volume",
                                OutputProcessor::Unit::m3,
                                thisEvapCooler.EvapWaterStarvMakup,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                thisEvapCooler.Name,
                                _,
                                "MainsWater",
                                "Cooling",
                                _,
                                "System");
        }
    }
}

void InitEvapCooler(EnergyPlusData &state, int const EvapCoolNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   October 2000
    //       MODIFIED       B. Griffith, May 2009, added EMS setpoint check
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for  initializations of the EvapCooler Components.

    // METHODOLOGY EMPLOYED:
    // Uses the status flags to trigger events.

    // Using/Aliasing
    auto &DoSetPointTest = state.dataHVACGlobal->DoSetPointTest;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool localSetPointCheck(false);

    auto &EvapCond(state.dataEvapCoolers->EvapCond);

    // Check that setpoint is active
    if (!state.dataGlobal->SysSizingCalc && state.dataEvapCoolers->MySetPointCheckFlag && DoSetPointTest) {
        for (int EvapUnitNum = 1; EvapUnitNum <= state.dataEvapCoolers->NumEvapCool; ++EvapUnitNum) {

            // only check evap coolers that are supposed to have a control node
            if ((EvapCond(EvapCoolNum).evapCoolerType != EvapCoolerType::IndirectRDDSpecial) &&
                (EvapCond(EvapCoolNum).evapCoolerType != EvapCoolerType::DirectResearchSpecial))
                continue;

            int ControlNode = EvapCond(EvapUnitNum).EvapControlNodeNum;
            if (ControlNode > 0) {
                if (state.dataLoopNodes->Node(ControlNode).TempSetPoint == DataLoopNode::SensedNodeFlagValue) {
                    if (!state.dataGlobal->AnyEnergyManagementSystemInModel) {
                        ShowSevereError(state, "Missing temperature setpoint for Evap Cooler unit " + EvapCond(EvapCoolNum).Name);
                        ShowContinueError(state, " use a Setpoint Manager to establish a setpoint at the unit control node.");
                    } else {
                        localSetPointCheck = false;
                        CheckIfNodeSetPointManagedByEMS(state, ControlNode, EMSManager::SPControlType::TemperatureSetPoint, localSetPointCheck);
                        state.dataLoopNodes->NodeSetpointCheck(ControlNode).needsSetpointChecking = false;
                        // Let it slide apparently
                        if (localSetPointCheck) {
                            ShowSevereError(state, "Missing temperature setpoint for Evap Cooler unit " + EvapCond(EvapCoolNum).Name);
                            ShowContinueError(state, " use a Setpoint Manager to establish a setpoint at the unit control node.");
                            ShowContinueError(state, " or use an EMS actuator to establish a setpoint at the unit control node.");
                        }
                    }
                }
            }
        }
        state.dataEvapCoolers->MySetPointCheckFlag = false;
    }

    auto &thisEvapCond = EvapCond(EvapCoolNum);
    if (!state.dataGlobal->SysSizingCalc && thisEvapCond.MySizeFlag) {
        // for each cooler, do the sizing once.
        SizeEvapCooler(state, EvapCoolNum);

        thisEvapCond.MySizeFlag = false;
    }

    // Do the following initializations (every time step): This should be the info from
    // the previous components outlets or the node data in this section.

    // Transfer the node data to EvapCond data structure
    auto &thisInletNode = state.dataLoopNodes->Node(thisEvapCond.InletNode);

    Real64 const RhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisInletNode.Temp, thisInletNode.HumRat);

    // set the volume flow rates from the input mass flow rates
    thisEvapCond.VolFlowRate = thisInletNode.MassFlowRate / RhoAir;

    // Calculate the entering wet bulb temperature for inlet conditions
    thisEvapCond.InletWetBulbTemp = Psychrometrics::PsyTwbFnTdbWPb(state, thisInletNode.Temp, thisInletNode.HumRat, state.dataEnvrn->OutBaroPress);

    // Set all of the inlet mass flow variables from the nodes
    thisEvapCond.InletMassFlowRate = thisInletNode.MassFlowRate;
    thisEvapCond.InletMassFlowRateMaxAvail = thisInletNode.MassFlowRateMaxAvail;
    thisEvapCond.InletMassFlowRateMinAvail = thisInletNode.MassFlowRateMinAvail;
    // Set all of the inlet state variables from the inlet nodes
    thisEvapCond.InletTemp = thisInletNode.Temp;
    thisEvapCond.InletHumRat = thisInletNode.HumRat;
    thisEvapCond.InletEnthalpy = thisInletNode.Enthalpy;
    thisEvapCond.InletPressure = thisInletNode.Press;
    // Set default outlet state to inlet states(?)
    thisEvapCond.OutletTemp = thisEvapCond.InletTemp;
    thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;
    thisEvapCond.OutletEnthalpy = thisEvapCond.InletEnthalpy;
    thisEvapCond.OutletPressure = thisEvapCond.InletPressure;

    thisEvapCond.OutletMassFlowRate = thisEvapCond.InletMassFlowRate;
    thisEvapCond.OutletMassFlowRateMaxAvail = thisEvapCond.InletMassFlowRateMaxAvail;
    thisEvapCond.OutletMassFlowRateMinAvail = thisEvapCond.InletMassFlowRateMinAvail;

    // Set all of the secondary inlet mass flow variables from the nodes
    if (thisEvapCond.SecondaryInletNode != 0) {
        auto &thisSecInletNode = state.dataLoopNodes->Node(thisEvapCond.SecondaryInletNode);
        thisEvapCond.SecInletMassFlowRate = thisSecInletNode.MassFlowRate;
        thisEvapCond.SecInletMassFlowRateMaxAvail = thisSecInletNode.MassFlowRateMaxAvail;
        thisEvapCond.SecInletMassFlowRateMinAvail = thisSecInletNode.MassFlowRateMinAvail;
        thisEvapCond.SecInletTemp = thisSecInletNode.Temp;
        thisEvapCond.SecInletHumRat = thisSecInletNode.HumRat;
        thisEvapCond.SecInletEnthalpy = thisSecInletNode.Enthalpy;
        thisEvapCond.SecInletPressure = thisSecInletNode.Press;
    } else {
        thisEvapCond.SecInletMassFlowRate = thisEvapCond.IndirectVolFlowRate * state.dataEnvrn->OutAirDensity;
        thisEvapCond.SecInletMassFlowRateMaxAvail = thisEvapCond.IndirectVolFlowRate * state.dataEnvrn->OutAirDensity;
        thisEvapCond.SecInletMassFlowRateMinAvail = 0.0;
        thisEvapCond.SecInletTemp = state.dataEnvrn->OutDryBulbTemp;
        thisEvapCond.SecInletHumRat =
            Psychrometrics::PsyWFnTdbTwbPb(state, state.dataEnvrn->OutDryBulbTemp, state.dataEnvrn->OutWetBulbTemp, state.dataEnvrn->OutBaroPress);
        thisEvapCond.SecInletEnthalpy = state.dataEnvrn->OutEnthalpy;
        thisEvapCond.SecInletPressure = state.dataEnvrn->OutBaroPress;
    }
    // Set the energy consumption to zero each time through for reporting
    thisEvapCond.EvapCoolerEnergy = 0.0;
    thisEvapCond.EvapCoolerPower = 0.0;
    thisEvapCond.DewPointBoundFlag = 0;
    // Set the water consumption to zero each time through for reporting
    thisEvapCond.EvapWaterConsumpRate = 0.0;
    thisEvapCond.EvapWaterConsump = 0.0;
    thisEvapCond.EvapWaterStarvMakup = 0.0;

    // Set the Saturation and Stage Efficiency to zero each time through for reporting
    thisEvapCond.StageEff = 0.0;
    thisEvapCond.SatEff = 0.0;

    // These initializations are done every iteration
    int OutNode = thisEvapCond.OutletNode;
    int ControlNode = thisEvapCond.EvapControlNodeNum;
    thisEvapCond.IECOperatingStatus = 0;

    if (ControlNode == 0) {
        thisEvapCond.DesiredOutletTemp = 0.0;
    } else if (ControlNode == OutNode) {
        thisEvapCond.DesiredOutletTemp = state.dataLoopNodes->Node(ControlNode).TempSetPoint;
    } else {
        thisEvapCond.DesiredOutletTemp = state.dataLoopNodes->Node(ControlNode).TempSetPoint -
                                         (state.dataLoopNodes->Node(ControlNode).Temp - state.dataLoopNodes->Node(OutNode).Temp);
    }
}

void SizeEvapCooler(EnergyPlusData &state, int const EvapCoolNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   March 2009
    //       MODIFIED       March 2014 Daeho Kang, Add sizing additional fields
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Size calculations for Evap coolers
    //  currently just for secondary side of Research Special Indirect evap cooler

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool CoolerOnOApath(false);
    bool CoolerOnMainAirLoop(false);
    int AirSysBranchLoop(0);
    int BranchComp(0);
    bool HardSizeNoDesRun;          // Indicator to a hard-sized field with no design sizing data
    bool IsAutoSize;                // Indicator to autosize
    Real64 IndirectVolFlowRateDes;  // Autosized volume flow rate for reporting
    Real64 IndirectVolFlowRateUser; // Hardsized volume flow rate for reporting
    bool SizingDesRunThisAirSys;    // true if a particular air system had a Sizing:System object and system sizing done
    bool SizingDesRunThisZone;      // true if a particular zone had a Sizing:Zone object and zone sizing was done
    Real64 PadAreaDes;              // Autosized celdek pad area for reporting
    Real64 PadAreaUser;             // Hardsized celdek pad area for reporting
    Real64 PadDepthDes;             // Autosized celdek pad depth for reporting
    Real64 PadDepthUser;            // Hardsized celdek pad depth for reporting

    Real64 volFlowRateDes; // Autosized volume flow rate for reporting
    std::string CompType;  // for ease in getting objects

    // inits
    CoolerOnOApath = false;
    CoolerOnMainAirLoop = false;
    IndirectVolFlowRateDes = 0.0;
    IndirectVolFlowRateUser = 0.0;
    PadAreaDes = 0.0;
    PadAreaUser = 0.0;
    PadDepthDes = 0.0;
    PadDepthUser = 0.0;

    auto &CurSysNum(state.dataSize->CurSysNum);
    auto &CurZoneEqNum(state.dataSize->CurZoneEqNum);
    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &EvapCond(state.dataEvapCoolers->EvapCond);
    auto &thisEvapCond(EvapCond(EvapCoolNum));

    HardSizeNoDesRun = !((state.dataSize->SysSizingRunDone || state.dataSize->ZoneSizingRunDone));

    if (CurSysNum > 0) {
        CheckThisAirSystemForSizing(state, CurSysNum, SizingDesRunThisAirSys);
    } else {
        SizingDesRunThisAirSys = false;
    }
    if (CurZoneEqNum > 0) {
        CheckThisZoneForSizing(state, CurZoneEqNum, SizingDesRunThisZone);
    } else {
        SizingDesRunThisZone = false;
    }
    if (SizingDesRunThisAirSys) {
        HardSizeNoDesRun = false; // Check if design infomation is available
    }

    CompType = evapCoolerTypeNames[static_cast<int>(thisEvapCond.evapCoolerType)];

    // Search once for the object on an air system
    if (CurSysNum > 0) { // central system
        // where is this cooler located, is it on OA system or main loop?
        // search for this component in Air loop branches.
        for (AirSysBranchLoop = 1; AirSysBranchLoop <= state.dataAirSystemsData->PrimaryAirSystems(CurSysNum).NumBranches; ++AirSysBranchLoop) {
            for (BranchComp = 1; BranchComp <= state.dataAirSystemsData->PrimaryAirSystems(CurSysNum).Branch(AirSysBranchLoop).TotalComponents;
                 ++BranchComp) {

                if (UtilityRoutines::SameString(state.dataAirSystemsData->PrimaryAirSystems(CurSysNum).Branch(AirSysBranchLoop).Comp(BranchComp).Name,
                                                thisEvapCond.Name)) {
                    CoolerOnMainAirLoop = true;
                }
            }
        }
        if (!CoolerOnMainAirLoop) CoolerOnOApath = true;
    }

    // Start with the indirect volume flow rate
    IsAutoSize = false;
    if (thisEvapCond.IndirectVolFlowRate == DataSizing::AutoSize) {
        IsAutoSize = true;
    }
    if (CurSysNum > 0 && !IsAutoSize && !SizingDesRunThisAirSys) {
        HardSizeNoDesRun = true;
    }
    if (CurSysNum > 0) { // central system
        if (!IsAutoSize && !SizingDesRunThisAirSys) {
            if (thisEvapCond.IndirectVolFlowRate > 0.0) {
                if (thisEvapCond.evapCoolerType == EvapCoolerType::IndirectCELDEKPAD ||
                    thisEvapCond.evapCoolerType == EvapCoolerType::IndirectWETCOIL ||
                    thisEvapCond.evapCoolerType == EvapCoolerType::IndirectRDDSpecial) {
                    BaseSizer::reportSizerOutput(
                        state, CompType, thisEvapCond.Name, "User-Specified Secondary Fan Flow Rate [m3/s]", thisEvapCond.IndirectVolFlowRate);
                }
            }
        } else { // Autosize or hardsize with design data
            CheckSysSizing(state, CompType, thisEvapCond.Name);
            if (CoolerOnMainAirLoop) {
                IndirectVolFlowRateDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
            } else if (CoolerOnOApath) {
                IndirectVolFlowRateDes = max(FinalSysSizing(CurSysNum).DesOutAirVolFlow, 0.5 * FinalSysSizing(CurSysNum).DesMainVolFlow);
            }
            // apply scaling factor the secondary air fan flow rate
            if (thisEvapCond.evapCoolerType == EvapCoolerType::IndirectRDDSpecial) {
                IndirectVolFlowRateDes = IndirectVolFlowRateDes * thisEvapCond.IndirectVolFlowScalingFactor;
            }
        }
    } else if (CurZoneEqNum > 0) { // zone equipment
        if (!IsAutoSize && !SizingDesRunThisAirSys) {
            if (thisEvapCond.IndirectVolFlowRate > 0.0) {
                // report for the indirect evap cooler types only
                if (thisEvapCond.evapCoolerType == EvapCoolerType::IndirectCELDEKPAD ||
                    thisEvapCond.evapCoolerType == EvapCoolerType::IndirectWETCOIL ||
                    thisEvapCond.evapCoolerType == EvapCoolerType::IndirectRDDSpecial) {
                    BaseSizer::reportSizerOutput(
                        state, CompType, thisEvapCond.Name, "User-Specified Secondary Fan Flow Rate [m3/s]", thisEvapCond.IndirectVolFlowRate);
                }
            }
        } else { // Autosize or hardsize with design data
            // zone equip evap coolers
            IndirectVolFlowRateDes = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
            // apply scaling factor the secondary air fan flow rate
            if (thisEvapCond.evapCoolerType == EvapCoolerType::IndirectRDDSpecial) {
                IndirectVolFlowRateDes = IndirectVolFlowRateDes * thisEvapCond.IndirectVolFlowScalingFactor;
            }
        }

    } else {
    }
    if (!HardSizeNoDesRun) {
        if (IsAutoSize) {
            thisEvapCond.IndirectVolFlowRate = IndirectVolFlowRateDes;
            if (thisEvapCond.evapCoolerType == EvapCoolerType::IndirectCELDEKPAD || thisEvapCond.evapCoolerType == EvapCoolerType::IndirectWETCOIL ||
                thisEvapCond.evapCoolerType == EvapCoolerType::IndirectRDDSpecial) {
                BaseSizer::reportSizerOutput(
                    state, CompType, thisEvapCond.Name, "Design Size Secondary Fan Flow Rate [m3/s]", thisEvapCond.IndirectVolFlowRate);
            }
        } else {
            if (thisEvapCond.IndirectVolFlowRate > 0.0 && IndirectVolFlowRateDes > 0.0) {
                IndirectVolFlowRateUser = thisEvapCond.IndirectVolFlowRate;
                BaseSizer::reportSizerOutput(state,
                                             "EvaporativeCooler:Indirect:ResearchSpecial",
                                             thisEvapCond.Name,
                                             "Design Size Secondary Fan Flow Rate [m3/s]",
                                             IndirectVolFlowRateDes,
                                             "User-Specified Secondary Fan Flow Rate [m3/s]",
                                             IndirectVolFlowRateUser);
                if (state.dataGlobal->DisplayExtraWarnings) {
                    if ((std::abs(IndirectVolFlowRateDes - IndirectVolFlowRateUser) / IndirectVolFlowRateUser) >
                        state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(state,
                                    "SizeEvaporativeCooler:Indirect:ResearchSpecial: Potential issue with equipment sizing for " + thisEvapCond.Name);
                        ShowContinueError(state, format("User-Specified Secondary Fan Flow Rate of {:.5R} [m3/s]", IndirectVolFlowRateUser));
                        ShowContinueError(state, format("differs from Design Size Secondary Fan Flow Rate of {:.5R} [m3/s]", IndirectVolFlowRateDes));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        }
    }

    // Next up the other volume flow rate
    IsAutoSize = false;
    if (thisEvapCond.DesVolFlowRate == DataSizing::AutoSize) {
        IsAutoSize = true;
    }
    if (CurSysNum > 0 && !IsAutoSize && !SizingDesRunThisAirSys) {
        HardSizeNoDesRun = true;
    }
    if (CurSysNum > 0) { // central system
        if (!IsAutoSize && !SizingDesRunThisAirSys) {
            // the .VolFlowRate variable wasn't reported to the eio in develop, so not doing it here
            // if ( EvapCond( EvapCoolNum ).VolFlowRate > 0.0 ) {
            // BaseSizer::reportSizerOutput( CompType, EvapCond( EvapCoolNum ).Name,
            //"User-Specified Secondary Fan Flow Rate [m3/s]", EvapCond( EvapCoolNum ).VolFlowRate );
            //}
        } else { // Autosize or hardsize with design data
            CheckSysSizing(state, CompType, thisEvapCond.Name);
            if (CoolerOnMainAirLoop) {
                volFlowRateDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
            } else if (CoolerOnOApath) {
                volFlowRateDes = max(FinalSysSizing(CurSysNum).DesOutAirVolFlow, 0.5 * FinalSysSizing(CurSysNum).DesMainVolFlow);
            }
            // no scaling factor on the volFlowRate in develop, so not doing it here
        }
    } else if (CurZoneEqNum > 0) { // zone equipment
        // zone equip evap coolers

        if (!IsAutoSize && !SizingDesRunThisAirSys) {
            // the .VolFlowRate variable wasn't reported to the eio in develop, so not doing it here
            // if ( EvapCond( EvapCoolNum ).VolFlowRate > 0.0 ) {
            // BaseSizer::reportSizerOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).Name,
            //"User-Specified Secondary Fan Flow Rate [m3/s]", EvapCond( EvapCoolNum ).VolFlowRate );
            //}
        } else { // Autosize or hardsize with design data
            volFlowRateDes = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
        }

    } else { // zone equipment
             // can't do zone equip evap coolers yet
    }
    if (!HardSizeNoDesRun) {
        if (IsAutoSize) {
            thisEvapCond.DesVolFlowRate = volFlowRateDes;
            // only these two evap coolers has primary air design flow rate
            if (thisEvapCond.evapCoolerType == EvapCoolerType::IndirectRDDSpecial) {
                BaseSizer::reportSizerOutput(state,
                                             "EvaporativeCooler:Indirect:ResearchSpecial",
                                             thisEvapCond.Name,
                                             "Primary Air Design Flow Rate [m3/s]",
                                             thisEvapCond.DesVolFlowRate);
                BaseSizer::reportSizerOutput(state,
                                             "EvaporativeCooler:Indirect:ResearchSpecial",
                                             thisEvapCond.Name,
                                             "Secondary Air Design Flow Rate [m3/s]",
                                             thisEvapCond.IndirectVolFlowRate);
            } else if (thisEvapCond.evapCoolerType == EvapCoolerType::DirectResearchSpecial) {
                BaseSizer::reportSizerOutput(state,
                                             "EvaporativeCooler:Direct:ResearchSpecial",
                                             thisEvapCond.Name,
                                             "Primary Air Design Flow Rate [m3/s]",
                                             thisEvapCond.DesVolFlowRate);
            }
        } else {
            // the .VolFlowRate variable wasn't reported to the eio in develop, so not doing it here
            // if ( EvapCond( EvapCoolNum ).IndirectVolFlowRate > 0.0 && IndirectVolFlowRateDes > 0.0 ) {
            // IndirectVolFlowRateUser = EvapCond( EvapCoolNum ).IndirectVolFlowRate;
            // BaseSizer::reportSizerOutput( "EvaporativeCooler:Indirect:ResearchSpecial", EvapCond( EvapCoolNum ).Name,
            //"Design Size Secondary Fan Flow Rate [m3/s]", IndirectVolFlowRateDes,
            //"User-Specified Secondary Fan Flow Rate [m3/s]", IndirectVolFlowRateUser );
            // if ( DisplayExtraWarnings ) {
            // if ( ( std::abs( IndirectVolFlowRateDes - IndirectVolFlowRateUser ) / IndirectVolFlowRateUser ) > AutoVsHardSizingThreshold ) {
            // ShowMessage(state,  "SizeEvaporativeCooler:Indirect:ResearchSpecial: \nPotential issue with equipment sizing for " + EvapCond(
            // EvapCoolNum
            // ).Name );  ShowContinueError(state,  "User-Specified Secondary Fan Flow Rate of " +  RoundSigDigits(
            // IndirectVolFlowRateUser, 5 ) + " [m3/s]" ); ShowContinueError(state,  format("differs from Design Size Secondary Fan Flow Rate of
            // {:.5R}", IndirectVolFlowRateDes) + " [m3/s]" ); ShowContinueError(state,  "This may, or may not, indicate mismatched component
            // sizes." ); ShowContinueError(state,  "Verify that the value entered is intended and is consistent with other components." );
            //}
            //}
            //}
        }
    }

    if (thisEvapCond.evapCoolerType == EvapCoolerType::DirectCELDEKPAD) {
        IsAutoSize = false;
        if (thisEvapCond.PadArea == DataSizing::AutoSize) {
            IsAutoSize = true;
        }
        if (CurSysNum > 0 && !IsAutoSize && !SizingDesRunThisAirSys) {
            HardSizeNoDesRun = true;
        }
        if (SizingDesRunThisAirSys) HardSizeNoDesRun = false; // Check if design infomation is available
        // Design air flow rate
        if (CurSysNum > 0) { // central system
            if (!IsAutoSize && !SizingDesRunThisAirSys) {
                HardSizeNoDesRun = true;
                if (thisEvapCond.PadArea > 0.0) {
                    BaseSizer::reportSizerOutput(
                        state, "EvaporativeCooler:Direct:CelDekPad", thisEvapCond.Name, "User-Specified Celdek Pad Area [m2]", thisEvapCond.PadArea);
                }
            } else { // Autosize or hardsize with design data
                CheckSysSizing(state, CompType, thisEvapCond.Name);
                if (CoolerOnMainAirLoop) {
                    IndirectVolFlowRateDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                } else if (CoolerOnOApath) {
                    IndirectVolFlowRateDes = std::max(FinalSysSizing(CurSysNum).DesOutAirVolFlow, 0.50 * FinalSysSizing(CurSysNum).DesMainVolFlow);
                }
                // Face air velocity of 3m/s is assumed
                PadAreaDes = IndirectVolFlowRateDes / 3.0;
            }
        } else if (CurZoneEqNum > 0) { // zone equipment
            // zone equip evap coolers
            if (!IsAutoSize && !SizingDesRunThisAirSys) {
                HardSizeNoDesRun = true;
                if (thisEvapCond.PadArea > 0.0) {
                    // report for the indirect evap cooler types only
                    if (thisEvapCond.PadArea > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     "EvaporativeCooler:Direct:CelDekPad",
                                                     thisEvapCond.Name,
                                                     "User-Specified Celdek Pad Area [m2]",
                                                     thisEvapCond.PadArea);
                    }
                }
            } else { // Autosize or hardsize with design data
                // zone equip evap coolers
                IndirectVolFlowRateDes = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                // Face air velocity of 3m/s is assumed
                PadAreaDes = IndirectVolFlowRateDes / 3.0;
            }
        } else {
        }

        if (!HardSizeNoDesRun) {
            if (IsAutoSize) {
                thisEvapCond.PadArea = PadAreaDes;
                BaseSizer::reportSizerOutput(
                    state, "EvaporativeCooler:Direct:CelDekPad", thisEvapCond.Name, "Design Size Celdek Pad Area [m2]", PadAreaDes);
            } else {
                if (thisEvapCond.PadArea > 0.0 && PadAreaDes > 0.0) {
                    PadAreaUser = thisEvapCond.PadArea;
                    BaseSizer::reportSizerOutput(state,
                                                 "EvaporativeCooler:Direct:CelDekPad",
                                                 thisEvapCond.Name,
                                                 "Design Size Celdek Pad Area [m2]",
                                                 PadAreaDes,
                                                 "User-Specified Celdek Pad Area [m2]",
                                                 PadAreaUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(PadAreaDes - PadAreaUser) / PadAreaUser) > state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizeEvaporativeCooler:Direct:CelDekPad: Potential issue with equipment sizing for " + thisEvapCond.Name);
                            ShowContinueError(state, format("User-Specified Celdek Pad Area of{:.2R} [m2]", PadAreaUser));
                            ShowContinueError(state, format("differs from Design Size Celdek Pad Area of {:.2R} [m2]", PadAreaDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }

        IsAutoSize = false;
        if (thisEvapCond.PadDepth == DataSizing::AutoSize) {
            IsAutoSize = true;
        }
        if (CurSysNum > 0 && !IsAutoSize && !SizingDesRunThisAirSys) {
            HardSizeNoDesRun = true;
        }
        // The following regression equation is used to determine pad depth,
        // assuming saturation effectiveness of 70% and face air velocity of 3m/s:
        // Effectiveness = 0.792714 + 0.958569D - 0.25193V - 1.03215D^2 + 0.0262659V^2 + 0.914869DV -
        // 1.48241VD^2 - 0.018992V^3D + 1.13137D^3V + 0.0327622V^3D^2 - 0.145384D^3V^2
        PadDepthDes = 0.17382;
        if (IsAutoSize) {
            thisEvapCond.PadDepth = PadDepthDes;
            BaseSizer::reportSizerOutput(
                state, "EvaporativeCooler:Direct:CelDekPad", thisEvapCond.Name, "Design Size Celdek Pad Depth [m]", PadDepthDes);
        } else {
            if (thisEvapCond.PadDepth > 0.0 && PadDepthDes > 0.0) {
                PadDepthUser = thisEvapCond.PadDepth;
                BaseSizer::reportSizerOutput(state,
                                             "EvaporativeCooler:Direct:CelDekPad",
                                             thisEvapCond.Name,
                                             "Design Size Celdek Pad Depth [m]",
                                             PadDepthDes,
                                             "User-Specified Celdek Pad Depth [m]",
                                             PadDepthUser);
                if (state.dataGlobal->DisplayExtraWarnings) {
                    if ((std::abs(PadDepthDes - PadDepthUser) / PadDepthUser) > state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(state, "SizeEvaporativeCooler:Direct:CelDekPad: Potential issue with equipment sizing for " + thisEvapCond.Name);
                        ShowContinueError(state, format("User-Specified Celdek Pad Depth of {:.2R} [m]", PadDepthUser));
                        ShowContinueError(state, format("differs from Design Size Celdek Pad Depth of {:.2R} [m]", PadDepthDes));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        }
    }

    if (thisEvapCond.evapCoolerType == EvapCoolerType::IndirectCELDEKPAD) {
        IsAutoSize = false;

        if (thisEvapCond.IndirectPadArea == DataSizing::AutoSize) {
            IsAutoSize = true;
        }
        if (SizingDesRunThisAirSys) {
            HardSizeNoDesRun = false; // Check if design infomation is available
        }
        // Design air flow rate
        if (CurSysNum > 0) { // central system
            // where is this cooler located, is it on OA system or main loop?
            // search for this component in Air loop branches.
            for (AirSysBranchLoop = 1; AirSysBranchLoop <= state.dataAirSystemsData->PrimaryAirSystems(CurSysNum).NumBranches; ++AirSysBranchLoop) {
                for (BranchComp = 1; BranchComp <= state.dataAirSystemsData->PrimaryAirSystems(CurSysNum).Branch(AirSysBranchLoop).TotalComponents;
                     ++BranchComp) {
                    if (UtilityRoutines::SameString(
                            state.dataAirSystemsData->PrimaryAirSystems(CurSysNum).Branch(AirSysBranchLoop).Comp(BranchComp).Name,
                            thisEvapCond.Name)) {
                        CoolerOnMainAirLoop = true;
                    }
                }
            }
            if (!IsAutoSize && !SizingDesRunThisAirSys) {
                HardSizeNoDesRun = true;
                if (thisEvapCond.IndirectPadArea > 0.0) {
                    BaseSizer::reportSizerOutput(state,
                                                 "EvaporativeCooler:Indirect:CelDekPad",
                                                 thisEvapCond.Name,
                                                 "User-Specified Celdek Pad Area [m2]",
                                                 thisEvapCond.IndirectPadArea);
                }
            } else { // Autosize or hardsize with design data
                CheckSysSizing(state, CompType, thisEvapCond.Name);
                if (!CoolerOnMainAirLoop) {
                    CoolerOnOApath = true;
                }
                if (CoolerOnMainAirLoop) {
                    IndirectVolFlowRateDes = FinalSysSizing(CurSysNum).DesMainVolFlow;
                } else if (CoolerOnOApath) {
                    IndirectVolFlowRateDes = std::max(FinalSysSizing(CurSysNum).DesOutAirVolFlow, 0.5 * FinalSysSizing(CurSysNum).DesMainVolFlow);
                }
                // Face air velocity of 3m/s is assumed
                PadAreaDes = IndirectVolFlowRateDes / 3.0;
            }
        } else if (CurZoneEqNum > 0) { // zone equipment
            // zone equip evap coolers
            if (!IsAutoSize && !SizingDesRunThisAirSys) {
                HardSizeNoDesRun = true;
                if (thisEvapCond.IndirectPadArea > 0.0) {
                    // report for the indirect evap cooler types only
                    if (thisEvapCond.PadArea > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     "EvaporativeCooler:Indirect:CelDekPad",
                                                     thisEvapCond.Name,
                                                     "User-Specified Celdek Pad Area [m2]",
                                                     thisEvapCond.IndirectPadArea);
                    }
                }
            } else { // Autosize or hardsize with design data
                // zone equip evap coolers
                IndirectVolFlowRateDes = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                // Face air velocity of 3m/s is assumed
                PadAreaDes = IndirectVolFlowRateDes / 3.0;
            }
        } else {
        }

        if (!HardSizeNoDesRun) {
            if (IsAutoSize) {
                thisEvapCond.IndirectPadArea = PadAreaDes;
                BaseSizer::reportSizerOutput(
                    state, "EvaporativeCooler:Indirect:CelDekPad", thisEvapCond.Name, "Design Size Celdek Pad Area [m2]", PadAreaDes);
            } else {
                if (thisEvapCond.IndirectPadArea > 0.0 && PadAreaDes > 0.0) {
                    PadAreaUser = thisEvapCond.IndirectPadArea;
                    BaseSizer::reportSizerOutput(state,
                                                 "EvaporativeCooler:Indirect:CelDekPad",
                                                 thisEvapCond.Name,
                                                 "Design Size Celdek Pad Area [m2]",
                                                 PadAreaDes,
                                                 "User-Specified Celdek Pad Area [m2]",
                                                 PadAreaUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(PadAreaDes - PadAreaUser) / PadAreaUser) > state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizeEvaporativeCooler:Indirect:CelDekPad: Potential issue with equipment sizing for " + thisEvapCond.Name);
                            ShowContinueError(state, format("User-Specified Celdek Pad Area {:.2R} [m2]", PadAreaUser));
                            ShowContinueError(state, format("differs from Design Size Celdek Pad Area of {:.2R} [m2]", PadAreaDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }

        IsAutoSize = thisEvapCond.IndirectPadDepth == DataSizing::AutoSize;
        // The following regression equation is used to determine pad depth,
        // assuming saturation effectiveness of 70% and face air velocity of 3m/s:
        // Effectiveness = 0.792714 + 0.958569D - 0.25193V - 1.03215D^2 + 0.0262659V^2 + 0.914869DV -
        // 1.48241VD^2 - 0.018992V^3D + 1.13137D^3V + 0.0327622V^3D^2 - 0.145384D^3V^2

        PadDepthDes = 0.17382;
        if (IsAutoSize) {
            thisEvapCond.IndirectPadDepth = PadDepthDes;
            BaseSizer::reportSizerOutput(
                state, "EvaporativeCooler:Indirect:CelDekPad", thisEvapCond.Name, "Design Size Celdek Pad Depth [m]", PadDepthDes);
        } else {
            if (thisEvapCond.IndirectPadDepth > 0.0 && PadDepthDes > 0.0) {
                PadDepthUser = thisEvapCond.IndirectPadDepth;
                BaseSizer::reportSizerOutput(state,
                                             "EvaporativeCooler:Indirect:CelDekPad",
                                             thisEvapCond.Name,
                                             "Design Size Celdek Pad Depth [m]",
                                             PadDepthDes,
                                             "User-Specified Celdek Pad Depth [m]",
                                             PadDepthUser);
                if (state.dataGlobal->DisplayExtraWarnings) {
                    if ((std::abs(PadDepthDes - PadDepthUser) / PadDepthUser) > state.dataSize->AutoVsHardSizingThreshold) {
                        ShowMessage(state,
                                    "SizeEvaporativeCooler:Indirect:CelDekPad: Potential issue with equipment sizing for " + thisEvapCond.Name);
                        ShowContinueError(state, format("User-Specified Celdek Pad Depth of {:.2R} [m]", PadDepthUser));
                        ShowContinueError(state, format("differs from Design Size Celdek Pad Depth of {:.2R} [m]", PadDepthDes));
                        ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                        ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                    }
                }
            }
        }
    }

    if (thisEvapCond.evapCoolerType == EvapCoolerType::IndirectRDDSpecial) {
        // secondary air fan sizing: Secondary flow Rate (m3/s) * Fan Flow Sizing Factor (W/(m3/s)
        if (thisEvapCond.IndirectFanPower == DataSizing::AutoSize) {
            thisEvapCond.IndirectFanPower = thisEvapCond.IndirectVolFlowRate * thisEvapCond.FanSizingSpecificPower;
            BaseSizer::reportSizerOutput(
                state, "EvaporativeCooler:Indirect:ResearchSpecial", thisEvapCond.Name, "Secondary Fan Power [W]", thisEvapCond.IndirectFanPower);
        }
        // recirculating water pump sizing: Secondary flow Rate (m3/s) * Pump Sizing Factor (W/(m3/s)
        if (thisEvapCond.IndirectRecircPumpPower == DataSizing::AutoSize) {
            thisEvapCond.IndirectRecircPumpPower = thisEvapCond.IndirectVolFlowRate * thisEvapCond.RecircPumpSizingFactor;
            BaseSizer::reportSizerOutput(state,
                                         "EvaporativeCooler:Indirect:ResearchSpecial",
                                         thisEvapCond.Name,
                                         "Recirculating Pump Power [W]",
                                         thisEvapCond.IndirectRecircPumpPower);
        }
    }

    if (thisEvapCond.evapCoolerType == EvapCoolerType::DirectResearchSpecial) {
        // recirculating water pump sizing: Primary Air Design flow Rate (m3/s) * Pump Sizing Factor (W/(m3/s)
        if (thisEvapCond.RecircPumpPower == DataSizing::AutoSize) {
            thisEvapCond.RecircPumpPower = thisEvapCond.DesVolFlowRate * thisEvapCond.RecircPumpSizingFactor;
            BaseSizer::reportSizerOutput(
                state, "EvaporativeCooler:Direct:ResearchSpecial", thisEvapCond.Name, "Recirculating Pump Power [W]", thisEvapCond.RecircPumpPower);
        }
    }
}

void CalcDirectEvapCooler(EnergyPlusData &state, int EvapCoolNum, Real64 const PartLoadRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   October 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 PadDepth; // EvapCooler Pad Depth in Meters as input by the User
    Real64 SatEff;   // Saturation Efficiency of the CelDek Pad
    Real64 AirVel;   // The Calculated Air Velocity through the Pad
    Real64 TEDB;     // Entering Dry Bulb Temperature
    Real64 TEWB;     // Entering Wet Bulb Temperature
    Real64 RhoWater;

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    // If the Evaporative Cooler  is operating there should be some mass flow rate
    //  Also the evap cooler has to be scheduled to be available
    if ((thisEvapCond.InletMassFlowRate > 0.0) && (ScheduleManager::GetCurrentScheduleValue(state, thisEvapCond.SchedPtr) > 0.0)) {

        PadDepth = thisEvapCond.PadDepth;
        //******************************************************************************
        //   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
        //   FOR A DIRECT EVAPORATIVE AIR COOLER SUPPLIED WITH CFMAir,DIRPAD,TEWB,TEDB,
        //   AND PB (ATM. PRESS.) FOR AIR DENSITY CALCULATIONS.
        //******************************************************************************

        AirVel = thisEvapCond.VolFlowRate / thisEvapCond.PadArea;

        //******************************************************************************
        //   SAT EFF IS FOR DIFFERENT THICKNESS CELDEK PAD (CURVE FIT FROM DATA)
        //******************************************************************************
        SatEff = 0.792714 + 0.958569 * PadDepth - 0.25193 * AirVel - 1.03215 * pow_2(PadDepth) + 2.62659e-2 * pow_2(AirVel) +
                 0.914869 * PadDepth * AirVel - 1.48241 * AirVel * pow_2(PadDepth) - 1.89919e-2 * pow_3(AirVel) * PadDepth +
                 1.13137 * pow_3(PadDepth) * AirVel + 3.27622e-2 * pow_3(AirVel) * pow_2(PadDepth) - 0.145384 * pow_3(PadDepth) * pow_2(AirVel);

        if (SatEff >= 1.0) SatEff = 1.0;
        if (SatEff < 0.0) { // we have a serious problem.  Pad Area and/or depth not suitable for system air flow rates
            ShowSevereError(state, "EVAPCOOLER:DIRECT:CELDEKPAD: " + thisEvapCond.Name + " has a problem");
            ShowContinueError(state, "Check size of Pad Area and/or Pad Depth in input");
            ShowContinueError(state, format("Cooler Effectiveness calculated as: {:.2R}", SatEff));
            ShowContinueError(state, format("Air velocity (m/s) through pads calculated as: {:.2R}", AirVel));
            ShowFatalError(state, "Program Terminates due to previous error condition");
        }
        thisEvapCond.SatEff = SatEff;
        //***************************************************************************
        //   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
        //   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
        //   ACROSS A DIRECT EVAPORATION COOLER.
        TEWB = thisEvapCond.InletWetBulbTemp;
        TEDB = thisEvapCond.InletTemp;

        thisEvapCond.OutletTemp = TEDB - ((TEDB - TEWB) * SatEff);

        thisEvapCond.OuletWetBulbTemp = thisEvapCond.InletWetBulbTemp;

        thisEvapCond.OutletHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, thisEvapCond.OutletTemp, TEWB, state.dataEnvrn->OutBaroPress);

        thisEvapCond.OutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.OutletTemp, thisEvapCond.OutletHumRat);

        //***************************************************************************
        //                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
        // Add the pump energy to the total Evap Cooler energy comsumption
        thisEvapCond.EvapCoolerPower += PartLoadRatio * thisEvapCond.RecircPumpPower;
        //******************
        //             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
        //             H2O [m3/s] = Delta W[kgWater/kDryAir]*Mass Flow Air[kgDryAir/s]
        //                                /RhoWater [kgWater/m3]
        //******************
        RhoWater = Psychrometrics::RhoH2O(thisEvapCond.OutletTemp);
        thisEvapCond.EvapWaterConsumpRate = (thisEvapCond.OutletHumRat - thisEvapCond.InletHumRat) * thisEvapCond.InletMassFlowRate / RhoWater;
        // A numerical check to keep from having very tiny negative water consumption values being reported
        if (thisEvapCond.EvapWaterConsumpRate < 0.0) thisEvapCond.EvapWaterConsumpRate = 0.0;

    } else {
        // The evap cooler is not running and does not change conditions from inlet to outlet
        thisEvapCond.OutletTemp = thisEvapCond.InletTemp;

        thisEvapCond.OuletWetBulbTemp = thisEvapCond.InletWetBulbTemp;

        thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;

        thisEvapCond.OutletEnthalpy = thisEvapCond.InletEnthalpy;

        thisEvapCond.EvapCoolerEnergy = 0.0;

        thisEvapCond.EvapWaterConsumpRate = 0.0;
    }
    // all of the mass flowrates are not changed across the evap cooler
    thisEvapCond.OutletMassFlowRate = thisEvapCond.InletMassFlowRate;
    thisEvapCond.OutletMassFlowRateMaxAvail = thisEvapCond.InletMassFlowRateMaxAvail;
    thisEvapCond.OutletMassFlowRateMinAvail = thisEvapCond.InletMassFlowRateMinAvail;

    // the pressure is not changed across the evap cooler
    thisEvapCond.OutletPressure = thisEvapCond.InletPressure;
}

void CalcDryIndirectEvapCooler(EnergyPlusData &state, int EvapCoolNum, Real64 const PartLoadRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   October 2000
    //       MODIFIED       BG Feb. 2007 secondary air inlet node
    //       RE-ENGINEERED  na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 PadDepth;  // EvapCooler Pad Depth in Meters as input by the User
    Real64 SatEff;    // Saturation Efficiency of the CelDek Pad
    Real64 AirVel;    // The Calculated Air Velocity through the Pad
    Real64 TDBSec;    // Secondary leaving dry bulb
    Real64 TWBSec;    // Secondary Leaving Wet Bulb
    Real64 HumRatSec; // Secondary leaving Humidity Ratio
    Real64 EffHX;     // Effectiveness of Secondary Heat Exchanger
    Real64 QHX;       // Q Across Sec HX
    Real64 RhoWater;
    Real64 RhoAir; // Density of the primary side air
    Real64 CpAir;  // Cp of the primary side air
    Real64 CFMAir;
    Real64 CFMSec;

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    // If the Evaporative Cooler  is operating there should be some mass flow rate
    //  Also the evap cooler has to be scheduled to be available
    if ((thisEvapCond.InletMassFlowRate > 0.0) && (ScheduleManager::GetCurrentScheduleValue(state, thisEvapCond.SchedPtr) > 0.0)) {

        PadDepth = thisEvapCond.IndirectPadDepth;
        //******************************************************************************
        //   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
        //   FOR A DIRECT EVAPORATIVE AIR COOLER SUPPLIED WITH CFMAir,DIRPAD,TEWB,TEDB,
        //   AND PB (ATM. PRESS.) FOR AIR DENSITY CALCULATIONS.
        //******************************************************************************

        AirVel = thisEvapCond.IndirectVolFlowRate / thisEvapCond.IndirectPadArea;

        //******************************************************************************
        //   SAT EFF IS FOR DIFFERENT THICKNESS CELDEK PAD (CURVE FIT FROM DATA)
        //******************************************************************************
        SatEff = 0.792714 + 0.958569 * PadDepth - 0.25193 * AirVel - 1.03215 * pow_2(PadDepth) + 2.62659e-2 * pow_2(AirVel) +
                 0.914869 * PadDepth * AirVel - 1.48241 * AirVel * pow_2(PadDepth) - 1.89919e-2 * pow_3(AirVel) * PadDepth +
                 1.13137 * pow_3(PadDepth) * AirVel + 3.27622e-2 * pow_3(AirVel) * pow_2(PadDepth) - 0.145384 * pow_3(PadDepth) * pow_2(AirVel);

        if (SatEff >= 1.0) SatEff = 1.0;
        thisEvapCond.SatEff = SatEff;
        //***************************************************************************
        //   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
        //   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE PAD BEFORE THE HX.
        //***************************************************************************
        //***** FIRST CHECK IF THIS TEWB IS A FEASIBLE POINT ON PSYCH CHART**********

        // BG Feb 2007 mods for oa node (eg. height-dependent outside air model)
        TWBSec = Psychrometrics::PsyTwbFnTdbWPb(state,
                                                thisEvapCond.SecInletTemp,
                                                thisEvapCond.SecInletHumRat,
                                                thisEvapCond.SecInletPressure); //  OutWetBulbTemp
        TDBSec = thisEvapCond.SecInletTemp - ((thisEvapCond.SecInletTemp - TWBSec) * SatEff);

        HumRatSec = Psychrometrics::PsyWFnTdbTwbPb(state, TDBSec, TWBSec, thisEvapCond.SecInletPressure);

        //***************************************************************************
        //                  CALCULATE THE TLDB FROM HX EQUATIONS GIVEN AN EFFICIENCY
        //***************************************************************************
        EffHX = thisEvapCond.IndirectHXEffectiveness;
        CpAir = Psychrometrics::PsyCpAirFnW(thisEvapCond.InletHumRat);
        RhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisEvapCond.InletTemp, thisEvapCond.InletHumRat);
        CFMAir = thisEvapCond.VolFlowRate;         // Volume Flow Rate Primary Side
        CFMSec = thisEvapCond.IndirectVolFlowRate; // Volume Flolw Rate Secondary Side

        QHX = EffHX * min(CFMSec, CFMAir) * RhoAir * CpAir * (thisEvapCond.InletTemp - TDBSec);
        thisEvapCond.OutletTemp = thisEvapCond.InletTemp - QHX / (RhoAir * CFMAir * CpAir);
        // This is a rough approximation of the Total Indirect Stage Efficiency for the Dry stage which
        //   is a 2 step process the first being teh pad efficiency and then the HX Effectiveness.  I think that
        //   this would mainly be used for evap sizing purposes.
        thisEvapCond.StageEff = SatEff * EffHX;
        //***************************************************************************
        //                  CALCULATE THE WET BULB TEMP in the primary system air USING PSYCH ROUTINES
        // There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
        thisEvapCond.OuletWetBulbTemp =
            Psychrometrics::PsyTwbFnTdbWPb(state, thisEvapCond.OutletTemp, thisEvapCond.InletHumRat, state.dataEnvrn->OutBaroPress);
        //***************************************************************************
        //   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
        //   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
        //   ACROSS A DIRECT EVAPORATION COOLER.

        thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;

        thisEvapCond.OutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.OutletTemp, thisEvapCond.OutletHumRat);

        //***************************************************************************
        //                  POWER OF THE SECONDARY AIR FAN
        if (thisEvapCond.IndirectFanEff > 0.0) {
            thisEvapCond.EvapCoolerPower +=
                PartLoadRatio * thisEvapCond.IndirectFanDeltaPress * thisEvapCond.IndirectVolFlowRate / thisEvapCond.IndirectFanEff;
        }

        //                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
        //                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
        // Add the pump energy to the total Evap Cooler energy comsumption
        thisEvapCond.EvapCoolerPower += PartLoadRatio * thisEvapCond.IndirectRecircPumpPower;

        //******************
        //             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
        //             H2O [m3/s] = Delta W[kgWater/kgDryAir]*Mass Flow Air[kgDryAir/s]
        //                                /RhoWater [kgWater/m3]
        //******************
        RhoWater = Psychrometrics::RhoH2O(TDBSec);
        RhoAir = (Psychrometrics::PsyRhoAirFnPbTdbW(state, thisEvapCond.SecInletPressure, thisEvapCond.SecInletTemp, thisEvapCond.SecInletHumRat) +
                  Psychrometrics::PsyRhoAirFnPbTdbW(state, thisEvapCond.SecInletPressure, TDBSec, HumRatSec)) /
                 2.0;
        thisEvapCond.EvapWaterConsumpRate =
            PartLoadRatio * (HumRatSec - thisEvapCond.SecInletHumRat) * thisEvapCond.IndirectVolFlowRate * RhoAir / RhoWater;
        // A numerical check to keep from having very tiny negative water consumption values being reported
        if (thisEvapCond.EvapWaterConsumpRate < 0.0) thisEvapCond.EvapWaterConsumpRate = 0.0;

    } else {
        // The evap cooler is not running and does not change conditions from inlet to outlet
        thisEvapCond.OutletTemp = thisEvapCond.InletTemp;

        thisEvapCond.OuletWetBulbTemp = thisEvapCond.InletWetBulbTemp;

        thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;

        thisEvapCond.OutletEnthalpy = thisEvapCond.InletEnthalpy;

        thisEvapCond.EvapCoolerEnergy = 0.0;

        thisEvapCond.EvapWaterConsumpRate = 0.0;
    }
    // all of the mass flowrates are not changed across the evap cooler
    thisEvapCond.OutletMassFlowRate = thisEvapCond.InletMassFlowRate;
    thisEvapCond.OutletMassFlowRateMaxAvail = thisEvapCond.InletMassFlowRateMaxAvail;
    thisEvapCond.OutletMassFlowRateMinAvail = thisEvapCond.InletMassFlowRateMinAvail;

    // the pressure is not changed across the evap cooler
    thisEvapCond.OutletPressure = thisEvapCond.InletPressure;
}

void CalcWetIndirectEvapCooler(EnergyPlusData &state, int EvapCoolNum, Real64 const PartLoadRatio)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   October 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  Jan. 2017, Rongpeng Zhang, added fouling fault for evaporative coolers

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 StageEff; // Stage Efficiency of the Heat Exchanger
    Real64 TEDB;     // Entering Dry Bulb Temperature
    Real64 TEWB;     // Entering Wet Bulb Temperature
    Real64 QHX;      // Q Across Sec HX in Watts or J/sec
    Real64 RhoWater;
    Real64 RhoAir; // Density of the primary side air
    Real64 CFMAir;
    Real64 CFMSec;
    Real64 TWBSec; // wet bulb of secondary air

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    // If the Evaporative Cooler  is operating there should be some mass flow rate
    //  Also the evap cooler has to be scheduled to be available
    if ((thisEvapCond.InletMassFlowRate > 0.0) && (ScheduleManager::GetCurrentScheduleValue(state, thisEvapCond.SchedPtr) > 0.0)) {

        //******************************************************************************
        //   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
        //   FOR A WET COIL EVAPORATIVE COOLER
        //******************************************************************************
        //  INDIRECT STAGE EFFICIENCY FOR WET COIL INDIRECT EVAP COOLERS
        CFMAir = thisEvapCond.VolFlowRate;         // Volume Flow Rate Primary Side
        CFMSec = thisEvapCond.IndirectVolFlowRate; // Volume Flolw Rate Secondary Side

        StageEff = thisEvapCond.WetCoilMaxEfficiency - min(thisEvapCond.WetCoilFlowRatio * CFMAir / CFMSec, thisEvapCond.WetCoilMaxEfficiency);

        if (StageEff >= 1.0) StageEff = 1.0;
        // This is a rough approximation of the Total Indirect Stage Efficiency.  I think that
        //   this would mainly be used for evap sizing purposes.

        // If there is a fault of fouling
        if (thisEvapCond.FaultyEvapCoolerFoulingFlag && (!state.dataGlobal->WarmupFlag) && (!state.dataGlobal->DoingSizing) &&
            (!state.dataGlobal->KickOffSimulation)) {
            int FaultIndex = thisEvapCond.FaultyEvapCoolerFoulingIndex;
            Real64 StageEff_ff = StageEff;

            // calculate the Faulty Evaporative Cooler Fouling Factor using fault information
            thisEvapCond.FaultyEvapCoolerFoulingFactor = state.dataFaultsMgr->FaultsEvapCoolerFouling(FaultIndex).CalFoulingFactor(state);

            // update the StageEff at faulty cases
            StageEff = StageEff_ff * thisEvapCond.FaultyEvapCoolerFoulingFactor;
        }

        thisEvapCond.StageEff = StageEff;
        //***************************************************************************
        //   TEMP LEAVING DRY BULB IS CALCULATED FROM A SIMPLE WET BULB APPROACH
        //   MODEL GIVEN THE INDIRECT STAGE EFFICIENCY.
        //   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE INDIRECT STAGE.
        //***************************************************************************
        //                  CALCULATE THE TLDB
        TEWB = thisEvapCond.InletWetBulbTemp;
        TEDB = thisEvapCond.InletTemp;
        TWBSec = Psychrometrics::PsyTwbFnTdbWPb(state, thisEvapCond.SecInletTemp, thisEvapCond.SecInletHumRat, thisEvapCond.SecInletPressure);
        thisEvapCond.OutletTemp = TEDB - StageEff * (TEDB - TWBSec);

        //***************************************************************************
        //                  CALCULATE THE WET BULB TEMP in the primary system air using PSYCH ROUTINES
        // There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
        thisEvapCond.OuletWetBulbTemp =
            Psychrometrics::PsyTwbFnTdbWPb(state, thisEvapCond.OutletTemp, thisEvapCond.InletHumRat, state.dataEnvrn->OutBaroPress);
        //***************************************************************************
        //                  CALCULATE other outlet properties using PSYCH ROUTINES
        thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;

        thisEvapCond.OutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.OutletTemp, thisEvapCond.OutletHumRat);

        //***************************************************************************
        // Real64 FlowFraction = 1.0;
        // Real64 MassFlowRateMax = Node(thisEvapCond.InletNode).MassFlowRateMax;
        // if (MassFlowRateMax > 0) {
        //    FlowFraction = thisEvapCond.InletMassFlowRate / MassFlowRateMax;
        //}
        //                  POWER OF THE SECONDARY AIR FAN
        if (thisEvapCond.IndirectFanEff > 0.0) {
            thisEvapCond.EvapCoolerPower +=
                PartLoadRatio * thisEvapCond.IndirectFanDeltaPress * thisEvapCond.IndirectVolFlowRate / thisEvapCond.IndirectFanEff;
        }

        //                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
        //                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
        // Add the pump energy to the total Evap Cooler energy comsumption
        thisEvapCond.EvapCoolerPower += PartLoadRatio * thisEvapCond.IndirectRecircPumpPower;

        //******************
        //             WATER CONSUMPTION IN m3 OF WATER FOR Wet InDIRECT
        //             H2O [m3/s] = (QHX [J/s])/(2,500,000 [J/kgWater] * RhoWater [kgWater/m3])
        //******************
        //***** FIRST calculate the heat exchange on the primary air side**********
        RhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisEvapCond.InletTemp, thisEvapCond.InletHumRat);
        QHX = PartLoadRatio * CFMAir * RhoAir * (thisEvapCond.InletEnthalpy - thisEvapCond.OutletEnthalpy);

        RhoWater = Psychrometrics::RhoH2O(thisEvapCond.SecInletTemp);
        thisEvapCond.EvapWaterConsumpRate = (QHX / StageEff) / (2500000.0 * RhoWater);
        // A numerical check to keep from having very tiny negative water consumption values being reported
        if (thisEvapCond.EvapWaterConsumpRate < 0.0) thisEvapCond.EvapWaterConsumpRate = 0.0;

    } else {
        // The evap cooler is not running and does not change conditions from inlet to outlet
        thisEvapCond.OutletTemp = thisEvapCond.InletTemp;

        thisEvapCond.OuletWetBulbTemp = thisEvapCond.InletWetBulbTemp;

        thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;

        thisEvapCond.OutletEnthalpy = thisEvapCond.InletEnthalpy;

        thisEvapCond.EvapCoolerEnergy = 0.0;

        thisEvapCond.EvapWaterConsumpRate = 0.0;
    }
    // all of the mass flowrates are not changed across the evap cooler
    thisEvapCond.OutletMassFlowRate = thisEvapCond.InletMassFlowRate;
    thisEvapCond.OutletMassFlowRateMaxAvail = thisEvapCond.InletMassFlowRateMaxAvail;
    thisEvapCond.OutletMassFlowRateMinAvail = thisEvapCond.InletMassFlowRateMinAvail;

    // the pressure is not changed across the evap cooler
    thisEvapCond.OutletPressure = thisEvapCond.InletPressure;
}

void CalcResearchSpecialPartLoad(EnergyPlusData &state, int EvapCoolNum)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // REFERENCES:
    // copied CalcWetIndirectEvapCooler as template for new cooler

    Real64 constexpr MinAirMassFlow(0.001);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 FullOutput(0.0);
    Real64 ReqOutput(0.0);
    int InletNode;
    int OutletNode;
    int ControlNode;
    Real64 PartLoadFrac;
    Real64 DesOutTemp;
    // Set local variables

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));
    auto &Node(state.dataLoopNodes->Node);

    // Retrieve the load on the controlled zone
    OutletNode = thisEvapCond.OutletNode;
    InletNode = thisEvapCond.InletNode;
    ControlNode = thisEvapCond.EvapControlNodeNum;
    DesOutTemp = thisEvapCond.DesiredOutletTemp;
    PartLoadFrac = 0.0;

    // If Evap Cooler runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
    if ((ScheduleManager::GetCurrentScheduleValue(state, thisEvapCond.SchedPtr) > 0.0) && (Node(InletNode).MassFlowRate > MinAirMassFlow) &&
        (Node(InletNode).Temp > Node(ControlNode).TempSetPoint) && (std::abs(Node(InletNode).Temp - DesOutTemp) > DataHVACGlobals::TempControlTol)) {

        // Get full load result, depending on model
        thisEvapCond.PartLoadFract = 1.0;
        switch (thisEvapCond.evapCoolerType) {
        case EvapCoolerType::IndirectRDDSpecial: {
            CalcIndirectResearchSpecialEvapCooler(state, EvapCoolNum);
            UpdateEvapCooler(state, EvapCoolNum);
            FullOutput = Node(InletNode).MassFlowRate * (Psychrometrics::PsyHFnTdbW(Node(OutletNode).Temp, Node(InletNode).HumRat) -
                                                         Psychrometrics::PsyHFnTdbW(Node(InletNode).Temp, Node(InletNode).HumRat));

            ReqOutput = Node(InletNode).MassFlowRate * (Psychrometrics::PsyHFnTdbW(thisEvapCond.DesiredOutletTemp, Node(InletNode).HumRat) -
                                                        Psychrometrics::PsyHFnTdbW(Node(InletNode).Temp, Node(InletNode).HumRat));

            // now reinit after test call
            InitEvapCooler(state, EvapCoolNum);

        } break;
        case EvapCoolerType::DirectResearchSpecial: {
            CalcDirectResearchSpecialEvapCooler(state, EvapCoolNum);
            UpdateEvapCooler(state, EvapCoolNum);
            FullOutput = Node(OutletNode).Temp - Node(InletNode).Temp;
            ReqOutput = thisEvapCond.DesiredOutletTemp - Node(InletNode).Temp;

            // now reinit after test call
            InitEvapCooler(state, EvapCoolNum);

        } break;
        default: {
            assert(false);
        } break;
        }

        // Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
        // Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
        // Calculate the part load fraction
        if (FullOutput == 0.0) {
            FullOutput = 0.00001;
        }
        PartLoadFrac = ReqOutput / FullOutput;
        if (PartLoadFrac > 1.0) {
            PartLoadFrac = 1.0;
        } else if (PartLoadFrac < 0.0) {
            PartLoadFrac = 0.0;
        }

    } else { // No cooling
        PartLoadFrac = 0.0;

    } // End of the cooler running If block
    // Set the final results
    thisEvapCond.PartLoadFract = PartLoadFrac;
}

void CalcIndirectResearchSpecialEvapCooler(EnergyPlusData &state, int const EvapCoolNum, Real64 const FanPLR)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  October 2014, B Nigusse, added dry and wet operating modes
    //                      and secondary air flow control

    // PURPOSE OF THIS SUBROUTINE:
    // Subroutine models a "special" cooler that allows high effectiveness and controls

    // REFERENCES:
    // copied CalcWetIndirectEvapCooler as template for new cooler

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 SecondaryInletDryBulbTemp;  // entering drybulb for secondary/purge side
    Real64 SecondaryInletWetBulbTemp;  // entering wet bulb for secondary/purge side
    Real64 SecondaryInletDewPointTemp; // entering dewpoint for secondary/purge side
    Real64 SecondaryInletHumRatio;     // entering humidity ratio for secondary/purge side
    Real64 StageEff;                   // Stage Efficiency of the Heat Exchanger
    Real64 TEDB;                       // Entering Dry Bulb Temperature
    Real64 TEWB;                       // Entering Wet Bulb Temperature
    Real64 QHX;                        // Q Across Sec HX in Watts or J/sec
    Real64 RhoWater;
    Real64 RhoAir; // Density of the primary side air
    Real64 CFMAir;
    int TertNode;     // inlet node for relief (from bulding) to mix for purge
    Real64 BoundTemp; // temperature limit for outlet
    Real64 PartLoad;
    Real64 TotalVolFlow;
    Real64 TertMdot;
    Real64 TertHumRate;
    Real64 TertTemp;
    Real64 TertRho;
    Real64 TertVdot;
    Real64 SecVdot;
    Real64 SecRho;
    Real64 SecMdot;
    Real64 PurgeMdot;
    Real64 PurgeHumRat;
    Real64 PurgeEnthalpy;
    Real64 PurgeTemp;
    Real64 BlowDownVdot(0.0);
    Real64 DriftVdot(0.0);
    Real64 EvapVdot(0.0);

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    // If the Evaporative Cooler  is operating there should be some mass flow rate
    //  Also the evap cooler has to be scheduled to be available
    if ((thisEvapCond.InletMassFlowRate > 0.0) && (ScheduleManager::GetCurrentScheduleValue(state, thisEvapCond.SchedPtr) > 0.0)) {

        //******************************************************************************
        //   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
        //   FOR A WET COIL EVAPORATIVE COOLER
        //******************************************************************************
        //  INDIRECT STAGE EFFICIENCY FOR WET COIL INDIRECT EVAP COOLERS
        CFMAir = thisEvapCond.VolFlowRate; // Volume Flow Rate Primary Side
        StageEff = thisEvapCond.WetCoilMaxEfficiency;

        // This is model is for special indirect cooler with efficiency greater than 1.0
        if (StageEff >= 1.5) StageEff = 1.5;

        thisEvapCond.StageEff = StageEff;

        //***********************************************
        //  Unit is allowed to mix relief air that would otherwise be exhausted outdoors for ventilation
        //  If tertiary node is set >0 then it assumed that this node is the exhaust out of the building
        //  and the remainder will be made up with outside air from the secondary node
        //*********************************************

        TertNode = thisEvapCond.TertiaryInletNode;
        if (TertNode == 0) {
            SecondaryInletDryBulbTemp = thisEvapCond.SecInletTemp;
            SecondaryInletWetBulbTemp =
                Psychrometrics::PsyTwbFnTdbWPb(state, thisEvapCond.SecInletTemp, thisEvapCond.SecInletHumRat, state.dataEnvrn->OutBaroPress);
            SecondaryInletDewPointTemp =
                Psychrometrics::PsyTdpFnTdbTwbPb(state, thisEvapCond.SecInletTemp, SecondaryInletWetBulbTemp, state.dataEnvrn->OutBaroPress);
            SecondaryInletHumRatio = thisEvapCond.SecInletHumRat;

        } else {

            TotalVolFlow = thisEvapCond.IndirectVolFlowRate;
            TertMdot = state.dataLoopNodes->Node(TertNode).MassFlowRate;
            TertHumRate = state.dataLoopNodes->Node(TertNode).HumRat;
            TertTemp = state.dataLoopNodes->Node(TertNode).Temp;
            // is Node pressure available or better? using outdoor pressure for now
            TertRho = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, TertTemp, TertHumRate);
            TertVdot = TertMdot / TertRho;

            SecVdot = TotalVolFlow - TertVdot;

            if (SecVdot < 0.0) { // all tertiary/releif air e.g. econonizer wide open
                SecVdot = 0.0;
                SecondaryInletDryBulbTemp = TertTemp;
                SecondaryInletWetBulbTemp = Psychrometrics::PsyTwbFnTdbWPb(state, TertTemp, TertHumRate, state.dataEnvrn->OutBaroPress);
                SecondaryInletDewPointTemp =
                    Psychrometrics::PsyTdpFnTdbTwbPb(state, TertTemp, SecondaryInletWetBulbTemp, state.dataEnvrn->OutBaroPress);
                SecondaryInletHumRatio = TertHumRate;
            } else {

                // First determine mass flow of OA,  in secondary
                SecRho =
                    Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisEvapCond.SecInletTemp, thisEvapCond.SecInletHumRat);
                SecMdot = SecRho * SecVdot;
                // Mass balance on moisture to get outlet air humidity ratio
                // this mixing takes place before wet media.
                PurgeMdot = SecMdot + TertMdot;
                PurgeHumRat = (SecMdot * thisEvapCond.SecInletHumRat + TertMdot * TertHumRate) / PurgeMdot;

                // Energy balance to get outlet air enthalpy

                PurgeEnthalpy = (SecMdot * Psychrometrics::PsyHFnTdbW(thisEvapCond.SecInletTemp, thisEvapCond.SecInletHumRat) +
                                 TertMdot * Psychrometrics::PsyHFnTdbW(TertTemp, TertHumRate)) /
                                PurgeMdot;

                // Use Enthalpy and humidity ratio to get outlet temperature from psych chart

                PurgeTemp = Psychrometrics::PsyTdbFnHW(PurgeEnthalpy, PurgeHumRat);
                SecondaryInletDryBulbTemp = PurgeTemp;
                SecondaryInletWetBulbTemp = Psychrometrics::PsyTwbFnTdbWPb(state, PurgeTemp, PurgeHumRat, state.dataEnvrn->OutBaroPress);
                SecondaryInletDewPointTemp =
                    Psychrometrics::PsyTdpFnTdbTwbPb(state, PurgeTemp, SecondaryInletWetBulbTemp, state.dataEnvrn->OutBaroPress);
                SecondaryInletHumRatio = PurgeHumRat;
            }
        }
        if (thisEvapCond.EvapCoolerOperationControlFlag) {
            // addvanced mode: runs either in dry or wet depending on the entering conditions
            CalcIndirectResearchSpecialEvapCoolerAdvanced(
                state, EvapCoolNum, SecondaryInletDryBulbTemp, SecondaryInletWetBulbTemp, SecondaryInletDewPointTemp, SecondaryInletHumRatio);

        } else {

            TEWB = thisEvapCond.InletWetBulbTemp;
            TEDB = thisEvapCond.InletTemp;
            PartLoad = thisEvapCond.PartLoadFract;

            //***************************************************************************
            //   TEMP LEAVING DRY BULB IS CALCULATED FROM A SIMPLE WET BULB APPROACH
            //   MODEL GIVEN THE INDIRECT STAGE EFFICIENCY.
            //   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE INDIRECT STAGE.
            //***************************************************************************
            if (PartLoad == 1.0) {
                //                                 Tout = Tin -  (   0.7    (Tin  - Tpurge,wb,in)
                thisEvapCond.OutletTemp = TEDB - StageEff * (TEDB - SecondaryInletWetBulbTemp);
                //  now bound with secondary dewpoint.
                // unless the resulting Tout<=Tpurge,dp,in ; in which case Tout = Tin - 0.9(Tin-Tpurge,dp,in)

                BoundTemp = TEDB - thisEvapCond.DPBoundFactor * (TEDB - SecondaryInletDewPointTemp);
                if (thisEvapCond.OutletTemp < BoundTemp) {
                    thisEvapCond.OutletTemp = BoundTemp;
                    thisEvapCond.DewPointBoundFlag = 1;
                }
            } else if ((PartLoad < 1.0) && (PartLoad > 0.0)) {
                // assume perfect control Use PLF for energy consumption
                if (thisEvapCond.DesiredOutletTemp < TEDB) {
                    thisEvapCond.OutletTemp = thisEvapCond.DesiredOutletTemp;
                }
            } else {
                // part load set to zero so no cooling
                thisEvapCond.OutletTemp = thisEvapCond.InletTemp;
            }

            //***************************************************************************
            //                  POWER OF THE SECONDARY AIR FAN with part load factor applied (assumes const efficiency)
            thisEvapCond.EvapCoolerPower += thisEvapCond.IndirectVolFlowRate * thisEvapCond.FanSizingSpecificPower * PartLoad * FanPLR;

            //                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
            //                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
            // Add the pump energy to the total Evap Cooler energy comsumption
            thisEvapCond.EvapCoolerPower += thisEvapCond.IndirectRecircPumpPower * PartLoad * FanPLR;

            //***************************************************************************
            //                  CALCULATE THE WET BULB TEMP in the primary system air using PSYCH ROUTINES
            // There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
            thisEvapCond.OuletWetBulbTemp =
                Psychrometrics::PsyTwbFnTdbWPb(state, thisEvapCond.OutletTemp, thisEvapCond.InletHumRat, state.dataEnvrn->OutBaroPress);
            //***************************************************************************
            //                  CALCULATE other outlet propertiesusing PSYCH ROUTINES
            thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;

            thisEvapCond.OutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.OutletTemp, thisEvapCond.OutletHumRat);
            //******************
            //             WATER CONSUMPTION IN m3 OF WATER FOR Wet InDIRECT
            //             H2O [m3/s] = (QHX [J/s])/(2,500,000 [J/kgWater] * RhoWater [kgWater/m3])
            //******************
            //***** FIRST calculate the heat exchange on the primary air side**********
            RhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisEvapCond.InletTemp, thisEvapCond.InletHumRat);
            QHX = CFMAir * RhoAir * (thisEvapCond.InletEnthalpy - thisEvapCond.OutletEnthalpy);

            RhoWater = Psychrometrics::RhoH2O(state.dataEnvrn->OutDryBulbTemp);
            EvapVdot = (QHX) / (2500000.0 * RhoWater);
            DriftVdot = EvapVdot * thisEvapCond.DriftFraction;
            if (thisEvapCond.BlowDownRatio > 0.0) {
                BlowDownVdot = EvapVdot / (thisEvapCond.BlowDownRatio - 1) - DriftVdot;
                if (BlowDownVdot < 0.0) BlowDownVdot = 0.0;
            } else {
                BlowDownVdot = 0.0;
            }
            thisEvapCond.EvapWaterConsumpRate = EvapVdot + DriftVdot + BlowDownVdot;
            // A numerical check to keep from having very tiny negative water consumption values being reported
            if (thisEvapCond.EvapWaterConsumpRate < 0.0) thisEvapCond.EvapWaterConsumpRate = 0.0;
        }

    } else {
        // The evap cooler is not running and does not change conditions from inlet to outlet
        thisEvapCond.OutletTemp = thisEvapCond.InletTemp;
        thisEvapCond.OuletWetBulbTemp = thisEvapCond.InletWetBulbTemp;
        thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;
        thisEvapCond.OutletEnthalpy = thisEvapCond.InletEnthalpy;
        thisEvapCond.EvapCoolerEnergy = 0.0;
        thisEvapCond.EvapCoolerPower = 0.0;
        thisEvapCond.EvapWaterConsumpRate = 0.0;
        thisEvapCond.SecInletMassFlowRate = 0.0;
    }

    // all of the mass flowrates are not changed across the evap cooler
    thisEvapCond.OutletMassFlowRate = thisEvapCond.InletMassFlowRate;
    thisEvapCond.OutletMassFlowRateMaxAvail = thisEvapCond.InletMassFlowRateMaxAvail;
    thisEvapCond.OutletMassFlowRateMinAvail = thisEvapCond.InletMassFlowRateMinAvail;
    // set secondary air side inlet mass flow rate to the outlet node
    thisEvapCond.SecOutletMassFlowRate = thisEvapCond.SecInletMassFlowRate;
    state.dataLoopNodes->Node(thisEvapCond.SecondaryInletNode).MassFlowRate = thisEvapCond.SecInletMassFlowRate;

    // the pressure is not changed across the evap cooler
    thisEvapCond.OutletPressure = thisEvapCond.InletPressure;
}

void CalcIndirectResearchSpecialEvapCoolerAdvanced(EnergyPlusData &state,
                                                   int const EvapCoolNum,
                                                   Real64 const InletDryBulbTempSec,
                                                   Real64 const InletWetBulbTempSec,
                                                   Real64 const InletDewPointTempSec,
                                                   Real64 const InletHumRatioSec)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Bigusse
    //       DATE WRITTEN   October 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Subroutine models indirect evaporative cooler with variable effectiveness for wet and dry
    // operating modes depending on entering conditions

    // SUBROUTINE PARAMETER DEFINITIONS:
    int constexpr MaxIte(500);      // Maximum number of iterations for solver
    Real64 constexpr TempTol(0.01); // convergence tollerance

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 TEDB;      // Entering Dry Bulb Temperature
    Real64 TEWB;      // Entering Wet Bulb Temperature
    Real64 BoundTemp; // temperature limit for outlet
    Real64 PartLoad;
    Real64 SecRho;
    Real64 TdbOutSysWetMin;                 // system( primary ) air drybulb outlet temperature minimum based on wet coil
    Real64 TdbOutSysDryMin;                 // system (primary) air drybulb outlet temperature minimum based on dry coil
    Real64 SysTempSetPoint;                 // evaporative cooler outlet setpoint temperature, drybulb
    Real64 MassFlowRateSecMax;              // Design secondary air mass flow rate
    Real64 AirMassFlowSec;                  // current secondary air mass flow rate
    Real64 AirMassFlowSecDry;               // current secondary air mass flow rate in dry mode
    Real64 AirMassFlowSecWet;               // current secondary air mass flow rate in wet mode
    Real64 FlowRatioSec;                    // secondary air flow ratio in dry and wet mode
    Real64 FlowRatioSecDry;                 // current secondary air mass flow ratio in dry mode
    Real64 FlowRatioSecWet;                 // current secondary air mass flow ratio in wet mode
    Real64 EvapCoolerTotalElectricPowerDry; // evaporative cooler current total electric power drawn
    Real64 EvapCoolerTotalElectricPowerWet; // evaporative cooler current total electric power drawn
    int SolFla;                             // Flag of solver
    std::array<Real64, 6> Par = {0.0};      // Parameter array passed to solver
    Real64 QHXLatent;                       // evaporative cooler latent heat transfer rate
    Real64 hfg;                             // latent heat of vaporization of water at the secondary air inlet condition

    Real64 QHX; // Q Across Sec HX in Watts or J/sec
    Real64 RhoWater;
    Real64 RhoAir; // Density of the primary side air
    Real64 MassFlowRateSecMin;
    Real64 BlowDownVdot(0.0);
    Real64 DriftVdot(0.0);
    Real64 EvapVdot(0.0);

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    FlowRatioSecDry = 0.0;
    FlowRatioSecWet = 0.0;
    thisEvapCond.EvapCoolerRDDOperatingMode = OperatingMode::None;
    TEDB = thisEvapCond.InletTemp;
    TEWB = thisEvapCond.InletWetBulbTemp;
    SysTempSetPoint = thisEvapCond.DesiredOutletTemp;
    SecRho = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, InletDryBulbTempSec, InletHumRatioSec);
    MassFlowRateSecMax = SecRho * thisEvapCond.IndirectVolFlowRate;
    CalcIndirectRDDEvapCoolerOutletTemp(
        state, EvapCoolNum, OperatingMode::WetFull, MassFlowRateSecMax, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec);
    TdbOutSysWetMin = thisEvapCond.OutletTemp;
    CalcIndirectRDDEvapCoolerOutletTemp(
        state, EvapCoolNum, OperatingMode::DryFull, MassFlowRateSecMax, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec);
    TdbOutSysDryMin = thisEvapCond.OutletTemp;

    // get current operating modes of indirect evaporative cooler research special
    thisEvapCond.EvapCoolerRDDOperatingMode = IndirectResearchSpecialEvapCoolerOperatingMode(
        state, EvapCoolNum, InletDryBulbTempSec, InletWetBulbTempSec, TdbOutSysWetMin, TdbOutSysDryMin);

    MassFlowRateSecMin = 0.0;
    AirMassFlowSec = MassFlowRateSecMax;
    PartLoad = thisEvapCond.PartLoadFract;
    {
        if (thisEvapCond.EvapCoolerRDDOperatingMode == OperatingMode::DryModulated) {
            Par[0] = double(EvapCoolNum);
            Par[1] = double(OperatingMode::DryModulated);
            Par[2] = SysTempSetPoint;
            Par[3] = InletDryBulbTempSec;
            Par[4] = InletWetBulbTempSec;
            Par[5] = InletHumRatioSec;
            General::SolveRoot(
                state, TempTol, MaxIte, SolFla, AirMassFlowSec, CalcEvapCoolRDDSecFlowResidual, MassFlowRateSecMin, MassFlowRateSecMax, Par);
            // if the numerical inversion failed, issue error messages.
            if (SolFla == -1) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (thisEvapCond.IterationLimit == 0) {
                        ShowSevereError(state,
                                        "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect "
                                        "Evaporative Cooler Research Special = " +
                                            thisEvapCond.Name);
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state, format("  Iteration limit [{}] exceeded in calculating secondary air mass flow rate", MaxIte));
                        ShowContinueError(state, "  Simulation continues");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        "Secondary air mass flow Iteration limit exceeded in Indirect Evaporative Cooler Research Special = " + thisEvapCond.Name,
                        thisEvapCond.IterationLimit);
                }
            } else if (SolFla == -2) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (thisEvapCond.IterationFailed == 0) {
                        ShowSevereError(state,
                                        "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect "
                                        "Evaporative Cooler Research Special = " +
                                            thisEvapCond.Name);
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state, "...Bad secondary air mass flow rate limits");
                        ShowContinueError(state, format("...Given minimum secondary air mass flow rate={:.3R} kg/s", MassFlowRateSecMin));
                        ShowContinueError(state, format("...Given maximum secondary air mass flow rate={:.3R} kg/s", MassFlowRateSecMax));
                        ShowContinueError(state, " Simulation continues");
                    }
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "Secondary air mass flow control failed in Indirect Evaporative Cooler Research Special = " +
                                                       thisEvapCond.Name,
                                                   thisEvapCond.IterationFailed);
                }
            }
            thisEvapCond.SecInletMassFlowRate = AirMassFlowSec;
            if (AirMassFlowSec > 0.0) {
                if (MassFlowRateSecMax > 0.0) {
                    FlowRatioSec = AirMassFlowSec / MassFlowRateSecMax;
                } else {
                    FlowRatioSec = 0.0;
                }
            } else {
                FlowRatioSec = 0.0;
            }
            thisEvapCond.EvapCoolerPower = IndEvapCoolerPower(state, EvapCoolNum, OperatingMode::DryModulated, FlowRatioSec);
            thisEvapCond.IECOperatingStatus = 1;
        } else if (thisEvapCond.EvapCoolerRDDOperatingMode == OperatingMode::DryFull) {
            CalcIndirectRDDEvapCoolerOutletTemp(
                state, EvapCoolNum, OperatingMode::DryFull, MassFlowRateSecMax, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec);
            thisEvapCond.SecInletMassFlowRate = MassFlowRateSecMax;
            FlowRatioSec = 1.0;
            thisEvapCond.EvapCoolerPower = IndEvapCoolerPower(state, EvapCoolNum, OperatingMode::DryFull, FlowRatioSec);
            thisEvapCond.IECOperatingStatus = 1;
        } else if (thisEvapCond.EvapCoolerRDDOperatingMode == OperatingMode::DryWetModulated) {
            Par[0] = double(EvapCoolNum);
            Par[2] = SysTempSetPoint;
            Par[3] = InletDryBulbTempSec;
            Par[4] = InletWetBulbTempSec;
            Par[5] = InletHumRatioSec;
            // get dry operation performance first
            Par[1] = double(OperatingMode::DryModulated);
            General::SolveRoot(
                state, TempTol, MaxIte, SolFla, AirMassFlowSec, CalcEvapCoolRDDSecFlowResidual, MassFlowRateSecMin, MassFlowRateSecMax, Par);
            // if the numerical inversion failed, issue error messages.
            if (SolFla == -1) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (thisEvapCond.IterationLimit == 0) {
                        ShowSevereError(state,
                                        "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect "
                                        "Evaporative Cooler Research Special = " +
                                            thisEvapCond.Name);
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state, format("  Iteration limit [{}] exceeded in calculating secondary air mass flow rate", MaxIte));
                        ShowContinueError(state, "  Simulation continues");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        "Secondary air mass flow Iteration limit exceeded in Indirect Evaporative Cooler Research Special = " + thisEvapCond.Name,
                        thisEvapCond.IterationLimit);
                }
            } else if (SolFla == -2) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (thisEvapCond.IterationFailed == 0) {
                        ShowSevereError(state,
                                        "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect "
                                        "Evaporative Cooler Research Special = " +
                                            thisEvapCond.Name);
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state, "...Bad secondary air mass flow rate limits");
                        ShowContinueError(state, format("...Given minimum secondary air mass flow rate={:.3R} kg/s", MassFlowRateSecMin));
                        ShowContinueError(state, format("...Given maximum secondary air mass flow rate={:.3R} kg/s", MassFlowRateSecMax));
                        ShowContinueError(state, " Simulation continues");
                    }
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "Secondary air mass flow control failed in Indirect Evaporative Cooler Research Special = " +
                                                       thisEvapCond.Name,
                                                   thisEvapCond.IterationFailed);
                }
            }
            if (AirMassFlowSec > 0.0) {
                if (MassFlowRateSecMax > 0.0) {
                    FlowRatioSec = AirMassFlowSec / MassFlowRateSecMax;
                } else {
                    FlowRatioSec = 0.0;
                }
            } else {
                FlowRatioSec = 0.0;
            }
            FlowRatioSecDry = FlowRatioSec;
            AirMassFlowSecDry = AirMassFlowSec;
            EvapCoolerTotalElectricPowerDry = IndEvapCoolerPower(state, EvapCoolNum, OperatingMode::DryModulated, FlowRatioSecDry);
            // get wet operation performance
            Par[1] = double(OperatingMode::WetModulated);
            General::SolveRoot(
                state, TempTol, MaxIte, SolFla, AirMassFlowSec, CalcEvapCoolRDDSecFlowResidual, MassFlowRateSecMin, MassFlowRateSecMax, Par);
            // if the numerical inversion failed, issue error messages.
            if (SolFla == -1) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (thisEvapCond.IterationLimit == 0) {
                        ShowSevereError(state,
                                        "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect "
                                        "Evaporative Cooler Research Special = " +
                                            thisEvapCond.Name);
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state, format("  Iteration limit [{}] exceeded in calculating secondary air mass flow rate", MaxIte));
                        ShowContinueError(state, "  Simulation continues");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        "Secondary air mass flow Iteration limit exceeded in Indirect Evaporative Cooler Research Special = " + thisEvapCond.Name,
                        thisEvapCond.IterationLimit);
                }
            } else if (SolFla == -2) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (thisEvapCond.IterationFailed == 0) {
                        ShowSevereError(state,
                                        "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect "
                                        "Evaporative Cooler Research Special = " +
                                            thisEvapCond.Name);
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state, "...Bad secondary air mass flow rate limits");
                        ShowContinueError(state, format("...Given minimum secondary air mass flow rate={:.3R} kg/s", MassFlowRateSecMin));
                        ShowContinueError(state, format("...Given maximum secondary air mass flow rate={:.3R} kg/s", MassFlowRateSecMax));
                        ShowContinueError(state, " Simulation continues");
                    }
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "Secondary air mass flow control failed in Indirect Evaporative Cooler Research Special = " +
                                                       thisEvapCond.Name,
                                                   thisEvapCond.IterationFailed);
                }
            }
            if (AirMassFlowSec > 0.0) {
                if (MassFlowRateSecMax > 0.0) {
                    FlowRatioSec = AirMassFlowSec / MassFlowRateSecMax;
                } else {
                    FlowRatioSec = 0.0;
                }
            } else {
                FlowRatioSec = 0.0;
            }
            FlowRatioSecWet = FlowRatioSec;
            AirMassFlowSecWet = AirMassFlowSec;
            EvapCoolerTotalElectricPowerWet = IndEvapCoolerPower(state, EvapCoolNum, OperatingMode::WetModulated, FlowRatioSecWet);
            // compare the dry and wet operation total electric power
            if (EvapCoolerTotalElectricPowerDry < EvapCoolerTotalElectricPowerWet) {
                thisEvapCond.EvapCoolerRDDOperatingMode = OperatingMode::DryModulated;
                FlowRatioSec = FlowRatioSecDry;
                thisEvapCond.SecInletMassFlowRate = AirMassFlowSecDry;
                CalcIndirectRDDEvapCoolerOutletTemp(
                    state, EvapCoolNum, OperatingMode::DryModulated, AirMassFlowSecDry, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec);
                thisEvapCond.EvapCoolerPower = IndEvapCoolerPower(state, EvapCoolNum, OperatingMode::DryModulated, FlowRatioSec);
                thisEvapCond.IECOperatingStatus = 1;
            } else {
                thisEvapCond.EvapCoolerRDDOperatingMode = OperatingMode::WetModulated;
                FlowRatioSec = FlowRatioSecWet;
                thisEvapCond.SecInletMassFlowRate = AirMassFlowSecWet;
                CalcIndirectRDDEvapCoolerOutletTemp(
                    state, EvapCoolNum, OperatingMode::WetModulated, AirMassFlowSecWet, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec);
                thisEvapCond.EvapCoolerPower = IndEvapCoolerPower(state, EvapCoolNum, OperatingMode::WetModulated, FlowRatioSec);
                thisEvapCond.IECOperatingStatus = 2;
            }
        } else if (thisEvapCond.EvapCoolerRDDOperatingMode == OperatingMode::WetModulated) {
            Par[0] = double(EvapCoolNum);
            Par[1] = double(OperatingMode::WetModulated);
            Par[2] = SysTempSetPoint;
            Par[3] = InletDryBulbTempSec;
            Par[4] = InletWetBulbTempSec;
            Par[5] = InletHumRatioSec;
            General::SolveRoot(
                state, TempTol, MaxIte, SolFla, AirMassFlowSec, CalcEvapCoolRDDSecFlowResidual, MassFlowRateSecMin, MassFlowRateSecMax, Par);
            // if the numerical inversion failed, issue error messages.
            if (SolFla == -1) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (thisEvapCond.IterationLimit == 0) {
                        ShowSevereError(state,
                                        "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect "
                                        "Evaporative Cooler Research Special = " +
                                            thisEvapCond.Name);
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state, format("  Iteration limit [{}] exceeded in calculating secondary air mass flow rate", MaxIte));
                        ShowContinueError(state, "  Simulation continues");
                    }
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        "Secondary air mass flow Iteration limit exceeded in Indirect Evaporative Cooler Research Special = " + thisEvapCond.Name,
                        thisEvapCond.IterationLimit);
                }
            } else if (SolFla == -2) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (thisEvapCond.IterationFailed == 0) {
                        ShowSevereError(state,
                                        "CalcIndirectResearchSpecialEvapCooler: calculate secondary air mass flow failed for Indirect "
                                        "Evaporative Cooler Research Special = " +
                                            thisEvapCond.Name);
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state, "...Bad secondary air mass flow rate limits");
                        ShowContinueError(state, format("...Given minimum secondary air mass flow rate={:.3R} kg/s", MassFlowRateSecMin));
                        ShowContinueError(state, format("...Given maximum secondary air mass flow rate={:.3R} kg/s", MassFlowRateSecMax));
                        ShowContinueError(state, " Simulation continues");
                    }
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "Secondary air mass flow control failed in Indirect Evaporative Cooler Research Special = " +
                                                       thisEvapCond.Name,
                                                   thisEvapCond.IterationFailed);
                }
            }
            thisEvapCond.SecInletMassFlowRate = AirMassFlowSec;
            if (AirMassFlowSec > 0.0) {
                if (MassFlowRateSecMax > 0.0) {
                    FlowRatioSec = AirMassFlowSec / MassFlowRateSecMax;
                } else {
                    FlowRatioSec = 0.0;
                }
            } else {
                FlowRatioSec = 0.0;
            }
            thisEvapCond.EvapCoolerPower = IndEvapCoolerPower(state, EvapCoolNum, OperatingMode::WetModulated, FlowRatioSec);
            thisEvapCond.IECOperatingStatus = 2;
        } else if (thisEvapCond.EvapCoolerRDDOperatingMode == OperatingMode::WetFull) {
            CalcIndirectRDDEvapCoolerOutletTemp(
                state, EvapCoolNum, OperatingMode::WetFull, MassFlowRateSecMax, InletDryBulbTempSec, InletWetBulbTempSec, InletHumRatioSec);
            thisEvapCond.SecInletMassFlowRate = MassFlowRateSecMax;
            FlowRatioSec = 1.0;
            thisEvapCond.EvapCoolerPower = IndEvapCoolerPower(state, EvapCoolNum, OperatingMode::WetFull, FlowRatioSec);
            thisEvapCond.IECOperatingStatus = 2;
        }
    }
    if (PartLoad == 1.0) {
        if (thisEvapCond.EvapCoolerRDDOperatingMode == OperatingMode::WetModulated ||
            thisEvapCond.EvapCoolerRDDOperatingMode == OperatingMode::WetFull) {
            BoundTemp = TEDB - thisEvapCond.DPBoundFactor * (TEDB - InletDewPointTempSec);
            if (thisEvapCond.OutletTemp < BoundTemp) {
                thisEvapCond.OutletTemp = BoundTemp;
                thisEvapCond.DewPointBoundFlag = 1;
            }
        }
    } else if ((PartLoad < 1.0) && (PartLoad > 0.0)) {
        // assume perfect control Use PLF for energy consumption
        if (thisEvapCond.DesiredOutletTemp < TEDB) {
            thisEvapCond.OutletTemp = thisEvapCond.DesiredOutletTemp;
        }
    } else {
        // part load set to zero so no cooling
        thisEvapCond.OutletTemp = thisEvapCond.InletTemp;
    }
    if (thisEvapCond.EvapCoolerRDDOperatingMode != OperatingMode::None) {
        // There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
        thisEvapCond.OuletWetBulbTemp =
            Psychrometrics::PsyTwbFnTdbWPb(state, thisEvapCond.OutletTemp, thisEvapCond.InletHumRat, state.dataEnvrn->OutBaroPress);
        thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;
        thisEvapCond.OutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.OutletTemp, thisEvapCond.OutletHumRat);
        RhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisEvapCond.InletTemp, thisEvapCond.InletHumRat);
        QHX = thisEvapCond.VolFlowRate * RhoAir * (thisEvapCond.InletEnthalpy - thisEvapCond.OutletEnthalpy);
        if (QHX > DataHVACGlobals::SmallLoad) {
            // get secondary air outlet condition
            CalcSecondaryAirOutletCondition(state,
                                            EvapCoolNum,
                                            thisEvapCond.EvapCoolerRDDOperatingMode,
                                            thisEvapCond.SecInletMassFlowRate,
                                            InletDryBulbTempSec,
                                            InletWetBulbTempSec,
                                            InletHumRatioSec,
                                            QHX,
                                            QHXLatent);
            RhoWater = Psychrometrics::RhoH2O(state.dataEnvrn->OutDryBulbTemp); // this if it is at the outside air inlet node condition
            hfg = Psychrometrics::PsyHfgAirFnWTdb(InletHumRatioSec, InletDryBulbTempSec);
            EvapVdot = (QHXLatent) / (hfg * RhoWater);
            DriftVdot = EvapVdot * thisEvapCond.DriftFraction;
            if (thisEvapCond.BlowDownRatio > 0.0) {
                BlowDownVdot = EvapVdot / (thisEvapCond.BlowDownRatio - 1) - DriftVdot;
                if (BlowDownVdot < 0.0) BlowDownVdot = 0.0;
            } else {
                BlowDownVdot = 0.0;
            }
            thisEvapCond.EvapWaterConsumpRate = EvapVdot + DriftVdot + BlowDownVdot;
            // A numerical check to keep from having very tiny negative water consumption values being reported
            if (thisEvapCond.EvapWaterConsumpRate < 0.0) thisEvapCond.EvapWaterConsumpRate = 0.0;
        } else {
            thisEvapCond.OutletTemp = thisEvapCond.InletTemp;
            thisEvapCond.OuletWetBulbTemp = thisEvapCond.InletWetBulbTemp;
            thisEvapCond.OutletEnthalpy = thisEvapCond.InletEnthalpy;
            thisEvapCond.EvapCoolerEnergy = 0.0;
            thisEvapCond.EvapCoolerPower = 0.0;
            thisEvapCond.EvapWaterConsumpRate = 0.0;
            thisEvapCond.SecInletMassFlowRate = 0.0;
            thisEvapCond.IECOperatingStatus = 0;
            thisEvapCond.StageEff = 0.0;
            CalcSecondaryAirOutletCondition(state,
                                            EvapCoolNum,
                                            thisEvapCond.EvapCoolerRDDOperatingMode,
                                            0.0,
                                            InletDryBulbTempSec,
                                            InletWetBulbTempSec,
                                            InletHumRatioSec,
                                            QHX,
                                            QHXLatent);
        }

    } else {
        // The evap cooler is not running and does not change conditions from inlet to outlet
        thisEvapCond.OutletTemp = thisEvapCond.InletTemp;
        thisEvapCond.OuletWetBulbTemp = thisEvapCond.InletWetBulbTemp;
        thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;
        thisEvapCond.OutletEnthalpy = thisEvapCond.InletEnthalpy;
        thisEvapCond.SecOutletTemp = thisEvapCond.SecInletTemp;
        thisEvapCond.SecOutletHumRat = thisEvapCond.SecInletHumRat;
        thisEvapCond.SecOutletEnthalpy = thisEvapCond.SecInletEnthalpy;
        thisEvapCond.SecOutletMassFlowRate = thisEvapCond.SecInletMassFlowRate;
        thisEvapCond.EvapCoolerEnergy = 0.0;
        thisEvapCond.EvapCoolerPower = 0.0;
        thisEvapCond.EvapWaterConsumpRate = 0.0;
        thisEvapCond.SecInletMassFlowRate = 0.0;
        thisEvapCond.IECOperatingStatus = 0;
        thisEvapCond.StageEff = 0.0;
    }
}

OperatingMode IndirectResearchSpecialEvapCoolerOperatingMode(EnergyPlusData &state,
                                                             int const EvapCoolNum,
                                                             Real64 const InletDryBulbTempSec,
                                                             Real64 const InletWetBulbTempSec,
                                                             Real64 const TdbOutSysWetMin,
                                                             Real64 const TdbOutSysDryMin)
{

    // PURPOSE OF THIS SUBROUTINE:
    // Determines current operating mode of indirect research special evaporative cooler
    // from the five valid operating modes depending the primary and secondary air
    // temperatures, setpoint temperature, and full capacity air outlet temperature.

    // METHODOLOGY EMPLOYED:
    // compares various temperatures to determine the operating mode

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 InletDryBulbTempPri;  // entering air dry bulb temperature of primary air
    Real64 SysTempSetPoint;      // evaporative cooler outlet setpoint temperature, drybulb
    OperatingMode OperatingMode; // current operating mode of indrect evaporative cooler

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    InletDryBulbTempPri = thisEvapCond.InletTemp;
    SysTempSetPoint = thisEvapCond.DesiredOutletTemp;

    // Now determine the operating modes of indirect evaporative cooler research special. There are five allowed operating modes
    if ((InletDryBulbTempPri <= SysTempSetPoint) ||
        (InletDryBulbTempPri > thisEvapCond.MaxOATDBEvapCooler && InletWetBulbTempSec > thisEvapCond.MaxOATWBEvapCooler) ||
        (InletDryBulbTempPri < InletDryBulbTempSec)) {
        OperatingMode = OperatingMode::None;
    } else if ((InletDryBulbTempSec < thisEvapCond.MinOATDBEvapCooler && TdbOutSysDryMin < SysTempSetPoint)) {
        OperatingMode = OperatingMode::DryModulated; // dry mode capacity modulated
    } else if ((InletDryBulbTempSec < thisEvapCond.MinOATDBEvapCooler && SysTempSetPoint <= TdbOutSysDryMin)) {
        OperatingMode = OperatingMode::DryFull; // dry mode in full capacity
    } else if ((InletDryBulbTempSec >= thisEvapCond.MinOATDBEvapCooler && InletWetBulbTempSec < thisEvapCond.MaxOATWBEvapCooler &&
                SysTempSetPoint <= TdbOutSysWetMin)) {
        OperatingMode = OperatingMode::WetFull; // wet mode in full capacity
    } else if ((InletDryBulbTempSec >= thisEvapCond.MinOATDBEvapCooler && InletWetBulbTempSec < thisEvapCond.MaxOATWBEvapCooler &&
                TdbOutSysWetMin < SysTempSetPoint)) { // && SysTempSetPoint < TdbOutSysDryMin
        OperatingMode = OperatingMode::WetModulated;  // wet mode capacity modulated
    } else if ((InletDryBulbTempSec >= thisEvapCond.MinOATDBEvapCooler && InletDryBulbTempSec < thisEvapCond.MaxOATDBEvapCooler &&
                InletWetBulbTempSec < thisEvapCond.MaxOATWBEvapCooler && SysTempSetPoint < TdbOutSysDryMin && TdbOutSysWetMin < SysTempSetPoint)) {
        OperatingMode = OperatingMode::DryWetModulated; // modulated in dry and wet mode, and the lower total power will be used
    } else {
        OperatingMode = OperatingMode::None; // this condition should not happen unless the bounds do not cover all combinations possible
    }
    return OperatingMode;
}

Real64 CalcEvapCoolRDDSecFlowResidual(EnergyPlusData &state,
                                      Real64 const AirMassFlowSec,     // secondary air mass flow rate in kg/s
                                      std::array<Real64, 6> const &Par // Par(2) is desired outlet temperature of Evap Cooler
)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Nigusse
    //       DATE WRITTEN   Sep 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Indirect research special evaporative cooler part load operation:
    // determines the secondary air flow rate

    // METHODOLOGY EMPLOYED:
    // Uses regula falsi to minimize setpoint temperature residual to by varying the
    // secondary air flow rate.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int EvapCoolIndex;                   // evaporative cooler index
    OperatingMode DryOrWetOperatingMode; // provides index for dry mode and wet mode operation
    Real64 EDBTSecAirSide;               // current entering dry bulb temperature of the secondary side
    Real64 EWBTSecAirSide;               // current entering wet bulb temperature of the secondary side
    Real64 EHumRatSecAirSide;            // current entering humidity ratio of the secondary side
    Real64 OutletAirTemp;                // evap Coler outlet air temperature
    Real64 SysTempSetPoint;              // evaporative cooler outlet setpoint temperature, drybulb
    Real64 Residuum;                     // Residual to be minimized to zero

    auto &EvapCond(state.dataEvapCoolers->EvapCond);

    EvapCoolIndex = int(Par[0]);
    DryOrWetOperatingMode = OperatingMode(int(Par[1]));
    SysTempSetPoint = Par[2];
    EDBTSecAirSide = Par[3];
    EWBTSecAirSide = Par[4];
    EHumRatSecAirSide = Par[5];
    state.dataEvapCoolers->EvapCond(EvapCoolIndex).SecInletMassFlowRate = AirMassFlowSec;
    CalcIndirectRDDEvapCoolerOutletTemp(
        state, EvapCoolIndex, DryOrWetOperatingMode, AirMassFlowSec, EDBTSecAirSide, EWBTSecAirSide, EHumRatSecAirSide);
    OutletAirTemp = EvapCond(EvapCoolIndex).OutletTemp;
    Residuum = SysTempSetPoint - OutletAirTemp;

    return Residuum;
}

void CalcIndirectRDDEvapCoolerOutletTemp(EnergyPlusData &state,
                                         int const EvapCoolNum,
                                         OperatingMode DryOrWetOperatingMode,
                                         Real64 const AirMassFlowSec,
                                         Real64 const EDBTSec,
                                         Real64 const EWBTSec,
                                         Real64 const EHumRatSec)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Nigusse
    //       DATE WRITTEN   Sep 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Indirect research special evaporative cooler perfomance:
    // determines the IEC primary air outlet temperature

    // METHODOLOGY EMPLOYED:
    // Uses effectiveness and energy balance equations to determine
    // primary air outlet temperature.  The dry and wet effectiveness
    // values are used depending on operating modes.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 OutletTemp;       // evaporative cooler current outlet air drybulb temperature
    Real64 RhoAirSec;        // density of secondary air at inlet condition
    Real64 RhoAirSys;        // density of primary air at inlet condition
    Real64 EffectivenessDry; // dry coil effectiveness
    Real64 EffectivenessWet; // wet coil effectiveness
    Real64 FlowRatio;        // flow ratio based on current to the design of secondary air flow rate
    Real64 EffModDryMode;    // dry mode effectiveness modifier for flow ratio
    Real64 EffModWetMode;    // wet mode effectiveness modifier for flow ratio
    Real64 CapFlowSys;       // capacity flow (massFlowRate * Specific Heat) of primary air system
    Real64 CapFlowSec;       // capacity flow (massFlowRate * Specific Heat) of secondary system
    Real64 CpAirSec;         // specific heat of secondary air at inlet condition
    Real64 CpAirSys;         // specific heat of primary air at inlet condition

    Real64 QHXRate;            // total heat transfer rate
    Real64 OutletTempSec;      // secondary air outlet temperature
    Real64 SecOutletAirHumRat; // secondary air humidity ratio at constant temperature (Pure mass transfer)
    Real64 SecOutletEnthalpy;  // secondary air outlet enthalpy

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    if (thisEvapCond.InletMassFlowRate > 0.0) {
        FlowRatio = AirMassFlowSec / thisEvapCond.InletMassFlowRate; // ratio of current secondary air flow to current primary air flow
    } else {
        FlowRatio = 1.0;
    }
    if (AirMassFlowSec > 0.0) {
        RhoAirSec = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, EDBTSec, EHumRatSec);
        RhoAirSys = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisEvapCond.InletTemp, thisEvapCond.InletHumRat);
        if (DryOrWetOperatingMode == OperatingMode::DryModulated || DryOrWetOperatingMode == OperatingMode::DryFull) {
            if (thisEvapCond.DrybulbEffecCurveIndex > 0) {
                EffModDryMode = CurveManager::CurveValue(state, thisEvapCond.DrybulbEffecCurveIndex, FlowRatio);
            } else {
                EffModDryMode = 1.0;
            }
            EffectivenessDry = thisEvapCond.DryCoilMaxEfficiency * EffModDryMode;
            thisEvapCond.StageEff = EffectivenessDry;
            OutletTemp = thisEvapCond.InletTemp - EffectivenessDry * (thisEvapCond.InletTemp - EDBTSec);
            if (OutletTemp > thisEvapCond.InletTemp) {
                OutletTemp = thisEvapCond.InletTemp;
            }
            CpAirSys = Psychrometrics::PsyCpAirFnW(thisEvapCond.InletHumRat);
            CapFlowSys = thisEvapCond.InletMassFlowRate * CpAirSys;
            QHXRate = CapFlowSys * (thisEvapCond.InletTemp - OutletTemp);
            CpAirSec = Psychrometrics::PsyCpAirFnW(EHumRatSec);
            CapFlowSec = AirMassFlowSec * CpAirSec;
            OutletTempSec = EDBTSec + QHXRate / CapFlowSec;
            if (OutletTempSec >= thisEvapCond.InletTemp) {
                OutletTempSec = thisEvapCond.InletTemp - 0.2;
                QHXRate = CapFlowSec * (OutletTempSec - EDBTSec);
                OutletTemp = thisEvapCond.InletTemp - QHXRate / CapFlowSys;
            }
            thisEvapCond.SecOutletTemp = OutletTempSec;
        } else if (DryOrWetOperatingMode == OperatingMode::WetModulated || DryOrWetOperatingMode == OperatingMode::WetFull) {
            if (thisEvapCond.WetbulbEffecCurveIndex > 0) {
                EffModWetMode = CurveManager::CurveValue(state, thisEvapCond.WetbulbEffecCurveIndex, FlowRatio);
            } else {
                EffModWetMode = 1.0;
            }
            EffectivenessWet = thisEvapCond.WetCoilMaxEfficiency * EffModWetMode;
            thisEvapCond.StageEff = EffectivenessWet;
            OutletTemp = thisEvapCond.InletTemp - EffectivenessWet * (thisEvapCond.InletTemp - EWBTSec);
            if (OutletTemp > thisEvapCond.InletTemp) {
                OutletTemp = thisEvapCond.InletTemp;
            }
            CpAirSys = Psychrometrics::PsyCpAirFnW(thisEvapCond.InletHumRat);
            CapFlowSys = thisEvapCond.InletMassFlowRate * CpAirSys;
            QHXRate = CapFlowSys * (thisEvapCond.InletTemp - OutletTemp);
            SecOutletEnthalpy = thisEvapCond.SecInletEnthalpy + QHXRate / AirMassFlowSec;
            SecOutletAirHumRat = Psychrometrics::PsyWFnTdbH(state, EDBTSec, SecOutletEnthalpy); // assumes constant temperature moisture addition
            // we may need check based on maximum allowed humidity ratio
            thisEvapCond.SecOutletTemp = EDBTSec;
            thisEvapCond.SecOutletHumRat = SecOutletAirHumRat;
            thisEvapCond.SecOutletEnthalpy = SecOutletEnthalpy;
        } else {
            OutletTemp = thisEvapCond.InletTemp;
            thisEvapCond.StageEff = 0.0;
        }
    } else {
        OutletTemp = thisEvapCond.InletTemp;
        thisEvapCond.StageEff = 0.0;
    }
    // set results to into output variables
    thisEvapCond.OutletTemp = OutletTemp;
}

void CalcSecondaryAirOutletCondition(EnergyPlusData &state,
                                     int const EvapCoolNum,
                                     OperatingMode OperatingMode,
                                     Real64 const AirMassFlowSec,
                                     Real64 const EDBTSec,
                                     Real64 const EWBTSec,
                                     Real64 const EHumRatSec,
                                     Real64 const QHXTotal,
                                     Real64 &QHXLatent)
{
    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Nigusse
    //       DATE WRITTEN   Oct 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Indirect research special evaporative cooler: determines the secondary air outlet conditions

    // METHODOLOGY EMPLOYED:
    // applies energy balance equations to determine the secondary air outlet condition
    // For wt operations assumes the secondary air leaves at at inlet temperature, i.e.,
    // latent heat transfer only.  For dry operation the humdity ratio remains constant.

    // REFERENCES:
    // CalculateWaterUsage routine of cooling towers for wet operation mode

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 SecOutletAirHumRat; // secondary air humidity ratio at the outlet node
    Real64 SecOutletEnthalpy;  // secondary air outlet enthalpy
    Real64 CpAirSec;           // specific heat of secondary air at inlet condition
    Real64 hfg;                // secondary air side enthaly of evaporation

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    QHXLatent = 0.0;
    if (AirMassFlowSec > 0.0) {
        if ((OperatingMode == OperatingMode::DryModulated || OperatingMode == OperatingMode::DryFull)) {
            thisEvapCond.SecOutletHumRat = EHumRatSec;
            CpAirSec = Psychrometrics::PsyCpAirFnW(EHumRatSec);
            thisEvapCond.SecOutletTemp = EDBTSec + QHXTotal / AirMassFlowSec / CpAirSec;
            thisEvapCond.SecOutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.SecOutletTemp, EHumRatSec);
            thisEvapCond.SecOuletWetBulbTemp =
                Psychrometrics::PsyTwbFnTdbWPb(state, thisEvapCond.SecOutletTemp, EHumRatSec, state.dataEnvrn->OutBaroPress);
        } else if ((OperatingMode == OperatingMode::WetModulated || OperatingMode == OperatingMode::WetFull)) {
            SecOutletEnthalpy = thisEvapCond.SecInletEnthalpy + QHXTotal / AirMassFlowSec;
            SecOutletAirHumRat = Psychrometrics::PsyWFnTdbH(state, EDBTSec, SecOutletEnthalpy); // assumes a constant temperature moisture addition
            thisEvapCond.SecOutletTemp = EDBTSec;
            thisEvapCond.SecOutletHumRat = SecOutletAirHumRat;
            thisEvapCond.SecOutletEnthalpy = SecOutletEnthalpy;
            thisEvapCond.SecOuletWetBulbTemp =
                Psychrometrics::PsyTwbFnTdbWPb(state, thisEvapCond.SecOutletTemp, SecOutletAirHumRat, state.dataEnvrn->OutBaroPress);
            hfg = Psychrometrics::PsyHfgAirFnWTdb(EHumRatSec, EDBTSec);
            QHXLatent = min(QHXTotal, AirMassFlowSec * (SecOutletAirHumRat - EHumRatSec) * hfg);
        } else {
            // set results to into output variables
            thisEvapCond.SecOutletTemp = EDBTSec;
            thisEvapCond.SecOuletWetBulbTemp = EWBTSec;
            thisEvapCond.SecOutletHumRat = EHumRatSec;
            thisEvapCond.SecOutletEnthalpy = thisEvapCond.SecInletEnthalpy;
        }
    } else {
        thisEvapCond.SecOutletTemp = EDBTSec;
        thisEvapCond.SecOuletWetBulbTemp = EWBTSec;
        thisEvapCond.SecOutletHumRat = EHumRatSec;
        thisEvapCond.SecOutletEnthalpy = thisEvapCond.SecInletEnthalpy;
    }
}

Real64 IndEvapCoolerPower(EnergyPlusData &state,
                          int const EvapCoolIndex,  // Unit index
                          OperatingMode DryWetMode, // dry or wet operating mode of evaporator cooler
                          Real64 const FlowRatio    // secondary air flow fraction
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Nigusse
    //       DATE WRITTEN   Sep 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the Indirect Evaporative Cooler Total Electric Power

    // METHODOLOGY EMPLOYED:
    // Scales the design fan and pump power depending on secondary air flow fraction
    // and sums the two to determine the evaporative cooler total electric power.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 FanPowerModCurveValue;  // fan power modifier curve value
    Real64 PumpPowerModCurveValue; // fan power modifier curve value
    Real64 EvapCoolertotalPower;   // current evaporative cooler total electric power

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolIndex));

    EvapCoolertotalPower = 0.0;
    if (FlowRatio > 0.0) {
        if (thisEvapCond.FanPowerModifierCurveIndex > 0) {
            FanPowerModCurveValue = CurveManager::CurveValue(state, thisEvapCond.FanPowerModifierCurveIndex, FlowRatio);
        } else {
            FanPowerModCurveValue = thisEvapCond.PartLoadFract * FlowRatio;
        }
        EvapCoolertotalPower += thisEvapCond.IndirectFanPower * FanPowerModCurveValue;
        if (DryWetMode == OperatingMode::WetModulated || DryWetMode == OperatingMode::WetFull) {
            // Add the pump power to the total Evap Cooler power for wet operating mode
            if (thisEvapCond.PumpPowerModifierCurveIndex > 0) {
                PumpPowerModCurveValue = CurveManager::CurveValue(state, thisEvapCond.PumpPowerModifierCurveIndex, FlowRatio);
            } else {
                // linearly scale pump power using part-load-fraction when pump power modifier curve is not specified
                PumpPowerModCurveValue = thisEvapCond.PartLoadFract * FlowRatio;
            }
            EvapCoolertotalPower += thisEvapCond.IndirectRecircPumpPower * PumpPowerModCurveValue;
        }
    } else {
        EvapCoolertotalPower = 0.0;
    }
    return EvapCoolertotalPower;
}

void CalcDirectResearchSpecialEvapCooler(EnergyPlusData &state, int const EvapCoolNum, Real64 const FanPLR)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   March 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // calculate model for direct evaporative cooler that is simple and controllable

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 SatEff; // Saturation Efficiency of the CelDek Pad
    Real64 TEDB;   // Entering Dry Bulb Temperature
    Real64 TEWB;   // Entering Wet Bulb Temperature
    Real64 RhoWater;
    Real64 PartLoad;
    Real64 EffModCurveValue;       // effectiveness modifier curve value
    Real64 PumpPowerModCurveValue; // recirculation pump power modifier curve value
    Real64 FlowRatio(0);           // primary air flow frcation (current flow divided by the design flow rate)
    Real64 MassFlowRateSysDesign;  // primary air design mass flow rate
    Real64 MassFlowRateSys;        // primary air current mass flow rate
    int InletNode;                 // inlet node number
    Real64 BlowDownVdot(0.0);
    Real64 DriftVdot(0.0);
    Real64 EvapVdot(0.0);
    bool EvapCoolerOperatingLimitFlag(false);

    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    EvapCoolerOperatingLimitFlag = false;
    TEDB = thisEvapCond.InletTemp;
    TEWB = thisEvapCond.InletWetBulbTemp;
    if (thisEvapCond.EvapCoolerOperationControlFlag) {
        if (TEDB >= thisEvapCond.MinOATDBEvapCooler && (TEWB <= thisEvapCond.MaxOATWBEvapCooler || TEDB <= thisEvapCond.MaxOATDBEvapCooler)) {
            EvapCoolerOperatingLimitFlag = true;
        }
    } else {
        EvapCoolerOperatingLimitFlag = true;
    }

    // If the Evaporative Cooler  is operating there should be some mass flow rate
    //  Also the evap cooler has to be scheduled to be available
    if ((thisEvapCond.InletMassFlowRate > 0.0) && (ScheduleManager::GetCurrentScheduleValue(state, thisEvapCond.SchedPtr) > 0.0) &&
        EvapCoolerOperatingLimitFlag) {

        //***************************************************************************
        //   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
        //   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
        //   ACROSS A DIRECT EVAPORATION COOLER.
        TEWB = thisEvapCond.InletWetBulbTemp;
        TEDB = thisEvapCond.InletTemp;
        InletNode = thisEvapCond.InletNode;

        MassFlowRateSys = thisEvapCond.InletMassFlowRate;
        MassFlowRateSysDesign = state.dataLoopNodes->Node(InletNode).MassFlowRateMax;
        if (MassFlowRateSysDesign > 0.0) {
            if (MassFlowRateSys > 0.0) {
                FlowRatio = MassFlowRateSys / MassFlowRateSysDesign;
            } else {
                FlowRatio = 1.0;
            }
        }
        if (thisEvapCond.WetbulbEffecCurveIndex > 0) {
            EffModCurveValue = CurveManager::CurveValue(state, thisEvapCond.WetbulbEffecCurveIndex, FlowRatio);
        } else {
            // if no curve specified assume constant effectiveness
            EffModCurveValue = 1.0;
        }
        SatEff = thisEvapCond.DirectEffectiveness * EffModCurveValue;
        thisEvapCond.StageEff = SatEff;
        PartLoad = thisEvapCond.PartLoadFract;
        if (PartLoad == 1.0) {
            thisEvapCond.OutletTemp = TEDB - ((TEDB - TEWB) * SatEff);
            thisEvapCond.OuletWetBulbTemp = TEWB;
            thisEvapCond.OutletHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, thisEvapCond.OutletTemp, TEWB, state.dataEnvrn->OutBaroPress);
            thisEvapCond.OutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.OutletTemp, thisEvapCond.OutletHumRat);
        } else if ((PartLoad < 1.0) && (PartLoad > 0.0)) {
            // assume perfect control Use PLF for energy consumption
            if (thisEvapCond.DesiredOutletTemp < TEDB) {
                thisEvapCond.OutletTemp = thisEvapCond.DesiredOutletTemp;
                thisEvapCond.OuletWetBulbTemp = TEWB;
                thisEvapCond.OutletHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, thisEvapCond.OutletTemp, TEWB, state.dataEnvrn->OutBaroPress);

                thisEvapCond.OutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.OutletTemp, thisEvapCond.OutletHumRat);
            } else { // do no cooling
                thisEvapCond.OutletTemp = TEDB;
                thisEvapCond.OuletWetBulbTemp = TEWB;
                thisEvapCond.OutletHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, thisEvapCond.OutletTemp, TEWB, state.dataEnvrn->OutBaroPress);
                thisEvapCond.OutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.OutletTemp, thisEvapCond.OutletHumRat);
            }
        } else {
            // part load set to zero so no cooling
            thisEvapCond.OutletTemp = TEDB;
            thisEvapCond.OuletWetBulbTemp = TEWB;
            thisEvapCond.OutletHumRat = Psychrometrics::PsyWFnTdbTwbPb(state, thisEvapCond.OutletTemp, TEWB, state.dataEnvrn->OutBaroPress);
            thisEvapCond.OutletEnthalpy = Psychrometrics::PsyHFnTdbW(thisEvapCond.OutletTemp, thisEvapCond.OutletHumRat);
        }

        //***************************************************************************
        //                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
        // Add the pump energy to the total Evap Cooler energy comsumption
        if (thisEvapCond.PumpPowerModifierCurveIndex > 0) {
            PumpPowerModCurveValue = CurveManager::CurveValue(state, thisEvapCond.PumpPowerModifierCurveIndex, FlowRatio);
        } else {
            // if no pump power modifier curve specified, then assume linear variation with part-load and primary fan PLR
            PumpPowerModCurveValue = PartLoad * FanPLR;
        }
        thisEvapCond.EvapCoolerPower = thisEvapCond.RecircPumpPower * PumpPowerModCurveValue;
        //******************
        //             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
        //             H2O [m3/s] = Delta W[kgWater/kgDryAir]*Mass Flow Air[kgDryAir]
        //                                /RhoWater [kgWater/m3]
        //******************
        RhoWater = Psychrometrics::RhoH2O(thisEvapCond.OutletTemp);
        EvapVdot = (thisEvapCond.OutletHumRat - thisEvapCond.InletHumRat) * thisEvapCond.InletMassFlowRate / RhoWater;
        DriftVdot = EvapVdot * thisEvapCond.DriftFraction;

        if (thisEvapCond.BlowDownRatio > 0.0) {
            BlowDownVdot = EvapVdot / (thisEvapCond.BlowDownRatio - 1.0) - DriftVdot;
            if (BlowDownVdot < 0.0) BlowDownVdot = 0.0;
        } else {
            BlowDownVdot = 0.0;
        }

        thisEvapCond.EvapWaterConsumpRate = EvapVdot + DriftVdot + BlowDownVdot;

        // A numerical check to keep from having very tiny negative water consumption values being reported
        if (thisEvapCond.EvapWaterConsumpRate < 0.0) thisEvapCond.EvapWaterConsumpRate = 0.0;

    } else {
        // The evap cooler is not running and does not change conditions from inlet to outlet
        thisEvapCond.OutletTemp = thisEvapCond.InletTemp;

        thisEvapCond.OuletWetBulbTemp = thisEvapCond.InletWetBulbTemp;

        thisEvapCond.OutletHumRat = thisEvapCond.InletHumRat;

        thisEvapCond.OutletEnthalpy = thisEvapCond.InletEnthalpy;
        thisEvapCond.EvapCoolerPower = 0.0;
        thisEvapCond.EvapCoolerEnergy = 0.0;

        thisEvapCond.EvapWaterConsumpRate = 0.0;
    }
    // all of the mass flowrates are not changed across the evap cooler
    thisEvapCond.OutletMassFlowRate = thisEvapCond.InletMassFlowRate;
    thisEvapCond.OutletMassFlowRateMaxAvail = thisEvapCond.InletMassFlowRateMaxAvail;
    thisEvapCond.OutletMassFlowRateMinAvail = thisEvapCond.InletMassFlowRateMinAvail;

    // the pressure is not changed across the evap cooler
    thisEvapCond.OutletPressure = thisEvapCond.InletPressure;
}

void UpdateEvapCooler(EnergyPlusData &state, int const EvapCoolNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   October 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    auto &thisEvapCond = state.dataEvapCoolers->EvapCond(EvapCoolNum);
    auto &thisOutletNode = state.dataLoopNodes->Node(thisEvapCond.OutletNode);
    auto &thisInletNode = state.dataLoopNodes->Node(thisEvapCond.InletNode);

    // Set the outlet air nodes of the EvapCooler
    thisOutletNode.MassFlowRate = thisEvapCond.OutletMassFlowRate;
    thisOutletNode.MassFlowRateMaxAvail = thisEvapCond.OutletMassFlowRateMaxAvail;
    thisOutletNode.MassFlowRateMinAvail = thisEvapCond.OutletMassFlowRateMinAvail;
    thisOutletNode.Temp = thisEvapCond.OutletTemp;
    thisOutletNode.HumRat = thisEvapCond.OutletHumRat;
    thisOutletNode.Enthalpy = thisEvapCond.OutletEnthalpy;
    thisOutletNode.Press = thisEvapCond.OutletPressure;

    if (thisEvapCond.SecondaryOutletNode > 0) {
        auto &thisOutletNodeSec = state.dataLoopNodes->Node(thisEvapCond.SecondaryOutletNode);
        // set outlet nodes of the secondary air side of the EvapCooler (mass Flow Rate Only)
        if (thisEvapCond.evapCoolerType == EvapCoolerType::IndirectRDDSpecial && thisEvapCond.EvapCoolerOperationControlFlag) {
            thisOutletNodeSec.Temp = thisEvapCond.SecOutletTemp;
            thisOutletNodeSec.HumRat = thisEvapCond.SecOutletHumRat;
            thisOutletNodeSec.Enthalpy = thisEvapCond.SecOutletEnthalpy;
            thisOutletNodeSec.MassFlowRate = thisEvapCond.SecOutletMassFlowRate;
        }
    }

    // Set the outlet nodes for properties that just pass through & not used
    thisOutletNode.Quality = thisInletNode.Quality;

    // Set the demand request for supply water from water storage tank (if needed)
    if (thisEvapCond.EvapWaterSupplyMode == WaterSupply::FromTank) {
        state.dataWaterData->WaterStorage(thisEvapCond.EvapWaterSupTankID).VdotRequestDemand(thisEvapCond.EvapWaterTankDemandARRID) =
            thisEvapCond.EvapWaterConsumpRate;
    }

    // check if should be starved by restricted flow from tank
    if (thisEvapCond.EvapWaterSupplyMode == WaterSupply::FromTank) {
        Real64 AvailWaterRate =
            state.dataWaterData->WaterStorage(thisEvapCond.EvapWaterSupTankID).VdotAvailDemand(thisEvapCond.EvapWaterTankDemandARRID);
        if (AvailWaterRate < thisEvapCond.EvapWaterConsumpRate) {
            thisEvapCond.EvapWaterStarvMakupRate = thisEvapCond.EvapWaterConsumpRate - AvailWaterRate;
            thisEvapCond.EvapWaterConsumpRate = AvailWaterRate;
        } else {
            thisEvapCond.EvapWaterStarvMakupRate = 0.0;
        }
    }

    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        thisOutletNode.CO2 = thisInletNode.CO2;
    }

    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        thisOutletNode.GenContam = thisInletNode.GenContam;
    }
}

void ReportEvapCooler(EnergyPlusData &state, int const EvapCoolNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   Oct 2000
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;
    auto &thisEvapCond(state.dataEvapCoolers->EvapCond(EvapCoolNum));

    // report the Evap Cooler energy from this component
    thisEvapCond.EvapCoolerPower = thisEvapCond.EvapCoolerPower;
    thisEvapCond.EvapCoolerEnergy = thisEvapCond.EvapCoolerPower * TimeStepSys * DataGlobalConstants::SecInHour;

    // Report Water comsumption in cubic meters per timestep
    thisEvapCond.EvapWaterConsump = thisEvapCond.EvapWaterConsumpRate * TimeStepSys * DataGlobalConstants::SecInHour;
    thisEvapCond.EvapWaterStarvMakup = thisEvapCond.EvapWaterStarvMakupRate * TimeStepSys * DataGlobalConstants::SecInHour;
}

void SimZoneEvaporativeCoolerUnit(EnergyPlusData &state,
                                  std::string_view CompName,      // name of the packaged terminal heat pump
                                  int const ZoneNum,              // number of zone being served
                                  Real64 &SensibleOutputProvided, // sensible capacity delivered to zone
                                  Real64 &LatentOutputProvided,   // Latent add/removal  (kg/s), dehumid = negative
                                  int &CompIndex                  // index to zone hvac unit
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // public simulation routine for managing zone hvac evaporative cooler unit

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int CompNum;

    auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);

    if (state.dataEvapCoolers->GetInputZoneEvapUnit) {
        GetInputZoneEvaporativeCoolerUnit(state);
        state.dataEvapCoolers->GetInputZoneEvapUnit = false;
    }

    // Find the correct Equipment
    if (CompIndex == 0) {
        CompNum = UtilityRoutines::FindItemInList(CompName, ZoneEvapUnit);
        if (CompNum == 0) {
            ShowFatalError(state, "SimZoneEvaporativeCoolerUnit: Zone evaporative cooler unit not found.");
        }
        CompIndex = CompNum;
    } else {
        CompNum = CompIndex;
        if (CompNum < 1 || CompNum > state.dataEvapCoolers->NumZoneEvapUnits) {
            ShowFatalError(state,
                           format("SimZoneEvaporativeCoolerUnit: Invalid CompIndex passed={}, Number of units ={}, Entered Unit name = {}",
                                  CompNum,
                                  state.dataEvapCoolers->NumZoneEvapUnits,
                                  CompName));
        }
        if (state.dataEvapCoolers->CheckZoneEvapUnitName(CompNum)) {
            if (CompName != ZoneEvapUnit(CompNum).Name) {
                ShowFatalError(state,
                               format("SimZoneEvaporativeCoolerUnit: Invalid CompIndex passed={}, Unit name={}, stored unit name for that index={}",
                                      CompNum,
                                      CompName,
                                      ZoneEvapUnit(CompNum).Name));
            }
            state.dataEvapCoolers->CheckZoneEvapUnitName(CompNum) = false;
        }
    }

    InitZoneEvaporativeCoolerUnit(state, CompNum, ZoneNum);

    CalcZoneEvaporativeCoolerUnit(state, CompNum, ZoneNum, SensibleOutputProvided, LatentOutputProvided);

    ReportZoneEvaporativeCoolerUnit(state, CompNum);
}

void GetInputZoneEvaporativeCoolerUnit(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // get input for zone evap cooler unit

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetInputZoneEvaporativeCoolerUnit: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string CurrentModuleObject; // Object type for getting and error messages
    Array1D_string Alphas;           // Alpha items for object
    Array1D<Real64> Numbers;         // Numeric items for object
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_string cNumericFields;   // Numeric field names
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    Array1D_bool lNumericBlanks;     // Logical array, numeric field input BLANK = .TRUE.
    int NumAlphas;                   // Number of Alphas for each GetObjectItem call
    int NumNumbers;                  // Number of Numbers for each GetObjectItem call
    int MaxAlphas;                   // Maximum number of alpha fields in all objects
    int MaxNumbers;                  // Maximum number of numeric fields in all objects
    int NumFields;                   // Total number of fields in object
    int IOStatus;                    // Used in GetObjectItem
    bool ErrorsFound(false);         // Set to true if errors in input, fatal at end of routine
    bool errFlag;
    Real64 FanVolFlow;
    int UnitLoop;
    int CtrlZone; // index to loop counter
    int NodeNum;  // index to loop counter

    auto &EvapCond(state.dataEvapCoolers->EvapCond);
    auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);

    if (state.dataEvapCoolers->GetInputEvapComponentsFlag) {
        GetEvapInput(state);
        state.dataEvapCoolers->GetInputEvapComponentsFlag = false;
    }

    state.dataEvapCoolers->GetInputZoneEvapUnit = false;
    MaxNumbers = 0;
    MaxAlphas = 0;

    CurrentModuleObject = "ZoneHVAC:EvaporativeCoolerUnit";
    state.dataEvapCoolers->NumZoneEvapUnits = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NumFields, NumAlphas, NumNumbers);
    MaxNumbers = max(MaxNumbers, NumNumbers);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    Alphas.allocate(MaxAlphas);
    Numbers.dimension(MaxNumbers, 0.0);
    cAlphaFields.allocate(MaxAlphas);
    cNumericFields.allocate(MaxNumbers);
    lAlphaBlanks.dimension(MaxAlphas, true);
    lNumericBlanks.dimension(MaxNumbers, true);

    if (state.dataEvapCoolers->NumZoneEvapUnits > 0) {
        state.dataEvapCoolers->CheckZoneEvapUnitName.dimension(state.dataEvapCoolers->NumZoneEvapUnits, true);
        ZoneEvapUnit.allocate(state.dataEvapCoolers->NumZoneEvapUnits);

        for (UnitLoop = 1; UnitLoop <= state.dataEvapCoolers->NumZoneEvapUnits; ++UnitLoop) {
            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     CurrentModuleObject,
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

            UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);

            auto &thisZoneEvapUnit = ZoneEvapUnit(UnitLoop);
            thisZoneEvapUnit.Name = Alphas(1);
            if (lAlphaBlanks(2)) {
                thisZoneEvapUnit.AvailSchedIndex = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                thisZoneEvapUnit.AvailSchedIndex =
                    ScheduleManager::GetScheduleIndex(state, Alphas(2)); // convert schedule name to pointer (index number)
                if (thisZoneEvapUnit.AvailSchedIndex == 0) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                    ShowContinueError(state, "invalid-not found " + cAlphaFields(2) + "=\"" + Alphas(2) + "\".");
                    ErrorsFound = true;
                }
            }

            if (!lAlphaBlanks(3)) {
                thisZoneEvapUnit.AvailManagerListName = Alphas(3);
            }

            thisZoneEvapUnit.OAInletNodeNum = GetOnlySingleNode(state,
                                                                Alphas(4),
                                                                ErrorsFound,
                                                                DataLoopNode::ConnectionObjectType::ZoneHVACEvaporativeCoolerUnit,
                                                                Alphas(1),
                                                                DataLoopNode::NodeFluidType::Air,
                                                                DataLoopNode::ConnectionType::OutsideAir,
                                                                NodeInputManager::CompFluidStream::Primary,
                                                                DataLoopNode::ObjectIsParent);

            thisZoneEvapUnit.UnitOutletNodeNum = GetOnlySingleNode(state,
                                                                   Alphas(5),
                                                                   ErrorsFound,
                                                                   DataLoopNode::ConnectionObjectType::ZoneHVACEvaporativeCoolerUnit,
                                                                   Alphas(1),
                                                                   DataLoopNode::NodeFluidType::Air,
                                                                   DataLoopNode::ConnectionType::Outlet,
                                                                   NodeInputManager::CompFluidStream::Primary,
                                                                   DataLoopNode::ObjectIsParent);

            if (!lAlphaBlanks(6)) {
                thisZoneEvapUnit.UnitReliefNodeNum = GetOnlySingleNode(state,
                                                                       Alphas(6),
                                                                       ErrorsFound,
                                                                       DataLoopNode::ConnectionObjectType::ZoneHVACEvaporativeCoolerUnit,
                                                                       Alphas(1),
                                                                       DataLoopNode::NodeFluidType::Air,
                                                                       DataLoopNode::ConnectionType::Inlet,
                                                                       NodeInputManager::CompFluidStream::Primary,
                                                                       DataLoopNode::ObjectIsParent);
            }

            thisZoneEvapUnit.FanObjectClassName = Alphas(7);
            thisZoneEvapUnit.FanName = Alphas(8);
            errFlag = false;
            if (!UtilityRoutines::SameString(thisZoneEvapUnit.FanObjectClassName, "Fan:SystemModel")) {
                Fans::GetFanType(state, thisZoneEvapUnit.FanName, thisZoneEvapUnit.FanType_Num, errFlag, CurrentModuleObject, thisZoneEvapUnit.Name);
                Fans::GetFanIndex(state, thisZoneEvapUnit.FanName, thisZoneEvapUnit.FanIndex, errFlag, CurrentModuleObject);
                thisZoneEvapUnit.FanInletNodeNum =
                    Fans::GetFanInletNode(state, thisZoneEvapUnit.FanObjectClassName, thisZoneEvapUnit.FanName, errFlag);
                thisZoneEvapUnit.FanOutletNodeNum =
                    Fans::GetFanOutletNode(state, thisZoneEvapUnit.FanObjectClassName, thisZoneEvapUnit.FanName, errFlag);
                Fans::GetFanVolFlow(state, thisZoneEvapUnit.FanIndex, FanVolFlow);
                thisZoneEvapUnit.ActualFanVolFlowRate = FanVolFlow;
                // Get the fan's availability schedule
                thisZoneEvapUnit.FanAvailSchedPtr =
                    Fans::GetFanAvailSchPtr(state, thisZoneEvapUnit.FanObjectClassName, thisZoneEvapUnit.FanName, errFlag);
                if (errFlag) {
                    ShowContinueError(state, "...specified in " + CurrentModuleObject + " = " + thisZoneEvapUnit.Name);
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(thisZoneEvapUnit.FanObjectClassName, "Fan:SystemModel")) {

                thisZoneEvapUnit.FanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, thisZoneEvapUnit.FanName)); // call constructor
                thisZoneEvapUnit.FanIndex = HVACFan::getFanObjectVectorIndex(state, thisZoneEvapUnit.FanName);
                thisZoneEvapUnit.FanInletNodeNum = state.dataHVACFan->fanObjs[thisZoneEvapUnit.FanIndex]->inletNodeNum;
                thisZoneEvapUnit.FanOutletNodeNum = state.dataHVACFan->fanObjs[thisZoneEvapUnit.FanIndex]->outletNodeNum;
                thisZoneEvapUnit.ActualFanVolFlowRate = state.dataHVACFan->fanObjs[thisZoneEvapUnit.FanIndex]->designAirVolFlowRate;
                thisZoneEvapUnit.FanAvailSchedPtr = state.dataHVACFan->fanObjs[thisZoneEvapUnit.FanIndex]->availSchedIndex;
            }

            // set evap unit to cycling mode for all fan types. Note OpMode var is not used
            // with used for ZONECOOLINGLOADVARIABLESPEEDFAN Cooler Unit Control Method
            thisZoneEvapUnit.OpMode = DataHVACGlobals::CycFanCycCoil;

            FanVolFlow = 0.0;
            if (errFlag) {
                ShowContinueError(state, "specified in " + CurrentModuleObject + " = " + thisZoneEvapUnit.Name);
                ErrorsFound = true;
            }

            thisZoneEvapUnit.DesignAirVolumeFlowRate = Numbers(1);

            constexpr std::array<std::string_view, static_cast<int>(FanPlacement::Num)> fanPlacementNamesUC = {"BLOWTHROUGH", "DRAWTHROUGH"};
            thisZoneEvapUnit.FanLocation = static_cast<FanPlacement>(getEnumerationValue(fanPlacementNamesUC, Alphas(9)));
            if (thisZoneEvapUnit.FanLocation == FanPlacement::Invalid) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                ShowContinueError(state, "invalid choice found " + cAlphaFields(9) + "=\"" + Alphas(9) + "\".");
                ErrorsFound = true;
            }

            // get the zone numer served by the zoneHVAC evaporative cooler
            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                    if (thisZoneEvapUnit.UnitOutletNodeNum == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                        thisZoneEvapUnit.ZonePtr = CtrlZone;
                        break;
                    }
                }
            }

            constexpr std::array<std::string_view, static_cast<int>(ControlType::Num)> controlTypeNamesUC = {
                "ZONETEMPERATUREDEADBANDONOFFCYCLING", "ZONECOOLINGLOADONOFFCYCLING", "ZONECOOLINGLOADVARIABLESPEEDFAN"};
            thisZoneEvapUnit.ControlSchemeType = static_cast<ControlType>(getEnumerationValue(controlTypeNamesUC, Alphas(10)));
            if (thisZoneEvapUnit.ControlSchemeType == ControlType::Invalid) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                ShowContinueError(state, "invalid choice found " + cAlphaFields(10) + "=\"" + Alphas(10) + "\".");
                ErrorsFound = true;
            }

            thisZoneEvapUnit.ThrottlingRange = Numbers(2);
            thisZoneEvapUnit.ThresholdCoolingLoad = Numbers(3);

            thisZoneEvapUnit.EvapCooler_1_Type_Num = static_cast<EvapCoolerType>(getEnumerationValue(evapCoolerTypeNamesUC, Alphas(11)));
            if (thisZoneEvapUnit.EvapCooler_1_Type_Num != EvapCoolerType::Invalid) {
                thisZoneEvapUnit.EvapCooler_1_ObjectClassName = evapCoolerTypeNames[static_cast<int>(thisZoneEvapUnit.EvapCooler_1_Type_Num)];
            } else {
                ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                ShowContinueError(state, "invalid choice found " + cAlphaFields(11) + "=\"" + Alphas(11) + "\".");
                ErrorsFound = true;
            }

            thisZoneEvapUnit.EvapCooler_1_Name = Alphas(12);
            thisZoneEvapUnit.EvapCooler_1_Index = UtilityRoutines::FindItemInList(Alphas(12), state.dataEvapCoolers->EvapCond, &EvapConditions::Name);
            if (thisZoneEvapUnit.EvapCooler_1_Index == 0) {
                ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                ShowContinueError(state, "invalid, not found " + cAlphaFields(12) + "=\"" + Alphas(12) + "\".");
                ErrorsFound = true;
            }

            if (!lAlphaBlanks(13)) {
                thisZoneEvapUnit.EvapCooler_2_Type_Num = static_cast<EvapCoolerType>(getEnumerationValue(evapCoolerTypeNamesUC, Alphas(13)));
                if (thisZoneEvapUnit.EvapCooler_2_Type_Num != EvapCoolerType::Invalid) {
                    thisZoneEvapUnit.EvapCooler_2_ObjectClassName = evapCoolerTypeNames[static_cast<int>(thisZoneEvapUnit.EvapCooler_2_Type_Num)];
                } else {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                    ShowContinueError(state, "invalid choice found " + cAlphaFields(13) + "=\"" + Alphas(13) + "\".");
                    ErrorsFound = true;
                }

                if (!lAlphaBlanks(14)) {
                    thisZoneEvapUnit.EvapCooler_2_Name = Alphas(14);
                    thisZoneEvapUnit.EvapCooler_2_Index =
                        UtilityRoutines::FindItemInList(Alphas(14), state.dataEvapCoolers->EvapCond, &EvapConditions::Name);
                    if (thisZoneEvapUnit.EvapCooler_2_Index == 0) {
                        ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                        ShowContinueError(state, "invalid, not found " + cAlphaFields(14) + "=\"" + Alphas(14) + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                    ShowContinueError(state, "missing input for " + cAlphaFields(14));
                    ErrorsFound = true;
                }
            }

            thisZoneEvapUnit.HVACSizingIndex = 0;
            if (!lAlphaBlanks(15)) {
                thisZoneEvapUnit.HVACSizingIndex = UtilityRoutines::FindItemInList(Alphas(15), state.dataSize->ZoneHVACSizing);
                if (thisZoneEvapUnit.HVACSizingIndex == 0) {
                    ShowSevereError(state, cAlphaFields(15) + " = " + Alphas(15) + " not found.");
                    ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + thisZoneEvapUnit.Name);
                    ErrorsFound = true;
                }
            }

            // Add fan to component sets array
            BranchNodeConnections::SetUpCompSets(state,
                                                 CurrentModuleObject,
                                                 thisZoneEvapUnit.Name,
                                                 thisZoneEvapUnit.FanObjectClassName,
                                                 thisZoneEvapUnit.FanName,
                                                 state.dataLoopNodes->NodeID(thisZoneEvapUnit.FanInletNodeNum),
                                                 state.dataLoopNodes->NodeID(thisZoneEvapUnit.FanOutletNodeNum));

            // Add first evap cooler to component sets array
            BranchNodeConnections::SetUpCompSets(state,
                                                 CurrentModuleObject,
                                                 thisZoneEvapUnit.Name,
                                                 thisZoneEvapUnit.EvapCooler_1_ObjectClassName,
                                                 thisZoneEvapUnit.EvapCooler_1_Name,
                                                 state.dataLoopNodes->NodeID(EvapCond(thisZoneEvapUnit.EvapCooler_1_Index).InletNode),
                                                 state.dataLoopNodes->NodeID(EvapCond(thisZoneEvapUnit.EvapCooler_1_Index).OutletNode));

            if (thisZoneEvapUnit.EvapCooler_2_Index > 0) {
                // Add second evap cooler to component sets array
                BranchNodeConnections::SetUpCompSets(state,
                                                     CurrentModuleObject,
                                                     thisZoneEvapUnit.Name,
                                                     thisZoneEvapUnit.EvapCooler_2_ObjectClassName,
                                                     thisZoneEvapUnit.EvapCooler_2_Name,
                                                     state.dataLoopNodes->NodeID(EvapCond(thisZoneEvapUnit.EvapCooler_2_Index).InletNode),
                                                     state.dataLoopNodes->NodeID(EvapCond(thisZoneEvapUnit.EvapCooler_2_Index).OutletNode));
            }

            // check that fan type is consistent with control method
            if (thisZoneEvapUnit.ControlSchemeType == ControlType::ZoneCoolingLoadVariableSpeedFan) { // must have a VS fan type
                if (thisZoneEvapUnit.FanType_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                    ShowContinueError(state, "Fan:ConstantVolume is not consistent with control method ZoneCoolingLoadVariableSpeedFan.");
                    ShowContinueError(state, "Change to a variable speed fan object type");
                    ErrorsFound = true;
                } else if (thisZoneEvapUnit.FanType_Num == DataHVACGlobals::FanType_SimpleOnOff) {
                    ShowSevereError(state, CurrentModuleObject + "=\"" + thisZoneEvapUnit.Name + "\" invalid data.");
                    ShowContinueError(state, "Fan:OnOff is not consistent with control method ZoneCoolingLoadVariableSpeedFan.");
                    ShowContinueError(state, "Change to a variable speed fan object type");
                    ErrorsFound = true;
                }
            }

        } // unit loop
    }

    //***********************************************************************************

    Alphas.deallocate();
    Numbers.deallocate();
    cAlphaFields.deallocate();
    cNumericFields.deallocate();
    lAlphaBlanks.deallocate();
    lNumericBlanks.deallocate();

    if (ErrorsFound) {
        ShowFatalError(state, std::string{RoutineName} + "Errors found in getting input.");
        ShowContinueError(state, "... Preceding condition causes termination.");
    }

    // setup output variables
    for (UnitLoop = 1; UnitLoop <= state.dataEvapCoolers->NumZoneEvapUnits; ++UnitLoop) {
        auto &thisZoneEvapUnit = ZoneEvapUnit(UnitLoop);
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            thisZoneEvapUnit.UnitTotalCoolingRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisZoneEvapUnit.Name);
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            thisZoneEvapUnit.UnitTotalCoolingEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            thisZoneEvapUnit.Name,
                            _,
                            "ENERGYTRANSFER",
                            "COOLINGCOILS",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            thisZoneEvapUnit.UnitSensibleCoolingRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisZoneEvapUnit.Name);
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            thisZoneEvapUnit.UnitSensibleCoolingEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            thisZoneEvapUnit.Name);
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            thisZoneEvapUnit.UnitLatentHeatingRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisZoneEvapUnit.Name);
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            thisZoneEvapUnit.UnitLatentHeatingEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            thisZoneEvapUnit.Name);
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            thisZoneEvapUnit.UnitLatentCoolingRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisZoneEvapUnit.Name);
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            thisZoneEvapUnit.UnitLatentCoolingEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            thisZoneEvapUnit.Name);
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Fan Speed Ratio",
                            OutputProcessor::Unit::None,
                            thisZoneEvapUnit.UnitFanSpeedRatio,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisZoneEvapUnit.Name);
        SetupOutputVariable(state,
                            "Zone Evaporative Cooler Unit Fan Availability Status",
                            OutputProcessor::Unit::None,
                            thisZoneEvapUnit.FanAvailStatus,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            thisZoneEvapUnit.Name);
        if (thisZoneEvapUnit.ControlSchemeType != ControlType::ZoneCoolingLoadVariableSpeedFan) {
            SetupOutputVariable(state,
                                "Zone Evaporative Cooler Unit Part Load Ratio",
                                OutputProcessor::Unit::None,
                                thisZoneEvapUnit.UnitPartLoadRatio,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                thisZoneEvapUnit.Name);
        }
    }
}

void InitZoneEvaporativeCoolerUnit(EnergyPlusData &state,
                                   int const UnitNum, // unit number
                                   int const ZoneNum  // number of zone being served
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    auto &SysTimeElapsed = state.dataHVACGlobal->SysTimeElapsed;
    auto &ZoneComp = state.dataHVACGlobal->ZoneComp;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 TimeElapsed;

    auto &EvapCond(state.dataEvapCoolers->EvapCond);
    auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);

    if (allocated(ZoneComp)) {
        if (ZoneEvapUnit(UnitNum).MyZoneEq) { // initialize the name of each availability manager list and zone number
            ZoneComp(DataZoneEquipment::ZoneEquip::ZoneEvaporativeCoolerUnit).ZoneCompAvailMgrs(UnitNum).AvailManagerListName =
                ZoneEvapUnit(UnitNum).AvailManagerListName;
            ZoneComp(DataZoneEquipment::ZoneEquip::ZoneEvaporativeCoolerUnit).ZoneCompAvailMgrs(UnitNum).ZoneNum = ZoneNum;
            ZoneEvapUnit(UnitNum).MyZoneEq = false;
        }
        ZoneEvapUnit(UnitNum).FanAvailStatus =
            ZoneComp(DataZoneEquipment::ZoneEquip::ZoneEvaporativeCoolerUnit).ZoneCompAvailMgrs(UnitNum).AvailStatus;
    }

    if (!state.dataEvapCoolers->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataEvapCoolers->ZoneEquipmentListChecked = true;
        for (int Loop = 1; Loop <= state.dataEvapCoolers->NumZoneEvapUnits; ++Loop) {
            if (DataZoneEquipment::CheckZoneEquipmentList(state, "ZoneHVAC:EvaporativeCoolerUnit", ZoneEvapUnit(Loop).Name)) {
                ZoneEvapUnit(Loop).ZoneNodeNum = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ZoneNode;
            } else {
                ShowSevereError(state,
                                "InitZoneEvaporativeCoolerUnit: ZoneHVAC:EvaporativeCoolerUnit = " + ZoneEvapUnit(Loop).Name +
                                    ", is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }
    }

    if (!state.dataGlobal->SysSizingCalc && ZoneEvapUnit(UnitNum).MySize) {
        SizeZoneEvaporativeCoolerUnit(state, UnitNum);
        ZoneEvapUnit(UnitNum).MySize = false;
    }

    if (ZoneEvapUnit(UnitNum).MyFan) {
        if (ZoneEvapUnit(UnitNum).ActualFanVolFlowRate != DataSizing::AutoSize) {

            if (ZoneEvapUnit(UnitNum).ActualFanVolFlowRate < ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate) {
                ShowSevereError(state, "InitZoneEvaporativeCoolerUnit: ZoneHVAC:EvaporativeCoolerUnit = " + ZoneEvapUnit(UnitNum).Name);
                ShowContinueError(state, "...unit fan volumetric flow rate less than evaporative cooler unit design supply air flow rate.");
                ShowContinueError(state, format("...fan volumetric flow rate = {:.5T} m3/s.", ZoneEvapUnit(UnitNum).ActualFanVolFlowRate));
                ShowContinueError(state,
                                  format("...evap cooler unit volumetric flow rate = {:.5T} m3/s.", ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate));
                ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate = ZoneEvapUnit(UnitNum).ActualFanVolFlowRate;
                ShowContinueError(state, "...evaporative cooler unit design supply air flow rate will match fan flow rate and simulation continues.");
                ZoneEvapUnit(UnitNum).MyEnvrn = true; // re-initialize to set mass flow rate and max mass flow rate
            }

            if (ZoneEvapUnit(UnitNum).ActualFanVolFlowRate > 0.0) {
                ZoneEvapUnit(UnitNum).DesignFanSpeedRatio =
                    ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate / ZoneEvapUnit(UnitNum).ActualFanVolFlowRate;
            }

            ZoneEvapUnit(UnitNum).MyFan = false;
        } else {
            if (ZoneEvapUnit(UnitNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
                Fans::GetFanVolFlow(state, ZoneEvapUnit(UnitNum).FanIndex, ZoneEvapUnit(UnitNum).ActualFanVolFlowRate);
            } else {
                ZoneEvapUnit(UnitNum).ActualFanVolFlowRate = state.dataHVACFan->fanObjs[ZoneEvapUnit(UnitNum).FanIndex]->designAirVolFlowRate;
            }
        }
    }

    if (ZoneEvapUnit(UnitNum).FanAvailSchedPtr > 0) {
        // include fan is not available, then unit is not available
        ZoneEvapUnit(UnitNum).UnitIsAvailable = ((ScheduleManager::GetCurrentScheduleValue(state, ZoneEvapUnit(UnitNum).FanAvailSchedPtr) > 0.0) &&
                                                 (ScheduleManager::GetCurrentScheduleValue(state, ZoneEvapUnit(UnitNum).AvailSchedIndex) > 0.0));
    } else {
        ZoneEvapUnit(UnitNum).UnitIsAvailable = (ScheduleManager::GetCurrentScheduleValue(state, ZoneEvapUnit(UnitNum).AvailSchedIndex) > 0.0);
    }

    ZoneEvapUnit(UnitNum).EvapCooler_1_AvailStatus =
        (ScheduleManager::GetCurrentScheduleValue(state, EvapCond(ZoneEvapUnit(UnitNum).EvapCooler_1_Index).SchedPtr) > 0.0);

    if (ZoneEvapUnit(UnitNum).EvapCooler_2_Index > 0) {
        ZoneEvapUnit(UnitNum).EvapCooler_2_AvailStatus =
            (ScheduleManager::GetCurrentScheduleValue(state, EvapCond(ZoneEvapUnit(UnitNum).EvapCooler_2_Index).SchedPtr) > 0.0);
    }
    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && ZoneEvapUnit(UnitNum).MyEnvrn) {

        ZoneEvapUnit(UnitNum).DesignAirMassFlowRate = state.dataEnvrn->StdRhoAir * ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate;
        state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRateMax = ZoneEvapUnit(UnitNum).DesignAirMassFlowRate;
        state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRateMin = 0.0;
        state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRateMinAvail = 0.0;

        state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRateMax = ZoneEvapUnit(UnitNum).DesignAirMassFlowRate;
        state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRateMin = 0.0;
        state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRateMinAvail = 0.0;

        if (ZoneEvapUnit(UnitNum).UnitReliefNodeNum > 0) {
            state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).UnitReliefNodeNum).MassFlowRateMax = ZoneEvapUnit(UnitNum).DesignAirMassFlowRate;
            state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).UnitReliefNodeNum).MassFlowRateMin = 0.0;
            state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).UnitReliefNodeNum).MassFlowRateMinAvail = 0.0;
        }
        ZoneEvapUnit(UnitNum).WasOnLastTimestep = false;
        ZoneEvapUnit(UnitNum).IsOnThisTimestep = false;
        ZoneEvapUnit(UnitNum).FanSpeedRatio = 0.0;
        ZoneEvapUnit(UnitNum).UnitFanSpeedRatio = 0.0;
        ZoneEvapUnit(UnitNum).UnitTotalCoolingRate = 0.0;
        ZoneEvapUnit(UnitNum).UnitTotalCoolingEnergy = 0.0;
        ZoneEvapUnit(UnitNum).UnitSensibleCoolingRate = 0.0;
        ZoneEvapUnit(UnitNum).UnitSensibleCoolingEnergy = 0.0;
        ZoneEvapUnit(UnitNum).UnitLatentHeatingRate = 0.0;
        ZoneEvapUnit(UnitNum).UnitLatentHeatingEnergy = 0.0;
        ZoneEvapUnit(UnitNum).UnitLatentCoolingRate = 0.0;
        ZoneEvapUnit(UnitNum).UnitLatentCoolingEnergy = 0.0;
        ZoneEvapUnit(UnitNum).FanAvailStatus = 0.0;

        // place default cold setpoints on control nodes of select evap coolers
        if ((ZoneEvapUnit(UnitNum).EvapCooler_1_Type_Num == EvapCoolerType::DirectResearchSpecial) ||
            (ZoneEvapUnit(UnitNum).EvapCooler_1_Type_Num == EvapCoolerType::IndirectRDDSpecial)) {
            if (EvapCond(ZoneEvapUnit(UnitNum).EvapCooler_1_Index).EvapControlNodeNum > 0) {
                state.dataLoopNodes->Node(EvapCond(ZoneEvapUnit(UnitNum).EvapCooler_1_Index).EvapControlNodeNum).TempSetPoint = -20.0;
            }
        }
        if ((ZoneEvapUnit(UnitNum).EvapCooler_2_Type_Num == EvapCoolerType::DirectResearchSpecial) ||
            (ZoneEvapUnit(UnitNum).EvapCooler_2_Type_Num == EvapCoolerType::IndirectRDDSpecial)) {
            if (EvapCond(ZoneEvapUnit(UnitNum).EvapCooler_2_Index).EvapControlNodeNum > 0) {
                state.dataLoopNodes->Node(EvapCond(ZoneEvapUnit(UnitNum).EvapCooler_2_Index).EvapControlNodeNum).TempSetPoint = -20.0;
            }
        }

        ZoneEvapUnit(UnitNum).MyEnvrn = false;
    }
    if (!state.dataGlobal->BeginEnvrnFlag) {
        ZoneEvapUnit(UnitNum).MyEnvrn = true;
    }

    TimeElapsed = state.dataGlobal->HourOfDay + state.dataGlobal->TimeStep * state.dataGlobal->TimeStepZone + SysTimeElapsed;
    if (ZoneEvapUnit(UnitNum).TimeElapsed != TimeElapsed) {
        ZoneEvapUnit(UnitNum).WasOnLastTimestep = ZoneEvapUnit(UnitNum).IsOnThisTimestep;

        ZoneEvapUnit(UnitNum).TimeElapsed = TimeElapsed;
    }
}

void SizeZoneEvaporativeCoolerUnit(EnergyPlusData &state, int const UnitNum) // unit number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2013
    //       MODIFIED       August 2014 Bereket Nigusse, added scalable sizing
    //       MODIFIED       January 2013 Daeho Kang, add component sizing table entries
    //       RE-ENGINEERED  na

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("SizeZoneEvaporativeCoolerUnit: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string CompName;     // component name
    std::string CompType;     // component type
    std::string SizingString; // input field sizing description (e.g., Nominal Capacity)
    Real64 TempSize;          // autosized value of coil input field
    int SizingMethod;         // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing,
                              // CoolingCapacitySizing, HeatingCapacitySizing, etc.)
    bool PrintFlag;           // TRUE when sizing information is reported in the eio file
    int zoneHVACIndex;        // index of zoneHVAC equipment sizing specification
    int SAFMethod(0);         // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow,
                              // FractionOfAutosizedHeatingAirflow ...)

    auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);

    state.dataSize->DataScalableSizingON = false;
    state.dataSize->ZoneHeatingOnlyFan = false;
    state.dataSize->ZoneCoolingOnlyFan = false;

    CompType = "ZoneHVAC:EvaporativeCoolerUnit";
    CompName = ZoneEvapUnit(UnitNum).Name;
    state.dataSize->DataZoneNumber = ZoneEvapUnit(UnitNum).ZonePtr;
    PrintFlag = true;
    bool errorsFound = false;

    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
    auto &CurZoneEqNum(state.dataSize->CurZoneEqNum);

    if (CurZoneEqNum > 0) {

        if (ZoneEvapUnit(UnitNum).HVACSizingIndex > 0) {
            state.dataSize->ZoneCoolingOnlyFan = true;
            zoneHVACIndex = ZoneEvapUnit(UnitNum).HVACSizingIndex;
            SizingMethod = DataHVACGlobals::CoolingAirflowSizing;
            SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod;
            ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
            if (SAFMethod == DataSizing::None || SAFMethod == DataSizing::SupplyAirFlowRate || SAFMethod == DataSizing::FlowPerFloorArea ||
                SAFMethod == DataSizing::FractionOfAutosizedCoolingAirflow) {
                switch (SAFMethod) {
                case DataSizing::SupplyAirFlowRate:
                    if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow > 0.0) {
                        ZoneEqSizing(CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                    }
                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                    if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow > 0.0) {
                        PrintFlag = false;
                    }
                    break;
                case DataSizing::FlowPerFloorArea:
                    ZoneEqSizing(CurZoneEqNum).SystemAirFlow = true;
                    ZoneEqSizing(CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow *
                                                            state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                    TempSize = ZoneEqSizing(CurZoneEqNum).AirVolFlow;
                    state.dataSize->DataScalableSizingON = true;
                    break;
                case DataSizing::FractionOfAutosizedCoolingAirflow:
                    state.dataSize->DataFracOfAutosizedCoolingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                    TempSize = DataSizing::AutoSize;
                    state.dataSize->DataScalableSizingON = true;
                    break;
                default:
                    TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                }

                CoolingAirFlowSizer sizingCoolingAirFlow;
                std::string stringOverride = "Design Supply Air Flow Rate [m3/s]";
                if (state.dataGlobal->isEpJSON) stringOverride = "design_supply_air_flow_rate [m3/s]";
                sizingCoolingAirFlow.overrideSizingString(stringOverride);
                sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate = sizingCoolingAirFlow.size(state, TempSize, errorsFound);

            } else if (SAFMethod == DataSizing::FlowPerCoolingCapacity) {
                SizingMethod = DataHVACGlobals::CoolingCapacitySizing;
                TempSize = DataSizing::AutoSize;
                PrintFlag = false;
                state.dataSize->DataScalableSizingON = true;
                state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(CurZoneEqNum).DesCoolVolFlow;
                if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod == DataSizing::FractionOfAutosizedCoolingCapacity) {
                    state.dataSize->DataFracOfAutosizedCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                }
                CoolingCapacitySizer sizerCoolingCapacity;
                sizerCoolingCapacity.overrideSizingString(SizingString);
                sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                state.dataSize->DataCapacityUsedForSizing = sizerCoolingCapacity.size(state, TempSize, errorsFound);
                state.dataSize->DataFlowPerCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                PrintFlag = true;
                TempSize = DataSizing::AutoSize;

                CoolingAirFlowSizer sizingCoolingAirFlow;
                std::string stringOverride = "Design Supply Air Flow Rate [m3/s]";
                if (state.dataGlobal->isEpJSON) stringOverride = "design_supply_air_flow_rate [m3/s]";
                sizingCoolingAirFlow.overrideSizingString(stringOverride);
                sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate = sizingCoolingAirFlow.size(state, TempSize, errorsFound);
            }
            state.dataSize->DataScalableSizingON = false;
            state.dataSize->ZoneCoolingOnlyFan = false;
        } else {
            // no scalble sizing method has been specified. Sizing proceeds using the method
            // specified in the zoneHVAC object
            // N1 , \field Maximum Supply Air Flow Rate
            state.dataSize->ZoneCoolingOnlyFan = true;
            if (ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate > 0.0) {
                PrintFlag = false;
            }
            TempSize = ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate;
            CoolingAirFlowSizer sizingCoolingAirFlow;
            std::string stringOverride = "Design Supply Air Flow Rate [m3/s]";
            if (state.dataGlobal->isEpJSON) stringOverride = "design_supply_air_flow_rate [m3/s]";
            sizingCoolingAirFlow.overrideSizingString(stringOverride);
            sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
            ZoneEvapUnit(UnitNum).DesignAirVolumeFlowRate = sizingCoolingAirFlow.size(state, TempSize, errorsFound);
            state.dataSize->ZoneCoolingOnlyFan = false;
        }
    }
}

void CalcZoneEvaporativeCoolerUnit(EnergyPlusData &state,
                                   int const UnitNum,              // unit number
                                   int const ZoneNum,              // number of zone being served
                                   Real64 &SensibleOutputProvided, // sensible capacity delivered to zone
                                   Real64 &LatentOutputProvided    // Latent add/removal  (kg/s), dehumid = negative
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneCoolingLoad;
    Real64 CoolingLoadThreashold;
    Real64 ZoneTemp;
    Real64 CoolSetLowThrottle;
    Real64 CoolSetHiThrottle;
    Real64 PartLoadRatio;

    {
        auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);

        if (ZoneEvapUnit(UnitNum).ControlSchemeType == ControlType::ZoneTemperatureDeadBandOnOffCycling) {
            ZoneTemp = state.dataLoopNodes->Node(ZoneEvapUnit(UnitNum).ZoneNodeNum).Temp;
            CoolSetLowThrottle = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) - (0.5 * ZoneEvapUnit(UnitNum).ThrottlingRange);
            CoolSetHiThrottle = state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) + (0.5 * ZoneEvapUnit(UnitNum).ThrottlingRange);

            if ((ZoneTemp < CoolSetLowThrottle) || !ZoneEvapUnit(UnitNum).UnitIsAvailable) {
                ZoneEvapUnit(UnitNum).IsOnThisTimestep = false;
            } else if (ZoneTemp > CoolSetHiThrottle) {
                ZoneEvapUnit(UnitNum).IsOnThisTimestep = true;
            } else {
                if (ZoneEvapUnit(UnitNum).WasOnLastTimestep) {
                    ZoneEvapUnit(UnitNum).IsOnThisTimestep = true;
                } else {
                    ZoneEvapUnit(UnitNum).IsOnThisTimestep = false;
                }
            }

            if (ZoneEvapUnit(UnitNum).IsOnThisTimestep) {

                if (ZoneEvapUnit(UnitNum).OpMode == DataHVACGlobals::ContFanCycCoil) {
                    PartLoadRatio = 1.0;
                    ZoneEvapUnit(UnitNum).UnitPartLoadRatio = PartLoadRatio;
                    CalcZoneEvapUnitOutput(state, UnitNum, PartLoadRatio, SensibleOutputProvided, LatentOutputProvided);
                } else {
                    ZoneCoolingLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
                    // calculate part load ratio for cycling fan/unit first
                    ControlZoneEvapUnitOutput(state, UnitNum, ZoneCoolingLoad);
                    PartLoadRatio = ZoneEvapUnit(UnitNum).UnitPartLoadRatio;
                    CalcZoneEvapUnitOutput(state, UnitNum, PartLoadRatio, SensibleOutputProvided, LatentOutputProvided);
                }

            } else { // not running

                PartLoadRatio = 0.0;
                ZoneEvapUnit(UnitNum).UnitPartLoadRatio = PartLoadRatio;
                CalcZoneEvapUnitOutput(state, UnitNum, PartLoadRatio, SensibleOutputProvided, LatentOutputProvided);
            }

        } else if (ZoneEvapUnit(UnitNum).ControlSchemeType == ControlType::ZoneCoolingLoadOnOffCycling) {

            // get zone loads
            ZoneCoolingLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
            CoolingLoadThreashold = -1.0 * ZoneEvapUnit(UnitNum).ThresholdCoolingLoad;

            if ((ZoneCoolingLoad < CoolingLoadThreashold) && ZoneEvapUnit(UnitNum).UnitIsAvailable) {

                if (ZoneEvapUnit(UnitNum).OpMode == DataHVACGlobals::ContFanCycCoil) {
                    PartLoadRatio = 1.0;
                    ZoneEvapUnit(UnitNum).UnitPartLoadRatio = PartLoadRatio;
                    CalcZoneEvapUnitOutput(state, UnitNum, PartLoadRatio, SensibleOutputProvided, LatentOutputProvided);
                } else {
                    // calculate part load ratio for cycling fan/unit first
                    ControlZoneEvapUnitOutput(state, UnitNum, ZoneCoolingLoad);
                    PartLoadRatio = ZoneEvapUnit(UnitNum).UnitPartLoadRatio;
                    CalcZoneEvapUnitOutput(state, UnitNum, PartLoadRatio, SensibleOutputProvided, LatentOutputProvided);
                }

            } else {
                // unit is off
                PartLoadRatio = 0.0;
                ZoneEvapUnit(UnitNum).UnitPartLoadRatio = PartLoadRatio;
                CalcZoneEvapUnitOutput(state, UnitNum, PartLoadRatio, SensibleOutputProvided, LatentOutputProvided);
            }

        } else if (ZoneEvapUnit(UnitNum).ControlSchemeType == ControlType::ZoneCoolingLoadVariableSpeedFan) {
            // get zone loads
            ZoneCoolingLoad = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToCoolSP;
            CoolingLoadThreashold = -1.0 * ZoneEvapUnit(UnitNum).ThresholdCoolingLoad;
            if ((ZoneCoolingLoad < CoolingLoadThreashold) && ZoneEvapUnit(UnitNum).UnitIsAvailable) {

                // determine fan speed to meet load
                ControlVSEvapUnitToMeetLoad(state, UnitNum, ZoneNum, ZoneCoolingLoad);
                // variable speed fan used fan speed ratio instead of partload ratio
                CalcZoneEvapUnitOutput(state, UnitNum, ZoneEvapUnit(UnitNum).FanSpeedRatio, SensibleOutputProvided, LatentOutputProvided);

            } else {
                // unit is off
                PartLoadRatio = 0.0;
                CalcZoneEvapUnitOutput(state, UnitNum, PartLoadRatio, SensibleOutputProvided, LatentOutputProvided);
            }
        }
    }
}

void CalcZoneEvapUnitOutput(EnergyPlusData &state,
                            int const UnitNum,              // unit number
                            Real64 const PartLoadRatio,     // zone evap unit part load ratiod
                            Real64 &SensibleOutputProvided, // target cooling load
                            Real64 &LatentOutputProvided    // target cooling load
)
{
    // caculates zone evaporative cooler sensible and latent outputs

    Real64 MinHumRat; // minimum humidity ratio

    auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);
    auto &Node(state.dataLoopNodes->Node);

    int const ZoneNodeNum = ZoneEvapUnit(UnitNum).ZoneNodeNum;
    int const OAInletNodeNum = ZoneEvapUnit(UnitNum).OAInletNodeNum;
    int const OutletNodeNum = ZoneEvapUnit(UnitNum).UnitOutletNodeNum;
    int const ReliefNodeNum = ZoneEvapUnit(UnitNum).UnitReliefNodeNum;
    int const FanInletNodeNum = ZoneEvapUnit(UnitNum).FanInletNodeNum;
    int const FanOutletNodeNum = ZoneEvapUnit(UnitNum).FanOutletNodeNum;
    int const EvapCooler_1_Index = ZoneEvapUnit(UnitNum).EvapCooler_1_Index;
    int const EvapCooler_2_Index = ZoneEvapUnit(UnitNum).EvapCooler_2_Index;

    auto &EvapCond(state.dataEvapCoolers->EvapCond);

    // calculate unit sensible cooling output
    if (PartLoadRatio > 0) {
        Node(OAInletNodeNum).MassFlowRate = ZoneEvapUnit(UnitNum).DesignAirMassFlowRate * PartLoadRatio;
        Node(OAInletNodeNum).MassFlowRateMaxAvail = Node(OAInletNodeNum).MassFlowRate;
        Node(OutletNodeNum).MassFlowRate = Node(OAInletNodeNum).MassFlowRate;
        Node(OutletNodeNum).MassFlowRateMaxAvail = Node(OutletNodeNum).MassFlowRate;
    } else { // not running
        Node(OAInletNodeNum).MassFlowRate = 0.0;
        Node(OAInletNodeNum).MassFlowRateMaxAvail = 0.0;
        Node(FanInletNodeNum).MassFlowRate = 0.0;
        Node(FanInletNodeNum).MassFlowRateMaxAvail = 0.0;
        Node(FanOutletNodeNum).MassFlowRate = 0.0;
        Node(FanOutletNodeNum).MassFlowRateMaxAvail = 0.0;
        Node(OutletNodeNum).MassFlowRate = 0.0;
        Node(OutletNodeNum).MassFlowRateMaxAvail = 0.0;

        Node(EvapCond(EvapCooler_1_Index).InletNode).MassFlowRate = 0.0;
        Node(EvapCond(EvapCooler_1_Index).InletNode).MassFlowRateMaxAvail = 0.0;
        Node(EvapCond(EvapCooler_1_Index).OutletNode).MassFlowRate = 0.0;
        Node(EvapCond(EvapCooler_1_Index).OutletNode).MassFlowRateMaxAvail = 0.0;

        if (EvapCooler_2_Index > 0) {
            Node(EvapCond(EvapCooler_2_Index).InletNode).MassFlowRate = 0.0;
            Node(EvapCond(EvapCooler_2_Index).InletNode).MassFlowRateMaxAvail = 0.0;
            Node(EvapCond(EvapCooler_2_Index).OutletNode).MassFlowRate = 0.0;
            Node(EvapCond(EvapCooler_2_Index).OutletNode).MassFlowRateMaxAvail = 0.0;
        }
    }
    if (ReliefNodeNum > 0) {
        Node(ReliefNodeNum).MassFlowRate = Node(OAInletNodeNum).MassFlowRate;
        Node(ReliefNodeNum).MassFlowRateMaxAvail = Node(OAInletNodeNum).MassFlowRate;
    }
    if (ZoneEvapUnit(UnitNum).FanLocation == FanPlacement::BlowThruFan) {
        Node(FanOutletNodeNum).MassFlowRate = Node(OAInletNodeNum).MassFlowRate;
        Node(FanOutletNodeNum).MassFlowRateMaxAvail = Node(OAInletNodeNum).MassFlowRate;
        if (ZoneEvapUnit(UnitNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            Fans::SimulateFanComponents(state,
                                        ZoneEvapUnit(UnitNum).FanName,
                                        false,
                                        ZoneEvapUnit(UnitNum).FanIndex,
                                        _,
                                        state.dataHVACGlobal->ZoneCompTurnFansOn,
                                        state.dataHVACGlobal->ZoneCompTurnFansOff);
        } else {
            state.dataHVACFan->fanObjs[ZoneEvapUnit(UnitNum).FanIndex]->simulate(
                state, _, state.dataHVACGlobal->ZoneCompTurnFansOn, state.dataHVACGlobal->ZoneCompTurnFansOff, _);
        }
    }

    if (ZoneEvapUnit(UnitNum).EvapCooler_1_AvailStatus) {
        SimEvapCooler(state, ZoneEvapUnit(UnitNum).EvapCooler_1_Name, ZoneEvapUnit(UnitNum).EvapCooler_1_Index, PartLoadRatio);
    }

    if ((ZoneEvapUnit(UnitNum).EvapCooler_2_Index > 0) && ZoneEvapUnit(UnitNum).EvapCooler_2_AvailStatus) {
        SimEvapCooler(state, ZoneEvapUnit(UnitNum).EvapCooler_2_Name, ZoneEvapUnit(UnitNum).EvapCooler_2_Index, PartLoadRatio);
    }
    if (ZoneEvapUnit(UnitNum).FanLocation == FanPlacement::DrawThruFan) {
        if (ZoneEvapUnit(UnitNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            Fans::SimulateFanComponents(state,
                                        ZoneEvapUnit(UnitNum).FanName,
                                        false,
                                        ZoneEvapUnit(UnitNum).FanIndex,
                                        _,
                                        state.dataHVACGlobal->ZoneCompTurnFansOn,
                                        state.dataHVACGlobal->ZoneCompTurnFansOff);
        } else {
            state.dataHVACFan->fanObjs[ZoneEvapUnit(UnitNum).FanIndex]->simulate(
                state, _, state.dataHVACGlobal->ZoneCompTurnFansOn, state.dataHVACGlobal->ZoneCompTurnFansOff, _);
        }
    }

    // calculate sensible and latent outputs delivered
    MinHumRat = min(Node(ZoneNodeNum).HumRat, Node(OutletNodeNum).HumRat);
    SensibleOutputProvided = Node(OutletNodeNum).MassFlowRate * (Psychrometrics::PsyHFnTdbW(Node(OutletNodeNum).Temp, MinHumRat) -
                                                                 Psychrometrics::PsyHFnTdbW(Node(ZoneNodeNum).Temp, MinHumRat));
    LatentOutputProvided = Node(OutletNodeNum).MassFlowRate * (Node(OutletNodeNum).HumRat - Node(ZoneNodeNum).HumRat);
}

void ControlZoneEvapUnitOutput(EnergyPlusData &state,
                               int const UnitNum,           // unit number
                               Real64 const ZoneCoolingLoad // target cooling load
)
{

    // calculates unit cooling part load ratio using root solver numerical method

    // local variables
    int constexpr MaxIte(50);      // maximum number of iterations
    Real64 constexpr Tol(0.01);    // error tolerance
    int SolFla;                    // Flag of root solver
    Real64 PartLoadRatio;          // cooling part load ratio
    Real64 FullFlowSensibleOutput; // full flow sensible cooling output
    Real64 FullFlowLatentOutput;   // full flow sensible cooling output

    auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);

    // get full flow sensible cooling output
    PartLoadRatio = 1.0;
    CalcZoneEvapUnitOutput(state, UnitNum, PartLoadRatio, FullFlowSensibleOutput, FullFlowLatentOutput);

    // calculate part load ratio
    if (FullFlowSensibleOutput < ZoneCoolingLoad) {
        std::array<Real64, 2> Par = {Real64(UnitNum), ZoneCoolingLoad}; // Parameters passed to root solver

        General::SolveRoot(state, Tol, MaxIte, SolFla, PartLoadRatio, ZoneEvapUnitLoadResidual, 0.0, 1.0, Par);
        if (SolFla == -1) {
            if (ZoneEvapUnit(UnitNum).UnitLoadControlMaxIterErrorIndex == 0) {
                ShowWarningError(state, "Iteration limit exceeded calculating evap unit part load ratio, for unit=" + ZoneEvapUnit(UnitNum).Name);
                ShowContinueErrorTimeStamp(state, "");
                ShowContinueError(state, format("Unit part load ratio returned={:.2R}", PartLoadRatio));
                ShowContinueError(state, "Check input for Fan Placement.");
            }
            ShowRecurringWarningErrorAtEnd(
                state,
                format("Zone Evaporative Cooler unit part load ratio control failed (iteration limit [{}]) for ZoneHVAC:EvaporativeCoolerUnit =\"{}",
                       MaxIte,
                       ZoneEvapUnit(UnitNum).Name),
                ZoneEvapUnit(UnitNum).UnitLoadControlMaxIterErrorIndex);

        } else if (SolFla == -2) {
            if (ZoneEvapUnit(UnitNum).UnitLoadControlLimitsErrorIndex == 0) {
                ShowWarningError(state,
                                 "Zone Evaporative Cooler unit calculation failed: unit part load ratio limits exceeded, for unit = " +
                                     ZoneEvapUnit(UnitNum).Name);
                ShowContinueError(state, "Check input for Fan Placement.");
                ShowContinueErrorTimeStamp(state, "");
                if (state.dataGlobal->WarmupFlag) ShowContinueError(state, "Error occurred during warmup days.");
            }
            ShowRecurringWarningErrorAtEnd(
                state,
                "Zone Evaporative Cooler unit part load ratio control failed (limits exceeded) for ZoneHVAC:EvaporativeCoolerUnit =\"" +
                    ZoneEvapUnit(UnitNum).Name,
                ZoneEvapUnit(UnitNum).UnitLoadControlLimitsErrorIndex);
        }

    } else {
        PartLoadRatio = 1.0;
    }
    ZoneEvapUnit(UnitNum).UnitPartLoadRatio = PartLoadRatio;
}

Real64 ZoneEvapUnitLoadResidual(EnergyPlusData &state, Real64 const PartLoadRatio, std::array<Real64, 2> const &Par // parameters
)
{
    // calculates cooling load residual by varying part load ratio
    int UnitNum = int(Par[0]);
    Real64 LoadToBeMet = Par[1];
    Real64 QSensOutputProvided; // sensible output at a given PLR
    Real64 QLatOutputProvided;  // latent output at a given PLR
    CalcZoneEvapUnitOutput(state, UnitNum, PartLoadRatio, QSensOutputProvided, QLatOutputProvided);
    return QSensOutputProvided - LoadToBeMet;
}

void ControlVSEvapUnitToMeetLoad(EnergyPlusData &state,
                                 int const UnitNum,           // unit number
                                 int const ZoneNum,           // number of zone being served
                                 Real64 const ZoneCoolingLoad // target cooling load
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
    auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;

    // SUBROUTINE PARAMETER DEFINITIONS:
    int constexpr MaxIte(500); // maximum number of iterations

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 MinHumRat;
    Real64 FanSpeedRatio;
    Real64 ErrorToler(0.001); // error tolerance
    int SolFla;               // Flag of RegulaFalsi solver
    Real64 FullFlowSensibleOutputProvided;

    auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);
    auto &Node(state.dataLoopNodes->Node);

    // first get full load result
    ErrorToler = 0.01;

    ZoneEvapUnit(UnitNum).FanSpeedRatio = 1.0;
    Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate = ZoneEvapUnit(UnitNum).DesignAirMassFlowRate;
    Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRateMaxAvail = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
    Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRate = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
    Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRateMaxAvail = Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRate;

    if (ZoneEvapUnit(UnitNum).UnitReliefNodeNum > 0) {
        Node(ZoneEvapUnit(UnitNum).UnitReliefNodeNum).MassFlowRate = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
        Node(ZoneEvapUnit(UnitNum).UnitReliefNodeNum).MassFlowRateMaxAvail = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
    }
    if (ZoneEvapUnit(UnitNum).FanLocation == FanPlacement::BlowThruFan) {
        Node(ZoneEvapUnit(UnitNum).FanOutletNodeNum).MassFlowRate = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
        Node(ZoneEvapUnit(UnitNum).FanOutletNodeNum).MassFlowRateMaxAvail = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
        if (ZoneEvapUnit(UnitNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            Fans::SimulateFanComponents(
                state, ZoneEvapUnit(UnitNum).FanName, false, ZoneEvapUnit(UnitNum).FanIndex, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff);
        } else {
            state.dataHVACFan->fanObjs[ZoneEvapUnit(UnitNum).FanIndex]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
        }
    }

    if (ZoneEvapUnit(UnitNum).EvapCooler_1_AvailStatus) {
        SimEvapCooler(state, ZoneEvapUnit(UnitNum).EvapCooler_1_Name, ZoneEvapUnit(UnitNum).EvapCooler_1_Index);
    }

    if ((ZoneEvapUnit(UnitNum).EvapCooler_2_Index > 0) && ZoneEvapUnit(UnitNum).EvapCooler_2_AvailStatus) {
        SimEvapCooler(state, ZoneEvapUnit(UnitNum).EvapCooler_2_Name, ZoneEvapUnit(UnitNum).EvapCooler_2_Index);
    }
    if (ZoneEvapUnit(UnitNum).FanLocation == FanPlacement::DrawThruFan) {
        if (ZoneEvapUnit(UnitNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            Fans::SimulateFanComponents(
                state, ZoneEvapUnit(UnitNum).FanName, false, ZoneEvapUnit(UnitNum).FanIndex, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff);
        } else {
            state.dataHVACFan->fanObjs[ZoneEvapUnit(UnitNum).FanIndex]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
        }
    }

    // calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
    MinHumRat = min(Node(ZoneEvapUnit(UnitNum).ZoneNodeNum).HumRat, Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).HumRat);
    FullFlowSensibleOutputProvided = Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRate *
                                     (Psychrometrics::PsyHFnTdbW(Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).Temp, MinHumRat) -
                                      Psychrometrics::PsyHFnTdbW(Node(ZoneEvapUnit(UnitNum).ZoneNodeNum).Temp, MinHumRat));

    if (FullFlowSensibleOutputProvided < ZoneCoolingLoad) { // find speed ratio by regula falsi numerical method
        std::array<Real64, 5> Par = {0.0};                  // Parameters passed to RegulaFalsi
        Par[0] = UnitNum;
        Par[1] = ZoneNum;
        Par[2] = ZoneEvapUnit(UnitNum).ZoneNodeNum;
        Par[4] = ZoneCoolingLoad;
        FanSpeedRatio = 1.0;

        General::SolveRoot(state, ErrorToler, MaxIte, SolFla, FanSpeedRatio, VSEvapUnitLoadResidual, 0.0, 1.0, Par);
        if (SolFla == -1) {
            if (ZoneEvapUnit(UnitNum).UnitVSControlMaxIterErrorIndex == 0) {
                ShowWarningError(
                    state, "Iteration limit exceeded calculating variable speed evap unit fan speed ratio, for unit=" + ZoneEvapUnit(UnitNum).Name);
                ShowContinueErrorTimeStamp(state, "");
                ShowContinueError(state, format("Fan speed ratio returned={:.2R}", FanSpeedRatio));
                ShowContinueError(state, "Check input for Fan Placement.");
            }
            ShowRecurringWarningErrorAtEnd(
                state,
                format("Zone Evaporative Cooler unit control failed (iteration limit [{}]) for ZoneHVAC:EvaporativeCoolerUnit =\"{}",
                       MaxIte,
                       ZoneEvapUnit(UnitNum).Name),
                ZoneEvapUnit(UnitNum).UnitVSControlMaxIterErrorIndex);

        } else if (SolFla == -2) {
            if (ZoneEvapUnit(UnitNum).UnitVSControlLimitsErrorIndex == 0) {
                ShowWarningError(state,
                                 "Variable speed evaporative cooler unit calculation failed: fan speed ratio limits exceeded, for unit = " +
                                     ZoneEvapUnit(UnitNum).Name);
                ShowContinueError(state, "Check input for Fan Placement.");
                ShowContinueErrorTimeStamp(state, "");
                if (state.dataGlobal->WarmupFlag) ShowContinueError(state, "Error occurred during warmup days.");
            }
            ShowRecurringWarningErrorAtEnd(state,
                                           "Zone Evaporative Cooler unit control failed (limits exceeded) for ZoneHVAC:EvaporativeCoolerUnit =\"" +
                                               ZoneEvapUnit(UnitNum).Name,
                                           ZoneEvapUnit(UnitNum).UnitVSControlLimitsErrorIndex);
        }
        ZoneEvapUnit(UnitNum).FanSpeedRatio = FanSpeedRatio;
    }
}

Real64 VSEvapUnitLoadResidual(EnergyPlusData &state, Real64 const FanSpeedRatio, std::array<Real64, 5> const &Par // parameters
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         <author>
    //       DATE WRITTEN   <date_written>
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // Using/Aliasing
    auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
    auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;

    // Return value
    Real64 Residual;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int UnitNum;
    int ZoneNum;
    int ZoneNodeNum;
    Real64 LoadToBeMet; // sensible load to be met
    Real64 MinHumRat;
    Real64 SensibleOutputProvided;

    auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);
    auto &Node(state.dataLoopNodes->Node);

    UnitNum = int(Par[0]);
    ZoneNum = int(Par[1]);
    ZoneNodeNum = int(Par[2]);
    LoadToBeMet = Par[4];

    Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate = ZoneEvapUnit(UnitNum).DesignAirMassFlowRate * FanSpeedRatio;
    Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRateMaxAvail = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
    Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRate = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
    Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRateMaxAvail = Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRate;

    if (ZoneEvapUnit(UnitNum).UnitReliefNodeNum > 0) {
        Node(ZoneEvapUnit(UnitNum).UnitReliefNodeNum).MassFlowRate = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
        Node(ZoneEvapUnit(UnitNum).UnitReliefNodeNum).MassFlowRateMaxAvail = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
    }
    if (ZoneEvapUnit(UnitNum).FanLocation == FanPlacement::BlowThruFan) {
        Node(ZoneEvapUnit(UnitNum).FanOutletNodeNum).MassFlowRate = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
        Node(ZoneEvapUnit(UnitNum).FanOutletNodeNum).MassFlowRateMaxAvail = Node(ZoneEvapUnit(UnitNum).OAInletNodeNum).MassFlowRate;
        if (ZoneEvapUnit(UnitNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            Fans::SimulateFanComponents(
                state, ZoneEvapUnit(UnitNum).FanName, false, ZoneEvapUnit(UnitNum).FanIndex, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff);
        } else {
            state.dataHVACFan->fanObjs[ZoneEvapUnit(UnitNum).FanIndex]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
        }
    }

    if (ZoneEvapUnit(UnitNum).EvapCooler_1_AvailStatus) {
        SimEvapCooler(state, ZoneEvapUnit(UnitNum).EvapCooler_1_Name, ZoneEvapUnit(UnitNum).EvapCooler_1_Index, FanSpeedRatio);
    }

    if ((ZoneEvapUnit(UnitNum).EvapCooler_2_Index > 0) && ZoneEvapUnit(UnitNum).EvapCooler_2_AvailStatus) {
        SimEvapCooler(state, ZoneEvapUnit(UnitNum).EvapCooler_2_Name, ZoneEvapUnit(UnitNum).EvapCooler_2_Index, FanSpeedRatio);
    }
    if (ZoneEvapUnit(UnitNum).FanLocation == FanPlacement::DrawThruFan) {
        if (ZoneEvapUnit(UnitNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            Fans::SimulateFanComponents(
                state, ZoneEvapUnit(UnitNum).FanName, false, ZoneEvapUnit(UnitNum).FanIndex, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff);
        } else {
            state.dataHVACFan->fanObjs[ZoneEvapUnit(UnitNum).FanIndex]->simulate(state, _, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
        }
    }

    MinHumRat = min(Node(ZoneEvapUnit(UnitNum).ZoneNodeNum).HumRat, Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).HumRat);
    SensibleOutputProvided = Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).MassFlowRate *
                             (Psychrometrics::PsyHFnTdbW(Node(ZoneEvapUnit(UnitNum).UnitOutletNodeNum).Temp, MinHumRat) -
                              Psychrometrics::PsyHFnTdbW(Node(ZoneEvapUnit(UnitNum).ZoneNodeNum).Temp, MinHumRat));

    Residual = SensibleOutputProvided - LoadToBeMet;

    return Residual;
}

void ReportZoneEvaporativeCoolerUnit(EnergyPlusData &state, int const UnitNum) // unit number
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         B. Griffith
    //       DATE WRITTEN   July 2013
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // update output variables for the zone evap unit

    // Using/Aliasing
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ZoneNodeNum;
    int UnitOutletNodeNum;
    Real64 AirMassFlow;
    Real64 MinHumRat;
    Real64 QTotUnitOut;
    Real64 QSensUnitOut;

    auto &ZoneEvapUnit(state.dataEvapCoolers->ZoneEvapUnit);

    ZoneNodeNum = ZoneEvapUnit(UnitNum).ZoneNodeNum;
    UnitOutletNodeNum = ZoneEvapUnit(UnitNum).UnitOutletNodeNum;
    AirMassFlow = state.dataLoopNodes->Node(UnitOutletNodeNum).MassFlowRate;
    QTotUnitOut = AirMassFlow * (state.dataLoopNodes->Node(UnitOutletNodeNum).Enthalpy - state.dataLoopNodes->Node(ZoneNodeNum).Enthalpy);
    MinHumRat = min(state.dataLoopNodes->Node(ZoneNodeNum).HumRat, state.dataLoopNodes->Node(UnitOutletNodeNum).HumRat);
    QSensUnitOut = AirMassFlow * (Psychrometrics::PsyHFnTdbW(state.dataLoopNodes->Node(UnitOutletNodeNum).Temp, MinHumRat) -
                                  Psychrometrics::PsyHFnTdbW(state.dataLoopNodes->Node(ZoneNodeNum).Temp, MinHumRat));

    ZoneEvapUnit(UnitNum).UnitTotalCoolingRate = std::abs(min(0.0, QTotUnitOut));
    ZoneEvapUnit(UnitNum).UnitTotalCoolingEnergy = ZoneEvapUnit(UnitNum).UnitTotalCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
    ZoneEvapUnit(UnitNum).UnitSensibleCoolingRate = std::abs(min(0.0, QSensUnitOut));
    ZoneEvapUnit(UnitNum).UnitSensibleCoolingEnergy = ZoneEvapUnit(UnitNum).UnitSensibleCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
    ZoneEvapUnit(UnitNum).UnitLatentHeatingRate = std::abs(max(0.0, (QTotUnitOut - QSensUnitOut)));
    ZoneEvapUnit(UnitNum).UnitLatentHeatingEnergy = ZoneEvapUnit(UnitNum).UnitLatentHeatingRate * TimeStepSys * DataGlobalConstants::SecInHour;
    ZoneEvapUnit(UnitNum).UnitLatentCoolingRate = std::abs(min(0.0, (QTotUnitOut - QSensUnitOut)));
    ZoneEvapUnit(UnitNum).UnitLatentCoolingEnergy = ZoneEvapUnit(UnitNum).UnitLatentCoolingRate * TimeStepSys * DataGlobalConstants::SecInHour;
    ZoneEvapUnit(UnitNum).UnitFanSpeedRatio = ZoneEvapUnit(UnitNum).FanSpeedRatio;
}

int GetInletNodeNum(EnergyPlusData &state, std::string const &EvapCondName, bool &ErrorsFound)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   May 2019
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given EvapCond and returns the air inlet node number.
    // If incorrect EvapCond name is given, ErrorsFound is returned as true and node number as zero.

    if (state.dataEvapCoolers->GetInputEvapComponentsFlag) { // First time subroutine has been entered
        GetEvapInput(state);
        state.dataEvapCoolers->GetInputEvapComponentsFlag = false;
    }

    int WhichEvapCond =
        UtilityRoutines::FindItemInList(EvapCondName, state.dataEvapCoolers->EvapCond, &EvapConditions::Name, state.dataEvapCoolers->NumEvapCool);
    if (WhichEvapCond != 0) {
        return state.dataEvapCoolers->EvapCond(WhichEvapCond).InletNode;
    } else {
        ShowSevereError(state, "GetInletNodeNum: Could not find EvaporativeCooler = \"" + EvapCondName + "\"");
        ErrorsFound = true;
        return 0;
    }
}

int GetOutletNodeNum(EnergyPlusData &state, std::string const &EvapCondName, bool &ErrorsFound)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   May 2019
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up the given EvapCond and returns the air outlet node number.
    // If incorrect EvapCond name is given, ErrorsFound is returned as true and node number as zero.

    if (state.dataEvapCoolers->GetInputEvapComponentsFlag) { // First time subroutine has been entered
        GetEvapInput(state);
        state.dataEvapCoolers->GetInputEvapComponentsFlag = false;
    }
    int WhichEvapCond =
        UtilityRoutines::FindItemInList(EvapCondName, state.dataEvapCoolers->EvapCond, &EvapConditions::Name, state.dataEvapCoolers->NumEvapCool);
    if (WhichEvapCond != 0) {
        return state.dataEvapCoolers->EvapCond(WhichEvapCond).OutletNode;
    } else {
        ShowSevereError(state, "GetOutletNodeNum: Could not find EvaporativeCooler = \"" + EvapCondName + "\"");
        ErrorsFound = true;
        return 0;
    }
}

} // namespace EnergyPlus::EvaporativeCoolers
