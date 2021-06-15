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

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/HeatBalanceIntRadExchange.hh>
#include <EnergyPlus/HeatBalanceSurfaceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SteamBaseboardRadiator.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace SteamBaseboardRadiator {

    // Module -- (ref: Object: ZoneHVAC:Baseboard:RadiantConvective:Steam)

    // Module containing the routines dealing with the steam baseboard heaters

    // MODULE INFORMATION:
    //       AUTHOR         Daeho Kang
    //       DATE WRITTEN   September 2009
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // The purpose of this module is to simulate steam baseboard heaters.

    // METHODOLOGY EMPLOYED:

    // REFERENCES:
    // 1. HWBaseboardRadiator module (ZoneHVAC:Baseboard:RadiantConvective:Water)
    // 2. SteamCoils module (Coil:Heating:Steam)

    using DataHVACGlobals::SmallLoad;
    using DataLoopNode::ObjectIsNotParent;
    using DataPlant::TypeOf_Baseboard_Rad_Conv_Steam;
    using DataZoneEquipment::CheckZoneEquipmentList;

    static constexpr std::string_view fluidNameSteam("STEAM");

    void SimSteamBaseboard(EnergyPlusData &state,
                           std::string const &EquipName,
                           int const ActualZoneNum,
                           int const ControlledZoneNum,
                           bool const FirstHVACIteration,
                           Real64 &PowerMet,
                           int &CompIndex)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //       DATE WRITTEN   Nov 1997
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine simulates the steam baseboards or radiators.

        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BaseboardNum; // index of unit in baseboard array
        Real64 QZnReq;    // zone load not yet satisfied
        Real64 MaxSteamFlow;
        Real64 MinSteamFlow;

        if (state.dataSteamBaseboardRadiator->GetInputFlag) {
            GetSteamBaseboardInput(state);
            state.dataSteamBaseboardRadiator->GetInputFlag = false;
        }

        // Find the correct Baseboard Equipment
        if (CompIndex == 0) {
            BaseboardNum =
                UtilityRoutines::FindItemInList(EquipName, state.dataSteamBaseboardRadiator->SteamBaseboard, &SteamBaseboardParams::EquipID);
            if (BaseboardNum == 0) {
                ShowFatalError(state, "SimSteamBaseboard: Unit not found=" + EquipName);
            }
            CompIndex = BaseboardNum;
        } else {
            BaseboardNum = CompIndex;
            if (BaseboardNum > state.dataSteamBaseboardRadiator->NumSteamBaseboards || BaseboardNum < 1) {
                ShowFatalError(state,
                               format("SimSteamBaseboard:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                      BaseboardNum,
                                      state.dataSteamBaseboardRadiator->NumSteamBaseboards,
                                      EquipName));
            }
            if (state.dataSteamBaseboardRadiator->CheckEquipName(BaseboardNum)) {
                if (EquipName != state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID) {
                    ShowFatalError(state,
                                   format("SimSteamBaseboard: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                          BaseboardNum,
                                          EquipName,
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID));
                }
                state.dataSteamBaseboardRadiator->CheckEquipName(BaseboardNum) = false;
            }
        }

        if (CompIndex > 0) {

            InitSteamBaseboard(state, BaseboardNum, ControlledZoneNum, FirstHVACIteration);

            QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputReqToHeatSP;

            SteamBaseboardDesignData SteamBaseboardDesignDataObject{state.dataSteamBaseboardRadiator->SteamBaseboardDesign(
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum)
                    .DesignObjectPtr)}; // Array that contains the design data for steam baseboard objects

            if (QZnReq > SmallLoad && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ActualZoneNum) &&
                (GetCurrentScheduleValue(state, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr) > 0.0)) {

                // On the first HVAC iteration the system values are given to the controller, but after that
                // the demand limits are in place and there needs to be feedback to the Zone Equipment
                if (FirstHVACIteration) {
                    MaxSteamFlow = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRateMax;
                    MinSteamFlow = 0.0;
                } else {
                    MaxSteamFlow =
                        state.dataLoopNodes->Node(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode).MassFlowRateMaxAvail;
                    MinSteamFlow =
                        state.dataLoopNodes->Node(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode).MassFlowRateMinAvail;
                }

                {
                    auto const SELECT_CASE_var(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipType);

                    if (SELECT_CASE_var == TypeOf_Baseboard_Rad_Conv_Steam) { // 'ZoneHVAC:Baseboard:RadiantConvective:Steam'
                        ControlCompOutput(state,
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                          state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                          BaseboardNum,
                                          FirstHVACIteration,
                                          QZnReq,
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode,
                                          MaxSteamFlow,
                                          MinSteamFlow,
                                          SteamBaseboardDesignDataObject.Offset,
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ControlCompTypeNum,
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompErrIndex,
                                          _,
                                          _,
                                          _,
                                          _,
                                          _,
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum);
                    } else {
                        ShowSevereError(state,
                                        "SimSteamBaseboard: Errors in Baseboard=" +
                                            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                        ShowContinueError(state,
                                          format("Invalid or unimplemented equipment type={}",
                                                 state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipType));
                        ShowFatalError(state, "Preceding condition causes termination.");
                    }
                }

                PowerMet = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotPower;
            } else {
                // baseboard is off, don't bother going into ControlCompOutput
                Real64 mdot = 0.0;
                SetComponentFlowRate(state,
                                     mdot,
                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode,
                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletNode,
                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum);
                CalcSteamBaseboard(state, BaseboardNum, PowerMet);
            }

            UpdateSteamBaseboard(state, BaseboardNum);

            ReportSteamBaseboard(state, BaseboardNum);

        } else {
            ShowFatalError(state, "SimSteamBaseboard: Unit not found=" + EquipName);
        }
    }

    void GetSteamBaseboardInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   September 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the input for the baseboard units.

        // METHODOLOGY EMPLOYED:
        // Standard input processor calls.

        // REFERENCES:
        // HWBaseboardRadiator module

        // Using/Aliasing
        using BranchNodeConnections::TestCompSet;
        using FluidProperties::FindRefrigerant;

        using GlobalNames::VerifyUniqueBaseboardName;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;
        using namespace DataSizing;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("GetSteamBaseboardInput:");
        Real64 const MaxFraction(1.0);       // Maximum limit of fractional values
        Real64 const MinFraction(0.0);       // Minimum limit of fractional values
        Real64 const MaxSteamFlowRate(10.0); // Maximum limit of steam volume flow rate in m3/s
        Real64 const MinSteamFlowRate(0.0);  // Minimum limit of steam volume flow rate in m3/s
        //    INTEGER,PARAMETER :: MaxDistribSurfaces   = 20          ! Maximum number of surfaces that a baseboard heater can radiate to
        int const MinDistribSurfaces(1);            // Minimum number of surfaces that a baseboard heater can radiate to
        int const iHeatCAPMAlphaNum(2);             // get input index to steam baseboard Radiator system heating capacity sizing method
        int const iHeatDesignCapacityNumericNum(1); // get input index to steam baseboard Radiator system electric heating capacity
        int const iHeatCapacityPerFloorAreaNumericNum(
            1); // get input index to steam baseboard Radiator system electric heating capacity per floor area sizing
        int const iHeatFracOfAutosizedCapacityNumericNum(
            2); //  get input index to steam baseboard Radiator system electric heating capacity sizing as fraction of autozized heating capacity

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AllFracsSummed;     // Sum of the fractions radiant
        int BaseboardNum;          // Baseboard number
        int BaseboardDesignNum(0); // Baseboard number
        int NumAlphas;             // Number of Alphas for each GetobjectItem call
        int NumNumbers;            // Number of Numbers for each GetobjectItem call
        int SurfNum;               // Surface number Do loop counter
        int IOStat;
        bool ErrorsFound(false); // If errors detected in input
        bool SteamMessageNeeded;

        SteamMessageNeeded = true;
        state.dataSteamBaseboardRadiator->NumSteamBaseboards =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam);
        state.dataSteamBaseboardRadiator->NumSteamBaseboardsDesign =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design);

        // Count total number of baseboard units

        state.dataSteamBaseboardRadiator->SteamBaseboard.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboards);
        state.dataSteamBaseboardRadiator->CheckEquipName.dimension(state.dataSteamBaseboardRadiator->NumSteamBaseboards, true);
        state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboards);

        // Count total number of baseboard design objects

        state.dataSteamBaseboardRadiator->SteamBaseboardDesign.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboardsDesign);
        state.dataSteamBaseboardRadiator->CheckDesignObjectName.dimension(state.dataSteamBaseboardRadiator->NumSteamBaseboardsDesign, true);
        state.dataSteamBaseboardRadiator->SteamBaseboardDesignNumericFields.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboardsDesign);
        state.dataSteamBaseboardRadiator->SteamBaseboardDesignNames.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboardsDesign);

        // Get the data from the user input related to baseboard heater designobjects
        for (BaseboardDesignNum = 1; BaseboardDesignNum <= state.dataSteamBaseboardRadiator->NumSteamBaseboardsDesign; ++BaseboardDesignNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design,
                                                                     BaseboardDesignNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(
                state, state.dataIPShortCut->cAlphaArgs(1), state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design, ErrorsFound);
            state.dataSteamBaseboardRadiator->SteamBaseboardDesignNumericFields(BaseboardDesignNum).FieldNames.allocate(NumNumbers);
            state.dataSteamBaseboardRadiator->SteamBaseboardDesignNumericFields(BaseboardDesignNum).FieldNames = "";
            state.dataSteamBaseboardRadiator->SteamBaseboardDesignNumericFields(BaseboardDesignNum).FieldNames =
                state.dataIPShortCut->cNumericFieldNames;

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            VerifyUniqueBaseboardName(state,
                                      state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design,
                                      state.dataIPShortCut->cAlphaArgs(1),
                                      ErrorsFound,
                                      state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + " Name");

            state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).designName =
                state.dataIPShortCut->cAlphaArgs(1); // Name of the design object of baseboard
            state.dataSteamBaseboardRadiator->SteamBaseboardDesignNames(BaseboardDesignNum) =
                state.dataIPShortCut->cAlphaArgs(1); // Add to  array of design object names

            // Determine steam baseboard radiator system heating design capacity sizing method
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "HeatingDesignCapacity")) {
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).HeatingCapMethod = HeatingDesignCapacity;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "CapacityPerFloorArea")) {
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).HeatingCapMethod = CapacityPerFloorArea;
                if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                    state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).DesignScaledHeatingCapacity =
                        state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum);
                    if (state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).DesignScaledHeatingCapacity <= 0.0) {
                        ShowSevereError(state,
                                        state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + " = " +
                                            state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).designName);
                        ShowContinueError(state,
                                          "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                              state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum),
                                                 state.dataIPShortCut->rNumericArgs(iHeatCapacityPerFloorAreaNumericNum)));
                        ErrorsFound = true;
                    } else if (state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).DesignScaledHeatingCapacity == AutoSize) {
                        ShowSevereError(state,
                                        state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + " = " +
                                            state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).designName);
                        ShowContinueError(state,
                                          "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                              state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError(state,
                                          "Illegal " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + " = " +
                                        state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).designName);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state,
                                      "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity")) {
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                    state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).DesignScaledHeatingCapacity =
                        state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum);
                    if (state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).DesignScaledHeatingCapacity < 0.0) {
                        ShowSevereError(state,
                                        state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + " = " +
                                            state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).designName);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum),
                                                 state.dataIPShortCut->rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + " = " +
                                        state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).designName);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(
                        state, "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state,
                                state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + " = " +
                                    state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).designName);
                ShowContinueError(state,
                                  "Illegal " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                      state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                ErrorsFound = true;
            }

            state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).Offset = state.dataIPShortCut->rNumericArgs(3);
            // Set default convergence tolerance
            if (state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).Offset <= 0.0) {
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).Offset = 0.001;
                ShowWarningError(state,
                                 std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + "=\"" +
                                     state.dataIPShortCut->cAlphaArgs(1) + "\", " + state.dataIPShortCut->cNumericFieldNames(3) +
                                     " was less than the allowable minimum.");
                ShowContinueError(state, "...reset to default value=[0.001].");
            }

            // Fraction of radiant heat out of the total heating rate of the unit
            state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracRadiant = state.dataIPShortCut->rNumericArgs(4);
            if (state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracRadiant < MinFraction) {
                ShowWarningError(state,
                                 std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + "=\"" +
                                     state.dataIPShortCut->cAlphaArgs(1) + "\", " + state.dataIPShortCut->cNumericFieldNames(4) +
                                     " was lower than the allowable minimum.");
                ShowContinueError(state, format("...reset to minimum value=[{:.3R}].", MinFraction));
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracRadiant = MinFraction;
            } else if (state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracRadiant > MaxFraction) {
                ShowWarningError(state,
                                 std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + "=\"" +
                                     state.dataIPShortCut->cAlphaArgs(1) + "\", " + state.dataIPShortCut->cNumericFieldNames(4) +
                                     " was higher than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.3R}].", MaxFraction));
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracRadiant = MaxFraction;
            }

            // Fraction of radiant heat addition to the people within the radiant heating capacity specified by the user
            state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracDistribPerson = state.dataIPShortCut->rNumericArgs(5);
            if (state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracDistribPerson < MinFraction) {
                ShowWarningError(state,
                                 std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + "=\"" +
                                     state.dataIPShortCut->cAlphaArgs(1) + "\", " + state.dataIPShortCut->cNumericFieldNames(5) +
                                     " was lower than the allowable minimum.");
                ShowContinueError(state, format("...reset to minimum value=[{:.3R}].", MinFraction));
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracDistribPerson = MinFraction;
            }
            if (state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracDistribPerson > MaxFraction) {
                ShowWarningError(state,
                                 std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam_Design + "=\"" +
                                     state.dataIPShortCut->cAlphaArgs(1) + "\", " + state.dataIPShortCut->cNumericFieldNames(5) +
                                     " was higher than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.3R}].", MaxFraction));
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(BaseboardDesignNum).FracDistribPerson = MaxFraction;
            }
        }

        // Get the data from the user input related to baseboard heaters
        for (BaseboardNum = 1; BaseboardNum <= state.dataSteamBaseboardRadiator->NumSteamBaseboards; ++BaseboardNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                                                     BaseboardNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(
                state, state.dataIPShortCut->cAlphaArgs(1), state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam, ErrorsFound);
            state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields(BaseboardNum).FieldNames.allocate(NumNumbers);
            state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields(BaseboardNum).FieldNames = "";
            state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields(BaseboardNum).FieldNames = state.dataIPShortCut->cNumericFieldNames;

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            VerifyUniqueBaseboardName(state,
                                      state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                      state.dataIPShortCut->cAlphaArgs(1),
                                      ErrorsFound,
                                      state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " Name");

            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID = state.dataIPShortCut->cAlphaArgs(1); // Name of the baseboard
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipType =
                TypeOf_Baseboard_Rad_Conv_Steam; //'ZoneHVAC:Baseboard:RadiantConvective:Steam'

            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).designObjectName =
                state.dataIPShortCut->cAlphaArgs(2); // Name of the design object for this baseboard
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).DesignObjectPtr =
                UtilityRoutines::FindItemInList(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).designObjectName,
                                                state.dataSteamBaseboardRadiator->SteamBaseboardDesignNames);
            SteamBaseboardDesignData SteamBaseboardDesignDataObject{
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum)
                                                                           .DesignObjectPtr)}; // Contains the design data for steam baseboard object

            // Get schedule
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Schedule = state.dataIPShortCut->cAlphaArgs(3);
            if (state.dataIPShortCut->lAlphaFieldBlanks(3)) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr =
                    GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr == 0) {
                    ShowSevereError(state,
                                    std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(1) + "\", " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" +
                                        state.dataIPShortCut->cAlphaArgs(3) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Get inlet node number
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode =
                GetOnlySingleNode(state,
                                  state.dataIPShortCut->cAlphaArgs(4),
                                  ErrorsFound,
                                  state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                  state.dataIPShortCut->cAlphaArgs(1),
                                  DataLoopNode::NodeFluidType::Steam,
                                  DataLoopNode::NodeConnectionType::Inlet,
                                  1,
                                  ObjectIsNotParent);

            // Get outlet node number
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletNode =
                GetOnlySingleNode(state,
                                  state.dataIPShortCut->cAlphaArgs(5),
                                  ErrorsFound,
                                  state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                  state.dataIPShortCut->cAlphaArgs(1),
                                  DataLoopNode::NodeFluidType::Steam,
                                  DataLoopNode::NodeConnectionType::Outlet,
                                  1,
                                  ObjectIsNotParent);
            TestCompSet(state,
                        state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                        state.dataIPShortCut->cAlphaArgs(1),
                        state.dataIPShortCut->cAlphaArgs(4),
                        state.dataIPShortCut->cAlphaArgs(5),
                        "Hot Steam Nodes");

            // Determine steam baseboard radiator system heating design capacity sizing method
            if (SteamBaseboardDesignDataObject.HeatingCapMethod == HeatingDesignCapacity) {
                if (!state.dataIPShortCut->lNumericFieldBlanks(iHeatDesignCapacityNumericNum)) {
                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity =
                        state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum);
                    if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity < 0.0 &&
                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity != AutoSize) {
                        ShowSevereError(state,
                                        state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " +
                                            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                        ShowContinueError(state,
                                          format("Illegal {} = {:.7T}",
                                                 state.dataIPShortCut->cNumericFieldNames(iHeatDesignCapacityNumericNum),
                                                 state.dataIPShortCut->rNumericArgs(iHeatDesignCapacityNumericNum)));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state,
                                    state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " +
                                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                    ShowContinueError(state,
                                      "Input for " + state.dataIPShortCut->cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " +
                                          state.dataIPShortCut->cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError(state,
                                      "Blank field not allowed for " + state.dataIPShortCut->cNumericFieldNames(iHeatDesignCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (SteamBaseboardDesignDataObject.HeatingCapMethod == CapacityPerFloorArea) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity =
                    SteamBaseboardDesignDataObject.DesignScaledHeatingCapacity;
            } else if (SteamBaseboardDesignDataObject.HeatingCapMethod == FractionOfAutosizedHeatingCapacity) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity =
                    SteamBaseboardDesignDataObject.DesignScaledHeatingCapacity;
            }

            // Desired degree of cooling
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).DegOfSubcooling = state.dataIPShortCut->rNumericArgs(2);
            // Maximum steam flow rate
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax = state.dataIPShortCut->rNumericArgs(3);
            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax >= MaxSteamFlowRate) {
                ShowWarningError(state,
                                 std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", " + state.dataIPShortCut->cNumericFieldNames(3) + " was higher than the allowable maximum.");
                ShowContinueError(state, format("...reset to maximum value=[{:.2R}].", MaxSteamFlowRate));
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax = MaxSteamFlowRate;
            } else if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax <= MinSteamFlowRate &&
                       state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax != AutoSize) {
                ShowWarningError(state,
                                 std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", " + state.dataIPShortCut->cNumericFieldNames(3) + " was less than the allowable minimum.");
                ShowContinueError(state, format("...reset to minimum value=[{:.2R}].", MinSteamFlowRate));
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax = MinSteamFlowRate;
            }
            // Remaining fraction is added to the zone as convective heat transfer
            AllFracsSummed = SteamBaseboardDesignDataObject.FracRadiant;
            if (AllFracsSummed > MaxFraction) {
                ShowWarningError(state,
                                 std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", Fraction Radiant was higher than the allowable maximum.");
                SteamBaseboardDesignDataObject.FracRadiant = MaxFraction;
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracConvect = 0.0;
            } else {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracConvect = 1.0 - AllFracsSummed;
            }

            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib = NumNumbers - 3;
            //      IF (SteamBaseboard(BaseboardNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
            //        CALL ShowWarningError(state, RoutineName//cCMO_BBRadiator_Steam//'="'//TRIM(state.dataIPShortCut->cAlphaArgs(1))// &
            //          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
            //        CALL ShowContinueError(state, '...only the maximum value=['//TRIM(RoundSigDigits(MaxDistribSurfaces))//  &
            //           '] will be processed.')
            //        SteamBaseboard(BaseboardNum)%TotSurfToDistrib = MaxDistribSurfaces
            //      END IF
            if ((state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib < MinDistribSurfaces) &&
                (SteamBaseboardDesignDataObject.FracRadiant > MinFraction)) {
                ShowSevereError(state,
                                std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", the number of surface/radiant fraction groups entered was less than the allowable minimum.");
                ShowContinueError(state, format("...the minimum that must be entered=[{}].", MinDistribSurfaces));
                ErrorsFound = true;
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib = 0;
            }
            // Allocate the surfaces and fractions
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum)
                .SurfaceName.allocate(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfaceName = "";
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum)
                .SurfacePtr.allocate(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr = 0;
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum)
                .FracDistribToSurf.allocate(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf = 0.0;

            // search zone equipment list structure for zone index
            for (int ctrlZone = 1; ctrlZone <= state.dataGlobal->NumOfZones; ++ctrlZone) {
                for (int zoneEquipTypeNum = 1; zoneEquipTypeNum <= state.dataZoneEquip->ZoneEquipList(ctrlZone).NumOfEquipTypes; ++zoneEquipTypeNum) {
                    if (state.dataZoneEquip->ZoneEquipList(ctrlZone).EquipType_Num(zoneEquipTypeNum) == DataZoneEquipment::BBSteam_Num &&
                        state.dataZoneEquip->ZoneEquipList(ctrlZone).EquipName(zoneEquipTypeNum) ==
                            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID) {
                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr = ctrlZone;
                    }
                }
            }
            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr <= 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" +
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID +
                                    "\" is not on any ZoneHVAC:EquipmentList.");
                ErrorsFound = true;
                continue;
            }

            AllFracsSummed = SteamBaseboardDesignDataObject.FracDistribPerson;
            for (SurfNum = 1; SurfNum <= state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib; ++SurfNum) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfaceName(SurfNum) = state.dataIPShortCut->cAlphaArgs(SurfNum + 5);
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr(SurfNum) =
                    HeatBalanceIntRadExchange::GetRadiantSystemSurface(
                        state,
                        state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr,
                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfaceName(SurfNum),
                        ErrorsFound);
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) =
                    state.dataIPShortCut->rNumericArgs(SurfNum + 3);
                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) > MaxFraction) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" +
                                         state.dataIPShortCut->cAlphaArgs(1) + "\", " + state.dataIPShortCut->cNumericFieldNames(SurfNum + 3) +
                                         "was greater than the allowable maximum.");
                    ShowContinueError(state, format("...reset to maximum value=[{:.1R}].", MaxFraction));
                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib = MaxFraction;
                }
                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) < MinFraction) {
                    ShowWarningError(state,
                                     std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" +
                                         state.dataIPShortCut->cAlphaArgs(1) + "\", " + state.dataIPShortCut->cNumericFieldNames(SurfNum + 3) +
                                         "was less than the allowable minimum.");
                    ShowContinueError(state, format("...reset to maximum value=[{:.1R}].", MinFraction));
                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib = MinFraction;
                }
                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr(SurfNum) != 0) {
                    state.dataSurface->SurfIntConvSurfGetsRadiantHeat(
                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr(SurfNum)) = true;
                }

                AllFracsSummed += state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(SurfNum);
            } // surfaces

            if (AllFracsSummed > (MaxFraction + 0.01)) {
                ShowSevereError(state,
                                "Fraction of radiation distributed to surfaces sums up to greater than 1 for " + state.dataIPShortCut->cAlphaArgs(1));
                ShowContinueError(state, "Occurs in Baseboard Heater=" + state.dataIPShortCut->cAlphaArgs(1));
                ErrorsFound = true;
            }
            if ((AllFracsSummed < (MaxFraction - 0.01)) &&
                (SteamBaseboardDesignDataObject.FracRadiant >
                 MinFraction)) { // User didn't distribute all of the | radiation warn that some will be lost
                ShowWarningError(state,
                                 std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                     "\", Summed radiant fractions for people + surface groups < 1.0");
                ShowContinueError(state, "The rest of the radiant energy delivered by the baseboard heater will be lost");
            }

            if (state.dataSteamBaseboardRadiator->SteamIndex == 0 && BaseboardNum == 1) {
                state.dataSteamBaseboardRadiator->SteamIndex = FindRefrigerant(state, "Steam");
                if (state.dataSteamBaseboardRadiator->SteamIndex == 0) {
                    ShowSevereError(state, std::string{RoutineName} + "Steam Properties for " + state.dataIPShortCut->cAlphaArgs(1) + " not found.");
                    if (SteamMessageNeeded) ShowContinueError(state, "Steam Fluid Properties should have been included in the input file.");
                    ErrorsFound = true;
                    SteamMessageNeeded = false;
                }
            }
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex = state.dataSteamBaseboardRadiator->SteamIndex;
        }

        if (ErrorsFound) {
            ShowFatalError(state,
                           std::string{RoutineName} + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "Errors found getting input. Program terminates.");
        }

        // Setup Report variables for the Coils
        for (BaseboardNum = 1; BaseboardNum <= state.dataSteamBaseboardRadiator->NumSteamBaseboards; ++BaseboardNum) {
            // CurrentModuleObject='ZoneHVAC:Baseboard:RadiantConvective:Steam'
            SetupOutputVariable(state,
                                "Baseboard Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotPower,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);

            SetupOutputVariable(state,
                                "Baseboard Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvPower,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state,
                                "Baseboard Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadPower,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state,
                                "Baseboard Total Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotEnergy,
                                "System",
                                "Sum",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                _,
                                "ENERGYTRANSFER",
                                "BASEBOARD",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Baseboard Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvEnergy,
                                "System",
                                "Sum",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state,
                                "Baseboard Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadEnergy,
                                "System",
                                "Sum",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state,
                                "Baseboard Steam Energy",
                                OutputProcessor::Unit::J,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Energy,
                                "System",
                                "Sum",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "BASEBOARD",
                                _,
                                "System");
            SetupOutputVariable(state,
                                "Baseboard Steam Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Power,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state,
                                "Baseboard Steam Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRate,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state,
                                "Baseboard Steam Inlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletTemp,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state,
                                "Baseboard Steam Outlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletTemp,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
        }
    }

    void InitSteamBaseboard(EnergyPlusData &state, int const BaseboardNum, int const ControlledZoneNumSub, bool const FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //                      Rick Strand
        //       DATE WRITTEN   Nov 1997
        //                      Feb 2001
        //       MODIFIED       Sep 2009 Daeho Kang (Add Radiant Component)
        //                      Sept 2010 Chandan Sharma, FSEC (plant interactions)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine initializes the baseboard units.

        // METHODOLOGY EMPLOYED:
        // The initialization subrotines both in high temperature radiant radiator
        // and convective only baseboard radiator are combined and modified.
        // The heater is assumed to be crossflow with both fluids unmixed.

        // REFERENCES:

        // Using/Aliasing
        using FluidProperties::GetSatDensityRefrig;
        using FluidProperties::GetSatEnthalpyRefrig;
        using PlantUtilities::InitComponentNodes;
        using PlantUtilities::ScanPlantLoopsForObject;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("InitSteamCoil");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &MyEnvrnFlag = state.dataSteamBaseboardRadiator->MyEnvrnFlag;
        int Loop;
        int SteamInletNode;
        int ZoneNode;
        int ZoneNum;
        Real64 StartEnthSteam;
        Real64 SteamDensity;
        bool errFlag;

        // Do the one time initializations
        if (state.dataSteamBaseboardRadiator->MyOneTimeFlag) {

            // initialize the environment and sizing flags
            MyEnvrnFlag.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboards);
            state.dataSteamBaseboardRadiator->MySizeFlag.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboards);
            state.dataSteamBaseboardRadiator->ZeroSourceSumHATsurf.dimension(state.dataGlobal->NumOfZones, 0.0);
            state.dataSteamBaseboardRadiator->QBBSteamRadSource.dimension(state.dataSteamBaseboardRadiator->NumSteamBaseboards, 0.0);
            state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg.dimension(state.dataSteamBaseboardRadiator->NumSteamBaseboards, 0.0);
            state.dataSteamBaseboardRadiator->LastQBBSteamRadSrc.dimension(state.dataSteamBaseboardRadiator->NumSteamBaseboards, 0.0);
            state.dataSteamBaseboardRadiator->LastSysTimeElapsed.dimension(state.dataSteamBaseboardRadiator->NumSteamBaseboards, 0.0);
            state.dataSteamBaseboardRadiator->LastTimeStepSys.dimension(state.dataSteamBaseboardRadiator->NumSteamBaseboards, 0.0);
            state.dataSteamBaseboardRadiator->SetLoopIndexFlag.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboards);
            MyEnvrnFlag = true;
            state.dataSteamBaseboardRadiator->MySizeFlag = true;
            state.dataSteamBaseboardRadiator->MyOneTimeFlag = false;
            state.dataSteamBaseboardRadiator->SetLoopIndexFlag = true;
        }

        if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr <= 0)
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr =
                state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ActualZoneNum;

        // Need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
        if (!state.dataSteamBaseboardRadiator->ZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
            state.dataSteamBaseboardRadiator->ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= state.dataSteamBaseboardRadiator->NumSteamBaseboards; ++Loop) {
                if (CheckZoneEquipmentList(state,
                                           state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                           state.dataSteamBaseboardRadiator->SteamBaseboard(Loop).EquipID))
                    continue;
                ShowSevereError(state,
                                "InitBaseboard: Unit=[" + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + ',' +
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(Loop).EquipID +
                                    "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (state.dataSteamBaseboardRadiator->SetLoopIndexFlag(BaseboardNum)) {
            if (allocated(state.dataPlnt->PlantLoop)) {
                errFlag = false;
                ScanPlantLoopsForObject(state,
                                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipType,
                                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum,
                                        errFlag,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _);
                state.dataSteamBaseboardRadiator->SetLoopIndexFlag(BaseboardNum) = false;
                if (errFlag) {
                    ShowFatalError(state, "InitSteamBaseboard: Program terminated for previous conditions.");
                }
            }
        }

        if (!state.dataGlobal->SysSizingCalc && state.dataSteamBaseboardRadiator->MySizeFlag(BaseboardNum) &&
            (!state.dataSteamBaseboardRadiator->SetLoopIndexFlag(BaseboardNum))) {
            // For each coil, do the sizing once
            SizeSteamBaseboard(state, BaseboardNum);
            state.dataSteamBaseboardRadiator->MySizeFlag(BaseboardNum) = false;
        }

        // Do the Begin Environment initializations
        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag(BaseboardNum)) {
            // Initialize
            SteamInletNode = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode;
            state.dataLoopNodes->Node(SteamInletNode).Temp = 100.0;
            state.dataLoopNodes->Node(SteamInletNode).Press = 101325.0;
            SteamDensity = GetSatDensityRefrig(state,
                                               fluidNameSteam,
                                               state.dataLoopNodes->Node(SteamInletNode).Temp,
                                               1.0,
                                               state.dataLoopNodes->Node(SteamInletNode).FluidIndex,
                                               RoutineName);
            StartEnthSteam = GetSatEnthalpyRefrig(state,
                                                  fluidNameSteam,
                                                  state.dataLoopNodes->Node(SteamInletNode).Temp,
                                                  1.0,
                                                  state.dataLoopNodes->Node(SteamInletNode).FluidIndex,
                                                  RoutineName);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRateMax =
                SteamDensity * state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax;
            InitComponentNodes(state,
                               0.0,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRateMax,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletNode,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum);
            state.dataLoopNodes->Node(SteamInletNode).Enthalpy = StartEnthSteam;
            state.dataLoopNodes->Node(SteamInletNode).Quality = 1.0;
            state.dataLoopNodes->Node(SteamInletNode).HumRat = 0.0;

            // Initializes radiant sources
            state.dataSteamBaseboardRadiator->ZeroSourceSumHATsurf = 0.0;
            state.dataSteamBaseboardRadiator->QBBSteamRadSource = 0.0;
            state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg = 0.0;
            state.dataSteamBaseboardRadiator->LastQBBSteamRadSrc = 0.0;
            state.dataSteamBaseboardRadiator->LastSysTimeElapsed = 0.0;
            state.dataSteamBaseboardRadiator->LastTimeStepSys = 0.0;

            MyEnvrnFlag(BaseboardNum) = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            MyEnvrnFlag(BaseboardNum) = true;
        }

        if (state.dataGlobal->BeginTimeStepFlag && FirstHVACIteration) {
            ZoneNum = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr;
            state.dataSteamBaseboardRadiator->ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(state, ZoneNum);
            state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg(BaseboardNum) = 0.0;
            state.dataSteamBaseboardRadiator->LastQBBSteamRadSrc(BaseboardNum) = 0.0;
            state.dataSteamBaseboardRadiator->LastSysTimeElapsed(BaseboardNum) = 0.0;
            state.dataSteamBaseboardRadiator->LastTimeStepSys(BaseboardNum) = 0.0;
        }

        // Do the every time step initializations
        SteamInletNode = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode;
        ZoneNode = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNumSub).ZoneNode;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRate = state.dataLoopNodes->Node(SteamInletNode).MassFlowRate;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletTemp = state.dataLoopNodes->Node(SteamInletNode).Temp;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletEnthalpy = state.dataLoopNodes->Node(SteamInletNode).Enthalpy;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletPress = state.dataLoopNodes->Node(SteamInletNode).Press;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletQuality = state.dataLoopNodes->Node(SteamInletNode).Quality;

        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotPower = 0.0;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Power = 0.0;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvPower = 0.0;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadPower = 0.0;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotEnergy = 0.0;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Energy = 0.0;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvEnergy = 0.0;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadEnergy = 0.0;
    }

    void SizeSteamBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   February 2002
        //       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
        //                      August 2014 Bereket Nigusse, added scalable sizing
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for sizing steam baseboard components

        // METHODOLOGY EMPLOYED:
        // Obtains flow rates from the zone sizing arrays and plant sizing data.

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSizing;
        using DataHVACGlobals::HeatingCapacitySizing;
        using FluidProperties::GetSatDensityRefrig;
        using FluidProperties::GetSatEnthalpyRefrig;
        using FluidProperties::GetSatSpecificHeatRefrig;
        using PlantUtilities::RegisterPlantCompDesignFlow;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("SizeSteamBaseboard");

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int PltSizSteamNum(0);               // Index of plant sizing object for 1st steam loop
        Real64 DesCoilLoad(0.0);             // Design heating load in the zone
        Real64 SteamInletTemp;               // Inlet steam temperature in C
        Real64 EnthSteamInDry;               // Enthalpy of dry steam
        Real64 EnthSteamOutWet;              // Enthalpy of wet steam
        Real64 LatentHeatSteam;              // latent heat of steam
        Real64 SteamDensity;                 // Density of steam
        Real64 Cp;                           // local fluid specific heat
        bool ErrorsFound(false);             // If errors detected in input
        bool IsAutoSize(false);              // Indicator to autosizing steam flow
        Real64 SteamVolFlowRateMaxDes(0.0);  // Design maximum steam volume flow for reporting
        Real64 SteamVolFlowRateMaxUser(0.0); // User hard-sized maximum steam volume flow for reporting
        std::string CompName;                // component name
        std::string CompType;                // component type
        std::string SizingString;            // input field sizing description (e.g., Nominal Capacity)
        Real64 TempSize;                     // autosized value of coil input field
        int FieldNum = 1;                    // IDD numeric field number where input field description is found
        int SizingMethod;                    // Integer representation of sizing method name (HeatingCapacitySizing)
        bool PrintFlag;                      // TRUE when sizing information is reported in the eio file
        int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, and FractionOfAutosizedHeatingCapacity )

        auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);
        auto &CurZoneEqNum(state.dataSize->CurZoneEqNum);

        SteamBaseboardDesignData SteamBaseboardDesignDataObject{state.dataSteamBaseboardRadiator->SteamBaseboardDesign(
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).DesignObjectPtr)}; // Contains the data for variable flow hydronic systems

        // Find the appropriate steam plant sizing object
        PltSizSteamNum = state.dataPlnt->PlantLoop(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum).PlantSizNum;
        //    PltSizSteamNum = MyPlantSizingIndex('Coil:Heating:Steam', SteamBaseboard(BaseboardNum)%EquipID, &
        //                    SteamBaseboard(BaseboardNum)%SteamInletNode, &
        //                    SteamBaseboard(BaseboardNum)%SteamOutletNode, ErrorsFound)

        if (PltSizSteamNum > 0) {

            state.dataSize->DataScalableCapSizingON = false;

            if (CurZoneEqNum > 0) {

                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax == AutoSize) {
                    IsAutoSize = true;
                }
                if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) {
                    if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax > 0.0) {
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                                     "User-Specified Maximum Water Flow Rate [m3/s]",
                                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax);
                    }
                } else {
                    CheckZoneSizing(state,
                                    state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);

                    CompType = state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam;
                    CompName = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID;
                    state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;
                    state.dataSize->DataZoneNumber = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr;
                    SizingMethod = HeatingCapacitySizing;
                    FieldNum = 1;
                    PrintFlag = false;
                    SizingString = state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields(BaseboardNum).FieldNames(FieldNum) + " [W]";
                    CapSizingMethod = SteamBaseboardDesignDataObject.HeatingCapMethod;
                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {

                        if (CapSizingMethod == HeatingDesignCapacity) {
                            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                                CheckZoneSizing(state, CompType, CompName);
                                ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = state.dataSize->FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            }
                            TempSize = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity;
                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity *
                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                            TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                            state.dataSize->DataScalableCapSizingON = true;
                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                            CheckZoneSizing(state, CompType, CompName);
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            state.dataSize->DataFracOfAutosizedHeatingCapacity =
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = state.dataSize->FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            TempSize = AutoSize;
                            state.dataSize->DataScalableCapSizingON = true;
                        } else {
                            TempSize = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity;
                        }
                        bool errorsFound = false;
                        HeatingCapacitySizer sizerHeatingCapacity;
                        sizerHeatingCapacity.overrideSizingString(SizingString);
                        sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                        state.dataSize->DataScalableCapSizingON = false;
                    } else {
                        DesCoilLoad = 0.0; // FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                    }

                    if (DesCoilLoad >= SmallLoad) {
                        SteamInletTemp = 100.0;
                        EnthSteamInDry = GetSatEnthalpyRefrig(state,
                                                              fluidNameSteam,
                                                              SteamInletTemp,
                                                              1.0,
                                                              state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex,
                                                              RoutineName);
                        EnthSteamOutWet = GetSatEnthalpyRefrig(state,
                                                               fluidNameSteam,
                                                               SteamInletTemp,
                                                               0.0,
                                                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex,
                                                               RoutineName);
                        LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                        SteamDensity = GetSatDensityRefrig(state,
                                                           fluidNameSteam,
                                                           SteamInletTemp,
                                                           1.0,
                                                           state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex,
                                                           RoutineName);
                        Cp = GetSatSpecificHeatRefrig(state,
                                                      fluidNameSteam,
                                                      SteamInletTemp,
                                                      0.0,
                                                      state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex,
                                                      RoutineName);

                        SteamVolFlowRateMaxDes =
                            DesCoilLoad /
                            (SteamDensity * (LatentHeatSteam + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).DegOfSubcooling * Cp));
                    } else {
                        SteamVolFlowRateMaxDes = 0.0;
                    }

                    if (IsAutoSize) {
                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax = SteamVolFlowRateMaxDes;
                        BaseSizer::reportSizerOutput(state,
                                                     state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                                     "Design Size Maximum Steam Flow Rate [m3/s]",
                                                     SteamVolFlowRateMaxDes);
                    } else { // Hard size with sizing data
                        if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax > 0.0 &&
                            SteamVolFlowRateMaxDes > 0.0) {
                            SteamVolFlowRateMaxUser = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax;
                            BaseSizer::reportSizerOutput(state,
                                                         state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                                         state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                                         "Design Size Maximum Steam Flow Rate [m3/s]",
                                                         SteamVolFlowRateMaxDes,
                                                         "User-Speicified Maximum Steam Flow Rate [m3/s]",
                                                         SteamVolFlowRateMaxUser);
                            if (state.dataGlobal->DisplayExtraWarnings) {
                                // Report difference between design size and user-specified values
                                if ((std::abs(SteamVolFlowRateMaxDes - SteamVolFlowRateMaxUser) / SteamVolFlowRateMaxUser) >
                                    state.dataSize->AutoVsHardSizingThreshold) {
                                    ShowMessage(state,
                                                "SizeSteamBaseboard: Potential issue with equipment sizing for "
                                                "ZoneHVAC:Baseboard:RadiantConvective:Steam=\"" +
                                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID + "\".");
                                    ShowContinueError(state,
                                                      format("User-Specified Maximum Steam Flow Rate of {:.5R} [m3/s]", SteamVolFlowRateMaxUser));
                                    ShowContinueError(
                                        state, format("differs from Design Size Maximum Steam Flow Rate of {:.5R} [m3/s]", SteamVolFlowRateMaxDes));
                                    ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                                }
                            }
                        }
                    }
                }
            }
        } else {
            if (IsAutoSize) {
                // if there is no heating Sizing:Plant object and autosizng was requested, issue an error message
                // first error will be issued by MyPlantSizingIndex
                ShowSevereError(state, "Autosizing of steam baseboard requires a heating loop Sizing:Plant object");
                ShowContinueError(state, "Occurs in Baseboard Heater=" + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                ErrorsFound = true;
            }
        }

        RegisterPlantCompDesignFlow(state,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax);

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
    }

    void CalcSteamBaseboard(EnergyPlusData &state, int &BaseboardNum, Real64 &LoadMet)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   September 2009
        //       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calculates both the convective and radiant heat transfer rate
        // of steam baseboard heaters. The heater is assumed to be crossflow with
        // both fluids unmixed. The air flow is bouyancy driven and a constant airflow.

        // METHODOLOGY EMPLOYED:
        // Equations that calculates heating capacity of steam coils and outlet air and water temperatures
        // of the zone control steam coil in steam coil module in EnergyPlus are employed.

        // REFERENCES:

        // Using/Aliasing
        using DataHVACGlobals::SmallLoad;
        using FluidProperties::GetSatDensityRefrig;
        using FluidProperties::GetSatEnthalpyRefrig;
        using FluidProperties::GetSatSpecificHeatRefrig;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static constexpr std::string_view RoutineName("CalcSteamBaseboard");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ZoneNum;
        Real64 RadHeat;
        Real64 SteamBBHeat;
        Real64 SteamInletTemp;
        Real64 SteamOutletTemp;
        Real64 SteamMassFlowRate;
        Real64 SubcoolDeltaT;
        Real64 QZnReq;
        Real64 EnthSteamInDry;
        Real64 EnthSteamOutWet;
        Real64 LatentHeatSteam;
        Real64 Cp;

        SteamBaseboardDesignData SteamBaseboardDesignDataObject{state.dataSteamBaseboardRadiator->SteamBaseboardDesign(
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).DesignObjectPtr)}; // Contains the data for variable flow hydronic systems

        ZoneNum = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr;
        QZnReq = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        SteamInletTemp = state.dataLoopNodes->Node(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode).Temp;
        SteamOutletTemp = SteamInletTemp;
        SteamMassFlowRate = state.dataLoopNodes->Node(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode).MassFlowRate;
        SubcoolDeltaT = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).DegOfSubcooling;

        if (QZnReq > SmallLoad && !state.dataZoneEnergyDemand->CurDeadBandOrSetback(ZoneNum) && SteamMassFlowRate > 0.0 &&
            GetCurrentScheduleValue(state, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr) > 0) {
            // Unit is on
            EnthSteamInDry = GetSatEnthalpyRefrig(
                state, fluidNameSteam, SteamInletTemp, 1.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);
            EnthSteamOutWet = GetSatEnthalpyRefrig(
                state, fluidNameSteam, SteamInletTemp, 0.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);
            LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
            Cp = GetSatSpecificHeatRefrig(
                state, fluidNameSteam, SteamInletTemp, 0.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);
            SteamBBHeat = SteamMassFlowRate * (LatentHeatSteam + SubcoolDeltaT * Cp); // Baseboard heating rate
            SteamOutletTemp = SteamInletTemp - SubcoolDeltaT;                         // Outlet temperature of steam
            // Estimate radiant heat addition
            RadHeat = SteamBBHeat * SteamBaseboardDesignDataObject.FracRadiant; // Radiant heating rate
            state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) =
                RadHeat; // Radiant heat source which will be distributed to surfaces and people

            // Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
            DistributeBBSteamRadGains(state);
            // Now "simulate" the system by recalculating the heat balances
            HeatBalanceSurfaceManager::CalcHeatBalanceOutsideSurf(state, ZoneNum);
            HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf(state, ZoneNum);

            // Here an assumption is made regarding radiant heat transfer to people.
            // While the radiant heat transfer to people array will be used by the thermal comfort
            // routines, the energy transfer to people would get lost from the perspective
            // of the heat balance.  So, to avoid this net loss of energy which clearly
            // gets added to the zones, we must account for it somehow.  This assumption
            // that all energy radiated to people is converted to convective energy is
            // not very precise, but at least it conserves energy. The system impact to heat balance
            // should include this.

            // Actual system load that the unit should meet
            LoadMet = (SumHATsurf(state, ZoneNum) - state.dataSteamBaseboardRadiator->ZeroSourceSumHATsurf(ZoneNum)) +
                      (SteamBBHeat * state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracConvect) +
                      (RadHeat * SteamBaseboardDesignDataObject.FracDistribPerson);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy =
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletEnthalpy - SteamBBHeat / SteamMassFlowRate;
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletQuality = 0.0;
        } else {
            SteamOutletTemp = SteamInletTemp;
            SteamBBHeat = 0.0;
            LoadMet = 0.0;
            RadHeat = 0.0;
            SteamMassFlowRate = 0.0;
            state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) = 0.0;
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletQuality = 0.0;
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy =
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletEnthalpy;
        }

        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletTemp = SteamOutletTemp;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRate = SteamMassFlowRate;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy =
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletQuality =
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletQuality;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotPower = LoadMet;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Power = SteamBBHeat;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvPower = SteamBBHeat - RadHeat;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadPower = RadHeat;
    }

    void UpdateSteamBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Russ Taylor
        //                      Rick Strand
        //       DATE WRITTEN   Nov 1997
        //                      February 2001
        //       MODIFIED       Sep 2009 Daeho Kang (add radiant component)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:

        // METHODOLOGY EMPLOYED:
        // The update subrotines both in high temperature radiant radiator
        // and convective only baseboard radiator are combined and modified.

        using PlantUtilities::SafeCopyPlantNode;

        int SteamInletNode;
        int SteamOutletNode;

        // First, update the running average if necessary...
        if (state.dataSteamBaseboardRadiator->LastSysTimeElapsed(BaseboardNum) == state.dataHVACGlobal->SysTimeElapsed) {
            state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg(BaseboardNum) -= state.dataSteamBaseboardRadiator->LastQBBSteamRadSrc(BaseboardNum) *
                                                                                 state.dataSteamBaseboardRadiator->LastTimeStepSys(BaseboardNum) /
                                                                                 state.dataGlobal->TimeStepZone;
        }
        // Update the running average and the "last" values with the current values of the appropriate variables
        state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg(BaseboardNum) +=
            state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) * state.dataHVACGlobal->TimeStepSys / state.dataGlobal->TimeStepZone;

        state.dataSteamBaseboardRadiator->LastQBBSteamRadSrc(BaseboardNum) = state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum);
        state.dataSteamBaseboardRadiator->LastSysTimeElapsed(BaseboardNum) = state.dataHVACGlobal->SysTimeElapsed;
        state.dataSteamBaseboardRadiator->LastTimeStepSys(BaseboardNum) = state.dataHVACGlobal->TimeStepSys;

        SteamInletNode = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode;
        SteamOutletNode = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletNode;

        // Set the outlet air nodes of the Baseboard
        // Set the outlet water nodes for the Coil
        SafeCopyPlantNode(state, SteamInletNode, SteamOutletNode);
        state.dataLoopNodes->Node(SteamOutletNode).Temp = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletTemp;
        state.dataLoopNodes->Node(SteamOutletNode).Enthalpy = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy;
    }

    void UpdateBBSteamRadSourceValAvg(EnergyPlusData &state, bool &SteamBaseboardSysOn) // .TRUE. if the radiant system has run this zone time step
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       Aug 2009 Daeho Kang (modify only for baseboard)
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To transfer the average value of the heat source over the entire
        // zone time step back to the heat balance routines so that the heat
        // balance algorithms can simulate one last time with the average source
        // to maintain some reasonable amount of continuity and energy balance
        // in the temperature and flux histories.

        // METHODOLOGY EMPLOYED:
        // All of the record keeping for the average term is done in the Update
        // routine so the only other thing that this subroutine does is check to
        // see if the system was even on.  If any average term is non-zero, then
        // one or more of the radiant systems was running.

        int BaseboardNum; // DO loop counter for surface index

        SteamBaseboardSysOn = false;

        // If this was never allocated, then there are no radiant systems in this input file (just RETURN)
        if (!allocated(state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg)) return;

        // If it was allocated, then we have to check to see if this was running at all...
        for (BaseboardNum = 1; BaseboardNum <= state.dataSteamBaseboardRadiator->NumSteamBaseboards; ++BaseboardNum) {
            if (state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg(BaseboardNum) != 0.0) {
                SteamBaseboardSysOn = true;
                break; // DO loop
            }
        }

        state.dataSteamBaseboardRadiator->QBBSteamRadSource = state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg;

        DistributeBBSteamRadGains(state); // QBBRadSource has been modified so we need to redistribute gains
    }

    void DistributeBBSteamRadGains(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Rick Strand
        //       DATE WRITTEN   February 2001
        //       MODIFIED       Aug. 2009 Daeho Kang (modify only for steam baseboard)
        //                      April 2010 Brent Griffith, max limit to protect surface temperature calcs
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // To distribute the gains from the steam basebaord heater
        // as specified in the user input file.  This includes distribution
        // of long wavelength radiant gains to surfaces and "people."

        // METHODOLOGY EMPLOYED:
        // We must cycle through all of the radiant systems because each
        // surface could feel the effect of more than one radiant system.
        // Note that the energy radiated to people is assumed to affect them
        // but them it is assumed to be convected to the air.

        using DataHeatBalFanSys::MaxRadHeatFlux;

        Real64 const SmallestArea(0.001); // Smallest area in meters squared (to avoid a divide by zero)

        int RadSurfNum;           // Counter for surfaces receiving radiation from radiant heater
        int BaseboardNum;         // Counter for the baseboard
        int SurfNum;              // Pointer to the Surface derived type
        int ZoneNum;              // Pointer to the Zone derived type
        Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface

        state.dataHeatBalFanSys->QSteamBaseboardSurf = 0.0;
        state.dataHeatBalFanSys->QSteamBaseboardToPerson = 0.0;

        for (BaseboardNum = 1; BaseboardNum <= state.dataSteamBaseboardRadiator->NumSteamBaseboards; ++BaseboardNum) {

            ZoneNum = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr;
            SteamBaseboardDesignData SteamBaseboardDesignDataObject{
                state.dataSteamBaseboardRadiator->SteamBaseboardDesign(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum)
                                                                           .DesignObjectPtr)}; // Contains the data for variable flow hydronic systems
            state.dataHeatBalFanSys->QSteamBaseboardToPerson(ZoneNum) +=
                state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) * SteamBaseboardDesignDataObject.FracDistribPerson;

            for (RadSurfNum = 1; RadSurfNum <= state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib; ++RadSurfNum) {
                SurfNum = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr(RadSurfNum);
                if (state.dataSurface->Surface(SurfNum).Area > SmallestArea) {
                    ThisSurfIntensity = (state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) *
                                         state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(RadSurfNum) /
                                         state.dataSurface->Surface(SurfNum).Area);
                    state.dataHeatBalFanSys->QSteamBaseboardSurf(SurfNum) += ThisSurfIntensity;

                    if (ThisSurfIntensity > MaxRadHeatFlux) { // CR 8074, trap for excessive intensity (throws off surface balance )
                        ShowSevereError(state, "DistributeBBSteamRadGains:  excessive thermal radiation heat flux intensity detected");
                        ShowContinueError(state, "Surface = " + state.dataSurface->Surface(SurfNum).Name);
                        ShowContinueError(state, format("Surface area = {:.3R} [m2]", state.dataSurface->Surface(SurfNum).Area));
                        ShowContinueError(state,
                                          "Occurs in " + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " +
                                              state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                        ShowContinueError(state, format("Radiation intensity = {:.2R} [W/m2]", ThisSurfIntensity));
                        ShowContinueError(
                            state, "Assign a larger surface area or more surfaces in " + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam);
                        ShowFatalError(state, "DistributeBBSteamRadGains:  excessive thermal radiation heat flux intensity detected");
                    }
                } else { // small surface
                    ShowSevereError(state, "DistributeBBSteamRadGains:  surface not large enough to receive thermal radiation heat flux");
                    ShowContinueError(state, "Surface = " + state.dataSurface->Surface(SurfNum).Name);
                    ShowContinueError(state, format("Surface area = {:.3R} [m2]", state.dataSurface->Surface(SurfNum).Area));
                    ShowContinueError(state,
                                      "Occurs in " + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " +
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                    ShowContinueError(state,
                                      "Assign a larger surface area or more surfaces in " + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam);
                    ShowFatalError(state, "DistributeBBSteamRadGains:  surface not large enough to receive thermal radiation heat flux");
                }
            }
        }
    }

    void ReportSteamBaseboard(EnergyPlusData &state, int const BaseboardNum)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Daeho Kang
        //       DATE WRITTEN   September 2009
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotEnergy =
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotPower * state.dataHVACGlobal->TimeStepSys *
            DataGlobalConstants::SecInHour;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Energy =
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvEnergy =
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvPower * state.dataHVACGlobal->TimeStepSys *
            DataGlobalConstants::SecInHour;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadEnergy =
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadPower * state.dataHVACGlobal->TimeStepSys *
            DataGlobalConstants::SecInHour;
    }

    Real64 SumHATsurf(EnergyPlusData &state, int const ZoneNum) // Zone number
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   July 2003
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
        // The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
        // and should be updated accordingly.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // Using/Aliasing
        using namespace DataSurfaces;
        using namespace DataHeatBalance;
        using namespace DataHeatBalSurface;

        // Return value
        Real64 SumHATsurf;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int SurfNum; // Surface number
        Real64 Area; // Effective surface area

        SumHATsurf = 0.0;

        for (SurfNum = state.dataHeatBal->Zone(ZoneNum).HTSurfaceFirst; SurfNum <= state.dataHeatBal->Zone(ZoneNum).HTSurfaceLast; ++SurfNum) {
            Area = state.dataSurface->Surface(SurfNum).Area;

            if (state.dataSurface->Surface(SurfNum).Class == SurfaceClass::Window) {
                if (ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                    // The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
                    Area += state.dataSurface->SurfWinDividerArea(SurfNum);
                }

                if (state.dataSurface->SurfWinFrameArea(SurfNum) > 0.0) {
                    // Window frame contribution
                    SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinFrameArea(SurfNum) *
                                  (1.0 + state.dataSurface->SurfWinProjCorrFrIn(SurfNum)) * state.dataSurface->SurfWinFrameTempSurfIn(SurfNum);
                }

                if (state.dataSurface->SurfWinDividerArea(SurfNum) > 0.0 &&
                    !ANY_INTERIOR_SHADE_BLIND(state.dataSurface->SurfWinShadingFlag(SurfNum))) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * state.dataSurface->SurfWinDividerArea(SurfNum) *
                                  (1.0 + 2.0 * state.dataSurface->SurfWinProjCorrDivIn(SurfNum)) *
                                  state.dataSurface->SurfWinDividerTempSurfIn(SurfNum);
                }
            }

            SumHATsurf += state.dataHeatBal->HConvIn(SurfNum) * Area * state.dataHeatBalSurf->TempSurfInTmp(SurfNum);
        }

        return SumHATsurf;
    }

    void UpdateSteamBaseboardPlantConnection(EnergyPlusData &state,
                                             int const BaseboardTypeNum,               // type index
                                             std::string const &BaseboardName,         // component name
                                             [[maybe_unused]] int const EquipFlowCtrl, // Flow control mode for the equipment
                                             [[maybe_unused]] int const LoopNum,       // Plant loop index for where called from
                                             [[maybe_unused]] int const LoopSide,      // Plant loop side index for where called from
                                             int &CompIndex,                           // Chiller number pointer
                                             [[maybe_unused]] bool const FirstHVACIteration,
                                             bool &InitLoopEquip // If not zero, calculate the max load for operating conditions
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Chandan Sharma
        //       DATE WRITTEN   Sept. 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // update sim routine called from plant

        // METHODOLOGY EMPLOYED:
        // check input, provide comp index, call utility routines

        // REFERENCES:
        // Based on UpdateBaseboardPlantConnection from Brent Griffith, Sept 2010

        // Using/Aliasing
        using DataPlant::ccSimPlantEquipTypes;
        using DataPlant::TypeOf_Baseboard_Rad_Conv_Steam;

        using PlantUtilities::PullCompInterconnectTrigger;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int BaseboardNum;

        // Find the correct baseboard
        if (CompIndex == 0) {
            BaseboardNum =
                UtilityRoutines::FindItemInList(BaseboardName, state.dataSteamBaseboardRadiator->SteamBaseboard, &SteamBaseboardParams::EquipID);
            if (BaseboardNum == 0) {
                ShowFatalError(state, "UpdateSteamBaseboardPlantConnection: Specified baseboard not valid =" + BaseboardName);
            }
            CompIndex = BaseboardNum;
        } else {
            BaseboardNum = CompIndex;
            if (BaseboardNum > state.dataSteamBaseboardRadiator->NumSteamBaseboards || BaseboardNum < 1) {
                ShowFatalError(
                    state,
                    format("UpdateSteamBaseboardPlantConnection:  Invalid CompIndex passed={}, Number of baseboards={}, Entered baseboard name={}",
                           BaseboardNum,
                           state.dataSteamBaseboardRadiator->NumSteamBaseboards,
                           BaseboardName));
            }
            if (state.dataGlobal->KickOffSimulation) {
                if (BaseboardName != state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID) {
                    ShowFatalError(state,
                                   format("UpdateSteamBaseboardPlantConnection: Invalid CompIndex passed={}, baseboard name={}, stored baseboard "
                                          "Name for that index={}",
                                          BaseboardNum,
                                          BaseboardName,
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID));
                }
                if (BaseboardTypeNum != TypeOf_Baseboard_Rad_Conv_Steam) {
                    ShowFatalError(state,
                                   format("UpdateSteamBaseboardPlantConnection: Invalid CompIndex passed={}, baseboard name={}, stored baseboard "
                                          "Name for that index={}",
                                          BaseboardNum,
                                          BaseboardName,
                                          ccSimPlantEquipTypes(BaseboardTypeNum)));
                }
            }
        }

        if (InitLoopEquip) {
            return;
        }

        PullCompInterconnectTrigger(state,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BBLoadReSimIndex,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    DataPlant::iCriteriaType::HeatTransferRate,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Power);

        PullCompInterconnectTrigger(state,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BBLoadReSimIndex,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    DataPlant::iCriteriaType::MassFlowRate,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRate);

        PullCompInterconnectTrigger(state,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BBLoadReSimIndex,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    DataPlant::iCriteriaType::Temperature,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletTemp);
    }

} // namespace SteamBaseboardRadiator

} // namespace EnergyPlus
