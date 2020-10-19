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

// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchAirLoopPlant.hh>
#include <EnergyPlus/DataEnvironment.hh>
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
#include <EnergyPlus/Psychrometrics.hh>
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

    using namespace DataGlobals;
    using DataHVACGlobals::SmallLoad;
    using DataHVACGlobals::SysTimeElapsed;
    using DataHVACGlobals::TimeStepSys;
    using DataLoopNode::Node;
    using DataLoopNode::NodeConnectionType_Inlet;
    using DataLoopNode::NodeConnectionType_Outlet;
    using DataLoopNode::NodeType_Steam;
    using DataLoopNode::ObjectIsNotParent;
    using DataPlant::PlantLoop;
    using DataPlant::TypeOf_Baseboard_Rad_Conv_Steam;
    using DataZoneEquipment::CheckZoneEquipmentList;
    using DataZoneEquipment::ZoneEquipConfig;
    using DataZoneEquipment::ZoneEquipInputsFilled;

    // Use statements for access to subroutines in other modules
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;

    static std::string const fluidNameSteam("STEAM");

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

        // Using/Aliasing
        using DataZoneEnergyDemands::CurDeadBandOrSetback;
        using DataZoneEnergyDemands::ZoneSysEnergyDemand;
        using General::TrimSigDigits;
        using PlantUtilities::SetComponentFlowRate;
        using ScheduleManager::GetCurrentScheduleValue;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int BaseboardNum; // index of unit in baseboard array
        Real64 QZnReq;    // zone load not yet satisfied
        Real64 MaxSteamFlow;
        Real64 MinSteamFlow;
        static Real64 mdot(0.0);

        if (state.dataSteamBaseboardRadiator->GetInputFlag) {
            GetSteamBaseboardInput(state);
            state.dataSteamBaseboardRadiator->GetInputFlag = false;
        }

        // Find the correct Baseboard Equipment
        if (CompIndex == 0) {
            BaseboardNum = UtilityRoutines::FindItemInList(EquipName, state.dataSteamBaseboardRadiator->SteamBaseboard, &SteamBaseboardParams::EquipID);
            if (BaseboardNum == 0) {
                ShowFatalError("SimSteamBaseboard: Unit not found=" + EquipName);
            }
            CompIndex = BaseboardNum;
        } else {
            BaseboardNum = CompIndex;
            if (BaseboardNum > state.dataSteamBaseboardRadiator->NumSteamBaseboards || BaseboardNum < 1) {
                ShowFatalError("SimSteamBaseboard:  Invalid CompIndex passed=" + TrimSigDigits(BaseboardNum) +
                               ", Number of Units=" + TrimSigDigits(state.dataSteamBaseboardRadiator->NumSteamBaseboards) + ", Entered Unit name=" + EquipName);
            }
            if (state.dataSteamBaseboardRadiator->CheckEquipName(BaseboardNum)) {
                if (EquipName != state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID) {
                    ShowFatalError("SimSteamBaseboard: Invalid CompIndex passed=" + TrimSigDigits(BaseboardNum) + ", Unit name=" + EquipName +
                                   ", stored Unit Name for that index=" + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                }
                state.dataSteamBaseboardRadiator->CheckEquipName(BaseboardNum) = false;
            }
        }

        if (CompIndex > 0) {

            InitSteamBaseboard(state, BaseboardNum, ControlledZoneNum, FirstHVACIteration);

            QZnReq = ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputReqToHeatSP;

            if (QZnReq > SmallLoad && !CurDeadBandOrSetback(ActualZoneNum) &&
                (GetCurrentScheduleValue(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr) > 0.0)) {

                // On the first HVAC iteration the system values are given to the controller, but after that
                // the demand limits are in place and there needs to be feedback to the Zone Equipment
                if (FirstHVACIteration) {
                    MaxSteamFlow = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRateMax;
                    MinSteamFlow = 0.0;
                } else {
                    MaxSteamFlow = Node(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode).MassFlowRateMaxAvail;
                    MinSteamFlow = Node(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode).MassFlowRateMinAvail;
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
                                          state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Offset,
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
                        ShowSevereError("SimSteamBaseboard: Errors in Baseboard=" + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                        ShowContinueError("Invalid or unimplemented equipment type=" + TrimSigDigits(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipType));
                        ShowFatalError("Preceding condition causes termination.");
                    }
                }

                PowerMet = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotPower;
            } else {
                // baseboard is off, don't bother going into ControlCompOutput
                mdot = 0.0;
                SetComponentFlowRate(mdot,
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
            ShowFatalError("SimSteamBaseboard: Unit not found=" + EquipName);
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
        using DataSurfaces::Surface;
        using FluidProperties::FindRefrigerant;
        using General::RoundSigDigits;
        using General::TrimSigDigits;
        using GlobalNames::VerifyUniqueBaseboardName;
        using NodeInputManager::GetOnlySingleNode;
        using ScheduleManager::GetCurrentScheduleValue;
        using ScheduleManager::GetScheduleIndex;
        using namespace DataIPShortCuts;
        using namespace DataSizing;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetSteamBaseboardInput:");
        Real64 const MaxFraction(1.0);       // Maximum limit of fractional values
        Real64 const MinFraction(0.0);       // Minimum limit of fractional values
        Real64 const MaxSteamFlowRate(10.0); // Maximum limit of steam volume flow rate in m3/s
        Real64 const MinSteamFlowRate(0.0);  // Minimum limit of steam volume flow rate in m3/s
        //    INTEGER,PARAMETER :: MaxDistribSurfaces   = 20          ! Maximum number of surfaces that a baseboard heater can radiate to
        int const MinDistribSurfaces(1);            // Minimum number of surfaces that a baseboard heater can radiate to
        int const iHeatCAPMAlphaNum(5);             // get input index to steam baseboard Radiator system heating capacity sizing method
        int const iHeatDesignCapacityNumericNum(1); // get input index to steam baseboard Radiator system electric heating capacity
        int const iHeatCapacityPerFloorAreaNumericNum(
            2); // get input index to steam baseboard Radiator system electric heating capacity per floor area sizing
        int const iHeatFracOfAutosizedCapacityNumericNum(
            3); //  get input index to steam baseboard Radiator system electric heating capacity sizing as fraction of autozized heating capacity

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Real64 AllFracsSummed; // Sum of the fractions radiant
        int BaseboardNum;      // Baseboard number
        int NumAlphas;         // Number of Alphas for each GetobjectItem call
        int NumNumbers;        // Number of Numbers for each GetobjectItem call
        int SurfNum;           // Surface number Do loop counter
        int IOStat;
        bool ErrorsFound(false); // If errors detected in input
        bool SteamMessageNeeded;

        SteamMessageNeeded = true;
        state.dataSteamBaseboardRadiator->NumSteamBaseboards = inputProcessor->getNumObjectsFound(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam);

        // Count total number of baseboard units

        state.dataSteamBaseboardRadiator->SteamBaseboard.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboards);
        state.dataSteamBaseboardRadiator->CheckEquipName.dimension(state.dataSteamBaseboardRadiator->NumSteamBaseboards, true);
        state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields.allocate(state.dataSteamBaseboardRadiator->NumSteamBaseboards);

        // Get the data from the user input related to baseboard heaters
        for (BaseboardNum = 1; BaseboardNum <= state.dataSteamBaseboardRadiator->NumSteamBaseboards; ++BaseboardNum) {

            inputProcessor->getObjectItem(state,
                                          state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                          BaseboardNum,
                                          cAlphaArgs,
                                          NumAlphas,
                                          rNumericArgs,
                                          NumNumbers,
                                          IOStat,
                                          lNumericFieldBlanks,
                                          lAlphaFieldBlanks,
                                          cAlphaFieldNames,
                                          cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(cAlphaArgs(1), state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam, ErrorsFound);
            state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields(BaseboardNum).FieldNames.allocate(NumNumbers);
            state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields(BaseboardNum).FieldNames = "";
            state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields(BaseboardNum).FieldNames = cNumericFieldNames;

            // ErrorsFound will be set to True if problem was found, left untouched otherwise
            VerifyUniqueBaseboardName(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam, cAlphaArgs(1), ErrorsFound, state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " Name");

            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID = cAlphaArgs(1);                     // Name of the baseboard
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipType = TypeOf_Baseboard_Rad_Conv_Steam; //'ZoneHVAC:Baseboard:RadiantConvective:Steam'

            // Get schedule
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Schedule = cAlphaArgs(2);
            if (lAlphaFieldBlanks(2)) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr = ScheduleAlwaysOn;
            } else {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr == 0) {
                    ShowSevereError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cAlphaFieldNames(2) + "=\"" +
                                    cAlphaArgs(2) + "\" not found.");
                    ErrorsFound = true;
                }
            }

            // Get inlet node number
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode = GetOnlySingleNode(state,
                cAlphaArgs(3), ErrorsFound, state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam, cAlphaArgs(1), NodeType_Steam, NodeConnectionType_Inlet, 1, ObjectIsNotParent);

            // Get outlet node number
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletNode = GetOnlySingleNode(state,
                cAlphaArgs(4), ErrorsFound, state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam, cAlphaArgs(1), NodeType_Steam, NodeConnectionType_Outlet, 1, ObjectIsNotParent);
            TestCompSet(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam, cAlphaArgs(1), cAlphaArgs(3), cAlphaArgs(4), "Hot Steam Nodes");

            // Determine steam baseboard radiator system heating design capacity sizing method
            if (UtilityRoutines::SameString(cAlphaArgs(iHeatCAPMAlphaNum), "HeatingDesignCapacity")) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).HeatingCapMethod = HeatingDesignCapacity;

                if (!lNumericFieldBlanks(iHeatDesignCapacityNumericNum)) {
                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity = rNumericArgs(iHeatDesignCapacityNumericNum);
                    if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity < 0.0 && state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity != AutoSize) {
                        ShowSevereError(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                        ShowContinueError("Illegal " + cNumericFieldNames(iHeatDesignCapacityNumericNum) + " = " +
                                          TrimSigDigits(rNumericArgs(iHeatDesignCapacityNumericNum), 7));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                    ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError("Blank field not allowed for " + cNumericFieldNames(iHeatDesignCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(iHeatCAPMAlphaNum), "CapacityPerFloorArea")) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).HeatingCapMethod = CapacityPerFloorArea;
                if (!lNumericFieldBlanks(iHeatCapacityPerFloorAreaNumericNum)) {
                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity = rNumericArgs(iHeatCapacityPerFloorAreaNumericNum);
                    if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity <= 0.0) {
                        ShowSevereError(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                        ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError("Illegal " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = " +
                                          TrimSigDigits(rNumericArgs(iHeatCapacityPerFloorAreaNumericNum), 7));
                        ErrorsFound = true;
                    } else if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                        ShowSevereError(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                        ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                        ShowContinueError("Illegal " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum) + " = Autosize");
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                    ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError("Blank field not allowed for " + cNumericFieldNames(iHeatCapacityPerFloorAreaNumericNum));
                    ErrorsFound = true;
                }
            } else if (UtilityRoutines::SameString(cAlphaArgs(iHeatCAPMAlphaNum), "FractionOfAutosizedHeatingCapacity")) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).HeatingCapMethod = FractionOfAutosizedHeatingCapacity;
                if (!lNumericFieldBlanks(iHeatFracOfAutosizedCapacityNumericNum)) {
                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity = rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum);
                    if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity < 0.0) {
                        ShowSevereError(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                        ShowContinueError("Illegal " + cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum) + " = " +
                                          TrimSigDigits(rNumericArgs(iHeatFracOfAutosizedCapacityNumericNum), 7));
                        ErrorsFound = true;
                    }
                } else {
                    ShowSevereError(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                    ShowContinueError("Input for " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                    ShowContinueError("Blank field not allowed for " + cNumericFieldNames(iHeatFracOfAutosizedCapacityNumericNum));
                    ErrorsFound = true;
                }
            } else {
                ShowSevereError(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                ShowContinueError("Illegal " + cAlphaFieldNames(iHeatCAPMAlphaNum) + " = " + cAlphaArgs(iHeatCAPMAlphaNum));
                ErrorsFound = true;
            }

            // Desired degree of cooling
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).DegOfSubcooling = rNumericArgs(4);
            // Maximum steam flow rate
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax = rNumericArgs(5);
            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax >= MaxSteamFlowRate) {
                ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(5) +
                                 " was higher than the allowable maximum.");
                ShowContinueError("...reset to maximum value=[" + RoundSigDigits(MaxSteamFlowRate, 2) + "].");
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax = MaxSteamFlowRate;
            } else if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax <= MinSteamFlowRate &&
                       state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax != AutoSize) {
                ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(2) +
                                 " was less than the allowable minimum.");
                ShowContinueError("...reset to minimum value=[" + RoundSigDigits(MinSteamFlowRate, 2) + "].");
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax = MinSteamFlowRate;
            }

            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Offset = rNumericArgs(6);
            // Set default convergence tolerance
            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Offset <= 0.0) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Offset = 0.001;
                ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(6) +
                                 " was less than the allowable minimum.");
                ShowContinueError("...reset to default value=[0.001].");
            }
            // Fraction of radiant heat out of the total heating rate of the unit
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant = rNumericArgs(7);
            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant < MinFraction) {
                ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(7) +
                                 " was lower than the allowable minimum.");
                ShowContinueError("...reset to minimum value=[" + RoundSigDigits(MinFraction, 3) + "].");
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant = MinFraction;
            } else if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant > MaxFraction) {
                ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(7) +
                                 " was higher than the allowable maximum.");
                ShowContinueError("...reset to maximum value=[" + RoundSigDigits(MaxFraction, 3) + "].");
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant = MaxFraction;
            }

            // Remaining fraction is added to the zone as convective heat transfer
            AllFracsSummed = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant;
            if (AllFracsSummed > MaxFraction) {
                ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) +
                                 "\", Fraction Radiant was higher than the allowable maximum.");
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant = MaxFraction;
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracConvect = 0.0;
            } else {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracConvect = 1.0 - AllFracsSummed;
            }
            // Fraction of radiant heat addition to the people within the radiant heating capacity specified by the user
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribPerson = rNumericArgs(8);
            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribPerson < MinFraction) {
                ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(8) +
                                 " was lower than the allowable minimum.");
                ShowContinueError("...reset to minimum value=[" + RoundSigDigits(MinFraction, 3) + "].");
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribPerson = MinFraction;
            }
            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribPerson > MaxFraction) {
                ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(8) +
                                 " was higher than the allowable maximum.");
                ShowContinueError("...reset to maximum value=[" + RoundSigDigits(MaxFraction, 3) + "].");
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribPerson = MaxFraction;
            }
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib = NumNumbers - 8;
            //      IF (SteamBaseboard(BaseboardNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
            //        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//TRIM(cAlphaArgs(1))// &
            //          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
            //        CALL ShowContinueError('...only the maximum value=['//TRIM(RoundSigDigits(MaxDistribSurfaces))//  &
            //           '] will be processed.')
            //        SteamBaseboard(BaseboardNum)%TotSurfToDistrib = MaxDistribSurfaces
            //      END IF
            if ((state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib < MinDistribSurfaces) && (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant > MinFraction)) {
                ShowSevereError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) +
                                "\", the number of surface/radiant fraction groups entered was less than the allowable minimum.");
                ShowContinueError("...the minimum that must be entered=[" + RoundSigDigits(MinDistribSurfaces) + "].");
                ErrorsFound = true;
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib = 0;
            }
            // Allocate the surfaces and fractions
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfaceName.allocate(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfaceName = "";
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr.allocate(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr = 0;
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf.allocate(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf = 0.0;

            // search zone equipment list structure for zone index
            for (int ctrlZone = 1; ctrlZone <= DataGlobals::NumOfZones; ++ctrlZone) {
                for (int zoneEquipTypeNum = 1; zoneEquipTypeNum <= DataZoneEquipment::ZoneEquipList(ctrlZone).NumOfEquipTypes; ++zoneEquipTypeNum) {
                    if (DataZoneEquipment::ZoneEquipList(ctrlZone).EquipType_Num(zoneEquipTypeNum) == DataZoneEquipment::BBSteam_Num &&
                        DataZoneEquipment::ZoneEquipList(ctrlZone).EquipName(zoneEquipTypeNum) == state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID) {
                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr = ctrlZone;
                    }
                }
            }
            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr <= 0) {
                ShowSevereError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID +
                                "\" is not on any ZoneHVAC:EquipmentList.");
                ErrorsFound = true;
                continue;
            }

            AllFracsSummed = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribPerson;
            for (SurfNum = 1; SurfNum <= state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib; ++SurfNum) {
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfaceName(SurfNum) = cAlphaArgs(SurfNum + 5);
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr(SurfNum) =
                    HeatBalanceIntRadExchange::GetRadiantSystemSurface(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                                                       state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                                                       state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr,
                                                                       state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfaceName(SurfNum),
                                                                       ErrorsFound);
                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) = rNumericArgs(SurfNum + 8);
                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) > MaxFraction) {
                    ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(SurfNum + 8) +
                                     "was greater than the allowable maximum.");
                    ShowContinueError("...reset to maximum value=[" + RoundSigDigits(MaxFraction, 1) + "].");
                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib = MaxFraction;
                }
                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(SurfNum) < MinFraction) {
                    ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) + "\", " + cNumericFieldNames(SurfNum + 8) +
                                     "was less than the allowable minimum.");
                    ShowContinueError("...reset to maximum value=[" + RoundSigDigits(MinFraction, 1) + "].");
                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib = MinFraction;
                }
                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr(SurfNum) != 0) {
                    Surface(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr(SurfNum)).IntConvSurfGetsRadiantHeat = true;
                }

                AllFracsSummed += state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(SurfNum);
            } // surfaces

            if (AllFracsSummed > (MaxFraction + 0.01)) {
                ShowSevereError("Fraction of radiation distributed to surfaces sums up to greater than 1 for " + cAlphaArgs(1));
                ShowContinueError("Occurs in Baseboard Heater=" + cAlphaArgs(1));
                ErrorsFound = true;
            }
            if ((AllFracsSummed < (MaxFraction - 0.01)) &&
                (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant >
                 MinFraction)) { // User didn't distribute all of the | radiation warn that some will be lost
                ShowWarningError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "=\"" + cAlphaArgs(1) +
                                 "\", Summed radiant fractions for people + surface groups < 1.0");
                ShowContinueError("The rest of the radiant energy delivered by the baseboard heater will be lost");
            }

            if (state.dataSteamBaseboardRadiator->SteamIndex == 0 && BaseboardNum == 1) {
                state.dataSteamBaseboardRadiator->SteamIndex = FindRefrigerant(state, "Steam");
                if (state.dataSteamBaseboardRadiator->SteamIndex == 0) {
                    ShowSevereError(RoutineName + "Steam Properties for " + cAlphaArgs(1) + " not found.");
                    if (SteamMessageNeeded) ShowContinueError("Steam Fluid Properties should have been included in the input file.");
                    ErrorsFound = true;
                    SteamMessageNeeded = false;
                }
            }

            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex = state.dataSteamBaseboardRadiator->SteamIndex;
        }

        if (ErrorsFound) {
            ShowFatalError(RoutineName + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + "Errors found getting input. Program terminates.");
        }

        // Setup Report variables for the Coils
        for (BaseboardNum = 1; BaseboardNum <= state.dataSteamBaseboardRadiator->NumSteamBaseboards; ++BaseboardNum) {
            // CurrentModuleObject='ZoneHVAC:Baseboard:RadiantConvective:Steam'
            SetupOutputVariable(state, "Baseboard Total Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotPower,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);

            SetupOutputVariable(state, "Baseboard Convective Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvPower,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Radiant Heating Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadPower,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Total Heating Energy",
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
            SetupOutputVariable(state, "Baseboard Convective Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvEnergy,
                                "System",
                                "Sum",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Radiant Heating Energy",
                                OutputProcessor::Unit::J,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadEnergy,
                                "System",
                                "Sum",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Steam Energy",
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
            SetupOutputVariable(state, "Baseboard Steam Rate",
                                OutputProcessor::Unit::W,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Power,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Steam Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRate,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Steam Inlet Temperature",
                                OutputProcessor::Unit::C,
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletTemp,
                                "System",
                                "Average",
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
            SetupOutputVariable(state, "Baseboard Steam Outlet Temperature",
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
        static std::string const RoutineName("InitSteamCoil");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        static Array1D_bool MyEnvrnFlag;
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
            state.dataSteamBaseboardRadiator->ZeroSourceSumHATsurf.dimension(NumOfZones, 0.0);
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

        if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr <= 0) state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr = ZoneEquipConfig(ControlledZoneNumSub).ActualZoneNum;

        // Need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
        if (!state.dataSteamBaseboardRadiator->ZoneEquipmentListChecked && ZoneEquipInputsFilled) {
            state.dataSteamBaseboardRadiator->ZoneEquipmentListChecked = true;
            for (Loop = 1; Loop <= state.dataSteamBaseboardRadiator->NumSteamBaseboards; ++Loop) {
                if (CheckZoneEquipmentList(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam, state.dataSteamBaseboardRadiator->SteamBaseboard(Loop).EquipID)) continue;
                ShowSevereError("InitBaseboard: Unit=[" + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + ',' + state.dataSteamBaseboardRadiator->SteamBaseboard(Loop).EquipID +
                                "] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
            }
        }

        if (state.dataSteamBaseboardRadiator->SetLoopIndexFlag(BaseboardNum)) {
            if (allocated(PlantLoop)) {
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
                    ShowFatalError("InitSteamBaseboard: Program terminated for previous conditions.");
                }
            }
        }

        if (!SysSizingCalc && state.dataSteamBaseboardRadiator->MySizeFlag(BaseboardNum) && (!state.dataSteamBaseboardRadiator->SetLoopIndexFlag(BaseboardNum))) {
            // For each coil, do the sizing once
            SizeSteamBaseboard(state, BaseboardNum);
            state.dataSteamBaseboardRadiator->MySizeFlag(BaseboardNum) = false;
        }

        // Do the Begin Environment initializations
        if (BeginEnvrnFlag && MyEnvrnFlag(BaseboardNum)) {
            // Initialize
            SteamInletNode = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode;
            Node(SteamInletNode).Temp = 100.0;
            Node(SteamInletNode).Press = 101325.0;
            SteamDensity = GetSatDensityRefrig(state, fluidNameSteam, Node(SteamInletNode).Temp, 1.0, Node(SteamInletNode).FluidIndex, RoutineName);
            StartEnthSteam = GetSatEnthalpyRefrig(state, fluidNameSteam, Node(SteamInletNode).Temp, 1.0, Node(SteamInletNode).FluidIndex, RoutineName);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRateMax = SteamDensity * state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax;
            InitComponentNodes(0.0,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRateMax,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletNode,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                               state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum);
            Node(SteamInletNode).Enthalpy = StartEnthSteam;
            Node(SteamInletNode).Quality = 1.0;
            Node(SteamInletNode).HumRat = 0.0;

            // Initializes radiant sources
            state.dataSteamBaseboardRadiator->ZeroSourceSumHATsurf = 0.0;
            state.dataSteamBaseboardRadiator->QBBSteamRadSource = 0.0;
            state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg = 0.0;
            state.dataSteamBaseboardRadiator->LastQBBSteamRadSrc = 0.0;
            state.dataSteamBaseboardRadiator->LastSysTimeElapsed = 0.0;
            state.dataSteamBaseboardRadiator->LastTimeStepSys = 0.0;

            MyEnvrnFlag(BaseboardNum) = false;
        }

        if (!BeginEnvrnFlag) {
            MyEnvrnFlag(BaseboardNum) = true;
        }

        if (BeginTimeStepFlag && FirstHVACIteration) {
            ZoneNum = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr;
            state.dataSteamBaseboardRadiator->ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum);
            state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg(BaseboardNum) = 0.0;
            state.dataSteamBaseboardRadiator->LastQBBSteamRadSrc(BaseboardNum) = 0.0;
            state.dataSteamBaseboardRadiator->LastSysTimeElapsed(BaseboardNum) = 0.0;
            state.dataSteamBaseboardRadiator->LastTimeStepSys(BaseboardNum) = 0.0;
        }

        // Do the every time step initializations
        SteamInletNode = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode;
        ZoneNode = ZoneEquipConfig(ControlledZoneNumSub).ZoneNode;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRate = Node(SteamInletNode).MassFlowRate;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletTemp = Node(SteamInletNode).Temp;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletEnthalpy = Node(SteamInletNode).Enthalpy;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletPress = Node(SteamInletNode).Press;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletQuality = Node(SteamInletNode).Quality;

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
        using FluidProperties::GetSatDensityRefrig;
        using FluidProperties::GetSatEnthalpyRefrig;
        using FluidProperties::GetSatSpecificHeatRefrig;
        using PlantUtilities::RegisterPlantCompDesignFlow;
        //  USE BranchInputManager,  ONLY: MyPlantSizingIndex
        using DataHeatBalance::Zone;
        using DataHVACGlobals::HeatingCapacitySizing;
        using General::RoundSigDigits;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("SizeSteamBaseboard");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

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

        // Find the appropriate steam plant sizing object
        PltSizSteamNum = PlantLoop(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum).PlantSizNum;
        //    PltSizSteamNum = MyPlantSizingIndex('Coil:Heating:Steam', SteamBaseboard(BaseboardNum)%EquipID, &
        //                    SteamBaseboard(BaseboardNum)%SteamInletNode, &
        //                    SteamBaseboard(BaseboardNum)%SteamOutletNode, ErrorsFound)

        if (PltSizSteamNum > 0) {

            DataScalableCapSizingON = false;

            if (CurZoneEqNum > 0) {

                if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax == AutoSize) {
                    IsAutoSize = true;
                }
                if (!IsAutoSize && !ZoneSizingRunDone) {
                    if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax > 0.0) {
                        BaseSizer::reportSizerOutput(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                                     "User-Specified Maximum Water Flow Rate [m3/s]",
                                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax);
                    }
                } else {
                    CheckZoneSizing(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);

                    CompType = state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam;
                    CompName = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID;
                    DataFracOfAutosizedHeatingCapacity = 1.0;
                    DataZoneNumber = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr;
                    SizingMethod = HeatingCapacitySizing;
                    FieldNum = 1;
                    PrintFlag = false;
                    SizingString = state.dataSteamBaseboardRadiator->SteamBaseboardNumericFields(BaseboardNum).FieldNames(FieldNum) + " [W]";
                    CapSizingMethod = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).HeatingCapMethod;
                    ZoneEqSizing(CurZoneEqNum).SizingMethod(SizingMethod) = CapSizingMethod;
                    if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                        CapSizingMethod == FractionOfAutosizedHeatingCapacity) {

                        if (CapSizingMethod == HeatingDesignCapacity) {
                            if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity == AutoSize) {
                                CheckZoneSizing(CompType, CompName);
                                ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                                ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            }
                            TempSize = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity;
                        } else if (CapSizingMethod == CapacityPerFloorArea) {
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad =
                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity * Zone(DataZoneNumber).FloorArea;
                            TempSize = ZoneEqSizing(CurZoneEqNum).DesHeatingLoad;
                            DataScalableCapSizingON = true;
                        } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                            CheckZoneSizing(CompType, CompName);
                            ZoneEqSizing(CurZoneEqNum).HeatingCapacity = true;
                            DataFracOfAutosizedHeatingCapacity = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity;
                            ZoneEqSizing(CurZoneEqNum).DesHeatingLoad = FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                            TempSize = AutoSize;
                            DataScalableCapSizingON = true;
                        } else {
                            TempSize = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ScaledHeatingCapacity;
                        }
                        bool errorsFound = false;
                        HeatingCapacitySizer sizerHeatingCapacity;
                        sizerHeatingCapacity.overrideSizingString(SizingString);
                        sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        DesCoilLoad = sizerHeatingCapacity.size(state, TempSize, errorsFound);
                        DataScalableCapSizingON = false;
                    } else {
                        DesCoilLoad = 0.0; // FinalZoneSizing(CurZoneEqNum).NonAirSysDesHeatLoad;
                    }

                    if (DesCoilLoad >= SmallLoad) {
                        SteamInletTemp = 100.0;
                        EnthSteamInDry =
                            GetSatEnthalpyRefrig(state, fluidNameSteam, SteamInletTemp, 1.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);
                        EnthSteamOutWet =
                            GetSatEnthalpyRefrig(state, fluidNameSteam, SteamInletTemp, 0.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);
                        LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
                        SteamDensity = GetSatDensityRefrig(state, fluidNameSteam, SteamInletTemp, 1.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);
                        Cp = GetSatSpecificHeatRefrig(state, fluidNameSteam, SteamInletTemp, 0.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);

                        SteamVolFlowRateMaxDes = DesCoilLoad / (SteamDensity * (LatentHeatSteam + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).DegOfSubcooling * Cp));
                    } else {
                        SteamVolFlowRateMaxDes = 0.0;
                    }

                    if (IsAutoSize) {
                        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax = SteamVolFlowRateMaxDes;
                        BaseSizer::reportSizerOutput(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                                     state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                                     "Design Size Maximum Steam Flow Rate [m3/s]",
                                                     SteamVolFlowRateMaxDes);
                    } else { // Hard size with sizing data
                        if (state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax > 0.0 && SteamVolFlowRateMaxDes > 0.0) {
                            SteamVolFlowRateMaxUser = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax;
                            BaseSizer::reportSizerOutput(state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam,
                                                         state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID,
                                                         "Design Size Maximum Steam Flow Rate [m3/s]",
                                                         SteamVolFlowRateMaxDes,
                                                         "User-Speicified Maximum Steam Flow Rate [m3/s]",
                                                         SteamVolFlowRateMaxUser);
                            if (DisplayExtraWarnings) {
                                // Report difference between design size and user-specified values
                                if ((std::abs(SteamVolFlowRateMaxDes - SteamVolFlowRateMaxUser) / SteamVolFlowRateMaxUser) >
                                    AutoVsHardSizingThreshold) {
                                    ShowMessage("SizeSteamBaseboard: Potential issue with equipment sizing for "
                                                "ZoneHVAC:Baseboard:RadiantConvective:Steam=\"" +
                                                state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID + "\".");
                                    ShowContinueError("User-Specified Maximum Steam Flow Rate of " + RoundSigDigits(SteamVolFlowRateMaxUser, 5) +
                                                      " [m3/s]");
                                    ShowContinueError("differs from Design Size Maximum Steam Flow Rate of " +
                                                      RoundSigDigits(SteamVolFlowRateMaxDes, 5) + " [m3/s]");
                                    ShowContinueError("This may, or may not, indicate mismatched component sizes.");
                                    ShowContinueError("Verify that the value entered is intended and is consistent with other components.");
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
                ShowSevereError("Autosizing of steam baseboard requires a heating loop Sizing:Plant object");
                ShowContinueError("Occurs in Baseboard Heater=" + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                ErrorsFound = true;
            }
        }

        RegisterPlantCompDesignFlow(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamVolFlowRateMax);

        if (ErrorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
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
        using DataZoneEnergyDemands::CurDeadBandOrSetback;
        using DataZoneEnergyDemands::ZoneSysEnergyDemand;
        using FluidProperties::GetSatDensityRefrig;
        using FluidProperties::GetSatEnthalpyRefrig;
        using FluidProperties::GetSatSpecificHeatRefrig;
        using ScheduleManager::GetCurrentScheduleValue;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("CalcSteamBaseboard");

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

        ZoneNum = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr;
        QZnReq = ZoneSysEnergyDemand(ZoneNum).RemainingOutputReqToHeatSP;
        SteamInletTemp = Node(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode).Temp;
        SteamOutletTemp = SteamInletTemp;
        SteamMassFlowRate = Node(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode).MassFlowRate;
        SubcoolDeltaT = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).DegOfSubcooling;

        if (QZnReq > SmallLoad && !CurDeadBandOrSetback(ZoneNum) && SteamMassFlowRate > 0.0 &&
            GetCurrentScheduleValue(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SchedPtr) > 0) {
            // Unit is on
            EnthSteamInDry = GetSatEnthalpyRefrig(state, fluidNameSteam, SteamInletTemp, 1.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);
            EnthSteamOutWet = GetSatEnthalpyRefrig(state, fluidNameSteam, SteamInletTemp, 0.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);
            LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet;
            Cp = GetSatSpecificHeatRefrig(state, fluidNameSteam, SteamInletTemp, 0.0, state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FluidIndex, RoutineName);
            SteamBBHeat = SteamMassFlowRate * (LatentHeatSteam + SubcoolDeltaT * Cp); // Baseboard heating rate
            SteamOutletTemp = SteamInletTemp - SubcoolDeltaT;                         // Outlet temperature of steam
            // Estimate radiant heat addition
            RadHeat = SteamBBHeat * state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracRadiant; // Radiant heating rate
            state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) = RadHeat;                        // Radiant heat source which will be distributed to surfaces and people

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
            LoadMet = (SumHATsurf(ZoneNum) - state.dataSteamBaseboardRadiator->ZeroSourceSumHATsurf(ZoneNum)) + (SteamBBHeat * state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracConvect) +
                      (RadHeat * state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribPerson);
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletEnthalpy - SteamBBHeat / SteamMassFlowRate;
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletQuality = 0.0;
        } else {
            SteamOutletTemp = SteamInletTemp;
            SteamBBHeat = 0.0;
            LoadMet = 0.0;
            RadHeat = 0.0;
            SteamMassFlowRate = 0.0;
            state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) = 0.0;
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletQuality = 0.0;
            state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletEnthalpy;
        }

        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletTemp = SteamOutletTemp;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRate = SteamMassFlowRate;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy;
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletQuality = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletQuality;
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

        using DataGlobals::BeginEnvrnFlag;
        using DataGlobals::TimeStepZone;
        using PlantUtilities::SafeCopyPlantNode;

        int SteamInletNode;
        int SteamOutletNode;

        // First, update the running average if necessary...
        if (state.dataSteamBaseboardRadiator->LastSysTimeElapsed(BaseboardNum) == SysTimeElapsed) {
            state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg(BaseboardNum) -= state.dataSteamBaseboardRadiator->LastQBBSteamRadSrc(BaseboardNum) * state.dataSteamBaseboardRadiator->LastTimeStepSys(BaseboardNum) / TimeStepZone;
        }
        // Update the running average and the "last" values with the current values of the appropriate variables
        state.dataSteamBaseboardRadiator->QBBSteamRadSrcAvg(BaseboardNum) += state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) * TimeStepSys / TimeStepZone;

        state.dataSteamBaseboardRadiator->LastQBBSteamRadSrc(BaseboardNum) = state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum);
        state.dataSteamBaseboardRadiator->LastSysTimeElapsed(BaseboardNum) = SysTimeElapsed;
        state.dataSteamBaseboardRadiator->LastTimeStepSys(BaseboardNum) = TimeStepSys;

        SteamInletNode = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamInletNode;
        SteamOutletNode = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletNode;

        // Set the outlet air nodes of the Baseboard
        // Set the outlet water nodes for the Coil
        SafeCopyPlantNode(SteamInletNode, SteamOutletNode);
        Node(SteamOutletNode).Temp = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletTemp;
        Node(SteamOutletNode).Enthalpy = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletEnthalpy;
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

        // FLOW:
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

        using DataHeatBalance::Zone;
        using DataHeatBalFanSys::MaxRadHeatFlux;
        using DataHeatBalFanSys::QSteamBaseboardSurf;
        using DataHeatBalFanSys::QSteamBaseboardToPerson;
        using DataSurfaces::Surface;
        using General::RoundSigDigits;

        Real64 const SmallestArea(0.001); // Smallest area in meters squared (to avoid a divide by zero)

        int RadSurfNum;           // Counter for surfaces receiving radiation from radiant heater
        int BaseboardNum;         // Counter for the baseboard
        int SurfNum;              // Pointer to the Surface derived type
        int ZoneNum;              // Pointer to the Zone derived type
        Real64 ThisSurfIntensity; // temporary for W/m2 term for rad on a surface

        QSteamBaseboardSurf = 0.0;
        QSteamBaseboardToPerson = 0.0;

        for (BaseboardNum = 1; BaseboardNum <= state.dataSteamBaseboardRadiator->NumSteamBaseboards; ++BaseboardNum) {

            ZoneNum = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ZonePtr;
            QSteamBaseboardToPerson(ZoneNum) += state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) * state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribPerson;

            for (RadSurfNum = 1; RadSurfNum <= state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotSurfToDistrib; ++RadSurfNum) {
                SurfNum = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SurfacePtr(RadSurfNum);
                if (Surface(SurfNum).Area > SmallestArea) {
                    ThisSurfIntensity =
                        (state.dataSteamBaseboardRadiator->QBBSteamRadSource(BaseboardNum) * state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).FracDistribToSurf(RadSurfNum) / Surface(SurfNum).Area);
                    QSteamBaseboardSurf(SurfNum) += ThisSurfIntensity;

                    if (ThisSurfIntensity > MaxRadHeatFlux) { // CR 8074, trap for excessive intensity (throws off surface balance )
                        ShowSevereError("DistributeBBSteamRadGains:  excessive thermal radiation heat flux intensity detected");
                        ShowContinueError("Surface = " + Surface(SurfNum).Name);
                        ShowContinueError("Surface area = " + RoundSigDigits(Surface(SurfNum).Area, 3) + " [m2]");
                        ShowContinueError("Occurs in " + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                        ShowContinueError("Radiation intensity = " + RoundSigDigits(ThisSurfIntensity, 2) + " [W/m2]");
                        ShowContinueError("Assign a larger surface area or more surfaces in " + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam);
                        ShowFatalError("DistributeBBSteamRadGains:  excessive thermal radiation heat flux intensity detected");
                    }
                } else { // small surface
                    ShowSevereError("DistributeBBSteamRadGains:  surface not large enough to receive thermal radiation heat flux");
                    ShowContinueError("Surface = " + Surface(SurfNum).Name);
                    ShowContinueError("Surface area = " + RoundSigDigits(Surface(SurfNum).Area, 3) + " [m2]");
                    ShowContinueError("Occurs in " + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam + " = " + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                    ShowContinueError("Assign a larger surface area or more surfaces in " + state.dataSteamBaseboardRadiator->cCMO_BBRadiator_Steam);
                    ShowFatalError("DistributeBBSteamRadGains:  surface not large enough to receive thermal radiation heat flux");
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

        using DataSurfaces::Surface;

        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotEnergy = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).TotPower * TimeStepSys * DataGlobalConstants::SecInHour();
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Energy = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Power * TimeStepSys * DataGlobalConstants::SecInHour();
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvEnergy = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).ConvPower * TimeStepSys * DataGlobalConstants::SecInHour();
        state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadEnergy = state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).RadPower * TimeStepSys * DataGlobalConstants::SecInHour();
    }

    Real64 SumHATsurf(int const ZoneNum) // Zone number
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

        // FLOW:
        SumHATsurf = 0.0;

        for (SurfNum = Zone(ZoneNum).SurfaceFirst; SurfNum <= Zone(ZoneNum).SurfaceLast; ++SurfNum) {
            if (!Surface(SurfNum).HeatTransSurf) continue; // Skip non-heat transfer surfaces

            Area = Surface(SurfNum).Area;

            if (Surface(SurfNum).Class == SurfaceClass::Window) {
                if (SurfWinShadingFlag(SurfNum) == IntShadeOn || SurfWinShadingFlag(SurfNum) == IntBlindOn) {
                    // The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
                    Area += SurfWinDividerArea(SurfNum);
                }

                if (SurfWinFrameArea(SurfNum) > 0.0) {
                    // Window frame contribution
                    SumHATsurf += HConvIn(SurfNum) * SurfWinFrameArea(SurfNum) * (1.0 + SurfWinProjCorrFrIn(SurfNum)) *
                                  SurfWinFrameTempSurfIn(SurfNum);
                }

                if (SurfWinDividerArea(SurfNum) > 0.0 && SurfWinShadingFlag(SurfNum) != IntShadeOn &&
                    SurfWinShadingFlag(SurfNum) != IntBlindOn) {
                    // Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
                    SumHATsurf += HConvIn(SurfNum) * SurfWinDividerArea(SurfNum) * (1.0 + 2.0 * SurfWinProjCorrDivIn(SurfNum)) *
                                  SurfWinDividerTempSurfIn(SurfNum);
                }
            }

            SumHATsurf += HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum);
        }

        return SumHATsurf;
    }

    void UpdateSteamBaseboardPlantConnection(EnergyPlusData &state, int const BaseboardTypeNum,         // type index
                                             std::string const &BaseboardName,   // component name
                                             int const EP_UNUSED(EquipFlowCtrl), // Flow control mode for the equipment
                                             int const EP_UNUSED(LoopNum),       // Plant loop index for where called from
                                             int const EP_UNUSED(LoopSide),      // Plant loop side index for where called from
                                             int &CompIndex,                     // Chiller number pointer
                                             bool const EP_UNUSED(FirstHVACIteration),
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
        using DataGlobals::KickOffSimulation;
        using DataPlant::ccSimPlantEquipTypes;
        using DataPlant::CriteriaType_HeatTransferRate;
        using DataPlant::CriteriaType_MassFlowRate;
        using DataPlant::CriteriaType_Temperature;
        using DataPlant::TypeOf_Baseboard_Rad_Conv_Steam;
        using General::TrimSigDigits;
        using PlantUtilities::PullCompInterconnectTrigger;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int BaseboardNum;

        // Find the correct baseboard
        if (CompIndex == 0) {
            BaseboardNum = UtilityRoutines::FindItemInList(BaseboardName, state.dataSteamBaseboardRadiator->SteamBaseboard, &SteamBaseboardParams::EquipID);
            if (BaseboardNum == 0) {
                ShowFatalError("UpdateSteamBaseboardPlantConnection: Specified baseboard not valid =" + BaseboardName);
            }
            CompIndex = BaseboardNum;
        } else {
            BaseboardNum = CompIndex;
            if (BaseboardNum > state.dataSteamBaseboardRadiator->NumSteamBaseboards || BaseboardNum < 1) {
                ShowFatalError("UpdateSteamBaseboardPlantConnection:  Invalid CompIndex passed=" + TrimSigDigits(BaseboardNum) +
                               ", Number of baseboards=" + TrimSigDigits(state.dataSteamBaseboardRadiator->NumSteamBaseboards) + ", Entered baseboard name=" + BaseboardName);
            }
            if (KickOffSimulation) {
                if (BaseboardName != state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID) {
                    ShowFatalError("UpdateSteamBaseboardPlantConnection: Invalid CompIndex passed=" + TrimSigDigits(BaseboardNum) +
                                   ", baseboard name=" + BaseboardName +
                                   ", stored baseboard Name for that index=" + state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).EquipID);
                }
                if (BaseboardTypeNum != TypeOf_Baseboard_Rad_Conv_Steam) {
                    ShowFatalError("UpdateSteamBaseboardPlantConnection: Invalid CompIndex passed=" + TrimSigDigits(BaseboardNum) +
                                   ", baseboard name=" + BaseboardName +
                                   ", stored baseboard Name for that index=" + ccSimPlantEquipTypes(BaseboardTypeNum));
                }
            }
        }

        if (InitLoopEquip) {
            return;
        }

        PullCompInterconnectTrigger(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BBLoadReSimIndex,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    CriteriaType_HeatTransferRate,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).Power);

        PullCompInterconnectTrigger(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BBLoadReSimIndex,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    CriteriaType_MassFlowRate,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamMassFlowRate);

        PullCompInterconnectTrigger(state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BranchNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).CompNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).BBLoadReSimIndex,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopNum,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).LoopSideNum,
                                    CriteriaType_Temperature,
                                    state.dataSteamBaseboardRadiator->SteamBaseboard(BaseboardNum).SteamOutletTemp);
    }

} // namespace SteamBaseboardRadiator

} // namespace EnergyPlus
