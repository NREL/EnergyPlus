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
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/Autosizing/CoolingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/CoolingCapacitySizing.hh>
#include <EnergyPlus/Autosizing/HeatingAirFlowSizing.hh>
#include <EnergyPlus/Autosizing/HeatingCapacitySizing.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/PurchasedAirManager.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh>

namespace EnergyPlus::PurchasedAirManager {

// Module containing data and routines dealing with Ideal Loads Air System (formerly PURCHASED AIR).

// MODULE INFORMATION:
//       AUTHOR         Russ Taylor
//       DATE WRITTEN   May 1997
//       MODIFIED       Fred Buhl Dec 1999
//                      B. Griffith Dec 2006. added OA lookup function, moved getinputflag up to Module
//                      M. Witte June 2011, add new features including DCV, economizer, dehumidification and humidification
//                      NOTE: MJW Sep 13, 2011:  Still need to review checks for negative loads and impossible supply temps???
//                           There are no Deallocate statements in here - should there be?
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To encapsulate the data and algorithms required to simulate the
// Zone Ideal Loads Air System component. This component supplies hot or cold air
// at a fixed or variable temperature to a zone to meet the zone load.
// With the June 2011 enhancements it will also supply outdoor air with optional demand-controlled ventilation
// and economizer controls, plus new options for controlling zone humidity.

// METHODOLOGY EMPLOYED:
// The user can choose via input the max/min hot and cold supply air
// temperature and humidity ratio. The air mass flow rate is chosen
// to meet the (remaining) zone load or based on the outdoor air flow requirement.
// If the outdoor air flow sets the flow rate, the supply air temperature and
// humidity ratio are adjusted to meet the zone load.

// Using/Aliasing
using namespace DataHVACGlobals;
using namespace ScheduleManager;
using Psychrometrics::PsyCpAirFnW;
using Psychrometrics::PsyHFnTdbW;
using Psychrometrics::PsyRhoAirFnPbTdbW;
using Psychrometrics::PsyTdbFnHW;
using Psychrometrics::PsyTsatFnHPb;
using Psychrometrics::PsyWFnTdbH;
using Psychrometrics::PsyWFnTdbRhPb;

// Delta humidity ratio limit, 0.00025 equals delta between 45F dewpoint and 46F dewpoint
// used to prevent dividing by near zero
Real64 constexpr SmallDeltaHumRat(0.00025);

void SimPurchasedAir(EnergyPlusData &state,
                     std::string const &PurchAirName,
                     Real64 &SysOutputProvided,
                     Real64 &MoistOutputProvided, // Moisture output provided (kg/s), dehumidification = negative
                     bool const FirstHVACIteration,
                     int const ControlledZoneNum,
                     int const ActualZoneNum,
                     int &CompIndex)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   May 1997
    //       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided - now MoistOutputProvided)
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine manages Purchased Air component simulation.
    // It is called from SimZoneEquipment in the ZoneEquipmentManager
    // at the system time step.

    int PurchAirNum;

    if (state.dataPurchasedAirMgr->GetPurchAirInputFlag) {
        GetPurchasedAir(state);
        state.dataPurchasedAirMgr->GetPurchAirInputFlag = false;
    }

    // Find the correct PurchasedAir Equipment
    if (CompIndex == 0) {
        PurchAirNum = UtilityRoutines::FindItemInList(PurchAirName, state.dataPurchasedAirMgr->PurchAir);
        if (PurchAirNum == 0) {
            ShowFatalError(state, "SimPurchasedAir: Unit not found=" + PurchAirName);
        }
        CompIndex = PurchAirNum;
    } else {
        PurchAirNum = CompIndex;
        if (PurchAirNum > state.dataPurchasedAirMgr->NumPurchAir || PurchAirNum < 1) {
            ShowFatalError(state,
                           format("SimPurchasedAir:  Invalid CompIndex passed={}, Number of Units={}, Entered Unit name={}",
                                  PurchAirNum,
                                  state.dataPurchasedAirMgr->NumPurchAir,
                                  PurchAirName));
        }
        if (state.dataPurchasedAirMgr->CheckEquipName(PurchAirNum)) {
            if (PurchAirName != state.dataPurchasedAirMgr->PurchAir(PurchAirNum).Name) {
                ShowFatalError(state,
                               format("SimPurchasedAir: Invalid CompIndex passed={}, Unit name={}, stored Unit Name for that index={}",
                                      PurchAirNum,
                                      PurchAirName,
                                      state.dataPurchasedAirMgr->PurchAir(PurchAirNum).Name));
            }
            state.dataPurchasedAirMgr->CheckEquipName(PurchAirNum) = false;
        }
    }

    InitPurchasedAir(state, PurchAirNum, FirstHVACIteration, ControlledZoneNum, ActualZoneNum);

    CalcPurchAirLoads(state, PurchAirNum, SysOutputProvided, MoistOutputProvided, ControlledZoneNum, ActualZoneNum);

    UpdatePurchasedAir(state, PurchAirNum, FirstHVACIteration);

    ReportPurchasedAir(state, PurchAirNum);
}

void GetPurchasedAir(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   June 1997
    //       MODIFIED       M. Witte, June 2011, add new features including DCV, economizer, dehumidification
    //                                           and humidification controls
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Get the input data for the Purchased Air objects.
    // Set up output variables.

    // Using/Aliasing
    using NodeInputManager::CheckUniqueNodes;
    using NodeInputManager::EndUniqueNodeCheck;
    using NodeInputManager::GetOnlySingleNode;
    using NodeInputManager::InitUniqueNodeCheck;
    using OutAirNodeManager::CheckAndAddAirNodeNumber;
    using namespace DataLoopNode;
    using ZonePlenum::GetReturnPlenumIndex;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int PurchAirNum;
    int NumAlphas;
    int NumNums;
    int IOStat;
    int CtrlZone;                                              // zone index
    int NodeNum;                                               // node index
    static std::string const RoutineName("GetPurchasedAir: "); // include trailing blank space
    bool ErrorsFound(false);                                   // If errors detected in input
    bool IsOANodeListed;                                       // Flag for OA node name listed in OutdoorAir:Node or Nodelist
    bool UniqueNodeError;                                      // Flag for non-unique node error(s)
    auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
    cCurrentModuleObject = "ZoneHVAC:IdealLoadsAirSystem";

    auto &PurchAir(state.dataPurchasedAirMgr->PurchAir);

    state.dataPurchasedAirMgr->NumPurchAir = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

    PurchAir.allocate(state.dataPurchasedAirMgr->NumPurchAir);
    state.dataPurchasedAirMgr->CheckEquipName.allocate(state.dataPurchasedAirMgr->NumPurchAir);
    state.dataPurchasedAirMgr->PurchAirNumericFields.allocate(state.dataPurchasedAirMgr->NumPurchAir);
    state.dataPurchasedAirMgr->CheckEquipName = true;

    if (state.dataPurchasedAirMgr->NumPurchAir > 0) {
        InitUniqueNodeCheck(state, cCurrentModuleObject);
        for (PurchAirNum = 1; PurchAirNum <= state.dataPurchasedAirMgr->NumPurchAir; ++PurchAirNum) {
            PurchAir(PurchAirNum).cObjectName = cCurrentModuleObject;

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     PurchAirNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNums,
                                                                     IOStat,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);

            state.dataPurchasedAirMgr->PurchAirNumericFields(PurchAirNum).FieldNames.allocate(NumNums);
            state.dataPurchasedAirMgr->PurchAirNumericFields(PurchAirNum).FieldNames = "";
            state.dataPurchasedAirMgr->PurchAirNumericFields(PurchAirNum).FieldNames = state.dataIPShortCut->cNumericFieldNames;
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);

            PurchAir(PurchAirNum).Name = state.dataIPShortCut->cAlphaArgs(1);
            // get optional  availability schedule
            PurchAir(PurchAirNum).AvailSched = state.dataIPShortCut->cAlphaArgs(2);
            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) {
                PurchAir(PurchAirNum).AvailSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                PurchAir(PurchAirNum).AvailSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(2));
                if (PurchAir(PurchAirNum).AvailSchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                    ShowContinueError(state,
                                      "Invalid-not found " + state.dataIPShortCut->cAlphaFieldNames(2) + "=\"" + state.dataIPShortCut->cAlphaArgs(2) +
                                          "\".");
                    ErrorsFound = true;
                }
            }
            // Purchased air supply air node is an outlet node
            PurchAir(PurchAirNum).ZoneSupplyAirNodeNum = GetOnlySingleNode(state,
                                                                           state.dataIPShortCut->cAlphaArgs(3),
                                                                           ErrorsFound,
                                                                           cCurrentModuleObject,
                                                                           state.dataIPShortCut->cAlphaArgs(1),
                                                                           DataLoopNode::NodeFluidType::Air,
                                                                           DataLoopNode::NodeConnectionType::Outlet,
                                                                           NodeInputManager::compFluidStream::Primary,
                                                                           ObjectIsNotParent);
            UniqueNodeError = false;
            CheckUniqueNodes(state,
                             state.dataIPShortCut->cAlphaFieldNames(3),
                             "NodeName",
                             UniqueNodeError,
                             state.dataIPShortCut->cAlphaArgs(3),
                             _,
                             state.dataIPShortCut->cAlphaArgs(1));
            if (UniqueNodeError) ErrorsFound = true;
            // If new (optional) exhaust air node name is present, then register it as inlet
            if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    PurchAir(PurchAirNum).ZoneExhaustAirNodeNum = GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(4),
                                                                                    ErrorsFound,
                                                                                    cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Inlet,
                                                                                    NodeInputManager::compFluidStream::Primary,
                                                                                    ObjectIsNotParent);
                } else {
                    PurchAir(PurchAirNum).ZoneExhaustAirNodeNum = GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(4),
                                                                                    ErrorsFound,
                                                                                    cCurrentModuleObject,
                                                                                    state.dataIPShortCut->cAlphaArgs(1),
                                                                                    DataLoopNode::NodeFluidType::Air,
                                                                                    DataLoopNode::NodeConnectionType::Outlet,
                                                                                    NodeInputManager::compFluidStream::Primary,
                                                                                    ObjectIsNotParent);
                }
                UniqueNodeError = false;
                CheckUniqueNodes(state,
                                 state.dataIPShortCut->cAlphaFieldNames(4),
                                 "NodeName",
                                 UniqueNodeError,
                                 state.dataIPShortCut->cAlphaArgs(4),
                                 _,
                                 state.dataIPShortCut->cAlphaArgs(1));
                if (UniqueNodeError) ErrorsFound = true;
            }
            if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                PurchAir(PurchAirNum).PlenumExhaustAirNodeNum = GetOnlySingleNode(state,
                                                                                  state.dataIPShortCut->cAlphaArgs(5),
                                                                                  ErrorsFound,
                                                                                  cCurrentModuleObject,
                                                                                  state.dataIPShortCut->cAlphaArgs(1),
                                                                                  DataLoopNode::NodeFluidType::Air,
                                                                                  DataLoopNode::NodeConnectionType::Inlet,
                                                                                  NodeInputManager::compFluidStream::Primary,
                                                                                  ObjectIsNotParent);
            }
            PurchAir(PurchAirNum).MaxHeatSuppAirTemp = state.dataIPShortCut->rNumericArgs(1);
            PurchAir(PurchAirNum).MinCoolSuppAirTemp = state.dataIPShortCut->rNumericArgs(2);
            PurchAir(PurchAirNum).MaxHeatSuppAirHumRat = state.dataIPShortCut->rNumericArgs(3);
            PurchAir(PurchAirNum).MinCoolSuppAirHumRat = state.dataIPShortCut->rNumericArgs(4);

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "NoLimit")) {
                PurchAir(PurchAirNum).HeatingLimit = LimitType::NoLimit;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "LimitFlowRate")) {
                if (state.dataIPShortCut->lNumericFieldBlanks(5)) {
                    PurchAir(PurchAirNum).HeatingLimit = LimitType::NoLimit;
                } else {
                    PurchAir(PurchAirNum).HeatingLimit = LimitType::LimitFlowRate;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "LimitCapacity")) {
                if (state.dataIPShortCut->lNumericFieldBlanks(6)) {
                    PurchAir(PurchAirNum).HeatingLimit = LimitType::NoLimit;
                } else {
                    PurchAir(PurchAirNum).HeatingLimit = LimitType::LimitCapacity;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(6), "LimitFlowRateAndCapacity")) {
                if (state.dataIPShortCut->lNumericFieldBlanks(5) && state.dataIPShortCut->lNumericFieldBlanks(6)) {
                    PurchAir(PurchAirNum).HeatingLimit = LimitType::NoLimit;
                } else if (state.dataIPShortCut->lNumericFieldBlanks(5)) {
                    PurchAir(PurchAirNum).HeatingLimit = LimitType::LimitCapacity;
                } else if (state.dataIPShortCut->lNumericFieldBlanks(6)) {
                    PurchAir(PurchAirNum).HeatingLimit = LimitType::LimitFlowRate;
                } else {
                    PurchAir(PurchAirNum).HeatingLimit = LimitType::LimitFlowRateAndCapacity;
                }
            } else {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                ShowContinueError(state,
                                  "Invalid-entry " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" + state.dataIPShortCut->cAlphaArgs(6) + "\".");
                ShowContinueError(state, "Valid entries are NoLimit, LimitFlowRate, LimitCapacity, or LimitFlowRateAndCapacity");
                ErrorsFound = true;
            }
            PurchAir(PurchAirNum).MaxHeatVolFlowRate = state.dataIPShortCut->rNumericArgs(5);
            PurchAir(PurchAirNum).MaxHeatSensCap = state.dataIPShortCut->rNumericArgs(6);

            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "NoLimit")) {
                PurchAir(PurchAirNum).CoolingLimit = LimitType::NoLimit;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "LimitFlowRate")) {
                if (state.dataIPShortCut->lNumericFieldBlanks(7)) {
                    PurchAir(PurchAirNum).CoolingLimit = LimitType::NoLimit;
                } else {
                    PurchAir(PurchAirNum).CoolingLimit = LimitType::LimitFlowRate;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "LimitCapacity")) {
                if (state.dataIPShortCut->lNumericFieldBlanks(8)) {
                    PurchAir(PurchAirNum).CoolingLimit = LimitType::NoLimit;
                } else {
                    PurchAir(PurchAirNum).CoolingLimit = LimitType::LimitCapacity;
                }
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(7), "LimitFlowRateAndCapacity")) {
                if (state.dataIPShortCut->lNumericFieldBlanks(7) && state.dataIPShortCut->lNumericFieldBlanks(8)) {
                    PurchAir(PurchAirNum).CoolingLimit = LimitType::NoLimit;
                } else if (state.dataIPShortCut->lNumericFieldBlanks(7)) {
                    PurchAir(PurchAirNum).CoolingLimit = LimitType::LimitCapacity;
                } else if (state.dataIPShortCut->lNumericFieldBlanks(8)) {
                    PurchAir(PurchAirNum).CoolingLimit = LimitType::LimitFlowRate;
                } else {
                    PurchAir(PurchAirNum).CoolingLimit = LimitType::LimitFlowRateAndCapacity;
                }
            } else {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                ShowContinueError(state,
                                  "Invalid-entry " + state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) + "\".");
                ShowContinueError(state, "Valid entries are NoLimit, LimitFlowRate, LimitCapacity, or LimitFlowRateAndCapacity");
                ErrorsFound = true;
            }
            PurchAir(PurchAirNum).MaxCoolVolFlowRate = state.dataIPShortCut->rNumericArgs(7);
            PurchAir(PurchAirNum).MaxCoolTotCap = state.dataIPShortCut->rNumericArgs(8);

            // get optional heating availability schedule
            PurchAir(PurchAirNum).HeatSched = state.dataIPShortCut->cAlphaArgs(8);
            if (state.dataIPShortCut->lAlphaFieldBlanks(8)) {
                PurchAir(PurchAirNum).HeatSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                PurchAir(PurchAirNum).HeatSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(8));
                if (PurchAir(PurchAirNum).HeatSchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                    ShowContinueError(state,
                                      "Invalid-not found " + state.dataIPShortCut->cAlphaFieldNames(8) + "=\"" + state.dataIPShortCut->cAlphaArgs(8) +
                                          "\".");
                    ErrorsFound = true;
                }
            }
            // get optional cooling availability schedule
            PurchAir(PurchAirNum).CoolSched = state.dataIPShortCut->cAlphaArgs(9);
            if (state.dataIPShortCut->lAlphaFieldBlanks(9)) {
                PurchAir(PurchAirNum).CoolSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
            } else {
                PurchAir(PurchAirNum).CoolSchedPtr = GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(9));
                if (PurchAir(PurchAirNum).CoolSchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                    ShowContinueError(state,
                                      "Invalid-not found " + state.dataIPShortCut->cAlphaFieldNames(9) + "=\"" + state.dataIPShortCut->cAlphaArgs(9) +
                                          "\".");
                    ErrorsFound = true;
                }
            }
            // get Dehumidification control type
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "None")) {
                PurchAir(PurchAirNum).DehumidCtrlType = HumControl::None;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "ConstantSensibleHeatRatio")) {
                PurchAir(PurchAirNum).DehumidCtrlType = HumControl::ConstantSensibleHeatRatio;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "Humidistat")) {
                PurchAir(PurchAirNum).DehumidCtrlType = HumControl::Humidistat;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(10), "ConstantSupplyHumidityRatio")) {
                PurchAir(PurchAirNum).DehumidCtrlType = HumControl::ConstantSupplyHumidityRatio;
            } else {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                ShowContinueError(
                    state, "Invalid-entry " + state.dataIPShortCut->cAlphaFieldNames(10) + "=\"" + state.dataIPShortCut->cAlphaArgs(10) + "\".");
                ShowContinueError(state, "Valid entries are ConstantSensibleHeatRatio, Humidistat, or ConstantSupplyHumidityRatio");
                ErrorsFound = true;
            }
            PurchAir(PurchAirNum).CoolSHR = state.dataIPShortCut->rNumericArgs(9);

            // get Humidification control type
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(11), "None")) {
                PurchAir(PurchAirNum).HumidCtrlType = HumControl::None;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(11), "Humidistat")) {
                PurchAir(PurchAirNum).HumidCtrlType = HumControl::Humidistat;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(11), "ConstantSupplyHumidityRatio")) {
                PurchAir(PurchAirNum).HumidCtrlType = HumControl::ConstantSupplyHumidityRatio;
            } else {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                ShowContinueError(
                    state, "Invalid-entry " + state.dataIPShortCut->cAlphaFieldNames(11) + "=\"" + state.dataIPShortCut->cAlphaArgs(11) + "\".");
                ShowContinueError(state, "Valid entries are None, Humidistat, or ConstantSupplyHumidityRatio");
                ErrorsFound = true;
            }

            // get Design specification outdoor air object
            if (!state.dataIPShortCut->lAlphaFieldBlanks(12)) {
                PurchAir(PurchAirNum).OARequirementsPtr =
                    UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(12), state.dataSize->OARequirements);
                if (PurchAir(PurchAirNum).OARequirementsPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                    ShowContinueError(state,
                                      "Invalid-not found" + state.dataIPShortCut->cAlphaFieldNames(12) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(12) + "\".");
                    ErrorsFound = true;
                } else {
                    PurchAir(PurchAirNum).OutdoorAir = true;
                }
            }

            // If outdoor air specified, then get Outdoor air inlet node and other outdoor air inputs
            if (PurchAir(PurchAirNum).OutdoorAir) {
                if (state.dataIPShortCut->lAlphaFieldBlanks(13)) {
                    // If there is outdoor air and outdoor air inlet node is blank, then create one
                    if (len(state.dataIPShortCut->cAlphaArgs(1)) <
                        DataGlobalConstants::MaxNameLength - 23) { // protect against long name leading to > 100 chars
                        state.dataIPShortCut->cAlphaArgs(13) = state.dataIPShortCut->cAlphaArgs(1) + " OUTDOOR AIR INLET NODE";
                    } else {
                        state.dataIPShortCut->cAlphaArgs(13) = state.dataIPShortCut->cAlphaArgs(1).substr(0, 75) + " OUTDOOR AIR INLET NODE";
                    }
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        ShowWarningError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " blank field");
                        ShowContinueError(
                            state, state.dataIPShortCut->cAlphaFieldNames(13) + " is blank, but there is outdoor air requested for this system.");
                        ShowContinueError(state, "Creating node name =" + state.dataIPShortCut->cAlphaArgs(13));
                    }
                }
                // Register OA node
                PurchAir(PurchAirNum).OutdoorAirNodeNum = GetOnlySingleNode(state,
                                                                            state.dataIPShortCut->cAlphaArgs(13),
                                                                            ErrorsFound,
                                                                            cCurrentModuleObject,
                                                                            state.dataIPShortCut->cAlphaArgs(1),
                                                                            DataLoopNode::NodeFluidType::Air,
                                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                                            NodeInputManager::compFluidStream::Primary,
                                                                            ObjectIsNotParent);
                // Check if OA node is initialized in OutdoorAir:Node or OutdoorAir:Nodelist
                CheckAndAddAirNodeNumber(state, PurchAir(PurchAirNum).OutdoorAirNodeNum, IsOANodeListed);
                if ((!IsOANodeListed) && state.dataGlobal->DisplayExtraWarnings) {
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " missing data");
                    ShowContinueError(state,
                                      state.dataIPShortCut->cAlphaArgs(13) + " does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
                    ShowContinueError(state, "Adding OutdoorAir:Node=" + state.dataIPShortCut->cAlphaArgs(13));
                }
                UniqueNodeError = false;
                CheckUniqueNodes(state,
                                 state.dataIPShortCut->cAlphaFieldNames(13),
                                 "NodeName",
                                 UniqueNodeError,
                                 state.dataIPShortCut->cAlphaArgs(13),
                                 _,
                                 state.dataIPShortCut->cAlphaArgs(1));
                if (UniqueNodeError) ErrorsFound = true;

                // get Demand controlled ventilation type
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(14), "None")) {
                    PurchAir(PurchAirNum).DCVType = DCV::NoDCV;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(14), "OccupancySchedule")) {
                    PurchAir(PurchAirNum).DCVType = DCV::OccupancySchedule;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(14), "CO2Setpoint")) {
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        PurchAir(PurchAirNum).DCVType = DCV::CO2SetPoint;
                    } else {
                        PurchAir(PurchAirNum).DCVType = DCV::NoDCV;
                        ShowWarningError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                        ShowContinueError(state,
                                          state.dataIPShortCut->cAlphaFieldNames(14) + '=' + state.dataIPShortCut->cAlphaArgs(14) +
                                              " but CO2 simulation is not active.");
                        ShowContinueError(state, "Resetting " + state.dataIPShortCut->cAlphaFieldNames(14) + " to NoDCV");
                        ShowContinueError(state,
                                          "To activate CO2 simulation, use ZoneAirContaminantBalance object and specify \"Carbon Dioxide "
                                          "Concentration\"=\"Yes\".");
                    }
                } else {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                    ShowContinueError(state,
                                      "Invalid-entry " + state.dataIPShortCut->cAlphaFieldNames(14) + '=' + state.dataIPShortCut->cAlphaArgs(14));
                    ShowContinueError(state, "Valid entries are None, OccupancySchedule, or CO2Setpoint");
                    ErrorsFound = true;
                }
                // get Outdoor air economizer type
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(15), "NoEconomizer")) {
                    PurchAir(PurchAirNum).EconomizerType = Econ::NoEconomizer;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(15), "DifferentialDryBulb")) {
                    PurchAir(PurchAirNum).EconomizerType = Econ::DifferentialDryBulb;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(15), "DifferentialEnthalpy")) {
                    PurchAir(PurchAirNum).EconomizerType = Econ::DifferentialEnthalpy;
                } else {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                    ShowContinueError(state,
                                      "Invalid-entry " + state.dataIPShortCut->cAlphaFieldNames(15) + '=' + state.dataIPShortCut->cAlphaArgs(15));
                    ShowContinueError(state, "Valid entries are NoEconomizer, DifferentialDryBulb, or DifferentialEnthalpy");
                    ErrorsFound = true;
                }
                // get Outdoor air heat recovery type and effectiveness
                if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(16), "None")) {
                    PurchAir(PurchAirNum).HtRecType = HeatRecovery::NoHeatRecovery;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(16), "Sensible")) {
                    PurchAir(PurchAirNum).HtRecType = HeatRecovery::Sensible;
                } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(16), "Enthalpy")) {
                    PurchAir(PurchAirNum).HtRecType = HeatRecovery::Enthalpy;
                } else {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + " invalid data");
                    ShowContinueError(state,
                                      "Invalid-entry " + state.dataIPShortCut->cAlphaFieldNames(16) + '=' + state.dataIPShortCut->cAlphaArgs(16));
                    ShowContinueError(state, "Valid entries are None, Sensible, or Enthalpy");
                    ErrorsFound = true;
                }
            } else { // No outdoorair
                PurchAir(PurchAirNum).DCVType = DCV::NoDCV;
                PurchAir(PurchAirNum).EconomizerType = Econ::NoEconomizer;
                PurchAir(PurchAirNum).HtRecType = HeatRecovery::NoHeatRecovery;
            }

            PurchAir(PurchAirNum).HtRecSenEff = state.dataIPShortCut->rNumericArgs(10);
            PurchAir(PurchAirNum).HtRecLatEff = state.dataIPShortCut->rNumericArgs(11);

            for (CtrlZone = 1; CtrlZone <= state.dataGlobal->NumOfZones; ++CtrlZone) {
                if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZone).IsControlled) continue;
                for (NodeNum = 1; NodeNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZone).NumInletNodes; ++NodeNum) {
                    if (PurchAir(PurchAirNum).ZoneSupplyAirNodeNum == state.dataZoneEquip->ZoneEquipConfig(CtrlZone).InletNode(NodeNum)) {
                        PurchAir(PurchAirNum).ZonePtr = CtrlZone;
                    }
                }
            }

            PurchAir(PurchAirNum).HVACSizingIndex = 0;
            if (!state.dataIPShortCut->lAlphaFieldBlanks(17)) {
                PurchAir(PurchAirNum).HVACSizingIndex =
                    UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(17), state.dataSize->ZoneHVACSizing);
                if (PurchAir(PurchAirNum).HVACSizingIndex == 0) {
                    ShowSevereError(state, state.dataIPShortCut->cAlphaFieldNames(17) + " = " + state.dataIPShortCut->cAlphaArgs(17) + " not found.");
                    ShowContinueError(state, "Occurs in " + cCurrentModuleObject + " = " + PurchAir(PurchAirNum).Name);
                    ErrorsFound = true;
                }
            }

            // initialize the calculated and report values
            PurchAir(PurchAirNum).MaxHeatMassFlowRate = 0.0;
            PurchAir(PurchAirNum).MaxCoolMassFlowRate = 0.0;
            PurchAir(PurchAirNum).SenHeatEnergy = 0.0;
            PurchAir(PurchAirNum).LatHeatEnergy = 0.0;
            PurchAir(PurchAirNum).TotHeatEnergy = 0.0;
            PurchAir(PurchAirNum).SenCoolEnergy = 0.0;
            PurchAir(PurchAirNum).LatCoolEnergy = 0.0;
            PurchAir(PurchAirNum).TotCoolEnergy = 0.0;
            PurchAir(PurchAirNum).ZoneSenHeatEnergy = 0.0;
            PurchAir(PurchAirNum).ZoneLatHeatEnergy = 0.0;
            PurchAir(PurchAirNum).ZoneTotHeatEnergy = 0.0;
            PurchAir(PurchAirNum).ZoneSenCoolEnergy = 0.0;
            PurchAir(PurchAirNum).ZoneLatCoolEnergy = 0.0;
            PurchAir(PurchAirNum).ZoneTotCoolEnergy = 0.0;
            PurchAir(PurchAirNum).OASenHeatEnergy = 0.0;
            PurchAir(PurchAirNum).OALatHeatEnergy = 0.0;
            PurchAir(PurchAirNum).OATotHeatEnergy = 0.0;
            PurchAir(PurchAirNum).OASenCoolEnergy = 0.0;
            PurchAir(PurchAirNum).OALatCoolEnergy = 0.0;
            PurchAir(PurchAirNum).OATotCoolEnergy = 0.0;
            PurchAir(PurchAirNum).HtRecSenHeatEnergy = 0.0;
            PurchAir(PurchAirNum).HtRecLatHeatEnergy = 0.0;
            PurchAir(PurchAirNum).HtRecTotHeatEnergy = 0.0;
            PurchAir(PurchAirNum).HtRecSenCoolEnergy = 0.0;
            PurchAir(PurchAirNum).HtRecLatCoolEnergy = 0.0;
            PurchAir(PurchAirNum).HtRecTotCoolEnergy = 0.0;
            PurchAir(PurchAirNum).SenHeatRate = 0.0;
            PurchAir(PurchAirNum).LatHeatRate = 0.0;
            PurchAir(PurchAirNum).TotHeatRate = 0.0;
            PurchAir(PurchAirNum).SenCoolRate = 0.0;
            PurchAir(PurchAirNum).LatCoolRate = 0.0;
            PurchAir(PurchAirNum).TotCoolRate = 0.0;
            PurchAir(PurchAirNum).ZoneSenHeatRate = 0.0;
            PurchAir(PurchAirNum).ZoneLatHeatRate = 0.0;
            PurchAir(PurchAirNum).ZoneTotHeatRate = 0.0;
            PurchAir(PurchAirNum).ZoneSenCoolRate = 0.0;
            PurchAir(PurchAirNum).ZoneLatCoolRate = 0.0;
            PurchAir(PurchAirNum).ZoneTotCoolRate = 0.0;
            PurchAir(PurchAirNum).OASenHeatRate = 0.0;
            PurchAir(PurchAirNum).OALatHeatRate = 0.0;
            PurchAir(PurchAirNum).OATotHeatRate = 0.0;
            PurchAir(PurchAirNum).OASenCoolRate = 0.0;
            PurchAir(PurchAirNum).OALatCoolRate = 0.0;
            PurchAir(PurchAirNum).OATotCoolRate = 0.0;
            PurchAir(PurchAirNum).HtRecSenHeatRate = 0.0;
            PurchAir(PurchAirNum).HtRecLatHeatRate = 0.0;
            PurchAir(PurchAirNum).HtRecTotHeatRate = 0.0;
            PurchAir(PurchAirNum).HtRecSenCoolRate = 0.0;
            PurchAir(PurchAirNum).HtRecLatCoolRate = 0.0;
            PurchAir(PurchAirNum).HtRecTotCoolRate = 0.0;

            PurchAir(PurchAirNum).OutdoorAirMassFlowRate = 0.0;
            PurchAir(PurchAirNum).OutdoorAirVolFlowRateStdRho = 0.0;
            PurchAir(PurchAirNum).SupplyAirMassFlowRate = 0.0;
            PurchAir(PurchAirNum).SupplyAirVolFlowRateStdRho = 0.0;
        }
        EndUniqueNodeCheck(state, cCurrentModuleObject);
    }

    for (PurchAirNum = 1; PurchAirNum <= state.dataPurchasedAirMgr->NumPurchAir; ++PurchAirNum) {

        // Setup Output variables
        //    energy variables
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Sensible Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).SenHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).LatHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Total Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).TotHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name,
                            _,
                            "DISTRICTHEATING",
                            "Heating",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).SenCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).LatCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).TotCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name,
                            _,
                            "DISTRICTCOOLING",
                            "Cooling",
                            _,
                            "System");
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Sensible Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).ZoneSenHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).ZoneLatHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Total Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).ZoneTotHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).ZoneSenCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).ZoneLatCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).ZoneTotCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Sensible Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).OASenHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).OALatHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Total Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).OATotHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).OASenCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).OALatCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).OATotCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Sensible Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).HtRecSenHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Latent Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).HtRecLatHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Total Heating Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).HtRecTotHeatEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Sensible Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).HtRecSenCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Latent Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).HtRecLatCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Total Cooling Energy",
                            OutputProcessor::Unit::J,
                            PurchAir(PurchAirNum).HtRecTotCoolEnergy,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);

        //    rate variables
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Sensible Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).SenHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).LatHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Total Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).TotHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).SenCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).LatCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).TotCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Sensible Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).ZoneSenHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).ZoneLatHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Total Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).ZoneTotHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).ZoneSenCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).ZoneLatCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Zone Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).ZoneTotCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Sensible Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).OASenHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).OALatHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Total Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).OATotHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).OASenCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).OALatCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).OATotCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Sensible Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).HtRecSenHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Latent Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).HtRecLatHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Total Heating Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).HtRecTotHeatRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Sensible Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).HtRecSenCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Latent Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).HtRecLatCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Total Cooling Rate",
                            OutputProcessor::Unit::W,
                            PurchAir(PurchAirNum).HtRecTotCoolRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);

        SetupOutputVariable(state,
                            "Zone Ideal Loads Economizer Active Time",
                            OutputProcessor::Unit::hr,
                            PurchAir(PurchAirNum).TimeEconoActive,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Heat Recovery Active Time",
                            OutputProcessor::Unit::hr,
                            PurchAir(PurchAirNum).TimeHtRecActive,
                            "System",
                            "Sum",
                            PurchAir(PurchAirNum).Name);

        SetupOutputVariable(state,
                            "Zone Ideal Loads Hybrid Ventilation Available Status",
                            OutputProcessor::Unit::None,
                            PurchAir(PurchAirNum).AvailStatus,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);

        // air flows
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            PurchAir(PurchAirNum).OutdoorAirMassFlowRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Outdoor Air Standard Density Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            PurchAir(PurchAirNum).OutdoorAirVolFlowRateStdRho,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            PurchAir(PurchAirNum).SupplyAirMassFlowRate,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Standard Density Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            PurchAir(PurchAirNum).SupplyAirVolFlowRateStdRho,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);

        // Supply Air temperature
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Temperature",
                            OutputProcessor::Unit::C,
                            PurchAir(PurchAirNum).SupplyTemp,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        // Supply Air Humidity Ratio
        SetupOutputVariable(state,
                            "Zone Ideal Loads Supply Air Humidity Ratio",
                            OutputProcessor::Unit::kgWater_kgDryAir,
                            PurchAir(PurchAirNum).SupplyHumRat,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);

        // Mixed Air temperature
        SetupOutputVariable(state,
                            "Zone Ideal Loads Mixed Air Temperature",
                            OutputProcessor::Unit::C,
                            PurchAir(PurchAirNum).MixedAirTemp,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);
        // Mixed Air Humidity Ratio
        SetupOutputVariable(state,
                            "Zone Ideal Loads Mixed Air Humidity Ratio",
                            OutputProcessor::Unit::kgWater_kgDryAir,
                            PurchAir(PurchAirNum).MixedAirHumRat,
                            "System",
                            "Average",
                            PurchAir(PurchAirNum).Name);

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            SetupEMSActuator(state,
                             "Ideal Loads Air System",
                             PurchAir(PurchAirNum).Name,
                             "Air Mass Flow Rate",
                             "[kg/s]",
                             PurchAir(PurchAirNum).EMSOverrideMdotOn,
                             PurchAir(PurchAirNum).EMSValueMassFlowRate);
            SetupEMSActuator(state,
                             "Ideal Loads Air System",
                             PurchAir(PurchAirNum).Name,
                             "Outdoor Air Mass Flow Rate",
                             "[kg/s]",
                             PurchAir(PurchAirNum).EMSOverrideOAMdotOn,
                             PurchAir(PurchAirNum).EMSValueOAMassFlowRate);
            SetupEMSActuator(state,
                             "Ideal Loads Air System",
                             PurchAir(PurchAirNum).Name,
                             "Air Temperature",
                             "[C]",
                             PurchAir(PurchAirNum).EMSOverrideSupplyTempOn,
                             PurchAir(PurchAirNum).EMSValueSupplyTemp);
            SetupEMSActuator(state,
                             "Ideal Loads Air System",
                             PurchAir(PurchAirNum).Name,
                             "Air Humidity Ratio",
                             "[kgWater/kgDryAir]",
                             PurchAir(PurchAirNum).EMSOverrideSupplyHumRatOn,
                             PurchAir(PurchAirNum).EMSValueSupplyHumRat);
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, RoutineName + "Errors found in input. Preceding conditions cause termination.");
    }
}

void InitPurchasedAir(EnergyPlusData &state,
                      int const PurchAirNum,
                      [[maybe_unused]] bool const FirstHVACIteration, // unused1208
                      int const ControlledZoneNum,
                      int const ActualZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Nov 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Initialize the PurchAir data structure.

    // Using/Aliasing
    using DataZoneEquipment::CheckZoneEquipmentList;
    using General::FindNumberInList;
    using ZonePlenum::GetReturnPlenumIndex;
    using ZonePlenum::GetReturnPlenumName;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;
    bool UnitOn;        // simple checks for error
    bool CoolOn;        // simple checks for error
    bool HeatOn;        // simple checks for error
    int SupplyNodeNum;  // Node number for ideal loads supply node
    int ExhaustNodeNum; // Node number for ideal loads exhaust node
    int NodeIndex;      // Array index of zone inlet or zone exhaust node that matches ideal loads node
    bool UseReturnNode; // simple checks for error

    auto &PurchAir(state.dataPurchasedAirMgr->PurchAir);

    // Do the Begin Simulation initializations
    if (state.dataPurchasedAirMgr->InitPurchasedAirMyOneTimeFlag) {
        state.dataPurchasedAirMgr->InitPurchasedAirMyEnvrnFlag.allocate(state.dataPurchasedAirMgr->NumPurchAir);
        state.dataPurchasedAirMgr->InitPurchasedAirMySizeFlag.allocate(state.dataPurchasedAirMgr->NumPurchAir);
        state.dataPurchasedAirMgr->InitPurchasedAirOneTimeUnitInitsDone.allocate(state.dataPurchasedAirMgr->NumPurchAir);
        state.dataPurchasedAirMgr->InitPurchasedAirMyEnvrnFlag = true;
        state.dataPurchasedAirMgr->InitPurchasedAirMySizeFlag = true;
        state.dataPurchasedAirMgr->InitPurchasedAirOneTimeUnitInitsDone = false;
        state.dataPurchasedAirMgr->InitPurchasedAirMyOneTimeFlag = false;
    }

    // need to check all units to see if they are on Zone Equipment List or issue warning
    if (!state.dataPurchasedAirMgr->InitPurchasedAirZoneEquipmentListChecked && state.dataZoneEquip->ZoneEquipInputsFilled) {
        state.dataPurchasedAirMgr->InitPurchasedAirZoneEquipmentListChecked = true;
        for (Loop = 1; Loop <= state.dataPurchasedAirMgr->NumPurchAir; ++Loop) {

            // link with return plenum if used (i.e., PlenumExhaustAirNodeNum will be non-zero)
            if (PurchAir(Loop).PlenumExhaustAirNodeNum > 0) {
                PurchAir(Loop).ReturnPlenumIndex = GetReturnPlenumIndex(state, PurchAir(Loop).PlenumExhaustAirNodeNum);
                if (PurchAir(Loop).ReturnPlenumIndex > 0) {
                    GetReturnPlenumName(state, PurchAir(Loop).ReturnPlenumIndex, PurchAir(Loop).ReturnPlenumName);
                    InitializePlenumArrays(state, Loop);
                } else {
                    ShowSevereError(state,
                                    "InitPurchasedAir: " + PurchAir(Loop).cObjectName + " = " + PurchAir(Loop).Name +
                                        " cannot find ZoneHVAC:ReturnPlenum.  It will not be simulated.");
                }
            }

            if (CheckZoneEquipmentList(state, PurchAir(Loop).cObjectName, PurchAir(Loop).Name)) continue;
            ShowSevereError(state,
                            "InitPurchasedAir: " + PurchAir(Loop).cObjectName + " = " + PurchAir(Loop).Name +
                                " is not on any ZoneHVAC:EquipmentList.  It will not be simulated.");
        }
    }

    // one time inits for each unit - links PurchAirNum with static input data from ControlledZoneNum and ActualZoneNum
    if (!state.dataPurchasedAirMgr->InitPurchasedAirOneTimeUnitInitsDone(PurchAirNum)) {
        state.dataPurchasedAirMgr->InitPurchasedAirOneTimeUnitInitsDone(PurchAirNum) = true;

        // Is the supply node really a zone inlet node?
        // this check has to be done here because of SimPurchasedAir passing in ControlledZoneNum
        SupplyNodeNum = PurchAir(PurchAirNum).ZoneSupplyAirNodeNum;
        if (SupplyNodeNum > 0) {
            NodeIndex = FindNumberInList(SupplyNodeNum,
                                         state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNode,
                                         state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes);
            if (NodeIndex == 0) {
                ShowSevereError(state, "InitPurchasedAir: In " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name);
                ShowContinueError(state, "Zone Supply Air Node Name=" + state.dataLoopNodes->NodeID(SupplyNodeNum) + " is not a zone inlet node.");
                ShowContinueError(state,
                                  "Check ZoneHVAC:EquipmentConnections for zone=" + state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
                ShowFatalError(state, "Preceding condition causes termination.");
            }
        }

        // Set recirculation node number
        // If exhaust node is specified, then recirculation is exhaust node, otherwise use zone return node
        // this check has to be done here because of SimPurchasedAir passing in ControlledZoneNum
        UseReturnNode = false;
        if (PurchAir(PurchAirNum).ZoneExhaustAirNodeNum > 0) {
            ExhaustNodeNum = PurchAir(PurchAirNum).ZoneExhaustAirNodeNum;
            NodeIndex = FindNumberInList(ExhaustNodeNum,
                                         state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ExhaustNode,
                                         state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes);
            if (NodeIndex == 0) {
                ShowSevereError(state, "InitPurchasedAir: In " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name);
                ShowContinueError(state,
                                  "Zone Exhaust Air Node Name=" + state.dataLoopNodes->NodeID(ExhaustNodeNum) + " is not a zone exhaust node.");
                ShowContinueError(state,
                                  "Check ZoneHVAC:EquipmentConnections for zone=" + state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneName);
                ShowContinueError(state, "Zone return air node will be used for ideal loads recirculation air.");
                UseReturnNode = true;
            } else {
                PurchAir(PurchAirNum).ZoneRecircAirNodeNum = PurchAir(PurchAirNum).ZoneExhaustAirNodeNum;
            }
        } else {
            UseReturnNode = true;
        }
        if (UseReturnNode) {
            if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumReturnNodes == 1) {
                PurchAir(PurchAirNum).ZoneRecircAirNodeNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNode(1);
            } else if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumReturnNodes > 1) {
                ShowWarningError(state, "InitPurchasedAir: In " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name);
                ShowContinueError(state,
                                  "No Zone Exhaust Air Node Name has been specified for this system and the zone has more than one Return Air Node.");
                ShowContinueError(state,
                                  "Using the first return air node =" +
                                      state.dataLoopNodes->NodeID(state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ReturnNode(1)));
            } else {
                ShowFatalError(state, "InitPurchasedAir: In " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name);
                ShowContinueError(
                    state,
                    " Invalid recirculation node. No exhaust or return node has been specified for this zone in ZoneHVAC:EquipmentConnections.");
                ShowFatalError(state, "Preceding condition causes termination.");
            }
        }
        // If there is OA and economizer is active, then there must be a limit on cooling flow rate
        if (PurchAir(PurchAirNum).OutdoorAir && (PurchAir(PurchAirNum).EconomizerType != Econ::NoEconomizer)) {
            if ((PurchAir(PurchAirNum).CoolingLimit == LimitType::NoLimit) || (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitCapacity)) {
                ShowSevereError(state, "InitPurchasedAir: In " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name);
                ShowContinueError(state, "There is outdoor air with economizer active but there is no limit on cooling air flow rate.");
                ShowContinueError(state,
                                  "Cooling Limit must be set to LimitFlowRate or LimitFlowRateAndCapacity, and Maximum Cooling Air Flow Rate "
                                  "must be set to a value or autosize.");
                ShowContinueError(state, "Simulation will proceed with no limit on outdoor air flow rate.");
            }
        }
    }

    if (!state.dataGlobal->SysSizingCalc && state.dataPurchasedAirMgr->InitPurchasedAirMySizeFlag(PurchAirNum)) {

        SizePurchasedAir(state, PurchAirNum);

        state.dataPurchasedAirMgr->InitPurchasedAirMySizeFlag(PurchAirNum) = false;
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && state.dataPurchasedAirMgr->InitPurchasedAirMyEnvrnFlag(PurchAirNum)) {

        if ((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRate) ||
            (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity)) {
            PurchAir(PurchAirNum).MaxHeatMassFlowRate = state.dataEnvrn->StdRhoAir * PurchAir(PurchAirNum).MaxHeatVolFlowRate;
        } else {
            PurchAir(PurchAirNum).MaxHeatMassFlowRate = 0.0;
        }
        if ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRate) ||
            (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity)) {
            PurchAir(PurchAirNum).MaxCoolMassFlowRate = state.dataEnvrn->StdRhoAir * PurchAir(PurchAirNum).MaxCoolVolFlowRate;
        } else {
            PurchAir(PurchAirNum).MaxCoolMassFlowRate = 0.0;
        }
        state.dataPurchasedAirMgr->InitPurchasedAirMyEnvrnFlag(PurchAirNum) = false;
    }

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataPurchasedAirMgr->InitPurchasedAirMyEnvrnFlag(PurchAirNum) = true;
    }

    // These initializations are done every iteration
    // check that supply air temps can meet the zone thermostat setpoints
    if (PurchAir(PurchAirNum).MinCoolSuppAirTemp > state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) &&
        state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum) != 0 && PurchAir(PurchAirNum).CoolingLimit == LimitType::NoLimit) {
        // Check if the unit is scheduled off
        UnitOn = true;
        //        IF (PurchAir(PurchAirNum)%AvailSchedPtr > 0) THEN
        if (GetCurrentScheduleValue(state, PurchAir(PurchAirNum).AvailSchedPtr) <= 0) {
            UnitOn = false;
        }
        //        END IF
        // Check if cooling available
        CoolOn = true;
        //        IF (PurchAir(PurchAirNum)%CoolSchedPtr > 0) THEN
        if (GetCurrentScheduleValue(state, PurchAir(PurchAirNum).CoolSchedPtr) <= 0) {
            CoolOn = false;
        }
        //        END IF
        if (UnitOn && CoolOn) {
            if (PurchAir(PurchAirNum).CoolErrIndex == 0) {
                ShowSevereError(state,
                                "InitPurchasedAir: For " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name + " serving Zone " +
                                    state.dataHeatBal->Zone(ActualZoneNum).Name);
                ShowContinueError(state,
                                  format("..the minimum supply air temperature for cooling [{:.2R}] is greater than the zone cooling mean air "
                                         "temperature (MAT) setpoint [{:.2R}].",
                                         PurchAir(PurchAirNum).MinCoolSuppAirTemp,
                                         state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ActualZoneNum)));
                ShowContinueError(state, "..For operative and comfort thermostat controls, the MAT setpoint is computed.");
                ShowContinueError(state, "..This error may indicate that the mean radiant temperature or another comfort factor is too warm.");
                ShowContinueError(state, "Unit availability is nominally ON and Cooling availability is nominally ON.");
                ShowContinueError(state, format("Limit Cooling Capacity Type={}", cLimitType(PurchAir(PurchAirNum).CoolingLimit)));
                // could check for optemp control or comfort control here
                ShowContinueErrorTimeStamp(state, "");
            }
            ShowRecurringSevereErrorAtEnd(state,
                                          "InitPurchasedAir: For " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name +
                                              " serving Zone " + state.dataHeatBal->Zone(ActualZoneNum).Name +
                                              ", the minimum supply air temperature for cooling error continues",
                                          PurchAir(PurchAirNum).CoolErrIndex,
                                          PurchAir(PurchAirNum).MinCoolSuppAirTemp,
                                          PurchAir(PurchAirNum).MinCoolSuppAirTemp,
                                          _,
                                          "C",
                                          "C");
        }
    }
    if (PurchAir(PurchAirNum).MaxHeatSuppAirTemp < state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) &&
        state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum) != 0 && PurchAir(PurchAirNum).HeatingLimit == LimitType::NoLimit) {
        // Check if the unit is scheduled off
        UnitOn = true;
        //        IF (PurchAir(PurchAirNum)%AvailSchedPtr > 0) THEN
        if (GetCurrentScheduleValue(state, PurchAir(PurchAirNum).AvailSchedPtr) <= 0) {
            UnitOn = false;
        }
        //        END IF
        // Check if heating and cooling available
        HeatOn = true;
        //        IF (PurchAir(PurchAirNum)%HeatSchedPtr > 0) THEN
        if (GetCurrentScheduleValue(state, PurchAir(PurchAirNum).HeatSchedPtr) <= 0) {
            HeatOn = false;
        }
        //        END IF
        if (UnitOn && HeatOn) {
            if (PurchAir(PurchAirNum).HeatErrIndex == 0) {
                ShowSevereMessage(state,
                                  "InitPurchasedAir: For " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name +
                                      " serving Zone " + state.dataHeatBal->Zone(ActualZoneNum).Name);
                ShowContinueError(state,
                                  format("..the maximum supply air temperature for heating [{:.2R}] is less than the zone mean air temperature "
                                         "heating setpoint [{:.2R}].",
                                         PurchAir(PurchAirNum).MaxHeatSuppAirTemp,
                                         state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ActualZoneNum)));
                ShowContinueError(state, "..For operative and comfort thermostat controls, the MAT setpoint is computed.");
                ShowContinueError(state, "..This error may indicate that the mean radiant temperature or another comfort factor is too cold.");
                ShowContinueError(state, "Unit availability is nominally ON and Heating availability is nominally ON.");
                ShowContinueError(state, format("Limit Heating Capacity Type={}", cLimitType(PurchAir(PurchAirNum).HeatingLimit)));
                // could check for optemp control or comfort control here
                ShowContinueErrorTimeStamp(state, "");
            }
            ShowRecurringSevereErrorAtEnd(state,
                                          "InitPurchasedAir: For " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name +
                                              " serving Zone " + state.dataHeatBal->Zone(ActualZoneNum).Name +
                                              ", maximum supply air temperature for heating error continues",
                                          PurchAir(PurchAirNum).HeatErrIndex,
                                          PurchAir(PurchAirNum).MaxHeatSuppAirTemp,
                                          PurchAir(PurchAirNum).MaxHeatSuppAirTemp,
                                          _,
                                          "C",
                                          "C");
        }
    }
    //      IF (ErrorsFound .and. .not. WarmupFlag) THEN
    //        CALL ShowFatalError(state, 'Preceding conditions cause termination.')
    //      ENDIF
}

void SizePurchasedAir(EnergyPlusData &state, int const PurchAirNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   April 2003
    //       MODIFIED       M. Witte, June 2011, add sizing for new capacity fields
    //                      August 2013 Daeho Kang, add component sizing table entries
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing Purchased Air Components for which flow rates have not been
    // specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the zone sizing arrays.

    // Using/Aliasing
    using namespace DataSizing;
    using DataHVACGlobals::CoolingCapacitySizing;
    using DataHVACGlobals::HeatingAirflowSizing;
    using DataHVACGlobals::HeatingCapacitySizing;
    using Psychrometrics::CPCW;
    using Psychrometrics::CPHW;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::RhoH2O;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("SizePurchasedAir: "); // include trailing blank space

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool IsAutoSize;               // Indicator to autosize
    Real64 MaxHeatVolFlowRateDes;  // Autosized maximum heating air flow for reporting
    Real64 MaxHeatVolFlowRateUser; // Hardsized maximum heating air flow for reporting
    Real64 MaxCoolVolFlowRateDes;  // Autosized maximum cooling air flow for reporting
    Real64 MaxCoolVolFlowRateUser; // Hardsized maximum cooling air flow for reporting
    Real64 MaxHeatSensCapDes;      // Autosized maximum sensible heating capacity for reporting
    Real64 MaxHeatSensCapUser;     // Hardsized maximum sensible heating capacity for reporting
    Real64 MaxCoolTotCapDes;       // Autosized maximum sensible cooling capacity for reporting
    Real64 MaxCoolTotCapUser;      // Hardsized maximum sensible cooling capacity for reporting
    std::string CompName;          // component name
    std::string CompType;          // component type
    std::string SizingString;      // input field sizing description (e.g., Nominal Capacity)
    Real64 TempSize;               // autosized value of coil input field
    int FieldNum = 2;              // IDD numeric field number where input field description is found
    int SizingMethod;       // Integer representation of sizing method name (e.g., CoolingAirflowSizing, HeatingAirflowSizing, CoolingCapacitySizing,
                            // HeatingCapacitySizing, etc.)
    bool PrintFlag;         // TRUE when sizing information is reported in the eio file
    int zoneHVACIndex;      // index of zoneHVAC equipment sizing specification
    int SAFMethod(0);       // supply air flow rate sizing method (SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingAirflow,
                            // FractionOfAutosizedHeatingAirflow ...)
    int CapSizingMethod(0); // capacity sizing methods (HeatingDesignCapacity, CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity, and
                            // FractionOfAutosizedHeatingCapacity )
    Real64 CoolingAirVolFlowDes(0.0); // cooling supply air flow rate
    Real64 HeatingAirVolFlowDes(0.0); // heating supply air flow rate

    auto &PurchAir(state.dataPurchasedAirMgr->PurchAir);
    auto &ZoneEqSizing(state.dataSize->ZoneEqSizing);

    IsAutoSize = false;
    MaxHeatVolFlowRateDes = 0.0;
    MaxHeatVolFlowRateUser = 0.0;
    MaxCoolVolFlowRateDes = 0.0;
    MaxCoolVolFlowRateUser = 0.0;
    MaxHeatSensCapDes = 0.0;
    MaxHeatSensCapUser = 0.0;
    MaxCoolTotCapDes = 0.0;
    MaxCoolTotCapUser = 0.0;

    state.dataSize->ZoneHeatingOnlyFan = false;
    state.dataSize->ZoneCoolingOnlyFan = false;
    CompType = PurchAir(PurchAirNum).cObjectName;
    CompName = PurchAir(PurchAirNum).Name;
    bool ErrorsFound = false;

    if (state.dataSize->CurZoneEqNum > 0) {
        if (PurchAir(PurchAirNum).HVACSizingIndex > 0) {
            state.dataSize->DataZoneNumber = PurchAir(PurchAirNum).ZonePtr;
            zoneHVACIndex = PurchAir(PurchAirNum).HVACSizingIndex;

            FieldNum = 5; // N5 , \field Maximum Heating Air Flow Rate
            PrintFlag = true;
            SizingString = state.dataPurchasedAirMgr->PurchAirNumericFields(PurchAirNum).FieldNames(FieldNum) + " [m3/s]";
            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod > 0) {
                SizingMethod = HeatingAirflowSizing;
                state.dataSize->ZoneHeatingOnlyFan = true;
                SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingSAFMethod;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                if (SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedHeatingAirflow) {
                    if (SAFMethod == SupplyAirFlowRate) {
                        if ((state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow == AutoSize) &&
                            ((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRate) ||
                             (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity))) {
                            TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                            HeatingAirFlowSizer sizingHeatingAirFlow;
                            sizingHeatingAirFlow.overrideSizingString(SizingString);
                            // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                            sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                            HeatingAirVolFlowDes = sizingHeatingAirFlow.size(state, TempSize, ErrorsFound);
                        } else {
                            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow > 0.0) {
                                HeatingAirFlowSizer sizingHeatingAirFlow;
                                sizingHeatingAirFlow.overrideSizingString(SizingString);
                                // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                                sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                HeatingAirVolFlowDes =
                                    sizingHeatingAirFlow.size(state, state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow, ErrorsFound);
                            }
                        }
                    } else if (SAFMethod == FlowPerFloorArea) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow *
                                                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                        TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                        state.dataSize->DataScalableSizingON = true;
                        HeatingAirFlowSizer sizingHeatingAirFlow;
                        sizingHeatingAirFlow.overrideSizingString(SizingString);
                        // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        HeatingAirVolFlowDes = sizingHeatingAirFlow.size(state, TempSize, ErrorsFound);
                    } else if (SAFMethod == FractionOfAutosizedHeatingAirflow) {
                        state.dataSize->DataFracOfAutosizedHeatingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        if ((state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow == AutoSize) &&
                            ((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRate) ||
                             (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity))) {
                            TempSize = AutoSize;
                            state.dataSize->DataScalableSizingON = true;
                            HeatingAirFlowSizer sizingHeatingAirFlow;
                            sizingHeatingAirFlow.overrideSizingString(SizingString);
                            // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                            sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                            HeatingAirVolFlowDes = sizingHeatingAirFlow.size(state, TempSize, ErrorsFound);
                        }

                    } else {
                        // Invalid sizing method
                    }
                } else if (SAFMethod == FlowPerHeatingCapacity) {
                    SizingMethod = HeatingCapacitySizing;
                    TempSize = AutoSize;
                    PrintFlag = false;
                    if ((state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow == AutoSize) &&
                        ((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRate) ||
                         (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity))) {
                        TempSize = AutoSize;
                        state.dataSize->DataScalableSizingON = true;
                        HeatingCapacitySizer sizerHeatingCapacity;
                        sizerHeatingCapacity.overrideSizingString(SizingString);
                        sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        state.dataSize->DataAutosizedHeatingCapacity = sizerHeatingCapacity.size(state, TempSize, ErrorsFound);
                        state.dataSize->DataFlowPerHeatingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxHeatAirVolFlow;
                        SizingMethod = HeatingAirflowSizing;
                        PrintFlag = true;
                        TempSize = AutoSize;
                        HeatingAirFlowSizer sizingHeatingAirFlow;
                        sizingHeatingAirFlow.overrideSizingString(SizingString);
                        // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        HeatingAirVolFlowDes = sizingHeatingAirFlow.size(state, TempSize, ErrorsFound);
                    }
                }
                MaxHeatVolFlowRateDes = max(0.0, HeatingAirVolFlowDes);
                PurchAir(PurchAirNum).MaxHeatVolFlowRate = MaxHeatVolFlowRateDes;
                state.dataSize->ZoneHeatingOnlyFan = false;

                CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).HeatingCapMethod;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).CapSizingMethod = CapSizingMethod;
                if (CapSizingMethod == HeatingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                    CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                    if (CapSizingMethod == HeatingDesignCapacity) {
                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity > 0.0) {
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad =
                                state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                        }
                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                    } else if (CapSizingMethod == CapacityPerFloorArea) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).HeatingCapacity = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesHeatingLoad =
                            state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity *
                            state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                        state.dataSize->DataScalableSizingON = true;
                    } else if (CapSizingMethod == FractionOfAutosizedHeatingCapacity) {
                        state.dataSize->DataFracOfAutosizedHeatingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledHeatingCapacity;
                        TempSize = AutoSize;
                    }
                }
                SizingMethod = HeatingCapacitySizing;
                SizingString = "";
                state.dataSize->ZoneHeatingOnlyFan = true;
                PrintFlag = false;
                HeatingCapacitySizer sizerHeatingCapacity;
                sizerHeatingCapacity.overrideSizingString(SizingString);
                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                MaxHeatSensCapDes = sizerHeatingCapacity.size(state, TempSize, ErrorsFound);
                state.dataSize->ZoneHeatingOnlyFan = false;
                if (MaxHeatSensCapDes < SmallLoad) {
                    MaxHeatSensCapDes = 0.0;
                }
                if (IsAutoSize) {
                    PurchAir(PurchAirNum).MaxHeatSensCap = MaxHeatSensCapDes;
                    BaseSizer::reportSizerOutput(state,
                                                 PurchAir(PurchAirNum).cObjectName,
                                                 PurchAir(PurchAirNum).Name,
                                                 "Design Size Maximum Sensible Heating Capacity [W]",
                                                 MaxHeatSensCapDes);
                    // If there is OA, check if sizing calcs have OA>0, throw warning if not
                    if ((PurchAir(PurchAirNum).OutdoorAir) && (state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA == 0.0)) {
                        ShowWarningError(state, "InitPurchasedAir: In " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name);
                        ShowContinueError(state, "There is outdoor air specified in this object, but the design outdoor air flow rate for this ");
                        ShowContinueError(state, "zone is zero. The Maximum Sensible Heating Capacity will be autosized for zero outdoor air flow. ");
                        ShowContinueError(state,
                                          "Check the outdoor air specifications in the Sizing:Zone object for zone " +
                                              state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneName + '.');
                    }
                } else {
                    if (PurchAir(PurchAirNum).MaxHeatSensCap > 0.0 && MaxHeatSensCapDes > 0.0) {
                        MaxHeatSensCapUser = PurchAir(PurchAirNum).MaxHeatSensCap;
                        BaseSizer::reportSizerOutput(state,
                                                     PurchAir(PurchAirNum).cObjectName,
                                                     PurchAir(PurchAirNum).Name,
                                                     "Design Size Maximum Sensible Heating Capacity [W]",
                                                     MaxHeatSensCapDes,
                                                     "User-Specified Maximum Sensible Heating Capacity [W]",
                                                     MaxHeatSensCapUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxHeatSensCapDes - MaxHeatSensCapUser) / MaxHeatSensCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizePurchasedAir: Potential issue with equipment sizing for " + PurchAir(PurchAirNum).cObjectName + ' ' +
                                                PurchAir(PurchAirNum).Name);
                                ShowContinueError(state,
                                                  format("...User-Specified Maximum Sensible Heating Capacity of {:.2R} [W]", MaxHeatSensCapUser));
                                ShowContinueError(
                                    state, format("...differs from Design Size Maximum Sensible Heating Capacity of {:.2R} [W]", MaxHeatSensCapDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }

            PrintFlag = true;
            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod > 0) {
                state.dataSize->ZoneCoolingOnlyFan = true;
                SAFMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingSAFMethod;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).SizingMethod(SizingMethod) = SAFMethod;
                if (SAFMethod == SupplyAirFlowRate || SAFMethod == FlowPerFloorArea || SAFMethod == FractionOfAutosizedCoolingAirflow) {
                    if (SAFMethod == SupplyAirFlowRate) {
                        if ((state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow == AutoSize) &&
                            ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRate) ||
                             (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity) ||
                             (PurchAir(PurchAirNum).OutdoorAir && PurchAir(PurchAirNum).EconomizerType != Econ::NoEconomizer))) {
                            TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                            CoolingAirFlowSizer sizingCoolingAirFlow;
                            sizingCoolingAirFlow.overrideSizingString(SizingString);
                            sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                            CoolingAirVolFlowDes = sizingCoolingAirFlow.size(state, TempSize, ErrorsFound);
                        } else {
                            if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow > 0.0) {
                                CoolingAirVolFlowDes = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                                CoolingAirFlowSizer sizingCoolingAirFlow;
                                sizingCoolingAirFlow.overrideSizingString(SizingString);
                                sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                                CoolingAirVolFlowDes = sizingCoolingAirFlow.size(state, CoolingAirVolFlowDes, ErrorsFound);
                            }
                        }
                    } else if (SAFMethod == FlowPerFloorArea) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).SystemAirFlow = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow *
                                                                                state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                        TempSize = ZoneEqSizing(state.dataSize->CurZoneEqNum).AirVolFlow;
                        state.dataSize->DataScalableSizingON = true;
                        CoolingAirFlowSizer sizingCoolingAirFlow;
                        std::string stringOverride = "Maximum Cooling Air Flow Rate [m3/s]";
                        if (state.dataGlobal->isEpJSON) stringOverride = "maximum_cooling_air_flow_rate [m3/s]";
                        sizingCoolingAirFlow.overrideSizingString(stringOverride);
                        // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        CoolingAirVolFlowDes = sizingCoolingAirFlow.size(state, TempSize, ErrorsFound);
                    } else if (SAFMethod == FractionOfAutosizedCoolingAirflow) {
                        if ((state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow == AutoSize) &&
                            ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRate) ||
                             (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity) ||
                             (PurchAir(PurchAirNum).OutdoorAir && PurchAir(PurchAirNum).EconomizerType != Econ::NoEconomizer))) {
                            state.dataSize->DataFracOfAutosizedCoolingAirflow = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                            TempSize = AutoSize;
                            state.dataSize->DataScalableSizingON = true;
                            CoolingAirFlowSizer sizingCoolingAirFlow;
                            std::string stringOverride = "Maximum Cooling Air Flow Rate [m3/s]";
                            if (state.dataGlobal->isEpJSON) stringOverride = "maximum_cooling_air_flow_rate [m3/s]";
                            sizingCoolingAirFlow.overrideSizingString(stringOverride);
                            // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                            sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                            CoolingAirVolFlowDes = sizingCoolingAirFlow.size(state, TempSize, ErrorsFound);
                        }
                    } else {
                        // Invalid scalable sizing method
                    }
                } else if (SAFMethod == FlowPerCoolingCapacity) {
                    if ((state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow == AutoSize) &&
                        ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRate) ||
                         (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity) ||
                         (PurchAir(PurchAirNum).OutdoorAir && PurchAir(PurchAirNum).EconomizerType != Econ::NoEconomizer))) {
                        SizingMethod = CoolingCapacitySizing;
                        TempSize = AutoSize;
                        PrintFlag = false;
                        CoolingCapacitySizer sizerCoolingCapacity;
                        sizerCoolingCapacity.overrideSizingString(SizingString);
                        sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        state.dataSize->DataAutosizedCoolingCapacity = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                        state.dataSize->DataFlowPerCoolingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).MaxCoolAirVolFlow;
                        PrintFlag = true;
                        TempSize = AutoSize;
                        state.dataSize->DataScalableSizingON = true;
                        CoolingAirFlowSizer sizingCoolingAirFlow;
                        std::string stringOverride = "Maximum Cooling Air Flow Rate [m3/s]";
                        if (state.dataGlobal->isEpJSON) stringOverride = "maximum_cooling_air_flow_rate [m3/s]";
                        sizingCoolingAirFlow.overrideSizingString(stringOverride);
                        // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                        sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                        CoolingAirVolFlowDes = sizingCoolingAirFlow.size(state, TempSize, ErrorsFound);
                    }
                }
                MaxCoolVolFlowRateDes = max(0.0, CoolingAirVolFlowDes);
                PurchAir(PurchAirNum).MaxCoolVolFlowRate = MaxCoolVolFlowRateDes;
                state.dataSize->ZoneCoolingOnlyFan = false;
                state.dataSize->DataScalableSizingON = false;

                CapSizingMethod = state.dataSize->ZoneHVACSizing(zoneHVACIndex).CoolingCapMethod;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).CapSizingMethod = CapSizingMethod;
                if (CapSizingMethod == CoolingDesignCapacity || CapSizingMethod == CapacityPerFloorArea ||
                    CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                    if (CapSizingMethod == CoolingDesignCapacity) {
                        if (state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity > 0.0) {
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                            ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                                state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                        } else {
                            state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolMassFlow;
                        }
                        TempSize = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                    } else if (CapSizingMethod == CapacityPerFloorArea) {
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).CoolingCapacity = true;
                        ZoneEqSizing(state.dataSize->CurZoneEqNum).DesCoolingLoad =
                            state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity *
                            state.dataHeatBal->Zone(state.dataSize->DataZoneNumber).FloorArea;
                        state.dataSize->DataScalableSizingON = true;
                    } else if (CapSizingMethod == FractionOfAutosizedCoolingCapacity) {
                        state.dataSize->DataFracOfAutosizedHeatingCapacity = state.dataSize->ZoneHVACSizing(zoneHVACIndex).ScaledCoolingCapacity;
                        state.dataSize->DataFlowUsedForSizing = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).DesCoolMassFlow;
                        TempSize = AutoSize;
                    }
                }
                SizingMethod = CoolingCapacitySizing;
                SizingString = "";
                state.dataSize->ZoneCoolingOnlyFan = true;
                PrintFlag = false;
                TempSize = PurchAir(PurchAirNum).MaxCoolTotCap;
                CoolingCapacitySizer sizerCoolingCapacity;
                sizerCoolingCapacity.overrideSizingString(SizingString);
                sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                MaxCoolTotCapDes = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                state.dataSize->ZoneCoolingOnlyFan = false;
                if (MaxCoolTotCapDes < SmallLoad) {
                    MaxCoolTotCapDes = 0.0;
                }
                if (IsAutoSize) {
                    PurchAir(PurchAirNum).MaxCoolTotCap = MaxCoolTotCapDes;
                    BaseSizer::reportSizerOutput(state,
                                                 PurchAir(PurchAirNum).cObjectName,
                                                 PurchAir(PurchAirNum).Name,
                                                 "Design Size Maximum Total Cooling Capacity [W]",
                                                 MaxCoolTotCapDes);
                    // If there is OA, check if sizing calcs have OA>0, throw warning if not
                    if ((PurchAir(PurchAirNum).OutdoorAir) && (state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA == 0.0)) {
                        ShowWarningError(state, "SizePurchasedAir: In " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name);
                        ShowContinueError(state, "There is outdoor air specified in this object, but the design outdoor air flow rate for this ");
                        ShowContinueError(state, "zone is zero. The Maximum Total Cooling Capacity will be autosized for zero outdoor air flow. ");
                        ShowContinueError(state,
                                          "Check the outdoor air specifications in the Sizing:Zone object for zone " +
                                              state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneName + '.');
                    }
                } else {
                    if (PurchAir(PurchAirNum).MaxCoolTotCap > 0.0 && MaxCoolTotCapDes > 0.0) {
                        MaxCoolTotCapUser = PurchAir(PurchAirNum).MaxCoolTotCap;
                        BaseSizer::reportSizerOutput(state,
                                                     PurchAir(PurchAirNum).cObjectName,
                                                     PurchAir(PurchAirNum).Name,
                                                     "Design Size Maximum Total Cooling Capacity [W]",
                                                     MaxCoolTotCapDes,
                                                     "User-Specified Maximum Total Cooling Capacity [W]",
                                                     MaxCoolTotCapUser);
                        if (state.dataGlobal->DisplayExtraWarnings) {
                            if ((std::abs(MaxCoolTotCapDes - MaxCoolTotCapUser) / MaxCoolTotCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                                ShowMessage(state,
                                            "SizePurchasedAir: Potential issue with equipment sizing for " + PurchAir(PurchAirNum).cObjectName + ' ' +
                                                PurchAir(PurchAirNum).Name);
                                ShowContinueError(state, format("User-Specified Maximum Total Cooling Capacity of {:.2R} [W]", MaxCoolTotCapUser));
                                ShowContinueError(state,
                                                  format("differs from Design Size Maximum Total Cooling Capacity of {:.2R} [W]", MaxCoolTotCapDes));
                                ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                                ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                            }
                        }
                    }
                }
            }

        } else {
            // SizingString = "Maximum Heating Air Flow Rate [m3/s]";
            SizingMethod = HeatingAirflowSizing;
            FieldNum = 5;
            SizingString = state.dataPurchasedAirMgr->PurchAirNumericFields(PurchAirNum).FieldNames(FieldNum) + " [m3/s]";
            IsAutoSize = false;
            PrintFlag = true;
            if ((PurchAir(PurchAirNum).MaxHeatVolFlowRate == AutoSize) &&
                ((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRate) ||
                 (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity))) {
                IsAutoSize = true;
            }
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
                if (PurchAir(PurchAirNum).MaxHeatVolFlowRate > 0.0) {
                    HeatingAirFlowSizer sizingHeatingAirFlow;
                    sizingHeatingAirFlow.overrideSizingString(SizingString);
                    // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                    sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    PurchAir(PurchAirNum).MaxHeatVolFlowRate =
                        sizingHeatingAirFlow.size(state, PurchAir(PurchAirNum).MaxHeatVolFlowRate, ErrorsFound);
                }
                MaxHeatVolFlowRateDes = 0.0;
            } else {
                state.dataSize->ZoneHeatingOnlyFan = true;
                TempSize = PurchAir(PurchAirNum).MaxHeatVolFlowRate;
                HeatingAirFlowSizer sizingHeatingAirFlow;
                sizingHeatingAirFlow.overrideSizingString(SizingString);
                // sizingHeatingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                sizingHeatingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                MaxHeatVolFlowRateDes = sizingHeatingAirFlow.size(state, PurchAir(PurchAirNum).MaxHeatVolFlowRate, ErrorsFound);
                PurchAir(PurchAirNum).MaxHeatVolFlowRate = MaxHeatVolFlowRateDes;
                state.dataSize->ZoneHeatingOnlyFan = false;
            }

            IsAutoSize = false;
            SizingMethod = HeatingCapacitySizing;
            FieldNum = 6; // N6, \field Maximum Sensible Heating Capacity
            SizingString = state.dataPurchasedAirMgr->PurchAirNumericFields(PurchAirNum).FieldNames(FieldNum) + " [m3/s]";
            if ((PurchAir(PurchAirNum).MaxHeatSensCap == AutoSize) && ((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitCapacity) ||
                                                                       (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity))) {
                IsAutoSize = true;
            }
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
                if (PurchAir(PurchAirNum).MaxHeatSensCap > 0.0) {
                    HeatingCapacitySizer sizerHeatingCapacity;
                    sizerHeatingCapacity.overrideSizingString(SizingString);
                    sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    MaxHeatSensCapDes = sizerHeatingCapacity.size(state, PurchAir(PurchAirNum).MaxHeatSensCap, ErrorsFound);
                }
            } else {
                TempSize = PurchAir(PurchAirNum).MaxHeatSensCap;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA;
                state.dataSize->ZoneHeatingOnlyFan = true;
                PrintFlag = false;
                HeatingCapacitySizer sizerHeatingCapacity;
                sizerHeatingCapacity.overrideSizingString(SizingString);
                sizerHeatingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                MaxHeatSensCapDes = sizerHeatingCapacity.size(state, TempSize, ErrorsFound);
                state.dataSize->ZoneHeatingOnlyFan = false;
            }
            if (MaxHeatSensCapDes < SmallLoad) {
                MaxHeatSensCapDes = 0.0;
            }
            if (IsAutoSize) {
                PurchAir(PurchAirNum).MaxHeatSensCap = MaxHeatSensCapDes;
                BaseSizer::reportSizerOutput(state,
                                             PurchAir(PurchAirNum).cObjectName,
                                             PurchAir(PurchAirNum).Name,
                                             "Design Size Maximum Sensible Heating Capacity [W]",
                                             MaxHeatSensCapDes);
                // If there is OA, check if sizing calcs have OA>0, throw warning if not
                if ((PurchAir(PurchAirNum).OutdoorAir) && (state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA == 0.0)) {
                    ShowWarningError(state, "InitPurchasedAir: In " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name);
                    ShowContinueError(state, "There is outdoor air specified in this object, but the design outdoor air flow rate for this ");
                    ShowContinueError(state, "zone is zero. The Maximum Sensible Heating Capacity will be autosized for zero outdoor air flow. ");
                    ShowContinueError(state,
                                      "Check the outdoor air specifications in the Sizing:Zone object for zone " +
                                          state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneName + '.');
                }
            } else {
                if (PurchAir(PurchAirNum).MaxHeatSensCap > 0.0 && MaxHeatSensCapDes > 0.0) {
                    MaxHeatSensCapUser = PurchAir(PurchAirNum).MaxHeatSensCap;
                    BaseSizer::reportSizerOutput(state,
                                                 PurchAir(PurchAirNum).cObjectName,
                                                 PurchAir(PurchAirNum).Name,
                                                 "Design Size Maximum Sensible Heating Capacity [W]",
                                                 MaxHeatSensCapDes,
                                                 "User-Specified Maximum Sensible Heating Capacity [W]",
                                                 MaxHeatSensCapUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxHeatSensCapDes - MaxHeatSensCapUser) / MaxHeatSensCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePurchasedAir: Potential issue with equipment sizing for " + PurchAir(PurchAirNum).cObjectName + ' ' +
                                            PurchAir(PurchAirNum).Name);
                            ShowContinueError(state, format("...User-Specified Maximum Sensible Heating Capacity of {:.2R} [W]", MaxHeatSensCapUser));
                            ShowContinueError(
                                state, format("...differs from Design Size Maximum Sensible Heating Capacity of {:.2R} [W]", MaxHeatSensCapDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }

            PrintFlag = true;
            IsAutoSize = false;
            if ((PurchAir(PurchAirNum).MaxCoolVolFlowRate == AutoSize) &&
                ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRate) ||
                 (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity) ||
                 (PurchAir(PurchAirNum).OutdoorAir && PurchAir(PurchAirNum).EconomizerType != Econ::NoEconomizer))) {
                IsAutoSize = true;
            }
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
                if (PurchAir(PurchAirNum).MaxCoolVolFlowRate > 0.0) {
                    CoolingAirFlowSizer sizingCoolingAirFlow;
                    std::string stringOverride = "Maximum Cooling Air Flow Rate [m3/s]";
                    if (state.dataGlobal->isEpJSON) stringOverride = "maximum_cooling_air_flow_rate [m3/s]";
                    sizingCoolingAirFlow.overrideSizingString(stringOverride);
                    // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                    sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    PurchAir(PurchAirNum).MaxCoolVolFlowRate =
                        sizingCoolingAirFlow.size(state, PurchAir(PurchAirNum).MaxCoolVolFlowRate, ErrorsFound);
                }
            } else {
                state.dataSize->ZoneCoolingOnlyFan = true;
                TempSize = PurchAir(PurchAirNum).MaxCoolVolFlowRate;
                CoolingAirFlowSizer sizingCoolingAirFlow;
                std::string stringOverride = "Maximum Cooling Air Flow Rate [m3/s]";
                if (state.dataGlobal->isEpJSON) stringOverride = "maximum_cooling_air_flow_rate [m3/s]";
                sizingCoolingAirFlow.overrideSizingString(stringOverride);
                // sizingCoolingAirFlow.setHVACSizingIndexData(FanCoil(FanCoilNum).HVACSizingIndex);
                sizingCoolingAirFlow.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                MaxCoolVolFlowRateDes = sizingCoolingAirFlow.size(state, TempSize, ErrorsFound);
                PurchAir(PurchAirNum).MaxCoolVolFlowRate = MaxCoolVolFlowRateDes;
                state.dataSize->ZoneCoolingOnlyFan = false;
            }

            IsAutoSize = false;
            SizingMethod = CoolingCapacitySizing;
            FieldNum = 8; // N8, \field Maximum Total Cooling Capacity
            SizingString = state.dataPurchasedAirMgr->PurchAirNumericFields(PurchAirNum).FieldNames(FieldNum) + " [m3/s]";
            if ((PurchAir(PurchAirNum).MaxCoolTotCap == AutoSize) && ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitCapacity) ||
                                                                      (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity))) {
                IsAutoSize = true;
            }
            if (!IsAutoSize && !state.dataSize->ZoneSizingRunDone) { // Simulation continue
                if (PurchAir(PurchAirNum).MaxCoolTotCap > 0.0) {
                    CoolingCapacitySizer sizerCoolingCapacity;
                    sizerCoolingCapacity.overrideSizingString(SizingString);
                    sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                    PurchAir(PurchAirNum).MaxCoolTotCap = sizerCoolingCapacity.size(state, PurchAir(PurchAirNum).MaxCoolTotCap, ErrorsFound);
                }
            } else {
                state.dataSize->ZoneCoolingOnlyFan = true;
                ZoneEqSizing(state.dataSize->CurZoneEqNum).OAVolFlow = state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA;
                PrintFlag = false;
                TempSize = PurchAir(PurchAirNum).MaxCoolTotCap;
                CoolingCapacitySizer sizerCoolingCapacity;
                sizerCoolingCapacity.overrideSizingString(SizingString);
                sizerCoolingCapacity.initializeWithinEP(state, CompType, CompName, PrintFlag, RoutineName);
                MaxCoolTotCapDes = sizerCoolingCapacity.size(state, TempSize, ErrorsFound);
                state.dataSize->ZoneCoolingOnlyFan = false;
            }
            if (MaxCoolTotCapDes < SmallLoad) {
                MaxCoolTotCapDes = 0.0;
            }
            if (IsAutoSize) {
                PurchAir(PurchAirNum).MaxCoolTotCap = MaxCoolTotCapDes;
                BaseSizer::reportSizerOutput(state,
                                             PurchAir(PurchAirNum).cObjectName,
                                             PurchAir(PurchAirNum).Name,
                                             "Design Size Maximum Total Cooling Capacity [W]",
                                             MaxCoolTotCapDes);
                // If there is OA, check if sizing calcs have OA>0, throw warning if not
                if ((PurchAir(PurchAirNum).OutdoorAir) && (state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).MinOA == 0.0)) {
                    ShowWarningError(state, "SizePurchasedAir: In " + PurchAir(PurchAirNum).cObjectName + " = " + PurchAir(PurchAirNum).Name);
                    ShowContinueError(state, "There is outdoor air specified in this object, but the design outdoor air flow rate for this ");
                    ShowContinueError(state, "zone is zero. The Maximum Total Cooling Capacity will be autosized for zero outdoor air flow. ");
                    ShowContinueError(state,
                                      "Check the outdoor air specifications in the Sizing:Zone object for zone " +
                                          state.dataSize->FinalZoneSizing(state.dataSize->CurZoneEqNum).ZoneName + '.');
                }
            } else {
                if (PurchAir(PurchAirNum).MaxCoolTotCap > 0.0 && MaxCoolTotCapDes > 0.0) {
                    MaxCoolTotCapUser = PurchAir(PurchAirNum).MaxCoolTotCap;
                    BaseSizer::reportSizerOutput(state,
                                                 PurchAir(PurchAirNum).cObjectName,
                                                 PurchAir(PurchAirNum).Name,
                                                 "Design Size Maximum Total Cooling Capacity [W]",
                                                 MaxCoolTotCapDes,
                                                 "User-Specified Maximum Total Cooling Capacity [W]",
                                                 MaxCoolTotCapUser);
                    if (state.dataGlobal->DisplayExtraWarnings) {
                        if ((std::abs(MaxCoolTotCapDes - MaxCoolTotCapUser) / MaxCoolTotCapUser) > state.dataSize->AutoVsHardSizingThreshold) {
                            ShowMessage(state,
                                        "SizePurchasedAir: Potential issue with equipment sizing for " + PurchAir(PurchAirNum).cObjectName + ' ' +
                                            PurchAir(PurchAirNum).Name);
                            ShowContinueError(state, format("User-Specified Maximum Total Cooling Capacity of {:.2R} [W]", MaxCoolTotCapUser));
                            ShowContinueError(state,
                                              format("differs from Design Size Maximum Total Cooling Capacity of {:.2R} [W]", MaxCoolTotCapDes));
                            ShowContinueError(state, "This may, or may not, indicate mismatched component sizes.");
                            ShowContinueError(state, "Verify that the value entered is intended and is consistent with other components.");
                        }
                    }
                }
            }
        }
    }

    //      IF (PurchAir(PurchAirNum)%OutdoorAir .AND. PurchAir(PurchAirNum)%OutsideAirVolFlowRate == AutoSize) THEN
    //        IF (CurZoneEqNum > 0) THEN
    //          CALL CheckZoneSizing(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name)
    //          PurchAir(PurchAirNum)%OutsideAirVolFlowRate = FinalZoneSizing(CurZoneEqNum)%MinOA
    //          IF (PurchAir(PurchAirNum)%OutsideAirVolFlowRate < SmallAirVolFlow) THEN
    //            PurchAir(PurchAirNum)%OutsideAirVolFlowRate = 0.0
    //          END IF
    //          CALL BaseSizer::reportSizerOutput(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name, &
    //                              'Outdoor Air Flow Rate [m3/s]', PurchAir(PurchAirNum)%OutsideAirVolFlowRate )
    //        END IF
    //      END IF
}

void CalcPurchAirLoads(EnergyPlusData &state,
                       int const PurchAirNum,
                       Real64 &SysOutputProvided,   // Sensible output provided [W] cooling = negative
                       Real64 &MoistOutputProvided, // Moisture output provided [kg/s] dehumidification = negative
                       int const ControlledZoneNum,
                       int const ActualZoneNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Nov 1997
    //       MODIFIED       Shirey, Aug 2009 (LatOutputProvided - now MoistOutputProvided)
    //                      M. Witte June 2011, add new features including DCV, economizer, dehumidification
    //                          and humidification,
    //                      July 2012, Chandan Sharma - FSEC: Added hybrid ventilation manager
    //       RE-ENGINEERED  na

    // Using/Aliasing
    using DataHVACGlobals::ForceOff;
    using DataHVACGlobals::SmallLoad;
    auto &ZoneComp = state.dataHVACGlobal->ZoneComp;
    using DataZoneEquipment::PurchasedAir_Num;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("CalcPurchAirLoads");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int InNodeNum; // Ideal loads supply node to zone
    //         INTEGER   :: ExhNodeNum        ! Ideal loads exhaust node from zone
    int ZoneNodeNum;                   // Zone air node
    int OANodeNum;                     // Outdoor air inlet node
    int RecircNodeNum;                 // Return air or zone exhaust node
    OpMode OperatingMode;              // current operating mode, Off, Heat, Cool, or DeadBand
    Real64 SupplyMassFlowRate;         // System supply air mass flow rate [kg/s]
    Real64 SupplyMassFlowRateForHumid; // System supply air mass flow rate required to meet humdification load [kg/s]
    Real64 SupplyMassFlowRateForDehum; // System supply air mass flow rate required to meet dehumidification load [kg/s]
    Real64 SupplyMassFlowRateForCool;  // System supply air mass flow rate required to meet sensible cooling load[kg/s]
    Real64 SupplyMassFlowRateForHeat;  // System supply air mass flow rate required to meet sensible heating load[kg/s]
    Real64 SupplyHumRatForHumid;       // Supply air humidity ratio require to meet the humidification load [kgWater/kgDryAir]
    Real64 SupplyHumRatForDehum;       // Supply air humidity ratio require to meet the dehumidification load [kgWater/kgDryAir]
    Real64 OAMassFlowRate;             // Outdoor air mass flow rate [kg/s]
    Real64 OAVolFlowRate;              // Outdoor air volume flow rate at standard density [m3/s]
    Real64 MinOASensOutput;            // Minimum Outdoor air sensible output [W], <0 means OA is cooler than zone air
    Real64 MinOALatOutput;             // Minimum Outdoor air moisture load [kg/s]
    Real64 SensOutput;                 // Sensible output [W] (psitive means heating, negative means cooling)
    Real64 HeatSensOutput;             // Heating sensible output [W]
    Real64 CoolSensOutput;             // Cooling sensible output [W] (positive value menas cooling)
    Real64 LatOutput;                  // Latent output [W] (positive value means hudmification, negative means dehumidification)
    Real64 CoolLatOutput;              // Cooling latent output [W] (positive value means dehumidification)
    Real64 CoolTotOutput;              // Cooling total output [W] (positive value means cooling)
    Real64 DeltaT;                     // Delta temperature - reused in multiple places
    Real64 DeltaHumRat;                // Delta humidity ratio - reused in multiple places
    Real64 QZnHeatSP;                  // Load required to meet heating setpoint [W] (>0 is a heating load)
    Real64 QZnCoolSP;                  // Load required to meet cooling setpoint [W] (<0 is a cooling load)
    Real64 MdotZnHumidSP;              // Load required to meet humidifying setpoint [kgWater/s] (>0 = a humidify load)
    Real64 MdotZnDehumidSP;            // Load required to meet dehumidifying setpoint [kgWater/s] (<0 = a dehumidify load)
    bool UnitOn;
    bool HeatOn;             // Flag for heating and humidification availbility schedule, true if heating is on
    bool CoolOn;             // Flag for cooling and dehumidification availbility schedule, true if cooling is on
    bool EconoOn;            // Flag for economizer operation, true if economizer is on
    Real64 SupplyHumRatOrig; // Supply inlet to zone humidity ratio before saturation check [kgWater/kgDryAir]
    Real64 SupplyHumRatSat;  // Supply inlet to zone humidity ratio saturation at SupplyTemp [kgWater/kgDryAir]
    Real64 SupplyEnthalpy;   // Supply inlet to zone enthalpy [J/kg]
    Real64 MixedAirEnthalpy; // Mixed air enthalpy [J/kg]
    Real64 CpAir;            // Specific heat [J/kg-C] reused in multiple places
    //         REAL(r64) :: SpecHumOut   ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
    //         REAL(r64) :: SpecHumIn    ! Specific humidity ratio of inlet [zone] air (kg moisture / kg moist air)

    auto &PurchAir(state.dataPurchasedAirMgr->PurchAir);

    // Sign convention: SysOutputProvided <0 Supply air is heated on entering zone (zone is cooled)
    //                  SysOutputProvided >0 Supply air is cooled on entering zone (zone is heated)
    InNodeNum = PurchAir(PurchAirNum).ZoneSupplyAirNodeNum;
    ZoneNodeNum = state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ZoneNode;
    OANodeNum = PurchAir(PurchAirNum).OutdoorAirNodeNum;
    RecircNodeNum = PurchAir(PurchAirNum).ZoneRecircAirNodeNum;
    SupplyMassFlowRate = 0.0;
    OAMassFlowRate = 0.0;
    PurchAir(PurchAirNum).MinOAMassFlowRate = 0.0;
    PurchAir(PurchAirNum).TimeEconoActive = 0.0;
    PurchAir(PurchAirNum).TimeHtRecActive = 0.0;
    SysOutputProvided = 0.0;
    MoistOutputProvided = 0.0;
    CoolSensOutput = 0.0;
    CoolLatOutput = 0.0;
    CoolTotOutput = 0.0;
    HeatSensOutput = 0.0;
    LatOutput = 0.0;

    // default unit to ON
    UnitOn = true;
    EconoOn = false;
    // get current zone requirements
    QZnHeatSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputReqToHeatSP;
    QZnCoolSP = state.dataZoneEnergyDemand->ZoneSysEnergyDemand(ActualZoneNum).RemainingOutputReqToCoolSP;

    if (allocated(ZoneComp)) {
        ZoneComp(PurchasedAir_Num).ZoneCompAvailMgrs(PurchAirNum).ZoneNum = ActualZoneNum;
        PurchAir(PurchAirNum).AvailStatus = ZoneComp(PurchasedAir_Num).ZoneCompAvailMgrs(PurchAirNum).AvailStatus;
        // Check if the hybrid ventilation availability manager is turning the unit off
        if (PurchAir(PurchAirNum).AvailStatus == ForceOff) {
            UnitOn = false;
        }
    }

    // Check if the unit is scheduled off
    //         IF (PurchAir(PurchAirNum)%AvailSchedPtr > 0) THEN
    if (GetCurrentScheduleValue(state, PurchAir(PurchAirNum).AvailSchedPtr) <= 0) {
        UnitOn = false;
    }
    //         END IF
    // Check if heating and cooling available
    HeatOn = true;
    //         IF (PurchAir(PurchAirNum)%HeatSchedPtr > 0) THEN
    if (GetCurrentScheduleValue(state, PurchAir(PurchAirNum).HeatSchedPtr) <= 0) {
        HeatOn = false;
    }
    //         END IF
    CoolOn = true;
    //         IF (PurchAir(PurchAirNum)%CoolSchedPtr > 0) THEN
    if (GetCurrentScheduleValue(state, PurchAir(PurchAirNum).CoolSchedPtr) <= 0) {
        CoolOn = false;
    }
    //         END IF

    if (UnitOn) {
        // Calculate current minimum outdoor air flow rate based on design OA specifications and DCV or CO2 control
        CalcPurchAirMinOAMassFlow(state, PurchAirNum, ActualZoneNum, OAMassFlowRate);

        // EMS override point  Purch air outdoor air massflow rate.....
        if (PurchAir(PurchAirNum).EMSOverrideOAMdotOn) {
            OAMassFlowRate = PurchAir(PurchAirNum).EMSValueOAMassFlowRate;
        }

        // Calculate minimum outdoor air sensible and latent load
        if (PurchAir(PurchAirNum).OutdoorAir) {
            CpAir = PsyCpAirFnW(state.dataLoopNodes->Node(OANodeNum).HumRat);
            MinOASensOutput = OAMassFlowRate * CpAir * (state.dataLoopNodes->Node(OANodeNum).Temp - state.dataLoopNodes->Node(ZoneNodeNum).Temp);
            MinOALatOutput = OAMassFlowRate * (state.dataLoopNodes->Node(OANodeNum).HumRat - state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
        } else {
            MinOASensOutput = 0.0;
            MinOALatOutput = 0.0;
        }
        SupplyMassFlowRate = OAMassFlowRate;

        // Check if cooling of the supply air stream is required

        // Cooling operation
        if ((MinOASensOutput >= QZnCoolSP) && (state.dataHeatBalFanSys->TempControlType(ActualZoneNum) != SingleHeatingSetPoint)) {
            OperatingMode = OpMode::Cool;
            // Calculate supply mass flow, temp and humidity with the following constraints:
            //  Min cooling supply temp
            //  Max total cooling capacity
            //  Max cooling airflow
            //  Min cooling supply humrat  (and Max heating supply humrat)
            //  Min OA mass flow rate

            // Check if OA flow rate greater than max cooling airflow limit
            if (((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRate) ||
                 (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity)) &&
                (OAMassFlowRate > PurchAir(PurchAirNum).MaxCoolMassFlowRate)) {
                OAVolFlowRate = OAMassFlowRate / state.dataEnvrn->StdRhoAir;
                if (PurchAir(PurchAirNum).OAFlowMaxCoolOutputError < 1) {
                    ++PurchAir(PurchAirNum).OAFlowMaxCoolOutputError;
                    ShowWarningError(state,
                                     format("{} \"{}\" Requested outdoor air flow rate = {:.5T} [m3/s] exceeds limit.",
                                            PurchAir(PurchAirNum).cObjectName,
                                            PurchAir(PurchAirNum).Name,
                                            OAVolFlowRate));
                    ShowContinueError(
                        state,
                        format(" Will be reduced to the Maximum Cooling Air Flow Rate = {:.5T} [m3/s]", PurchAir(PurchAirNum).MaxCoolVolFlowRate));
                    ShowContinueErrorTimeStamp(state, "");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        PurchAir(PurchAirNum).cObjectName + " \"" + PurchAir(PurchAirNum).Name +
                            "\" Requested outdoor air flow rate [m3/s] reduced to Maximum Cooling Air Flow Rate warning continues...",
                        PurchAir(PurchAirNum).OAFlowMaxCoolOutputIndex,
                        OAVolFlowRate);
                }
                OAMassFlowRate = PurchAir(PurchAirNum).MaxCoolMassFlowRate;

            } else {
                // Model economizer
                if (PurchAir(PurchAirNum).EconomizerType != Econ::NoEconomizer) {
                    if (((PurchAir(PurchAirNum).EconomizerType == Econ::DifferentialDryBulb) &&
                         (state.dataLoopNodes->Node(OANodeNum).Temp < state.dataLoopNodes->Node(PurchAir(PurchAirNum).ZoneRecircAirNodeNum).Temp)) ||
                        ((PurchAir(PurchAirNum).EconomizerType == Econ::DifferentialEnthalpy) &&
                         (state.dataLoopNodes->Node(OANodeNum).Enthalpy <
                          state.dataLoopNodes->Node(PurchAir(PurchAirNum).ZoneRecircAirNodeNum).Enthalpy))) {

                        // Calculate supply MassFlowRate based on sensible load but limit to Max Cooling Supply Air Flow Rate if specified
                        CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ActualZoneNum));
                        DeltaT = (state.dataLoopNodes->Node(OANodeNum).Temp - state.dataLoopNodes->Node(ZoneNodeNum).Temp);
                        if (DeltaT < -SmallTempDiff) {
                            SupplyMassFlowRate = QZnCoolSP / CpAir / DeltaT;
                            if (((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRate) ||
                                 (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity)) &&
                                (PurchAir(PurchAirNum).MaxCoolMassFlowRate > 0.0)) {
                                SupplyMassFlowRate = min(max(SupplyMassFlowRate, 0.0), PurchAir(PurchAirNum).MaxCoolMassFlowRate);
                            }
                            if (SupplyMassFlowRate > OAMassFlowRate) {
                                EconoOn = true;
                                OAMassFlowRate = SupplyMassFlowRate;
                                PurchAir(PurchAirNum).TimeEconoActive = state.dataHVACGlobal->TimeStepSys;
                            }
                        }
                    }
                }
            }

            // Determine supply mass flow rate
            // Mass flow rate to meet sensible load, at Minimum Cooling Supply Air Temperature
            SupplyMassFlowRateForCool = 0.0;
            if (CoolOn) {
                CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ActualZoneNum));
                DeltaT = (PurchAir(PurchAirNum).MinCoolSuppAirTemp - state.dataLoopNodes->Node(ZoneNodeNum).Temp);
                if (DeltaT < -SmallTempDiff) {
                    SupplyMassFlowRateForCool = QZnCoolSP / CpAir / DeltaT;
                }
            }

            // Mass flow rate to meet dehumidification load, if applicable, at Minimum Cooling Supply Humidity Ratio
            SupplyMassFlowRateForDehum = 0.0;
            if (CoolOn) {
                if (PurchAir(PurchAirNum).DehumidCtrlType == HumControl::Humidistat) {
                    MdotZnDehumidSP = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ActualZoneNum).RemainingOutputReqToDehumidSP;
                    DeltaHumRat = (PurchAir(PurchAirNum).MinCoolSuppAirHumRat - state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
                    if ((DeltaHumRat < -SmallDeltaHumRat) && (MdotZnDehumidSP < 0.0)) {
                        SupplyMassFlowRateForDehum = MdotZnDehumidSP / DeltaHumRat;
                    }
                }
            }

            // Mass flow rate to meet humidification load, if applicable, at Maximum Heating Supply Humidity Ratio
            // This section is the cooling section, so humidification should activate only if humidification control = humidistat
            //   and if dehumidification control = humidistat or none
            SupplyMassFlowRateForHumid = 0.0;
            if (HeatOn) {
                if (PurchAir(PurchAirNum).HumidCtrlType == HumControl::Humidistat) {
                    if ((PurchAir(PurchAirNum).DehumidCtrlType == HumControl::Humidistat) ||
                        (PurchAir(PurchAirNum).DehumidCtrlType == HumControl::None)) {
                        MdotZnHumidSP = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ActualZoneNum).RemainingOutputReqToHumidSP;
                        DeltaHumRat = (PurchAir(PurchAirNum).MaxHeatSuppAirHumRat - state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
                        if ((DeltaHumRat > SmallDeltaHumRat) && (MdotZnHumidSP > 0.0)) {
                            SupplyMassFlowRateForHumid = MdotZnHumidSP / DeltaHumRat;
                        }
                    }
                }
            }

            // If cooling capacity is limited to zero, SupplyMassFlowRate* should be set to zero
            if (((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitCapacity) ||
                 (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity)) &&
                (PurchAir(PurchAirNum).MaxCoolTotCap == 0)) {
                SupplyMassFlowRateForCool = 0;
                SupplyMassFlowRateForDehum = 0;
                SupplyMassFlowRateForHumid = 0;
            }

            // Supply mass flow is greatest of these, but limit to cooling max flow rate, if applicable
            SupplyMassFlowRate = max(0.0, OAMassFlowRate, SupplyMassFlowRateForCool, SupplyMassFlowRateForDehum, SupplyMassFlowRateForHumid);
            // EMS override point  Purch air massflow rate..... but only if unit is on, i.e. SupplyMassFlowRate>0.0
            if (PurchAir(PurchAirNum).EMSOverrideMdotOn) {
                SupplyMassFlowRate = PurchAir(PurchAirNum).EMSValueMassFlowRate;
                OAMassFlowRate = min(OAMassFlowRate, SupplyMassFlowRate);
            }
            if (((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRate) ||
                 (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity)) &&
                (PurchAir(PurchAirNum).MaxCoolMassFlowRate > 0.0)) {
                SupplyMassFlowRate = min(SupplyMassFlowRate, PurchAir(PurchAirNum).MaxCoolMassFlowRate);
            }

            if (SupplyMassFlowRate <= VerySmallMassFlow) SupplyMassFlowRate = 0.0;

            // Calculate mixed air conditions
            CalcPurchAirMixedAir(state,
                                 PurchAirNum,
                                 OAMassFlowRate,
                                 SupplyMassFlowRate,
                                 PurchAir(PurchAirNum).MixedAirTemp,
                                 PurchAir(PurchAirNum).MixedAirHumRat,
                                 MixedAirEnthalpy,
                                 OperatingMode);

            // Calculate supply air conditions using final massflow rate, imposing capacity limits if specified
            // If capacity limits are exceeded, keep massflow rate where it is and adjust supply temp
            // In general, in the cooling section, don't let SupplyTemp be set to something that results in heating
            if (SupplyMassFlowRate > 0.0) {
                // Calculate supply temp at SupplyMassFlowRate and recheck limit on Minimum Cooling Supply Air Temperature
                CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ActualZoneNum));
                PurchAir(PurchAirNum).SupplyTemp = QZnCoolSP / (CpAir * SupplyMassFlowRate) + state.dataLoopNodes->Node(ZoneNodeNum).Temp;
                PurchAir(PurchAirNum).SupplyTemp = max(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).MinCoolSuppAirTemp);
                // This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
                PurchAir(PurchAirNum).SupplyTemp = min(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).MixedAirTemp);
                PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
                SupplyEnthalpy = PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).SupplyHumRat);

                // Check sensible load vs max total cooling capacity, if specified, and adjust supply temp before applying humidity controls
                // Will check again later, too
                if ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitCapacity) ||
                    (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity)) {
                    CpAir = PsyCpAirFnW(PurchAir(PurchAirNum).MixedAirHumRat);
                    CoolSensOutput = SupplyMassFlowRate * (MixedAirEnthalpy - SupplyEnthalpy);
                    if (CoolSensOutput >= PurchAir(PurchAirNum).MaxCoolTotCap) {
                        CoolSensOutput = PurchAir(PurchAirNum).MaxCoolTotCap;
                        SupplyEnthalpy = MixedAirEnthalpy - CoolSensOutput / SupplyMassFlowRate;
                        PurchAir(PurchAirNum).SupplyTemp = PsyTdbFnHW(SupplyEnthalpy, PurchAir(PurchAirNum).SupplyHumRat);
                        // This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
                        PurchAir(PurchAirNum).SupplyTemp = min(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).MixedAirTemp);
                    } // Capacity limit exceeded
                }

                // Set supply humidity ratio for cooling/dehumidification
                PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
                {
                    auto const SELECT_CASE_var(PurchAir(PurchAirNum).DehumidCtrlType);
                    if (SELECT_CASE_var == HumControl::None) {
                        PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat; // Unnecessary line?
                    } else if (SELECT_CASE_var == HumControl::ConstantSensibleHeatRatio) {
                        // SHR = CoolSensOutput/CoolTotOutput
                        // CoolTotOutput = CoolSensOutput/SHR
                        CpAir = PsyCpAirFnW(PurchAir(PurchAirNum).MixedAirHumRat);
                        CoolSensOutput = SupplyMassFlowRate * CpAir * (PurchAir(PurchAirNum).MixedAirTemp - PurchAir(PurchAirNum).SupplyTemp);
                        CoolTotOutput = CoolSensOutput / PurchAir(PurchAirNum).CoolSHR;
                        SupplyEnthalpy = MixedAirEnthalpy - CoolTotOutput / SupplyMassFlowRate;
                        //  Limit for overdrying (avoid Pysch errors which occur if SupplyEnthalpy is too low for SupplyTemp)
                        SupplyEnthalpy = max(SupplyEnthalpy, PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, 0.00001));
                        PurchAir(PurchAirNum).SupplyHumRat =
                            min(PurchAir(PurchAirNum).SupplyHumRat, PsyWFnTdbH(state, PurchAir(PurchAirNum).SupplyTemp, SupplyEnthalpy, RoutineName));
                        // Apply min cooling humidity ratio limit
                        PurchAir(PurchAirNum).SupplyHumRat = max(PurchAir(PurchAirNum).SupplyHumRat, PurchAir(PurchAirNum).MinCoolSuppAirHumRat);
                        // But don't let it be higher than incoming MixedAirHumRat
                        PurchAir(PurchAirNum).SupplyHumRat = min(PurchAir(PurchAirNum).SupplyHumRat, PurchAir(PurchAirNum).MixedAirHumRat);
                    } else if (SELECT_CASE_var == HumControl::Humidistat) {
                        MdotZnDehumidSP = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ActualZoneNum).RemainingOutputReqToDehumidSP;
                        SupplyHumRatForDehum = MdotZnDehumidSP / SupplyMassFlowRate + state.dataLoopNodes->Node(ZoneNodeNum).HumRat;
                        SupplyHumRatForDehum = min(SupplyHumRatForDehum, PurchAir(PurchAirNum).MinCoolSuppAirHumRat);
                        PurchAir(PurchAirNum).SupplyHumRat = min(PurchAir(PurchAirNum).MixedAirHumRat, SupplyHumRatForDehum);
                    } else if (SELECT_CASE_var == HumControl::ConstantSupplyHumidityRatio) {
                        PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MinCoolSuppAirHumRat;
                    } else {
                        PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
                    }
                }

                // Check supply humidity ratio for humidification (SupplyHumRatForHum should always be < SupplyHumRatForDehum)
                // This section is the cooling section, so humidification should activate only if humidification control = humidistat
                //   and if dehumidification control = humidistat or none
                if (HeatOn) {
                    if (PurchAir(PurchAirNum).HumidCtrlType == HumControl::Humidistat) {
                        if ((PurchAir(PurchAirNum).DehumidCtrlType == HumControl::Humidistat) ||
                            (PurchAir(PurchAirNum).DehumidCtrlType == HumControl::None)) {
                            MdotZnHumidSP = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ActualZoneNum).RemainingOutputReqToHumidSP;
                            SupplyHumRatForHumid = MdotZnHumidSP / SupplyMassFlowRate + state.dataLoopNodes->Node(ZoneNodeNum).HumRat;
                            SupplyHumRatForHumid = min(SupplyHumRatForHumid, PurchAir(PurchAirNum).MaxHeatSuppAirHumRat);
                            PurchAir(PurchAirNum).SupplyHumRat = max(PurchAir(PurchAirNum).SupplyHumRat, SupplyHumRatForHumid);
                        }
                    }
                }

                //   Limit supply humidity ratio to saturation at supply outlet temp

                SupplyHumRatOrig = PurchAir(PurchAirNum).SupplyHumRat;
                SupplyHumRatSat = PsyWFnTdbRhPb(state, PurchAir(PurchAirNum).SupplyTemp, 1.0, state.dataEnvrn->OutBaroPress, RoutineName);
                PurchAir(PurchAirNum).SupplyHumRat = min(SupplyHumRatOrig, SupplyHumRatSat);
                SupplyEnthalpy = PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).SupplyHumRat);

                // Check max total Cooling capacity, if specified
                if ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitCapacity) ||
                    (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity)) {
                    // If dehumidifying, compare total cooling to the limit
                    if (PurchAir(PurchAirNum).SupplyHumRat < PurchAir(PurchAirNum).MixedAirHumRat) { // Dehumidifying
                        CoolTotOutput = SupplyMassFlowRate * (MixedAirEnthalpy - SupplyEnthalpy);
                        if ((CoolTotOutput) > PurchAir(PurchAirNum).MaxCoolTotCap) {
                            CoolTotOutput = PurchAir(PurchAirNum).MaxCoolTotCap;
                            SupplyEnthalpy = MixedAirEnthalpy - CoolTotOutput / SupplyMassFlowRate;
                            // Adjust output based on dehumidification control type
                            {
                                auto const SELECT_CASE_var(PurchAir(PurchAirNum).DehumidCtrlType);
                                if (SELECT_CASE_var == HumControl::ConstantSensibleHeatRatio) {
                                    // Adjust both supply temp and humidity ratio to maintain SHR
                                    // SHR = CoolSensOutput/CoolTotOutput
                                    // CoolSensOutput = SHR*CoolTotOutput
                                    CpAir = PsyCpAirFnW(PurchAir(PurchAirNum).MixedAirHumRat);
                                    CoolSensOutput = CoolTotOutput * PurchAir(PurchAirNum).CoolSHR;
                                    PurchAir(PurchAirNum).SupplyTemp =
                                        PurchAir(PurchAirNum).MixedAirTemp - CoolSensOutput / (CpAir * SupplyMassFlowRate);
                                    // This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
                                    PurchAir(PurchAirNum).SupplyTemp = min(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).MixedAirTemp);
                                    //  Limit for overdrying (avoid Pysch errors which occur if SupplyEnthalpy is too low for SupplyTemp)
                                    SupplyEnthalpy = max(SupplyEnthalpy, PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, 0.00001));
                                    PurchAir(PurchAirNum).SupplyHumRat =
                                        PsyWFnTdbH(state, PurchAir(PurchAirNum).SupplyTemp, SupplyEnthalpy, RoutineName);
                                } else if (SELECT_CASE_var == HumControl::Humidistat) {
                                    // Keep supply temp and adjust humidity ratio to reduce load
                                    PurchAir(PurchAirNum).SupplyHumRat =
                                        PsyWFnTdbH(state, PurchAir(PurchAirNum).SupplyTemp, SupplyEnthalpy, RoutineName);
                                } else if ((SELECT_CASE_var == HumControl::None) || (SELECT_CASE_var == HumControl::ConstantSupplyHumidityRatio)) {
                                    // Keep humidity ratio and adjust supply temp
                                    // Check if latent output exceeds capacity
                                    CpAir = PsyCpAirFnW(PurchAir(PurchAirNum).MixedAirHumRat);
                                    CoolSensOutput =
                                        SupplyMassFlowRate * CpAir * (PurchAir(PurchAirNum).MixedAirTemp - PurchAir(PurchAirNum).SupplyTemp);
                                    CoolLatOutput = CoolTotOutput - CoolSensOutput;
                                    if (CoolLatOutput >= PurchAir(PurchAirNum).MaxCoolTotCap) {
                                        PurchAir(PurchAirNum).SupplyTemp = PurchAir(PurchAirNum).MixedAirTemp;
                                        PurchAir(PurchAirNum).SupplyHumRat =
                                            PsyWFnTdbH(state, PurchAir(PurchAirNum).SupplyTemp, SupplyEnthalpy, RoutineName);
                                        CoolLatOutput = PurchAir(PurchAirNum).MaxCoolTotCap;
                                    } else {
                                        PurchAir(PurchAirNum).SupplyTemp = PsyTdbFnHW(SupplyEnthalpy, PurchAir(PurchAirNum).SupplyHumRat);
                                        // This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
                                        PurchAir(PurchAirNum).SupplyTemp = min(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).MixedAirTemp);
                                    }
                                }
                            }
                            // Limit supply humidity ratio to saturation at supply outlet temp
                            // If saturation exceeded, then honor capacity limit and set to dew point at supplyenthalpy

                            SupplyHumRatOrig = PurchAir(PurchAirNum).SupplyHumRat;
                            SupplyHumRatSat = PsyWFnTdbRhPb(state, PurchAir(PurchAirNum).SupplyTemp, 1.0, state.dataEnvrn->OutBaroPress, RoutineName);
                            if (SupplyHumRatSat < SupplyHumRatOrig) {
                                PurchAir(PurchAirNum).SupplyTemp = PsyTsatFnHPb(state, SupplyEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName);

                                // This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
                                PurchAir(PurchAirNum).SupplyTemp = min(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).MixedAirTemp);
                                PurchAir(PurchAirNum).SupplyHumRat = PsyWFnTdbH(state, PurchAir(PurchAirNum).SupplyTemp, SupplyEnthalpy, RoutineName);
                                SupplyEnthalpy = PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).SupplyHumRat);
                                // CpAir = PsyCpAirFnW(MixedAirHumRat)
                                // CoolSensOutput = SupplyMassFlowRate * CpAir * (MixedAirTemp - SupplyTemp)
                                // CoolTotOutput = SupplyMassFlowRate * (MixedAirEnthalpy - SupplyEnthalpy)
                            }
                        }    // Capacity limit exceeded
                    } else { // Not dehumidifying
                        // If not dehumidifying, compare sensible cooling to the limit
                        // This section will only increase supply temp, so no need to recheck for super-saturation
                        CpAir = PsyCpAirFnW(PurchAir(PurchAirNum).MixedAirHumRat);
                        CoolSensOutput = SupplyMassFlowRate * CpAir * (PurchAir(PurchAirNum).MixedAirTemp - PurchAir(PurchAirNum).SupplyTemp);
                        if (CoolSensOutput >= PurchAir(PurchAirNum).MaxCoolTotCap) {
                            CoolSensOutput = PurchAir(PurchAirNum).MaxCoolTotCap;
                            PurchAir(PurchAirNum).SupplyTemp = PurchAir(PurchAirNum).MixedAirTemp - CoolSensOutput / (SupplyMassFlowRate * CpAir);
                        } // Capacity limit exceeded
                    }     // Dehumidifying or not
                }         // Capacity limit active

            } else { // SupplyMassFlowRate is zero
                SupplyEnthalpy = MixedAirEnthalpy;
                PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
                PurchAir(PurchAirNum).SupplyTemp = PurchAir(PurchAirNum).MixedAirTemp;
                CoolSensOutput = 0.0;
                CoolTotOutput = 0.0;
            }
            // Heating or no-load operation
        } else { // Heating or no-load case
            if ((MinOASensOutput < QZnHeatSP) && (state.dataHeatBalFanSys->TempControlType(ActualZoneNum) != SingleCoolingSetPoint)) {
                OperatingMode = OpMode::Heat;
            } else { // DeadBand mode shuts off heat recovery and economizer
                OperatingMode = OpMode::DeadBand;
            }
            // Calculate supply mass flow, temp and humidity with the following constraints:
            //  Max heating supply temp
            //  Max sensible heating capacity
            //  Max heating airflow
            //  Max heating supply humrat (and Min cooling supply humrat)
            //  Min OA mass flow rate

            // Check if OA flow rate greater than max heating airflow limit
            if (((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRate) ||
                 (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity)) &&
                (OAMassFlowRate > PurchAir(PurchAirNum).MaxHeatMassFlowRate)) {
                OAVolFlowRate = OAMassFlowRate / state.dataEnvrn->StdRhoAir;
                if (PurchAir(PurchAirNum).OAFlowMaxHeatOutputError < 1) {
                    ++PurchAir(PurchAirNum).OAFlowMaxHeatOutputError;
                    ShowWarningError(state,
                                     format("{} \"{}\" Requested outdoor air flow rate = {:.5T} [m3/s] exceeds limit.",
                                            PurchAir(PurchAirNum).cObjectName,
                                            PurchAir(PurchAirNum).Name,
                                            OAVolFlowRate));
                    ShowContinueError(
                        state,
                        format(" Will be reduced to the Maximum Heating Air Flow Rate = {:.5T} [m3/s]", PurchAir(PurchAirNum).MaxHeatVolFlowRate));
                    ShowContinueErrorTimeStamp(state, "");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        PurchAir(PurchAirNum).cObjectName + " \"" + PurchAir(PurchAirNum).Name +
                            "\" Requested outdoor air flow rate [m3/s] reduced to Maximum Heating Air Flow Rate warning continues...",
                        PurchAir(PurchAirNum).OAFlowMaxHeatOutputIndex,
                        OAVolFlowRate);
                }
                OAMassFlowRate = PurchAir(PurchAirNum).MaxHeatMassFlowRate;
            }

            SupplyMassFlowRate = OAMassFlowRate;

            // Determine supply mass flow rate
            // Mass flow rate to meet sensible load, at Minimum Cooling Supply Air Temperature
            SupplyMassFlowRateForHeat = 0.0;
            if ((HeatOn) && (OperatingMode == OpMode::Heat)) {
                CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ActualZoneNum));
                DeltaT = (PurchAir(PurchAirNum).MaxHeatSuppAirTemp - state.dataLoopNodes->Node(ZoneNodeNum).Temp);
                if (DeltaT > SmallTempDiff) {
                    SupplyMassFlowRateForHeat = QZnHeatSP / CpAir / DeltaT;
                }
            }

            // Mass flow rate to meet dehumidification load, if applicable, at Minimum Cooling Supply Humidity Ratio
            // This section is the heating/deadband section, so dehumidification should activate
            //   only if dehumidification control = humidistat
            //   and if humidification control = humidistat or none or if operating in deadband mode
            SupplyMassFlowRateForDehum = 0.0;
            if (CoolOn) {
                if (PurchAir(PurchAirNum).DehumidCtrlType == HumControl::Humidistat) {
                    if ((PurchAir(PurchAirNum).HumidCtrlType == HumControl::Humidistat) ||
                        (PurchAir(PurchAirNum).HumidCtrlType == HumControl::None) || (OperatingMode == OpMode::DeadBand)) {
                        MdotZnDehumidSP = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ActualZoneNum).RemainingOutputReqToDehumidSP;
                        DeltaHumRat = (PurchAir(PurchAirNum).MinCoolSuppAirHumRat - state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
                        if ((DeltaHumRat < -SmallDeltaHumRat) && (MdotZnDehumidSP < 0.0)) {
                            SupplyMassFlowRateForDehum = MdotZnDehumidSP / DeltaHumRat;
                        }
                    }
                }
            }

            // Mass flow rate to meet humidification load, if applicable, at Maximum Heating Supply Humidity Ratio
            SupplyMassFlowRateForHumid = 0.0;
            if (HeatOn) {
                if (PurchAir(PurchAirNum).HumidCtrlType == HumControl::Humidistat) {
                    MdotZnHumidSP = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ActualZoneNum).RemainingOutputReqToHumidSP;
                    DeltaHumRat = (PurchAir(PurchAirNum).MaxHeatSuppAirHumRat - state.dataLoopNodes->Node(ZoneNodeNum).HumRat);
                    if ((DeltaHumRat > SmallDeltaHumRat) && (MdotZnHumidSP > 0.0)) {
                        SupplyMassFlowRateForHumid = MdotZnHumidSP / DeltaHumRat;
                    }
                }
            }

            // If heating capacity is limited to zero, SupplyMassFlowRate* should be set to zero
            if (((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitCapacity) ||
                 (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity)) &&
                (PurchAir(PurchAirNum).MaxHeatSensCap == 0)) {
                SupplyMassFlowRateForHeat = 0;
                SupplyMassFlowRateForDehum = 0;
                SupplyMassFlowRateForHumid = 0;
            }

            // Supply mass flow is greatest of these, but limit to heating max flow rate, if applicable
            SupplyMassFlowRate = max(0.0, OAMassFlowRate, SupplyMassFlowRateForHeat, SupplyMassFlowRateForDehum, SupplyMassFlowRateForHumid);
            // EMS override point  Purch air massflow rate..... but only if unit is on, i.e. SupplyMassFlowRate>0.0
            if (PurchAir(PurchAirNum).EMSOverrideMdotOn) {
                SupplyMassFlowRate = PurchAir(PurchAirNum).EMSValueMassFlowRate;
                OAMassFlowRate = min(OAMassFlowRate, SupplyMassFlowRate);
            }
            if (((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRate) ||
                 (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity)) &&
                (PurchAir(PurchAirNum).MaxHeatMassFlowRate > 0.0)) {
                SupplyMassFlowRate = min(SupplyMassFlowRate, PurchAir(PurchAirNum).MaxHeatMassFlowRate);
            }

            if (SupplyMassFlowRate <= VerySmallMassFlow) SupplyMassFlowRate = 0.0;

            // Calculate mixed air conditions
            CalcPurchAirMixedAir(state,
                                 PurchAirNum,
                                 OAMassFlowRate,
                                 SupplyMassFlowRate,
                                 PurchAir(PurchAirNum).MixedAirTemp,
                                 PurchAir(PurchAirNum).MixedAirHumRat,
                                 MixedAirEnthalpy,
                                 OperatingMode);

            // Calculate supply air conditions using final massflow rate, imposing capacity limits if specified
            // If capacity limits are exceeded, keep massflow rate where it is and adjust supply temp
            if (SupplyMassFlowRate > 0.0) {
                if ((HeatOn) && (OperatingMode == OpMode::Heat)) {
                    // Calculate supply temp at SupplyMassFlowRate and check limit on Maximum Heating Supply Air Temperature
                    CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ActualZoneNum));
                    PurchAir(PurchAirNum).SupplyTemp = QZnHeatSP / (CpAir * SupplyMassFlowRate) + state.dataLoopNodes->Node(ZoneNodeNum).Temp;
                    PurchAir(PurchAirNum).SupplyTemp = min(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).MaxHeatSuppAirTemp);
                    // This is the heating mode, so SupplyTemp can't be less than MixedAirTemp
                    PurchAir(PurchAirNum).SupplyTemp = max(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).MixedAirTemp);
                    // Check max heating capacity, if specified
                    if ((PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitCapacity) ||
                        (PurchAir(PurchAirNum).HeatingLimit == LimitType::LimitFlowRateAndCapacity)) {
                        CpAir = PsyCpAirFnW(PurchAir(PurchAirNum).MixedAirHumRat);
                        HeatSensOutput = SupplyMassFlowRate * CpAir * (PurchAir(PurchAirNum).SupplyTemp - PurchAir(PurchAirNum).MixedAirTemp);
                        if (HeatSensOutput > PurchAir(PurchAirNum).MaxHeatSensCap) {
                            PurchAir(PurchAirNum).SupplyTemp =
                                PurchAir(PurchAirNum).MaxHeatSensCap / (SupplyMassFlowRate * CpAir) + PurchAir(PurchAirNum).MixedAirTemp;
                            HeatSensOutput = PurchAir(PurchAirNum).MaxHeatSensCap;
                        }
                    }
                } else { // Heat is off or operating mode is deadband (i.e. don't do any heating)
                    PurchAir(PurchAirNum).SupplyTemp = PurchAir(PurchAirNum).MixedAirTemp;
                }

                // Set supply humidity ratio first for heating/humidification
                PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
                {
                    auto const SELECT_CASE_var(PurchAir(PurchAirNum).HumidCtrlType);
                    if (SELECT_CASE_var == HumControl::None) {
                        PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
                    } else if (SELECT_CASE_var == HumControl::Humidistat) {
                        MdotZnHumidSP = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ActualZoneNum).RemainingOutputReqToHumidSP;
                        SupplyHumRatForHumid = MdotZnHumidSP / SupplyMassFlowRate + state.dataLoopNodes->Node(ZoneNodeNum).HumRat;
                        SupplyHumRatForHumid = min(SupplyHumRatForHumid, PurchAir(PurchAirNum).MaxHeatSuppAirHumRat);
                        PurchAir(PurchAirNum).SupplyHumRat = max(PurchAir(PurchAirNum).SupplyHumRat, SupplyHumRatForHumid);
                    } else if (SELECT_CASE_var == HumControl::ConstantSupplyHumidityRatio) {
                        if (OperatingMode == OpMode::Heat) {
                            // If this results in dehumidification, must check cooling capacity limit
                            if (PurchAir(PurchAirNum).MixedAirHumRat > PurchAir(PurchAirNum).MaxHeatSuppAirHumRat) {
                                if ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitCapacity) ||
                                    (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity)) {
                                    PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MaxHeatSuppAirHumRat;
                                    SupplyEnthalpy = PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).SupplyHumRat);
                                    CoolTotOutput = SupplyMassFlowRate * (MixedAirEnthalpy - SupplyEnthalpy);
                                    CpAir = PsyCpAirFnW(PurchAir(PurchAirNum).MixedAirHumRat);
                                    CoolSensOutput =
                                        SupplyMassFlowRate * CpAir * (PurchAir(PurchAirNum).MixedAirTemp - PurchAir(PurchAirNum).SupplyTemp);
                                    CoolLatOutput = CoolTotOutput - CoolSensOutput;
                                    if (CoolLatOutput >= PurchAir(PurchAirNum).MaxCoolTotCap) {
                                        CoolLatOutput = PurchAir(PurchAirNum).MaxCoolTotCap;
                                        CoolTotOutput = CoolSensOutput + CoolLatOutput;
                                        SupplyEnthalpy = MixedAirEnthalpy - CoolTotOutput / SupplyMassFlowRate;
                                        PurchAir(PurchAirNum).SupplyHumRat =
                                            PsyWFnTdbH(state, PurchAir(PurchAirNum).SupplyTemp, SupplyEnthalpy, RoutineName);
                                    }
                                } else {
                                    PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MaxHeatSuppAirHumRat;
                                }
                            } else {
                                PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MaxHeatSuppAirHumRat;
                            }
                        } else {
                            PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
                        }
                    } else {
                        PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
                    }
                }
                SupplyEnthalpy = PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).SupplyHumRat);

                // Check supply humidity ratio for dehumidification (SupplyHumRatForHumid should always be < SupplyHumRatForDehum)
                // This section is the heating/deadband section, so dehumidification should activate
                //   only if dehumidification control = humidistat
                //   and if humidification control = humidistat or none or if operating in deadband mode
                if (CoolOn) {
                    if (PurchAir(PurchAirNum).DehumidCtrlType == HumControl::Humidistat) {
                        if ((PurchAir(PurchAirNum).HumidCtrlType == HumControl::Humidistat) ||
                            (PurchAir(PurchAirNum).HumidCtrlType == HumControl::None) || (OperatingMode == OpMode::DeadBand)) {
                            MdotZnDehumidSP = state.dataZoneEnergyDemand->ZoneSysMoistureDemand(ActualZoneNum).RemainingOutputReqToDehumidSP;
                            SupplyHumRatForDehum = MdotZnDehumidSP / SupplyMassFlowRate + state.dataLoopNodes->Node(ZoneNodeNum).HumRat;
                            SupplyHumRatForDehum = max(SupplyHumRatForDehum, PurchAir(PurchAirNum).MinCoolSuppAirHumRat);
                            PurchAir(PurchAirNum).SupplyHumRat = min(PurchAir(PurchAirNum).SupplyHumRat, SupplyHumRatForDehum);
                            SupplyEnthalpy = PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).SupplyHumRat);
                            if (PurchAir(PurchAirNum).SupplyHumRat < PurchAir(PurchAirNum).MixedAirHumRat) {
                                // At this point, the system is heating or deadband but dehumidifying, check max cooling cap limit
                                CpAir = PsyCpAirFnW(PurchAir(PurchAirNum).MixedAirHumRat);
                                SensOutput = SupplyMassFlowRate * CpAir * (PurchAir(PurchAirNum).SupplyTemp - PurchAir(PurchAirNum).MixedAirTemp);
                                LatOutput = SupplyMassFlowRate * (SupplyEnthalpy - MixedAirEnthalpy) - SensOutput;
                                if ((PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitCapacity) ||
                                    (PurchAir(PurchAirNum).CoolingLimit == LimitType::LimitFlowRateAndCapacity)) {
                                    if (LatOutput > PurchAir(PurchAirNum).MaxCoolTotCap) {
                                        LatOutput = PurchAir(PurchAirNum).MaxCoolTotCap;
                                        SupplyEnthalpy = MixedAirEnthalpy + (LatOutput + SensOutput) / SupplyMassFlowRate;
                                        PurchAir(PurchAirNum).SupplyHumRat =
                                            PsyWFnTdbH(state, PurchAir(PurchAirNum).SupplyTemp, SupplyEnthalpy, RoutineName);
                                    }
                                }
                            }
                        }
                    }
                }

                //   Limit supply humidity ratio to saturation at supply outlet temp

                SupplyHumRatOrig = PurchAir(PurchAirNum).SupplyHumRat;
                PurchAir(PurchAirNum).SupplyHumRat =
                    min(PurchAir(PurchAirNum).SupplyHumRat,
                        PsyWFnTdbRhPb(state, PurchAir(PurchAirNum).SupplyTemp, 1.0, state.dataEnvrn->OutBaroPress, RoutineName));
                SupplyEnthalpy = PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).SupplyHumRat);

            } else { // SupplyMassFlowRate is zero
                SupplyEnthalpy = MixedAirEnthalpy;
                PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
                PurchAir(PurchAirNum).SupplyTemp = PurchAir(PurchAirNum).MixedAirTemp;
                HeatSensOutput = 0.0;
            }

        } // Cooling or heating required

        if (SupplyMassFlowRate > 0.0) {
            // EMS override point  Purch air supply temp and humidty ratio ..... but only if unit is on, SupplyMassFlowRate>0.0
            if (PurchAir(PurchAirNum).EMSOverrideSupplyTempOn) {
                PurchAir(PurchAirNum).SupplyTemp = PurchAir(PurchAirNum).EMSValueSupplyTemp;
            }
            if (PurchAir(PurchAirNum).EMSOverrideSupplyHumRatOn) {
                PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).EMSValueSupplyHumRat;
            }
            SupplyEnthalpy = PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).SupplyHumRat);

            // compute coil loads
            if ((PurchAir(PurchAirNum).SupplyHumRat == PurchAir(PurchAirNum).MixedAirHumRat) &&
                (PurchAir(PurchAirNum).SupplyTemp == PurchAir(PurchAirNum).MixedAirTemp)) {
                // If no change in humrat or temp, then set loads to zero
                PurchAir(PurchAirNum).SenCoilLoad = 0.0;
                PurchAir(PurchAirNum).LatCoilLoad = 0.0;
            } else if ((PurchAir(PurchAirNum).SupplyHumRat == PurchAir(PurchAirNum).MixedAirHumRat) &&
                       (PurchAir(PurchAirNum).SupplyTemp != PurchAir(PurchAirNum).MixedAirTemp)) {
                // If no change in humrat, then set latent load to zero and use enthalpies to calculate sensible load
                PurchAir(PurchAirNum).SenCoilLoad = SupplyMassFlowRate * (SupplyEnthalpy - MixedAirEnthalpy);
                PurchAir(PurchAirNum).LatCoilLoad = 0.0;
            } else {
                CpAir = PsyCpAirFnW(PurchAir(PurchAirNum).MixedAirHumRat);
                PurchAir(PurchAirNum).SenCoilLoad =
                    SupplyMassFlowRate * CpAir * (PurchAir(PurchAirNum).SupplyTemp - PurchAir(PurchAirNum).MixedAirTemp);
                PurchAir(PurchAirNum).LatCoilLoad = SupplyMassFlowRate * (SupplyEnthalpy - MixedAirEnthalpy) - PurchAir(PurchAirNum).SenCoilLoad;
            }

            // Apply heating and cooling availability schedules to sensible load
            if (((PurchAir(PurchAirNum).SenCoilLoad > 0.0) && !HeatOn) || ((PurchAir(PurchAirNum).SenCoilLoad < 0.0) && !CoolOn)) {
                // Coil is off
                PurchAir(PurchAirNum).SenCoilLoad = 0.0;
                PurchAir(PurchAirNum).SupplyTemp = PurchAir(PurchAirNum).MixedAirTemp;
            }

            // Apply heating and cooling availability schedules to latent load
            if (((PurchAir(PurchAirNum).LatCoilLoad > 0.0) && !HeatOn) || ((PurchAir(PurchAirNum).LatCoilLoad < 0.0) && !CoolOn)) {
                // Coil is off
                PurchAir(PurchAirNum).LatCoilLoad = 0.0;
                PurchAir(PurchAirNum).SupplyHumRat = PurchAir(PurchAirNum).MixedAirHumRat;
            }

            // Double-check if saturation exceeded, then thow warning, shouldn't happen here, don't reset, just warn

            SupplyHumRatOrig = PurchAir(PurchAirNum).SupplyHumRat;
            SupplyHumRatSat = PsyWFnTdbRhPb(state, PurchAir(PurchAirNum).SupplyTemp, 1.0, state.dataEnvrn->OutBaroPress, RoutineName);

            DeltaHumRat = SupplyHumRatOrig - SupplyHumRatSat;
            if (DeltaHumRat > SmallDeltaHumRat) {
                if (PurchAir(PurchAirNum).SaturationOutputError < 1) {
                    ++PurchAir(PurchAirNum).SaturationOutputError;
                    ShowWarningError(state,
                                     format("{} \"{}\" Supply humidity ratio = {:.5T} exceeds saturation limit {:.5T} [kgWater/kgDryAir]",
                                            PurchAir(PurchAirNum).cObjectName,
                                            PurchAir(PurchAirNum).Name,
                                            SupplyHumRatOrig,
                                            SupplyHumRatSat));
                    ShowContinueError(state, " Simulation continuing . . . ");
                    ShowContinueErrorTimeStamp(state, "");
                } else {
                    ShowRecurringWarningErrorAtEnd(
                        state,
                        PurchAir(PurchAirNum).cObjectName + " \"" + PurchAir(PurchAirNum).Name +
                            "\" Supply humidity ratio exceeds saturation limit warning continues, delta max/min [kgWater/kgDryAir]...",
                        PurchAir(PurchAirNum).SaturationOutputIndex,
                        DeltaHumRat,
                        DeltaHumRat);
                }
            }

            SupplyEnthalpy = PsyHFnTdbW(PurchAir(PurchAirNum).SupplyTemp, PurchAir(PurchAirNum).SupplyHumRat);

            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ActualZoneNum));
            SysOutputProvided = SupplyMassFlowRate * CpAir * (PurchAir(PurchAirNum).SupplyTemp - state.dataLoopNodes->Node(ZoneNodeNum).Temp);
            MoistOutputProvided =
                SupplyMassFlowRate * (PurchAir(PurchAirNum).SupplyHumRat - state.dataLoopNodes->Node(ZoneNodeNum).HumRat); // Latent rate, kg/s

            PurchAir(PurchAirNum).SenOutputToZone = SysOutputProvided;
            PurchAir(PurchAirNum).LatOutputToZone =
                SupplyMassFlowRate * (SupplyEnthalpy - state.dataLoopNodes->Node(ZoneNodeNum).Enthalpy) - PurchAir(PurchAirNum).SenOutputToZone;

            CpAir = PsyCpAirFnW(state.dataHeatBalFanSys->ZoneAirHumRat(ActualZoneNum));
            if (PurchAir(PurchAirNum).OutdoorAir) {
                PurchAir(PurchAirNum).OASenOutput =
                    OAMassFlowRate * CpAir * (state.dataLoopNodes->Node(OANodeNum).Temp - state.dataLoopNodes->Node(ZoneNodeNum).Temp);
                PurchAir(PurchAirNum).OALatOutput =
                    OAMassFlowRate * (state.dataLoopNodes->Node(OANodeNum).Enthalpy - state.dataLoopNodes->Node(ZoneNodeNum).Enthalpy) -
                    PurchAir(PurchAirNum).OASenOutput;
            } else {
                PurchAir(PurchAirNum).OASenOutput = 0.0;
                PurchAir(PurchAirNum).OALatOutput = 0.0;
            }
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                if (PurchAir(PurchAirNum).OutdoorAir) {
                    state.dataLoopNodes->Node(InNodeNum).CO2 = ((SupplyMassFlowRate - OAMassFlowRate) * state.dataLoopNodes->Node(RecircNodeNum).CO2 +
                                                                OAMassFlowRate * state.dataLoopNodes->Node(OANodeNum).CO2) /
                                                               SupplyMassFlowRate;
                } else {
                    state.dataLoopNodes->Node(InNodeNum).CO2 = state.dataLoopNodes->Node(RecircNodeNum).CO2;
                }
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                if (PurchAir(PurchAirNum).OutdoorAir) {
                    state.dataLoopNodes->Node(InNodeNum).GenContam =
                        ((SupplyMassFlowRate - OAMassFlowRate) * state.dataLoopNodes->Node(RecircNodeNum).GenContam +
                         OAMassFlowRate * state.dataLoopNodes->Node(OANodeNum).GenContam) /
                        SupplyMassFlowRate;
                } else {
                    state.dataLoopNodes->Node(InNodeNum).GenContam = state.dataLoopNodes->Node(RecircNodeNum).GenContam;
                }
            }
        } else { // SupplyMassFlowRate = 0.0
            SysOutputProvided = 0.0;
            MoistOutputProvided = 0.0;

            PurchAir(PurchAirNum).SenOutputToZone = 0.0;
            PurchAir(PurchAirNum).LatOutputToZone = 0.0;
            PurchAir(PurchAirNum).SenCoilLoad = 0.0;
            PurchAir(PurchAirNum).LatCoilLoad = 0.0;
            PurchAir(PurchAirNum).OASenOutput = 0.0;
            PurchAir(PurchAirNum).OALatOutput = 0.0;

            PurchAir(PurchAirNum).MixedAirTemp = state.dataLoopNodes->Node(RecircNodeNum).Temp;
            PurchAir(PurchAirNum).MixedAirHumRat = state.dataLoopNodes->Node(RecircNodeNum).HumRat;
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {

                state.dataLoopNodes->Node(InNodeNum).CO2 = state.dataLoopNodes->Node(ZoneNodeNum).CO2;
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataLoopNodes->Node(InNodeNum).GenContam = state.dataLoopNodes->Node(ZoneNodeNum).GenContam;
            }
        }

        state.dataLoopNodes->Node(InNodeNum).Temp = PurchAir(PurchAirNum).SupplyTemp;
        state.dataLoopNodes->Node(InNodeNum).HumRat = PurchAir(PurchAirNum).SupplyHumRat;
        state.dataLoopNodes->Node(InNodeNum).Enthalpy = SupplyEnthalpy;
        state.dataLoopNodes->Node(InNodeNum).MassFlowRate = SupplyMassFlowRate;
        if (PurchAir(PurchAirNum).OutdoorAir) state.dataLoopNodes->Node(OANodeNum).MassFlowRate = OAMassFlowRate;

    } else { // purchased air OFF

        SysOutputProvided = 0.0;
        MoistOutputProvided = 0.0;
        SupplyMassFlowRate = 0.0;
        OAMassFlowRate = 0.0;
        state.dataLoopNodes->Node(InNodeNum).Temp = state.dataLoopNodes->Node(ZoneNodeNum).Temp;
        state.dataLoopNodes->Node(InNodeNum).HumRat = state.dataLoopNodes->Node(ZoneNodeNum).HumRat;
        state.dataLoopNodes->Node(InNodeNum).Enthalpy = state.dataLoopNodes->Node(ZoneNodeNum).Enthalpy;
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(InNodeNum).CO2 = state.dataLoopNodes->Node(ZoneNodeNum).CO2;
        }
        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(InNodeNum).GenContam = state.dataLoopNodes->Node(ZoneNodeNum).GenContam;
        }

        state.dataLoopNodes->Node(InNodeNum).MassFlowRate = 0.0;
        if (PurchAir(PurchAirNum).OutdoorAir) state.dataLoopNodes->Node(OANodeNum).MassFlowRate = 0.0;
        PurchAir(PurchAirNum).SenHeatRate = 0.0;
        PurchAir(PurchAirNum).SenCoolRate = 0.0;
        PurchAir(PurchAirNum).TotCoolRate = 0.0;

        PurchAir(PurchAirNum).SenOutputToZone = 0.0;
        PurchAir(PurchAirNum).LatOutputToZone = 0.0;
        PurchAir(PurchAirNum).SenCoilLoad = 0.0;
        PurchAir(PurchAirNum).LatCoilLoad = 0.0;
        PurchAir(PurchAirNum).OASenOutput = 0.0;
        PurchAir(PurchAirNum).OALatOutput = 0.0;
        PurchAir(PurchAirNum).MixedAirTemp = state.dataLoopNodes->Node(RecircNodeNum).Temp;
        PurchAir(PurchAirNum).MixedAirHumRat = state.dataLoopNodes->Node(RecircNodeNum).HumRat;
        PurchAir(PurchAirNum).SupplyTemp = state.dataLoopNodes->Node(InNodeNum).Temp;
        PurchAir(PurchAirNum).SupplyHumRat = state.dataLoopNodes->Node(InNodeNum).HumRat;
    }

    PurchAir(PurchAirNum).OutdoorAirMassFlowRate = OAMassFlowRate;
    PurchAir(PurchAirNum).OutdoorAirVolFlowRateStdRho = OAMassFlowRate / state.dataEnvrn->StdRhoAir;
    PurchAir(PurchAirNum).SupplyAirMassFlowRate = SupplyMassFlowRate;

    PurchAir(PurchAirNum).SupplyAirVolFlowRateStdRho = SupplyMassFlowRate / state.dataEnvrn->StdRhoAir;

    if (PurchAir(PurchAirNum).PlenumExhaustAirNodeNum > 0) {
        state.dataLoopNodes->Node(PurchAir(PurchAirNum).PlenumExhaustAirNodeNum).MassFlowRate = SupplyMassFlowRate;
    }
    state.dataLoopNodes->Node(RecircNodeNum).MassFlowRate = SupplyMassFlowRate;
}

void CalcPurchAirMinOAMassFlow(EnergyPlusData &state,
                               int const PurchAirNum,   // index to ideal loads unit
                               int const ActualZoneNum, // index to actual zone number
                               Real64 &OAMassFlowRate   // outside air mass flow rate [kg/s] from volume flow using std density
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         M. Witte (GARD)
    //       DATE WRITTEN   Jun 2011 (taken from HVACSingleDuctSystem.cc and adapted for Ideal Loads System)
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the amount of outside air required based on optional user input.
    // Zone multipliers have been applied in GetInput.

    // METHODOLOGY EMPLOYED:
    // User input defines method used to calculate OA.

    // Using/Aliasing
    using DataZoneEquipment::CalcDesignSpecificationOutdoorAir;

    // FUNCTION PARAMETER DEFINITIONS:
    bool const UseMinOASchFlag(true); // Always use min OA schedule in calculations.

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    bool UseOccSchFlag;      // TRUE = use actual occupancy, FALSE = use total zone people
    Real64 OAVolumeFlowRate; // outside air flow rate (m3/s)

    auto &PurchAir(state.dataPurchasedAirMgr->PurchAir);

    if (PurchAir(PurchAirNum).OutdoorAir) {

        if (PurchAir(PurchAirNum).DCVType == DCV::OccupancySchedule) {
            UseOccSchFlag = true;
        } else {
            UseOccSchFlag = false;
        }
        OAVolumeFlowRate =
            CalcDesignSpecificationOutdoorAir(state, PurchAir(PurchAirNum).OARequirementsPtr, ActualZoneNum, UseOccSchFlag, UseMinOASchFlag);
        OAMassFlowRate = OAVolumeFlowRate * state.dataEnvrn->StdRhoAir;

        // If DCV with CO2SetPoint then check required OA flow to meet CO2 setpoint
        if (PurchAir(PurchAirNum).DCVType == DCV::CO2SetPoint) {
            OAMassFlowRate = max(OAMassFlowRate, state.dataContaminantBalance->ZoneSysContDemand(ActualZoneNum).OutputRequiredToCO2SP);
        }

        if (OAMassFlowRate <= VerySmallMassFlow) OAMassFlowRate = 0.0;

    } else { // No outdoor air
        OAMassFlowRate = 0.0;
    }
    PurchAir(PurchAirNum).MinOAMassFlowRate = OAMassFlowRate;
}

void CalcPurchAirMixedAir(EnergyPlusData &state,
                          int const PurchAirNum,           // index to ideal loads unit
                          Real64 const OAMassFlowRate,     // outside air mass flow rate [kg/s]
                          Real64 const SupplyMassFlowRate, // supply air mass flow rate [kg/s]
                          Real64 &MixedAirTemp,            // Mixed air dry bulb temperature [C]
                          Real64 &MixedAirHumRat,          // Mixed air humidity ratio [kgWater/kgDryAir]
                          Real64 &MixedAirEnthalpy,        // Mixed air enthalpy [J/kg]
                          OpMode const OperatingMode       // current operating mode, Off, Heating, Cooling, or DeadBand
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         M. Witte (GARD)
    //       DATE WRITTEN   Sep 2011
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculates the mixed air conditions, accounting for heat recovery.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("CalcPurchAirMixedAir");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int RecircNodeNum;           // Zone return air node
    int OANodeNum;               // Outdoor air inlet node
    Real64 RecircTemp;           // Recirculated air from zone dry bulb temperature [C]
    Real64 RecircHumRat;         // Recirculated air from zone humidity ratio [kgWater/kgDryAir]
    Real64 RecircEnthalpy;       // Recirculated air from zone enthalpy [J/kg]
    Real64 RecircMassFlowRate;   // Recirculated air mass flow rate [kg/s]
    Real64 OAInletTemp;          // Outdoor air inlet dry bulb temperature [C]
    Real64 OAInletHumRat;        // Outdoor air inlet humidity ratio [kgWater/kgDryAir]
    Real64 OAInletEnthalpy;      // Outdoor air inlet enthalpy [J/kg]
    Real64 OAAfterHtRecTemp;     // Outdoor air after heat recovery to mixing box dry bulb temperature [C]
    Real64 OAAfterHtRecHumRat;   // Outdoor air after heat recovery to mixing box humidity ratio [kgWater/kgDryAir]
    Real64 OAAfterHtRecEnthalpy; // Outdoor air after heat recovery to mixing box enthalpy [J/kg]
    bool HeatRecOn;
    Real64 CpAir; // Specific heat [J/kg-C] reused in multiple places

    auto &PurchAir(state.dataPurchasedAirMgr->PurchAir);

    // Initializations
    OANodeNum = PurchAir(PurchAirNum).OutdoorAirNodeNum;
    RecircNodeNum = PurchAir(PurchAirNum).ZoneRecircAirNodeNum;

    RecircMassFlowRate = 0.0;
    RecircTemp = state.dataLoopNodes->Node(RecircNodeNum).Temp;
    RecircHumRat = state.dataLoopNodes->Node(RecircNodeNum).HumRat;
    RecircEnthalpy = state.dataLoopNodes->Node(RecircNodeNum).Enthalpy;
    if (PurchAir(PurchAirNum).OutdoorAir) {
        OAInletTemp = state.dataLoopNodes->Node(OANodeNum).Temp;
        OAInletHumRat = state.dataLoopNodes->Node(OANodeNum).HumRat;
        OAInletEnthalpy = state.dataLoopNodes->Node(OANodeNum).Enthalpy;
        OAAfterHtRecTemp = OAInletTemp;
        OAAfterHtRecHumRat = OAInletHumRat;
        OAAfterHtRecEnthalpy = OAInletEnthalpy;
    } else {
        OAInletTemp = 0.0;
        OAInletHumRat = 0.0;
        OAInletEnthalpy = 0.0;
        OAAfterHtRecTemp = OAInletTemp;
        OAAfterHtRecHumRat = OAInletHumRat;
        OAAfterHtRecEnthalpy = OAInletEnthalpy;
    }
    HeatRecOn = false;

    if (PurchAir(PurchAirNum).OutdoorAir && (OAMassFlowRate > 0.0)) {
        // Determine if heat recovery is beneficial
        if (PurchAir(PurchAirNum).HtRecType == HeatRecovery::Sensible) {
            if ((OperatingMode == OpMode::Heat) && (RecircTemp > OAInletTemp)) HeatRecOn = true;
            if ((OperatingMode == OpMode::Cool) && (RecircTemp < OAInletTemp)) HeatRecOn = true;
        }
        if (PurchAir(PurchAirNum).HtRecType == HeatRecovery::Enthalpy) {
            if ((OperatingMode == OpMode::Heat) && (RecircEnthalpy > OAInletEnthalpy)) HeatRecOn = true;
            if ((OperatingMode == OpMode::Cool) && (RecircEnthalpy < OAInletEnthalpy)) HeatRecOn = true;
        }
        // Calculate heat recovery if active
        if (HeatRecOn) {
            PurchAir(PurchAirNum).TimeHtRecActive = state.dataHVACGlobal->TimeStepSys;
            OAAfterHtRecTemp = OAInletTemp + PurchAir(PurchAirNum).HtRecSenEff * (RecircTemp - OAInletTemp);
            if (PurchAir(PurchAirNum).HtRecType == HeatRecovery::Enthalpy)
                OAAfterHtRecHumRat = OAInletHumRat + PurchAir(PurchAirNum).HtRecLatEff * (RecircHumRat - OAInletHumRat);
            OAAfterHtRecEnthalpy = PsyHFnTdbW(OAAfterHtRecTemp, OAAfterHtRecHumRat);
            //   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
            if (PsyTsatFnHPb(state, OAAfterHtRecEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName) > OAAfterHtRecTemp) {
                OAAfterHtRecTemp = PsyTsatFnHPb(state, OAAfterHtRecEnthalpy, state.dataEnvrn->OutBaroPress, RoutineName);
                OAAfterHtRecHumRat = PsyWFnTdbH(state, OAAfterHtRecTemp, OAAfterHtRecEnthalpy, RoutineName);
            }
        }

        if (SupplyMassFlowRate > OAMassFlowRate) {
            RecircMassFlowRate = SupplyMassFlowRate - OAMassFlowRate;
            MixedAirEnthalpy =
                (RecircMassFlowRate * state.dataLoopNodes->Node(RecircNodeNum).Enthalpy + OAMassFlowRate * OAAfterHtRecEnthalpy) / SupplyMassFlowRate;
            MixedAirHumRat =
                (RecircMassFlowRate * state.dataLoopNodes->Node(RecircNodeNum).HumRat + OAMassFlowRate * OAAfterHtRecHumRat) / SupplyMassFlowRate;
            // Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
            MixedAirTemp = PsyTdbFnHW(MixedAirEnthalpy, MixedAirHumRat);
        } else {
            RecircMassFlowRate = 0.0;
            MixedAirEnthalpy = OAAfterHtRecEnthalpy;
            MixedAirHumRat = OAAfterHtRecHumRat;
            MixedAirTemp = OAAfterHtRecTemp;
        }

        // Calculate OA and heat recovery sensible and latent rates
        CpAir = PsyCpAirFnW(OAInletHumRat);
        PurchAir(PurchAirNum).HtRecSenOutput = OAMassFlowRate * CpAir * (OAAfterHtRecTemp - OAInletTemp);
        PurchAir(PurchAirNum).HtRecLatOutput = OAMassFlowRate * (OAAfterHtRecEnthalpy - OAInletEnthalpy) - PurchAir(PurchAirNum).HtRecSenOutput;

    } else { // No outdoor air
        RecircMassFlowRate = SupplyMassFlowRate;
        MixedAirTemp = RecircTemp;
        MixedAirHumRat = RecircHumRat;
        MixedAirEnthalpy = RecircEnthalpy;
        PurchAir(PurchAirNum).HtRecSenOutput = 0.0;
        PurchAir(PurchAirNum).HtRecLatOutput = 0.0;
    }
}

void UpdatePurchasedAir(EnergyPlusData &state, int const PurchAirNum, bool const FirstHVACIteration)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         M. J. Witte
    //       DATE WRITTEN   Sep 2011
    //       MODIFIED       R. Raustad, July 2017, added return plenum
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update node data for Ideal Loads (purchased air) system

    // USE STATEMENTS:
    using ZonePlenum::SimAirZonePlenum;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    // na
    bool FirstCall;
    bool SupPathInletChanged;

    FirstCall = true;            // just used to avoid redundant calulations
    SupPathInletChanged = false; // don't care if something changes

    auto &PurchAir(state.dataPurchasedAirMgr->PurchAir);

    if (PurchAir(PurchAirNum).ReturnPlenumIndex > 0) {

        // if connected to a return plenum, set the flag that this ideal loads air system was simulated
        state.dataPurchasedAirMgr->PurchAirPlenumArrays(PurchAir(PurchAirNum).ReturnPlenumIndex)
            .IsSimulated(PurchAir(PurchAirNum).PurchAirArrayIndex) = true;

        // if all ideal loads air systems connected to the same plenum have been simulated, simulate the zone air plenum
        if (all(state.dataPurchasedAirMgr->PurchAirPlenumArrays(PurchAir(PurchAirNum).ReturnPlenumIndex).IsSimulated)) {
            SimAirZonePlenum(state,
                             PurchAir(PurchAirNum).ReturnPlenumName,
                             DataZoneEquipment::ZoneReturnPlenum_Type,
                             PurchAir(PurchAirNum).ReturnPlenumIndex,
                             FirstHVACIteration,
                             FirstCall,
                             SupPathInletChanged);
            // reset this plenums flags for next iteration
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(PurchAir(PurchAirNum).ReturnPlenumIndex).IsSimulated = false;
        }
    }
}

void ReportPurchasedAir(EnergyPlusData &state, int const PurchAirNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Russ Taylor
    //       DATE WRITTEN   Nov 1997
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate values of report variables, if necessary.

    // Using/Aliasing
    auto &TimeStepSys = state.dataHVACGlobal->TimeStepSys;

    auto &PurchAir(state.dataPurchasedAirMgr->PurchAir);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 ReportingConstant;

    // Sort out heating and cooling rates
    PurchAir(PurchAirNum).SenHeatRate = max(PurchAir(PurchAirNum).SenCoilLoad, 0.0);
    PurchAir(PurchAirNum).SenCoolRate = std::abs(min(PurchAir(PurchAirNum).SenCoilLoad, 0.0));
    PurchAir(PurchAirNum).LatHeatRate = max(PurchAir(PurchAirNum).LatCoilLoad, 0.0);
    PurchAir(PurchAirNum).LatCoolRate = std::abs(min(PurchAir(PurchAirNum).LatCoilLoad, 0.0));
    PurchAir(PurchAirNum).TotHeatRate = PurchAir(PurchAirNum).SenHeatRate + PurchAir(PurchAirNum).LatHeatRate;
    PurchAir(PurchAirNum).TotCoolRate = PurchAir(PurchAirNum).SenCoolRate + PurchAir(PurchAirNum).LatCoolRate;

    PurchAir(PurchAirNum).ZoneSenHeatRate = max(PurchAir(PurchAirNum).SenOutputToZone, 0.0);
    PurchAir(PurchAirNum).ZoneSenCoolRate = std::abs(min(PurchAir(PurchAirNum).SenOutputToZone, 0.0));
    PurchAir(PurchAirNum).ZoneLatHeatRate = max(PurchAir(PurchAirNum).LatOutputToZone, 0.0);
    PurchAir(PurchAirNum).ZoneLatCoolRate = std::abs(min(PurchAir(PurchAirNum).LatOutputToZone, 0.0));
    PurchAir(PurchAirNum).ZoneTotHeatRate = PurchAir(PurchAirNum).ZoneSenHeatRate + PurchAir(PurchAirNum).ZoneLatHeatRate;
    PurchAir(PurchAirNum).ZoneTotCoolRate = PurchAir(PurchAirNum).ZoneSenCoolRate + PurchAir(PurchAirNum).ZoneLatCoolRate;

    // Sort out outdoor air "loads"
    // OASenOutput = Outdoor air sensible output relative to zone conditions [W], <0 means OA is cooler than zone air
    // OALatOutput  = Outdoor air latent output relative to zone conditions [W], <0 means OA is drier than zone air
    if (PurchAir(PurchAirNum).SenCoilLoad > 0.0) { // Heating is active
        PurchAir(PurchAirNum).OASenHeatRate = std::abs(min(PurchAir(PurchAirNum).OASenOutput, 0.0));
    } else {
        PurchAir(PurchAirNum).OASenHeatRate = 0.0;
    }
    if (PurchAir(PurchAirNum).SenCoilLoad < 0.0) { // Cooling is active
        PurchAir(PurchAirNum).OASenCoolRate = max(PurchAir(PurchAirNum).OASenOutput, 0.0);
    } else {
        PurchAir(PurchAirNum).OASenCoolRate = 0.0;
    }
    if (PurchAir(PurchAirNum).LatCoilLoad > 0.0) { // Humidification is active
        PurchAir(PurchAirNum).OALatHeatRate = std::abs(min(PurchAir(PurchAirNum).OALatOutput, 0.0));
    } else {
        PurchAir(PurchAirNum).OALatHeatRate = 0.0;
    }
    if (PurchAir(PurchAirNum).LatCoilLoad < 0.0) { // Dehumidification is active
        PurchAir(PurchAirNum).OALatCoolRate = max(PurchAir(PurchAirNum).OALatOutput, 0.0);
    } else {
        PurchAir(PurchAirNum).OALatCoolRate = 0.0;
    }

    PurchAir(PurchAirNum).OATotHeatRate = PurchAir(PurchAirNum).OASenHeatRate + PurchAir(PurchAirNum).OALatHeatRate;
    PurchAir(PurchAirNum).OATotCoolRate = PurchAir(PurchAirNum).OASenCoolRate + PurchAir(PurchAirNum).OALatCoolRate;

    PurchAir(PurchAirNum).HtRecSenHeatRate = max(PurchAir(PurchAirNum).HtRecSenOutput, 0.0);
    PurchAir(PurchAirNum).HtRecSenCoolRate = std::abs(min(PurchAir(PurchAirNum).HtRecSenOutput, 0.0));
    PurchAir(PurchAirNum).HtRecLatHeatRate = max(PurchAir(PurchAirNum).HtRecLatOutput, 0.0);
    PurchAir(PurchAirNum).HtRecLatCoolRate = std::abs(min(PurchAir(PurchAirNum).HtRecLatOutput, 0.0));
    PurchAir(PurchAirNum).HtRecTotHeatRate = PurchAir(PurchAirNum).HtRecSenHeatRate + PurchAir(PurchAirNum).HtRecLatHeatRate;
    PurchAir(PurchAirNum).HtRecTotCoolRate = PurchAir(PurchAirNum).HtRecSenCoolRate + PurchAir(PurchAirNum).HtRecLatCoolRate;

    ReportingConstant = TimeStepSys * DataGlobalConstants::SecInHour;

    PurchAir(PurchAirNum).SenHeatEnergy = PurchAir(PurchAirNum).SenHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).SenCoolEnergy = PurchAir(PurchAirNum).SenCoolRate * ReportingConstant;
    PurchAir(PurchAirNum).LatHeatEnergy = PurchAir(PurchAirNum).LatHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).LatCoolEnergy = PurchAir(PurchAirNum).LatCoolRate * ReportingConstant;
    PurchAir(PurchAirNum).TotHeatEnergy = PurchAir(PurchAirNum).TotHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).TotCoolEnergy = PurchAir(PurchAirNum).TotCoolRate * ReportingConstant;

    PurchAir(PurchAirNum).ZoneSenHeatEnergy = PurchAir(PurchAirNum).ZoneSenHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).ZoneSenCoolEnergy = PurchAir(PurchAirNum).ZoneSenCoolRate * ReportingConstant;
    PurchAir(PurchAirNum).ZoneLatHeatEnergy = PurchAir(PurchAirNum).ZoneLatHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).ZoneLatCoolEnergy = PurchAir(PurchAirNum).ZoneLatCoolRate * ReportingConstant;
    PurchAir(PurchAirNum).ZoneTotHeatEnergy = PurchAir(PurchAirNum).ZoneTotHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).ZoneTotCoolEnergy = PurchAir(PurchAirNum).ZoneTotCoolRate * ReportingConstant;

    PurchAir(PurchAirNum).OASenHeatEnergy = PurchAir(PurchAirNum).OASenHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).OASenCoolEnergy = PurchAir(PurchAirNum).OASenCoolRate * ReportingConstant;
    PurchAir(PurchAirNum).OALatHeatEnergy = PurchAir(PurchAirNum).OALatHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).OALatCoolEnergy = PurchAir(PurchAirNum).OALatCoolRate * ReportingConstant;
    PurchAir(PurchAirNum).OATotHeatEnergy = PurchAir(PurchAirNum).OATotHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).OATotCoolEnergy = PurchAir(PurchAirNum).OATotCoolRate * ReportingConstant;

    PurchAir(PurchAirNum).HtRecSenHeatEnergy = PurchAir(PurchAirNum).HtRecSenHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).HtRecSenCoolEnergy = PurchAir(PurchAirNum).HtRecSenCoolRate * ReportingConstant;
    PurchAir(PurchAirNum).HtRecLatHeatEnergy = PurchAir(PurchAirNum).HtRecLatHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).HtRecLatCoolEnergy = PurchAir(PurchAirNum).HtRecLatCoolRate * ReportingConstant;
    PurchAir(PurchAirNum).HtRecTotHeatEnergy = PurchAir(PurchAirNum).HtRecTotHeatRate * ReportingConstant;
    PurchAir(PurchAirNum).HtRecTotCoolEnergy = PurchAir(PurchAirNum).HtRecTotCoolRate * ReportingConstant;
}

Real64 GetPurchasedAirOutAirMassFlow(EnergyPlusData &state, int const PurchAirNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Dec  2006
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // lookup function for OA inlet mass flow for ventilation rate reporting

    // METHODOLOGY EMPLOYED:
    // most analagous functions look up an outside air node but this function
    // gets the actual mass flow of outdoor air, following the features of the model

    if (state.dataPurchasedAirMgr->GetPurchAirInputFlag) {
        GetPurchasedAir(state);
        state.dataPurchasedAirMgr->GetPurchAirInputFlag = false;
    }
    return state.dataPurchasedAirMgr->PurchAir(PurchAirNum).OutdoorAirMassFlowRate;
}

int GetPurchasedAirZoneInletAirNode(EnergyPlusData &state, int const PurchAirNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Dec  2006
    //       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // lookup function for zone inlet node for ventilation rate reporting

    if (state.dataPurchasedAirMgr->GetPurchAirInputFlag) {
        GetPurchasedAir(state);
        state.dataPurchasedAirMgr->GetPurchAirInputFlag = false;
    }

    int GetPurchasedAirZoneInletAirNode = 0;
    if (PurchAirNum > 0 && PurchAirNum <= state.dataPurchasedAirMgr->NumPurchAir) {
        GetPurchasedAirZoneInletAirNode = state.dataPurchasedAirMgr->PurchAir(PurchAirNum).ZoneSupplyAirNodeNum;
    }

    return GetPurchasedAirZoneInletAirNode;
}

int GetPurchasedAirReturnAirNode(EnergyPlusData &state, int const PurchAirNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Dec  2006
    //       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // lookup function for recirculation air node for ventilation rate reporting

    if (state.dataPurchasedAirMgr->GetPurchAirInputFlag) {
        GetPurchasedAir(state);
        state.dataPurchasedAirMgr->GetPurchAirInputFlag = false;
    }

    int GetPurchasedAirReturnAirNode = 0;
    if (PurchAirNum > 0 && PurchAirNum <= state.dataPurchasedAirMgr->NumPurchAir) {
        GetPurchasedAirReturnAirNode = state.dataPurchasedAirMgr->PurchAir(PurchAirNum).ZoneRecircAirNodeNum;
    }

    return GetPurchasedAirReturnAirNode;
}

Real64 GetPurchasedAirMixedAirTemp(EnergyPlusData &state, int const PurchAirNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Dec  2006
    //       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // lookup function for mixed air Temp for ventilation rate reporting

    // METHODOLOGY EMPLOYED:
    // most analagous functions look up an outside air node but this function
    // gets the actual mass flow of outdoor air, following the features of the model

    if (state.dataPurchasedAirMgr->GetPurchAirInputFlag) {
        GetPurchasedAir(state);
        state.dataPurchasedAirMgr->GetPurchAirInputFlag = false;
    }

    return state.dataPurchasedAirMgr->PurchAir(PurchAirNum).MixedAirTemp;
}

Real64 GetPurchasedAirMixedAirHumRat(EnergyPlusData &state, int const PurchAirNum)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         B Griffith
    //       DATE WRITTEN   Dec  2006
    //       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // lookup function for mixed air HumRat for ventilation rate reporting

    // METHODOLOGY EMPLOYED:
    // most analogous functions look up an outside air node but this function
    // gets the actual mass flow of outdoor air, following the features of the model

    if (state.dataPurchasedAirMgr->GetPurchAirInputFlag) {
        GetPurchasedAir(state);
        state.dataPurchasedAirMgr->GetPurchAirInputFlag = false;
    }

    return state.dataPurchasedAirMgr->PurchAir(PurchAirNum).MixedAirHumRat;
}

bool CheckPurchasedAirForReturnPlenum(EnergyPlusData &state, int const &ReturnPlenumIndex)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         R Raustad
    //       DATE WRITTEN   July  2017

    // PURPOSE OF THIS FUNCTION:
    // lookup function to check if return plenum is used

    // Return value
    bool CheckPurchasedAirForReturnPlenum;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int PurchAirNum;

    if (state.dataPurchasedAirMgr->GetPurchAirInputFlag) {
        GetPurchasedAir(state);
        state.dataPurchasedAirMgr->GetPurchAirInputFlag = false;
    }

    CheckPurchasedAirForReturnPlenum = false;
    for (PurchAirNum = 1; PurchAirNum <= state.dataPurchasedAirMgr->NumPurchAir; ++PurchAirNum) {
        if (ReturnPlenumIndex != state.dataPurchasedAirMgr->PurchAir(PurchAirNum).ReturnPlenumIndex) continue;
        CheckPurchasedAirForReturnPlenum = true;
    }

    return CheckPurchasedAirForReturnPlenum;
}

void InitializePlenumArrays(EnergyPlusData &state, int const PurchAirNum)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         R Raustad
    //       DATE WRITTEN   July  2017

    // PURPOSE OF THIS FUNCTION:
    // to initialize arrays needed to manage ideal load air system used with return plenums
    //
    // Example:
    // NumPlenumArrays = 2 (same as there are two ZoneHVAC:ReturnPlenums objects connected to two or more ideal loads air systems
    // In this example ideal loads air system #4 is not connected to a zone return plenum
    //
    // ZoneHVAC:ReturnPlenum( 1 ) = ReturnPlenum1 is not connected to any ideal loads air systems
    // ZoneHVAC:ReturnPlenum( 2 ) = ReturnPlenum2 is connected to PurchAirPlenumArrays( 1 )
    // ZoneHVAC:ReturnPlenum( 3 ) = ReturnPlenum3 is connected to PurchAirPlenumArrays( 2 )
    //
    // PurchAirPlenumArrays( 1 )
    //   PurchAirPlenumArrays( 1 ).NumPurchAir = 2, there are 2 ideal loads air systems connected to this plenum
    //      PurchAirPlenumArrays( 1 ).PurchAirArray( 1 ) = 1, ideal loads air system #1 is attached to this plenum
    //      PurchAirPlenumArrays( 1 ).PurchAirArray( 2 ) = 3, ideal loads air system #3 is attached to this plenum
    //      PurchAirPlenumArrays( 1 ).IsSimulated( 1 ) = true, ideal loads air system #1 has been simulated this iteration
    //      PurchAirPlenumArrays( 1 ).IsSimulated( 2 ) = false, ideal loads air system #3 has not yet been simulated this iteration
    //
    //      Ideal loads air sytems keep track of which plenum they are connected to
    //      PurchAir( 1 ).PlenumArrayIndex = 1
    //      PurchAir( 1 ).ReturnPlenumName = ReturnPlenum2;
    //      PurchAir( 3 ).PlenumArrayIndex = 1
    //      PurchAir( 3 ).ReturnPlenumName = ReturnPlenum2;
    //
    //      The ideal loads air sytems also keep track of which item they are in the int and bool arrays
    //      PurchAir( 1 ).PurchAirArrayIndex = 1
    //      PurchAir( 3 ).PurchAirArrayIndex = 2
    //
    // PurchAirPlenumArrays( 2 )
    //   PurchAirPlenumArrays( 2 ).NumPurchAir = 3, there are 3 ideal loads air systems connected to this plenum
    //      PurchAirPlenumArrays( 2 ).PurchAirArray( 1 ) = 2, ideal loads air system #2 is attached to this plenum
    //      PurchAirPlenumArrays( 2 ).PurchAirArray( 2 ) = 5, ideal loads air system #5 is attached to this plenum
    //      PurchAirPlenumArrays( 2 ).PurchAirArray( 3 ) = 6, ideal loads air system #6 is attached to this plenum
    //      PurchAirPlenumArrays( 2 ).IsSimulated( 1 ) = true, ideal loads air system #4 has been simulated this iteration
    //      PurchAirPlenumArrays( 2 ).IsSimulated( 2 ) = false, ideal loads air system #5 has not yet been simulated this iteration
    //      PurchAirPlenumArrays( 2 ).IsSimulated( 3 ) = false, ideal loads air system #6 has not yet been simulated this iteration
    //
    //      Ideal loads air sytems keep track of which plenum they are connected to
    //      PurchAir( 2 ).PlenumArrayIndex = 2;
    //      PurchAir( 2 ).ReturnPlenumName = ReturnPlenum3;
    //      PurchAir( 5 ).PlenumArrayIndex = 2;
    //      PurchAir( 5 ).ReturnPlenumName = ReturnPlenum3;
    //      PurchAir( 6 ).PlenumArrayIndex = 2;
    //      PurchAir( 6 ).ReturnPlenumName = ReturnPlenum3;
    //
    //      The ideal loads air sytems also keep track of which item they are in the int and bool arrays
    //      PurchAir( 2 ).PurchAirArrayIndex = 1;
    //      PurchAir( 5 ).PurchAirArrayIndex = 2;
    //      PurchAir( 6 ).PurchAirArrayIndex = 3;
    //
    //      Given these connections, the data in the IsSimulated array can be set (or checked) according to this syntax:
    //
    //      Each time an ideal loads air system is simulated the IsSimulated flag is set to true
    //      PurchAirPlenumArrays( PurchAir( PurchNum ).PlenumArrayIndex ).IsSimulated( PurchAir( PurchNum ).PurchAirArrayIndex ) = true;
    //
    //     if all ideal loads air systems connected to the same plenum have been simulated, simulate the zone air return plenum (once per set of
    //     ideal loads systems) if ( all( PurchAirPlenumArrays( PurchAir( PurchAirNum ).ReturnPlenumIndex ).IsSimulated ) ) {
    //         SimAirZonePlenum( PurchAir( PurchAirNum ).ReturnPlenumName, DataZoneEquipment::ZoneReturnPlenum_Type, PurchAir( PurchAirNum
    //         ).ReturnPlenumIndex, FirstHVACIteration, FirstCall, SupPathInletChanged ); reset all IsSimulated flags for next iteration
    //         PurchAirPlenumArrays( PurchAir( PurchAirNum ).ReturnPlenumIndex ).IsSimulated = false;
    //     }

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int ReturnPlenumIndex;        // index to ZoneHVAC:ReturnPlenum object
    int ReturnPlenumNum;          // loop counter
    bool PlenumNotFound;          // logical to determine if same plenum is used by other ideal loads air systems
    int Loop;                     // loop counters
    int Loop2;                    // loop counters
    Array1D_int TempPurchArray;   // temporary array used for dynamic allocation
    Array1D_bool TempIsSimulated; // temporary array used for dynamic allocation

    // index to ZoneHVAC:ReturnPlenum object
    ReturnPlenumIndex = state.dataPurchasedAirMgr->PurchAir(PurchAirNum).ReturnPlenumIndex;
    PlenumNotFound = true;

    // if first time through, set up arrays
    if (!state.dataPurchasedAirMgr->PurchAirPlenumArrays.allocated()) {

        // the ideal loads air system keeps track of which item this system is in a list
        state.dataPurchasedAirMgr->PurchAir(PurchAirNum).PurchAirArrayIndex = 1;
        // keep track of how many arrays (i.e., how many different plenums are attached to different ideal loads air systems
        state.dataPurchasedAirMgr->NumPlenumArrays = 1;

        // allocate new array
        state.dataPurchasedAirMgr->PurchAirPlenumArrays.allocate(state.dataPurchasedAirMgr->NumPlenumArrays);
        // set counter for how many ideal loads air systems are attached to this plenum
        state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).NumPurchAir =
            1; // keeps track of how many ideal loads air system are connected to this return plenum
        // keep track of which plenum this is ( i.e., PurchAirPlenumArrays(1) is ZoneHVAC:ReturnPlenum #4 )
        state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).ReturnPlenumIndex =
            ReturnPlenumIndex; // stores index of return plenum (e.g., 4 of 5)
        // allocate array holding index to one or more ideal loads air systems
        state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).PurchAirArray.allocate(1);
        // allocate boolean to keep track of whether or not this ideal loads air system has been simulated
        state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).IsSimulated.allocate(1);
        // save the data
        state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).PurchAirArray(1) = PurchAirNum;
        state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).IsSimulated(1) = false;

    } else {

        // find the correct index to PurchAirPlenumArrays
        for (ReturnPlenumNum = 1; ReturnPlenumNum <= state.dataPurchasedAirMgr->NumPlenumArrays; ++ReturnPlenumNum) {
            if (ReturnPlenumIndex != state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).ReturnPlenumIndex) continue;

            // allocate temporary arrays and save existing data
            TempPurchArray.allocate(state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).NumPurchAir);
            TempIsSimulated.allocate(state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).NumPurchAir);
            // these are the  member arrays in an existing PurchAirPlenumArrays
            TempPurchArray = state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).PurchAirArray;
            TempIsSimulated = state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).IsSimulated;

            // if this array has been used before, we need to increase member array space to save new PurchAir data
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).NumPurchAir += 1;
            // save the location of this ideal loads air system in the member arrays
            state.dataPurchasedAirMgr->PurchAir(PurchAirNum).PurchAirArrayIndex =
                state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).NumPurchAir;

            // allocate more space, this will wipe out data previously stored
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum)
                .PurchAirArray.allocate(state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).NumPurchAir);
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum)
                .IsSimulated.allocate(state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).NumPurchAir);

            // re-initialize previous data
            for (Loop = 1; Loop < state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).NumPurchAir; ++Loop) {
                state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).PurchAirArray(Loop) = TempPurchArray(Loop);
                state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).IsSimulated(Loop) = TempIsSimulated(Loop);
            }
            // delete temporary array
            TempPurchArray.deallocate();
            TempIsSimulated.deallocate();

            // save new data in expanded member array
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum)
                .PurchAirArray(state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).NumPurchAir) = PurchAirNum;
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum)
                .IsSimulated(state.dataPurchasedAirMgr->PurchAirPlenumArrays(ReturnPlenumNum).NumPurchAir) = false;

            PlenumNotFound = false;
            break;
        }

        if (PlenumNotFound) {

            // need to allocate additional space for new plenum array
            // keep track of how many arrays (i.e., how many different plenums are attached to different ideal loads air systems)
            state.dataPurchasedAirMgr->NumPlenumArrays += 1;

            // allocate temporary array and save existing data
            state.dataPurchasedAirMgr->TempPurchAirPlenumArrays.allocate(state.dataPurchasedAirMgr->NumPlenumArrays);
            for (Loop = 1; Loop < state.dataPurchasedAirMgr->NumPlenumArrays; ++Loop) {
                state.dataPurchasedAirMgr->TempPurchAirPlenumArrays(Loop).NumPurchAir =
                    state.dataPurchasedAirMgr->PurchAirPlenumArrays(Loop).NumPurchAir;
                state.dataPurchasedAirMgr->TempPurchAirPlenumArrays(Loop).ReturnPlenumIndex =
                    state.dataPurchasedAirMgr->PurchAirPlenumArrays(Loop).ReturnPlenumIndex;
                state.dataPurchasedAirMgr->TempPurchAirPlenumArrays(Loop).PurchAirArray.allocate(
                    state.dataPurchasedAirMgr->PurchAirPlenumArrays(Loop).NumPurchAir);
                state.dataPurchasedAirMgr->TempPurchAirPlenumArrays(Loop).IsSimulated.allocate(
                    state.dataPurchasedAirMgr->PurchAirPlenumArrays(Loop).NumPurchAir);
                for (Loop2 = 1; Loop2 <= state.dataPurchasedAirMgr->PurchAirPlenumArrays(Loop).NumPurchAir; ++Loop2) {
                    state.dataPurchasedAirMgr->TempPurchAirPlenumArrays(Loop).PurchAirArray(Loop2) =
                        state.dataPurchasedAirMgr->PurchAirPlenumArrays(Loop).PurchAirArray(Loop2);
                    state.dataPurchasedAirMgr->TempPurchAirPlenumArrays(Loop).IsSimulated(Loop2) =
                        state.dataPurchasedAirMgr->PurchAirPlenumArrays(Loop).IsSimulated(Loop2);
                }
            }

            // delete primary array (probably could just re-allocate, but this is only done a few times per simulation)
            state.dataPurchasedAirMgr->PurchAirPlenumArrays.deallocate();
            // reallocate to new size
            state.dataPurchasedAirMgr->PurchAirPlenumArrays.allocate(state.dataPurchasedAirMgr->NumPlenumArrays);

            // allocate member arrays to same size as before
            for (Loop = 1; Loop < state.dataPurchasedAirMgr->NumPlenumArrays; ++Loop) {
                state.dataPurchasedAirMgr->PurchAirPlenumArrays(Loop).PurchAirArray.allocate(
                    state.dataPurchasedAirMgr->TempPurchAirPlenumArrays(Loop).NumPurchAir);
                state.dataPurchasedAirMgr->PurchAirPlenumArrays(Loop).IsSimulated.allocate(
                    state.dataPurchasedAirMgr->TempPurchAirPlenumArrays(Loop).NumPurchAir);
            }

            // save the data
            state.dataPurchasedAirMgr->PurchAirPlenumArrays = state.dataPurchasedAirMgr->TempPurchAirPlenumArrays;
            // delete temporary data
            state.dataPurchasedAirMgr->TempPurchAirPlenumArrays.deallocate();

            // save the index to where this ideal loads air system data is stored
            state.dataPurchasedAirMgr->PurchAir(PurchAirNum).PurchAirArrayIndex = 1;
            // save the number of ideal loads air systems stored in these arrays
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).NumPurchAir = 1;
            // save the index the the ZoneHVAC:ReturnPlenum
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).ReturnPlenumIndex = ReturnPlenumIndex;
            // allocate member array and store data
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).PurchAirArray.allocate(1);
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).PurchAirArray(1) = PurchAirNum;
            // allocate member array and store data
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).IsSimulated.allocate(1);
            state.dataPurchasedAirMgr->PurchAirPlenumArrays(state.dataPurchasedAirMgr->NumPlenumArrays).IsSimulated(1) = false;
        }
    }
}

} // namespace EnergyPlus::PurchasedAirManager
