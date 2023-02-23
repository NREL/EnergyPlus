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
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterManager.hh>
#include <EnergyPlus/WaterUse.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>

namespace EnergyPlus {

namespace WaterUse {

    // MODULE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   August 2006
    //       MODIFIED       Brent Griffith, plant upgrade

    void SimulateWaterUse(EnergyPlusData &state, bool FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED       Brent Griffith, March 2010, separated plant connected to different sim routine

        // PURPOSE OF THIS SUBROUTINE:
        // This routine is called from non zone equipment manager and serves to call
        // water use and connections that are not connected to a full plant loop

        int constexpr MaxIterations(100);
        Real64 constexpr Tolerance(0.1); // Make input?

        if (state.dataWaterUse->getWaterUseInputFlag) {
            GetWaterUseInput(state);
            state.dataWaterUse->getWaterUseInputFlag = false;
        }

        if (state.dataGlobal->BeginEnvrnFlag && state.dataWaterUse->MyEnvrnFlagLocal) {
            if (state.dataWaterUse->numWaterEquipment > 0) {
                for (auto &e : state.dataWaterUse->WaterEquipment) {
                    e.SensibleRate = 0.0;
                    e.SensibleEnergy = 0.0;
                    e.LatentRate = 0.0;
                    e.LatentEnergy = 0.0;
                    e.MixedTemp = 0.0;
                    e.TotalMassFlowRate = 0.0;
                    e.DrainTemp = 0.0;
                }
            }

            if (state.dataWaterUse->numWaterConnections > 0) {
                for (auto &e : state.dataWaterUse->WaterConnections)
                    e.TotalMassFlowRate = 0.0;
            }

            state.dataWaterUse->MyEnvrnFlagLocal = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) state.dataWaterUse->MyEnvrnFlagLocal = true;

        // Simulate all unconnected WATER USE EQUIPMENT objects
        for (auto &waterEquipment : state.dataWaterUse->WaterEquipment) {
            if (waterEquipment.Connections == 0) {
                waterEquipment.CalcEquipmentFlowRates(state);
                waterEquipment.CalcEquipmentDrainTemp(state);
            }
        } // WaterEquipNum

        ReportStandAloneWaterUse(state);

        // Simulate WATER USE CONNECTIONS objects and connected WATER USE EQUIPMENT objects
        for (auto &waterConnection : state.dataWaterUse->WaterConnections) {

            if (!waterConnection.StandAlone) continue; // only model non plant connections here

            waterConnection.InitConnections(state);

            int NumIteration = 0;

            while (true) {
                ++NumIteration;

                waterConnection.CalcConnectionsFlowRates(state, FirstHVACIteration);
                waterConnection.CalcConnectionsDrainTemp(state);
                waterConnection.CalcConnectionsHeatRecovery(state);

                if (waterConnection.TempError < Tolerance) {
                    break;
                } else if (NumIteration > MaxIterations) {
                    if (!state.dataGlobal->WarmupFlag) {
                        if (waterConnection.MaxIterationsErrorIndex == 0) {
                            ShowWarningError(state,
                                             format("WaterUse:Connections = {}:  Heat recovery temperature did not converge", waterConnection.Name));
                            ShowContinueErrorTimeStamp(state, "");
                        }
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "WaterUse:Connections = " + waterConnection.Name +
                                                           ":  Heat recovery temperature did not converge",
                                                       waterConnection.MaxIterationsErrorIndex);
                    }
                    break;
                }

            } // WHILE

            waterConnection.UpdateWaterConnections(state);
            waterConnection.ReportWaterUse(state);

        } // WaterConnNum
    }

    PlantComponent *WaterConnectionsType::factory(EnergyPlusData &state, std::string const &objectName)
    {
        // Process the input data
        if (state.dataWaterUse->getWaterUseInputFlag) {
            GetWaterUseInput(state);
            state.dataWaterUse->getWaterUseInputFlag = false;
        }

        // Now look for this particular object in the list
        for (auto &thisWC : state.dataWaterUse->WaterConnections) {
            if (thisWC.Name == objectName) {
                return &thisWC;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError(state, format("LocalWaterUseConnectionFactory: Error getting inputs for object named: {}", objectName)); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void WaterConnectionsType::simulate(EnergyPlusData &state,
                                        [[maybe_unused]] const PlantLocation &calledFromLocation,
                                        bool FirstHVACIteration,
                                        [[maybe_unused]] Real64 &CurLoad,
                                        [[maybe_unused]] bool RunFlag)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Brent Griffith March 2010, Demand Side Update
        //       DATE WRITTEN   August 2006

        // PURPOSE OF THIS SUBROUTINE:
        // Plant sim call for plant loop connected water use and connections

        int constexpr MaxIterations(100);
        Real64 constexpr Tolerance(0.1); // Make input?

        if (state.dataGlobal->BeginEnvrnFlag && this->MyEnvrnFlag) {
            if (state.dataWaterUse->numWaterEquipment > 0) {
                for (auto &waterEquipment : state.dataWaterUse->WaterEquipment) {
                    waterEquipment.reset();
                    if (waterEquipment.setupMyOutputVars) {
                        waterEquipment.setupOutputVars(state);
                        waterEquipment.setupMyOutputVars = false;
                    }
                }
            }

            if (state.dataWaterUse->numWaterConnections > 0) {
                for (auto &waterConnections : state.dataWaterUse->WaterConnections)
                    waterConnections.TotalMassFlowRate = 0.0;
            }

            this->MyEnvrnFlag = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) this->MyEnvrnFlag = true;

        this->InitConnections(state);

        int NumIteration = 0;

        while (true) {
            ++NumIteration;

            this->CalcConnectionsFlowRates(state, FirstHVACIteration);
            this->CalcConnectionsDrainTemp(state);
            this->CalcConnectionsHeatRecovery(state);

            if (this->TempError < Tolerance) {
                break;
            } else if (NumIteration > MaxIterations) {
                if (!state.dataGlobal->WarmupFlag) {
                    if (this->MaxIterationsErrorIndex == 0) {
                        ShowWarningError(state, format("WaterUse:Connections = {}:  Heat recovery temperature did not converge", this->Name));
                        ShowContinueErrorTimeStamp(state, "");
                    }
                    ShowRecurringWarningErrorAtEnd(state,
                                                   "WaterUse:Connections = " + this->Name + ":  Heat recovery temperature did not converge",
                                                   this->MaxIterationsErrorIndex);
                }
                break;
            }
        } // WHILE

        this->UpdateWaterConnections(state);
        this->ReportWaterUse(state);
    }

    void GetWaterUseInput(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006

        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int IOStatus;            // Used in GetObjectItem
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int AlphaNum;

        constexpr std::array<std::string_view, static_cast<int>(HeatRecovHX::Num)> HeatRecoverHXNamesUC{"IDEAL", "COUNTERFLOW", "CROSSFLOW"};

        constexpr std::array<std::string_view, static_cast<int>(HeatRecovConfig::Num)> HeatRecoveryConfigNamesUC{
            "PLANT", "EQUIPMENT", "PLANTANDEQUIPMENT"};

        state.dataIPShortCut->cCurrentModuleObject = "WaterUse:Equipment";
        state.dataWaterUse->numWaterEquipment =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataWaterUse->numWaterEquipment > 0) {
            state.dataWaterUse->WaterEquipment.allocate(state.dataWaterUse->numWaterEquipment);

            for (int WaterEquipNum = 1; WaterEquipNum <= state.dataWaterUse->numWaterEquipment; ++WaterEquipNum) {
                auto &thisWEq = state.dataWaterUse->WaterEquipment(WaterEquipNum);
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         state.dataIPShortCut->cCurrentModuleObject,
                                                                         WaterEquipNum,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         _,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);
                thisWEq.Name = state.dataIPShortCut->cAlphaArgs(1);

                thisWEq.EndUseSubcatName = state.dataIPShortCut->cAlphaArgs(2);

                thisWEq.PeakVolFlowRate = state.dataIPShortCut->rNumericArgs(1);

                if ((NumAlphas > 2) && (!state.dataIPShortCut->lAlphaFieldBlanks(3))) {
                    thisWEq.FlowRateFracSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
                    // If no FlowRateFracSchedule, fraction defaults to 1.0

                    if (thisWEq.FlowRateFracSchedule == 0) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(3), state.dataIPShortCut->cAlphaArgs(3)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, thisWEq.Name));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 3) && (!state.dataIPShortCut->lAlphaFieldBlanks(4))) {
                    thisWEq.TargetTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));

                    if (thisWEq.TargetTempSchedule == 0) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(4), state.dataIPShortCut->cAlphaArgs(4)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, thisWEq.Name));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 4) && (!state.dataIPShortCut->lAlphaFieldBlanks(5))) {
                    thisWEq.HotTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));
                    // If no HotTempSchedule, there is no hot water.
                    // HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

                    if (thisWEq.HotTempSchedule == 0) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(5), state.dataIPShortCut->cAlphaArgs(5)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, thisWEq.Name));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 5) && (!state.dataIPShortCut->lAlphaFieldBlanks(6))) {
                    thisWEq.ColdTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
                    // If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

                    if (thisWEq.ColdTempSchedule == 0) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(6), state.dataIPShortCut->cAlphaArgs(6)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, thisWEq.Name));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 6) && (!state.dataIPShortCut->lAlphaFieldBlanks(7))) {
                    thisWEq.Zone = UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(7), state.dataHeatBal->Zone);

                    if (thisWEq.Zone == 0) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(7), state.dataIPShortCut->cAlphaArgs(7)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, thisWEq.Name));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 7) && (!state.dataIPShortCut->lAlphaFieldBlanks(8))) {
                    thisWEq.SensibleFracSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(8));

                    if (thisWEq.SensibleFracSchedule == 0) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(8), state.dataIPShortCut->cAlphaArgs(8)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, thisWEq.Name));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 8) && (!state.dataIPShortCut->lAlphaFieldBlanks(9))) {
                    thisWEq.LatentFracSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(9));

                    if (thisWEq.LatentFracSchedule == 0) {
                        ShowSevereError(state,
                                        format("Invalid {}={}", state.dataIPShortCut->cAlphaFieldNames(9), state.dataIPShortCut->cAlphaArgs(9)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, thisWEq.Name));
                        ErrorsFound = true;
                    }
                }

            } // WaterEquipNum

            if (ErrorsFound) ShowFatalError(state, format("Errors found in processing input for {}", state.dataIPShortCut->cCurrentModuleObject));
        }

        state.dataIPShortCut->cCurrentModuleObject = "WaterUse:Connections";
        state.dataWaterUse->numWaterConnections =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataWaterUse->numWaterConnections > 0) {
            state.dataWaterUse->WaterConnections.allocate(state.dataWaterUse->numWaterConnections);

            for (int WaterConnNum = 1; WaterConnNum <= state.dataWaterUse->numWaterConnections; ++WaterConnNum) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         state.dataIPShortCut->cCurrentModuleObject,
                                                                         WaterConnNum,
                                                                         state.dataIPShortCut->cAlphaArgs,
                                                                         NumAlphas,
                                                                         state.dataIPShortCut->rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         _,
                                                                         state.dataIPShortCut->lAlphaFieldBlanks,
                                                                         state.dataIPShortCut->cAlphaFieldNames,
                                                                         state.dataIPShortCut->cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), state.dataIPShortCut->cCurrentModuleObject, ErrorsFound);
                auto &waterConnection = state.dataWaterUse->WaterConnections(WaterConnNum);
                waterConnection.Name = state.dataIPShortCut->cAlphaArgs(1);

                if ((!state.dataIPShortCut->lAlphaFieldBlanks(2)) || (!state.dataIPShortCut->lAlphaFieldBlanks(3))) {
                    waterConnection.InletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                    state.dataIPShortCut->cAlphaArgs(2),
                                                                                    ErrorsFound,
                                                                                    DataLoopNode::ConnectionObjectType::WaterUseConnections,
                                                                                    waterConnection.Name,
                                                                                    DataLoopNode::NodeFluidType::Water,
                                                                                    DataLoopNode::ConnectionType::Inlet,
                                                                                    NodeInputManager::CompFluidStream::Primary,
                                                                                    DataLoopNode::ObjectIsNotParent);
                    waterConnection.OutletNode = NodeInputManager::GetOnlySingleNode(state,
                                                                                     state.dataIPShortCut->cAlphaArgs(3),
                                                                                     ErrorsFound,
                                                                                     DataLoopNode::ConnectionObjectType::WaterUseConnections,
                                                                                     waterConnection.Name,
                                                                                     DataLoopNode::NodeFluidType::Water,
                                                                                     DataLoopNode::ConnectionType::Outlet,
                                                                                     NodeInputManager::CompFluidStream::Primary,
                                                                                     DataLoopNode::ObjectIsNotParent);

                    // Check plant connections
                    BranchNodeConnections::TestCompSet(state,
                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                       waterConnection.Name,
                                                       state.dataIPShortCut->cAlphaArgs(2),
                                                       state.dataIPShortCut->cAlphaArgs(3),
                                                       "DHW Nodes");
                } else {
                    // If no plant nodes are connected, simulate in stand-alone mode.
                    waterConnection.StandAlone = true;
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    WaterManager::SetupTankDemandComponent(state,
                                                           waterConnection.Name,
                                                           state.dataIPShortCut->cCurrentModuleObject,
                                                           state.dataIPShortCut->cAlphaArgs(4),
                                                           ErrorsFound,
                                                           waterConnection.SupplyTankNum,
                                                           waterConnection.TankDemandID);
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    WaterManager::SetupTankSupplyComponent(state,
                                                           waterConnection.Name,
                                                           state.dataIPShortCut->cCurrentModuleObject,
                                                           state.dataIPShortCut->cAlphaArgs(5),
                                                           ErrorsFound,
                                                           waterConnection.RecoveryTankNum,
                                                           waterConnection.TankSupplyID);
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    waterConnection.HotTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
                    // If no HotTempSchedule, there is no hot water.
                    // HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

                    if (waterConnection.HotTempSchedule == 0) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(6), state.dataIPShortCut->cAlphaArgs(6)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, waterConnection.Name));
                        ErrorsFound = true;
                    }
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    waterConnection.ColdTempSchedule = ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(7));
                    // If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

                    if (waterConnection.ColdTempSchedule == 0) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(7), state.dataIPShortCut->cAlphaArgs(7)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, waterConnection.Name));
                        ErrorsFound = true;
                    }
                }

                if ((!state.dataIPShortCut->lAlphaFieldBlanks(8)) && (state.dataIPShortCut->cAlphaArgs(8) != "NONE")) {
                    waterConnection.HeatRecovery = true;
                    waterConnection.HeatRecoveryHX = static_cast<HeatRecovHX>(
                        getEnumerationValue(HeatRecoverHXNamesUC, UtilityRoutines::MakeUPPERCase(state.dataIPShortCut->cAlphaArgs(8))));
                    if (waterConnection.HeatRecoveryHX == HeatRecovHX::Invalid) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(8), state.dataIPShortCut->cAlphaArgs(8)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, waterConnection.Name));
                        ErrorsFound = true;
                    }

                    waterConnection.HeatRecoveryConfig = static_cast<HeatRecovConfig>(
                        getEnumerationValue(HeatRecoveryConfigNamesUC, UtilityRoutines::MakeUPPERCase(state.dataIPShortCut->cAlphaArgs(9))));
                    if (waterConnection.HeatRecoveryConfig == HeatRecovConfig::Invalid) {
                        ShowSevereError(state,
                                        format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(9), state.dataIPShortCut->cAlphaArgs(9)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, waterConnection.Name));
                        ErrorsFound = true;
                    }
                }

                waterConnection.HXUA = state.dataIPShortCut->rNumericArgs(1);

                waterConnection.myWaterEquipArr.allocate(NumAlphas - 9);

                for (AlphaNum = 10; AlphaNum <= NumAlphas; ++AlphaNum) {
                    int WaterEquipNum =
                        UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(AlphaNum), state.dataWaterUse->WaterEquipment);

                    if (WaterEquipNum == 0) {
                        ShowSevereError(
                            state,
                            format("Invalid {} = {}", state.dataIPShortCut->cAlphaFieldNames(AlphaNum), state.dataIPShortCut->cAlphaArgs(AlphaNum)));
                        ShowContinueError(state, format("Entered in {} = {}", state.dataIPShortCut->cCurrentModuleObject, waterConnection.Name));
                        ErrorsFound = true;
                    } else {
                        if (state.dataWaterUse->WaterEquipment(WaterEquipNum).Connections > 0) {
                            ShowSevereError(state,
                                            format("{} = {}:  WaterUse:Equipment = {} is already referenced by another object.",
                                                   state.dataIPShortCut->cCurrentModuleObject,
                                                   waterConnection.Name,
                                                   state.dataIPShortCut->cAlphaArgs(AlphaNum)));
                            ErrorsFound = true;
                        } else {
                            state.dataWaterUse->WaterEquipment(WaterEquipNum).Connections = WaterConnNum;

                            ++waterConnection.NumWaterEquipment;
                            waterConnection.myWaterEquipArr(waterConnection.NumWaterEquipment) = WaterEquipNum;

                            waterConnection.PeakVolFlowRate +=
                                state.dataWaterUse->WaterEquipment(WaterEquipNum).PeakVolFlowRate; // this does not include possible multipliers
                        }
                    }
                }

            } // WaterConnNum

            if (ErrorsFound) ShowFatalError(state, format("Errors found in processing input for {}", state.dataIPShortCut->cCurrentModuleObject));

            if (state.dataWaterUse->numWaterConnections > 0) {
                state.dataWaterUse->CheckEquipName.allocate(state.dataWaterUse->numWaterConnections);
                state.dataWaterUse->CheckEquipName = true;
            }
        }

        // determine connection's peak mass flow rates.
        if (state.dataWaterUse->numWaterConnections > 0) {
            for (int WaterConnNum = 1; WaterConnNum <= state.dataWaterUse->numWaterConnections; ++WaterConnNum) {
                auto &waterConnection = state.dataWaterUse->WaterConnections(WaterConnNum);
                waterConnection.PeakMassFlowRate = 0.0;
                for (int WaterEquipNum = 1; WaterEquipNum <= waterConnection.NumWaterEquipment; ++WaterEquipNum) {
                    auto &thisWEq = state.dataWaterUse->WaterEquipment(waterConnection.myWaterEquipArr(WaterEquipNum));
                    if (thisWEq.Zone > 0) {
                        waterConnection.PeakMassFlowRate += thisWEq.PeakVolFlowRate * Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp) *
                                                            state.dataHeatBal->Zone(thisWEq.Zone).Multiplier *
                                                            state.dataHeatBal->Zone(thisWEq.Zone).ListMultiplier;
                    } else { // can't have multipliers
                        waterConnection.PeakMassFlowRate += thisWEq.PeakVolFlowRate * Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
                    }
                }
                PlantUtilities::RegisterPlantCompDesignFlow(
                    state, waterConnection.InletNode, waterConnection.PeakMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp));
            }
        }
        // need a good place to set a bool to calculate WaterUse hot and cold flow rates in CalcEquipmentFlowRates
        // WaterUse can be used with or without WaterUse:Connections, with or without WaterUse:Equipment hot temp schedule
        for (auto &waterEquipment : state.dataWaterUse->WaterEquipment) {
            // set logical if either hot water temp or target temp schedule are missing (will use cold water otherwise)
            // if a connections object is used then don't need to hot temp schedule
            waterEquipment.allowHotControl = (waterEquipment.TargetTempSchedule && waterEquipment.HotTempSchedule) || waterEquipment.Connections;
        }
    }

    void WaterEquipmentType::setupOutputVars(EnergyPlusData &state)
    {
        SetupOutputVariable(state,
                            "Water Use Equipment Hot Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->HotMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Cold Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->ColdMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Total Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->TotalMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Hot Water Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->HotVolFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Cold Water Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->ColdVolFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Total Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->TotalVolFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Hot Water Volume",
                            OutputProcessor::Unit::m3,
                            this->HotVolume,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Cold Water Volume",
                            OutputProcessor::Unit::m3,
                            this->ColdVolume,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Total Volume",
                            OutputProcessor::Unit::m3,
                            this->TotalVolume,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            "Water",
                            "WATERSYSTEMS",
                            this->EndUseSubcatName,
                            "Plant");
        SetupOutputVariable(state,
                            "Water Use Equipment Mains Water Volume",
                            OutputProcessor::Unit::m3,
                            this->TotalVolume,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name,
                            _,
                            "MainsWater",
                            "WATERSYSTEMS",
                            this->EndUseSubcatName,
                            "Plant");

        SetupOutputVariable(state,
                            "Water Use Equipment Hot Water Temperature",
                            OutputProcessor::Unit::C,
                            this->HotTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Cold Water Temperature",
                            OutputProcessor::Unit::C,
                            this->ColdTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Target Water Temperature",
                            OutputProcessor::Unit::C,
                            this->TargetTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Mixed Water Temperature",
                            OutputProcessor::Unit::C,
                            this->MixedTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Drain Water Temperature",
                            OutputProcessor::Unit::C,
                            this->DrainTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Heating Rate",
                            OutputProcessor::Unit::W,
                            this->Power,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        if (this->Connections == 0) {
            SetupOutputVariable(state,
                                "Water Use Equipment Heating Energy",
                                OutputProcessor::Unit::J,
                                this->Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                "DISTRICTHEATING",
                                "WATERSYSTEMS",
                                this->EndUseSubcatName,
                                "Plant");

        } else if (state.dataWaterUse->WaterConnections(this->Connections).StandAlone) {
            SetupOutputVariable(state,
                                "Water Use Equipment Heating Energy",
                                OutputProcessor::Unit::J,
                                this->Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                "DISTRICTHEATING",
                                "WATERSYSTEMS",
                                this->EndUseSubcatName,
                                "Plant");

        } else { // The EQUIPMENT is coupled to a plant loop via a CONNECTIONS object
            SetupOutputVariable(state,
                                "Water Use Equipment Heating Energy",
                                OutputProcessor::Unit::J,
                                this->Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                "ENERGYTRANSFER",
                                "WATERSYSTEMS",
                                this->EndUseSubcatName,
                                "Plant");
        }

        if (this->Zone > 0) {
            SetupOutputVariable(state,
                                "Water Use Equipment Zone Sensible Heat Gain Rate",
                                OutputProcessor::Unit::W,
                                this->SensibleRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Water Use Equipment Zone Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                this->SensibleEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name);

            SetupOutputVariable(state,
                                "Water Use Equipment Zone Latent Gain Rate",
                                OutputProcessor::Unit::W,
                                this->LatentRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Water Use Equipment Zone Latent Gain Energy",
                                OutputProcessor::Unit::J,
                                this->LatentEnergy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name);

            SetupOutputVariable(state,
                                "Water Use Equipment Zone Moisture Gain Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                this->MoistureRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                this->Name);
            SetupOutputVariable(state,
                                "Water Use Equipment Zone Moisture Gain Mass",
                                OutputProcessor::Unit::kg,
                                this->MoistureMass,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name);

            SetupZoneInternalGain(state,
                                  this->Zone,
                                  this->Name,
                                  DataHeatBalance::IntGainType::WaterUseEquipment,
                                  &this->SensibleRateNoMultiplier,
                                  nullptr,
                                  nullptr,
                                  &this->LatentRateNoMultiplier);
        }
    }

    void WaterConnectionsType::setupOutputVars(EnergyPlusData &state)
    {
        SetupOutputVariable(state,
                            "Water Use Connections Hot Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->HotMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Cold Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->ColdMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Total Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->TotalMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Drain Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->DrainMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Heat Recovery Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->RecoveryMassFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Hot Water Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->HotVolFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Cold Water Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->ColdVolFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Total Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->TotalVolFlowRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Hot Water Volume",
                            OutputProcessor::Unit::m3,
                            this->HotVolume,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Cold Water Volume",
                            OutputProcessor::Unit::m3,
                            this->ColdVolume,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Total Volume",
                            OutputProcessor::Unit::m3,
                            this->TotalVolume,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name); //, &
        // ResourceTypeKey='Water', EndUseKey='DHW', EndUseSubKey=EndUseSubcategoryName, GroupKey='Plant')
        // tHIS WAS double counting

        SetupOutputVariable(state,
                            "Water Use Connections Hot Water Temperature",
                            OutputProcessor::Unit::C,
                            this->HotTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Cold Water Temperature",
                            OutputProcessor::Unit::C,
                            this->ColdTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Drain Water Temperature",
                            OutputProcessor::Unit::C,
                            this->DrainTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Return Water Temperature",
                            OutputProcessor::Unit::C,
                            this->ReturnTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Waste Water Temperature",
                            OutputProcessor::Unit::C,
                            this->WasteTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Heat Recovery Water Temperature",
                            OutputProcessor::Unit::C,
                            this->RecoveryTemp,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Heat Recovery Effectiveness",
                            OutputProcessor::Unit::None,
                            this->Effectiveness,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Heat Recovery Rate",
                            OutputProcessor::Unit::W,
                            this->RecoveryRate,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Average,
                            this->Name);
        SetupOutputVariable(state,
                            "Water Use Connections Heat Recovery Energy",
                            OutputProcessor::Unit::J,
                            this->RecoveryEnergy,
                            OutputProcessor::SOVTimeStepType::System,
                            OutputProcessor::SOVStoreType::Summed,
                            this->Name);
        // Does this go on a meter?

        // To do:  Add report variable for starved flow when tank can't deliver?

        if (!this->StandAlone) {
            SetupOutputVariable(state,
                                "Water Use Connections Plant Hot Water Energy",
                                OutputProcessor::Unit::J,
                                this->Energy,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Summed,
                                this->Name,
                                _,
                                "PLANTLOOPHEATINGDEMAND",
                                "WATERSYSTEMS",
                                _,
                                "Plant");
        }
    }

    void WaterEquipmentType::CalcEquipmentFlowRates(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate desired hot and cold water flow rates

        Real64 TempDiff;
        Real64 constexpr EPSILON(1.e-3);

        if (this->setupMyOutputVars) {
            this->setupOutputVars(state);
            this->setupMyOutputVars = false;
        }

        if (this->Connections > 0) {
            // Get water temperature conditions from the CONNECTIONS object
            this->ColdTemp = state.dataWaterUse->WaterConnections(this->Connections).ColdTemp;
            this->HotTemp = state.dataWaterUse->WaterConnections(this->Connections).HotTemp;

        } else {
            // Get water temperature conditions from the WATER USE EQUIPMENT schedules
            if (this->ColdTempSchedule > 0) {
                this->ColdTemp = ScheduleManager::GetCurrentScheduleValue(state, this->ColdTempSchedule);
            } else { // If no ColdTempSchedule, use the mains temperature
                this->ColdTemp = state.dataEnvrn->WaterMainsTemp;
            }

            if (this->HotTempSchedule > 0) {
                this->HotTemp = ScheduleManager::GetCurrentScheduleValue(state, this->HotTempSchedule);
            } else { // If no HotTempSchedule, use all cold water
                this->HotTemp = this->ColdTemp;
            }
        }

        if (this->TargetTempSchedule > 0) {
            this->TargetTemp = ScheduleManager::GetCurrentScheduleValue(state, this->TargetTempSchedule);
        } else { // If no TargetTempSchedule, use all hot water
            this->TargetTemp = this->HotTemp;
        }

        // Get the requested total flow rate
        if (this->Zone > 0) {
            if (this->FlowRateFracSchedule > 0) {
                this->TotalVolFlowRate = this->PeakVolFlowRate * ScheduleManager::GetCurrentScheduleValue(state, this->FlowRateFracSchedule) *
                                         state.dataHeatBal->Zone(this->Zone).Multiplier * state.dataHeatBal->Zone(this->Zone).ListMultiplier;
            } else {
                this->TotalVolFlowRate =
                    this->PeakVolFlowRate * state.dataHeatBal->Zone(this->Zone).Multiplier * state.dataHeatBal->Zone(this->Zone).ListMultiplier;
            }
        } else {
            if (this->FlowRateFracSchedule > 0) {
                this->TotalVolFlowRate = this->PeakVolFlowRate * ScheduleManager::GetCurrentScheduleValue(state, this->FlowRateFracSchedule);
            } else {
                this->TotalVolFlowRate = this->PeakVolFlowRate;
            }
        }

        this->TotalMassFlowRate = this->TotalVolFlowRate * Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);

        // Calculate hot and cold water mixing at the tap
        if (this->TotalMassFlowRate > 0.0 && this->allowHotControl) {
            // Calculate the flow rates needed to meet the target temperature
            if (this->TargetTemp <= this->ColdTemp + EPSILON) {
                // don't need to mix (use cold) or hot water flow would be very small if within EPSILON above cold temp (use cold)
                this->HotMassFlowRate = 0.0;
                if (!state.dataGlobal->WarmupFlag && this->TargetTemp < this->ColdTemp) {
                    // print error for variables of target water temperature
                    ++this->TargetCWTempErrorCount;
                    TempDiff = this->ColdTemp - this->TargetTemp;
                    if (this->TargetCWTempErrorCount < 2) {
                        ShowWarningError(
                            state,
                            format("CalcEquipmentFlowRates: \"{}\" - Target water temperature is less than the cold water temperature by ({:.2R} C)",
                                   this->Name,
                                   TempDiff));
                        ShowContinueErrorTimeStamp(state, "");
                        ShowContinueError(state, format("...target water temperature     = {:.2R} C", this->TargetTemp));
                        ShowContinueError(state, format("...cold water temperature       = {:.2R} C", this->ColdTemp));
                        ShowContinueError(state,
                                          "...Target water temperature should be greater than or equal to the cold water temperature. "
                                          "Verify temperature setpoints and schedules.");
                    } else {
                        ShowRecurringWarningErrorAtEnd(
                            state,
                            format(
                                "\"{}\" - Target water temperature should be greater than or equal to the cold water temperature error continues...",
                                this->Name),
                            this->TargetCWTempErrIndex,
                            TempDiff,
                            TempDiff);
                    }
                }
            } else if (this->TargetTemp >= this->HotTemp) {
                // don't need to mix (use hot), or need to purge stagnant hot water temps (use hot)
                this->HotMassFlowRate = this->TotalMassFlowRate;
                if (!state.dataGlobal->WarmupFlag) {
                    // print error for variables of target water temperature
                    if (this->ColdTemp > (this->HotTemp + EPSILON)) {
                        // print error for variables of hot water temperature
                        ++this->CWHWTempErrorCount;
                        TempDiff = this->ColdTemp - this->HotTemp;
                        if (this->CWHWTempErrorCount < 2) {
                            ShowWarningError(
                                state,
                                format("CalcEquipmentFlowRates: \"{}\" - Hot water temperature is less than the cold water temperature by ({:.2R} C)",
                                       this->Name,
                                       TempDiff));
                            ShowContinueErrorTimeStamp(state, "");
                            ShowContinueError(state, format("...hot water temperature        = {:.2R} C", this->HotTemp));
                            ShowContinueError(state, format("...cold water temperature       = {:.2R} C", this->ColdTemp));
                            ShowContinueError(state,
                                              "...Hot water temperature should be greater than or equal to the cold water temperature. "
                                              "Verify temperature setpoints and schedules.");
                        } else {
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                format("\"{}\" - Hot water temperature should be greater than the cold water temperature error continues... ",
                                       this->Name),
                                this->CWHWTempErrIndex,
                                TempDiff,
                                TempDiff);
                        }
                    } else if (this->TargetTemp > this->HotTemp) {
                        TempDiff = this->TargetTemp - this->HotTemp;
                        ++this->TargetHWTempErrorCount;
                        if (this->TargetHWTempErrorCount < 2) {
                            ShowWarningError(state,
                                             format("CalcEquipmentFlowRates: \"{}\" - Target water temperature is greater than the hot water "
                                                    "temperature by ({:.2R} C)",
                                                    this->Name,
                                                    TempDiff));
                            ShowContinueErrorTimeStamp(state, "");
                            ShowContinueError(state, format("...target water temperature     = {:.2R} C", this->TargetTemp));
                            ShowContinueError(state, format("...hot water temperature        = {:.2R} C", this->HotTemp));
                            ShowContinueError(state,
                                              "...Target water temperature should be less than or equal to the hot water temperature. "
                                              "Verify temperature setpoints and schedules.");
                        } else {
                            ShowRecurringWarningErrorAtEnd(state,
                                                           format("\"{}\" - Target water temperature should be less than or equal to the hot "
                                                                  "water temperature error continues...",
                                                                  this->Name),
                                                           this->TargetHWTempErrIndex,
                                                           TempDiff,
                                                           TempDiff);
                        }
                    }
                }
            } else {
                // Check hot less than cold temp, else calculate hot flow.
                if (this->HotTemp <= this->ColdTemp + EPSILON) {
                    // will need to avoid divide by 0 and stagnant region. Target temp is greater than ColdTemp so use hot water
                    // continue using hot water until hot water temp is EPSILON C above cold water temp (i.e., avoid very small denominator)
                    this->HotMassFlowRate = this->TotalMassFlowRate;
                    if (!state.dataGlobal->WarmupFlag && this->HotTemp < this->ColdTemp) {
                        // print error for variables of hot water temperature
                        ++this->CWHWTempErrorCount;
                        TempDiff = this->ColdTemp - this->HotTemp;
                        if (this->CWHWTempErrorCount < 2) {
                            ShowWarningError(state,
                                             format("CalcEquipmentFlowRates: \"{}\" - Hot water temperature is less than the cold water "
                                                    "temperature by ({:.2R} C)",
                                                    this->Name,
                                                    TempDiff));
                            ShowContinueErrorTimeStamp(state, "");
                            ShowContinueError(state, format("...hot water temperature        = {:.2R} C", this->HotTemp));
                            ShowContinueError(state, format("...cold water temperature       = {:.2R} C", this->ColdTemp));
                            ShowContinueError(state,
                                              "...Hot water temperature should be greater than or equal to the cold water temperature. "
                                              "Verify temperature setpoints and schedules.");
                        } else {
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                format("\"{}\" - Hot water temperature should be greater than the cold water temperature error continues... ",
                                       this->Name),
                                this->CWHWTempErrIndex,
                                TempDiff,
                                TempDiff);
                        }
                    }
                } else {
                    // HotMassFlowRate should always be between 0 and TotalMassFlowRate
                    this->HotMassFlowRate = this->TotalMassFlowRate * (this->TargetTemp - this->ColdTemp) / (this->HotTemp - this->ColdTemp);
                }
            }

            this->ColdMassFlowRate = this->TotalMassFlowRate - this->HotMassFlowRate;
            this->MixedTemp = (this->ColdMassFlowRate * this->ColdTemp + this->HotMassFlowRate * this->HotTemp) / this->TotalMassFlowRate;
            // there should be no out of bounds results
            assert(this->ColdMassFlowRate >= 0.0 && this->ColdMassFlowRate <= this->TotalMassFlowRate);
            assert(this->HotMassFlowRate >= 0.0 && this->HotMassFlowRate <= this->TotalMassFlowRate);
            assert(std::abs(this->HotMassFlowRate + this->ColdMassFlowRate - this->TotalMassFlowRate) < EPSILON);
        } else {
            this->HotMassFlowRate = 0.0;
            this->ColdMassFlowRate = this->TotalMassFlowRate;
            this->MixedTemp = this->TargetTemp;
        }
    }

    void WaterEquipmentType::CalcEquipmentDrainTemp(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate drainwater temperature and heat and moisture gains to zone.

        static constexpr std::string_view RoutineName("CalcEquipmentDrainTemp");

        this->SensibleRate = 0.0;
        this->SensibleEnergy = 0.0;
        this->LatentRate = 0.0;
        this->LatentEnergy = 0.0;

        if ((this->Zone == 0) || (this->TotalMassFlowRate == 0.0)) {
            this->DrainTemp = this->MixedTemp;
            this->DrainMassFlowRate = this->TotalMassFlowRate;

        } else {
            auto &thisZoneHB = state.dataZoneTempPredictorCorrector->zoneHeatBalance(this->Zone);

            if (this->SensibleFracSchedule == 0) {
                this->SensibleRate = 0.0;
                this->SensibleEnergy = 0.0;
            } else {
                this->SensibleRate = ScheduleManager::GetCurrentScheduleValue(state, this->SensibleFracSchedule) * this->TotalMassFlowRate *
                                     Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * (this->MixedTemp - thisZoneHB.MAT);
                this->SensibleEnergy = this->SensibleRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            }

            if (this->LatentFracSchedule == 0) {
                this->LatentRate = 0.0;
                this->LatentEnergy = 0.0;
            } else {
                Real64 ZoneHumRat = thisZoneHB.ZoneAirHumRat;
                Real64 ZoneHumRatSat = Psychrometrics::PsyWFnTdbRhPb(state,
                                                                     thisZoneHB.MAT,
                                                                     1.0,
                                                                     state.dataEnvrn->OutBaroPress,
                                                                     RoutineName); // Humidratio at 100% relative humidity
                Real64 RhoAirDry = Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, thisZoneHB.MAT, 0.0);
                Real64 ZoneMassMax =
                    (ZoneHumRatSat - ZoneHumRat) * RhoAirDry * state.dataHeatBal->Zone(this->Zone).Volume; // Max water that can be evaporated to zone
                Real64 FlowMassMax =
                    this->TotalMassFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour; // Max water in flow
                Real64 MoistureMassMax = min(ZoneMassMax, FlowMassMax);

                this->MoistureMass = ScheduleManager::GetCurrentScheduleValue(state, this->LatentFracSchedule) * MoistureMassMax;
                this->MoistureRate = this->MoistureMass / (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);

                this->LatentRate = this->MoistureRate * Psychrometrics::PsyHfgAirFnWTdb(ZoneHumRat, thisZoneHB.MAT);
                this->LatentEnergy = this->LatentRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            }

            this->DrainMassFlowRate = this->TotalMassFlowRate - this->MoistureRate;

            if (this->DrainMassFlowRate == 0.0) {
                this->DrainTemp = this->MixedTemp;
            } else {
                this->DrainTemp = (this->TotalMassFlowRate * Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * this->MixedTemp -
                                   this->SensibleRate - this->LatentRate) /
                                  (this->DrainMassFlowRate * Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp));
            }
        }
    }

    void WaterConnectionsType::InitConnections(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED       Brent Griffith 2010, demand side update

        // Set the cold water temperature
        if (this->SupplyTankNum > 0) {
            this->ColdSupplyTemp = state.dataWaterData->WaterStorage(this->SupplyTankNum).Twater;

        } else if (this->ColdTempSchedule > 0) {
            this->ColdSupplyTemp = ScheduleManager::GetCurrentScheduleValue(state, this->ColdTempSchedule);

        } else {
            this->ColdSupplyTemp = state.dataEnvrn->WaterMainsTemp;
        }

        // Initially set ColdTemp to the ColdSupplyTemp; with heat recovery, ColdTemp will change during iteration
        this->ColdTemp = this->ColdSupplyTemp;

        // Set the hot water temperature
        if (this->StandAlone) {
            if (this->HotTempSchedule > 0) {
                this->HotTemp = ScheduleManager::GetCurrentScheduleValue(state, this->HotTempSchedule);
            } else {
                // If no HotTempSchedule, use all cold water
                this->HotTemp = this->ColdTemp;
            }

        } else {

            if (state.dataGlobal->BeginEnvrnFlag && this->Init) {
                // Clear node initial conditions
                if (this->InletNode > 0 && this->OutletNode > 0) {
                    PlantUtilities::InitComponentNodes(state, 0.0, this->PeakMassFlowRate, this->InletNode, this->OutletNode);

                    this->ReturnTemp = state.dataLoopNodes->Node(this->InletNode).Temp;
                }

                this->Init = false;
            }

            if (!state.dataGlobal->BeginEnvrnFlag) this->Init = true;

            if (this->InletNode > 0) {
                if (!state.dataGlobal->DoingSizing) {
                    this->HotTemp = state.dataLoopNodes->Node(this->InletNode).Temp;
                } else {
                    // plant loop will not be running so need a value here.
                    // should change to use tank setpoint but water use connections don't have knowledge of the tank they are fed by
                    this->HotTemp = 60.0;
                }
            }
        }
    }

    void WaterConnectionsType::CalcConnectionsFlowRates(EnergyPlusData &state, bool FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate summed values for WATER USE CONNECTIONS (to prepare to request flow from plant, and for reporting).

        this->ColdMassFlowRate = 0.0;
        this->HotMassFlowRate = 0.0;

        for (int Loop = 1; Loop <= this->NumWaterEquipment; ++Loop) {
            auto &thisWEq = state.dataWaterUse->WaterEquipment(this->myWaterEquipArr(Loop));

            thisWEq.CalcEquipmentFlowRates(state);

            this->ColdMassFlowRate += thisWEq.ColdMassFlowRate;
            this->HotMassFlowRate += thisWEq.HotMassFlowRate;
        } // Loop

        this->TotalMassFlowRate = this->ColdMassFlowRate + this->HotMassFlowRate;

        if (!this->StandAlone) { // Interact with the plant loop
            if (this->InletNode > 0) {
                if (FirstHVACIteration) {
                    // Request the mass flow rate from the demand side manager
                    PlantUtilities::SetComponentFlowRate(state, this->HotMassFlowRate, this->InletNode, this->OutletNode, this->plantLoc);

                } else {
                    Real64 DesiredHotWaterMassFlow = this->HotMassFlowRate;
                    PlantUtilities::SetComponentFlowRate(state, DesiredHotWaterMassFlow, this->InletNode, this->OutletNode, this->plantLoc);
                    // readjust if more than actual available mass flow rate determined by the demand side manager
                    if ((this->HotMassFlowRate != DesiredHotWaterMassFlow) && (this->HotMassFlowRate > 0.0)) { // plant didn't give what was asked for

                        Real64 AvailableFraction = DesiredHotWaterMassFlow / this->HotMassFlowRate;

                        this->ColdMassFlowRate = this->TotalMassFlowRate - this->HotMassFlowRate; // Preserve the total mass flow rate

                        // Proportionally reduce hot water and increase cold water for all WATER USE EQUIPMENT
                        for (int Loop = 1; Loop <= this->NumWaterEquipment; ++Loop) {
                            auto &thisWEq = state.dataWaterUse->WaterEquipment(this->myWaterEquipArr(Loop));

                            // Recalculate flow rates for water equipment within connection
                            thisWEq.HotMassFlowRate *= AvailableFraction;
                            thisWEq.ColdMassFlowRate = thisWEq.TotalMassFlowRate - thisWEq.HotMassFlowRate;

                            // Recalculate mixed water temperature
                            if (thisWEq.TotalMassFlowRate > 0.0) {
                                thisWEq.MixedTemp = (thisWEq.ColdMassFlowRate * thisWEq.ColdTemp + thisWEq.HotMassFlowRate * thisWEq.HotTemp) /
                                                    thisWEq.TotalMassFlowRate;
                            } else {
                                thisWEq.MixedTemp = thisWEq.TargetTemp;
                            }
                        } // Loop
                    }
                }
            }
        }

        if (this->SupplyTankNum > 0) {
            // Set the demand request for supply water from water storage tank
            this->ColdVolFlowRate = this->ColdMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
            state.dataWaterData->WaterStorage(this->SupplyTankNum).VdotRequestDemand(this->TankDemandID) = this->ColdVolFlowRate;

            // Check if cold flow rate should be starved by restricted flow from tank
            // Currently, the tank flow is not really starved--water continues to flow at the tank water temperature
            // But the user can see the error by comparing report variables for TankVolFlowRate < ColdVolFlowRate
            this->TankVolFlowRate = state.dataWaterData->WaterStorage(this->SupplyTankNum).VdotAvailDemand(this->TankDemandID);
            this->TankMassFlowRate = this->TankVolFlowRate * Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
        }
    }

    void WaterConnectionsType::CalcConnectionsDrainTemp(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006

        Real64 MassFlowTempSum = 0.0;
        this->DrainMassFlowRate = 0.0;

        for (int Loop = 1; Loop <= this->NumWaterEquipment; ++Loop) {
            auto &thisWEq = state.dataWaterUse->WaterEquipment(this->myWaterEquipArr(Loop));

            thisWEq.CalcEquipmentDrainTemp(state);

            this->DrainMassFlowRate += thisWEq.DrainMassFlowRate;
            MassFlowTempSum += thisWEq.DrainMassFlowRate * thisWEq.DrainTemp;
        } // Loop

        if (this->DrainMassFlowRate > 0.0) {
            this->DrainTemp = MassFlowTempSum / this->DrainMassFlowRate;
        } else {
            this->DrainTemp = this->HotTemp;
        }

        this->DrainVolFlowRate = this->DrainMassFlowRate * Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
    }

    void WaterConnectionsType::CalcConnectionsHeatRecovery(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate drainwater heat recovery

        if (!this->HeatRecovery) {
            this->RecoveryTemp = this->ColdSupplyTemp;
            this->ReturnTemp = this->ColdSupplyTemp;
            this->WasteTemp = this->DrainTemp;

        } else if (this->TotalMassFlowRate == 0.0) {
            this->Effectiveness = 0.0;
            this->RecoveryRate = 0.0;
            this->RecoveryTemp = this->ColdSupplyTemp;
            this->ReturnTemp = this->ColdSupplyTemp;
            this->WasteTemp = this->DrainTemp;

        } else { // WaterConnections(WaterConnNum)%TotalMassFlowRate > 0.0

            switch (this->HeatRecoveryConfig) {
            case HeatRecovConfig::Plant: {
                this->RecoveryMassFlowRate = this->HotMassFlowRate;
            } break;
            case HeatRecovConfig::Equipment: {
                this->RecoveryMassFlowRate = this->ColdMassFlowRate;
            } break;
            case HeatRecovConfig::PlantAndEquip: {
                this->RecoveryMassFlowRate = this->TotalMassFlowRate;
            } break;
            default:
                break;
            }

            Real64 HXCapacityRate = Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * this->RecoveryMassFlowRate;
            Real64 DrainCapacityRate = Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * this->DrainMassFlowRate;
            Real64 MinCapacityRate = min(DrainCapacityRate, HXCapacityRate);

            switch (this->HeatRecoveryHX) {
            case HeatRecovHX::Ideal: {
                this->Effectiveness = 1.0;
            } break;
            case HeatRecovHX::CounterFlow: { // Unmixed
                Real64 CapacityRatio = MinCapacityRate / max(DrainCapacityRate, HXCapacityRate);
                Real64 NTU = this->HXUA / MinCapacityRate;
                if (CapacityRatio == 1.0) {
                    this->Effectiveness = NTU / (1.0 + NTU);
                } else {
                    Real64 ExpVal = std::exp(-NTU * (1.0 - CapacityRatio));
                    this->Effectiveness = (1.0 - ExpVal) / (1.0 - CapacityRatio * ExpVal);
                }
            } break;
            case HeatRecovHX::CrossFlow: { // Unmixed
                Real64 CapacityRatio = MinCapacityRate / max(DrainCapacityRate, HXCapacityRate);
                Real64 NTU = this->HXUA / MinCapacityRate;
                this->Effectiveness = 1.0 - std::exp((std::pow(NTU, 0.22) / CapacityRatio) * (std::exp(-CapacityRatio * std::pow(NTU, 0.78)) - 1.0));
            } break;
            default:
                break;
            }

            this->RecoveryRate = this->Effectiveness * MinCapacityRate * (this->DrainTemp - this->ColdSupplyTemp);
            this->RecoveryTemp =
                this->ColdSupplyTemp + this->RecoveryRate / (Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * this->TotalMassFlowRate);
            this->WasteTemp =
                this->DrainTemp - this->RecoveryRate / (Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * this->TotalMassFlowRate);

            if (this->RecoveryTankNum > 0) {
                state.dataWaterData->WaterStorage(this->RecoveryTankNum).VdotAvailSupply(this->TankSupplyID) = this->DrainVolFlowRate;
                state.dataWaterData->WaterStorage(this->RecoveryTankNum).TwaterSupply(this->TankSupplyID) = this->WasteTemp;
            }

            switch (this->HeatRecoveryConfig) {
            case HeatRecovConfig::Plant: {
                this->TempError = 0.0; // No feedback back to the cold supply
                this->ReturnTemp = this->RecoveryTemp;
            } break;
            case HeatRecovConfig::Equipment: {
                this->TempError = std::abs(this->ColdTemp - this->RecoveryTemp);

                this->ColdTemp = this->RecoveryTemp;
                this->ReturnTemp = this->ColdSupplyTemp;
            } break;
            case HeatRecovConfig::PlantAndEquip: {
                this->TempError = std::abs(this->ColdTemp - this->RecoveryTemp);

                this->ColdTemp = this->RecoveryTemp;
                this->ReturnTemp = this->RecoveryTemp;
            } break;
            default:
                break;
            }
        }
    }

    void WaterConnectionsType::UpdateWaterConnections(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006

        // PURPOSE OF THIS SUBROUTINE:
        // Updates the node variables with local variables.

        if (this->InletNode > 0 && this->OutletNode > 0) {
            // Pass all variables from inlet to outlet node
            PlantUtilities::SafeCopyPlantNode(state, this->InletNode, this->OutletNode, this->plantLoc.loopNum);

            // Set outlet node variables that are possibly changed
            state.dataLoopNodes->Node(this->OutletNode).Temp = this->ReturnTemp;
            // should add enthalpy update to return?
        }
    }

    void ReportStandAloneWaterUse(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith, Peter Graham Ellis
        //       DATE WRITTEN   Nov. 2011
        //       MODIFIED       Brent Griffith, March 2010 added argument

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates report variables for stand alone water use

        for (int WaterEquipNum = 1; WaterEquipNum <= state.dataWaterUse->numWaterEquipment; ++WaterEquipNum) {
            auto &thisWEq = state.dataWaterUse->WaterEquipment(WaterEquipNum);
            thisWEq.ColdVolFlowRate = thisWEq.ColdMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
            thisWEq.HotVolFlowRate = thisWEq.HotMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
            thisWEq.TotalVolFlowRate = thisWEq.ColdVolFlowRate + thisWEq.HotVolFlowRate;

            thisWEq.ColdVolume = thisWEq.ColdVolFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            thisWEq.HotVolume = thisWEq.HotVolFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            thisWEq.TotalVolume = thisWEq.TotalVolFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

            if (thisWEq.Connections == 0) {
                thisWEq.Power =
                    thisWEq.HotMassFlowRate * Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * (thisWEq.HotTemp - thisWEq.ColdTemp);
            } else {
                thisWEq.Power = thisWEq.HotMassFlowRate * Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) *
                                (thisWEq.HotTemp - state.dataWaterUse->WaterConnections(thisWEq.Connections).ReturnTemp);
            }

            thisWEq.Energy = thisWEq.Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        }
    }

    void WaterConnectionsType::ReportWaterUse(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED       Brent Griffith, March 2010 added argument

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates report variables.

        for (int Loop = 1; Loop <= this->NumWaterEquipment; ++Loop) {

            auto &thisWEq = state.dataWaterUse->WaterEquipment(this->myWaterEquipArr(Loop));

            thisWEq.ColdVolFlowRate = thisWEq.ColdMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
            thisWEq.HotVolFlowRate = thisWEq.HotMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
            thisWEq.TotalVolFlowRate = thisWEq.ColdVolFlowRate + thisWEq.HotVolFlowRate;
            thisWEq.ColdVolume = thisWEq.ColdVolFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            thisWEq.HotVolume = thisWEq.HotVolFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            thisWEq.TotalVolume = thisWEq.TotalVolFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

            if (thisWEq.Connections == 0) {
                thisWEq.Power =
                    thisWEq.HotMassFlowRate * Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * (thisWEq.HotTemp - thisWEq.ColdTemp);
            } else {
                thisWEq.Power = thisWEq.HotMassFlowRate * Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) *
                                (thisWEq.HotTemp - state.dataWaterUse->WaterConnections(thisWEq.Connections).ReturnTemp);
            }

            thisWEq.Energy = thisWEq.Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        }

        this->ColdVolFlowRate = this->ColdMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
        this->HotVolFlowRate = this->HotMassFlowRate / Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
        this->TotalVolFlowRate = this->ColdVolFlowRate + this->HotVolFlowRate;
        this->ColdVolume = this->ColdVolFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->HotVolume = this->HotVolFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->TotalVolume = this->TotalVolFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->Power = this->HotMassFlowRate * Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * (this->HotTemp - this->ReturnTemp);
        this->Energy = this->Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
        this->RecoveryEnergy = this->RecoveryRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
    }
    void WaterConnectionsType::oneTimeInit_new(EnergyPlusData &state)
    {

        this->setupOutputVars(state);

        if (allocated(state.dataPlnt->PlantLoop) && !this->StandAlone) {
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(
                state, this->Name, DataPlant::PlantEquipmentType::WaterUseConnection, this->plantLoc, errFlag, _, _, _, _, _);
            if (errFlag) {
                ShowFatalError(state, "InitConnections: Program terminated due to previous condition(s).");
            }
        }
    }
    void WaterConnectionsType::oneTimeInit([[maybe_unused]] EnergyPlusData &state)
    {
    }

    void CalcWaterUseZoneGains(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the zone internal gains due to water use sensible and latent loads.

        bool MyEnvrnFlagLocal(true);

        if (state.dataWaterUse->numWaterEquipment == 0) return;

        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlagLocal) {
            for (auto &e : state.dataWaterUse->WaterEquipment) {
                e.SensibleRate = 0.0;
                e.SensibleEnergy = 0.0;
                e.SensibleRateNoMultiplier = 0.0;
                e.LatentRate = 0.0;
                e.LatentEnergy = 0.0;
                e.LatentRateNoMultiplier = 0.0;
                e.MixedTemp = 0.0;
                e.TotalMassFlowRate = 0.0;
                e.DrainTemp = 0.0;
                e.ColdVolFlowRate = 0.0;
                e.HotVolFlowRate = 0.0;
                e.TotalVolFlowRate = 0.0;
                e.ColdMassFlowRate = 0.0;
                e.HotMassFlowRate = 0.0;
            }
            MyEnvrnFlagLocal = false;
        }

        if (!state.dataGlobal->BeginEnvrnFlag) MyEnvrnFlagLocal = true;

        for (int WaterEquipNum = 1; WaterEquipNum <= state.dataWaterUse->numWaterEquipment; ++WaterEquipNum) {
            if (state.dataWaterUse->WaterEquipment(WaterEquipNum).Zone == 0) continue;
            int ZoneNum = state.dataWaterUse->WaterEquipment(WaterEquipNum).Zone;
            state.dataWaterUse->WaterEquipment(WaterEquipNum).SensibleRateNoMultiplier =
                state.dataWaterUse->WaterEquipment(WaterEquipNum).SensibleRate /
                (state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier);
            state.dataWaterUse->WaterEquipment(WaterEquipNum).LatentRateNoMultiplier =
                state.dataWaterUse->WaterEquipment(WaterEquipNum).LatentRate /
                (state.dataHeatBal->Zone(ZoneNum).Multiplier * state.dataHeatBal->Zone(ZoneNum).ListMultiplier);
        }
    }
} // namespace WaterUse
} // namespace EnergyPlus
