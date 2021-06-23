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
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
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

namespace EnergyPlus {

namespace WaterUse {

    // MODULE INFORMATION:
    //       AUTHOR         Peter Graham Ellis
    //       DATE WRITTEN   August 2006
    //       MODIFIED       Brent Griffith, plant upgrade
    //       RE-ENGINEERED  na

    void SimulateWaterUse(EnergyPlusData &state, bool FirstHVACIteration)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED       Brent Griffith, March 2010, separated plant connected to different sim routine
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine is called from non zone equipment manager and serves to call
        // water use and connections that are not connected to a full plant loop

        int const MaxIterations(100);
        Real64 const Tolerance(0.1); // Make input?

        int WaterEquipNum;
        int WaterConnNum;
        int NumIteration;

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
        for (WaterEquipNum = 1; WaterEquipNum <= state.dataWaterUse->numWaterEquipment; ++WaterEquipNum) {
            if (state.dataWaterUse->WaterEquipment(WaterEquipNum).Connections == 0) {
                state.dataWaterUse->WaterEquipment(WaterEquipNum).CalcEquipmentFlowRates(state);
                state.dataWaterUse->WaterEquipment(WaterEquipNum).CalcEquipmentDrainTemp(state);
            }
        } // WaterEquipNum

        ReportStandAloneWaterUse(state);

        // Simulate WATER USE CONNECTIONS objects and connected WATER USE EQUIPMENT objects
        for (WaterConnNum = 1; WaterConnNum <= state.dataWaterUse->numWaterConnections; ++WaterConnNum) {

            if (!state.dataWaterUse->WaterConnections(WaterConnNum).StandAlone) continue; // only model non plant connections here

            state.dataWaterUse->WaterConnections(WaterConnNum).InitConnections(state);

            NumIteration = 0;

            while (true) {
                ++NumIteration;

                state.dataWaterUse->WaterConnections(WaterConnNum).CalcConnectionsFlowRates(state, FirstHVACIteration);
                state.dataWaterUse->WaterConnections(WaterConnNum).CalcConnectionsDrainTemp(state);
                state.dataWaterUse->WaterConnections(WaterConnNum).CalcConnectionsHeatRecovery(state);

                if (state.dataWaterUse->WaterConnections(WaterConnNum).TempError < Tolerance) {
                    break;
                } else if (NumIteration > MaxIterations) {
                    if (!state.dataGlobal->WarmupFlag) {
                        if (state.dataWaterUse->WaterConnections(WaterConnNum).MaxIterationsErrorIndex == 0) {
                            ShowWarningError(state,
                                             "WaterUse:Connections = " + state.dataWaterUse->WaterConnections(WaterConnNum).Name +
                                                 ":  Heat recovery temperature did not converge");
                            ShowContinueErrorTimeStamp(state, "");
                        }
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "WaterUse:Connections = " + state.dataWaterUse->WaterConnections(WaterConnNum).Name +
                                                           ":  Heat recovery temperature did not converge",
                                                       state.dataWaterUse->WaterConnections(WaterConnNum).MaxIterationsErrorIndex);
                    }
                    break;
                }

            } // WHILE

            state.dataWaterUse->WaterConnections(WaterConnNum).UpdateWaterConnections(state);
            state.dataWaterUse->WaterConnections(WaterConnNum).ReportWaterUse(state);

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
        ShowFatalError(state, "LocalWaterUseConnectionFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
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
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Plant sim call for plant loop connected water use and connections

        int const MaxIterations(100);
        Real64 const Tolerance(0.1); // Make input?

        if (state.dataGlobal->BeginEnvrnFlag && this->MyEnvrnFlag) {
            if (state.dataWaterUse->numWaterEquipment > 0) {
                for (int i = state.dataWaterUse->WaterEquipment.l(), e = state.dataWaterUse->WaterEquipment.u(); i <= e; ++i) {
                    state.dataWaterUse->WaterEquipment(i).reset();

                    if (state.dataWaterUse->WaterEquipment(i).setupMyOutputVars) {
                        state.dataWaterUse->WaterEquipment(i).setupOutputVars(state);
                        state.dataWaterUse->WaterEquipment(i).setupMyOutputVars = false;
                    }
                }
            }

            if (state.dataWaterUse->numWaterConnections > 0) {
                for (auto &e : state.dataWaterUse->WaterConnections)
                    e.TotalMassFlowRate = 0.0;
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
                        ShowWarningError(state, "WaterUse:Connections = " + this->Name + ":  Heat recovery temperature did not converge");
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
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int IOStatus;            // Used in GetObjectItem
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int AlphaNum;

        state.dataIPShortCut->cCurrentModuleObject = "WaterUse:Equipment";
        state.dataWaterUse->numWaterEquipment =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, state.dataIPShortCut->cCurrentModuleObject);

        if (state.dataWaterUse->numWaterEquipment > 0) {
            state.dataWaterUse->WaterEquipment.allocate(state.dataWaterUse->numWaterEquipment);

            for (int WaterEquipNum = 1; WaterEquipNum <= state.dataWaterUse->numWaterEquipment; ++WaterEquipNum) {
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
                state.dataWaterUse->WaterEquipment(WaterEquipNum).Name = state.dataIPShortCut->cAlphaArgs(1);

                state.dataWaterUse->WaterEquipment(WaterEquipNum).EndUseSubcatName = state.dataIPShortCut->cAlphaArgs(2);

                state.dataWaterUse->WaterEquipment(WaterEquipNum).PeakVolFlowRate = state.dataIPShortCut->rNumericArgs(1);

                if ((NumAlphas > 2) && (!state.dataIPShortCut->lAlphaFieldBlanks(3))) {
                    state.dataWaterUse->WaterEquipment(WaterEquipNum).FlowRateFracSchedule =
                        ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(3));
                    // If no FlowRateFracSchedule, fraction defaults to 1.0

                    if (state.dataWaterUse->WaterEquipment(WaterEquipNum).FlowRateFracSchedule == 0) {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(3) + '=' + state.dataIPShortCut->cAlphaArgs(3));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 3) && (!state.dataIPShortCut->lAlphaFieldBlanks(4))) {
                    state.dataWaterUse->WaterEquipment(WaterEquipNum).TargetTempSchedule =
                        ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));

                    if (state.dataWaterUse->WaterEquipment(WaterEquipNum).TargetTempSchedule == 0) {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(4) + '=' + state.dataIPShortCut->cAlphaArgs(4));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 4) && (!state.dataIPShortCut->lAlphaFieldBlanks(5))) {
                    state.dataWaterUse->WaterEquipment(WaterEquipNum).HotTempSchedule =
                        ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(5));
                    // If no HotTempSchedule, there is no hot water.
                    // HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

                    if (state.dataWaterUse->WaterEquipment(WaterEquipNum).HotTempSchedule == 0) {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(5) + '=' + state.dataIPShortCut->cAlphaArgs(5));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 5) && (!state.dataIPShortCut->lAlphaFieldBlanks(6))) {
                    state.dataWaterUse->WaterEquipment(WaterEquipNum).ColdTempSchedule =
                        ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
                    // If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

                    if (state.dataWaterUse->WaterEquipment(WaterEquipNum).ColdTempSchedule == 0) {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + state.dataIPShortCut->cAlphaArgs(6));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 6) && (!state.dataIPShortCut->lAlphaFieldBlanks(7))) {
                    state.dataWaterUse->WaterEquipment(WaterEquipNum).Zone =
                        UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(7), state.dataHeatBal->Zone);

                    if (state.dataWaterUse->WaterEquipment(WaterEquipNum).Zone == 0) {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 7) && (!state.dataIPShortCut->lAlphaFieldBlanks(8))) {
                    state.dataWaterUse->WaterEquipment(WaterEquipNum).SensibleFracSchedule =
                        ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(8));

                    if (state.dataWaterUse->WaterEquipment(WaterEquipNum).SensibleFracSchedule == 0) {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + state.dataIPShortCut->cAlphaArgs(8));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 8) && (!state.dataIPShortCut->lAlphaFieldBlanks(9))) {
                    state.dataWaterUse->WaterEquipment(WaterEquipNum).LatentFracSchedule =
                        ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(9));

                    if (state.dataWaterUse->WaterEquipment(WaterEquipNum).LatentFracSchedule == 0) {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + state.dataIPShortCut->cAlphaArgs(9));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

            } // WaterEquipNum

            if (ErrorsFound) ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);
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
                state.dataWaterUse->WaterConnections(WaterConnNum).Name = state.dataIPShortCut->cAlphaArgs(1);

                if ((!state.dataIPShortCut->lAlphaFieldBlanks(2)) || (!state.dataIPShortCut->lAlphaFieldBlanks(3))) {
                    state.dataWaterUse->WaterConnections(WaterConnNum).InletNode =
                        NodeInputManager::GetOnlySingleNode(state,
                                                            state.dataIPShortCut->cAlphaArgs(2),
                                                            ErrorsFound,
                                                            state.dataIPShortCut->cCurrentModuleObject,
                                                            state.dataIPShortCut->cAlphaArgs(1),
                                                            DataLoopNode::NodeFluidType::Water,
                                                            DataLoopNode::NodeConnectionType::Inlet,
                                                            NodeInputManager::compFluidStream::Primary,
                                                            DataLoopNode::ObjectIsNotParent);
                    state.dataWaterUse->WaterConnections(WaterConnNum).OutletNode =
                        NodeInputManager::GetOnlySingleNode(state,
                                                            state.dataIPShortCut->cAlphaArgs(3),
                                                            ErrorsFound,
                                                            state.dataIPShortCut->cCurrentModuleObject,
                                                            state.dataIPShortCut->cAlphaArgs(1),
                                                            DataLoopNode::NodeFluidType::Water,
                                                            DataLoopNode::NodeConnectionType::Outlet,
                                                            NodeInputManager::compFluidStream::Primary,
                                                            DataLoopNode::ObjectIsNotParent);

                    // Check plant connections
                    BranchNodeConnections::TestCompSet(state,
                                                       state.dataIPShortCut->cCurrentModuleObject,
                                                       state.dataIPShortCut->cAlphaArgs(1),
                                                       state.dataIPShortCut->cAlphaArgs(2),
                                                       state.dataIPShortCut->cAlphaArgs(3),
                                                       "DHW Nodes");
                } else {
                    // If no plant nodes are connected, simulate in stand-alone mode.
                    state.dataWaterUse->WaterConnections(WaterConnNum).StandAlone = true;
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(4)) {
                    WaterManager::SetupTankDemandComponent(state,
                                                           state.dataWaterUse->WaterConnections(WaterConnNum).Name,
                                                           state.dataIPShortCut->cCurrentModuleObject,
                                                           state.dataIPShortCut->cAlphaArgs(4),
                                                           ErrorsFound,
                                                           state.dataWaterUse->WaterConnections(WaterConnNum).SupplyTankNum,
                                                           state.dataWaterUse->WaterConnections(WaterConnNum).TankDemandID);
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(5)) {
                    WaterManager::SetupTankSupplyComponent(state,
                                                           state.dataWaterUse->WaterConnections(WaterConnNum).Name,
                                                           state.dataIPShortCut->cCurrentModuleObject,
                                                           state.dataIPShortCut->cAlphaArgs(5),
                                                           ErrorsFound,
                                                           state.dataWaterUse->WaterConnections(WaterConnNum).RecoveryTankNum,
                                                           state.dataWaterUse->WaterConnections(WaterConnNum).TankSupplyID);
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(6)) {
                    state.dataWaterUse->WaterConnections(WaterConnNum).HotTempSchedule =
                        ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
                    // If no HotTempSchedule, there is no hot water.
                    // HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

                    if (state.dataWaterUse->WaterConnections(WaterConnNum).HotTempSchedule == 0) {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(6) + '=' + state.dataIPShortCut->cAlphaArgs(6));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                    state.dataWaterUse->WaterConnections(WaterConnNum).ColdTempSchedule =
                        ScheduleManager::GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(7));
                    // If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

                    if (state.dataWaterUse->WaterConnections(WaterConnNum).ColdTempSchedule == 0) {
                        ShowSevereError(state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(7) + '=' + state.dataIPShortCut->cAlphaArgs(7));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((!state.dataIPShortCut->lAlphaFieldBlanks(8)) && (state.dataIPShortCut->cAlphaArgs(8) != "NONE")) {
                    state.dataWaterUse->WaterConnections(WaterConnNum).HeatRecovery = true;

                    {
                        auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(8));
                        if (SELECT_CASE_var == "IDEAL") {
                            state.dataWaterUse->WaterConnections(WaterConnNum).HeatRecoveryHX = HeatRecoveryHXEnum::Ideal;
                        } else if (SELECT_CASE_var == "COUNTERFLOW") {
                            state.dataWaterUse->WaterConnections(WaterConnNum).HeatRecoveryHX = HeatRecoveryHXEnum::CounterFlow;
                        } else if (SELECT_CASE_var == "CROSSFLOW") {
                            state.dataWaterUse->WaterConnections(WaterConnNum).HeatRecoveryHX = HeatRecoveryHXEnum::CrossFlow;
                        } else {
                            ShowSevereError(state,
                                            "Invalid " + state.dataIPShortCut->cAlphaFieldNames(8) + '=' + state.dataIPShortCut->cAlphaArgs(8));
                            ShowContinueError(state,
                                              "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                    }

                    {
                        auto const SELECT_CASE_var(state.dataIPShortCut->cAlphaArgs(9));
                        if (SELECT_CASE_var == "PLANT") {
                            state.dataWaterUse->WaterConnections(WaterConnNum).HeatRecoveryConfig = HeatRecoveryConfigEnum::Plant;
                        } else if (SELECT_CASE_var == "EQUIPMENT") {
                            state.dataWaterUse->WaterConnections(WaterConnNum).HeatRecoveryConfig = HeatRecoveryConfigEnum::Equipment;
                        } else if (SELECT_CASE_var == "PLANTANDEQUIPMENT") {
                            state.dataWaterUse->WaterConnections(WaterConnNum).HeatRecoveryConfig = HeatRecoveryConfigEnum::PlantAndEquip;
                        } else {
                            ShowSevereError(state,
                                            "Invalid " + state.dataIPShortCut->cAlphaFieldNames(9) + '=' + state.dataIPShortCut->cAlphaArgs(9));
                            ShowContinueError(state,
                                              "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                    }
                }

                state.dataWaterUse->WaterConnections(WaterConnNum).HXUA = state.dataIPShortCut->rNumericArgs(1);

                state.dataWaterUse->WaterConnections(WaterConnNum).myWaterEquipArr.allocate(NumAlphas - 9);

                for (AlphaNum = 10; AlphaNum <= NumAlphas; ++AlphaNum) {
                    int WaterEquipNum =
                        UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(AlphaNum), state.dataWaterUse->WaterEquipment);

                    if (WaterEquipNum == 0) {
                        ShowSevereError(
                            state, "Invalid " + state.dataIPShortCut->cAlphaFieldNames(AlphaNum) + '=' + state.dataIPShortCut->cAlphaArgs(AlphaNum));
                        ShowContinueError(state,
                                          "Entered in " + state.dataIPShortCut->cCurrentModuleObject + '=' + state.dataIPShortCut->cAlphaArgs(1));
                        ErrorsFound = true;
                    } else {
                        if (state.dataWaterUse->WaterEquipment(WaterEquipNum).Connections > 0) {
                            ShowSevereError(state,
                                            state.dataIPShortCut->cCurrentModuleObject + " = " + state.dataIPShortCut->cAlphaArgs(1) +
                                                ":  WaterUse:Equipment = " + state.dataIPShortCut->cAlphaArgs(AlphaNum) +
                                                " is already referenced by another object.");
                            ErrorsFound = true;
                        } else {
                            state.dataWaterUse->WaterEquipment(WaterEquipNum).Connections = WaterConnNum;

                            ++state.dataWaterUse->WaterConnections(WaterConnNum).NumWaterEquipment;
                            state.dataWaterUse->WaterConnections(WaterConnNum)
                                .myWaterEquipArr(state.dataWaterUse->WaterConnections(WaterConnNum).NumWaterEquipment) = WaterEquipNum;

                            state.dataWaterUse->WaterConnections(WaterConnNum).PeakVolFlowRate +=
                                state.dataWaterUse->WaterEquipment(WaterEquipNum).PeakVolFlowRate; // this does not include possible multipliers
                        }
                    }
                }

            } // WaterConnNum

            if (ErrorsFound) ShowFatalError(state, "Errors found in processing input for " + state.dataIPShortCut->cCurrentModuleObject);

            if (state.dataWaterUse->numWaterConnections > 0) {
                state.dataWaterUse->CheckEquipName.allocate(state.dataWaterUse->numWaterConnections);
                state.dataWaterUse->CheckEquipName = true;
            }
        }

        // determine connection's peak mass flow rates.
        if (state.dataWaterUse->numWaterConnections > 0) {
            for (int WaterConnNum = 1; WaterConnNum <= state.dataWaterUse->numWaterConnections; ++WaterConnNum) {
                state.dataWaterUse->WaterConnections(WaterConnNum).PeakMassFlowRate = 0.0;
                for (int WaterEquipNum = 1; WaterEquipNum <= state.dataWaterUse->WaterConnections(WaterConnNum).NumWaterEquipment; ++WaterEquipNum) {
                    int thisWaterEquipNum = state.dataWaterUse->WaterConnections(WaterConnNum).myWaterEquipArr(WaterEquipNum);
                    if (state.dataWaterUse->WaterEquipment(thisWaterEquipNum).Zone > 0) {
                        state.dataWaterUse->WaterConnections(WaterConnNum).PeakMassFlowRate +=
                            state.dataWaterUse->WaterEquipment(thisWaterEquipNum).PeakVolFlowRate *
                            Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp) *
                            state.dataHeatBal->Zone(state.dataWaterUse->WaterEquipment(thisWaterEquipNum).Zone).Multiplier *
                            state.dataHeatBal->Zone(state.dataWaterUse->WaterEquipment(thisWaterEquipNum).Zone).ListMultiplier;
                    } else { // can't have multipliers
                        state.dataWaterUse->WaterConnections(WaterConnNum).PeakMassFlowRate +=
                            state.dataWaterUse->WaterEquipment(thisWaterEquipNum).PeakVolFlowRate *
                            Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp);
                    }
                }
                PlantUtilities::RegisterPlantCompDesignFlow(state,
                                                            state.dataWaterUse->WaterConnections(WaterConnNum).InletNode,
                                                            state.dataWaterUse->WaterConnections(WaterConnNum).PeakMassFlowRate /
                                                                Psychrometrics::RhoH2O(DataGlobalConstants::InitConvTemp));
            }
        }
    }

    void WaterEquipmentType::setupOutputVars(EnergyPlusData &state)
    {
        SetupOutputVariable(state,
                            "Water Use Equipment Hot Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->HotMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Cold Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->ColdMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(
            state, "Water Use Equipment Total Mass Flow Rate", OutputProcessor::Unit::kg_s, this->TotalMassFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Hot Water Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->HotVolFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Cold Water Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->ColdVolFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Total Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->TotalVolFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state, "Water Use Equipment Hot Water Volume", OutputProcessor::Unit::m3, this->HotVolume, "System", "Sum", this->Name);

        SetupOutputVariable(state, "Water Use Equipment Cold Water Volume", OutputProcessor::Unit::m3, this->ColdVolume, "System", "Sum", this->Name);

        SetupOutputVariable(state,
                            "Water Use Equipment Total Volume",
                            OutputProcessor::Unit::m3,
                            this->TotalVolume,
                            "System",
                            "Sum",
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
                            "System",
                            "Sum",
                            this->Name,
                            _,
                            "MainsWater",
                            "WATERSYSTEMS",
                            this->EndUseSubcatName,
                            "Plant");

        SetupOutputVariable(
            state, "Water Use Equipment Hot Water Temperature", OutputProcessor::Unit::C, this->HotTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Water Use Equipment Cold Water Temperature", OutputProcessor::Unit::C, this->ColdTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Water Use Equipment Target Water Temperature", OutputProcessor::Unit::C, this->TargetTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Water Use Equipment Mixed Water Temperature", OutputProcessor::Unit::C, this->MixedTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Water Use Equipment Drain Water Temperature", OutputProcessor::Unit::C, this->DrainTemp, "System", "Average", this->Name);

        SetupOutputVariable(state, "Water Use Equipment Heating Rate", OutputProcessor::Unit::W, this->Power, "System", "Average", this->Name);

        if (this->Connections == 0) {
            SetupOutputVariable(state,
                                "Water Use Equipment Heating Energy",
                                OutputProcessor::Unit::J,
                                this->Energy,
                                "System",
                                "Sum",
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
                                "System",
                                "Sum",
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
                                "System",
                                "Sum",
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
                                "System",
                                "Average",
                                this->Name);
            SetupOutputVariable(state,
                                "Water Use Equipment Zone Sensible Heat Gain Energy",
                                OutputProcessor::Unit::J,
                                this->SensibleEnergy,
                                "System",
                                "Sum",
                                this->Name);

            SetupOutputVariable(
                state, "Water Use Equipment Zone Latent Gain Rate", OutputProcessor::Unit::W, this->LatentRate, "System", "Average", this->Name);
            SetupOutputVariable(
                state, "Water Use Equipment Zone Latent Gain Energy", OutputProcessor::Unit::J, this->LatentEnergy, "System", "Sum", this->Name);

            SetupOutputVariable(state,
                                "Water Use Equipment Zone Moisture Gain Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                this->MoistureRate,
                                "System",
                                "Average",
                                this->Name);
            SetupOutputVariable(
                state, "Water Use Equipment Zone Moisture Gain Mass", OutputProcessor::Unit::kg, this->MoistureMass, "System", "Sum", this->Name);

            SetupZoneInternalGain(state,
                                  this->Zone,
                                  "WaterUse:Equipment",
                                  this->Name,
                                  DataHeatBalance::IntGainTypeOf_WaterUseEquipment,
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
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Cold Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->ColdMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Total Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->TotalMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Drain Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->DrainMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Heat Recovery Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->RecoveryMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Hot Water Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->HotVolFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Cold Water Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->ColdVolFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Total Volume Flow Rate",
                            OutputProcessor::Unit::m3_s,
                            this->TotalVolFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state, "Water Use Connections Hot Water Volume", OutputProcessor::Unit::m3, this->HotVolume, "System", "Sum", this->Name);

        SetupOutputVariable(
            state, "Water Use Connections Cold Water Volume", OutputProcessor::Unit::m3, this->ColdVolume, "System", "Sum", this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Total Volume",
                            OutputProcessor::Unit::m3,
                            this->TotalVolume,
                            "System",
                            "Sum",
                            this->Name); //, &
        // ResourceTypeKey='Water', EndUseKey='DHW', EndUseSubKey=EndUseSubcategoryName, GroupKey='Plant')
        // tHIS WAS double counting

        SetupOutputVariable(
            state, "Water Use Connections Hot Water Temperature", OutputProcessor::Unit::C, this->HotTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Water Use Connections Cold Water Temperature", OutputProcessor::Unit::C, this->ColdTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Water Use Connections Drain Water Temperature", OutputProcessor::Unit::C, this->DrainTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Water Use Connections Return Water Temperature", OutputProcessor::Unit::C, this->ReturnTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            state, "Water Use Connections Waste Water Temperature", OutputProcessor::Unit::C, this->WasteTemp, "System", "Average", this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Heat Recovery Water Temperature",
                            OutputProcessor::Unit::C,
                            this->RecoveryTemp,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(state,
                            "Water Use Connections Heat Recovery Effectiveness",
                            OutputProcessor::Unit::None,
                            this->Effectiveness,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(
            state, "Water Use Connections Heat Recovery Rate", OutputProcessor::Unit::W, this->RecoveryRate, "System", "Average", this->Name);
        SetupOutputVariable(
            state, "Water Use Connections Heat Recovery Energy", OutputProcessor::Unit::J, this->RecoveryEnergy, "System", "Sum", this->Name);
        // Does this go on a meter?

        // To do:  Add report variable for starved flow when tank can't deliver?

        if (!this->StandAlone) {
            SetupOutputVariable(state,
                                "Water Use Connections Plant Hot Water Energy",
                                OutputProcessor::Unit::J,
                                this->Energy,
                                "System",
                                "Sum",
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
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate desired hot and cold water flow rates

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
        if (this->TotalMassFlowRate > 0.0) {
            // Calculate the flow rates needed to meet the target temperature
            if (this->HotTemp == this->ColdTemp) { // Avoid divide by zero
                // There is no hot water
                this->HotMassFlowRate = 0.0;

                // Need a special case for HotTemp < ColdTemp, due to bad user input  (but could happen in a plant loop accidentally)

            } else if (this->TargetTemp > this->HotTemp) {
                this->HotMassFlowRate = this->TotalMassFlowRate;

            } else {
                this->HotMassFlowRate = this->TotalMassFlowRate * (this->TargetTemp - this->ColdTemp) / (this->HotTemp - this->ColdTemp);
            }

            if (this->HotMassFlowRate < 0.0) {
                // Target temp is colder than the cold water temp; don't allow colder
                this->HotMassFlowRate = 0.0;
            }

            this->ColdMassFlowRate = this->TotalMassFlowRate - this->HotMassFlowRate;

            if (this->ColdMassFlowRate < 0.0) this->ColdMassFlowRate = 0.0;

            this->MixedTemp = (this->ColdMassFlowRate * this->ColdTemp + this->HotMassFlowRate * this->HotTemp) / this->TotalMassFlowRate;
        } else {
            this->HotMassFlowRate = 0.0;
            this->ColdMassFlowRate = 0.0;
            this->MixedTemp = this->TargetTemp;
        }
    }

    void WaterEquipmentType::CalcEquipmentDrainTemp(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate drainwater temperature and heat and moisture gains to zone.

        static std::string const RoutineName("CalcEquipmentDrainTemp");

        this->SensibleRate = 0.0;
        this->SensibleEnergy = 0.0;
        this->LatentRate = 0.0;
        this->LatentEnergy = 0.0;

        if ((this->Zone == 0) || (this->TotalMassFlowRate == 0.0)) {
            this->DrainTemp = this->MixedTemp;
            this->DrainMassFlowRate = this->TotalMassFlowRate;

        } else {

            if (this->SensibleFracSchedule == 0) {
                this->SensibleRate = 0.0;
                this->SensibleEnergy = 0.0;
            } else {
                this->SensibleRate = ScheduleManager::GetCurrentScheduleValue(state, this->SensibleFracSchedule) * this->TotalMassFlowRate *
                                     Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) *
                                     (this->MixedTemp - state.dataHeatBalFanSys->MAT(this->Zone));
                this->SensibleEnergy = this->SensibleRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;
            }

            if (this->LatentFracSchedule == 0) {
                this->LatentRate = 0.0;
                this->LatentEnergy = 0.0;
            } else {
                Real64 ZoneHumRat = state.dataHeatBalFanSys->ZoneAirHumRat(this->Zone);
                Real64 ZoneHumRatSat = Psychrometrics::PsyWFnTdbRhPb(state,
                                                                     state.dataHeatBalFanSys->MAT(this->Zone),
                                                                     1.0,
                                                                     state.dataEnvrn->OutBaroPress,
                                                                     RoutineName); // Humidratio at 100% relative humidity
                Real64 RhoAirDry =
                    Psychrometrics::PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, state.dataHeatBalFanSys->MAT(this->Zone), 0.0);
                Real64 ZoneMassMax =
                    (ZoneHumRatSat - ZoneHumRat) * RhoAirDry * state.dataHeatBal->Zone(this->Zone).Volume; // Max water that can be evaporated to zone
                Real64 FlowMassMax =
                    this->TotalMassFlowRate * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour; // Max water in flow
                Real64 MoistureMassMax = min(ZoneMassMax, FlowMassMax);

                this->MoistureMass = ScheduleManager::GetCurrentScheduleValue(state, this->LatentFracSchedule) * MoistureMassMax;
                this->MoistureRate = this->MoistureMass / (state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour);

                this->LatentRate = this->MoistureRate * Psychrometrics::PsyHfgAirFnWTdb(ZoneHumRat, state.dataHeatBalFanSys->MAT(this->Zone));
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
        //       RE-ENGINEERED  na

        if (this->setupMyOutputVars) {
            this->setupOutputVars(state);
            this->setupMyOutputVars = false;
        }

        if (this->plantScanFlag && allocated(state.dataPlnt->PlantLoop) && !this->StandAlone) {
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(state,
                                                    this->Name,
                                                    DataPlant::TypeOf_WaterUseConnection,
                                                    this->PlantLoopNum,
                                                    this->PlantLoopSide,
                                                    this->PlantLoopBranchNum,
                                                    this->PlantLoopCompNum,
                                                    errFlag,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _);
            if (errFlag) {
                ShowFatalError(state, "InitConnections: Program terminated due to previous condition(s).");
            }
            this->plantScanFlag = false;
        }

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
                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       this->PeakMassFlowRate,
                                                       this->InletNode,
                                                       this->OutletNode,
                                                       this->PlantLoopNum,
                                                       this->PlantLoopSide,
                                                       this->PlantLoopBranchNum,
                                                       this->PlantLoopCompNum);

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
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate summed values for WATER USE CONNECTIONS (to prepare to request flow from plant, and for reporting).

        this->ColdMassFlowRate = 0.0;
        this->HotMassFlowRate = 0.0;

        for (int Loop = 1; Loop <= this->NumWaterEquipment; ++Loop) {
            int WaterEquipNum = this->myWaterEquipArr(Loop);

            state.dataWaterUse->WaterEquipment(WaterEquipNum).CalcEquipmentFlowRates(state);

            this->ColdMassFlowRate += state.dataWaterUse->WaterEquipment(WaterEquipNum).ColdMassFlowRate;
            this->HotMassFlowRate += state.dataWaterUse->WaterEquipment(WaterEquipNum).HotMassFlowRate;
        } // Loop

        this->TotalMassFlowRate = this->ColdMassFlowRate + this->HotMassFlowRate;

        if (!this->StandAlone) { // Interact with the plant loop
            if (this->InletNode > 0) {
                if (FirstHVACIteration) {
                    // Request the mass flow rate from the demand side manager
                    PlantUtilities::SetComponentFlowRate(state,
                                                         this->HotMassFlowRate,
                                                         this->InletNode,
                                                         this->OutletNode,
                                                         this->PlantLoopNum,
                                                         this->PlantLoopSide,
                                                         this->PlantLoopBranchNum,
                                                         this->PlantLoopCompNum);

                } else {
                    Real64 DesiredHotWaterMassFlow = this->HotMassFlowRate;
                    PlantUtilities::SetComponentFlowRate(state,
                                                         DesiredHotWaterMassFlow,
                                                         this->InletNode,
                                                         this->OutletNode,
                                                         this->PlantLoopNum,
                                                         this->PlantLoopSide,
                                                         this->PlantLoopBranchNum,
                                                         this->PlantLoopCompNum);
                    // readjust if more than actual available mass flow rate determined by the demand side manager
                    if ((this->HotMassFlowRate != DesiredHotWaterMassFlow) && (this->HotMassFlowRate > 0.0)) { // plant didn't give what was asked for

                        Real64 AvailableFraction = DesiredHotWaterMassFlow / this->HotMassFlowRate;

                        this->ColdMassFlowRate = this->TotalMassFlowRate - this->HotMassFlowRate; // Preserve the total mass flow rate

                        // Proportionally reduce hot water and increase cold water for all WATER USE EQUIPMENT
                        for (int Loop = 1; Loop <= this->NumWaterEquipment; ++Loop) {
                            int WaterEquipNum = this->myWaterEquipArr(Loop);

                            // Recalculate flow rates for water equipment within connection
                            state.dataWaterUse->WaterEquipment(WaterEquipNum).HotMassFlowRate *= AvailableFraction;
                            state.dataWaterUse->WaterEquipment(WaterEquipNum).ColdMassFlowRate =
                                state.dataWaterUse->WaterEquipment(WaterEquipNum).TotalMassFlowRate -
                                state.dataWaterUse->WaterEquipment(WaterEquipNum).HotMassFlowRate;

                            // Recalculate mixed water temperature
                            if (state.dataWaterUse->WaterEquipment(WaterEquipNum).TotalMassFlowRate > 0.0) {
                                state.dataWaterUse->WaterEquipment(WaterEquipNum).MixedTemp =
                                    (state.dataWaterUse->WaterEquipment(WaterEquipNum).ColdMassFlowRate *
                                         state.dataWaterUse->WaterEquipment(WaterEquipNum).ColdTemp +
                                     state.dataWaterUse->WaterEquipment(WaterEquipNum).HotMassFlowRate *
                                         state.dataWaterUse->WaterEquipment(WaterEquipNum).HotTemp) /
                                    state.dataWaterUse->WaterEquipment(WaterEquipNum).TotalMassFlowRate;
                            } else {
                                state.dataWaterUse->WaterEquipment(WaterEquipNum).MixedTemp =
                                    state.dataWaterUse->WaterEquipment(WaterEquipNum).TargetTemp;
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
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        Real64 MassFlowTempSum = 0.0;
        this->DrainMassFlowRate = 0.0;

        for (int Loop = 1; Loop <= this->NumWaterEquipment; ++Loop) {
            int WaterEquipNum = this->myWaterEquipArr(Loop);

            state.dataWaterUse->WaterEquipment(WaterEquipNum).CalcEquipmentDrainTemp(state);

            this->DrainMassFlowRate += state.dataWaterUse->WaterEquipment(WaterEquipNum).DrainMassFlowRate;
            MassFlowTempSum +=
                state.dataWaterUse->WaterEquipment(WaterEquipNum).DrainMassFlowRate * state.dataWaterUse->WaterEquipment(WaterEquipNum).DrainTemp;
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
        //       MODIFIED       na
        //       RE-ENGINEERED  na

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

            {
                auto const SELECT_CASE_var(this->HeatRecoveryConfig);
                if (SELECT_CASE_var == HeatRecoveryConfigEnum::Plant) {
                    this->RecoveryMassFlowRate = this->HotMassFlowRate;
                } else if (SELECT_CASE_var == HeatRecoveryConfigEnum::Equipment) {
                    this->RecoveryMassFlowRate = this->ColdMassFlowRate;
                } else if (SELECT_CASE_var == HeatRecoveryConfigEnum::PlantAndEquip) {
                    this->RecoveryMassFlowRate = this->TotalMassFlowRate;
                }
            }

            Real64 HXCapacityRate = Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * this->RecoveryMassFlowRate;
            Real64 DrainCapacityRate = Psychrometrics::CPHW(DataGlobalConstants::InitConvTemp) * this->DrainMassFlowRate;
            Real64 MinCapacityRate = min(DrainCapacityRate, HXCapacityRate);

            {
                auto const SELECT_CASE_var(this->HeatRecoveryHX);
                if (SELECT_CASE_var == HeatRecoveryHXEnum::Ideal) {
                    this->Effectiveness = 1.0;

                } else if (SELECT_CASE_var == HeatRecoveryHXEnum::CounterFlow) { // Unmixed
                    Real64 CapacityRatio = MinCapacityRate / max(DrainCapacityRate, HXCapacityRate);
                    Real64 NTU = this->HXUA / MinCapacityRate;
                    if (CapacityRatio == 1.0) {
                        this->Effectiveness = NTU / (1.0 + NTU);
                    } else {
                        Real64 ExpVal = std::exp(-NTU * (1.0 - CapacityRatio));
                        this->Effectiveness = (1.0 - ExpVal) / (1.0 - CapacityRatio * ExpVal);
                    }

                } else if (SELECT_CASE_var == HeatRecoveryHXEnum::CrossFlow) { // Unmixed
                    Real64 CapacityRatio = MinCapacityRate / max(DrainCapacityRate, HXCapacityRate);
                    Real64 NTU = this->HXUA / MinCapacityRate;
                    this->Effectiveness =
                        1.0 - std::exp((std::pow(NTU, 0.22) / CapacityRatio) * (std::exp(-CapacityRatio * std::pow(NTU, 0.78)) - 1.0));
                }
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

            {
                auto const SELECT_CASE_var(this->HeatRecoveryConfig);
                if (SELECT_CASE_var == HeatRecoveryConfigEnum::Plant) {
                    this->TempError = 0.0; // No feedback back to the cold supply
                    this->ReturnTemp = this->RecoveryTemp;

                } else if (SELECT_CASE_var == HeatRecoveryConfigEnum::Equipment) {
                    this->TempError = std::abs(this->ColdTemp - this->RecoveryTemp);

                    this->ColdTemp = this->RecoveryTemp;
                    this->ReturnTemp = this->ColdSupplyTemp;

                } else if (SELECT_CASE_var == HeatRecoveryConfigEnum::PlantAndEquip) {
                    this->TempError = std::abs(this->ColdTemp - this->RecoveryTemp);

                    this->ColdTemp = this->RecoveryTemp;
                    this->ReturnTemp = this->RecoveryTemp;
                }
            }
        }
    }

    void WaterConnectionsType::UpdateWaterConnections(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Updates the node variables with local variables.

        if (this->InletNode > 0 && this->OutletNode > 0) {
            // Pass all variables from inlet to outlet node
            PlantUtilities::SafeCopyPlantNode(state, this->InletNode, this->OutletNode, this->PlantLoopNum);

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
        //       RE-ENGINEERED  na

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
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates report variables.

        for (int Loop = 1; Loop <= this->NumWaterEquipment; ++Loop) {

            int WaterEquipNum = this->myWaterEquipArr(Loop);
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

    void CalcWaterUseZoneGains(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

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
