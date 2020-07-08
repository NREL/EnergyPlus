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
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/DataWater.hh>
#include <EnergyPlus/HeatBalanceInternalHeatGains.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
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

    void SimulateWaterUse(BranchInputManagerData &dataBranchInputManager, WaterUseData &dataWaterUse, bool FirstHVACIteration)
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
        static bool MyEnvrnFlagLocal(true);

        if (dataWaterUse.getWaterUseInputFlag) {
            GetWaterUseInput(dataWaterUse);
            dataWaterUse.getWaterUseInputFlag = false;
        }

        if (DataGlobals::BeginEnvrnFlag && MyEnvrnFlagLocal) {
            if (dataWaterUse.numWaterEquipment > 0) {
                for (auto &e : dataWaterUse.WaterEquipment) {
                    e.SensibleRate = 0.0;
                    e.SensibleEnergy = 0.0;
                    e.LatentRate = 0.0;
                    e.LatentEnergy = 0.0;
                    e.MixedTemp = 0.0;
                    e.TotalMassFlowRate = 0.0;
                    e.DrainTemp = 0.0;
                }
            }

            if (dataWaterUse.numWaterConnections > 0) {
                for (auto &e : dataWaterUse.WaterConnections)
                    e.TotalMassFlowRate = 0.0;
            }

            MyEnvrnFlagLocal = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) MyEnvrnFlagLocal = true;

        // Simulate all unconnected WATER USE EQUIPMENT objects
        for (WaterEquipNum = 1; WaterEquipNum <= dataWaterUse.numWaterEquipment; ++WaterEquipNum) {
            if (dataWaterUse.WaterEquipment(WaterEquipNum).Connections == 0) {
                dataWaterUse.WaterEquipment(WaterEquipNum).CalcEquipmentFlowRates(dataWaterUse);
                dataWaterUse.WaterEquipment(WaterEquipNum).CalcEquipmentDrainTemp();
            }
        } // WaterEquipNum

        ReportStandAloneWaterUse(dataWaterUse);

        // Simulate WATER USE CONNECTIONS objects and connected WATER USE EQUIPMENT objects
        for (WaterConnNum = 1; WaterConnNum <= dataWaterUse.numWaterConnections; ++WaterConnNum) {

            if (!dataWaterUse.WaterConnections(WaterConnNum).StandAlone) continue; // only model non plant connections here

            dataWaterUse.WaterConnections(WaterConnNum).InitConnections(dataBranchInputManager, dataWaterUse);

            NumIteration = 0;

            while (true) {
                ++NumIteration;

                dataWaterUse.WaterConnections(WaterConnNum).CalcConnectionsFlowRates(dataWaterUse, FirstHVACIteration);
                dataWaterUse.WaterConnections(WaterConnNum).CalcConnectionsDrainTemp(dataWaterUse);
                dataWaterUse.WaterConnections(WaterConnNum).CalcConnectionsHeatRecovery();

                if (dataWaterUse.WaterConnections(WaterConnNum).TempError < Tolerance) {
                    break;
                } else if (NumIteration > MaxIterations) {
                    if (!DataGlobals::WarmupFlag) {
                        if (dataWaterUse.WaterConnections(WaterConnNum).MaxIterationsErrorIndex == 0) {
                            ShowWarningError("WaterUse:Connections = " + dataWaterUse.WaterConnections(WaterConnNum).Name +
                                             ":  Heat recovery temperature did not converge");
                            ShowContinueErrorTimeStamp("");
                        }
                        ShowRecurringWarningErrorAtEnd("WaterUse:Connections = " + dataWaterUse.WaterConnections(WaterConnNum).Name +
                                                           ":  Heat recovery temperature did not converge",
                                                       dataWaterUse.WaterConnections(WaterConnNum).MaxIterationsErrorIndex);
                    }
                    break;
                }

            } // WHILE

            dataWaterUse.WaterConnections(WaterConnNum).UpdateWaterConnections();
            dataWaterUse.WaterConnections(WaterConnNum).ReportWaterUse(dataWaterUse);

        } // WaterConnNum
    }

    PlantComponent *WaterConnectionsType::factory(WaterUseData &dataWaterUse, std::string const &objectName)
    {
        // Process the input data
        if (dataWaterUse.getWaterUseInputFlag) {
            GetWaterUseInput(dataWaterUse);
            dataWaterUse.getWaterUseInputFlag = false;
        }

        // Now look for this particular object in the list
        for (auto &thisWC : dataWaterUse.WaterConnections) {
            if (thisWC.Name == objectName) {
                return &thisWC;
            }
        }
        // If we didn't find it, fatal
        ShowFatalError("LocalWaterUseConnectionFactory: Error getting inputs for object named: " + objectName); // LCOV_EXCL_LINE
        // Shut up the compiler
        return nullptr; // LCOV_EXCL_LINE
    }

    void WaterConnectionsType::simulate(EnergyPlusData &state, const PlantLocation &EP_UNUSED(calledFromLocation),
                                        bool FirstHVACIteration,
                                        Real64 &EP_UNUSED(CurLoad),
                                        bool EP_UNUSED(RunFlag))
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

        if (DataGlobals::BeginEnvrnFlag && this->MyEnvrnFlag) {
            if (state.dataWaterUse.numWaterEquipment > 0) {
                for (int i = state.dataWaterUse.WaterEquipment.l(), e = state.dataWaterUse.WaterEquipment.u(); i <= e; ++i) {
                    state.dataWaterUse.WaterEquipment(i).reset();

                    if (state.dataWaterUse.WaterEquipment(i).setupMyOutputVars) {
                        state.dataWaterUse.WaterEquipment(i).setupOutputVars(state.dataWaterUse);
                        state.dataWaterUse.WaterEquipment(i).setupMyOutputVars = false;
                    }
                }
            }

            if (state.dataWaterUse.numWaterConnections > 0) {
                for (auto &e : state.dataWaterUse.WaterConnections)
                    e.TotalMassFlowRate = 0.0;
            }

            this->MyEnvrnFlag = false;
        }

        if (!DataGlobals::BeginEnvrnFlag) this->MyEnvrnFlag = true;

        this->InitConnections(state.dataBranchInputManager, state.dataWaterUse);

        int NumIteration = 0;

        while (true) {
            ++NumIteration;

            this->CalcConnectionsFlowRates(state.dataWaterUse, FirstHVACIteration);
            this->CalcConnectionsDrainTemp(state.dataWaterUse);
            this->CalcConnectionsHeatRecovery();

            if (this->TempError < Tolerance) {
                break;
            } else if (NumIteration > MaxIterations) {
                if (!DataGlobals::WarmupFlag) {
                    if (this->MaxIterationsErrorIndex == 0) {
                        ShowWarningError("WaterUse:Connections = " + this->Name + ":  Heat recovery temperature did not converge");
                        ShowContinueErrorTimeStamp("");
                    }
                    ShowRecurringWarningErrorAtEnd("WaterUse:Connections = " + this->Name + ":  Heat recovery temperature did not converge",
                                                   this->MaxIterationsErrorIndex);
                }
                break;
            }
        } // WHILE

        this->UpdateWaterConnections();
        this->ReportWaterUse(state.dataWaterUse);
    }

    void GetWaterUseInput(WaterUseData &dataWaterUse)
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

        DataIPShortCuts::cCurrentModuleObject = "WaterUse:Equipment";
        dataWaterUse.numWaterEquipment = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (dataWaterUse.numWaterEquipment > 0) {
            dataWaterUse.WaterEquipment.allocate(dataWaterUse.numWaterEquipment);

            for (int WaterEquipNum = 1; WaterEquipNum <= dataWaterUse.numWaterEquipment; ++WaterEquipNum) {
                inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                              WaterEquipNum,
                                              DataIPShortCuts::cAlphaArgs,
                                              NumAlphas,
                                              DataIPShortCuts::rNumericArgs,
                                              NumNumbers,
                                              IOStatus,
                                              _,
                                              DataIPShortCuts::lAlphaFieldBlanks,
                                              DataIPShortCuts::cAlphaFieldNames,
                                              DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);
                dataWaterUse.WaterEquipment(WaterEquipNum).Name = DataIPShortCuts::cAlphaArgs(1);

                dataWaterUse.WaterEquipment(WaterEquipNum).EndUseSubcatName = DataIPShortCuts::cAlphaArgs(2);

                dataWaterUse.WaterEquipment(WaterEquipNum).PeakVolFlowRate = DataIPShortCuts::rNumericArgs(1);

                if ((NumAlphas > 2) && (!DataIPShortCuts::lAlphaFieldBlanks(3))) {
                    dataWaterUse.WaterEquipment(WaterEquipNum).FlowRateFracSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(3));
                    // If no FlowRateFracSchedule, fraction defaults to 1.0

                    if (dataWaterUse.WaterEquipment(WaterEquipNum).FlowRateFracSchedule == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(3) + '=' + DataIPShortCuts::cAlphaArgs(3));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 3) && (!DataIPShortCuts::lAlphaFieldBlanks(4))) {
                    dataWaterUse.WaterEquipment(WaterEquipNum).TargetTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(4));

                    if (dataWaterUse.WaterEquipment(WaterEquipNum).TargetTempSchedule == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(4) + '=' + DataIPShortCuts::cAlphaArgs(4));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 4) && (!DataIPShortCuts::lAlphaFieldBlanks(5))) {
                    dataWaterUse.WaterEquipment(WaterEquipNum).HotTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(5));
                    // If no HotTempSchedule, there is no hot water.
                    // HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

                    if (dataWaterUse.WaterEquipment(WaterEquipNum).HotTempSchedule == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(5) + '=' + DataIPShortCuts::cAlphaArgs(5));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 5) && (!DataIPShortCuts::lAlphaFieldBlanks(6))) {
                    dataWaterUse.WaterEquipment(WaterEquipNum).ColdTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(6));
                    // If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

                    if (dataWaterUse.WaterEquipment(WaterEquipNum).ColdTempSchedule == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + '=' + DataIPShortCuts::cAlphaArgs(6));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 6) && (!DataIPShortCuts::lAlphaFieldBlanks(7))) {
                    dataWaterUse.WaterEquipment(WaterEquipNum).Zone = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(7), DataHeatBalance::Zone);

                    if (dataWaterUse.WaterEquipment(WaterEquipNum).Zone == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + '=' + DataIPShortCuts::cAlphaArgs(7));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 7) && (!DataIPShortCuts::lAlphaFieldBlanks(8))) {
                    dataWaterUse.WaterEquipment(WaterEquipNum).SensibleFracSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(8));

                    if (dataWaterUse.WaterEquipment(WaterEquipNum).SensibleFracSchedule == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(8) + '=' + DataIPShortCuts::cAlphaArgs(8));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((NumAlphas > 8) && (!DataIPShortCuts::lAlphaFieldBlanks(9))) {
                    dataWaterUse.WaterEquipment(WaterEquipNum).LatentFracSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(9));

                    if (dataWaterUse.WaterEquipment(WaterEquipNum).LatentFracSchedule == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + DataIPShortCuts::cAlphaArgs(9));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

            } // WaterEquipNum

            if (ErrorsFound) ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);
        }

        DataIPShortCuts::cCurrentModuleObject = "WaterUse:Connections";
        dataWaterUse.numWaterConnections = inputProcessor->getNumObjectsFound(DataIPShortCuts::cCurrentModuleObject);

        if (dataWaterUse.numWaterConnections > 0) {
            dataWaterUse.WaterConnections.allocate(dataWaterUse.numWaterConnections);

            for (int WaterConnNum = 1; WaterConnNum <= dataWaterUse.numWaterConnections; ++WaterConnNum) {
                inputProcessor->getObjectItem(DataIPShortCuts::cCurrentModuleObject,
                                              WaterConnNum,
                                              DataIPShortCuts::cAlphaArgs,
                                              NumAlphas,
                                              DataIPShortCuts::rNumericArgs,
                                              NumNumbers,
                                              IOStatus,
                                              _,
                                              DataIPShortCuts::lAlphaFieldBlanks,
                                              DataIPShortCuts::cAlphaFieldNames,
                                              DataIPShortCuts::cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(DataIPShortCuts::cAlphaArgs(1), DataIPShortCuts::cCurrentModuleObject, ErrorsFound);
                dataWaterUse.WaterConnections(WaterConnNum).Name = DataIPShortCuts::cAlphaArgs(1);

                if ((!DataIPShortCuts::lAlphaFieldBlanks(2)) || (!DataIPShortCuts::lAlphaFieldBlanks(3))) {
                    dataWaterUse.WaterConnections(WaterConnNum).InletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(2),
                                                                                                   ErrorsFound,
                                                                                                   DataIPShortCuts::cCurrentModuleObject,
                                                                                                   DataIPShortCuts::cAlphaArgs(1),
                                                                                                   DataLoopNode::NodeType_Water,
                                                                                                   DataLoopNode::NodeConnectionType_Inlet,
                                                                                                   1,
                                                                                                   DataLoopNode::ObjectIsNotParent);
                    dataWaterUse.WaterConnections(WaterConnNum).OutletNode = NodeInputManager::GetOnlySingleNode(DataIPShortCuts::cAlphaArgs(3),
                                                                                                    ErrorsFound,
                                                                                                    DataIPShortCuts::cCurrentModuleObject,
                                                                                                    DataIPShortCuts::cAlphaArgs(1),
                                                                                                    DataLoopNode::NodeType_Water,
                                                                                                    DataLoopNode::NodeConnectionType_Outlet,
                                                                                                    1,
                                                                                                    DataLoopNode::ObjectIsNotParent);

                    // Check plant connections
                    BranchNodeConnections::TestCompSet(DataIPShortCuts::cCurrentModuleObject,
                                                       DataIPShortCuts::cAlphaArgs(1),
                                                       DataIPShortCuts::cAlphaArgs(2),
                                                       DataIPShortCuts::cAlphaArgs(3),
                                                       "DHW Nodes");
                } else {
                    // If no plant nodes are connected, simulate in stand-alone mode.
                    dataWaterUse.WaterConnections(WaterConnNum).StandAlone = true;
                }

                if (!DataIPShortCuts::lAlphaFieldBlanks(4)) {
                    WaterManager::SetupTankDemandComponent(dataWaterUse.WaterConnections(WaterConnNum).Name,
                                                           DataIPShortCuts::cCurrentModuleObject,
                                                           DataIPShortCuts::cAlphaArgs(4),
                                                           ErrorsFound,
                                                           dataWaterUse.WaterConnections(WaterConnNum).SupplyTankNum,
                                                           dataWaterUse.WaterConnections(WaterConnNum).TankDemandID);
                }

                if (!DataIPShortCuts::lAlphaFieldBlanks(5)) {
                    WaterManager::SetupTankSupplyComponent(dataWaterUse.WaterConnections(WaterConnNum).Name,
                                                           DataIPShortCuts::cCurrentModuleObject,
                                                           DataIPShortCuts::cAlphaArgs(5),
                                                           ErrorsFound,
                                                           dataWaterUse.WaterConnections(WaterConnNum).RecoveryTankNum,
                                                           dataWaterUse.WaterConnections(WaterConnNum).TankSupplyID);
                }

                if (!DataIPShortCuts::lAlphaFieldBlanks(6)) {
                    dataWaterUse.WaterConnections(WaterConnNum).HotTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(6));
                    // If no HotTempSchedule, there is no hot water.
                    // HotTempSchedule is ignored if connected to a plant loop via WATER USE CONNECTIONS

                    if (dataWaterUse.WaterConnections(WaterConnNum).HotTempSchedule == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(6) + '=' + DataIPShortCuts::cAlphaArgs(6));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if (!DataIPShortCuts::lAlphaFieldBlanks(7)) {
                    dataWaterUse.WaterConnections(WaterConnNum).ColdTempSchedule = ScheduleManager::GetScheduleIndex(DataIPShortCuts::cAlphaArgs(7));
                    // If no ColdTempSchedule, temperatures will be calculated by WATER MAINS TEMPERATURES object

                    if (dataWaterUse.WaterConnections(WaterConnNum).ColdTempSchedule == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(7) + '=' + DataIPShortCuts::cAlphaArgs(7));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    }
                }

                if ((!DataIPShortCuts::lAlphaFieldBlanks(8)) && (DataIPShortCuts::cAlphaArgs(8) != "NONE")) {
                    dataWaterUse.WaterConnections(WaterConnNum).HeatRecovery = true;

                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(8));
                        if (SELECT_CASE_var == "IDEAL") {
                            dataWaterUse.WaterConnections(WaterConnNum).HeatRecoveryHX = HeatRecoveryHXEnum::Ideal;
                        } else if (SELECT_CASE_var == "COUNTERFLOW") {
                            dataWaterUse.WaterConnections(WaterConnNum).HeatRecoveryHX = HeatRecoveryHXEnum::CounterFlow;
                        } else if (SELECT_CASE_var == "CROSSFLOW") {
                            dataWaterUse.WaterConnections(WaterConnNum).HeatRecoveryHX = HeatRecoveryHXEnum::CrossFlow;
                        } else {
                            ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(8) + '=' + DataIPShortCuts::cAlphaArgs(8));
                            ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                    }

                    {
                        auto const SELECT_CASE_var(DataIPShortCuts::cAlphaArgs(9));
                        if (SELECT_CASE_var == "PLANT") {
                            dataWaterUse.WaterConnections(WaterConnNum).HeatRecoveryConfig = HeatRecoveryConfigEnum::Plant;
                        } else if (SELECT_CASE_var == "EQUIPMENT") {
                            dataWaterUse.WaterConnections(WaterConnNum).HeatRecoveryConfig = HeatRecoveryConfigEnum::Equipment;
                        } else if (SELECT_CASE_var == "PLANTANDEQUIPMENT") {
                            dataWaterUse.WaterConnections(WaterConnNum).HeatRecoveryConfig = HeatRecoveryConfigEnum::PlantAndEquip;
                        } else {
                            ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(9) + '=' + DataIPShortCuts::cAlphaArgs(9));
                            ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                            ErrorsFound = true;
                        }
                    }
                }

                dataWaterUse.WaterConnections(WaterConnNum).HXUA = DataIPShortCuts::rNumericArgs(1);

                dataWaterUse.WaterConnections(WaterConnNum).myWaterEquipArr.allocate(NumAlphas - 9);

                for (AlphaNum = 10; AlphaNum <= NumAlphas; ++AlphaNum) {
                    int WaterEquipNum = UtilityRoutines::FindItemInList(DataIPShortCuts::cAlphaArgs(AlphaNum), dataWaterUse.WaterEquipment);

                    if (WaterEquipNum == 0) {
                        ShowSevereError("Invalid " + DataIPShortCuts::cAlphaFieldNames(AlphaNum) + '=' + DataIPShortCuts::cAlphaArgs(AlphaNum));
                        ShowContinueError("Entered in " + DataIPShortCuts::cCurrentModuleObject + '=' + DataIPShortCuts::cAlphaArgs(1));
                        ErrorsFound = true;
                    } else {
                        if (dataWaterUse.WaterEquipment(WaterEquipNum).Connections > 0) {
                            ShowSevereError(DataIPShortCuts::cCurrentModuleObject + " = " + DataIPShortCuts::cAlphaArgs(1) +
                                            ":  WaterUse:Equipment = " + DataIPShortCuts::cAlphaArgs(AlphaNum) +
                                            " is already referenced by another object.");
                            ErrorsFound = true;
                        } else {
                            dataWaterUse.WaterEquipment(WaterEquipNum).Connections = WaterConnNum;

                            ++dataWaterUse.WaterConnections(WaterConnNum).NumWaterEquipment;
                            dataWaterUse.WaterConnections(WaterConnNum).myWaterEquipArr(dataWaterUse.WaterConnections(WaterConnNum).NumWaterEquipment) = WaterEquipNum;

                            dataWaterUse.WaterConnections(WaterConnNum).PeakVolFlowRate +=
                                dataWaterUse.WaterEquipment(WaterEquipNum).PeakVolFlowRate; // this does not include possible multipliers
                        }
                    }
                }

            } // WaterConnNum

            if (ErrorsFound) ShowFatalError("Errors found in processing input for " + DataIPShortCuts::cCurrentModuleObject);

            if (dataWaterUse.numWaterConnections > 0) {
                dataWaterUse.CheckEquipName.allocate(dataWaterUse.numWaterConnections);
                dataWaterUse.CheckEquipName = true;
            }
        }

        // determine connection's peak mass flow rates.
        if (dataWaterUse.numWaterConnections > 0) {
            for (int WaterConnNum = 1; WaterConnNum <= dataWaterUse.numWaterConnections; ++WaterConnNum) {
                dataWaterUse.WaterConnections(WaterConnNum).PeakMassFlowRate = 0.0;
                for (int WaterEquipNum = 1; WaterEquipNum <= dataWaterUse.WaterConnections(WaterConnNum).NumWaterEquipment; ++WaterEquipNum) {
                    int thisWaterEquipNum = dataWaterUse.WaterConnections(WaterConnNum).myWaterEquipArr(WaterEquipNum);
                    if (dataWaterUse.WaterEquipment(thisWaterEquipNum).Zone > 0) {
                        dataWaterUse.WaterConnections(WaterConnNum).PeakMassFlowRate +=
                            dataWaterUse.WaterEquipment(thisWaterEquipNum).PeakVolFlowRate * Psychrometrics::RhoH2O(DataGlobals::InitConvTemp) *
                            DataHeatBalance::Zone(dataWaterUse.WaterEquipment(thisWaterEquipNum).Zone).Multiplier *
                            DataHeatBalance::Zone(dataWaterUse.WaterEquipment(thisWaterEquipNum).Zone).ListMultiplier;
                    } else { // can't have multipliers
                        dataWaterUse.WaterConnections(WaterConnNum).PeakMassFlowRate +=
                            dataWaterUse.WaterEquipment(thisWaterEquipNum).PeakVolFlowRate * Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
                    }
                }
                PlantUtilities::RegisterPlantCompDesignFlow(dataWaterUse.WaterConnections(WaterConnNum).InletNode,
                                                            dataWaterUse.WaterConnections(WaterConnNum).PeakMassFlowRate /
                                                                Psychrometrics::RhoH2O(DataGlobals::InitConvTemp));
            }
        }
    }

    void WaterEquipmentType::setupOutputVars(WaterUseData &dataWaterUse)
    {
        SetupOutputVariable(
            "Water Use Equipment Hot Water Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HotMassFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Equipment Cold Water Mass Flow Rate", OutputProcessor::Unit::kg_s, this->ColdMassFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Equipment Total Mass Flow Rate", OutputProcessor::Unit::kg_s, this->TotalMassFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Equipment Hot Water Volume Flow Rate", OutputProcessor::Unit::m3_s, this->HotVolFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Equipment Cold Water Volume Flow Rate", OutputProcessor::Unit::m3_s, this->ColdVolFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Equipment Total Volume Flow Rate", OutputProcessor::Unit::m3_s, this->TotalVolFlowRate, "System", "Average", this->Name);

        SetupOutputVariable("Water Use Equipment Hot Water Volume", OutputProcessor::Unit::m3, this->HotVolume, "System", "Sum", this->Name);

        SetupOutputVariable("Water Use Equipment Cold Water Volume", OutputProcessor::Unit::m3, this->ColdVolume, "System", "Sum", this->Name);

        SetupOutputVariable("Water Use Equipment Total Volume",
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
        SetupOutputVariable("Water Use Equipment Mains Water Volume",
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

        SetupOutputVariable("Water Use Equipment Hot Water Temperature", OutputProcessor::Unit::C, this->HotTemp, "System", "Average", this->Name);

        SetupOutputVariable("Water Use Equipment Cold Water Temperature", OutputProcessor::Unit::C, this->ColdTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Equipment Target Water Temperature", OutputProcessor::Unit::C, this->TargetTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Equipment Mixed Water Temperature", OutputProcessor::Unit::C, this->MixedTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Equipment Drain Water Temperature", OutputProcessor::Unit::C, this->DrainTemp, "System", "Average", this->Name);

        SetupOutputVariable("Water Use Equipment Heating Rate", OutputProcessor::Unit::W, this->Power, "System", "Average", this->Name);

        if (this->Connections == 0) {
            SetupOutputVariable("Water Use Equipment Heating Energy",
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

        } else if (dataWaterUse.WaterConnections(this->Connections).StandAlone) {
            SetupOutputVariable("Water Use Equipment Heating Energy",
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
            SetupOutputVariable("Water Use Equipment Heating Energy",
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
            SetupOutputVariable(
                "Water Use Equipment Zone Sensible Heat Gain Rate", OutputProcessor::Unit::W, this->SensibleRate, "System", "Average", this->Name);
            SetupOutputVariable(
                "Water Use Equipment Zone Sensible Heat Gain Energy", OutputProcessor::Unit::J, this->SensibleEnergy, "System", "Sum", this->Name);

            SetupOutputVariable(
                "Water Use Equipment Zone Latent Gain Rate", OutputProcessor::Unit::W, this->LatentRate, "System", "Average", this->Name);
            SetupOutputVariable(
                "Water Use Equipment Zone Latent Gain Energy", OutputProcessor::Unit::J, this->LatentEnergy, "System", "Sum", this->Name);

            SetupOutputVariable("Water Use Equipment Zone Moisture Gain Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                this->MoistureRate,
                                "System",
                                "Average",
                                this->Name);
            SetupOutputVariable(
                "Water Use Equipment Zone Moisture Gain Mass", OutputProcessor::Unit::kg, this->MoistureMass, "System", "Sum", this->Name);

            SetupZoneInternalGain(this->Zone,
                                  "WaterUse:Equipment",
                                  this->Name,
                                  DataHeatBalance::IntGainTypeOf_WaterUseEquipment,
                                  &this->SensibleRateNoMultiplier,
                                  nullptr,
                                  nullptr,
                                  &this->LatentRateNoMultiplier);
        }
    }

    void WaterConnectionsType::setupOutputVars(WaterUseData &dataWaterUse)
    {
        SetupOutputVariable(
            "Water Use Connections Hot Water Mass Flow Rate", OutputProcessor::Unit::kg_s, this->HotMassFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Cold Water Mass Flow Rate", OutputProcessor::Unit::kg_s, this->ColdMassFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Total Mass Flow Rate", OutputProcessor::Unit::kg_s, this->TotalMassFlowRate, "System", "Average", this->Name);

        SetupOutputVariable("Water Use Connections Drain Water Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->DrainMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable("Water Use Connections Heat Recovery Mass Flow Rate",
                            OutputProcessor::Unit::kg_s,
                            this->RecoveryMassFlowRate,
                            "System",
                            "Average",
                            this->Name);

        SetupOutputVariable(
            "Water Use Connections Hot Water Volume Flow Rate", OutputProcessor::Unit::m3_s, this->HotVolFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Cold Water Volume Flow Rate", OutputProcessor::Unit::m3_s, this->ColdVolFlowRate, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Total Volume Flow Rate", OutputProcessor::Unit::m3_s, this->TotalVolFlowRate, "System", "Average", this->Name);

        SetupOutputVariable("Water Use Connections Hot Water Volume", OutputProcessor::Unit::m3, this->HotVolume, "System", "Sum", this->Name);

        SetupOutputVariable("Water Use Connections Cold Water Volume", OutputProcessor::Unit::m3, this->ColdVolume, "System", "Sum", this->Name);

        SetupOutputVariable("Water Use Connections Total Volume", OutputProcessor::Unit::m3, this->TotalVolume, "System", "Sum",
                            this->Name); //, &
        // ResourceTypeKey='Water', EndUseKey='DHW', EndUseSubKey=EndUseSubcategoryName, GroupKey='Plant')
        // tHIS WAS double counting

        SetupOutputVariable("Water Use Connections Hot Water Temperature", OutputProcessor::Unit::C, this->HotTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Cold Water Temperature", OutputProcessor::Unit::C, this->ColdTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Drain Water Temperature", OutputProcessor::Unit::C, this->DrainTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Return Water Temperature", OutputProcessor::Unit::C, this->ReturnTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Waste Water Temperature", OutputProcessor::Unit::C, this->WasteTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Heat Recovery Water Temperature", OutputProcessor::Unit::C, this->RecoveryTemp, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Heat Recovery Effectiveness", OutputProcessor::Unit::None, this->Effectiveness, "System", "Average", this->Name);

        SetupOutputVariable(
            "Water Use Connections Heat Recovery Rate", OutputProcessor::Unit::W, this->RecoveryRate, "System", "Average", this->Name);
        SetupOutputVariable(
            "Water Use Connections Heat Recovery Energy", OutputProcessor::Unit::J, this->RecoveryEnergy, "System", "Sum", this->Name);
        // Does this go on a meter?

        // To do:  Add report variable for starved flow when tank can't deliver?

        if (!this->StandAlone) {
            SetupOutputVariable("Water Use Connections Plant Hot Water Energy",
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

    void WaterEquipmentType::CalcEquipmentFlowRates(WaterUseData &dataWaterUse)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculate desired hot and cold water flow rates

        if (this->setupMyOutputVars) {
            this->setupOutputVars(dataWaterUse);
            this->setupMyOutputVars = false;
        }

        if (this->Connections > 0) {
            // Get water temperature conditions from the CONNECTIONS object
            this->ColdTemp = dataWaterUse.WaterConnections(this->Connections).ColdTemp;
            this->HotTemp = dataWaterUse.WaterConnections(this->Connections).HotTemp;

        } else {
            // Get water temperature conditions from the WATER USE EQUIPMENT schedules
            if (this->ColdTempSchedule > 0) {
                this->ColdTemp = ScheduleManager::GetCurrentScheduleValue(this->ColdTempSchedule);
            } else { // If no ColdTempSchedule, use the mains temperature
                this->ColdTemp = DataEnvironment::WaterMainsTemp;
            }

            if (this->HotTempSchedule > 0) {
                this->HotTemp = ScheduleManager::GetCurrentScheduleValue(this->HotTempSchedule);
            } else { // If no HotTempSchedule, use all cold water
                this->HotTemp = this->ColdTemp;
            }
        }

        if (this->TargetTempSchedule > 0) {
            this->TargetTemp = ScheduleManager::GetCurrentScheduleValue(this->TargetTempSchedule);
        } else { // If no TargetTempSchedule, use all hot water
            this->TargetTemp = this->HotTemp;
        }

        // Get the requested total flow rate
        if (this->Zone > 0) {
            if (this->FlowRateFracSchedule > 0) {
                this->TotalVolFlowRate = this->PeakVolFlowRate * ScheduleManager::GetCurrentScheduleValue(this->FlowRateFracSchedule) *
                                         DataHeatBalance::Zone(this->Zone).Multiplier * DataHeatBalance::Zone(this->Zone).ListMultiplier;
            } else {
                this->TotalVolFlowRate =
                    this->PeakVolFlowRate * DataHeatBalance::Zone(this->Zone).Multiplier * DataHeatBalance::Zone(this->Zone).ListMultiplier;
            }
        } else {
            if (this->FlowRateFracSchedule > 0) {
                this->TotalVolFlowRate = this->PeakVolFlowRate * ScheduleManager::GetCurrentScheduleValue(this->FlowRateFracSchedule);
            } else {
                this->TotalVolFlowRate = this->PeakVolFlowRate;
            }
        }

        this->TotalMassFlowRate = this->TotalVolFlowRate * Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);

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

    void WaterEquipmentType::CalcEquipmentDrainTemp()
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
                this->SensibleRate = ScheduleManager::GetCurrentScheduleValue(this->SensibleFracSchedule) * this->TotalMassFlowRate *
                                     Psychrometrics::CPHW(DataGlobals::InitConvTemp) * (this->MixedTemp - DataHeatBalFanSys::MAT(this->Zone));
                this->SensibleEnergy = this->SensibleRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            }

            if (this->LatentFracSchedule == 0) {
                this->LatentRate = 0.0;
                this->LatentEnergy = 0.0;
            } else {
                Real64 ZoneHumRat = DataHeatBalFanSys::ZoneAirHumRat(this->Zone);
                Real64 ZoneHumRatSat = Psychrometrics::PsyWFnTdbRhPb(
                    DataHeatBalFanSys::MAT(this->Zone), 1.0, DataEnvironment::OutBaroPress, RoutineName); // Humidratio at 100% relative humidity
                Real64 RhoAirDry = Psychrometrics::PsyRhoAirFnPbTdbW(DataEnvironment::OutBaroPress, DataHeatBalFanSys::MAT(this->Zone), 0.0);
                Real64 ZoneMassMax =
                    (ZoneHumRatSat - ZoneHumRat) * RhoAirDry * DataHeatBalance::Zone(this->Zone).Volume; // Max water that can be evaporated to zone
                Real64 FlowMassMax = this->TotalMassFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour; // Max water in flow
                Real64 MoistureMassMax = min(ZoneMassMax, FlowMassMax);

                this->MoistureMass = ScheduleManager::GetCurrentScheduleValue(this->LatentFracSchedule) * MoistureMassMax;
                this->MoistureRate = this->MoistureMass / (DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour);

                this->LatentRate = this->MoistureRate * Psychrometrics::PsyHfgAirFnWTdb(ZoneHumRat, DataHeatBalFanSys::MAT(this->Zone));
                this->LatentEnergy = this->LatentRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            }

            this->DrainMassFlowRate = this->TotalMassFlowRate - this->MoistureRate;

            if (this->DrainMassFlowRate == 0.0) {
                this->DrainTemp = this->MixedTemp;
            } else {
                this->DrainTemp = (this->TotalMassFlowRate * Psychrometrics::CPHW(DataGlobals::InitConvTemp) * this->MixedTemp - this->SensibleRate -
                                   this->LatentRate) /
                                  (this->DrainMassFlowRate * Psychrometrics::CPHW(DataGlobals::InitConvTemp));
            }
        }
    }

    void WaterConnectionsType::InitConnections(BranchInputManagerData &dataBranchInputManager, WaterUseData &dataWaterUse)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED       Brent Griffith 2010, demand side update
        //       RE-ENGINEERED  na

        if (this->setupMyOutputVars) {
            this->setupOutputVars(dataWaterUse);
            this->setupMyOutputVars = false;
        }

        if (this->plantScanFlag && allocated(DataPlant::PlantLoop) && !this->StandAlone) {
            bool errFlag = false;
            PlantUtilities::ScanPlantLoopsForObject(dataBranchInputManager,
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
                ShowFatalError("InitConnections: Program terminated due to previous condition(s).");
            }
            this->plantScanFlag = false;
        }

        // Set the cold water temperature
        if (this->SupplyTankNum > 0) {
            this->ColdSupplyTemp = DataWater::WaterStorage(this->SupplyTankNum).Twater;

        } else if (this->ColdTempSchedule > 0) {
            this->ColdSupplyTemp = ScheduleManager::GetCurrentScheduleValue(this->ColdTempSchedule);

        } else {
            this->ColdSupplyTemp = DataEnvironment::WaterMainsTemp;
        }

        // Initially set ColdTemp to the ColdSupplyTemp; with heat recovery, ColdTemp will change during iteration
        this->ColdTemp = this->ColdSupplyTemp;

        // Set the hot water temperature
        if (this->StandAlone) {
            if (this->HotTempSchedule > 0) {
                this->HotTemp = ScheduleManager::GetCurrentScheduleValue(this->HotTempSchedule);
            } else {
                // If no HotTempSchedule, use all cold water
                this->HotTemp = this->ColdTemp;
            }

        } else {

            if (DataGlobals::BeginEnvrnFlag && this->Init) {
                // Clear node initial conditions
                if (this->InletNode > 0 && this->OutletNode > 0) {
                    PlantUtilities::InitComponentNodes(0.0,
                                                       this->PeakMassFlowRate,
                                                       this->InletNode,
                                                       this->OutletNode,
                                                       this->PlantLoopNum,
                                                       this->PlantLoopSide,
                                                       this->PlantLoopBranchNum,
                                                       this->PlantLoopCompNum);

                    this->ReturnTemp = DataLoopNode::Node(this->InletNode).Temp;
                }

                this->Init = false;
            }

            if (!DataGlobals::BeginEnvrnFlag) this->Init = true;

            if (this->InletNode > 0) {
                if (!DataGlobals::DoingSizing) {
                    this->HotTemp = DataLoopNode::Node(this->InletNode).Temp;
                } else {
                    // plant loop will not be running so need a value here.
                    // should change to use tank setpoint but water use connections don't have knowledge of the tank they are fed by
                    this->HotTemp = 60.0;
                }
            }
        }
    }

    void WaterConnectionsType::CalcConnectionsFlowRates(WaterUseData &dataWaterUse, bool FirstHVACIteration)
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

            dataWaterUse.WaterEquipment(WaterEquipNum).CalcEquipmentFlowRates(dataWaterUse);

            this->ColdMassFlowRate += dataWaterUse.WaterEquipment(WaterEquipNum).ColdMassFlowRate;
            this->HotMassFlowRate += dataWaterUse.WaterEquipment(WaterEquipNum).HotMassFlowRate;
        } // Loop

        this->TotalMassFlowRate = this->ColdMassFlowRate + this->HotMassFlowRate;

        if (!this->StandAlone) { // Interact with the plant loop
            if (this->InletNode > 0) {
                if (FirstHVACIteration) {
                    // Request the mass flow rate from the demand side manager
                    PlantUtilities::SetComponentFlowRate(this->HotMassFlowRate,
                                                         this->InletNode,
                                                         this->OutletNode,
                                                         this->PlantLoopNum,
                                                         this->PlantLoopSide,
                                                         this->PlantLoopBranchNum,
                                                         this->PlantLoopCompNum);

                } else {
                    Real64 DesiredHotWaterMassFlow = this->HotMassFlowRate;
                    PlantUtilities::SetComponentFlowRate(DesiredHotWaterMassFlow,
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
                            dataWaterUse.WaterEquipment(WaterEquipNum).HotMassFlowRate *= AvailableFraction;
                            dataWaterUse.WaterEquipment(WaterEquipNum).ColdMassFlowRate =
                                dataWaterUse.WaterEquipment(WaterEquipNum).TotalMassFlowRate - dataWaterUse.WaterEquipment(WaterEquipNum).HotMassFlowRate;

                            // Recalculate mixed water temperature
                            if (dataWaterUse.WaterEquipment(WaterEquipNum).TotalMassFlowRate > 0.0) {
                                dataWaterUse.WaterEquipment(WaterEquipNum).MixedTemp =
                                    (dataWaterUse.WaterEquipment(WaterEquipNum).ColdMassFlowRate * dataWaterUse.WaterEquipment(WaterEquipNum).ColdTemp +
                                        dataWaterUse.WaterEquipment(WaterEquipNum).HotMassFlowRate * dataWaterUse.WaterEquipment(WaterEquipNum).HotTemp) /
                                    dataWaterUse.WaterEquipment(WaterEquipNum).TotalMassFlowRate;
                            } else {
                                dataWaterUse.WaterEquipment(WaterEquipNum).MixedTemp = dataWaterUse.WaterEquipment(WaterEquipNum).TargetTemp;
                            }
                        } // Loop
                    }
                }
            }
        }

        if (this->SupplyTankNum > 0) {
            // Set the demand request for supply water from water storage tank
            this->ColdVolFlowRate = this->ColdMassFlowRate / Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
            DataWater::WaterStorage(this->SupplyTankNum).VdotRequestDemand(this->TankDemandID) = this->ColdVolFlowRate;

            // Check if cold flow rate should be starved by restricted flow from tank
            // Currently, the tank flow is not really starved--water continues to flow at the tank water temperature
            // But the user can see the error by comparing report variables for TankVolFlowRate < ColdVolFlowRate
            this->TankVolFlowRate = DataWater::WaterStorage(this->SupplyTankNum).VdotAvailDemand(this->TankDemandID);
            this->TankMassFlowRate = this->TankVolFlowRate * Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
        }
    }

    void WaterConnectionsType::CalcConnectionsDrainTemp(WaterUseData &dataWaterUse)
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

            dataWaterUse.WaterEquipment(WaterEquipNum).CalcEquipmentDrainTemp();

            this->DrainMassFlowRate += dataWaterUse.WaterEquipment(WaterEquipNum).DrainMassFlowRate;
            MassFlowTempSum += dataWaterUse.WaterEquipment(WaterEquipNum).DrainMassFlowRate * dataWaterUse.WaterEquipment(WaterEquipNum).DrainTemp;
        } // Loop

        if (this->DrainMassFlowRate > 0.0) {
            this->DrainTemp = MassFlowTempSum / this->DrainMassFlowRate;
        } else {
            this->DrainTemp = this->HotTemp;
        }

        this->DrainVolFlowRate = this->DrainMassFlowRate * Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
    }

    void WaterConnectionsType::CalcConnectionsHeatRecovery()
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

        } else { // dataWaterUse.WaterConnections(WaterConnNum)%TotalMassFlowRate > 0.0

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

            Real64 HXCapacityRate = Psychrometrics::CPHW(DataGlobals::InitConvTemp) * this->RecoveryMassFlowRate;
            Real64 DrainCapacityRate = Psychrometrics::CPHW(DataGlobals::InitConvTemp) * this->DrainMassFlowRate;
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
                this->ColdSupplyTemp + this->RecoveryRate / (Psychrometrics::CPHW(DataGlobals::InitConvTemp) * this->TotalMassFlowRate);
            this->WasteTemp = this->DrainTemp - this->RecoveryRate / (Psychrometrics::CPHW(DataGlobals::InitConvTemp) * this->TotalMassFlowRate);

            if (this->RecoveryTankNum > 0) {
                DataWater::WaterStorage(this->RecoveryTankNum).VdotAvailSupply(this->TankSupplyID) = this->DrainVolFlowRate;
                DataWater::WaterStorage(this->RecoveryTankNum).TwaterSupply(this->TankSupplyID) = this->WasteTemp;
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

    void WaterConnectionsType::UpdateWaterConnections()
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
            PlantUtilities::SafeCopyPlantNode(this->InletNode, this->OutletNode, this->PlantLoopNum);

            // Set outlet node variables that are possibly changed
            DataLoopNode::Node(this->OutletNode).Temp = this->ReturnTemp;
            // should add enthalpy update to return?
        }
    }

    void ReportStandAloneWaterUse(WaterUseData &dataWaterUse)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         B. Griffith, Peter Graham Ellis
        //       DATE WRITTEN   Nov. 2011
        //       MODIFIED       Brent Griffith, March 2010 added argument
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates report variables for stand alone water use

        for (int WaterEquipNum = 1; WaterEquipNum <= dataWaterUse.numWaterEquipment; ++WaterEquipNum) {
            auto &thisWEq = dataWaterUse.WaterEquipment(WaterEquipNum);
            thisWEq.ColdVolFlowRate = thisWEq.ColdMassFlowRate / Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
            thisWEq.HotVolFlowRate = thisWEq.HotMassFlowRate / Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
            thisWEq.TotalVolFlowRate = thisWEq.ColdVolFlowRate + thisWEq.HotVolFlowRate;

            thisWEq.ColdVolume = thisWEq.ColdVolFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            thisWEq.HotVolume = thisWEq.HotVolFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            thisWEq.TotalVolume = thisWEq.TotalVolFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

            if (thisWEq.Connections == 0) {
                thisWEq.Power = thisWEq.HotMassFlowRate * Psychrometrics::CPHW(DataGlobals::InitConvTemp) * (thisWEq.HotTemp - thisWEq.ColdTemp);
            } else {
                thisWEq.Power = thisWEq.HotMassFlowRate * Psychrometrics::CPHW(DataGlobals::InitConvTemp) *
                                (thisWEq.HotTemp - dataWaterUse.WaterConnections(thisWEq.Connections).ReturnTemp);
            }

            thisWEq.Energy = thisWEq.Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        }
    }

    void WaterConnectionsType::ReportWaterUse(WaterUseData &dataWaterUse)
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
            auto &thisWEq = dataWaterUse.WaterEquipment(WaterEquipNum);

            thisWEq.ColdVolFlowRate = thisWEq.ColdMassFlowRate / Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
            thisWEq.HotVolFlowRate = thisWEq.HotMassFlowRate / Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
            thisWEq.TotalVolFlowRate = thisWEq.ColdVolFlowRate + thisWEq.HotVolFlowRate;
            thisWEq.ColdVolume = thisWEq.ColdVolFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            thisWEq.HotVolume = thisWEq.HotVolFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
            thisWEq.TotalVolume = thisWEq.TotalVolFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;

            if (thisWEq.Connections == 0) {
                thisWEq.Power = thisWEq.HotMassFlowRate * Psychrometrics::CPHW(DataGlobals::InitConvTemp) * (thisWEq.HotTemp - thisWEq.ColdTemp);
            } else {
                thisWEq.Power = thisWEq.HotMassFlowRate * Psychrometrics::CPHW(DataGlobals::InitConvTemp) *
                                (thisWEq.HotTemp - dataWaterUse.WaterConnections(thisWEq.Connections).ReturnTemp);
            }

            thisWEq.Energy = thisWEq.Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        }

        this->ColdVolFlowRate = this->ColdMassFlowRate / Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
        this->HotVolFlowRate = this->HotMassFlowRate / Psychrometrics::RhoH2O(DataGlobals::InitConvTemp);
        this->TotalVolFlowRate = this->ColdVolFlowRate + this->HotVolFlowRate;
        this->ColdVolume = this->ColdVolFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->HotVolume = this->HotVolFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->TotalVolume = this->TotalVolFlowRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->Power = this->HotMassFlowRate * Psychrometrics::CPHW(DataGlobals::InitConvTemp) * (this->HotTemp - this->ReturnTemp);
        this->Energy = this->Power * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
        this->RecoveryEnergy = this->RecoveryRate * DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    }

    void CalcWaterUseZoneGains(WaterUseData &dataWaterUse)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   August 2006
        //       MODIFIED
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Calculates the zone internal gains due to water use sensible and latent loads.

        bool MyEnvrnFlagLocal(true);

        if (dataWaterUse.numWaterEquipment == 0) return;

        if (DataGlobals::BeginEnvrnFlag && MyEnvrnFlagLocal) {
            for (auto &e : dataWaterUse.WaterEquipment) {
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

        if (!DataGlobals::BeginEnvrnFlag) MyEnvrnFlagLocal = true;

        for (int WaterEquipNum = 1; WaterEquipNum <= dataWaterUse.numWaterEquipment; ++WaterEquipNum) {
            if (dataWaterUse.WaterEquipment(WaterEquipNum).Zone == 0) continue;
            int ZoneNum = dataWaterUse.WaterEquipment(WaterEquipNum).Zone;
            dataWaterUse.WaterEquipment(WaterEquipNum).SensibleRateNoMultiplier =
                dataWaterUse.WaterEquipment(WaterEquipNum).SensibleRate /
                (DataHeatBalance::Zone(ZoneNum).Multiplier * DataHeatBalance::Zone(ZoneNum).ListMultiplier);
            dataWaterUse.WaterEquipment(WaterEquipNum).LatentRateNoMultiplier =
                dataWaterUse.WaterEquipment(WaterEquipNum).LatentRate /
                (DataHeatBalance::Zone(ZoneNum).Multiplier * DataHeatBalance::Zone(ZoneNum).ListMultiplier);
        }
    }
} // namespace WaterUse
} // namespace EnergyPlus
