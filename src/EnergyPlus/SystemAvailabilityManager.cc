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
#include <algorithm>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/AirflowNetworkBalanceManager.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

namespace SystemAvailabilityManager {

    // Module containing the System Availability Manager routines

    // MODULE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   August 2001
    //       MODIFIED       February 2004, PGE: Added plant managers.
    //       MODIFIED       March 2007, LG: Added hybrid ventilation control.
    //                      August 2008, R. Raustad - FSEC: added 2 new scheduled sys avail managers
    //                      March 2011, Chandan Sharma - FSEC: Added zone sys avail managers
    //                      August 2013, Xiufeng Pang (XP) - added algorithms for optimal start
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE
    // To encapsulate the data and algorithms required to
    // determine system (loop) availability and "cycle on" status.

    // METHODOLOGY EMPLOYED:
    // Previous time step node data and current zone thermostat setpoints are used
    // in a set of fixed, precoded algorithms to determine the current time step
    // on/off status of systems and loops.

    // USE STATEMENTS:
    // Use statements for data only modules
    using namespace DataHVACGlobals;
    using namespace ScheduleManager;

    void ManageSystemAvailability(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   August 2001
        //       MODIFIED       L. Gu, April, 2007. Added hybrid ventilation control
        //                      Chandan Sharma, March 2011/July 2012 - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manage the simulation of the System Availability Managers

        using DataZoneEquipment::NumValidSysAvailZoneComponents;
        using namespace DataLoopNode;
        using namespace DataAirLoop;
        using namespace DataPlant;

        int PriAirSysNum;         // Primary Air System index
        int PriAirSysAvailMgrNum; // Index of Sys Avail Manager in a Primary Air System
        int PlantNum;             // Plant Loop index
        int PlantAvailMgrNum;     // Index of Plant Avail Manager in a Plant Loop
        int AvailStatus;
        int PreviousStatus;
        int ZoneInSysNum;
        int CtrldZoneNum;
        int HybridVentNum;          // Hybrid ventilation control number
        int ZoneEquipType;          // Type of ZoneHVAC:* component
        int CompNum;                // Index of ZoneHVAC:* component
        int ZoneCompAvailMgrNum;    // Index of availability manager associated with the ZoneHVAC:* component
        int const DummyArgument(1); // This variable is used when SimSysAvailManager is called for a ZoneHVAC:* component

        if (state.dataSystemAvailabilityManager->GetAvailMgrInputFlag) {
            GetSysAvailManagerInputs(state);
            state.dataSystemAvailabilityManager->GetAvailMgrInputFlag = false;
            return;
        }

        InitSysAvailManagers(state);

        for (PriAirSysNum = 1; PriAirSysNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++PriAirSysNum) { // loop over the primary air systems

            PreviousStatus = state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailStatus; // Save the previous status for differential thermostat
            state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailStatus = NoAction;       // initialize the availability to "take no action"

            for (PriAirSysAvailMgrNum = 1; PriAirSysAvailMgrNum <= state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).NumAvailManagers;
                 ++PriAirSysAvailMgrNum) { // loop over the avail managers in system

                SimSysAvailManager(state,
                                   state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailManagerType(PriAirSysAvailMgrNum),
                                   state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailManagerName(PriAirSysAvailMgrNum),
                                   state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailManagerNum(PriAirSysAvailMgrNum),
                                   PriAirSysNum,
                                   PreviousStatus,
                                   AvailStatus);

                if (AvailStatus == ForceOff) {
                    state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailStatus = ForceOff;
                    break; // Fans forced off takes precedence
                } else if (AvailStatus == CycleOnZoneFansOnly) {
                    state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailStatus = CycleOnZoneFansOnly; // zone fans only takes next precedence
                } else if ((AvailStatus == CycleOn) && (state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailStatus == NoAction)) {
                    state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailStatus = CycleOn; // cycle on is lowest precedence
                }

            } // end of availability manager loop

            // Add hybrid ventilation control
            if (state.dataHVACGlobal->NumHybridVentSysAvailMgrs > 0) {
                for (HybridVentNum = 1; HybridVentNum <= state.dataHVACGlobal->NumHybridVentSysAvailMgrs; ++HybridVentNum) {
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum).AirLoopNum == PriAirSysNum &&
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum).VentilationCtrl ==
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Open) {
                        state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailStatus = ForceOff; // Force the system off
                    }
                }
            }

            // loop over the zones served by the system and set the zone equipment availability
            for (ZoneInSysNum = 1; ZoneInSysNum <= state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).NumZonesCooled; ++ZoneInSysNum) {

                CtrldZoneNum = state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).CoolCtrlZoneNums(ZoneInSysNum);
                state.dataZoneEquip->ZoneEquipAvail(CtrldZoneNum) = state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailStatus;
            }

        } // end of primary air system loop

        for (PlantNum = 1; PlantNum <= state.dataHVACGlobal->NumPlantLoops; ++PlantNum) {

            PreviousStatus = state.dataPlnt->PlantAvailMgr(PlantNum).AvailStatus; // Save the previous status for differential thermostat
            state.dataPlnt->PlantAvailMgr(PlantNum).AvailStatus = NoAction;       // Initialize the availability to "take no action"

            for (PlantAvailMgrNum = 1; PlantAvailMgrNum <= state.dataPlnt->PlantAvailMgr(PlantNum).NumAvailManagers;
                 ++PlantAvailMgrNum) { // loop over the avail managers in plant

                SimSysAvailManager(state,
                                   state.dataPlnt->PlantAvailMgr(PlantNum).AvailManagerType(PlantAvailMgrNum),
                                   state.dataPlnt->PlantAvailMgr(PlantNum).AvailManagerName(PlantAvailMgrNum),
                                   state.dataPlnt->PlantAvailMgr(PlantNum).AvailManagerNum(PlantAvailMgrNum),
                                   PlantNum,
                                   PreviousStatus,
                                   AvailStatus);

                if (AvailStatus != NoAction) {
                    state.dataPlnt->PlantAvailMgr(PlantNum).AvailStatus = AvailStatus;
                    break; // First manager to do anything other than "NoAction" gets to set the availability
                }

            } // end of availability manager loop

        } // end of plant loop

        auto &ZoneComp = state.dataHVACGlobal->ZoneComp;
        for (ZoneEquipType = 1; ZoneEquipType <= NumValidSysAvailZoneComponents;
             ++ZoneEquipType) { // loop over the zone equipment types which allow system avail managers
            if (allocated(ZoneComp)) {
                if (ZoneComp(ZoneEquipType).TotalNumComp > 0) {
                    for (CompNum = 1; CompNum <= ZoneComp(ZoneEquipType).TotalNumComp; ++CompNum) {
                        if (allocated(ZoneComp(ZoneEquipType).ZoneCompAvailMgrs)) {
                            if (ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).NumAvailManagers > 0) {
                                // Save the previous status for differential thermostat
                                PreviousStatus = ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailStatus;
                                // initialize the availability to "take no action"
                                ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailStatus = NoAction;
                                for (ZoneCompAvailMgrNum = 1;
                                     ZoneCompAvailMgrNum <= ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).NumAvailManagers;
                                     ++ZoneCompAvailMgrNum) {
                                    // loop over the avail managers in ZoneHVAC:* components
                                    SimSysAvailManager(state,
                                                       ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerType(ZoneCompAvailMgrNum),
                                                       ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerName(ZoneCompAvailMgrNum),
                                                       ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerNum(ZoneCompAvailMgrNum),
                                                       DummyArgument,
                                                       PreviousStatus,
                                                       AvailStatus,
                                                       ZoneEquipType,
                                                       CompNum);
                                    if (AvailStatus == ForceOff) {
                                        ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailStatus = ForceOff;
                                        break; // Fans forced off takes precedence
                                    } else if ((AvailStatus == CycleOn) &&
                                               (ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailStatus == NoAction)) {
                                        // cycle on is next precedence
                                        ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailStatus = CycleOn;
                                    }
                                } // end of availability manager loop
                            }
                        } else {
                            ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailStatus = NoAction;
                        }
                        if (ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).ZoneNum > 0) {
                            if (state.dataHVACGlobal->NumHybridVentSysAvailMgrs > 0) {
                                for (HybridVentNum = 1; HybridVentNum <= state.dataHVACGlobal->NumHybridVentSysAvailMgrs; ++HybridVentNum) {
                                    if (!state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum)
                                             .HybridVentMgrConnectedToAirLoop) {
                                        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum).ActualZoneNum ==
                                            ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).ZoneNum) {
                                            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum).VentilationCtrl ==
                                                state.dataSystemAvailabilityManager->HybridVentCtrl_Open) {
                                                ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailStatus = ForceOff;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } // end of zone equip types
    }

    void GetSysAvailManagerInputs(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   August 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for System Availability Managers and stores it in
        // appropriate data structures.

        // METHODOLOGY EMPLOYED:
        // Uses InputProcessor "Get" routines to obtain data.

        // Using/Aliasing
        using NodeInputManager::GetOnlySingleNode;
        using NodeInputManager::MarkNode;
        using namespace DataLoopNode;
        using DataZoneEquipment::cValidSysAvailManagerCompTypes;
        using DataZoneEquipment::NumValidSysAvailZoneComponents;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetSysAvailManagerInputs: "); // include trailing blank

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string cAlphaFieldNames;
        Array1D_string cNumericFieldNames;
        Array1D_bool lNumericFieldBlanks;
        Array1D_bool lAlphaFieldBlanks;
        Array1D_string cAlphaArgs;
        Array1D<Real64> rNumericArgs;
        std::string cCurrentModuleObject;
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int maxAlphas;           // maximum number of alphas for this set of objects
        int maxNumbers;          // maximum number of numbers for this set of objects
        int numArgs;             // maximum number of arguments for this set of objects
        int IOStatus;            // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int SysAvailNum;         // DO loop index for all System Availability Managers
        int CyclingTimeSteps;
        int ZoneEquipType;
        int TotalNumComp;
        int ZoneListNum;
        int ZoneNumInList;

        // Get the number of occurrences of each type of manager and read in data
        cCurrentModuleObject = "AvailabilityManager:Scheduled";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = NumNumbers;
        maxAlphas = NumAlphas;
        cCurrentModuleObject = "AvailabilityManager:ScheduledOn";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);
        cCurrentModuleObject = "AvailabilityManager:ScheduledOff";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);
        cCurrentModuleObject = "AvailabilityManager:NightCycle";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);
        cCurrentModuleObject = "AvailabilityManager:DifferentialThermostat";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);
        cCurrentModuleObject = "AvailabilityManager:HighTemperatureTurnOff";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);
        cCurrentModuleObject = "AvailabilityManager:HighTemperatureTurnOn";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);
        cCurrentModuleObject = "AvailabilityManager:LowTemperatureTurnOff";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);
        cCurrentModuleObject = "AvailabilityManager:LowTemperatureTurnOn";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);
        cCurrentModuleObject = "AvailabilityManager:NightVentilation";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);
        cCurrentModuleObject = "AvailabilityManager:OptimumStart";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        maxNumbers = max(maxNumbers, NumNumbers);
        maxAlphas = max(maxAlphas, NumAlphas);

        cAlphaFieldNames.allocate(maxAlphas);
        cAlphaArgs.allocate(maxAlphas);
        lAlphaFieldBlanks.dimension(maxAlphas, false);
        cNumericFieldNames.allocate(maxNumbers);
        rNumericArgs.dimension(maxNumbers, 0.0);
        lNumericFieldBlanks.dimension(maxNumbers, false);

        if (!allocated(state.dataHVACGlobal->ZoneComp)) {
            state.dataHVACGlobal->ZoneComp.allocate(NumValidSysAvailZoneComponents);
        }

        for (ZoneEquipType = 1; ZoneEquipType <= NumValidSysAvailZoneComponents; ++ZoneEquipType) {
            if (!allocated(state.dataHVACGlobal->ZoneComp(ZoneEquipType).ZoneCompAvailMgrs)) {
                TotalNumComp = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cValidSysAvailManagerCompTypes(ZoneEquipType));
                state.dataHVACGlobal->ZoneComp(ZoneEquipType).TotalNumComp = TotalNumComp;
                if (TotalNumComp > 0) {
                    state.dataHVACGlobal->ZoneComp(ZoneEquipType).ZoneCompAvailMgrs.allocate(TotalNumComp);
                }
            }
        }

        cCurrentModuleObject = "AvailabilityManager:Scheduled";
        state.dataSystemAvailabilityManager->NumSchedSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumSchedSysAvailMgrs > 0) {

            state.dataSystemAvailabilityManager->SchedSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumSchedSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumSchedSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->SchedSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->SchedSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_Scheduled;

                state.dataSystemAvailabilityManager->SchedSysAvailMgrData(SysAvailNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataSystemAvailabilityManager->SchedSysAvailMgrData(SysAvailNum).SchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }

                SetupOutputVariable(state,
                                    "Availability Manager Scheduled Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->SchedSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->SchedSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cCurrentModuleObject = "AvailabilityManager:ScheduledOn";
        state.dataSystemAvailabilityManager->NumSchedOnSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumSchedOnSysAvailMgrs > 0) {

            state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumSchedOnSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumSchedOnSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_ScheduledOn;

                state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData(SysAvailNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData(SysAvailNum).SchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + " = \"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }

                SetupOutputVariable(state,
                                    "Availability Manager Scheduled On Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cCurrentModuleObject = "AvailabilityManager:ScheduledOff";
        state.dataSystemAvailabilityManager->NumSchedOffSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumSchedOffSysAvailMgrs > 0) {

            state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumSchedOffSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumSchedOffSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_ScheduledOff;

                state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData(SysAvailNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData(SysAvailNum).SchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + " = \"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }

                SetupOutputVariable(state,
                                    "Availability Manager Scheduled Off Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cCurrentModuleObject = "AvailabilityManager:NightCycle";
        state.dataSystemAvailabilityManager->NumNCycSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        CyclingTimeSteps = 0;

        if (state.dataSystemAvailabilityManager->NumNCycSysAvailMgrs > 0) {

            state.dataSystemAvailabilityManager->NCycSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumNCycSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumNCycSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_NightCycle;
                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).TempTolRange = rNumericArgs(1);
                CyclingTimeSteps = nint((rNumericArgs(2) / DataGlobalConstants::SecInHour) * double(state.dataGlobal->NumOfTimeStepInHour));
                CyclingTimeSteps = max(1, CyclingTimeSteps);
                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CyclingTimeSteps = CyclingTimeSteps;
                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).SchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + " = \"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }
                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).FanSched = cAlphaArgs(3);
                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).FanSchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).FanSchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + " = \"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                    ErrorsFound = true;
                }

                {
                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(cAlphaArgs(4)));
                    if (SELECT_CASE_var == "STAYOFF") {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType = state.dataSystemAvailabilityManager->StayOff;
                    } else if (SELECT_CASE_var == "CYCLEONANY") {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->CycleOnAny;
                    } else if (SELECT_CASE_var == "CYCLEONCONTROLZONE") {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->CycleOnControlZone;
                    } else if (SELECT_CASE_var == "CYCLEONANYZONEFANSONLY") {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->ZoneFansOnly;
                    } else if (SELECT_CASE_var == "CYCLEONANYCOOLINGORHEATINGZONE") {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->CycleOnAnyCoolingOrHeatingZone;
                    } else if (SELECT_CASE_var == "CYCLEONANYCOOLINGZONE") {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->CycleOnAnyCoolingZone;
                    } else if (SELECT_CASE_var == "CYCLEONANYHEATINGZONE") {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->CycleOnAnyHeatingZone;
                    } else if (SELECT_CASE_var == "CYCLEONANYHEATINGZONEFANSONLY") {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->CycleOnAnyHeatingZoneFansOnly;
                    } else {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                        ShowSevereError(state, RoutineName + "incorrect value: " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                        ErrorsFound = true;
                    }
                }

                // Cycling Run Time Control Type
                if (!lAlphaFieldBlanks(5)) {
                    {
                        auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(cAlphaArgs(5)));
                        if (SELECT_CASE_var == "FIXEDRUNTIME") {
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CycRunTimeCntrlType =
                                state.dataSystemAvailabilityManager->FixedRunTime;
                        } else if (SELECT_CASE_var == "THERMOSTAT") {
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CycRunTimeCntrlType =
                                state.dataSystemAvailabilityManager->Thermostat;
                        } else if (SELECT_CASE_var == "THERMOSTATWITHMINIMUMRUNTIME") {
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CycRunTimeCntrlType =
                                state.dataSystemAvailabilityManager->ThermostatWithMinimumRunTime;
                        } else {
                            ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                            ShowSevereError(state, RoutineName + "incorrect value: " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                            ErrorsFound = true;
                        }
                    }
                }

                // Control zone or zonelist
                if (!lAlphaFieldBlanks(6)) {
                    state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZoneListName = cAlphaArgs(6);
                    int ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(6), state.dataHeatBal->Zone);
                    if (ZoneNum > 0) {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfCtrlZones = 1;
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZonePtrs.allocate(1);
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZonePtrs(1) = ZoneNum;
                    } else {
                        int ZoneListNum = 0;
                        if (state.dataHeatBal->NumOfZoneLists > 0)
                            ZoneListNum = UtilityRoutines::FindItemInList(cAlphaArgs(6), state.dataHeatBal->ZoneList);
                        if (ZoneListNum > 0) {
                            int NumZones = state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones;
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfCtrlZones = NumZones;
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZonePtrs.allocate(NumZones);
                            for (int ZoneNumInList = 1; ZoneNumInList <= NumZones; ++ZoneNumInList) {
                                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZonePtrs(ZoneNumInList) =
                                    state.dataHeatBal->ZoneList(ZoneListNum).Zone(ZoneNumInList);
                            }
                        } else {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(6) + "=\"" +
                                                cAlphaArgs(6) + "\" not found.");
                            ErrorsFound = true;
                        }
                    }
                }

                // Cooling zone or zonelist
                if (!lAlphaFieldBlanks(7)) {
                    state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CoolingZoneListName = cAlphaArgs(7);
                    int ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(7), state.dataHeatBal->Zone);
                    if (ZoneNum > 0) {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfCoolingZones = 1;
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CoolingZonePtrs.allocate(1);
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CoolingZonePtrs(1) = ZoneNum;
                    } else {
                        int ZoneListNum = 0;
                        if (state.dataHeatBal->NumOfZoneLists > 0)
                            ZoneListNum = UtilityRoutines::FindItemInList(cAlphaArgs(7), state.dataHeatBal->ZoneList);
                        if (ZoneListNum > 0) {
                            int NumZones = state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones;
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfCoolingZones = NumZones;
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CoolingZonePtrs.allocate(NumZones);
                            for (int ZoneNumInList = 1; ZoneNumInList <= NumZones; ++ZoneNumInList) {
                                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CoolingZonePtrs(ZoneNumInList) =
                                    state.dataHeatBal->ZoneList(ZoneListNum).Zone(ZoneNumInList);
                            }
                        } else {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(7) + "=\"" +
                                                cAlphaArgs(7) + "\" not found.");
                            ErrorsFound = true;
                        }
                    }
                }

                // Heating zone or zonelist
                if (!lAlphaFieldBlanks(8)) {
                    state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatingZoneListName = cAlphaArgs(8);
                    int ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(8), state.dataHeatBal->Zone);
                    if (ZoneNum > 0) {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfHeatingZones = 1;
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatingZonePtrs.allocate(1);
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatingZonePtrs(1) = ZoneNum;
                    } else {
                        int ZoneListNum = 0;
                        if (state.dataHeatBal->NumOfZoneLists > 0)
                            ZoneListNum = UtilityRoutines::FindItemInList(cAlphaArgs(8), state.dataHeatBal->ZoneList);
                        if (ZoneListNum > 0) {
                            int NumZones = state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones;
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfHeatingZones = NumZones;
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatingZonePtrs.allocate(NumZones);
                            for (int ZoneNumInList = 1; ZoneNumInList <= NumZones; ++ZoneNumInList) {
                                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatingZonePtrs(ZoneNumInList) =
                                    state.dataHeatBal->ZoneList(ZoneListNum).Zone(ZoneNumInList);
                            }
                        } else {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(8) + "=\"" +
                                                cAlphaArgs(8) + "\" not found.");
                            ErrorsFound = true;
                        }
                    }
                }

                // HeatZnFan zone or zonelist
                if (!lAlphaFieldBlanks(9)) {
                    state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatZnFanZoneListName = cAlphaArgs(9);
                    int ZoneNum = UtilityRoutines::FindItemInList(cAlphaArgs(9), state.dataHeatBal->Zone);
                    if (ZoneNum > 0) {
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfHeatZnFanZones = 1;
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatZnFanZonePtrs.allocate(1);
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatZnFanZonePtrs(1) = ZoneNum;
                    } else {
                        int ZoneListNum = 0;
                        if (state.dataHeatBal->NumOfZoneLists > 0)
                            ZoneListNum = UtilityRoutines::FindItemInList(cAlphaArgs(9), state.dataHeatBal->ZoneList);
                        if (ZoneListNum > 0) {
                            int NumZones = state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones;
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfHeatZnFanZones = NumZones;
                            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatZnFanZonePtrs.allocate(NumZones);
                            for (int ZoneNumInList = 1; ZoneNumInList <= NumZones; ++ZoneNumInList) {
                                state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatZnFanZonePtrs(ZoneNumInList) =
                                    state.dataHeatBal->ZoneList(ZoneListNum).Zone(ZoneNumInList);
                            }
                        } else {
                            ShowSevereError(state,
                                            RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\" invalid " + cAlphaFieldNames(9) + "=\"" +
                                                cAlphaArgs(9) + "\" not found.");
                            ErrorsFound = true;
                        }
                    }
                }

                SetupOutputVariable(state,
                                    "Availability Manager Night Cycle Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cCurrentModuleObject = "AvailabilityManager:OptimumStart";
        state.dataSystemAvailabilityManager->NumOptStartSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);
        CyclingTimeSteps = 0;

        if (state.dataSystemAvailabilityManager->NumOptStartSysAvailMgrs > 0) {
            // Array size of variable type OptStartSysAvailMgrData is updated
            state.dataSystemAvailabilityManager->OptStartSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumOptStartSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumOptStartSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_OptimumStart;
                state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).SchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + " = \"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }
                state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).FanSched = cAlphaArgs(3);
                state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).FanSchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).FanSchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + " = \"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                    ErrorsFound = true;
                }

                state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).MaxOptStartTime = rNumericArgs(1);

                {
                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(cAlphaArgs(4)));
                    if (SELECT_CASE_var == "STAYOFF") {
                        state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->StayOff;
                    } else if (SELECT_CASE_var == "CONTROLZONE") {
                        state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->ControlZone;
                    } else if (SELECT_CASE_var == "MAXIMUMOFZONELIST") {
                        state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->MaximumOfZoneList;
                    } else {
                        state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlType =
                            state.dataSystemAvailabilityManager->ControlZone;
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                        ShowSevereError(state, RoutineName + "incorrect value: " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                        ErrorsFound = true;
                    }
                }

                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlType ==
                    state.dataSystemAvailabilityManager->ControlZone) {
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlZoneName = cAlphaArgs(5);
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ZoneNum =
                        UtilityRoutines::FindItemInList(cAlphaArgs(5), state.dataHeatBal->Zone);
                    if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ZoneNum == 0) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                        ShowSevereError(state, "not found: " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                        ErrorsFound = true;
                    }
                }

                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlType ==
                    state.dataSystemAvailabilityManager->MaximumOfZoneList) {
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ZoneListName = cAlphaArgs(6);
                    for (ZoneListNum = 1; ZoneListNum <= state.dataHeatBal->NumOfZoneLists; ++ZoneListNum) {
                        if (state.dataHeatBal->ZoneList(ZoneListNum).Name == cAlphaArgs(6)) {
                            state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).NumOfZones =
                                state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones;
                            state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum)
                                .ZonePtrs.allocate(state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones);
                            for (ZoneNumInList = 1; ZoneNumInList <= state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones; ++ZoneNumInList) {
                                state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ZonePtrs(ZoneNumInList) =
                                    state.dataHeatBal->ZoneList(ZoneListNum).Zone(ZoneNumInList);
                            }
                        }
                    }
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).NumOfZones =
                        UtilityRoutines::FindItemInList(cAlphaArgs(6), state.dataHeatBal->ZoneList);
                    if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).NumOfZones == 0) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                        ShowSevereError(state, "not found: " + cAlphaFieldNames(6) + "=\"" + cAlphaArgs(6) + "\".");
                        ErrorsFound = true;
                    }
                }

                {
                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(cAlphaArgs(7)));
                    if (SELECT_CASE_var == "CONSTANTTEMPERATUREGRADIENT") {
                        state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType =
                            state.dataSystemAvailabilityManager->ConstantTemperatureGradient;
                    } else if (SELECT_CASE_var == "ADAPTIVETEMPERATUREGRADIENT") {
                        state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType =
                            state.dataSystemAvailabilityManager->AdaptiveTemperatureGradient;
                    } else if (SELECT_CASE_var == "ADAPTIVEASHRAE") {
                        state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType =
                            state.dataSystemAvailabilityManager->AdaptiveASHRAE;
                    } else if (SELECT_CASE_var == "CONSTANTSTARTTIME") {
                        state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType =
                            state.dataSystemAvailabilityManager->ConstantStartTime;
                    } else {
                        state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType =
                            state.dataSystemAvailabilityManager->AdaptiveASHRAE;
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                        ShowSevereError(state, RoutineName + "incorrect value: " + cAlphaFieldNames(7) + "=\"" + cAlphaArgs(7) + "\".");
                        ErrorsFound = true;
                    }
                }

                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType ==
                    state.dataSystemAvailabilityManager->ConstantTemperatureGradient) {
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ConstTGradCool = rNumericArgs(2);
                }

                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType ==
                    state.dataSystemAvailabilityManager->ConstantTemperatureGradient) {
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ConstTGradHeat = rNumericArgs(3);
                }

                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType ==
                    state.dataSystemAvailabilityManager->AdaptiveTemperatureGradient) {
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).InitTGradCool = rNumericArgs(4);
                }

                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType ==
                    state.dataSystemAvailabilityManager->AdaptiveTemperatureGradient) {
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).InitTGradHeat = rNumericArgs(5);
                }

                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType ==
                    state.dataSystemAvailabilityManager->ConstantStartTime) {
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ConstStartTime = rNumericArgs(6);
                }

                if (state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlAlgType ==
                    state.dataSystemAvailabilityManager->AdaptiveTemperatureGradient) {
                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).NumPreDays = rNumericArgs(7);
                }

                SetupOutputVariable(state,
                                    "Availability Manager Optimum Start Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).Name);

                // add
                SetupOutputVariable(state,
                                    "Availability Manager Optimum Start Time Before Occupancy",
                                    OutputProcessor::Unit::hr,
                                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).NumHoursBeforeOccupancy,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).Name,
                                    "Daily");
            }
        }

        cCurrentModuleObject = "AvailabilityManager:DifferentialThermostat";
        state.dataSystemAvailabilityManager->NumDiffTSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumDiffTSysAvailMgrs > 0) {

            state.dataSystemAvailabilityManager->DiffTSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumDiffTSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumDiffTSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_DiffThermo;

                state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).HotNode =
                    GetOnlySingleNode(state,
                                      cAlphaArgs(2),
                                      ErrorsFound,
                                      cCurrentModuleObject,
                                      cAlphaArgs(1),
                                      DataLoopNode::NodeFluidType::blank,
                                      DataLoopNode::NodeConnectionType::Sensor,
                                      NodeInputManager::compFluidStream::Primary,
                                      ObjectIsNotParent);
                MarkNode(state,
                         state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).HotNode,
                         cCurrentModuleObject,
                         cAlphaArgs(1),
                         "Hot Node");
                state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).ColdNode =
                    GetOnlySingleNode(state,
                                      cAlphaArgs(3),
                                      ErrorsFound,
                                      cCurrentModuleObject,
                                      cAlphaArgs(1),
                                      DataLoopNode::NodeFluidType::blank,
                                      DataLoopNode::NodeConnectionType::Sensor,
                                      NodeInputManager::compFluidStream::Primary,
                                      ObjectIsNotParent);
                MarkNode(state,
                         state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).ColdNode,
                         cCurrentModuleObject,
                         cAlphaArgs(1),
                         "Cold Node");

                state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).TempDiffOn = rNumericArgs(1);

                if (NumNumbers > 1) {
                    state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).TempDiffOff = rNumericArgs(2);
                } else {
                    state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).TempDiffOff =
                        state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).TempDiffOn;
                }

                if (state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).TempDiffOff >
                    state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).TempDiffOn) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + " = \"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "The " + cNumericFieldNames(2) + " is greater than the " + cNumericFieldNames(1) + '.');
                    ErrorsFound = true;
                }

                SetupOutputVariable(state,
                                    "Availability Manager Differential Thermostat Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cCurrentModuleObject = "AvailabilityManager:HighTemperatureTurnOff";
        state.dataSystemAvailabilityManager->NumHiTurnOffSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumHiTurnOffSysAvailMgrs > 0) {
            state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumHiTurnOffSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumHiTurnOffSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_HiTempTOff;

                state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).Node =
                    GetOnlySingleNode(state,
                                      cAlphaArgs(2),
                                      ErrorsFound,
                                      cCurrentModuleObject,
                                      cAlphaArgs(1),
                                      DataLoopNode::NodeFluidType::blank,
                                      DataLoopNode::NodeConnectionType::Sensor,
                                      NodeInputManager::compFluidStream::Primary,
                                      ObjectIsNotParent);
                MarkNode(state,
                         state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).Node,
                         cCurrentModuleObject,
                         cAlphaArgs(1),
                         "Sensor Node");

                state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).Temp = rNumericArgs(1);

                SetupOutputVariable(state,
                                    "Availability Manager High Temperature Turn Off Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cCurrentModuleObject = "AvailabilityManager:HighTemperatureTurnOn";
        state.dataSystemAvailabilityManager->NumHiTurnOnSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumHiTurnOnSysAvailMgrs > 0) {

            state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumHiTurnOnSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumHiTurnOnSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_HiTempTOn;

                state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).Node =
                    GetOnlySingleNode(state,
                                      cAlphaArgs(2),
                                      ErrorsFound,
                                      cCurrentModuleObject,
                                      cAlphaArgs(1),
                                      DataLoopNode::NodeFluidType::blank,
                                      DataLoopNode::NodeConnectionType::Sensor,
                                      NodeInputManager::compFluidStream::Primary,
                                      ObjectIsNotParent);
                MarkNode(state,
                         state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).Node,
                         cCurrentModuleObject,
                         cAlphaArgs(1),
                         "Sensor Node");

                state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).Temp = rNumericArgs(1);

                SetupOutputVariable(state,
                                    "Availability Manager High Temperature Turn On Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cCurrentModuleObject = "AvailabilityManager:LowTemperatureTurnOff";
        state.dataSystemAvailabilityManager->NumLoTurnOffSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumLoTurnOffSysAvailMgrs > 0) {

            state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumLoTurnOffSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumLoTurnOffSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_LoTempTOff;

                state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).Node =
                    GetOnlySingleNode(state,
                                      cAlphaArgs(2),
                                      ErrorsFound,
                                      cCurrentModuleObject,
                                      cAlphaArgs(1),
                                      DataLoopNode::NodeFluidType::blank,
                                      DataLoopNode::NodeConnectionType::Sensor,
                                      NodeInputManager::compFluidStream::Primary,
                                      ObjectIsNotParent);
                MarkNode(state,
                         state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).Node,
                         cCurrentModuleObject,
                         cAlphaArgs(1),
                         "Sensor Node");

                state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).Temp = rNumericArgs(1);

                if (!lAlphaFieldBlanks(3)) {
                    state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                    if (state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).SchedPtr == 0) {
                        ShowSevereError(state, RoutineName + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\" not found.");
                        ShowContinueError(state, "Occurs in " + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\".");
                        ErrorsFound = true;
                    }
                } else {
                    state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).SchedPtr = 0;
                }

                SetupOutputVariable(state,
                                    "Availability Manager Low Temperature Turn Off Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cCurrentModuleObject = "AvailabilityManager:LowTemperatureTurnOn";
        state.dataSystemAvailabilityManager->NumLoTurnOnSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumLoTurnOnSysAvailMgrs > 0) {

            state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumLoTurnOnSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumLoTurnOnSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_LoTempTOn;

                state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).Node =
                    GetOnlySingleNode(state,
                                      cAlphaArgs(2),
                                      ErrorsFound,
                                      cCurrentModuleObject,
                                      cAlphaArgs(1),
                                      DataLoopNode::NodeFluidType::blank,
                                      DataLoopNode::NodeConnectionType::Sensor,
                                      NodeInputManager::compFluidStream::Primary,
                                      ObjectIsNotParent);
                MarkNode(state,
                         state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).Node,
                         cCurrentModuleObject,
                         cAlphaArgs(1),
                         "Sensor Node");

                state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).Temp = rNumericArgs(1);

                SetupOutputVariable(state,
                                    "Availability Manager Low Temperature Turn On Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cCurrentModuleObject = "AvailabilityManager:NightVentilation";
        state.dataSystemAvailabilityManager->NumNVentSysAvailMgrs =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumNVentSysAvailMgrs > 0) {

            state.dataSystemAvailabilityManager->NVentSysAvailMgrData.allocate(state.dataSystemAvailabilityManager->NumNVentSysAvailMgrs);

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumNVentSysAvailMgrs; ++SysAvailNum) {

                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         SysAvailNum,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).Name = cAlphaArgs(1);
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).MgrType =
                    state.dataSystemAvailabilityManager->SysAvailMgr_NightVent;

                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).SchedPtr = GetScheduleIndex(state, cAlphaArgs(2));
                if (state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).SchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(2) + "=\"" + cAlphaArgs(2) + "\".");
                    ErrorsFound = true;
                }
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).FanSched = cAlphaArgs(3);
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).FanSchedPtr = GetScheduleIndex(state, cAlphaArgs(3));
                if (state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).FanSchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(3) + "=\"" + cAlphaArgs(3) + "\".");
                    ErrorsFound = true;
                }
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentTempSched = cAlphaArgs(4);
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentTempSchedPtr = GetScheduleIndex(state, cAlphaArgs(4));
                if (state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentTempSchedPtr == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(4) + "=\"" + cAlphaArgs(4) + "\".");
                    ErrorsFound = true;
                }
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentDelT = rNumericArgs(1);
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentTempLowLim = rNumericArgs(2);
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentFlowFrac = rNumericArgs(3);
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).CtrlZoneName = cAlphaArgs(5);
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).ZoneNum =
                    UtilityRoutines::FindItemInList(cAlphaArgs(5), state.dataHeatBal->Zone);
                if (state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).ZoneNum == 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs(1) + "\", invalid");
                    ShowContinueError(state, "not found: " + cAlphaFieldNames(5) + "=\"" + cAlphaArgs(5) + "\".");
                    ErrorsFound = true;
                }

                SetupOutputVariable(state,
                                    "Availability Manager Night Ventilation Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).AvailStatus,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).Name);

            } // SysAvailNum
        }

        cAlphaFieldNames.deallocate();
        cAlphaArgs.deallocate();
        lAlphaFieldBlanks.deallocate();
        cNumericFieldNames.deallocate();
        rNumericArgs.deallocate();
        lNumericFieldBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, RoutineName + "Errors found in input.  Preceding condition(s) cause termination.");
        }
    }

    void GetSysAvailManagerListInputs(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   August 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine gets the System Availability Manager List object input and stores
        // it for later retrieval of items from the Plant and Air Loops.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        Array1D_string cAlphaFieldNames;
        Array1D_string cNumericFieldNames;
        Array1D_bool lNumericFieldBlanks;
        Array1D_bool lAlphaFieldBlanks;
        Array1D_string cAlphaArgs;
        Array1D<Real64> rNumericArgs;
        std::string cCurrentModuleObject;
        int NumAlphas;
        int NumNumbers;
        int numArgs;
        int Item;
        int IOStatus;
        bool ErrorsFound;
        int list;
        int itemnum;

        if (state.dataSystemAvailabilityManager->GetAvailMgrInputFlag) {
            GetSysAvailManagerInputs(state);
            state.dataSystemAvailabilityManager->GetAvailMgrInputFlag = false;
        }

        ErrorsFound = false;

        cCurrentModuleObject = "AvailabilityManagerAssignmentList";
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, cCurrentModuleObject, numArgs, NumAlphas, NumNumbers);
        cAlphaFieldNames.allocate(NumAlphas);
        cAlphaArgs.allocate(NumAlphas);
        lAlphaFieldBlanks.dimension(NumAlphas, false);
        cNumericFieldNames.allocate(NumNumbers);
        rNumericArgs.dimension(NumNumbers, 0.0);
        lNumericFieldBlanks.dimension(NumNumbers, false);

        cCurrentModuleObject = "AvailabilityManagerAssignmentList";
        state.dataSystemAvailabilityManager->NumAvailManagerLists =
            state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (state.dataSystemAvailabilityManager->NumAvailManagerLists > 0) {

            state.dataSystemAvailabilityManager->SysAvailMgrListData.allocate(state.dataSystemAvailabilityManager->NumAvailManagerLists);

            for (Item = 1; Item <= state.dataSystemAvailabilityManager->NumAvailManagerLists; ++Item) {
                state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                         cCurrentModuleObject,
                                                                         Item,
                                                                         cAlphaArgs,
                                                                         NumAlphas,
                                                                         rNumericArgs,
                                                                         NumNumbers,
                                                                         IOStatus,
                                                                         lNumericFieldBlanks,
                                                                         lAlphaFieldBlanks,
                                                                         cAlphaFieldNames,
                                                                         cNumericFieldNames);
                UtilityRoutines::IsNameEmpty(state, cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
                state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).Name = cAlphaArgs(1);

                state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).NumItems = (NumAlphas - 1) / 2; // Subtract off the list name first
                state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).AvailManagerName.allocate(
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).NumItems);
                state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).AvailManagerName = "";
                state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).cAvailManagerType.allocate(
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).NumItems);
                state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).cAvailManagerType = "";
                state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).AvailManagerType.allocate(
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).NumItems);
                state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).AvailManagerType = 0;

                // retrieve data

                itemnum = 1;
                for (list = 1; list <= state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).NumItems; ++list) {
                    ++itemnum;
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).cAvailManagerType(list) = cAlphaArgs(itemnum);
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).AvailManagerType(list) =
                        ValidateAndSetSysAvailabilityManagerType(state, cAlphaArgs(itemnum));
                    // these are validated individually in the GetPlant, GetSystem and GetZoneEq lists
                    ++itemnum;
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Item).AvailManagerName(list) = cAlphaArgs(itemnum);
                } // End of retrieving items
            }
        }

        cAlphaFieldNames.deallocate();
        cAlphaArgs.deallocate();
        lAlphaFieldBlanks.deallocate();
        cNumericFieldNames.deallocate();
        rNumericArgs.deallocate();
        lNumericFieldBlanks.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, "GetSysAvailManagerListInputs: Program terminates due to preceding conditions.");
        }
    }

    void GetPlantAvailabilityManager(EnergyPlusData &state,
                                     std::string const &AvailabilityListName, // name that should be an Availability Manager List Name
                                     int const Loop,                          // which loop this is
                                     int const NumPlantLoops,                 // Total number of plant loops
                                     bool &ErrorsFound                        // true if certain errors are detected here
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   August 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the plant availability manager data for the indicated
        // loop.  If the PlantAvailMgr structure has not been allocated, it will be allocated
        // to "number of plant loops".

        using namespace DataPlant;

        int Found;
        int Num;

        if (state.dataSystemAvailabilityManager->GetAvailListsInput) {
            GetSysAvailManagerListInputs(state);
            state.dataSystemAvailabilityManager->GetAvailListsInput = false;
        }

        if (!allocated(state.dataPlnt->PlantAvailMgr)) {
            state.dataPlnt->PlantAvailMgr.allocate(NumPlantLoops);
        }

        Found = 0;
        if (state.dataSystemAvailabilityManager->NumAvailManagerLists > 0)
            Found = UtilityRoutines::FindItemInList(AvailabilityListName, state.dataSystemAvailabilityManager->SysAvailMgrListData);

        if (Found != 0) {
            state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers = state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).NumItems;
            state.dataPlnt->PlantAvailMgr(Loop).AvailStatus = NoAction;
            state.dataPlnt->PlantAvailMgr(Loop).StartTime = 0;
            state.dataPlnt->PlantAvailMgr(Loop).StopTime = 0;
            state.dataPlnt->PlantAvailMgr(Loop).AvailManagerName.allocate(state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers);
            state.dataPlnt->PlantAvailMgr(Loop).AvailManagerType.allocate(state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers);
            state.dataPlnt->PlantAvailMgr(Loop).AvailManagerNum.allocate(state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers);
            for (Num = 1; Num <= state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers; ++Num) {
                state.dataPlnt->PlantAvailMgr(Loop).AvailManagerName(Num) =
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num);
                state.dataPlnt->PlantAvailMgr(Loop).AvailManagerNum(Num) = 0;
                state.dataPlnt->PlantAvailMgr(Loop).AvailManagerType(Num) =
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerType(Num);
                if (state.dataPlnt->PlantAvailMgr(Loop).AvailManagerType(Num) == 0) {
                    ShowSevereError(state,
                                    "GetPlantLoopData/GetPlantAvailabilityManager: Invalid System Availability Manager Type entered=\"" +
                                        state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).cAvailManagerType(Num) + "\".");
                    ShowContinueError(state, "Occurs in AvailabilityManagerAssignmentList=\"" + AvailabilityListName + "\".");
                    ErrorsFound = true;
                }
                if (state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerType(Num) ==
                        state.dataSystemAvailabilityManager->SysAvailMgr_DiffThermo &&
                    Num != state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers) {
                    ShowWarningError(state,
                                     "GetPlantLoopData/GetPlantAvailabilityManager: AvailabilityManager:DifferentialThermostat=\"" +
                                         state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num) + "\".");
                    ShowContinueError(
                        state, "...is not the last manager on the AvailabilityManagerAssignmentList.  Any remaining managers will not be used.");
                    ShowContinueError(state, "Occurs in AvailabilityManagerAssignmentList =\"" + AvailabilityListName + "\".");
                }
                if (state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerType(Num) ==
                        state.dataSystemAvailabilityManager->SysAvailMgr_NightVent ||
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerType(Num) ==
                        state.dataSystemAvailabilityManager->SysAvailMgr_NightCycle) {
                    ShowSevereError(state,
                                    "GetPlantLoopData/GetPlantAvailabilityManager: Invalid System Availability Manager Type entered=\"" +
                                        state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).cAvailManagerType(Num) + "\".");
                    ShowContinueError(state, "...this manager is not used in a Plant Loop.");
                    ShowContinueError(state, "Occurs in AvailabilityManagerAssignmentList=\"" + AvailabilityListName + "\".");
                    ErrorsFound = true;
                }
            } // End of Num Loop

        } else {
            if (AvailabilityListName != "") {
                ShowWarningError(state,
                                 "GetPlantLoopData/GetPlantAvailabilityManager: AvailabilityManagerAssignmentList=" + AvailabilityListName +
                                     " not found in lists.  No availability will be used.");
            }
            state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers = 0;
            state.dataPlnt->PlantAvailMgr(Loop).AvailStatus = NoAction;
            state.dataPlnt->PlantAvailMgr(Loop).AvailManagerName.allocate(state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers);
            state.dataPlnt->PlantAvailMgr(Loop).AvailManagerType.allocate(state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers);
            state.dataPlnt->PlantAvailMgr(Loop).AvailManagerNum.allocate(state.dataPlnt->PlantAvailMgr(Loop).NumAvailManagers);
        }
    }

    void GetAirLoopAvailabilityManager(EnergyPlusData &state,
                                       std::string const &AvailabilityListName, // name that should be an Availability Manager List Name
                                       int const Loop,                          // which loop this is
                                       int const NumAirLoops,                   // Total number of air loops
                                       bool &ErrorsFound                        // true if certain errors are detected here
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   August 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the availability manager data for the indicated air
        // loop or for the indicated type of zone equipment component.
        // If the PriAirSysAvailMgr structure has not been allocated, it will be allocated
        // to "number of air loops".

        using namespace DataAirLoop;

        int Found;
        int Num;
        //  INTEGER :: CompNumAvailManagers ! Number of availability managers associated with a ZoneHVAC:* component

        if (state.dataSystemAvailabilityManager->GetAvailListsInput) {
            GetSysAvailManagerListInputs(state);
            state.dataSystemAvailabilityManager->GetAvailListsInput = false;
        }

        if (!allocated(state.dataAirLoop->PriAirSysAvailMgr)) {
            state.dataAirLoop->PriAirSysAvailMgr.allocate(NumAirLoops);
        }

        Found = 0;
        if (state.dataSystemAvailabilityManager->NumAvailManagerLists > 0)
            Found = UtilityRoutines::FindItemInList(AvailabilityListName, state.dataSystemAvailabilityManager->SysAvailMgrListData);

        if (Found != 0) {
            state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers = state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).NumItems;
            state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailStatus = NoAction;
            state.dataAirLoop->PriAirSysAvailMgr(Loop).StartTime = 0;
            state.dataAirLoop->PriAirSysAvailMgr(Loop).StopTime = 0;
            state.dataAirLoop->PriAirSysAvailMgr(Loop).ReqSupplyFrac = 1.0;
            state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerName.allocate(state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers);
            state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerType.allocate(state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers);
            state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerNum.allocate(state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers);
            for (Num = 1; Num <= state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers; ++Num) {
                state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerName(Num) =
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num);
                state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerNum(Num) = 0;
                state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerType(Num) =
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerType(Num);
                if (state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerType(Num) == 0) {
                    ShowSevereError(state,
                                    "GetAirPathData/GetAirLoopAvailabilityManager: Invalid AvailabilityManagerAssignmentList Type entered=\"" +
                                        state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).cAvailManagerType(Num) + "\".");
                    ShowContinueError(state,
                                      "Occurs in AvailabilityManagerAssignmentList=\"" +
                                          state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num) + "\".");
                    ErrorsFound = true;
                }
                if (state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerType(Num) ==
                        state.dataSystemAvailabilityManager->SysAvailMgr_DiffThermo &&
                    Num != state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers) {
                    ShowWarningError(state,
                                     "GetAirPathData/GetAirLoopAvailabilityManager: AvailabilityManager:DifferentialThermostat=\"" +
                                         state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num) + "\".");
                    ShowContinueError(
                        state, "...is not the last manager on the AvailabilityManagerAssignmentList.  Any remaining managers will not be used.");
                    ShowContinueError(state,
                                      "Occurs in AvailabilityManagerAssignmentList=\"" +
                                          state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num) + "\".");
                }
            } // End of Num Loop

        } else {
            if (AvailabilityListName != "") {
                ShowWarningError(state,
                                 "GetAirPathData/GetAirLoopAvailabilityManager: AvailabilityManagerAssignmentList=" + AvailabilityListName +
                                     " not found in lists.  No availability will be used.");
            }
            state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers = 0;
            state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailStatus = NoAction;
            state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerName.allocate(state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers);
            state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerType.allocate(state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers);
            state.dataAirLoop->PriAirSysAvailMgr(Loop).AvailManagerNum.allocate(state.dataAirLoop->PriAirSysAvailMgr(Loop).NumAvailManagers);
        }
    }

    void GetZoneEqAvailabilityManager(EnergyPlusData &state,
                                      int const ZoneEquipType, // Type of ZoneHVAC:* component
                                      int const CompNum,       // Index of a particular ZoneHVAC:* component
                                      bool &ErrorsFound        // true if certain errors are detected here
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2011
        //       MODIFIED       Chandan Sharma, March 2011/July 2012 - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine gets the availability manager data for the indicated type of zone
        // equipment component.
        // If not allocated, ZoneComp structure will be allocated to "Total num of zone equip types" and
        // ZoneCompAvailMgrs structure will be allocated to "Total number of components of the indicated type".

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        std::string AvailabilityListName; // name that should be an Availability Manager List Name
        int Found;
        int Num;
        int CompNumAvailManagers; // Number of availability managers associated with a ZoneHVAC:* component

        auto &ZoneComp = state.dataHVACGlobal->ZoneComp;

        if (state.dataSystemAvailabilityManager->GetAvailListsInput) {
            GetSysAvailManagerListInputs(state);
            state.dataSystemAvailabilityManager->GetAvailListsInput = false;
        }

        if (ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).Input) { // when both air loop and zone eq avail managers are present, zone avail mngrs
                                                                        // list name has not been read in first time through here (see end of if
                                                                        // block)
            AvailabilityListName = ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerListName;
            Found = 0;
            if (state.dataSystemAvailabilityManager->NumAvailManagerLists > 0)
                Found = UtilityRoutines::FindItemInList(AvailabilityListName, state.dataSystemAvailabilityManager->SysAvailMgrListData);
            if (Found != 0) {
                ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).NumAvailManagers =
                    state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).NumItems;
                CompNumAvailManagers = ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).NumAvailManagers;
                ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailStatus = NoAction;
                ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StartTime = 0;
                ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StopTime = 0;
                if (!allocated(ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerName)) {
                    ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerName.allocate(CompNumAvailManagers);
                    ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerType.allocate(CompNumAvailManagers);
                    ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerNum.allocate(CompNumAvailManagers);
                }
                for (Num = 1; Num <= ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).NumAvailManagers; ++Num) {
                    ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerName(Num) =
                        state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num);
                    ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerNum(Num) = 0;
                    ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerType(Num) =
                        state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerType(Num);
                    if (ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).AvailManagerType(Num) == 0) {
                        ShowSevereError(state,
                                        "GetZoneEqAvailabilityManager: Invalid AvailabilityManagerAssignmentList Type entered=\"" +
                                            state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).cAvailManagerType(Num) + "\".");
                        ShowContinueError(state,
                                          "Occurs in AvailabilityManagerAssignmentList=\"" +
                                              state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num) + "\".");
                        ErrorsFound = true;
                    }
                    if (state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerType(Num) ==
                            state.dataSystemAvailabilityManager->SysAvailMgr_DiffThermo &&
                        Num != ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).NumAvailManagers) {
                        ShowWarningError(state,
                                         "GetZoneEqAvailabilityManager: AvailabilityManager:DifferentialThermostat=\"" +
                                             state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num) + "\".");
                        ShowContinueError(
                            state, "...is not the last manager on the AvailabilityManagerAssignmentList.  Any remaining managers will not be used.");
                        ShowContinueError(state,
                                          "Occurs in AvailabilityManagerAssignmentList=\"" +
                                              state.dataSystemAvailabilityManager->SysAvailMgrListData(Found).AvailManagerName(Num) + "\".");
                    }
                } // End of Num Loop
            }
            if (ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).Count > 0 || Found > 0)
                ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).Input = false;
            ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).Count += 1;
        }
    }

    void InitSysAvailManagers(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   August 2001
        //       MODIFIED       Brent Griffith, CR8376 initialize to NoAction every timestep
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the System Availability Manager objects.

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        using DataZoneEquipment::NumValidSysAvailZoneComponents;

        int SysAvailNum; // DO loop indes for Sys Avail Manager objects
        int ZoneEquipType;
        int ZoneListNum;
        int ScanZoneListNum;
        int ZoneNum;
        // One time initializations

        if (state.dataSystemAvailabilityManager->InitSysAvailManagers_MyOneTimeFlag) {

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumNCycSysAvailMgrs; ++SysAvailNum) {
                if (state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType ==
                    state.dataSystemAvailabilityManager->CycleOnControlZone) {
                    // set the controlled zone numbers
                    for (int index = 1; index <= state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfCtrlZones; ++index) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                            if (allocated(state.dataZoneEquip->ZoneEquipConfig)) {
                                if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum ==
                                    state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZonePtrs(index)) {
                                    state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZonePtrs(index) = ControlledZoneNum;
                                    break;
                                }
                            }
                        }
                    }
                }
            }

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumOptStartSysAvailMgrs; ++SysAvailNum) {
                {
                    auto const SELECT_CASE_var(state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).CtrlType);
                    if (SELECT_CASE_var == state.dataSystemAvailabilityManager->ControlZone) {
                        // set the controlled zone numbers
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                            if (allocated(state.dataZoneEquip->ZoneEquipConfig)) {
                                if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum ==
                                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ZoneNum) {
                                    state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ControlledZoneNum = ControlledZoneNum;
                                    break;
                                }
                            }
                        }
                    } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->MaximumOfZoneList) {
                        // a zone list
                        ZoneListNum = UtilityRoutines::FindItemInList(
                            state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ZoneListName, state.dataHeatBal->ZoneList);
                        if (ZoneListNum > 0) {
                            state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).NumOfZones =
                                state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones;
                            if (!allocated(state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ZonePtrs)) {
                                state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum)
                                    .ZonePtrs.allocate({1, state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones});
                            }
                            for (ScanZoneListNum = 1; ScanZoneListNum <= state.dataHeatBal->ZoneList(ZoneListNum).NumOfZones; ++ScanZoneListNum) {
                                ZoneNum = state.dataHeatBal->ZoneList(ZoneListNum).Zone(ScanZoneListNum);
                                state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum).ZonePtrs(ScanZoneListNum) = ZoneNum;
                            }
                        }
                    }
                }
            }

            for (SysAvailNum = 1; SysAvailNum <= state.dataSystemAvailabilityManager->NumNVentSysAvailMgrs; ++SysAvailNum) {
                // set the controlled zone numbers
                for (int ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                    if (allocated(state.dataZoneEquip->ZoneEquipConfig)) {
                        if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum ==
                            state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).ZoneNum) {
                            state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).ControlledZoneNum = ControlledZoneNum;
                            break;
                        }
                    }
                }
            }

            state.dataSystemAvailabilityManager->InitSysAvailManagers_MyOneTimeFlag = false;

        } // end 1 time initializations

        // initialize individual availability managers to no action (CR 8376 reporting issue)
        if (allocated(state.dataSystemAvailabilityManager->SchedSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->SchedSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->NCycSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->NCycSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->NVentSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->NVentSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->DiffTSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->DiffTSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData)
                e.AvailStatus = NoAction;
        if (allocated(state.dataSystemAvailabilityManager->OptStartSysAvailMgrData)) {
            for (auto &e : state.dataSystemAvailabilityManager->OptStartSysAvailMgrData) {
                e.AvailStatus = NoAction;
                e.isSimulated = false;
            }
        }
        //  HybridVentSysAvailMgrData%AvailStatus= NoAction
        for (ZoneEquipType = 1; ZoneEquipType <= NumValidSysAvailZoneComponents; ++ZoneEquipType) { // loop over the zone equipment types
            if (allocated(state.dataHVACGlobal->ZoneComp)) {
                if (state.dataHVACGlobal->ZoneComp(ZoneEquipType).TotalNumComp > 0)
                    for (auto &e : state.dataHVACGlobal->ZoneComp(ZoneEquipType).ZoneCompAvailMgrs)
                        e.AvailStatus = NoAction;
            }
        }
    }

    void SimSysAvailManager(EnergyPlusData &state,
                            int const SysAvailType,
                            std::string const &SysAvailName,
                            int &SysAvailNum,
                            int const PriAirSysNum, // Primary Air System index. If being called for a ZoneHVAC:* component
                            int const PreviousStatus,
                            int &AvailStatus,
                            Optional_int_const ZoneEquipType, // Type of ZoneHVAC:* equipment component
                            Optional_int_const CompNum        // Index of ZoneHVAC:* equipment component
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   August 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE
        // Loop over all the System Availability Managers and invoke the correct
        // System Availability Manager algorithm.

        // Using/Aliasing

        {
            auto const SELECT_CASE_var(SysAvailType);
            if (SELECT_CASE_var == state.dataSystemAvailabilityManager->SysAvailMgr_Scheduled) { // 'AvailabilityManager:Scheduled'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->SchedSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcSchedSysAvailMgr(state, SysAvailNum, AvailStatus);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:Scheduled not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->SysAvailMgr_ScheduledOn) { // 'AvailabilityManager:ScheduledOn'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcSchedOnSysAvailMgr(state, SysAvailNum, AvailStatus);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:ScheduledOn not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->SysAvailMgr_ScheduledOff) { // 'AvailabilityManager:ScheduledOff'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcSchedOffSysAvailMgr(state, SysAvailNum, AvailStatus);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:ScheduledOff not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->SysAvailMgr_NightCycle) { // 'AvailabilityManager:NightCycle'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->NCycSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcNCycSysAvailMgr(state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:NightCycle not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->SysAvailMgr_OptimumStart) { // 'AvailabilityManager:OptimumStart'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->OptStartSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcOptStartSysAvailMgr(state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType, CompNum);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:OptimumStart not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->SysAvailMgr_NightVent) { // 'AvailabilityManager:NightVentilation'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->NVentSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcNVentSysAvailMgr(state, SysAvailNum, PriAirSysNum, AvailStatus, ZoneEquipType);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:NightVentilation not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var ==
                       state.dataSystemAvailabilityManager->SysAvailMgr_DiffThermo) { // 'AvailabilityManager:DifferentialThermostat'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->DiffTSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcDiffTSysAvailMgr(state, SysAvailNum, PreviousStatus, AvailStatus);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:DifferentialThermostat not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var ==
                       state.dataSystemAvailabilityManager->SysAvailMgr_HiTempTOff) { // 'AvailabilityManager:HighTemperatureTurnOff'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcHiTurnOffSysAvailMgr(state, SysAvailNum, AvailStatus);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:HighTemperatureTurnOff not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->SysAvailMgr_HiTempTOn) { // 'AvailabilityManager:HighTemperatureTurnOn'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcHiTurnOnSysAvailMgr(state, SysAvailNum, AvailStatus);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:HighTemperatureTurnOn not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var ==
                       state.dataSystemAvailabilityManager->SysAvailMgr_LoTempTOff) { // 'AvailabilityManager:LowTemperatureTurnOff'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcLoTurnOffSysAvailMgr(state, SysAvailNum, AvailStatus);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:LowTemperatureTurnOff not found: " + SysAvailName);
                }

            } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->SysAvailMgr_LoTempTOn) { // 'AvailabilityManager:LowTemperatureTurnOn'
                if (SysAvailNum == 0) {
                    SysAvailNum = UtilityRoutines::FindItemInList(SysAvailName, state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData);
                }
                if (SysAvailNum > 0) {
                    CalcLoTurnOnSysAvailMgr(state, SysAvailNum, AvailStatus);
                } else {
                    ShowFatalError(state, "SimSysAvailManager: AvailabilityManager:LowTemperatureTurnOn not found: " + SysAvailName);
                }

            } else {
                ShowSevereError(state, format("AvailabilityManager Type not found: {}", SysAvailType));
                ShowContinueError(state, "Occurs in Manager=" + SysAvailName);
                ShowFatalError(state, "Preceding condition causes termination.");
            }
        }
    }

    void CalcSchedSysAvailMgr(EnergyPlusData &state,
                              int const SysAvailNum, // number of the current scheduled system availability manager
                              int &AvailStatus       // System status indicator
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   August 2001
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a primary air loop, plant loop or ZoneHVAC component.

        // METHODOLOGY EMPLOYED:
        // Looks at the System Availability Manager schedule and sets the
        // AvailStatus indicator accordingly. Mostly a useless algorithm
        // since the fan schedules can do the same thing.

        if (GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->SchedSysAvailMgrData(SysAvailNum).SchedPtr) > 0.0) {
            AvailStatus = CycleOn;
        } else {
            AvailStatus = ForceOff;
        }

        state.dataSystemAvailabilityManager->SchedSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
    }

    void CalcSchedOnSysAvailMgr(EnergyPlusData &state,
                                int const SysAvailNum, // number of the current scheduled on system availability manager
                                int &AvailStatus       // System status indicator
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad - FSEC
        //       DATE WRITTEN   August 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a primary air loop, plant loop or ZoneHVAC component.

        // METHODOLOGY EMPLOYED:
        // Looks at the System Availability Manager schedule and sets the
        // AvailStatus indicator accordingly. If the schedule value is > 0
        // the availability status is CycleOn, ELSE the status is NoAction.

        if (GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData(SysAvailNum).SchedPtr) > 0.0) {
            AvailStatus = CycleOn;
        } else {
            AvailStatus = NoAction;
        }

        state.dataSystemAvailabilityManager->SchedOnSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
    }

    void CalcSchedOffSysAvailMgr(EnergyPlusData &state,
                                 int const SysAvailNum, // number of the current scheduled off system availability manager
                                 int &AvailStatus       // System status indicator
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         R. Raustad - FSEC
        //       DATE WRITTEN   August 2008
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a primary air loop, plant loop or ZoneHVAC component.

        // METHODOLOGY EMPLOYED:
        // Looks at the System Availability Manager schedule and sets the
        // AvailStatus indicator accordingly.  If the schedule value is = 0
        // the availability status is ForceOff, ELSE the status is NoAction.

        if (GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData(SysAvailNum).SchedPtr) == 0.0) {
            AvailStatus = ForceOff;
        } else {
            AvailStatus = NoAction;
        }

        state.dataSystemAvailabilityManager->SchedOffSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
    }

    void CalcNCycSysAvailMgr(EnergyPlusData &state,
                             int const SysAvailNum,            // number of the current scheduled system availability manager
                             int const PriAirSysNum,           // number of the primary air system affected by this Avail. Manager
                             int &AvailStatus,                 // System status indicator
                             Optional_int_const ZoneEquipType, // Type of ZoneHVAC equipment component
                             Optional_int_const CompNum        // Index of ZoneHVAC equipment component
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   August 2001
        //       MODIFIED       March 2011, Chandan Sharma - FSEC: Allowed night cycle
        //                             availability manager to work for ZoneHVAC component
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a primary air loop or ZoneHVAC component.

        // METHODOLOGY EMPLOYED:
        // For air loop, depending on the type of control, looks at 1 named zone or all the zones
        // attached to a primary air system, compares zone temperature to the setup
        // or setback thermostat setpoint, and sets the AvailStaus indicator according
        // to whether the system needs to be cycled on or not.
        // For ZoneHVAC component, uses the exact same method as above but only looks at the
        // zone where component is located.

        using namespace DataAirLoop;

        int StartTime;
        int StopTime;
        int ZoneInSysNum;
        int CtrldZoneNum;
        int ZoneNum;
        Real64 TempTol;
        auto &ZoneCompNCControlType = state.dataSystemAvailabilityManager->ZoneCompNCControlType;
        int CyclingRunTimeControlType;

        auto &ZoneComp = state.dataHVACGlobal->ZoneComp;
        if (present(ZoneEquipType)) {
            if (state.dataGlobal->WarmupFlag && state.dataGlobal->BeginDayFlag) {
                // reset start/stop times at beginning of each day during warmup to prevent non-convergence due to rotating start times
                ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StartTime = state.dataGlobal->SimTimeSteps;
                ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StopTime = state.dataGlobal->SimTimeSteps;
            }

            StartTime = ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StartTime;
            StopTime = ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StopTime;
            if (state.dataSystemAvailabilityManager->CalcNCycSysAvailMgr_OneTimeFlag) {
                ZoneCompNCControlType.dimension(state.dataSystemAvailabilityManager->NumNCycSysAvailMgrs, true);
                state.dataSystemAvailabilityManager->CalcNCycSysAvailMgr_OneTimeFlag = false;
            }
        } else {
            if (state.dataGlobal->WarmupFlag && state.dataGlobal->BeginDayFlag) {
                // reset start/stop times at beginning of each day during warmup to prevent non-convergence due to rotating start times
                state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = state.dataGlobal->SimTimeSteps;
                state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime = state.dataGlobal->SimTimeSteps;
            }

            StartTime = state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime;
            StopTime = state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime;
        }
        // CR 7913 changed to allow during warmup
        if ((GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).SchedPtr) <= 0.0) ||
            (GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).FanSchedPtr) > 0.0)) {
            AvailStatus = NoAction;
            state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus; // CR 8358
            return;
        }

        CyclingRunTimeControlType = state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CycRunTimeCntrlType;

        if (CyclingRunTimeControlType == state.dataSystemAvailabilityManager->FixedRunTime) {
            TempTol = 0.5 * state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).TempTolRange;
        } else {
            TempTol = 0.05;
        }

        if (present(ZoneEquipType)) {
            if (state.dataGlobal->SimTimeSteps >= StartTime && state.dataGlobal->SimTimeSteps < StopTime &&
                (CyclingRunTimeControlType == state.dataSystemAvailabilityManager->FixedRunTime ||
                 CyclingRunTimeControlType == state.dataSystemAvailabilityManager->ThermostatWithMinimumRunTime)) { // if cycled on
                AvailStatus = CycleOn;
            } else if (state.dataGlobal->SimTimeSteps == StopTime &&
                       CyclingRunTimeControlType ==
                           state.dataSystemAvailabilityManager->FixedRunTime) { // if end of cycle run time, shut down if fan off
                AvailStatus = NoAction;
            } else {

                {
                    auto const SELECT_CASE_var(
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType); // select type of night cycle control

                    if (SELECT_CASE_var == state.dataSystemAvailabilityManager->StayOff) {
                        AvailStatus = NoAction;

                    } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->CycleOnControlZone) {

                        ZoneNum = state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZonePtrs(1);

                        {
                            auto const SELECT_CASE_var1(state.dataHeatBalFanSys->TempControlType(ZoneNum)); // select on thermostat control

                            if (SELECT_CASE_var1 == SingleHeatingSetPoint) {
                                if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <
                                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - TempTol) {
                                    AvailStatus = CycleOn;
                                } else {
                                    AvailStatus = NoAction;
                                }

                            } else if (SELECT_CASE_var1 == SingleCoolingSetPoint) {
                                if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) >
                                    state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) + TempTol) {
                                    AvailStatus = CycleOn;
                                } else {
                                    AvailStatus = NoAction;
                                }

                            } else if (SELECT_CASE_var1 == SingleHeatCoolSetPoint) {
                                if ((state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <
                                     state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - TempTol) ||
                                    (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) >
                                     state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) + TempTol)) {
                                    AvailStatus = CycleOn;
                                } else {
                                    AvailStatus = NoAction;
                                }

                            } else if (SELECT_CASE_var1 == DualSetPointWithDeadBand) {
                                if ((state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <
                                     state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - TempTol) ||
                                    (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) >
                                     state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) + TempTol)) {
                                    AvailStatus = CycleOn;
                                } else {
                                    AvailStatus = NoAction;
                                }

                            } else {
                                AvailStatus = NoAction;
                            }
                        } // end select on thermostat control

                    } else if ((SELECT_CASE_var == state.dataSystemAvailabilityManager->CycleOnAny) ||
                               (SELECT_CASE_var == state.dataSystemAvailabilityManager->ZoneFansOnly)) {
                        if (ZoneCompNCControlType(SysAvailNum)) {
                            ShowWarningError(
                                state,
                                "AvailabilityManager:NightCycle = " + state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).Name +
                                    ", is specified for a ZoneHVAC component.");
                            ShowContinueError(state, "The only valid Control Types for ZoneHVAC components are CycleOnControlZone and StayOff.");
                            ShowContinueError(state,
                                              "Night Cycle operation will not be modeled for ZoneHVAC components that reference this manager.");
                            ZoneCompNCControlType(SysAvailNum) = false;
                        }
                        AvailStatus = NoAction;

                    } else {
                        AvailStatus = NoAction;
                    }
                } // end select type of night cycle control

                if (AvailStatus == CycleOn) {                                                           // reset the start and stop times
                    if (CyclingRunTimeControlType == state.dataSystemAvailabilityManager->Thermostat) { // Cycling Run Time is ignored
                        ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StartTime = state.dataGlobal->SimTimeSteps;
                        ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StopTime = state.dataGlobal->SimTimeSteps;
                    } else {
                        ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StartTime = state.dataGlobal->SimTimeSteps;
                        ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(CompNum).StopTime =
                            state.dataGlobal->SimTimeSteps + state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CyclingTimeSteps;
                    }
                }
            }
        } else {
            if (state.dataGlobal->SimTimeSteps >= StartTime && state.dataGlobal->SimTimeSteps < StopTime &&
                (CyclingRunTimeControlType == state.dataSystemAvailabilityManager->FixedRunTime ||
                 CyclingRunTimeControlType == state.dataSystemAvailabilityManager->ThermostatWithMinimumRunTime)) { // if cycled on
                AvailStatus = state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).PriorAvailStatus;
                if (state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType ==
                    state.dataSystemAvailabilityManager->ZoneFansOnly)
                    AvailStatus = CycleOnZoneFansOnly;
            } else if (state.dataGlobal->SimTimeSteps == StopTime &&
                       CyclingRunTimeControlType ==
                           state.dataSystemAvailabilityManager->FixedRunTime) { // if end of cycle run time, shut down if fan off
                AvailStatus = NoAction;
            } else {

                {
                    auto const SELECT_CASE_var(
                        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType); // select type of night cycle control

                    if (SELECT_CASE_var == state.dataSystemAvailabilityManager->StayOff) {
                        AvailStatus = NoAction;

                    } else if ((SELECT_CASE_var == state.dataSystemAvailabilityManager->CycleOnAny) ||
                               (SELECT_CASE_var == state.dataSystemAvailabilityManager->ZoneFansOnly)) {

                        // If no zones cooled, Availstatus could be "unknown"
                        AvailStatus = NoAction;

                        for (ZoneInSysNum = 1; ZoneInSysNum <= state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).NumZonesCooled;
                             ++ZoneInSysNum) { // loop over zones in system

                            CtrldZoneNum = state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).CoolCtrlZoneNums(ZoneInSysNum);
                            ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrldZoneNum).ActualZoneNum;

                            {
                                auto const SELECT_CASE_var1(state.dataHeatBalFanSys->TempControlType(ZoneNum)); // select on thermostat control

                                if (SELECT_CASE_var1 == SingleHeatingSetPoint) {
                                    if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <
                                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - TempTol) {
                                        AvailStatus = CycleOn;
                                        break;
                                    } else {
                                        AvailStatus = NoAction;
                                    }

                                } else if (SELECT_CASE_var1 == SingleCoolingSetPoint) {
                                    if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) >
                                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) + TempTol) {
                                        AvailStatus = CycleOn;
                                        break;
                                    } else {
                                        AvailStatus = NoAction;
                                    }

                                } else if (SELECT_CASE_var1 == SingleHeatCoolSetPoint) {
                                    if ((state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <
                                         state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - TempTol) ||
                                        (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) >
                                         state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) + TempTol)) {
                                        AvailStatus = CycleOn;
                                        break;
                                    } else {
                                        AvailStatus = NoAction;
                                    }

                                } else if (SELECT_CASE_var1 == DualSetPointWithDeadBand) {
                                    if ((state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <
                                         state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - TempTol) ||
                                        (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) >
                                         state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) + TempTol)) {
                                        AvailStatus = CycleOn;
                                        break;
                                    } else {
                                        AvailStatus = NoAction;
                                    }

                                } else {
                                    AvailStatus = NoAction;
                                }
                            } // end select on thermostat control

                        } // end loop over zones in system

                    } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->CycleOnControlZone) {
                        AvailStatus = NoAction;
                        if (CoolingZoneOutOfTolerance(state,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZonePtrs,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfCtrlZones,
                                                      TempTol))
                            AvailStatus = CycleOn;
                        if (HeatingZoneOutOfTolerance(state,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlZonePtrs,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfCtrlZones,
                                                      TempTol))
                            AvailStatus = CycleOn;
                    } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->CycleOnAnyCoolingOrHeatingZone) {
                        if (CoolingZoneOutOfTolerance(state,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CoolingZonePtrs,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfCoolingZones,
                                                      TempTol)) {
                            AvailStatus = CycleOn;
                        } else if (HeatingZoneOutOfTolerance(state,
                                                             state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatingZonePtrs,
                                                             state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfHeatingZones,
                                                             TempTol)) {
                            AvailStatus = CycleOn;
                        } else if (HeatingZoneOutOfTolerance(
                                       state,
                                       state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatZnFanZonePtrs,
                                       state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfHeatZnFanZones,
                                       TempTol)) {
                            AvailStatus = CycleOnZoneFansOnly;
                        } else {
                            AvailStatus = NoAction;
                        }
                    } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->CycleOnAnyCoolingZone) {
                        if (CoolingZoneOutOfTolerance(state,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CoolingZonePtrs,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfCoolingZones,
                                                      TempTol)) {
                            AvailStatus = CycleOn;
                        } else {
                            AvailStatus = NoAction;
                        }
                    } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->CycleOnAnyHeatingZone) {
                        if (HeatingZoneOutOfTolerance(state,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatingZonePtrs,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfHeatingZones,
                                                      TempTol)) {
                            AvailStatus = CycleOn;
                        } else if (HeatingZoneOutOfTolerance(
                                       state,
                                       state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatZnFanZonePtrs,
                                       state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfHeatZnFanZones,
                                       TempTol)) {
                            AvailStatus = CycleOnZoneFansOnly;
                        } else {
                            AvailStatus = NoAction;
                        }
                    } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->CycleOnControlZone) {
                        if (HeatingZoneOutOfTolerance(state,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).HeatZnFanZonePtrs,
                                                      state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).NumOfHeatZnFanZones,
                                                      TempTol)) {
                            AvailStatus = CycleOnZoneFansOnly;
                        } else {
                            AvailStatus = NoAction;
                        }
                    } else {
                        AvailStatus = NoAction;
                    }
                } // end select type of night cycle control

                if ((AvailStatus == CycleOn) || (AvailStatus == CycleOnZoneFansOnly)) { // reset the start and stop times
                    if (state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CtrlType ==
                        state.dataSystemAvailabilityManager->ZoneFansOnly)
                        AvailStatus = CycleOnZoneFansOnly;
                    // issue #6151
                    if (CyclingRunTimeControlType == state.dataSystemAvailabilityManager->Thermostat) { // Cycling Run Time is ignored
                        state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = state.dataGlobal->SimTimeSteps;
                        state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime = state.dataGlobal->SimTimeSteps;
                    } else {
                        state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StartTime = state.dataGlobal->SimTimeSteps;
                        state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).StopTime =
                            state.dataGlobal->SimTimeSteps + state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).CyclingTimeSteps;
                    }
                }
            }
        }
        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
        state.dataSystemAvailabilityManager->NCycSysAvailMgrData(SysAvailNum).PriorAvailStatus = AvailStatus;
    }

    bool CoolingZoneOutOfTolerance(EnergyPlusData &state,
                                   Array1D_int const ZonePtrList, // list of controlled zone pointers
                                   int const NumZones,            // number of zones in list
                                   Real64 const TempTolerance     // temperature tolerance
    )
    {
        // Check if any zone temperature is above the cooling setpoint plus tolerance
        for (int Index = 1; Index <= NumZones; ++Index) { // loop over zones in list
            int ZoneNum = ZonePtrList(Index);
            {
                auto const tstatType(state.dataHeatBalFanSys->TempControlType(ZoneNum));

                if ((tstatType == SingleCoolingSetPoint) || (tstatType == SingleHeatCoolSetPoint)) {
                    if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) >
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) + TempTolerance) {
                        return true; // return on the first zone found
                    }
                } else if (tstatType == DualSetPointWithDeadBand) {
                    if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) > state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum) + TempTolerance) {
                        return true; // return on the first zone found
                    }
                }
            }
        }
        return false;
    }

    bool HeatingZoneOutOfTolerance(EnergyPlusData &state,
                                   Array1D_int const ZonePtrList, // list of controlled zone pointers
                                   int const NumZones,            // number of zones in list
                                   Real64 const TempTolerance     // temperature tolerance
    )
    {
        // Check if any zone temperature is below the heating setpoint less tolerance
        for (int Index = 1; Index <= NumZones; ++Index) { // loop over zones in list
            int ZoneNum = ZonePtrList(Index);
            {
                auto const tstatType(state.dataHeatBalFanSys->TempControlType(ZoneNum));

                if ((tstatType == SingleHeatingSetPoint) || (tstatType == SingleHeatCoolSetPoint)) {
                    if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <
                        state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum) - TempTolerance) {
                        return true; // return on the first zone found
                    }
                } else if (tstatType == DualSetPointWithDeadBand) {
                    if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) < state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum) - TempTolerance) {
                        return true; // return on the first zone found
                    }
                }
            }
        }
        return false;
    }

    void CalcOptStartSysAvailMgr(EnergyPlusData &state,
                                 int const SysAvailNum,  // number of the current scheduled system availability manager
                                 int const PriAirSysNum, // number of the primary air system affected by this Avail. Manager
                                 int &AvailStatus,       // System status indicator
                                 [[maybe_unused]] Optional_int_const ZoneEquipType, // Type of ZoneHVAC equipment component
                                 [[maybe_unused]] Optional_int_const CompNum        // Index of ZoneHVAC equipment component
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR            Xiufeng Pang (XP)
        //       DATE WRITTEN      August 2013
        //       MODIFIED
        //       RE-ENGINEERED

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a primary air loop, plant loop or ZoneHVAC component

        // METHODOLOGY EMPLOYED:
        // Sets the AvailStatus indicator according to the
        // optimum start algorithm

        // Using/Aliasing
        using namespace DataAirLoop;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        int ScheduleIndex;
        Array2D<Real64> DayValues;
        Array2D<Real64> DayValuesTmr;
        int JDay;
        int TmrJDay;
        int TmrDayOfWeek;
        int ZoneNum;
        Real64 FanStartTime;
        Real64 FanStartTimeTmr;
        Real64 PreStartTime;
        Real64 PreStartTimeTmr;
        Real64 DeltaTime;
        Real64 TempDiff;
        Real64 TempDiffHi;
        Real64 TempDiffLo;
        bool FirstTimeATGFlag(true);
        bool OverNightStartFlag(false); // Flag to indicate the optimum start starts before mid night.
        bool CycleOnFlag(false);
        bool OSReportVarFlag(true);
        int NumPreDays;
        int NumOfZonesInList;
        Real64 AdaTempGradHeat;
        Real64 AdaTempGradCool;
        Real64 ATGUpdateTime1(0.0);
        Real64 ATGUpdateTime2(0.0);
        Real64 ATGUpdateTemp1(0.0);
        Real64 ATGUpdateTemp2(0.0);
        bool ATGUpdateFlag1(false);
        bool ATGUpdateFlag2(false);
        int ATGCounter;
        int ATGWCZoneNumHi;
        int ATGWCZoneNumLo;
        Real64 NumHoursBeforeOccupancy; // Variable to store the number of hours before occupancy in optimum start period
        bool exitLoop;                  // exit loop on found data

        auto &OptStartMgr(state.dataSystemAvailabilityManager->OptStartSysAvailMgrData(SysAvailNum));

        // some avail managers may be used in air loop and plant availability manager lists, if so they only need be simulated once
        if (OptStartMgr.isSimulated) {
            AvailStatus = OptStartMgr.AvailStatus;
            return;
        }
        OptStartMgr.isSimulated = true;

        // update air loop specific data
        TempDiffLo = OptStartMgr.TempDiffLo;
        TempDiffHi = OptStartMgr.TempDiffHi;
        ATGWCZoneNumLo = OptStartMgr.ATGWCZoneNumLo;
        ATGWCZoneNumHi = OptStartMgr.ATGWCZoneNumHi;
        CycleOnFlag = OptStartMgr.CycleOnFlag;
        ATGUpdateFlag1 = OptStartMgr.ATGUpdateFlag1;
        ATGUpdateFlag2 = OptStartMgr.ATGUpdateFlag2;
        NumHoursBeforeOccupancy = OptStartMgr.NumHoursBeforeOccupancy;
        FirstTimeATGFlag = OptStartMgr.FirstTimeATGFlag;
        OverNightStartFlag = OptStartMgr.OverNightStartFlag;
        OSReportVarFlag = OptStartMgr.OSReportVarFlag;

        if (OptStartMgr.CtrlAlgType == state.dataSystemAvailabilityManager->AdaptiveTemperatureGradient) {
            NumPreDays = OptStartMgr.NumPreDays;
            if (!allocated(state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat)) {
                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat.allocate(NumPreDays);
                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool.allocate(NumPreDays);
            }
            if (!allocated(OptStartMgr.AdaTempGradTrdHeat)) {
                OptStartMgr.AdaTempGradTrdHeat.allocate(NumPreDays);
                OptStartMgr.AdaTempGradTrdHeat = 0.0;
                OptStartMgr.AdaTempGradTrdCool.allocate(NumPreDays);
                OptStartMgr.AdaTempGradTrdCool = 0.0;
            }
            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat = OptStartMgr.AdaTempGradTrdHeat;
            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool = OptStartMgr.AdaTempGradTrdCool;
            AdaTempGradHeat = OptStartMgr.AdaTempGradHeat;
            AdaTempGradCool = OptStartMgr.AdaTempGradCool;
            ATGUpdateTime1 = OptStartMgr.ATGUpdateTime1;
            ATGUpdateTime2 = OptStartMgr.ATGUpdateTime2;
            ATGUpdateTemp1 = OptStartMgr.ATGUpdateTemp1;
            ATGUpdateTemp2 = OptStartMgr.ATGUpdateTemp2;
        }

        auto &OptStartData = state.dataHVACGlobal->OptStartData;

        // add or use a new variable OptStartSysAvailMgrData(SysAvailNum)%FanSchIndex
        if (state.dataGlobal->KickOffSimulation) {
            AvailStatus = NoAction;
        } else {
            ScheduleIndex = GetScheduleIndex(state, OptStartMgr.FanSched);
            JDay = state.dataEnvrn->DayOfYear;
            TmrJDay = JDay + 1;
            TmrDayOfWeek = state.dataEnvrn->DayOfWeekTomorrow;

            DayValues.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
            DayValuesTmr.allocate(state.dataGlobal->NumOfTimeStepInHour, 24);
            if (!allocated(OptStartData.OptStartFlag)) {
                OptStartData.OptStartFlag.allocate(state.dataGlobal->NumOfZones);
                OptStartData.OccStartTime.allocate(state.dataGlobal->NumOfZones);
            }
            if (!allocated(OptStartData.ActualZoneNum)) OptStartData.ActualZoneNum.allocate(state.dataGlobal->NumOfZones);

            // OptStartFlag needs to be reset each timestep to not stay set to true post-occupancy
            OptStartData.OptStartFlag = false;

            // reset OptStartData once per beginning of day
            if (state.dataGlobal->BeginDayFlag) {
                NumHoursBeforeOccupancy = 0.0; // Initialize the hours of optimum start period. This variable is for reporting purpose.
                if (state.dataSystemAvailabilityManager->BeginOfDayResetFlag) {
                    OptStartData.OccStartTime = 22.99; // initialize the zone occupancy start time
                    state.dataSystemAvailabilityManager->BeginOfDayResetFlag = false;
                }
            }
            if (!state.dataGlobal->BeginDayFlag) state.dataSystemAvailabilityManager->BeginOfDayResetFlag = true;

            GetScheduleValuesForDay(state, ScheduleIndex, DayValues);
            GetScheduleValuesForDay(state, ScheduleIndex, DayValuesTmr, TmrJDay, TmrDayOfWeek);

            FanStartTime = 0.0;
            FanStartTimeTmr = 0.0;
            exitLoop = false;
            for (int I = 1; I <= 24; ++I) {
                for (int J = 1; J <= state.dataGlobal->NumOfTimeStepInHour; ++J) {
                    if (DayValues(J, I) <= 0.0) continue;
                    FanStartTime = I - 1 + 1.0 / state.dataGlobal->NumOfTimeStepInHour * J - 0.01;
                    exitLoop = true;
                    break;
                }
                if (exitLoop) break;
            }

            exitLoop = false;
            for (int I = 1; I <= 24; ++I) {
                for (int J = 1; J <= state.dataGlobal->NumOfTimeStepInHour; ++J) {
                    if (DayValuesTmr(J, I) <= 0.0) continue;
                    FanStartTimeTmr = I - 1 + 1.0 / state.dataGlobal->NumOfTimeStepInHour * J - 0.01;
                    exitLoop = true;
                    break;
                }
                if (exitLoop) break;
            }

            if (FanStartTimeTmr == 0.0) FanStartTimeTmr = 24.0;

            // Pass the start time to ZoneTempPredictorCorrector
            for (int counter = 1; counter <= state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).NumZonesCooled; ++counter) {
                int actZoneNum =
                    state.dataZoneEquip->ZoneEquipConfig(state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).CoolCtrlZoneNums(counter)).ActualZoneNum;
                OptStartData.OccStartTime(actZoneNum) = FanStartTime;
                OptStartData.ActualZoneNum(actZoneNum) = actZoneNum;
            }
            for (int counter = 1; counter <= state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).NumZonesHeated; ++counter) {
                int actZoneNum =
                    state.dataZoneEquip->ZoneEquipConfig(state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).HeatCtrlZoneNums(counter)).ActualZoneNum;
                OptStartData.OccStartTime(actZoneNum) = FanStartTime;
                OptStartData.ActualZoneNum(actZoneNum) = actZoneNum;
            }

            if (state.dataEnvrn->DSTIndicator > 0) {
                --FanStartTime;
                --FanStartTimeTmr;
            }

            {
                auto const SELECT_CASE_var(OptStartMgr.CtrlAlgType);
                if (SELECT_CASE_var == state.dataSystemAvailabilityManager->ConstantStartTime) {
                    if (OptStartMgr.CtrlType == state.dataSystemAvailabilityManager->StayOff) {
                        AvailStatus = NoAction;
                    } else {
                        DeltaTime = OptStartMgr.ConstStartTime;
                        if (DeltaTime > OptStartMgr.MaxOptStartTime) {
                            DeltaTime = OptStartMgr.MaxOptStartTime;
                        }
                        PreStartTime = FanStartTime - DeltaTime;
                        if (PreStartTime < 0.0) PreStartTime = -0.1;
                        PreStartTimeTmr = FanStartTimeTmr - DeltaTime;
                        if (PreStartTimeTmr < 0.0) {
                            PreStartTimeTmr += 24.0;
                            OverNightStartFlag = true;
                        } else {
                            OverNightStartFlag = false;
                        }
                        if (!OverNightStartFlag) {
                            if (FanStartTime == 0.0 || state.dataGlobal->PreviousHour > FanStartTime) {
                                AvailStatus = NoAction;
                                OSReportVarFlag = true;
                            } else if (PreStartTime < state.dataGlobal->CurrentTime) {
                                if (OSReportVarFlag) {
                                    NumHoursBeforeOccupancy = DeltaTime;
                                    OSReportVarFlag = false;
                                }
                                AvailStatus = CycleOn;
                                OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                            } else {
                                AvailStatus = NoAction;
                                OSReportVarFlag = true;
                            }
                        } else {
                            if (FanStartTime == 0.0 ||
                                (state.dataGlobal->HourOfDay > FanStartTime && state.dataGlobal->CurrentTime <= PreStartTimeTmr)) {
                                AvailStatus = NoAction;
                                OSReportVarFlag = true;
                            } else if (PreStartTime < state.dataGlobal->CurrentTime || PreStartTimeTmr < state.dataGlobal->CurrentTime) {
                                if (OSReportVarFlag) {
                                    NumHoursBeforeOccupancy = DeltaTime;
                                    OSReportVarFlag = false;
                                }
                                AvailStatus = CycleOn;
                                OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                            } else {
                                AvailStatus = NoAction;
                                OSReportVarFlag = true;
                            }
                        }
                    }

                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->ConstantTemperatureGradient) {
                    if (OptStartMgr.CtrlType == state.dataSystemAvailabilityManager->ControlZone) {
                        ZoneNum = OptStartMgr.ZoneNum;
                        if ((!allocated(state.dataHeatBalFanSys->TempTstatAir)) || (!allocated(state.dataHeatBalFanSys->ZoneThermostatSetPointLo)) ||
                            (!allocated(state.dataHeatBalFanSys->ZoneThermostatSetPointHi))) {
                            TempDiff = 0.0;
                        } else {
                            if (!CycleOnFlag) {
                                if (allocated(state.dataZoneCtrls->OccRoomTSetPointHeat) && allocated(state.dataZoneCtrls->OccRoomTSetPointCool)) {
                                    TempDiffHi = state.dataHeatBalFanSys->TempTstatAir(ZoneNum) - state.dataZoneCtrls->OccRoomTSetPointCool(ZoneNum);
                                    TempDiffLo = state.dataHeatBalFanSys->TempTstatAir(ZoneNum) - state.dataZoneCtrls->OccRoomTSetPointHeat(ZoneNum);
                                } else {
                                    TempDiffHi = 0.0;
                                    TempDiffLo = 0.0;
                                }
                            }
                        }

                        if (TempDiffHi < 0.0) {
                            TempDiff = TempDiffLo;
                            if (TempDiff < 0.0) { // Heating Mode
                                TempDiff = std::abs(TempDiff);
                                DeltaTime = TempDiff / OptStartMgr.ConstTGradHeat;
                                if (DeltaTime > OptStartMgr.MaxOptStartTime) {
                                    DeltaTime = OptStartMgr.MaxOptStartTime;
                                }
                                PreStartTime = FanStartTime - DeltaTime;
                                if (PreStartTime < 0) PreStartTime = -0.1;
                                PreStartTimeTmr = FanStartTimeTmr - DeltaTime;
                                if (PreStartTimeTmr < 0) {
                                    PreStartTimeTmr += 24.0;
                                    OverNightStartFlag = true;
                                } else {
                                    OverNightStartFlag = false;
                                }
                                if (!OverNightStartFlag) {
                                    if (FanStartTime == 0.0 || state.dataGlobal->CurrentTime > FanStartTime) {
                                        CycleOnFlag = false;
                                        OSReportVarFlag = true;
                                    } else if (CycleOnFlag) {
                                        AvailStatus = CycleOn;
                                        OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                        if (state.dataGlobal->CurrentTime > FanStartTime) CycleOnFlag = false;
                                    } else if (PreStartTime < state.dataGlobal->CurrentTime) {
                                        AvailStatus = CycleOn;
                                        CycleOnFlag = true;
                                        if (OSReportVarFlag) {
                                            NumHoursBeforeOccupancy = DeltaTime;
                                            OSReportVarFlag = false;
                                        }
                                        OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                    } else {
                                        AvailStatus = NoAction;
                                        CycleOnFlag = false;
                                        OSReportVarFlag = true;
                                    }
                                } else {
                                    if (FanStartTime == 0.0 ||
                                        (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime <= PreStartTimeTmr)) {
                                        AvailStatus = NoAction;
                                        CycleOnFlag = false;
                                        OSReportVarFlag = true;
                                    } else if (CycleOnFlag) {
                                        AvailStatus = CycleOn;
                                        OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                        if (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime < PreStartTimeTmr)
                                            CycleOnFlag = false;
                                    } else if (PreStartTime < state.dataGlobal->CurrentTime || PreStartTimeTmr < state.dataGlobal->CurrentTime) {
                                        if (OSReportVarFlag) {
                                            NumHoursBeforeOccupancy = DeltaTime;
                                            OSReportVarFlag = false;
                                        }
                                        AvailStatus = CycleOn;
                                        CycleOnFlag = true;
                                        OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                    } else {
                                        AvailStatus = NoAction;
                                        CycleOnFlag = false;
                                        OSReportVarFlag = true;
                                    }
                                }
                            } else {
                                AvailStatus = NoAction;
                                CycleOnFlag = false;
                            }
                        } else if (state.dataZoneCtrls->OccRoomTSetPointCool(ZoneNum) < 50.0) { // Cooling Mode
                            TempDiff = TempDiffHi;
                            DeltaTime = TempDiff / OptStartMgr.ConstTGradCool;
                            if (DeltaTime > OptStartMgr.MaxOptStartTime) {
                                DeltaTime = OptStartMgr.MaxOptStartTime;
                            }
                            PreStartTime = FanStartTime - DeltaTime;
                            if (PreStartTime < 0) PreStartTime = -0.1;
                            PreStartTimeTmr = FanStartTimeTmr - DeltaTime;
                            if (PreStartTimeTmr < 0) {
                                PreStartTimeTmr += 24.0;
                                OverNightStartFlag = true;
                            } else {
                                OverNightStartFlag = false;
                            }
                            if (!OverNightStartFlag) {
                                if (FanStartTime == 0.0 || state.dataGlobal->CurrentTime > FanStartTime) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else if (PreStartTime < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            } else {
                                if (FanStartTime == 0.0 ||
                                    (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime <= PreStartTimeTmr)) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else if (PreStartTime < state.dataGlobal->CurrentTime || PreStartTimeTmr < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            }
                        } else {
                            AvailStatus = NoAction;
                            CycleOnFlag = false;
                        }
                    } else if (OptStartMgr.CtrlType == state.dataSystemAvailabilityManager->MaximumOfZoneList) {

                        NumOfZonesInList = OptStartMgr.NumOfZones;
                        if ((!allocated(state.dataHeatBalFanSys->TempTstatAir)) || (!allocated(state.dataHeatBalFanSys->ZoneThermostatSetPointLo)) ||
                            (!allocated(state.dataHeatBalFanSys->ZoneThermostatSetPointHi))) {
                            TempDiff = 0.0;
                        } else {
                            if (!CycleOnFlag) {
                                if (allocated(state.dataZoneCtrls->OccRoomTSetPointHeat) && allocated(state.dataZoneCtrls->OccRoomTSetPointCool)) {
                                    TempDiffHi = 0.0;
                                    TempDiffLo = 0.0;
                                    for (ZoneNum = 1; ZoneNum <= NumOfZonesInList; ++ZoneNum) {
                                        TempDiff = state.dataHeatBalFanSys->TempTstatAir(OptStartMgr.ZonePtrs(ZoneNum)) -
                                                   state.dataZoneCtrls->OccRoomTSetPointCool(OptStartMgr.ZonePtrs(ZoneNum));
                                        TempDiffHi = max(TempDiffHi, TempDiff);
                                        TempDiff = state.dataHeatBalFanSys->TempTstatAir(OptStartMgr.ZonePtrs(ZoneNum)) -
                                                   state.dataZoneCtrls->OccRoomTSetPointHeat(OptStartMgr.ZonePtrs(ZoneNum));
                                        TempDiffLo = min(TempDiffLo, TempDiff);
                                    }
                                } else {
                                    TempDiffHi = 0.0;
                                    TempDiffLo = 0.0;
                                }
                            }
                        }
                        if ((TempDiffHi < 0.0 && TempDiffLo < 0.0) ||
                            (std::abs(TempDiffLo) > std::abs(TempDiffHi) && TempDiffLo < 0)) { // Heating Mode
                            TempDiff = TempDiffLo;
                            TempDiff = std::abs(TempDiff);
                            DeltaTime = TempDiff / OptStartMgr.ConstTGradHeat;
                            if (DeltaTime > OptStartMgr.MaxOptStartTime) {
                                DeltaTime = OptStartMgr.MaxOptStartTime;
                            }
                            PreStartTime = FanStartTime - DeltaTime;
                            if (PreStartTime < 0) PreStartTime = -0.1;
                            PreStartTimeTmr = FanStartTimeTmr - DeltaTime;
                            if (PreStartTimeTmr < 0) {
                                PreStartTimeTmr += 24.0;
                                OverNightStartFlag = true;
                            } else {
                                OverNightStartFlag = false;
                            }
                            if (!OverNightStartFlag) {
                                if (FanStartTime == 0.0 || state.dataGlobal->CurrentTime > FanStartTime) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                    if (state.dataGlobal->CurrentTime > FanStartTime) CycleOnFlag = false;
                                } else if (PreStartTime < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            } else {
                                if (FanStartTime == 0.0 ||
                                    (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime <= PreStartTimeTmr)) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                    if (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime < PreStartTimeTmr)
                                        CycleOnFlag = false;
                                } else if (PreStartTime < state.dataGlobal->CurrentTime || PreStartTimeTmr < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            }
                        } else if (TempDiffHi <= 0.0 && TempDiffLo >= 0.0) { // not heating and not cooling
                            AvailStatus = NoAction;
                            CycleOnFlag = false;
                            TempDiffHi = 0.0;
                            TempDiffLo = 0.0;
                        } else if (TempDiffHi < 30.0) { // Cooling Mode
                            TempDiff = TempDiffHi;
                            DeltaTime = TempDiff / OptStartMgr.ConstTGradCool;
                            if (DeltaTime > OptStartMgr.MaxOptStartTime) {
                                DeltaTime = OptStartMgr.MaxOptStartTime;
                            }
                            PreStartTime = FanStartTime - DeltaTime;
                            if (PreStartTime < 0) PreStartTime = -0.1;
                            PreStartTimeTmr = FanStartTimeTmr - DeltaTime;
                            if (PreStartTimeTmr < 0) {
                                PreStartTimeTmr += 24.0;
                                OverNightStartFlag = true;
                            } else {
                                OverNightStartFlag = false;
                            }
                            if (!OverNightStartFlag) {
                                if (FanStartTime == 0.0 || state.dataGlobal->CurrentTime > FanStartTime) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else if (PreStartTime < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            } else {
                                if (FanStartTime == 0.0 ||
                                    (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime <= PreStartTimeTmr)) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else if (PreStartTime < state.dataGlobal->CurrentTime || PreStartTimeTmr < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            }
                        } else {
                            AvailStatus = NoAction;
                            CycleOnFlag = false;
                        }
                    } else {
                        AvailStatus = NoAction;
                    }

                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->AdaptiveTemperatureGradient) {

                    if (OptStartMgr.CtrlType == state.dataSystemAvailabilityManager->ControlZone) {
                        ZoneNum = OptStartMgr.ZoneNum;
                        if ((!allocated(state.dataHeatBalFanSys->TempTstatAir)) || (!allocated(state.dataHeatBalFanSys->ZoneThermostatSetPointLo)) ||
                            (!allocated(state.dataHeatBalFanSys->ZoneThermostatSetPointHi))) {
                            TempDiff = 0.0;
                        } else {
                            if (!CycleOnFlag) {
                                if (allocated(state.dataZoneCtrls->OccRoomTSetPointHeat) && allocated(state.dataZoneCtrls->OccRoomTSetPointCool)) {
                                    TempDiffHi = state.dataHeatBalFanSys->TempTstatAir(ZoneNum) - state.dataZoneCtrls->OccRoomTSetPointCool(ZoneNum);
                                    TempDiffLo = state.dataHeatBalFanSys->TempTstatAir(ZoneNum) - state.dataZoneCtrls->OccRoomTSetPointHeat(ZoneNum);
                                } else {
                                    TempDiffHi = 0.0;
                                    TempDiffLo = 0.0;
                                }
                            }
                        }
                        // Store adaptive temperature gradients for previous days and calculate the adaptive temp gradients
                        //-----------------------------------------------------------------------------
                        if (state.dataGlobal->WarmupFlag) {
                            AdaTempGradHeat = OptStartMgr.InitTGradHeat;
                            AdaTempGradCool = OptStartMgr.InitTGradCool;
                        } else if (state.dataGlobal->DayOfSim == 1 && state.dataGlobal->BeginDayFlag) {
                            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat = OptStartMgr.InitTGradHeat;
                            AdaTempGradHeat = OptStartMgr.InitTGradHeat;
                            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool = OptStartMgr.InitTGradCool;
                            AdaTempGradCool = OptStartMgr.InitTGradCool;
                        } else {
                            if (state.dataGlobal->BeginDayFlag && FirstTimeATGFlag) {
                                FirstTimeATGFlag = false;
                                AdaTempGradHeat += state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) / NumPreDays -
                                                   state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(1) / NumPreDays;
                                AdaTempGradCool += state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) / NumPreDays -
                                                   state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(1) / NumPreDays;
                                if (FanStartTime > 0) {
                                    for (ATGCounter = 1; ATGCounter <= NumPreDays - 1; ++ATGCounter) {
                                        state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(ATGCounter) =
                                            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(ATGCounter + 1);
                                        state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(ATGCounter) =
                                            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(ATGCounter + 1);
                                    }
                                }
                            }
                        }

                        if (state.dataGlobal->CurrentTime >= 1.0) FirstTimeATGFlag = true;
                        //------------------------------------------------------------------------------

                        if (TempDiffHi < 0.0) {
                            TempDiff = TempDiffLo;
                            if (TempDiff < 0.0) { // Heating Mode
                                TempDiff = std::abs(TempDiff);
                                DeltaTime = TempDiff / AdaTempGradHeat;
                                if (DeltaTime > OptStartMgr.MaxOptStartTime) {
                                    DeltaTime = OptStartMgr.MaxOptStartTime;
                                }
                                PreStartTime = FanStartTime - DeltaTime;
                                if (PreStartTime < 0.0) PreStartTime = -0.1;
                                PreStartTimeTmr = FanStartTimeTmr - DeltaTime;
                                if (PreStartTimeTmr < 0.0) {
                                    PreStartTimeTmr += 24.0;
                                    OverNightStartFlag = true;
                                } else {
                                    OverNightStartFlag = false;
                                }
                                if (!OverNightStartFlag) {
                                    if (FanStartTime == 0.0 || state.dataGlobal->CurrentTime > FanStartTime) {
                                        AvailStatus = NoAction;
                                        CycleOnFlag = false;
                                        OSReportVarFlag = true;
                                    } else if (CycleOnFlag) {
                                        AvailStatus = CycleOn;
                                        OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                        if (state.dataGlobal->CurrentTime > FanStartTime) CycleOnFlag = false;
                                        // Calculate the current day actual temperature gradient --------------------------
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (ATGUpdateFlag1) {
                                                ATGUpdateTime1 = state.dataGlobal->CurrentTime;
                                                ATGUpdateTemp1 = state.dataHeatBalFanSys->TempTstatAir(ZoneNum);
                                                ATGUpdateFlag1 = false;
                                            }
                                            if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) >=
                                                    state.dataZoneCtrls->OccRoomTSetPointHeat(ZoneNum) &&
                                                ATGUpdateFlag2) {
                                                ATGUpdateTime2 = state.dataGlobal->CurrentTime;
                                                ATGUpdateTemp2 = state.dataHeatBalFanSys->TempTstatAir(ZoneNum);
                                                ATGUpdateFlag2 = false;
                                                if (std::abs(ATGUpdateTime2 - ATGUpdateTime1) > 1.e-10) {
                                                    state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) =
                                                        (ATGUpdateTemp2 - ATGUpdateTemp1) / (ATGUpdateTime2 - ATGUpdateTime1);
                                                } else {
                                                    state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) =
                                                        (ATGUpdateTemp2 - ATGUpdateTemp1) * state.dataGlobal->NumOfTimeStepInHour;
                                                }
                                            }
                                        }
                                        //---------------------------------------------------------------------------------
                                    } else if (PreStartTime < state.dataGlobal->CurrentTime) {
                                        if (OSReportVarFlag) {
                                            NumHoursBeforeOccupancy = DeltaTime;
                                            OSReportVarFlag = false;
                                        }
                                        AvailStatus = CycleOn;
                                        CycleOnFlag = true;
                                        ATGUpdateFlag1 = true;
                                        ATGUpdateFlag2 = true;
                                        OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                    } else {
                                        AvailStatus = NoAction;
                                        CycleOnFlag = false;
                                        OSReportVarFlag = true;
                                    }
                                } else {
                                    if (FanStartTime == 0.0 ||
                                        (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime <= PreStartTimeTmr)) {
                                        AvailStatus = NoAction;
                                        CycleOnFlag = false;
                                        OSReportVarFlag = true;
                                    } else if (CycleOnFlag) {
                                        AvailStatus = CycleOn;
                                        OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                        if (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime < PreStartTimeTmr)
                                            CycleOnFlag = false;
                                        // Calculate the current day actual temperature gradient --------------------------
                                        if (!state.dataGlobal->WarmupFlag) {
                                            if (ATGUpdateFlag1) {
                                                ATGUpdateTime1 = state.dataGlobal->CurrentTime;
                                                ATGUpdateTemp1 = state.dataHeatBalFanSys->TempTstatAir(ZoneNum);
                                                ATGUpdateFlag1 = false;
                                            }
                                            if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) >=
                                                    state.dataZoneCtrls->OccRoomTSetPointHeat(ZoneNum) &&
                                                ATGUpdateFlag2) {
                                                ATGUpdateTime2 = state.dataGlobal->CurrentTime;
                                                ATGUpdateTemp2 = state.dataHeatBalFanSys->TempTstatAir(ZoneNum);
                                                ATGUpdateFlag2 = false;
                                                if (std::abs(ATGUpdateTime2 - ATGUpdateTime1 + 24.0) > 1.e-10) {
                                                    state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) =
                                                        (ATGUpdateTemp2 - ATGUpdateTemp1) / (ATGUpdateTime2 - ATGUpdateTime1 + 24.0);
                                                } else {
                                                    state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) =
                                                        (ATGUpdateTemp2 - ATGUpdateTemp1) * state.dataGlobal->NumOfTimeStepInHour;
                                                }
                                            }
                                        }
                                        //---------------------------------------------------------------------------------
                                    } else if (PreStartTime < state.dataGlobal->CurrentTime || PreStartTimeTmr < state.dataGlobal->CurrentTime) {
                                        if (OSReportVarFlag) {
                                            NumHoursBeforeOccupancy = DeltaTime;
                                            OSReportVarFlag = false;
                                        }
                                        AvailStatus = CycleOn;
                                        CycleOnFlag = true;
                                        ATGUpdateFlag1 = true;
                                        ATGUpdateFlag2 = true;
                                        OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                    } else {
                                        AvailStatus = NoAction;
                                        CycleOnFlag = false;
                                        OSReportVarFlag = true;
                                    }
                                }
                            } else {
                                AvailStatus = NoAction;
                                CycleOnFlag = false;
                            }
                        } else if (state.dataZoneCtrls->OccRoomTSetPointCool(ZoneNum) < 50.0) { // Cooling Mode
                            TempDiff = TempDiffHi;
                            DeltaTime = TempDiff / AdaTempGradCool;
                            if (DeltaTime > OptStartMgr.MaxOptStartTime) {
                                DeltaTime = OptStartMgr.MaxOptStartTime;
                            }
                            PreStartTime = FanStartTime - DeltaTime;
                            if (PreStartTime < 0.0) PreStartTime = -0.1;
                            PreStartTimeTmr = FanStartTimeTmr - DeltaTime;
                            if (PreStartTimeTmr < 0.0) {
                                PreStartTimeTmr += 24.0;
                                OverNightStartFlag = true;
                            } else {
                                OverNightStartFlag = false;
                            }
                            if (!OverNightStartFlag) {
                                if (FanStartTime == 0.0 || state.dataGlobal->CurrentTime > FanStartTime) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (ATGUpdateFlag1) {
                                            ATGUpdateTime1 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp1 = state.dataHeatBalFanSys->TempTstatAir(ZoneNum);
                                            ATGUpdateFlag1 = false;
                                        }
                                        if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <= state.dataZoneCtrls->OccRoomTSetPointCool(ZoneNum) &&
                                            ATGUpdateFlag2) {
                                            ATGUpdateTime2 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp2 = state.dataHeatBalFanSys->TempTstatAir(ZoneNum);
                                            ATGUpdateFlag2 = false;
                                            if (std::abs(ATGUpdateTime2 - ATGUpdateTime1) > 1.e-10) {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) =
                                                    (ATGUpdateTemp1 - ATGUpdateTemp2) / (ATGUpdateTime2 - ATGUpdateTime1);
                                            } else {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) =
                                                    (ATGUpdateTemp1 - ATGUpdateTemp2) * state.dataGlobal->NumOfTimeStepInHour;
                                            }
                                        }
                                    }
                                } else if (PreStartTime < state.dataGlobal->CurrentTime) {
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    ATGUpdateFlag1 = true;
                                    ATGUpdateFlag2 = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            } else {
                                if (FanStartTime == 0.0 ||
                                    (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime <= PreStartTimeTmr)) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (ATGUpdateFlag1) {
                                            ATGUpdateTime1 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp1 = state.dataHeatBalFanSys->TempTstatAir(ZoneNum);
                                            ATGUpdateFlag1 = false;
                                        }
                                        if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <= state.dataZoneCtrls->OccRoomTSetPointCool(ZoneNum) &&
                                            ATGUpdateFlag2) {
                                            ATGUpdateTime2 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp2 = state.dataHeatBalFanSys->TempTstatAir(ZoneNum);
                                            ATGUpdateFlag2 = false;
                                            if (std::abs(ATGUpdateTime2 - ATGUpdateTime1 + 24.0) > 1.e-10) {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) =
                                                    (ATGUpdateTemp1 - ATGUpdateTemp2) / (ATGUpdateTime2 - ATGUpdateTime1 + 24.0);
                                            } else {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) =
                                                    (ATGUpdateTemp1 - ATGUpdateTemp2) * state.dataGlobal->NumOfTimeStepInHour;
                                            }
                                        }
                                    }
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else if (PreStartTime < state.dataGlobal->CurrentTime || PreStartTimeTmr < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    ATGUpdateFlag1 = true;
                                    ATGUpdateFlag2 = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            }
                        } else { // Not heating nor cooling mode
                            AvailStatus = NoAction;
                            CycleOnFlag = false;
                        }
                    } else if (OptStartMgr.CtrlType == state.dataSystemAvailabilityManager->MaximumOfZoneList) {

                        NumOfZonesInList = OptStartMgr.NumOfZones;
                        ATGWCZoneNumHi = OptStartMgr.ZonePtrs(1);
                        ATGWCZoneNumLo = OptStartMgr.ZonePtrs(1);
                        if ((!allocated(state.dataHeatBalFanSys->TempTstatAir)) || (!allocated(state.dataHeatBalFanSys->ZoneThermostatSetPointLo)) ||
                            (!allocated(state.dataHeatBalFanSys->ZoneThermostatSetPointHi))) {
                            TempDiff = 0.0;
                        } else {
                            if (!CycleOnFlag) {
                                if (allocated(state.dataZoneCtrls->OccRoomTSetPointHeat) && allocated(state.dataZoneCtrls->OccRoomTSetPointCool)) {
                                    TempDiffHi = 0.0;
                                    TempDiffLo = 0.0;
                                    ATGWCZoneNumHi = OptStartMgr.ZonePtrs(1);
                                    ATGWCZoneNumLo = OptStartMgr.ZonePtrs(1);
                                    for (ZoneNum = 1; ZoneNum <= NumOfZonesInList; ++ZoneNum) {
                                        TempDiff = state.dataHeatBalFanSys->TempTstatAir(OptStartMgr.ZonePtrs(ZoneNum)) -
                                                   state.dataZoneCtrls->OccRoomTSetPointCool(OptStartMgr.ZonePtrs(ZoneNum));
                                        TempDiffHi = max(TempDiffHi, TempDiff);
                                        // Store the worse case zone number for actual temperature gradient calculation
                                        if (TempDiff == TempDiffHi) {
                                            ATGWCZoneNumHi = OptStartMgr.ZonePtrs(ZoneNum);
                                        }
                                        TempDiff = state.dataHeatBalFanSys->TempTstatAir(OptStartMgr.ZonePtrs(ZoneNum)) -
                                                   state.dataZoneCtrls->OccRoomTSetPointHeat(OptStartMgr.ZonePtrs(ZoneNum));
                                        TempDiffLo = min(TempDiffLo, TempDiff);
                                        if (TempDiff == TempDiffLo) {
                                            ATGWCZoneNumLo = OptStartMgr.ZonePtrs(ZoneNum);
                                        }
                                    }
                                } else {
                                    TempDiffHi = 0.0;
                                    TempDiffLo = 0.0;
                                }
                            }
                        }
                        // Store adaptive temperature gradients for previous days and calculate the adaptive temp gradients
                        //-----------------------------------------------------------------------------
                        if (state.dataGlobal->WarmupFlag) {
                            AdaTempGradHeat = OptStartMgr.InitTGradHeat;
                            AdaTempGradCool = OptStartMgr.InitTGradCool;
                        } else if (state.dataGlobal->DayOfSim == 1 && state.dataGlobal->BeginDayFlag) {
                            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat = OptStartMgr.InitTGradHeat;
                            AdaTempGradHeat = OptStartMgr.InitTGradHeat;
                            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool = OptStartMgr.InitTGradCool;
                            AdaTempGradCool = OptStartMgr.InitTGradCool;
                        } else {
                            if (state.dataGlobal->BeginDayFlag && FirstTimeATGFlag) {
                                FirstTimeATGFlag = false;
                                AdaTempGradHeat += state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) / NumPreDays -
                                                   state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(1) / NumPreDays;
                                AdaTempGradCool += state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) / NumPreDays -
                                                   state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(1) / NumPreDays;
                                if (FanStartTime > 0) {
                                    for (ATGCounter = 1; ATGCounter <= NumPreDays - 1; ++ATGCounter) {
                                        state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(ATGCounter) =
                                            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(ATGCounter + 1);
                                        state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(ATGCounter) =
                                            state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(ATGCounter + 1);
                                    }
                                }
                            }
                        }

                        if (state.dataGlobal->CurrentTime >= 1.0) FirstTimeATGFlag = true;
                        //------------------------------------------------------------------------------

                        if ((TempDiffHi < 0.0 && TempDiffLo < 0.0) ||
                            (std::abs(TempDiffLo) > std::abs(TempDiffHi) && TempDiffLo < 0.0)) { // Heating Mode
                            TempDiff = TempDiffLo;
                            TempDiff = std::abs(TempDiff);
                            DeltaTime = TempDiff / AdaTempGradHeat;
                            if (DeltaTime > OptStartMgr.MaxOptStartTime) {
                                DeltaTime = OptStartMgr.MaxOptStartTime;
                            }
                            PreStartTime = FanStartTime - DeltaTime;
                            if (PreStartTime < 0.0) PreStartTime = -0.1;
                            PreStartTimeTmr = FanStartTimeTmr - DeltaTime;
                            if (PreStartTimeTmr < 0.0) {
                                PreStartTimeTmr += 24.0;
                                OverNightStartFlag = true;
                            } else {
                                OverNightStartFlag = false;
                            }
                            if (!OverNightStartFlag) {
                                if (FanStartTime == 0.0 || state.dataGlobal->CurrentTime > FanStartTime) {
                                    OSReportVarFlag = true;
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                    if (state.dataGlobal->CurrentTime > FanStartTime) CycleOnFlag = false;
                                    // Calculate the current day actual temperature gradient --------------------------
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (ATGUpdateFlag1) {
                                            ATGUpdateTime1 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp1 = state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumLo);
                                            ATGUpdateFlag1 = false;
                                        }
                                        if (state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumLo) >=
                                                state.dataZoneCtrls->OccRoomTSetPointHeat(ATGWCZoneNumLo) &&
                                            ATGUpdateFlag2) {
                                            ATGUpdateTime2 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp2 = state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumLo);
                                            ATGUpdateFlag2 = false;
                                            if (std::abs(ATGUpdateTime2 - ATGUpdateTime1) > 1.e-10) {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) =
                                                    (ATGUpdateTemp2 - ATGUpdateTemp1) / (ATGUpdateTime2 - ATGUpdateTime1);
                                            } else {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) =
                                                    (ATGUpdateTemp2 - ATGUpdateTemp1) * state.dataGlobal->NumOfTimeStepInHour;
                                            }
                                        }
                                    }
                                    //---------------------------------------------------------------------------------
                                } else if (PreStartTime < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    ATGUpdateFlag1 = true;
                                    ATGUpdateFlag2 = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            } else {
                                if (FanStartTime == 0.0 ||
                                    (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime <= PreStartTimeTmr)) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    // Calculate the current day actual temperature gradient --------------------------
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (ATGUpdateFlag1) {
                                            ATGUpdateTime1 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp1 = state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumLo);
                                            ATGUpdateFlag1 = false;
                                        }
                                        if (state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumLo) >=
                                                state.dataZoneCtrls->OccRoomTSetPointHeat(ATGWCZoneNumLo) &&
                                            ATGUpdateFlag2) {
                                            ATGUpdateTime2 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp2 = state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumLo);
                                            ATGUpdateFlag2 = false;
                                            if (std::abs(ATGUpdateTime2 - ATGUpdateTime1 + 24.0) > 1.e-10) {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) =
                                                    (ATGUpdateTemp2 - ATGUpdateTemp1) / (ATGUpdateTime2 - ATGUpdateTime1 + 24.0);
                                            } else {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat(NumPreDays) =
                                                    (ATGUpdateTemp2 - ATGUpdateTemp1) * state.dataGlobal->NumOfTimeStepInHour;
                                            }
                                        }
                                    }
                                    //---------------------------------------------------------------------------------
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                    if (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime < PreStartTimeTmr)
                                        CycleOnFlag = false;
                                } else if (PreStartTime < state.dataGlobal->CurrentTime || PreStartTimeTmr < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    ATGUpdateFlag1 = true;
                                    ATGUpdateFlag2 = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            }
                        } else if (TempDiffHi <= 0.0 && TempDiffLo >= 0.0) { // not heating and not cooling
                            AvailStatus = NoAction;
                            CycleOnFlag = false;
                            TempDiffHi = 0.0;
                            TempDiffLo = 0.0;
                        } else if (TempDiffHi < 30.0) { // Cooling Mode
                            TempDiff = TempDiffHi;
                            DeltaTime = TempDiff / AdaTempGradCool;
                            if (DeltaTime > OptStartMgr.MaxOptStartTime) {
                                DeltaTime = OptStartMgr.MaxOptStartTime;
                            }
                            PreStartTime = FanStartTime - DeltaTime;
                            if (PreStartTime < 0) PreStartTime = -0.1;
                            PreStartTimeTmr = FanStartTimeTmr - DeltaTime;
                            if (PreStartTimeTmr < 0) {
                                PreStartTimeTmr += 24.0;
                                OverNightStartFlag = true;
                            } else {
                                OverNightStartFlag = false;
                            }
                            if (!OverNightStartFlag) {
                                if (FanStartTime == 0.0 || state.dataGlobal->CurrentTime > FanStartTime) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    // Calculate the current day actual temperature gradient --------------------------
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (ATGUpdateFlag1) {
                                            ATGUpdateTime1 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp1 = state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumHi);
                                            ATGUpdateFlag1 = false;
                                        }
                                        if (state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumHi) <=
                                                state.dataZoneCtrls->OccRoomTSetPointCool(ATGWCZoneNumHi) &&
                                            ATGUpdateFlag2) {
                                            ATGUpdateTime2 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp2 = state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumHi);
                                            ATGUpdateFlag2 = false;
                                            if (std::abs(ATGUpdateTime2 - ATGUpdateTime1) > 1.e-10) {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) =
                                                    (ATGUpdateTemp1 - ATGUpdateTemp2) / (ATGUpdateTime2 - ATGUpdateTime1);
                                            } else {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) =
                                                    (ATGUpdateTemp1 - ATGUpdateTemp2) * state.dataGlobal->NumOfTimeStepInHour;
                                            }
                                        }
                                    }
                                    //---------------------------------------------------------------------------------
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else if (PreStartTime < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    ATGUpdateFlag1 = true;
                                    ATGUpdateFlag2 = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            } else {
                                if (FanStartTime == 0.0 ||
                                    (state.dataGlobal->CurrentTime > FanStartTime && state.dataGlobal->CurrentTime <= PreStartTimeTmr)) {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                } else if (CycleOnFlag) {
                                    AvailStatus = CycleOn;
                                    // Calculate the current day actual temperature gradient --------------------------
                                    if (!state.dataGlobal->WarmupFlag) {
                                        if (ATGUpdateFlag1) {
                                            ATGUpdateTime1 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp1 = state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumHi);
                                            ATGUpdateFlag1 = false;
                                        }
                                        if (state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumHi) <=
                                                state.dataZoneCtrls->OccRoomTSetPointCool(ATGWCZoneNumHi) &&
                                            ATGUpdateFlag2) {
                                            ATGUpdateTime2 = state.dataGlobal->CurrentTime;
                                            ATGUpdateTemp2 = state.dataHeatBalFanSys->TempTstatAir(ATGWCZoneNumHi);
                                            ATGUpdateFlag2 = false;
                                            if (std::abs(ATGUpdateTime2 - ATGUpdateTime1 + 24.0) > 1.e-10) {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) =
                                                    (ATGUpdateTemp1 - ATGUpdateTemp2) / (ATGUpdateTime2 - ATGUpdateTime1 + 24.0);
                                            } else {
                                                state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool(NumPreDays) =
                                                    (ATGUpdateTemp1 - ATGUpdateTemp2) * state.dataGlobal->NumOfTimeStepInHour;
                                            }
                                        }
                                    }
                                    //---------------------------------------------------------------------------------
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else if (PreStartTime < state.dataGlobal->CurrentTime || PreStartTimeTmr < state.dataGlobal->CurrentTime) {
                                    if (OSReportVarFlag) {
                                        NumHoursBeforeOccupancy = DeltaTime;
                                        OSReportVarFlag = false;
                                    }
                                    AvailStatus = CycleOn;
                                    CycleOnFlag = true;
                                    ATGUpdateFlag2 = true;
                                    ATGUpdateFlag1 = true;
                                    OptStartMgr.SetOptStartFlag(state, PriAirSysNum);
                                } else {
                                    AvailStatus = NoAction;
                                    CycleOnFlag = false;
                                    OSReportVarFlag = true;
                                }
                            }
                        } else {
                            AvailStatus = NoAction;
                            CycleOnFlag = false;
                        }
                    } else {
                        AvailStatus = NoAction;
                    }

                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->AdaptiveASHRAE) {
                    AvailStatus = NoAction;
                }
            }
        }

        OptStartMgr.AvailStatus = AvailStatus;
        OptStartMgr.NumHoursBeforeOccupancy = NumHoursBeforeOccupancy;
        OptStartMgr.TempDiffLo = TempDiffLo;
        OptStartMgr.TempDiffHi = TempDiffHi;
        OptStartMgr.ATGWCZoneNumLo = ATGWCZoneNumLo;
        OptStartMgr.ATGWCZoneNumHi = ATGWCZoneNumHi;
        OptStartMgr.CycleOnFlag = CycleOnFlag;
        OptStartMgr.ATGUpdateFlag1 = ATGUpdateFlag1;
        OptStartMgr.ATGUpdateFlag2 = ATGUpdateFlag2;
        OptStartMgr.FirstTimeATGFlag = FirstTimeATGFlag;
        OptStartMgr.OverNightStartFlag = OverNightStartFlag;
        OptStartMgr.OSReportVarFlag = OSReportVarFlag;
        if (OptStartMgr.CtrlAlgType == state.dataSystemAvailabilityManager->AdaptiveTemperatureGradient) {
            OptStartMgr.AdaTempGradTrdHeat = state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdHeat;
            OptStartMgr.AdaTempGradTrdCool = state.dataSystemAvailabilityManager->OptStart_AdaTempGradTrdCool;
            OptStartMgr.AdaTempGradHeat = AdaTempGradHeat;
            OptStartMgr.AdaTempGradCool = AdaTempGradCool;
            OptStartMgr.ATGUpdateTime1 = ATGUpdateTime1;
            OptStartMgr.ATGUpdateTime2 = ATGUpdateTime2;
            OptStartMgr.ATGUpdateTemp1 = ATGUpdateTemp1;
            OptStartMgr.ATGUpdateTemp2 = ATGUpdateTemp2;
        }
    }

    void DefineOptStartSysAvailManager::SetOptStartFlag(EnergyPlusData &state, int const AirLoopNum)
    {
        // Set the OptStartFlag true for all zones on the air loop
        auto const &thisAirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo(AirLoopNum));
        for (int counter = 1; counter <= thisAirToZoneNodeInfo.NumZonesCooled; ++counter) {
            int actZoneNum = state.dataZoneEquip->ZoneEquipConfig(thisAirToZoneNodeInfo.CoolCtrlZoneNums(counter)).ActualZoneNum;
            state.dataHVACGlobal->OptStartData.OptStartFlag(actZoneNum) = true;
        }
        for (int counter = 1; counter <= thisAirToZoneNodeInfo.NumZonesHeated; ++counter) {
            int actZoneNum = state.dataZoneEquip->ZoneEquipConfig(thisAirToZoneNodeInfo.HeatCtrlZoneNums(counter)).ActualZoneNum;
            state.dataHVACGlobal->OptStartData.OptStartFlag(actZoneNum) = true;
        }
    }
    void CalcNVentSysAvailMgr(EnergyPlusData &state,
                              int const SysAvailNum,           // number of the current scheduled system availability manager
                              int const PriAirSysNum,          // number of the primary air system affected by this Avail. Manager
                              int &AvailStatus,                // System status indicator
                              Optional_int_const ZoneEquipType // Type of zone equipment component
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Fred Buhl
        //       DATE WRITTEN   December 2004
        //       MODIFIED       March 2011, Chandan Sharma - FSEC: Allowed night ventilation
        //                             availability manager to work for zone component
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a primary air loop and ZoneHVAC component and sets a specified flow
        // rate fraction for the air loop for use during night ventilation.

        // METHODOLOGY EMPLOYED:
        // Looks at outside and indoor conditions to determine if night ventilation
        // is beneficial. If it is and it is scheduled on the AvailStatus is set to cycle
        // on and the loop flow rate fractionis set to the specified night ventilation
        // value.

        using namespace DataAirLoop;

        int ZoneInSysNum;
        int CtrldZoneNum;
        int ZoneNum;
        bool TempCheck;     // TRUE if one zone's temperature is above the value of the vent temp sched
        bool DelTCheck;     // TRUE if the control zone temperature - outside temperature > VentDelT
        bool LowLimCheck;   // TRUE if one zones's air temperature is below this value
        Real64 VentTemp;    // value of the ventilation temperature schedule
        int ControlZoneNum; // actual zone number of the control zone

        TempCheck = false;
        DelTCheck = false;
        LowLimCheck = false;
        // check if night venting allowed: not allowed if avail sched is off or fan sched is on
        // CR 7913 changed to allow during warmup
        if ((GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).SchedPtr) <= 0.0) ||
            (GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).FanSchedPtr) > 0.0)) {
            AvailStatus = NoAction;
        } else {

            VentTemp = GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentTempSchedPtr);
            ControlZoneNum = state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).ZoneNum;

            if (present(ZoneEquipType)) {
                // if the room temperature is greater than the vent temp sched value, set the vent temp check to TRUE
                if (state.dataHeatBalFanSys->TempTstatAir(ControlZoneNum) > VentTemp) {
                    TempCheck = true;
                }
                // if the room temperature is less than the low limit set the low limit check to TRUE
                if (state.dataHeatBalFanSys->TempTstatAir(ControlZoneNum) <
                    state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentTempLowLim) {
                    LowLimCheck = true;
                }
            } else {
                for (ZoneInSysNum = 1; ZoneInSysNum <= state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).NumZonesCooled;
                     ++ZoneInSysNum) { // loop over zones in system

                    CtrldZoneNum = state.dataAirLoop->AirToZoneNodeInfo(PriAirSysNum).CoolCtrlZoneNums(ZoneInSysNum);
                    ZoneNum = state.dataZoneEquip->ZoneEquipConfig(CtrldZoneNum).ActualZoneNum;
                    // if the room temperature is greater than the vent temp sched value, set the vent temp check to TRUE
                    if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) > VentTemp) {
                        TempCheck = true;
                    }
                    // if the room temperature is less than the low limit set the low limit check to TRUE
                    if (state.dataHeatBalFanSys->TempTstatAir(ZoneNum) <
                        state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentTempLowLim) {
                        LowLimCheck = true;
                    }
                }
            }
            // If the difference between the control zone temperature and the outside temperature is greater than
            // the specified night venting delta T then set the delta T check to TRUE
            if ((state.dataHeatBalFanSys->TempTstatAir(ControlZoneNum) - state.dataEnvrn->OutDryBulbTemp) >
                state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentDelT) {
                DelTCheck = true;
            }
            // If the limit requirements are met turn on night ventilation
            if (TempCheck && DelTCheck && !LowLimCheck) {
                AvailStatus = CycleOn;
            } else {
                AvailStatus = NoAction;
            }
        }

        if (!present(ZoneEquipType)) {
            if (AvailStatus == CycleOn) {
                state.dataAirLoop->AirLoopControlInfo(PriAirSysNum).LoopFlowRateSet = true;
                state.dataAirLoop->AirLoopControlInfo(PriAirSysNum).NightVent = true;
                state.dataAirLoop->AirLoopFlow(PriAirSysNum).ReqSupplyFrac =
                    state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).VentFlowFrac;
            }
        }

        state.dataSystemAvailabilityManager->NVentSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
    }

    void CalcDiffTSysAvailMgr(EnergyPlusData &state,
                              int const SysAvailNum,    // Number of the current scheduled system availability manager
                              int const PreviousStatus, // System status for the previous timestep
                              int &AvailStatus          // System status indicator
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   February 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

        // METHODOLOGY EMPLOYED:

        Real64 DeltaTemp;

        DeltaTemp = state.dataLoopNodes->Node(state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).HotNode).Temp -
                    state.dataLoopNodes->Node(state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).ColdNode).Temp;

        if (DeltaTemp >= state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).TempDiffOn) {
            AvailStatus = CycleOn;
        } else if (DeltaTemp <= state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).TempDiffOff) {
            AvailStatus = ForceOff;
        } else {

            if (PreviousStatus == NoAction) {
                AvailStatus = ForceOff;
            } else {
                AvailStatus = PreviousStatus; // No change, but not "NoAction"; it should always be on or off.
            }
        }

        state.dataSystemAvailabilityManager->DiffTSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
    }

    void CalcHiTurnOffSysAvailMgr(EnergyPlusData &state,
                                  int const SysAvailNum, // Number of the current scheduled system availability manager
                                  int &AvailStatus       // System status indicator
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   February 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

        if (state.dataLoopNodes->Node(state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).Node).Temp >=
            state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).Temp) {
            AvailStatus = ForceOff;
        } else {
            AvailStatus = NoAction;
        }

        state.dataSystemAvailabilityManager->HiTurnOffSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
    }

    void CalcHiTurnOnSysAvailMgr(EnergyPlusData &state,
                                 int const SysAvailNum, // Number of the current scheduled system availability manager
                                 int &AvailStatus       // System status indicator
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   February 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

        if (state.dataLoopNodes->Node(state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).Node).Temp >=
            state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).Temp) {
            AvailStatus = CycleOn;
        } else {
            AvailStatus = NoAction;
        }

        state.dataSystemAvailabilityManager->HiTurnOnSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
    }

    void CalcLoTurnOffSysAvailMgr(EnergyPlusData &state,
                                  int const SysAvailNum, // Number of the current scheduled system availability manager
                                  int &AvailStatus       // System status indicator
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   February 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

        // If applicability schedule is off, then availability manager is inactive, return no action
        if (state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).SchedPtr > 0) {
            if (GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).SchedPtr) <= 0.0) {
                AvailStatus = NoAction;
                state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
                return;
            }
        }

        // Availability manager is active, check temperature limit
        if (state.dataLoopNodes->Node(state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).Node).Temp <=
            state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).Temp) {
            AvailStatus = ForceOff;
        } else {
            AvailStatus = NoAction;
        }

        state.dataSystemAvailabilityManager->LoTurnOffSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
    }

    void CalcLoTurnOnSysAvailMgr(EnergyPlusData &state,
                                 int const SysAvailNum, // Number of the current scheduled system availability manager
                                 int &AvailStatus       // System status indicator
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Peter Graham Ellis
        //       DATE WRITTEN   February 2004
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

        if (state.dataLoopNodes->Node(state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).Node).Temp <=
            state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).Temp) {
            AvailStatus = CycleOn;
        } else {
            AvailStatus = NoAction;
        }

        state.dataSystemAvailabilityManager->LoTurnOnSysAvailMgrData(SysAvailNum).AvailStatus = AvailStatus;
    }

    int ValidateAndSetSysAvailabilityManagerType(EnergyPlusData &state, std::string const &AvailMgrName) // name to validate
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   April 2005
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function returns true for a valid System Availability Manager Type
        // and false if not.

        // Return value
        int ValidType; // result of validation

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int Found;

        Found = UtilityRoutines::FindItem(AvailMgrName,
                                          state.dataSystemAvailabilityManager->cValidSysAvailManagerTypes,
                                          state.dataSystemAvailabilityManager->NumValidSysAvailManagerTypes);
        if (Found > 0) {
            //   Hybrid ventilation must not be specified in a list
            if (state.dataSystemAvailabilityManager->ValidSysAvailManagerTypes(Found) !=
                state.dataSystemAvailabilityManager->SysAvailMgr_HybridVent) {
                ValidType = state.dataSystemAvailabilityManager->ValidSysAvailManagerTypes(Found);
            } else {
                ValidType = 0;
            }
        } else {
            ValidType = 0;
        }

        return ValidType;
    }

    void ManageHybridVentilation(EnergyPlusData &state)
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   March 2007
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Manage the simulation of the Hybrid Ventilation Control System Availability Managers

        using namespace DataLoopNode;
        using namespace DataAirLoop;

        int PriAirSysNum; // Primary Air System index
        int SysAvailNum;

        if (state.dataSystemAvailabilityManager->GetHybridInputFlag) {
            GetHybridVentilationInputs(state);
            state.dataSystemAvailabilityManager->GetHybridInputFlag = false;
        }

        if (state.dataHVACGlobal->NumHybridVentSysAvailMgrs == 0) return;

        InitHybridVentSysAvailMgr(state);

        for (SysAvailNum = 1; SysAvailNum <= state.dataHVACGlobal->NumHybridVentSysAvailMgrs; ++SysAvailNum) {
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).HybridVentMgrConnectedToAirLoop) {
                for (PriAirSysNum = 1; PriAirSysNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++PriAirSysNum) {
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum == PriAirSysNum)
                        CalcHybridVentSysAvailMgr(state, SysAvailNum, PriAirSysNum);
                }
            } else {
                // Hybrid ventilation manager is applied to zone component
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimHybridVentSysAvailMgr) {
                    CalcHybridVentSysAvailMgr(state, SysAvailNum);
                }
            }
        }
    }

    void GetHybridVentilationInputs(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   March 2007
        //       MODIFIED       L. GU, 6/23/08, Added more controls, including simple airflow objects
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Obtains input data for Hybrid Ventilation Control System Availability Managers and stores it in
        // appropriate data structures.

        // METHODOLOGY EMPLOYED:
        // Uses InputProcessor "Get" routines to obtain data.

        // Using/Aliasing
        using NodeInputManager::GetOnlySingleNode;
        using NodeInputManager::MarkNode;
        using namespace DataLoopNode;

        using CurveManager::CurveValue;
        using CurveManager::GetCurveIndex;
        using CurveManager::GetCurveMinMaxValues;

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetHybridVentilationInputs: "); // include trailing blank

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NumAlphas;           // Number of Alphas for each GetObjectItem call
        int NumNumbers;          // Number of Numbers for each GetObjectItem call
        int IOStatus;            // Used in GetObjectItem
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int SysAvailNum;         // DO loop index for all System Availability Managers
        Real64 SchedMin;         // Minimum value specified in a schedule
        Real64 SchedMax;         // Maximum value specified in a schedule
        Real64 CurveMin;         // Minimum value specified in a curve
        Real64 CurveMax;         // Maximum value specified in a curve
        Real64 CurveVal;         // Curve value

        auto &NumHybridVentSysAvailMgrs = state.dataHVACGlobal->NumHybridVentSysAvailMgrs;
        auto &HybridVentSysAvailAirLoopNum = state.dataHVACGlobal->HybridVentSysAvailAirLoopNum;
        auto &HybridVentSysAvailActualZoneNum = state.dataHVACGlobal->HybridVentSysAvailActualZoneNum;
        auto &HybridVentSysAvailVentCtrl = state.dataHVACGlobal->HybridVentSysAvailVentCtrl;
        auto &HybridVentSysAvailANCtrlStatus = state.dataHVACGlobal->HybridVentSysAvailANCtrlStatus;
        auto &HybridVentSysAvailMaster = state.dataHVACGlobal->HybridVentSysAvailMaster;
        auto &HybridVentSysAvailWindModifier = state.dataHVACGlobal->HybridVentSysAvailWindModifier;
        auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;

        // Get the number of occurrences of each type of System Availability Manager
        cCurrentModuleObject = "AvailabilityManager:HybridVentilation";
        NumHybridVentSysAvailMgrs = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        if (NumHybridVentSysAvailMgrs == 0) return;

        // Allocate the data arrays
        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData.allocate(NumHybridVentSysAvailMgrs);
        HybridVentSysAvailAirLoopNum.allocate(NumHybridVentSysAvailMgrs);
        HybridVentSysAvailActualZoneNum.allocate(NumHybridVentSysAvailMgrs);
        HybridVentSysAvailVentCtrl.allocate(NumHybridVentSysAvailMgrs);
        HybridVentSysAvailANCtrlStatus.allocate(NumHybridVentSysAvailMgrs);
        HybridVentSysAvailMaster.allocate(NumHybridVentSysAvailMgrs);
        HybridVentSysAvailWindModifier.allocate(NumHybridVentSysAvailMgrs);
        HybridVentSysAvailANCtrlStatus = 0;
        HybridVentSysAvailMaster = 0;

        for (SysAvailNum = 1; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {

            state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                     cCurrentModuleObject,
                                                                     SysAvailNum,
                                                                     state.dataIPShortCut->cAlphaArgs,
                                                                     NumAlphas,
                                                                     state.dataIPShortCut->rNumericArgs,
                                                                     NumNumbers,
                                                                     IOStatus,
                                                                     state.dataIPShortCut->lNumericFieldBlanks,
                                                                     state.dataIPShortCut->lAlphaFieldBlanks,
                                                                     state.dataIPShortCut->cAlphaFieldNames,
                                                                     state.dataIPShortCut->cNumericFieldNames);
            UtilityRoutines::IsNameEmpty(state, state.dataIPShortCut->cAlphaArgs(1), cCurrentModuleObject, ErrorsFound);
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name = state.dataIPShortCut->cAlphaArgs(1);
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MgrType =
                state.dataSystemAvailabilityManager->SysAvailMgr_HybridVent;

            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName = state.dataIPShortCut->cAlphaArgs(2);

            if (state.dataIPShortCut->lAlphaFieldBlanks(2)) { // Hybrid ventilation manager applied to zone
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).HybridVentMgrConnectedToAirLoop = false;
            }
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlZoneName = state.dataIPShortCut->cAlphaArgs(3);
            // Check zone number
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum =
                UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(3), state.dataHeatBal->Zone);
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum == 0) {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid");
                ShowContinueError(state,
                                  "not found: " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" + state.dataIPShortCut->cAlphaArgs(3) + "\".");
                ErrorsFound = true;
            }

            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr =
                GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(4));
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr == 0) {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" invalid");
                ShowContinueError(state,
                                  "not found: " + state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) + "\".");
                ErrorsFound = true;
            }

            // Check schedule values
            SchedMin = GetScheduleMinValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr);
            SchedMax = GetScheduleMaxValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr);
            if (SchedMin == 0 && SchedMax == 0) {
                ShowWarningError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) +
                                      "\" specifies control mode 0 for all entries.");
                ShowContinueError(state,
                                  "All zones using this " + state.dataIPShortCut->cAlphaFieldNames(4) + " have no hybrid ventilation control.");
            }
            if (SchedMax > 7.0) {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) +
                                      "\", the maximum schedule value should be 7. However, ");
                ShowContinueError(state, format("the maximum entered value in the schedule is {:.1T}", SchedMax));
                ErrorsFound = true;
            }
            if (SchedMin < 0.0) {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) +
                                      "the minimum schedule value should be 0. However, ");
                ShowContinueError(state, format("the minimum entered value in the schedule is {:.1T}", SchedMin));
                ErrorsFound = true;
            }
            if (SchedMax == 7.0 && !state.dataContaminantBalance->Contaminant.CO2Simulation) {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                ShowContinueError(state,
                                  state.dataIPShortCut->cAlphaFieldNames(4) + "=\"" + state.dataIPShortCut->cAlphaArgs(4) +
                                      "\", When the schedule value is 7, carbon dioxide (CO2) control is requested. ");
                ShowContinueError(state, "However, CO2 simulation is not enabled. Please use ZoneAirContaminantBalance object to simulate CO2.");
                ErrorsFound = true;
            }
            // Read use weather rain indicator
            if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "YES")) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).UseRainIndicator = true;
            } else if (UtilityRoutines::SameString(state.dataIPShortCut->cAlphaArgs(5), "NO")) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).UseRainIndicator = false;
            } else {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                ShowContinueError(
                    state, "..invalid value: " + state.dataIPShortCut->cAlphaFieldNames(5) + "=\"" + state.dataIPShortCut->cAlphaArgs(5) + "\".");
                ShowContinueError(state, "Valid choices are Yes or No.");
                ErrorsFound = true;
            }

            // Check max wind speed
            if (NumNumbers > 0) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MaxWindSpeed = state.dataIPShortCut->rNumericArgs(1);
                if (state.dataIPShortCut->rNumericArgs(1) > 40.0 || state.dataIPShortCut->rNumericArgs(1) < 0.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(1) + " is beyond the range.");
                    ShowContinueError(
                        state,
                        format("The input value is {:.0T}. The allowed value must be >= 0 and <= 40 m/s", state.dataIPShortCut->rNumericArgs(1)));
                    ErrorsFound = true;
                }
            }

            // Read Max and Min outdoor temperature
            if (NumNumbers > 1) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOutdoorTemp = state.dataIPShortCut->rNumericArgs(2);
                if (state.dataIPShortCut->rNumericArgs(2) > 100.0 || state.dataIPShortCut->rNumericArgs(2) < -100.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(2) + " is beyond the range.");
                    ShowContinueError(state,
                                      format("The input value is {:.0T}. The allowed value must be between -100 C and +100 C",
                                             state.dataIPShortCut->rNumericArgs(2)));
                    ErrorsFound = true;
                }
            }
            if (NumNumbers > 2) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MaxOutdoorTemp = state.dataIPShortCut->rNumericArgs(3);
                if (state.dataIPShortCut->rNumericArgs(3) > 100.0 || state.dataIPShortCut->rNumericArgs(3) < -100.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(3) + " is beyond the range.");
                    ShowContinueError(state,
                                      format("The input value is {:.0T}. The allowed value must be between -100 C and +100 C",
                                             state.dataIPShortCut->rNumericArgs(3)));
                    ErrorsFound = true;
                }
            }
            // Ensure MaxTemp >= MinTemp
            if (state.dataIPShortCut->rNumericArgs(2) >= state.dataIPShortCut->rNumericArgs(3)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" The " +
                                    state.dataIPShortCut->cNumericFieldNames(2) + " must be less than the " +
                                    state.dataIPShortCut->cNumericFieldNames(3));
                ShowContinueError(state,
                                  format("The {} is {:.0T}. The {} is {:.0T}.",
                                         state.dataIPShortCut->cNumericFieldNames(2),
                                         state.dataIPShortCut->rNumericArgs(2),
                                         state.dataIPShortCut->cNumericFieldNames(3),
                                         state.dataIPShortCut->rNumericArgs(3)));
                ErrorsFound = true;
            }

            // Read Max and Min outdoor enthalpy
            if (NumNumbers > 3) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOutdoorEnth = state.dataIPShortCut->rNumericArgs(4);
                if (state.dataIPShortCut->rNumericArgs(4) > 300000.0 || state.dataIPShortCut->rNumericArgs(4) < 0.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(4) + " is beyond the range.");
                    ShowContinueError(state,
                                      format("The input value is {:.0T}. The allowed value must be between 0 and 300000 J/kg",
                                             state.dataIPShortCut->rNumericArgs(4)));
                    ErrorsFound = true;
                }
            }
            if (NumNumbers > 4) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MaxOutdoorEnth = state.dataIPShortCut->rNumericArgs(5);
                if (state.dataIPShortCut->rNumericArgs(5) > 300000.0 || state.dataIPShortCut->rNumericArgs(5) < 0.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(5) + " is beyond the range.");
                    ShowContinueError(state,
                                      format("The input value is {:.0T}. The allowed value must be between 0 and 300000 J/kg",
                                             state.dataIPShortCut->rNumericArgs(5)));
                    ErrorsFound = true;
                }
            }
            // Ensure MaxEnth >= MiniEnth
            if (state.dataIPShortCut->rNumericArgs(4) >= state.dataIPShortCut->rNumericArgs(5)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" The " +
                                    state.dataIPShortCut->cNumericFieldNames(4) + " must be less than the " +
                                    state.dataIPShortCut->cNumericFieldNames(5));
                ShowContinueError(state,
                                  format("The {} is {:.0T}. The {} is {:.0T}.",
                                         state.dataIPShortCut->cNumericFieldNames(4),
                                         state.dataIPShortCut->rNumericArgs(4),
                                         state.dataIPShortCut->cNumericFieldNames(5),
                                         state.dataIPShortCut->rNumericArgs(5)));
                ErrorsFound = true;
            }

            // Read Max and Min outdoor dew point
            if (NumNumbers > 5) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOutdoorDewPoint =
                    state.dataIPShortCut->rNumericArgs(6);
                if (state.dataIPShortCut->rNumericArgs(6) > 100.0 || state.dataIPShortCut->rNumericArgs(6) < -100.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(6) + " is beyond the range.");
                    ShowContinueError(state,
                                      format("The input value is {:.0T}. The allowed value must be between -100 C and +100 C",
                                             state.dataIPShortCut->rNumericArgs(6)));
                    ErrorsFound = true;
                }
            }
            if (NumNumbers > 6) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MaxOutdoorDewPoint =
                    state.dataIPShortCut->rNumericArgs(7);
                if (state.dataIPShortCut->rNumericArgs(7) > 100.0 || state.dataIPShortCut->rNumericArgs(7) < -100.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state, state.dataIPShortCut->cNumericFieldNames(7) + " is beyond the range.");
                    ShowContinueError(state,
                                      format("The input value is {:.0T}. The allowed value must be between -100 C and +100 C",
                                             state.dataIPShortCut->rNumericArgs(7)));
                    ErrorsFound = true;
                }
            }
            // Ensure MaxTemp >= MinTemp
            if (state.dataIPShortCut->rNumericArgs(6) >= state.dataIPShortCut->rNumericArgs(7)) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\" The " +
                                    state.dataIPShortCut->cNumericFieldNames(6) + " must be less than the " +
                                    state.dataIPShortCut->cNumericFieldNames(7));
                ShowContinueError(state,
                                  format("The {} is {:.0T}. The {} is {:.0T}.",
                                         state.dataIPShortCut->cNumericFieldNames(6),
                                         state.dataIPShortCut->rNumericArgs(6),
                                         state.dataIPShortCut->cNumericFieldNames(7),
                                         state.dataIPShortCut->rNumericArgs(7)));
                ErrorsFound = true;
            }

            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOASched = state.dataIPShortCut->cAlphaArgs(6);
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOASchedPtr =
                GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(6));
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOASchedPtr == 0) {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\", invalid");
                ShowContinueError(state,
                                  "..not found: " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" + state.dataIPShortCut->cAlphaArgs(6) + "\".");
                ErrorsFound = true;
            }
            SchedMin = GetScheduleMinValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOASchedPtr);
            if (SchedMin < 0.0) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) +
                                    "\", Schedule value must be >= 0 in " + state.dataIPShortCut->cAlphaFieldNames(6) + "=\"" +
                                    state.dataIPShortCut->cAlphaArgs(6) + "\".");
                ShowContinueError(state, format("The minimum schedule value is {:.1T}", SchedMin));
                ErrorsFound = true;
            }

            if (!state.dataIPShortCut->lAlphaFieldBlanks(7)) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OpeningFactorFWS =
                    GetCurveIndex(state, state.dataIPShortCut->cAlphaArgs(7));
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OpeningFactorFWS <= 0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(
                        state, " not found: " + state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) + "\".");
                    ErrorsFound = true;
                } else {
                    GetCurveMinMaxValues(
                        state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OpeningFactorFWS, CurveMin, CurveMax);
                    if (CurveMin < 0.0) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                        ShowContinueError(state,
                                          "The minimum wind speed used in " + state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" +
                                              state.dataIPShortCut->cAlphaArgs(7) + "should be greater than or equal to 0.0 (m/s)");
                        ShowContinueError(state, "Curve minimum value appears to be less than 0.");
                        ErrorsFound = true;
                    }
                    CurveVal =
                        CurveValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OpeningFactorFWS, CurveMin);
                    if (CurveVal < 0.0) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                        ShowContinueError(state,
                                          "The minimum value of " + state.dataIPShortCut->cAlphaFieldNames(7) +
                                              " must be greater than or equal to 0.0 at the minimum value of wind speed.");
                        ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) + "\".");
                        ShowContinueError(state, format("Curve output at the minimum wind speed = {:.3T}", CurveVal));
                        ErrorsFound = true;
                    }
                    CurveVal =
                        CurveValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OpeningFactorFWS, CurveMax);
                    if (CurveVal > 1.0) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                        ShowContinueError(state,
                                          "The maximum value of " + state.dataIPShortCut->cAlphaFieldNames(7) +
                                              " must be less than or equal to 1.0 at the maximum value of wind speed.");
                        ShowContinueError(state, state.dataIPShortCut->cAlphaFieldNames(7) + "=\"" + state.dataIPShortCut->cAlphaArgs(7) + "\".");
                        ShowContinueError(state, format("Curve output at the maximum wind speed = {:.3T}", CurveVal));
                        ErrorsFound = true;
                    }
                    // Check curve type
                    ErrorsFound |= CurveManager::CheckCurveDims(
                        state,
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OpeningFactorFWS, // Curve index
                        {1},                                                                                          // Valid dimensions
                        RoutineName,                                                                                  // Routine name
                        cCurrentModuleObject,                                                                         // Object Type
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name,             // Object Name
                        state.dataIPShortCut->cAlphaFieldNames(7));                                                   // Field Name
                }
            }

            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ANControlTypeSchedPtr =
                GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(8));
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ANControlTypeSchedPtr > 0) {
                HybridVentSysAvailMaster(SysAvailNum) = state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum;
                // Check schedule values
                SchedMin =
                    GetScheduleMinValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ANControlTypeSchedPtr);
                SchedMax =
                    GetScheduleMaxValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ANControlTypeSchedPtr);
                HybridVentSysAvailANCtrlStatus(SysAvailNum) =
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ANControlTypeSchedPtr;
                if (SchedMax > 1.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state,
                                      " For " + state.dataIPShortCut->cAlphaFieldNames(8) + "=\"" + state.dataIPShortCut->cAlphaArgs(8) + "\",");
                    ShowContinueError(state, "the maximum schedule value should be 1. However, ");
                    ShowContinueError(state, format("the maximum entered value in the schedule is {:.1T}", SchedMax));
                    ErrorsFound = true;
                }
                if (SchedMin < 0.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state,
                                      "For " + state.dataIPShortCut->cAlphaFieldNames(8) + "=\"" + state.dataIPShortCut->cAlphaArgs(8) + "\",");
                    ShowContinueError(state, "the minimum schedule value should be 0. However, ");
                    ShowContinueError(state, format("the minimum entered value in the schedule is {:.1T}", SchedMin));
                    ErrorsFound = true;
                }
            }

            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr =
                GetScheduleIndex(state, state.dataIPShortCut->cAlphaArgs(9));
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0 &&
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ANControlTypeSchedPtr > 0) {
                ShowWarningError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                ShowContinueError(state,
                                  "The inputs for" + state.dataIPShortCut->cAlphaFieldNames(8) + " and " + state.dataIPShortCut->cAlphaFieldNames(9) +
                                      " are valid.");
                ShowContinueError(state, "But both objects cannot work at the same time. The Simple Airflow Control is disabled");
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr = 0;
            } else if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0) {
                // Check schedule values
                SchedMin =
                    GetScheduleMinValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr);
                SchedMax =
                    GetScheduleMaxValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr);
                if (SchedMax > 1.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state,
                                      "For " + state.dataIPShortCut->cAlphaFieldNames(9) + "=\"" + state.dataIPShortCut->cAlphaArgs(9) + "\",");
                    ShowContinueError(state, "the maximum schedule value should be 1. However, ");
                    ShowContinueError(state, format("the maximum entered value in the schedule is {:.1T}", SchedMax));
                    ErrorsFound = true;
                }
                if (SchedMin < 0.0) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(state,
                                      "For " + state.dataIPShortCut->cAlphaFieldNames(9) + "=\"" + state.dataIPShortCut->cAlphaArgs(9) + "\",");
                    ShowContinueError(state, "the minimum schedule value should be 0. However, ");
                    ShowContinueError(state, format("the minimum entered value in the schedule is {:.1T}", SchedMin));
                    ErrorsFound = true;
                }
            }

            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationName = state.dataIPShortCut->cAlphaArgs(10);
                if (state.dataHeatBal->TotVentilation > 0) {
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr =
                        UtilityRoutines::FindItemInList(state.dataIPShortCut->cAlphaArgs(10), state.dataHeatBal->Ventilation);
                    HybridVentSysAvailMaster(SysAvailNum) =
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr;
                    SchedMax = GetScheduleMaxValue(
                        state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr);
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr <= 0 && int(SchedMax) == 1) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                        ShowContinueError(state,
                                          state.dataIPShortCut->cAlphaFieldNames(10) + "=\"" + state.dataIPShortCut->cAlphaArgs(10) +
                                              "\" is required and not found.");
                        ErrorsFound = true;
                    } // Otherwise check later
                }
            }

            // Check simple airflow object
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0 &&
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr > 0) {
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum !=
                    state.dataHeatBal->Ventilation(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr)
                        .ZonePtr) {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                    ShowContinueError(
                        state,
                        "The Zone name specified in the Ventilation object " +
                            state.dataHeatBal
                                ->Zone(state.dataHeatBal
                                           ->Ventilation(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr)
                                           .ZonePtr)
                                .Name);
                    ShowContinueError(state,
                                      "is not equal to the " + state.dataIPShortCut->cAlphaFieldNames(3) + "=\"" +
                                          state.dataIPShortCut->cAlphaArgs(3) + "\".");
                    ErrorsFound = true;
                }
            }

            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0 &&
                state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlSimple) {
                ShowSevereError(state,
                                RoutineName + cCurrentModuleObject + "=\"" +
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name + "\"");
                ShowContinueError(state, "The simple airflow objects are used for natural ventilation calculation.");
                ShowContinueError(state,
                                  "The Airflow Network model is not allowed to perform. Please set the control type = NoMultizoneOrDistribution");
                ErrorsFound = true;
            }

            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr == 0) {
                if (state.dataAirflowNetwork->SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                    ShowWarningError(state,
                                     RoutineName + cCurrentModuleObject + "=\"" +
                                         state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name + "\"");
                    ShowContinueError(state, "The Airflow Network model is not available for Hybrid Ventilation Control.");
                } else if (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS) {
                    ShowWarningError(state,
                                     RoutineName + cCurrentModuleObject + "=\"" +
                                         state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name + "\"");
                    ShowContinueError(state, "Please check the AirflowNetwork Control field in the AirflowNetwork:SimulationControl object.");
                    ShowContinueError(state, "The suggested choices are MultizoneWithDistribution or MultizoneWithoutDistribution.");
                }
            }

            // Disallow combination of simple control and OA control mode
            SchedMax = GetScheduleMaxValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr);
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0 && SchedMax == 4.0) {
                ShowSevereError(state, RoutineName + cCurrentModuleObject + "=\"" + state.dataIPShortCut->cAlphaArgs(1) + "\"");
                ShowContinueError(state,
                                  "The outdoor ventilation air control type defined in " + state.dataIPShortCut->cAlphaArgs(4) +
                                      " cannot work together with " + state.dataIPShortCut->cAlphaFieldNames(9));
                ErrorsFound = true;
            }

            if (!state.dataIPShortCut->lNumericFieldBlanks(8)) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOperTime = state.dataIPShortCut->rNumericArgs(8);
            }
            if (!state.dataIPShortCut->lNumericFieldBlanks(9)) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinVentTime = state.dataIPShortCut->rNumericArgs(9);
            }

        } // SysAvailNum

        if (NumHybridVentSysAvailMgrs > 1) {
            for (SysAvailNum = 2; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum - 1).ANControlTypeSchedPtr > 0) {
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0) {
                        ShowSevereError(state,
                                        "The AirflowNetwork model is used for natural ventilation calculation in " + cCurrentModuleObject + "=\"" +
                                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum - 1).Name + "\"");
                        ShowContinueError(state,
                                          "The simple airflow objects are used for natural ventilation calculation in " + cCurrentModuleObject +
                                              "=\"" + state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name + "\"");
                        ShowContinueError(state, "The hybrid ventilation control requires the same models to calculate natural ventilation");
                        ErrorsFound = true;
                    }
                }
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum - 1).SimpleControlTypeSchedPtr > 0) {
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ANControlTypeSchedPtr > 0) {
                        ShowSevereError(state,
                                        "The Airflow Network model is used for natural ventilation calculation in " + cCurrentModuleObject + "=\"" +
                                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name + "\"");
                        ShowContinueError(state,
                                          "The simple airflow objects are used for natural ventilation calculation in " + cCurrentModuleObject +
                                              "=\"" + state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum - 1).Name + "\"");
                        ShowContinueError(state, "The hybrid ventilation control requires the same models to calculate natural ventilation");
                        ErrorsFound = true;
                    }
                }
            } // SysAvailNum
        }

        if (ErrorsFound) {
            ShowFatalError(state, RoutineName + "Errors found in input.  Preceding condition(s) cause termination.");
        }

        // Set up output variables
        for (SysAvailNum = 1; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).HybridVentMgrConnectedToAirLoop) {
                SetupOutputVariable(state,
                                    "Availability Manager Hybrid Ventilation Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName);
                SetupOutputVariable(state,
                                    "Availability Manager Hybrid Ventilation Control Mode",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName);
            } else {
                SetupOutputVariable(state,
                                    "Availability Manager Hybrid Ventilation Control Status",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlZoneName);
                SetupOutputVariable(state,
                                    "Availability Manager Hybrid Ventilation Control Mode",
                                    OutputProcessor::Unit::None,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlZoneName);
            }

            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOperTime > 0) {
                SetupOutputVariable(state,
                                    "Hybrid Ventilation Control HVAC System Operation Elapsed Time",
                                    OutputProcessor::Unit::min,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeOperDuration,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name);
            }

            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinVentTime > 0) {
                SetupOutputVariable(state,
                                    "Hybrid Ventilation Control Natural Ventilation Elapsed Time",
                                    OutputProcessor::Unit::min,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeVentDuration,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name);
            }

            if (CheckScheduleValue(state,
                                   state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr,
                                   state.dataSystemAvailabilityManager->HybridVentMode_OperT80) ||
                CheckScheduleValue(state,
                                   state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr,
                                   state.dataSystemAvailabilityManager->HybridVentMode_OperT90)) {
                SetupOutputVariable(state,
                                    "Hybrid Ventilation Operative Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OperativeTemp,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name);
                SetupOutputVariable(state,
                                    "Hybrid Ventilation Lower Limit Operative Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).minAdaTem,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name);
                SetupOutputVariable(state,
                                    "Hybrid Ventilation Upper Limit Operative Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).maxAdaTem,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name);
            }

            if (CheckScheduleValue(state,
                                   state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr,
                                   state.dataSystemAvailabilityManager->HybridVentMode_CO2)) {
                SetupOutputVariable(state,
                                    "Hybrid Ventilation CO2 Concentration",
                                    OutputProcessor::Unit::ppm,
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).CO2,
                                    "System",
                                    "Average",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name);
            }
        }
    }

    void InitHybridVentSysAvailMgr(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   March 2007
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine is for initializations of the Hybrid Ventilation Control System Availability Manager

        // METHODOLOGY EMPLOYED:
        // Uses the status flags to trigger initializations.

        // Using/Aliasing
        using DataZoneEquipment::NumValidSysAvailZoneComponents;

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        auto &MyOneTimeFlag = state.dataSystemAvailabilityManager->MyOneTimeFlag;
        auto &MyEnvrnFlag = state.dataSystemAvailabilityManager->MyEnvrnFlag;
        int SysAvailNum;         // DO loop index for Sys Avail Manager objects
        int ControlledZoneNum;   // Index into the ZoneEquipConfig array
        bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine
        int AirLoopNum;          // Air loop number
        int ControlMode;         // Hybrid control mode
        int AirLoopCount;        // Air loop name count
        Real64 SchedMax;         // Maximum value specified in a schedule
        int SysAvailIndex;       // Hybrid Ventilation Sys Avail Manager index
        int ZoneEquipType;
        int HybridVentNum;

        auto &NumHybridVentSysAvailMgrs = state.dataHVACGlobal->NumHybridVentSysAvailMgrs;
        auto &HybridVentSysAvailAirLoopNum = state.dataHVACGlobal->HybridVentSysAvailAirLoopNum;
        auto &HybridVentSysAvailActualZoneNum = state.dataHVACGlobal->HybridVentSysAvailActualZoneNum;
        auto &HybridVentSysAvailVentCtrl = state.dataHVACGlobal->HybridVentSysAvailVentCtrl;
        auto &HybridVentSysAvailMaster = state.dataHVACGlobal->HybridVentSysAvailMaster;
        auto &HybridVentSysAvailWindModifier = state.dataHVACGlobal->HybridVentSysAvailWindModifier;
        auto &ZoneComp = state.dataHVACGlobal->ZoneComp;

        // One time initializations
        if (MyOneTimeFlag && allocated(state.dataZoneEquip->ZoneEquipConfig) && allocated(state.dataAirSystemsData->PrimaryAirSystems)) {

            // Ensure the controlled zone is listed and defined in an HVAC Air Loop
            for (SysAvailNum = 1; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0 &&
                    state.dataHeatBal->TotVentilation > 0 &&
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr == 0) {
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr = UtilityRoutines::FindItemInList(
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationName, state.dataHeatBal->Ventilation);
                    HybridVentSysAvailMaster(SysAvailNum) =
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr;
                    SchedMax = GetScheduleMaxValue(
                        state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr);
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr <= 0 && int(SchedMax) == 1) {
                        ShowSevereError(state,
                                        "ZoneVentilation Object Name=\"" +
                                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationName +
                                            "\" is required and not found.");
                        ShowContinueError(state,
                                          "Occurs in AvailabilityManager:HybridVentilation=\"" +
                                              state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name + "\".");
                        ErrorsFound = true;
                    }
                }
                // Check air loop number
                for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) { // loop over the primary air systems
                    if (UtilityRoutines::SameString(state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name,
                                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName)) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum = AirLoopNum;
                    }
                }
                HybridVentSysAvailAirLoopNum(SysAvailNum) = state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum;
                HybridVentSysAvailActualZoneNum(SysAvailNum) =
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum;

                // set the controlled zone numbers
                for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                    if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum ==
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlledZoneNum = ControlledZoneNum;
                        bool zoneFound = false;
                        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).HybridVentMgrConnectedToAirLoop) {
                            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlledZoneNum > 0) {
                                for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++zoneInNode) {
                                    if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode) ==
                                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum) {
                                        zoneFound = true;
                                        break;
                                    }
                                }
                                if (!zoneFound) {
                                    ShowSevereError(state,
                                                    state.dataSystemAvailabilityManager->cValidSysAvailManagerTypes(
                                                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MgrType) +
                                                        ", The controlled zone =" +
                                                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlZoneName +
                                                        " is not served by this Air Loop=" +
                                                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName);
                                    ErrorsFound = true;
                                }
                            }
                            break;
                        }
                    }
                    if (std::any_of(
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData.begin(),
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData.end(),
                            [](SystemAvailabilityManager::DefineHybridVentSysAvailManager const &e) { return e.HybridVentMgrConnectedToAirLoop; })) {
                        for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                            if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode) ==
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum &&
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum > 0) {
                                for (HybridVentNum = 1; HybridVentNum <= NumHybridVentSysAvailMgrs; ++HybridVentNum) {
                                    if (!state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum)
                                             .HybridVentMgrConnectedToAirLoop &&
                                        (HybridVentNum != SysAvailNum)) {
                                        if (state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum ==
                                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum).ActualZoneNum &&
                                            state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum > 0) {
                                            ShowWarningError(
                                                state,
                                                "AvailabilityManager:HybridVentilation = \"" +
                                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum).Name +
                                                    "\" has the controlled zone name = \"" +
                                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum).ControlZoneName +
                                                    "\".");
                                            ShowContinueError(
                                                state,
                                                "This controlled zone already has hybrid ventilation control through this air loop = \"" +
                                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName + "\".");
                                            ShowContinueError(state,
                                                              "Only AvailabilityManager:HybridVentilation = \"" +
                                                                  state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name +
                                                                  "\" will be simulated. Simulation continues...");
                                        } else {
                                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(HybridVentNum).SimHybridVentSysAvailMgr =
                                                true;
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        for (auto &e : state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData)
                            e.SimHybridVentSysAvailMgr = true;
                    }
                }

                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlledZoneNum == 0) {
                    ShowSevereError(state,
                                    state.dataSystemAvailabilityManager->cValidSysAvailManagerTypes(
                                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MgrType) +
                                        ", The controlled zone is not defined correctly =" +
                                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlZoneName);
                    ErrorsFound = true;
                }
                // check schedule value for adaptive temperature control
                if (CheckScheduleValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr, 5.0) ||
                    CheckScheduleValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr, 6.0)) {
                    if (!state.dataHeatBal->AdaptiveComfortRequested_ASH55) {
                        ShowSevereError(state,
                                        "GetHybridVentilationInputs: AvailabilityManager:HybridVentilation =\"" +
                                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).Name + "\"");
                        ShowContinueError(
                            state,
                            "Ventilation Control Mode Schedule Name =\"" +
                                state.dataScheduleMgr
                                    ->Schedule(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr)
                                    .Name +
                                "\", When the schedule value is 5 or 6, operative temperature control is requested. ");
                        ShowContinueError(state,
                                          "However, AdaptiveASH55 is not entered in the Thermal Comfort Model Type fields in the People object.");
                        ErrorsFound = true;
                    }
                }
            }

            // Ensure an airloop name is not used more than once in the hybrid ventilation control objects
            for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) { // loop over the primary air systems
                AirLoopCount = 0;
                for (SysAvailNum = 1; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {
                    if (UtilityRoutines::SameString(state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name,
                                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName)) {
                        ++AirLoopCount;
                        if (AirLoopCount > 1) SysAvailIndex = SysAvailNum;
                    }
                }
                if (AirLoopCount > 1) {
                    ShowSevereError(
                        state,
                        state.dataSystemAvailabilityManager->cValidSysAvailManagerTypes(
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailIndex).MgrType) +
                            ", The AirLoopHVAC name found more than once=" + state.dataAirSystemsData->PrimaryAirSystems(AirLoopNum).Name);
                    ShowContinueError(state, "Each AirLoopHVAC allows one hybrid ventilation control object.");
                    ErrorsFound = true;
                }
            }

            if (ErrorsFound) {
                ShowFatalError(state, "Errors found in getting AvailabilityManager:* inputs");
            }

            MyOneTimeFlag = false;

        } // end 1 time initializations

        for (SysAvailNum = 1; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {
            ControlMode =
                GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlModeSchedPtr);
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode = ControlMode;
            // -1 means that the value will be determined inside CalcHybridVentSysAvailMgr.
            // IF the value is still -1, the program will stop.
            HybridVentSysAvailVentCtrl(SysAvailNum) = -1;
            HybridVentSysAvailWindModifier(SysAvailNum) = -1.0;
        }

        if (allocated(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData))
            for (auto &e : state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData)
                e.AvailStatus = NoAction;

        for (ZoneEquipType = 1; ZoneEquipType <= NumValidSysAvailZoneComponents; ++ZoneEquipType) { // loop over the zone equipment types
            if (allocated(ZoneComp)) {
                if (ZoneComp(ZoneEquipType).TotalNumComp > 0)
                    for (auto &e : ZoneComp(ZoneEquipType).ZoneCompAvailMgrs)
                        e.AvailStatus = NoAction;
            }
        }

        if (state.dataGlobal->BeginEnvrnFlag && MyEnvrnFlag) {
            for (SysAvailNum = 1; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeVentDuration = 0.0;
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeOperDuration = 0.0;
            }
            MyEnvrnFlag = false;
        }
        if (!state.dataGlobal->BeginEnvrnFlag) {
            MyEnvrnFlag = true;
        }
        // check minimum operation time
        state.dataSystemAvailabilityManager->CurrentEndTime = state.dataGlobal->CurrentTime + state.dataHVACGlobal->SysTimeElapsed;
        if (state.dataSystemAvailabilityManager->CurrentEndTime > state.dataSystemAvailabilityManager->CurrentEndTimeLast &&
            state.dataHVACGlobal->TimeStepSys >= state.dataSystemAvailabilityManager->TimeStepSysLast) {
            for (SysAvailNum = 1; SysAvailNum <= NumHybridVentSysAvailMgrs; ++SysAvailNum) {
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                    state.dataSystemAvailabilityManager->HybridVentCtrl_NoAction) {
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeOperDuration = 0.0;
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeVentDuration = 0.0;
                }
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinVentTime > 0.0) {
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                        state.dataSystemAvailabilityManager->HybridVentCtrl_Open) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeVentDuration +=
                            (state.dataSystemAvailabilityManager->CurrentEndTime - state.dataSystemAvailabilityManager->CurrentEndTimeLast) * 60.0;
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeOperDuration = 0.0;
                    }
                }
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOperTime > 0.0) {
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                        state.dataSystemAvailabilityManager->HybridVentCtrl_Close) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeOperDuration +=
                            (state.dataSystemAvailabilityManager->CurrentEndTime - state.dataSystemAvailabilityManager->CurrentEndTimeLast) * 60.0;
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeVentDuration = 0.0;
                    }
                }
            }
        }
        state.dataSystemAvailabilityManager->TimeStepSysLast = state.dataHVACGlobal->TimeStepSys;
        state.dataSystemAvailabilityManager->CurrentEndTimeLast = state.dataSystemAvailabilityManager->CurrentEndTime;
    }

    void CalcHybridVentSysAvailMgr(EnergyPlusData &state,
                                   int const SysAvailNum,          // number of the current scheduled system availability manager
                                   Optional_int_const PriAirSysNum // number of the primary air system affected by this Avail. Manager
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   March 2007
        //       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // Set AvailStatus indicator for a primary air loop and AirflowNetwork model to prevent
        // windows or doors open during HVAC system operation

        // METHODOLOGY EMPLOYED:
        // Looks at outside and indoor conditions to determine if hybrid ventilation
        // is beneficial. If it is and it is scheduled on the AvailStatus is set to cycle
        // on and open windows or doors.

        using namespace DataAirLoop;
        using AirflowNetworkBalanceManager::GetZoneOutdoorAirChangeRate;
        using AirflowNetworkBalanceManager::ManageAirflowNetworkBalance;
        using CurveManager::CurveValue;
        using DataHeatBalance::HybridControlTypeClose;
        using DataHeatBalance::HybridControlTypeGlobal;
        using DataHeatBalance::HybridControlTypeIndiv;
        using DataZoneEquipment::NumValidSysAvailZoneComponents;
        using Psychrometrics::PsyHFnTdbW;
        using Psychrometrics::PsyRhFnTdbWPb;
        using Psychrometrics::PsyTdpFnWPb;
        using Psychrometrics::PsyWFnTdbRhPb;

        int ZoneNum;                        // actual zone number of the control zone
        int ControlMode;                    // Hybrid control mode
        int HStatZoneNum;                   // Humidity control zone number
        Real64 ZoneAirEnthalpy;             // Zone air enthalpy
        Real64 ZoneAirDewPoint;             // Zone air dew point temperature
        Real64 ZoneAirRH;                   // Zone air relative humidity
        Real64 TempExt;                     // Outdoor dry bulb temperature at zone height
        Real64 WindExt;                     // Outdoor wind speed at zone height
        Real64 WSetPoint;                   // Humidity ratio setpoint from a given RH setpoint schedule
        Real64 OASetPoint;                  // Outdoor air setpoint from a given OA setpoint schedule
        Real64 ACH;                         // Zone air change per hour
        bool found;                         // Used for humidistat object
        bool HybridVentModeOA;              // USed to check whether HybridVentModeOA is allowed
        Real64 ZoneRHHumidifyingSetPoint;   // Zone humidifying setpoint (%)
        Real64 ZoneRHDehumidifyingSetPoint; // Zone dehumidifying setpoint (%)
        int ControlledZoneNum;              // Index into the ZoneEquipConfig array
        int SimpleControlType;              // Simple control type from a schedule: 0 individual, 1 global
        int i;                              // Array index
        Real64 minAdaTem;                   // minimum adaptive temperature for adaptive temperature control
        Real64 maxAdaTem;                   // maximum adaptive temperature for adaptive temperature control
        bool KeepStatus;                    // true, if minimum time operation is needed
        int ZoneEquipType;
        int ZoneCompNum;
        int AirLoopNum;
        int Num;
        int AvailStatus;

        KeepStatus = false;
        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeVentDuration > 0.0 &&
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeVentDuration <=
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinVentTime) {
            KeepStatus = true;
        }
        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeOperDuration > 0.0 &&
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).TimeOperDuration <=
                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOperTime) {
            KeepStatus = true;
        }

        ControlMode = state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode;

        ZoneNum = state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum;
        if (!KeepStatus)
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                state.dataSystemAvailabilityManager->HybridVentCtrl_NoAction;
        TempExt = state.dataHeatBal->Zone(ZoneNum).OutDryBulbTemp;
        WindExt = state.dataHeatBal->Zone(ZoneNum).WindSpeed;
        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OperativeTemp = 0.0;
        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).minAdaTem = 0.0;
        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).maxAdaTem = 0.0;

        if (!KeepStatus) {
            {
                auto const SELECT_CASE_var(ControlMode);

                if (SELECT_CASE_var == state.dataSystemAvailabilityManager->HybridVentMode_No) {
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                        state.dataSystemAvailabilityManager->HybridVentCtrl_NoAction;

                    // Temperature control
                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->HybridVentMode_Temp) {
                    if (TempExt >= state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOutdoorTemp &&
                        TempExt <= state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MaxOutdoorTemp) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Open;
                    } else {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                    }

                    // Enthalpy control
                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->HybridVentMode_Enth) {
                    ZoneAirEnthalpy = PsyHFnTdbW(state.dataHeatBalFanSys->MAT(ZoneNum), state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum));
                    if (state.dataEnvrn->OutEnthalpy >= state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOutdoorEnth &&
                        state.dataEnvrn->OutEnthalpy <= state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MaxOutdoorEnth) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Open;
                    } else {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                    }

                    // Dew point control
                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->HybridVentMode_DewPoint) {
                    if (state.dataEnvrn->OutDewPointTemp >=
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOutdoorDewPoint &&
                        state.dataEnvrn->OutDewPointTemp <=
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MaxOutdoorDewPoint) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Open;
                    } else {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                    }

                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->HybridVentMode_OA) {
                    OASetPoint =
                        GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MinOASchedPtr);
                    ACH = 0.0;
                    HybridVentModeOA = true;
                    if (!state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).HybridVentMgrConnectedToAirLoop) {
                        if (state.dataAirflowNetwork->SimulateAirflowNetwork <= AirflowNetwork::AirflowNetworkControlSimple) {
                            HybridVentModeOA = false;
                        }
                    }

                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ANControlTypeSchedPtr > 0 && HybridVentModeOA) {
                        ManageAirflowNetworkBalance(state, true);
                        ACH = GetZoneOutdoorAirChangeRate(state, ZoneNum);
                    }
                    if (ACH > OASetPoint) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Open;
                    } else {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                    }

                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->HybridVentMode_OperT80) {
                    if (state.dataThermalComforts->runningAverageASH >= 10.0 && state.dataThermalComforts->runningAverageASH <= 33.5) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OperativeTemp =
                            0.5 * (state.dataHeatBalFanSys->MAT(ZoneNum) + state.dataHeatBal->ZoneMRT(ZoneNum));
                        minAdaTem = 0.31 * state.dataThermalComforts->runningAverageASH + 14.3;
                        maxAdaTem = 0.31 * state.dataThermalComforts->runningAverageASH + 21.3;
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).minAdaTem = minAdaTem;
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).maxAdaTem = maxAdaTem;
                        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OperativeTemp <= maxAdaTem &&
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OperativeTemp >= minAdaTem) {
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                state.dataSystemAvailabilityManager->HybridVentCtrl_Open;
                        } else {
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                        }
                    } else {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                    }

                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->HybridVentMode_OperT90) {
                    if (state.dataThermalComforts->runningAverageASH >= 10.0 && state.dataThermalComforts->runningAverageASH <= 33.5) {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OperativeTemp =
                            0.5 * (state.dataHeatBalFanSys->MAT(ZoneNum) + state.dataHeatBal->ZoneMRT(ZoneNum));
                        minAdaTem = 0.31 * state.dataThermalComforts->runningAverageASH + 15.3;
                        maxAdaTem = 0.31 * state.dataThermalComforts->runningAverageASH + 20.3;
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).minAdaTem = minAdaTem;
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).maxAdaTem = maxAdaTem;
                        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OperativeTemp <= maxAdaTem &&
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OperativeTemp >= minAdaTem) {
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                state.dataSystemAvailabilityManager->HybridVentCtrl_Open;
                        } else {
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                        }
                    } else {
                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                            state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                    }

                } else if (SELECT_CASE_var == state.dataSystemAvailabilityManager->HybridVentMode_CO2) {
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).CO2 =
                        state.dataContaminantBalance->ZoneAirCO2(ZoneNum);
                    if (state.dataContaminantBalance->ZoneAirCO2(ZoneNum) > state.dataContaminantBalance->ZoneCO2SetPoint(ZoneNum)) {
                        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).HybridVentMgrConnectedToAirLoop) {
                            AirLoopNum = state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum;
                            for (Num = 1; Num <= state.dataAirLoop
                                                     ->PriAirSysAvailMgr(
                                                         state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum)
                                                     .NumAvailManagers;
                                 ++Num) {
                                SimSysAvailManager(state,
                                                   state.dataAirLoop->PriAirSysAvailMgr(AirLoopNum).AvailManagerType(Num),
                                                   state.dataAirLoop->PriAirSysAvailMgr(AirLoopNum).AvailManagerName(Num),
                                                   state.dataAirLoop->PriAirSysAvailMgr(AirLoopNum).AvailManagerNum(Num),
                                                   AirLoopNum,
                                                   state.dataAirLoop->PriAirSysAvailMgr(AirLoopNum).AvailStatus,
                                                   AvailStatus);
                            }
                            if (AvailStatus == CycleOn) {
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                    state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                            } else {
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                    state.dataSystemAvailabilityManager->HybridVentCtrl_Open;
                            }
                        } else if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimHybridVentSysAvailMgr) {
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                state.dataSystemAvailabilityManager->HybridVentCtrl_Open;
                            for (ZoneEquipType = 1; ZoneEquipType <= NumValidSysAvailZoneComponents; ++ZoneEquipType) {
                                for (ZoneCompNum = 1; ZoneCompNum <= state.dataHVACGlobal->ZoneComp(ZoneEquipType).TotalNumComp; ++ZoneCompNum) {
                                    if (state.dataHVACGlobal->ZoneComp(ZoneEquipType).ZoneCompAvailMgrs(ZoneCompNum).AvailStatus == CycleOn) {
                                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                            state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                                        break;
                                    }
                                }
                            }
                        } else {
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                state.dataSystemAvailabilityManager->HybridVentCtrl_Open;
                        }
                    }
                } else {
                    ShowSevereError(
                        state,
                        state.dataSystemAvailabilityManager->cValidSysAvailManagerTypes(
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MgrType) +
                            ": incorrect Control Type: " + state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName);
                    ShowFatalError(state,
                                   "Errors found in getting " +
                                       state.dataSystemAvailabilityManager->cValidSysAvailManagerTypes(
                                           state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MgrType) +
                                       " Control mode value");
                }
            }

            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                state.dataSystemAvailabilityManager->HybridVentCtrl_Open) {

                // Temperature and enthalpy control
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode ==
                        state.dataSystemAvailabilityManager->HybridVentMode_Temp ||
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode ==
                        state.dataSystemAvailabilityManager->HybridVentMode_Enth) {

                    {
                        auto const SELECT_CASE_var(state.dataHeatBalFanSys->TempControlType(ZoneNum)); // select on thermostat control

                        if (SELECT_CASE_var == SingleHeatingSetPoint) {
                            if (state.dataHeatBalFanSys->MAT(ZoneNum) < state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum)) {
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                    state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                            }

                        } else if (SELECT_CASE_var == SingleCoolingSetPoint) {
                            if (state.dataHeatBalFanSys->MAT(ZoneNum) > state.dataHeatBalFanSys->TempZoneThermostatSetPoint(ZoneNum)) {
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                    state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                            }

                        } else if (SELECT_CASE_var == SingleHeatCoolSetPoint) {
                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                            ++state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SingleHCErrCount;
                            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SingleHCErrCount < 2) {
                                ShowWarningError(state,
                                                 "Hybrid ventilation control: " +
                                                     state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName +
                                                     ": The zone temperature control type is ThermostatSetpoint:SingleHeatingOrCooling. Natural "
                                                     "ventilation is not allowed.");
                                ShowContinueErrorTimeStamp(state, "");
                            } else {
                                ShowRecurringWarningErrorAtEnd(
                                    state,
                                    "Hybrid ventilation control: " +
                                        state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName +
                                        ": No natural ventilation continues with a ThermostatSetpoint:SingleHeatingOrCooling type...",
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SingleHCErrIndex,
                                    double(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode),
                                    double(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode));
                            }

                        } else if (SELECT_CASE_var == DualSetPointWithDeadBand) {
                            if ((state.dataHeatBalFanSys->MAT(ZoneNum) < state.dataHeatBalFanSys->ZoneThermostatSetPointLo(ZoneNum)) ||
                                (state.dataHeatBalFanSys->MAT(ZoneNum) > state.dataHeatBalFanSys->ZoneThermostatSetPointHi(ZoneNum))) {
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                    state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                            }

                        } else {
                        }
                    } // end select on thermostat control
                }

                // Dew point control mode
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode ==
                    state.dataSystemAvailabilityManager->HybridVentMode_DewPoint) {
                    ZoneAirRH = PsyRhFnTdbWPb(state,
                                              state.dataHeatBalFanSys->MAT(ZoneNum),
                                              state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum),
                                              state.dataEnvrn->OutBaroPress) *
                                100.0;
                    ZoneAirDewPoint = PsyTdpFnWPb(state, state.dataHeatBalFanSys->ZoneAirHumRat(ZoneNum), state.dataEnvrn->OutBaroPress);
                    if (state.dataZoneCtrls->NumHumidityControlZones == 0) {
                        ++state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).DewPointNoRHErrCount;
                        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).DewPointNoRHErrCount < 2) {
                            ShowWarningError(state,
                                             "Hybrid ventilation control: Dew point control mode is selected, but no ZoneControl:Humidistat object=" +
                                                 state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName);
                            ShowContinueError(state, "The hybrid ventilation control is triggered by outdoor min and max dewpoint only.");
                            ShowContinueError(state, "HVAC system may turn off when outdoor dewpoint is between min and max dewpoint.");
                            ShowContinueErrorTimeStamp(state, "");
                        } else {
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                "Hybrid ventilation control: " +
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName +
                                    ": no ZoneControl:Humidistat object continues...",
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).DewPointNoRHErrIndex,
                                double(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode),
                                double(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode));
                        }
                    }
                    found = false;
                    for (HStatZoneNum = 1; HStatZoneNum <= state.dataZoneCtrls->NumHumidityControlZones; ++HStatZoneNum) {
                        if (state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).ActualZoneNum == ZoneNum) {
                            found = true;
                            ZoneRHHumidifyingSetPoint =
                                GetCurrentScheduleValue(state, state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).HumidifyingSchedIndex);
                            ZoneRHDehumidifyingSetPoint =
                                GetCurrentScheduleValue(state, state.dataZoneCtrls->HumidityControlZone(HStatZoneNum).DehumidifyingSchedIndex);
                            if (ZoneAirRH > ZoneRHDehumidifyingSetPoint) { // Need dehumidification
                                WSetPoint = PsyWFnTdbRhPb(state,
                                                          state.dataHeatBalFanSys->MAT(ZoneNum),
                                                          (ZoneRHDehumidifyingSetPoint / 100.0),
                                                          state.dataEnvrn->OutBaroPress);
                                if (WSetPoint < state.dataEnvrn->OutHumRat)
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                        state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                            } else if (ZoneAirRH < ZoneRHHumidifyingSetPoint) { // Need humidification
                                WSetPoint = PsyWFnTdbRhPb(
                                    state, state.dataHeatBalFanSys->MAT(ZoneNum), (ZoneRHHumidifyingSetPoint / 100.0), state.dataEnvrn->OutBaroPress);
                                if (WSetPoint > state.dataEnvrn->OutHumRat)
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                        state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                            } else {
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                                    state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
                            }
                        }
                    }
                    if (!found && state.dataZoneCtrls->NumHumidityControlZones > 0) {
                        ++state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).DewPointErrCount;
                        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).DewPointErrCount < 2) {
                            ShowWarningError(state,
                                             "Hybrid ventilation control: The zone for dew point control mode is different from the zone for "
                                             "ZoneControl:Humidistat=" +
                                                 state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName);
                            ShowContinueError(
                                state, "The Zone name for hybrid control is " + state.dataHeatBal->Zone(ZoneNum).Name + ". Humidistat has no impact");
                            ShowContinueError(state, "HVAC system may turn off when outdoor dewpoint is between min and max dewpoint.");
                            ShowContinueErrorTimeStamp(state, "");
                        } else {
                            ShowRecurringWarningErrorAtEnd(
                                state,
                                "Hybrid ventilation control: " +
                                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopName +
                                    " No humidistat control impact continues...",
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).DewPointErrIndex,
                                double(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode),
                                double(state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode));
                        }
                    }
                }

                // Outdoor ventilation air control mode
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ControlMode ==
                    state.dataSystemAvailabilityManager->HybridVentMode_OA) {
                }
            }
        }

        if (WindExt > state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).MaxWindSpeed) {
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
        }

        if (state.dataEnvrn->IsRain && state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).UseRainIndicator) {
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl =
                state.dataSystemAvailabilityManager->HybridVentCtrl_Close;
        }
        // Sent a signal to the AirflowNetwork to ensure large onpenings are close or open based on this logic
        state.dataHVACGlobal->HybridVentSysAvailVentCtrl(SysAvailNum) =
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl;
        if (state.dataHVACGlobal->HybridVentSysAvailVentCtrl(SysAvailNum) < 0) {
            // Fatal error
            ShowFatalError(
                state, "Hybrid ventilation control: the ventilation control status is beyond the range. Please check input of control mode schedule");
        }

        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).HybridVentMgrConnectedToAirLoop) {
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                state.dataSystemAvailabilityManager->HybridVentCtrl_Close) {
                state.dataAirLoop->PriAirSysAvailMgr(PriAirSysNum).AvailStatus = CycleOn;
            }
        }

        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                state.dataSystemAvailabilityManager->HybridVentCtrl_Open &&
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ANControlTypeSchedPtr > 0 &&
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OpeningFactorFWS > 0) {
            state.dataHVACGlobal->HybridVentSysAvailWindModifier(SysAvailNum) =
                CurveValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).OpeningFactorFWS, WindExt);
        }

        // Set up flags to control simple airflow objects
        if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum > 0 &&
            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0) {
            SimpleControlType =
                GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr);
            for (ControlledZoneNum = 1; ControlledZoneNum <= state.dataGlobal->NumOfZones; ++ControlledZoneNum) {
                for (int zoneInNode = 1; zoneInNode <= state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).NumInletNodes; ++zoneInNode) {
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).AirLoopNum ==
                        state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode)) {
                        // Setup flag for ventilation objects
                        for (i = 1; i <= state.dataHeatBal->TotVentilation; ++i) {
                            if (state.dataHeatBal->Ventilation(i).ZonePtr == state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum) {
                                state.dataHeatBal->Ventilation(i).HybridControlType = HybridControlTypeIndiv;
                                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                                    state.dataSystemAvailabilityManager->HybridVentCtrl_Close) {
                                    state.dataHeatBal->Ventilation(i).HybridControlType = HybridControlTypeClose;
                                } else {
                                    if (SimpleControlType == 1) {
                                        state.dataHeatBal->Ventilation(i).HybridControlType = HybridControlTypeGlobal;
                                        state.dataHeatBal->Ventilation(i).HybridControlMasterNum =
                                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr;
                                    }
                                }
                            }
                        }
                        // Setup flag for Mixing objects
                        for (i = 1; i <= state.dataHeatBal->TotMixing; ++i) {
                            if (state.dataHeatBal->Mixing(i).ZonePtr == state.dataZoneEquip->ZoneEquipConfig(ControlledZoneNum).ActualZoneNum) {
                                state.dataHeatBal->Mixing(i).HybridControlType = HybridControlTypeIndiv;
                                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                                    state.dataSystemAvailabilityManager->HybridVentCtrl_Close) {
                                    state.dataHeatBal->Mixing(i).HybridControlType = HybridControlTypeClose;
                                } else {
                                    if (SimpleControlType == 1) {
                                        state.dataHeatBal->Mixing(i).HybridControlType = HybridControlTypeGlobal;
                                        state.dataHeatBal->Mixing(i).HybridControlMasterNum =
                                            state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } else if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0) {
            SimpleControlType =
                GetCurrentScheduleValue(state, state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr);
            // Hybrid ventilation manager is applied to zone component
            // setup flag for ventilation objects
            for (i = 1; i <= state.dataHeatBal->TotVentilation; ++i) {
                if (state.dataHeatBal->Ventilation(i).ZonePtr ==
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum) {
                    state.dataHeatBal->Ventilation(i).HybridControlType = HybridControlTypeIndiv;
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                        state.dataSystemAvailabilityManager->HybridVentCtrl_Close) {
                        state.dataHeatBal->Ventilation(i).HybridControlType = HybridControlTypeClose;
                    } else {
                        if (SimpleControlType == 1) {
                            state.dataHeatBal->Ventilation(i).HybridControlType = HybridControlTypeGlobal;
                            state.dataHeatBal->Ventilation(i).HybridControlMasterNum =
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr;
                        }
                    }
                }
            }
            // Setup flag for Mixing objects
            for (i = 1; i <= state.dataHeatBal->TotMixing; ++i) {
                if (state.dataHeatBal->Mixing(i).ZonePtr ==
                    state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum) {
                    state.dataHeatBal->Mixing(i).HybridControlType = HybridControlTypeIndiv;
                    if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationCtrl ==
                        state.dataSystemAvailabilityManager->HybridVentCtrl_Close) {
                        state.dataHeatBal->Mixing(i).HybridControlType = HybridControlTypeClose;
                    } else {
                        if (SimpleControlType == 1) {
                            state.dataHeatBal->Mixing(i).HybridControlType = HybridControlTypeGlobal;
                            state.dataHeatBal->Mixing(i).HybridControlMasterNum =
                                state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).VentilationPtr;
                        }
                    }
                }
            }
        }
    }

    bool GetHybridVentilationControlStatus(EnergyPlusData &state, int const ZoneNum) // Index of zone
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Lixing Gu
        //       DATE WRITTEN   July 2010
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This routine was designed to find whether this zone is controlled by hybrid ventilation
        // ventilation control option.

        // Return value
        bool VentControl; // Set to true if ventilation control in the same zone

        int SysAvailNum; // index to system availability manager number

        if (state.dataSystemAvailabilityManager->GetHybridInputFlag) { // First time subroutine has been entered
            GetHybridVentilationInputs(state);
            state.dataSystemAvailabilityManager->GetHybridInputFlag = false;
        }

        VentControl = false;

        for (SysAvailNum = 1; SysAvailNum <= state.dataHVACGlobal->NumHybridVentSysAvailMgrs; ++SysAvailNum) {
            if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).ActualZoneNum == ZoneNum) {
                if (state.dataSystemAvailabilityManager->HybridVentSysAvailMgrData(SysAvailNum).SimpleControlTypeSchedPtr > 0) {
                    VentControl = true;
                }
            }
        }

        return VentControl;
    }

} // namespace SystemAvailabilityManager

} // namespace EnergyPlus
