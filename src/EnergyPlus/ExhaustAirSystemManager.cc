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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/ExhaustAirSystemManager.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixerComponent.hh>
#include <EnergyPlus/NodeInputManager.hh>
// #include <EnergyPlus/ReturnAirPathManager.hh> //2022-01-14: replace with exhaust system
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh> //2022-01-14: may not needed for this exhaust system

namespace EnergyPlus {

namespace ExhaustAirSystemManager {
    // Module containing the routines dealing with the AirLoopHVAC:ReturnPath (formerly Return Air Path)

    // MODULE INFORMATION:
    //       AUTHOR
    //       DATE WRITTEN   January 2022
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS MODULE:
    // To manage the exhaust air system.

    void SimExhaustAirSystem(EnergyPlusData &state)
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR:          Russ Taylor
        //       DATE WRITTEN:    Nov 1997

        // Locals
        int ExhaustAirSystemNum;

        // Obtains and Allocates Mixer related parameters from input file
        // 2022-01-14: To do: Need to define a correponding state.dataExhAirSystemMgr data structure
        if (state.dataExhAirSystemMrg->GetInputFlag) { // First time subroutine has been entered
            GetExhaustAirSystemInput(state);
            state.dataExhAirSystemMrg->GetInputFlag = false;
        }

        for (ExhaustAirSystemNum = 1; ExhaustAirSystemNum <= state.dataZoneEquip->NumExhaustAirSystems; ++ExhaustAirSystemNum) {
            CalcExhaustAirSystem(state, ExhaustAirSystemNum);
        }
    }

    void GetExhaustAirSystemInput(EnergyPlusData &state)
    {
        // 2022-01-11: before setting up a seperate set of files for exhaust systems (which will be done later
        // use this function to develop the input processing function of exhaust systems

        // SUBROUTINE INFORMATION:
        //       AUTHOR:
        //       DATE WRITTEN:    Jan 2022

        // PURPOSE OF THIS SUBROUTINE: Process exhaust system inputs

        // Using/Aliasing
        using NodeInputManager::GetOnlySingleNode;
        using namespace DataLoopNode;

        // Locals
        int PathNum;
        int CompNum;
        int NumAlphas;
        int NumNums;
        int IOStat;
        int Counter;
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool ErrorsFound( false );
        ////////////////////////////////////////////////////////////////////////////////////
        bool IsNotOK; // Flag to verify name

        bool ErrorsFound = false;

        // 2022-01-12: This means a correponding data structure should be build for the exhaust path as well
        // 2022-01-12: So here is another item to be built first: stateZoneEquip->ExhaustAirPath
        // 2022-01-13: Not sure if the following is needed with the json helper method?
        // 2022-01-17: The logic of the code seems to be allocated = process, so used to stop the processing is it is already allocated.
        if (allocated(state.dataZoneEquip->ExhaustAirSystem)) {
            return;
        }

        // 2022-01-12: After setting the exhaust air system structs and a few related definitions
        // auto &cCurrentModuleObject = state.dataIPShortCut->cCurrentModuleObject;
        // cCurrentModuleObject = "AirLoopHVAC:ExhaustSystem";
        // state.dataZoneEquip->NumExhaustAirSystems = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, cCurrentModuleObject);

        // 2022-01-12: More processing code here:
        // 2022-01-13: Use the json helper to process input
        constexpr const char *RoutineName("GetExhaustAirSystemInput: ");
        std::string cCurrentModuleObject = "AirLoopHVAC:ExhaustSystem";
        auto &ip = state.dataInputProcessing->inputProcessor;
        auto const instances = ip->epJSON.find(cCurrentModuleObject);
        if (instances != ip->epJSON.end()) {
            auto const &objectSchemaProps = ip->getObjectSchemaProps(state, cCurrentModuleObject);
            auto &instancesValue = instances.value();
            int numExhaustSystems = instancesValue.size();
            int exhSysNum = 0;

            if (numExhaustSystems > 0) {
                state.dataZoneEquip->ExhaustAirSystem.allocate(numExhaustSystems);
            }

            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                ++exhSysNum;
                auto const &objectFields = instance.value();
                auto &thisExhSys = state.dataZoneEquip->ExhaustAirSystem(exhSysNum);
                thisExhSys.Name = UtilityRoutines::MakeUPPERCase(instance.key());
                ip->markObjectAsUsed(cCurrentModuleObject, instance.key());

                std::string availSchName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "availability_manager_list_name");
                int availSchNum = ScheduleManager::GetScheduleIndex(state, availSchName);
                if (availSchNum > 0) {
                    // normal conditions
                } else if (availSchNum == 0) {
                    // blank or anything like that, treat as always avaialabe
                    /* */
                } else {
                    availSchNum = 0;
                    // maybe not need for a severe error; a regular warnign would do.
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "Avaiability Manager Name =" + availSchName + "not found.");
                    // ErrorsFound = true;
                }
                // 2022-01-13: To do: Add related data structure to store Availability Manager  (and for all other fields as well)

                std::string zoneMixerName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "airloophvac_zonemixer_name");
                int zoneMixerNum = 0;
                int zoneMixerIndex = 0; 

                // Obtains and Allocates Mixer related parameters from input file
                if (state.dataMixerComponent->SimAirMixerInputFlag) { // First time subroutine has been entered
                    EnergyPlus::MixerComponent::GetMixerInput(state);
                    state.dataMixerComponent->SimAirMixerInputFlag = false;
                }

                // Find the correct MixerNumber
                if (zoneMixerNum == 0) {
                    zoneMixerNum = UtilityRoutines::FindItemInList(zoneMixerName, state.dataMixerComponent->MixerCond, &EnergyPlus::MixerComponent::MixerConditions::MixerName);
                    if (zoneMixerNum == 0) {
                        // 2022-01-19: May need to change the message a little bit to get rid of "SimAirLoopMixer:"
                        ShowFatalError(state, "GetExhaustAirSystemInput: Mixer not found=" + std::string{zoneMixerName});
                    }
                    zoneMixerIndex = zoneMixerNum;
                } else {
                    zoneMixerNum = zoneMixerIndex;
                    if (zoneMixerNum > state.dataMixerComponent->NumMixers || zoneMixerNum < 1) {
                        // 2022-01-19: May need to change the warning mesage a little bit
                        ShowFatalError(state,
                                       format("GetExhaustAirSystemInput: Invalid zoneMixerIndex passed={}, Number of Mixers={}, Mixer name={}",
                                              zoneMixerNum,
                                              state.dataMixerComponent->NumMixers,
                                              zoneMixerName));
                    }
                    if (state.dataMixerComponent->CheckEquipName(zoneMixerNum)) {
                        if (zoneMixerName != state.dataMixerComponent->MixerCond(zoneMixerNum).MixerName) {
                            // 2022-01-19: May need to change the warning mesage a little bit
                            ShowFatalError(state,
                                           format("GetExhaustAirSystemInput: Invalid zoneMixerIndex passed={}, Mixer name={}, stored Mixer Name for that index={}",
                                                  zoneMixerNum,
                                                  zoneMixerName,
                                                  state.dataMixerComponent->MixerCond(zoneMixerNum).MixerName));
                        }
                        state.dataMixerComponent->CheckEquipName(zoneMixerNum) = false;
                    }
                }

                // With the correct MixerNum Initialize
                EnergyPlus::MixerComponent::InitAirMixer(state, zoneMixerNum); // Initialize all Mixer related parameters
                // 2022-01-19: Till this point are the code revised from example

                // 2022-01-19: Originally put the following lines to check componets a few days ago. But now it seems 
                // redudant with the zone mixer's CheckEquipName() function. 
                ValidateComponent(state, "AirLoopHVAC:ZoneMixer", zoneMixerName, IsNotOK, "AirLoopHVAC:ExhaustSystem");
                if (IsNotOK) {
                    // zoneMixerNum = 0;
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "ZoneMixer Name =" + zoneMixerName + "not found.");
                    ErrorsFound = true;
                } else {
                    // normal conditions
                    // 2022-01-13: To do: Add related data struct to store zoneMixer number (actually need a local zone num definition as well)
                }

                std::string centralFanType = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "fan_object_type");
                int centralFanTypeNum = 0; 
                // 2022-01: Check fan types and gives warnings
                if (UtilityRoutines::SameString(centralFanType, "Fan:SystemModel")) {
                    centralFanTypeNum = DataHVACGlobals::FanType_SystemModelObject;
                } else if (UtilityRoutines::SameString(centralFanType, "Fan:ComponentModel")) {
                    centralFanTypeNum = DataHVACGlobals::FanType_ComponentModel;
                } else {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "Fan Type =" + centralFanType + "not supported; ");
                    ShowContinueError(state, "it needs to be either Fan:SystemModel or Fan:ComponentModel.");
                    ErrorsFound = true;
                }

                std::string centralFanName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "fan_name");
                int centralFanIndex = 0; // zero based 
                // 2022-01: also in general, should this processing may need to checked first that
                // all the fan objects have already been processed already to be fail-safe.
                // probably simialr to other like schedules etc, although schedules might have been processed early in most cases.
                centralFanIndex = HVACFan::getFanObjectVectorIndex(state, centralFanName); // zero-based
                if (centralFanIndex >= 0) {
                // normal index
                // 2022-01: to do: if some constant information need to be extracted, here might be a good place to do so:
                    /* //e.g. an example in PIU processing: 
                    if (HVACFan::checkIfFanNameIsAFanSystem(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName)) {
                        state.dataPowerInductionUnits->PIU(PIUNum).Fan_Num = DataHVACGlobals::FanType_SystemModelObject;
                    state.dataHVACFan->fanObjs.emplace_back(
                    new HVACFan::FanSystem(state, state.dataPowerInductionUnits->PIU(PIUNum).FanName)); // call constructor
                    } 
                    */
                } else {
                    centralFanIndex = -1;
                    // here a severe error message is needed
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "Fan Name =" + centralFanName + "not found.");
                    ErrorsFound = true;
                }
            }
            state.dataZoneEquip->NumReturnAirPaths = numExhaustSystems;

        } else {
            // If no exhaust systems are defined, then do something <or nothing>:
            /* */
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found getting AirLoopHVAC:ExhaustSystem.  Preceding condition(s) causes termination.");
        }
    }

    void InitExhaustAirSystem([[maybe_unused]] int &ExhaustAirSystemNum) // maybe unused
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:
        //       DATE WRITTEN:    Jan 2022

        // PURPOSE OF THIS SUBROUTINE: This subroutine

        // METHODOLOGY EMPLOYED:

        // REFERENCES:

        // USE STATEMENTS:
    }

    void CalcExhaustAirSystem(EnergyPlusData &state, int &ExhaustAirSystemNum)
    {
        //// SUBROUTINE INFORMATION:
        ////       AUTHOR:
        ////       DATE WRITTEN:    Jan 2022

        //// PURPOSE OF THIS SUBROUTINE: This subroutine

        //// METHODOLOGY EMPLOYED:

        //// REFERENCES:

        //// USE STATEMENTS:

        //// Using/Aliasing
        // using MixerComponent::SimAirMixer;
        // using ZonePlenum::SimAirZonePlenum;

        //// Locals
        // int ComponentNum;

        // for (ComponentNum = 1; ComponentNum <= state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).NumOfComponents; ++ComponentNum) {
        //    switch (state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentTypeEnum(ComponentNum)) {
        //    case DataZoneEquipment::AirLoopHVACZone::Mixer: // 'AirLoopHVAC:ZoneMixer'

        //        if (!(state.dataAirflowNetwork->AirflowNetworkFanActivated &&
        //              state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone)) {
        //            SimAirMixer(state,
        //                        state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentName(ComponentNum),
        //                        state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentIndex(ComponentNum));
        //        }

        //        break;

        //    case DataZoneEquipment::AirLoopHVACZone::ReturnPlenum: // 'AirLoopHVAC:ReturnPlenum'

        //        SimAirZonePlenum(state,
        //                         state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentName(ComponentNum),
        //                         DataZoneEquipment::AirLoopHVACZone::ReturnPlenum,
        //                         state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentIndex(ComponentNum));
        //        break;

        //    default:
        //        ShowSevereError(state,
        //                        "Invalid AirLoopHVAC:ReturnPath Component=" +
        //                            state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).ComponentType(ComponentNum));
        //        ShowContinueError(state, "Occurs in AirLoopHVAC:ReturnPath =" + state.dataZoneEquip->ReturnAirPath(ReturnAirPathNum).Name);
        //        ShowFatalError(state, "Preceding condition causes termination.");
        //        break;
        //    }
        //}
    }

    void ReportExhaustAirSystem([[maybe_unused]] int &ExhaustAirSystemNum) // maybe unused
    {
        // SUBROUTINE INFORMATION:
        //       AUTHOR:
        //       DATE WRITTEN:    Jan 2022

        // PURPOSE OF THIS SUBROUTINE: This subroutine

        // METHODOLOGY EMPLOYED:

        // REFERENCES:

        // USE STATEMENTS:
    }

    void GetZoneExhaustControlInput(EnergyPlusData &state)
    {
        // This function is for the ZoneExhaust Control input processing;
        // It might not be finally belongs to here; but started here to code the processing part
        //
        // 
        // Using/Aliasing
        using NodeInputManager::GetOnlySingleNode;
        using namespace DataLoopNode;

        // Locals
        int PathNum;
        int CompNum;
        int NumAlphas;
        int NumNums;
        int IOStat;
        int Counter;
        //////////// hoisted into namespace ////////////////////////////////////////////////
        // static bool ErrorsFound( false );
        ////////////////////////////////////////////////////////////////////////////////////
        bool IsNotOK; // Flag to verify name

        bool ErrorsFound = false;

        // 2022-01-13: Use the json helper to process input
        constexpr const char *RoutineName("GetZoneExhaustControlInput: ");
        std::string cCurrentModuleObject = "ZoneHVAC:ExhaustControl";
        auto &ip = state.dataInputProcessing->inputProcessor;
        auto const instances = ip->epJSON.find(cCurrentModuleObject);
        if (instances != ip->epJSON.end()) {
            auto const &objectSchemaProps = ip->getObjectSchemaProps(state, cCurrentModuleObject);
            auto &instancesValue = instances.value();
            int numExhaustControls = instancesValue.size();
            int exhCtrlNum = 0;

            if (numExhaustControls > 0) {
                state.dataZoneEquip->ExhaustAirSystem.allocate(numExhaustControls);
            }

            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                ++exhCtrlNum;
                auto const &objectFields = instance.value();
                auto &thisExhCtrl = state.dataZoneEquip->ExhaustAirSystem(exhCtrlNum);
                thisExhCtrl.Name = UtilityRoutines::MakeUPPERCase(instance.key());
                ip->markObjectAsUsed(cCurrentModuleObject, instance.key());

                std::string availSchName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "availability_schedule_name");
                int availSchNum = 0;
                availSchNum = ScheduleManager::GetScheduleIndex(state, availSchName);
                // UtilityRoutines::FindItemInList(availSchName, state.dataSystemAvailabilityManager->SchedSysAvailMgrData);
                // To do: here to use schedule match function to process data.

                if (availSchNum > 0) {
                    // normal conditions
                } else if (availSchNum == 0) {
                    // blank or anything like that, treat as always avaialabe
                    /* //may not need to process the detailed schedule value here, but if yes, use this example: 
                    GetCurrentScheduleValue(state, state.dataPowerInductionUnits->PIU(PIUNum).SchedPtr)
                    */
                } else {
                    availSchNum = 0;
                    // a regular warning is ok.
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Avaiability Manager Name = " + availSchName + "not found.");
                    // ErrorsFound = true;
                }
                // 2022-01-13: To do: Add related data structure to store Availability Manager  (and for all other fields as well)


                // These two nodes are required inputs: 
                std::string inletNodeName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "inlet_node_name");
                // 2022-01: What about nodelist, can GetOnlySingleNode() still used for that?
                int inletNodeNum = GetOnlySingleNode(state,
                                                  inletNodeName,
                                                  ErrorsFound,
                                                  cCurrentModuleObject,
                                                  thisExhCtrl.Name,
                                                  DataLoopNode::NodeFluidType::Air,
                                                  DataLoopNode::NodeConnectionType::Inlet,
                                                  NodeInputManager::CompFluidStream::Primary,
                                                  ObjectIsParent);

                std::string outletNodeName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "outlet_node_name");

                 int outletNodeNum = GetOnlySingleNode(state,
                                                     outletNodeName,
                                                     ErrorsFound,
                                                     cCurrentModuleObject,
                                                     thisExhCtrl.Name,
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::NodeConnectionType::Outlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     ObjectIsParent);

                Real64 designExhaustFlowRate = ip->getRealFieldValue(objectFields, objectSchemaProps, "design_exhaust_flow_rate_");
                // 2022-01: to do (for all locals): add data zone equip struct connections.

                std::string flowControlType = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "flow_control_type");
                // To do: use an int or enum type to convert the flow control type from string to some numerical value

                std::string exhaustFlowFractionScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "exhaust_flow_fraction_schedule_name");
                // to do so schedule matching
                int exhaustFlowFractionScheduleNum = 0;
                // now here dealing with schedule rather than availability manager.
                exhaustFlowFractionScheduleNum = ScheduleManager::GetScheduleIndex(state, exhaustFlowFractionScheduleName);
                // To do: here to use schedule match function to process data.

                if (exhaustFlowFractionScheduleNum > 0) {
                    // normal conditions
                } else if (exhaustFlowFractionScheduleNum == 0) {
                    // blank or anything like that, treat as always avaialabe?
                    /* */
                } else {
                    exhaustFlowFractionScheduleNum = 0;
                    // a regular warning would do.
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Schedule Name =" + exhaustFlowFractionScheduleName + "not found.");
                    // ErrorsFound = true;
                }

                std::string supplyNodeOrNodelistName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "supply_node_or_nodelist_name");
                // int zoneMixerNum = UtilityRoutines::FindItemInList(zoneMixerName, state);
                // to do: check the requirement dependent on the control type:
                /* */
                // Also to do: convert text to interger node values (or node list values?)
                /* */
                int supplyNodeOrNodelistNum = 0;                

                std::string minZoneTempLimitScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "minimum_zone_temperature_limit_schedule_name");
                // to do so schedule matching
                int minZoneTempLimitScheduleNum = 0;
                // now here dealing with schedule rather than availability manager.
                minZoneTempLimitScheduleNum = ScheduleManager::GetScheduleIndex(state, minZoneTempLimitScheduleName);
                // To do: here to use schedule match function to process data.

                if (minZoneTempLimitScheduleNum > 0) {
                    // normal conditions
                } else if (minZoneTempLimitScheduleNum == 0) {
                    // blank or anything like that, treat as always avaialabe?
                    /* */
                } else {
                    minZoneTempLimitScheduleNum = 0;
                    // a regular warning would do.
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Schedule Name =" + minZoneTempLimitScheduleName + "not found.");
                    // ErrorsFound = true;
                }

                std::string minExhFlowFracScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "minimum_exhaust_flow_fraction_schedule_name");
                // to do so schedule matching
                int minExhFlowFracScheduleNum = 0;
                minExhFlowFracScheduleNum = ScheduleManager::GetScheduleIndex(state, minExhFlowFracScheduleName);
                // To do: here to use schedule match function to process data.

                if (minExhFlowFracScheduleNum > 0) {
                    // normal conditions
                } else if (minExhFlowFracScheduleNum == 0) {
                    // blank or anything like that, treat as always avaialabe?
                    /* */
                } else {
                    minExhFlowFracScheduleNum = 0;
                    // a regular warning would do.
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Schedule Name =" + minExhFlowFracScheduleName + "not found.");
                    // ErrorsFound = true;
                }

                std::string balancedExhFracScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "balanced_exhaust_fraction_schedule_name");
                // to do so schedule matching
                int balancedExhFracScheduleNum = 0;
                balancedExhFracScheduleNum = ScheduleManager::GetScheduleIndex(state, balancedExhFracScheduleName);
                // To do: here to use schedule match function to process data.

                if (balancedExhFracScheduleNum > 0) {
                    // normal conditions
                } else if (balancedExhFracScheduleNum == 0) {
                    // blank or anything like that, treat as always avaialabe?
                    /* */
                } else {
                    balancedExhFracScheduleNum = 0;
                    // a regular warning would do.
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Schedule Name =" + balancedExhFracScheduleName + "not found.");
                    // ErrorsFound = true;
                }
            }

            // 2022-01: Need an equivalent of the zone exhaust numbers
            // However, probably need one for all numbers (sum of all zones);
            // and also need to create a map that contains a table of for each zone how many of these exhasut controls
            /* state.dataZoneEquip->NumZoneExhaustControls =  numExhaustControls; // or exhCtrlNum? */

        } else {
            // If no exhaust systems are defined, then do something <or nothing>:
            /* */
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found getting ZoneHVAC:ExhaustControl.  Preceding condition(s) causes termination.");
        }
    }

} // namespace ExhaustAirSystemManager

} // namespace EnergyPlus
