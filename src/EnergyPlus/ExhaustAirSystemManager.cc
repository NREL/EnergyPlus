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
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/ExhaustAirSystemManager.hh>
#include <EnergyPlus/Fans.hh>
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
    // Module containing the routines dealing with the AirLoopHVAC:ExhaustSystem
    // Date: January 2022

    // PURPOSE OF THIS MODULE:
    // To manage the exhaust air system.

    void SimExhaustAirSystem(EnergyPlusData &state, bool FirstHVACIteration)
    {
        // Date: Jan 2022

        // Locals
        int ExhaustAirSystemNum;

        // Obtains and Allocates Mixer related parameters from input file
        // 2022-01-14: To do: Need to define a correponding state.dataExhAirSystemMgr data structure
        if (state.dataExhAirSystemMrg->GetInputFlag) { // First time subroutine has been entered
            GetExhaustAirSystemInput(state);
            state.dataExhAirSystemMrg->GetInputFlag = false;
        }

        for (ExhaustAirSystemNum = 1; ExhaustAirSystemNum <= state.dataZoneEquip->NumExhaustAirSystems; ++ExhaustAirSystemNum) {
            CalcExhaustAirSystem(state, ExhaustAirSystemNum, FirstHVACIteration);
        }
    }

    void GetExhaustAirSystemInput(EnergyPlusData &state)
    {
        // 2022-01-11: before setting up a seperate set of files for exhaust systems (which will be done later
        // use this function to develop the input processing function of exhaust systems

        // Date:    Jan 2022

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

                std::string availSchName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "availability_schedule_name");
                int availSchNum = ScheduleManager::GetScheduleIndex(state, availSchName);
                if (availSchNum > 0) {
                    // normal conditions
                } else if (availSchNum == 0) {
                    // blank or anything like that, treat as always avaialabe
                    /* */
                } else {
                    availSchNum = 0;
                    // a regular warning
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "Avaiability Schedule Name =" + availSchName + "not found.");
                    // ErrorsFound = true;
                }
                thisExhSys.AvailScheduleNum = availSchNum;

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
                    zoneMixerNum = UtilityRoutines::FindItemInList(
                        zoneMixerName, state.dataMixerComponent->MixerCond, &EnergyPlus::MixerComponent::MixerConditions::MixerName);
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
                            ShowFatalError(
                                state,
                                format(
                                    "GetExhaustAirSystemInput: Invalid zoneMixerIndex passed={}, Mixer name={}, stored Mixer Name for that index={}",
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
                thisExhSys.ZoneMixerName = zoneMixerName;
                thisExhSys.ZoneMixerIndex = zoneMixerIndex;

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
                thisExhSys.CentralFanTypeNum = centralFanTypeNum;

                std::string centralFanName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "fan_name");
                int centralFanIndex = 0; // zero based
                // 2022-01: also in general, should this processing may need to checked first that
                // all the fan objects have already been processed already to be fail-safe.
                // probably simialr to other like schedules etc, although schedules might have been processed early in most cases.
                if (centralFanTypeNum == DataHVACGlobals::FanType_SystemModelObject) {
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
                } else if (centralFanTypeNum == DataHVACGlobals::FanType_ComponentModel) {
                    bool isNotOK(false);
                    int fanType_Num_Check(0);
                    EnergyPlus::Fans::GetFanType(state, centralFanName, fanType_Num_Check, isNotOK, cCurrentModuleObject, thisExhSys.Name);
                    // 2022-01: to do: can do a check on fanType_Num_Check with centralFanTypeNum, if not feeling redudant.

                    if (isNotOK) {
                        ShowSevereError(state, "Occurs in " + cCurrentModuleObject + " = " + thisExhSys.Name);
                        ErrorsFound = true;
                    } else {
                        isNotOK = false;
                        ValidateComponent(state, centralFanType, centralFanName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowSevereError(state, "Occurs in " + cCurrentModuleObject + " = " + thisExhSys.Name);
                            ErrorsFound = true;
                        } else { // mine data from fan object
                            // Get the fan index
                            bool errFlag(false);
                            // EnergyPlus::Fans::GetFanIndex(state, centralFanName, centralFanIndex, errFlag, ObjexxFCL::Optional_string_const());
                            EnergyPlus::Fans::GetFanIndex(state, centralFanName, centralFanIndex, errFlag);
                            if (errFlag) {
                                ShowContinueError(state, "Occurs in " + cCurrentModuleObject + " = " + thisExhSys.Name);
                                ErrorsFound = true;
                            }
                        }
                    }
                } else {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "Fan Type =" + centralFanType + "not supported; ");
                    ShowContinueError(state, "it needs to be either Fan:SystemModel or Fan:ComponentModel.");
                    ErrorsFound = true;
                }

                thisExhSys.CentralFanIndex = centralFanIndex;
            }
            state.dataZoneEquip->NumReturnAirPaths = numExhaustSystems;

        } else {
            // If no exhaust systems are defined, then do something <or nothing>:
            /* */
        }

        // 2022-01-28: Investigate how the IDD AirLoopHVAC:ExhaustSystem knows which zones each of its forks is connected to.
        // One poissble way might to add the zonehvac:exhaustcontrol names to the input fields. But may have other ways as well.

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found getting AirLoopHVAC:ExhaustSystem.  Preceding condition(s) causes termination.");
        }

        // 2022-01-26: Either here or at the end, or blended in the code above, set up the output variables:
        /* // some example code to set up outpput variables:
            SetupOutputVariable(state,
                                "Fan Unbalanced Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                Fan(FanNum).UnbalancedOutletMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Fan(FanNum).FanName);
            SetupOutputVariable(state,
                                "Fan Balanced Air Mass Flow Rate",
                                OutputProcessor::Unit::kg_s,
                                Fan(FanNum).BalancedOutletMassFlowRate,
                                OutputProcessor::SOVTimeStepType::System,
                                OutputProcessor::SOVStoreType::Average,
                                Fan(FanNum).FanName);
        */
    }

    void InitExhaustAirSystem([[maybe_unused]] int &ExhaustAirSystemNum) // maybe unused
    {
        // Date: Jan 2022
    }

    void CalcExhaustAirSystem(EnergyPlusData &state, int &ExhaustAirSystemNum, bool FirstHVACIteration)
    {
        // Date: Jan 2022

        // Using/Aliasing
        using MixerComponent::SimAirMixer;

        // For DataZoneEquipment::AirLoopHVACZone::Mixer: // 'AirLoopHVAC:ZoneMixer'
        // 2022-01: Simulate Zone Air Mixer
        if (!(state.dataAirflowNetwork->AirflowNetworkFanActivated &&
              state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone)) {
            SimAirMixer(state,
                        state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).ZoneMixerName,
                        state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).ZoneMixerIndex);
        }

        // 2022-01-26: One additional step here might be to consider the avaiability schedule of the exhasut system
        // or the central exhaust fan's own avaiability schedule.
        // Need to prodeed differently for the cases when the exhasut system (fan) is available or not available.

        // 2022-01: Simulate the fan (need some clean up 2022-01-26)
        // capacity control method is VariableFanVariableFlow, VariableFanConstantFlow, or ASHRAE90.1

        // calculate fan speed ratio for Fan:OnOff or Fan:SystemModel (not used for other fan types). Only used in fan:OnOff model if performance
        // curves are present.
        Real64 FanAirVolFlow = 1.0; // 2022-01: this should be something like a design or rate fan flow rate
        Real64 FanSpeedRatio = 1.0; //  Node(InletNode).MassFlowRate / (FanAirVolFlow * state.dataEnvrn->StdRhoAir);

        // Constant fan and variable flow calculation AND variable fan
        auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;

        // bool FirstHVACIteration = false; // 2022-01: This was passed in as a calling parameter in Fan::SimulateFanComponents()'s parent call
        // 2022-01: may still need find another way to pass this in or deal with the first HVAC iteration scenario

        if (state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanTypeNum == DataHVACGlobals::FanType_SystemModelObject) {
            // 2022-01: The system model, look in HVACFan name space
            // state.dataHVACFan->fanObjs[state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanIndex]->simulate(
            //    state, FanSpeedRatio, ZoneCompTurnFansOn, ZoneCompTurnFansOff, _);
            // 2022-01: use a simpler call instead (checkout SimAirServingZones.cc)
            state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
            state.dataHVACFan->fanObjs[state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanIndex]->simulate(state, _, _, _, _);
        } else if (state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanTypeNum == DataHVACGlobals::FanType_ComponentModel) {
            // 2022-01: Component model, look in Fan name space
            // if (FanCoil(FanCoilNum).FanType_Num != DataHVACGlobals::FanType_SystemModelObject) {
            /*Fans::SimulateFanComponents(state,
                                        state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanName,
                                        FirstHVACIteration,
                                        state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanIndex,
                                        FanSpeedRatio,
                                        ZoneCompTurnFansOn,
                                        ZoneCompTurnFansOff);*/
            // 2022-01: use a simpler call instead (checkout SimAirServingZones.cc)
            Fans::SimulateFanComponents(state,
                                        state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanName,
                                        FirstHVACIteration,
                                        state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanIndex); //,
                                                                                                                     // FanSpeedRatio,
                                                                                                                     // ZoneCompTurnFansOn,
                                                                                                                     // ZoneCompTurnFansOff);
        }

        // 2022-01: Determine if there are some "iteration" or revisit step for the zone mixer and fan simulation
        // depending on which should determine the flow based on control types:
        /* // if (control type == follow-supply) { //then sum up the flow and assign to fan};
        // else if(controltype == scheduled) { //then sum up based on schedule} */

        // 2022-01: Errors and warning messages:
        // ShowSevereError(state,
        //                "Problems found in simulating AirLoopHVAC:ExhaustSystem =" +
        //                state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).Name));
        //// ShowContinueError(state, "Occurs in AirLoopHVAC:ExhaustSystem =" + state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).Name);
        // ShowFatalError(state, "Preceding condition causes termination.");
    }

    void ReportExhaustAirSystem([[maybe_unused]] int &ExhaustAirSystemNum) // maybe unused
    {
        // Date: Jan 2022
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
            int numZoneExhaustControls = instancesValue.size();
            int exhCtrlNum = 0;

            if (numZoneExhaustControls > 0) {
                state.dataZoneEquip->ZoneExhaustControlSystem.allocate(numZoneExhaustControls);
            }

            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                ++exhCtrlNum;
                auto const &objectFields = instance.value();
                auto &thisExhCtrl = state.dataZoneEquip->ZoneExhaustControlSystem(exhCtrlNum);
                thisExhCtrl.Name = UtilityRoutines::MakeUPPERCase(instance.key());
                ip->markObjectAsUsed(cCurrentModuleObject, instance.key());

                std::string availSchName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "availability_schedule_name");
                int availSchNum = 0;
                availSchNum = ScheduleManager::GetScheduleIndex(state, availSchName);
                // UtilityRoutines::FindItemInList(availSchName, state.dataSystemAvailabilityManager->SchedSysAvailMgrData);

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
                thisExhCtrl.AvailScheduleNum = availSchNum;

                // 2022-01-28: Also need an extra zone name field here: 
                /* zone_name */
                std::string zoneName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "zone_name");
                thisExhCtrl.ZoneName = zoneName;
                int zoneNum = UtilityRoutines::FindItemInList(zoneName, state.dataHeatBal->Zone);
                thisExhCtrl.ZoneNum = zoneNum;
                // 2022-01: need to make sure zoneNum is greater than zero. 

                // These two nodes are required inputs:
                std::string inletNodeName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "inlet_node_name");
                // 2022-01: What about nodelist, can GetOnlySingleNode() still used for that?
                int inletNodeNum = GetOnlySingleNode(state,
                                                     inletNodeName,
                                                     ErrorsFound,
                                                     DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl,
                                                     thisExhCtrl.Name,
                                                     DataLoopNode::NodeFluidType::Air,
                                                     DataLoopNode::ConnectionType::Inlet,
                                                     NodeInputManager::CompFluidStream::Primary,
                                                     ObjectIsParent);
                thisExhCtrl.InletNodeNum = inletNodeNum;

                std::string outletNodeName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "outlet_node_name");

                int outletNodeNum = GetOnlySingleNode(state,
                                                      outletNodeName,
                                                      ErrorsFound,
                                                      DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl,
                                                      thisExhCtrl.Name,
                                                      DataLoopNode::NodeFluidType::Air,
                                                      DataLoopNode::ConnectionType::Outlet,
                                                      NodeInputManager::CompFluidStream::Primary,
                                                      ObjectIsParent);
                thisExhCtrl.OutletNodeNum = outletNodeNum;

                Real64 designExhaustFlowRate = ip->getRealFieldValue(objectFields, objectSchemaProps, "design_exhaust_flow_rate_");
                // 2022-01-20: may need some sanity check about the input values
                thisExhCtrl.DesignExhaustFlowRate = designExhaustFlowRate;
                // 2022-01-28: Need to consider the auto-size option: 
                // This will probably need zone name and some information about zone supplies? 
                /* */

                std::string flowControlType = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "flow_control_type");
                // 2022-01-20: may need some sanity check here about the input values
                int flowControlTypeNum = 0;
                // 2022-01-20: do a same string comparison here:
                thisExhCtrl.FlowControlTypeNum = flowControlTypeNum;

                std::string exhaustFlowFractionScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "exhaust_flow_fraction_schedule_name");
                // to do so schedule matching
                int exhaustFlowFractionScheduleNum = 0;
                // now here dealing with schedule rather than availability manager.
                exhaustFlowFractionScheduleNum = ScheduleManager::GetScheduleIndex(state, exhaustFlowFractionScheduleName);

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
                thisExhCtrl.ExhaustFlowFractionScheduleNum = exhaustFlowFractionScheduleNum;

                std::string supplyNodeOrNodelistName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "supply_node_or_nodelist_name");
                int supplyNodeOrNodelistNum = 0;
                // to do: check the requirement dependent on the control type:
                /* // Logic: if (control is scheduled) then do not process this or
                   // set a number out of range
                   // if (control is follow-supply) then need to make sure this is not blank
                   // and also need make sure it is a valid node or nodelist
                */
                // Also to do: convert text to interger node values (or node list values?)

                // InducedNodeListName = AlphArray(5);
                bool NodeListError = false;
                int NumNum = 0;
                int NumParams = 0;
                int NumNodes = 0;
                Array1D_int NodeNums;

                Array1D_int supplyNodeOrNodelistArray; // 2022-01: this needs some extra allocation and initialization
                // still having a problem getting it debugged properly. Maybe for now replace with a single node to move forward.
                // just a temp way to test further:
                bool singlenodeyes = true;
                int supplynodenum_single = 0;
                if (singlenodeyes) {
                    supplynodenum_single = GetOnlySingleNode(state,
                                                          supplyNodeOrNodelistName,
                                                          ErrorsFound,
                                                          DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl, // may need to change type, maybe zone inlets?
                                                          thisExhCtrl.Name,
                                                          DataLoopNode::NodeFluidType::Air,
                                                          DataLoopNode::ConnectionType::Sensor,
                                                          NodeInputManager::CompFluidStream::Primary,
                                                          ObjectIsParent);
                    thisExhCtrl.SupplyNodeOrNodelistNum = supplynodenum_single;
                } else {
                    // 2022-02: Refer to GetZoneEquipmentData() in DataZoneEquipment.cc:
                    ip->getObjectDefMaxArgs(state, "NodeList", NumParams, NumAlphas, NumNums);
                    NodeNums.dimension(NumParams, 0);
                    GetNodeNums(state,
                                supplyNodeOrNodelistName,
                                NumNodes,
                                NodeNums,
                                NodeListError,
                                DataLoopNode::NodeFluidType::Air,
                                DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl, // maybe zone inlets?
                                thisExhCtrl.Name,
                                DataLoopNode::ConnectionType::Sensor,
                                NodeInputManager::CompFluidStream::Primary,
                                ObjectIsNotParent); // ,
                                                    // _,
                                                    // supplyNodeOrNodelistName);
                    thisExhCtrl.SupplyNodeOrNodelistNum = supplyNodeOrNodelistNum;
                }
                // 2022-02: Following the node/nodelist checkout, it also need to verify these nodes are "indeed" supply nodes.

                std::string minZoneTempLimitScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "minimum_zone_temperature_limit_schedule_name");
                int minZoneTempLimitScheduleNum = 0;
                // now here dealing with schedule rather than availability manager.
                minZoneTempLimitScheduleNum = ScheduleManager::GetScheduleIndex(state, minZoneTempLimitScheduleName);

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
                thisExhCtrl.MinZoneTempLimitScheduleNum = minZoneTempLimitScheduleNum;

                std::string minExhFlowFracScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "minimum_exhaust_flow_fraction_schedule_name");
                // to do so schedule matching
                int minExhFlowFracScheduleNum = 0;
                minExhFlowFracScheduleNum = ScheduleManager::GetScheduleIndex(state, minExhFlowFracScheduleName);

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
                thisExhCtrl.MinExhFlowFracScheduleNum = minExhFlowFracScheduleNum;

                std::string balancedExhFracScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "balanced_exhaust_fraction_schedule_name");
                // to do so schedule matching
                int balancedExhFracScheduleNum = 0;
                balancedExhFracScheduleNum = ScheduleManager::GetScheduleIndex(state, balancedExhFracScheduleName);

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

                // 2022-01-27: Need an additional check per IORef:
                // This input field must be blank when the zone air flow balance is enforced.If user specifies a schedule and zone air flow balance is
                //     enforced, then EnergyPlus throws a warning error message, ignores the schedule and simulation continues.

                thisExhCtrl.BalancedExhFracScheduleNum = balancedExhFracScheduleNum;
            }

            // 2022-01: Need an equivalent of the zone exhaust numbers
            // However, probably need one for all numbers (sum of all zones);
            // and also need to create a map that contains a table of for each zone how many of these exhasut controls
            state.dataZoneEquip->NumZoneExhaustControls = numZoneExhaustControls; // or exhCtrlNum? */

        } else {
            // If no exhaust systems are defined, then do something <or nothing>:
            /* */
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found getting ZoneHVAC:ExhaustControl.  Preceding condition(s) causes termination.");
        }
    }

    void SimZoneHVACExhaustControls(EnergyPlusData &state, bool FirstHVACIteration)
    {
        // 2022-01: calling steps:
        // first, call the input processing and set the first time input flag
        // second, initialize if needed?, or sizing first (step3)
        // third, sizing
        // fourth, run calc for all exhaust controls
        // fifth, report variables. (so need to set up variables as well somehwere before the actual simulation

        // Locals
        int ExhaustControlNum;

        // 2022-01: Step 1:
        if (state.dataExhCtrlSystemMrg->GetInputFlag) { // First time subroutine has been entered
            GetZoneExhaustControlInput(state);
            state.dataExhCtrlSystemMrg->GetInputFlag = false;
        }

        // 2022-01: Step 2 and/or 3: initialize or sizing if needed:

        // 2022-01: Step 4: run calc for all exhaust controls;
        for (ExhaustControlNum = 1; ExhaustControlNum <= state.dataZoneEquip->NumZoneExhaustControls; ++ExhaustControlNum) {
            CalcZoneHVACExhaustControl(state, ExhaustControlNum, FirstHVACIteration);
        }

        // 2022-01: Step 5: report results
    }

    void CalcZoneHVACExhaustControl(EnergyPlusData &state, int &ZoneHVACExhaustControlNum, bool FirstHVACIteration)
    {
        // 2022-01: Calculate a zonehvac exhaust control system
        // basically, based on the incoming node information, as well as the input parameters
        // determine the outlet node values and possible other state variable (if any)

        // just for posted here for temp reference, will remove later:
        // std::string const idf_objects = delimited_string({
        //    "ZoneHVAC:ExhaustControl,",
        //    "    Zone1 Exhaust Control,           !-Name",
        //    "    HVACOperationSchd,              !- Availability Schedule Name",
        //    "    Zone2 Exhaust Node,             !- Inlet Node Name",
        //    "    Zone2 ExhaustSystem Node,       !- Outlet Node Name",
        //    "    0.1,                            !- Design Flow Rate {m3/s}",
        //    "    Scheduled,                      !- Flow Control Type (Scheduled, FollowSupply, Other?)",
        //    "    Zone2 Exhaust Flow Sched,       !- Flow Fraction Schedule Name",
        //    "    ,                               !- Supply Node or NodeList Name (used with FollowSupply control type)",
        //    "    ,                               !- Minimum Zone Temperature Limit Schedule Name",
        //    "    Zone2 Min Exhaust Flow Sched,   !- Minimum Flow Fraction Schedule Name",
        //    "    FlowBalancedSched;        !-Balanced Exhaust Fraction Schedule Name",
        //});

        auto &thisExhCtrl = state.dataZoneEquip->ZoneExhaustControlSystem(ZoneHVACExhaustControlNum);
        int InletNode = thisExhCtrl.InletNodeNum;
        int OutletNode = thisExhCtrl.OutletNodeNum;
        Real64 MassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        Real64 Tin = state.dataLoopNodes->Node(InletNode).Temp;
        Real64 HRin = state.dataLoopNodes->Node(InletNode).HumRat;
        // RhoAir = RhoAirStdInit;

        // Availability schedule:
        if (EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.AvailScheduleNum) <= 0.0) {
            // state.dataLoopNodes->Node(OutletNode).MassFlowRate = 0.0;
            MassFlow = 0.0;
            state.dataLoopNodes->Node(InletNode).MassFlowRate = 0.0;
        } else {
            // set inlet and outlet flows to zero}
        }
        // Basic relations:
        // 0. Outlet node flow rate and conditions= inlet flow rate and conditions (this might should be the last step instead, after everything is
        // calculated).
        //2022-01-07: moved this Step 0 to the very end:
        // state.dataLoopNodes->Node(OutletNode).MassFlowRate = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        // state.dataLoopNodes->Node(OutletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
        // state.dataLoopNodes->Node(OutletNode).HumRat = state.dataLoopNodes->Node(InletNode).HumRat;

        // 1. outlet node flow rate = Design flow rate * flow fraction schedule name for schedule flow control;
        // 1a. outlet node flow rate is proportional (maybe by 1.0) to supply flow rate for follow-supply;
        // 2. outlet node flow rate need to be >= than the min flow fraction * Design flow rate if scheduled flow;
        // 2a?. outlet node flow rate >= min fraction *(design flow rate still, or design supply flow, or something else?)
        // 2b. if 2 or 2a are not true, then set the flow rate to min

        Real64 DesignFlowRate = thisExhCtrl.DesignExhaustFlowRate;
        Real64 FlowFrac = EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.ExhaustFlowFractionScheduleNum);
        Real64 MinFlowFrac = EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.MinExhFlowFracScheduleNum);

        // 2022-01-27: Need to be refined more based on the schedule availability as well
        // if (thisExhCtrl.FlowControlTypeNum == 0) { // scheduled
        if (FlowFrac < MinFlowFrac) {
            FlowFrac = MinFlowFrac;
        } // else {
        //
        // }
        // MassFlow = DesignFlowRate * FlowFrac;
        // } else { // follow-supply
        // 2022-01: Deal with the node or nodelist flow sum etc.
        // }

        // 3. If the zone temperature < min zone temp schedule value, set flow to min fraction, the method would follow 2, 2a, and 2b.
        // 2022-01: try to adapt from SimZoneExhaustFan() in Fan.cc, probably need to use actual flow rate determinations,

        bool runExhaust = true;
        if (EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.AvailScheduleNum) > 0.0) { // available
            if (thisExhCtrl.MinZoneTempLimitScheduleNum > 0) {
                if (Tin >= EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.MinZoneTempLimitScheduleNum)) {
                    runExhaust = true;
                } else {
                    runExhaust = false;
                    FlowFrac = MinFlowFrac;
                }
            } else {
                runExhaust = true;
                // flow not changed
            }
        } else {
            runExhaust = false;
            FlowFrac = 0.0; // or directly set flow rate to zero.
        }
        // 2022-01-27: still need some logic to blend availabiltiy, frac, min fract, and flow rate together to get a single final flow number.
        double SupplyFlowRate = DesignFlowRate; // 2022-01: This needs an update of value when having the supply nodes sum
        if (thisExhCtrl.FlowControlTypeNum == 0) { // scheduled
            MassFlow = DesignFlowRate * FlowFrac;
        } else { // follow-supply
            MassFlow = SupplyFlowRate *FlowFrac;
        }

        // 4. balanced exhaust fraction // 2022-01: Here seems to be an example fo how fan zone exhaust use it:
        // from UpdateFan() in Fan.cc
        state.dataHVACGlobal->UnbalExhMassFlow = MassFlow; // Fan(FanNum).InletAirMassFlowRate;
        if (thisExhCtrl.BalancedExhFracScheduleNum > 0) {
            state.dataHVACGlobal->BalancedExhMassFlow = state.dataHVACGlobal->UnbalExhMassFlow * EnergyPlus::ScheduleManager::GetCurrentScheduleValue(
                                                                                                     state, thisExhCtrl.BalancedExhFracScheduleNum);
            state.dataHVACGlobal->UnbalExhMassFlow = state.dataHVACGlobal->UnbalExhMassFlow - state.dataHVACGlobal->BalancedExhMassFlow;
        } else {
            state.dataHVACGlobal->BalancedExhMassFlow = 0.0;
        }

        // Set the outlet conditions of the exhaust control (A good summary Step 0)
        state.dataLoopNodes->Node(OutletNode).MassFlowRate = MassFlow;
        state.dataLoopNodes->Node(OutletNode).Temp = state.dataLoopNodes->Node(InletNode).Temp;
        state.dataLoopNodes->Node(OutletNode).HumRat = state.dataLoopNodes->Node(InletNode).HumRat;
        state.dataLoopNodes->Node(OutletNode).Enthalpy = state.dataLoopNodes->Node(InletNode).Enthalpy;
        // Set the outlet nodes for properties that just pass through & not used
        state.dataLoopNodes->Node(OutletNode).Quality = state.dataLoopNodes->Node(InletNode).Quality;
        state.dataLoopNodes->Node(OutletNode).Press = state.dataLoopNodes->Node(InletNode).Press;

        // 2022-02: Just some test code to explore node elements; may not be necessary but may not do harm either:
        state.dataLoopNodes->Node(OutletNode).MassFlowRateMax = state.dataLoopNodes->Node(InletNode).MassFlowRateMax;
        state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail;

        // Set the Node Flow Control Variables from the Fan Control Variables? No need
        // state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = Fan(FanNum).MassFlowRateMaxAvail;
        // state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = Fan(FanNum).MassFlowRateMinAvail;

        //// these might also be useful to pass through (founnd in UpdateFan() in Fan.cc)
        // if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
        //    state.dataLoopNodes->Node(OutletNode).CO2 = state.dataLoopNodes->Node(InletNode).CO2;
        //}

        // if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
        //    state.dataLoopNodes->Node(OutletNode).GenContam = state.dataLoopNodes->Node(InletNode).GenContam;
        //}

        // finer details:
        // In step 3, use the zone temperature, or the the exhaust node temperature for comparision?
        // since there is an exhasut node temperature calculation in EnergyPlus considering the radiation details.
        // 2022-01-27: fan coil module used the fan inlet temperature as the comparision value.
    }

    void SizeExhaustSystem(EnergyPlusData &state)
    {

        // variables playground:        
        // state.dataHeatBal->ZoneAirMassFlow.
        // state.dataHeatBal->Zone(1).
         
        
        // Write fan sizing to eio report: example code in SizeFan() in Fan.cc:

        /*
        // Report fan, belt, motor, and VFD characteristics at design condition to .eio file
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Fan Airflow [m3/s]", FanVolFlow);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Fan Static Pressure Rise [Pa]", Fan(FanNum).DeltaPress);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Fan Shaft Power [W]", Fan(FanNum).FanShaftPower);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Motor Output Power [W]", Fan(FanNum).MotorMaxOutPwr);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design VFD Output Power [W]", Fan(FanNum).VFDMaxOutPwr);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Rated Power [W]", RatedPower);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Drive Ratio []", Fan(FanNum).PulleyDiaRatio);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Belt Output Torque [Nm]", Fan(FanNum).BeltMaxTorque);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Fan Efficiency  []", Fan(FanNum).FanWheelEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Maximum Belt Efficiency []", Fan(FanNum).BeltMaxEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Belt Efficiency []", Fan(FanNum).BeltEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Maximum Motor Efficiency []", Fan(FanNum).MotorMaxEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Motor Efficiency []", Fan(FanNum).MotEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design VFD Efficiency []", Fan(FanNum).VFDEff);
        BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Combined Efficiency []", Fan(FanNum).FanEff);
        */ 

    }

} // namespace ExhaustAirSystemManager

} // namespace EnergyPlus
