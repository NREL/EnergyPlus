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
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
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
    struct MixerBranchZone
    {
        int mixerInletNodeNum;
        int zoneExhaustNodeNum;
        int zoneNum;
        bool collected;

        // default constructor
        // Question: why the constructor skipped the first element std::string Name?
        MixerBranchZone() : mixerInletNodeNum(0), zoneExhaustNodeNum(0), zoneNum(0), collected(false)
        {
        }

        MixerBranchZone(int a, int b, int c, bool d) : mixerInletNodeNum(a), zoneExhaustNodeNum(b), zoneNum(c), collected(d)
        {
        }
    };

    std::vector<MixerBranchZone> mixerToZoneTable;
    // map might be aa better choice: 
    std::map<int, int> mixerBranchMap;

    bool mappingDone = false;

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
                bool zoneMixerErrFound = false;
                EnergyPlus::MixerComponent::GetZoneMixerIndex(state, zoneMixerName, zoneMixerIndex, zoneMixerErrFound, thisExhSys.Name);

                if (!zoneMixerErrFound) {
                    // With the correct MixerNum Initialize
                    EnergyPlus::MixerComponent::InitAirMixer(state, zoneMixerNum); // Initialize all Mixer related parameters

                    // See if need to do the zone mixer's CheckEquipName() function
                    ValidateComponent(state, "AirLoopHVAC:ZoneMixer", zoneMixerName, IsNotOK, "AirLoopHVAC:ExhaustSystem");
                    if (IsNotOK) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                        ShowContinueError(state, "ZoneMixer Name =" + zoneMixerName + " mismatch or not found.");
                        ErrorsFound = true;
                    } else {
                        // normal conditions
                    }
                }
                else {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "ZoneMixer Name =" + zoneMixerName + "not found.");
                    ErrorsFound = true;
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
                int centralFanIndex = -1; // zero based
                if (centralFanTypeNum == DataHVACGlobals::FanType_SystemModelObject) {
                    // 2022-02-04: This type is zero indexed.
                    // 2022-02-04: Need to process the System fan here first
                    state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, centralFanName));

                    centralFanIndex = HVACFan::getFanObjectVectorIndex(state, centralFanName); // zero-based
                    if (centralFanIndex >= 0) {
                        // normal index
                        SetupOutputVariable(state,
                                            "Central Exhaust Fan Outlet Air Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            thisExhSys.centralFan_MassFlowRate,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Average,
                                            thisExhSys.Name);

                        SetupOutputVariable(state,
                                            "Central Exhaust Fan Power",
                                            OutputProcessor::Unit::W,
                                            thisExhSys.centralFan_Power,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Average,
                                            thisExhSys.Name);

                        SetupOutputVariable(state,
                                            "Central Exhaust Fan Energy",
                                            OutputProcessor::Unit::J,
                                            thisExhSys.centralFan_Energy,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Summed,
                                            thisExhSys.Name);

                    } else {
                        centralFanIndex = -1;
                        // here a severe error message is needed
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                        ShowContinueError(state, "Fan Name =" + centralFanName + "not found.");
                        ErrorsFound = true;
                    }
                } else if (centralFanTypeNum == DataHVACGlobals::FanType_ComponentModel) {
                    // 2022-02-04: This type index starting from 1.

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

                            SetupOutputVariable(state,
                                                "Central Exhaust Fan Outlet Air Mass Flow Rate",
                                                OutputProcessor::Unit::kg_s,
                                                thisExhSys.centralFan_MassFlowRate,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                thisExhSys.Name);

                            SetupOutputVariable(state,
                                                "Central Exhaust Fan Power",
                                                OutputProcessor::Unit::W,
                                                thisExhSys.centralFan_Power,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                thisExhSys.Name);

                            SetupOutputVariable(state,
                                                "Central Exhaust Fan Energy",
                                                OutputProcessor::Unit::J,
                                                thisExhSys.centralFan_Energy,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Summed,
                                                thisExhSys.Name);

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
            state.dataZoneEquip->NumExhaustAirSystems = numExhaustSystems;

        } else {
            // If no exhaust systems are defined, then do something <or nothing>:
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found getting AirLoopHVAC:ExhaustSystem.  Preceding condition(s) causes termination.");
        }

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
        } else {
            // 2022-02: Give a warning that the current model does not work with AirflowNetwork for now
        
        }

        // 2022-01-26: One additional step here might be to consider the avaiability schedule of the exhasut system
        // or the central exhaust fan's own avaiability schedule.
        // Need to prodeed differently for the cases when the exhasut system (fan) is available or not available.

        // 2022-01: Simulate the fan (need some clean up 2022-01-26)
        // calculate fan speed ratio for Fan:OnOff or Fan:SystemModel (not used for other fan types). 
        Real64 FanAirVolFlow = 1.0; // 2022-01: this should be something like a design or rate fan flow rate
        Real64 FanSpeedRatio = 1.0; //  Node(InletNode).MassFlowRate / (FanAirVolFlow * state.dataEnvrn->StdRhoAir);

        // Constant fan and variable flow calculation AND variable fan
        auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;

        if (state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanTypeNum == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
            state.dataHVACFan->fanObjs[state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanIndex]->simulate(state, _, _, _, _);
        
            // Update report variables
            int outletNode_Num = state.dataHVACFan->fanObjs[state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanIndex]->outletNodeNum;
            state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).centralFan_MassFlowRate = state.dataLoopNodes->Node(outletNode_Num).MassFlowRate;

            state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).centralFan_Power =
                state.dataHVACFan->fanObjs[state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanIndex]->fanPower();

            state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).centralFan_Energy =
                state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).centralFan_Power * state.dataHVACGlobal->TimeStepSys *
                DataGlobalConstants::SecInHour;
        
        } else if (state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanTypeNum == DataHVACGlobals::FanType_ComponentModel) {
            // 2022-01: Component model fan
            // 2022-01: use a simpler call instead (checkout SimAirServingZones.cc)
            Fans::SimulateFanComponents(state,
                                        state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanName,
                                        FirstHVACIteration,
                                        state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanIndex); //,
            // FanSpeedRatio, // ZoneCompTurnFansOn, // ZoneCompTurnFansOff);
        
            // Update output variables

            auto &fancomp = state.dataFans->Fan(state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).CentralFanIndex);

            state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).centralFan_MassFlowRate = fancomp.OutletAirMassFlowRate;

            state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).centralFan_Power = fancomp.FanPower * 1000.0;

            state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum).centralFan_Power = fancomp.FanEnergy * 1000.0;
        }

        // 2022-01: Determine if there are some "iteration" or revisit step for the zone mixer and fan simulation
    }

    void ReportExhaustAirSystem([[maybe_unused]] int &ExhaustAirSystemNum) // maybe unused
    {
        // Date: Jan 2022
    }

    void GetZoneExhaustControlInput(EnergyPlusData &state)
    {
        // This function is for the ZoneExhaust Control input processing;
        //
        // Using/Aliasing
        // using NodeInputManager::GetOnlySingleNode;
        using namespace DataLoopNode;

        // Locals
        int PathNum;
        int CompNum;
        int NumAlphas;
        int NumNums;
        int IOStat;
        int Counter;
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
                int inletNodeNum =
                    EnergyPlus::NodeInputManager::GetOnlySingleNode(state,
                                                                    inletNodeName,
                                                                    ErrorsFound,
                                                                    EnergyPlus::DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl,
                                                                    thisExhCtrl.Name,
                                                                    EnergyPlus::DataLoopNode::NodeFluidType::Air,
                                                                    EnergyPlus::DataLoopNode::ConnectionType::Inlet,
                                                                    EnergyPlus::NodeInputManager::CompFluidStream::Primary,
                                                                    EnergyPlus::DataLoopNode::ObjectIsParent);
                thisExhCtrl.InletNodeNum = inletNodeNum;

                std::string outletNodeName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "outlet_node_name");

                int outletNodeNum = EnergyPlus::NodeInputManager::GetOnlySingleNode(state,
                                                      outletNodeName,
                                                      ErrorsFound,
                                                      EnergyPlus::DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl,
                                                      thisExhCtrl.Name,
                                                      EnergyPlus::DataLoopNode::NodeFluidType::Air,
                                                      EnergyPlus::DataLoopNode::ConnectionType::Outlet,
                                                      EnergyPlus::NodeInputManager::CompFluidStream::Primary,
                                                      EnergyPlus::DataLoopNode::ObjectIsParent);
                thisExhCtrl.OutletNodeNum = outletNodeNum;

                if (!mappingDone) {
                    mixerToZoneTable.emplace_back(MixerBranchZone{outletNodeNum, inletNodeNum, zoneNum, true});
                    // map could be a better solution:
                    mixerBranchMap.emplace(outletNodeNum, zoneNum);
                }

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
                // Array1D_int NodeNums;

                Array1D_int supplyNodeOrNodelistArray; // 2022-01: this needs some extra allocation and initialization
                // still having a problem getting it debugged properly. Maybe for now replace with a single node to move forward.
                // just a temp way to test further:
                //bool singlenodeyes = true;
                //int supplynodenum_single = 0;
                //if (singlenodeyes) {
                //    supplynodenum_single = GetOnlySingleNode(state,
                //                                          supplyNodeOrNodelistName,
                //                                          ErrorsFound,
                //                                          DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl, // may need to change type, maybe zone inlets?
                //                                          thisExhCtrl.Name,
                //                                          DataLoopNode::NodeFluidType::Air,
                //                                          DataLoopNode::ConnectionType::Sensor,
                //                                          NodeInputManager::CompFluidStream::Primary,
                //                                          ObjectIsParent);
                //    thisExhCtrl.SupplyNodeOrNodelistNum = supplynodenum_single;
                //} else {
                
                // 2022-02: Refer to GetZoneEquipmentData() in DataZoneEquipment.cc:
                ip->getObjectDefMaxArgs(state, "NodeList", NumParams, NumAlphas, NumNums);
                thisExhCtrl.SuppNodeNums.dimension(NumParams, 0);
                EnergyPlus::NodeInputManager::GetNodeNums(state,
                            supplyNodeOrNodelistName,
                            NumNodes,
                            thisExhCtrl.SuppNodeNums,
                            NodeListError,
                            EnergyPlus::DataLoopNode::NodeFluidType::Air,
                            EnergyPlus::DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl, // maybe zone inlets?
                            thisExhCtrl.Name,
                            EnergyPlus::DataLoopNode::ConnectionType::Sensor,
                            EnergyPlus::NodeInputManager::CompFluidStream::Primary,
                            EnergyPlus::DataLoopNode::ObjectIsNotParent); // ,
                                                // _,
                                                // supplyNodeOrNodelistName);
                thisExhCtrl.SupplyNodeOrNodelistNum = supplyNodeOrNodelistNum;
                // 2022-02: thisExhCtrl.SuppNodeNums already updated in the above function call.
                // 2022-02: Following the node/nodelist checkout, it also need to verify these nodes are "indeed" supply nodes.

                // 2022-02: Deal with design exhaust auto size here;
                if (thisExhCtrl.DesignExhaustFlowRate == EnergyPlus::DataSizing::AutoSize) {
                    SizeExhaustControlFlow(state, exhCtrlNum, thisExhCtrl.SuppNodeNums);
                }

                std::string minZoneTempLimitScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "minimum_zone_temperature_limit_schedule_name");
                int minZoneTempLimitScheduleNum = 0;
                minZoneTempLimitScheduleNum = ScheduleManager::GetScheduleIndex(state, minZoneTempLimitScheduleName);

                if (minZoneTempLimitScheduleNum > 0) {
                    // normal conditions
                } else if (minZoneTempLimitScheduleNum == 0) {
                    // blank or anything like that, treat as no comparision
                } else {
                    minZoneTempLimitScheduleNum = 0;
                    // a regular warning
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
                    // blank, meaning minimum is zero
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
                    // blank, treated as not activated
                } else {
                    balancedExhFracScheduleNum = 0;
                    // a regular warning would do.
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Schedule Name =" + balancedExhFracScheduleName + "not found.");
                    // ErrorsFound = true;
                }

                // 2022-01-27: Need an additional check per IORef:
                // This input field must be blank when the zone air flow balance is enforced. If user specifies a schedule and zone air flow balance is
                //     enforced, then EnergyPlus throws a warning error message, ignores the schedule and simulation continues.

                thisExhCtrl.BalancedExhFracScheduleNum = balancedExhFracScheduleNum;
            }

            state.dataZoneEquip->NumZoneExhaustControls = numZoneExhaustControls; // or exhCtrlNum? */
 
            // Done with creating a map that contains a table of for each zone to exhasut controls
             mappingDone = true;
        } else {
            // If no exhaust systems are defined, then do something <or nothing>:
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found getting ZoneHVAC:ExhaustControl.  Preceding condition(s) causes termination.");
        }
    }

    void SimZoneHVACExhaustControls(EnergyPlusData &state, bool FirstHVACIteration)
    {
        // 2022-01: calling steps:
 
        // Locals
        int ExhaustControlNum;

        // 2022-01: Step 1:
        if (state.dataExhCtrlSystemMrg->GetInputFlag) { // First time subroutine has been entered
            GetZoneExhaustControlInput(state);
            state.dataExhCtrlSystemMrg->GetInputFlag = false;
        }

        // 2022-01: Step 2 and/or 3: initialize or sizing if needed
        // 2022-02: done in input processing:

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

        // std::string const idf_objects = delimited_string({
        //    "ZoneHVAC:ExhaustControl,",
        //    "    Zone1 Exhaust Control,          !-Name",
        //    "    HVACOperationSchd,              !- Availability Schedule Name",
        //    "    Zone2,                          !- Zone Name",
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

        Real64 DesignFlowRate = thisExhCtrl.DesignExhaustFlowRate;
        Real64 FlowFrac = 0.0; 
        if (thisExhCtrl.MinExhFlowFracScheduleNum > 0) {
            FlowFrac = EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.ExhaustFlowFractionScheduleNum);
        }
        Real64 MinFlowFrac = 0.0;
        if (thisExhCtrl.MinExhFlowFracScheduleNum > 0) {
            MinFlowFrac = EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.MinExhFlowFracScheduleNum);
        }
        // 2022-01-27: Need to be refined more based on the schedule availability as well
        if (FlowFrac < MinFlowFrac) {
            FlowFrac = MinFlowFrac;
        } 

        bool runExhaust = true;
        if (thisExhCtrl.AvailScheduleNum == 0 || (thisExhCtrl.AvailScheduleNum > 0 && EnergyPlus::ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.AvailScheduleNum) >
            0.0)) { // available
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

        if (thisExhCtrl.FlowControlTypeNum == 0) { // scheduled
            MassFlow = DesignFlowRate * FlowFrac;
        } else { // follow-supply
            Real64 supplyFlowRate = 0.0;
            int numOfSuppNodes = thisExhCtrl.SuppNodeNums.size();
            for (int i = 1; i <= numOfSuppNodes; ++i) {
                supplyFlowRate += state.dataLoopNodes->Node(thisExhCtrl.SuppNodeNums(i)).MassFlowRate;
            }
            MassFlow = supplyFlowRate *FlowFrac;
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
        state.dataLoopNodes->Node(OutletNode).MassFlowRateMaxAvail = state.dataLoopNodes->Node(InletNode).MassFlowRateMaxAvail;
        state.dataLoopNodes->Node(OutletNode).MassFlowRateMinAvail = state.dataLoopNodes->Node(InletNode).MassFlowRateMinAvail;

        // these might also be useful to pass through (founnd in UpdateFan() in Fan.cc)
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            state.dataLoopNodes->Node(OutletNode).CO2 = state.dataLoopNodes->Node(InletNode).CO2;
        }

        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            state.dataLoopNodes->Node(OutletNode).GenContam = state.dataLoopNodes->Node(InletNode).GenContam;
        }

        // finer details:
        // In step 3, use the zone temperature, or the the exhaust node temperature for comparision?
        // since there is an exhasut node temperature calculation in EnergyPlus considering the radiation details.
        // 2022-01-27: fan coil module used the fan inlet temperature as the comparision value.
    }

    void SizeExhaustSystem(EnergyPlusData &state)
    {
        // Sizing and write fan sizing to eio report: example code in SizeFan() in Fan.cc:
        //// Report fan, belt, motor, and VFD characteristics at design condition to .eio file
        // BaseSizer::reportSizerOutput(state, Fan(FanNum).FanType, Fan(FanNum).FanName, "Design Fan Airflow [m3/s]", FanVolFlow);
    }

    void SizeExhaustControlFlow(EnergyPlusData &state, int zoneExhCtrlNum, Array1D_int & NodeNums)
    {
        auto &thisExhCtrl = state.dataZoneEquip->ZoneExhaustControlSystem(zoneExhCtrlNum);

        Real64 designFlow = 0.0;
 
        if (thisExhCtrl.FlowControlTypeNum == 1) { // FollowSupply
            // size based on supply nodelist flow
            // for (auto instance = NodeNums.begin(); instance != NodeNums.end(); ++instance) {
            for (int i = 1; i <= NodeNums.size(); ++i)
            {
                designFlow += state.dataLoopNodes->Node(NodeNums(i)).MassFlowRateMax;
            }
        } else { // scheduled etc.
            // based on zone OA.
            designFlow = state.dataSize->FinalZoneSizing(thisExhCtrl.ZoneNum).MinOA;
        }

        thisExhCtrl.DesignExhaustFlowRate = designFlow;
    }

    void CheckForSupplyNode(EnergyPlusData &state, int const SupplyNodeNum, bool &NodeNotFound)
    {
        // 2022-02: trying to check a node to see if it is truely a supply node
        //          for a nodelist, need a call loop to check each node in the list
        // SUBROUTINE INFORMATION:
        // This subroutine checks that the supply node number matches the air inlet node number of some zone or airloop hvac systems
        // Refer to some exapmle code in CheckForSensorAndSetPointNode() or CheckActuatorNode() in WaterCoils.cc
    }

} // namespace ExhaustAirSystemManager

} // namespace EnergyPlus
