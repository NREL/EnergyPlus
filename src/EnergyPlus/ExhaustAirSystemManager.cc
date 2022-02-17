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
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
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
// #include <EnergyPlus/ZonePlenum.hh> //2022-01-14: may not needed for this exhaust system

namespace EnergyPlus {

namespace ExhaustAirSystemManager {
    // Module containing the routines dealing with the AirLoopHVAC:ExhaustSystem

    // map might be aa better choice:
    // std::map<int, int> mixerBranchMap;
    std::map<int, int> mixerIndexMap;

    bool mappingDone = false;

    void SimExhaustAirSystem(EnergyPlusData &state, bool FirstHVACIteration)
    {
        // Locals
        int ExhaustAirSystemNum;

        // Obtains and Allocates Mixer related parameters from input file
        if (state.dataExhAirSystemMrg->GetInputFlag) { // First time subroutine has been entered
            GetExhaustAirSystemInput(state);
            state.dataExhAirSystemMrg->GetInputFlag = false;
        }

        for (ExhaustAirSystemNum = 1; ExhaustAirSystemNum <= state.dataZoneEquip->NumExhaustAirSystems; ++ExhaustAirSystemNum) {
            CalcExhaustAirSystem(state, ExhaustAirSystemNum, FirstHVACIteration);
        }

        // After this, update the exhaust flows according to zone grouping:
        UpdateZoneExhaustControl(state);
    }

    void GetExhaustAirSystemInput(EnergyPlusData &state)
    {
        // Locals
        bool IsNotOK; // Flag to verify name

        bool ErrorsFound = false;

        if (allocated(state.dataZoneEquip->ExhaustAirSystem)) {
            return;
        }

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

                std::string zoneMixerName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "zone_mixer_name");
                int zoneMixerIndex = 0;
                bool zoneMixerErrFound = false;
                MixerComponent::GetZoneMixerIndex(state, zoneMixerName, zoneMixerIndex, zoneMixerErrFound, thisExhSys.Name);

                if (!zoneMixerErrFound) {
                    // With the correct MixerNum Initialize
                    MixerComponent::InitAirMixer(state, zoneMixerIndex); // Initialize all Mixer related parameters

                    // See if need to do the zone mixer's CheckEquipName() function
                    ValidateComponent(state, "AirLoopHVAC:ZoneMixer", zoneMixerName, IsNotOK, "AirLoopHVAC:ExhaustSystem");
                    if (IsNotOK) {
                        ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                        ShowContinueError(state, "ZoneMixer Name =" + zoneMixerName + " mismatch or not found.");
                        ErrorsFound = true;
                    } else {
                        // normal conditions
                    }
                } else {
                    ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "ZoneMixer Name =" + zoneMixerName + "not found.");
                    ErrorsFound = true;
                }
                thisExhSys.ZoneMixerName = zoneMixerName;
                thisExhSys.ZoneMixerIndex = zoneMixerIndex;

                std::string centralFanType = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "fan_object_type");
                int centralFanTypeNum = 0;
                // getEnumerationValue(DataDaylighting::LtgCtrlTypeNamesUC, UtilityRoutines::MakeUPPERCase(state.dataIPShortCut->cAlphaArgs(5)));

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
                int availSchNum = 0;
                int centralFanIndex = -1; // zero based or 1 based
                if (centralFanTypeNum == DataHVACGlobals::FanType_SystemModelObject) {
                    // This type is zero indexed.
                    state.dataHVACFan->fanObjs.emplace_back(new HVACFan::FanSystem(state, centralFanName));

                    centralFanIndex = HVACFan::getFanObjectVectorIndex(state, centralFanName); // zero-based
                    if (centralFanIndex >= 0) {
                        availSchNum = state.dataHVACFan->fanObjs[centralFanIndex]->availSchedIndex;
                        // normal index
                        SetupOutputVariable(state,
                                            "Central Exhaust Fan Outlet Air Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            thisExhSys.centralFan_MassFlowRate,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Average,
                                            thisExhSys.Name);

                        SetupOutputVariable(state,
                                            "Central Exhaust Fan Outlet Air Volume Flow Rate Standard",
                                            OutputProcessor::Unit::m3_s,
                                            thisExhSys.centralFan_VolumeFlowRate_Std,
                                            OutputProcessor::SOVTimeStepType::System,
                                            OutputProcessor::SOVStoreType::Average,
                                            thisExhSys.Name);

                        SetupOutputVariable(state,
                                            "Central Exhaust Fan Outlet Air Volume Flow Rate Current",
                                            OutputProcessor::Unit::m3_s,
                                            thisExhSys.centralFan_VolumeFlowRate_Cur,
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
                    // This type's fan index starting from 1.
                    bool isNotOK(false);
                    int fanType_Num_Check(0);
                    Fans::GetFanType(state, centralFanName, fanType_Num_Check, isNotOK, cCurrentModuleObject, thisExhSys.Name);

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
                            bool errFlag(false);
                            Fans::GetFanIndex(state, centralFanName, centralFanIndex, errFlag);

                            availSchNum = state.dataFans->Fan(centralFanIndex).AvailSchedPtrNum;

                            BranchNodeConnections::SetUpCompSets(state,
                                                                 cCurrentModuleObject,
                                                                 thisExhSys.Name,
                                                                 centralFanType,
                                                                 centralFanName,
                                                                 state.dataLoopNodes->NodeID(state.dataFans->Fan(centralFanIndex).InletNodeNum),
                                                                 state.dataLoopNodes->NodeID(state.dataFans->Fan(centralFanIndex).OutletNodeNum));

                            SetupOutputVariable(state,
                                                "Central Exhaust Fan Outlet Air Mass Flow Rate",
                                                OutputProcessor::Unit::kg_s,
                                                thisExhSys.centralFan_MassFlowRate,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                thisExhSys.Name);

                            SetupOutputVariable(state,
                                                "Central Exhaust Fan Outlet Air Volume Flow Rate Standard",
                                                OutputProcessor::Unit::m3_s,
                                                thisExhSys.centralFan_VolumeFlowRate_Std,
                                                OutputProcessor::SOVTimeStepType::System,
                                                OutputProcessor::SOVStoreType::Average,
                                                thisExhSys.Name);

                            SetupOutputVariable(state,
                                                "Central Exhaust Fan Outlet Air Volume Flow Rate Current",
                                                OutputProcessor::Unit::m3_s,
                                                thisExhSys.centralFan_VolumeFlowRate_Cur,
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

                if (availSchNum > 0) {
                    // normal conditions
                } else if (availSchNum == 0) {
                    // blank or anything like that, treat as always avaialabe
                } else { // no match
                    availSchNum = 0;
                    // a regular warning
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                    ShowContinueError(state, "Could not find a match for Central Exhaust Fan's Avaiability Schedule.");
                    ShowContinueError(state, "It will be treated as always available.");
                }
                thisExhSys.AvailScheduleNum = availSchNum;

                // sizing
                if (thisExhSys.SizingFlag) {
                    SizeExhaustSystem(state, exhSysNum);
                }
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
    }

    void CalcExhaustAirSystem(EnergyPlusData &state, int const ExhaustAirSystemNum, bool FirstHVACIteration)
    {
        auto &thisExhSys = state.dataZoneEquip->ExhaustAirSystem(ExhaustAirSystemNum);

        std::string RoutineName = "CalExhaustAirSystem: ";
        std::string cCurrentModuleObject = "AirloopHVAC:ExhaustSystem";
        bool ErrorsFound = false;
        if (!(state.dataAirflowNetwork->AirflowNetworkFanActivated &&
              state.dataAirflowNetwork->SimulateAirflowNetwork > AirflowNetwork::AirflowNetworkControlMultizone)) {
            MixerComponent::SimAirMixer(state, thisExhSys.ZoneMixerName, thisExhSys.ZoneMixerIndex);
        } else {
            // Give a warning that the current model does not work with AirflowNetwork for now
            ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
            ShowContinueError(state, "AirloopHVAC:ExhaustSystem currently does not work with AirflowNetwork.");
            ErrorsFound = true;
        }

        if (ErrorsFound) {
            ShowFatalError(state, "Errors found conducting CalcExhasutAirSystem(). Preceding condition(s) causes termination.");
        }

        Real64 mixerFlow_Prior = 0.0;
        int outletNode_index = state.dataMixerComponent->MixerCond(thisExhSys.ZoneMixerIndex).OutletNode;
        mixerFlow_Prior = state.dataLoopNodes->Node(outletNode_index).MassFlowRate;
        if (mixerFlow_Prior == 0.0) {
            // No flow coming out from the exhaust controls;
            // fan should be cut off now;
        }

        // Real64 FanAirVolFlow = 1.0; // This should be something like a design or rate fan flow rate
        // Real64 FanSpeedRatio = 1.0; //  Node(InletNode).MassFlowRate / (FanAirVolFlow * state.dataEnvrn->StdRhoAir);

        // Constant fan and variable flow calculation AND variable fan
        // auto &ZoneCompTurnFansOff = state.dataHVACGlobal->ZoneCompTurnFansOff;
        // auto &ZoneCompTurnFansOn = state.dataHVACGlobal->ZoneCompTurnFansOn;

        int outletNode_Num = 0;

        if (thisExhSys.CentralFanTypeNum == DataHVACGlobals::FanType_SystemModelObject) {
            state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
            state.dataHVACFan->fanObjs[thisExhSys.CentralFanIndex]->simulate(state, _, _, _, _);

            // Update report variables
            outletNode_Num = state.dataHVACFan->fanObjs[thisExhSys.CentralFanIndex]->outletNodeNum;

            thisExhSys.centralFan_MassFlowRate = state.dataLoopNodes->Node(outletNode_Num).MassFlowRate;

            thisExhSys.centralFan_VolumeFlowRate_Std = state.dataLoopNodes->Node(outletNode_Num).MassFlowRate / state.dataEnvrn->StdRhoAir;

            thisExhSys.centralFan_VolumeFlowRate_Cur =
                state.dataLoopNodes->Node(outletNode_Num).MassFlowRate / state.dataLoopNodes->MoreNodeInfo(outletNode_Num).Density;

            thisExhSys.centralFan_Power = state.dataHVACFan->fanObjs[thisExhSys.CentralFanIndex]->fanPower();

            thisExhSys.centralFan_Energy = thisExhSys.centralFan_Power * state.dataHVACGlobal->TimeStepSys * DataGlobalConstants::SecInHour;

        } else if (thisExhSys.CentralFanTypeNum == DataHVACGlobals::FanType_ComponentModel) {
            Fans::SimulateFanComponents(state, thisExhSys.CentralFanName, FirstHVACIteration,
                                        thisExhSys.CentralFanIndex); //,
            // FanSpeedRatio, // ZoneCompTurnFansOn, // ZoneCompTurnFansOff);

            // Update output variables
            auto &fancomp = state.dataFans->Fan(thisExhSys.CentralFanIndex);

            thisExhSys.centralFan_MassFlowRate = fancomp.OutletAirMassFlowRate;

            thisExhSys.centralFan_VolumeFlowRate_Std = fancomp.OutletAirMassFlowRate / state.dataEnvrn->StdRhoAir;

            thisExhSys.centralFan_VolumeFlowRate_Cur = fancomp.OutletAirMassFlowRate / state.dataLoopNodes->MoreNodeInfo(outletNode_Num).Density;

            thisExhSys.centralFan_Power = fancomp.FanPower * 1000.0;

            thisExhSys.centralFan_Energy = fancomp.FanEnergy * 1000.0;
        }

        Real64 mixerFlow_Posterior = 0.0;
        mixerFlow_Posterior = state.dataLoopNodes->Node(outletNode_index).MassFlowRate;
        if (mixerFlow_Posterior < DataHVACGlobals::SmallMassFlow) {
            // fan flow is nearly zero and should be considered off
            // but this still can use the ratio
        }
        if (mixerFlow_Prior < DataHVACGlobals::SmallMassFlow) {
            // this is the case where the fan flow should be resetted to zeros and not run the ratio
        }
        if ((mixerFlow_Prior - mixerFlow_Posterior > DataHVACGlobals::SmallMassFlow) ||
            (mixerFlow_Prior - mixerFlow_Posterior < -DataHVACGlobals::SmallMassFlow)) {
            // calculate a ratio
            Real64 flowRatio = mixerFlow_Posterior / mixerFlow_Prior;
            if (flowRatio > 1.0) {
                ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhSys.Name);
                ShowContinueError(state, "Requested flow rate is lower than the exhasut fan flow rate.");
                ShowContinueError(state, "Will scale up the requested flow rate to meet fan flow rate.");
            }

            // get the mixer inlet node index
            int zoneMixerIndex = thisExhSys.ZoneMixerIndex;
            for (int i = 1; i <= state.dataMixerComponent->MixerCond(zoneMixerIndex).NumInletNodes; ++i) {
                int exhLegIndex = mixerIndexMap[state.dataMixerComponent->MixerCond(zoneMixerIndex).InletNode(i)];
                CalcZoneHVACExhaustControl(state, exhLegIndex, flowRatio);
            }

            // Simulate the mixer again to update the flow
            MixerComponent::SimAirMixer(state, thisExhSys.ZoneMixerName, thisExhSys.ZoneMixerIndex);

            // if the adjustment matches, then no need to run fan simulation again.
        }
    }

    void ReportExhaustAirSystem([[maybe_unused]] int &ExhaustAirSystemNum) // maybe unused
    {
    }

    void GetZoneExhaustControlInput(EnergyPlusData &state)
    {
        // This function is for the ZoneExhaust Control input processing;

        // Locals
        int NumAlphas;
        int NumNums;
        bool ErrorsFound = false;

        // Use the json helper to process input
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
                    // blank or anything like that, treat as always available
                } else {
                    availSchNum = 0;
                    // a regular warning
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Avaiability Manager Name = " + availSchName + "not found.");
                }
                thisExhCtrl.AvailScheduleNum = availSchNum;

                std::string zoneName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "zone_name");
                thisExhCtrl.ZoneName = zoneName;
                int zoneNum = UtilityRoutines::FindItemInList(zoneName, state.dataHeatBal->Zone);
                thisExhCtrl.ZoneNum = zoneNum;

                thisExhCtrl.ControlledZoneNum = UtilityRoutines::FindItemInList(zoneName, state.dataHeatBal->Zone);

                // These two nodes are required inputs:
                std::string inletNodeName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "inlet_node_name");
                int inletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                       inletNodeName,
                                                                       ErrorsFound,
                                                                       DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl,
                                                                       thisExhCtrl.Name,
                                                                       DataLoopNode::NodeFluidType::Air,
                                                                       DataLoopNode::ConnectionType::Inlet,
                                                                       NodeInputManager::CompFluidStream::Primary,
                                                                       DataLoopNode::ObjectIsParent);
                thisExhCtrl.InletNodeNum = inletNodeNum;

                std::string outletNodeName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "outlet_node_name");

                int outletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                        outletNodeName,
                                                                        ErrorsFound,
                                                                        DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl,
                                                                        thisExhCtrl.Name,
                                                                        DataLoopNode::NodeFluidType::Air,
                                                                        DataLoopNode::ConnectionType::Outlet,
                                                                        NodeInputManager::CompFluidStream::Primary,
                                                                        DataLoopNode::ObjectIsParent);
                thisExhCtrl.OutletNodeNum = outletNodeNum;

                if (!mappingDone) {
                    // mixerBranchMap.emplace(outletNodeNum, zoneNum);
                    mixerIndexMap.emplace(outletNodeNum, exhCtrlNum);
                }

                Real64 designExhaustFlowRate = ip->getRealFieldValue(objectFields, objectSchemaProps, "design_exhaust_flow_rate_");
                thisExhCtrl.DesignExhaustFlowRate = designExhaustFlowRate;

                std::string flowControlType = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "flow_control_type");
                int flowControlTypeNum = 0;
                thisExhCtrl.FlowControlTypeNum = flowControlTypeNum;

                std::string exhaustFlowFractionScheduleName =
                    ip->getAlphaFieldValue(objectFields, objectSchemaProps, "exhaust_flow_fraction_schedule_name");
                // Schedule matching
                int exhaustFlowFractionScheduleNum = 0;
                exhaustFlowFractionScheduleNum = ScheduleManager::GetScheduleIndex(state, exhaustFlowFractionScheduleName);

                if (exhaustFlowFractionScheduleNum > 0) {
                    // normal conditions
                } else if (exhaustFlowFractionScheduleNum == 0) {
                    // blank or anything like that, treat as always available?
                } else {
                    exhaustFlowFractionScheduleNum = 0;
                    // a regular warning
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Schedule Name =" + exhaustFlowFractionScheduleName + "not found.");
                }
                thisExhCtrl.ExhaustFlowFractionScheduleNum = exhaustFlowFractionScheduleNum;

                std::string supplyNodeOrNodelistName = ip->getAlphaFieldValue(objectFields, objectSchemaProps, "supply_node_or_nodelist_name");
                int supplyNodeOrNodelistNum = 0;

                bool NodeListError = false;
                int NumNum = 0;
                int NumParams = 0;
                int NumNodes = 0;

                ip->getObjectDefMaxArgs(state, "NodeList", NumParams, NumAlphas, NumNums);
                thisExhCtrl.SuppNodeNums.dimension(NumParams, 0);
                NodeInputManager::GetNodeNums(state,
                                              supplyNodeOrNodelistName,
                                              NumNodes,
                                              thisExhCtrl.SuppNodeNums,
                                              NodeListError,
                                              DataLoopNode::NodeFluidType::Air,
                                              DataLoopNode::ConnectionObjectType::ZoneHVACExhaustControl, // maybe zone inlets?
                                              thisExhCtrl.Name,
                                              DataLoopNode::ConnectionType::Sensor,
                                              NodeInputManager::CompFluidStream::Primary,
                                              DataLoopNode::ObjectIsNotParent); // ,
                                                                                // _,
                                                                                // supplyNodeOrNodelistName);
                thisExhCtrl.SupplyNodeOrNodelistNum = supplyNodeOrNodelistNum;
                // Verify these nodes are indeed supply nodes:
                bool nodeNotFound = false;
                if (thisExhCtrl.FlowControlTypeNum == 1) { // FollowSupply
                    for (size_t i = 1; i <= thisExhCtrl.SuppNodeNums.size(); ++i) {
                        CheckForSupplyNode(state, thisExhCtrl.SuppNodeNums(i), nodeNotFound);
                        if (nodeNotFound) {
                            ShowSevereError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                            ShowContinueError(state, "Node or NodeList Name =" + supplyNodeOrNodelistName + ". Must all be supply node(s).");
                            ErrorsFound = true;
                        }
                    }
                }

                // Deal with design exhaust auto size here;
                if (thisExhCtrl.DesignExhaustFlowRate == DataSizing::AutoSize) {
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
                    // a regular warning
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Schedule Name =" + minExhFlowFracScheduleName + "not found.");
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
                    // a regular warning
                    ShowWarningError(state, RoutineName + cCurrentModuleObject + "=" + thisExhCtrl.Name);
                    ShowContinueError(state, "Schedule Name =" + balancedExhFracScheduleName + "not found.");
                }

                // Maybe an additional check per IORef:
                // This input field must be blank when the zone air flow balance is enforced. If user specifies a schedule and zone air flow balance
                // is enforced, then EnergyPlus throws a warning error message, ignores the schedule and simulation continues.

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

    void SimZoneHVACExhaustControls(EnergyPlusData &state)
    {
        // Locals
        int ExhaustControlNum;

        if (state.dataExhCtrlSystemMrg->GetInputFlag) { // First time subroutine has been entered
            GetZoneExhaustControlInput(state);
            state.dataExhCtrlSystemMrg->GetInputFlag = false;
        }

        for (ExhaustControlNum = 1; ExhaustControlNum <= state.dataZoneEquip->NumZoneExhaustControls; ++ExhaustControlNum) {
            CalcZoneHVACExhaustControl(state, ExhaustControlNum, _);
        }

        // report results if needed
    }

    void CalcZoneHVACExhaustControl(EnergyPlusData &state, int const ZoneHVACExhaustControlNum, Optional<bool const> FlowRatio)
    {
        // Calculate a zonehvac exhaust control system

        auto &thisExhCtrl = state.dataZoneEquip->ZoneExhaustControlSystem(ZoneHVACExhaustControlNum);
        int InletNode = thisExhCtrl.InletNodeNum;
        int OutletNode = thisExhCtrl.OutletNodeNum;
        auto &thisExhInlet = state.dataLoopNodes->Node(InletNode);
        auto &thisExhOutlet = state.dataLoopNodes->Node(OutletNode);
        Real64 MassFlow = state.dataLoopNodes->Node(InletNode).MassFlowRate;
        Real64 Tin = state.dataHeatBalFanSys->ZT(thisExhCtrl.ZoneNum);

        if (present(FlowRatio)) {
            thisExhCtrl.BalancedFlow *= FlowRatio;
            thisExhCtrl.UnbalancedFlow *= FlowRatio;

            thisExhInlet.MassFlowRate *= FlowRatio;
        } else {
            // Availability schedule:
            if (ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.AvailScheduleNum) <= 0.0) {
                MassFlow = 0.0;
                thisExhInlet.MassFlowRate = 0.0;
            } else {
                //
            }

            Real64 DesignFlowRate = thisExhCtrl.DesignExhaustFlowRate;
            Real64 FlowFrac = 0.0;
            if (thisExhCtrl.MinExhFlowFracScheduleNum > 0) {
                FlowFrac = ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.ExhaustFlowFractionScheduleNum);
            }

            Real64 MinFlowFrac = 0.0;
            if (thisExhCtrl.MinExhFlowFracScheduleNum > 0) {
                MinFlowFrac = ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.MinExhFlowFracScheduleNum);
            }

            if (FlowFrac < MinFlowFrac) {
                FlowFrac = MinFlowFrac;
            }

            bool runExhaust = true;
            if (thisExhCtrl.AvailScheduleNum == 0 ||
                (thisExhCtrl.AvailScheduleNum > 0 &&
                 ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.AvailScheduleNum) > 0.0)) { // available
                if (thisExhCtrl.MinZoneTempLimitScheduleNum > 0) {
                    if (Tin >= ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.MinZoneTempLimitScheduleNum)) {
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
                FlowFrac = 0.0; // directly set flow rate to zero.
            }

            if (thisExhCtrl.FlowControlTypeNum == 0) { // scheduled
                MassFlow = DesignFlowRate * FlowFrac;
            } else { // follow-supply
                Real64 supplyFlowRate = 0.0;
                int numOfSuppNodes = thisExhCtrl.SuppNodeNums.size();
                for (int i = 1; i <= numOfSuppNodes; ++i) {
                    supplyFlowRate += state.dataLoopNodes->Node(thisExhCtrl.SuppNodeNums(i)).MassFlowRate;
                }
                MassFlow = supplyFlowRate * FlowFrac;
            }

            if (thisExhCtrl.BalancedExhFracScheduleNum > 0) {
                thisExhCtrl.BalancedFlow = // state.dataHVACGlobal->BalancedExhMassFlow =
                    MassFlow *             // state.dataHVACGlobal->UnbalExhMassFlow *
                    ScheduleManager::GetCurrentScheduleValue(state, thisExhCtrl.BalancedExhFracScheduleNum);
                thisExhCtrl.UnbalancedFlow =  // state.dataHVACGlobal->UnbalExhMassFlow =
                    MassFlow -                // = state.dataHVACGlobal->UnbalExhMassFlow -
                    thisExhCtrl.BalancedFlow; // state.dataHVACGlobal->BalancedExhMassFlow;
            } else {
                thisExhCtrl.BalancedFlow = 0.0;
                thisExhCtrl.UnbalancedFlow = MassFlow;
            }

            thisExhInlet.MassFlowRate = MassFlow;
        }

        thisExhOutlet.MassFlowRate = thisExhInlet.MassFlowRate;

        thisExhOutlet.Temp = thisExhInlet.Temp;
        thisExhOutlet.HumRat = thisExhInlet.HumRat;
        thisExhOutlet.Enthalpy = thisExhInlet.Enthalpy;
        // Set the outlet nodes for properties that just pass through & not used
        thisExhOutlet.Quality = thisExhInlet.Quality;
        thisExhOutlet.Press = thisExhInlet.Press;

        // More node elements
        thisExhOutlet.MassFlowRateMax = thisExhInlet.MassFlowRateMax;
        thisExhOutlet.MassFlowRateMaxAvail = thisExhInlet.MassFlowRateMaxAvail;

        // Set the Node Flow Control Variables from the Fan Control Variables
        thisExhOutlet.MassFlowRateMaxAvail = thisExhInlet.MassFlowRateMaxAvail;
        thisExhOutlet.MassFlowRateMinAvail = thisExhInlet.MassFlowRateMinAvail;

        // these might also be useful to pass through
        if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
            thisExhOutlet.CO2 = thisExhInlet.CO2;
        }

        if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
            thisExhOutlet.GenContam = thisExhInlet.GenContam;
        }
    }

    void SizeExhaustSystem(EnergyPlusData &state, int const exhSysNum)
    {
        auto &thisExhSys = state.dataZoneEquip->ExhaustAirSystem(exhSysNum);

        if (!thisExhSys.SizingFlag) {
            return;
        }

        // mixer outlet sizing:
        Real64 outletFlowMaxAvail = 0.0;
        int inletNode_index = 0;
        for (size_t i = 1; i <= state.dataMixerComponent->MixerCond(thisExhSys.ZoneMixerIndex).NumInletNodes; ++i) {
            inletNode_index = state.dataMixerComponent->MixerCond(thisExhSys.ZoneMixerIndex).InletNode(i);
            outletFlowMaxAvail += state.dataLoopNodes->Node(inletNode_index).MassFlowRateMaxAvail;
        }

        // mixer outlet considered OutletMassFlowRateMaxAvail?
        int outletNode_index = state.dataMixerComponent->MixerCond(thisExhSys.ZoneMixerIndex).OutletNode;
        state.dataLoopNodes->Node(outletNode_index).MassFlowRateMaxAvail = outletFlowMaxAvail;

        // then central exhasut fan sizing here:
        if (thisExhSys.CentralFanTypeNum == DataHVACGlobals::FanType_SystemModelObject) {
            if (state.dataHVACFan->fanObjs[thisExhSys.CentralFanIndex]->designAirVolFlowRate == DataSizing::AutoSize) {
                state.dataHVACFan->fanObjs[thisExhSys.CentralFanIndex]->designAirVolFlowRate = outletFlowMaxAvail / state.dataEnvrn->StdRhoAir;
            }
            BaseSizer::reportSizerOutput(state,
                                         "FAN:SYSTEMMODEL",
                                         state.dataHVACFan->fanObjs[thisExhSys.CentralFanIndex]->name,
                                         "Design Fan Airflow [m3/s]",
                                         state.dataHVACFan->fanObjs[thisExhSys.CentralFanIndex]->designAirVolFlowRate);
        } else if (thisExhSys.CentralFanTypeNum == DataHVACGlobals::FanType_ComponentModel) {
            if (state.dataFans->Fan(thisExhSys.CentralFanIndex).MaxAirMassFlowRate == DataSizing::AutoSize) {
                state.dataFans->Fan(thisExhSys.CentralFanIndex).MaxAirMassFlowRate =
                    outletFlowMaxAvail * state.dataFans->Fan(thisExhSys.CentralFanIndex).FanSizingFactor;
            }
            BaseSizer::reportSizerOutput(state,
                                         state.dataFans->Fan(thisExhSys.CentralFanIndex).FanType,
                                         state.dataFans->Fan(thisExhSys.CentralFanIndex).FanName,
                                         "Design Fan Airflow [m3/s]",
                                         state.dataFans->Fan(thisExhSys.CentralFanIndex).MaxAirMassFlowRate);
        } else {
            //
        }

        // after evertyhing sized, set the sizing flag
        thisExhSys.SizingFlag = false;
    }

    void SizeExhaustControlFlow(EnergyPlusData &state, int const zoneExhCtrlNum, Array1D_int &NodeNums)
    {
        auto &thisExhCtrl = state.dataZoneEquip->ZoneExhaustControlSystem(zoneExhCtrlNum);

        Real64 designFlow = 0.0;

        if (thisExhCtrl.FlowControlTypeNum == 1) { // FollowSupply
            // size based on supply nodelist flow
            for (size_t i = 1; i <= NodeNums.size(); ++i) {
                designFlow += state.dataLoopNodes->Node(NodeNums(i)).MassFlowRateMax;
            }
        } else { // scheduled etc.
            // based on zone OA.
            designFlow = state.dataSize->FinalZoneSizing(thisExhCtrl.ZoneNum).MinOA;
        }

        thisExhCtrl.DesignExhaustFlowRate = designFlow;
    }

    void UpdateZoneExhaustControl(EnergyPlusData &state)
    {
        for (size_t i = 1; state.dataZoneEquip->NumZoneExhaustControls; ++i) {
            int controlledZoneNum = state.dataZoneEquip->ZoneExhaustControlSystem(i).ControlledZoneNum;
            state.dataZoneEquip->ZoneEquipConfig(controlledZoneNum).ZoneExh +=
                state.dataZoneEquip->ZoneExhaustControlSystem(i).BalancedFlow + state.dataZoneEquip->ZoneExhaustControlSystem(i).UnbalancedFlow;
            state.dataZoneEquip->ZoneEquipConfig(controlledZoneNum).ZoneExhBalanced += state.dataZoneEquip->ZoneExhaustControlSystem(i).BalancedFlow;
        }
    }

    void CheckForSupplyNode(EnergyPlusData &state, int const SupplyNodeNum, bool &NodeNotFound)
    {
        // Trying to check a node to see if it is truely a supply node
        // for a nodelist, need a call loop to check each node in the list
    }

} // namespace ExhaustAirSystemManager

} // namespace EnergyPlus
