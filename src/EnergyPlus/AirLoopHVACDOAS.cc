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
#include <string>

// EnergyPlus Headers
#include <EnergyPlus/AirLoopHVACDOAS.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/HVACDXHeatPumpSystem.hh>
#include <EnergyPlus/HVACDXSystem.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HVACVariableRefrigerantFlow.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/PhotovoltaicThermalCollectors.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/TranspiredCollector.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/WeatherManager.hh>

namespace EnergyPlus {

namespace AirLoopHVACDOAS {

    void AirLoopDOAS::SimAirLoopHVACDOAS(EnergyPlusData &state, bool const FirstHVACIteration, int &CompIndex)
    {

        // Obtains and Allocates unitary system related parameters from input file
        if (state.dataAirLoopHVACDOAS->GetInputOnceFlag) {
            // Get the AirLoopHVACDOAS input
            getAirLoopDOASInput(state);
            state.dataAirLoopHVACDOAS->GetInputOnceFlag = false;
        }

        if (CompIndex == -1) {
            CompIndex = this->m_AirLoopDOASNum;
        }

        if (this->SizingOnceFlag) {
            this->SizingAirLoopDOAS(state);
            this->SizingOnceFlag = false;
        }

        this->initAirLoopDOAS(state, FirstHVACIteration);

        if (this->SumMassFlowRate == 0.0 && !state.dataGlobal->BeginEnvrnFlag) {
            state.dataLoopNodes->Node(this->m_CompPointerAirLoopMixer->OutletNodeNum).MassFlowRate = 0.0;
            return;
        }

        this->CalcAirLoopDOAS(state, FirstHVACIteration);
    }

    AirLoopMixer *AirLoopMixer::factory(EnergyPlusData &state, int object_num, std::string const &objectName)
    {

        if (state.dataAirLoopHVACDOAS->getAirLoopMixerInputOnceFlag) {
            AirLoopMixer::getAirLoopMixer(state);
            state.dataAirLoopHVACDOAS->getAirLoopMixerInputOnceFlag = false;
        }

        int MixerNum = -1;
        for (auto &dSpec : state.dataAirLoopHVACDOAS->airloopMixer) {
            ++MixerNum;
            if (UtilityRoutines::SameString(dSpec.name, objectName) && dSpec.m_AirLoopMixer_Num == object_num) {
                return &dSpec;
            }
        }

        ShowSevereError(state, "AirLoopMixer factory: Error getting inputs for system named: " + objectName);
        return nullptr;
    }

    void AirLoopMixer::getAirLoopMixer(EnergyPlusData &state)
    {
        bool errorsFound(false);

        std::string cCurrentModuleObject = "AirLoopHVAC:Mixer";
        std::string cFieldName;

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int AirLoopMixerNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);
                ++AirLoopMixerNum;
                AirLoopMixer thisMixer;

                thisMixer.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisMixer.OutletNodeName = UtilityRoutines::MakeUPPERCase(AsString(fields.at("outlet_node_name")));
                thisMixer.m_AirLoopMixer_Num = AirLoopMixerNum - 1;
                thisMixer.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                              thisMixer.OutletNodeName,
                                                                              errorsFound,
                                                                              cCurrentModuleObject,
                                                                              thisObjectName,
                                                                              DataLoopNode::NodeFluidType::Air,
                                                                              DataLoopNode::NodeConnectionType::Outlet,
                                                                              1,
                                                                              DataLoopNode::ObjectIsParent);

                auto NodeNames = fields.find("nodes");
                if (NodeNames != fields.end()) {
                    auto NodeArray = NodeNames.value();
                    thisMixer.numOfInletNodes = NodeArray.size();
                    int num = 0;
                    for (auto NodeDOASName : NodeArray) {
                        num += 1;
                        std::string name = UtilityRoutines::MakeUPPERCase(AsString(NodeDOASName.at("inlet_node_name")));
                        int NodeNum = UtilityRoutines::FindItemInList(name, state.dataLoopNodes->NodeID);
                        if (NodeNum > 0 && num <= thisMixer.numOfInletNodes) {
                            thisMixer.InletNodeName.push_back(name);
                            thisMixer.InletNodeNum.push_back(NodeNum);
                        } else {
                            cFieldName = "Inlet Node Name";
                            ShowSevereError(state, cCurrentModuleObject + ", \"" + thisMixer.name + "\" " + name + " not found: " + cFieldName);
                            errorsFound = true;
                        }
                    }
                }

                state.dataAirLoopHVACDOAS->airloopMixer.push_back(thisMixer);
            }
            if (errorsFound) {
                ShowFatalError(state, "getAirLoopMixer: Previous errors cause termination.");
            }
        }
    } // namespace AirLoopMixer

    void AirLoopMixer::CalcAirLoopMixer(EnergyPlusData &state)
    {
        Real64 outletTemp = 0.0;
        Real64 outletHumRat = 0.0;
        Real64 massSum = 0.0;
        int InletNum;

        for (int i = 1; i <= this->numOfInletNodes; i++) {
            InletNum = this->InletNodeNum[i - 1];
            massSum += state.dataLoopNodes->Node(InletNum).MassFlowRate;
            outletTemp += state.dataLoopNodes->Node(InletNum).MassFlowRate * state.dataLoopNodes->Node(InletNum).Temp;
            outletHumRat += state.dataLoopNodes->Node(InletNum).MassFlowRate * state.dataLoopNodes->Node(InletNum).HumRat;
        }
        if (massSum > 0.0) {
            state.dataLoopNodes->Node(this->OutletNodeNum).Temp = outletTemp / massSum;
            state.dataLoopNodes->Node(this->OutletNodeNum).HumRat = outletHumRat / massSum;
            state.dataLoopNodes->Node(this->OutletNodeNum).MassFlowRate = massSum;
            state.dataLoopNodes->Node(this->OutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(outletTemp / massSum, outletHumRat / massSum);
            this->OutletTemp = state.dataLoopNodes->Node(this->OutletNodeNum).Temp;
        } else {
            state.dataLoopNodes->Node(this->OutletNodeNum).Temp = state.dataLoopNodes->Node(this->InletNodeNum[0]).Temp;
            state.dataLoopNodes->Node(this->OutletNodeNum).HumRat = state.dataLoopNodes->Node(this->InletNodeNum[0]).HumRat;
            state.dataLoopNodes->Node(this->OutletNodeNum).MassFlowRate = 0.0;
            state.dataLoopNodes->Node(this->OutletNodeNum).Enthalpy = state.dataLoopNodes->Node(this->InletNodeNum[0]).Enthalpy;
            this->OutletTemp = state.dataLoopNodes->Node(this->InletNodeNum[0]).Temp;
        }
    }

    int getAirLoopMixerIndex(EnergyPlusData &state, std::string const &objectName)
    {
        if (state.dataAirLoopHVACDOAS->getAirLoopMixerInputOnceFlag) {
            AirLoopMixer::getAirLoopMixer(state);
            state.dataAirLoopHVACDOAS->getAirLoopMixerInputOnceFlag = false;
        }

        int index = -1;
        for (std::size_t loop = 0; loop < state.dataAirLoopHVACDOAS->airloopMixer.size(); ++loop) {
            AirLoopMixer *thisAirLoopMixerObjec = &state.dataAirLoopHVACDOAS->airloopMixer[loop];
            if (UtilityRoutines::SameString(objectName, thisAirLoopMixerObjec->name)) {
                index = loop;
                return index;
            }
        }
        ShowSevereError(state, "getAirLoopMixer: did not find AirLoopHVAC:Mixer name =" + objectName + ". Check inputs");
        return index;
    }

    AirLoopSplitter *AirLoopSplitter::factory(EnergyPlusData &state, int object_num, std::string const &objectName)
    {

        if (state.dataAirLoopHVACDOAS->getAirLoopSplitterInputOnceFlag) {
            AirLoopSplitter::getAirLoopSplitter(state);
            state.dataAirLoopHVACDOAS->getAirLoopSplitterInputOnceFlag = false;
        }

        int SplitterNum = -1;
        for (auto &dSpec : state.dataAirLoopHVACDOAS->airloopSplitter) {
            SplitterNum++;
            if (UtilityRoutines::SameString(dSpec.name, objectName) && dSpec.m_AirLoopSplitter_Num == object_num) {
                return &dSpec;
            }
        }
        ShowSevereError(state, "AirLoopSplitter factory: Error getting inputs for system named: " + objectName);
        return nullptr;
    }

    void AirLoopSplitter::CalcAirLoopSplitter(EnergyPlusData &state, Real64 Temp, Real64 HumRat)
    {
        for (int i = 0; i < this->numOfOutletNodes; i++) {
            state.dataLoopNodes->Node(this->OutletNodeNum[i]).Temp = Temp;
            state.dataLoopNodes->Node(this->OutletNodeNum[i]).HumRat = HumRat;
            state.dataLoopNodes->Node(this->OutletNodeNum[i]).Enthalpy = Psychrometrics::PsyHFnTdbW(Temp, HumRat);
        }
        this->InletTemp = Temp;
    }

    int getAirLoopSplitterIndex(EnergyPlusData &state, std::string const &objectName)
    {
        if (state.dataAirLoopHVACDOAS->getAirLoopSplitterInputOnceFlag) {
            AirLoopSplitter::getAirLoopSplitter(state);
            state.dataAirLoopHVACDOAS->getAirLoopSplitterInputOnceFlag = false;
        }

        int index = -1;
        for (std::size_t loop = 0; loop < state.dataAirLoopHVACDOAS->airloopSplitter.size(); ++loop) {
            AirLoopSplitter *thisAirLoopSplitterObjec = &state.dataAirLoopHVACDOAS->airloopSplitter[loop];
            if (UtilityRoutines::SameString(objectName, thisAirLoopSplitterObjec->name)) {
                index = loop;
                return index;
            }
        }
        ShowSevereError(state, "getAirLoopSplitter: did not find AirLoopSplitter name =" + objectName + ". Check inputs");
        return index;
    }

    void AirLoopSplitter::getAirLoopSplitter(EnergyPlusData &state)
    {
        bool errorsFound(false);

        std::string cCurrentModuleObject = "AirLoopHVAC:Splitter";
        std::string cFieldName;

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int AirLoopSplitterNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);

                ++AirLoopSplitterNum;
                AirLoopSplitter thisSplitter;

                thisSplitter.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisSplitter.InletNodeName = UtilityRoutines::MakeUPPERCase(AsString(fields.at("inlet_node_name")));
                thisSplitter.m_AirLoopSplitter_Num = AirLoopSplitterNum - 1;

                auto NodeNames = fields.find("nodes");
                if (NodeNames != fields.end()) {
                    auto NodeArray = NodeNames.value();
                    thisSplitter.numOfOutletNodes = NodeArray.size();
                    int num = 0;
                    for (auto NodeDOASName : NodeArray) {
                        num += 1;
                        std::string name = UtilityRoutines::MakeUPPERCase(AsString(NodeDOASName.at("outlet_node_name")));
                        int NodeNum = UtilityRoutines::FindItemInList(name, state.dataLoopNodes->NodeID);
                        if (NodeNum > 0 && num <= thisSplitter.numOfOutletNodes) {
                            thisSplitter.OutletNodeName.push_back(name);
                            thisSplitter.OutletNodeNum.push_back(NodeNum);
                        } else {
                            cFieldName = "Outlet Node Name";
                            ShowSevereError(state, cCurrentModuleObject + ", \"" + thisSplitter.name + "\" " + cFieldName + " not found: " + name);
                            errorsFound = true;
                        }
                    }
                }

                state.dataAirLoopHVACDOAS->airloopSplitter.push_back(thisSplitter);
            }
            if (errorsFound) {
                ShowFatalError(state, "getAirLoopSplitter: Previous errors cause termination.");
            }
        }
    } // namespace AirLoopSplitter

    void AirLoopDOAS::getAirLoopDOASInput(EnergyPlusData &state)
    {

        using ScheduleManager::GetScheduleIndex;

        bool errorsFound(false);

        std::string cCurrentModuleObject = "AirLoopHVAC:DedicatedOutdoorAirSystem";
        std::string cFieldName;

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int AirLoopDOASNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);
                ++AirLoopDOASNum;
                AirLoopDOAS thisDOAS;

                thisDOAS.Name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                // get OA and avail num
                thisDOAS.OASystemName = UtilityRoutines::MakeUPPERCase(AsString(fields.at("airloophvac_outdoorairsystem_name")));
                thisDOAS.m_OASystemNum = UtilityRoutines::FindItemInList(thisDOAS.OASystemName, state.dataAirLoop->OutsideAirSys);
                if (thisDOAS.m_OASystemNum == 0) {
                    cFieldName = "AirLoopHVAC:OutdoorAirSystem Name";
                    ShowSevereError(state,
                                    cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName + " not found: " + thisDOAS.OASystemName);
                    errorsFound = true;
                }
                // Check controller type
                std::string CurrentModuleObject = "AirLoopHVAC:OutdoorAirSystem";
                for (int InListNum = 1; InListNum <= state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).NumControllers; ++InListNum) {
                    if (UtilityRoutines::SameString(state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ControllerType(InListNum),
                                                    "Controller:OutdoorAir")) {
                        ShowSevereError(state,
                                        "When " + CurrentModuleObject + " = " +
                                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ControllerName(InListNum) +
                                            " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError(state, "The Controller:OutdoorAir can not be used as a controller. Please remove it");
                        errorsFound = true;
                    }
                }

                // get inlet and outlet node number from equipment list
                CurrentModuleObject = "AirLoopHVAC:OutdoorAirSystem:EquipmentList";
                for (int CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).NumComponents; ++CompNum) {
                    std::string CompType = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentType(CompNum);
                    std::string CompName = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum);
                    bool InletNodeErrFlag = false;
                    bool OutletNodeErrFlag = false;

                    auto const SELECT_CASE_var(
                        UtilityRoutines::MakeUPPERCase(state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentType(CompNum)));

                    if (SELECT_CASE_var == "OUTDOORAIR:MIXER") {
                        ShowSevereError(state,
                                        "When " + CurrentModuleObject + " = " +
                                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum) +
                                            " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError(state, " the OUTDOORAIR:MIXER can not be used as a component. Please remove it");
                        errorsFound = true;

                    } else if (SELECT_CASE_var == "FAN:CONSTANTVOLUME") {
                        ShowSevereError(state,
                                        "When " + CurrentModuleObject + " = " +
                                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum) +
                                            " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError(state,
                                          " the FAN:CONSTANTVOLUME can not be used as a component. The alllowed fan types are FAN:SYSTEMMODEL and "
                                          "FAN:COMPONENTMODEL. Please change it");
                        errorsFound = true;
                    } else if (SELECT_CASE_var == "FAN:VARIABLEVOLUME") {
                        ShowSevereError(state,
                                        "When " + CurrentModuleObject + " = " +
                                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum) +
                                            " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError(state,
                                          " the FAN:VARIABLEVOLUME can not be used as a component. The alllowed fan types are FAN:SYSTEMMODEL and "
                                          "FAN:COMPONENTMODEL. Please change it");
                        errorsFound = true;
                    } else if (SELECT_CASE_var == "FAN:SYSTEMMODEL") {
                        thisDOAS.FanName = CompName;
                        thisDOAS.m_FanTypeNum = SimAirServingZones::Fan_System_Object;
                        thisDOAS.m_FanIndex = HVACFan::getFanObjectVectorIndex(state, CompName);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            state.dataHVACFan->fanObjs[thisDOAS.m_FanIndex]->inletNodeNum;
                        if (state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) == 0) {
                            InletNodeErrFlag = true;
                        }
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            state.dataHVACFan->fanObjs[thisDOAS.m_FanIndex]->outletNodeNum;
                        if (state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) == 0) {
                            OutletNodeErrFlag = true;
                        }
                        thisDOAS.m_FanInletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum);
                        thisDOAS.m_FanOutletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum);
                        if (CompNum == 1) {
                            thisDOAS.FanBlowTroughFlag = true;
                        }
                        if (!(CompNum == 1 || CompNum == state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).NumComponents)) {
                            ShowSevereError(state,
                                            "The fan placement is either first as blow through or last as draw through in" + CurrentModuleObject +
                                                " = " + state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                            ShowContinueError(state, format("The current position is number {}", CompNum));
                            errorsFound = true;
                        }
                    } else if (SELECT_CASE_var == "FAN:COMPONENTMODEL") {
                        thisDOAS.m_FanTypeNum = SimAirServingZones::Fan_ComponentModel;
                        Fans::GetFanIndex(state, CompName, thisDOAS.m_FanIndex, errorsFound, ObjexxFCL::Optional_string_const());
                        thisDOAS.FanName = CompName;
                        if (CompNum == 1) {
                            thisDOAS.FanBlowTroughFlag = true;
                        }
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            Fans::GetFanInletNode(state,
                                                  SELECT_CASE_var,
                                                  state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                  InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            Fans::GetFanOutletNode(state,
                                                   SELECT_CASE_var,
                                                   state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                   OutletNodeErrFlag);
                        thisDOAS.m_FanInletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum);
                        thisDOAS.m_FanOutletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum);
                        if (!(CompNum == 1 || CompNum == state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).NumComponents)) {
                            ShowSevereError(state,
                                            "The fan placement is either first as blow through or last as draw through in" + CurrentModuleObject +
                                                " = " + state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                            ShowContinueError(state, format("The current position is number {}", CompNum));
                            errorsFound = true;
                        }
                    } else if (SELECT_CASE_var == "COIL:COOLING:WATER") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            WaterCoils::GetCoilInletNode(state,
                                                         SELECT_CASE_var,
                                                         state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                         InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            WaterCoils::GetCoilOutletNode(state,
                                                          SELECT_CASE_var,
                                                          state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                          OutletNodeErrFlag);
                        thisDOAS.CWCtrlNodeNum = WaterCoils::GetCoilWaterInletNode(state, "COIL:COOLING:WATER", CompName, errorsFound);
                        if (errorsFound) {
                            ShowContinueError(state,
                                              "The control node number is not found in " + CurrentModuleObject + " = " +
                                                  state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        }
                        PlantUtilities::ScanPlantLoopsForObject(state,
                                                                CompName,
                                                                DataPlant::TypeOf_CoilWaterCooling,
                                                                thisDOAS.CWLoopNum,
                                                                thisDOAS.CWLoopSide,
                                                                thisDOAS.CWBranchNum,
                                                                thisDOAS.CWCompNum,
                                                                errorsFound,
                                                                _,
                                                                _,
                                                                _,
                                                                _,
                                                                _);
                        if (errorsFound) { // is this really needed here, program fatals out later on when errorsFound = true
                            ShowFatalError(state, "GetAirLoopDOASInput: Program terminated for previous conditions.");
                        }
                    } else if (SELECT_CASE_var == "COIL:HEATING:WATER") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            WaterCoils::GetCoilInletNode(state,
                                                         SELECT_CASE_var,
                                                         state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                         InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            WaterCoils::GetCoilOutletNode(state,
                                                          SELECT_CASE_var,
                                                          state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                          OutletNodeErrFlag);
                        thisDOAS.HWCtrlNodeNum = WaterCoils::GetCoilWaterInletNode(state, "Coil:Heating:Water", CompName, errorsFound);
                        if (errorsFound) {
                            ShowContinueError(state,
                                              "The control node number is not found in " + CurrentModuleObject + " = " +
                                                  state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        }
                        PlantUtilities::ScanPlantLoopsForObject(state,
                                                                CompName,
                                                                DataPlant::TypeOf_CoilWaterSimpleHeating,
                                                                thisDOAS.HWLoopNum,
                                                                thisDOAS.HWLoopSide,
                                                                thisDOAS.HWBranchNum,
                                                                thisDOAS.HWCompNum,
                                                                errorsFound,
                                                                _,
                                                                _,
                                                                _,
                                                                _,
                                                                _);
                        if (errorsFound) { // is this really needed here, program fatals out later on when errorsFound = true
                            ShowFatalError(state, "GetAirLoopDOASInput: Program terminated for previous conditions.");
                        }

                    } else if (SELECT_CASE_var == "COIL:HEATING:STEAM") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            SteamCoils::GetCoilSteamInletNode(state, CompType, CompName, InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            SteamCoils::GetCoilSteamOutletNode(state, CompType, CompName, OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            WaterCoils::GetCoilInletNode(state,
                                                         SELECT_CASE_var,
                                                         state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                         InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            WaterCoils::GetCoilOutletNode(state,
                                                          SELECT_CASE_var,
                                                          state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                          OutletNodeErrFlag);
                        thisDOAS.CWCtrlNodeNum =
                            WaterCoils::GetCoilWaterInletNode(state, "Coil:Cooling:Water:DetailedGeometry", CompName, errorsFound);
                        if (errorsFound) {
                            ShowContinueError(state,
                                              "The control node number is not found in " + CurrentModuleObject + " = " +
                                                  state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        }
                        PlantUtilities::ScanPlantLoopsForObject(state,
                                                                CompName,
                                                                DataPlant::TypeOf_CoilWaterDetailedFlatCooling,
                                                                thisDOAS.CWLoopNum,
                                                                thisDOAS.CWLoopSide,
                                                                thisDOAS.CWBranchNum,
                                                                thisDOAS.CWCompNum,
                                                                errorsFound,
                                                                _,
                                                                _,
                                                                _,
                                                                _,
                                                                _);
                        if (errorsFound) { // is this really needed here, program fatals out later on when errorsFound = true
                            ShowFatalError(state, "GetAirLoopDOASInput: Program terminated for previous conditions.");
                        }
                    } else if (SELECT_CASE_var == "COIL:HEATING:ELECTRIC") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HeatingCoils::GetCoilInletNode(state,
                                                           SELECT_CASE_var,
                                                           state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                           InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HeatingCoils::GetCoilOutletNode(state,
                                                            SELECT_CASE_var,
                                                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                            OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "COIL:HEATING:FUEL") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HeatingCoils::GetCoilInletNode(state,
                                                           SELECT_CASE_var,
                                                           state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                           InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HeatingCoils::GetCoilOutletNode(state,
                                                            SELECT_CASE_var,
                                                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum),
                                                            OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HVACHXAssistedCoolingCoil::GetCoilInletNode(state, CompType, CompName, InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HVACHXAssistedCoolingCoil::GetCoilOutletNode(state, CompType, CompName, OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "COILSYSTEM:COOLING:DX") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = HVACDXSystem::GetCoolingCoilInletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = HVACDXSystem::GetCoolingCoilOutletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).DXCoolingCoilFlag = true;
                    } else if (SELECT_CASE_var == "COILSYSTEM:HEATING:DX") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HVACDXHeatPumpSystem::GetHeatingCoilInletNodeNum(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HVACDXHeatPumpSystem::GetHeatingCoilOutletNodeNum(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "AIRLOOPHVAC:UNITARYSYSTEM") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum)
                                .compPointer[CompNum]
                                ->getAirInNode(
                                    state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), 0, InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum)
                                .compPointer[CompNum]
                                ->getAirOutNode(
                                    state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), 0, OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "COIL:USERDEFINED") {
                        ShowSevereError(state,
                                        "When " + CurrentModuleObject + " = " +
                                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum) +
                                            " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError(state, " the COIL:USERDEFINED can not be used as a component.");
                        errorsFound = true;
                        // Heat recovery
                    } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).HeatExchangerFlag = true;
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = HeatRecovery::GetSupplyInletNode(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = HeatRecovery::GetSupplyOutletNode(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).HeatExchangerFlag = true;
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = HeatRecovery::GetSupplyInletNode(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = HeatRecovery::GetSupplyOutletNode(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "HEATEXCHANGER:DESICCANT:BALANCEDFLOW") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).HeatExchangerFlag = true;
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = HeatRecovery::GetSupplyInletNode(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = HeatRecovery::GetSupplyOutletNode(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                        // Desiccant Dehumidifier
                    } else if (SELECT_CASE_var == "DEHUMIDIFIER:DESICCANT:NOFANS") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            DesiccantDehumidifiers::GetProcAirInletNodeNum(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            DesiccantDehumidifiers::GetProcAirOutletNodeNum(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "DEHUMIDIFIER:DESICCANT:SYSTEM") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            DesiccantDehumidifiers::GetProcAirInletNodeNum(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            DesiccantDehumidifiers::GetProcAirOutletNodeNum(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                        // Humidifiers: Humidifier:Steam:Electric and Humidifier:Steam:Gas
                    } else if (SELECT_CASE_var == "HUMIDIFIER:STEAM:ELECTRIC") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = Humidifiers::GetAirInletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = Humidifiers::GetAirOutletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "HUMIDIFIER:STEAM:GAS") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = Humidifiers::GetAirInletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = Humidifiers::GetAirOutletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                        // Unglazed Transpired Solar Collector
                    } else if (SELECT_CASE_var == "SOLARCOLLECTOR:UNGLAZEDTRANSPIRED") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = TranspiredCollector::GetAirInletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = TranspiredCollector::GetAirOutletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                        // PVT air heater
                    } else if (SELECT_CASE_var == "SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            PhotovoltaicThermalCollectors::GetAirInletNodeNum(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            PhotovoltaicThermalCollectors::GetAirOutletNodeNum(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                        // Evaporative Cooler Types
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:CELDEKPAD") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = EvaporativeCoolers::GetInletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = EvaporativeCoolers::GetOutletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = EvaporativeCoolers::GetInletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = EvaporativeCoolers::GetOutletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:WETCOIL") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = EvaporativeCoolers::GetInletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = EvaporativeCoolers::GetOutletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = EvaporativeCoolers::GetInletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = EvaporativeCoolers::GetOutletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = EvaporativeCoolers::GetInletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = EvaporativeCoolers::GetOutletNodeNum(
                            state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else if (SELECT_CASE_var == "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW") {
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HVACVariableRefrigerantFlow::GetVRFTUInAirNodeFromName(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), InletNodeErrFlag);
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HVACVariableRefrigerantFlow::GetVRFTUOutAirNodeFromName(
                                state, state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), OutletNodeErrFlag);
                    } else {
                        ShowSevereError(state,
                                        CurrentModuleObject + " = \"" + std::string{CompName} + "\" invalid Outside Air Component=\"" +
                                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentType(CompNum) + "\".");
                        errorsFound = true;
                    }
                    if (InletNodeErrFlag) {
                        ShowSevereError(state, "Inlet node number is not found in " + CurrentModuleObject + " = " + std::string{CompName});
                        errorsFound = true;
                    }
                    if (OutletNodeErrFlag) {
                        ShowSevereError(state, "Outlet node number is not found in " + CurrentModuleObject + " = " + std::string{CompName});
                        errorsFound = true;
                    }
                }

                thisDOAS.m_InletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(1);
                thisDOAS.m_OutletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum)
                                               .OutletNodeNum(state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).NumComponents);
                state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).AirLoopDOASNum = AirLoopDOASNum - 1;
                // Set up parent-child connection
                BranchNodeConnections::SetUpCompSets(state,
                                                     cCurrentModuleObject,
                                                     thisDOAS.Name,
                                                     "AIRLOOPHVAC:OUTDOORAIRSYSTEM",
                                                     thisDOAS.OASystemName,
                                                     state.dataLoopNodes->NodeID(thisDOAS.m_InletNodeNum),
                                                     state.dataLoopNodes->NodeID(thisDOAS.m_OutletNodeNum));

                if (state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).HeatExchangerFlag) {
                    thisDOAS.m_HeatExchangerFlag = true;
                }

                thisDOAS.AvailManagerSchedName = UtilityRoutines::MakeUPPERCase(AsString(fields.at("availability_schedule_name")));
                thisDOAS.m_AvailManagerSchedPtr = GetScheduleIndex(state, thisDOAS.AvailManagerSchedName);
                if (thisDOAS.m_AvailManagerSchedPtr == 0) {
                    cFieldName = "Availability Schedule Name";
                    ShowSevereError(
                        state, cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName + " not found: " + thisDOAS.AvailManagerSchedName);
                    errorsFound = true;
                }

                thisDOAS.AirLoopMixerName = UtilityRoutines::MakeUPPERCase(AsString(fields.at("airloophvac_mixer_name"))); //
                thisDOAS.m_AirLoopMixerIndex = getAirLoopMixerIndex(state, thisDOAS.AirLoopMixerName);
                if (thisDOAS.m_AirLoopMixerIndex < 0) {
                    cFieldName = "AirLoopHVAC:Mixer Name";
                    ShowSevereError(state,
                                    cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName + " not found: " + thisDOAS.AirLoopMixerName);
                    errorsFound = true;
                }
                AirLoopMixer thisAirLoopMixer;
                thisDOAS.m_CompPointerAirLoopMixer = thisAirLoopMixer.factory(state, thisDOAS.m_AirLoopMixerIndex, thisDOAS.AirLoopMixerName);
                thisDOAS.AirLoopSplitterName = UtilityRoutines::MakeUPPERCase(AsString(fields.at("airloophvac_splitter_name"))); //
                thisDOAS.m_AirLoopSplitterIndex = getAirLoopSplitterIndex(state, thisDOAS.AirLoopSplitterName);
                if (thisDOAS.m_AirLoopSplitterIndex < 0) {
                    cFieldName = "AirLoopHVAC:Splitter Name";
                    ShowSevereError(
                        state, cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName + " not found: " + thisDOAS.AirLoopSplitterName);
                    errorsFound = true;
                }
                AirLoopSplitter thisAirLoopSplitter;
                thisDOAS.m_CompPointerAirLoopSplitter =
                    thisAirLoopSplitter.factory(state, thisDOAS.m_AirLoopSplitterIndex, thisDOAS.AirLoopSplitterName);

                // get pretreated desing conditions
                thisDOAS.PreheatTemp = fields.at("preheat_design_temperature");
                thisDOAS.PreheatHumRat = fields.at("preheat_design_humidity_ratio");
                thisDOAS.PrecoolTemp = fields.at("precool_design_temperature");
                thisDOAS.PrecoolHumRat = fields.at("precool_design_humidity_ratio");

                // get info on AirLoops
                thisDOAS.NumOfAirLoops = fields.at("number_of_airloophvac"); //
                if (thisDOAS.NumOfAirLoops < 1) {
                    cFieldName = "Number of AirLoopHVAC";
                    ShowSevereError(state,
                                    fmt::format("{}, \"{}\" {} = {}", cCurrentModuleObject, thisDOAS.Name, cFieldName, thisDOAS.NumOfAirLoops));
                    ShowContinueError(state, " The minimum value should be 1.");
                    errorsFound = true;
                }

                auto AirLoopNames = fields.find("airloophvacs");
                if (AirLoopNames != fields.end()) {
                    auto AirLoopArray = AirLoopNames.value();
                    int num = 0;
                    for (auto AirLoopHVACName : AirLoopArray) {
                        std::string name = UtilityRoutines::MakeUPPERCase(AsString(AirLoopHVACName.at("airloophvac_name")));
                        int LoopNum = UtilityRoutines::FindItemInList(name, state.dataAirSystemsData->PrimaryAirSystems);
                        num += 1;
                        if (LoopNum > 0 && num <= thisDOAS.NumOfAirLoops) {
                            thisDOAS.AirLoopName.push_back(name);
                            thisDOAS.m_AirLoopNum.push_back(LoopNum);
                        } else {
                            cFieldName = "AirLoopHVAC Name";
                            ShowSevereError(state, cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName + " not found: " + name);
                            errorsFound = true;
                        }
                    }
                }

                thisDOAS.m_AirLoopDOASNum = AirLoopDOASNum - 1;
                state.dataAirLoopHVACDOAS->airloopDOAS.push_back(thisDOAS);
            }

            // Check valid OA controller
            for (int OASysNum = 1; OASysNum <= state.dataAirLoop->NumOASystems; OASysNum++) {
                if (UtilityRoutines::SameString(state.dataAirLoop->OutsideAirSys(OASysNum).ControllerListName, "")) {
                    if (state.dataAirLoop->OutsideAirSys(OASysNum).AirLoopDOASNum == -1) {
                        ShowSevereError(state,
                                        "AirLoopHVAC:OutdoorAirSystem = \"" + state.dataAirLoop->OutsideAirSys(OASysNum).Name +
                                            "\" invalid Controller List Name = \" not found.");
                        errorsFound = true;
                    }
                }
            }
            if (errorsFound) {
                ShowFatalError(state, "getAirLoopHVACDOAS: Previous errors cause termination.");
            }
        }
    }

    void AirLoopDOAS::initAirLoopDOAS(EnergyPlusData &state, bool const FirstHVACIteration)
    {
        int LoopOA;
        int NodeNum;
        Real64 SchAvailValue;
        static constexpr std::string_view RoutineName = "AirLoopDOAS::initAirLoopDOAS";
        bool ErrorsFound = false;

        if (state.dataGlobal->BeginEnvrnFlag && this->MyEnvrnFlag) {
            Real64 rho;
            state.dataSize->CurSysNum = this->m_OASystemNum;
            for (int CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).NumComponents; ++CompNum) {
                std::string CompType = state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).ComponentType(CompNum);
                std::string CompName = state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).ComponentName(CompNum);
                if (UtilityRoutines::SameString(CompType, "FAN:SYSTEMMODEL")) {
                    state.dataHVACFan->fanObjs[this->m_FanIndex]->simulate(state);
                }
                if (UtilityRoutines::SameString(CompType, "FAN:COMPONENTMODEL")) {
                    Fans::SimulateFanComponents(state, CompName, FirstHVACIteration, this->m_FanIndex);
                }

                if (UtilityRoutines::SameString(CompType, "COIL:HEATING:WATER")) {
                    WaterCoils::SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, this->m_HeatCoilNum);
                    Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate(state, "Coil:Heating:Water", CompName, ErrorsFound);
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->HWLoopNum).FluidName,
                                                            DataGlobalConstants::HWInitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->HWLoopNum).FluidIndex,
                                                            RoutineName);
                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       CoilMaxVolFlowRate * rho,
                                                       this->HWCtrlNodeNum,
                                                       state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum),
                                                       this->HWLoopNum,
                                                       this->HWLoopSide,
                                                       this->HWBranchNum,
                                                       this->HWCompNum);
                }
                if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER")) {
                    WaterCoils::SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, this->m_CoolCoilNum);
                    Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate(state, "Coil:Cooling:Water", CompName, ErrorsFound);
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                            DataGlobalConstants::CWInitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);
                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       CoilMaxVolFlowRate * rho,
                                                       this->CWCtrlNodeNum,
                                                       state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum),
                                                       this->CWLoopNum,
                                                       this->CWLoopSide,
                                                       this->CWBranchNum,
                                                       this->CWCompNum);
                }
                if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY")) {
                    WaterCoils::SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, this->m_CoolCoilNum);
                    Real64 CoilMaxVolFlowRate =
                        WaterCoils::GetCoilMaxWaterFlowRate(state, "Coil:Cooling:Water:DetailedGeometry", CompName, ErrorsFound);
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->CWLoopNum).FluidName,
                                                            DataGlobalConstants::CWInitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);
                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       CoilMaxVolFlowRate * rho,
                                                       this->CWCtrlNodeNum,
                                                       state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum),
                                                       this->CWLoopNum,
                                                       this->CWLoopSide,
                                                       this->CWBranchNum,
                                                       this->CWCompNum);
                }
            }

            this->MyEnvrnFlag = false;
            if (ErrorsFound) {
                ShowFatalError(state, "initAirLoopDOAS: Previous errors cause termination.");
            }
        }

        if (!state.dataGlobal->BeginEnvrnFlag) {
            this->MyEnvrnFlag = true;
        }

        this->SumMassFlowRate = 0.0;

        for (LoopOA = 0; LoopOA < this->m_CompPointerAirLoopSplitter->numOfOutletNodes; LoopOA++) {
            NodeNum = this->m_CompPointerAirLoopSplitter->OutletNodeNum[LoopOA];
            this->SumMassFlowRate += state.dataLoopNodes->Node(NodeNum).MassFlowRate;
        }

        SchAvailValue = ScheduleManager::GetCurrentScheduleValue(state, this->m_AvailManagerSchedPtr);
        if (SchAvailValue < 1.0) {
            this->SumMassFlowRate = 0.0;
        }
        state.dataLoopNodes->Node(this->m_InletNodeNum).MassFlowRate = this->SumMassFlowRate;

        if (this->SumMassFlowRate == 0.0) {
            for (int CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).NumComponents; CompNum++) {
                state.dataLoopNodes->Node(state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum)) =
                    state.dataLoopNodes->Node(this->m_InletNodeNum);
            }
        }
    }

    void AirLoopDOAS::CalcAirLoopDOAS(EnergyPlusData &state, bool const FirstHVACIteration)
    {
        using MixedAir::ManageOutsideAirSystem;

        this->m_CompPointerAirLoopMixer->CalcAirLoopMixer(state);
        if (this->m_FanIndex > -1) {
            state.dataLoopNodes->Node(this->m_FanInletNodeNum).MassFlowRateMaxAvail = this->SumMassFlowRate;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMaxAvail = this->SumMassFlowRate;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMax = this->SumMassFlowRate;
            if (!this->FanBlowTroughFlag) {
                state.dataLoopNodes->Node(state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).InletNodeNum(1)).MassFlowRateMaxAvail =
                    this->SumMassFlowRate;
            }
        }
        ManageOutsideAirSystem(state, this->OASystemName, FirstHVACIteration, 0, this->m_OASystemNum);
        Real64 Temp = state.dataLoopNodes->Node(this->m_OutletNodeNum).Temp;
        Real64 HumRat = state.dataLoopNodes->Node(this->m_OutletNodeNum).HumRat;
        state.dataLoopNodes->Node(this->m_OutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(Temp, HumRat);

        this->m_CompPointerAirLoopSplitter->CalcAirLoopSplitter(state, Temp, HumRat);
    }

    void AirLoopDOAS::SizingAirLoopDOAS(EnergyPlusData &state)
    {
        Real64 sizingMassFlow = 0;
        int AirLoopNum;

        for (int AirLoop = 1; AirLoop <= this->NumOfAirLoops; AirLoop++) {
            AirLoopNum = this->m_AirLoopNum[AirLoop - 1];
            this->m_OACtrlNum.push_back(state.dataAirLoop->AirLoopControlInfo(AirLoopNum).OACtrlNum);

            if (this->m_OACtrlNum[AirLoop - 1] > 0) {
                sizingMassFlow += state.dataMixedAir->OAController(this->m_OACtrlNum[AirLoop - 1]).MaxOA;
            }
        }
        this->SizingMassFlow = sizingMassFlow;
        this->GetDesignDayConditions(state);

        if (this->m_FanIndex > -1 && this->m_FanTypeNum == SimAirServingZones::Fan_System_Object) {
            state.dataHVACFan->fanObjs[this->m_FanIndex]->designAirVolFlowRate = sizingMassFlow / state.dataEnvrn->StdRhoAir;
            state.dataLoopNodes->Node(this->m_FanInletNodeNum).MassFlowRateMaxAvail = sizingMassFlow;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMaxAvail = sizingMassFlow;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMax = sizingMassFlow;
        }
        bool errorsFound = false;
        if (this->m_FanIndex > 0 && this->m_FanTypeNum == SimAirServingZones::Fan_ComponentModel) {
            Fans::SetFanData(state, this->m_FanIndex, errorsFound, Name, sizingMassFlow / state.dataEnvrn->StdRhoAir, 0);
            state.dataFans->Fan(this->m_FanIndex).MaxAirMassFlowRate = sizingMassFlow;
            state.dataLoopNodes->Node(this->m_FanInletNodeNum).MassFlowRateMaxAvail = sizingMassFlow;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMaxAvail = sizingMassFlow;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMax = sizingMassFlow;
        }
        if (errorsFound) {
            ShowFatalError(state, "Preceding sizing errors cause program termination");
        }
        state.dataSize->CurSysNum = state.dataHVACGlobal->NumPrimaryAirSys + this->m_AirLoopDOASNum + 1;
        state.dataSize->CurOASysNum = this->m_OASystemNum;
    }

    void getAirLoopHVACDOASInput(EnergyPlusData &state)
    {
        if (state.dataAirLoopHVACDOAS->GetInputOnceFlag) {
            AirLoopDOAS::getAirLoopDOASInput(state);
            state.dataAirLoopHVACDOAS->GetInputOnceFlag = false;
        }
    }

    void AirLoopDOAS::GetDesignDayConditions(EnergyPlusData &state)
    {

        int const summerDesignDayTypeIndex(9);
        int const winterDesignDayTypeIndex(10);

        for (size_t i = 1; i <= state.dataWeatherManager->DesDayInput.size(); i++) {
            // Summer design day
            if (state.dataWeatherManager->DesDayInput(i).DayType == summerDesignDayTypeIndex && state.dataAirLoopHVACDOAS->SummerDesignDayFlag) {
                this->SizingCoolOATemp = state.dataWeatherManager->DesDayInput(i).MaxDryBulb;
                if (state.dataWeatherManager->DesDayInput(i).HumIndType == WeatherManager::DDHumIndType::WetBulb) { // wet bulb temperature
                    this->SizingCoolOAHumRat = Psychrometrics::PsyWFnTdbTwbPb(
                        state, this->SizingCoolOATemp, state.dataWeatherManager->DesDayInput(i).HumIndValue, DataEnvironment::StdPressureSeaLevel);
                } else if (state.dataWeatherManager->DesDayInput(i).HumIndType == WeatherManager::DDHumIndType::DewPoint) { // dewpoint
                    this->SizingCoolOAHumRat = Psychrometrics::PsyWFnTdpPb(
                        state, state.dataWeatherManager->DesDayInput(i).HumIndValue, DataEnvironment::StdPressureSeaLevel);
                } else if (state.dataWeatherManager->DesDayInput(i).HumIndType == WeatherManager::DDHumIndType::HumRatio) {
                    this->SizingCoolOAHumRat = state.dataWeatherManager->DesDayInput(i).HumIndValue;
                } // else { // What about other cases?
                state.dataAirLoopHVACDOAS->SummerDesignDayFlag = false;
            }
            // Winter design day
            if (state.dataWeatherManager->DesDayInput(i).DayType == winterDesignDayTypeIndex && state.dataAirLoopHVACDOAS->WinterDesignDayFlag) {
                this->HeatOutTemp = state.dataWeatherManager->DesDayInput(i).MaxDryBulb;
                if (state.dataWeatherManager->DesDayInput(i).HumIndType == WeatherManager::DDHumIndType::WetBulb) { // wet bulb temperature
                    this->HeatOutHumRat = Psychrometrics::PsyWFnTdbTwbPb(
                        state, this->HeatOutTemp, state.dataWeatherManager->DesDayInput(i).HumIndValue, DataEnvironment::StdPressureSeaLevel);
                } else if (state.dataWeatherManager->DesDayInput(i).HumIndType == WeatherManager::DDHumIndType::DewPoint) { // dewpoint
                    this->HeatOutHumRat = Psychrometrics::PsyWFnTdpPb(
                        state, state.dataWeatherManager->DesDayInput(i).HumIndValue, DataEnvironment::StdPressureSeaLevel);
                } else if (state.dataWeatherManager->DesDayInput(i).HumIndType == WeatherManager::DDHumIndType::HumRatio) {
                    this->HeatOutHumRat = state.dataWeatherManager->DesDayInput(i).HumIndValue;
                } // else { // What about other cases?
                state.dataAirLoopHVACDOAS->WinterDesignDayFlag = false;
            }
        }
    }

    void CheckConvergence(EnergyPlusData &state)
    {

        Real64 maxDiff;
        Real64 Diff;
        Real64 OldTemp;
        for (auto &loop : state.dataAirLoopHVACDOAS->airloopDOAS) {
            maxDiff = 0.0;
            Diff = std::abs(loop.m_CompPointerAirLoopSplitter->InletTemp -
                            state.dataLoopNodes->Node(loop.m_CompPointerAirLoopSplitter->OutletNodeNum[0]).Temp);
            if (Diff > maxDiff) {
                maxDiff = Diff;
            }
            if (loop.m_HeatExchangerFlag) {
                OldTemp = loop.m_CompPointerAirLoopMixer->OutletTemp;
                loop.m_CompPointerAirLoopMixer->CalcAirLoopMixer(state);
                Diff = std::abs(OldTemp - loop.m_CompPointerAirLoopMixer->OutletTemp);
                if (Diff > maxDiff) {
                    maxDiff = Diff;
                }
            }
            if (maxDiff > 1.0e-6) {
                if (loop.ConveCount == 0) {
                    ++loop.ConveCount;
                    ShowWarningError(state, "Convergence limit is above 1.0e-6 for unit=" + loop.Name);
                    ShowContinueErrorTimeStamp(
                        state, format("The max difference of node temperatures between AirLoopDOAS outlet and OA mixer inlet ={:.6R}", maxDiff));
                } else {
                    ++loop.ConveCount;
                    ShowRecurringWarningErrorAtEnd(state,
                                                   loop.Name + "\": The max difference of node temperatures exceeding 1.0e-6  continues...",
                                                   loop.ConveIndex,
                                                   maxDiff,
                                                   maxDiff);
                }
            }
        }
    }

} // namespace AirLoopHVACDOAS
} // namespace EnergyPlus
