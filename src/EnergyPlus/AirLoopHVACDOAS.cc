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
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/WeatherManager.hh>

namespace EnergyPlus {

namespace AirLoopHVACDOAS {

    // the equipment list object has its own subset of E+ components that are valid, this covers that list
    enum class ValidEquipListType
    {
        Invalid = -1,
        OutdoorAirMixer,
        FanConstantVolume,
        FanVariableVolume,
        FanSystemModel,
        FanComponentModel,
        CoilCoolingWater,
        CoilHeatingWater,
        CoilHeatingSteam,
        CoilCoolingWaterDetailedGeometry,
        CoilHeatingElectric,
        CoilHeatingFuel,
        CoilSystemCoolingWaterHeatExchangerAssisted,
        CoilSystemCoolingDX,
        CoilSystemHeatingDX,
        AirLoopHVACUnitarySystem,
        CoilUserDefined,
        HeatExchangerAirToAirFlatPlate,
        HeatExchangerAirToAirSensibleAndLatent,
        HeatExchangerDesiccantBalancedFlow,
        DehumidifierDesiccantNoFans,
        DehumidifierDesiccantSystem,
        HumidifierSteamElectric,
        HumidifierSteamGas,
        SolarCollectorUnglazedTranspired,
        SolarCollectorFlatPlatePhotovoltaicThermal,
        EvaporativeCoolerDirectCeldekPad,
        EvaporativeCoolerIndirectCeldekPad,
        EvaporativeCoolerIndirectWetCoil,
        EvaporativeCoolerIndirectResearchSpecial,
        EvaporativeCoolerDirectResearchSpecial,
        ZoneHVACTerminalUnitVariableRefrigerantFlow,
        Num
    };
    constexpr std::array<std::string_view, static_cast<int>(ValidEquipListType::Num)> validEquipNamesUC = {
        "OUTDOORAIR:MIXER",
        "FAN:CONSTANTVOLUME",
        "FAN:VARIABLEVOLUME",
        "FAN:SYSTEMMODEL",
        "FAN:COMPONENTMODEL",
        "COIL:COOLING:WATER",
        "COIL:HEATING:WATER",
        "COIL:HEATING:STEAM",
        "COIL:COOLING:WATER:DETAILEDGEOMETRY",
        "COIL:HEATING:ELECTRIC",
        "COIL:HEATING:FUEL",
        "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED",
        "COILSYSTEM:COOLING:DX",
        "COILSYSTEM:HEATING:DX",
        "AIRLOOPHVAC:UNITARYSYSTEM",
        "COIL:USERDEFINED",
        "HEATEXCHANGER:AIRTOAIR:FLATPLATE",
        "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT",
        "HEATEXCHANGER:DESICCANT:BALANCEDFLOW",
        "DEHUMIDIFIER:DESICCANT:NOFANS",
        "DEHUMIDIFIER:DESICCANT:SYSTEM",
        "HUMIDIFIER:STEAM:ELECTRIC",
        "HUMIDIFIER:STEAM:GAS",
        "SOLARCOLLECTOR:UNGLAZEDTRANSPIRED",
        "SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL",
        "EVAPORATIVECOOLER:DIRECT:CELDEKPAD",
        "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD",
        "EVAPORATIVECOOLER:INDIRECT:WETCOIL",
        "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL",
        "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL",
        "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW",
    };

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

        ShowSevereError(state, format("AirLoopMixer factory: Error getting inputs for system named: {}", objectName));
        return nullptr;
    }

    void AirLoopMixer::getAirLoopMixer(EnergyPlusData &state)
    {
        bool errorsFound(false);

        std::string const &cCurrentModuleObject = "AirLoopHVAC:Mixer";
        std::string cFieldName;

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int AirLoopMixerNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &fields = instance.value();
                std::string const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);
                ++AirLoopMixerNum;
                AirLoopMixer thisMixer;

                thisMixer.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisMixer.OutletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("outlet_node_name").get<std::string>());
                thisMixer.m_AirLoopMixer_Num = AirLoopMixerNum - 1;
                thisMixer.OutletNodeNum = NodeInputManager::GetOnlySingleNode(state,
                                                                              thisMixer.OutletNodeName,
                                                                              errorsFound,
                                                                              DataLoopNode::ConnectionObjectType::AirLoopHVACMixer,
                                                                              thisObjectName,
                                                                              DataLoopNode::NodeFluidType::Air,
                                                                              DataLoopNode::ConnectionType::Outlet,
                                                                              NodeInputManager::CompFluidStream::Primary,
                                                                              DataLoopNode::ObjectIsParent);

                auto NodeNames = fields.find("nodes");
                if (NodeNames != fields.end()) {
                    auto NodeArray = NodeNames.value();
                    thisMixer.numOfInletNodes = NodeArray.size();
                    int num = 0;
                    for (nlohmann::json &NodeDOASName : NodeArray) {
                        num += 1;
                        std::string name = UtilityRoutines::MakeUPPERCase(NodeDOASName.at("inlet_node_name").get<std::string>());
                        int NodeNum = UtilityRoutines::FindItemInList(name, state.dataLoopNodes->NodeID);
                        if (NodeNum > 0 && num <= thisMixer.numOfInletNodes) {
                            thisMixer.InletNodeName.push_back(name);
                            thisMixer.InletNodeNum.push_back(NodeNum);
                        } else {
                            cFieldName = "Inlet Node Name";
                            ShowSevereError(state, format("{}, \"{}\" {} not found: {}", cCurrentModuleObject, thisMixer.name, name, cFieldName));
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
        ShowSevereError(state, format("getAirLoopMixer: did not find AirLoopHVAC:Mixer name ={}. Check inputs", objectName));
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
        ShowSevereError(state, format("AirLoopSplitter factory: Error getting inputs for system named: {}", objectName));
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
        ShowSevereError(state, format("getAirLoopSplitter: did not find AirLoopSplitter name ={}. Check inputs", objectName));
        return index;
    }

    void AirLoopSplitter::getAirLoopSplitter(EnergyPlusData &state)
    {
        bool errorsFound(false);

        std::string const &cCurrentModuleObject = "AirLoopHVAC:Splitter";
        std::string cFieldName;

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int AirLoopSplitterNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &fields = instance.value();
                std::string const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);

                ++AirLoopSplitterNum;
                AirLoopSplitter thisSplitter;

                thisSplitter.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisSplitter.InletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("inlet_node_name").get<std::string>());
                thisSplitter.m_AirLoopSplitter_Num = AirLoopSplitterNum - 1;

                auto NodeNames = fields.find("nodes");
                if (NodeNames != fields.end()) {
                    auto NodeArray = NodeNames.value();
                    thisSplitter.numOfOutletNodes = NodeArray.size();
                    int num = 0;
                    for (nlohmann::json &NodeDOASName : NodeArray) {
                        num += 1;
                        std::string name = UtilityRoutines::MakeUPPERCase(NodeDOASName.at("outlet_node_name").get<std::string>());
                        int NodeNum = UtilityRoutines::FindItemInList(name, state.dataLoopNodes->NodeID);
                        if (NodeNum > 0 && num <= thisSplitter.numOfOutletNodes) {
                            thisSplitter.OutletNodeName.push_back(name);
                            thisSplitter.OutletNodeNum.push_back(NodeNum);
                        } else {
                            cFieldName = "Outlet Node Name";
                            ShowSevereError(state, format("{}, \"{}\"{} not found: {}", cCurrentModuleObject, thisSplitter.name, cFieldName, name));
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

        std::string const &cCurrentModuleObject = "AirLoopHVAC:DedicatedOutdoorAirSystem";
        std::string cFieldName;

        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int AirLoopDOASNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &fields = instance.value();
                std::string const &thisObjectName = instance.key();
                state.dataInputProcessing->inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);
                ++AirLoopDOASNum;
                AirLoopDOAS thisDOAS;

                thisDOAS.Name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                // get OA and avail num
                thisDOAS.OASystemName = UtilityRoutines::MakeUPPERCase(fields.at("airloophvac_outdoorairsystem_name").get<std::string>());
                thisDOAS.m_OASystemNum = UtilityRoutines::FindItemInList(thisDOAS.OASystemName, state.dataAirLoop->OutsideAirSys);
                if (thisDOAS.m_OASystemNum == 0) {
                    cFieldName = "AirLoopHVAC:OutdoorAirSystem Name";
                    ShowSevereError(state,
                                    format("{}, \"{}\", {} not found: {}\n", cCurrentModuleObject, thisDOAS.Name, cFieldName, thisDOAS.OASystemName));
                    errorsFound = true;
                }
                // Check controller type
                std::string_view CurrentModuleObject = "AirLoopHVAC:OutdoorAirSystem";
                for (int InListNum = 1; InListNum <= state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).NumControllers; ++InListNum) {
                    if (UtilityRoutines::SameString(state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ControllerType(InListNum),
                                                    "Controller:OutdoorAir")) {
                        ShowSevereError(state,
                                        format("When {} = {} is used in AirLoopHVAC:DedicatedOutdoorAirSystem,",
                                               CurrentModuleObject,
                                               state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ControllerName(InListNum)));
                        ShowContinueError(state, "The Controller:OutdoorAir can not be used as a controller. Please remove it");
                        errorsFound = true;
                    }
                }

                // get inlet and outlet node number from equipment list
                CurrentModuleObject = "AirLoopHVAC:OutdoorAirSystem:EquipmentList";
                int CoolingCoilOrder = 0;
                int FanOrder = 0;
                for (int CompNum = 1; CompNum <= state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).NumComponents; ++CompNum) {
                    auto &CompType = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentType(CompNum);
                    auto &CompName = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum);
                    auto &thisInletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum);
                    auto &thisOutletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum);

                    bool InletNodeErrFlag = false;
                    bool OutletNodeErrFlag = false;

                    const std::string typeNameUC =
                        UtilityRoutines::MakeUPPERCase(state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentType(CompNum));
                    auto foundType = static_cast<ValidEquipListType>(getEnumerationValue(validEquipNamesUC, typeNameUC));

                    switch (foundType) {
                    case ValidEquipListType::OutdoorAirMixer:
                        ShowSevereError(state,
                                        format("When {} = {} is used in AirLoopHVAC:DedicatedOutdoorAirSystem,", CurrentModuleObject, CompName));
                        ShowContinueError(state, " the OUTDOORAIR:MIXER can not be used as a component. Please remove it");
                        errorsFound = true;
                        break;

                    case ValidEquipListType::FanConstantVolume:
                        ShowSevereError(state,
                                        format("When {} = {} is used in AirLoopHVAC:DedicatedOutdoorAirSystem,", CurrentModuleObject, CompName));
                        ShowContinueError(state,
                                          " the FAN:CONSTANTVOLUME can not be used as a component. The allowed fan types are FAN:SYSTEMMODEL and "
                                          "FAN:COMPONENTMODEL. Please change it");
                        errorsFound = true;
                        break;

                    case ValidEquipListType::FanVariableVolume:
                        ShowSevereError(state,
                                        format("When {} = {} is used in AirLoopHVAC:DedicatedOutdoorAirSystem,", CurrentModuleObject, CompName));
                        ShowContinueError(state,
                                          " the FAN:VARIABLEVOLUME can not be used as a component. The allowed fan types are FAN:SYSTEMMODEL and "
                                          "FAN:COMPONENTMODEL. Please change it");
                        errorsFound = true;
                        break;

                    case ValidEquipListType::FanSystemModel:
                        thisDOAS.FanName = CompName;
                        thisDOAS.m_FanTypeNum = SimAirServingZones::CompType::Fan_System_Object;
                        thisDOAS.m_FanIndex = HVACFan::getFanObjectVectorIndex(state, CompName);
                        thisInletNodeNum = state.dataHVACFan->fanObjs[thisDOAS.m_FanIndex]->inletNodeNum;
                        if (thisInletNodeNum == 0) {
                            InletNodeErrFlag = true;
                        }
                        thisOutletNodeNum = state.dataHVACFan->fanObjs[thisDOAS.m_FanIndex]->outletNodeNum;
                        if (thisOutletNodeNum == 0) {
                            OutletNodeErrFlag = true;
                        }
                        thisDOAS.m_FanInletNodeNum = thisInletNodeNum;
                        thisDOAS.m_FanOutletNodeNum = thisOutletNodeNum;
                        if (CompNum == 1) {
                            thisDOAS.FanBeforeCoolingCoilFlag = true;
                        }
                        FanOrder = CompNum;
                        break;

                    case ValidEquipListType::FanComponentModel:
                        thisDOAS.m_FanTypeNum = SimAirServingZones::CompType::Fan_ComponentModel;
                        Fans::GetFanIndex(state, CompName, thisDOAS.m_FanIndex, errorsFound);
                        thisDOAS.FanName = CompName;
                        if (CompNum == 1) {
                            thisDOAS.FanBeforeCoolingCoilFlag = true;
                        }
                        thisInletNodeNum = Fans::GetFanInletNode(state, typeNameUC, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = Fans::GetFanOutletNode(state, typeNameUC, CompName, OutletNodeErrFlag);
                        thisDOAS.m_FanInletNodeNum = thisInletNodeNum;
                        thisDOAS.m_FanOutletNodeNum = thisOutletNodeNum;
                        FanOrder = CompNum;
                        break;

                    case ValidEquipListType::CoilCoolingWater:
                        thisInletNodeNum = WaterCoils::GetCoilInletNode(state, typeNameUC, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = WaterCoils::GetCoilOutletNode(state, typeNameUC, CompName, OutletNodeErrFlag);
                        thisDOAS.CWCtrlNodeNum = WaterCoils::GetCoilWaterInletNode(state, "COIL:COOLING:WATER", CompName, errorsFound);
                        if (errorsFound) {
                            ShowContinueError(state, format("The control node number is not found in {} = {}", CurrentModuleObject, CompName));
                        }
                        PlantUtilities::ScanPlantLoopsForObject(
                            state, CompName, DataPlant::PlantEquipmentType::CoilWaterCooling, thisDOAS.CWPlantLoc, errorsFound, _, _, _, _, _);
                        if (errorsFound) { // is this really needed here, program fatals out later on when errorsFound = true
                            ShowFatalError(state, "GetAirLoopDOASInput: Program terminated for previous conditions.");
                        }
                        CoolingCoilOrder = CompNum;
                        break;

                    case ValidEquipListType::CoilHeatingWater:
                        thisInletNodeNum = WaterCoils::GetCoilInletNode(state, typeNameUC, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = WaterCoils::GetCoilOutletNode(state, typeNameUC, CompName, OutletNodeErrFlag);
                        thisDOAS.HWCtrlNodeNum = WaterCoils::GetCoilWaterInletNode(state, "Coil:Heating:Water", CompName, errorsFound);
                        if (errorsFound) {
                            ShowContinueError(state, format("The control node number is not found in {} = {}", CurrentModuleObject, CompName));
                        }
                        PlantUtilities::ScanPlantLoopsForObject(
                            state, CompName, DataPlant::PlantEquipmentType::CoilWaterSimpleHeating, thisDOAS.HWPlantLoc, errorsFound, _, _, _, _, _);
                        if (errorsFound) { // is this really needed here, program fatals out later on when errorsFound = true
                            ShowFatalError(state, "GetAirLoopDOASInput: Program terminated for previous conditions.");
                        }
                        break;

                    case ValidEquipListType::CoilHeatingSteam:
                        thisInletNodeNum = SteamCoils::GetCoilSteamInletNode(state, CompType, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = SteamCoils::GetCoilSteamOutletNode(state, CompType, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::CoilCoolingWaterDetailedGeometry:
                        thisInletNodeNum = WaterCoils::GetCoilInletNode(state, typeNameUC, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = WaterCoils::GetCoilOutletNode(state, typeNameUC, CompName, OutletNodeErrFlag);
                        thisDOAS.CWCtrlNodeNum =
                            WaterCoils::GetCoilWaterInletNode(state, "Coil:Cooling:Water:DetailedGeometry", CompName, errorsFound);
                        if (errorsFound) {
                            ShowContinueError(state, format("The control node number is not found in {} = {}", CurrentModuleObject, CompName));
                        }
                        PlantUtilities::ScanPlantLoopsForObject(state,
                                                                CompName,
                                                                DataPlant::PlantEquipmentType::CoilWaterDetailedFlatCooling,
                                                                thisDOAS.CWPlantLoc,
                                                                errorsFound,
                                                                _,
                                                                _,
                                                                _,
                                                                _,
                                                                _);
                        if (errorsFound) { // is this really needed here, program fatals out later on when errorsFound = true
                            ShowFatalError(state, "GetAirLoopDOASInput: Program terminated for previous conditions.");
                        }
                        CoolingCoilOrder = CompNum;
                        break;

                    case ValidEquipListType::CoilHeatingElectric:
                    case ValidEquipListType::CoilHeatingFuel:
                        thisInletNodeNum = HeatingCoils::GetCoilInletNode(state, typeNameUC, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = HeatingCoils::GetCoilOutletNode(state, typeNameUC, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::CoilSystemCoolingWaterHeatExchangerAssisted:
                        thisInletNodeNum = HVACHXAssistedCoolingCoil::GetCoilInletNode(state, CompType, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = HVACHXAssistedCoolingCoil::GetCoilOutletNode(state, CompType, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::CoilSystemCoolingDX:
                    case ValidEquipListType::AirLoopHVACUnitarySystem:
                        if (state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).compPointer[CompNum] == nullptr) {
                            UnitarySystems::UnitarySys thisSys;
                            state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).compPointer[CompNum] =
                                UnitarySystems::UnitarySys::factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, CompName, false, 0);
                        }
                        thisInletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum)
                                               .compPointer[CompNum]
                                               ->getAirInNode(state, CompName, 0, InletNodeErrFlag);
                        thisOutletNodeNum = state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum)
                                                .compPointer[CompNum]
                                                ->getAirOutNode(state, CompName, 0, OutletNodeErrFlag);
                        CoolingCoilOrder = CompNum;
                        break;

                    case ValidEquipListType::CoilSystemHeatingDX:
                        thisInletNodeNum = HVACDXHeatPumpSystem::GetHeatingCoilInletNodeNum(state, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = HVACDXHeatPumpSystem::GetHeatingCoilOutletNodeNum(state, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::CoilUserDefined:
                        ShowSevereError(state,
                                        format("When {} = {} is used in AirLoopHVAC:DedicatedOutdoorAirSystem,", CurrentModuleObject, CompName));
                        ShowContinueError(state, " the COIL:USERDEFINED can not be used as a component.");
                        errorsFound = true;
                        break;

                    case ValidEquipListType::HeatExchangerAirToAirFlatPlate:
                    case ValidEquipListType::HeatExchangerAirToAirSensibleAndLatent:
                    case ValidEquipListType::HeatExchangerDesiccantBalancedFlow:
                        state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).HeatExchangerFlag = true;
                        thisInletNodeNum = HeatRecovery::GetSupplyInletNode(state, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = HeatRecovery::GetSupplyOutletNode(state, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::DehumidifierDesiccantNoFans:
                    case ValidEquipListType::DehumidifierDesiccantSystem:
                        thisInletNodeNum = DesiccantDehumidifiers::GetProcAirInletNodeNum(state, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = DesiccantDehumidifiers::GetProcAirOutletNodeNum(state, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::HumidifierSteamElectric:
                    case ValidEquipListType::HumidifierSteamGas:
                        thisInletNodeNum = Humidifiers::GetAirInletNodeNum(state, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = Humidifiers::GetAirOutletNodeNum(state, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::SolarCollectorUnglazedTranspired:
                        thisInletNodeNum = TranspiredCollector::GetAirInletNodeNum(state, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = TranspiredCollector::GetAirOutletNodeNum(state, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::SolarCollectorFlatPlatePhotovoltaicThermal:
                        thisInletNodeNum = PhotovoltaicThermalCollectors::GetAirInletNodeNum(state, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = PhotovoltaicThermalCollectors::GetAirOutletNodeNum(state, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::EvaporativeCoolerDirectCeldekPad:
                    case ValidEquipListType::EvaporativeCoolerIndirectCeldekPad:
                    case ValidEquipListType::EvaporativeCoolerIndirectWetCoil:
                    case ValidEquipListType::EvaporativeCoolerIndirectResearchSpecial:
                    case ValidEquipListType::EvaporativeCoolerDirectResearchSpecial:
                        thisInletNodeNum = EvaporativeCoolers::GetInletNodeNum(state, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = EvaporativeCoolers::GetOutletNodeNum(state, CompName, OutletNodeErrFlag);
                        break;

                    case ValidEquipListType::ZoneHVACTerminalUnitVariableRefrigerantFlow:
                        thisInletNodeNum = HVACVariableRefrigerantFlow::GetVRFTUInAirNodeFromName(state, CompName, InletNodeErrFlag);
                        thisOutletNodeNum = HVACVariableRefrigerantFlow::GetVRFTUOutAirNodeFromName(state, CompName, OutletNodeErrFlag);
                        break;

                    default:
                        ShowSevereError(state,
                                        format(R"({} = "{}" invalid Outside Air Component="{}".)",
                                               CurrentModuleObject,
                                               CompName,
                                               state.dataAirLoop->OutsideAirSys(thisDOAS.m_OASystemNum).ComponentType(CompNum)));
                        errorsFound = true;
                    }
                    if (CoolingCoilOrder > FanOrder && !thisDOAS.FanBeforeCoolingCoilFlag) {
                        thisDOAS.FanBeforeCoolingCoilFlag = true;
                    }
                    if (InletNodeErrFlag) {
                        ShowSevereError(state, format("Inlet node number is not found in {} = {}", CurrentModuleObject, CompName));
                        errorsFound = true;
                    }
                    if (OutletNodeErrFlag) {
                        ShowSevereError(state, format("Outlet node number is not found in {} = {}", CurrentModuleObject, CompName));
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

                thisDOAS.AvailManagerSchedName = UtilityRoutines::MakeUPPERCase(fields.at("availability_schedule_name").get<std::string>());
                thisDOAS.m_AvailManagerSchedPtr = GetScheduleIndex(state, thisDOAS.AvailManagerSchedName);
                if (thisDOAS.m_AvailManagerSchedPtr == 0) {
                    cFieldName = "Availability Schedule Name";
                    ShowSevereError(
                        state,
                        format("{}, \"{}\" {} not found: {}", cCurrentModuleObject, thisDOAS.Name, cFieldName, thisDOAS.AvailManagerSchedName));
                    errorsFound = true;
                }

                thisDOAS.AirLoopMixerName = UtilityRoutines::MakeUPPERCase(fields.at("airloophvac_mixer_name").get<std::string>()); //
                thisDOAS.m_AirLoopMixerIndex = getAirLoopMixerIndex(state, thisDOAS.AirLoopMixerName);
                if (thisDOAS.m_AirLoopMixerIndex < 0) {
                    cFieldName = "AirLoopHVAC:Mixer Name";
                    ShowSevereError(
                        state, format("{}, \"{}\" {} not found: {}", cCurrentModuleObject, thisDOAS.Name, cFieldName, thisDOAS.AirLoopMixerName));
                    errorsFound = true;
                }
                AirLoopMixer thisAirLoopMixer;
                thisDOAS.m_CompPointerAirLoopMixer = thisAirLoopMixer.factory(state, thisDOAS.m_AirLoopMixerIndex, thisDOAS.AirLoopMixerName);
                thisDOAS.AirLoopSplitterName = UtilityRoutines::MakeUPPERCase(fields.at("airloophvac_splitter_name").get<std::string>()); //
                thisDOAS.m_AirLoopSplitterIndex = getAirLoopSplitterIndex(state, thisDOAS.AirLoopSplitterName);
                if (thisDOAS.m_AirLoopSplitterIndex < 0) {
                    cFieldName = "AirLoopHVAC:Splitter Name";
                    ShowSevereError(
                        state, format("{}, \"{}\" {} not found: {}", cCurrentModuleObject, thisDOAS.Name, cFieldName, thisDOAS.AirLoopSplitterName));
                    errorsFound = true;
                }
                AirLoopSplitter thisAirLoopSplitter;
                thisDOAS.m_CompPointerAirLoopSplitter =
                    thisAirLoopSplitter.factory(state, thisDOAS.m_AirLoopSplitterIndex, thisDOAS.AirLoopSplitterName);

                // get pretreated desing conditions
                thisDOAS.PreheatTemp = fields.at("preheat_design_temperature").get<Real64>();
                thisDOAS.PreheatHumRat = fields.at("preheat_design_humidity_ratio").get<Real64>();
                thisDOAS.PrecoolTemp = fields.at("precool_design_temperature").get<Real64>();
                thisDOAS.PrecoolHumRat = fields.at("precool_design_humidity_ratio").get<Real64>();

                // get info on AirLoops
                thisDOAS.NumOfAirLoops = fields.at("number_of_airloophvac").get<int>(); //
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
                        std::string name = UtilityRoutines::MakeUPPERCase(AirLoopHVACName.at("airloophvac_name").get<std::string>());
                        int LoopNum = UtilityRoutines::FindItemInList(name, state.dataAirSystemsData->PrimaryAirSystems);
                        num += 1;
                        if (LoopNum > 0 && num <= thisDOAS.NumOfAirLoops) {
                            thisDOAS.AirLoopName.push_back(name);
                            thisDOAS.m_AirLoopNum.push_back(LoopNum);
                        } else {
                            cFieldName = "AirLoopHVAC Name";
                            ShowSevereError(state, format("{}, \"{}\" {} not found: {}", cCurrentModuleObject, thisDOAS.Name, cFieldName, name));
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
                                        format("AirLoopHVAC:OutdoorAirSystem = \"{}\" invalid Controller List Name = \" not found.",
                                               state.dataAirLoop->OutsideAirSys(OASysNum).Name));
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
                                                            state.dataPlnt->PlantLoop(this->HWPlantLoc.loopNum).FluidName,
                                                            DataGlobalConstants::HWInitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->HWPlantLoc.loopNum).FluidIndex,
                                                            RoutineName);
                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       CoilMaxVolFlowRate * rho,
                                                       this->HWCtrlNodeNum,
                                                       state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum));
                }
                if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER")) {
                    WaterCoils::SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, this->m_CoolCoilNum);
                    Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate(state, "Coil:Cooling:Water", CompName, ErrorsFound);
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->CWPlantLoc.loopNum).FluidName,
                                                            DataGlobalConstants::CWInitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->CWPlantLoc.loopNum).FluidIndex,
                                                            RoutineName);
                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       CoilMaxVolFlowRate * rho,
                                                       this->CWCtrlNodeNum,
                                                       state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum));
                }
                if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY")) {
                    WaterCoils::SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, this->m_CoolCoilNum);
                    Real64 CoilMaxVolFlowRate =
                        WaterCoils::GetCoilMaxWaterFlowRate(state, "Coil:Cooling:Water:DetailedGeometry", CompName, ErrorsFound);
                    rho = FluidProperties::GetDensityGlycol(state,
                                                            state.dataPlnt->PlantLoop(this->CWPlantLoc.loopNum).FluidName,
                                                            DataGlobalConstants::CWInitConvTemp,
                                                            state.dataPlnt->PlantLoop(this->CWPlantLoc.loopNum).FluidIndex,
                                                            RoutineName);
                    PlantUtilities::InitComponentNodes(state,
                                                       0.0,
                                                       CoilMaxVolFlowRate * rho,
                                                       this->CWCtrlNodeNum,
                                                       state.dataAirLoop->OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum));
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
    }

    void AirLoopDOAS::CalcAirLoopDOAS(EnergyPlusData &state, bool const FirstHVACIteration)
    {
        using MixedAir::ManageOutsideAirSystem;

        this->m_CompPointerAirLoopMixer->CalcAirLoopMixer(state);
        if (this->m_FanIndex > -1) {
            state.dataLoopNodes->Node(this->m_FanInletNodeNum).MassFlowRateMaxAvail = this->SumMassFlowRate;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMaxAvail = this->SumMassFlowRate;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMax = this->SumMassFlowRate;
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

        if (this->m_FanIndex > -1 && this->m_FanTypeNum == SimAirServingZones::CompType::Fan_System_Object) {
            state.dataHVACFan->fanObjs[this->m_FanIndex]->designAirVolFlowRate = sizingMassFlow / state.dataEnvrn->StdRhoAir;
            state.dataLoopNodes->Node(this->m_FanInletNodeNum).MassFlowRateMaxAvail = sizingMassFlow;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMaxAvail = sizingMassFlow;
            state.dataLoopNodes->Node(this->m_FanOutletNodeNum).MassFlowRateMax = sizingMassFlow;
        }
        bool errorsFound = false;
        if (this->m_FanIndex > 0 && this->m_FanTypeNum == SimAirServingZones::CompType::Fan_ComponentModel) {
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
        for (auto &env : state.dataWeatherManager->Environment) {
            if (env.KindOfEnvrn != DataGlobalConstants::KindOfSim::DesignDay && env.KindOfEnvrn != DataGlobalConstants::KindOfSim::RunPeriodDesign)
                continue;
            if (env.maxCoolingOATSizing > this->SizingCoolOATemp) {
                this->SizingCoolOATemp = env.maxCoolingOATSizing;
                // DesignDayNum = 0 for KindOfSim == RunPeriodDesign
                if (env.KindOfEnvrn == DataGlobalConstants::KindOfSim::DesignDay &&
                    state.dataWeatherManager->DesDayInput(env.DesignDayNum).PressureEntered) {
                    this->SizingCoolOAHumRat = Psychrometrics::PsyWFnTdpPb(
                        state, env.maxCoolingOADPSizing, state.dataWeatherManager->DesDayInput(env.DesignDayNum).PressBarom);
                } else {
                    this->SizingCoolOAHumRat = Psychrometrics::PsyWFnTdpPb(state, env.maxCoolingOADPSizing, state.dataEnvrn->StdBaroPress);
                }
            }
            if (env.minHeatingOATSizing < this->HeatOutTemp) {
                this->HeatOutTemp = env.minHeatingOATSizing;
                if (env.KindOfEnvrn == DataGlobalConstants::KindOfSim::DesignDay &&
                    state.dataWeatherManager->DesDayInput(env.DesignDayNum).PressureEntered) {
                    this->HeatOutHumRat = Psychrometrics::PsyWFnTdpPb(
                        state, env.minHeatingOADPSizing, state.dataWeatherManager->DesDayInput(env.DesignDayNum).PressBarom);
                } else {
                    this->HeatOutHumRat = Psychrometrics::PsyWFnTdpPb(state, env.minHeatingOADPSizing, state.dataEnvrn->StdBaroPress);
                }
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
                    ShowWarningError(state, format("Convergence limit is above 1.0e-6 for unit={}", loop.Name));
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
