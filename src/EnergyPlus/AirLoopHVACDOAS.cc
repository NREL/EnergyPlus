// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#include <EnergyPlus/AirLoopHVACDOAS.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACControllers.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACDXHeatPumpSystem.hh>
#include <EnergyPlus/HVACDXSystem.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutdoorAirUnit.hh>
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
#include <string> // std::string, std::to_string

namespace EnergyPlus {
namespace AirLoopHVACDOAS {

    int numAirLoopDOAS(0);

    bool GetInputOnceFlag(true);
    bool getAirLoopMixerInputOnceFlag(true);
    bool getAirLoopSplitterInputOnceFlag(true);

    std::vector<AirLoopDOAS> airloopDOAS;
    std::vector<AirLoopMixer> airloopMixer;
    std::vector<AirLoopSplitter> airloopSplitter;

    AirLoopMixer::AirLoopMixer() : numOfInletNodes(0), m_AirLoopMixer_Num(0), OutletNodeNum(0), OutletTemp(0.0)
    {
    }

    AirLoopSplitter::AirLoopSplitter() : numOfOutletNodes(0), m_AirLoopSplitter_Num(0), InletTemp(0.0)
    {
    }

    AirLoopDOAS::AirLoopDOAS() // constructor
        : SumMassFlowRate(0.0), PreheatTemp(-999.0), PrecoolTemp(-999.0), PreheatHumRat(-999.0), PrecoolHumRat(-999.0), SizingMassFlow(0.0),
          SizingCoolOATemp(-999.0), SizingCoolOAHumRat(-999.0), HeatOutTemp(0.0), HeatOutHumRat(0.0), m_OASystemNum(0), m_AvailManagerSchedPtr(0),
          m_AirLoopMixerIndex(-1), m_AirLoopSplitterIndex(-1), NumOfAirLoops(0), m_InletNodeNum(0), m_OutletNodeNum(0), m_FanIndex(-1),
          m_FanInletNodeNum(0), m_FanOutletNodeNum(0), m_FanTypeNum(0), m_HeatCoilNum(0), m_CoolCoilNum(0), ConveCount(0), ConveIndex(0),
          m_HeatExchangerFlag(false), SizingOnceFlag(true), DXCoilFlag(false), FanBlowTroughFlag(false), HWLoopNum(0), HWLoopSide(0), HWBranchNum(0),
          HWCompNum(0), HWCtrlNodeNum(0), CWLoopNum(0), CWLoopSide(0), CWBranchNum(0), CWCompNum(0), CWCtrlNodeNum(0)

    {
    }

    // Clears the global data in AirLoopDOAS.
    void clear_state()
    {
        GetInputOnceFlag = true;
        getAirLoopMixerInputOnceFlag = true;
        getAirLoopSplitterInputOnceFlag = true;

        airloopDOAS.clear();
        airloopMixer.clear();
        airloopSplitter.clear();

        numAirLoopDOAS = 0;
    }

    void AirLoopDOAS::SimAirLoopHVACDOAS(bool const FirstHVACIteration, int &CompIndex)
    {

        // Obtains and Allocates unitary system related parameters from input file
        if (GetInputOnceFlag) {
            // Get the AirLoopHVACDOAS input
            getAirLoopDOASInput();
            GetInputOnceFlag = false;
        }

        if (CompIndex == -1) {
            CompIndex = this->m_AirLoopDOASNum;
        }

        if (this->SizingOnceFlag) {
            this->SizingAirLoopDOAS();
            this->SizingOnceFlag = false;
        }

        this->initAirLoopDOAS(FirstHVACIteration);

        if (this->SumMassFlowRate == 0.0 && !DataGlobals::BeginEnvrnFlag) {
            DataLoopNode::Node(this->m_CompPointerAirLoopMixer->OutletNodeNum).MassFlowRate = 0.0;
            return;
        }

        this->CalcAirLoopDOAS(FirstHVACIteration);
    }

    AirLoopMixer *AirLoopMixer::factory(int object_num, std::string const objectName)
    {

        if (getAirLoopMixerInputOnceFlag) {
            AirLoopMixer::getAirLoopMixer();
            getAirLoopMixerInputOnceFlag = false;
        }

        int MixerNum = -1;
        for (auto &dSpec : airloopMixer) {
            ++MixerNum;
            if (UtilityRoutines::SameString(dSpec.name, objectName) && dSpec.m_AirLoopMixer_Num == object_num) {
                return &dSpec;
            }
        }

        ShowSevereError("AirLoopMixer factory: Error getting inputs for system named: " + objectName);
        return nullptr;
    }

    void AirLoopMixer::getAirLoopMixer()
    {
        using DataLoopNode::NodeID;

        bool errorsFound(false);

        std::string cCurrentModuleObject = "AirLoopHVAC:Mixer";
        std::string cFieldName;

        auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int AirLoopMixerNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);
                ++AirLoopMixerNum;
                AirLoopMixer thisMixer;

                thisMixer.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisMixer.OutletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("outlet_node_name"));
                thisMixer.m_AirLoopMixer_Num = AirLoopMixerNum - 1;
                thisMixer.OutletNodeNum = NodeInputManager::GetOnlySingleNode(thisMixer.OutletNodeName,
                                                                              errorsFound,
                                                                              cCurrentModuleObject,
                                                                              thisObjectName,
                                                                              DataLoopNode::NodeType_Air,
                                                                              DataLoopNode::NodeConnectionType_Outlet,
                                                                              1,
                                                                              DataLoopNode::ObjectIsParent);

                auto NodeNames = fields.find("nodes");
                if (NodeNames != fields.end()) {
                    auto NodeArray = NodeNames.value();
                    thisMixer.numOfInletNodes = NodeArray.size();
                    int num = 0;
                    for (auto NodeDOASName : NodeArray) {
                        num += 1;
                        std::string name = UtilityRoutines::MakeUPPERCase(NodeDOASName.at("inlet_node_name"));
                        int NodeNum = UtilityRoutines::FindItemInList(name, NodeID);
                        if (NodeNum > 0 && num <= thisMixer.numOfInletNodes) {
                            thisMixer.InletNodeName.push_back(name);
                            thisMixer.InletNodeNum.push_back(NodeNum);
                        } else {
                            cFieldName = "Inlet Node Name";
                            ShowSevereError(cCurrentModuleObject + ", \"" + thisMixer.name + "\" " + name + " not found: " + cFieldName);
                            errorsFound = true;
                        }
                    }
                }

                airloopMixer.push_back(thisMixer);
            }
            if (errorsFound) {
                ShowFatalError("getAirLoopMixer: Previous errors cause termination.");
            }
        }
    } // namespace AirLoopMixer

    void AirLoopMixer::CalcAirLoopMixer()
    {
        Real64 OutletTemp = 0.0;
        Real64 OutletHumRat = 0.0;
        Real64 MassSum = 0.0;
        int InletNum;

        for (int i = 1; i <= this->numOfInletNodes; i++) {
            InletNum = this->InletNodeNum[i - 1];
            MassSum += DataLoopNode::Node(InletNum).MassFlowRate;
            OutletTemp += DataLoopNode::Node(InletNum).MassFlowRate * DataLoopNode::Node(InletNum).Temp;
            OutletHumRat += DataLoopNode::Node(InletNum).MassFlowRate * DataLoopNode::Node(InletNum).HumRat;
        }
        if (MassSum > 0.0) {
            DataLoopNode::Node(this->OutletNodeNum).Temp = OutletTemp / MassSum;
            DataLoopNode::Node(this->OutletNodeNum).HumRat = OutletHumRat / MassSum;
            DataLoopNode::Node(this->OutletNodeNum).MassFlowRate = MassSum;
            DataLoopNode::Node(this->OutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(OutletTemp / MassSum, OutletHumRat / MassSum);
            this->OutletTemp = DataLoopNode::Node(this->OutletNodeNum).Temp;
        } else {
            DataLoopNode::Node(this->OutletNodeNum).Temp = DataLoopNode::Node(this->InletNodeNum[0]).Temp;
            DataLoopNode::Node(this->OutletNodeNum).HumRat = DataLoopNode::Node(this->InletNodeNum[0]).HumRat;
            DataLoopNode::Node(this->OutletNodeNum).MassFlowRate = 0.0;
            DataLoopNode::Node(this->OutletNodeNum).Enthalpy = DataLoopNode::Node(this->InletNodeNum[0]).Enthalpy;
            this->OutletTemp = DataLoopNode::Node(this->InletNodeNum[0]).Temp;
        }
    }

    int getAirLoopMixerIndex(         // lookup vector index for AirLoopHVAC:Mixer object name
        std::string const &objectName // IDF name in input
    )
    {
        if (getAirLoopMixerInputOnceFlag) {
            AirLoopMixer::getAirLoopMixer();
            getAirLoopMixerInputOnceFlag = false;
        }

        int index = -1;
        for (std::size_t loop = 0; loop < airloopMixer.size(); ++loop) {
            AirLoopMixer *thisAirLoopMixerObjec = &airloopMixer[loop];
            if (UtilityRoutines::SameString(objectName, thisAirLoopMixerObjec->name)) {
                index = loop;
                return index;
            }
        }
        ShowSevereError("getAirLoopMixer: did not find AirLoopHVAC:Mixer name =" + objectName + ". Check inputs");
        return index;
    }

    AirLoopSplitter *AirLoopSplitter::factory(int object_num, std::string const objectName)
    {

        if (getAirLoopSplitterInputOnceFlag) {
            AirLoopSplitter::getAirLoopSplitter();
            getAirLoopSplitterInputOnceFlag = false;
        }

        int SplitterNum = -1;
        for (auto &dSpec : airloopSplitter) {
            SplitterNum++;
            if (UtilityRoutines::SameString(dSpec.name, objectName) && dSpec.m_AirLoopSplitter_Num == object_num) {
                return &dSpec;
            }
        }
        ShowSevereError("AirLoopSplitter factory: Error getting inputs for system named: " + objectName);
        return nullptr;
    }

    void AirLoopSplitter::CalcAirLoopSplitter(Real64 Temp, Real64 HumRat)
    {
        for (int i = 0; i < this->numOfOutletNodes; i++) {
            DataLoopNode::Node(this->OutletNodeNum[i]).Temp = Temp;
            DataLoopNode::Node(this->OutletNodeNum[i]).HumRat = HumRat;
            DataLoopNode::Node(this->OutletNodeNum[i]).Enthalpy = Psychrometrics::PsyHFnTdbW(Temp, HumRat);
        }
        this->InletTemp = Temp;
    }

    int getAirLoopSplitterIndex(      // lookup vector index for AirLoopHVAC:Splitter object name
        std::string const &objectName // IDF name in input
    )
    {
        if (getAirLoopSplitterInputOnceFlag) {
            AirLoopSplitter::getAirLoopSplitter();
            getAirLoopSplitterInputOnceFlag = false;
        }

        int index = -1;
        for (std::size_t loop = 0; loop < airloopSplitter.size(); ++loop) {
            AirLoopSplitter *thisAirLoopSplitterObjec = &airloopSplitter[loop];
            if (UtilityRoutines::SameString(objectName, thisAirLoopSplitterObjec->name)) {
                index = loop;
                return index;
            }
        }
        ShowSevereError("getAirLoopSplitter: did not find AirLoopSplitter name =" + objectName + ". Check inputs");
        return index;
    }

    void AirLoopSplitter::getAirLoopSplitter()
    {
        using DataLoopNode::NodeID;

        bool errorsFound(false);

        std::string cCurrentModuleObject = "AirLoopHVAC:Splitter";
        std::string cFieldName;

        auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int AirLoopSplitterNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);

                ++AirLoopSplitterNum;
                AirLoopSplitter thisSplitter;

                thisSplitter.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisSplitter.InletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("inlet_node_name"));
                thisSplitter.m_AirLoopSplitter_Num = AirLoopSplitterNum - 1;

                auto NodeNames = fields.find("nodes");
                if (NodeNames != fields.end()) {
                    auto NodeArray = NodeNames.value();
                    thisSplitter.numOfOutletNodes = NodeArray.size();
                    int num = 0;
                    for (auto NodeDOASName : NodeArray) {
                        num += 1;
                        std::string name = UtilityRoutines::MakeUPPERCase(NodeDOASName.at("outlet_node_name"));
                        int NodeNum = UtilityRoutines::FindItemInList(name, NodeID);
                        if (NodeNum > 0 && num <= thisSplitter.numOfOutletNodes) {
                            thisSplitter.OutletNodeName.push_back(name);
                            thisSplitter.OutletNodeNum.push_back(NodeNum);
                        } else {
                            cFieldName = "Outlet Node Name";
                            ShowSevereError(cCurrentModuleObject + ", \"" + thisSplitter.name + "\" " + cFieldName +
                                            " not found: " + name);
                            errorsFound = true;
                        }
                    }
                }

                airloopSplitter.push_back(thisSplitter);
            }
            if (errorsFound) {
                ShowFatalError("getAirLoopSplitter: Previous errors cause termination.");
            }
        }
    } // namespace AirLoopSplitter

    void AirLoopDOAS::getAirLoopDOASInput()
    {

        using DataAirLoop::OutsideAirSys;
        using DataAirSystems::PrimaryAirSystem;
        using ScheduleManager::GetScheduleIndex;

        bool errorsFound(false);

        std::string cCurrentModuleObject = "AirLoopHVAC:DedicatedOutdoorAirSystem";
        std::string cFieldName;

        auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            int AirLoopDOASNum = 0;
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {

                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed(cCurrentModuleObject, thisObjectName);
                ++AirLoopDOASNum;
                AirLoopDOAS thisDOAS;

                thisDOAS.Name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                // get OA and avail num
                thisDOAS.OASystemName = UtilityRoutines::MakeUPPERCase(fields.at("airloophvac_outdoorairsystem_name"));
                thisDOAS.m_OASystemNum = UtilityRoutines::FindItemInList(thisDOAS.OASystemName, OutsideAirSys);
                if (thisDOAS.m_OASystemNum == 0) {
                    cFieldName = "AirLoopHVAC:OutdoorAirSystem Name";
                    ShowSevereError(cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName + " not found: " + thisDOAS.OASystemName);
                    errorsFound = true;
                }
                // Check controller type
                std::string CurrentModuleObject = "AirLoopHVAC:OutdoorAirSystem";
                for (int InListNum = 1; InListNum <= OutsideAirSys(thisDOAS.m_OASystemNum).NumControllers; ++InListNum) {
                    if (UtilityRoutines::SameString(OutsideAirSys(thisDOAS.m_OASystemNum).ControllerType(InListNum), "Controller:OutdoorAir")) {
                        ShowSevereError("When " + CurrentModuleObject + " = " + OutsideAirSys(thisDOAS.m_OASystemNum).ControllerName(InListNum) +
                                        " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError("The Controller:OutdoorAir can not be used as a controller. Please remove it");
                        errorsFound = true;
                    }
                }

                // get inlet and outlet node number from equipment list
                CurrentModuleObject = "AirLoopHVAC:OutdoorAirSystem:EquipmentList";
                for (int CompNum = 1; CompNum <= OutsideAirSys(thisDOAS.m_OASystemNum).NumComponents; ++CompNum) {
                    std::string CompType = OutsideAirSys(thisDOAS.m_OASystemNum).ComponentType(CompNum);
                    std::string CompName = OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum);
                    bool InletNodeErrFlag = false;
                    bool OutletNodeErrFlag = false;

                    auto const SELECT_CASE_var(UtilityRoutines::MakeUPPERCase(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentType(CompNum)));

                    if (SELECT_CASE_var == "OUTDOORAIR:MIXER") {
                        ShowSevereError("When " + CurrentModuleObject + " = " + OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum) +
                                        " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError(" the OUTDOORAIR:MIXER can not be used as a component. Please remove it");
                        errorsFound = true;

                    } else if (SELECT_CASE_var == "FAN:CONSTANTVOLUME") {
                        ShowSevereError("When " + CurrentModuleObject + " = " + OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum) +
                                        " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError(" the FAN:CONSTANTVOLUME can not be used as a component. The alllowed fan types are FAN:SYSTEMMODEL and "
                                          "FAN:COMPONENTMODEL. Please change it");
                        errorsFound = true;
                    } else if (SELECT_CASE_var == "FAN:VARIABLEVOLUME") {
                        ShowSevereError("When " + CurrentModuleObject + " = " + OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum) +
                                        " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError(" the FAN:VARIABLEVOLUME can not be used as a component. The alllowed fan types are FAN:SYSTEMMODEL and "
                                          "FAN:COMPONENTMODEL. Please change it");
                        errorsFound = true;
                    } else if (SELECT_CASE_var == "FAN:SYSTEMMODEL") {
                        thisDOAS.FanName = CompName;
                        thisDOAS.m_FanTypeNum = SimAirServingZones::Fan_System_Object;
                        thisDOAS.m_FanIndex = HVACFan::getFanObjectVectorIndex(CompName);
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = HVACFan::fanObjs[thisDOAS.m_FanIndex]->inletNodeNum;
                        if (OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) == 0) {
                            InletNodeErrFlag = true;
                            errorsFound = true;
                        }
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = HVACFan::fanObjs[thisDOAS.m_FanIndex]->outletNodeNum;
                        if (OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) == 0) {
                            OutletNodeErrFlag = true;
                            errorsFound = true;
                        }
                        thisDOAS.m_FanInletNodeNum = OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum);
                        thisDOAS.m_FanOutletNodeNum = OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum);
                        if (CompNum == 1) {
                            thisDOAS.FanBlowTroughFlag = true;
                        }
                        if (!(CompNum == 1 || CompNum == OutsideAirSys(thisDOAS.m_OASystemNum).NumComponents)) {
                            ShowSevereError("The fan placement is either first as blow through or last as draw through in" + CurrentModuleObject +
                                            " = " + OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                            ShowContinueError("The current position is number " + General::RoundSigDigits(CompNum));
                            errorsFound = true;
                        }
                    } else if (SELECT_CASE_var == "FAN:COMPONENTMODEL") {
                        thisDOAS.m_FanTypeNum = SimAirServingZones::Fan_ComponentModel;
                        Fans::GetFanIndex(CompName, thisDOAS.m_FanIndex, errorsFound);
                        thisDOAS.FanName = CompName;
                        if (CompNum == 1) {
                            thisDOAS.FanBlowTroughFlag = true;
                        }
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            Fans::GetFanInletNode(SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            Fans::GetFanOutletNode(SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                        thisDOAS.m_FanInletNodeNum = OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum);
                        thisDOAS.m_FanOutletNodeNum = OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum);
                        if (!(CompNum == 1 || CompNum == OutsideAirSys(thisDOAS.m_OASystemNum).NumComponents)) {
                            ShowSevereError("The fan placement is either first as blow through or last as draw through in" + CurrentModuleObject +
                                            " = " + OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                            ShowContinueError("The current position is number " + General::RoundSigDigits(CompNum));
                            errorsFound = true;
                        }
                    } else if (SELECT_CASE_var == "COIL:COOLING:WATER") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            WaterCoils::GetCoilInletNode(SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            WaterCoils::GetCoilOutletNode(SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                        thisDOAS.CWCtrlNodeNum = WaterCoils::GetCoilWaterInletNode("COIL:COOLING:WATER", CompName, errorsFound);
                        if (errorsFound) {
                            ShowContinueError("The control node number is not found in " + CurrentModuleObject + " = " +
                                              OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        }
                        PlantUtilities::ScanPlantLoopsForObject(CompName,
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
                        if (errorsFound) {
                            ShowFatalError("GetAirLoopDOASInput: Program terminated for previous conditions.");
                        }
                    } else if (SELECT_CASE_var == "COIL:HEATING:WATER") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            WaterCoils::GetCoilInletNode(SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            WaterCoils::GetCoilOutletNode(SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                        thisDOAS.HWCtrlNodeNum = WaterCoils::GetCoilWaterInletNode("Coil:Heating:Water", CompName, errorsFound);
                        if (errorsFound) {
                            ShowContinueError("The control node number is not found in " + CurrentModuleObject + " = " +
                                              OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        }
                        PlantUtilities::ScanPlantLoopsForObject(CompName,
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
                        if (errorsFound) {
                            ShowFatalError("GetAirLoopDOASInput: Program terminated for previous conditions.");
                        }

                    } else if (SELECT_CASE_var == "COIL:HEATING:STEAM") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            SteamCoils::GetCoilSteamInletNode(CompType, CompName, errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            SteamCoils::GetCoilSteamOutletNode(CompType, CompName, errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            WaterCoils::GetCoilInletNode(SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            WaterCoils::GetCoilOutletNode(SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                        thisDOAS.CWCtrlNodeNum = WaterCoils::GetCoilWaterInletNode("Coil:Cooling:Water:DetailedGeometry", CompName, errorsFound);
                        if (errorsFound) {
                            ShowContinueError("The control node number is not found in " + CurrentModuleObject + " = " +
                                              OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        }
                        PlantUtilities::ScanPlantLoopsForObject(CompName,
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
                        if (errorsFound) {
                            ShowFatalError("GetAirLoopDOASInput: Program terminated for previous conditions.");
                        }
                    } else if (SELECT_CASE_var == "COIL:HEATING:ELECTRIC") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = HeatingCoils::GetCoilInletNode(
                            SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = HeatingCoils::GetCoilOutletNode(
                            SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "COIL:HEATING:FUEL") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = HeatingCoils::GetCoilInletNode(
                            SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) = HeatingCoils::GetCoilOutletNode(
                            SELECT_CASE_var, OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HVACHXAssistedCoolingCoil::GetCoilInletNode(CompType, CompName, errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HVACHXAssistedCoolingCoil::GetCoilOutletNode(CompType, CompName, errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "COILSYSTEM:COOLING:DX") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HVACDXSystem::GetCoolingCoilInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        if (OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) == 0) {
                            InletNodeErrFlag = true;
                            errorsFound = true;
                        }
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HVACDXSystem::GetCoolingCoilOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        if (OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) == 0) {
                            OutletNodeErrFlag = true;
                            errorsFound = true;
                        }
                        OutsideAirSys(thisDOAS.m_OASystemNum).DXCoolingCoilFlag = true;
                    } else if (SELECT_CASE_var == "COILSYSTEM:HEATING:DX") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HVACDXHeatPumpSystem::GetHeatingCoilInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        if (OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) == 0) {
                            InletNodeErrFlag = true;
                            errorsFound = true;
                        }
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HVACDXHeatPumpSystem::GetHeatingCoilOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        if (OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) == 0) {
                            OutletNodeErrFlag = true;
                            errorsFound = true;
                        }

                    } else if (SELECT_CASE_var == "AIRLOOPHVAC:UNITARYSYSTEM") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            OutsideAirSys(thisDOAS.m_OASystemNum).compPointer[CompNum]->AirInNode;
                        if (OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) == 0) {
                            InletNodeErrFlag = true;
                            errorsFound = true;
                        }
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            OutsideAirSys(thisDOAS.m_OASystemNum).compPointer[CompNum]->AirOutNode;
                        if (OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) == 0) {
                            OutletNodeErrFlag = true;
                            errorsFound = true;
                        }
                    } else if (SELECT_CASE_var == "COIL:USERDEFINED") {
                        ShowSevereError("When " + CurrentModuleObject + " = " + OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum) +
                                        " is used in AirLoopHVAC:DedicatedOutdoorAirSystem,");
                        ShowContinueError(" the COIL:USERDEFINED can not be used as a component. Please change it");
                        errorsFound = true;
                        // Heat recovery
                    } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:FLATPLATE") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).HeatExchangerFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HeatRecovery::GetSupplyInletNode(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HeatRecovery::GetSupplyOutletNode(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) {
                            ShowContinueError("Node number is not found in " + CurrentModuleObject + " = " +
                                              OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum));
                        }
                    } else if (SELECT_CASE_var == "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).HeatExchangerFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HeatRecovery::GetSupplyInletNode(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HeatRecovery::GetSupplyOutletNode(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "HEATEXCHANGER:DESICCANT:BALANCEDFLOW") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).HeatExchangerFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            HeatRecovery::GetSupplyInletNode(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(CompNum) =
                            HeatRecovery::GetSupplyOutletNode(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                        // Desiccant Dehumidifier
                    } else if (SELECT_CASE_var == "DEHUMIDIFIER:DESICCANT:NOFANS") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            DesiccantDehumidifiers::GetProcAirInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = DesiccantDehumidifiers::GetProcAirOutletNodeNum(
                            OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "DEHUMIDIFIER:DESICCANT:SYSTEM") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            DesiccantDehumidifiers::GetProcAirInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = DesiccantDehumidifiers::GetProcAirOutletNodeNum(
                            OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                        // Humidifiers: Humidifier:Steam:Electric and Humidifier:Steam:Gas
                    } else if (SELECT_CASE_var == "HUMIDIFIER:STEAM:ELECTRIC") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            Humidifiers::GetAirInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            Humidifiers::GetAirOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "HUMIDIFIER:STEAM:GAS") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            Humidifiers::GetAirInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            Humidifiers::GetAirOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;

                        // Unglazed Transpired Solar Collector
                    } else if (SELECT_CASE_var == "SOLARCOLLECTOR:UNGLAZEDTRANSPIRED") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            TranspiredCollector::GetAirInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            TranspiredCollector::GetAirOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                        // PVT air heater
                    } else if (SELECT_CASE_var == "SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = PhotovoltaicThermalCollectors::GetAirInletNodeNum(
                            OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) = PhotovoltaicThermalCollectors::GetAirOutletNodeNum(
                            OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                        // Evaporative Cooler Types
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:CELDEKPAD") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:WETCOIL") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else if (SELECT_CASE_var == "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL") {
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetInletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) InletNodeErrFlag = true;
                        OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(CompNum) =
                            EvaporativeCoolers::GetOutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).ComponentName(CompNum), errorsFound);
                        if (errorsFound) OutletNodeErrFlag = true;
                    } else {
                        ShowSevereError(CurrentModuleObject + " = \"" + CompName + "\" invalid Outside Air Component=\"" +
                                        OutsideAirSys(thisDOAS.m_OASystemNum).ComponentType(CompNum) + "\".");
                        errorsFound = true;
                    }
                    if (InletNodeErrFlag) {
                        ShowSevereError("Inlet node number is not found in " + CurrentModuleObject + " = " + CompName);
                    }
                    if (OutletNodeErrFlag) {
                        ShowSevereError("Outlet node number is not found in " + CurrentModuleObject + " = " + CompName);
                    }

                }

                thisDOAS.m_InletNodeNum = OutsideAirSys(thisDOAS.m_OASystemNum).InletNodeNum(1);
                thisDOAS.m_OutletNodeNum = OutsideAirSys(thisDOAS.m_OASystemNum).OutletNodeNum(OutsideAirSys(thisDOAS.m_OASystemNum).NumComponents);
                OutsideAirSys(thisDOAS.m_OASystemNum).AirLoopDOASNum = AirLoopDOASNum - 1;
                // Set up parent-child connection
                BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                     thisDOAS.Name,
                                                     "AIRLOOPHVAC:OUTDOORAIRSYSTEM",
                                                     thisDOAS.OASystemName,
                                                     DataLoopNode::NodeID(thisDOAS.m_InletNodeNum),
                                                     DataLoopNode::NodeID(thisDOAS.m_OutletNodeNum));

                if (OutsideAirSys(thisDOAS.m_OASystemNum).HeatExchangerFlag) {
                    thisDOAS.m_HeatExchangerFlag = true;
                }

                thisDOAS.AvailManagerSchedName = UtilityRoutines::MakeUPPERCase(fields.at("availability_schedule_name"));
                thisDOAS.m_AvailManagerSchedPtr = GetScheduleIndex(thisDOAS.AvailManagerSchedName);
                if (thisDOAS.m_AvailManagerSchedPtr == 0) {
                    cFieldName = "Availability Schedule Name";
                    ShowSevereError(cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName +
                                    " not found: " + thisDOAS.AvailManagerSchedName);
                    errorsFound = true;
                }

                thisDOAS.AirLoopMixerName = UtilityRoutines::MakeUPPERCase(fields.at("airloophvac_mixer_name")); //
                thisDOAS.m_AirLoopMixerIndex = getAirLoopMixerIndex(thisDOAS.AirLoopMixerName);
                if (thisDOAS.m_AirLoopMixerIndex < 0) {
                    cFieldName = "AirLoopHVAC:Mixer Name";
                    ShowSevereError(cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName + " not found: " + thisDOAS.AirLoopMixerName);
                    errorsFound = true;
                }
                AirLoopMixer thisAirLoopMixer;
                thisDOAS.m_CompPointerAirLoopMixer = thisAirLoopMixer.factory(thisDOAS.m_AirLoopMixerIndex, thisDOAS.AirLoopMixerName);
                thisDOAS.AirLoopSplitterName = UtilityRoutines::MakeUPPERCase(fields.at("airloophvac_splitter_name")); //
                thisDOAS.m_AirLoopSplitterIndex = getAirLoopSplitterIndex(thisDOAS.AirLoopSplitterName);
                if (thisDOAS.m_AirLoopSplitterIndex < 0) {
                    cFieldName = "AirLoopHVAC:Splitter Name";
                    ShowSevereError(cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName +
                                    " not found: " + thisDOAS.AirLoopSplitterName);
                    errorsFound = true;
                }
                AirLoopSplitter thisAirLoopSplitter;
                thisDOAS.m_CompPointerAirLoopSplitter = thisAirLoopSplitter.factory(thisDOAS.m_AirLoopSplitterIndex, thisDOAS.AirLoopSplitterName);

                // get pretreated desing conditions
                thisDOAS.PreheatTemp = fields.at("preheat_design_temperature");
                thisDOAS.PreheatHumRat = fields.at("preheat_design_humidity_ratio");
                thisDOAS.PrecoolTemp = fields.at("precool_design_temperature");
                thisDOAS.PrecoolHumRat = fields.at("precool_design_humidity_ratio");

                // get info on AirLoops
                thisDOAS.NumOfAirLoops = fields.at("number_of_airloophvac"); //
                if (thisDOAS.NumOfAirLoops < 1) {
                    cFieldName = "Number of AirLoopHVAC";
                    ShowSevereError(cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName + " = " +
                                    General::TrimSigDigits(thisDOAS.NumOfAirLoops));
                    ShowContinueError(" The minimum value should be 1.");
                    errorsFound = true;
                }

                auto AirLoopNames = fields.find("airloophvacs");
                if (AirLoopNames != fields.end()) {
                    auto AirLoopArray = AirLoopNames.value();
                    int num = 0;
                    for (auto AirLoopHAVCName : AirLoopArray) {
                        std::string name = AirLoopHAVCName.at("airloophvac_name");
                        int LoopNum = UtilityRoutines::FindItemInList(name, PrimaryAirSystem);
                        num += 1;
                        if (LoopNum > 0 && num <= thisDOAS.NumOfAirLoops) {
                            thisDOAS.AirLoopName.push_back(name);
                            thisDOAS.m_AirLoopNum.push_back(LoopNum);
                        } else {
                            cFieldName = "AirLoopHVAC Name";
                            ShowSevereError(cCurrentModuleObject + ", \"" + thisDOAS.Name + "\" " + cFieldName +
                                            " not found: " + name);
                            errorsFound = true;
                        }
                    }
                }

                thisDOAS.m_AirLoopDOASNum = AirLoopDOASNum - 1;
                airloopDOAS.push_back(thisDOAS);
            }

            // Check valid OA controller
            for (int OASysNum = 1; OASysNum <= DataAirLoop::NumOASystems; OASysNum++) {
                if (UtilityRoutines::SameString(OutsideAirSys(OASysNum).ControllerListName, "")) {
                    if (OutsideAirSys(OASysNum).AirLoopDOASNum == -1) {
                        ShowSevereError("AirLoopHVAC:OutdoorAirSystem = \"" + OutsideAirSys(OASysNum).Name +
                                        "\" invalid Controller List Name = \" not found.");
                        errorsFound = true;
                    }
                }
            }
            if (errorsFound) {
                ShowFatalError("getAirLoopHVACDOAS: Previous errors cause termination.");
            }
        }
    }

    void AirLoopDOAS::initAirLoopDOAS(bool const FirstHVACIteration)
    {
        int LoopOA;
        int NodeNum;
        Real64 SchAvailValue;
        static Array1D_bool MyEnvrnFlag; // Used for initializations each begin environment flag
        static bool MyOneTimeFlag(true); // Initialization flag
        std::string RoutineName = "AirLoopDOAS::initAirLoopDOAS";
        bool ErrorsFound = false;

        if (MyOneTimeFlag) {
            MyEnvrnFlag.allocate(numAirLoopDOAS);
            MyEnvrnFlag = true;
            MyOneTimeFlag = false;
        }

        if (DataGlobals::BeginEnvrnFlag && MyEnvrnFlag(this->m_AirLoopDOASNum + 1)) {
            Real64 rho;
            DataSizing::CurSysNum = this->m_OASystemNum;
            for (int CompNum = 1; CompNum <= DataAirLoop::OutsideAirSys(this->m_OASystemNum).NumComponents; ++CompNum) {
                std::string CompType = DataAirLoop::OutsideAirSys(this->m_OASystemNum).ComponentType(CompNum);
                std::string CompName = DataAirLoop::OutsideAirSys(this->m_OASystemNum).ComponentName(CompNum);
                if (UtilityRoutines::SameString(CompType, "FAN:SYSTEMMODEL")) {
                    HVACFan::fanObjs[this->m_FanIndex]->simulate();
                }
                if (UtilityRoutines::SameString(CompType, "FAN:COMPONENTMODEL")) {
                    Fans::SimulateFanComponents(CompName, FirstHVACIteration, this->m_FanIndex);
                }

                if (UtilityRoutines::SameString(CompType, "COIL:HEATING:WATER")) {
                    WaterCoils::SimulateWaterCoilComponents(CompName, FirstHVACIteration, this->m_HeatCoilNum);
                    Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", CompName, ErrorsFound);
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->HWLoopNum).FluidName,
                                                            DataGlobals::HWInitConvTemp,
                                                            DataPlant::PlantLoop(this->HWLoopNum).FluidIndex,
                                                            RoutineName);
                    PlantUtilities::InitComponentNodes(0.0,
                                                       CoilMaxVolFlowRate * rho,
                                                       this->HWCtrlNodeNum,
                                                       DataAirLoop::OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum),
                                                       this->HWLoopNum,
                                                       this->HWLoopSide,
                                                       this->HWBranchNum,
                                                       this->HWCompNum);
                }
                if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER")) {
                    WaterCoils::SimulateWaterCoilComponents(CompName, FirstHVACIteration, this->m_CoolCoilNum);
                    Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Cooling:Water", CompName, ErrorsFound);
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                            DataGlobals::CWInitConvTemp,
                                                            DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);
                    PlantUtilities::InitComponentNodes(0.0,
                                                       CoilMaxVolFlowRate * rho,
                                                       this->CWCtrlNodeNum,
                                                       DataAirLoop::OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum),
                                                       this->CWLoopNum,
                                                       this->CWLoopSide,
                                                       this->CWBranchNum,
                                                       this->CWCompNum);
                }
                if (UtilityRoutines::SameString(CompType, "COIL:COOLING:WATER:DETAILEDGEOMETRY")) {
                    WaterCoils::SimulateWaterCoilComponents(CompName, FirstHVACIteration, this->m_CoolCoilNum);
                    Real64 CoilMaxVolFlowRate = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Cooling:Water:DetailedGeometry", CompName, ErrorsFound);
                    rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(this->CWLoopNum).FluidName,
                                                            DataGlobals::CWInitConvTemp,
                                                            DataPlant::PlantLoop(this->CWLoopNum).FluidIndex,
                                                            RoutineName);
                    PlantUtilities::InitComponentNodes(0.0,
                                                       CoilMaxVolFlowRate * rho,
                                                       this->CWCtrlNodeNum,
                                                       DataAirLoop::OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum),
                                                       this->CWLoopNum,
                                                       this->CWLoopSide,
                                                       this->CWBranchNum,
                                                       this->CWCompNum);
                }
            }

            MyEnvrnFlag(this->m_AirLoopDOASNum + 1) = false;
            if (ErrorsFound) {
                ShowFatalError("initAirLoopDOAS: Previous errors cause termination.");
            }
        }

        if (!DataGlobals::BeginEnvrnFlag) {
            MyEnvrnFlag(this->m_AirLoopDOASNum + 1) = true;
        }

        this->SumMassFlowRate = 0.0;

        for (LoopOA = 0; LoopOA < this->m_CompPointerAirLoopSplitter->numOfOutletNodes; LoopOA++) {
            NodeNum = this->m_CompPointerAirLoopSplitter->OutletNodeNum[LoopOA];
            this->SumMassFlowRate += DataLoopNode::Node(NodeNum).MassFlowRate;
        }

        SchAvailValue = ScheduleManager::GetCurrentScheduleValue(this->m_AvailManagerSchedPtr);
        if (SchAvailValue < 1.0) {
            this->SumMassFlowRate = 0.0;
        }
        DataLoopNode::Node(this->m_InletNodeNum).MassFlowRate = this->SumMassFlowRate;

        if (this->SumMassFlowRate == 0.0) {
            for (int CompNum = 1; CompNum <= DataAirLoop::OutsideAirSys(this->m_OASystemNum).NumComponents; CompNum++) {
                DataLoopNode::Node(DataAirLoop::OutsideAirSys(this->m_OASystemNum).OutletNodeNum(CompNum)) = DataLoopNode::Node(this->m_InletNodeNum);
            }
        }
    }

    void AirLoopDOAS::CalcAirLoopDOAS(bool const FirstHVACIteration)
    {
        using DataAirLoop::OutsideAirSys;
        using MixedAir::ManageOutsideAirSystem;

        this->m_CompPointerAirLoopMixer->CalcAirLoopMixer();
        if (this->m_FanIndex > -1) {
            DataLoopNode::Node(this->m_FanInletNodeNum).MassFlowRateMaxAvail = this->SumMassFlowRate;
            DataLoopNode::Node(this->m_FanOutletNodeNum).MassFlowRateMaxAvail = this->SumMassFlowRate;
            DataLoopNode::Node(this->m_FanOutletNodeNum).MassFlowRateMax = this->SumMassFlowRate;
            if (!this->FanBlowTroughFlag) {
                DataLoopNode::Node(OutsideAirSys(this->m_OASystemNum).InletNodeNum(1)).MassFlowRateMaxAvail = this->SumMassFlowRate;
            }
        }
        ManageOutsideAirSystem(this->OASystemName, FirstHVACIteration, 0, this->m_OASystemNum);
        Real64 Temp = DataLoopNode::Node(this->m_OutletNodeNum).Temp;
        Real64 HumRat = DataLoopNode::Node(this->m_OutletNodeNum).HumRat;
        DataLoopNode::Node(this->m_OutletNodeNum).Enthalpy = Psychrometrics::PsyHFnTdbW(Temp, HumRat);

        this->m_CompPointerAirLoopSplitter->CalcAirLoopSplitter(Temp, HumRat);
    }

    void AirLoopDOAS::SizingAirLoopDOAS()
    {
        Real64 SizingMassFlow = 0;
        int AirLoopNum;

        for (int AirLoop = 1; AirLoop <= this->NumOfAirLoops; AirLoop++) {
            AirLoopNum = this->m_AirLoopNum[AirLoop - 1];
            this->m_OACtrlNum.push_back(DataAirLoop::AirLoopControlInfo(AirLoopNum).OACtrlNum);

            if (this->m_OACtrlNum[AirLoop - 1] > 0) {
                SizingMassFlow += MixedAir::OAController(this->m_OACtrlNum[AirLoop - 1]).MaxOA;
            }
        }
        this->SizingMassFlow = SizingMassFlow;
        this->GetDesignDayConditions();

        if (this->m_FanIndex > -1 && this->m_FanTypeNum == SimAirServingZones::Fan_System_Object) {
            HVACFan::fanObjs[this->m_FanIndex]->designAirVolFlowRate = SizingMassFlow / DataEnvironment::StdRhoAir;
            DataLoopNode::Node(this->m_FanInletNodeNum).MassFlowRateMaxAvail = SizingMassFlow;
            DataLoopNode::Node(this->m_FanOutletNodeNum).MassFlowRateMaxAvail = SizingMassFlow;
            DataLoopNode::Node(this->m_FanOutletNodeNum).MassFlowRateMax = SizingMassFlow;
        }
        bool errorsFound = false;
        if (this->m_FanIndex > 0 && this->m_FanTypeNum == SimAirServingZones::Fan_ComponentModel) {
            Fans::SetFanData(this->m_FanIndex, errorsFound, Name, SizingMassFlow / DataEnvironment::StdRhoAir, 0);
            Fans::Fan(this->m_FanIndex).MaxAirMassFlowRate = SizingMassFlow;
            DataLoopNode::Node(this->m_FanInletNodeNum).MassFlowRateMaxAvail = SizingMassFlow;
            DataLoopNode::Node(this->m_FanOutletNodeNum).MassFlowRateMaxAvail = SizingMassFlow;
            DataLoopNode::Node(this->m_FanOutletNodeNum).MassFlowRateMax = SizingMassFlow;
        }
        if (errorsFound) {
            ShowFatalError("Preceding sizing errors cause program termination");
        }
        DataSizing::CurSysNum = DataHVACGlobals::NumPrimaryAirSys + this->m_AirLoopDOASNum + 1;
        DataSizing::CurOASysNum = this->m_OASystemNum;
    }

    void getAirLoopHVACDOASInput()
    {
        if (GetInputOnceFlag) {
            AirLoopDOAS::getAirLoopDOASInput();
            GetInputOnceFlag = false;
        }
    }

    void AirLoopDOAS::GetDesignDayConditions()
    {
        // Using/Aliasing
        using WeatherManager::DesDayInput;

        int const summerDesignDayTypeIndex(9);
        int const winterDesignDayTypeIndex(10);
        bool static SummerDesignDayFlag = true;
        bool static WinterDesignDayFlag = true;

        for (size_t i = 1; i <= DesDayInput.size(); i++) {
            // Summer design day
            if (DesDayInput(i).DayType == summerDesignDayTypeIndex && SummerDesignDayFlag) {
                this->SizingCoolOATemp = DesDayInput(i).MaxDryBulb;
                if (DesDayInput(i).HumIndType == WeatherManager::DDHumIndType_WetBulb) { // wet bulb temperature
                    this->SizingCoolOAHumRat =
                        Psychrometrics::PsyWFnTdbTwbPb(this->SizingCoolOATemp, DesDayInput(i).HumIndValue, DataEnvironment::StdPressureSeaLevel);
                } else if (DesDayInput(i).HumIndType == WeatherManager::DDHumIndType_DewPoint) { // dewpoint
                    this->SizingCoolOAHumRat = Psychrometrics::PsyWFnTdpPb(DesDayInput(i).HumIndValue, DataEnvironment::StdPressureSeaLevel);
                } else if (DesDayInput(i).HumIndType == WeatherManager::DDHumIndType_HumRatio) {
                    this->SizingCoolOAHumRat = DesDayInput(i).HumIndValue;
                } // else { // What about other cases?
                SummerDesignDayFlag = false;
            }
            // Winter design day
            if (DesDayInput(i).DayType == winterDesignDayTypeIndex && WinterDesignDayFlag) {
                this->HeatOutTemp = DesDayInput(i).MaxDryBulb;
                if (DesDayInput(i).HumIndType == WeatherManager::DDHumIndType_WetBulb) { // wet bulb temperature
                    this->HeatOutHumRat =
                        Psychrometrics::PsyWFnTdbTwbPb(this->HeatOutTemp, DesDayInput(i).HumIndValue, DataEnvironment::StdPressureSeaLevel);
                } else if (DesDayInput(i).HumIndType == WeatherManager::DDHumIndType_DewPoint) { // dewpoint
                    this->HeatOutHumRat = Psychrometrics::PsyWFnTdpPb(DesDayInput(i).HumIndValue, DataEnvironment::StdPressureSeaLevel);
                } else if (DesDayInput(i).HumIndType == WeatherManager::DDHumIndType_HumRatio) {
                    this->HeatOutHumRat = DesDayInput(i).HumIndValue;
                } // else { // What about other cases?
                WinterDesignDayFlag = false;
            }
        }
    }

    void CheckConvergence()
    {

        Real64 maxDiff;
        Real64 Diff;
        Real64 OldTemp;
        for (std::size_t loop = 0; loop < airloopDOAS.size(); ++loop) {
            maxDiff = 0.0;
            Diff = std::abs(airloopDOAS[loop].m_CompPointerAirLoopSplitter->InletTemp -
                            DataLoopNode::Node(airloopDOAS[loop].m_CompPointerAirLoopSplitter->OutletNodeNum[0]).Temp);
            if (Diff > maxDiff) {
                maxDiff = Diff;
            }
            if (airloopDOAS[loop].m_HeatExchangerFlag) {
                OldTemp = airloopDOAS[loop].m_CompPointerAirLoopMixer->OutletTemp;
                airloopDOAS[loop].m_CompPointerAirLoopMixer->CalcAirLoopMixer();
                Diff = std::abs(OldTemp - airloopDOAS[loop].m_CompPointerAirLoopMixer->OutletTemp);
                if (Diff > maxDiff) {
                    maxDiff = Diff;
                }
            }
            if (maxDiff > 1.0e-6) {
                if (airloopDOAS[loop].ConveCount == 0) {
                    ++airloopDOAS[loop].ConveCount;
                    ShowWarningError("Convergence limit is above 1.0e-6 for unit=" + airloopDOAS[loop].Name);
                    ShowContinueErrorTimeStamp("The max difference of node temperatures between AirLoopDOAS outlet and OA mixer inlet =" +
                                               General::RoundSigDigits(maxDiff, 6));
                } else {
                    ++airloopDOAS[loop].ConveCount;
                    ShowRecurringWarningErrorAtEnd(airloopDOAS[loop].Name +
                                                       "\": The max difference of node temperatures exceeding 1.0e-6  continues...",
                                                   airloopDOAS[loop].ConveIndex,
                                                   maxDiff,
                                                   maxDiff);
                }
            }
        }
    }

} // namespace AirLoopHVACDOAS
} // namespace EnergyPlus
