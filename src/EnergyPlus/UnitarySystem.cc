// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

#include <UnitarySystem.hh>
#include <DataHeatBalance.hh>
//#include <DataIPShortCuts.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <ScheduleManager.hh>
#include <SimAirServingZones.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {
namespace UnitarySystems {

    bool myOneTimeFlag(true);
    bool getInputOnceFlag(true);
    std::vector<UnitarySys> unitarySys;

    UnitarySys::UnitarySys() // constructor
        : TypeOfNum(0), availSchedIndex(0), controlType(controlTypeEnum::controlTypeNone),
        controlZoneIndex(0), dehumidificationControl(0), inletNodeNum(0), outletNodeNum(0),
        validASHRAECoolCoil(false), validASHRAEHeatCoil(false)
    {}

    void UnitarySys::simulate(std::string const & objectName, bool const firstHVACIteration, int const & AirLoopNum, int & CompIndex, bool & HeatingActive, bool & CoolingActive)
    {
        UnitarySys::init(firstHVACIteration);
    }

    UnitarySys * UnitarySys::factory(int object_type_of_num, std::string const objectName)
    {
        if (getInputOnceFlag) {
            UnitarySys::getInput();
            getInputOnceFlag = false;
        }
        for (auto &sys : unitarySys) {
            if (sys.name == objectName && sys.TypeOfNum == object_type_of_num) {
                return &sys;
            }
        }
        ShowFatalError("UnitarySystem factory: Error getting inputs for system named: " + objectName);
        return nullptr;

    }

    void UnitarySys::init(bool const firstHVACIteration)
    {

        if (myOneTimeFlag) {
            // initialize or allocate something once
            myOneTimeFlag = false;
        }

    }

    void UnitarySys::getInput()
    {

        bool errorsFound(false);

        UnitarySys::getInputData(errorsFound);

        if (errorsFound) {
            // show fatal warning
        }
    }

    void UnitarySys::getInputData(bool errorsFound)
    {
        //using namespace DataIPShortCuts;

        errorsFound = false;

        std::string cCurrentModuleObject = "UnitarySystemPerformance:Multispeed";

        cCurrentModuleObject = "UnitarySystem";
        int numUnitarySys = inputProcessor->getNumObjectsFound(cCurrentModuleObject);

        if (numUnitarySys > 0) {
            auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
            if (instances == inputProcessor->epJSON.end()) {
                errorsFound = true;
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                UnitarySys thisSys;

                std::string availSch("");
                if (fields.find("availability_schedule_name") != fields.end()) { // not required field
                    availSch = UtilityRoutines::MakeUPPERCase(fields.at("availability_schedule_name"));
                }
                std::string ctrlType = fields.at("control_type");
                std::string ctrlZoneName("");
                if (fields.find("controlling_zone_or_thermostat_location") != fields.end()) { // not required field
                    ctrlZoneName = UtilityRoutines::MakeUPPERCase(fields.at("controlling_zone_or_thermostat_location"));
                } else if (ctrlType == "Load") {
                    ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                    ShowContinueError("Controlling Zone or Thermostat Location cannot be blank when Control Type = Load");
                    errorsFound = true;
                }
                std::string dehumCtrlType("");
                if (fields.find("dehumidification_control_type") != fields.end()) { // not required field, has default
                    dehumCtrlType = fields.at("dehumidification_control_type");
                } else {
                    dehumCtrlType = "None"; // default value
                }
                std::string airInNodeName = fields.at("air_inlet_node_name");
                std::string airOutNodeName = fields.at("air_outlet_node_name");

                thisSys.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisSys.TypeOfNum = SimAirServingZones::UnitarySystemModel;
                thisSys.availSchedIndex = ScheduleManager::GetScheduleIndex(availSch);
                if (UtilityRoutines::SameString(ctrlType, "Load")) {
                    thisSys.controlType = controlTypeEnum::controlTypeLoad;
                } else if (UtilityRoutines::SameString(ctrlType, "SetPoint")) {
                    thisSys.controlType = controlTypeEnum::controlTypeSetpoint;
                } else if (UtilityRoutines::SameString(ctrlType, "SingleZoneVAV")) {
                    thisSys.controlType = controlTypeEnum::controlTypeCCMASHRAE;
                    thisSys.validASHRAECoolCoil = true;
                    thisSys.validASHRAEHeatCoil = true;
                } else {
                    ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                    ShowContinueError("Invalid Control Type = " + ctrlType);
                    errorsFound = true;
                }
                if (ctrlZoneName != "") thisSys.controlZoneIndex = UtilityRoutines::FindItemInList(ctrlZoneName, DataHeatBalance::Zone);
                if (UtilityRoutines::SameString(dehumCtrlType, "None")) {
                    thisSys.dehumidificationControl = 0; // DehumidControl_None;
                } else if (UtilityRoutines::SameString(dehumCtrlType, "CoolReheat")) {
                    thisSys.dehumidificationControl = 1; // DehumidControl_CoolReheat;
                } else if (UtilityRoutines::SameString(dehumCtrlType, "Multimode")) {
                    thisSys.dehumidificationControl = 2; // DehumidControl_Multimode;
                }

                thisSys.inletNodeNum = NodeInputManager::GetOnlySingleNode(airInNodeName,
                    errorsFound,
                    cCurrentModuleObject,
                    thisSys.name,
                    DataLoopNode::NodeType_Air,
                    DataLoopNode::NodeConnectionType_Inlet,
                    1,
                    DataLoopNode::ObjectIsParent);
                thisSys.outletNodeNum = NodeInputManager::GetOnlySingleNode(airOutNodeName,
                    errorsFound,
                    cCurrentModuleObject,
                    thisSys.name,
                    DataLoopNode::NodeType_Air,
                    DataLoopNode::NodeConnectionType_Outlet,
                    1,
                    DataLoopNode::ObjectIsParent);

                unitarySys.push_back(thisSys);
            }
        }
    }

} // namespace UnitarySystems
} // namespace EnergyPlus
