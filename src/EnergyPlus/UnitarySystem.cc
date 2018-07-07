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

#include <BranchNodeConnections.hh>
#include <DXCoils.hh>
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataHVACControllers.hh>
#include <DataHeatBalance.hh>
#include <DataSizing.hh>
#include <DataZoneControls.hh>
#include <DataZoneEquipment.hh>
#include <Fans.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HVACFan.hh>
#include <HVACHXAssistedCoolingCoil.hh>
#include <HeatingCoils.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <PackagedThermalStorageCoil.hh>
#include <ReportCoilSelection.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <SimAirServingZones.hh>
#include <SingleDuct.hh>
#include <SteamCoils.hh>
#include <UnitarySystem.hh>
#include <UserDefinedComponents.hh>
#include <UtilityRoutines.hh>
#include <VariableSpeedCoils.hh>
#include <WaterCoils.hh>
#include <WaterToAirHeatPumpSimple.hh>
#include <string> // std::string, std::to_string

namespace EnergyPlus {
namespace UnitarySystems {

    int numUnitarySystems(0);
    bool myOneTimeFlag(true);
    bool getInputFlag(true);
    bool getInputOnceFlag(true);
    bool getMSHPInputOnceFlag(true);
    std::vector<UnitarySys> unitarySys;
    std::vector<DesignSpecMSHP> designSpecMSHP;
    static std::string const fluidNameSteam("STEAM");

    // Supply Air Sizing Option
    int const None(1);
    int const SupplyAirFlowRate(2);
    int const FlowPerFloorArea(3);
    int const FractionOfAutoSizedCoolingValue(4);
    int const FractionOfAutoSizedHeatingValue(5);
    int const FlowPerCoolingCapacity(6);
    int const FlowPerHeatingCapacity(7);

    DesignSpecMSHP::DesignSpecMSHP()
        : designSpecMSHPType_Num(0), noLoadAirFlowRateRatio(0.0), numOfSpeedHeating(0), numOfSpeedCooling(0), singleModeFlag(false)
    {
    }

    UnitarySys::UnitarySys() // constructor
        : unitarySystemType_Num(0), sysAvailSchedPtr(0), controlType(controlTypeEnum::controlTypeNone), controlZoneNum(0),
          dehumidControlType_Num(dehumCtrlTypeEnum::dehumidControl_None), humidistat(false), airInNode(0), airOutNode(0), validASHRAECoolCoil(false),
          validASHRAEHeatCoil(false), fanIndex(0), fanPlace(fanPlaceEnum::notYetSet), fanOpModeSchedPtr(0), fanExists(false), fanType_Num(0),
          requestAutoSize(false), actualFanVolFlowRate(0.0), designFanVolFlowRate(0.0), designMassFlowRate(0.0), fanAvailSchedPtr(0),
          fanOpMode(fanOpModeEnum::fanOpModeNotYetSet), ATMixerIndex(0), ATMixerType(0), ATMixerPriNode(0), ATMixerSecNode(0), ATMixerOutNode(0),
          ATMixerExists(false), nodeNumOfControlledZone(0), airLoopEquipment(false), zoneInletNode(0), zoneSequenceCoolingNum(0),
          zoneSequenceHeatingNum(0), myGetInputSuccessfulFlag(false), heatCoilExists(false), heatingSizingRatio(0.0), heatingCoilType_Num(0),
          DXHeatingCoil(false), heatingCoilIndex(0), heatingCoilAvailSchPtr(0), designHeatingCapacity(0.0), maxHeatAirVolFlow(0.0),
          numOfSpeedHeating(0), heatCoilFluidInletNode(0), maxHeatCoilFluidFlow(0.0), multiSpeedHeatingCoil(false), varSpeedHeatingCoil(false),
          systemHeatControlNodeNum(0), heatCoilInletNodeNum(0), heatCoilOutletNodeNum(0), coolCoilExists(false), coolingCoilType_Num(0),
          numOfSpeedCooling(0), coolingCoilAvailSchPtr(0), designCoolingCapacity(0.0), maxCoolAirVolFlow(0.0), condenserNodeNum(0),
          coolingCoilIndex(0), heatPump(false), actualDXCoilIndexForHXAssisted(0), maxCoolCoilFluidFlow(0.0), coolCoilFluidInletNode(0),
          multiSpeedCoolingCoil(false), varSpeedCoolingCoil(false), systemCoolControlNodeNum(0), coolCoilInletNodeNum(0), coolCoilOutletNodeNum(0),
          waterCyclingMode(0), ISHundredPercentDOASDXCoil(false), designMinOutletTemp(0.0), runOnSensibleLoad(false), runOnLatentLoad(false),
          runOnLatentOnlyWithSensible(false), suppHeatCoilType_Num(0), suppCoilExists(0), designSuppHeatingCapacity(0.0), suppCoilAirInletNode(0),
          suppCoilAirOutletNode(0), suppCoilFluidInletNode(0), maxSuppCoilFluidFlow(0.0), suppHeatCoilIndex(0), suppHeatControlNodeNum(0),
          coolingSAFMethod(0), heatingSAFMethod(0), noCoolHeatSAFMethod(0), maxNoCoolHeatAirVolFlow(0.0), airFlowControl(useCompFlow::flowNotYetSet),
          coolingCoilUpstream(false), designMaxOutletTemp(0.0), maxOATSuppHeat(0.0), minOATCompressorCooling(0.0), minOATCompressorHeating(0.0),
          maxONOFFCyclesperHour(0.0), HPTimeConstant(0.0), onCyclePowerFraction(0.0), fanDelayTime(0.0), ancillaryOnPower(0.0),
          ancillaryOffPower(0.0), designHRWaterVolumeFlow(0.0), maxHROutletWaterTemp(0.0), heatRecActive(false), heatRecoveryInletNodeNum(0),
          heatRecoveryOutletNodeNum(0), noLoadAirFlowRateRatio(0.0), singleMode(0), multiOrVarSpeedHeatCoil(false), multiOrVarSpeedCoolCoil(false),
          coolingPartLoadFrac(0.0), heatingPartLoadFrac(0.0), suppHeatPartLoadFrac(0.0), heatCompPartLoadRatio(0.0), speedRatio(0.0), cycRatio(0.0)
    {
    }

    void UnitarySys::simulate(std::string const &unitarySystemName,
                              bool const FirstHVACIteration,
                              int const &AirLoopNum,
                              int &CompIndex,
                              bool &HeatActive,
                              bool &CoolActive,
                              int const OAUnitNum,
                              Real64 const OAUCoilOutTemp,
                              bool const ZoneEquipment)
    {
        int CompOn = 0;
        int unitarySysNum(-1);

        // Obtains and Allocates unitary system related parameters from input file
        if (UnitarySystems::getInputFlag) {
            // Get the unitary system input
            getUnitarySystemInput();
            UnitarySystems::getInputFlag = false;
        }

        // Find the correct unitary system Number
        if (CompIndex == 0) {
            // unitarySysNum = UtilityRoutines::FindItemInList(unitarySystemName, UnitarySystems::UnitarySys);
            unitarySysNum = getUnitarySystemIndex(unitarySystemName);
            if (unitarySysNum == -1) { // zero based index
                ShowFatalError("SimUnitarySystem: Unitary System not found=" + unitarySystemName);
            }
            CompIndex = unitarySysNum;
        } else {
            unitarySysNum = CompIndex;
            // zero based index
            if (unitarySysNum > (numUnitarySystems-1) || unitarySysNum < 0) {
                ShowFatalError("SimUnitarySystem:  Invalid CompIndex passed=" + General::TrimSigDigits(unitarySysNum) + ", Number of Unit Systems=" +
                               General::TrimSigDigits(numUnitarySystems) + ", Unitary System name=" + unitarySystemName);
            }
            // if (CheckEquipName(unitarySysNum)) {
            //    if (UnitarySystemName != UnitarySystem(UnitarySysNum).Name) {
            //        ShowFatalError("SimUnitarySystem: Invalid CompIndex passed=" + TrimSigDigits(UnitarySysNum) + ", Unitary System name=" +
            //                       UnitarySystemName + ", stored Unit Name for that index=" + UnitarySystem(UnitarySysNum).Name);
            //    }
            //    CheckEquipName(UnitarySysNum) = false;
            //}
        }

        if (!this->myGetInputSuccessfulFlag) {
            // Need to do inputs again for this and any others
            UnitarySystems::getInputFlag = true;
            this->getUnitarySystemInput();
            UnitarySystems::getInputFlag = false;
        }

        // if (present(HeatActive)) HeatActive = false;
        // if (present(CoolActive)) CoolActive = false;
        HeatActive = false;
        CoolActive = false;

        fanSpeedRatio = 1.0;
        // if (present(ZoneEquipment)) {
        if (ZoneEquipment) {
            this->initUnitarySystems(unitarySysNum, 0, FirstHVACIteration, OAUnitNum, OAUCoilOutTemp);
        } else {
            this->initUnitarySystems(unitarySysNum, AirLoopNum, FirstHVACIteration, OAUnitNum, OAUCoilOutTemp);
        }

        // MassFlowRateMaxAvail issues are impeding non-VAV air loop equipment by limiting air flow
        // temporarily open up flow limits while simulating, and then set this same value at the INLET after this parent has simulated
        Real64 tempMassFlowRateMaxAvail = DataLoopNode::Node(this->airInNode).MassFlowRateMaxAvail;
        DataLoopNode::Node(this->airInNode).MassFlowRateMaxAvail = this->designMassFlowRate;

        bool HXUnitOn = false;
        {
            auto const SELECT_CASE_var(this->controlType);
            if (SELECT_CASE_var == controlTypeEnum::controlTypeSetpoint) {
                // if (present(ZoneEquipment)) {
                if (ZoneEquipment) {
                    // ControlUnitarySystemtoSP(UnitarySysNum, 0, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                } else {
                    // ControlUnitarySystemtoSP(UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                }
            } else if (SELECT_CASE_var == controlTypeEnum::controlTypeLoad || SELECT_CASE_var == controlTypeEnum::controlTypeCCMASHRAE) {
                // if (present(ZoneEquipment)) {
                if (ZoneEquipment) {
                    // ControlUnitarySystemtoLoad(UnitarySysNum, 0, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                } else {
                    // ControlUnitarySystemtoLoad(UnitarySysNum, AirLoopNum, FirstHVACIteration, CompOn, OAUCoilOutTemp, HXUnitOn);
                }
            }
        }

        // Report the current output
        // if (present(ZoneEquipment)) {
        if (ZoneEquipment) {
            // ReportUnitarySystem(UnitarySysNum, 0);
        } else {
            // ReportUnitarySystem(UnitarySysNum, AirLoopNum);
        }

        // if (present(CoolActive)) {
        CoolActive = false;
        if (this->coolingPartLoadFrac * double(CompOn) > 0.0) CoolActive = true;
        //}
        // if (present(HeatActive)) {
        HeatActive = false;
        if (this->heatingPartLoadFrac * double(CompOn) > 0.0 || this->suppHeatPartLoadFrac * double(CompOn) > 0.0) HeatActive = true;
        //}

        // set econo lockout flag
        // If the sysem is not an equipment of Outdoor air unit
        //  IF (AirLoopNum /=-1 .AND. ALLOCATED(AirLoopControlInfo) .AND. UnitarySystem(UnitarySysNum)%AirLoopEquipment) THEN
        if (AirLoopNum > 0 && allocated(DataAirLoop::AirLoopControlInfo) && this->airLoopEquipment) {

            if ((this->heatCompPartLoadRatio > 0.0 || this->speedRatio > 0.0 || this->cycRatio > 0.0) &&
                DataAirLoop::AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor) {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = true;
            } else {
                DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithCompressor = false;
            }

            //if (present(HeatActive)) {
                if ((HeatActive) && (DataAirLoop::AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithCompressor ||
                                     DataAirLoop::AirLoopControlInfo(AirLoopNum).CanLockoutEconoWithHeating)) {
                    DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = true;
                } else {
                    DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = false;
                }
            //} else {
            //    DataAirLoop::AirLoopControlInfo(AirLoopNum).ReqstEconoLockoutWithHeating = false;
            //}
        }

        // Calculate heat recovery
        if (this->heatRecActive) {
            // UnitarySystemHeatRecovery(UnitarySysNum);
        }

        // Coils should have been sized by now. Set this flag to false in case other equipment is downstream of Unitary System.
        // can't do this since there are other checks that need this flag (e.g., HVACManager, SetHeatToReturnAirFlag())
        //  AirLoopControlInfo(AirLoopNum)%UnitarySys = .FALSE.

        DataLoopNode::Node(this->airInNode).MassFlowRateMaxAvail = tempMassFlowRateMaxAvail;
    }

    DesignSpecMSHP *DesignSpecMSHP::factory(int object_type_of_num, std::string const objectName)
    {

        if (getMSHPInputOnceFlag) {
            DesignSpecMSHP::getDesignSpecMSHP();
            getMSHPInputOnceFlag = false;
        }
        for (auto &dSpec : designSpecMSHP) {
            if (UtilityRoutines::SameString(dSpec.name, objectName) && dSpec.designSpecMSHPType_Num == object_type_of_num) {
                return &dSpec;
            }
        }
        ShowSevereError("Design Specification MultiSpeed Heat Pump factory: Error getting inputs for system named: " + objectName);
        return nullptr;
    }

    void DesignSpecMSHP::getDesignSpecMSHP()
    {
        bool errorsFound(false);

        DesignSpecMSHP::getDesignSpecMSHPdata(errorsFound);

        if (errorsFound) {
            // show fatal warning
        }
    }

    void DesignSpecMSHP::getDesignSpecMSHPdata(bool errorsFound)
    {
        std::string cCurrentModuleObject = "UnitarySystemPerformance:Multispeed";

        auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == inputProcessor->epJSON.end()) {
            errorsFound = true;
        } else {
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                DesignSpecMSHP thisDesignSpec;

                thisDesignSpec.name = thisObjectName;
                thisDesignSpec.numOfSpeedHeating = fields.at("number_of_speeds_for_heating"); // required field
                thisDesignSpec.numOfSpeedCooling = fields.at("number_of_speeds_for_cooling"); // required field
                int maxSpeeds = max(thisDesignSpec.numOfSpeedHeating, thisDesignSpec.numOfSpeedCooling);
                thisDesignSpec.designSpecMSHPType_Num = 1; // add global int value for factory

                std::string loc_singleModeOp("No");
                if (fields.find("single_mode_operation") != fields.end()) { // not required field
                    loc_singleModeOp = UtilityRoutines::MakeUPPERCase(fields.at("single_mode_operation"));
                }
                // set JSON data to design spec object
                if (UtilityRoutines::SameString(loc_singleModeOp, "Yes")) {
                    thisDesignSpec.singleModeFlag = true;
                } else if (UtilityRoutines::SameString(loc_singleModeOp, "No")) {
                    thisDesignSpec.singleModeFlag = false;
                } else {
                }

                Real64 loc_noLoadAirFlowRateRatio(1.0);
                if (fields.find("no_load_supply_air_flow_rate_ratio") != fields.end()) { // not required field
                    loc_noLoadAirFlowRateRatio = fields.at("no_load_supply_air_flow_rate_ratio");
                }
                thisDesignSpec.noLoadAirFlowRateRatio = loc_noLoadAirFlowRateRatio;

                thisDesignSpec.MSHeatingSpeedRatio.resize(maxSpeeds);
                thisDesignSpec.MSCoolingSpeedRatio.resize(maxSpeeds);

                auto speedFlowRatios = fields.find("flow_ratios");
                if (speedFlowRatios != fields.end()) {
                    auto flowRatioArray = speedFlowRatios.value();
                    int speedNum = -1;
                    for (auto flowRatio : flowRatioArray) {
                        speedNum += 1;
                        auto coolingSpeedRatioObject = flowRatio.at("cooling_speed_supply_air_flow_ratio");
                        if (coolingSpeedRatioObject == "Autosize") {
                            thisDesignSpec.MSCoolingSpeedRatio[speedNum] = -99999;
                        } else {
                            thisDesignSpec.MSCoolingSpeedRatio[speedNum] = coolingSpeedRatioObject;
                        }
                        auto heatingSpeedRatioObject = flowRatio.at("heating_speed_supply_air_flow_ratio");
                        if (heatingSpeedRatioObject == "Autosize") {
                            thisDesignSpec.MSHeatingSpeedRatio[speedNum] = -99999;
                        } else {
                            thisDesignSpec.MSHeatingSpeedRatio[speedNum] = heatingSpeedRatioObject;
                        }
                    }
                }
                designSpecMSHP.push_back(thisDesignSpec);
            }
        }
    }

    UnitarySys *UnitarySys::factory(int object_type_of_num, std::string const objectName)
    {
        if (UnitarySystems::getInputOnceFlag) {
            UnitarySys::getUnitarySystemInput();
            UnitarySystems::getInputOnceFlag = false;
        }
        for (auto &sys : unitarySys) {
            if (UtilityRoutines::SameString(sys.name, objectName) && sys.unitarySystemType_Num == object_type_of_num) {
                return &sys;
            }
        }
        ShowFatalError("UnitarySystem factory: Error getting inputs for system named: " + objectName);
        return nullptr;
    }

    int getDesignSpecMSHPIndex(       // lookup vector index for fan object name in object array EnergyPlus::HVACFan::fanObjs
        std::string const &objectName // IDF name in input
    )
    {
        int index = -1;
        bool found = false;
        for (std::size_t loop = 0; loop < designSpecMSHP.size(); ++loop) {
            DesignSpecMSHP *thisDesignSpecMSHPObjec = &designSpecMSHP[loop];
            if (UtilityRoutines::SameString(objectName, thisDesignSpecMSHPObjec->name)) {
                index = loop;
                found = true;
                break;
            }
        }
        if (!found) {
            ShowSevereError("getDesignSpecMSHPIndex: did not find UnitarySystemPerformance:Multispeed name =" + objectName + ". Check inputs");
        }
        return index;
    }

    int getUnitarySystemIndex(       // lookup vector index for UnitarySystem object name in object array EnergyPlus::UnitarySystems::unitarySys
        std::string const &objectName // IDF name in input
    )
    {
        int index = -1;
        bool found = false;
        for (std::size_t loop = 0; loop < unitarySys.size(); ++loop) {
            UnitarySys *thisUnitarySysObjec = &unitarySys[loop];
            if (UtilityRoutines::SameString(objectName, thisUnitarySysObjec->name)) {
                index = loop;
                found = true;
                break;
            }
        }
        return index;
    }

    void UnitarySys::initUnitarySystems(bool const firstHVACIteration,
                                        int const &AirLoopNum,
                                        bool const &FirstHVACIteration,
                                        Optional_int_const OAUnitNum,
                                        Optional<Real64 const> OAUCoilOutTemp)
    {

        if (myOneTimeFlag) {
            // initialize or allocate something once
            myOneTimeFlag = false;
        }
    }

    void UnitarySys::getUnitarySystemInput()
    {

        bool errorsFound(false);

        UnitarySys::getUnitarySystemInputData(errorsFound);

        if (errorsFound) {
            ShowSevereError("getUnitarySystemInputData: did not find UnitarySystem. Check inputs");
        }
    }

    void UnitarySys::getUnitarySystemInputData(bool errorsFound)
    {

        static std::string const getUnitarySystemInput("getUnitarySystemInputData");
        static std::string const unitarySysHeatPumpPerformanceObjectType("UnitarySystemPerformance:Multispeed");

        std::string cCurrentModuleObject = "UnitarySystem";

        auto const instances = inputProcessor->epJSON.find(cCurrentModuleObject);
        if (instances == inputProcessor->epJSON.end()) {
            ShowSevereError("getUnitarySystemInputData: did not find UnitarySystem object in input file. Check inputs");
            errorsFound = true;
        } else {
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                UnitarySys thisSys;

                if (getUnitarySystemIndex(thisObjectName) > -1) continue;

                ++numUnitarySystems;

                std::string loc_controlType = fields.at("control_type");
                std::string loc_controlZoneName("");
                if (fields.find("controlling_zone_or_thermostat_location") != fields.end()) { // not required field
                    loc_controlZoneName = UtilityRoutines::MakeUPPERCase(fields.at("controlling_zone_or_thermostat_location"));
                } else if (loc_controlType == "Load") {
                    ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                    ShowContinueError("Controlling Zone or Thermostat Location cannot be blank when Control Type = Load");
                    errorsFound = true;
                }
                std::string loc_dehumControlType("");
                if (fields.find("dehumidification_control_type") != fields.end()) { // not required field, has default
                    loc_dehumControlType = UtilityRoutines::MakeUPPERCase(fields.at("dehumidification_control_type"));
                } else {
                    loc_dehumControlType = "NONE"; // default value
                }
                std::string loc_sysAvailSched("");
                if (fields.find("availability_schedule_name") != fields.end()) { // not required field
                    loc_sysAvailSched = UtilityRoutines::MakeUPPERCase(fields.at("availability_schedule_name"));
                }
                std::string loc_airInNodeName = UtilityRoutines::MakeUPPERCase(fields.at("air_inlet_node_name"));   // required field
                std::string loc_airOutNodeName = UtilityRoutines::MakeUPPERCase(fields.at("air_outlet_node_name")); // required field

                std::string loc_fanType("");
                if (fields.find("supply_fan_object_type") != fields.end()) { // not required field
                    loc_fanType = UtilityRoutines::MakeUPPERCase(fields.at("supply_fan_object_type"));
                }

                std::string loc_fanName("");
                if (fields.find("supply_fan_name") != fields.end()) { // not required field
                    loc_fanName = UtilityRoutines::MakeUPPERCase(fields.at("supply_fan_name"));
                }

                std::string loc_supFanPlace("");
                if (fields.find("fan_placement") != fields.end()) { // not required field
                    loc_supFanPlace = UtilityRoutines::MakeUPPERCase(fields.at("fan_placement"));
                }

                std::string loc_supFanOpMode("");
                if (fields.find("supply_air_fan_operating_mode_schedule_name") != fields.end()) { // not required field
                    loc_supFanOpMode = UtilityRoutines::MakeUPPERCase(fields.at("supply_air_fan_operating_mode_schedule_name"));
                }

                std::string loc_heatingCoilType("");
                if (fields.find("heating_coil_object_type") != fields.end()) { // not required field
                    loc_heatingCoilType = UtilityRoutines::MakeUPPERCase(fields.at("heating_coil_object_type"));
                }

                std::string loc_heatingCoilName("");
                if (fields.find("heating_coil_name") != fields.end()) { // not required field
                    loc_heatingCoilName = UtilityRoutines::MakeUPPERCase(fields.at("heating_coil_name"));
                }

                Real64 loc_heatingSizingRatio(1.0);
                if (fields.find("dx_heating_coil_sizing_ratio") != fields.end()) { // not required field, has default
                    loc_heatingSizingRatio = fields.at("dx_heating_coil_sizing_ratio");
                }

                std::string loc_coolingCoilType("");
                if (fields.find("cooling_coil_object_type") != fields.end()) { // not required field
                    loc_coolingCoilType = UtilityRoutines::MakeUPPERCase(fields.at("cooling_coil_object_type"));
                }

                std::string loc_coolingCoilName("");
                if (fields.find("cooling_coil_name") != fields.end()) { // not required field
                    loc_coolingCoilName = UtilityRoutines::MakeUPPERCase(fields.at("cooling_coil_name"));
                }

                std::string loc_ISHundredPercentDOASDXCoil("No");
                if (fields.find("use_doas_dx_cooling_coil") != fields.end()) { // not required field, has default
                    loc_ISHundredPercentDOASDXCoil = UtilityRoutines::MakeUPPERCase(fields.at("use_doas_dx_cooling_coil"));
                }

                Real64 loc_designMinOutletTemp(2.0);
                if (fields.find("minimum_supply_air_temperature") != fields.end()) { // not required field, has default
                    loc_designMinOutletTemp = fields.at("minimum_supply_air_temperature");
                }

                std::string loc_latentControlFlag("SensibleOnlyLoadControl");
                if (fields.find("latent_load_control") != fields.end()) { // not required field, has default
                    loc_latentControlFlag = UtilityRoutines::MakeUPPERCase(fields.at("latent_load_control"));
                }

                std::string loc_suppHeatCoilType("");
                if (fields.find("supplemental_heating_coil_object_type") != fields.end()) { // not required field
                    loc_suppHeatCoilType = UtilityRoutines::MakeUPPERCase(fields.at("supplemental_heating_coil_object_type"));
                }

                std::string loc_suppHeatCoilName("");
                if (fields.find("supplemental_heating_coil_name") != fields.end()) { // not required field
                    loc_suppHeatCoilName = UtilityRoutines::MakeUPPERCase(fields.at("supplemental_heating_coil_name"));
                }

                std::string loc_coolingSAFMethod("");
                if (fields.find("cooling_supply_air_flow_rate_method") != fields.end()) { // not required field
                    loc_coolingSAFMethod = UtilityRoutines::MakeUPPERCase(fields.at("cooling_supply_air_flow_rate_method"));
                }

                Real64 loc_coolingSAFMethod_SAFlow(-999.0);
                if (fields.find("cooling_supply_air_flow_rate") != fields.end()) { // not required field, autosizable
                    std::string tempFieldVal = UtilityRoutines::MakeUPPERCase(fields.at("cooling_supply_air_flow_rate"));
                    if (UtilityRoutines::SameString(tempFieldVal, "AutoSize")) {
                        loc_coolingSAFMethod_SAFlow = DataSizing::AutoSize;
                    } else {
                        loc_coolingSAFMethod_SAFlow = fields.at("cooling_supply_air_flow_rate");
                    }
                }

                Real64 loc_coolingSAFMethod_SAFlowPerFloorArea(-999.0);
                if (fields.find("cooling_supply_air_flow_rate_per_floor_area") != fields.end()) { // not required field
                    loc_coolingSAFMethod_SAFlowPerFloorArea = fields.at("cooling_supply_air_flow_rate_per_floor_area");
                }

                Real64 loc_coolingSAFMethod_FracOfAutosizedCoolingSAFlow(-999.0);
                if (fields.find("cooling_fraction_of_autosized_cooling_supply_air_flow") != fields.end()) { // not required field
                    loc_coolingSAFMethod_FracOfAutosizedCoolingSAFlow = fields.at("cooling_fraction_of_autosized_cooling_supply_air_flow");
                }

                Real64 loc_coolingSAFMethod_FlowPerCoolingCapacity(-999.0);
                if (fields.find("cooling_supply_air_flow_rate_per_unit_of_capacity") != fields.end()) { // not required field
                    loc_coolingSAFMethod_FlowPerCoolingCapacity = fields.at("cooling_supply_air_flow_rate_per_unit_of_capacity");
                }

                std::string loc_heatingSAFMethod("");
                if (fields.find("heating_supply_air_flow_rate_method") != fields.end()) { // not required field
                    loc_heatingSAFMethod = UtilityRoutines::MakeUPPERCase(fields.at("heating_supply_air_flow_rate_method"));
                }

                Real64 loc_heatingSAFMethod_SAFlow(-999.0);
                if (fields.find("heating_supply_air_flow_rate") != fields.end()) { // not required field
                    std::string tempFieldVal = UtilityRoutines::MakeUPPERCase(fields.at("cooling_supply_air_flow_rate"));
                    if (UtilityRoutines::SameString(tempFieldVal, "AutoSize")) {
                        loc_coolingSAFMethod_SAFlow = DataSizing::AutoSize;
                    } else {
                        loc_heatingSAFMethod_SAFlow = fields.at("heating_supply_air_flow_rate");
                    }
                }

                Real64 loc_heatingSAFMethod_SAFlowPerFloorArea(-999.0);
                if (fields.find("heating_supply_air_flow_rate_per_floor_area") != fields.end()) { // not required field
                    loc_heatingSAFMethod_SAFlowPerFloorArea = fields.at("heating_supply_air_flow_rate_per_floor_area");
                }

                Real64 loc_heatingSAFMethod_FracOfAutosizedHeatingSAFlow(-999.0);
                if (fields.find("heating_fraction_of_autosized_heating_supply_air_flow") != fields.end()) { // not required field
                    loc_heatingSAFMethod_FracOfAutosizedHeatingSAFlow = fields.at("heating_fraction_of_autosized_heating_supply_air_flow");
                }

                Real64 loc_heatingSAFMethod_FlowPerHeatingCapacity(-999.0);
                if (fields.find("heating_supply_air_flow_rate_per_unit_of_capacity") != fields.end()) { // not required field
                    loc_heatingSAFMethod_FlowPerHeatingCapacity = fields.at("heating_supply_air_flow_rate_per_unit_of_capacity");
                }

                std::string loc_noCoolHeatSAFMethod("");
                if (fields.find("no_load_supply_air_flow_rate_method") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod = UtilityRoutines::MakeUPPERCase(fields.at("no_load_supply_air_flow_rate_method"));
                }

                Real64 loc_noCoolHeatSAFMethod_SAFlow(-999.0);
                if (fields.find("no_load_supply_air_flow_rate") != fields.end()) { // not required field
                    std::string tempFieldVal = UtilityRoutines::MakeUPPERCase(fields.at("cooling_supply_air_flow_rate"));
                    if (UtilityRoutines::SameString(tempFieldVal, "AutoSize")) {
                        loc_coolingSAFMethod_SAFlow = DataSizing::AutoSize;
                    } else {
                        loc_noCoolHeatSAFMethod_SAFlow = fields.at("no_load_supply_air_flow_rate");
                    }
                }

                Real64 loc_noCoolHeatSAFMethod_SAFlowPerFloorArea(-999.0);
                if (fields.find("no_load_supply_air_flow_rate_per_floor_area") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_SAFlowPerFloorArea = fields.at("no_load_supply_air_flow_rate_per_floor_area");
                }

                Real64 loc_noCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow(-999.0);
                if (fields.find("no_load_fraction_of_autosized_cooling_supply_air_flow") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow = fields.at("no_load_fraction_of_autosized_cooling_supply_air_flow");
                }

                Real64 loc_noCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow(-999.0);
                if (fields.find("no_load_fraction_of_autosized_heating_supply_air_flow") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow = fields.at("no_load_fraction_of_autosized_heating_supply_air_flow");
                }

                Real64 loc_noCoolHeatSAFMethod_FlowPerCoolingCapacity(-999.0);
                if (fields.find("no_load_supply_air_flow_rate_per_unit_of_capacity_during_cooling_operation") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_FlowPerCoolingCapacity =
                        fields.at("no_load_supply_air_flow_rate_per_unit_of_capacity_during_cooling_operation");
                }

                Real64 loc_noCoolHeatSAFMethod_FlowPerHeatingCapacity(-999.0);
                if (fields.find("heating_supply_air_flow_rate_per_unit_of_capacity_during_heating_operation") != fields.end()) { // not required field
                    loc_noCoolHeatSAFMethod_FlowPerHeatingCapacity =
                        fields.at("heating_supply_air_flow_rate_per_unit_of_capacity_during_heating_operation");
                }

                Real64 loc_designMaxOutletTemp(80.0);
                if (fields.find("maximum_supply_air_temperature") != fields.end()) { // not required field, has default
                    loc_designMaxOutletTemp = fields.at("maximum_supply_air_temperature");
                }

                Real64 loc_maxOATSuppHeat(21.0);
                if (fields.find("maximum_outdoor_dry_bulb_temperature_for_supplemental_heater_operation") !=
                    fields.end()) { // not required field, has default
                    loc_maxOATSuppHeat = fields.at("maximum_outdoor_dry_bulb_temperature_for_supplemental_heater_operation");
                }

                std::string loc_condenserInletNodeName = "";
                if (fields.find("outdoor_dry_bulb_temperature_sensor_node_name") != fields.end()) { // not required field
                    loc_condenserInletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("outdoor_dry_bulb_temperature_sensor_node_name"));
                }

                Real64 loc_maxONOFFCyclesperHour(2.5);
                if (fields.find("maximum_cycling_rate") != fields.end()) { // not required field, has default
                    loc_maxONOFFCyclesperHour = fields.at("maximum_cycling_rate");
                }

                Real64 loc_HPTimeConstant(60.0);
                if (fields.find("heat_pump_time_constant") != fields.end()) { // not required field, has default
                    loc_HPTimeConstant = fields.at("heat_pump_time_constant");
                }

                Real64 loc_onCyclePowerFraction(0.01);
                if (fields.find("fraction_of_on_cycle_power_use") != fields.end()) { // not required field, has default
                    loc_onCyclePowerFraction = fields.at("fraction_of_on_cycle_power_use");
                }

                Real64 loc_fanDelayTime(60.0);
                if (fields.find("heat_pump_fan_delay_time") != fields.end()) { // not required field, has default
                    loc_fanDelayTime = fields.at("heat_pump_fan_dealy_time");
                }

                Real64 loc_ancillaryOnPower(0.0);
                if (fields.find("ancillary_on_cycle_electric_power") != fields.end()) { // not required field, has default
                    loc_ancillaryOnPower = fields.at("ancillary_on_cycle_electric_power");
                }

                Real64 loc_ancillaryOffPower(0.0);
                if (fields.find("ancillary_off_cycle_electric_power") != fields.end()) { // not required field, has default
                    loc_ancillaryOffPower = fields.at("ancillary_off_cycle_electric_power");
                }

                Real64 loc_designHRWaterVolumeFlow(0.0);
                if (fields.find("design_heat_recovery_water_flow_rate") != fields.end()) { // not required field, has default
                    loc_designHRWaterVolumeFlow = fields.at("design_heat_recovery_water_flow_rate");
                }

                Real64 loc_maxHROutletWaterTemp(80.0);
                if (fields.find("maximum_temperature_for_heat_recovery") != fields.end()) { // not required field, has default
                    loc_maxHROutletWaterTemp = fields.at("maximum_temperature_for_heat_recovery");
                }

                std::string loc_heatRecoveryInletNodeName = "";
                if (fields.find("heat_recovery_water_inlet_node_name") != fields.end()) { // not required field
                    loc_heatRecoveryInletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("heat_recovery_water_inlet_node_name"));
                }

                std::string loc_heatRecoveryOutletNodeName = "";
                if (fields.find("heat_recovery_water_outlet_node_name") != fields.end()) { // not required field
                    loc_heatRecoveryOutletNodeName = UtilityRoutines::MakeUPPERCase(fields.at("heat_recovery_water_outlet_node_name"));
                }

                std::string loc_designSpecMultispeedHPType = "";
                if (fields.find("design_specification_multispeed_object_type") != fields.end()) { // not required field
                    loc_designSpecMultispeedHPType = UtilityRoutines::MakeUPPERCase(fields.at("design_specification_multispeed_object_type"));
                }

                std::string loc_designSpecMultispeedHPName = "";
                if (fields.find("design_specification_multispeed_object_name") != fields.end()) { // not required field
                    loc_designSpecMultispeedHPName = UtilityRoutines::MakeUPPERCase(fields.at("design_specification_multispeed_object_name"));
                }

                int FanInletNode = 0;
                int FanOutletNode = 0;
                Real64 FanVolFlowRate = 0.0;
                int CoolingCoilInletNode = 0;
                int CoolingCoilOutletNode = 0;
                int HeatingCoilInletNode = 0;
                int HeatingCoilOutletNode = 0;
                int SupHeatCoilInletNode = 0;
                int SupHeatCoilOutletNode = 0;

                bool errFlag = false;
                bool isNotOK = false;

                thisSys.name = UtilityRoutines::MakeUPPERCase(thisObjectName);
                thisSys.unitType = cCurrentModuleObject;
                thisSys.unitarySystemType_Num = SimAirServingZones::UnitarySystemModel;
                thisSys.sysAvailSchedPtr = ScheduleManager::GetScheduleIndex(loc_sysAvailSched);

                if (UtilityRoutines::SameString(loc_controlType, "Load")) {
                    thisSys.controlType = controlTypeEnum::controlTypeLoad;
                } else if (UtilityRoutines::SameString(loc_controlType, "SetPoint")) {
                    thisSys.controlType = controlTypeEnum::controlTypeSetpoint;
                } else if (UtilityRoutines::SameString(loc_controlType, "SingleZoneVAV")) {
                    thisSys.controlType = controlTypeEnum::controlTypeCCMASHRAE;
                    thisSys.validASHRAECoolCoil = true;
                    thisSys.validASHRAEHeatCoil = true;
                } else {
                    ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                    ShowContinueError("Invalid Control Type = " + loc_controlType);
                    errorsFound = true;
                }

                if (loc_controlZoneName != "") thisSys.controlZoneNum = UtilityRoutines::FindItemInList(loc_controlZoneName, DataHeatBalance::Zone);

                if (UtilityRoutines::SameString(loc_dehumControlType, "None")) {
                    thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_None;
                    thisSys.humidistat = false;
                } else if (UtilityRoutines::SameString(loc_dehumControlType, "CoolReheat")) {
                    thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_CoolReheat;
                    thisSys.humidistat = true;
                } else if (UtilityRoutines::SameString(loc_dehumControlType, "Multimode")) {
                    thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_Multimode;
                    thisSys.humidistat = true;
                }

                thisSys.airInNode = NodeInputManager::GetOnlySingleNode(loc_airInNodeName,
                                                                        errorsFound,
                                                                        cCurrentModuleObject,
                                                                        thisObjectName,
                                                                        DataLoopNode::NodeType_Air,
                                                                        DataLoopNode::NodeConnectionType_Inlet,
                                                                        1,
                                                                        DataLoopNode::ObjectIsParent);
                thisSys.airOutNode = NodeInputManager::GetOnlySingleNode(loc_airOutNodeName,
                                                                         errorsFound,
                                                                         cCurrentModuleObject,
                                                                         thisObjectName,
                                                                         DataLoopNode::NodeType_Air,
                                                                         DataLoopNode::NodeConnectionType_Outlet,
                                                                         1,
                                                                         DataLoopNode::ObjectIsParent);

                Real64 TotalFloorAreaOnAirLoop = 0.0;
                int AirLoopNumber = 0;
                bool AirNodeFound = false;
                bool AirLoopFound = false;
                bool OASysFound = false;
                bool ZoneEquipmentFound = false;
                bool ZoneInletNodeFound = false;

                // Get AirTerminal mixer data
                SingleDuct::GetATMixer(thisObjectName,
                                       thisSys.ATMixerName,
                                       thisSys.ATMixerIndex,
                                       thisSys.ATMixerType,
                                       thisSys.ATMixerPriNode,
                                       thisSys.ATMixerSecNode,
                                       thisSys.ATMixerOutNode,
                                       thisSys.airOutNode);
                if (thisSys.ATMixerType == DataHVACGlobals::ATMixer_InletSide || thisSys.ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {
                    thisSys.ATMixerExists = true;
                }

                // check if the UnitarySystem is connected as zone equipment
                if (!thisSys.ATMixerExists && !AirLoopFound && !OASysFound) {
                    for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                        for (int ZoneExhNum = 1; ZoneExhNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes; ++ZoneExhNum) {
                            if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) != thisSys.airInNode) continue;
                            ZoneEquipmentFound = true;
                            //               Find the controlled zone number for the specified thermostat location
                            thisSys.nodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                            TotalFloorAreaOnAirLoop =
                                DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).FloorArea;
                            thisSys.airLoopEquipment = false;
                            thisSys.zoneInletNode = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum);
                            if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex > 0) {
                                for (int EquipNum = 1; EquipNum <= DataZoneEquipment::ZoneEquipList(
                                                                       DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                                       .NumOfEquipTypes;
                                     ++EquipNum) {
                                    if ((DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                             .EquipType_Num(EquipNum) != DataZoneEquipment::ZoneUnitarySystem_Num) ||
                                        DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .EquipName(EquipNum) != thisObjectName)
                                        continue;
                                    thisSys.zoneSequenceCoolingNum =
                                        DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                            .CoolingPriority(EquipNum);
                                    thisSys.zoneSequenceHeatingNum =
                                        DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                            .HeatingPriority(EquipNum);
                                }
                            }
                            thisSys.controlZoneNum = ControlledZoneNum;
                            break;
                        }
                        if (ZoneEquipmentFound) {
                            for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                 ++ZoneInletNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.airOutNode) continue;
                                ZoneInletNodeFound = true;
                                break;
                            }
                        }
                    }
                    if (!ZoneInletNodeFound) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                            for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                 ++ZoneInletNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.airOutNode) continue;
                                ZoneInletNodeFound = true;
                                ZoneEquipmentFound = true;
                                break;
                            }
                        }
                        if (!ZoneInletNodeFound && ZoneEquipmentFound) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Incorrect or misspelled " + cAlphaFields(iAirOutletNodeNameAlphaNum) + " = " +
                            //                  Alphas(iAirOutletNodeNameAlphaNum));
                            ShowContinueError("Node name does not match any controlled zone inlet node name. Check ZoneHVAC:EquipmentConnections "
                                              "object inputs.");
                            errorsFound = true;
                        }
                    }
                    // Need to move this to the end - just comment out for now
                    // if ( ! ZoneEquipmentFound ) {
                    //	ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
                    //	ShowContinueError( "Incorrect or misspelled " + cAlphaFields( iAirInletNodeNameAlphaNum ) + " = " + Alphas(
                    // iAirInletNodeNameAlphaNum ) ); 	ShowContinueError( "Node name does not match any controlled zone exhaust node name.
                    // Check ZoneHVAC:EquipmentConnections object inputs." ); 	ErrorsFound = true;
                    //}
                }

                // check if the UnitarySystem is connected as zone equipment
                if (thisSys.ATMixerExists && thisSys.ATMixerType == DataHVACGlobals::ATMixer_InletSide) {

                    if (!AirLoopFound && !OASysFound) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                            for (int ZoneExhNum = 1; ZoneExhNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes;
                                 ++ZoneExhNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) != thisSys.ATMixerSecNode) continue;
                                ZoneEquipmentFound = true;
                                //               Find the controlled zone number for the specified thermostat location
                                thisSys.nodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                                // TotalZonesOnAirLoop doesn't appear to be used anywhere
                                //++TotalZonesOnAirLoop;
                                TotalFloorAreaOnAirLoop =
                                    DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).FloorArea;
                                thisSys.airLoopEquipment = false;
                                thisSys.zoneInletNode = thisSys.airOutNode;
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex > 0) {
                                    for (int EquipNum = 1; EquipNum <= DataZoneEquipment::ZoneEquipList(
                                                                           DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                                           .NumOfEquipTypes;
                                         ++EquipNum) {
                                        if ((DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                 .EquipType_Num(EquipNum) != DataZoneEquipment::ZoneUnitarySystem_Num) ||
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                    .EquipName(EquipNum) != thisObjectName)
                                            continue;
                                        thisSys.zoneSequenceCoolingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                        thisSys.zoneSequenceHeatingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                    }
                                }
                                thisSys.controlZoneNum = ControlledZoneNum;
                                break;
                            }
                            if (ZoneEquipmentFound) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.airOutNode) continue;
                                    ZoneInletNodeFound = true;
                                    break;
                                }
                            }
                        }
                        if (!ZoneInletNodeFound) {
                            for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.airOutNode) continue;
                                    ZoneInletNodeFound = true;
                                    ZoneEquipmentFound = true;
                                    break;
                                }
                            }
                            if (!ZoneInletNodeFound && ZoneEquipmentFound) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Incorrect or misspelled " + cAlphaFields(iAirOutletNodeNameAlphaNum) + " = " +
                                //                  Alphas(iAirOutletNodeNameAlphaNum));
                                ShowContinueError("Node name does not match any controlled zone inlet node name. Check ZoneHVAC:EquipmentConnections "
                                                  "object inputs.");
                                errorsFound = true;
                            }
                        }
                        // Need to move this to the end - just comment out for now
                        // if ( !ZoneEquipmentFound ) {
                        //	ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
                        //	ShowContinueError( "Incorrect or misspelled " + cAlphaFields( iAirInletNodeNameAlphaNum ) + " = " + Alphas(
                        // iAirInletNodeNameAlphaNum ) ); 	ShowContinueError( "Node name does not match air terminal mixer secondary air node.
                        // Check AirTerminal:SingleDuct:Mixer object inputs." ); 	ErrorsFound = true;
                        //}
                    }
                }

                if (thisSys.ATMixerExists && thisSys.ATMixerType == DataHVACGlobals::ATMixer_SupplySide) {

                    if (!AirLoopFound && !OASysFound) {
                        for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                            for (int ZoneExhNum = 1; ZoneExhNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumExhaustNodes;
                                 ++ZoneExhNum) {
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ExhaustNode(ZoneExhNum) != thisSys.airInNode) continue;
                                ZoneEquipmentFound = true;
                                //               Find the controlled zone number for the specified thermostat location
                                thisSys.nodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                                // TotalZonesOnAirLoop doesn't appear to be used anywhere
                                //++TotalZonesOnAirLoop;
                                TotalFloorAreaOnAirLoop =
                                    DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum).FloorArea;
                                thisSys.airLoopEquipment = false;
                                thisSys.zoneInletNode = thisSys.ATMixerOutNode;
                                if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex > 0) {
                                    for (int EquipNum = 1; EquipNum <= DataZoneEquipment::ZoneEquipList(
                                                                           DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                                           .NumOfEquipTypes;
                                         ++EquipNum) {
                                        if ((DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                 .EquipType_Num(EquipNum) != DataZoneEquipment::ZoneUnitarySystem_Num) ||
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                    .EquipName(EquipNum) != thisObjectName)
                                            continue;
                                        thisSys.zoneSequenceCoolingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .CoolingPriority(EquipNum);
                                        thisSys.zoneSequenceHeatingNum =
                                            DataZoneEquipment::ZoneEquipList(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).EquipListIndex)
                                                .HeatingPriority(EquipNum);
                                    }
                                }
                                thisSys.controlZoneNum = ControlledZoneNum;
                                break;
                            }
                            if (ZoneEquipmentFound) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.ATMixerOutNode)
                                        continue;
                                    ZoneInletNodeFound = true;
                                    break;
                                }
                            }
                        }
                        if (!ZoneInletNodeFound) {
                            for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                                for (int ZoneInletNum = 1; ZoneInletNum <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                     ++ZoneInletNum) {
                                    if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(ZoneInletNum) != thisSys.ATMixerOutNode)
                                        continue;
                                    ZoneInletNodeFound = true;
                                    ZoneEquipmentFound = true;
                                    break;
                                }
                            }
                            if (!ZoneInletNodeFound && ZoneEquipmentFound) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Incorrect or misspelled " + cAlphaFields(iAirOutletNodeNameAlphaNum) + " = " +
                                //                  Alphas(iAirOutletNodeNameAlphaNum));
                                ShowContinueError("Node name does not match any air terminal mixer secondary air inlet node. Check "
                                                  "AirTerminal:SingleDuct:Mixer object inputs.");
                                errorsFound = true;
                            }
                        }
                        // Need to move this to the end - just comment out for now
                        // if ( !ZoneEquipmentFound ) {
                        //	ShowSevereError( CurrentModuleObject + " = " + UnitarySystem( UnitarySysNum ).Name );
                        //	ShowContinueError( "Incorrect or misspelled " + cAlphaFields( iAirInletNodeNameAlphaNum ) + " = " + Alphas(
                        // iAirInletNodeNameAlphaNum ) ); 	ShowContinueError( "Node name does not match any controlled zone exhaust node name.
                        // Check ZoneHVAC:EquipmentConnections object inputs." ); 	ErrorsFound = true;
                        //}
                    }
                }

                if (!ZoneEquipmentFound) {
                    // check if the UnitarySystem is connected to an air loop
                    for (int AirLoopNum = 1; AirLoopNum <= DataHVACGlobals::NumPrimaryAirSys; ++AirLoopNum) {
                        for (int BranchNum = 1; BranchNum <= DataAirSystems::PrimaryAirSystem(AirLoopNum).NumBranches; ++BranchNum) {
                            for (int CompNum = 1; CompNum <= DataAirSystems::PrimaryAirSystem(AirLoopNum).Branch(BranchNum).TotalComponents;
                                 ++CompNum) {
                                if (UtilityRoutines::SameString(DataAirSystems::PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name,
                                                                thisObjectName) &&
                                    UtilityRoutines::SameString(DataAirSystems::PrimaryAirSystem(AirLoopNum).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                                cCurrentModuleObject)) {
                                    AirLoopNumber = AirLoopNum;
                                    AirLoopFound = true;
                                    for (int ControlledZoneNum = 1; ControlledZoneNum <= DataGlobals::NumOfZones; ++ControlledZoneNum) {
                                        if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum != thisSys.controlZoneNum) continue;
                                        //             Find the controlled zone number for the specified thermostat location
                                        thisSys.nodeNumOfControlledZone = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ZoneNode;
                                        thisSys.controlZoneNum = ControlledZoneNum;
                                        //             Determine if system is on air loop served by the thermostat location specified
                                        for (int zoneInNode = 1; zoneInNode <= DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).NumInletNodes;
                                             ++zoneInNode) {
                                            if (DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNodeAirLoopNum(zoneInNode) ==
                                                AirLoopNumber) {
                                                // TotalZonesOnAirLoop doesn't appear to be used anywhere
                                                //++TotalZonesOnAirLoop;
                                                thisSys.zoneInletNode = DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).InletNode(zoneInNode);
                                                TotalFloorAreaOnAirLoop +=
                                                    DataHeatBalance::Zone(DataZoneEquipment::ZoneEquipConfig(ControlledZoneNum).ActualZoneNum)
                                                        .FloorArea;
                                            }
                                        }
                                        for (int TstatZoneNum = 1; TstatZoneNum <= DataZoneControls::NumTempControlledZones; ++TstatZoneNum) {
                                            if (DataZoneControls::TempControlledZone(TstatZoneNum).ActualZoneNum != thisSys.controlZoneNum) continue;
                                            AirNodeFound = true;
                                        }
                                        for (int TstatZoneNum = 1; TstatZoneNum <= DataZoneControls::NumComfortControlledZones; ++TstatZoneNum) {
                                            if (DataZoneControls::ComfortControlledZone(TstatZoneNum).ActualZoneNum != thisSys.controlZoneNum)
                                                continue;
                                            AirNodeFound = true;
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    // check if the UnitarySystem is connected to an outside air system
                    if (!AirLoopFound && DataSizing::CurOASysNum > 0) {
                        for (int OASysNum = 1; OASysNum <= DataAirLoop::NumOASystems; ++OASysNum) {
                            for (int OACompNum = 1; OACompNum <= DataAirLoop::OutsideAirSys(OASysNum).NumComponents; ++OACompNum) {
                                if (!UtilityRoutines::SameString(DataAirLoop::OutsideAirSys(OASysNum).ComponentName(OACompNum), thisObjectName) ||
                                    !UtilityRoutines::SameString(DataAirLoop::OutsideAirSys(OASysNum).ComponentType(OACompNum), cCurrentModuleObject))
                                    continue;
                                AirLoopNumber = OASysNum;
                                OASysFound = true;
                                break;
                            }
                        }
                    }
                }

                if (!AirLoopFound && !ZoneEquipmentFound && !OASysFound && !DataHVACGlobals::GetAirPathDataDone) {
                    // Unsucessful attempt
                    continue;
                } else {
                    thisSys.myGetInputSuccessfulFlag = true;
                }

                if (AirLoopNumber == 0 && !ZoneEquipmentFound &&
                    (thisSys.controlType == controlTypeEnum::controlTypeLoad || thisSys.controlType == controlTypeEnum::controlTypeCCMASHRAE)) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError("Did not find proper connection for AirLoopHVAC or ZoneHVAC system.");
                    // ShowContinueError("specified " + cAlphaFields(iControlZoneAlphaNum) + " = " + Alphas(iControlZoneAlphaNum));
                    if (!AirNodeFound && !ZoneEquipmentFound) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("Did not find air node (zone with thermostat).");
                        // ShowContinueError("specified " + cAlphaFields(iControlZoneAlphaNum) + " = " + Alphas(iControlZoneAlphaNum));
                        ShowContinueError(
                            "Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Thermostat object must be specified for this zone.");
                    }
                    errorsFound = true;
                }

                if (!ZoneEquipmentFound)
                    BranchNodeConnections::TestCompSet(cCurrentModuleObject, thisSys.name, loc_airInNodeName, loc_airOutNodeName, "Air Nodes");

                if (loc_fanName != "" && loc_fanType != "") {
                    if (UtilityRoutines::SameString(loc_fanType, "Fan:SystemModel")) {
                        if (!HVACFan::checkIfFanNameIsAFanSystem(loc_fanName)) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {
                            thisSys.fanType_Num = DataHVACGlobals::FanType_SystemModelObject;
                            isNotOK = false;
                            ValidateComponent(loc_fanType, loc_fanName, isNotOK, cCurrentModuleObject);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            } else {                                                                // mine data from fan object
                                HVACFan::fanObjs.emplace_back(new HVACFan::FanSystem(loc_fanName)); // call constructor
                                thisSys.fanIndex = HVACFan::getFanObjectVectorIndex(loc_fanName);
                                FanVolFlowRate = HVACFan::fanObjs[thisSys.fanIndex]->designAirVolFlowRate;
                                if (FanVolFlowRate == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                                thisSys.actualFanVolFlowRate = FanVolFlowRate;
                                thisSys.designFanVolFlowRate = FanVolFlowRate;
                                FanInletNode = HVACFan::fanObjs[thisSys.fanIndex]->inletNodeNum;
                                FanOutletNode = HVACFan::fanObjs[thisSys.fanIndex]->outletNodeNum;
                                thisSys.fanAvailSchedPtr = HVACFan::fanObjs[thisSys.fanIndex]->availSchedIndex;
                            }
                        }
                    } else {
                        Fans::GetFanType(loc_fanName, thisSys.fanType_Num, isNotOK, cCurrentModuleObject, loc_fanName);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {
                            isNotOK = false;
                            ValidateComponent(loc_fanType, loc_fanName, isNotOK, cCurrentModuleObject);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            } else { // mine data from fan object
                                // Get the fan index
                                Fans::GetFanIndex(loc_fanName, thisSys.fanIndex, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Design Fan Volume Flow Rate
                                errFlag = false;
                                FanVolFlowRate = Fans::GetFanDesignVolumeFlowRate(loc_fanType, loc_fanName, errFlag);
                                if (FanVolFlowRate == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                                thisSys.actualFanVolFlowRate = FanVolFlowRate;
                                thisSys.designFanVolFlowRate = FanVolFlowRate;
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Fan Inlet Node
                                errFlag = false;
                                FanInletNode = Fans::GetFanInletNode(loc_fanType, loc_fanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Fan Outlet Node
                                errFlag = false;
                                FanOutletNode = Fans::GetFanOutletNode(loc_fanType, loc_fanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the fan's availability schedule
                                errFlag = false;
                                thisSys.fanAvailSchedPtr = Fans::GetFanAvailSchPtr(loc_fanType, loc_fanName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                            } // IF (IsNotOK) THEN
                        }
                    }
                    thisSys.fanExists = true;
                    thisSys.fanName = loc_fanName;
                } else {
                    if ((loc_fanName == "" && loc_fanType != "") || (loc_fanName != "" && loc_fanType == "")) {
                        ShowSevereError("Input errors for " + cCurrentModuleObject + ":" + thisObjectName);
                        ShowContinueError("Invalid Fan Type or Name: Fan Name = " + loc_fanName + ", Fan Type = " + loc_fanType);
                        errorsFound = true;
                    }
                }

                // Add fan to component sets array
                if (thisSys.fanExists)
                    BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                         thisObjectName,
                                                         loc_fanType,
                                                         loc_fanName,
                                                         DataLoopNode::NodeID(FanInletNode),
                                                         DataLoopNode::NodeID(FanOutletNode));

                if (UtilityRoutines::SameString(loc_supFanPlace, "BlowThrough")) thisSys.fanPlace = fanPlaceEnum::blowThru;
                if (UtilityRoutines::SameString(loc_supFanPlace, "DrawThrough")) thisSys.fanPlace = fanPlaceEnum::drawThru;
                if (thisSys.fanPlace == fanPlaceEnum::notYetSet && thisSys.fanExists) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iFanPlaceAlphaNum) + " = " + Alphas(iFanPlaceAlphaNum));
                    errorsFound = true;
                }

                thisSys.fanOpModeSchedPtr = ScheduleManager::GetScheduleIndex(loc_supFanOpMode);
                if (loc_supFanOpMode != "" && thisSys.fanOpModeSchedPtr == 0) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iFanSchedAlphaNum) + " = " + Alphas(iFanSchedAlphaNum));
                    errorsFound = true;
                } else if (loc_supFanOpMode == "") {
                    if (thisSys.controlType == controlTypeEnum::controlTypeSetpoint) {
                        // Fan operating mode must be constant fan so that the coil outlet temp is proportional to PLR
                        // Cycling fan always outputs the full load outlet air temp so should not be used with set point based control
                        thisSys.fanOpMode = fanOpModeEnum::contFanCycCoil;
                    } else {
                        thisSys.fanOpMode = fanOpModeEnum::cycFanCycCoil;
                        // why does it have to be OnOff or SystemFan? I forget now.
                        // if (thisSys.FanType_Num != FanType_SimpleOnOff &&
                        //    thisSys.FanType_Num != FanType_SystemModelObject && thisSys.FanExists) {
                        //    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        //    ShowContinueError(cAlphaFields(iFanTypeAlphaNum) + " = " + Alphas(iFanTypeAlphaNum));
                        //    ShowContinueError("Fan type must be Fan:OnOff of Fan:SystemModel when " + cAlphaFields(iFanSchedAlphaNum) + " =
                        //    Blank."); ErrorsFound = true;
                        //}
                    }
                } else if (loc_supFanOpMode != "" && thisSys.fanOpMode > fanOpModeEnum::fanOpModeNotYetSet &&
                           thisSys.controlType == controlTypeEnum::controlTypeSetpoint) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.fanOpModeSchedPtr, ">", 0.0, "<=", 1.0)) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("For " + loc_fanType + " = " + loc_fanName);
                        ShowContinueError("Fan operating mode must be continuous (fan operating mode schedule values > 0).");
                        // ShowContinueError("Error found in " + cAlphaFields(iFanSchedAlphaNum) + " = " + Alphas(iFanSchedAlphaNum));
                        ShowContinueError("...schedule values must be (>0., <=1.)");
                        errorsFound = true;
                    }
                }

                // Check fan's schedule for cycling fan operation IF constant volume fan is used
                if (thisSys.fanOpModeSchedPtr > 0 && thisSys.fanType_Num == DataHVACGlobals::FanType_SimpleConstVolume) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.fanOpModeSchedPtr, ">", 0.0, "<=", 1.0)) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("For " + cAlphaFields(iFanTypeAlphaNum) + " = " + Alphas(iFanTypeAlphaNum));
                        ShowContinueError("Fan operating mode must be continuous (fan operating mode schedule values > 0).");
                        // ShowContinueError("Error found in " + cAlphaFields(iFanSchedAlphaNum) + " = " + Alphas(iFanSchedAlphaNum));
                        ShowContinueError("...schedule values must be (>0., <=1.)");
                        errorsFound = true;
                    }
                }

                bool PrintMessage = true;
                // Get coil data
                thisSys.heatingSizingRatio = loc_heatingSizingRatio;
                int HeatingCoilPLFCurveIndex = 0;
                thisSys.heatingCoilName = loc_heatingCoilName;
                thisSys.heatingCoilTypeName = loc_heatingCoilType; //  for coil selection report
                if (loc_heatingCoilType != "") {
                    thisSys.heatCoilExists = true;
                    PrintMessage = false;
                } else {
                    thisSys.validASHRAEHeatCoil = false;
                }

                if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:DX:VariableSpeed")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:DX:MultiSpeed")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::CoilDX_MultiSpeedHeating;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Water")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingWater;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Steam")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingSteam;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:WaterToAirHeatPump:EquationFit")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingWaterToAirHPSimple;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:WaterToAirHeatPump:ParameterEstimation")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingWaterToAirHP;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Electric:MultiStage")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingElectric_MultiStage;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Gas:MultiStage")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_HeatingGas_MultiStage;
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Fuel") ||
                           UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Electric") ||
                           UtilityRoutines::SameString(loc_heatingCoilType, "Coil:Heating:Desuperheater")) {
                    thisSys.heatingCoilType_Num = HeatingCoils::GetHeatingCoilTypeNum(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                } else if (UtilityRoutines::SameString(loc_heatingCoilType, "Coil:UserDefined")) {
                    thisSys.heatingCoilType_Num = DataHVACGlobals::Coil_UserDefined;
                } else if (thisSys.heatCoilExists) {
                    thisSys.heatingCoilType_Num = DXCoils::GetCoilTypeNum(loc_heatingCoilType, loc_heatingCoilName, errFlag, PrintMessage);
                }

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {

                    thisSys.DXHeatingCoil = true;
                    errFlag = false;

                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;

                    } else { // mine data from DX heating coil

                        // Get DX heating coil index
                        DXCoils::GetDXCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get DX heating coil capacity
                        thisSys.designHeatingCapacity = DXCoils::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.designHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get DX coil air flow rate.
                        thisSys.maxHeatAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Nodes
                        HeatingCoilInletNode = DXCoils::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        HeatingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        DXCoils::SetDXCoolingCoilData(
                            thisSys.heatingCoilIndex, errorsFound, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, loc_heatingSizingRatio);
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit) {

                    thisSys.DXHeatingCoil = true;
                    errFlag = false;

                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else {

                        thisSys.heatingCoilIndex = VariableSpeedCoils::GetCoilIndexVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.numOfSpeedHeating = VariableSpeedCoils::GetVSCoilNumOfSpeeds(loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;

                        thisSys.maxHeatAirVolFlow =
                            VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        HeatingCoilInletNode = VariableSpeedCoils::GetCoilInletNodeVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        HeatingCoilOutletNode = VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                        // Get DX heating coil capacity
                        thisSys.designHeatingCapacity =
                            VariableSpeedCoils::GetCoilCapacityVariableSpeed(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.designHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }
                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                    thisSys.DXHeatingCoil = true;
                    errFlag = false;
                    DXCoils::GetDXCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag, loc_heatingCoilType);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    thisSys.heatingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                    // Get DX coil air flow rate.
                    thisSys.maxHeatAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    HeatingCoilInletNode = DXCoils::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }
                    HeatingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    thisSys.designHeatingCapacity = DXCoils::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {

                    errFlag = false;
                    HeatingCoils::GetCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }
                    HeatingCoilInletNode = HeatingCoils::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }
                    HeatingCoilOutletNode = HeatingCoils::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    }

                    thisSys.heatingCoilAvailSchPtr = HeatingCoils::GetCoilAvailScheduleIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                    thisSys.designHeatingCapacity = HeatingCoils::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                    if (thisSys.designHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingDesuperheater) {
                    errFlag = false;
                    if (errFlag) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                        errFlag = false;
                    } else {

                        ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from heating coil

                            // Get heating coil index
                            errFlag = false;
                            HeatingCoils::GetCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the design heating capacity
                            thisSys.designHeatingCapacity = HeatingCoils::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                            if (thisSys.designHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.heatingCoilAvailSchPtr =
                                HeatingCoils::GetCoilAvailScheduleIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);

                            // Get the Heating Coil Inlet Node
                            HeatingCoilInletNode = HeatingCoils::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Heating Coil Outlet Node
                            HeatingCoilOutletNode = HeatingCoils::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Heating Coil PLF Curve Index
                            HeatingCoilPLFCurveIndex = HeatingCoils::GetHeatingCoilPLFCurveIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                            // These heating coil types do not have an air flow input field
                            if (thisSys.requestAutoSize) {
                                thisSys.maxHeatAirVolFlow = DataSizing::AutoSize;
                            }
                        } // IF (IsNotOK) THEN
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr =
                            WaterCoils::GetWaterCoilAvailScheduleIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilIndex = WaterCoils::GetWaterCoilIndex("COIL:HEATING:WATER", loc_heatingCoilName, errFlag);
                        if (thisSys.heatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil water Inlet or control Node number
                        thisSys.heatCoilFluidInletNode = WaterCoils::GetCoilWaterInletNode("Coil:Heating:Water", loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil hot water max volume flow rate
                        thisSys.maxHeatCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", loc_heatingCoilName, errFlag);
                        if (thisSys.maxHeatCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Inlet Node
                        HeatingCoilInletNode = WaterCoils::GetCoilInletNode("Coil:Heating:Water", loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = WaterCoils::GetCoilOutletNode("Coil:Heating:Water", loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr =
                            SteamCoils::GetSteamCoilAvailScheduleIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilIndex = SteamCoils::GetSteamCoilIndex("COIL:HEATING:STEAM", loc_heatingCoilName, errFlag);
                        if (thisSys.heatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil steam inlet node number
                        errFlag = false;
                        thisSys.heatCoilFluidInletNode = SteamCoils::GetCoilSteamInletNode("Coil:Heating:Steam", loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil steam max volume flow rate
                        thisSys.maxHeatCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(thisSys.heatingCoilIndex, errFlag);
                        if (thisSys.maxHeatCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if (thisSys.maxHeatCoilFluidFlow > 0.0) {
                            int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                            Real64 TempSteamIn = 100.0;
                            Real64 SteamDensity =
                                FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getUnitarySystemInput);
                            thisSys.maxHeatCoilFluidFlow *= SteamDensity;
                            errFlag = false;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = SteamCoils::GetCoilAirInletNode(thisSys.heatingCoilIndex, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = SteamCoils::GetCoilAirOutletNode(thisSys.heatingCoilIndex, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        if (thisSys.requestAutoSize) {
                            thisSys.maxHeatAirVolFlow = DataSizing::AutoSize;
                        }
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple) {
                    thisSys.DXHeatingCoil = true;
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.heatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.designHeatingCapacity = WaterToAirHeatPumpSimple::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                        errFlag = false;
                        thisSys.maxHeatAirVolFlow = WaterToAirHeatPumpSimple::GetCoilAirFlowRate(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP) {
                    thisSys.DXHeatingCoil = true;
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.heatingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (thisSys.heatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.designHeatingCapacity = WaterToAirHeatPumpSimple::GetCoilCapacity(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Inlet Node
                        errFlag = false;
                        HeatingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Heating Coil Outlet Node
                        HeatingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                    ValidateComponent(loc_heatingCoilType, loc_heatingCoilName, isNotOK, cCurrentModuleObject);
                    if (isNotOK) {
                        ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                        errorsFound = true;
                    } else { // mine data from Heating coil object

                        errFlag = false;
                        thisSys.heatingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        UserDefinedComponents::GetUserDefinedCoilIndex(loc_heatingCoilName, thisSys.heatingCoilIndex, errFlag, cCurrentModuleObject);
                        if (thisSys.heatingCoilIndex == 0) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilNameAlphaNum) + " = " + HeatingCoilName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // **** How to get this info ****
                        //						UnitarySystem( UnitarySysNum ).DesignHeatingCapacity = GetWtoAHPCoilCapacity(
                        // CoolingCoilType,  loc_coolingCoilName,  errFlag ); 						if ( errFlag ) {
                        //							ShowContinueError( "Occurs in " + CurrentModuleObject + " = " +
                        // UnitarySystem(
                        // UnitarySysNum
                        //).Name
                        //); 							ErrorsFound = true;
                        //							errFlag = false;
                        //						}

                        // Get the Cooling Coil Inlet Node
                        errFlag = false;
                        UserDefinedComponents::GetUserDefinedCoilAirInletNode(
                            loc_heatingCoilName, HeatingCoilInletNode, errFlag, cCurrentModuleObject);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        // Get the Cooling Coil Outlet Node
                        UserDefinedComponents::GetUserDefinedCoilAirOutletNode(
                            loc_heatingCoilName, HeatingCoilOutletNode, errFlag, cCurrentModuleObject);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }
                    }

                } else if (thisSys.heatCoilExists) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iHeatingCoilTypeAlphaNum) + " = " + Alphas(iHeatingCoilTypeAlphaNum));
                    errorsFound = true;
                } // IF (thisSys%heatingCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                    thisSys.multiSpeedHeatingCoil = true;
                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                    thisSys.varSpeedHeatingCoil = true;
                }

                // coil outlet node set point has priority, IF not exist, then use system outlet node
                if (SetPointManager::NodeHasSPMCtrlVarType(thisSys.airOutNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.systemHeatControlNodeNum = thisSys.airOutNode;
                if (SetPointManager::NodeHasSPMCtrlVarType(HeatingCoilOutletNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.systemHeatControlNodeNum = HeatingCoilOutletNode;

                thisSys.heatCoilInletNodeNum = HeatingCoilInletNode;
                thisSys.heatCoilOutletNodeNum = HeatingCoilOutletNode;
                thisSys.heatingCoilName = loc_heatingCoilName;

                // Add heating coil to component sets array
                if (thisSys.heatCoilExists) {
                    if (thisSys.heatingCoilType_Num != DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                        BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                             thisObjectName,
                                                             loc_heatingCoilType,
                                                             loc_heatingCoilName,
                                                             DataLoopNode::NodeID(HeatingCoilInletNode),
                                                             DataLoopNode::NodeID(HeatingCoilOutletNode));
                    } else {
                        BranchNodeConnections::SetUpCompSets(
                            cCurrentModuleObject, thisObjectName, loc_heatingCoilType, loc_heatingCoilName, "UNDEFINED", "UNDEFINED");
                    }
                }

                // Get Cooling Coil Information IF available
                if (loc_coolingCoilType != "" && loc_coolingCoilName != "") {
                    thisSys.coolCoilExists = true;

                    //       Find the type of coil. do not print message since this may not be the correct coil type.
                    errFlag = false;
                    if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:VariableSpeed")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:MultiSpeed")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_MultiSpeedCooling;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:Water")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWater;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:Water:DetailedGeometry")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterDetailed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:TwoStageWithHumidityControlMode")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "CoilSystem:Cooling:DX:HeatExchangerAssisted")) {
                        thisSys.coolingCoilType_Num =
                            HVACHXAssistedCoolingCoil::GetCoilGroupTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, PrintMessage);
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "CoilSystem:Cooling:Water:HeatExchangerAssisted")) {
                        thisSys.coolingCoilType_Num =
                            HVACHXAssistedCoolingCoil::GetCoilGroupTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, PrintMessage);
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:WaterToAirHeatPump:EquationFit")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterToAirHPSimple;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterToAirHP;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:SingleSpeed")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingSingleSpeed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:TwoSpeed")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_CoolingTwoSpeed;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:UserDefined")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::Coil_UserDefined;
                    } else if (UtilityRoutines::SameString(loc_coolingCoilType, "Coil:Cooling:DX:SingleSpeed:ThermalStorage")) {
                        thisSys.coolingCoilType_Num = DataHVACGlobals::CoilDX_PackagedThermalStorageCooling;
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilTypeAlphaNum) + " = " + Alphas(iCoolingCoilTypeAlphaNum));
                    }

                    if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed ||
                        thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from DX cooling coil

                            if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) thisSys.numOfSpeedCooling = 2;

                            // Get DX cooling coil index
                            DXCoils::GetDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            thisSys.coolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_coolingCoilType, loc_coolingCoilName, errFlag);

                            // Get DX cooling coil capacity
                            errFlag = false;
                            thisSys.designCoolingCapacity = DXCoils::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get DX coil air flow rate. Latter fields will overwrite this IF input field is present
                            errFlag = false;
                            thisSys.maxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = DXCoils::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            CoolingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get Outdoor condenser node from DX coil object
                            errFlag = false;
                            thisSys.condenserNodeNum = DXCoils::GetCoilCondenserInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            if (thisSys.fanExists) {
                                errFlag = false;
                                DXCoils::SetDXCoolingCoilData(
                                    thisSys.coolingCoilIndex, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, loc_fanName);
                                DXCoils::SetDXCoolingCoilData(
                                    thisSys.coolingCoilIndex, errFlag, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, thisSys.fanIndex);
                                DXCoils::SetDXCoolingCoilData(thisSys.coolingCoilIndex,
                                                              errFlag,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              _,
                                                              thisSys.fanType_Num);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }
                            }
                            if (thisSys.heatCoilExists) {
                                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                    thisSys.heatPump = true;
                                }

                                // set fan info for heating coils
                                if (thisSys.fanExists) {
                                    if (thisSys.fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.heatingCoilName,
                                                                                     thisSys.heatingCoilTypeName,
                                                                                     thisSys.fanName,
                                                                                     DataAirSystems::objectVectorOOFanSystemModel,
                                                                                     thisSys.fanIndex);
                                    } else {
                                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.heatingCoilName,
                                                                                     thisSys.heatingCoilTypeName,
                                                                                     thisSys.fanName,
                                                                                     DataAirSystems::structArrayLegacyFanModels,
                                                                                     thisSys.fanIndex);
                                    }
                                }
                            }

                        } // IF (IsNotOK) THEN

                        // Push heating coil PLF curve index to DX coil
                        if (HeatingCoilPLFCurveIndex > 0) {
                            DXCoils::SetDXCoolingCoilData(thisSys.coolingCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from DX cooling coil

                            // Get DX cooling coil index
                            DXCoils::GetDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            thisSys.coolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_coolingCoilType, loc_coolingCoilName, errFlag);

                            // Get DX cooling coil capacity
                            errFlag = false;
                            thisSys.designCoolingCapacity = DXCoils::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                            errFlag = false;
                            thisSys.maxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = DXCoils::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            CoolingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get Outdoor condenser node from DX coil object
                            errFlag = false;
                            thisSys.condenserNodeNum = DXCoils::GetCoilCondenserInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                        } // IF (IsNotOK) THEN

                        // Push heating coil PLF curve index to DX coil
                        if (HeatingCoilPLFCurveIndex > 0) {
                            DXCoils::SetDXCoolingCoilData(thisSys.coolingCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from heat exchanger assisted cooling coil

                            // Get DX heat exchanger assisted cooling coil index
                            errFlag = false;
                            HVACHXAssistedCoolingCoil::GetHXDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            std::string ChildCoolingCoilName =
                                HVACHXAssistedCoolingCoil::GetHXDXCoilName(loc_coolingCoilType, loc_coolingCoilName, isNotOK);
                            std::string ChildCoolingCoilType =
                                HVACHXAssistedCoolingCoil::GetHXDXCoilType(loc_coolingCoilType, loc_coolingCoilName, isNotOK);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            if (UtilityRoutines::SameString(ChildCoolingCoilType, "COIL:COOLING:DX:SINGLESPEED")) {

                                errFlag = false;
                                thisSys.coolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(ChildCoolingCoilType, ChildCoolingCoilName, errFlag);
                                if (isNotOK) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                                errFlag = false;
                                thisSys.maxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(ChildCoolingCoilType, ChildCoolingCoilName, errFlag);
                                if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get Outdoor condenser node from heat exchanger assisted DX coil object
                                errFlag = false;
                                thisSys.condenserNodeNum = DXCoils::GetCoilCondenserInletNode(
                                    "COIL:COOLING:DX:SINGLESPEED",
                                    HVACHXAssistedCoolingCoil::GetHXDXCoilName(loc_coolingCoilType, loc_coolingCoilName, errFlag),
                                    errFlag);

                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                            } else if (UtilityRoutines::SameString(ChildCoolingCoilType, "COIL:COOLING:DX:VARIABLESPEED")) {
                                thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                                errFlag = false;
                                thisSys.maxCoolAirVolFlow =
                                    VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed(ChildCoolingCoilType, ChildCoolingCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }
                                thisSys.condenserNodeNum = VariableSpeedCoils::GetVSCoilCondenserInletNode(ChildCoolingCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }
                            }

                            // Get DX cooling coil capacity
                            errFlag = false;
                            thisSys.designCoolingCapacity =
                                HVACHXAssistedCoolingCoil::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = HVACHXAssistedCoolingCoil::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            CoolingCoilOutletNode = HVACHXAssistedCoolingCoil::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Push heating coil PLF curve index to DX coil
                            if (HeatingCoilPLFCurveIndex > 0) {
                                // get the actual index to the DX cooling coil object
                                int DXCoilIndex =
                                    HVACHXAssistedCoolingCoil::GetActualDXCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errorsFound);
                                thisSys.actualDXCoilIndexForHXAssisted = DXCoilIndex;
                                int ActualCoolCoilType =
                                    HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, true);
                                if (ActualCoolCoilType == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                                    DXCoils::SetDXCoolingCoilData(DXCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                                }
                                // what could we do for VS coil here? odd thing here
                            }

                            if (thisSys.heatCoilExists) {
                                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                    thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                    thisSys.heatPump = true;
                                }
                            }

                        } // IF (IsNotOK) THEN
                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilWater_CoolingHXAssisted) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;

                        } else { // mine data from heat exchanger assisted cooling coil

                            errFlag = false;
                            int ActualCoolCoilType =
                                HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, true);
                            std::string HXCoilName = HVACHXAssistedCoolingCoil::GetHXDXCoilName(loc_coolingCoilType, loc_coolingCoilName, errFlag);

                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get DX heat exchanger assisted cooling coil index
                            errFlag = false;
                            HVACHXAssistedCoolingCoil::GetHXDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr =
                                WaterCoils::GetWaterCoilAvailScheduleIndex(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType), HXCoilName, errFlag);
                            thisSys.maxCoolCoilFluidFlow =
                                WaterCoils::GetCoilMaxWaterFlowRate(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType), HXCoilName, errFlag);
                            // Get the Cooling Coil water Inlet Node number
                            thisSys.coolCoilFluidInletNode =
                                WaterCoils::GetCoilWaterInletNode(DataHVACGlobals::cAllCoilTypes(ActualCoolCoilType), HXCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Nodes
                            errFlag = false;
                            CoolingCoilInletNode = HVACHXAssistedCoolingCoil::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            CoolingCoilOutletNode = HVACHXAssistedCoolingCoil::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            thisSys.maxCoolAirVolFlow =
                                HVACHXAssistedCoolingCoil::GetHXCoilAirFlowRate(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            thisSys.condenserNodeNum = 0;

                            // Push heating coil PLF curve index to DX coil
                            if (HeatingCoilPLFCurveIndex > 0) {
                                // get the actual index to the DX cooling coil object
                                int DXCoilIndex =
                                    HVACHXAssistedCoolingCoil::GetActualDXCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errorsFound);
                                thisSys.actualDXCoilIndexForHXAssisted = DXCoilIndex;
                                int ActualCoolCoilType =
                                    HVACHXAssistedCoolingCoil::GetCoilObjectTypeNum(loc_coolingCoilType, loc_coolingCoilName, errFlag, true);
                                if (ActualCoolCoilType == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                                    DXCoils::SetDXCoolingCoilData(DXCoilIndex, errorsFound, HeatingCoilPLFCurveIndex);
                                }
                                // VS coil issue here
                            }

                        } // IF (IsNotOK) THEN
                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed ||
                               thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {
                            errFlag = false;
                            thisSys.coolingCoilIndex =
                                VariableSpeedCoils::GetCoilIndexVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            CoolingCoilInletNode =
                                VariableSpeedCoils::GetCoilInletNodeVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            CoolingCoilOutletNode =
                                VariableSpeedCoils::GetCoilOutletNodeVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.condenserNodeNum = VariableSpeedCoils::GetVSCoilCondenserInletNode(loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;

                            thisSys.numOfSpeedCooling = VariableSpeedCoils::GetVSCoilNumOfSpeeds(loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            errFlag = false;
                            thisSys.designCoolingCapacity =
                                VariableSpeedCoils::GetCoilCapacityVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            errFlag = false;
                            thisSys.maxCoolAirVolFlow =
                                VariableSpeedCoils::GetCoilAirFlowRateVariableSpeed(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Set fan info
                            if (thisSys.fanExists) {
                                VariableSpeedCoils::setVarSpeedFanInfo(thisSys.coolingCoilIndex, loc_fanName, thisSys.fanIndex, thisSys.fanType_Num);
                            }
                        }

                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        errFlag = false;
                        DXCoils::GetDXCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, errFlag, loc_coolingCoilType);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        thisSys.coolingCoilAvailSchPtr = DXCoils::GetDXCoilAvailSchPtr(loc_coolingCoilType, loc_coolingCoilName, errFlag);

                        errFlag = false;
                        CoolingCoilInletNode = DXCoils::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        errFlag = false;
                        CoolingCoilOutletNode = DXCoils::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                            errFlag = false;
                        }

                        errFlag = false;
                        thisSys.designCoolingCapacity = DXCoils::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                        if (thisSys.designCoolingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                        errFlag = false;
                        thisSys.maxCoolAirVolFlow = DXCoils::GetDXCoilAirFlow(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                        if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                               thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {

                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr =
                                WaterCoils::GetWaterCoilAvailScheduleIndex(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.coolingCoilIndex = WaterCoils::GetWaterCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.coolingCoilIndex == 0) {
                                // ShowSevereError(cCurrentModuleObject + " illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " +
                                // HeatingCoilName);
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // call for air flow rate not valid for other water coil types
                            if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater) {
                                thisSys.maxCoolAirVolFlow = WaterCoils::GetWaterCoilDesAirFlow(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                    errFlag = false;
                                }
                            }

                            // Get the Cooling Coil water Inlet Node number
                            thisSys.coolCoilFluidInletNode = WaterCoils::GetCoilWaterInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            bool InletNodeNotControlled = true;
                            //  CALL CheckCoilWaterInletNode(thisSys%CoolCoilFluidInletNode,InletNodeNotControlled)
                            if (!InletNodeNotControlled) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError(DataHVACControllers::ControllerTypes(DataHVACControllers::ControllerSimple_Type) + " found for " +
                                                  loc_coolingCoilType + " = \"" + loc_coolingCoilName + ".\"");
                                ShowContinueError("...water coil controllers are not used with " + thisSys.unitType);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil chilled water max volume flow rate
                            errFlag = false;
                            thisSys.maxCoolCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Inlet Node
                            CoolingCoilInletNode = WaterCoils::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            CoolingCoilOutletNode = WaterCoils::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }
                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.coolingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.coolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_coolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.designCoolingCapacity =
                                WaterToAirHeatPumpSimple::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get DX coil air flow rate. Later fields will overwrite this IF input field is present
                            errFlag = false;
                            thisSys.maxCoolAirVolFlow =
                                WaterToAirHeatPumpSimple::GetCoilAirFlowRate(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            CoolingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            CoolingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHP) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.coolingCoilIndex = WaterToAirHeatPumpSimple::GetCoilIndex(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (thisSys.coolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_coolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            thisSys.designCoolingCapacity =
                                WaterToAirHeatPumpSimple::GetCoilCapacity(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            CoolingCoilInletNode = WaterToAirHeatPumpSimple::GetCoilInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            CoolingCoilOutletNode = WaterToAirHeatPumpSimple::GetCoilOutletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                        if (thisSys.heatCoilExists) {
                            if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHP ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                                thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical) {
                                thisSys.heatPump = true;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            UserDefinedComponents::GetUserDefinedCoilIndex(
                                loc_coolingCoilName, thisSys.coolingCoilIndex, errFlag, cCurrentModuleObject);
                            if (thisSys.coolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_coolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // **** How to get this info ****
                            //						UnitarySystem( UnitarySysNum ).DesignCoolingCapacity =
                            // GetWtoAHPCoilCapacity(
                            // CoolingCoilType, loc_coolingCoilName, errFlag ); 						if ( errFlag ) {
                            //							ShowContinueError( "Occurs in " + CurrentModuleObject + " = "
                            //+
                            // UnitarySystem(  UnitarySysNum ).Name ); 							ErrorsFound = true;
                            //							errFlag = false;
                            //						}

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            UserDefinedComponents::GetUserDefinedCoilAirInletNode(
                                loc_coolingCoilName, CoolingCoilInletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            UserDefinedComponents::GetUserDefinedCoilAirOutletNode(
                                loc_coolingCoilName, CoolingCoilOutletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_PackagedThermalStorageCooling) {
                        ValidateComponent(loc_coolingCoilType, loc_coolingCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Cooling coil object

                            errFlag = false;
                            thisSys.coolingCoilAvailSchPtr = DataGlobals::ScheduleAlwaysOn;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            PackagedThermalStorageCoil::GetTESCoilIndex(loc_coolingCoilName, thisSys.coolingCoilIndex, errFlag, cCurrentModuleObject);
                            if (thisSys.coolingCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilNameAlphaNum) + " = " + loc_coolingCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            PackagedThermalStorageCoil::GetTESCoilCoolingAirFlowRate(
                                loc_coolingCoilName, thisSys.maxCoolAirVolFlow, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            PackagedThermalStorageCoil::GetTESCoilCoolingCapacity(
                                loc_coolingCoilName, thisSys.designCoolingCapacity, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Inlet Node
                            errFlag = false;
                            PackagedThermalStorageCoil::GetTESCoilAirInletNode(
                                loc_coolingCoilName, CoolingCoilInletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the Cooling Coil Outlet Node
                            PackagedThermalStorageCoil::GetTESCoilAirOutletNode(
                                loc_coolingCoilName, CoolingCoilOutletNode, errFlag, cCurrentModuleObject);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                    } else { // IF(.NOT. lAlphaBlanks(16))THEN
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Illegal " + cAlphaFields(iCoolingCoilTypeAlphaNum) + " = " + Alphas(iCoolingCoilTypeAlphaNum));
                        errorsFound = true;
                    }

                    if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        thisSys.multiSpeedCoolingCoil = true;
                    } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit ||
                               thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                        thisSys.varSpeedCoolingCoil = true;
                    }

                    if (SetPointManager::NodeHasSPMCtrlVarType(thisSys.airOutNode, SetPointManager::iCtrlVarType_Temp))
                        thisSys.systemCoolControlNodeNum = thisSys.airOutNode;
                    if (SetPointManager::NodeHasSPMCtrlVarType(CoolingCoilOutletNode, SetPointManager::iCtrlVarType_Temp))
                        thisSys.systemCoolControlNodeNum = CoolingCoilOutletNode;

                    thisSys.coolCoilInletNodeNum = CoolingCoilInletNode;
                    thisSys.coolCoilOutletNodeNum = CoolingCoilOutletNode;
                    thisSys.coolingCoilName = loc_coolingCoilName;

                } else {
                    thisSys.validASHRAECoolCoil = false;
                }

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPSimple &&
                    thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPSimple) {
                    thisSys.waterCyclingMode = DataHVACGlobals::WaterCycling;
                    WaterToAirHeatPumpSimple::SetSimpleWSHPData(
                        thisSys.coolingCoilIndex, errorsFound, thisSys.waterCyclingMode, _, thisSys.heatingCoilIndex);
                }

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWaterToAirHPVSEquationFit &&
                    thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit) {
                    VariableSpeedCoils::SetVarSpeedCoilData(thisSys.coolingCoilIndex, errorsFound, _, thisSys.heatingCoilIndex);
                }

                // Add cooling coil to component sets array
                if (thisSys.coolCoilExists) {
                    if (thisSys.coolingCoilType_Num != DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                        BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                             thisObjectName,
                                                             loc_coolingCoilType,
                                                             loc_coolingCoilName,
                                                             DataLoopNode::NodeID(CoolingCoilInletNode),
                                                             DataLoopNode::NodeID(CoolingCoilOutletNode));
                    } else {
                        BranchNodeConnections::SetUpCompSets(
                            cCurrentModuleObject, thisObjectName, loc_coolingCoilType, loc_coolingCoilName, "UNDEFINED", "UNDEFINED");
                    }
                }
                // Run as 100% DOAS DX coil
                if (!UtilityRoutines::SameString(loc_ISHundredPercentDOASDXCoil, "Yes")) {
                    thisSys.ISHundredPercentDOASDXCoil = false;
                } else {
                    if (UtilityRoutines::SameString(loc_ISHundredPercentDOASDXCoil, "Yes")) {
                        thisSys.ISHundredPercentDOASDXCoil = true;
                        if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                            ShowWarningError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Invalid entry for " + cAlphaFields(iDOASDXCoilAlphaNum) + " :" + Alphas(iDOASDXCoilAlphaNum));
                            ShowContinueError("Variable DX Cooling Coil is not supported as 100% DOAS DX coil.");
                            ShowContinueError("Variable DX Cooling Coil is reset as a regular DX coil and the simulation continues.");
                            thisSys.ISHundredPercentDOASDXCoil = false;
                        }
                    } else if (UtilityRoutines::SameString(loc_ISHundredPercentDOASDXCoil, "")) {
                        thisSys.ISHundredPercentDOASDXCoil = false;
                    } else if (UtilityRoutines::SameString(loc_ISHundredPercentDOASDXCoil, "No")) {
                        thisSys.ISHundredPercentDOASDXCoil = false;
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iDOASDXCoilAlphaNum) + " :" + Alphas(iDOASDXCoilAlphaNum));
                        ShowContinueError("Must be Yes or No.");
                        errorsFound = true;
                    }
                }

                // considered as as 100% DOAS DX cooling coil
                if (thisSys.ISHundredPercentDOASDXCoil) {
                    // set the system DX Coil application type to the child DX coil
                    if (!(thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed ||
                          thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterToAirHPVSEquationFit)) {
                        DXCoils::SetDXCoilTypeData(thisSys.coolingCoilName);
                    }
                }
                // DOAS DX Cooling Coil Leaving Minimum Air Temperature
                // if (NumNumbers > 0) {
                // if (!lNumericBlanks(iDOASDXMinTempNumericNum)) {
                thisSys.designMinOutletTemp = loc_designMinOutletTemp;
                if (thisSys.controlType != controlTypeEnum::controlTypeCCMASHRAE && thisSys.designMinOutletTemp == DataSizing::AutoSize) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Invalid entry for " + cNumericFields(iDOASDXMinTempNumericNum) + " =DataSizing::AutoSize.");
                    // ShowContinueError("AutoSizing not allowed when " + cAlphaFields(iControlTypeAlphaNum) + " = " +
                    //                  Alphas(iControlTypeAlphaNum));
                    errorsFound = true;
                }
                if (thisSys.controlType != controlTypeEnum::controlTypeCCMASHRAE && thisSys.designMinOutletTemp > 7.5) {
                    ShowWarningError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Invalid entry for " + cNumericFields(iDOASDXMinTempNumericNum) + " = " +
                    //                  TrimSigDigits(Numbers(iDOASDXMinTempNumericNum), 3));
                    ShowContinueError("The minimum supply air temperature will be limited to 7.5C and the simulation continues.");
                    thisSys.designMinOutletTemp = 7.5;
                }
                //}
                //}

                // Get Latent Load Control flag
                if (loc_latentControlFlag != "") {
                    if (UtilityRoutines::SameString(loc_latentControlFlag, "SensibleOnlyLoadControl")) {
                        thisSys.runOnSensibleLoad = true;
                        thisSys.runOnLatentLoad = false;
                    } else if (UtilityRoutines::SameString(loc_latentControlFlag, "LatentOnlyLoadControl")) {
                        thisSys.runOnSensibleLoad = false;
                        thisSys.runOnLatentLoad = true;
                    } else if (UtilityRoutines::SameString(loc_latentControlFlag, "LatentOrSensibleLoadControl")) {
                        thisSys.runOnSensibleLoad = true;
                        thisSys.runOnLatentLoad = true;
                    } else if (UtilityRoutines::SameString(loc_latentControlFlag, "LatentWithSensibleLoadControl")) {
                        thisSys.runOnSensibleLoad = true;
                        thisSys.runOnLatentLoad = true;
                        thisSys.runOnLatentOnlyWithSensible = true;
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iRunOnLatentLoadAlphaNum) + " :" +
                        // Alphas(iRunOnLatentLoadAlphaNum));
                        ShowContinueError("Must be SensibleOnlyLoadControl, LatentOnlyLoadControl, LatentOrSensibleLoadControl, or "
                                          "LatentWithSensibleLoadControl.");
                    }
                }

                // Get reheat coil data if humidistat is used
                thisSys.suppHeatCoilName = loc_suppHeatCoilName;
                thisSys.suppHeatCoilTypeName = loc_suppHeatCoilType;
                errFlag = false;

                if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Water")) {
                    thisSys.suppHeatCoilType_Num = DataHVACGlobals::Coil_HeatingWater;
                } else if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Steam")) {
                    thisSys.suppHeatCoilType_Num = DataHVACGlobals::Coil_HeatingSteam;
                } else if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Fuel") ||
                           UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:Electric") ||
                           UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:Heating:DesuperHeater")) {
                    thisSys.suppHeatCoilType_Num = HeatingCoils::GetHeatingCoilTypeNum(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                } else if (UtilityRoutines::SameString(loc_suppHeatCoilType, "Coil:UserDefined")) {
                    thisSys.suppHeatCoilType_Num = DataHVACGlobals::Coil_UserDefined;
                }

                if (loc_suppHeatCoilType != "" && loc_suppHeatCoilName != "") {
                    thisSys.suppCoilExists = true;

                    if (thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel ||
                        thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingElectric ||
                        thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingDesuperheater) {

                        thisSys.suppHeatCoilType_Num = HeatingCoils::GetHeatingCoilTypeNum(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else {

                            ValidateComponent(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK, cCurrentModuleObject);
                            if (isNotOK) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;

                            } else { // mine data from reheat coil

                                // Get the heating coil index
                                thisSys.suppHeatCoilIndex = HeatingCoils::GetHeatingCoilIndex(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK);
                                if (isNotOK) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the design supplemental heating capacity
                                errFlag = false;
                                thisSys.designSuppHeatingCapacity =
                                    HeatingCoils::GetCoilCapacity(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                                if (thisSys.designSuppHeatingCapacity == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Reheat Coil Inlet Node
                                errFlag = false;
                                SupHeatCoilInletNode = HeatingCoils::GetCoilInletNode(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                                // Get the Reheat Coil Outlet Node
                                errFlag = false;
                                SupHeatCoilOutletNode = HeatingCoils::GetCoilOutletNode(loc_suppHeatCoilType, loc_suppHeatCoilName, errFlag);
                                if (errFlag) {
                                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                    errorsFound = true;
                                }

                            } // IF (IsNotOK) THEN
                        }

                        thisSys.suppCoilAirInletNode = SupHeatCoilInletNode;
                        thisSys.suppCoilAirOutletNode = SupHeatCoilOutletNode;

                    } else if (thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {

                        ValidateComponent(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from heating coil object

                            // Get the Heating Coil water Inlet or control Node number
                            errFlag = false;
                            thisSys.suppCoilFluidInletNode = WaterCoils::GetCoilWaterInletNode("Coil:Heating:Water", loc_suppHeatCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the ReHeat Coil hot water max volume flow rate
                            errFlag = false;
                            thisSys.maxSuppCoilFluidFlow = WaterCoils::GetCoilMaxWaterFlowRate("Coil:Heating:Water", loc_suppHeatCoilName, errFlag);
                            if (thisSys.maxSuppCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the ReHeat Coil Inlet Node
                            errFlag = false;
                            SupHeatCoilInletNode = WaterCoils::GetCoilInletNode("Coil:Heating:Water", loc_suppHeatCoilName, errFlag);
                            thisSys.suppCoilAirInletNode = SupHeatCoilInletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the ReHeat Coil Outlet Node
                            errFlag = false;
                            SupHeatCoilOutletNode = WaterCoils::GetCoilOutletNode("Coil:Heating:Water", loc_suppHeatCoilName, errFlag);
                            thisSys.suppCoilAirOutletNode = SupHeatCoilOutletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }
                        }

                    } else if (thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_HeatingSteam) {

                        ValidateComponent(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from heating coil object

                            errFlag = false;
                            thisSys.suppHeatCoilIndex = SteamCoils::GetSteamCoilIndex("COIL:HEATING:STEAM", loc_suppHeatCoilName, errFlag);
                            if (thisSys.suppHeatCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowSevereError("Illegal " + cAlphaFields(iSuppHeatCoilNameAlphaNum) + " = " + SuppHeatCoilName);
                                errorsFound = true;
                            }

                            // Get the Heating Coil steam inlet node number
                            errFlag = false;
                            thisSys.suppCoilFluidInletNode = SteamCoils::GetCoilSteamInletNode("Coil:Heating:Steam", loc_suppHeatCoilName, errFlag);
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }
                            // Get the Heating Coil steam max volume flow rate
                            thisSys.maxSuppCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(thisSys.suppHeatCoilIndex, errFlag);
                            if (thisSys.maxSuppCoilFluidFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                            if (thisSys.maxSuppCoilFluidFlow > 0.0) {
                                int SteamIndex = 0; // Function GetSatDensityRefrig will look up steam index if 0 is passed
                                Real64 TempSteamIn = 100.0;
                                Real64 SteamDensity =
                                    FluidProperties::GetSatDensityRefrig(fluidNameSteam, TempSteamIn, 1.0, SteamIndex, getUnitarySystemInput);
                                thisSys.maxSuppCoilFluidFlow = SteamCoils::GetCoilMaxSteamFlowRate(thisSys.suppHeatCoilIndex, errFlag) * SteamDensity;
                            }

                            // Get the Heating Coil Inlet Node
                            errFlag = false;
                            SupHeatCoilInletNode = SteamCoils::GetCoilAirInletNode(thisSys.suppHeatCoilIndex, loc_suppHeatCoilName, errFlag);
                            thisSys.suppCoilAirInletNode = SupHeatCoilInletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }

                            // Get the Heating Coil Outlet Node
                            errFlag = false;
                            SupHeatCoilOutletNode = SteamCoils::GetCoilAirOutletNode(thisSys.suppHeatCoilIndex, loc_suppHeatCoilName, errFlag);
                            thisSys.suppCoilAirOutletNode = SupHeatCoilOutletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                            }
                        }

                    } else if (thisSys.suppHeatCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                        ValidateComponent(loc_suppHeatCoilType, loc_suppHeatCoilName, isNotOK, cCurrentModuleObject);
                        if (isNotOK) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        } else { // mine data from Heating coil object

                            //						errFlag = false;
                            //						UnitarySystem( UnitarySysNum ).HeatingCoilAvailSchPtr =
                            // ScheduleAlwaysOn; 						if ( errFlag ) {
                            //							ShowContinueError( "Occurs in " + CurrentModuleObject + " = "
                            //+
                            // UnitarySystem(  UnitarySysNum ).Name ); 							ErrorsFound = true;
                            //							errFlag = false;
                            //						}

                            errFlag = false;
                            UserDefinedComponents::GetUserDefinedCoilIndex(
                                loc_suppHeatCoilName, thisSys.suppHeatCoilIndex, errFlag, cCurrentModuleObject);
                            if (thisSys.suppHeatCoilIndex == 0) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("Illegal " + cAlphaFields(iSuppHeatCoilNameAlphaNum) + " = " + SuppHeatCoilName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the supplemental heating Coil Inlet Node
                            errFlag = false;
                            UserDefinedComponents::GetUserDefinedCoilAirInletNode(
                                loc_suppHeatCoilName, SupHeatCoilInletNode, errFlag, cCurrentModuleObject);
                            thisSys.suppCoilAirInletNode = SupHeatCoilInletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }

                            // Get the supplemenatal heating Coil Outlet Node
                            UserDefinedComponents::GetUserDefinedCoilAirOutletNode(
                                loc_suppHeatCoilName, SupHeatCoilOutletNode, errFlag, cCurrentModuleObject);
                            thisSys.suppCoilAirOutletNode = SupHeatCoilOutletNode;
                            if (errFlag) {
                                ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                                errorsFound = true;
                                errFlag = false;
                            }
                        }

                    } else { // Illegal reheating coil type
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Illegal " + cAlphaFields(iSuppHeatCoilTypeAlphaNum) + " = " + Alphas(iSuppHeatCoilTypeAlphaNum));
                        errorsFound = true;
                    } // IF (thisSys%SuppHeatCoilType_Num == Coil_HeatingGasOrOtherFuel .OR. &, etc.

                } // IF(.NOT. lAlphaBlanks(iSuppHeatCoilTypeAlphaNum))THEN

                if (SetPointManager::NodeHasSPMCtrlVarType(thisSys.airOutNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.suppHeatControlNodeNum = thisSys.airOutNode;
                if (SetPointManager::NodeHasSPMCtrlVarType(SupHeatCoilOutletNode, SetPointManager::iCtrlVarType_Temp))
                    thisSys.suppHeatControlNodeNum = SupHeatCoilOutletNode;

                // Add supplemental heating coil to component sets array
                if (thisSys.suppCoilExists)
                    BranchNodeConnections::SetUpCompSets(cCurrentModuleObject,
                                                         thisObjectName,
                                                         loc_suppHeatCoilType,
                                                         loc_suppHeatCoilName,
                                                         DataLoopNode::NodeID(SupHeatCoilInletNode),
                                                         DataLoopNode::NodeID(SupHeatCoilOutletNode));

                // set fan info for heating coils
                if (thisSys.suppCoilExists && thisSys.fanExists) {
                    if (thisSys.fanType_Num == DataHVACGlobals::FanType_SystemModelObject) {
                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.suppHeatCoilName,
                                                                     thisSys.suppHeatCoilTypeName,
                                                                     thisSys.fanName,
                                                                     DataAirSystems::objectVectorOOFanSystemModel,
                                                                     thisSys.fanIndex);
                    } else {
                        coilSelectionReportObj->setCoilSupplyFanInfo(thisSys.suppHeatCoilName,
                                                                     thisSys.suppHeatCoilTypeName,
                                                                     thisSys.fanName,
                                                                     DataAirSystems::structArrayLegacyFanModels,
                                                                     thisSys.fanIndex);
                    }
                }

                // Users may not provide SA flow input fields (below) and leave them blank. Check if other coil isDataSizing::AutoSized first to
                // alieviate input requirements. check if coil has no air flow input (VolFlow = 0) and other coil isDataSizing::AutoSized. If so,
                // useDataSizing::AutoSize for coil with 0 air flow rate. This means that the coils MUST mine the air flow rate if it exists
                if (thisSys.coolCoilExists && thisSys.heatCoilExists) {
                    if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize && thisSys.maxHeatAirVolFlow == 0 && loc_heatingSAFMethod == "") {
                        thisSys.maxHeatAirVolFlow = DataSizing::AutoSize;
                    } else if (thisSys.maxCoolAirVolFlow == 0 && thisSys.maxHeatAirVolFlow == DataSizing::AutoSize && loc_coolingSAFMethod == "") {
                        thisSys.maxCoolAirVolFlow = DataSizing::AutoSize;
                    }
                }

                // Determine supply air flow rate sizing method for cooling mode
                if (UtilityRoutines::SameString(loc_coolingSAFMethod, "SupplyAirFlowRate")) {
                    thisSys.coolingSAFMethod = SupplyAirFlowRate;

                    if (loc_coolingSAFMethod_SAFlow != -999.0) {
                        thisSys.maxCoolAirVolFlow = loc_coolingSAFMethod_SAFlow;
                        if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if ((thisSys.maxCoolAirVolFlow < 0.0 && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxCoolAirVolFlow == 0.0 && thisSys.coolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cNumericFields(iMaxCoolAirVolFlowNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iMaxCoolAirVolFlowNumericNum), 7));
                            errorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iMaxCoolAirVolFlowNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_coolingSAFMethod, "FlowPerFloorArea")) {

                    thisSys.coolingSAFMethod = FlowPerFloorArea;
                    if (loc_coolingSAFMethod_SAFlowPerFloorArea != -999.0) {
                        thisSys.maxCoolAirVolFlow = loc_coolingSAFMethod_SAFlowPerFloorArea;
                        if ((thisSys.maxCoolAirVolFlow < 0.0 && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxCoolAirVolFlow == 0.0 && thisSys.coolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFloorAreaNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iCoolFlowPerFloorAreaNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFloorAreaNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.maxCoolAirVolFlow *= TotalFloorAreaOnAirLoop;
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iCoolFlowPerFloorAreaNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_coolingSAFMethod, "FractionOfAutosizedCoolingValue")) {

                    thisSys.coolingSAFMethod = FractionOfAutoSizedCoolingValue;
                    if (loc_coolingSAFMethod_FracOfAutosizedCoolingSAFlow != -999.0) {
                        thisSys.maxCoolAirVolFlow = loc_coolingSAFMethod_FracOfAutosizedCoolingSAFlow;
                        if ((thisSys.maxCoolAirVolFlow < 0.0 && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxCoolAirVolFlow == 0.0 && thisSys.coolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFracCoolNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iCoolFlowPerFracCoolNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerFracCoolNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iCoolFlowPerFracCoolNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_coolingSAFMethod, "FlowPerCoolingCapacity")) {

                    thisSys.coolingSAFMethod = FlowPerCoolingCapacity;
                    if (loc_coolingSAFMethod_FlowPerCoolingCapacity != -999.0) {
                        thisSys.maxCoolAirVolFlow = loc_coolingSAFMethod_FlowPerCoolingCapacity;
                        if ((thisSys.maxCoolAirVolFlow < 0.0 && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxCoolAirVolFlow == 0.0 && thisSys.coolCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerCoolCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iCoolFlowPerCoolCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxCoolAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iCoolFlowPerCoolCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iCoolFlowPerCoolCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_coolingSAFMethod, "None") || loc_coolingSAFMethod == "") {
                    thisSys.coolingSAFMethod = None;
                    //          thisSys%RequestAutosize = .TRUE. ! ??
                    if (thisSys.coolCoilExists && thisSys.maxCoolAirVolFlow == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iCoolSAFMAlphaNum) + " is blank and relates to " + loc_coolingCoilType +
                        // " = " +
                        //                  loc_coolingCoilName);
                        if (thisSys.heatCoilExists) {
                            ShowContinueError(
                                "Blank field not allowed for this coil type when heating coil air flow rate is notDataSizing::AutoSized.");
                        } else {
                            ShowContinueError("Blank field not allowed for this type of cooling coil.");
                        }
                        errorsFound = true;
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iCoolSAFMAlphaNum) + " = " + Alphas(iCoolSAFMAlphaNum));
                    ShowContinueError("Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingValue, "
                                      "FlowPerCoolingCapacity, or None ");
                    errorsFound = true;
                }

                // Determine supply air flow rate sizing method for heating mode
                if (UtilityRoutines::SameString(loc_heatingSAFMethod, "SupplyAirFlowRate")) {
                    thisSys.heatingSAFMethod = SupplyAirFlowRate;
                    if (loc_heatingSAFMethod_SAFlow != -999.0) {
                        thisSys.maxHeatAirVolFlow = loc_heatingSAFMethod_SAFlow;
                        if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if ((thisSys.maxHeatAirVolFlow < 0.0 && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cNumericFields(iMaxHeatAirVolFlowNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iMaxHeatAirVolFlowNumericNum), 7));
                            errorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iMaxHeatAirVolFlowNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_heatingSAFMethod, "FlowPerFloorArea")) {
                    thisSys.heatingSAFMethod = FlowPerFloorArea;
                    if (loc_heatingSAFMethod_SAFlowPerFloorArea != -999.0) {
                        thisSys.maxHeatAirVolFlow = loc_heatingSAFMethod_SAFlowPerFloorArea;
                        if ((thisSys.maxHeatAirVolFlow < 0.0 && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFloorAreaNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iHeatFlowPerFloorAreaNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFloorAreaNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.maxHeatAirVolFlow *= TotalFloorAreaOnAirLoop;
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFlowPerFloorAreaNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_heatingSAFMethod, "FractionOfAutosizedHeatingValue")) {
                    thisSys.heatingSAFMethod = FractionOfAutoSizedHeatingValue;
                    if (loc_heatingSAFMethod_FracOfAutosizedHeatingSAFlow != -999.0) {
                        thisSys.maxHeatAirVolFlow = loc_heatingSAFMethod_FracOfAutosizedHeatingSAFlow;
                        if ((thisSys.maxHeatAirVolFlow < 0.0 && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFracCoolNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iHeatFlowPerFracCoolNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerFracCoolNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFlowPerFracCoolNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_heatingSAFMethod, "FlowPerHeatingCapacity")) {
                    thisSys.heatingSAFMethod = FlowPerHeatingCapacity;
                    if (loc_heatingSAFMethod_FlowPerHeatingCapacity != -999.0) {
                        thisSys.maxHeatAirVolFlow = loc_heatingSAFMethod_FlowPerHeatingCapacity;
                        if ((thisSys.maxHeatAirVolFlow < 0.0 && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize) ||
                            (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists)) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerHeatCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iHeatFlowPerHeatCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iHeatFlowPerHeatCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iHeatFlowPerHeatCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_heatingSAFMethod, "None") || loc_heatingSAFMethod == "") {
                    thisSys.heatingSAFMethod = None;
                    //          thisSys%RequestAutosize = .TRUE. ! ??
                    if (thisSys.heatCoilExists && thisSys.maxHeatAirVolFlow == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iHeatSAFMAlphaNum) + " is blank and relates to " + loc_heatingCoilType +
                        // " = " +
                        //                  loc_heatingCoilName);
                        if (thisSys.coolCoilExists) {
                            ShowContinueError(
                                "Blank field not allowed for this coil type when cooling coil air flow rate is notDataSizing::AutoSized.");
                        } else {
                            ShowContinueError("Blank field not allowed for this type of heating coil.");
                        }
                        errorsFound = true;
                    }
                } else {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iHeatSAFMAlphaNum) + " = " + Alphas(iHeatSAFMAlphaNum));
                    ShowContinueError("Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedHeatingValue, "
                                      "FlowPerHeatingCapacity, or None ");
                    errorsFound = true;
                }

                // Determine supply air flow rate sizing method when cooling or heating is not needed
                if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "SupplyAirFlowRate")) {
                    thisSys.noCoolHeatSAFMethod = SupplyAirFlowRate;
                    if (loc_noCoolHeatSAFMethod_SAFlow != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_SAFlow;
                        if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) thisSys.requestAutoSize = true;

                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Illegal " + cNumericFields(iMaxNoCoolHeatAirVolFlowNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iMaxNoCoolHeatAirVolFlowNumericNum), 7));
                            errorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iMaxNoCoolHeatAirVolFlowNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FlowPerFloorArea")) {
                    thisSys.noCoolHeatSAFMethod = FlowPerFloorArea;
                    if (loc_noCoolHeatSAFMethod_SAFlowPerFloorArea != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_SAFlowPerFloorArea;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerFloorAreaNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.maxNoCoolHeatAirVolFlow *= TotalFloorAreaOnAirLoop;
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFloorAreaNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FractionOfAutosizedCoolingValue")) {
                    thisSys.noCoolHeatSAFMethod = FractionOfAutoSizedCoolingValue;
                    if (loc_noCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_FracOfAutosizedCoolingSAFlow;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerFracCoolNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFracCoolNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FractionOfAutosizedHeatingValue")) {
                    thisSys.noCoolHeatSAFMethod = FractionOfAutoSizedHeatingValue;
                    if (loc_noCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_FracOfAutosizedHeatingSAFlow;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerFracHeatNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerFracHeatNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FlowPerCoolingCapacity")) {
                    thisSys.noCoolHeatSAFMethod = FlowPerCoolingCapacity;
                    if (loc_noCoolHeatSAFMethod_FlowPerCoolingCapacity != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_FlowPerCoolingCapacity;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerCoolCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerCoolCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "FlowPerHeatingCapacity")) {
                    thisSys.noCoolHeatSAFMethod = FlowPerHeatingCapacity;
                    if (loc_noCoolHeatSAFMethod_FlowPerHeatingCapacity != -999.0) {
                        thisSys.maxNoCoolHeatAirVolFlow = loc_noCoolHeatSAFMethod_FlowPerHeatingCapacity;
                        if (thisSys.maxNoCoolHeatAirVolFlow < 0.0 && thisSys.maxNoCoolHeatAirVolFlow != DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum) + " = " +
                            //                  TrimSigDigits(Numbers(iNoCoolHeatFlowPerHeatCapNumericNum), 7));
                            errorsFound = true;
                            // DataSizing::AutoSized input is not allowed
                        } else if (thisSys.maxNoCoolHeatAirVolFlow == DataSizing::AutoSize) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                            // ShowContinueError("Illegal " + cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum) + " =DataSizing::AutoSize");
                            errorsFound = true;
                        } else {
                            thisSys.requestAutoSize = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Input for " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                        // ShowContinueError("Blank field not allowed for " + cNumericFields(iNoCoolHeatFlowPerHeatCapNumericNum));
                        errorsFound = true;
                    }
                } else if (UtilityRoutines::SameString(loc_noCoolHeatSAFMethod, "None") || loc_noCoolHeatSAFMethod == "") {
                    thisSys.noCoolHeatSAFMethod = None;
                    //          thisSys%RequestAutosize = .TRUE. ! ??
                } else {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iNoCoolHeatSAFMAlphaNum) + " = " + Alphas(iNoCoolHeatSAFMAlphaNum));
                    ShowContinueError("Valid entries are: SupplyAirFlowRate, FlowPerFloorArea, FractionOfAutosizedCoolingValue, "
                                      "FractionOfAutosizedHeatingValue, FlowPerCoolingCapacity, FlowPerHeatingCapacity, or None ");
                    errorsFound = true;
                }

                //       Fan operating mode (cycling or constant) schedule. IF constant fan, then set AirFlowControl
                if (thisSys.fanOpModeSchedPtr > 0) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.fanOpModeSchedPtr, ">=", 0.0, "<=", 0.0)) {
                        //           set fan operating mode to continuous so sizing can set VS coil data
                        thisSys.fanOpMode = DataHVACGlobals::ContFanCycCoil;
                        //           set air flow control mode:
                        //             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
                        //             UseCompressorOffFlow = operate at value specified by user
                        //           AirFlowControl only valid if fan opmode = ContFanCycComp
                        if (thisSys.maxNoCoolHeatAirVolFlow == 0.0) {
                            thisSys.airFlowControl = useCompFlow::useCompressorOnFlow;
                        } else {
                            thisSys.airFlowControl = useCompFlow::useCompressorOffFlow;
                        }
                    }
                }

                if (thisSys.coolingCoilType_Num != DataHVACGlobals::CoilDX_CoolingHXAssisted &&
                    thisSys.coolingCoilType_Num != DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl &&
                    thisSys.dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_Multimode) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    // ShowContinueError("Illegal " + cAlphaFields(iDehumidControlAlphaNum) + " = " + Alphas(iDehumidControlAlphaNum));
                    ShowContinueError("Multimode control must be used with a Heat Exchanger Assisted or Multimode Cooling Coil.");
                    if (loc_suppHeatCoilName == "" && loc_suppHeatCoilType == "") {
                    } else {
                        if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_UserDefined) {
                            ShowContinueError("Dehumidification control type is assumed to be None and the simulation continues.");
                            thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_None;
                        } else {
                            ShowContinueError("Dehumidification control type is assumed to be CoolReheat and the simulation continues.");
                            thisSys.dehumidControlType_Num = dehumCtrlTypeEnum::dehumidControl_CoolReheat;
                        }
                    }
                }

                //       Check placement of cooling coil with respect to fan placement and dehumidification control type

                if (thisSys.fanExists) {
                    if (thisSys.fanPlace == fanPlaceEnum::blowThru) {
                        if (FanOutletNode == HeatingCoilInletNode && thisSys.dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                            thisSys.coolingCoilUpstream = false;
                        }
                    } else if (thisSys.fanPlace == fanPlaceEnum::drawThru) {
                        if (HeatingCoilOutletNode == CoolingCoilInletNode &&
                            thisSys.dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                            thisSys.coolingCoilUpstream = false;
                        }
                    }
                } else {
                    if (HeatingCoilOutletNode == CoolingCoilInletNode &&
                        thisSys.dehumidControlType_Num != dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                        thisSys.coolingCoilUpstream = false;
                    }
                    if (ZoneEquipmentFound) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("ZoneHVAC equipment must contain a fan object.");
                        // ShowContinueError("specified " + cAlphaFields(iFanTypeAlphaNum) + " = " + Alphas(iFanTypeAlphaNum));
                        // ShowContinueError("specified " + cAlphaFields(iFanNameAlphaNum) + " = " + Alphas(iFanNameAlphaNum));
                        errorsFound = true;
                    }
                }

                // check node connections
                if (thisSys.fanPlace == fanPlaceEnum::blowThru) {

                    if (FanInletNode != thisSys.airInNode) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("When a blow through fan is specified, the fan inlet node name must be the same as the unitary system "
                                          "inlet node name.");
                        ShowContinueError("...Fan inlet node name           = " + DataLoopNode::NodeID(FanInletNode));
                        ShowContinueError("...UnitarySystem inlet node name = " + DataLoopNode::NodeID(thisSys.airInNode));
                        errorsFound = true;
                    }
                    if (thisSys.coolingCoilUpstream) {
                        if (FanOutletNode != CoolingCoilInletNode && thisSys.coolCoilExists && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a blow through fan is specified, the fan outlet node name must be the same as the cooling coil "
                                              "inlet node name.");
                            ShowContinueError("...Fan outlet node name         = " + DataLoopNode::NodeID(FanOutletNode));
                            ShowContinueError("...Cooling coil inlet node name = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != HeatingCoilInletNode && thisSys.coolCoilExists && thisSys.heatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Heating coil inlet node name  = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            errorsFound = true;
                        }
                        if (thisSys.suppCoilExists) {
                            if (SupHeatCoilOutletNode != thisSys.airOutNode) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError("The reheat coil outlet node name must be the same as the unitary system outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name   = " + DataLoopNode::NodeID(SupHeatCoilOutletNode));
                                ShowContinueError("...UnitarySystem outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                                //                ErrorsFound=.TRUE.
                            }
                        } else { // IF((thisSys%Humidistat ...
                            // Heating coil outlet node name must be the same as the Unitary system outlet node name
                            if (thisSys.heatCoilExists && HeatingCoilOutletNode != thisSys.airOutNode) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError("When a blow through fan is specified, the heating coil outlet node name must be the same as the "
                                                  "unitary system outlet node name.");
                                ShowContinueError("...Heating coil outlet node name  = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                                ShowContinueError("...Unitary system outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                                errorsFound = true;
                            }
                        }
                    } else { // IF(thisSys%CoolingCoilUpstream)THEN
                        if (FanOutletNode != HeatingCoilInletNode && thisSys.fanExists && thisSys.heatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a blow through fan is specified, the fan outlet node name must be the same as the heating coil "
                                              "inlet node name.");
                            ShowContinueError("...Fan outlet node name         = " + DataLoopNode::NodeID(FanOutletNode));
                            ShowContinueError("...Heating coil inlet node name = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            errorsFound = true;
                        }
                        if (HeatingCoilOutletNode != CoolingCoilInletNode && thisSys.coolCoilExists && thisSys.heatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The heating coil outlet node name must be the same as the cooling coil inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Cooling coil inlet node name  = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != thisSys.airOutNode && thisSys.coolCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "When a blow through fan is specified, the cooling coil outlet node name must be the same as the unitary "
                                "system outlet node name.");
                            ShowContinueError("...Cooling coil outlet node name   = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...UnitarySystem outlet node name  = " + DataLoopNode::NodeID(thisSys.airOutNode));
                            errorsFound = true;
                        }
                    }

                } else { // ELSE from IF(thisSys%FanPlace .EQ. BlowThru)THEN

                    if (thisSys.coolingCoilUpstream) {
                        if (CoolingCoilInletNode != thisSys.airInNode && CoolingCoilInletNode != 0 && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "When a draw through fan is specified, the cooling coil inlet node name must be the same as the unitary "
                                "system inlet node name.");
                            ShowContinueError("...Cooling coil inlet node name  = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            ShowContinueError("...UnitarySystem inlet node name = " + DataLoopNode::NodeID(thisSys.airInNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != HeatingCoilInletNode && thisSys.coolCoilExists && thisSys.heatCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The cooling coil outlet node name must be the same as the heating coil inlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Heating coil inlet node name  = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            errorsFound = true;
                        }
                        if (HeatingCoilOutletNode != FanInletNode && thisSys.heatCoilExists && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a draw through fan is specified, the heating coil outlet node name must be the same as the fan "
                                              "inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Fan inlet node name           = " + DataLoopNode::NodeID(FanInletNode));
                            errorsFound = true;
                        }
                        if (thisSys.suppCoilExists) {
                            if (FanOutletNode != SupHeatCoilInletNode && thisSys.fanExists) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError(
                                    "When a draw through fan is specified, the fan outlet node name must be the same as the reheat coil "
                                    "inlet node name.");
                                ShowContinueError("...Fan outlet node name        = " + DataLoopNode::NodeID(FanOutletNode));
                                ShowContinueError("...Reheat coil inlet node name = " + DataLoopNode::NodeID(SupHeatCoilInletNode));
                                //                ErrorsFound=.TRUE.
                            }
                            if (SupHeatCoilOutletNode != thisSys.airOutNode) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError("The reheat coil outlet node name must be the same as the unitary system outlet node name.");
                                ShowContinueError("...Reheat coil outlet node name   = " + DataLoopNode::NodeID(SupHeatCoilOutletNode));
                                ShowContinueError("...UnitarySystem outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                            }
                        } else {
                            if (FanOutletNode != thisSys.airOutNode && thisSys.fanExists) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                ShowContinueError(
                                    "When a draw through fan is specified, the fan outlet node name must be the same as the unitary system "
                                    "outlet node name.");
                                ShowContinueError("...Fan outlet node name        = " + DataLoopNode::NodeID(FanOutletNode));
                                ShowContinueError("...Unitary system outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                                errorsFound = true;
                            }
                        }
                    } else { // IF(thisSys%CoolingCoilUpstream)THEN
                        if (HeatingCoilInletNode != thisSys.airInNode && HeatingCoilInletNode != 0 && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError(
                                "When a draw through fan is specified, the heating coil inlet node name must be the same as the unitary "
                                "system inlet node name.");
                            ShowContinueError("...Heating coil inlet node name  = " + DataLoopNode::NodeID(HeatingCoilInletNode));
                            ShowContinueError("...UnitarySystem inlet node name = " + DataLoopNode::NodeID(thisSys.airInNode));
                            errorsFound = true;
                        }
                        if (HeatingCoilOutletNode != CoolingCoilInletNode && thisSys.heatCoilExists && thisSys.coolCoilExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("The heating coil outlet node name must be the same as the cooling coil inlet node name.");
                            ShowContinueError("...Heating coil outlet node name = " + DataLoopNode::NodeID(HeatingCoilOutletNode));
                            ShowContinueError("...Cooling coil inlet node name  = " + DataLoopNode::NodeID(CoolingCoilInletNode));
                            errorsFound = true;
                        }
                        if (CoolingCoilOutletNode != FanInletNode && thisSys.coolCoilExists && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a draw through fan is specified, the cooling coil outlet node name must be the same as the fan "
                                              "inlet node name.");
                            ShowContinueError("...Cooling coil outlet node name = " + DataLoopNode::NodeID(CoolingCoilOutletNode));
                            ShowContinueError("...Fan inlet node name           = " + DataLoopNode::NodeID(FanInletNode));
                            errorsFound = true;
                        }
                        if (FanOutletNode != thisSys.airOutNode && thisSys.fanExists) {
                            ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                            ShowContinueError("When a draw through fan is specified, the fan outlet node name must be the same as the unitary system "
                                              "outlet node name.");
                            ShowContinueError("...Fan outlet node name           = " + DataLoopNode::NodeID(FanOutletNode));
                            ShowContinueError("...UnitarySystem outlet node name = " + DataLoopNode::NodeID(thisSys.airOutNode));
                            errorsFound = true;
                        }
                    }
                } // ELSE from IF(thisSys%FanPlace .EQ. BlowThru)THEN

                // Set the unitary system supplemental heater max outlet temperature
                // this field will be 0 if the input is not specified (included) in the input file
                // someone may use a default other than what we intended, allow it to be used
                // so if this field is blank, and the input field is included, read the default, otherwise use 80
                // if (!lNumericBlanks(iDesignMaxOutletTempNumericNum) && NumNumbers > (iDesignMaxOutletTempNumericNum - 1)) {
                thisSys.designMaxOutletTemp = loc_designMaxOutletTemp;
                if (thisSys.designMaxOutletTemp == DataSizing::AutoSize) thisSys.requestAutoSize = true;
                //}

                // Set maximum Outdoor air temperature for supplemental heating coil operation
                // this field will be 0 if the input is not specified (included) in the input file
                // someone may use a default other than what we intended, allow it to be used
                // so if this field is blank, and the input field is included, read the default, otherwise use 9999
                // if (!lNumericBlanks(iMaxOATSuppHeatNumericNum) && NumNumbers > (iMaxOATSuppHeatNumericNum - 1)) {
                thisSys.maxOATSuppHeat = loc_maxOATSuppHeat;
                // Can't let MaxOATSuppHeat default to 21C if using cool reheat since it would shut off supp heater when dehumidifying
                // this may also allow supplemental heater to operate when in heating mode when it should not
                if (thisSys.maxOATSuppHeat == 21.0 && thisSys.dehumidControlType_Num == dehumCtrlTypeEnum::dehumidControl_CoolReheat) {
                    thisSys.maxOATSuppHeat = 999.0;
                }

                if (thisSys.maxCoolAirVolFlow > 0.0 && thisSys.maxHeatAirVolFlow > 0.0 && thisSys.maxNoCoolHeatAirVolFlow >= 0.0 &&
                    !thisSys.requestAutoSize) {
                    thisSys.designFanVolFlowRate = max(thisSys.maxCoolAirVolFlow, thisSys.maxHeatAirVolFlow, thisSys.maxNoCoolHeatAirVolFlow);
                } else if (thisSys.maxCoolAirVolFlow > 0.0 && thisSys.maxNoCoolHeatAirVolFlow >= 0.0 && !thisSys.requestAutoSize) {
                    thisSys.designFanVolFlowRate = max(thisSys.maxCoolAirVolFlow, thisSys.maxNoCoolHeatAirVolFlow);
                } else if (thisSys.maxHeatAirVolFlow > 0.0 && thisSys.maxNoCoolHeatAirVolFlow >= 0.0 && !thisSys.requestAutoSize) {
                    thisSys.designFanVolFlowRate = max(thisSys.maxHeatAirVolFlow, thisSys.maxNoCoolHeatAirVolFlow);
                } else {
                    if (thisSys.fanExists && thisSys.designFanVolFlowRate == 0.0) {
                        thisSys.designFanVolFlowRate = DataSizing::AutoSize;
                    }
                    // need more of this type of warning when flow cannot be determined
                    if (thisSys.maxHeatAirVolFlow == 0.0 && thisSys.heatCoilExists) {
                        if (thisSys.fanExists) {
                            if (thisSys.coolCoilExists && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize) {
                                if (thisSys.maxCoolAirVolFlow == 0.0) {
                                    thisSys.maxHeatAirVolFlow = thisSys.designFanVolFlowRate;
                                }
                            }
                        } else if (thisSys.coolCoilExists) {
                            thisSys.maxHeatAirVolFlow = thisSys.maxCoolAirVolFlow;
                        } else {
                            if (thisSys.heatingCoilType_Num != DataHVACGlobals::CoilDX_HeatingEmpirical &&
                                thisSys.heatingCoilType_Num != DataHVACGlobals::CoilDX_MultiSpeedHeating &&
                                thisSys.heatingCoilType_Num != DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                                ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                                // ShowContinueError("When non-DX heating coils are specified, the heating air flow rate must be entered in " +
                                //                  cAlphaFields(iHeatSAFMAlphaNum));
                                errorsFound = true;
                            }
                        }
                    } else if (thisSys.maxHeatAirVolFlow == 0.0 && !thisSys.fanExists && !thisSys.coolCoilExists) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("When non-DX heating coils are specified, the heating air flow rate must be entered in " +
                        //                  cAlphaFields(iHeatSAFMAlphaNum));
                    }
                }

                if (FanVolFlowRate != DataSizing::AutoSize && thisSys.fanExists) {
                    if (FanVolFlowRate < thisSys.maxCoolAirVolFlow && thisSys.maxCoolAirVolFlow != DataSizing::AutoSize && thisSys.coolCoilExists) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... air flow rate = " + General::TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + thisSys.fanName +
                                          " is less than the maximum HVAC system air flow rate in cooling mode.");
                        // ShowContinueError(" The " + cNumericFields(iMaxCoolAirVolFlowNumericNum) +
                        //                  " is reset to the fan flow rate and the simulation continues.");
                        thisSys.maxCoolAirVolFlow = FanVolFlowRate;
                        thisSys.designFanVolFlowRate = FanVolFlowRate;
                    }
                    if (FanVolFlowRate < thisSys.maxHeatAirVolFlow && thisSys.maxHeatAirVolFlow != DataSizing::AutoSize && thisSys.heatCoilExists) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... air flow rate = " + General::TrimSigDigits(FanVolFlowRate, 7) + " in fan object " + thisSys.fanName +
                                          " is less than the maximum HVAC system air flow rate in heating mode.");
                        // ShowContinueError(" The " + cNumericFields(3) + " is reset to the fan flow rate and the simulation continues.");
                        thisSys.maxHeatAirVolFlow = FanVolFlowRate;
                        thisSys.designFanVolFlowRate = FanVolFlowRate;
                    }
                }

                if (thisSys.fanOpModeSchedPtr > 0) {
                    if (!ScheduleManager::CheckScheduleValueMinMax(thisSys.fanOpModeSchedPtr, ">=", 0.0, "<=", 0.0)) {
                        //           set air flow control mode:
                        //             UseCompressorOnFlow = operate at last cooling or heating air flow requested when compressor is off
                        //             UseCompressorOffFlow = operate at value specified by user
                        //           AirFlowControl only valid IF fan opmode = ContFanCycComp
                        if (thisSys.maxNoCoolHeatAirVolFlow == 0.0) {
                            thisSys.airFlowControl = useCompFlow::useCompressorOnFlow;
                        } else {
                            thisSys.airFlowControl = useCompFlow::useCompressorOffFlow;
                        }
                    }
                }

                // Set minimum OAT for heat pump compressor operation in cooling mode
                // get from coil module
                errFlag = false;
                if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                    thisSys.minOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoSpeed) {
                    thisSys.minOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                    thisSys.minOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingTwoStageWHumControl) {
                    thisSys.minOATCompressorCooling = DXCoils::GetMinOATCompressor(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    thisSys.minOATCompressorCooling = VariableSpeedCoils::GetVSCoilMinOATCompressor(loc_coolingCoilName, errFlag);
                } else {
                    thisSys.minOATCompressorCooling = -1000.0;
                }
                if (errFlag) {
                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                    errorsFound = true;
                }

                // Set minimum OAT for heat pump compressor operation in heating mode
                // get from coil module
                errFlag = false;
                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                    thisSys.minOATCompressorHeating = VariableSpeedCoils::GetVSCoilMinOATCompressor(loc_heatingCoilName, errFlag);
                } else if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_HeatingEmpirical ||
                           thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                    thisSys.minOATCompressorHeating = DXCoils::GetMinOATCompressor(loc_heatingCoilType, loc_heatingCoilName, errFlag);
                    //       ELSEIF  ***... make sure we catch all possbile coil types here ...***
                } else {
                    thisSys.minOATCompressorHeating = -1000.0;
                }
                if (errFlag) {
                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                    errorsFound = true;
                }

                //       Mine heatpump Outdoor condenser node from DX coil object
                errFlag = false;
                if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                    thisSys.condenserNodeNum = DXCoils::GetCoilCondenserInletNode(loc_coolingCoilType, loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    thisSys.condenserNodeNum = VariableSpeedCoils::GetVSCoilCondenserInletNode(loc_coolingCoilName, errFlag);
                } else if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_CoolingHXAssisted) {
                    // already filled
                    // UnitarySystem( UnitarySysNum ).CondenserNodeNum = GetDXCoilCondenserInletNode( "Coil:Cooling:DX:SingleSpeed",
                    // GetHXDXCoilName( CoolingCoilType, loc_coolingCoilName, errFlag ), errFlag );

                } else {
                    if (loc_condenserInletNodeName != "") {
                        thisSys.condenserNodeNum = NodeInputManager::GetOnlySingleNode(loc_condenserInletNodeName,
                                                                                       errFlag,
                                                                                       cCurrentModuleObject,
                                                                                       thisObjectName,
                                                                                       DataLoopNode::NodeType_Air,
                                                                                       DataLoopNode::NodeConnectionType_Inlet,
                                                                                       1,
                                                                                       DataLoopNode::ObjectIsParent);
                    } else {
                        // do nothing?
                    }
                }
                if (errFlag) {
                    ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                    errorsFound = true;
                }

                // Set the heatpump cycling rate
                thisSys.maxONOFFCyclesperHour = loc_maxONOFFCyclesperHour;
                // if (NumNumbers < iMaxONOFFCycPerHourNumericNum) {
                //    thisSys.maxONOFFCyclesperHour = 2.5;
                //}

                // Set the heat pump time constant
                thisSys.HPTimeConstant = loc_HPTimeConstant;
                // if (NumNumbers < iHPTimeConstantNumericNum) {
                //    thisSys.HPTimeConstant = 60.0;
                //}

                // Set the heat pump on-cycle power use fraction
                thisSys.onCyclePowerFraction = loc_onCyclePowerFraction;
                // if (NumNumbers < iOnCyclePowerFracNumericNum) {
                //    thisSys.OnCyclePowerFraction = 0.01;
                //}

                // Set the heat pump fan delay time
                thisSys.fanDelayTime = loc_fanDelayTime;
                // if (NumNumbers < iFanDelayTimeNumericNum) {
                //    thisSys.FanDelayTime = 60.0;
                //}

                thisSys.ancillaryOnPower = loc_ancillaryOnPower;
                thisSys.ancillaryOffPower = loc_ancillaryOffPower;

                thisSys.designHRWaterVolumeFlow = loc_designHRWaterVolumeFlow;
                thisSys.maxHROutletWaterTemp = loc_maxHROutletWaterTemp;

                if (thisSys.designHRWaterVolumeFlow > 0.0) {
                    thisSys.heatRecActive = true;
                    errFlag = false;
                    if (loc_heatRecoveryInletNodeName != "" && loc_heatRecoveryOutletNodeName != "") {
                        thisSys.heatRecoveryInletNodeNum = NodeInputManager::GetOnlySingleNode(loc_heatRecoveryInletNodeName,
                                                                                               errFlag,
                                                                                               cCurrentModuleObject,
                                                                                               thisObjectName,
                                                                                               DataLoopNode::NodeType_Water,
                                                                                               DataLoopNode::NodeConnectionType_Inlet,
                                                                                               3,
                                                                                               DataLoopNode::ObjectIsNotParent);
                        thisSys.heatRecoveryOutletNodeNum = NodeInputManager::GetOnlySingleNode(loc_heatRecoveryOutletNodeName,
                                                                                                errFlag,
                                                                                                cCurrentModuleObject,
                                                                                                thisObjectName,
                                                                                                DataLoopNode::NodeType_Water,
                                                                                                DataLoopNode::NodeConnectionType_Outlet,
                                                                                                3,
                                                                                                DataLoopNode::ObjectIsNotParent);

                        BranchNodeConnections::TestCompSet(cCurrentModuleObject,
                                                           thisObjectName,
                                                           loc_heatRecoveryInletNodeName,
                                                           loc_heatRecoveryOutletNodeName,
                                                           "Unitary System Heat Recovery Nodes");

                        if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                            DXCoils::SetMSHPDXCoilHeatRecoveryFlag(thisSys.coolingCoilIndex);
                        }
                        if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating) {
                            DXCoils::SetMSHPDXCoilHeatRecoveryFlag(thisSys.heatingCoilIndex);
                        }
                        if (errFlag) {
                            ShowContinueError("Occurs in " + cCurrentModuleObject + " = " + thisObjectName);
                            errorsFound = true;
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Illegal " + cAlphaFields(iHRWaterInletNodeAlphaNum) + " = " + Alphas(iHRWaterInletNodeAlphaNum));
                        // ShowContinueError("Illegal " + cAlphaFields(iHRWaterOutletNodeAlphaNum) + " = " + Alphas(iHRWaterOutletNodeAlphaNum));
                        // ShowContinueError("... heat recovery nodes must be specified when " + cNumericFields(iDesignHRWaterVolFlowNumericNum) +
                        //                  " is greater than 0.");
                        // ShowContinueError("... " + cNumericFields(iDesignHRWaterVolFlowNumericNum) + " = " +
                        //                  TrimSigDigits(thisSys.DesignHRWaterVolumeFlow, 7));
                        errorsFound = true;
                    }
                }

                if (loc_designSpecMultispeedHPType != "" && loc_designSpecMultispeedHPName != "") {
                    thisSys.designSpecMultispeedHPType = loc_designSpecMultispeedHPType;
                    thisSys.designSpecMultispeedHPName = loc_designSpecMultispeedHPName;
                    int designSpecType_Num = 1;

                    DesignSpecMSHP thisDesignSpec;
                    thisSys.compPointerMSHP = thisDesignSpec.factory(designSpecType_Num, loc_designSpecMultispeedHPName);
                    thisSys.designSpecMSHPIndex = getDesignSpecMSHPIndex(thisSys.designSpecMultispeedHPName);

                    if (thisSys.compPointerMSHP != nullptr) {

                        thisSys.noLoadAirFlowRateRatio = thisSys.compPointerMSHP->noLoadAirFlowRateRatio;

                        if (thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating ||
                            thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingElectric_MultiStage ||
                            thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGas_MultiStage) {
                            thisSys.numOfSpeedHeating = thisSys.compPointerMSHP->numOfSpeedHeating;
                            thisSys.heatMassFlowRate.resize(thisSys.numOfSpeedHeating);
                            thisSys.heatVolumeFlowRate.resize(thisSys.numOfSpeedHeating);
                            thisSys.MSHeatingSpeedRatio.resize(thisSys.numOfSpeedHeating);
                            for (int i = 0; i < thisSys.numOfSpeedHeating; ++i) {
                                thisSys.heatMassFlowRate[i] = 0.0;
                                thisSys.heatVolumeFlowRate[i] = 0.0;
                                thisSys.MSHeatingSpeedRatio[i] = 1.0;
                            }
                        }

                        if (thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) {
                            thisSys.numOfSpeedCooling = thisSys.compPointerMSHP->numOfSpeedCooling;
                            thisSys.coolMassFlowRate.resize(thisSys.numOfSpeedCooling);
                            thisSys.coolVolumeFlowRate.resize(thisSys.numOfSpeedCooling);
                            thisSys.MSCoolingSpeedRatio.resize(thisSys.numOfSpeedCooling);
                            for (int i = 0; i < thisSys.numOfSpeedCooling; ++i) {
                                thisSys.coolMassFlowRate[i] = 0.0;
                                thisSys.coolVolumeFlowRate[i] = 0.0;
                                thisSys.MSCoolingSpeedRatio[i] = 1.0;
                            }
                        }
                    } else {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... one or both of the following inputs are invalid.");
                        // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPTypeAlphaNum) + " = " + Alphas(iDesignSpecMSHPTypeAlphaNum));
                        // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPNameAlphaNum) + " = " + Alphas(iDesignSpecMSHPNameAlphaNum));
                        errorsFound = true;
                    }
                } else if ((loc_designSpecMultispeedHPType == "" && loc_designSpecMultispeedHPName != "") ||
                           (loc_designSpecMultispeedHPType != "" && loc_designSpecMultispeedHPName == "")) {
                    ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                    ShowContinueError("... one or both of the following inputs are invalid.");
                    // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPTypeAlphaNum) + " = " + Alphas(iDesignSpecMSHPTypeAlphaNum));
                    // ShowContinueError("Field " + cAlphaFields(iDesignSpecMSHPNameAlphaNum) + " = " + Alphas(iDesignSpecMSHPNameAlphaNum));
                    errorsFound = true;
                    //} else if (thisSys.numOfSpeedHeating > 0) { // how do these last 2 get called?
                    //    int numOfSpeedHeating = thisSys.numOfSpeedHeating;

                    //    thisSys.heatMassFlowRate.allocate(numOfSpeedHeating);
                    //    thisSys.heatVolumeFlowRate.allocate(numOfSpeedHeating);
                    //    thisSys.MSHeatingSpeedRatio.allocate(numOfSpeedHeating);
                    //    thisSys.MSHeatingSpeedRatio = 1.0;

                    //} else if (thisSys.numOfSpeedCooling > 0) {
                    //    int numOfSpeedCooling = thisSys.numOfSpeedCooling;

                    //    thisSys.coolMassFlowRate.allocate(numOfSpeedCooling);
                    //    thisSys.coolVolumeFlowRate.allocate(numOfSpeedCooling);
                    //    thisSys.MSCoolingSpeedRatio.allocate(numOfSpeedCooling);
                    //    thisSys.MSCoolingSpeedRatio = 1.0;
                }

                if (thisSys.multiSpeedCoolingCoil) {

                    // int designSpecIndex = thisSys.designSpecMSHPIndex;
                    // if (designSpecIndex > 0) thisSys.numOfSpeedCooling = DesignSpecMSHP.numOfSpeedCooling;

                    if (thisSys.numOfSpeedCooling == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... Cooling coil object type requires valid " + unitarySysHeatPumpPerformanceObjectType +
                                          " for cooling to be specified with number of speeds > 0");
                        errorsFound = true;
                    }
                }
                if (thisSys.multiSpeedHeatingCoil) {

                    // int designSpecIndex = thisSys.designSpecMSHPIndex;
                    // if (designSpecIndex > 0) thisSys.numOfSpeedHeating = DesignSpecMSHP(designSpecIndex).NumOfSpeedHeating;

                    if (thisSys.numOfSpeedHeating == 0) {
                        ShowSevereError(cCurrentModuleObject + " = " + thisObjectName);
                        ShowContinueError("... Heating coil object type requires valid " + unitarySysHeatPumpPerformanceObjectType +
                                          " for heating to be specified with number of speeds > 0");
                        errorsFound = true;
                    }
                }

                if ((thisSys.heatingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedHeating &&
                     thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling) ||
                    (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingGasOrOtherFuel &&
                     thisSys.coolingCoilType_Num == DataHVACGlobals::CoilDX_MultiSpeedCooling)) {
                    // Index = thisSys.designSpecMSHPIndex;
                    if (thisSys.compPointerMSHP != nullptr) {
                        if (thisSys.compPointerMSHP->singleModeFlag) {
                            thisSys.singleMode = 1;
                        }
                    }
                } else {
                    if (thisSys.compPointerMSHP != nullptr) {
                        if (thisSys.compPointerMSHP->singleModeFlag) {
                            ShowSevereError(cCurrentModuleObject + ": " + thisObjectName);
                            ShowContinueError(
                                "In order to perform Single Mode Operation, the valid cooling coil type is Coil:Cooling:DX:MultiSpeed and "
                                "the valid heating is Coil:Heating:DX:MultiSpeed or Coil:Heating:Fuel.");
                            // ShowContinueError("The input cooling coil type = " + Alphas(iCoolingCoilTypeAlphaNum) +
                            //                  " and the input heating coil type = " + Alphas(iHeatingCoilTypeAlphaNum));
                        }
                    }
                }

                if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingAirToAirVariableSpeed) {
                    VariableSpeedCoils::SetVarSpeedCoilData(thisSys.coolingCoilIndex, errorsFound, _, _, thisSys.designSpecMSHPIndex);
                }

                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingAirToAirVariableSpeed) {
                    VariableSpeedCoils::SetVarSpeedCoilData(thisSys.heatingCoilIndex, errorsFound, _, _, thisSys.designSpecMSHPIndex);
                }

                // set global logicals that denote coil type
                if (thisSys.multiSpeedHeatingCoil || thisSys.varSpeedHeatingCoil) {
                    thisSys.multiOrVarSpeedHeatCoil = true;
                }
                if (thisSys.multiSpeedCoolingCoil || thisSys.varSpeedCoolingCoil) {
                    thisSys.multiOrVarSpeedCoolCoil = true;
                }

                // set global variables for multi-stage chilled and hot water coils
                if (thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWater ||
                    thisSys.coolingCoilType_Num == DataHVACGlobals::Coil_CoolingWaterDetailed) {
                    // int designSpecIndex = thisSys.designSpecMSHPIndex;
                    if (thisSys.compPointerMSHP != nullptr) {
                        thisSys.numOfSpeedCooling = thisSys.compPointerMSHP->numOfSpeedCooling;
                        if (thisSys.numOfSpeedCooling > 1) {
                            thisSys.multiSpeedCoolingCoil = true;
                            thisSys.multiOrVarSpeedCoolCoil = true;
                        }
                    }
                }
                if (thisSys.heatingCoilType_Num == DataHVACGlobals::Coil_HeatingWater) {
                    // designSpecIndex = thisSys.designSpecMSHPIndex;
                    if (thisSys.compPointerMSHP != nullptr) {
                        thisSys.numOfSpeedHeating = thisSys.compPointerMSHP->numOfSpeedHeating;
                        if (thisSys.numOfSpeedHeating > 1) {
                            thisSys.multiSpeedHeatingCoil = true;
                            thisSys.multiOrVarSpeedHeatCoil = true;
                        }
                    }
                }

                // check for specific input requirements for ASHRAE90.1 model
                if (thisSys.controlType == controlTypeEnum::controlTypeCCMASHRAE) {

                    // only allowed for water and DX cooling coils at this time
                    if (thisSys.coolCoilExists && thisSys.coolingCoilType_Num != DataHVACGlobals::Coil_CoolingWater &&
                        thisSys.coolingCoilType_Num != DataHVACGlobals::Coil_CoolingWaterDetailed &&
                        thisSys.coolingCoilType_Num != DataHVACGlobals::CoilDX_CoolingSingleSpeed) {
                        if (DataGlobals::DisplayExtraWarnings) {
                            ShowWarningError(cCurrentModuleObject + ": " + thisObjectName);
                            ShowContinueError("ASHRAE90.1 control method requires specific cooling coil types.");
                            ShowContinueError("Valid cooling coil types are Coil:Cooling:Water, Coil:Cooling:Water:DetailedGeometry and "
                                              "Coil:Cooling:DX:SingleSpeed.");
                            // ShowContinueError("The input cooling coil type = " + Alphas(iCoolingCoilTypeAlphaNum) +
                            //                  ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                        }
                        // mark this coil as non-ASHRAE90 type
                        thisSys.validASHRAECoolCoil = false;
                    }
                    // only allow for water, fuel, or electric at this time
                    if (thisSys.heatCoilExists && thisSys.heatingCoilType_Num != DataHVACGlobals::Coil_HeatingWater &&
                        thisSys.heatingCoilType_Num != DataHVACGlobals::Coil_HeatingGasOrOtherFuel &&
                        thisSys.heatingCoilType_Num != DataHVACGlobals::Coil_HeatingElectric &&
                        thisSys.heatingCoilType_Num != DataHVACGlobals::CoilDX_HeatingEmpirical) {
                        if (DataGlobals::DisplayExtraWarnings) {
                            ShowWarningError(cCurrentModuleObject + ": " + thisObjectName);
                            ShowContinueError("ASHRAE90.1 control method requires specific heating coil types.");
                            ShowContinueError("Valid heating coil types are Coil:Heating:Water, Coil:Heating:Fuel, Coil:Heating:Electric and "
                                              "Coil:Heating:DX:SingleSpeed.");
                            // ShowContinueError("The input heating coil type = " + Alphas(iHeatingCoilTypeAlphaNum) +
                            //                  ". This coil will not be modeled using the ASHRAE 90.1 algorithm.");
                        }
                        // mark this coil as non-ASHRAE90 type
                        thisSys.validASHRAEHeatCoil = false;
                    }
                    if (thisSys.dehumidControlType_Num == dehumidControl_Multimode || thisSys.dehumidControlType_Num == dehumidControl_CoolReheat) {
                        ShowWarningError(cCurrentModuleObject + ": " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iDehumidControlAlphaNum) + " = " +
                        // Alphas(iDehumidControlAlphaNum));
                        ShowContinueError(
                            "ASHRAE90.1 control method does not support dehumidification at this time. Dehumidification control type is "
                            "assumed to be None.");
                        thisSys.dehumidControlType_Num = dehumidControl_None;
                    }
                    if (thisSys.runOnLatentLoad) {
                        ShowWarningError(cCurrentModuleObject + " = " + thisObjectName);
                        // ShowContinueError("Invalid entry for " + cAlphaFields(iRunOnLatentLoadAlphaNum) + " :" +
                        // Alphas(iRunOnLatentLoadAlphaNum));
                        ShowContinueError(
                            "ASHRAE90.1 control method does not support latent load control at this time. This input must be selected as "
                            "SensibleOnlyLoadControl.");
                        thisSys.runOnSensibleLoad = true;
                        thisSys.runOnLatentLoad = false;
                        thisSys.runOnLatentOnlyWithSensible = false;
                    }
                }

                unitarySys.push_back(thisSys);
            }
        }
    }

} // namespace UnitarySystems
} // namespace EnergyPlus
