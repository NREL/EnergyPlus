#include <ObjexxFCL/Array1D.hh> // needs to be in BranchNodeConnections.hh

#include <BranchNodeConnections.hh>
#include <Coils/CoilCoolingDX.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

namespace EnergyPlus {
std::vector<CoilCoolingDX> coilCoolingDXs;
}

void CoilCoolingDX::instantiateFromInputSpec(CoilCoolingDXInputSpecification input_data)
{
    static const std::string routineName("CoilCoolingDX::instantiateFromInputSpec: ");
    this->original_input_specs = input_data;
    bool errorsFound = false;
    this->name = input_data.name;
    this->performance = CoilCoolingDXCurveFitPerformance(input_data.performance_object_name);

    // other construction below
    this->evapInletNodeIndex = NodeInputManager::GetOnlySingleNode(input_data.evaporator_inlet_node_name,
                                                                   errorsFound,
                                                                   this->object_name,
                                                                   input_data.name,
                                                                   DataLoopNode::NodeType_Air,
                                                                   DataLoopNode::NodeConnectionType_Inlet,
                                                                   1,
                                                                   DataLoopNode::ObjectIsNotParent);
    this->evapOutletNodeIndex = NodeInputManager::GetOnlySingleNode(input_data.evaporator_outlet_node_name,
                                                                    errorsFound,
                                                                    this->object_name,
                                                                    input_data.name,
                                                                    DataLoopNode::NodeType_Air,
                                                                    DataLoopNode::NodeConnectionType_Outlet,
                                                                    1,
                                                                    DataLoopNode::ObjectIsNotParent);
    if (input_data.condenser_inlet_node_name != "") {
        this->condInletNodeIndex = NodeInputManager::GetOnlySingleNode(input_data.condenser_inlet_node_name,
                                                                       errorsFound,
                                                                       this->object_name,
                                                                       input_data.name,
                                                                       DataLoopNode::NodeType_Air,
                                                                       DataLoopNode::NodeConnectionType_Inlet,
                                                                       2,
                                                                       DataLoopNode::ObjectIsNotParent);
        if (!OutAirNodeManager::CheckOutAirNodeNumber(this->condInletNodeIndex)) {
            ShowWarningError(routineName + this->object_name + "=\"" + this->name + "\", may be invalid");
            ShowContinueError("Condenser Inlet Node Name=\"" + input_data.condenser_inlet_node_name +
                "\", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
            ShowContinueError(
                "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues");
        }

    } else {
        this->condInletNodeIndex = 0;
    }
    if (input_data.condenser_outlet_node_name != "") {
        this->condOutletNodeIndex = NodeInputManager::GetOnlySingleNode(input_data.condenser_outlet_node_name,
                                                                        errorsFound,
                                                                        this->object_name,
                                                                        input_data.name,
                                                                        DataLoopNode::NodeType_Air,
                                                                        DataLoopNode::NodeConnectionType_Outlet,
                                                                        2,
                                                                        DataLoopNode::ObjectIsNotParent);
        if (!OutAirNodeManager::CheckOutAirNodeNumber(this->condOutletNodeIndex)) {
            ShowWarningError(routineName + this->object_name + "=\"" + this->name + "\", may be invalid");
            ShowContinueError("Condenser Outlet Node Name=\"" + input_data.condenser_outlet_node_name +
                "\", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.");
            ShowContinueError(
                "This node needs to be included in an air system or the coil model will not be valid, and the simulation continues");
        }

    } else {
        this->condOutletNodeIndex = 0;
    }
    if (input_data.availability_schedule_name == "") {
      this->availScheduleIndex = DataGlobals::ScheduleAlwaysOn;
    } else {
      this->availScheduleIndex = ScheduleManager::GetScheduleIndex(input_data.availability_schedule_name);
    }
    if (this->availScheduleIndex == 0) {
        ShowSevereError(routineName + this->object_name + "=\"" + this->name + "\", invalid");
        ShowContinueError("...Availability Schedule Name=\"" + input_data.availability_schedule_name + "\".");
        errorsFound = true;
    }

    BranchNodeConnections::TestCompSet(
        CoilCoolingDX::object_name, this->name, input_data.evaporator_inlet_node_name, input_data.evaporator_outlet_node_name, "Air Nodes");

    if (errorsFound) {
        ShowFatalError(routineName + "Errors found in getting " + this->object_name + " input. Preceding condition(s) causes termination.");
    }

}

void CoilCoolingDX::onetimeinit(){

    // setup output variables, needs to be done after object is instantiated and emplaced
    SetupOutputVariable("Cooling Coil Total Cooling Rate", OutputProcessor::Unit::W, this->totalCoolingEnergyRate, "System", "Average", this->name);
    SetupOutputVariable("Cooling Coil Total Cooling Energy",
                        OutputProcessor::Unit::J,
                        this->totalCoolingEnergy,
                        "System",
                        "Sum",
                        this->name,
                        _,
                        "ENERGYTRANSFER",
                        "COOLINGCOILS",
                        _,
                        "System");
    SetupOutputVariable("Cooling Coil Sensible Cooling Rate", OutputProcessor::Unit::W, this->sensCoolingEnergyRate, "System", "Average", this->name);
    SetupOutputVariable("Cooling Coil Sensible Cooling Energy", OutputProcessor::Unit::J, this->sensCoolingEnergy, "System", "Sum", this->name);
    SetupOutputVariable("Cooling Coil Latent Cooling Rate", OutputProcessor::Unit::W, this->latCoolingEnergyRate, "System", "Average", this->name);
    SetupOutputVariable("Cooling Coil Latent Cooling Energy", OutputProcessor::Unit::J, this->latCoolingEnergy, "System", "Sum", this->name);
    SetupOutputVariable("Cooling Coil Electric Power", OutputProcessor::Unit::W, this->elecCoolingPower, "System", "Average", this->name);
    SetupOutputVariable("Cooling Coil Electric Energy",
                        OutputProcessor::Unit::J,
                        this->elecCoolingConsumption,
                        "System",
                        "Sum",
                        this->name,
                        _,
                        "Electric",
                        "COOLING",
                        _,
                        "System");
    SetupOutputVariable(
        "Cooling Coil Runtime Fraction", OutputProcessor::Unit::None, this->coolingCoilRuntimeFraction, "System", "Average", this->name);
}

CoilCoolingDX::CoilCoolingDX(std::string name_to_find)
    : myOneTimeInitFlag(true), evapInletNodeIndex(0), evapOutletNodeIndex(0), availScheduleIndex(0), condZoneIndex(0),
      condInletNodeIndex(0), condOutletNodeIndex(0), condensateTankIndex(0), evaporativeCondSupplyTankIndex(0),

      // report variables
      totalCoolingEnergyRate(0.0), totalCoolingEnergy(0.0), sensCoolingEnergyRate(0.0), sensCoolingEnergy(0.0), latCoolingEnergyRate(0.0),
      latCoolingEnergy(0.0), elecCoolingPower(0.0), elecCoolingConsumption(0.0), coolingCoilRuntimeFraction(0.0)

{
    int numCoolingCoilDXs = inputProcessor->getNumObjectsFound(CoilCoolingDX::object_name);
    if (numCoolingCoilDXs <= 0) {
        // error
    }
    bool found_it = false;
    for (int coilNum = 1; coilNum <= numCoolingCoilDXs; ++coilNum) {
        int NumAlphas;  // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        inputProcessor->getObjectItem(CoilCoolingDX::object_name, coilNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus);
        if (!UtilityRoutines::SameString(name_to_find, cAlphaArgs(1))) {
            continue;
        }
        found_it = true;

        CoilCoolingDXInputSpecification input_specs;
        input_specs.name = cAlphaArgs(1);
        input_specs.evaporator_inlet_node_name = cAlphaArgs(2);
        input_specs.evaporator_outlet_node_name = cAlphaArgs(3);
        input_specs.availability_schedule_name = cAlphaArgs(4);
        input_specs.condenser_zone_name = cAlphaArgs(5);
        input_specs.condenser_inlet_node_name = cAlphaArgs(6);
        input_specs.condenser_outlet_node_name = cAlphaArgs(7);
        input_specs.performance_object_name = cAlphaArgs(8);
        input_specs.condensate_collection_water_storage_tank_name = cAlphaArgs(9);
        input_specs.evaporative_condenser_supply_water_storage_tank_name = cAlphaArgs(10);

        this->instantiateFromInputSpec(input_specs);
        if (found_it) break;
    }

    if (!found_it) {
        // error
    }
}

void CoilCoolingDX::simulate(bool useAlternateMode, Real64 PLR, int speedNum, Real64 speedRatio, int fanOpMode)
{
    if (this->myOneTimeInitFlag) {
        onetimeinit();
        this->myOneTimeInitFlag = false;
    }

    // get inlet conditions from inlet node
    auto &evapInletNode = DataLoopNode::Node(this->evapInletNodeIndex);
    auto &evapOutletNode = DataLoopNode::Node(this->evapOutletNodeIndex);

    // call the simulation, which returns useful data
    this->performance.simulate(evapInletNode, evapOutletNode, useAlternateMode, PLR, speedNum, speedRatio, fanOpMode);
    this->passThroughNodeData(evapInletNode, evapOutletNode);

    // calculate energy conversion factor
    Real64 reportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    // update report variables
    this->totalCoolingEnergyRate = evapOutletNode.MassFlowRate * (evapInletNode.Enthalpy - evapOutletNode.Enthalpy);
    this->totalCoolingEnergy = this->totalCoolingEnergyRate * reportingConstant;
    Real64 minAirHumRat = min(evapInletNode.HumRat, evapOutletNode.HumRat);
    this->sensCoolingEnergyRate = evapOutletNode.MassFlowRate * (Psychrometrics::PsyHFnTdbW(evapInletNode.Temp, minAirHumRat) -
                                                                 Psychrometrics::PsyHFnTdbW(evapOutletNode.Temp, minAirHumRat));
    this->sensCoolingEnergy = this->sensCoolingEnergyRate * reportingConstant;
    this->latCoolingEnergyRate = this->totalCoolingEnergyRate - this->sensCoolingEnergyRate;
    this->latCoolingEnergy = this->latCoolingEnergyRate * reportingConstant;
    this->coolingCoilRuntimeFraction = this->performance.RTF;
    this->elecCoolingPower = this->performance.powerUse;
    this->elecCoolingConsumption = this->performance.powerUse * reportingConstant;
}

void CoilCoolingDX::passThroughNodeData(EnergyPlus::DataLoopNode::NodeData &in,
                                        EnergyPlus::DataLoopNode::NodeData &out) {
    // pass through all the other node variables that we don't update as a part of this model calculation
    out.MassFlowRate = in.MassFlowRate;
    out.Press = in.Press;
    out.Quality = in.Quality;
    out.MassFlowRateMax = in.MassFlowRateMax;
    out.MassFlowRateMin = in.MassFlowRateMin;
    out.MassFlowRateMaxAvail = in.MassFlowRateMaxAvail;
    out.MassFlowRateMinAvail = in.MassFlowRateMinAvail;
}

int CoilCoolingDX::getDXCoilCapFTCurveIndex() {
    auto & performance = this->performance;
    if (performance.hasAlternateMode) {
        // Per IDD note - Operating Mode 1 is always used as the base design operating mode
        auto &mode = performance.normalMode; // TODO: Yeah, what?
        if (mode.speeds.size()) {
            auto &firstSpeed = mode.speeds[0];
            return firstSpeed.indexCapFT;
        }
        return -1;
    } else {
        auto & mode = performance.normalMode; // TODO: Like, what?
        if (mode.speeds.size()) {
            auto & firstSpeed = mode.speeds[0];
            return firstSpeed.indexCapFT;
        }
        return -1;
    }
}

Real64 CoilCoolingDX::getRatedGrossTotalCapacity() {
    // **should** we be checking if performance.hasAlternateMode before looking up the value?
	return this->performance.normalMode.ratedGrossTotalCap;
}
