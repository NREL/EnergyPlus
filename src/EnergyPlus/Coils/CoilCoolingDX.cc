#include <ObjexxFCL/Array1D.hh> // needs to be in BranchNodeConnections.hh

#include <BranchNodeConnections.hh>
#include <Coils/CoilCoolingDX.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataWater.hh>
#include <InputProcessing/InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <WaterManager.hh>
#include <EnergyPlus/DataEnvironment.hh>

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
    if (!input_data.condenser_inlet_node_name.empty()) {
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
    if (!input_data.condenser_outlet_node_name.empty()) {
        this->condOutletNodeIndex = NodeInputManager::GetOnlySingleNode(input_data.condenser_outlet_node_name,
                                                                        errorsFound,
                                                                        this->object_name,
                                                                        input_data.name,
                                                                        DataLoopNode::NodeType_Air,
                                                                        DataLoopNode::NodeConnectionType_Outlet,
                                                                        2,
                                                                        DataLoopNode::ObjectIsNotParent);
    } else {
        this->condOutletNodeIndex = 0;
    }

    if (!input_data.condensate_collection_water_storage_tank_name.empty()) {
        WaterManager::SetupTankSupplyComponent(this->name,
                                 this->object_name,
                                 input_data.condensate_collection_water_storage_tank_name,
                                 errorsFound,
                                 this->condensateTankIndex,
                                 this->condensateTankSupplyARRID);
    }

    if (!input_data.evaporative_condenser_supply_water_storage_tank_name.empty()) {
        WaterManager::SetupTankDemandComponent(this->name,
                                 this->object_name,
                                 input_data.evaporative_condenser_supply_water_storage_tank_name,
                                 errorsFound,
                                 this->evaporativeCondSupplyTankIndex,
                                 this->evaporativeCondSupplyTankARRID);
    }

    if (input_data.availability_schedule_name.empty()) {
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

void CoilCoolingDX::oneTimeInit(){

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
    SetupOutputVariable("Cooling Coil Electric Power", OutputProcessor::Unit::W, this->performance.powerUse, "System", "Average", this->name);
    SetupOutputVariable("Cooling Coil Electric Energy",
                        OutputProcessor::Unit::J,
                        this->performance.electricityConsumption,
                        "System",
                        "Sum",
                        this->name,
                        _,
                        "Electric",
                        "COOLING",
                        _,
                        "System");
    SetupOutputVariable(
        "Cooling Coil Runtime Fraction", OutputProcessor::Unit::None, this->performance.RTF, "System", "Average", this->name);
    SetupOutputVariable("Cooling Coil Crankcase Heater Electric Power",
                        OutputProcessor::Unit::W,
                        this->performance.crankcaseHeaterPower,
                        "System",
                        "Average",
                        this->name);
    SetupOutputVariable("Cooling Coil Crankcase Heater Electric Energy",
                        OutputProcessor::Unit::J,
                        this->performance.crankcaseHeaterElectricityConsumption,
                        "System",
                        "Sum",
                        this->name,
                        _,
                        "Electric",
                        "DHW",
                        _,
                        "Plant");
    if (this->performance.evapCondBasinHeatCap > 0) {
        SetupOutputVariable("Cooling Coil Basin Heater Electric Power",
                            OutputProcessor::Unit::W,
                            this->performance.basinHeaterPower,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable("Cooling Coil Basin Heater Electric Energy",
                            OutputProcessor::Unit::J,
                            this->performance.basinHeaterElectricityConsumption,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "Electric",
                            "COOLING",
                            _,
                            "System");
    }
    if (this->condensateTankIndex > 0) {
        SetupOutputVariable(
                "Cooling Coil Condensate Volume Flow Rate", OutputProcessor::Unit::m3_s, this->condensateVolumeFlow, "System", "Average", this->name);
        SetupOutputVariable("Cooling Coil Condensate Volume",
                            OutputProcessor::Unit::m3,
                            this->condensateVolumeConsumption,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "OnSiteWater",
                            "Condensate",
                            _,
                            "System");
    }
    if (this->evaporativeCondSupplyTankIndex > 0) {
        SetupOutputVariable("Cooling Coil Evaporative Condenser Pump Electric Power",
                            OutputProcessor::Unit::W,
                            this->evapCondPumpElecPower,
                            "System",
                            "Average",
                            this->name);
        SetupOutputVariable("Cooling Coil Evaporative Condenser Pump Electric Energy",
                            OutputProcessor::Unit::J,
                            this->evapCondPumpElecConsumption,
                            "System",
                            "Sum",
                            this->name,
                            _,
                            "Electric",
                            "COOLING",
                            _,
                            "System");
    }
}

CoilCoolingDX::CoilCoolingDX(std::string name_to_find)
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
        break;
    }

    if (!found_it) {
        // error
    }
}

void CoilCoolingDX::simulate(bool useAlternateMode, Real64 PLR, int speedNum, Real64 speedRatio, int fanOpMode)
{
    if (this->myOneTimeInitFlag) {
        this->oneTimeInit();
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

    // update condensate collection tank
    if (this->condensateTankIndex > 0) {
        // calculate and report condensation rates  (how much water extracted from the air stream)
        // water flow of water in m3/s for water system interactions
        Real64 averageTemp = (evapInletNode.Temp - evapOutletNode.Temp) / 2.0;
        Real64 waterDensity = Psychrometrics::RhoH2O(averageTemp);
        Real64 inHumidityRatio = evapInletNode.HumRat;
        Real64 outHumidityRatio = evapOutletNode.HumRat;
        //  mdot * del HumRat / rho water
        this->condensateVolumeFlow = max(0.0, (evapInletNode.MassFlowRate * (inHumidityRatio - outHumidityRatio) / waterDensity));
        this->condensateVolumeConsumption = this->condensateVolumeFlow * reportingConstant;

        DataWater::WaterStorage(this->condensateTankIndex).VdotAvailSupply(this->condensateTankSupplyARRID) = this->condensateVolumeFlow;
        DataWater::WaterStorage(this->condensateTankIndex).TwaterSupply(this->condensateTankSupplyARRID) = evapOutletNode.Temp;
    }

    // update requests for evap condenser tank
    if (this->evaporativeCondSupplyTankIndex > 0) {
        Real64 waterDensity = Psychrometrics::RhoH2O(DataEnvironment::OutDryBulbTemp);
        Real64 condInletTemp = DataEnvironment::OutWetBulbTemp + (DataEnvironment::OutDryBulbTemp - DataEnvironment::OutWetBulbTemp) * (1.0 - this->performance.normalMode.speeds[speedNum-1].evap_condenser_effectiveness);
        Real64 condInletHumRat = Psychrometrics::PsyWFnTdbTwbPb(condInletTemp, DataEnvironment::OutWetBulbTemp, DataEnvironment::OutBaroPress);
        Real64 outdoorHumRat = DataEnvironment::OutHumRat;
        auto &condInletNode = DataLoopNode::Node(this->condInletNodeIndex);
        Real64 condAirMassFlow = condInletNode.MassFlowRate;
        this->evaporativeCondSupplyTankVolumeFlow = (condInletHumRat - outdoorHumRat) * condAirMassFlow / waterDensity;
        if (!useAlternateMode) {
            this->evapCondPumpElecPower = this->performance.normalMode.getCurrentEvapCondPumpPower(speedNum);
        }
        DataWater::WaterStorage(this->evaporativeCondSupplyTankIndex).VdotRequestDemand(this->evaporativeCondSupplyTankARRID) =
                this->evaporativeCondSupplyTankVolumeFlow;
    }

    // update report variables
    this->totalCoolingEnergyRate = evapOutletNode.MassFlowRate * (evapInletNode.Enthalpy - evapOutletNode.Enthalpy);
    this->totalCoolingEnergy = this->totalCoolingEnergyRate * reportingConstant;
    Real64 minAirHumRat = min(evapInletNode.HumRat, evapOutletNode.HumRat);
    this->sensCoolingEnergyRate = evapOutletNode.MassFlowRate * (Psychrometrics::PsyHFnTdbW(evapInletNode.Temp, minAirHumRat) -
                                                                 Psychrometrics::PsyHFnTdbW(evapOutletNode.Temp, minAirHumRat));
    this->sensCoolingEnergy = this->sensCoolingEnergyRate * reportingConstant;
    this->latCoolingEnergyRate = this->totalCoolingEnergyRate - this->sensCoolingEnergyRate;
    this->latCoolingEnergy = this->latCoolingEnergyRate * reportingConstant;
    this->evapCondPumpElecConsumption = this->evapCondPumpElecPower * reportingConstant;

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
