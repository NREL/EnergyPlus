#include <ObjexxFCL/Array1D.hh> // needs to be in BranchNodeConnections.hh

#include <Coils/CoilCoolingDX.hh>
#include <BranchNodeConnections.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

namespace EnergyPlus {
    std::vector<CoilCoolingDX> coilCoolingDXs;
}

void CoilCoolingDX::instantiateFromInputSpec(CoilCoolingDXInputSpecification input_data) {
    this->original_input_specs = input_data;
    bool errorsFound = false;
    this->name = input_data.name;
    this->performance = CoilCoolingDXCurveFitPerformance(input_data.performance_object_name);

    // other construction below
    this->evapInletNodeIndex = NodeInputManager::GetOnlySingleNode( input_data.evaporator_inlet_node_name, errorsFound, this->object_name, input_data.name, DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent );
    this->evapOutletNodeIndex = NodeInputManager::GetOnlySingleNode( input_data.evaporator_outlet_node_name, errorsFound, this->object_name, input_data.name, DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 1, DataLoopNode::ObjectIsNotParent );
    if ( input_data.condenser_inlet_node_name != "" ) {
        this->condInletNodeIndex = NodeInputManager::GetOnlySingleNode( input_data.condenser_inlet_node_name, errorsFound, this->object_name, input_data.name, DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Inlet, 2, DataLoopNode::ObjectIsNotParent );
    } else {
        this->condInletNodeIndex = 0;
    }
    if ( input_data.condenser_outlet_node_name != "" ) {
        this->condOutletNodeIndex = NodeInputManager::GetOnlySingleNode( input_data.condenser_outlet_node_name, errorsFound, this->object_name, input_data.name, DataLoopNode::NodeType_Air, DataLoopNode::NodeConnectionType_Outlet, 2, DataLoopNode::ObjectIsNotParent );
    } else {
        this->condOutletNodeIndex = 0;
    }
    this->availScheduleIndex = ScheduleManager::GetScheduleIndex( input_data.availability_schedule_name );
    BranchNodeConnections::TestCompSet( CoilCoolingDX::object_name, this->name, input_data.evaporator_inlet_node_name, input_data.evaporator_outlet_node_name, "Air Nodes" );

    // setup output variables, should probably be done elsewhere
    SetupOutputVariable( "Cooling Coil Total Cooling Rate", OutputProcessor::Unit::W, this->totalCoolingEnergyRate, "System", "Average", this->name );
    SetupOutputVariable( "Cooling Coil Total Cooling Energy", OutputProcessor::Unit::J, this->totalCoolingEnergy, "System", "Sum", this->name, _, "ENERGYTRANSFER", "COOLINGCOILS", _, "System" );
    SetupOutputVariable( "Cooling Coil Sensible Cooling Rate", OutputProcessor::Unit::W, this->sensCoolingEnergyRate, "System", "Average", this->name );
    SetupOutputVariable( "Cooling Coil Sensible Cooling Energy", OutputProcessor::Unit::J, this->sensCoolingEnergy, "System", "Sum", this->name );
    SetupOutputVariable( "Cooling Coil Latent Cooling Rate", OutputProcessor::Unit::W, this->latCoolingEnergyRate, "System", "Average", this->name );
    SetupOutputVariable( "Cooling Coil Latent Cooling Energy", OutputProcessor::Unit::J, this->latCoolingEnergy, "System", "Sum", this->name );
    SetupOutputVariable( "Cooling Coil Electric Power", OutputProcessor::Unit::W, this->elecCoolingPower, "System", "Average", this->name );
    SetupOutputVariable( "Cooling Coil Electric Energy", OutputProcessor::Unit::J, this->elecCoolingConsumption, "System", "Sum", this->name, _, "Electric", "COOLING", _, "System" );
    SetupOutputVariable( "Cooling Coil Runtime Fraction", OutputProcessor::Unit::None, this->coolingCoilRuntimeFraction, "System", "Average", this->name );

}

CoilCoolingDX::CoilCoolingDX(std::string name_to_find) {
    int numCoolingCoilDXs = InputProcessor::GetNumObjectsFound(CoilCoolingDX::object_name);
    if (numCoolingCoilDXs <= 0) {
        // error
    }
    bool found_it = false;
    for (int coilNum = 1; coilNum <= numCoolingCoilDXs; ++coilNum) {
        int NumAlphas; // Number of Alphas for each GetObjectItem call
        int NumNumbers; // Number of Numbers for each GetObjectItem call
        int IOStatus;
        InputProcessor::GetObjectItem(CoilCoolingDX::object_name, coilNum, cAlphaArgs, NumAlphas, rNumericArgs,
                                      NumNumbers, IOStatus);
        if (!InputProcessor::SameString(name_to_find, cAlphaArgs(1))) {
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

    }

    if (!found_it) {
        // error
    }
}

void CoilCoolingDX::simulate(int mode, Real64 PLR, int speedNum, Real64 speedRatio, int fanOpMode) {

    // get inlet conditions from inlet node
    auto & evapInletNode = DataLoopNode::Node(this->evapInletNodeIndex);
    this->inletStateHolder.tdb = evapInletNode.Temp;
    this->inletStateHolder.h = evapInletNode.Enthalpy;
    this->inletStateHolder.w = evapInletNode.HumRat;
    this->inletStateHolder.massFlowRate = evapInletNode.MassFlowRate;
    this->inletStateHolder.p = evapInletNode.Press;

    // call the simulation, which returns useful data
    auto & myPerformance = this->performance;
    this->outletStateHolder = myPerformance.simulate(this->inletStateHolder, mode, PLR, speedNum, speedRatio, fanOpMode );

    // update outlet conditions
    auto & evapOutletNode = DataLoopNode::Node(this->evapOutletNodeIndex);
    evapOutletNode.Temp = this->outletStateHolder.tdb;
    evapOutletNode.HumRat = this->outletStateHolder.w;
    evapOutletNode.Enthalpy = this->outletStateHolder.h;
    evapOutletNode.MassFlowRate = this->inletStateHolder.massFlowRate; // pass through
    evapOutletNode.Press = this->inletStateHolder.p; // pass through
    evapOutletNode.Quality = evapInletNode.Quality; // pass through
    evapOutletNode.MassFlowRateMax = evapInletNode.MassFlowRateMax; // pass through
    evapOutletNode.MassFlowRateMin = evapInletNode.MassFlowRateMin; // pass through
    evapOutletNode.MassFlowRateMaxAvail = evapInletNode.MassFlowRateMaxAvail; // pass through
    evapOutletNode.MassFlowRateMinAvail = evapInletNode.MassFlowRateMinAvail; // pass through

    // calculate energy conversion factor
    Real64 reportingConstant = DataHVACGlobals::TimeStepSys * DataGlobals::SecInHour;
    // update report variables
    this->totalCoolingEnergyRate = evapOutletNode.MassFlowRate * ( evapInletNode.Enthalpy - evapOutletNode.Enthalpy );
    this->totalCoolingEnergy = this->totalCoolingEnergyRate * reportingConstant;
    Real64 minAirHumRat = min( evapInletNode.HumRat, evapOutletNode.HumRat );
    this->sensCoolingEnergyRate = evapOutletNode.MassFlowRate * ( Psychrometrics::PsyHFnTdbW( evapInletNode.Temp, minAirHumRat ) - Psychrometrics::PsyHFnTdbW( evapOutletNode.Temp, minAirHumRat ) );
    this->sensCoolingEnergy = this->sensCoolingEnergyRate * reportingConstant;
    this->latCoolingEnergyRate = this->totalCoolingEnergyRate - this->sensCoolingEnergyRate;
    this->latCoolingEnergy = this->latCoolingEnergyRate * reportingConstant;
    this->coolingCoilRuntimeFraction = myPerformance.RTF;
    this->elecCoolingPower = myPerformance.powerUse;
    this->elecCoolingConsumption = myPerformance.powerUse * reportingConstant;
}
