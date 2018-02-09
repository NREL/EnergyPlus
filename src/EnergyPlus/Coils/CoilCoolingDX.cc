#include <Coils/CoilCoolingDX.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>

using namespace EnergyPlus;
using namespace DataIPShortCuts;

void CoilCoolingDX::instantiateFromInputSpec(CoilCoolingDXInputSpecification input_data) {
    this->original_input_specs = input_data;
    bool errorsFound = false;
    this->name = input_data.name;
    this->performance = CoilCoolingDXCurveFitPerformance(input_data.performance_object_name);
    // other construction below
    NodeInputManager::GetOnlySingleNode( cAlphaArgs( 2 ), errorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), DataLoopNode::NodeType_Water, DataLoopNode::NodeConnectionType_Inlet, 1, DataLoopNode::ObjectIsNotParent );
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

void CoilCoolingDX::simulate() {
    // check things
    // pass things to performance object "simulate" function that will do "something"
    auto & myPerformance = this->performance;
    // access everything on the performance instance here
    myPerformance.simulate();
}

            // PlantProfile name
//            VerifyName( cAlphaArgs( 1 ), PlantProfile, ProfileNum - 1, IsNotOK, IsBlank, cCurrentModuleObject );
//            if ( IsNotOK ) {
//                ErrorsFound = true;
//                if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
//            }
//            PlantProfile( ProfileNum ).Name = cAlphaArgs( 1 );
//            PlantProfile( ProfileNum ).TypeNum = TypeOf_PlantLoadProfile; // parameter assigned in DataPlant !DSU
//
//            PlantProfile( ProfileNum ).InletNode = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
//            PlantProfile( ProfileNum ).OutletNode = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
//
//            PlantProfile( ProfileNum ).LoadSchedule = GetScheduleIndex( cAlphaArgs( 4 ) );
//
//            if ( PlantProfile( ProfileNum ).LoadSchedule == 0 ) {
//                ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"  The Schedule for " + cAlphaFieldNames( 4 ) + " called " + cAlphaArgs( 4 ) + " was not found." );
//                ErrorsFound = true;
//            }
//
//            PlantProfile( ProfileNum ).PeakVolFlowRate = rNumericArgs( 1 );
//
//            PlantProfile( ProfileNum ).FlowRateFracSchedule = GetScheduleIndex( cAlphaArgs( 5 ) );
//
//            if ( PlantProfile( ProfileNum ).FlowRateFracSchedule == 0 ) {
//                ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"  The Schedule for " + cAlphaFieldNames( 5 ) + " called " + cAlphaArgs( 5 ) + " was not found." );
//
//                ErrorsFound = true;
//            }
//
//            // Check plant connections
//            TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 2 ), cAlphaArgs( 3 ), cCurrentModuleObject + " Nodes" );
//
//            // Setup report variables
//            SetupOutputVariable( "Plant Load Profile Mass Flow Rate", OutputProcessor::Unit::kg_s, PlantProfile( ProfileNum ).MassFlowRate, "System", "Average", PlantProfile( ProfileNum ).Name );
//
//            SetupOutputVariable( "Plant Load Profile Heat Transfer Rate", OutputProcessor::Unit::W, PlantProfile( ProfileNum ).Power, "System", "Average", PlantProfile( ProfileNum ).Name );
//
//            SetupOutputVariable( "Plant Load Profile Heat Transfer Energy", OutputProcessor::Unit::J, PlantProfile( ProfileNum ).Energy, "System", "Sum", PlantProfile( ProfileNum ).Name, _, "ENERGYTRANSFER", "Heating", _, "Plant" ); // is EndUseKey right?
//
//            SetupOutputVariable( "Plant Load Profile Heating Energy", OutputProcessor::Unit::J, PlantProfile( ProfileNum ).HeatingEnergy, "System", "Sum", PlantProfile( ProfileNum ).Name, _, "PLANTLOOPHEATINGDEMAND", "Heating", _, "Plant" );
//
//            SetupOutputVariable( "Plant Load Profile Cooling Energy", OutputProcessor::Unit::J, PlantProfile( ProfileNum ).CoolingEnergy, "System", "Sum", PlantProfile( ProfileNum ).Name, _, "PLANTLOOPCOOLINGDEMAND", "Cooling", _, "Plant" );
//
//            if ( AnyEnergyManagementSystemInModel ) {
//                SetupEMSActuator( "Plant Load Profile", PlantProfile( ProfileNum ).Name, "Mass Flow Rate", "[kg/s]", PlantProfile( ProfileNum ).EMSOverrideMassFlow, PlantProfile( ProfileNum ).EMSMassFlowValue );
//                SetupEMSActuator( "Plant Load Profile", PlantProfile( ProfileNum ).Name, "Power", "[W]", PlantProfile( ProfileNum ).EMSOverridePower, PlantProfile( ProfileNum ).EMSPowerValue );
//            }
//
//            if ( ErrorsFound ) ShowFatalError( "Errors in " + cCurrentModuleObject + " input." );

