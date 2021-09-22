from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent

schedule_objects = {
    "Schedule:Compact": {
        "Always21": {
            "data": [
                {
                    "field": "Through: 12/31"
                },
                {
                    "field": "For: AllDays"
                },
                {
                    "field": "Until: 24:00"
                },
                {
                    "field": 21
                }
            ],
            "schedule_type_limits_name": "Any Number"
        },
        "Always33": {
            "data": [
                {
                    "field": "Through: 12/31"
                },
                {
                    "field": "For: AllDays"
                },
                {
                    "field": "Until: 24:00"
                },
                {
                    "field": 33
                }
            ],
            "schedule_type_limits_name": "Any Number"
        }
    }
}


class TestSimulationsPlantLoopMixedWaterLoop(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles',
                                               'HVACTemplate-5ZoneWaterToAirHeatPumpTowerBoiler.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:MixedWaterLoop:test_minimum_inputs")
    def test_minimum_inputs(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop'].pop('Only Water Loop')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:MixedWaterLoop': {
                    'Only Water Loop': {}
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:pump_schedule_name")
    def test_pump_schedule_name(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop']['pump_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Pump:ConstantSpeed']['Only Water Loop Supply Pump']['pump_flow_rate_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:pump_control_type_intermittent")
    def test_pump_control_type_intermittent(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop']['pump_control_type'] = 'Intermittent'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Intermittent',
            epjson_output['Pump:ConstantSpeed']['Only Water Loop Supply Pump']['pump_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:pump_control_type_continuous")
    def test_pump_control_type_continuous(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop']['pump_control_type'] = 'Continuous'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Continuous',
            epjson_output['Pump:ConstantSpeed']['Only Water Loop Supply Pump']['pump_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "hot_water_plant_operation_scheme_type")
    def test_operation_scheme_type(self):
        # todo_eo: legacy fails with message: PlantEquipmentOperationSchemes = "HOT WATER LOOP OPERATION CUSTOM",
        #  could not find PlantEquipmentOperation:HeatingLoad = "HOT WATER LOOP OPERATION ALL HOURS".
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                "Schedule:Compact": {
                    "HVACTemplate-Always1": {
                        "data": [
                            {
                                "field": "Through 12/31"
                            },
                            {
                                "field": "For AllDays"
                            },
                            {
                                "field": "Until 24:00"
                            },
                            {
                                "field": 1.0
                            }
                        ],
                        "schedule_type_limits_name": "Any Number"
                    }
                },
                "PlantEquipmentOperationSchemes": {
                    "Only Water Loop Operation Custom": {
                        "control_scheme_1_name": "Only Water Loop Heat Operation All Hours",
                        "control_scheme_1_object_type": "PlantEquipmentOperation:HeatingLoad",
                        "control_scheme_1_schedule_name": "HVACTemplate-Always1",
                        "control_scheme_2_name": "Only Water Loop Cool Operation All Hours",
                        "control_scheme_2_object_type": "PlantEquipmentOperation:CoolingLoad",
                        "control_scheme_2_schedule_name": "HVACTemplate-Always1"
                    }
                },
                "PlantEquipmentOperation:CoolingLoad": {
                    "Only Water Loop Cool Operation All Hours": {
                        "load_range_1_lower_limit": 0,
                        "load_range_1_upper_limit": 1000000000000000,
                        "range_1_equipment_list_name": "Only Water Loop Cooling All Equipment"
                    }
                },
                "PlantEquipmentOperation:HeatingLoad": {
                    "Only Water Loop Heat Operation All Hours": {
                        "load_range_1_lower_limit": 0,
                        "load_range_1_upper_limit": 1000000000000000,
                        "range_1_equipment_list_name": "Only Water Loop Heating All Equipment"
                    }
                }
            }
        )
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'operation_scheme_type'] = 'UserDefined'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'equipment_operation_schemes_name'] = 'Only Water Loop Operation Custom'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['PlantEquipmentOperationSchemes'].get('Only Water Loop Operation Custom'))
        return

    def test_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'high_temperature_setpoint_schedule_name'] = 'Always33'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'low_temperature_setpoint_schedule_name'] = 'Always21'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always21',
            epjson_output['SetpointManager:Scheduled:DualSetpoint']['Only Water Loop Temp Manager'][
                'low_setpoint_schedule_name'])
        self.assertEqual(
            'Always33',
            epjson_output['SetpointManager:Scheduled:DualSetpoint']['Only Water Loop Temp Manager'][
                'high_setpoint_schedule_name'])
        return

    def test_design_setpoint(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'high_temperature_design_setpoint'] = 33
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'low_temperature_design_setpoint'] = 21
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'HVACTemplate-Always21.0',
            epjson_output['SetpointManager:Scheduled:DualSetpoint']['Only Water Loop Temp Manager'][
                'low_setpoint_schedule_name'])
        self.assertEqual(
            'HVACTemplate-Always33.0',
            epjson_output['SetpointManager:Scheduled:DualSetpoint']['Only Water Loop Temp Manager'][
                'high_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_configuration_variable_flow")
    def test_water_pump_configuration_variable_flow(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_configuration'] = 'VariableFlow'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Pump:VariableSpeed'].get('Only Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_configuration_variable_flow")
    def test_water_pump_configuration_constant_flow(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_configuration'] = 'ConstantFlow'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Pump:ConstantSpeed'].get('Only Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_rated_head")
    def test_water_pump_rated_head(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_rated_head'] = 19000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            19000,
            epjson_output['Pump:ConstantSpeed']['Only Water Loop Supply Pump']['design_pump_head'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_single_pump")
    def test_water_pump_type_single_pump(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'SinglePump'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pump:ConstantSpeed'].get('Only Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_single_pump_variable")
    def test_water_pump_type_single_pump_variable(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'SinglePump'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pump:VariableSpeed'].get('Only Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_pump_per_tower_or_boiler")
    def test_water_pump_type_pump_per_tower_or_boiler(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_configuration'] = 'ConstantFlow'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'PumpPerTowerOrBoiler'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_rated_head'] = 19000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Main Boiler MW Branch Pump',
            epjson_output['Branch']['Main Boiler MW Branch']['components'][0]['component_name'])
        self.assertEqual(
            'Main Boiler',
            epjson_output['Branch']['Main Boiler MW Branch']['components'][1]['component_name'])
        self.assertEqual(
            'Main Tower Branch Pump',
            epjson_output['Branch']['Main Tower Branch']['components'][0]['component_name'])
        self.assertEqual(
            'Main Tower',
            epjson_output['Branch']['Main Tower Branch']['components'][1]['component_name'])
        self.assertEqual(
            19000,
            epjson_output['Pump:ConstantSpeed']['Main Tower Branch Pump']['design_pump_head'])
        self.assertEqual(
            19000,
            epjson_output['Pump:ConstantSpeed']['Main Boiler MW Branch Pump']['design_pump_head'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_pump_per_tower_or_boiler")
    def test_water_pump_type_pump_per_tower_or_boiler_variable(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'PumpPerTowerOrBoiler'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_rated_head'] = 19000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Main Boiler MW Branch Pump',
            epjson_output['Branch']['Main Boiler MW Branch']['components'][0]['component_name'])
        self.assertEqual(
            'Main Boiler',
            epjson_output['Branch']['Main Boiler MW Branch']['components'][1]['component_name'])
        self.assertEqual(
            'Main Tower Branch Pump',
            epjson_output['Branch']['Main Tower Branch']['components'][0]['component_name'])
        self.assertEqual(
            'Main Tower',
            epjson_output['Branch']['Main Tower Branch']['components'][1]['component_name'])
        self.assertEqual(
            19000,
            epjson_output['Pump:ConstantSpeed']['Main Tower Branch Pump']['design_pump_head'])
        self.assertEqual(
            19000,
            epjson_output['Pump:VariableSpeed']['Main Boiler MW Branch Pump']['design_pump_head'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_two_headered_pumps")
    def test_water_pump_type_two_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'TwoHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Only Water Loop Supply Pump'))
        self.assertEqual(
            2,
            epjson_output['HeaderedPumps:ConstantSpeed']['Only Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_two_headered_pumps_variable")
    def test_water_pump_type_two_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'TwoHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Only Water Loop Supply Pump'))
        self.assertEqual(
            2,
            epjson_output['HeaderedPumps:VariableSpeed']['Only Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_three_headered_pumps")
    def test_water_pump_type_three_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'ThreeHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Only Water Loop Supply Pump'))
        self.assertEqual(
            3,
            epjson_output['HeaderedPumps:ConstantSpeed']['Only Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_three_headered_pumps_variable")
    def test_water_pump_type_three_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'ThreeHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Only Water Loop Supply Pump'))
        self.assertEqual(
            3,
            epjson_output['HeaderedPumps:VariableSpeed']['Only Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_four_headered_pumps")
    def test_water_pump_type_four_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'FourHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Only Water Loop Supply Pump'))
        self.assertEqual(
            4,
            epjson_output['HeaderedPumps:ConstantSpeed']['Only Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_four_headered_pumps_variable")
    def test_water_pump_type_four_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'FourHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Only Water Loop Supply Pump'))
        self.assertEqual(
            4,
            epjson_output['HeaderedPumps:VariableSpeed']['Only Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_five_headered_pumps")
    def test_water_pump_type_five_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'FiveHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Only Water Loop Supply Pump'))
        self.assertEqual(
            5,
            epjson_output['HeaderedPumps:ConstantSpeed']['Only Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "water_pump_type_five_headered_pumps_variable")
    def test_water_pump_type_five_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'water_pump_type'] = 'FiveHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Only Water Loop Supply Pump'))
        self.assertEqual(
            5,
            epjson_output['HeaderedPumps:VariableSpeed']['Only Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "supply_side_bypass_pipe_yes")
    def test_supply_side_bypass_pipe_yes(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'supply_side_bypass_pipe'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pipe:Adiabatic'].get('Only Water Loop Supply Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "supply_side_bypass_pipe_no")
    def test_supply_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'supply_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Only Water Loop Supply Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "demand_side_bypass_pipe_yes")
    def test_demand_side_bypass_pipe_yes(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'demand_side_bypass_pipe'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pipe:Adiabatic'].get('Only Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "demand_side_bypass_pipe_no")
    def test_demand_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'demand_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Only Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "demand_side_bypass_pipe_no")
    def test_supply_side_bypass_pipe_no_demand_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'supply_side_bypass_pipe'] = 'No'
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'demand_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Only Water Loop Supply Bypass Pipe'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Only Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:fluid_type_water")
    def test_fluid_type_water(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'fluid_type'] = 'Water'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['PlantLoop']['Only Water Loop PlantLoop'].get('fluid_type'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:fluid_type_ethylene_glycol_30")
    def test_fluid_type_ethylene_glycol_30(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'fluid_type'] = 'EthyleneGlycol30'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.3,
            epjson_output['FluidProperties:GlycolConcentration']['Only Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:fluid_type_ethylene_glycol_40")
    def test_fluid_type_ethylene_glycol_40(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'fluid_type'] = 'EthyleneGlycol40'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.4,
            epjson_output['FluidProperties:GlycolConcentration']['Only Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:fluid_type_ethylene_glycol_50")
    def test_fluid_type_ethylene_glycol_50(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'fluid_type'] = 'EthyleneGlycol50'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.5,
            epjson_output['FluidProperties:GlycolConcentration']['Only Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:fluid_type_ethylene_glycol_60")
    def test_fluid_type_ethylene_glycol_60(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'fluid_type'] = 'EthyleneGlycol60'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.6,
            epjson_output['FluidProperties:GlycolConcentration']['Only Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:fluid_type_propylene_glycol_30")
    def test_fluid_type_propylene_glycol_30(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'fluid_type'] = 'PropyleneGlycol30'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.3,
            epjson_output['FluidProperties:GlycolConcentration']['Only Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:fluid_type_propylene_glycol_40")
    def test_fluid_type_propylene_glycol_40(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'fluid_type'] = 'PropyleneGlycol40'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.4,
            epjson_output['FluidProperties:GlycolConcentration']['Only Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:fluid_type_propylene_glycol_50")
    def test_fluid_type_propylene_glycol_50(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'fluid_type'] = 'PropyleneGlycol50'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.5,
            epjson_output['FluidProperties:GlycolConcentration']['Only Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:fluid_type_propylene_glycol_60")
    def test_fluid_type_propylene_glycol_60(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'fluid_type'] = 'PropyleneGlycol60'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.6,
            epjson_output['FluidProperties:GlycolConcentration']['Only Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:loop_design_delta_temperature")
    def test_loop_design_delta_temperature(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'loop_design_delta_temperature'] = 5.4
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            5.4,
            epjson_output['Sizing:Plant']['Only Water Loop Sizing Plant']['loop_design_temperature_difference'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "load_distribution_scheme_none")
    def test_load_distribution_scheme_none(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'].pop('load_distribution_scheme', None)
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialLoad',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "load_distribution_scheme_sequential_load")
    def test_load_distribution_scheme_sequential_load(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'load_distribution_scheme'] = 'SequentialLoad'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialLoad',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "load_distribution_scheme_optimal")
    def test_load_distribution_scheme_optimal(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'load_distribution_scheme'] = 'Optimal'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Optimal',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "load_distribution_scheme_uniform_load")
    def test_load_distribution_scheme_uniform_load(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'load_distribution_scheme'] = 'UniformLoad'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UniformLoad',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "load_distribution_scheme_uniform_plr")
    def test_load_distribution_scheme_uniform_plr(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'load_distribution_scheme'] = 'UniformPLR'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UniformPLR',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:MixedWaterLoop:"
                                              "load_distribution_scheme_sequential_uniform_plr")
    def test_load_distribution_scheme_sequential_uniform_plr(self):
        self.base_epjson['HVACTemplate:Plant:MixedWaterLoop']['Only Water Loop'][
            'load_distribution_scheme'] = 'SequentialUniformPLR'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialUniformPLR',
            epjson_output['PlantLoop']['Only Water Loop PlantLoop']['load_distribution_scheme'])
        return
