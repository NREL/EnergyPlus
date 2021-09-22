from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent


schedule_objects = {
    "Schedule:Compact": {
        "Always81": {
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
                    "field": 81
                }
            ],
            "schedule_type_limits_name": "Any Number"
        }
    }
}


class TestSimulationsPlantLoopHotWaterLoop(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVWaterCooled.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:HotWaterLoop:test_minimum_inputs")
    def test_minimum_inputs(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop'].pop('Hot Water Loop')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:HotWaterLoop': {
                    'Hot Water Loop': {}
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:pump_schedule_name")
    def test_pump_schedule_name(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop']['pump_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Pump:ConstantSpeed']['Hot Water Loop Supply Pump']['pump_flow_rate_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:pump_control_type_intermittent")
    def test_pump_control_type_intermittent(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop']['pump_control_type'] = 'Intermittent'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Intermittent',
            epjson_output['Pump:ConstantSpeed']['Hot Water Loop Supply Pump']['pump_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:pump_control_type_continuous")
    def test_pump_control_type_continuous(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop']['pump_control_type'] = 'Continuous'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Continuous',
            epjson_output['Pump:ConstantSpeed']['Hot Water Loop Supply Pump']['pump_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_plant_operation_scheme_type")
    def test_hot_water_plant_operation_scheme_type(self):
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
                    "Hot Water Loop Operation Custom": {
                        "control_scheme_1_name": "Hot Water Loop Operation All Hours",
                        "control_scheme_1_object_type": "PlantEquipmentOperation:HeatingLoad",
                        "control_scheme_1_schedule_name": "HVACTemplate-Always1"
                    }
                },
                "PlantEquipmentOperation:HeatingLoad": {
                    "Hot Water Loop Operation All Hours": {
                        "load_range_1_lower_limit": 0,
                        "load_range_1_upper_limit": 1000000000000000,
                        "range_1_equipment_list_name": "Hot Water Loop All Equipment"
                    }
                }
            }
        )
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_plant_operation_scheme_type'] = 'UserDefined'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_plant_equipment_operation_schemes_name'] = 'Hot Water Loop Operation Custom'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['PlantEquipmentOperationSchemes'].get('Hot Water Loop Operation Custom'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_setpoint_schedule_name")
    def test_hot_water_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_setpoint_reset_type'] = 'None'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_setpoint_schedule_name'] = 'Always81'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always81',
            epjson_output['SetpointManager:Scheduled']['Hot Water Loop Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_design_setpoint")
    def test_hot_water_design_setpoint(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_setpoint_reset_type'] = 'None'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_design_setpoint'] = 81
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'HVACTemplate-Always81.0',
            epjson_output['SetpointManager:Scheduled']['Hot Water Loop Temp Manager']['schedule_name'])
        self.assertEqual(
            81,
            epjson_output['Sizing:Plant']['Hot Water Loop Sizing Plant']['design_loop_exit_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_configuration_variable_flow")
    def test_hot_water_pump_configuration_variable_flow(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'VariableFlow'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Pump:VariableSpeed'].get('Hot Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_configuration_variable_flow")
    def test_hot_water_pump_configuration_constant_flow(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'ConstantFlow'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Pump:ConstantSpeed'].get('Hot Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_rated_head")
    def test_hot_water_pump_rated_head(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_rated_head'] = 19000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            19000,
            epjson_output['Pump:ConstantSpeed']['Hot Water Loop Supply Pump']['design_pump_head'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_setpoint_reset_type_none")
    def test_hot_water_setpoint_reset_type_none(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_setpoint_reset_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            "HVACTemplate-Always82.0",
            epjson_output['SetpointManager:Scheduled']['Hot Water Loop Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_setpoint_reset_type_outdoor_air_temperature_reset")
    def test_hot_water_setpoint_reset_type_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'hot_water_setpoint_reset_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['SetpointManager:OutdoorAirReset'].get('Hot Water Loop Temp Manager'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_setpoint_reset_type_outdoor_air_temperature_reset_inputs")
    def test_hot_water_setpoint_reset_type_outdoor_air_temperature_reset_inputs(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_setpoint_reset_type'] = 'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_setpoint_at_outdoor_dry_bulb_low'] = 82
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_reset_outdoor_dry_bulb_low'] = -6.7
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_setpoint_at_outdoor_dry_bulb_high'] = 65.6
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_reset_outdoor_dry_bulb_high'] = 10
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            10,
            epjson_output['SetpointManager:OutdoorAirReset']['Hot Water Loop Temp Manager'][
                'outdoor_high_temperature'])
        self.assertEqual(
            -6.7,
            epjson_output['SetpointManager:OutdoorAirReset']['Hot Water Loop Temp Manager'][
                'outdoor_low_temperature'])
        self.assertEqual(
            65.6,
            epjson_output['SetpointManager:OutdoorAirReset']['Hot Water Loop Temp Manager'][
                'setpoint_at_outdoor_high_temperature'])
        self.assertEqual(
            82,
            epjson_output['SetpointManager:OutdoorAirReset']['Hot Water Loop Temp Manager'][
                'setpoint_at_outdoor_low_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_single_pump")
    def test_hot_water_pump_type_single_pump(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'SinglePump'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pump:ConstantSpeed'].get('Hot Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_single_pump_variable")
    def test_hot_water_pump_type_single_pump_variable(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'SinglePump'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pump:VariableSpeed'].get('Hot Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_pump_per_boiler")
    def test_hot_water_pump_type_pump_per_boiler(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'ConstantFlow'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'PumpPerBoiler'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Main Boiler HW Branch Pump',
            epjson_output['Branch']['Main Boiler HW Branch']['components'][0]['component_name'])
        self.assertEqual(
            'Main Boiler',
            epjson_output['Branch']['Main Boiler HW Branch']['components'][1]['component_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_pump_per_boiler_district_hot_water")
    def test_hot_water_pump_type_pump_per_boiler_district_hot_water(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'boiler_type'] = 'DistrictHotWater'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'ConstantFlow'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'PumpPerBoiler'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Main Boiler HW Branch Pump',
            epjson_output['Branch']['Main Boiler HW Branch']['components'][0]['component_name'])
        self.assertEqual(
            'Main Boiler',
            epjson_output['Branch']['Main Boiler HW Branch']['components'][1]['component_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_two_headered_pumps")
    def test_hot_water_pump_type_two_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'TwoHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Hot Water Loop Supply Pump'))
        self.assertEqual(
            2,
            epjson_output['HeaderedPumps:ConstantSpeed']['Hot Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_two_headered_pumps_variable")
    def test_hot_water_pump_type_two_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'TwoHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Hot Water Loop Supply Pump'))
        self.assertEqual(
            2,
            epjson_output['HeaderedPumps:VariableSpeed']['Hot Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_three_headered_pumps")
    def test_hot_water_pump_type_three_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'ThreeHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Hot Water Loop Supply Pump'))
        self.assertEqual(
            3,
            epjson_output['HeaderedPumps:ConstantSpeed']['Hot Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_three_headered_pumps_variable")
    def test_hot_water_pump_type_three_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'ThreeHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Hot Water Loop Supply Pump'))
        self.assertEqual(
            3,
            epjson_output['HeaderedPumps:VariableSpeed']['Hot Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_four_headered_pumps")
    def test_hot_water_pump_type_four_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'FourHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Hot Water Loop Supply Pump'))
        self.assertEqual(
            4,
            epjson_output['HeaderedPumps:ConstantSpeed']['Hot Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_four_headered_pumps_variable")
    def test_hot_water_pump_type_four_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'FourHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Hot Water Loop Supply Pump'))
        self.assertEqual(
            4,
            epjson_output['HeaderedPumps:VariableSpeed']['Hot Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_five_headered_pumps")
    def test_hot_water_pump_type_five_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'FiveHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Hot Water Loop Supply Pump'))
        self.assertEqual(
            5,
            epjson_output['HeaderedPumps:ConstantSpeed']['Hot Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_pump_type_five_headered_pumps_variable")
    def test_hot_water_pump_type_five_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_type'] = 'FiveHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Hot Water Loop Supply Pump'))
        self.assertEqual(
            5,
            epjson_output['HeaderedPumps:VariableSpeed']['Hot Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_supply_side_bypass_pipe_yes")
    def test_hot_water_supply_side_bypass_pipe_yes(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'supply_side_bypass_pipe'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pipe:Adiabatic'].get('Hot Water Loop Supply Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_supply_side_bypass_pipe_no")
    def test_hot_water_supply_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'supply_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Hot Water Loop Supply Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_demand_side_bypass_pipe_yes")
    def test_hot_water_demand_side_bypass_pipe_yes(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'demand_side_bypass_pipe'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pipe:Adiabatic'].get('Hot Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_demand_side_bypass_pipe_no")
    def test_hot_water_demand_side_bypass_pipe_no(self):
        # Variable flow is needed to converge simulation
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'demand_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Hot Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "hot_water_supply_side_bypass_pipe_no_demand_side_bypass_pipe_no")
    def test_hot_water_supply_side_bypass_pipe_no_demand_side_bypass_pipe_no(self):
        # Variable flow is needed to converge simulation
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'hot_water_pump_configuration'] = 'VariableFlow'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'supply_side_bypass_pipe'] = 'No'
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'demand_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Hot Water Loop Supply Bypass Pipe'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Hot Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:fluid_type_water")
    def test_fluid_type_water(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'fluid_type'] = 'Water'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop'].get('fluid_type'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:fluid_type_ethylene_glycol_30")
    def test_fluid_type_ethylene_glycol_30(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'fluid_type'] = 'EthyleneGlycol30'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.3,
            epjson_output['FluidProperties:GlycolConcentration']['Hot Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:fluid_type_ethylene_glycol_40")
    def test_fluid_type_ethylene_glycol_40(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'fluid_type'] = 'EthyleneGlycol40'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.4,
            epjson_output['FluidProperties:GlycolConcentration']['Hot Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:fluid_type_ethylene_glycol_50")
    def test_fluid_type_ethylene_glycol_50(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'fluid_type'] = 'EthyleneGlycol50'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.5,
            epjson_output['FluidProperties:GlycolConcentration']['Hot Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:fluid_type_ethylene_glycol_60")
    def test_fluid_type_ethylene_glycol_60(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'fluid_type'] = 'EthyleneGlycol60'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.6,
            epjson_output['FluidProperties:GlycolConcentration']['Hot Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:fluid_type_propylene_glycol_30")
    def test_fluid_type_propylene_glycol_30(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'fluid_type'] = 'PropyleneGlycol30'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.3,
            epjson_output['FluidProperties:GlycolConcentration']['Hot Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:fluid_type_propylene_glycol_40")
    def test_fluid_type_propylene_glycol_40(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'fluid_type'] = 'PropyleneGlycol40'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.4,
            epjson_output['FluidProperties:GlycolConcentration']['Hot Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:fluid_type_propylene_glycol_50")
    def test_fluid_type_propylene_glycol_50(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'fluid_type'] = 'PropyleneGlycol50'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.5,
            epjson_output['FluidProperties:GlycolConcentration']['Hot Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:fluid_type_propylene_glycol_60")
    def test_fluid_type_propylene_glycol_60(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'fluid_type'] = 'PropyleneGlycol60'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.6,
            epjson_output['FluidProperties:GlycolConcentration']['Hot Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:loop_design_delta_temperature")
    def test_loop_design_delta_temperature(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'loop_design_delta_temperature'] = 10
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            10,
            epjson_output['Sizing:Plant']['Hot Water Loop Sizing Plant']['loop_design_temperature_difference'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "maximum_outdoor_dry_bulb_temperature_none")
    def test_maximum_outdoor_dry_bulb_temperature_none(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'].pop(
            'maximum_outdoor_dry_bulb_temperature', None)
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output.get('AvailabilityManager:HighTemperatureTurnOff'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "maximum_outdoor_dry_bulb_temperature_none")
    def test_maximum_outdoor_dry_bulb_temperature_value(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'maximum_outdoor_dry_bulb_temperature'] = 20
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('AvailabilityManager:LowTemperatureTurnOff'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "load_distribution_scheme_none")
    def test_load_distribution_scheme_none(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'].pop('load_distribution_scheme', None)
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialLoad',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "load_distribution_scheme_sequential_load")
    def test_load_distribution_scheme_sequential_load(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'load_distribution_scheme'] = 'SequentialLoad'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialLoad',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "load_distribution_scheme_optimal")
    def test_load_distribution_scheme_sequential_optimal(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'load_distribution_scheme'] = 'Optimal'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Optimal',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "load_distribution_scheme_uniform_load")
    def test_load_distribution_scheme_uniform_load(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'load_distribution_scheme'] = 'UniformLoad'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UniformLoad',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "load_distribution_scheme_uniform_plr")
    def test_load_distribution_scheme_uniform_plr(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'load_distribution_scheme'] = 'UniformPLR'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UniformPLR',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:HotWaterLoop:"
                                              "load_distribution_scheme_sequential_uniform_plr")
    def test_load_distribution_scheme_sequential_uniform_plr(self):
        self.base_epjson['HVACTemplate:Plant:HotWaterLoop']['Hot Water Loop'][
            'load_distribution_scheme'] = 'SequentialUniformPLR'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialUniformPLR',
            epjson_output['PlantLoop']['Hot Water Loop PlantLoop']['load_distribution_scheme'])
        return
