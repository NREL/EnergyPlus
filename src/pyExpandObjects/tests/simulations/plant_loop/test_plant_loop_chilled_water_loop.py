from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent

schedule_objects = {
    "Schedule:Compact": {
        "Always7.2": {
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
                    "field": 7.2
                }
            ],
            "schedule_type_limits_name": "Any Number"
        },
        "Always29": {
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
                    "field": 29.0
                }
            ],
            "schedule_type_limits_name": "Any Number"
        }
    }
}


class TestSimulationsPlantLoopChilledWaterLoop(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVWaterCooled.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:test_minimum_inputs")
    def test_minimum_inputs(self):
        # todo_eo: legacy generates odd warning
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop'].pop('Chilled Water Loop')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:ChilledWaterLoop': {
                    'Chilled Water Loop': {}
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    # todo_eo: test pump_schedule name when secondary pumps are active, do they have schedules applied as well?

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:pump_schedule_name")
    def test_pump_schedule_name(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop']['pump_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Pump:ConstantSpeed']['Chilled Water Loop Supply Pump']['pump_flow_rate_schedule_name'])
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Pump:VariableSpeed']['Condenser Water Loop Supply Pump']['pump_flow_rate_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:pump_control_type_intermittent")
    def test_pump_control_type_intermittent(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop']['pump_control_type'] = 'Intermittent'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Intermittent',
            epjson_output['Pump:ConstantSpeed']['Chilled Water Loop Supply Pump']['pump_control_type'])
        self.assertEqual(
            'Intermittent',
            epjson_output['Pump:VariableSpeed']['Condenser Water Loop Supply Pump']['pump_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:pump_control_type_continuous")
    def test_pump_control_type_continuous(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop']['pump_control_type'] = 'Continuous'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Continuous',
            epjson_output['Pump:ConstantSpeed']['Chilled Water Loop Supply Pump']['pump_control_type'])
        self.assertEqual(
            'Continuous',
            epjson_output['Pump:VariableSpeed']['Condenser Water Loop Supply Pump']['pump_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chiller_plant_operation_scheme_type")
    def test_chiller_plant_operation_scheme_type(self):
        # todo_eo: legacy fails with message: PlantEquipmentOperationSchemes = "CHILLED WATER LOOP OPERATION",
        #  could not find PlantEquipmentOperation:CoolingLoad = "CHILLED WATER LOOP OPERATION ALL HOURS".
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
                    "Chilled Water Loop Operation Custom": {
                        "control_scheme_1_name": "Chilled Water Loop Operation All Hours",
                        "control_scheme_1_object_type": "PlantEquipmentOperation:CoolingLoad",
                        "control_scheme_1_schedule_name": "HVACTemplate-Always1"
                    }
                },
                "PlantEquipmentOperation:CoolingLoad": {
                    "Chilled Water Loop Operation All Hours": {
                        "load_range_1_lower_limit": 0,
                        "load_range_1_upper_limit": 1000000000000000,
                        "range_1_equipment_list_name": "Chilled Water Loop All Equipment"
                    }
                }
            }
        )
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chiller_plant_operation_scheme_type'] = 'UserDefined'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chiller_plant_equipment_operation_schemes_name'] = 'Chilled Water Loop Operation Custom'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['PlantEquipmentOperationSchemes'].get('Chilled Water Loop Operation Custom'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_setpoint_schedule_name")
    def test_chilled_water_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_setpoint_schedule_name'] = 'Always7.2'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always7.2',
            epjson_output['SetpointManager:Scheduled']['Chilled Water Loop Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_design_setpoint")
    def test_chilled_water_design_setpoint(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_design_setpoint'] = 7.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'HVACTemplate-Always7.1',
            epjson_output['SetpointManager:Scheduled']['Chilled Water Loop Temp Manager']['schedule_name'])
        self.assertEqual(
            7.1,
            epjson_output['Sizing:Plant']['Chilled Water Loop Sizing Plant']['design_loop_exit_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_pump_configuration_constant_primary_no_secondary")
    def test_chilled_water_pump_configuration_constant_primary_no_secondary(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'ConstantPrimaryNoSecondary'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Pump:ConstantSpeed'].get('Chilled Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_pump_configuration_variable_primary_no_secondary")
    def test_chilled_water_pump_configuration_variable_primary_no_secondary(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'VariablePrimaryNoSecondary'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Pump:VariableSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertEqual(
            'LeavingSetpointModulated',
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['chiller_flow_mode'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_pump_configuration_constant_primary_variable_secondary")
    def test_chilled_water_pump_configuration_constant_primary_variable_secondary(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'ConstantPrimaryVariableSecondary'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Pump:ConstantSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertIsNotNone(epjson_output['Pump:VariableSpeed'].get('Chilled Water Loop Secondary Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_chilled_water_pump_rated_head")
    def test_chilled_water_primary_chilled_water_pump_rated_head(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'ConstantPrimaryVariableSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'primary_chilled_water_pump_rated_head'] = 19000
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'secondary_chilled_water_pump_rated_head'] = 19100
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            19000,
            epjson_output['Pump:ConstantSpeed']['Chilled Water Loop Supply Pump']['design_pump_head'])
        self.assertEqual(
            19100,
            epjson_output['Pump:VariableSpeed']['Chilled Water Loop Secondary Pump']['design_pump_head'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chiller_plant_operation_scheme_type")
    def test_condenser_plant_operation_scheme_type(self):
        # todo_eo: legacy fails with similar message to chilled water plant equipment operation scheme type
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
                "CondenserEquipmentOperationSchemes": {
                    "Condenser Water Loop Operation Custom": {
                        "control_scheme_1_name": "Condenser Water Loop Operation All Hours",
                        "control_scheme_1_object_type": "PlantEquipmentOperation:CoolingLoad",
                        "control_scheme_1_schedule_name": "HVACTemplate-Always1"
                    }
                }
            }
        )
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_plant_operation_scheme_type'] = 'UserDefined'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_equipment_operation_schemes_name'] = 'Condenser Water Loop Operation Custom'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['CondenserEquipmentOperationSchemes'].get('Condenser Water Loop Operation Custom'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_temperature_control_type_specified_setpoint")
    def test_condenser_water_temperature_control_type_specified_setpoint(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_temperature_control_type'] = 'SpecifiedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['SetpointManager:Scheduled'].get('Condenser Water Loop Temp Manager'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_temperature_control_type_outdoor_wet_bulb_temperature")
    def test_condenser_water_temperature_control_type_outdoor_wet_bulb_temperature(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_temperature_control_type'] = 'OutdoorWetBulbTemperature'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['SetpointManager:FollowOutdoorAirTemperature'].get('Condenser Water Loop Temp Manager'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_setpoint_schedule_name")
    def test_condenser_water_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_setpoint_schedule_name'] = 'Always29'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always29',
            epjson_output['SetpointManager:Scheduled']['Condenser Water Loop Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_design_setpoint")
    def test_condenser_water_design_setpoint(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_design_setpoint'] = 29.0
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'HVACTemplate-Always29.0',
            epjson_output['SetpointManager:Scheduled']['Condenser Water Loop Temp Manager']['schedule_name'])
        self.assertEqual(
            29,
            epjson_output['Sizing:Plant']['Condenser Water Loop Sizing Plant']['design_loop_exit_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_pump_rated_head")
    def test_condenser_water_pump_rated_head(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_pump_rated_head'] = 20000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            20000,
            epjson_output['Pump:VariableSpeed']['Condenser Water Loop Supply Pump']['design_pump_head'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_setpoint_reset_type_none")
    def test_chilled_water_setpoint_reset_type_none(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_setpoint_reset_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            "HVACTemplate-Always7.22",
            epjson_output['SetpointManager:Scheduled']['Chilled Water Loop Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_setpoint_reset_type_outdoor_air_temperature_reset")
    def test_chilled_water_setpoint_reset_type_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_setpoint_reset_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['SetpointManager:OutdoorAirReset'].get('Chilled Water Loop Temp Manager'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_setpoint_reset_type_outdoor_air_temperature_reset_inputs")
    def test_chilled_water_setpoint_reset_type_outdoor_air_temperature_reset_inputs(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_setpoint_reset_type'] = 'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_setpoint_at_outdoor_dry_bulb_low'] = 12.4
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_reset_outdoor_dry_bulb_low'] = 15.8
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_setpoint_at_outdoor_dry_bulb_high'] = 6.9
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_reset_outdoor_dry_bulb_high'] = 26.9
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            26.9,
            epjson_output['SetpointManager:OutdoorAirReset']['Chilled Water Loop Temp Manager'][
                'outdoor_high_temperature'])
        self.assertEqual(
            15.8,
            epjson_output['SetpointManager:OutdoorAirReset']['Chilled Water Loop Temp Manager'][
                'outdoor_low_temperature'])
        self.assertEqual(
            6.9,
            epjson_output['SetpointManager:OutdoorAirReset']['Chilled Water Loop Temp Manager'][
                'setpoint_at_outdoor_high_temperature'])
        self.assertEqual(
            12.4,
            epjson_output['SetpointManager:OutdoorAirReset']['Chilled Water Loop Temp Manager'][
                'setpoint_at_outdoor_low_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_single_pump")
    def test_chilled_water_primary_pump_type_single_pump(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'SinglePump'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pump:ConstantSpeed'].get('Chilled Water Loop Supply Pump'))
        return

    def test_chilled_water_primary_pump_type_single_pump_variable(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'VariablePrimaryNoSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'SinglePump'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pump:VariableSpeed'].get('Chilled Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_pump_per_chiller")
    def test_chilled_water_primary_pump_type_pump_per_chiller(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'ConstantPrimaryNoSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'PumpPerChiller'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Main Chiller ChW Branch Pump',
            epjson_output['Branch']['Main Chiller ChW Branch']['components'][0]['component_name'])
        self.assertEqual(
            'Main Chiller',
            epjson_output['Branch']['Main Chiller ChW Branch']['components'][1]['component_name'])
        return

    def test_chilled_water_primary_pump_type_pump_per_chiller_variable(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'VariablePrimaryNoSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'PumpPerChiller'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Main Chiller ChW Branch Pump',
            epjson_output['Branch']['Main Chiller ChW Branch']['components'][0]['component_name'])
        self.assertEqual(
            'Main Chiller',
            epjson_output['Branch']['Main Chiller ChW Branch']['components'][1]['component_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_two_headered_pumps")
    def test_chilled_water_primary_pump_type_two_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'TwoHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertEqual(
            2,
            epjson_output['HeaderedPumps:ConstantSpeed']['Chilled Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_two_headered_pumps_variable")
    def test_chilled_water_primary_pump_type_two_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'VariablePrimaryNoSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'TwoHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertEqual(
            2,
            epjson_output['HeaderedPumps:VariableSpeed']['Chilled Water Loop Supply Pump']['number_of_pumps_in_bank'])

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_three_headered_pumps")
    def test_chilled_water_primary_pump_type_three_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'ThreeHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertEqual(
            3,
            epjson_output['HeaderedPumps:ConstantSpeed']['Chilled Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_three_headered_pumps_variable")
    def test_chilled_water_primary_pump_type_three_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'VariablePrimaryNoSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'ThreeHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertEqual(
            3,
            epjson_output['HeaderedPumps:VariableSpeed']['Chilled Water Loop Supply Pump']['number_of_pumps_in_bank'])

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_four_headered_pumps")
    def test_chilled_water_primary_pump_type_four_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'FourHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertEqual(
            4,
            epjson_output['HeaderedPumps:ConstantSpeed']['Chilled Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_four_headered_pumps_variable")
    def test_chilled_water_primary_pump_type_four_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'VariablePrimaryNoSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'FourHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertEqual(
            4,
            epjson_output['HeaderedPumps:VariableSpeed']['Chilled Water Loop Supply Pump']['number_of_pumps_in_bank'])

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_five_headered_pumps")
    def test_chilled_water_primary_pump_type_five_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'FiveHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:ConstantSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertEqual(
            5,
            epjson_output['HeaderedPumps:ConstantSpeed']['Chilled Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_primary_pump_type_five_headered_pumps_variable")
    def test_chilled_water_primary_pump_type_five_headered_pumps_variable(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'VariablePrimaryNoSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_primary_pump_type'] = 'FiveHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Chilled Water Loop Supply Pump'))
        self.assertEqual(
            5,
            epjson_output['HeaderedPumps:VariableSpeed']['Chilled Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_secondary_pump_type_single_pump")
    def test_chilled_water_secondary_pump_type_two_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'ConstantPrimaryVariableSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_secondary_pump_type'] = 'SinglePump'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Chilled Water Loop Secondary Pump'))
        self.assertEqual(
            2,
            epjson_output['HeaderedPumps:VariableSpeed']['Chilled Water Loop Secondary Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_secondary_pump_type_two_headered_pumps")
    def test_chilled_water_secondary_pump_type_two_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'ConstantPrimaryVariableSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_secondary_pump_type'] = 'TwoHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Chilled Water Loop Secondary Pump'))
        self.assertEqual(
            2,
            epjson_output['HeaderedPumps:VariableSpeed']['Chilled Water Loop Secondary Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_secondary_pump_type_three_headered_pumps")
    def test_chilled_water_secondary_pump_type_three_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'ConstantPrimaryVariableSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_secondary_pump_type'] = 'ThreeHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Chilled Water Loop Secondary Pump'))
        self.assertEqual(
            3,
            epjson_output['HeaderedPumps:VariableSpeed']['Chilled Water Loop Secondary Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_secondary_pump_type_four_headered_pumps")
    def test_chilled_water_secondary_pump_type_four_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'ConstantPrimaryVariableSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_secondary_pump_type'] = 'FourHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Chilled Water Loop Secondary Pump'))
        self.assertEqual(
            4,
            epjson_output['HeaderedPumps:VariableSpeed']['Chilled Water Loop Secondary Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_secondary_pump_type_five_headered_pumps")
    def test_chilled_water_secondary_pump_type_five_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_pump_configuration'] = 'ConstantPrimaryVariableSecondary'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_secondary_pump_type'] = 'FiveHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Chilled Water Loop Secondary Pump'))
        self.assertEqual(
            5,
            epjson_output['HeaderedPumps:VariableSpeed']['Chilled Water Loop Secondary Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_primary_pump_type_single_pump")
    def test_condenser_water_primary_pump_type_single_pump(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_pump_type'] = 'SinglePump'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pump:ConstantSpeed'].get('Chilled Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_primary_pump_type_pump_per_tower")
    def test_condenser_water_primary_pump_type_pump_per_tower(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_pump_type'] = 'PumpPerTower'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pump:ConstantSpeed'].get('Chilled Water Loop Supply Pump'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_primary_pump_type_two_headered_pumps")
    def test_condenser_water_primary_pump_type_two_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_pump_type'] = 'TwoHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Condenser Water Loop Supply Pump'))
        self.assertEqual(
            2,
            epjson_output['HeaderedPumps:VariableSpeed']['Condenser Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_primary_pump_type_three_headered_pumps")
    def test_condenser_water_primary_pump_type_three_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_pump_type'] = 'ThreeHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Condenser Water Loop Supply Pump'))
        self.assertEqual(
            3,
            epjson_output['HeaderedPumps:VariableSpeed']['Condenser Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_primary_pump_type_four_headered_pumps")
    def test_condenser_water_primary_pump_type_four_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_pump_type'] = 'FourHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Condenser Water Loop Supply Pump'))
        self.assertEqual(
            4,
            epjson_output['HeaderedPumps:VariableSpeed']['Condenser Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_primary_pump_type_five_headered_pumps")
    def test_condenser_water_primary_pump_type_five_headered_pumps(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_pump_type'] = 'FiveHeaderedPumps'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeaderedPumps:VariableSpeed'].get('Condenser Water Loop Supply Pump'))
        self.assertEqual(
            5,
            epjson_output['HeaderedPumps:VariableSpeed']['Condenser Water Loop Supply Pump']['number_of_pumps_in_bank'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_supply_side_bypass_pipe_yes")
    def test_chilled_water_supply_side_bypass_pipe_yes(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_supply_side_bypass_pipe'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pipe:Adiabatic'].get('Chilled Water Loop Supply Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_supply_side_bypass_pipe_no")
    def test_chilled_water_supply_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_supply_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Chilled Water Loop Supply Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_demand_side_bypass_pipe_yes")
    def test_chilled_water_demand_side_bypass_pipe_yes(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_demand_side_bypass_pipe'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pipe:Adiabatic'].get('Chilled Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_demand_side_bypass_pipe_no")
    def test_chilled_water_demand_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_demand_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Chilled Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_supply_side_bypass_pipe_no_demand_side_bypass_pipe_no")
    def test_chilled_water_supply_side_bypass_pipe_no_demand_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_supply_side_bypass_pipe'] = 'No'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_demand_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Chilled Water Loop Supply Bypass Pipe'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Chilled Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_supply_side_bypass_pipe_yes")
    def test_condenser_water_supply_side_bypass_pipe_yes(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_supply_side_bypass_pipe'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pipe:Adiabatic'].get('Condenser Water Loop Supply Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_supply_side_bypass_pipe_no")
    def test_condenser_water_supply_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_supply_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Condenser Water Loop Supply Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_demand_side_bypass_pipe_yes")
    def test_condenser_water_demand_side_bypass_pipe_yes(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_demand_side_bypass_pipe'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Pipe:Adiabatic'].get('Condenser Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_demand_side_bypass_pipe_no")
    def test_condenser_water_demand_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_demand_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Condenser Water Loop Demand Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_supply_side_bypass_pipe_no_demand_side_bypass_no")
    def test_condenser_water_supply_side_bypass_pipe_no_demand_side_bypass_pipe_no(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_supply_side_bypass_pipe'] = 'No'
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_demand_side_bypass_pipe'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Condenser Water Loop Demand Bypass Pipe'))
        self.assertIsNone(
            epjson_output['Pipe:Adiabatic'].get('Condenser Water Loop Supply Bypass Pipe'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:fluid_type_water")
    def test_fluid_type_water(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'fluid_type'] = 'Water'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop'].get('fluid_type'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:fluid_type_ethylene_glycol_30")
    def test_fluid_type_ethylene_glycol_30(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'fluid_type'] = 'EthyleneGlycol30'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.3,
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:fluid_type_ethylene_glycol_40")
    def test_fluid_type_ethylene_glycol_40(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'fluid_type'] = 'EthyleneGlycol40'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.4,
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:fluid_type_ethylene_glycol_50")
    def test_fluid_type_ethylene_glycol_50(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'fluid_type'] = 'EthyleneGlycol50'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.5,
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:fluid_type_ethylene_glycol_60")
    def test_fluid_type_ethylene_glycol_60(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'fluid_type'] = 'EthyleneGlycol60'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'EthyleneGlycol',
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_type'])
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.6,
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:fluid_type_propylene_glycol_30")
    def test_fluid_type_propylene_glycol_30(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'fluid_type'] = 'PropyleneGlycol30'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'PropyleneGlycol',
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_type'])
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.3,
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:fluid_type_propylene_glycol_40")
    def test_fluid_type_propylene_glycol_40(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'fluid_type'] = 'PropyleneGlycol40'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'PropyleneGlycol',
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_type'])
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.4,
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:fluid_type_propylene_glycol_50")
    def test_fluid_type_propylene_glycol_50(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'fluid_type'] = 'PropyleneGlycol50'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'PropyleneGlycol',
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_type'])
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.5,
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:fluid_type_propylene_glycol_60")
    def test_fluid_type_propylene_glycol_60(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'fluid_type'] = 'PropyleneGlycol60'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'PropyleneGlycol',
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_type'])
        self.assertEqual(
            'UserDefinedFluidType',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['fluid_type'])
        self.assertEqual(
            0.6,
            epjson_output['FluidProperties:GlycolConcentration']['Chilled Water Loop Fluid']['glycol_concentration'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:loop_design_delta_temperature")
    def test_loop_design_delta_temperature(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'loop_design_delta_temperature'] = 6.8
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            6.8,
            epjson_output['Sizing:Plant']['Chilled Water Loop Sizing Plant']['loop_design_temperature_difference'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "minimum_outdoor_dry_bulb_temperature_none")
    def test_minimum_outdoor_dry_bulb_temperature_none(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'].pop(
            'minimum_outdoor_dry_bulb_temperature')
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output.get('AvailabilityManager:LowTemperatureTurnOff'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "minimum_outdoor_dry_bulb_temperature_value")
    def test_minimum_outdoor_dry_bulb_temperature_value(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'minimum_outdoor_dry_bulb_temperature'] = 7.5
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            7.5,
            epjson_output['AvailabilityManager:LowTemperatureTurnOff'][
                'Chilled Water Loop Availability Low Temp TurnOff']['temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_load_distribution_scheme_none")
    def test_chilled_water_load_distribution_scheme_none(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'].pop(
            'chilled_water_load_distribution_scheme', None)
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialLoad',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_load_distribution_scheme_sequential_load")
    def test_chilled_water_load_distribution_scheme_sequential_load(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_load_distribution_scheme'] = 'SequentialLoad'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialLoad',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_load_distribution_scheme_optimal")
    def test_chilled_water_load_distribution_scheme_optimal(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_load_distribution_scheme'] = 'Optimal'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Optimal',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_load_distribution_scheme_uniform_load")
    def test_chilled_water_load_distribution_scheme_uniform_load(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_load_distribution_scheme'] = 'UniformLoad'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UniformLoad',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_load_distribution_scheme_uniform_plr")
    def test_chilled_water_load_distribution_scheme_uniform_plr(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_load_distribution_scheme'] = 'UniformPLR'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UniformPLR',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "chilled_water_load_distribution_scheme_sequential_uniform_plr")
    def test_chilled_water_load_distribution_scheme_sequential_uniform_plr(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'chilled_water_load_distribution_scheme'] = 'SequentialUniformPLR'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialUniformPLR',
            epjson_output['PlantLoop']['Chilled Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_load_distribution_scheme_none")
    def test_condenser_water_load_distribution_scheme_none(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'].pop(
            'condenser_water_load_distribution_scheme', None)
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialLoad',
            epjson_output['CondenserLoop']['Condenser Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_load_distribution_scheme_sequential_load")
    def test_condenser_water_load_distribution_scheme_sequential_load(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_load_distribution_scheme'] = 'SequentialLoad'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialLoad',
            epjson_output['CondenserLoop']['Condenser Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_load_distribution_scheme_optional")
    def test_condenser_water_load_distribution_scheme_optimal(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_load_distribution_scheme'] = 'Optimal'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Optimal',
            epjson_output['CondenserLoop']['Condenser Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_load_distribution_scheme_uniform_load")
    def test_condenser_water_load_distribution_scheme_uniform_load(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_load_distribution_scheme'] = 'UniformLoad'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UniformLoad',
            epjson_output['CondenserLoop']['Condenser Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_load_distribution_scheme_uniform_plr")
    def test_condenser_water_load_distribution_scheme_uniform_plr(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_load_distribution_scheme'] = 'UniformPLR'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'UniformPLR',
            epjson_output['CondenserLoop']['Condenser Water Loop PlantLoop']['load_distribution_scheme'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantLoop:ChilledWaterLoop:"
                                              "condenser_water_load_distribution_scheme_sequential_uniform_plr")
    def test_condenser_water_load_distribution_scheme_sequential_uniform_plr(self):
        self.base_epjson['HVACTemplate:Plant:ChilledWaterLoop']['Chilled Water Loop'][
            'condenser_water_load_distribution_scheme'] = 'SequentialUniformPLR'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SequentialUniformPLR',
            epjson_output['CondenserLoop']['Condenser Water Loop PlantLoop']['load_distribution_scheme'])
        return
