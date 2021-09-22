from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent

schedule_objects = {
    "Schedule:Compact": {
        "Always0.8": {
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
                    "field": 0.8
                }
            ],
            "schedule_type_limits_name": "Any Number"
        },
        "Always6.8": {
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
                    "field": 6.8
                }
            ],
            "schedule_type_limits_name": "Any Number"
        },
        "Always12.5": {
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
                    "field": 12.5
                }
            ],
            "schedule_type_limits_name": "Any Number"
        },
        "Always62": {
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
                    "field": 62.0
                }
            ],
            "schedule_type_limits_name": "Any Number"
        }
    }
}


class TestSimulationsSystemDualDuct(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneDualDuct.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:minimum_inputs")
    def test_minimum_inputs(self):
        self.base_epjson['HVACTemplate:Zone:DualDuct']['HVACTemplate:Zone:DualDuct 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:DualDuct']['HVACTemplate:Zone:DualDuct 2'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:DualDuct']['HVACTemplate:Zone:DualDuct 3'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:DualDuct']['HVACTemplate:Zone:DualDuct 4'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:DualDuct']['HVACTemplate:Zone:DualDuct 5'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:System:DualDuct'].pop('SYS 1')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:System:DualDuct': {
                    'SYS 1': {
                        'economizer_type': 'NoEconomizer'
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:system_availability_schedule_name")
    def test_system_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'system_availability_schedule_name'] = 'OCCUPY-1'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'night_cycle_control'] = 'CycleOnAny'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Fan:ConstantVolume']['SYS 1 ColdDuct Supply Fan']['availability_schedule_name'])
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Fan:ConstantVolume']['SYS 1 HotDuct Supply Fan']['availability_schedule_name'])
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['AvailabilityManager:NightCycle']['SYS 1 Availability'][
                'fan_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "system_configuration_type_dual_fan_constant_volume")
    def test_system_configuration_type_dual_fan_constant_volume(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = 'DualFanConstantVolume'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Fan:ConstantVolume'].get('SYS 1 ColdDuct Supply Fan'))
        self.assertIsNotNone(
            epjson_output['Fan:ConstantVolume'].get('SYS 1 HotDuct Supply Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "system_configuration_type_single_fan_constant_volume")
    def test_system_configuration_type_single_fan_constant_volume(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanConstantVolume'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Fan:ConstantVolume'].get('SYS 1 Supply Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "system_configuration_type_dual_fan_variable_volume")
    def test_system_configuration_type_dual_fan_variable_volume(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = 'DualFanVariableVolume'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Fan:VariableVolume'].get('SYS 1 ColdDuct Supply Fan'))
        self.assertIsNotNone(
            epjson_output['Fan:VariableVolume'].get('SYS 1 HotDuct Supply Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "system_configuration_type_single_fan_variable_volume")
    def test_system_configuration_type_single_fan_variable_volume(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Fan:VariableVolume'].get('SYS 1 Supply Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_maximum_flow_rate_constant_volume")
    def test_main_supply_fan_maximum_flow_rate_constant_volume(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_maximum_flow_rate'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:System']['SYS 1 Sizing System']['cooling_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_maximum_flow_rate_variable_volume")
    def test_main_supply_fan_maximum_flow_rate_variable_volume(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_maximum_flow_rate'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:System']['SYS 1 Sizing System']['cooling_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_minimum_flow_fraction_variable_volume")
    def test_main_supply_fan_minimum_flow_fraction_variable_volume(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_minimum_flow_fraction'] = 0.15
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.15,
            epjson_output['Fan:VariableVolume']['SYS 1 Supply Fan']['fan_power_minimum_flow_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_total_efficiency")
    def test_main_supply_fan_total_efficiency(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_total_efficiency'] = 0.66
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.66,
            epjson_output['Fan:ConstantVolume']['SYS 1 Supply Fan']['fan_total_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_delta_pressure")
    def test_main_supply_fan_delta_pressure(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_delta_pressure'] = 750
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            750,
            epjson_output['Fan:ConstantVolume']['SYS 1 Supply Fan']['pressure_rise'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_delta_pressure")
    def test_main_supply_fan_motor_efficiency(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_motor_efficiency'] = 0.77
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.77,
            epjson_output['Fan:VariableVolume']['SYS 1 Supply Fan']['motor_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_motor_in_air_stream_fraction")
    def test_main_supply_fan_motor_in_air_stream_fraction(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_motor_in_air_stream_fraction'] = \
            0.85
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.85,
            epjson_output['Fan:VariableVolume']['SYS 1 Supply Fan']['motor_in_airstream_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_part_load_power_coefficients_inlet_vane_dampers")
    def test_main_supply_fan_part_load_power_coefficients_inlet_vane_dampers(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_part_load_power_coefficients'] = \
            'InletVaneDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.35071223,
            epjson_output['Fan:VariableVolume']['SYS 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_part_load_power_coefficients_outlet_dampers")
    def test_main_supply_fan_part_load_power_coefficients_outlet_dampers(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_part_load_power_coefficients'] = \
            'OutletDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.37073425,
            epjson_output['Fan:VariableVolume']['SYS 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_part_load_power_coefficients_variable_speed_motor")
    def test_main_supply_fan_part_load_power_coefficients_variable_speed_motor(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_part_load_power_coefficients'] = \
            'VariableSpeedMotor'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0015302446,
            epjson_output['Fan:VariableVolume']['SYS 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_part_load_power_coefficients_ashrae_appendix_g")
    def test_main_supply_fan_part_load_power_coefficients_ashrae_appendix_g(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_part_load_power_coefficients'] = \
            'ASHRAE90.1-2004AppendixG'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0013,
            epjson_output['Fan:VariableVolume']['SYS 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "main_supply_fan_part_load_power_coefficients"
                                              "_variable_speed_motor_pressure_reset")
    def test_main_supply_fan_part_load_power_coefficients_variable_speed_motor_pressure_reset(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['main_supply_fan_part_load_power_coefficients'] = \
            'VariableSpeedMotorPressureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.040759894,
            epjson_output['Fan:VariableVolume']['SYS 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_maximum_flow_rate_constant_volume")
    def test_cold_duct_supply_fan_maximum_flow_rate_constant_volume(self):
        # todo_eo: does not appear this value maps to anything
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_maximum_flow_rate'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:System']['SYS 1 Sizing System']['cooling_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_maximum_flow_rate_variable_volume")
    def test_cold_duct_supply_fan_maximum_flow_rate_variable_volume(self):
        # todo_eo: does not appear this value maps to anything
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_maximum_flow_rate'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:System']['SYS 1 Sizing System']['cooling_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_minimum_flow_fraction_variable_volume")
    def test_cold_duct_supply_fan_minimum_flow_fraction_variable_volume(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_minimum_flow_fraction'] = 0.15
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.15,
            epjson_output['Fan:VariableVolume']['SYS 1 ColdDuct Supply Fan']['fan_power_minimum_flow_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_total_efficiency")
    def test_cold_duct_supply_fan_total_efficiency(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_total_efficiency'] = 0.66
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.66,
            epjson_output['Fan:ConstantVolume']['SYS 1 ColdDuct Supply Fan']['fan_total_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_delta_pressure")
    def test_cold_duct_supply_fan_delta_pressure(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_delta_pressure'] = 750
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            750,
            epjson_output['Fan:ConstantVolume']['SYS 1 ColdDuct Supply Fan']['pressure_rise'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_motor_efficiency")
    def test_cold_duct_supply_fan_motor_efficiency(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_motor_efficiency'] = 0.77
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.77,
            epjson_output['Fan:VariableVolume']['SYS 1 ColdDuct Supply Fan']['motor_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_motor_in_air_stream_fraction")
    def test_cold_duct_supply_fan_motor_in_air_stream_fraction(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_motor_in_air_stream_fraction'] = 0.85
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.85,
            epjson_output['Fan:VariableVolume']['SYS 1 ColdDuct Supply Fan']['motor_in_airstream_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_part_load_power_coefficients_inlet_vane_dampers")
    def test_cold_duct_supply_fan_part_load_power_coefficients_inlet_vane_dampers(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_part_load_power_coefficients'] = 'InletVaneDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.35071223,
            epjson_output['Fan:VariableVolume']['SYS 1 ColdDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_part_load_power_coefficients_outlet_dampers")
    def test_cold_duct_supply_fan_part_load_power_coefficients_outlet_dampers(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_part_load_power_coefficients'] = 'OutletDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.37073425,
            epjson_output['Fan:VariableVolume']['SYS 1 ColdDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_part_load_power_coefficients_variable_speed_motor")
    def test_cold_duct_supply_fan_part_load_power_coefficients_variable_speed_motor(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_part_load_power_coefficients'] = 'VariableSpeedMotor'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0015302446,
            epjson_output['Fan:VariableVolume']['SYS 1 ColdDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_part_load_power_coefficients_ashrae_appendix_g")
    def test_cold_duct_supply_fan_part_load_power_coefficients_ashrae_appendix_g(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_part_load_power_coefficients'] = 'ASHRAE90.1-2004AppendixG'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0013,
            epjson_output['Fan:VariableVolume']['SYS 1 ColdDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_part_load_power_coefficients"
                                              "_variable_speed_motor_pressure_reset")
    def test_cold_duct_supply_fan_part_load_power_coefficients_variable_speed_motor_pressure_reset(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_part_load_power_coefficients'] = 'VariableSpeedMotorPressureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.040759894,
            epjson_output['Fan:VariableVolume']['SYS 1 ColdDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_placement_blow_through_constant")
    def test_cold_duct_supply_fan_placement_blow_through_constant(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_placement'] = 'BlowThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Supply Fan Outlet',
            epjson_output['Coil:Cooling:Water']['SYS 1 ColdDuct Cooling Coil']['air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_placement_blow_through_variable")
    def test_cold_duct_supply_fan_placement_blow_through_variable(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_placement'] = 'BlowThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Supply Fan Outlet',
            epjson_output['Coil:Cooling:Water']['SYS 1 ColdDuct Cooling Coil']['air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_placement_draw_through_constant")
    def test_cold_duct_supply_fan_placement_draw_through_constant(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_placement'] = 'DrawThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Coil Outlet',
            epjson_output['Fan:ConstantVolume']['SYS 1 ColdDuct Supply Fan']['air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_duct_supply_fan_placement_draw_through_variable")
    def test_cold_duct_supply_fan_placement_draw_through_variable(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_placement'] = 'DrawThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Coil Outlet',
            epjson_output['Fan:VariableVolume']['SYS 1 ColdDuct Supply Fan']['air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_maximum_flow_rate_constant_volume")
    def test_hot_duct_supply_fan_maximum_flow_rate_constant_volume(self):
        # todo_eo: does not appear this value maps to anything
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['hot_duct_supply_fan_maximum_flow_rate'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:System']['SYS 1 Sizing System']['heating_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_maximum_flow_rate_variable_volume")
    def test_hot_duct_supply_fan_maximum_flow_rate_variable_volume(self):
        # todo_eo: does not appear this value maps to anything
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['hot_duct_supply_fan_maximum_flow_rate'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:System']['SYS 1 Sizing System']['heating_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_minimum_flow_fraction_variable_volume")
    def test_hot_duct_supply_fan_minimum_flow_fraction_variable_volume(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['hot_duct_supply_fan_minimum_flow_fraction'] = 0.15
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.15,
            epjson_output['Fan:VariableVolume']['SYS 1 HotDuct Supply Fan']['fan_power_minimum_flow_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_total_efficiency")
    def test_hot_duct_supply_fan_total_efficiency(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['hot_duct_supply_fan_total_efficiency'] = 0.66
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.66,
            epjson_output['Fan:ConstantVolume']['SYS 1 HotDuct Supply Fan']['fan_total_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_delta_pressure")
    def test_hot_duct_supply_fan_delta_pressure(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['hot_duct_supply_fan_delta_pressure'] = 750
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            750,
            epjson_output['Fan:ConstantVolume']['SYS 1 HotDuct Supply Fan']['pressure_rise'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_motor_efficiency")
    def test_hot_duct_supply_fan_motor_efficiency(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['hot_duct_supply_fan_motor_efficiency'] = 0.77
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.77,
            epjson_output['Fan:VariableVolume']['SYS 1 HotDuct Supply Fan']['motor_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_motor_in_air_stream_fraction")
    def test_hot_duct_supply_fan_motor_in_air_stream_fraction(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_motor_in_air_stream_fraction'] = 0.85
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.85,
            epjson_output['Fan:VariableVolume']['SYS 1 HotDuct Supply Fan']['motor_in_airstream_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_part_load_power_coefficients_inlet_vane_dampers")
    def test_hot_duct_supply_fan_part_load_power_coefficients_inlet_vane_dampers(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_part_load_power_coefficients'] = 'InletVaneDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.35071223,
            epjson_output['Fan:VariableVolume']['SYS 1 HotDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_part_load_power_coefficients_outlet_dampers")
    def test_hot_duct_supply_fan_part_load_power_coefficients_outlet_dampers(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_part_load_power_coefficients'] = 'OutletDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.37073425,
            epjson_output['Fan:VariableVolume']['SYS 1 HotDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_part_load_power_coefficients_variable_speed_motor")
    def test_hot_duct_supply_fan_part_load_power_coefficients_variable_speed_motor(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_part_load_power_coefficients'] = 'VariableSpeedMotor'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0015302446,
            epjson_output['Fan:VariableVolume']['SYS 1 HotDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_part_load_power_coefficients_ashrae_appendix_g")
    def test_hot_duct_supply_fan_part_load_power_coefficients_ashrae_appendix_g(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_part_load_power_coefficients'] = 'ASHRAE90.1-2004AppendixG'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0013,
            epjson_output['Fan:VariableVolume']['SYS 1 HotDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_part_load_power_coefficients"
                                              "_variable_speed_motor_pressure_reset")
    def test_hot_duct_supply_fan_part_load_power_coefficients_variable_speed_motor_pressure_reset(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_part_load_power_coefficients'] = 'VariableSpeedMotorPressureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.040759894,
            epjson_output['Fan:VariableVolume']['SYS 1 HotDuct Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_placement_blow_through_constant")
    def test_hot_duct_supply_fan_placement_blow_through_constant(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_placement'] = 'BlowThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Supply Fan Outlet',
            epjson_output['Coil:Heating:Water']['SYS 1 HotDuct Heating Coil']['air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_placement_blow_through_variable")
    def test_hot_duct_supply_fan_placement_blow_through_variable(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_placement'] = 'BlowThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Supply Fan Outlet',
            epjson_output['Coil:Heating:Water']['SYS 1 HotDuct Heating Coil']['air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_placement_draw_through_constant")
    def test_hot_duct_supply_fan_placement_draw_through_constant(self):
        # todo_eo: EO fails with this option setup. It appears the hot branchlist is out of order
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_placement'] = 'DrawThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Cooling Coil Outlet',
            epjson_output['Fan:ConstantVolume']['SYS 1 HotDuct Supply Fan']['air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_duct_supply_fan_placement_draw_through_variable")
    def test_hot_duct_supply_fan_placement_draw_through_variable(self):
        # todo_eo: EO fails with this option setup.  It appears the hot branchlist is out of order
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_duct_supply_fan_placement'] = 'DrawThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Cooling Coil Outlet',
            epjson_output['Fan:VariableVolume']['SYS 1 HotDuct Supply Fan']['air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_type_chilled_water")
    def test_cooling_coil_type_chilled_water(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_type'] = 'ChilledWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Cooling:Water'].get('SYS 1 ColdDuct Cooling Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_type_chilled_water_detailed_flat_model")
    def test_cooling_coil_type_chilled_water_detailed_flat_model(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_type'] = 'ChilledWaterDetailedFlatModel'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Cooling:Water:DetailedGeometry'].get('SYS 1 ColdDuct Cooling Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_type_none")
    def test_cooling_coil_type_none(self):
        # todo_eo: EO fails with error message on conversion.  Similar error on simulation.
        #  Conversion message output: b"<root>[Branch][SYS 1 Cold Branch][components][1] - Missing required property
        #  'component_outlet_node_name'.\r\nErrors occurred when validating input file. Preceding condition(s) cause
        #  termination.\r\nInput file conversion failed:
        self.base_epjson.pop('HVACTemplate:Plant:Chiller')
        self.base_epjson.pop('HVACTemplate:Plant:ChilledWaterLoop')
        self.base_epjson.pop('HVACTemplate:Plant:Tower')
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Cooling:Water:DetailedGeometry'].get('SYS 1 ColdDuct Cooling Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_availability_schedule_name")
    def test_cooling_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_availability_schedule_name'] = \
            'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Cooling:Water']['SYS 1 ColdDuct Cooling Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_fixed_setpoint_"
                                              "dual_fan_draw_through")
    def test_cooling_coil_setpoint_control_type_fixed_setpoint_dual_draw_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'DrawThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Supply Fan Outlet',
            epjson_output['SetpointManager:Scheduled']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:MixedAir']['SYS 1 ColdDuct Cooling Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_fixed_setpoint_"
                                              "dual_fan_draw_through")
    def test_cooling_coil_setpoint_control_type_fixed_setpoint_dual_blow_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'BlowThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:Scheduled']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    def test_cooling_coil_setpoint_control_type_fixed_setpoint_single(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:Scheduled']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_scheduled_"
                                              "dual_fan_draw_through")
    def test_cooling_coil_setpoint_control_type_scheduled_dual_draw_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'DrawThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Supply Fan Outlet',
            epjson_output['SetpointManager:Scheduled']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:MixedAir']['SYS 1 ColdDuct Cooling Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_scheduled_"
                                              "dual_fan_blow_through")
    def test_cooling_coil_setpoint_control_type_scheduled_dual_blow_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'BlowThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:Scheduled']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_scheduled_"
                                              "single")
    def test_cooling_coil_setpoint_control_type_scheduled_single(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:Scheduled']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_outdoor_air_temperature_reset_"
                                              "dual_fan_draw_through")
    def test_cooling_coil_setpoint_control_type_outdoor_air_temperature_reset_dual_draw_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'DrawThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Supply Fan Outlet',
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:MixedAir']['SYS 1 ColdDuct Cooling Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_outdoor_air_temperature_reset_"
                                              "dual_fan_blow_through")
    def test_cooling_coil_setpoint_control_type_outdoor_air_temperature_reset_dual_blow_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'BlowThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_outdoor_air_temperature_reset_"
                                              "dual_fan_blow_through")
    def test_cooling_coil_setpoint_control_type_outdoor_air_temperature_reset_single(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_warmest_"
                                              "dual_fan_draw_through")
    def test_cooling_coil_setpoint_control_type_warmest_dual_draw_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'DrawThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'Warmest'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Supply Fan Outlet',
            epjson_output['SetpointManager:Warmest']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:MixedAir']['SYS 1 ColdDuct Cooling Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_warmest_"
                                              "dual_fan_draw_through")
    def test_cooling_coil_setpoint_control_type_warmest_dual_blow_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'BlowThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'Warmest'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:Warmest']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_control_type_warmest_"
                                              "single")
    def test_cooling_coil_setpoint_control_type_warmest_single(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'Warmest'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 ColdDuct Cooling Setpoint Nodes',
            epjson_output['SetpointManager:Warmest']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_warmest_temperature")
    def test_cooling_coil_warmest_temperature(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'Warmest'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_design_setpoint_temperature'] = 13
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            18.2,
            epjson_output['SetpointManager:Warmest']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'maximum_setpoint_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_design_setpoint_temperature")
    def test_cooling_coil_design_setpoint_temperature(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_design_setpoint_temperature'] = 13
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            13,
            epjson_output['Sizing:System']['SYS 1 Sizing System']['central_cooling_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cooling_coil_setpoint_schedule_name")
    def test_cooling_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'Scheduled'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_schedule_name'] = 'Always12.5'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always12.5',
            epjson_output['SetpointManager:Scheduled']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:cooling_coil_outdoor_reset_inputs")
    def test_cooling_coil_outdoor_reset_inputs(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cooling_coil_setpoint_control_type'] = \
            'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cooling_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cooling_coil_setpoint_at_outdoor_dry_bulb_low'] = 15.5
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cooling_coil_reset_outdoor_dry_bulb_low'] = 15.4
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cooling_coil_setpoint_at_outdoor_dry_bulb_high'] = 12.5
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cooling_coil_reset_outdoor_dry_bulb_high'] = 23.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            23.2,
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'outdoor_high_temperature'])
        self.assertEqual(
            12.5,
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_at_outdoor_high_temperature'])
        self.assertEqual(
            15.4,
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'outdoor_low_temperature'])
        self.assertEqual(
            15.5,
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 ColdDuct Cooling Supply Air Temp Manager'][
                'setpoint_at_outdoor_low_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_type_hot_water")
    def test_heating_coil_type_hot_water(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('SYS 1 HotDuct Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_type_electric")
    def test_heating_coil_type_electric(self):
        self.base_epjson.pop('HVACTemplate:Plant:Boiler')
        self.base_epjson.pop('HVACTemplate:Plant:HotWaterLoop')
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Electric'].get('SYS 1 HotDuct Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_type_gas")
    def test_heating_coil_type_gas(self):
        self.base_epjson.pop('HVACTemplate:Plant:Boiler')
        self.base_epjson.pop('HVACTemplate:Plant:HotWaterLoop')
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_type'] = 'Gas'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Fuel'].get('SYS 1 HotDuct Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_type_none")
    def test_heating_coil_type_none(self):
        # todo_eo: EO issues two messages when only one seems necessary
        self.base_epjson.pop('HVACTemplate:Plant:Boiler')
        self.base_epjson.pop('HVACTemplate:Plant:HotWaterLoop')
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Coil:Heating:Water'].get('SYS 1 HotDuct Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_availability_schedule_name")
    def test_heating_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_availability_schedule_name'] = \
            'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Water']['SYS 1 HotDuct Heating Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_setpoint_control_type_fixed_setpoint_draw_through")
    def test_heating_coil_setpoint_control_type_fixed_setpoint_draw_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'DrawThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_control_type'] = \
            'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Heating Coil Outlet',
            epjson_output['SetpointManager:Scheduled']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_setpoint_control_type_fixed_setpoint_blow_through")
    def test_heating_coil_setpoint_control_type_fixed_setpoint_blow_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'BlowThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_control_type'] = \
            'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Heating Coil Outlet',
            epjson_output['SetpointManager:Scheduled']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_setpoint_control_type_scheduled_draw_through")
    def test_heating_coil_setpoint_control_type_scheduled_draw_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'DrawThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_control_type'] = \
            'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Heating Coil Outlet',
            epjson_output['SetpointManager:Scheduled']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_setpoint_control_type_scheduled_blow_through")
    def test_heating_coil_setpoint_control_type_scheduled_blow_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'BlowThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_control_type'] = \
            'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Heating Coil Outlet',
            epjson_output['SetpointManager:Scheduled']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_setpoint_control_type_scheduled_draw_through")
    def test_heating_coil_setpoint_control_type_scheduled_draw_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'DrawThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_control_type'] = \
            'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Heating Coil Outlet',
            epjson_output['SetpointManager:Scheduled']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_setpoint_control_type_"
                                              "outdoor_air_temperature_reset_draw_through")
    def test_heating_coil_setpoint_control_type_outdoor_air_temperature_reset_draw_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'DrawThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_control_type'] = \
            'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Heating Coil Outlet',
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_setpoint_control_type_"
                                              "outdoor_air_temperature_reset_blow_through")
    def test_heating_coil_setpoint_control_type_outdoor_air_temperature_reset_blow_through(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['cold_duct_supply_fan_placement'] = \
            'BlowThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_control_type'] = \
            'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SYS 1 HotDuct Heating Coil Outlet',
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'setpoint_node_or_nodelist_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_design_setpoint")
    def test_heating_coil_design_setpoint(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_design_setpoint'] = 49
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            49,
            epjson_output['Sizing:System']['SYS 1 Sizing System']['central_heating_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_setpoint_schedule_name")
    def test_heating_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_control_type'] = \
            'Scheduled'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_schedule_name'] = 'Always12.5'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always12.5',
            epjson_output['SetpointManager:Scheduled']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:heating_coil_outdoor_reset_inputs")
    def test_heating_coil_outdoor_reset_inputs(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'DualFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_setpoint_control_type'] = \
            'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heatingg_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heating_coil_setpoint_at_outdoor_dry_bulb_low'] = 49
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heating_coil_reset_outdoor_dry_bulb_low'] = 7.8
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heating_coil_setpoint_at_outdoor_dry_bulb_high'] = 25
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heating_coil_reset_outdoor_dry_bulb_high'] = 12
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            12,
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'outdoor_high_temperature'])
        self.assertEqual(
            25,
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'setpoint_at_outdoor_high_temperature'])
        self.assertEqual(
            7.8,
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'outdoor_low_temperature'])
        self.assertEqual(
            49,
            epjson_output['SetpointManager:OutdoorAirReset']['SYS 1 HotDuct Heating Supply Air Temp Manager'][
                'setpoint_at_outdoor_low_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heating_coil_capacity")
    def test_heating_coil_capacity(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_capacity'] = 1000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1000,
            epjson_output['Coil:Heating:Water']['SYS 1 HotDuct Heating Coil']['rated_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "gas_heating_coil_inputs")
    def test_gas_heating_coil_inputs(self):
        self.base_epjson.pop('HVACTemplate:Plant:Boiler')
        self.base_epjson.pop('HVACTemplate:Plant:HotWaterLoop')
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heating_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['gas_heating_coil_efficiency'] = 0.77
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'gas_heating_coil_parasitic_electric_load'] = 1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.77,
            epjson_output['Coil:Heating:Fuel']['SYS 1 HotDuct Heating Coil']['burner_efficiency'])
        self.assertEqual(
            1,
            epjson_output['Coil:Heating:Fuel']['SYS 1 HotDuct Heating Coil']['parasitic_electric_load'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "preheat_coil_type_none")
    def test_preheat_coil_type_none(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['preheat_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        # epjson_output = self.ej._get_json_file(test_dir.joinpath(
        #     '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "preheat_coil_type_hot_water")
    def test_preheat_coil_type_hot_water(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['preheat_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('SYS 1 Preheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "preheat_coil_type_electric")
    def test_preheat_coil_type_electric(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['preheat_coil_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Electric'].get('SYS 1 Preheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "preheat_coil_type_gas")
    def test_preheat_coil_type_gas(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['preheat_coil_type'] = 'Gas'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Fuel'].get('SYS 1 Preheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:preheat_coil_availability_schedule_name")
    def test_preheat_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'preheat_coil_type'] = 'Electric'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'preheat_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Electric']['SYS 1 Preheat Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:preheat_coil_design_setpoint")
    def test_preheat_coil_design_setpoint(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'preheat_coil_design_setpoint'] = 7.0
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            7.0,
            epjson_output['Sizing:System']['SYS 1 Sizing System']['preheat_design_temperature'])
        self.assertEqual(
            'HVACTemplate-Always7.0',
            epjson_output['SetpointManager:Scheduled']['SYS 1 Preheat Coil Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:preheat_coil_setpoint_schedule_name")
    def test_preheat_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'preheat_coil_setpoint_schedule_name'] = 'Always6.8'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always6.8',
            epjson_output['SetpointManager:Scheduled']['SYS 1 Preheat Coil Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:gas_preheat_inputs")
    def test_gas_preheat_inputs(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'preheat_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'gas_preheat_coil_efficiency'] = 0.75
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'gas_preheat_coil_parasitic_electric_load'] = 1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.75,
            epjson_output['Coil:Heating:Fuel']['SYS 1 Preheat Coil']['burner_efficiency'])
        self.assertEqual(
            1,
            epjson_output['Coil:Heating:Fuel']['SYS 1 Preheat Coil']['parasitic_electric_load'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:outdoor_air_flow_rates")
    def test_outdoor_air_flow_rates(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'maximum_outdoor_air_flow_rate'] = 1.0
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'minimum_outdoor_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.0,
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['maximum_outdoor_air_flow_rate'])
        self.assertEqual(
            0.1,
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['minimum_outdoor_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "minimum_outdoor_air_control_type_proportional_minimum")
    def test_minimum_outdoor_air_control_type_proportional_minimum(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'minimum_outdoor_air_control_type'] = 'ProportionalMinimum'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ProportionalMinimum',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['minimum_limit_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "minimum_outdoor_air_control_type_fixed_minimum")
    def test_minimum_outdoor_air_control_type_fixed_minimum(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'minimum_outdoor_air_control_type'] = 'FixedMinimum'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedMinimum',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['minimum_limit_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:minimum_outdoor_air_schedule_name")
    def test_minimum_outdoor_air_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'minimum_outdoor_air_schedule_name'] = 'Always0.8'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always0.8',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['minimum_outdoor_air_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:economizer_type_no_economizer")
    def test_economizer_type_no_economizer(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_type'] = 'NoEconomizer'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'NoEconomizer',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:economizer_type_fixed_dry_bulb")
    def test_economizer_type_fixed_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_type'] = 'FixedDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedDryBulb',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:economizer_type_fixed_enthalpy")
    def test_economizer_type_fixed_enthalpy(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_type'] = 'FixedDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedDryBulb',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:economizer_type_differential_dry_bulb")
    def test_economizer_type_differential_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_duct_supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_type'] = 'DifferentialDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialDryBulb',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:economizer_type_differential_enthalpy")
    def test_economizer_type_differential_enthalpy(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_type'] = 'DifferentialEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialEnthalpy',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "economizer_type_fixed_dew_point_and_dry_bulb")
    def test_economizer_type_fixed_dew_point_and_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_type'] = 'FixedDewPointAndDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedDewPointAndDryBulb',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "economizer_type_electronic_enthalpy")
    def test_economizer_type_electronic_enthalpy(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_type'] = 'ElectronicEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ElectronicEnthalpy',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "economizer_type_differential_dry_bulb_and_enthalpy")
    def test_economizer_type_differential_dry_bulb_and_enthalpy(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = \
            'SingleFanVariableVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_type'] = 'DifferentialDryBulbAndEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialDryBulbAndEnthalpy',
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:economizer_upper_temperature_limit")
    def test_economizer_upper_temperature_limit(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_upper_temperature_limit'] = 18
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            18,
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller'][
                'economizer_maximum_limit_dry_bulb_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:economizer_lower_temperature_limit")
    def test_economizer_lower_temperature_limit(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_lower_temperature_limit'] = 6
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            6,
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller'][
                'economizer_minimum_limit_dry_bulb_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:economizer_upper_enthalpy_limit")
    def test_economizer_upper_enthalpy_limit(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_upper_enthalpy_limit'] = 100
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            100,
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller']['economizer_maximum_limit_enthalpy'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "economizer_maximum_limit_dewpoint_temperature")
    def test_economizer_maximum_limit_dewpoint_temperature(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'economizer_maximum_limit_dewpoint_temperature'] = 20
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            20,
            epjson_output['Controller:OutdoorAir']['SYS 1 OA Controller'][
                'economizer_maximum_limit_dewpoint_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_supply_plenum_name")
    def test_cold_supply_plenum_name(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'].pop('return_plenum_name')
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_supply_plenum_name'] = 'PLENUM-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['AirLoopHVAC:SupplyPlenum'].get('SYS 1 Cold Supply Plenum'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "hot_supply_plenum_name")
    def test_hot_supply_plenum_name(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'].pop('return_plenum_name')
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_supply_plenum_name'] = 'PLENUM-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['AirLoopHVAC:SupplyPlenum'].get('SYS 1 Hot Supply Plenum'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cold_and_hot_supply_plenum_name")
    def test_cold_and_hot_supply_plenum_name(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'].pop('return_plenum_name')
        self.base_epjson['Zone']['PLENUM-2'] = \
            self.base_epjson['Zone']['PLENUM-1']
        self.base_epjson['BuildingSurface:Detailed']['C1-2P'] = \
            self.base_epjson['BuildingSurface:Detailed']['C1-1P']
        self.base_epjson['BuildingSurface:Detailed']['C1-2P']['zone_name'] = 'PLENUM-2'
        self.base_epjson['BuildingSurface:Detailed']['C2-2P'] = \
            self.base_epjson['BuildingSurface:Detailed']['C2-1P']
        self.base_epjson['BuildingSurface:Detailed']['C2-2P']['zone_name'] = 'PLENUM-2'
        self.base_epjson['BuildingSurface:Detailed']['C3-2P'] = \
            self.base_epjson['BuildingSurface:Detailed']['C3-1P']
        self.base_epjson['BuildingSurface:Detailed']['C3-2P']['zone_name'] = 'PLENUM-2'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'cold_supply_plenum_name'] = 'PLENUM-1'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'hot_supply_plenum_name'] = 'PLENUM-2'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['AirLoopHVAC:SupplyPlenum'].get('SYS 1 Hot Supply Plenum'))
        self.assertIsNotNone(
            epjson_output['AirLoopHVAC:SupplyPlenum'].get('SYS 1 Cold Supply Plenum'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "return_plenum_name")
    def test_return_plenum_name_none(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'].pop('return_plenum_name')
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output.get('AirLoopHVAC:ReturnPlenum'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "return_plenum_name")
    def test_return_plenum_name(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['return_plenum_name'] = 'PLENUM-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['AirLoopHVAC:ReturnPlenum'].get('SYS 1 Return Plenum'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "night_cycle_control_stay_off")
    def test_night_cycle_control_stay_off(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['night_cycle_control'] = 'StayOff'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'StayOff',
            epjson_output['AvailabilityManager:NightCycle']['SYS 1 Availability']['control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "night_cycle_control_cycle_on_any")
    def test_night_cycle_control_cycle_on_any(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['night_cycle_control'] = 'CycleOnAny'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'CycleOnAny',
            epjson_output['AvailabilityManager:NightCycle']['SYS 1 Availability']['control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "cycle_on_control_zone")
    def test_night_cycle_control_cycle_on_control_zone(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['night_cycle_control'] = 'CycleOnControlZone'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['night_cycle_control_zone_name'] = 'SPACE1-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'CycleOnControlZone',
            epjson_output['AvailabilityManager:NightCycle']['SYS 1 Availability']['control_type'])
        self.assertEqual(
            'SPACE1-1',
            epjson_output['AvailabilityManager:NightCycle']['SYS 1 Availability']['control_zone_or_zone_list_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_none")
    def test_heat_recovery_type_none(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heat_recovery_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_sensible")
    def test_heat_recovery_type_sensible(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_sensible")
    def test_heat_recovery_type_enthalpy(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:heat_recovery_effectiveness_sensible")
    def test_heat_recovery_effectiveness_sensible(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_type'] = 'Sensible'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'sensible_heat_recovery_effectiveness'] = 0.72
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'sensible_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'sensible_effectiveness_at_75_heating_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'sensible_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'sensible_effectiveness_at_100_heating_air_flow'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:heat_recovery_effectiveness_enthalpy")
    def test_heat_recovery_effectiveness_enthalpy(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'sensible_heat_recovery_effectiveness'] = 0.72
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'latent_heat_recovery_effectiveness'] = 0.61
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'sensible_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'sensible_effectiveness_at_75_heating_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'sensible_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'sensible_effectiveness_at_100_heating_air_flow'])
        self.assertEqual(
            0.61,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'latent_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.61,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'latent_effectiveness_at_100_heating_air_flow'])
        self.assertEqual(
            0.66,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'latent_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.66,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery'][
                'latent_effectiveness_at_75_heating_air_flow'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:heat_recovery_exchanger_type_plate_enthalpy")
    def test_heat_recovery_exchanger_type_plate_enthalpy(self):
        # todo_eo: This input is frozen for enthalpy HR.  This is common to the input type so affects other systems
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_heat_exchanger_type'] = 'Plate'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Rotary',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery']['heat_exchanger_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:heat_recovery_exchanger_type_plate_sensible")
    def test_heat_recovery_exchanger_type_plate_sensible(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_type'] = 'Sensible'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_heat_exchanger_type'] = 'Plate'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Plate',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery']['heat_exchanger_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:heat_recovery_exchanger_type_rotary")
    def test_heat_recovery_exchanger_type_rotary(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_heat_exchanger_type'] = 'Rotary'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Rotary',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery']['heat_exchanger_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:heat_recovery_frost_control_type_none")
    def test_heat_recovery_frost_control_type_none_enthalpy(self):
        # todo_eo: This input is frozen for enthalpy HR.  This is common to the input type so affects other systems
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_frost_control_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'MinimumExhaustTemperature',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:heat_recovery_frost_control_type_none")
    def test_heat_recovery_frost_control_type_none_sensible(self):
        # todo_eo: This input is frozen for enthalpy HR.  This is common to the input type so affects other systems
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_type'] = 'Sensible'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_frost_control_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ExhaustOnly',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heat_recovery_frost_control_type_exhaust_air_recirculation_enthalpy")
    def test_heat_recovery_frost_control_type_exhaust_air_recirculation_enthalpy(self):
        # todo_eo: This input is frozen for enthalpy HR.  This is common to the input type so affects other systems
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_frost_control_type'] = 'ExhaustAirRecirculation'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'MinimumExhaustTemperature',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "heat_recovery_frost_control_type_exhaust_air_recirculation_sensible")
    def test_heat_recovery_frost_control_type_exhaust_air_recirculation_sensible(self):
        # todo_eo: This input is frozen for enthalpy HR.  This is common to the input type so affects other systems
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_type'] = 'Sensible'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'heat_recovery_frost_control_type'] = 'ExhaustAirRecirculation'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ExhaustOnly',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['SYS 1 Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "dehumidification_control_type_none")
    def test_dehumidification_control_type_none(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'dehumidification_control_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output.get('SetpointManager:SingleZone:Humidity:Maximum'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "dehumidification_control_type_cool_reheat")
    def test_dehumidification_control_type_cool_reheat(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'dehumidification_control_type'] = 'CoolReheat'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'dehumidification_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'dehumidification_relative_humidity_setpoint'] = 62
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SPACE1-1 Zone Air Node',
            epjson_output['SetpointManager:SingleZone:Humidity:Maximum'][
                'SYS 1 ColdDuct Dehumidification Setpoint Manager']['control_zone_air_node_name'])
        self.assertEqual(
            'HVACTemplate-Always62.0',
            epjson_output['ZoneControl:Humidistat'][
                'SYS 1 Dehumidification Humidistat']['dehumidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:"
                                              "dehumidification_control_type_cool_reheat")
    def test_dehumidification_relative_humidity_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'dehumidification_control_type'] = 'CoolReheat'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'dehumidification_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'dehumidification_relative_humidity_setpoint_schedule_name'] = 'Always62'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SPACE1-1 Zone Air Node',
            epjson_output['SetpointManager:SingleZone:Humidity:Maximum'][
                'SYS 1 ColdDuct Dehumidification Setpoint Manager']['control_zone_air_node_name'])
        self.assertEqual(
            'Always62',
            epjson_output['ZoneControl:Humidistat'][
                'SYS 1 Dehumidification Humidistat']['dehumidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:humidifier_type_none")
    def test_humidifier_type_none(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output.get('Humidifier:Steam:Electric'))
        self.assertIsNone(epjson_output.get('SetpointManager:SingleZone:Humidity:Minimum'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:humidifier_type_electric_steam")
    def test_humidifier_type_electric_steam(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_relative_humidity_setpoint'] = 31
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Humidifier:Steam:Electric'))
        self.assertEqual(
            'SPACE1-1 Zone Air Node',
            epjson_output['SetpointManager:SingleZone:Humidity:Minimum'][
                'SYS 1 HotDuct Humidification Setpoint Manager']['control_zone_air_node_name'])
        self.assertEqual(
            'HVACTemplate-Always31.0',
            epjson_output['ZoneControl:Humidistat']['SYS 1 Humidification Humidistat'][
                'humidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:humidifier_type_electric_steam")
    def test_humidifier_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_relative_humidity_setpoint'] = 31
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Humidifier:Steam:Electric'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Humidifier:Steam:Electric']['SYS 1 HotDuct Humidifier']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:humidifier_inputs")
    def test_humidifier_inputs(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_rated_capacity'] = 1000
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'humidifier_rated_electric_power'] = 1002
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Humidifier:Steam:Electric'))
        self.assertEqual(
            1000,
            epjson_output['Humidifier:Steam:Electric']['SYS 1 HotDuct Humidifier']['rated_capacity'])
        self.assertEqual(
            1002,
            epjson_output['Humidifier:Steam:Electric']['SYS 1 HotDuct Humidifier']['rated_power'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:sizing_option_non_coincident")
    def test_sizing_option_non_coincident(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'sizing_option'] = 'NonCoincident'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'NonCoincident',
            epjson_output['Sizing:System']['SYS 1 Sizing System']['type_of_zone_sum_to_use'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:sizing_option_coincident")
    def test_sizing_option_coincident(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1'][
            'sizing_option'] = 'Coincident'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Coincident',
            epjson_output['Sizing:System']['SYS 1 Sizing System']['type_of_zone_sum_to_use'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:return_fan_no")
    def test_return_fan_no(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['system_configuration_type'] = 'DualFanConstantVolume'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['return_fan'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output['Fan:ConstantVolume'].get('SYS 1 Return Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:return_fan_yes")
    def test_return_fan_yes(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['return_fan'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Fan:ConstantVolume'].get('SYS 1 Return Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DualDuct:return_fan_inputs")
    def test_return_fan_inputs(self):
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['return_fan'] = 'Yes'
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['return_fan_total_efficiency'] = 0.72
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['return_fan_delta_pressure'] = 295
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['return_fan_motor_efficiency'] = 0.85
        self.base_epjson['HVACTemplate:System:DualDuct']['SYS 1']['return_fan_motor_in_air_stream_fraction'] = 0.9
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.72,
            epjson_output['Fan:ConstantVolume']['SYS 1 Return Fan']['fan_total_efficiency'])
        self.assertEqual(
            295,
            epjson_output['Fan:ConstantVolume']['SYS 1 Return Fan']['pressure_rise'])
        self.assertEqual(
            0.85,
            epjson_output['Fan:ConstantVolume']['SYS 1 Return Fan']['motor_efficiency'])
        self.assertEqual(
            0.9,
            epjson_output['Fan:ConstantVolume']['SYS 1 Return Fan']['motor_in_airstream_fraction'])
        return
