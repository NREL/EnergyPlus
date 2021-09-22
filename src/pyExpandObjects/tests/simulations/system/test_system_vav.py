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
        "Always15.5": {
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
                    "field": 15.5
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


class TestSimulationsSystemVAV(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath(
            '..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVWaterCooled.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:minimum_inputs")
    # todo_eo: EO fails when transitioning to epJSON
    #  Conversion message output: b'<root>[Controller:OutdoorAir][VAV Sys 1 OA Controller][economizer_control_type]
    #  - "None" - Failed to match against any enum values.
    def test_minimum_inputs(self):
        self.base_epjson['HVACTemplate:Zone:VAV']['HVACTemplate:Zone:VAV 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:VAV']['HVACTemplate:Zone:VAV 2'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:VAV']['HVACTemplate:Zone:VAV 3'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:VAV']['HVACTemplate:Zone:VAV 4'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:VAV']['HVACTemplate:Zone:VAV 5'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:System:VAV'].pop('VAV Sys 1')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:System:VAV': {
                    'VAV Sys 1': {
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:system_availability_schedule_name")
    def test_system_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1']['system_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:supply_fan_maximum_flow_rate")
    def test_supply_fan_maximum_flow_rate(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1']['supply_fan_maximum_flow_rate'] = 2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            2.0,
            epjson_output['Sizing:System']['VAV Sys 1 Sizing System']['cooling_supply_air_flow_rate'])
        self.assertEqual(
            'Flow/System',
            epjson_output['Sizing:System']['VAV Sys 1 Sizing System']['cooling_supply_air_flow_rate_method'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:supply_fan_minimum_flow_rate")
    def test_supply_fan_minimum_flow_rate(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1']['supply_fan_minimum_flow_rate'] = 0.25
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.25,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['fan_power_minimum_air_flow_rate'])
        self.assertEqual(
            'FixedFlowRate',
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['fan_power_minimum_flow_rate_input_method'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:supply_fan_total_efficiency")
    def test_supply_fan_total_efficiency(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1']['supply_fan_total_efficiency'] = 0.75
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.75,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['fan_total_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:supply_fan_delta_pressure")
    def test_supply_fan_delta_pressure(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1']['supply_fan_delta_pressure'] = 1100
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1100,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['pressure_rise'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:supply_fan_motor_efficiency")
    def test_supply_fan_motor_efficiency(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1']['supply_fan_motor_efficiency'] = 0.8
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.8,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['motor_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "supply_fan_motor_in_air_stream_fraction")
    def test_supply_fan_motor_in_air_stream_fraction(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'supply_fan_motor_in_air_stream_fraction'] = 0.8
        base_file_path = self.create_idf_file_from_epjson(
            epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.8,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['motor_in_airstream_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "cooling_coil_type_chilled_water")
    def test_cooling_coil_type_chilled_water(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'cooling_coil_type'] = 'ChilledWater'
        base_file_path = self.create_idf_file_from_epjson(
            epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Cooling:Water'].get('VAV Sys 1 Cooling Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "cooling_coil_type_chilled_water_detailed_flat_model")
    def test_cooling_coil_type_chilled_water_detailed_flat_model(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'cooling_coil_type'] = 'ChilledWaterDetailedFlatModel'
        base_file_path = self.create_idf_file_from_epjson(
            epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Cooling:Water:DetailedGeometry'].get('VAV Sys 1 Cooling Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "cooling_coil_availability_schedule_name")
    def test_cooling_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'cooling_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Cooling:Water']['VAV Sys 1 Cooling Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "cooling_coil_setpoint_schedule_name")
    def test_cooling_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'cooling_coil_setpoint_schedule_name'] = 'Always12.5'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'cooling_coil_setpoint_reset_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always12.5',
            epjson_output['SetpointManager:Scheduled']['VAV Sys 1 Cooling Supply Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "cooling_coil_design_setpoint")
    def test_cooling_coil_design_setpoint(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'cooling_coil_design_setpoint'] = 13
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            13,
            epjson_output['Sizing:System']['VAV Sys 1 Sizing System'][
                'central_cooling_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:heating_coil_type_none")
    def test_heating_coil_type_none(self):
        # todo_eo: legacy issues two warnings that are very similar.  It seems only one should be output.
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heating_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Coil:Heating:Water'].get('VAV Sys 1 Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:heating_coil_type_hot_water")
    def test_heating_coil_type_hot_water(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heating_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('VAV Sys 1 Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:heating_coil_type_electric")
    def test_heating_coil_type_electric(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heating_coil_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Electric'].get('VAV Sys 1 Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:heating_coil_type_gas")
    def test_heating_coil_type_gas(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heating_coil_type'] = 'Gas'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Fuel'].get('VAV Sys 1 Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "heating_coil_availability_schedule_name")
    def test_heating_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heating_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Water']['VAV Sys 1 Heating Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "heating_coil_setpoint_schedule_name")
    def test_heating_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heating_coil_setpoint_schedule_name'] = 'Always6.8'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heating_coil_setpoint_reset_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always6.8',
            epjson_output['SetpointManager:Scheduled']['VAV Sys 1 Heating Supply Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "heating_coil_design_setpoint")
    def test_heating_coil_design_setpoint(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heating_coil_design_setpoint'] = 9.0
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            9.0,
            epjson_output['Sizing:System']['VAV Sys 1 Sizing System'][
                'central_heating_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:gas_heating_coil_inputs")
    def test_gas_heating_coil_inputs(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heating_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'gas_heating_coil_efficiency'] = 0.77
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'gas_heating_coil_parasitic_electric_load'] = 1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.77,
            epjson_output['Coil:Heating:Fuel']['VAV Sys 1 Heating Coil']['burner_efficiency'])
        self.assertEqual(
            1,
            epjson_output['Coil:Heating:Fuel']['VAV Sys 1 Heating Coil']['parasitic_electric_load'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:preheat_coil_type_none")
    def test_preheat_coil_type_none(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['Coil:Heating:Water'].get('VAV Sys 1 Preheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:preheat_coil_type_hot_water")
    def test_preheat_coil_type_hot_water(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('VAV Sys 1 Preheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "preheat_coil_type_hot_water_heat_recovery_type_sensible")
    def test_preheat_coil_type_none_heat_recovery_type_sensible(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'None'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('VAV Sys 1 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "preheat_coil_type_hot_water_heat_recovery_type_enthalpy")
    def test_preheat_coil_type_none_heat_recovery_type_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'None'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('VAV Sys 1 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "preheat_coil_type_hot_water_heat_recovery_type_sensible")
    def test_preheat_coil_type_hot_water_heat_recovery_type_sensible(self):
        # todo_eo: OutdoorAirSystem:EquipmentList for EO has a different order of components, and does not appear to
        #  travel in air flow path.  There is a slight variation, test if the order changes anything and report
        #  regardless
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('VAV Sys 1 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('VAV Sys 1 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "preheat_coil_type_hot_water_heat_recovery_type_enthalpy")
    def test_preheat_coil_type_hot_water_heat_recovery_type_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('VAV Sys 1 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('VAV Sys 1 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "preheat_coil_type_electric_heat_recovery_type_sensible")
    def test_preheat_coil_type_electric_heat_recovery_type_sensible(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'Electric'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Electric'].get('VAV Sys 1 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('VAV Sys 1 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "preheat_coil_type_electric_heat_recovery_type_enthalpy")
    def test_preheat_coil_type_electric_heat_recovery_type_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'Electric'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Electric'].get('VAV Sys 1 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('VAV Sys 1 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "preheat_coil_type_gas_heat_recovery_type_sensible")
    def test_preheat_coil_type_gas_heat_recovery_type_sensible(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Fuel'].get('VAV Sys 1 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('VAV Sys 1 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "preheat_coil_type_gas_heat_recovery_type_enthalpy")
    def test_preheat_coil_type_gas_heat_recovery_type_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Fuel'].get('VAV Sys 1 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('VAV Sys 1 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "preheat_coil_availability_schedule_name")
    def test_preheat_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_availability_schedule_name'] = 'OCCUPY-1'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Water']['VAV Sys 1 Preheat Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:preheat_coil_setpoint_schedule_name")
    def test_preheat_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_setpoint_schedule_name'] = 'Always6.8'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always6.8',
            epjson_output['SetpointManager:Scheduled']['VAV Sys 1 Preheat Coil Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:preheat_coil_design_setpoint")
    def test_preheat_coil_design_setpoint(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_design_setpoint'] = 7.0
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            7.0,
            epjson_output['Sizing:System']['VAV Sys 1 Sizing System']['preheat_design_temperature'])
        self.assertEqual(
            'HVACTemplate-Always7.0',
            epjson_output['SetpointManager:Scheduled']['VAV Sys 1 Preheat Coil Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:gas_preheat_coil_inputs")
    def test_gas_preheat_coil_inputs(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'preheat_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'gas_preheat_coil_efficiency'] = 0.77
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'gas_preheat_coil_parasitic_electric_load'] = 1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.77,
            epjson_output['Coil:Heating:Fuel']['VAV Sys 1 Preheat Coil']['burner_efficiency'])
        self.assertEqual(
            1,
            epjson_output['Coil:Heating:Fuel']['VAV Sys 1 Preheat Coil']['parasitic_electric_load'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "outdoor_air_flow_rates")
    def test_outdoor_air_flow_rates(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'maximum_outdoor_air_flow_rate'] = 0.66
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'minimum_outdoor_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.66,
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['maximum_outdoor_air_flow_rate'])
        self.assertEqual(
            0.1,
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['minimum_outdoor_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "minimum_outdoor_air_control_type_proportional_minimum")
    def test_minimum_outdoor_air_control_type_proportional_minimum(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'minimum_outdoor_air_control_type'] = 'ProportionalMinimum'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ProportionalMinimum',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['minimum_limit_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "minimum_outdoor_air_control_type_fixed_minimum")
    def test_minimum_outdoor_air_control_type_fixed_minimum(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'minimum_outdoor_air_control_type'] = 'FixedMinimum'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedMinimum',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['minimum_limit_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:minimum_outdoor_air_schedule_name")
    def test_minimum_outdoor_air_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'minimum_outdoor_air_schedule_name'] = 'Always0.8'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always0.8',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['minimum_outdoor_air_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_type_no_economizer")
    def test_economizer_type_no_economizer(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'NoEconomizer'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'NoEconomizer',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_type_fixed_dry_bulb")
    def test_economizer_type_fixed_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'FixedDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedDryBulb',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_type_fixed_enthalpy")
    def test_economizer_type_fixed_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'FixedEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedEnthalpy',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_type_differential_dry_bulb")
    def test_economizer_type_differential_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'DifferentialDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialDryBulb',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_type_differential_enthalpy")
    def test_economizer_type_differential_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'DifferentialEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialEnthalpy',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "economizer_type_fixed_dew_point_and_dry_bulb")
    def test_economizer_type_fixed_dew_point_and_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'FixedDewPointAndDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedDewPointAndDryBulb',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_type_electronic_enthalpy")
    def test_economizer_type_electronic_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'ElectronicEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ElectronicEnthalpy',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_type_"
                                              "differential_dry_bulb_and_enthalpy")
    def test_economizer_type_differential_dry_bulb_and_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'DifferentialDryBulbAndEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialDryBulbAndEnthalpy',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_lockout_no_lockout")
    def test_economizer_lockout_no_lockout(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_lockout'] = 'NoLockout'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'NoLockout',
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['lockout_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_temperature_limits")
    def test_economizer_temperature_limits(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'FixedDryBulb'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_upper_temperature_limit'] = 18
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_lower_temperature_limit'] = 5
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            18,
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller'][
                'economizer_maximum_limit_dry_bulb_temperature'])
        self.assertEqual(
            5,
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller'][
                'economizer_minimum_limit_dry_bulb_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:economizer_upper_enthalpy_limit")
    def test_economizer_upper_enthalpy_limit(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_upper_enthalpy_limit'] = 100
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            100,
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller']['economizer_maximum_limit_enthalpy'])
        return

    def test_economizer_maximum_limit_dewpoint_temperature(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_type'] = 'FixedDewPointAndDryBulb'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'economizer_maximum_limit_dewpoint_temperature'] = 7.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            7.1,
            epjson_output['Controller:OutdoorAir']['VAV Sys 1 OA Controller'][
                'economizer_maximum_limit_dewpoint_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:supply_plenum_name")
    def test_supply_plenum_name(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'].pop('return_plenum_name')
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'supply_plenum_name'] = 'PLENUM-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'PLENUM-1',
            epjson_output['AirLoopHVAC:SupplyPlenum']['VAV Sys 1 Supply Plenum']['zone_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:return_plenum_name")
    def test_return_plenum_name(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_plenum_name'] = 'PLENUM-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'PLENUM-1',
            epjson_output['AirLoopHVAC:ReturnPlenum']['VAV Sys 1 Return Plenum']['zone_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "supply_fan_part_load_power_coefficients_inlet_vane_dampers")
    def test_supply_fan_part_load_power_coefficients_inlet_vane_dampers(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'supply_fan_part_load_power_coefficients'] = 'InletVaneDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.35071223,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "supply_fan_part_load_power_coefficients_outlet_dampers")
    def test_supply_fan_part_load_power_coefficients_outlet_dampers(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'supply_fan_part_load_power_coefficients'] = 'OutletDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.37073425,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "supply_fan_part_load_power_coefficients_variable_speed_motor")
    def test_supply_fan_part_load_power_coefficients_variable_speed_motor(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'supply_fan_part_load_power_coefficients'] = 'VariableSpeedMotor'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0015302446,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "supply_fan_part_load_power_coefficients_ASHRAE901_2004_appendix_g")
    def test_supply_fan_part_load_power_coefficients_ASHRAE901_2004_appendix_g(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'supply_fan_part_load_power_coefficients'] = 'ASHRAE90.1-2004AppendixG'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0013,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "supply_fan_part_load_power_coefficients_"
                                              "variable_speed_motor_pressure_resst")
    def test_supply_fan_part_load_power_coefficients_variable_speed_motor_pressure_reset(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'supply_fan_part_load_power_coefficients'] = 'VariableSpeedMotorPressureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.040759894,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Supply Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:night_cycle_control_stay_off")
    def test_night_cycle_control_stay_off(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'night_cycle_control'] = 'StayOff'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'StayOff',
            epjson_output['AvailabilityManager:NightCycle']['VAV Sys 1 Availability']['control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:night_cycle_control_cycle_on_any")
    def test_night_cycle_control_cycle_on_any(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'night_cycle_control'] = 'CycleOnAny'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'CycleOnAny',
            epjson_output['AvailabilityManager:NightCycle']['VAV Sys 1 Availability']['control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:night_cycle_control_cycle_on_control_zone")
    def test_night_cycle_control_cycle_on_control_zone(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'night_cycle_control'] = 'CycleOnControlZone'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'night_cycle_control_zone_name'] = 'SPACE1-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'CycleOnControlZone',
            epjson_output['AvailabilityManager:NightCycle']['VAV Sys 1 Availability']['control_type'])
        self.assertEqual(
            'SPACE1-1',
            epjson_output['AvailabilityManager:NightCycle']['VAV Sys 1 Availability']['control_zone_or_zone_list_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:night_cycle_control_"
                                              "cycle_on_any_zone_fans_only")
    def test_night_cycle_control_cycle_on_any_zone_fans_only(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'night_cycle_control'] = 'CycleOnAnyZoneFansOnly'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'night_cycle_control_zone_name'] = 'SPACE1-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'CycleOnAnyZoneFansOnly',
            epjson_output['AvailabilityManager:NightCycle']['VAV Sys 1 Availability']['control_type'])
        self.assertEqual(
            'SPACE1-1',
            epjson_output['AvailabilityManager:NightCycle']['VAV Sys 1 Availability'][
                'control_zone_or_zone_list_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:heat_recovery_sensible")
    def test_heat_recovery_sensible(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:heat_recovery_enthalpy")
    def test_heat_recovery_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:heat_recovery_effectiveness_sensible")
    def test_heat_recovery_effectiveness_sensible(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Sensible'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'sensible_heat_recovery_effectiveness'] = 0.72
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'sensible_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'sensible_effectiveness_at_75_heating_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'sensible_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'sensible_effectiveness_at_100_heating_air_flow'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:heat_recovery_effectiveness_enthalpy")
    def test_heat_recovery_effectiveness_enthalpy(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'sensible_heat_recovery_effectiveness'] = 0.72
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'latent_heat_recovery_effectiveness'] = 0.61
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'sensible_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'sensible_effectiveness_at_75_heating_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'sensible_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'sensible_effectiveness_at_100_heating_air_flow'])
        self.assertEqual(
            0.61,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'latent_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.61,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'latent_effectiveness_at_100_heating_air_flow'])
        self.assertEqual(
            0.66,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'latent_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.66,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['VAV Sys 1 Heat Recovery'][
                'latent_effectiveness_at_75_heating_air_flow'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:dehumidification_control_type_none")
    def test_dehumidification_control_type_none(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'dehumidification_control_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:dehumidification_control_type_cool_reheat")
    def test_dehumidification_control_type_cool_reheat(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'dehumidification_control_type'] = 'CoolReheat'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'dehumidification_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'dehumidification_setpoint'] = 62
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['SetpointManager:SingleZone:Humidity:Maximum']
                             .get('VAV Sys 1 Dehumidification Setpoint Manager'))
        self.assertEqual(
            'HVACTemplate-Always62.0',
            epjson_output['ZoneControl:Humidistat']['VAV Sys 1 Dehumidification Humidistat'][
                'dehumidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:humidifier_type")
    def test_humidifier_type(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'humidifier_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'humidifier_setpoint'] = 29
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Humidifier:Steam:Electric'].get('VAV Sys 1 Humidifier'))
        self.assertEqual(
            'HVACTemplate-Always29.0',
            epjson_output['ZoneControl:Humidistat']['VAV Sys 1 Humidification Humidistat'][
                'humidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:humidifier_inputs")
    def test_humidifier_inputs(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'humidifier_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'humidifier_setpoint'] = 29
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'humidifier_availability_schedule_name'] = 'OCCUPY-1'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'humidifier_rated_capacity'] = 1
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'humidifier_rated_electric_power'] = 1000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Humidifier:Steam:Electric'].get('VAV Sys 1 Humidifier'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Humidifier:Steam:Electric']['VAV Sys 1 Humidifier']['availability_schedule_name'])
        self.assertEqual(
            1,
            epjson_output['Humidifier:Steam:Electric']['VAV Sys 1 Humidifier']['rated_capacity'])
        self.assertEqual(
            1000,
            epjson_output['Humidifier:Steam:Electric']['VAV Sys 1 Humidifier']['rated_power'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:sizing_option_non_coincident")
    def test_sizing_option_non_coincident(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'sizing_option'] = 'NonCoincident'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'NonCoincident',
            epjson_output['Sizing:System']['VAV Sys 1 Sizing System']['type_of_zone_sum_to_use'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:sizing_option_non_coincident")
    def test_sizing_option_coincident(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'sizing_option'] = 'Coincident'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Coincident',
            epjson_output['Sizing:System']['VAV Sys 1 Sizing System']['type_of_zone_sum_to_use'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:return_fan_yes")
    def test_return_fan_yes(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Fan:VariableVolume'].get('VAV Sys 1 Return Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:return_fan_no")
    def test_return_fan_no(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output['Fan:VariableVolume'].get('VAV Sys 1 Return Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:return_fan_inputs")
    def test_return_fan_inputs(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan'] = 'Yes'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan_total_efficiency'] = 0.72
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan_delta_pressure'] = 295
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan_motor_efficiency'] = 0.85
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan_motor_in_air_stream_fraction'] = 0.9
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.72,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Return Fan']['fan_total_efficiency'])
        self.assertEqual(
            295,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Return Fan']['pressure_rise'])
        self.assertEqual(
            0.85,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Return Fan']['motor_efficiency'])
        self.assertEqual(
            0.9,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Return Fan']['motor_in_airstream_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "return_fan_part_load_power_coefficients_inlet_vane_dampers")
    def test_return_fan_part_load_power_coefficients_inlet_vane_dampers(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan'] = 'Yes'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan_part_load_power_coefficients'] = 'InletVaneDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.35071223,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Return Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "return_fan_part_load_power_coefficients_outlet_dampers")
    def test_return_fan_part_load_power_coefficients_outlet_dampers(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan'] = 'Yes'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan_part_load_power_coefficients'] = 'OutletDampers'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.37073425,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Return Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "return_fan_part_load_power_coefficients_variable_speed_motor")
    def test_return_fan_part_load_power_coefficients_variable_speed_motor(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan'] = 'Yes'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan_part_load_power_coefficients'] = 'VariableSpeedMotor'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0015302446,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Return Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "return_fan_part_load_power_coefficients_ASHRAE901_2004_appendix_g")
    def test_return_fan_part_load_power_coefficients_ASHRAE901_2004_appendix_g(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan'] = 'Yes'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan_part_load_power_coefficients'] = 'ASHRAE90.1-2004AppendixG'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.0013,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Return Fan']['fan_power_coefficient_1'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VAV:"
                                              "return_fan_part_load_power_coefficients_"
                                              "variable_speed_motor_pressure_resst")
    def test_return_fan_part_load_power_coefficients_variable_speed_motor_pressure_reset(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan'] = 'Yes'
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'return_fan_part_load_power_coefficients'] = 'VariableSpeedMotorPressureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.040759894,
            epjson_output['Fan:VariableVolume']['VAV Sys 1 Return Fan']['fan_power_coefficient_1'])
        return
