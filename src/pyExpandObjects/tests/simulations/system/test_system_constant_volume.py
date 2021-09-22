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


class TestSimulationsSystemConstantVolume(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneConstantVolumeChillerBoiler.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:minimum_inputs")
    def test_minimum_inputs(self):
        # todo_eo: HotWater is default heating coil type, but legacy creates a system with the 'None' option.
        self.base_epjson['HVACTemplate:Zone:ConstantVolume']['HVACTemplate:Zone:ConstantVolume 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:ConstantVolume']['HVACTemplate:Zone:ConstantVolume 2'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:ConstantVolume']['HVACTemplate:Zone:ConstantVolume 3'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:ConstantVolume']['HVACTemplate:Zone:ConstantVolume 4'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:Zone:ConstantVolume']['HVACTemplate:Zone:ConstantVolume 5'][
            'zone_cooling_design_supply_air_temperature_input_method'] = 'SupplyAirTemperature'
        self.base_epjson['HVACTemplate:System:ConstantVolume'].pop('AHU 1 Spaces 1-4')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:System:ConstantVolume': {
                    'AHU 1 Spaces 1-4': {
                        'economizer_type': 'NoEconomizer'
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:system_availability_schedule_name")
    def test_system_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4']['system_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Fan:ConstantVolume']['AHU 1 Spaces 1-4 Supply Fan']['availability_schedule_name'])
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['AvailabilityManager:NightCycle']['AHU 1 Spaces 1-4 Availability']['fan_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_maximum_flow_rate")
    def test_supply_fan_maximum_flow_rate(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4']['supply_fan_maximum_flow_rate'] = 1.01
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.01,
            epjson_output['Sizing:System']['AHU 1 Spaces 1-4 Sizing System']['cooling_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_total_efficiency")
    def test_supply_fan_total_efficiency(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4']['supply_fan_total_efficiency'] = 0.65
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.65,
            epjson_output['Fan:ConstantVolume']['AHU 1 Spaces 1-4 Supply Fan']['fan_total_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_delta_pressure")
    def test_supply_fan_delta_pressure(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4']['supply_fan_delta_pressure'] = 500
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            500,
            epjson_output['Fan:ConstantVolume']['AHU 1 Spaces 1-4 Supply Fan']['pressure_rise'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_motor_efficiency")
    def test_supply_fan_motor_efficiency(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4']['supply_fan_motor_efficiency'] = 0.8
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.8,
            epjson_output['Fan:ConstantVolume']['AHU 1 Spaces 1-4 Supply Fan']['motor_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_motor_in_air_stream_fraction")
    def test_supply_fan_motor_in_air_stream_fraction(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_motor_in_air_stream_fraction'] = 0.9
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.9,
            epjson_output['Fan:ConstantVolume']['AHU 1 Spaces 1-4 Supply Fan']['motor_in_airstream_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_through_cooling_warmest")
    def test_supply_fan_placement_draw_through_cooling_warmest(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'Warmest'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_through_cooling_warmest")
    def test_supply_fan_placement_blow_through_cooling_warmest(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'Warmest'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_through_cooling_fixed_setpoint")
    def test_supply_fan_placement_draw_through_cooling_fixed_setpoint(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_through_cooling_fixed_setpoint")
    def test_supply_fan_placement_blow_through_cooling_fixed_setpoint(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_through_cooling_scheduled")
    def test_supply_fan_placement_draw_through_cooling_scheduled(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:"
                                              "supply_fan_placement_blow_through_cooling_scheduled")
    def test_supply_fan_placement_blow_through_cooling_scheduled(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_"
                                              "through_cooling_outdoor_air_temperature_reset")
    def test_supply_fan_placement_draw_through_cooling_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_"
                                              "through_cooling_outdoor_air_temperature_reset")
    def test_supply_fan_placement_blow_through_cooling_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_"
                                              "through_cooling_control_zone")
    def test_supply_fan_placement_draw_through_cooling_control_zone(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'ControlZone'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_control_zone_name'] = 'SPACE1-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_"
                                              "through_cooling_control_zone")
    def test_supply_fan_placement_blow_through_cooling_control_zone(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'ControlZone'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_control_zone_name'] = 'SPACE1-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_"
                                              "through_economizer_type_no_economizer")
    def test_supply_fan_placement_blow_through_economizer_type_no_economizer(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'NoEconomizer'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output['SetpointManager:MixedAir'].get('AHU 1 Spaces 1-4 Economizer Air Temp Manager'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_"
                                              "through_economizer_type_no_economizer")
    def test_supply_fan_placement_draw_through_economizer_type_no_economizer(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'NoEconomizer'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output['SetpointManager:MixedAir'].get('AHU 1 Spaces 1-4 Economizer Air Temp Manager'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_"
                                              "through_economizer_type_fixed_dry_bulb")
    def test_supply_fan_placement_blow_through_economizer_type_fixed_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'FixedDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['SetpointManager:MixedAir']['AHU 1 Spaces 1-4 Economizer Air Temp Manager'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_"
                                              "through_economizer_type_fixed_dry_bulb")
    def test_supply_fan_placement_draw_through_economizer_type_fixed_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'FixedDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['SetpointManager:MixedAir']['AHU 1 Spaces 1-4 Economizer Air Temp Manager'])
        return

    # todo_eo: perform testing for other economizer types

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_"
                                              "through_cooling_control_zone_no_heating")
    def test_supply_fan_placement_blow_through_cooling_control_zone_no_heating(self):
        # todo_eo: Legacy changes central_heating_design_supply_air_temperature for AHU 2 Space 5 even though only
        #  AHU 1 Spaces 1-4 has been modified
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'ControlZone'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_"
                                              "through_cooling_control_zone_no_heating")
    def test_supply_fan_placement_draw_through_cooling_control_zone_no_heating(self):
        # todo_eo: Legacy changes central_heating_design_supply_air_temperature for AHU 2 Space 5 even though only
        #  AHU 1 Spaces 1-4 has been modified
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'ControlZone'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_through"
                                              "_heating_outdoor_air_temperature_reset")
    def test_supply_fan_placement_draw_through_heating_control_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_through"
                                              "_heating_outdoor_air_temperature_reset")
    def test_supply_fan_placement_blow_through_heating_control_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    def test_supply_fan_placement_draw_through_heating_control_fixed_setpoint(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    def test_supply_fan_placement_blow_through_heating_control_fixed_setpoint(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_through_heating_scheduled")
    def test_supply_fan_placement_draw_through_heating_scheduled(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_blow_through_heating_scheduled")
    def test_supply_fan_placement_blow_through_heating_scheduled(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_through_heating_control_zone")
    def test_supply_fan_placement_draw_through_heating_control_zone(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'ControlZone'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_control_zone_name'] = 'SPACE1-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_fan_placement_draw_through_heating_control_zone")
    def test_supply_fan_placement_blow_through_heating_control_zone(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'ControlZone'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_control_zone_name'] = 'SPACE1-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:"
                                              "cooling_coil_type_none_draw_through")
    def test_cooling_coil_type_none_draw_through(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:"
                                              "cooling_coil_type_none_blow_through")
    def test_cooling_coil_type_none_blow_through(self):
        # todo_eo: both systems fail, it is difficult to make appropriate septointmanagers for this setup
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:cooling_coil_type_chilled_water")
    def test_cooling_coil_type_chilled_water(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_type'] = 'ChilledWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'AHU 1 Spaces 1-4 Mixed Air Outlet',
            epjson_output['Coil:Cooling:Water']['AHU 1 Spaces 1-4 Cooling Coil'][
                'air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:cooling_coil_type_chilled_water"
                                              "_detailed_flat_model")
    def test_cooling_coil_type_chilled_water_detailed_flat_model(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_type'] = 'ChilledWaterDetailedFlatModel'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'AHU 1 Spaces 1-4 Mixed Air Outlet',
            epjson_output['Coil:Cooling:Water:DetailedGeometry']['AHU 1 Spaces 1-4 Cooling Coil'][
                'air_inlet_node_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:cooling_coil_type_chilled_water"
                                              "_heat_exchanger_assisted_chilled_water")
    def test_cooling_coil_type_chilled_water_heat_exchanger_assisted_chilled_water(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_type'] = 'HeatExchangerAssistedChilledWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['CoilSystem:Cooling:Water:HeatExchangerAssisted'].get('AHU 1 Spaces 1-4 Heat Exchanger Assisted Cooling Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Cooling Coil Heat Exchanger'))
        return
    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:cooling_coil_type_chilled_water_blow_through"
                                              "_heat_exchanger_assisted_chilled_water")
    def test_cooling_coil_type_chilled_water_heat_exchanger_assisted_chilled_water_blow_through(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_type'] = 'HeatExchangerAssistedChilledWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['CoilSystem:Cooling:Water:HeatExchangerAssisted'].get('AHU 1 Spaces 1-4 Heat Exchanger Assisted Cooling Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Cooling Coil Heat Exchanger'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:cooling_coil_availability_schedule_name")
    def test_cooling_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Cooling:Water']['AHU 1 Spaces 1-4 Cooling Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:"
                                              "cooling_coil_design_setpoint_temperature")
    def test_cooling_coil_design_setpoint_temperature(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_design_setpoint_temperature'] = 13
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            13,
            epjson_output['Sizing:System']['AHU 1 Spaces 1-4 Sizing System']['central_cooling_design_supply_air_temperature'])
        self.assertEqual(
            18,
            epjson_output['SetpointManager:Warmest']['AHU 1 Spaces 1-4 Cooling Supply Air Temp Manager'][
                'maximum_setpoint_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:cooling_coil_setpoint_schedule_name")
    def test_cooling_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_schedule_name'] = 'Always12.5'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always12.5',
            epjson_output['SetpointManager:Scheduled']['AHU 1 Spaces 1-4 Cooling Supply Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:cooling_coil_outdoor_reset_inputs")
    def test_cooling_coil_outdoor_reset_inputs(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_at_outdoor_dry_bulb_low'] = 15.5
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_reset_outdoor_dry_bulb_low'] = 15.4
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_at_outdoor_dry_bulb_high'] = 12.5
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_reset_outdoor_dry_bulb_high'] = 23.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            23.2,
            epjson_output['SetpointManager:OutdoorAirReset']['AHU 1 Spaces 1-4 Cooling Supply Air Temp Manager']['outdoor_high_temperature'])
        self.assertEqual(
            12.5,
            epjson_output['SetpointManager:OutdoorAirReset']['AHU 1 Spaces 1-4 Cooling Supply Air Temp Manager']['setpoint_at_outdoor_high_temperature'])
        self.assertEqual(
            15.4,
            epjson_output['SetpointManager:OutdoorAirReset']['AHU 1 Spaces 1-4 Cooling Supply Air Temp Manager']['outdoor_low_temperature'])
        self.assertEqual(
            15.5,
            epjson_output['SetpointManager:OutdoorAirReset']['AHU 1 Spaces 1-4 Cooling Supply Air Temp Manager']['setpoint_at_outdoor_low_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heating_coil_type_hot_water")
    def test_heating_coil_type_hot_water(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('AHU 1 Spaces 1-4 Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heating_coil_type_electric")
    def test_heating_coil_type_electric(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Electric'].get('AHU 1 Spaces 1-4 Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heating_coil_type_gas")
    def test_heating_coil_type_gas(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_type'] = 'Gas'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Fuel'].get('AHU 1 Spaces 1-4 Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heating_coil_availability_schedule_name")
    def test_heating_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Water']['AHU 1 Spaces 1-4 Heating Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heating_coil_design_setpoint")
    def test_heating_coil_design_setpoint(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_design_setpoint'] = 16
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            16,
            epjson_output['Sizing:System']['AHU 1 Spaces 1-4 Sizing System']['central_heating_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heating_coil_setpoint_schedule_name")
    def test_heating_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_schedule_name'] = 'Always15.5'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always15.5',
            epjson_output['SetpointManager:Scheduled']['AHU 1 Spaces 1-4 Heating Supply Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heating_coil_outdoor_reset_inputs")
    def test_heating_coil_outdoor_reset_inputs(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_at_outdoor_dry_bulb_low'] = 14.9
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_reset_outdoor_dry_bulb_low'] = 7.7
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_setpoint_at_outdoor_dry_bulb_high'] = 12.1
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_reset_outdoor_dry_bulb_high'] = 12.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            12.2,
            epjson_output['SetpointManager:OutdoorAirReset']['AHU 1 Spaces 1-4 Heating Supply Air Temp Manager']['outdoor_high_temperature'])
        self.assertEqual(
            12.1,
            epjson_output['SetpointManager:OutdoorAirReset']['AHU 1 Spaces 1-4 Heating Supply Air Temp Manager']['setpoint_at_outdoor_high_temperature'])
        self.assertEqual(
            7.7,
            epjson_output['SetpointManager:OutdoorAirReset']['AHU 1 Spaces 1-4 Heating Supply Air Temp Manager']['outdoor_low_temperature'])
        self.assertEqual(
            14.9,
            epjson_output['SetpointManager:OutdoorAirReset']['AHU 1 Spaces 1-4 Heating Supply Air Temp Manager']['setpoint_at_outdoor_low_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heating_coil_capacity")
    def test_heating_coil_design_capacity(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_capacity'] = 2000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            2000,
            epjson_output['Sizing:System']['AHU 1 Spaces 1-4 Sizing System']['heating_design_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:gas_heating_coil_inputs")
    def test_gas_heating_coil_inputs(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heating_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'gas_heating_coil_efficiency'] = 0.75
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'gas_heating_coil_parasitic_electric_load'] = 1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.75,
            epjson_output['Coil:Heating:Fuel']['AHU 1 Spaces 1-4 Heating Coil']['burner_efficiency'])
        self.assertEqual(
            1,
            epjson_output['Coil:Heating:Fuel']['AHU 1 Spaces 1-4 Heating Coil']['parasitic_electric_load'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_hot_water")
    def test_preheat_coil_type_hot_water(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('AHU 1 Spaces 1-4 Preheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_electric")
    def test_preheat_coil_type_electric(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Electric'].get('AHU 1 Spaces 1-4 Preheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_electric")
    def test_preheat_coil_type_gas(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'Gas'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Fuel'].get('AHU 1 Spaces 1-4 Preheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_none"
                                              "_heat_recovery_sensible")
    def test_preheat_coil_type_none_heat_recovery_sensible(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'None'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_none"
                                              "_heat_recovery_enthalpy")
    def test_preheat_coil_type_none_heat_recovery_enthalpy(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'None'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_hot_water"
                                              "_heat_recovery_sensible")
    def test_preheat_coil_type_hot_water_heat_recovery_sensible(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('AHU 1 Spaces 1-4 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_electric"
                                              "_heat_recovery_sensible")
    def test_preheat_coil_type_electric_heat_recovery_sensible(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'Electric'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Electric'].get('AHU 1 Spaces 1-4 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_gas"
                                              "_heat_recovery_sensible")
    def test_preheat_coil_type_gas_heat_recovery_sensible(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Fuel'].get('AHU 1 Spaces 1-4 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_hot_water"
                                              "_heat_recovery_enthalpy")
    def test_preheat_coil_type_hot_water_heat_recovery_enthalpy(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Water'].get('AHU 1 Spaces 1-4 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_electric"
                                              "_heat_recovery_enthalpy")
    def test_preheat_coil_type_electric_heat_recovery_enthalpy(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'Electric'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Electric'].get('AHU 1 Spaces 1-4 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_type_gas"
                                              "_heat_recovery_enthalpy")
    def test_preheat_coil_type_gas_heat_recovery_enthalpy(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(
            epjson_output['Coil:Heating:Fuel'].get('AHU 1 Spaces 1-4 Preheat Coil'))
        self.assertIsNotNone(
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent'].get('AHU 1 Spaces 1-4 Heat Recovery'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_availability_schedule_name")
    def test_preheat_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Water']['AHU 1 Spaces 1-4 Preheat Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_design_setpoint")
    def test_preheat_coil_design_setpoint(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_design_setpoint'] = 7.0
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            7.0,
            epjson_output['Sizing:System']['AHU 1 Spaces 1-4 Sizing System']['preheat_design_temperature'])
        self.assertEqual(
            'HVACTemplate-Always7.0',
            epjson_output['SetpointManager:Scheduled']['AHU 1 Spaces 1-4 Preheat Coil Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:preheat_coil_setpoint_schedule_name")
    def test_preheat_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_setpoint_schedule_name'] = 'Always6.8'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always6.8',
            epjson_output['SetpointManager:Scheduled']['AHU 1 Spaces 1-4 Preheat Coil Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:gas_preheat_inputs")
    def test_gas_preheat_inputs(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'preheat_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'gas_preheat_coil_efficiency'] = 0.75
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'gas_preheat_coil_parasitic_electric_load'] = 1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.75,
            epjson_output['Coil:Heating:Fuel']['AHU 1 Spaces 1-4 Preheat Coil']['burner_efficiency'])
        self.assertEqual(
            1,
            epjson_output['Coil:Heating:Fuel']['AHU 1 Spaces 1-4 Preheat Coil']['parasitic_electric_load'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:outdoor_air_flow_rates")
    def test_outdoor_air_flow_rates(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'maximum_outdoor_air_flow_rate'] = 1.0
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'minimum_outdoor_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.0,
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['maximum_outdoor_air_flow_rate'])
        self.assertEqual(
            0.1,
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['minimum_outdoor_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:minimum_outdoor_air_schedule_name")
    def test_minimum_outdoor_air_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'minimum_outdoor_air_schedule_name'] = 'Always0.8'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always0.8',
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['minimum_outdoor_air_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_type_no_economizer")
    def test_economizer_type_no_economizer(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'NoEconomizer'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'NoEconomizer',
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_type_fixed_dry_bulb")
    def test_economizer_type_fixed_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'FixedDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedDryBulb',
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_type_fixed_enthalpy")
    def test_economizer_type_fixed_enthalpy(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'FixedEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedEnthalpy',
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_type_differential_dry_bulb")
    def test_economizer_type_differential_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'DifferentialDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialDryBulb',
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_type_differential_enthalpy")
    def test_economizer_type_differential_enthalpy(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'DifferentialEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialEnthalpy',
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:"
                                              "economizer_type_fixed_dew_point_and_dry_bulb")
    def test_economizer_type_fixed_dew_point_and_dry_bulb(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'FixedDewPointAndDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FixedDewPointAndDryBulb',
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_type_electronic_enthalpy")
    def test_economizer_type_electronic_enthalpy(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'ElectronicEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ElectronicEnthalpy',
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_type_"
                                              "differential_dry_bulb_and_enthalpy")
    def test_economizer_type_differential_dry_bulb_and_enthalpy(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_type'] = 'DifferentialDryBulbAndEnthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialDryBulbAndEnthalpy',
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_upper_temperature_limit")
    def test_economizer_upper_temperature_limit(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_upper_temperature_limit'] = 18
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            18,
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_maximum_limit_dry_bulb_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_lower_temperature_limit")
    def test_economizer_lower_temperature_limit(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_lower_temperature_limit'] = 6
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            6,
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_minimum_limit_dry_bulb_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_upper_enthalpy_limit")
    def test_economizer_upper_enthalpy_limit(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_upper_enthalpy_limit'] = 100
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            100,
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_maximum_limit_enthalpy'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:economizer_maximum_limit_dewpoint_temperature")
    def test_economizer_maximum_limit_dewpoint_temperature(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'economizer_maximum_limit_dewpoint_temperature'] = 20
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            20,
            epjson_output['Controller:OutdoorAir']['AHU 1 Spaces 1-4 OA Controller']['economizer_maximum_limit_dewpoint_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:supply_plenum_name")
    def test_supply_plenum_name(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_plenum_name'] = 'PLENUM-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirLoopHVAC:SupplyPlenum'])
        self.assertEqual(
            'PLENUM-1',
            epjson_output['AirLoopHVAC:SupplyPlenum']['AHU 1 Spaces 1-4 Supply Plenum']['zone_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:return_plenum_name")
    def test_return_plenum_name(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'return_plenum_name'] = 'PLENUM-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirLoopHVAC:ReturnPlenum'])
        self.assertEqual(
            'PLENUM-1',
            epjson_output['AirLoopHVAC:ReturnPlenum']['AHU 1 Spaces 1-4 Return Plenum']['zone_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:night_cycle_control_stay_off")
    def test_night_cycle_control_stay_off(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'night_cycle_control'] = 'StayOff'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'StayOff',
            epjson_output['AvailabilityManager:NightCycle']['AHU 1 Spaces 1-4 Availability']['control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:night_cycle_control_cycle_on_any")
    def test_night_cycle_control_cycle_on_any(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'night_cycle_control'] = 'CycleOnAny'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'CycleOnAny',
            epjson_output['AvailabilityManager:NightCycle']['AHU 1 Spaces 1-4 Availability']['control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:night_cycle_control_cycle_on_control_zone")
    def test_night_cycle_control_cycle_on_control_zone(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'night_cycle_control'] = 'CycleOnControlZone'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'night_cycle_control_zone_name'] = 'SPACE1-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'CycleOnControlZone',
            epjson_output['AvailabilityManager:NightCycle']['AHU 1 Spaces 1-4 Availability']['control_type'])
        self.assertEqual(
            'SPACE1-1',
            epjson_output['AvailabilityManager:NightCycle']['AHU 1 Spaces 1-4 Availability']['control_zone_or_zone_list_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:night_cycle_control_"
                                              "cycle_on_any_zone_fans_only")
    def test_night_cycle_control_cycle_on_any_zone_fans_only(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'night_cycle_control'] = 'CycleOnAnyZoneFansOnly'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'night_cycle_control_zone_name'] = 'SPACE1-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'CycleOnAnyZoneFansOnly',
            epjson_output['AvailabilityManager:NightCycle']['AHU 1 Spaces 1-4 Availability']['control_type'])
        self.assertEqual(
            'SPACE1-1',
            epjson_output['AvailabilityManager:NightCycle']['AHU 1 Spaces 1-4 Availability']['control_zone_or_zone_list_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_sensible")
    def test_heat_recovery_sensible(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_enthalpy")
    def test_heat_recovery_enthalpy(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_effectiveness")
    def test_heat_recovery_effectiveness(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'sensible_heat_recovery_effectiveness'] = 0.72
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'latent_heat_recovery_effectiveness'] = 0.61
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery'][
                'sensible_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery'][
                'sensible_effectiveness_at_75_heating_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery'][
                'sensible_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery'][
                'sensible_effectiveness_at_100_heating_air_flow'])
        self.assertEqual(
            0.61,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery'][
                'latent_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.61,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery'][
                'latent_effectiveness_at_100_heating_air_flow'])
        self.assertEqual(
            0.66,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery'][
                'latent_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.66,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery'][
                'latent_effectiveness_at_75_heating_air_flow'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_exchanger_type_plate")
    def test_heat_recovery_exchanger_type_plate(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_heat_exchanger_type'] = 'Plate'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Plate',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery']['heat_exchanger_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_exchanger_type_rotary")
    def test_heat_recovery_exchanger_type_rotary(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_heat_exchanger_type'] = 'Rotary'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Rotary',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery']['heat_exchanger_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_frost_control_type_none")
    def test_heat_recovery_frost_control_type_none(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_frost_control_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'None',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_frost_control_type"
                                              "exhaust_air_recirculation")
    def test_heat_recovery_frost_control_type_exhaust_air_recirculation(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_frost_control_type'] = 'ExhaustAirRecirculation'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ExhaustAirRecirculation',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_frost_control_type"
                                              "exhaust_only")
    def test_heat_recovery_frost_control_type_exhaust_only(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_frost_control_type'] = 'ExhaustOnly'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ExhaustOnly',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:heat_recovery_frost_control_type"
                                              "minimum_exhaust_temperature")
    def test_heat_recovery_frost_control_type_minimum_exhaust_temperature(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'heat_recovery_frost_control_type'] = 'MinimumExhaustTemperature'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'MinimumExhaustTemperature',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['AHU 1 Spaces 1-4 Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:dehumidification_control_type")
    def test_dehumidification_control_type(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'dehumidification_control_type'] = 'CoolReheat'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'dehumidification_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'dehumidification_relative_humidity_setpoint'] = 62
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'TemperatureAndHumidityRatio',
            epjson_output['Controller:WaterCoil']['AHU 1 Spaces 1-4 Cooling Coil Controller']['control_variable'])
        self.assertEqual(
            'HVACTemplate-Always62.0',
            epjson_output['ZoneControl:Humidistat']['AHU 1 Spaces 1-4 Dehumidification Humidistat'][
                'dehumidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:dehumidification_relative_"
                                              "humidity_setpoint_schedule_name")
    def test_dehumidification_relative_humidity_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'cooling_coil_setpoint_control_type'] = 'Warmest'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'dehumidification_control_type'] = 'CoolReheat'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'dehumidification_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'dehumidification_relative_humidity_setpoint_schedule_name'] = 'Always62'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'TemperatureAndHumidityRatio',
            epjson_output['Controller:WaterCoil']['AHU 1 Spaces 1-4 Cooling Coil Controller']['control_variable'])
        self.assertEqual(
            'Always62',
            epjson_output['ZoneControl:Humidistat']['AHU 1 Spaces 1-4 Dehumidification Humidistat'][
                'dehumidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:humidifier_type")
    def test_humidifier_type(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_relative_humidity_setpoint'] = 29
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Humidifier:Steam:Electric'].get('AHU 1 Spaces 1-4 Humidifier'))
        self.assertEqual(
            'HVACTemplate-Always29.0',
            epjson_output['ZoneControl:Humidistat']['AHU 1 Spaces 1-4 Humidification Humidistat'][
                'humidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:humidifier_inputs")
    def test_humidifier_inputs(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_relative_humidity_setpoint'] = 29
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_availability_schedule_name'] = 'OCCUPY-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_rated_capacity'] = 1
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_rated_electric_power'] = 1000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Humidifier:Steam:Electric'].get('AHU 1 Spaces 1-4 Humidifier'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Humidifier:Steam:Electric']['AHU 1 Spaces 1-4 Humidifier']['availability_schedule_name'])
        self.assertEqual(
            1,
            epjson_output['Humidifier:Steam:Electric']['AHU 1 Spaces 1-4 Humidifier']['rated_capacity'])
        self.assertEqual(
            1000,
            epjson_output['Humidifier:Steam:Electric']['AHU 1 Spaces 1-4 Humidifier']['rated_power'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:humidifier_relative_humidity_setpoint_schedule_name")
    def test_humidifier_relative_humidity_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_relative_humidity_setpoint_schedule_name'] = 'Always29'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Humidifier:Steam:Electric'].get('AHU 1 Spaces 1-4 Humidifier'))
        self.assertEqual(
            'Always29',
            epjson_output['ZoneControl:Humidistat']['AHU 1 Spaces 1-4 Humidification Humidistat'][
                'humidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:humidification_and_dehumidification")
    def test_humidification_and_dehumidification(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'dehumidification_control_type'] = 'CoolReheat'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'dehumidification_control_zone_name'] = 'SPACE1-1'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'dehumidification_relative_humidity_setpoint'] = 62
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'humidifier_relative_humidity_setpoint'] = 29
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'TemperatureAndHumidityRatio',
            epjson_output['Controller:WaterCoil']['AHU 1 Spaces 1-4 Cooling Coil Controller']['control_variable'])
        self.assertEqual(
            'HVACTemplate-Always62.0',
            epjson_output['ZoneControl:Humidistat']['AHU 1 Spaces 1-4 Humidistat'][
                'dehumidifying_relative_humidity_setpoint_schedule_name'])
        self.assertIsNotNone(epjson_output['Humidifier:Steam:Electric'].get('AHU 1 Spaces 1-4 Humidifier'))
        self.assertEqual(
            'HVACTemplate-Always29.0',
            epjson_output['ZoneControl:Humidistat']['AHU 1 Spaces 1-4 Humidistat'][
                'humidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:return_fan_yes")
    def test_return_fan_yes(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'return_fan'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Fan:ConstantVolume'].get('AHU 1 Spaces 1-4 Return Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:return_fan_no")
    def test_return_fan_no(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'return_fan'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output['Fan:ConstantVolume'].get('AHU 1 Spaces 1-4 Return Fan'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:ConstantVolume:return_fan_inputs")
    def test_return_fan_inputs(self):
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'return_fan'] = 'Yes'
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'return_fan_total_efficiency'] = 0.72
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'return_fan_delta_pressure'] = 295
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'return_fan_motor_efficiency'] = 0.85
        self.base_epjson['HVACTemplate:System:ConstantVolume']['AHU 1 Spaces 1-4'][
            'return_fan_motor_in_air_stream_fraction'] = 0.9
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.72,
            epjson_output['Fan:ConstantVolume']['AHU 1 Spaces 1-4 Return Fan']['fan_total_efficiency'])
        self.assertEqual(
            295,
            epjson_output['Fan:ConstantVolume']['AHU 1 Spaces 1-4 Return Fan']['pressure_rise'])
        self.assertEqual(
            0.85,
            epjson_output['Fan:ConstantVolume']['AHU 1 Spaces 1-4 Return Fan']['motor_efficiency'])
        self.assertEqual(
            0.9,
            epjson_output['Fan:ConstantVolume']['AHU 1 Spaces 1-4 Return Fan']['motor_in_airstream_fraction'])
        return
