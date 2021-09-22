from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent

chilled_water_objects = {
    "HVACTemplate:Plant:ChilledWaterLoop": {
        "Chilled Water Loop": {
            "chilled_water_design_setpoint": 7.22,
            "chilled_water_pump_configuration": "ConstantPrimaryNoSecondary",
            "chilled_water_reset_outdoor_dry_bulb_high": 26.7,
            "chilled_water_reset_outdoor_dry_bulb_low": 15.6,
            "chilled_water_setpoint_at_outdoor_dry_bulb_high": 6.7,
            "chilled_water_setpoint_at_outdoor_dry_bulb_low": 12.2,
            "chilled_water_setpoint_reset_type": "None",
            "chiller_plant_operation_scheme_type": "Default",
            "condenser_plant_operation_scheme_type": "Default",
            "condenser_water_design_setpoint": 29.4,
            "condenser_water_pump_rated_head": 179352,
            "minimum_outdoor_dry_bulb_temperature": 7.22,
            "primary_chilled_water_pump_rated_head": 179352,
            "pump_control_type": "Intermittent",
            "secondary_chilled_water_pump_rated_head": 179352
        }
    },
    "HVACTemplate:Plant:Chiller": {
        "Main Chiller": {
            "capacity": "Autosize",
            "chiller_type": "ElectricReciprocatingChiller",
            "condenser_type": "WaterCooled",
            "nominal_cop": 3.2,
            "priority": "1"
        }
    },
    "HVACTemplate:Plant:Tower": {
        "Main Tower": {
            "free_convection_capacity": "Autosize",
            "high_speed_fan_power": "Autosize",
            "high_speed_nominal_capacity": "Autosize",
            "low_speed_fan_power": "Autosize",
            "low_speed_nominal_capacity": "Autosize",
            "priority": "1",
            "tower_type": "SingleSpeed"
        }
    }
}

hot_water_objects = {
    "HVACTemplate:Plant:Boiler": {
        "Main Boiler": {
            "boiler_type": "HotWaterBoiler",
            "capacity": "Autosize",
            "efficiency": 0.8,
            "fuel_type": "NaturalGas",
            "priority": "1"
        }
    },
    "HVACTemplate:Plant:HotWaterLoop": {
        "Hot Water Loop": {
            "hot_water_design_setpoint": 82,
            "hot_water_plant_operation_scheme_type": "Default",
            "hot_water_pump_configuration": "ConstantFlow",
            "hot_water_pump_rated_head": 179352,
            "hot_water_reset_outdoor_dry_bulb_high": 10,
            "hot_water_reset_outdoor_dry_bulb_low": -6.7,
            "hot_water_setpoint_at_outdoor_dry_bulb_high": 65.6,
            "hot_water_setpoint_at_outdoor_dry_bulb_low": 82.2,
            "hot_water_setpoint_reset_type": "OutdoorAirTemperatureReset",
            "pump_control_type": "Intermittent"
        }
    }
}

schedule_objects = {
    "Schedule:Compact": {
        "Always0.003": {
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
                    "field": 0.003
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
        }
    }
}


class TestSimulationsSystemDedicatedOutdoorAir(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZonePTAC-DOAS.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:test_minimum_inputs")
    def test_minimum_inputs(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                **chilled_water_objects,
                **hot_water_objects
            }
        )
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir'].pop('DOAS')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:System:DedicatedOutdoorAir': {
                    'DOAS': {
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:system_availability_schedule_name")
    def test_system_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS']['system_availability_schedule_name'] = 'LIGHTS-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'LIGHTS-1',
            epjson_output['Fan:VariableVolume']['DOAS Supply Fan']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_flow_rate")
    def test_supply_fan_flow_rate(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS']['supply_fan_flow_rate'] = 0.48
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.48,
            epjson_output['Sizing:System']['DOAS Sizing System']['cooling_supply_air_flow_rate'])
        self.assertEqual(
            0.48,
            epjson_output['Sizing:System']['DOAS Sizing System']['design_outdoor_air_flow_rate'])
        self.assertEqual(
            0.48,
            epjson_output['Sizing:System']['DOAS Sizing System']['heating_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_inputs")
    def test_supply_fan_inputs(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS']['supply_fan_total_efficiency'] = 0.65
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS']['supply_fan_delta_pressure'] = 900
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS']['supply_fan_motor_efficiency'] = 0.8
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_motor_in_air_stream_fraction'] = 0.85
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.65,
            epjson_output['Fan:VariableVolume']['DOAS Supply Fan']['fan_total_efficiency'])
        self.assertEqual(
            900,
            epjson_output['Fan:VariableVolume']['DOAS Supply Fan']['pressure_rise'])
        self.assertEqual(
            0.8,
            epjson_output['Fan:VariableVolume']['DOAS Supply Fan']['motor_efficiency'])
        self.assertEqual(
            0.85,
            epjson_output['Fan:VariableVolume']['DOAS Supply Fan']['motor_in_airstream_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "blow_through_cooling_fixed_setpoint")
    def test_supply_fan_placement_blow_through_cooling_fixed_setpoint(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_control_type'] = 'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "draw_through_cooling_fixed_setpoint")
    def test_supply_fan_placement_draw_through_cooling_fixed_setpoint(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_control_type'] = 'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "blow_through_cooling_scheduled")
    def test_supply_fan_placement_blow_through_cooling_scheduled(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "draw_through_cooling_scheduled")
    def test_supply_fan_placement_draw_through_cooling_scheduled(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "blow_through_cooling_outdoor_air_temperature_reset")
    def test_supply_fan_placement_blow_through_cooling_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "draw_through_cooling_outdoor_air_temperature_reset")
    def test_supply_fan_placement_draw_through_cooling_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "blow_through_heating_fixed_setpoint")
    def test_supply_fan_placement_blow_through_heating_fixed_setpoint(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_control_type'] = 'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "draw_through_heating_fixed_setpoint")
    def test_supply_fan_placement_draw_through_heating_fixed_setpoint(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_control_type'] = 'FixedSetpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "blow_through_heating_scheduled")
    def test_supply_fan_placement_blow_through_heating_scheduled(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "draw_through_heating_scheduled")
    def test_supply_fan_placement_draw_through_heating_scheduled(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "blow_through_heating_outdoor_air_temperature_reset")
    def test_supply_fan_placement_blow_through_heating_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'BlowThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:supply_fan_placement_"
                                              "draw_through_heating_outdoor_air_temperature_reset")
    def test_supply_fan_placement_draw_through_heating_outdoor_air_temperature_reset(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_type_"
                                              "two_stage_humidity_control_dx")
    def test_cooling_coil_type_two_stage_humidity_control_dx(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'TwoStageHumidityControlDX'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Coil:Cooling:DX:TwoStageWithHumidityControlMode'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_type_chilled_water")
    def test_cooling_coil_type_chilled_water(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=chilled_water_objects)
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'ChilledWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Coil:Cooling:Water'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_type_"
                                              "chilled_water_detailed_flat_model")
    def test_cooling_coil_type_chilled_water_detailed_flat_model(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=chilled_water_objects)
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'ChilledWaterDetailedFlatModel'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Coil:Cooling:Water:DetailedGeometry'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_type_"
                                              "heat_exchanger_assisted_chilled_water")
    def test_cooling_coil_type_heat_exchanger_assisted_chilled_water(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=chilled_water_objects)
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'HeatExchangerAssistedChilledWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('CoilSystem:Cooling:Water:HeatExchangerAssisted'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_type_"
                                              "two_speed_dx")
    def test_cooling_coil_type_two_speed_dx(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'TwoSpeedDX'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Coil:Cooling:DX:TwoSpeed'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_type_"
                                              "two_stage_dx")
    def test_cooling_coil_type_two_stage_dx(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'TwoStageDX'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Coil:Cooling:DX:TwoStageWithHumidityControlMode'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_type_"
                                              "heat_exchanger_assisted_dx")
    def test_cooling_coil_type_heat_exchanger_assisted_dx(self):
        # todo_eo: EnergyPlus notes say that this setup is okay but a warning is issued.
        #  Combination of dehumidification_control_type and cooling_coil_type is not allowed.
        # todo_eo: simulation fails in legacy SetDXCoilTypeData: Could not find Coil
        #  "Name="DOAS HEAT EXCHANGER ASSISTED COOLING COIL"
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'HeatExchangerAssistedDX'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Coil:Cooling:Water:DetailedGeometry'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_type_none")
    def test_cooling_coil_type_none(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output.get('Coil:Cooling:DX:TwoStageWithHumidityControlMode'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_availability_schedule_name")
    def test_cooling_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Cooling:DX:TwoStageWithHumidityControlMode']['DOAS Cooling Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_design_setpoint")
    def test_cooling_coil_design_setpoint(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_design_setpoint'] = 13
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            13,
            epjson_output['Sizing:System']['DOAS Sizing System']['central_cooling_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_setpoint_schedule_name")
    def test_cooling_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_schedule_name'] = 'Always12.5'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always12.5',
            epjson_output['SetpointManager:Scheduled']['DOAS Cooling Supply Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:cooling_coil_outdoor_reset_inputs")
    def test_cooling_coil_outdoor_reset_inputs(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_at_outdoor_dry_bulb_low'] = 15.5
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_reset_outdoor_dry_bulb_low'] = 15.4
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_setpoint_at_outdoor_dry_bulb_high'] = 12.5
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_reset_outdoor_dry_bulb_high'] = 23.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            23.2,
            epjson_output['SetpointManager:OutdoorAirReset']['DOAS Cooling Supply Air Temp Manager']['outdoor_high_temperature'])
        self.assertEqual(
            12.5,
            epjson_output['SetpointManager:OutdoorAirReset']['DOAS Cooling Supply Air Temp Manager']['setpoint_at_outdoor_high_temperature'])
        self.assertEqual(
            15.4,
            epjson_output['SetpointManager:OutdoorAirReset']['DOAS Cooling Supply Air Temp Manager']['outdoor_low_temperature'])
        self.assertEqual(
            15.5,
            epjson_output['SetpointManager:OutdoorAirReset']['DOAS Cooling Supply Air Temp Manager']['setpoint_at_outdoor_low_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:dx_cooling_coil_gross_rated_total_capacity")
    def test_dx_cooling_coil_gross_rated_total_capacity(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dx_cooling_coil_gross_rated_total_capacity'] = 1000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            500,
            epjson_output['CoilPerformance:DX:Cooling']['DOAS Dehumid Perf 1']['gross_rated_total_cooling_capacity'])
        self.assertEqual(
            1000,
            epjson_output['CoilPerformance:DX:Cooling']['DOAS Dehumid Perf 1+2']['gross_rated_total_cooling_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:"
                                              "dx_cooling_coil_gross_rated_sensible_heat_ratio")
    def test_dx_cooling_coil_gross_rated_sensible_heat_ratio(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'TwoSpeedDX'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dehumidification_control_type'] = 'None'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dx_cooling_coil_gross_rated_sensible_heat_ratio'] = 0.75
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.75,
            epjson_output['Coil:Cooling:DX:TwoSpeed']['DOAS Cooling Coil']['high_speed_rated_sensible_heat_ratio'])
        self.assertEqual(
            0.75,
            epjson_output['Coil:Cooling:DX:TwoSpeed']['DOAS Cooling Coil']['low_speed_gross_rated_sensible_heat_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:"
                                              "dx_cooling_coil_gross_rated_sensible_heat_ratio")
    def test_dx_cooling_coil_gross_rated_sensible_heat_ratio_with_humidity_control(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'cooling_coil_type'] = 'TwoStageHumidityControlDX'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dehumidification_control_type'] = 'Multimode'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dx_cooling_coil_gross_rated_sensible_heat_ratio'] = 0.75
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.9 * 0.75,
            epjson_output['CoilPerformance:DX:Cooling']['DOAS Dehumid Perf 1']['gross_rated_sensible_heat_ratio'])
        self.assertEqual(
            0.9 * 0.75,
            epjson_output['CoilPerformance:DX:Cooling']['DOAS Dehumid Perf 1+2']['gross_rated_sensible_heat_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:dx_cooling_coil_gross_rated_cop")
    def test_dx_cooling_coil_gross_rated_cop(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dx_cooling_coil_gross_rated_cop'] = 3.111
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            2.7999,
            epjson_output['CoilPerformance:DX:Cooling']['DOAS Dehumid Perf 1']['gross_rated_cooling_cop'])
        self.assertEqual(
            3.111,
            epjson_output['CoilPerformance:DX:Cooling']['DOAS Standard Perf 1']['gross_rated_cooling_cop'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heating_coil_type_gas")
    def test_heating_coil_type_gas(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_type'] = 'Gas'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Coil:Heating:Fuel'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heating_coil_type_hot_water")
    def test_heating_coil_type_hot_water(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=hot_water_objects)
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Coil:Heating:Water'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heating_coil_type_electric")
    def test_heating_coil_type_electric(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Coil:Heating:Electric'].get('DOAS Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heating_coil_type_none")
    def test_heating_coil_type_none(self):
        # todo_eo: legacy does not issue cold temperature warning but it appears it should
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output.get('Coil:Heating:Electric'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heating_coil_availability_schedule_name")
    def test_heating_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Fuel']['DOAS Heating Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heating_coil_design_setpoint")
    def test_heating_coil_design_setpoint(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_design_setpoint'] = 16
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            16,
            epjson_output['Sizing:System']['DOAS Sizing System']['central_heating_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heating_coil_setpoint_schedule_name")
    def test_heating_coil_setpoint_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_schedule_name'] = 'Always15.5'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_control_type'] = 'Scheduled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always15.5',
            epjson_output['SetpointManager:Scheduled']['DOAS Heating Supply Air Temp Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heating_coil_outdoor_reset_inputs")
    def test_heating_coil_outdoor_reset_inputs(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_control_type'] = 'OutdoorAirTemperatureReset'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_at_outdoor_dry_bulb_low'] = 14.9
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_reset_outdoor_dry_bulb_low'] = 7.7
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_setpoint_at_outdoor_dry_bulb_high'] = 12.1
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_reset_outdoor_dry_bulb_high'] = 12.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            12.2,
            epjson_output['SetpointManager:OutdoorAirReset']['DOAS Heating Supply Air Temp Manager']['outdoor_high_temperature'])
        self.assertEqual(
            12.1,
            epjson_output['SetpointManager:OutdoorAirReset']['DOAS Heating Supply Air Temp Manager']['setpoint_at_outdoor_high_temperature'])
        self.assertEqual(
            7.7,
            epjson_output['SetpointManager:OutdoorAirReset']['DOAS Heating Supply Air Temp Manager']['outdoor_low_temperature'])
        self.assertEqual(
            14.9,
            epjson_output['SetpointManager:OutdoorAirReset']['DOAS Heating Supply Air Temp Manager']['setpoint_at_outdoor_low_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:gas_heating_coil_inputs")
    def test_gas_heating_coil_inputs(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heating_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'gas_heating_coil_efficiency'] = 0.75
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'gas_heating_coil_parasitic_electric_load'] = 1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.75,
            epjson_output['Coil:Heating:Fuel']['DOAS Heating Coil']['burner_efficiency'])
        self.assertEqual(
            1,
            epjson_output['Coil:Heating:Fuel']['DOAS Heating Coil']['parasitic_electric_load'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_none")
    def test_heat_recovery_none(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_sensible")
    def test_heat_recovery_sensible(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_sensible_draw_through")
    def test_heat_recovery_sensible_draw_through(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Sensible'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_enthalpy")
    def test_heat_recovery_enthalpy(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_enthalpy_draw_through")
    def test_heat_recovery_enthalpy_draw_through(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'supply_fan_placement'] = 'DrawThrough'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_effectiveness")
    def test_heat_recovery_effectiveness(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_sensible_effectiveness'] = 0.72
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_latent_effectiveness'] = 0.61
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('HeatExchanger:AirToAir:SensibleAndLatent'))
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery'][
                'sensible_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.77,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery'][
                'sensible_effectiveness_at_75_heating_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery'][
                'sensible_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.72,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery'][
                'sensible_effectiveness_at_100_heating_air_flow'])
        self.assertEqual(
            0.66,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery'][
                'latent_effectiveness_at_75_cooling_air_flow'])
        self.assertEqual(
            0.66,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery'][
                'latent_effectiveness_at_75_heating_air_flow'])
        self.assertEqual(
            0.61,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery'][
                'latent_effectiveness_at_100_cooling_air_flow'])
        self.assertEqual(
            0.61,
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery'][
                'latent_effectiveness_at_100_heating_air_flow'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_exchanger_type_plate")
    def test_heat_recovery_exchanger_type_plate(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_heat_exchanger_type'] = 'Plate'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Plate',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery']['heat_exchanger_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_exchanger_type_rotary")
    def test_heat_recovery_exchanger_type_rotary(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_heat_exchanger_type'] = 'Rotary'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Rotary',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery']['heat_exchanger_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_frost_control_type_none")
    def test_heat_recovery_frost_control_type_none(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_frost_control_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'None',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_frost_control_type"
                                              "exhaust_air_recirculation")
    def test_heat_recovery_frost_control_type_exhaust_air_recirculation(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_frost_control_type'] = 'ExhaustAirRecirculation'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ExhaustAirRecirculation',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_frost_control_type"
                                              "exhaust_only")
    def test_heat_recovery_frost_control_type_exhaust_only(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_frost_control_type'] = 'ExhaustOnly'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ExhaustOnly',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:heat_recovery_frost_control_type"
                                              "minimum_exhaust_temperature")
    def test_heat_recovery_frost_control_type_minimum_exhaust_temperature(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_type'] = 'Enthalpy'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'heat_recovery_frost_control_type'] = 'MinimumExhaustTemperature'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'MinimumExhaustTemperature',
            epjson_output['HeatExchanger:AirToAir:SensibleAndLatent']['DOAS Heat Recovery']['frost_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:dehumidification_control_type_none")
    def test_dehumidification_control_type_none(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dehumidification_control_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:dehumidification_control_type_"
                                              "cool_reheat_heating_coil")
    def test_dehumidification_control_type_cool_reheat_heating_coil(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dehumidification_control_type'] = 'CoolReheatHeatingCoil'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:dehumidification_control_type_"
                                              "cool_reheat_desuperheater")
    def test_dehumidification_control_type_cool_reheat_desuperheater(self):
        # todo_eo: epjson comparison turned off because the SetpointManager:Scheduled creates two objects in
        #  pyExpandObjects.  It's easier to add this way.  discuss with team if it is necessary to make identical
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dehumidification_control_type'] = 'CoolReheatDesuperheater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path, compare_epjson_files=False)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:dehumidification_setpoint")
    def test_dehumidification_setpoint(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'dehumidification_setpoint'] = 0.01
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'HVACTemplate-Always0.01',
            epjson_output['SetpointManager:Scheduled']['DOAS Dehumidification Setpoint Manager']['schedule_name']
        )
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:humidifier_type")
    def test_humidifier_type(self):
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'humidifier_constant_setpoint'] = 0.0029
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Humidifier:Steam:Electric'].get('DOAS Humidifier'))
        self.assertEqual(
            'HVACTemplate-Always0.0029',
            epjson_output['SetpointManager:Scheduled']['DOAS Humidification Setpoint Manager']['schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:DedicatedOutdoorAir:humidifier_inputs")
    def test_humidifier_inputs(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'humidifier_type'] = 'ElectricSteam'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'humidifier_availability_schedule_name'] = 'OCCUPY-1'
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'humidifier_rated_capacity'] = 1
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'humidifier_rated_electric_power'] = 1000
        self.base_epjson['HVACTemplate:System:DedicatedOutdoorAir']['DOAS'][
            'humidifier_setpoint_schedule_name'] = 'Always0.003'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Humidifier:Steam:Electric'].get('DOAS Humidifier'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Humidifier:Steam:Electric']['DOAS Humidifier']['availability_schedule_name'])
        self.assertEqual(
            1,
            epjson_output['Humidifier:Steam:Electric']['DOAS Humidifier']['rated_capacity'])
        self.assertEqual(
            1000,
            epjson_output['Humidifier:Steam:Electric']['DOAS Humidifier']['rated_power'])
        self.assertEqual(
            'Always0.003',
            epjson_output['SetpointManager:Scheduled']['DOAS Humidification Setpoint Manager']['schedule_name'])
        return