from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent

design_specification_objects = {
    "DesignSpecification:OutdoorAir": {
        "SPACE1-1 SZ DSOA Custom Object": {
            "outdoor_air_flow_per_zone": 0.01,
            "outdoor_air_method": "Flow/Zone"
        }
    },
    "DesignSpecification:ZoneAirDistribution": {
        "SPACE1-1 SZ DSZAD Custom Object": {}
    }
}

hot_water_loop_objects = {
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

doas_objects = {
    "HVACTemplate:System:DedicatedOutdoorAir": {
        "DOAS": {
            "air_outlet_type": "DirectIntoZone",
            "cooling_coil_design_setpoint": 12.8,
            "cooling_coil_reset_outdoor_dry_bulb_high": 23.3,
            "cooling_coil_reset_outdoor_dry_bulb_low": 15.6,
            "cooling_coil_setpoint_at_outdoor_dry_bulb_high": 12.8,
            "cooling_coil_setpoint_at_outdoor_dry_bulb_low": 15.6,
            "cooling_coil_setpoint_control_type": "FixedSetpoint",
            "cooling_coil_type": "TwoStageHumidityControlDX",
            "dehumidification_control_type": "Multimode",
            "dehumidification_setpoint": 0.00924,
            "dx_cooling_coil_gross_rated_cop": 3,
            "dx_cooling_coil_gross_rated_sensible_heat_ratio": "Autosize",
            "dx_cooling_coil_gross_rated_total_capacity": "Autosize",
            "gas_heating_coil_efficiency": 0.8,
            "heat_recovery_frost_control_type": "None",
            "heat_recovery_heat_exchanger_type": "Plate",
            "heat_recovery_latent_effectiveness": 0.65,
            "heat_recovery_sensible_effectiveness": 0.7,
            "heat_recovery_type": "None",
            "heating_coil_design_setpoint": 12.2,
            "heating_coil_reset_outdoor_dry_bulb_high": 12.2,
            "heating_coil_reset_outdoor_dry_bulb_low": 7.8,
            "heating_coil_setpoint_at_outdoor_dry_bulb_high": 12.2,
            "heating_coil_setpoint_at_outdoor_dry_bulb_low": 15,
            "heating_coil_setpoint_control_type": "FixedSetpoint",
            "heating_coil_type": "Gas",
            "humidifier_constant_setpoint": 0.003,
            "humidifier_rated_capacity": 1e-06,
            "humidifier_rated_electric_power": 2690,
            "humidifier_type": "None",
            "supply_fan_delta_pressure": 1000,
            "supply_fan_flow_rate": "Autosize",
            "supply_fan_motor_efficiency": 0.9,
            "supply_fan_motor_in_air_stream_fraction": 1,
            "supply_fan_placement": "BlowThrough",
            "supply_fan_total_efficiency": 0.7,
            "system_availability_schedule_name": "OCCUPY-1"
        }
    }
}


class TestSimulationsZonePTHP(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZonePTHP.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:test_minimum_inputs")
    def test_minimum_inputs(self):
        self.base_epjson['HVACTemplate:Zone:PTHP'].pop('HVACTemplate:Zone:PTHP 1')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Zone:PTHP': {
                    'HVACTemplate:Zone:PTHP 1': {
                        "template_thermostat_name": "All Zones",
                        "zone_name": "SPACE1-1"
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:test_minimum_inputs_doas")
    def test_minimum_inputs_doas(self):
        self.base_epjson['HVACTemplate:Zone:PTHP'].pop('HVACTemplate:Zone:PTHP 1')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Zone:PTHP': {
                    'HVACTemplate:Zone:PTHP 1': {
                        "dedicated_outdoor_air_system_name": "DOAS",
                        "template_thermostat_name": "All Zones",
                        "zone_name": "SPACE1-1"
                    }
                },
                'HVACTemplate:System:DedicatedOutdoorAir': {'DOAS': {
                    'cooling_coil_type': 'TwoSpeedDX',
                    'heating_coil_type': 'Electric'
                }}
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:cooling_supply_air_flow_rate")
    def test_cooling_supply_air_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['cooling_supply_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['cooling_design_air_flow_rate'])
        self.assertEqual(
            'Flow/Zone',
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['cooling_design_air_flow_method'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:cooling_supply_air_flow_rate")
    def test_heating_supply_air_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['heating_supply_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['heating_design_air_flow_rate'])
        self.assertEqual(
            'Flow/Zone',
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['heating_design_air_flow_method'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:no_load_supply_air_flow_rate")
    def test_no_load_supply_air_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['no_load_supply_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['ZoneHVAC:PackagedTerminalHeatPump']['SPACE1-1 PTHP']['no_load_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:zone_heating_sizing_factor")
    def test_zone_heating_sizing_factor(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['zone_heating_sizing_factor'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_heating_sizing_factor'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:zone_cooling_sizing_factor")
    def test_zone_cooling_sizing_factor(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['zone_cooling_sizing_factor'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_sizing_factor'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:outdoor_air_method_flow_per_person")
    def test_outdoor_air_method_flow_per_person(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'outdoor_air_method'] = 'Flow/Person'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'outdoor_air_flow_rate_per_person'] = 0.01
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Flow/Person',
            epjson_output['DesignSpecification:OutdoorAir']['SPACE1-1 SZ DSOA']['outdoor_air_method'])
        self.assertEqual(
            0.01,
            epjson_output['DesignSpecification:OutdoorAir']['SPACE1-1 SZ DSOA']['outdoor_air_flow_per_person'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:outdoor_air_method_flow_per_area")
    def test_outdoor_air_method_flow_per_area(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'outdoor_air_method'] = 'Flow/Area'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'outdoor_air_flow_rate_per_zone_floor_area'] = 0.0014
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Flow/Area',
            epjson_output['DesignSpecification:OutdoorAir']['SPACE1-1 SZ DSOA']['outdoor_air_method'])
        self.assertEqual(
            0.0014,
            epjson_output['DesignSpecification:OutdoorAir']['SPACE1-1 SZ DSOA']['outdoor_air_flow_per_zone_floor_area'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:outdoor_air_method_flow_per_zone")
    def test_outdoor_air_method_flow_per_zone(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'outdoor_air_method'] = 'Flow/Zone'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'outdoor_air_flow_rate_per_zone'] = 0.01
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Flow/Zone',
            epjson_output['DesignSpecification:OutdoorAir']['SPACE1-1 SZ DSOA']['outdoor_air_method'])
        self.assertEqual(
            0.01,
            epjson_output['DesignSpecification:OutdoorAir']['SPACE1-1 SZ DSOA']['outdoor_air_flow_per_zone'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:outdoor_air_method_detailed_specification")
    def test_outdoor_air_method_detailed_specification(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=design_specification_objects)
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'outdoor_air_method'] = 'DetailedSpecification'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'design_specification_outdoor_air_object_name'] = 'SPACE1-1 SZ DSOA Custom Object'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'design_specification_zone_air_distribution_object_name'] = 'SPACE1-1 SZ DSZAD Custom Object'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['DesignSpecification:OutdoorAir'].get('SPACE1-1 SZ DSOA Custom Object'))
        self.assertIsNotNone(epjson_output['DesignSpecification:ZoneAirDistribution'].get('SPACE1-1 SZ DSZAD Custom Object'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:system_availability_schedule_name")
    def test_system_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['system_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Fan:OnOff']['SPACE1-1 Supply Fan']['availability_schedule_name'])
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:PackagedTerminalHeatPump']['SPACE1-1 PTHP']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supply_fan_operating_mode_schedule_name")
    def test_supply_fan_operating_mode_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supply_fan_operating_mode_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:PackagedTerminalHeatPump']['SPACE1-1 PTHP']['supply_air_fan_operating_mode_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supply_fan_placement")
    def test_supply_fan_placement(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supply_fan_placement'] = 'BlowThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'BlowThrough',
            epjson_output['ZoneHVAC:PackagedTerminalHeatPump']['SPACE1-1 PTHP']['fan_placement'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supply_fan_total_efficiency")
    def test_supply_fan_total_efficiency(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supply_fan_total_efficiency'] = 0.65
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.65,
            epjson_output['Fan:OnOff']['SPACE1-1 Supply Fan']['fan_total_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supply_fan_delta_pressure")
    def test_supply_fan_delta_pressure(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supply_fan_delta_pressure'] = 80
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            80,
            epjson_output['Fan:OnOff']['SPACE1-1 Supply Fan']['pressure_rise'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supply_fan_motor_efficiency")
    def test_supply_fan_motor_efficiency(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supply_fan_motor_efficiency'] = 0.8
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.8,
            epjson_output['Fan:OnOff']['SPACE1-1 Supply Fan']['motor_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:cooling_coil_availability_schedule_name")
    def test_cooling_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['cooling_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Cooling:DX:SingleSpeed']['SPACE1-1 Cooling Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:cooling_coil_gross_rated_total_capacity")
    def test_cooling_coil_gross_rated_total_capacity(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['cooling_coil_gross_rated_total_capacity'] = 1000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1000,
            epjson_output['Coil:Cooling:DX:SingleSpeed']['SPACE1-1 Cooling Coil']['gross_rated_total_cooling_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:cooling_coil_gross_rated_sensible_heat_ratio")
    def test_cooling_coil_gross_rated_sensible_heat_ratio(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['cooling_coil_gross_rated_sensible_heat_ratio'] = 0.65
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.65,
            epjson_output['Coil:Cooling:DX:SingleSpeed']['SPACE1-1 Cooling Coil']['gross_rated_sensible_heat_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:cooling_coil_gross_rated_cop")
    def test_cooling_coil_gross_rated_cop(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['cooling_coil_gross_rated_cop'] = 3.3
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            3.3,
            epjson_output['Coil:Cooling:DX:SingleSpeed']['SPACE1-1 Cooling Coil']['gross_rated_cooling_cop'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:heating_coil_availability_schedule_name")
    def test_heating_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['heat_pump_heating_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:DX:SingleSpeed']['SPACE1-1 Heating Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:heat_pump_heating_coil_gross_rated_capacity")
    def test_heat_pump_heating_coil_gross_rated_capacity(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['heat_pump_heating_coil_gross_rated_capacity'] = 1000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1000,
            epjson_output['Coil:Heating:DX:SingleSpeed']['SPACE1-1 Heating Coil']['gross_rated_heating_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:heat_pump_heating_coil_gross_rated_cop")
    def test_heat_pump_heating_coil_gross_rated_cop(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['heat_pump_heating_coil_gross_rated_cop'] = 2.6
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            2.6,
            epjson_output['Coil:Heating:DX:SingleSpeed']['SPACE1-1 Heating Coil']['gross_rated_heating_cop'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:heat_pump_heating_minimum_outdoor_dry_bulb_temperature")
    def test_heat_pump_heating_minimum_outdoor_dry_bulb_temperature(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['heat_pump_heating_minimum_outdoor_dry_bulb_temperature'] = -5
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            -5,
            epjson_output['Coil:Heating:DX:SingleSpeed']['SPACE1-1 Heating Coil']['minimum_outdoor_dry_bulb_temperature_for_compressor_operation'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:heat_pump_defrost_maximum_outdoor_dry_bulb_temperature")
    def test_heat_pump_defrost_maximum_outdoor_dry_bulb_temperature(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['heat_pump_defrost_maximum_outdoor_dry_bulb_temperature'] = 3
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            3,
            epjson_output['Coil:Heating:DX:SingleSpeed']['SPACE1-1 Heating Coil']['maximum_outdoor_dry_bulb_temperature_for_defrost_operation'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:heat_pump_defrost_strategy")
    def test_heat_pump_defrost_strategy(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['heat_pump_defrost_strategy'] = 'Resistive'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Resistive',
            epjson_output['Coil:Heating:DX:SingleSpeed']['SPACE1-1 Heating Coil']['defrost_strategy'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:heat_pump_defrost_control")
    def test_heat_pump_defrost_control(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['heat_pump_defrost_control'] = 'OnDemand'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OnDemand',
            epjson_output['Coil:Heating:DX:SingleSpeed']['SPACE1-1 Heating Coil']['defrost_control'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:heat_pump_defrost_time_period_fraction")
    def test_heat_pump_defrost_time_period_fraction(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['heat_pump_defrost_time_period_fraction'] = 0.07
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.07,
            epjson_output['Coil:Heating:DX:SingleSpeed']['SPACE1-1 Heating Coil']['defrost_time_period_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supplemental_heating_coil_type_electric")
    def test_supplemental_heating_coil_type_electric(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_heating_coil_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Coil:Heating:Electric'].get('SPACE1-1 Supp Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supplemental_heating_coil_type_gas")
    def test_supplemental_heating_coil_type_gas(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_heating_coil_type'] = 'Gas'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Coil:Heating:Fuel'].get('SPACE1-1 Supp Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supplemental_heating_coil_type_hot_water")
    def test_supplemental_heating_coil_type_hot_water(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=hot_water_loop_objects)
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_heating_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Coil:Heating:Water'].get('SPACE1-1 Supp Heating Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supplemental_heating_coil_availability_schedule_name")
    def test_supplemental_heating_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_heating_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Electric']['SPACE1-1 Supp Heating Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:heating_coil_capacity")
    def test_supplemental_heating_coil_capacity(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=hot_water_loop_objects)
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_heating_coil_type'] = 'Electric'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_heating_coil_capacity'] = 100
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 2']['supplemental_heating_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 2']['supplemental_heating_coil_capacity'] = 101
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 5']['supplemental_heating_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 5']['supplemental_heating_coil_capacity'] = 102
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            100,
            epjson_output['Coil:Heating:Electric']['SPACE1-1 Supp Heating Coil']['nominal_capacity'])
        self.assertEqual(
            101,
            epjson_output['Coil:Heating:Water']['SPACE2-1 Supp Heating Coil']['rated_capacity'])
        self.assertEqual(
            102,
            epjson_output['Coil:Heating:Fuel']['SPACE5-1 Supp Heating Coil']['nominal_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supplemental_heating_coil_maximum_outdoor_dry_bulb_temperature")
    def test_supplemental_heating_coil_maximum_outdoor_dry_bulb_temperature(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_heating_coil_maximum_outdoor_dry_bulb_temperature'] = 18
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            18,
            epjson_output['ZoneHVAC:PackagedTerminalHeatPump']['SPACE1-1 PTHP']['maximum_outdoor_dry_bulb_temperature_for_supplemental_heater_operation'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supplemental_gas_heating_coil_efficiency")
    def test_supplemental_gas_heating_coil_efficiency(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_heating_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_gas_heating_coil_efficiency'] = 0.75
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.75,
            epjson_output['Coil:Heating:Fuel']['SPACE1-1 Supp Heating Coil']['burner_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:supplemental_gas_heating_coil_efficiency")
    def test_supplemental_gas_heating_coil_parasitic_electric_load(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_heating_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['supplemental_gas_heating_coil_parasitic_electric_load'] = 1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1,
            epjson_output['Coil:Heating:Fuel']['SPACE1-1 Supp Heating Coil']['parasitic_electric_load'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:dedicated_outdoor_air_system_name")
    def test_dedicated_outdoor_air_system_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=doas_objects)
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1']['dedicated_outdoor_air_system_name'] = 'DOAS'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirLoopHVAC'].get('DOAS'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP"
                                              "zone_cooling_design_supply_air_temperature_input_method_"
                                              "supply_air_temperature")
    def test_zone_cooling_design_supply_air_temperature_input_method_supply_air_temperature(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'zone_cooling_design_supply_air_temperature'] = 13.0
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = "SupplyAirTemperature"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            13.0,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:"
                                              "zone_cooling_design_supply_air_temperature_input_method_"
                                              "temperature_difference")
    def test_zone_cooling_design_supply_air_temperature_input_method_temperature_difference(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'zone_cooling_design_supply_air_temperature_difference'] = 11.5
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = "TemperatureDifference"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            11.5,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_design_supply_air_temperature_difference'])
        return

    def test_zone_heating_design_supply_air_temperature_input_method_supply_air_temperature(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'zone_heating_design_supply_air_temperature'] = 51
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'zone_heating_design_supply_air_temperature_input_method'] = "SupplyAirTemperature"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            51,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_heating_design_supply_air_temperature'])
        return

    def test_zone_heating_design_supply_air_temperature_input_method_temperature_difference(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'zone_heating_design_supply_air_temperature_difference'] = 31
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'zone_heating_design_supply_air_temperature_input_method'] = "TemperatureDifference"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            31,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_heating_design_supply_air_temperature_difference'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:baseboard_heating_type_hot_water")
    def test_baseboard_heating_type_hot_water(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=hot_water_loop_objects)
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'baseboard_heating_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['ZoneHVAC:Baseboard:RadiantConvective:Water'].get('SPACE1-1 Baseboard Heat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:baseboard_heating_type_electric")
    def test_baseboard_heating_type_electric(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'baseboard_heating_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['ZoneHVAC:Baseboard:Convective:Electric'].get('SPACE1-1 Baseboard Heat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:baseboard_heating_availability_schedule_name")
    def test_baseboard_heating_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'baseboard_heating_type'] = 'Electric'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'baseboard_heating_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:Baseboard:Convective:Electric']['SPACE1-1 Baseboard Heat']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:baseboard_heating_capacity")
    def test_baseboard_heating_capacity(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'baseboard_heating_type'] = 'Electric'
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'baseboard_heating_capacity'] = 200
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            200,
            epjson_output['ZoneHVAC:Baseboard:Convective:Electric']['SPACE1-1 Baseboard Heat']['heating_design_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:PTHP:capacity_control_method")
    def test_capacity_control_method(self):
        self.base_epjson['HVACTemplate:Zone:PTHP']['HVACTemplate:Zone:PTHP 1'][
            'capacity_control_method'] = 'SingleZoneVAV'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'HVACTemplate-Always1',
            epjson_output['ZoneHVAC:PackagedTerminalHeatPump']['SPACE1-1 PTHP']['supply_air_fan_operating_mode_schedule_name'])
        self.assertEqual(
            'Autosize',
            epjson_output['ZoneHVAC:PackagedTerminalHeatPump']['SPACE1-1 PTHP']['no_load_supply_air_flow_rate'])
        return
