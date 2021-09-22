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
        "Main HW Boiler": {
            "boiler_type": "HotWaterBoiler",
            "capacity": "Autosize",
            "efficiency": 0.8,
            "fuel_type": "NaturalGas",
            "priority": "1",
            "template_plant_loop_type": "HotWater"
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


class TestSimulationsZoneVRF(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVRF.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:test_minimum_inputs")
    def test_minimum_inputs(self):
        # todo_eo: legacy fails on conversion from idf to epjson because
        #  Missing required property 'gross_rated_sensible_heat_ratio'.
        #  \r\n<root>[Coil:Cooling:DX:VariableRefrigerantFlow][SPACE1-1 VRF Cooling Coil] - Missing required property
        #  'gross_rated_total_cooling_capacity'.\r\n<root>[Coil:Heating:DX:VariableRefrigerantFlow][SPACE1-1 VRF
        #  Heating Coil] - Missing required property 'gross_rated_heating_capacity'.\r\nErrors occurred when
        #  validating input file
        self.base_epjson['HVACTemplate:Zone:VRF'].pop('HVACTemplate:Zone:VRF 1')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Zone:VRF': {
                    'HVACTemplate:Zone:VRF 1': {
                        "template_thermostat_name": "All Zones",
                        "template_vrf_system_name": "VRF Sys 1 Water Source",
                        "zone_name": "SPACE1-1"
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:test_minimum_inputs_doas")
    def test_minimum_inputs_doas(self):
        # todo_eo: legacy fails on conversion from idf to epjson because
        #  Missing required property 'gross_rated_sensible_heat_ratio'.
        #  \r\n<root>[Coil:Cooling:DX:VariableRefrigerantFlow][SPACE1-1 VRF Cooling Coil] - Missing required property
        #  'gross_rated_total_cooling_capacity'.\r\n<root>[Coil:Heating:DX:VariableRefrigerantFlow][SPACE1-1 VRF
        #  Heating Coil] - Missing required property 'gross_rated_heating_capacity'.\r\nErrors occurred when
        #  validating input file
        self.base_epjson['HVACTemplate:Zone:VRF'].pop('HVACTemplate:Zone:VRF 1')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Zone:VRF': {
                    'HVACTemplate:Zone:VRF 1': {
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:zone_heating_sizing_factor")
    def test_zone_heating_sizing_factor(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1']['zone_heating_sizing_factor'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_heating_sizing_factor'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:zone_heating_sizing_factor")
    def test_zone_cooling_sizing_factor(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1']['zone_cooling_sizing_factor'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_sizing_factor'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:rated_total_heating_capacity_sizing_ratio")
    def test_rated_total_heating_capacity_sizing_ratio(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'rated_total_heating_capacity_sizing_ratio'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit']['rated_heating_capacity_sizing_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:cooling_supply_air_flow_rate")
    def test_cooling_supply_air_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'cooling_supply_air_flow_rate'] = 0.1
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:no_cooling_supply_air_flow_rate")
    def test_no_cooling_supply_air_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'no_cooling_supply_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit']['no_cooling_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:heating_supply_air_flow_rate")
    def test_heating_supply_air_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'heating_supply_air_flow_rate'] = 0.1
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:no_heating_supply_air_flow_rate")
    def test_no_heating_supply_air_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'no_heating_supply_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit']['no_heating_supply_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:cooling_outdoor_air_flow_rate")
    def test_cooling_outdoor_air_flow_rate(self):
        # todo_eo: does not appear that zonehvac object gets value applied
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'cooling_outdoor_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'][
                'cooling_outdoor_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:heating_outdoor_air_flow_rate")
    def test_heating_outdoor_air_flow_rate(self):
        # todo_eo: ZoneHVAC:TerminalUnit:VariableRefrigerantFlow heating outdoor air flow rate not set in legacy.
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'heating_outdoor_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'][
                'heating_outdoor_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:no_load_outdoor_air_flow_rate")
    def test_no_load_outdoor_air_flow_rate(self):
        # todo_eo: value is not mapping to anything in legacy
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'no_load_outdoor_air_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'][
                'no_load_outdoor_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:outdoor_air_method_flow_per_person")
    def test_outdoor_air_method_flow_per_person(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'outdoor_air_method'] = 'Flow/Person'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:outdoor_air_method_flow_per_area")
    def test_outdoor_air_method_flow_per_area(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'outdoor_air_method'] = 'Flow/Area'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:outdoor_air_method_flow_per_zone")
    def test_outdoor_air_method_flow_per_zone(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'outdoor_air_method'] = 'Flow/Zone'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:outdoor_air_method_detailed_specification")
    def test_outdoor_air_method_detailed_specification(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=design_specification_objects)
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'outdoor_air_method'] = 'DetailedSpecification'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'design_specification_outdoor_air_object_name'] = 'SPACE1-1 SZ DSOA Custom Object'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'design_specification_zone_air_distribution_object_name'] = 'SPACE1-1 SZ DSZAD Custom Object'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['DesignSpecification:OutdoorAir'].get('SPACE1-1 SZ DSOA Custom Object'))
        self.assertIsNotNone(epjson_output['DesignSpecification:ZoneAirDistribution'].get('SPACE1-1 SZ DSZAD Custom Object'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:system_availability_schedule_name")
    def test_system_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'system_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit']['terminal_unit_availability_schedule'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:supply_fan_operating_mode_schedule_name")
    def test_supply_fan_operating_mode_schedule_name(self):
        # todo_eo: schedule does not appear to be mapped to the ZoneHVAC:equipmentconnections even though the same
        #  name is used.  discuss with team if this should be removed for this template only.  Note, generally removing
        #  this transition will cause similar tests for pthp and ptac to fail
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'supply_fan_operating_mode_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'][
                'supply_air_fan_operating_mode_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:supply_air_fan_placement")
    def test_supply_air_fan_placement_blow_through(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'supply_air_fan_placement'] = 'BlowThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'BlowThrough',
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'][
                'supply_air_fan_placement'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:supply_air_fan_placement")
    def test_supply_air_fan_placement_draw_through(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'supply_air_fan_placement'] = 'DrawThrough'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DrawThrough',
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'][
                'supply_air_fan_placement'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:supply_fan_total_efficiency")
    def test_supply_fan_total_efficiency(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'supply_fan_total_efficiency'] = 0.65
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.65,
            epjson_output['Fan:OnOff']['SPACE1-1 Supply Fan']['fan_total_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:supply_fan_delta_pressure")
    def test_supply_fan_delta_pressure(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'supply_fan_delta_pressure'] = 65
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            65,
            epjson_output['Fan:OnOff']['SPACE1-1 Supply Fan']['pressure_rise'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:supply_fan_motor_efficiency")
    def test_supply_fan_motor_efficiency(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'supply_fan_motor_efficiency'] = 0.85
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.85,
            epjson_output['Fan:OnOff']['SPACE1-1 Supply Fan']['motor_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:cooling_coil_type_variable_refrigerant_flow_dx")
    def test_cooling_coil_type_variable_refrigerant_flow_dx(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'cooling_coil_type'] = 'VariableRefrigerantFlowDX'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Coil:Cooling:DX:VariableRefrigerantFlow',
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'][
                'cooling_coil_object_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:cooling_coil_type_variable_refrigerant_flow_dx")
    def test_cooling_coil_type_none(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'cooling_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'].get(
                'cooling_coil_object_type'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:cooling_coil_availability_schedule_name")
    def test_cooling_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'cooling_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Cooling:DX:VariableRefrigerantFlow']['SPACE1-1 VRF Cooling Coil'][
                'availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:cooling_coil_gross_rated_total_capacity")
    def test_cooling_coil_gross_rated_total_capacity(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'cooling_coil_gross_rated_total_capacity'] = 100
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            100,
            epjson_output['Coil:Cooling:DX:VariableRefrigerantFlow']['SPACE1-1 VRF Cooling Coil'][
                'gross_rated_total_cooling_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:cooling_coil_gross_rated_sensible_heat_ratio")
    def test_cooling_coil_gross_rated_sensible_heat_ratio(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'cooling_coil_gross_rated_sensible_heat_ratio'] = 0.65
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.65,
            epjson_output['Coil:Cooling:DX:VariableRefrigerantFlow']['SPACE1-1 VRF Cooling Coil'][
                'gross_rated_sensible_heat_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:heat_pump_heating_coil_type_variable_refrigerant_flow_dx")
    def test_heat_pump_heating_coil_type_variable_refrigerant_flow_dx(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'heat_pump_heating_coil_type'] = 'VariableRefrigerantFlowDX'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Coil:Heating:DX:VariableRefrigerantFlow',
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'][
                'heating_coil_object_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:heat_pump_heating_coil_type_variable_refrigerant_flow_dx")
    def test_heat_pump_heating_coil_type_none(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'heat_pump_heating_coil_type'] = 'None'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNone(
            epjson_output['ZoneHVAC:TerminalUnit:VariableRefrigerantFlow']['SPACE1-1 VRF Terminal Unit'].get(
                'heating_coil_object_type'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:heat_pump_heating_coil_availability_schedule_name")
    def test_heat_pump_heating_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'heat_pump_heating_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:DX:VariableRefrigerantFlow']['SPACE1-1 VRF Heating Coil'][
                'availability_schedule'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:heat_pump_heating_coil_gross_rated_total_capacity")
    def test_heat_pump_heating_coil_gross_rated_capacity(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'heat_pump_heating_coil_gross_rated_capacity'] = 100
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            100,
            epjson_output['Coil:Heating:DX:VariableRefrigerantFlow']['SPACE1-1 VRF Heating Coil'][
                'gross_rated_heating_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:dedicated_outdoor_air_system_name")
    def test_dedicated_outdoor_air_system_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=doas_objects)
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'dedicated_outdoor_air_system_name'] = 'DOAS'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirLoopHVAC'].get('DOAS'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:"
                                              "zone_cooling_design_supply_air_temperature_input_method_"
                                              "supply_air_temperature")
    def test_zone_cooling_design_supply_air_temperature_input_method_supply_air_temperature(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'zone_cooling_design_supply_air_temperature'] = 13.0
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = "SupplyAirTemperature"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            13.0,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:"
                                              "zone_cooling_design_supply_air_temperature_input_method_"
                                              "temperature_difference")
    def test_zone_cooling_design_supply_air_temperature_input_method_temperature_difference(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'zone_cooling_design_supply_air_temperature_difference'] = 11.5
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = "TemperatureDifference"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            11.5,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_design_supply_air_temperature_difference'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:"
                                              "zone_heating_design_supply_air_temperature_input_method_"
                                              "supply_air_temperature")
    def test_zone_heating_design_supply_air_temperature_input_method_supply_air_temperature(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'zone_heating_design_supply_air_temperature'] = 51
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'zone_heating_design_supply_air_temperature_input_method'] = "SupplyAirTemperature"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            51,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_heating_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:"
                                              "zone_heating_design_supply_air_temperature_input_method_"
                                              "temperature_difference")
    def test_zone_heating_design_supply_air_temperature_input_method_temperature_difference(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'zone_heating_design_supply_air_temperature_difference'] = 31
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'zone_heating_design_supply_air_temperature_input_method'] = "TemperatureDifference"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            31,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_heating_design_supply_air_temperature_difference'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:baseboard_heating_type_hot_water")
    def test_baseboard_heating_type_hot_water(self):
        # todo_eo: Legacy fails when a HVACTemplate:Plant:HotWaterLoop and HVACTemplate:Plant:Boiler are
        #  included in the same file as HVACTemplate:PLant:MixedWaterLoop and existing HVACTemplate:Plant:Boiler.
        #  The PlantEquipmentList for the MixedWaterLoop includes the HW boiler.
        #  Explicitly setting template_plant_loop_type in both boilers fixes this in legacy.
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=hot_water_loop_objects)
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler'][
            'template_plant_loop_type'] = 'MixedWater'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'baseboard_heating_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['ZoneHVAC:Baseboard:RadiantConvective:Water'].get('SPACE1-1 Baseboard Heat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:baseboard_heating_type_electric")
    def test_baseboard_heating_type_electric(self):
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'baseboard_heating_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['ZoneHVAC:Baseboard:Convective:Electric'].get('SPACE1-1 Baseboard Heat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:baseboard_heating_availability_schedule_name")
    def test_baseboard_heating_availability_schedule_name(self):
        # todo_eo: Legacy fails when a HVACTemplate:Plant:HotWaterLoop and HVACTemplate:Plant:Boiler are
        #  included in the same file as HVACTemplate:PLant:MixedWaterLoop and existing HVACTemplate:Plant:Boiler.
        #  The PlantEquipmentList for the MixedWaterLoop includes the HW boiler.
        #  Explicitly setting template_plant_loop_type in both boilers fixes this in legacy.
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=hot_water_loop_objects)
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler'][
            'template_plant_loop_type'] = 'MixedWater'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'baseboard_heating_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'baseboard_heating_availability_schedule_name'] = 'OCCUPY-1'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 2'][
            'baseboard_heating_type'] = 'Electric'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 2'][
            'baseboard_heating_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:Baseboard:RadiantConvective:Water']['SPACE1-1 Baseboard Heat']['availability_schedule_name'])
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:Baseboard:Convective:Electric']['SPACE2-1 Baseboard Heat']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VRF:baseboard_heating_capacity")
    def test_baseboard_heating_capacity(self):
        # todo_eo: Legacy fails when a HVACTemplate:Plant:HotWaterLoop and HVACTemplate:Plant:Boiler are
        #  included in the same file as HVACTemplate:PLant:MixedWaterLoop and existing HVACTemplate:Plant:Boiler.
        #  The PlantEquipmentList for the MixedWater        Loop includes the HW boiler.
        #  Explicitly setting template_plant_loop_type in both boilers fixes this in legacy.
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=hot_water_loop_objects)
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler'][
            'template_plant_loop_type'] = 'MixedWater'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'baseboard_heating_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 1'][
            'baseboard_heating_capacity'] = 200
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 2'][
            'baseboard_heating_type'] = 'Electric'
        self.base_epjson['HVACTemplate:Zone:VRF']['HVACTemplate:Zone:VRF 2'][
            'baseboard_heating_capacity'] = 200
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            200,
            epjson_output['ZoneHVAC:Baseboard:RadiantConvective:Water']['SPACE1-1 Baseboard Heat']['heating_design_capacity'])
        self.assertEqual(
            200,
            epjson_output['ZoneHVAC:Baseboard:Convective:Electric']['SPACE2-1 Baseboard Heat']['heating_design_capacity'])
        return