from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent


class TestSimulationsPlantEquipmentTower(BaseSimulationTest):
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:test_minimum_inputs")
    def test_minimum_inputs (self):
        # todo_eo: legacy fails with IDD message if 'priority' not set, but is not required in template.
        # todo_eo: priority must be a string or it silently fails in legacy.
        self.base_epjson['HVACTemplate:Plant:Tower'].pop('Main Tower')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Tower': {
                    'Main Tower': {
                        "tower_type": "SingleSpeed",
                        'priority': '1'
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:tower_type_single_speed")
    def test_tower_type_single_speed(self):
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['tower_type'] = 'SingleSpeed'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('CoolingTower:SingleSpeed'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:tower_type_two_speed")
    def test_tower_type_two_speed(self):
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['tower_type'] = 'TwoSpeed'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('CoolingTower:TwoSpeed'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:inputs_two_speed")
    def test_inputs_two_speed(self):
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['tower_type'] = 'TwoSpeed'
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['high_speed_nominal_capacity'] = 25000
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['high_speed_fan_power'] = 370
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['low_speed_nominal_capacity'] = 10000
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['low_speed_fan_power'] = 180
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['free_convection_capacity'] = 2000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            25000,
            epjson_output['CoolingTower:TwoSpeed']['Main Tower']['high_speed_nominal_capacity'])
        self.assertEqual(
            10000,
            epjson_output['CoolingTower:TwoSpeed']['Main Tower']['low_speed_nominal_capacity'])
        self.assertEqual(
            370,
            epjson_output['CoolingTower:TwoSpeed']['Main Tower']['high_fan_speed_fan_power'])
        self.assertEqual(
            180,
            epjson_output['CoolingTower:TwoSpeed']['Main Tower']['low_fan_speed_fan_power'])
        self.assertEqual(
            2000,
            epjson_output['CoolingTower:TwoSpeed']['Main Tower']['free_convection_nominal_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:inputs_single_speed")
    def test_inputs_single_speed(self):
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['tower_type'] = 'SingleSpeed'
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['high_speed_nominal_capacity'] = 25000
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['high_speed_fan_power'] = 370
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['free_convection_capacity'] = 2000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            25000,
            epjson_output['CoolingTower:SingleSpeed']['Main Tower']['nominal_capacity'])
        self.assertEqual(
            2000,
            epjson_output['CoolingTower:SingleSpeed']['Main Tower']['free_convection_capacity'])
        self.assertEqual(
            370,
            epjson_output['CoolingTower:SingleSpeed']['Main Tower']['design_fan_power'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:priority")
    def test_priority(self):
        # todo_eo: discuss with team that priority requires a string and not integer.  Otherwise, it silently fails
        #  and removes any object that is not 1 priority
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['priority'] = '2'
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Tower': {
                    'Second Tower': {
                        "free_convection_capacity": "Autosize",
                        "high_speed_fan_power": "Autosize",
                        "high_speed_nominal_capacity": "Autosize",
                        "low_speed_fan_power": "Autosize",
                        "low_speed_nominal_capacity": "Autosize",
                        "tower_type": "SingleSpeed",
                        "priority": '1'
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Second Tower',
            epjson_output['PlantEquipmentList']['Only Water Loop Cooling All Equipment']['equipment'][0]['equipment_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:sizing_factor_single_speed")
    def test_sizing_factor_single_speed(self):
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['tower_type'] = 'SingleSpeed'
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['sizing_factor'] = 1.25
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.25,
            epjson_output['CoolingTower:SingleSpeed']['Main Tower']['sizing_factor'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:sizing_factor_two_speed")
    def test_sizing_factor_two_speed(self):
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['tower_type'] = 'TwoSpeed'
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['sizing_factor'] = 1.25
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.25,
            epjson_output['CoolingTower:TwoSpeed']['Main Tower']['sizing_factor'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:template_plant_loop_type")
    def test_template_plant_loop_type(self):
        # todo_eo: legacy fails with this option. Plant Component CoolingTower:SingleSpeed called "MAIN CHW TOWER" was
        #  not found on plant loop="ONLY WATER LOOP MIXED WATER LOOP".** Severe  ** AuditBranches: There are 2
        #  branch(es) that do not appear on any BranchList.
        #  explicitly setting all template_plant_loop_type values fixes issue
        self.base_epjson['HVACTemplate:Zone:WaterToAirHeatPump'].pop('HVACTemplate:Zone:WaterToAirHeatPump 1')
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['template_plant_loop_type'] = 'MixedWater'
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Tower': {
                    'Second Tower': {
                        "free_convection_capacity": "Autosize",
                        "high_speed_fan_power": "Autosize",
                        "high_speed_nominal_capacity": "Autosize",
                        "low_speed_fan_power": "Autosize",
                        "low_speed_nominal_capacity": "Autosize",
                        "tower_type": "SingleSpeed",
                        'template_plant_loop_type': 'MixedWater'
                    },
                    "Main ChW Tower": {
                        "free_convection_capacity": "Autosize",
                        "high_speed_fan_power": "Autosize",
                        "high_speed_nominal_capacity": "Autosize",
                        "low_speed_fan_power": "Autosize",
                        "low_speed_nominal_capacity": "Autosize",
                        "priority": "1",
                        "tower_type": "SingleSpeed",
                        'template_plant_loop_type': 'ChilledWater'  # required for legacy to succeed
                    }
                },
                "HVACTemplate:Zone:ConstantVolume": {
                    "HVACTemplate:Zone:ConstantVolume 1": {
                        "baseboard_heating_type": "None",
                        "outdoor_air_flow_rate_per_person": 0.00944,
                        "outdoor_air_method": "Flow/Person",
                        "reheat_coil_type": "None",
                        "supply_air_maximum_flow_rate": "Autosize",
                        "template_constant_volume_system_name": "AHU 1 Spaces 1-4",
                        "template_thermostat_name": "All Zones",
                        "zone_cooling_design_supply_air_temperature": 12.8,
                        "zone_cooling_design_supply_air_temperature_difference": 11.11,
                        "zone_cooling_design_supply_air_temperature_input_method": "SystemSupplyAirTemperature",
                        "zone_heating_design_supply_air_temperature": 50,
                        "zone_heating_design_supply_air_temperature_difference": 30,
                        "zone_heating_design_supply_air_temperature_input_method": "SupplyAirTemperature",
                        "zone_name": "SPACE1-1"
                    },
                },
                "HVACTemplate:System:ConstantVolume": {
                    "AHU 1 Spaces 1-4": {
                        "cooling_coil_design_setpoint_temperature": 12.8,
                        "cooling_coil_reset_outdoor_dry_bulb_high": 23.3,
                        "cooling_coil_reset_outdoor_dry_bulb_low": 15.6,
                        "cooling_coil_setpoint_at_outdoor_dry_bulb_high": 12.8,
                        "cooling_coil_setpoint_at_outdoor_dry_bulb_low": 15.6,
                        "cooling_coil_setpoint_control_type": "Warmest",
                        "cooling_coil_type": "ChilledWater",
                        "dehumidification_control_type": "None",
                        "dehumidification_relative_humidity_setpoint": 60,
                        "economizer_lower_temperature_limit": 4,
                        "economizer_type": "FixedDryBulb",
                        "economizer_upper_temperature_limit": 19,
                        "gas_heating_coil_efficiency": 0.8,
                        "gas_preheat_coil_efficiency": 0.8,
                        "heat_recovery_frost_control_type": "None",
                        "heat_recovery_heat_exchanger_type": "Plate",
                        "heat_recovery_type": "None",
                        "heating_coil_capacity": "Autosize",
                        "heating_coil_design_setpoint": 15,
                        "heating_coil_reset_outdoor_dry_bulb_high": 12.2,
                        "heating_coil_reset_outdoor_dry_bulb_low": 7.8,
                        "heating_coil_setpoint_at_outdoor_dry_bulb_high": 12.2,
                        "heating_coil_setpoint_at_outdoor_dry_bulb_low": 15,
                        "heating_coil_setpoint_control_type": "OutdoorAirTemperatureReset",
                        "heating_coil_type": "Electric",
                        "humidifier_rated_capacity": 1e-06,
                        "humidifier_rated_electric_power": "Autosize",
                        "humidifier_relative_humidity_setpoint": 30,
                        "humidifier_type": "None",
                        "latent_heat_recovery_effectiveness": 0.65,
                        "maximum_outdoor_air_flow_rate": "Autosize",
                        "minimum_outdoor_air_flow_rate": "Autosize",
                        "night_cycle_control": "CycleOnAny",
                        "preheat_coil_design_setpoint": 7.2,
                        "preheat_coil_type": "None",
                        "return_fan": "Yes",
                        "return_fan_delta_pressure": 300,
                        "return_fan_motor_efficiency": 0.9,
                        "return_fan_motor_in_air_stream_fraction": 1,
                        "return_fan_total_efficiency": 0.7,
                        "sensible_heat_recovery_effectiveness": 0.7,
                        "supply_fan_delta_pressure": 600,
                        "supply_fan_maximum_flow_rate": "Autosize",
                        "supply_fan_motor_efficiency": 0.9,
                        "supply_fan_motor_in_air_stream_fraction": 1,
                        "supply_fan_placement": "DrawThrough",
                        "supply_fan_total_efficiency": 0.7,
                        "system_availability_schedule_name": "FanAvailSched"
                    }
                },
                "HVACTemplate:Plant:ChilledWaterLoop": {
                    "Chilled Water Loop": {
                        "chilled_water_design_setpoint": 7.22,
                        "chilled_water_pump_configuration": "ConstantPrimaryNoSecondary",
                        "chilled_water_reset_outdoor_dry_bulb_high": 26.7,
                        "chilled_water_reset_outdoor_dry_bulb_low": 15.6,
                        "chilled_water_setpoint_at_outdoor_dry_bulb_high": 6.7,
                        "chilled_water_setpoint_at_outdoor_dry_bulb_low": 12.2,
                        "chilled_water_setpoint_reset_type": "OutdoorAirTemperatureReset",
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
                "Schedule:Compact": {
                    "FanAvailSched": {
                        "data": [
                            {
                                "field": "Through: 12/31"
                            },
                            {
                                "field": "For: WeekDays CustomDay1 CustomDay2"
                            },
                            {
                                "field": "Until: 7:00"
                            },
                            {
                                "field": 0.0
                            },
                            {
                                "field": "Until: 21:00"
                            },
                            {
                                "field": 1.0
                            },
                            {
                                "field": "Until: 24:00"
                            },
                            {
                                "field": 0.0
                            },
                            {
                                "field": "For: Weekends Holiday"
                            },
                            {
                                "field": "Until: 24:00"
                            },
                            {
                                "field": 0.0
                            },
                            {
                                "field": "For: SummerDesignDay"
                            },
                            {
                                "field": "Until: 24:00"
                            },
                            {
                                "field": 1.0
                            },
                            {
                                "field": "For: WinterDesignDay"
                            },
                            {
                                "field": "Until: 24:00"
                            },
                            {
                                "field": 1.0
                            }
                        ],
                        "schedule_type_limits_name": "Fraction"
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        # todo_eo: test for main tower in condenserequipmentlist since that is the default allocation
        return
