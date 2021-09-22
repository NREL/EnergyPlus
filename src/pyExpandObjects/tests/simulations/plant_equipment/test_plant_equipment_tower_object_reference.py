from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent

mixed_water_objects = {
    'HVACTemplate:Plant:Boiler:ObjectReference': {
        'Second Boiler Connections': {
            "boiler_object_type": "Boiler:HotWater",
            "boiler_name": "Heat Pump Loop Boiler",
            "priority": 1,
            'template_plant_loop_type': 'MixedWater'
        }
    },
    'Curve:Quadratic': {
        "Heat Pump Loop Boiler Efficiency Curve": {
            "coefficient1_constant": 0.97,
            "coefficient2_x": 0.0633,
            "coefficient3_x_2": -0.0333,
            "maximum_value_of_x": 1.0,
            "minimum_value_of_x": 0.0
        }
    },
    "Boiler:HotWater": {
        "Heat Pump Loop Boiler": {
            "boiler_flow_mode": "ConstantFlow",
            "boiler_water_inlet_node_name": "Heat Pump Loop Boiler HW Inlet",
            "boiler_water_outlet_node_name": "Heat Pump Loop Boiler HW Outlet",
            "design_water_flow_rate": "Autosize",
            "efficiency_curve_temperature_evaluation_variable": "LeavingBoiler",
            "fuel_type": "NaturalGas",
            "maximum_part_load_ratio": 1.1,
            "minimum_part_load_ratio": 0.0,
            "nominal_capacity": "Autosize",
            "nominal_thermal_efficiency": 0.8,
            "normalized_boiler_efficiency_curve_name": "Heat Pump Loop Boiler Efficiency Curve",
            "optimum_part_load_ratio": 1,
            "parasitic_electric_load": 0,
            "sizing_factor": 1,
            "water_outlet_upper_temperature_limit": 100
        }
    },
    "HVACTemplate:Plant:MixedWaterLoop": {
        "Heat Pump Water Loop": {
            "demand_side_bypass_pipe": "Yes",
            "fluid_type": "Water",
            "high_temperature_design_setpoint": 33,
            "load_distribution_scheme": "SequentialLoad",
            "loop_design_delta_temperature": 5.6,
            "low_temperature_design_setpoint": 20,
            "operation_scheme_type": "Default",
            "pump_control_type": "Intermittent",
            "supply_side_bypass_pipe": "Yes",
            "water_pump_configuration": "ConstantFlow",
            "water_pump_rated_head": 179352,
            "water_pump_type": "SinglePump"
        }
    },
    "HVACTemplate:Plant:Tower": {
        "Heat Pump Loop Tower": {
            "free_convection_capacity": "Autosize",
            "high_speed_fan_power": "Autosize",
            "high_speed_nominal_capacity": "Autosize",
            "low_speed_fan_power": "Autosize",
            "low_speed_nominal_capacity": "Autosize",
            "sizing_factor": 1,
            "template_plant_loop_type": "MixedWater",
            "tower_type": "TwoSpeed"
        }
    },
    "HVACTemplate:System:UnitarySystem": {
        "Sys 4 Heat Pump WaterSource SnglSpd": {
            "control_type": "Load",
            "control_zone_or_thermostat_location_name": "SPACE4-1",
            "cooling_coil_type": "SingleSpeedDXWaterCooled",
            "cooling_design_supply_air_temperature": 12.8,
            "cooling_supply_air_flow_rate": "Autosize",
            "dehumidification_control_type": "None",
            "dehumidification_relative_humidity_setpoint": 60,
            "dx_cooling_coil_gross_rated_cop": 3,
            "dx_cooling_coil_gross_rated_sensible_heat_ratio": "Autosize",
            "dx_cooling_coil_gross_rated_total_capacity": "Autosize",
            "economizer_lockout": "LockoutWithCompressor",
            "economizer_maximum_limit_dry_bulb_temperature": 20,
            "economizer_type": "DifferentialDryBulb",
            "gas_heating_coil_efficiency": 0.8,
            "heat_pump_defrost_maximum_outdoor_dry_bulb_temperature": 5,
            "heat_pump_heating_coil_gross_rated_cop": 2.75,
            "heat_pump_heating_minimum_outdoor_dry_bulb_temperature": -8,
            "heat_recovery_frost_control_type": "None",
            "heat_recovery_heat_exchanger_type": "Plate",
            "heat_recovery_type": "None",
            "heating_coil_gross_rated_capacity": "Autosize",
            "heating_coil_type": "SingleSpeedDXHeatPumpWaterSource",
            "heating_design_supply_air_temperature": 50,
            "heating_supply_air_flow_rate": "Autosize",
            "humidifier_rated_capacity": 1e-06,
            "humidifier_rated_electric_power": "Autosize",
            "humidifier_relative_humidity_setpoint": 30,
            "humidifier_type": "None",
            "latent_heat_recovery_effectiveness": 0.65,
            "maximum_outdoor_air_flow_rate": "Autosize",
            "minimum_outdoor_air_flow_rate": "Autosize",
            "minimum_outdoor_air_schedule_name": "Min OA Sched",
            "no_load_supply_air_flow_rate": "Autosize",
            "number_of_speeds_for_cooling": 1,
            "number_of_speeds_or_stages_for_heating": 1,
            "return_fan": "No",
            "return_fan_delta_pressure": 300,
            "return_fan_motor_efficiency": 0.9,
            "return_fan_motor_in_air_stream_fraction": 1,
            "return_fan_total_efficiency": 0.7,
            "sensible_heat_recovery_effectiveness": 0.7,
            "sizing_option": "NonCoincident",
            "supplemental_gas_heating_or_reheat_coil_efficiency": 0.8,
            "supplemental_heating_or_reheat_coil_capacity": "Autosize",
            "supplemental_heating_or_reheat_coil_maximum_outdoor_dry_bulb_temperature": 21,
            "supplemental_heating_or_reheat_coil_type": "Electric",
            "supply_fan_delta_pressure": 600,
            "supply_fan_motor_efficiency": 0.9,
            "supply_fan_motor_in_air_stream_fraction": 1,
            "supply_fan_operating_mode_schedule_name": "FanAvailSched",
            "supply_fan_placement": "BlowThrough",
            "supply_fan_total_efficiency": 0.7
        }
    },
    "HVACTemplate:Zone:Unitary": {
        "HVACTemplate:Zone:Unitary 4": {
            "baseboard_heating_capacity": "Autosize",
            "baseboard_heating_type": "None",
            "outdoor_air_flow_rate_per_person": 0.00944,
            "outdoor_air_flow_rate_per_zone": 0.0,
            "outdoor_air_flow_rate_per_zone_floor_area": 0.0,
            "outdoor_air_method": "Flow/Person",
            "supply_air_maximum_flow_rate": "Autosize",
            "template_thermostat_name": "All Zones",
            "template_unitary_system_name": "Sys 4 Heat Pump WaterSource SnglSpd",
            "zone_cooling_design_supply_air_temperature_input_method": "SystemSupplyAirTemperature",
            "zone_heating_design_supply_air_temperature_input_method": "SystemSupplyAirTemperature",
            "zone_name": "SPACE4-1"
        }
    }
}


class TestSimulationsPlantEquipmentTowerObjectReference(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles',
                                               'HVACTemplate-5ZoneVAVWaterCooled-ObjectReference.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:ObjectReference:test_minimum_inputs")
    def test_minimum_inputs(self):
        # todo_eo: legacy fails with IDD message without 'priority', but it is not a required field
        self.base_epjson['HVACTemplate:Plant:Tower:ObjectReference'].pop('Main Tower Connection')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Tower:ObjectReference': {
                    'Main Tower Connection': {
                        'cooling_tower_name': 'Main Tower',
                        'priority': 1
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:ObjectReference:priority")
    def test_priority(self):
        # todo_eo: discuss with team that priority requires a string and not integer.
        self.base_epjson['HVACTemplate:Plant:Tower:ObjectReference']['Main Tower Connection']['priority'] = 2
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                "HVACTemplate:Plant:Tower:ObjectReference": {
                    "Second Tower Connection": {
                        "cooling_tower_name": "Second Tower",
                        "cooling_tower_object_type": "CoolingTower:SingleSpeed",
                        "priority": 1
                    }
                },
                "CoolingTower:SingleSpeed": {
                    "Second Tower": {
                        "blowdown_calculation_mode": "ConcentrationRatio",
                        "blowdown_concentration_ratio": 3,
                        "capacity_control": "FanCycling",
                        "design_air_flow_rate": "Autosize",
                        "design_fan_power": "Autosize",
                        "design_u_factor_times_area_value": "Autosize",
                        "design_water_flow_rate": "Autosize",
                        "drift_loss_percent": 0.008,
                        "evaporation_loss_mode": "SaturatedExit",
                        "free_convection_regime_air_flow_rate": "Autocalculate",
                        "free_convection_regime_u_factor_times_area_value": "Autocalculate",
                        "outdoor_air_inlet_node_name": "Second Tower Cooling Tower Outdoor Air Inlet Node",
                        "performance_input_method": "UFactorTimesAreaAndDesignWaterFlowRate",
                        "sizing_factor": 1.0,
                        "water_inlet_node_name": "Second Tower CndW Inlet",
                        "water_outlet_node_name": "Second Tower CndW Outlet"
                    }
                },
                "OutdoorAir:Node": {
                    "Second Tower Cooling Tower Outdoor Air Inlet Node": {
                        "height_above_ground": -1
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Second Tower',
            epjson_output['CondenserEquipmentList']['Condenser Water Loop All Equipment']['equipment'][0][
                'equipment_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Tower:ObjectReference:"
                                              "template_plant_loop_type")
    def test_template_plant_loop_type(self):
        # todo_eo: legacy fails if template_plant_loop_type not explicity set
        self.base_epjson['HVACTemplate:Plant:Boiler:ObjectReference']['Main Boiler Connection']['priority'] = 2
        self.base_epjson['HVACTemplate:Plant:Boiler:ObjectReference']['Main Boiler Connection'][
            'template_plant_loop_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:Plant:Tower:ObjectReference']['Main Tower Connection'][
            'template_plant_loop_type'] = 'ChilledWater'
        self.base_epjson['HVACTemplate:Zone:VAV'].pop('HVACTemplate:Zone:VAV 4')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=mixed_water_objects)
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Heat Pump Loop Tower',
            epjson_output['PlantEquipmentList']['Heat Pump Water Loop Cooling All Equipment']['equipment'][0][
                'equipment_name']
        )
        return