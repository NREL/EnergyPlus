from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent


class TestSimulationsPlantEquipmentBoiler(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVWaterCooled.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Boiler:test_minimum_inputs")
    def test_minimum_inputs(self):
        # todo_eo: legacy fails with IDD message if 'priority' not set, but is not required in template.
        # todo_eo: priority must be a string or it silently fails in legacy.
        self.base_epjson['HVACTemplate:Plant:Boiler'].pop('Main Boiler')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Boiler': {
                    'Main Boiler': {
                        'boiler_type': 'HotWaterBoiler',
                        'fuel_type': 'NaturalGas',
                        'priority': '1'
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Boiler:boiler_type_hot_water_boiler")
    def test_boiler_type_hot_water_boiler(self):
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['boiler_type'] = 'HotWaterBoiler'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Boiler:HotWater'))
        self.assertEqual(
            'Main Boiler Efficiency Curve',
            epjson_output['Boiler:HotWater']['Main Boiler']['normalized_boiler_efficiency_curve_name'])
        self.assertEqual(
            'LeavingBoiler',
            epjson_output['Boiler:HotWater']['Main Boiler']['efficiency_curve_temperature_evaluation_variable'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Boiler:boiler_type_district_hot_water")
    def test_boiler_type_district_hot_water(self):
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['boiler_type'] = 'DistrictHotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('DistrictHeating'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Boiler:boiler_type_condensing_hot_water_boiler")
    def test_boiler_type_condensing_hot_water_boiler(self):
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['boiler_type'] = 'CondensingHotWaterBoiler'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Main Boiler Condensing Boiler Efficiency Curve',
            epjson_output['Boiler:HotWater']['Main Boiler']['normalized_boiler_efficiency_curve_name'])
        self.assertEqual(
            'EnteringBoiler',
            epjson_output['Boiler:HotWater']['Main Boiler']['efficiency_curve_temperature_evaluation_variable'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Boiler:hot_water_boiler_efficiency")
    def test_hot_water_boiler_efficiency(self):
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['boiler_type'] = 'HotWaterBoiler'
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['efficiency'] = 0.77
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Boiler:HotWater'))
        self.assertEqual(
            0.77,
            epjson_output['Boiler:HotWater']['Main Boiler']['nominal_thermal_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Boiler:fuel_type")
    def test_fuel_type(self):
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['boiler_type'] = 'HotWaterBoiler'
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['fuel_type'] = 'Coal'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output.get('Boiler:HotWater'))
        self.assertEqual(
            'Coal',
            epjson_output['Boiler:HotWater']['Main Boiler']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Boiler:priority")
    def test_priority(self):
        # todo_eo: discuss with team that priority requires a string and not integer.  Otherwise, it silently fails
        #  and removes any object that is not 1 priority
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['boiler_type'] = 'HotWaterBoiler'
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['fuel_type'] = 'Coal'
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['priority'] = '2'
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Boiler': {
                    'Second Boiler': {
                        "boiler_type": "HotWaterBoiler",
                        "capacity": "Autosize",
                        "efficiency": 0.8,
                        "fuel_type": "Coal",
                        "priority": '1'
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Second Boiler',
            epjson_output['PlantEquipmentList']['Hot Water Loop All Equipment']['equipment'][0]['equipment_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Boiler:inputs")
    def test_inputs(self):
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['boiler_type'] = 'HotWaterBoiler'
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['sizing_factor'] = 1.1
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['minimum_part_load_ratio'] = 0.1
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['maximum_part_load_ratio'] = 0.9
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['optimum_part_load_ratio'] = 0.75
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['water_outlet_upper_temperature_limit'] = 95
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.1,
            epjson_output['Boiler:HotWater']['Main Boiler']['sizing_factor'])
        self.assertEqual(
            0.1,
            epjson_output['Boiler:HotWater']['Main Boiler']['minimum_part_load_ratio'])
        self.assertEqual(
            0.9,
            epjson_output['Boiler:HotWater']['Main Boiler']['maximum_part_load_ratio'])
        self.assertEqual(
            0.75,
            epjson_output['Boiler:HotWater']['Main Boiler']['optimum_part_load_ratio'])
        self.assertEqual(
            95,
            epjson_output['Boiler:HotWater']['Main Boiler']['water_outlet_upper_temperature_limit'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Boiler:template_plant_loop_type")
    def test_template_plant_loop_type(self):
        # todo_eo: legacy fails with this option. Plant Component Boiler:HotWater called "MAIN BOILER" was not found
        #  on plant loop="ONLY WATER LOOP MIXED WATER LOOP".
        #  Failures are gone when all template_plant_loop_type are explicitly stated
        self.base_epjson['HVACTemplate:Zone:VAV'].pop('HVACTemplate:Zone:VAV 1')
        self.base_epjson['HVACTemplate:Plant:Boiler']['Main Boiler']['template_plant_loop_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:Plant:Tower']['Main Tower']['template_plant_loop_type'] = 'ChilledWater'
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Tower': {
                    'Main MW Tower': {
                        "free_convection_capacity": "Autosize",
                        "high_speed_fan_power": "Autosize",
                        "high_speed_nominal_capacity": "Autosize",
                        "low_speed_fan_power": "Autosize",
                        "low_speed_nominal_capacity": "Autosize",
                        "tower_type": "SingleSpeed",
                        'template_plant_loop_type': 'MixedWater'
                    }
                },
                "HVACTemplate:Plant:MixedWaterLoop": {
                    "Only Water Loop": {
                        "high_temperature_design_setpoint": 34,
                        "low_temperature_design_setpoint": 20,
                        "operation_scheme_type": "Default",
                        "pump_control_type": "Intermittent",
                        "water_pump_configuration": "ConstantFlow",
                        "water_pump_rated_head": 179352
                    }
                },
                "HVACTemplate:Plant:Boiler": {
                    "Main MW Boiler": {
                        "boiler_type": "HotWaterBoiler",
                        "capacity": "Autosize",
                        "efficiency": 0.8,
                        "fuel_type": "NaturalGas",
                        "priority": "1",
                        'template_plant_loop_type': 'MixedWater'
                    }
                },
                "HVACTemplate:Zone:WaterToAirHeatPump": {
                    "HVACTemplate:Zone:WaterToAirHeatPump 1": {
                        "cooling_coil_gross_rated_cop": 3.5,
                        "cooling_coil_gross_rated_sensible_heat_ratio": "Autosize",
                        "cooling_coil_gross_rated_total_capacity": "Autosize",
                        "cooling_coil_type": "Coil:Cooling:WaterToAirHeatPump:EquationFit",
                        "cooling_supply_air_flow_rate": "Autosize",
                        "fraction_of_on_cycle_power_use": 0.01,
                        "heat_pump_fan_delay_time": 60,
                        "heat_pump_heating_coil_gross_rated_capacity": "Autosize",
                        "heat_pump_heating_coil_gross_rated_cop": 4.2,
                        "heat_pump_heating_coil_type": "Coil:Heating:WaterToAirHeatPump:EquationFit",
                        "heat_pump_time_constant": 60,
                        "heating_supply_air_flow_rate": "Autosize",
                        "maximum_cycling_rate": 2.5,
                        "outdoor_air_flow_rate_per_person": 0.00944,
                        "outdoor_air_method": "Flow/Person",
                        "supplemental_heating_coil_capacity": "Autosize",
                        "supplemental_heating_coil_type": "Electric",
                        "supply_fan_delta_pressure": 75,
                        "supply_fan_motor_efficiency": 0.9,
                        "supply_fan_placement": "DrawThrough",
                        "supply_fan_total_efficiency": 0.7,
                        "template_thermostat_name": "All Zones",
                        "zone_cooling_design_supply_air_temperature": 14.0,
                        "zone_cooling_design_supply_air_temperature_input_method": "SupplyAirTemperature",
                        "zone_heating_design_supply_air_temperature": 50.0,
                        "zone_heating_design_supply_air_temperature_input_method": "SupplyAirTemperature",
                        "zone_name": "SPACE1-1"
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        return
