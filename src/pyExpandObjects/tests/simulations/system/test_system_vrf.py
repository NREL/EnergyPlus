from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent

schedule_objects = {
    "Schedule:Compact": {
        "Always1": {
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
                    "field": 1
                }
            ],
            "schedule_type_limits_name": "Any Number"
        }
    }
}


class TestSimulationsSystemVRF(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath(
            '..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVRF.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:system_availability_schedule_name")
    def test_system_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'system_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:gross_rated_total_cooling_capacity")
    def test_gross_rated_total_cooling_capacity(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'gross_rated_total_cooling_capacity'] = 2000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            2000,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['gross_rated_total_cooling_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:gross_rated_cooling_cop")
    def test_gross_rated_cooling_cop(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'gross_rated_cooling_cop'] = 2.7
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            2.7,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['gross_rated_cooling_cop'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:minimum_outdoor_temperature_in_cooling_mode")
    def test_minimum_outdoor_temperature_in_cooling_mode(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'minimum_outdoor_temperature_in_cooling_mode'] = -7
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            -7,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['minimum_condenser_inlet_node_temperature_in_cooling_mode'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:maximum_outdoor_temperature_in_cooling_mode")
    def test_maximum_outdoor_temperature_in_cooling_mode(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'maximum_outdoor_temperature_in_cooling_mode'] = 42
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            42,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['maximum_condenser_inlet_node_temperature_in_cooling_mode'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:gross_rated_heating_capacity")
    def test_gross_rated_heating_capacity(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'gross_rated_heating_capacity'] = 2000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            2000,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['gross_rated_heating_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:rated_heating_capacity_sizing_ratio")
    def test_rated_heating_capacity_sizing_ratio(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'rated_heating_capacity_sizing_ratio'] = 1.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.1,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['rated_heating_capacity_sizing_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:gross_rated_heating_cop")
    def test_gross_rated_heating_cop(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'gross_rated_heating_cop'] = 3.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            3.2,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['gross_rated_heating_cop'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:minimum_outdoor_temperature_in_heating_mode")
    def test_minimum_outdoor_temperature_in_heating_mode(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'minimum_outdoor_temperature_in_heating_mode'] = -19
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            -19,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['minimum_condenser_inlet_node_temperature_in_heating_mode'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:maximum_outdoor_temperature_in_heating_mode")
    def test_maximum_outdoor_temperature_in_heating_mode(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'maximum_outdoor_temperature_in_heating_mode'] = -19
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            -19,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['maximum_condenser_inlet_node_temperature_in_heating_mode'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:minimum_heat_pump_part_load_ratio")
    def test_minimum_heat_pump_part_load_ratio(self):
        # todo_eo: minimum_heat_pump_part_load_ratio is hard-coded to 0.25, but it should be connected to this input.
        #  This has been commented out to match the current setup for the base simulation to work.
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'minimum_heat_pump_part_load_ratio'] = 0.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.2,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['minimum_heat_pump_part_load_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:zone_name_for_master_thermostat_location")
    def test_zone_name_for_master_thermostat_location(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'zone_name_for_master_thermostat_location'] = 'SPACE5-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'SPACE5-1',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['zone_name_for_master_thermostat_location'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "master_thermostat_priority_control_type_master_thermostat_priority")
    def test_master_thermostat_priority_control_type_master_thermostat_priority(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'master_thermostat_priority_control_type'] = 'MasterThermostatPriority'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'MasterThermostatPriority',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['master_thermostat_priority_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "master_thermostat_priority_control_type_load_priority")
    def test_master_thermostat_priority_control_type_load_priority(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'master_thermostat_priority_control_type'] = 'LoadPriority'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'LoadPriority',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['master_thermostat_priority_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "master_thermostat_priority_control_type_zone_priority")
    def test_master_thermostat_priority_control_type_zone_priority(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'master_thermostat_priority_control_type'] = 'ZonePriority'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ZonePriority',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['master_thermostat_priority_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "master_thermostat_priority_control_type_thermostat_offset_priority")
    def test_master_thermostat_priority_control_type_thermostat_offset_priority(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'master_thermostat_priority_control_type'] = 'ThermostatOffsetPriority'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ThermostatOffsetPriority',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['master_thermostat_priority_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "zone_name_for_master_thermostat_priority_control_type_scheduled")
    def test_zone_name_for_master_thermostat_priority_control_type_scheduled(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'master_thermostat_priority_control_type'] = 'Scheduled'
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'thermostat_priority_schedule_name'] = 'Always1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Scheduled',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['master_thermostat_priority_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "waste_heat_recovery_no")
    def test_heat_pump_waste_heat_recovery_no(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'heat_pump_waste_heat_recovery'] = 'No'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'No',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['heat_pump_waste_heat_recovery'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "waste_heat_recovery_yes")
    def test_heat_pump_waste_heat_recovery_yes(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'heat_pump_waste_heat_recovery'] = 'Yes'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Yes',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['heat_pump_waste_heat_recovery'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "equivalent_piping_length_used_for_piping_correction_factor"
                                              "_in_cooling_mode")
    def test_equivalent_piping_length_used_for_piping_correction_factor_in_cooling_mode(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'equivalent_piping_length_used_for_piping_correction_factor_in_cooling_mode'] = 40
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            40,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'equivalent_piping_length_used_for_piping_correction_factor_in_cooling_mode'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "vertical_height_used_for_piping_correction_factor")
    def test_vertical_height_used_for_piping_correction_factor(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'vertical_height_used_for_piping_correction_factor'] = 20
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            20,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'vertical_height_used_for_piping_correction_factor'])
        return

    def test_equivalent_piping_length_used_for_piping_correction_factor_in_heating_mode(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'equivalent_piping_length_used_for_piping_correction_factor_in_heating_mode'] = 40
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            40,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'equivalent_piping_length_used_for_piping_correction_factor_in_heating_mode'])
        return

    def test_crankcase_heater_power_per_compressor(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'crankcase_heater_power_per_compressor'] = 1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'crankcase_heater_power_per_compressor'])
        return

    def test_number_of_compressors(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'number_of_compressors'] = 3
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            3,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'number_of_compressors'])
        return

    def test_ratio_of_compressor_size_to_total_compressor_capacity(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'ratio_of_compressor_size_to_total_compressor_capacity'] = 0.6
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.6,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'ratio_of_compressor_size_to_total_compressor_capacity'])
        return

    def test_maximum_outdoor_dry_bulb_temperature_for_crankcase_heater(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'maximum_outdoor_dry_bulb_temperature_for_crankcase_heater'] = 7
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            7,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'maximum_outdoor_dry_bulb_temperature_for_crankcase_heater'])
        return

    def test_defrost_strategy_resistive(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'defrost_strategy'] = 'Resistive'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Resistive',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'defrost_strategy'])
        return

    def test_defrost_strategy_reverse_cycle(self):
        # todo_eo: EO and pyEO fail due to mising curve.
        #  ** Severe  ** AirConditioner:VariableRefrigerantFlow, "VRF SYS 2 AIR SOURCE VRF HEAT PUMP"
        #  defrost_energy_input_ratio_modifier_function_of_temperature_curve_name not found:
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'defrost_strategy'] = 'ReverseCycle'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ReverseCycle',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'defrost_strategy'])
        return

    def test_defrost_control_timed(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'defrost_control'] = 'Timed'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Timed',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'defrost_control'])
        return

    def test_defrost_control_on_demand(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'defrost_control'] = 'OnDemand'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OnDemand',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'defrost_control'])
        return

    def test_defrost_time_period_fraction(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'defrost_time_period_fraction'] = 0.06
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.06,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'defrost_time_period_fraction'])
        return

    def test_resistive_defrost_heater_capacity(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'defrost_strategy'] = 'Resistive'
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'resistive_defrost_heater_capacity'] = 1000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1000,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump'][
                'resistive_defrost_heater_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:maximum_outdoor_dry_bulb_temperature_for"
                                              "_defrost_operation")
    def test_maximum_outdoor_dry_bulb_temperature_for_defrost_operation(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'maximum_outdoor_dry_bulb_temperature_for_defrost_operation'] = 4
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            4,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['maximum_outdoor_dry_bulb_temperature_for_defrost_operation'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:condenser_type_air_cooled")
    def test_condenser_type_air_cooled(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'AirCooled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'AirCooled',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['condenser_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:condenser_type_evaporatively_cooled")
    def test_condenser_type_evaporatively_cooled(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'EvaporativelyCooled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'EvaporativelyCooled',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['condenser_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:condenser_type_water_cooled")
    def test_condenser_type_water_cooled(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'WaterCooled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'WaterCooled',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['condenser_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:water_condenser_volume_flow_rate")
    def test_water_condenser_volume_flow_rate(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'EvaporativelyCooled'
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'water_condenser_volume_flow_rate'] = 0.1
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['water_condenser_volume_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:evaporative_condenser_effectiveness")
    def test_evaporative_condenser_effectiveness(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'EvaporativelyCooled'
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'evaporative_condenser_effectiveness'] = 0.85
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.85,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['evaporative_condenser_effectiveness'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:evaporative_condenser_air_flow_rate")
    def test_evaporative_condenser_air_flow_rate(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'EvaporativelyCooled'
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'evaporative_condenser_air_flow_rate'] = 0.5
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.5,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['evaporative_condenser_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:"
                                              "evaporative_condenser_pump_rated_power_consumption")
    def test_evaporative_condenser_pump_rated_power_consumption(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'EvaporativelyCooled'
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'evaporative_condenser_pump_rated_power_consumption'] = 500
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            500,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['evaporative_condenser_pump_rated_power_consumption'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:basin_heater_capacity")
    def test_basin_heater_capacity(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'EvaporativelyCooled'
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'basin_heater_capacity'] = 500
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            500,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['basin_heater_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:basin_heater_setpoint_temperature")
    def test_basin_heater_setpoint_temperature(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'EvaporativelyCooled'
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'basin_heater_setpoint_temperature'] = 3
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            3,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['basin_heater_setpoint_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:basin_heater_operating_schedule_name")
    def test_basin_heater_operating_schedule_name(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=schedule_objects)
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'condenser_type'] = 'EvaporativelyCooled'
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'basin_heater_capacity'] = 500
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'basin_heater_operating_schedule_name'] = 'Always1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Always1',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['basin_heater_operating_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:fuel_type_electricity")
    def test_fuel_type_electricity(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'fuel_type'] = 'Electricity'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Electricity',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:fuel_type_natural_gas")
    def test_fuel_type_natural_gas(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'fuel_type'] = 'NaturalGas'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'NaturalGas',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:fuel_type_propane")
    def test_fuel_type_propane(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'fuel_type'] = 'Propane'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Propane',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:fuel_type_diesel")
    def test_fuel_type_diesel(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'fuel_type'] = 'Diesel'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Diesel',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:fuel_type_gasoline")
    def test_fuel_type_gasoline(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'fuel_type'] = 'Gasoline'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Gasoline',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:fuel_type_fuel_oil_no_1")
    def test_fuel_type_fuel_oil_no_1(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'fuel_type'] = 'FuelOilNo1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FuelOilNo1',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:fuel_type_fuel_oil_no_2")
    def test_fuel_type_fuel_oil_no_2(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'fuel_type'] = 'FuelOilNo2'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'FuelOilNo2',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:fuel_type_other_fuel_1")
    def test_fuel_type_other_fuel_1(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'fuel_type'] = 'OtherFuel1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OtherFuel1',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:fuel_type_other_fuel_2")
    def test_fuel_type_other_fuel_2(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'fuel_type'] = 'OtherFuel2'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OtherFuel2',
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['fuel_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:minimum_outdoor_temperature_in_heat_recovery_mode")
    def test_minimum_outdoor_temperature_in_heat_recovery_mode(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'minimum_outdoor_temperature_in_heat_recovery_mode'] = -14
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            -14,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['minimum_condenser_inlet_node_temperature_in_heat_recovery_mode'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:System:VRF:maximum_outdoor_temperature_in_heat_recovery_mode")
    def test_maximum_outdoor_temperature_in_heat_recovery_mode(self):
        self.base_epjson['HVACTemplate:System:VRF']['VRF Sys 2 Air Source'][
            'maximum_outdoor_temperature_in_heat_recovery_mode'] = 44
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            44,
            epjson_output['AirConditioner:VariableRefrigerantFlow'][
                'VRF Sys 2 Air Source VRF Heat Pump']['maximum_condenser_inlet_node_temperature_in_heat_recovery_mode'])
        return
