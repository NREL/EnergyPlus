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


class TestSimulationsZoneIdealLoadsAirSystem(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZonePurchAir.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:system_availability_schedule_name")
    def test_system_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'system_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:maximum_heating_supply_air_temperature")
    def test_maximum_heating_supply_air_temperature(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'maximum_heating_supply_air_temperature'] = 45
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            45,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['maximum_heating_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:minimum_cooling_supply_air_temperature")
    def test_minimum_cooling_supply_air_temperature(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'minimum_cooling_supply_air_temperature'] = 15
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            15,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['minimum_cooling_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:maximum_heating_supply_air_humidity_ratio")
    def test_maximum_heating_supply_air_humidity_ratio(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'maximum_heating_supply_air_humidity_ratio'] = 0.017
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.017,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['maximum_heating_supply_air_humidity_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:minimum_cooling_supply_air_humidity_ratio")
    def test_minimum_cooling_supply_air_humidity_ratio(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'minimum_cooling_supply_air_humidity_ratio'] = 0.009
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.009,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['minimum_cooling_supply_air_humidity_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:maximum_heating_air_flow_rate")
    def test_maximum_heating_air_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'heating_limit'] = 'LimitFlowRate'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'maximum_heating_air_flow_rate'] = 0.5
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'LimitFlowRate',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['heating_limit'])
        self.assertEqual(
            0.5,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['maximum_heating_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:maximum_sensible_heating_capacity")
    def test_maximum_sensible_heating_capacity(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'heating_limit'] = 'LimitCapacity'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'maximum_sensible_heating_capacity'] = 500
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'LimitCapacity',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['heating_limit'])
        self.assertEqual(
            500,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['maximum_sensible_heating_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:maximum_cooling_air_flow_rate")
    def test_maximum_cooling_air_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'cooling_limit'] = 'LimitFlowRate'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'maximum_cooling_air_flow_rate'] = 0.5
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'LimitFlowRate',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['cooling_limit'])
        self.assertEqual(
            0.5,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['maximum_cooling_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:maximum_total_cooling_capacity")
    def test_maximum_total_cooling_capacity(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'cooling_limit'] = 'LimitCapacity'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'maximum_total_cooling_capacity'] = 500
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'LimitCapacity',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['cooling_limit'])
        self.assertEqual(
            500,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['maximum_total_cooling_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:heating_availability_schedule_name")
    def test_heating_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'heating_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['heating_availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:cooling_availability_schedule_name")
    def test_cooling_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'cooling_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['cooling_availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:dehumidification_control_type")
    def test_dehumidification_control_type(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'dehumidification_control_type'] = 'ConstantSupplyHumidityRatio'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ConstantSupplyHumidityRatio',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['dehumidification_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:cooling_sensible_heat_ratio")
    def test_cooling_sensible_heat_ratio(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'dehumidification_control_type'] = 'ConstantSensibleHeatRatio'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'cooling_sensible_heat_ratio'] = 0.65
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.65,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['cooling_sensible_heat_ratio'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:dehumidification_setpoint")
    def test_dehumidification_setpoint(self):
        # todo_eo: legacy makes ZoneControl:Humidistat for all zones and setpoints do not appear to align with template
        #  inputs for each zone
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'dehumidification_control_type'] = 'Humidistat'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'dehumidification_setpoint'] = 65
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'HVACTemplate-Always65.0',
            epjson_output['ZoneControl:Humidistat']['SPACE1-1 Humidification Humidistat'][
                'dehumidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:humidification_control_type")
    def test_humidification_control_type(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'humidification_control_type'] = 'ConstantSupplyHumidityRatio'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'ConstantSupplyHumidityRatio',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System'][
                'humidification_control_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:humidification_setpoint")
    def test_humidification_setpoint(self):
        # todo_eo: legacy makes ZoneControl:Humidistat for all zones and setpoints do not appear to align with template
        #  inputs for each zone
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'humidification_control_type'] = 'Humidistat'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'humidification_setpoint'] = 33
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'HVACTemplate-Always33.0',
            epjson_output['ZoneControl:Humidistat']['SPACE1-1 Humidification Humidistat'][
                'humidifying_relative_humidity_setpoint_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:outdoor_air_method_flow_per_person")
    def test_outdoor_air_method_flow_per_person(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'outdoor_air_method'] = 'Flow/Person'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:outdoor_air_method_flow_per_area")
    def test_outdoor_air_method_flow_per_area(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'outdoor_air_method'] = 'Flow/Area'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:outdoor_air_method_flow_per_zone")
    def test_outdoor_air_method_flow_per_zone(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'outdoor_air_method'] = 'Flow/Zone'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:outdoor_air_method_detailed_specification")
    def test_outdoor_air_method_detailed_specification(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=design_specification_objects)
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'outdoor_air_method'] = 'DetailedSpecification'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'design_specification_outdoor_air_object_name'] = 'SPACE1-1 SZ DSOA Custom Object'
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'design_specification_zone_air_distribution_object_name'] = 'SPACE1-1 SZ DSZAD Custom Object'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['DesignSpecification:OutdoorAir'].get('SPACE1-1 SZ DSOA Custom Object'))
        self.assertIsNotNone(epjson_output['DesignSpecification:ZoneAirDistribution'].get('SPACE1-1 SZ DSZAD Custom Object'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:demand_controlled_ventilation_type")
    def test_demand_controlled_ventilation_type_occupancy_schedule(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'demand_controlled_ventilation_type'] = 'OccupancySchedule'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OccupancySchedule',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['demand_controlled_ventilation_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:demand_controlled_ventilation_type")
    def test_demand_controlled_ventilation_type_co2_setpoint(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'demand_controlled_ventilation_type'] = 'CO2Setpoint'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'CO2Setpoint',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['demand_controlled_ventilation_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:outdoor_air_economizer_type")
    def test_outdoor_air_economizer_type(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'outdoor_air_economizer_type'] = 'DifferentialDryBulb'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'DifferentialDryBulb',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['outdoor_air_economizer_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:IdealLoadsAirSystem:heat_recovery_type")
    def test_heat_recovery_type(self):
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'sensible_heat_recovery_effectiveness'] = 0.65
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'latent_heat_recovery_effectiveness'] = 0.6
        self.base_epjson['HVACTemplate:Zone:IdealLoadsAirSystem']['HVACTemplate:Zone:IdealLoadsAirSystem 1'][
            'heat_recovery_type'] = 'Enthalpy'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.65,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['sensible_heat_recovery_effectiveness'])
        self.assertEqual(
            0.6,
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['latent_heat_recovery_effectiveness'])
        self.assertEqual(
            'Enthalpy',
            epjson_output['ZoneHVAC:IdealLoadsAirSystem']['SPACE1-1 Ideal Loads Air System']['heat_recovery_type'])
        return