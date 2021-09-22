from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent

design_specification_objects = {
    "DesignSpecification:OutdoorAir": {
        "SPACE1-1 SZ DSOA Custom Object": {
            "outdoor_air_flow_per_person": 0.00944,
            "outdoor_air_method": "Flow/Person"
        }
    },
    "DesignSpecification:ZoneAirDistribution": {
        "SPACE1-1 SZ DSZAD Custom Object": {}
    }
}


class TestSimulationsZoneVAVFanPowered(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVFanPowered.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:primary_supply_air_maximum_flow_rate")
    def test_primary_supply_air_maximum_flow_rate(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'primary_supply_air_maximum_flow_rate'] = 0.1
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'reheat_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.1,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['cooling_design_air_flow_rate'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:zone_heating_sizing_factor")
    def test_zone_heating_sizing_factor(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_heating_sizing_factor'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_heating_sizing_factor'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:zone_heating_sizing_factor")
    def test_zone_cooling_sizing_factor(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_cooling_sizing_factor'] = 1.2
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.2,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_sizing_factor'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:primary_supply_air_minimum_flow_fraction")
    def test_primary_supply_air_minimum_flow_fraction(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'primary_supply_air_minimum_flow_fraction'] = 0.25
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.25,
            epjson_output['AirTerminal:SingleDuct:SeriesPIU:Reheat']['SPACE1-1 Series PIU Reheat'][
                'minimum_primary_air_flow_fraction'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:flow_type_series")
    def test_flow_type_series(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'flow_type'] = 'Series'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirTerminal:SingleDuct:SeriesPIU:Reheat'].get('SPACE1-1 Series PIU Reheat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:flow_type_paralles")
    def test_flow_type_parallel(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'flow_type'] = 'Parallel'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath(
            '..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirTerminal:SingleDuct:ParallelPIU:Reheat'].get(
            'SPACE1-1 Parallel PIU Reheat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:flow_type_series_from_plenum")
    def test_flow_type_series_from_plenum(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'flow_type'] = 'SeriesFromPlenum'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirTerminal:SingleDuct:SeriesPIU:Reheat'].get('SPACE1-1 Series PIU Reheat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:flow_type_series_from_plenum")
    def test_flow_type_parallel_from_plenum(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'flow_type'] = 'ParallelFromPlenum'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirTerminal:SingleDuct:ParallelPIU:Reheat'].get('SPACE1-1 Parallel PIU Reheat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:outdoor_air_method_flow_per_person")
    def test_outdoor_air_method_flow_per_person(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'outdoor_air_method'] = 'Flow/Person'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:outdoor_air_method_flow_per_area")
    def test_outdoor_air_method_flow_per_area(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'outdoor_air_method'] = 'Flow/Area'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:outdoor_air_method_flow_per_zone")
    def test_outdoor_air_method_flow_per_zone(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'outdoor_air_method'] = 'Flow/Zone'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:outdoor_air_method_detailed_specification")
    def test_outdoor_air_method_detailed_specification(self):
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary=design_specification_objects)
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'outdoor_air_method'] = 'DetailedSpecification'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'design_specification_outdoor_air_object_name'] = 'SPACE1-1 SZ DSOA Custom Object'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'design_specification_zone_air_distribution_object_name'] = 'SPACE1-1 SZ DSZAD Custom Object'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['DesignSpecification:OutdoorAir'].get('SPACE1-1 SZ DSOA Custom Object'))
        self.assertIsNotNone(epjson_output['DesignSpecification:ZoneAirDistribution'].get('SPACE1-1 SZ DSZAD Custom Object'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:reheat_coil_type_electric")
    def test_reheat_coil_type_electric(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'reheat_coil_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirTerminal:SingleDuct:SeriesPIU:Reheat'].get('SPACE1-1 Series PIU Reheat'))
        self.assertIsNotNone(epjson_output['Coil:Heating:Electric'].get('SPACE1-1 Reheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:reheat_coil_type_hot_water")
    def test_reheat_coil_type_hot_water(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'reheat_coil_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirTerminal:SingleDuct:SeriesPIU:Reheat'].get('SPACE1-1 Series PIU Reheat'))
        self.assertIsNotNone(epjson_output['Coil:Heating:Water'].get('SPACE4-1 Reheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:reheat_coil_type_gas")
    def test_reheat_coil_type_gas(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'reheat_coil_type'] = 'Gas'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirTerminal:SingleDuct:SeriesPIU:Reheat'].get('SPACE1-1 Series PIU Reheat'))
        self.assertIsNotNone(epjson_output['Coil:Heating:Fuel'].get('SPACE1-1 Reheat Coil'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:reheat_coil_availability_schedule_name")
    def test_reheat_coil_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'reheat_coil_type'] = 'Electric'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'reheat_coil_availability_schedule_name'] = 'OCCUPY-1'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 2'][
            'reheat_coil_type'] = 'Gas'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 2'][
            'reheat_coil_availability_schedule_name'] = 'OCCUPY-1'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 3'][
            'reheat_coil_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 3'][
            'reheat_coil_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Electric']['SPACE1-1 Reheat Coil']['availability_schedule_name'])
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Fuel']['SPACE2-1 Reheat Coil']['availability_schedule_name'])
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Coil:Heating:Water']['SPACE3-1 Reheat Coil']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:fan_total_efficiency")
    def test_fan_total_efficiency(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'fan_total_efficiency'] = 0.65
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.65,
            epjson_output['Fan:ConstantVolume']['SPACE1-1 Supply Fan']['fan_total_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:fan_delta_pressure")
    def test_fan_delta_pressure(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'fan_delta_pressure'] = 900
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            900,
            epjson_output['Fan:ConstantVolume']['SPACE1-1 Supply Fan']['pressure_rise'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:fan_motor_efficiency")
    def test_fan_motor_efficiency(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'fan_motor_efficiency'] = 0.85
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            0.85,
            epjson_output['Fan:ConstantVolume']['SPACE1-1 Supply Fan']['motor_efficiency'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:supply_plenum_name")
    def test_supply_plenum_name(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'].pop('return_plenum_name')
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'supply_plenum_name'] = 'PLENUM-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirLoopHVAC:SupplyPlenum'].get('SPACE1-1 Supply Plenum'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:return_plenum_name")
    def test_return_plenum_name(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'].pop('return_plenum_name')
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'return_plenum_name'] = 'PLENUM-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['AirLoopHVAC:ReturnPlenum'].get('SPACE1-1 Return Plenum'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:baseboard_heating_type_hot_water")
    def test_baseboard_heating_type_hot_water(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'baseboard_heating_type'] = 'HotWater'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['ZoneHVAC:Baseboard:RadiantConvective:Water'].get('SPACE1-1 Baseboard Heat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:baseboard_heating_type_electric")
    def test_baseboard_heating_type_electric(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'baseboard_heating_type'] = 'Electric'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['ZoneHVAC:Baseboard:Convective:Electric'].get('SPACE1-1 Baseboard Heat'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:baseboard_heating_availability_schedule_name")
    def test_baseboard_heating_availability_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'baseboard_heating_type'] = 'Electric'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'baseboard_heating_availability_schedule_name'] = 'OCCUPY-1'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 2'][
            'baseboard_heating_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 2'][
            'baseboard_heating_availability_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:Baseboard:Convective:Electric']['SPACE1-1 Baseboard Heat']['availability_schedule_name'])
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['ZoneHVAC:Baseboard:RadiantConvective:Water']['SPACE2-1 Baseboard Heat']['availability_schedule_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:baseboard_heating_capacity")
    def test_baseboard_heating_capacity(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'baseboard_heating_type'] = 'Electric'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'baseboard_heating_capacity'] = 200
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 2'][
            'baseboard_heating_type'] = 'HotWater'
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 2'][
            'baseboard_heating_capacity'] = 200
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            200,
            epjson_output['ZoneHVAC:Baseboard:Convective:Electric']['SPACE1-1 Baseboard Heat']['heating_design_capacity'])
        self.assertEqual(
            200,
            epjson_output['ZoneHVAC:Baseboard:RadiantConvective:Water']['SPACE2-1 Baseboard Heat']['heating_design_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:"
                                              "zone_cooling_design_supply_air_temperature_input_method_"
                                              "system_supply_air_temperature")
    def test_zone_cooling_design_supply_air_temperature_input_method_system_supply_air_temperature(self):
        self.base_epjson['HVACTemplate:System:VAV']['VAV Sys 1'][
            'cooling_coil_design_setpoint'] = 13.0
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = "SystemSupplyAirTemperature"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            13.0,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:VAV:FanPowered:"
                                              "zone_cooling_design_supply_air_temperature_input_method_"
                                              "supply_air_temperature")
    def test_zone_cooling_design_supply_air_temperature_input_method_supply_air_temperature(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_cooling_design_supply_air_temperature'] = 13.0
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = "SupplyAirTemperature"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            13.0,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:"
                                              "zone_cooling_design_supply_air_temperature_input_method_"
                                              "temperature_difference")
    def test_zone_cooling_design_supply_air_temperature_input_method_temperature_difference(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_cooling_design_supply_air_temperature_difference'] = 11.5
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_cooling_design_supply_air_temperature_input_method'] = "TemperatureDifference"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            11.5,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_cooling_design_supply_air_temperature_difference'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:"
                                              "zone_heating_design_supply_air_temperature_input_method_"
                                              "supply_air_temperature")
    def test_zone_heating_design_supply_air_temperature_input_method_supply_air_temperature(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_heating_design_supply_air_temperature'] = 51
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_heating_design_supply_air_temperature_input_method'] = "SupplyAirTemperature"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            51,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_heating_design_supply_air_temperature'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:"
                                              "zone_heating_design_supply_air_temperature_input_method_"
                                              "temperature_difference")
    def test_zone_heating_design_supply_air_temperature_input_method_temperature_difference(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_heating_design_supply_air_temperature_difference'] = 31
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_heating_design_supply_air_temperature_input_method'] = "TemperatureDifference"
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            31,
            epjson_output['Sizing:Zone']['SPACE1-1 Sizing Zone']['zone_heating_design_supply_air_temperature_difference'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Zone:VAV:FanPowered:zone_piu_fan_schedule_name")
    def test_zone_piu_fan_schedule_name(self):
        self.base_epjson['HVACTemplate:Zone:VAV:FanPowered']['HVACTemplate:Zone:VAV:FanPowered 1'][
            'zone_piu_fan_schedule_name'] = 'OCCUPY-1'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'OCCUPY-1',
            epjson_output['Fan:ConstantVolume']['SPACE1-1 Supply Fan']['availability_schedule_name'])
        return
