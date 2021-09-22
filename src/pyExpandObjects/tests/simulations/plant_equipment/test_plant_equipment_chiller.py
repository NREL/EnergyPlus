from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent


class TestSimulationsPlantEquipmentChiller(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVWaterCooled.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:test_minimum_inputs")
    def test_minimum_inputs (self):
        # todo_eo: legacy fails with IDD message if 'priority' not set, but is not required in template.
        # todo_eo: priority must be a string or it silently fails in legacy.
        self.base_epjson['HVACTemplate:Plant:Chiller'].pop('Main Chiller')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Chiller': {
                    'Main Chiller': {
                        'chiller_type': 'ElectricReciprocatingChiller',
                        'nominal_cop': 6.1,
                        'priority': '1'
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:"
                                              "chiller_electric_reciprocating_chiller_water_cooled")
    def test_chiller_type_chiller_electric_reciprocating_chiller_water_cooled(self):
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['chiller_type'] = 'ElectricReciprocatingChiller'
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['condenser_type'] = 'WaterCooled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Chiller:Electric:EIR'].get('Main Chiller'))
        self.assertEqual(
            'WaterCooled',
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['condenser_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:"
                                              "chiller_type_electric_reciprocating_chiller_air_cooled")
    def test_chiller_type_chiller_electric_reciprocating_chiller_air_cooled(self):
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['chiller_type'] = 'ElectricReciprocatingChiller'
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['condenser_type'] = 'AirCooled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Chiller:Electric:EIR'].get('Main Chiller'))
        self.assertEqual(
            'AirCooled',
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['condenser_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:"
                                              "chiller_type_district_chilled_water")
    def test_chiller_type_district_chilled_water(self):
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['chiller_type'] = 'DistrictChilledWater'
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller'].pop('condenser_type', None)
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['DistrictCooling'].get('Main Chiller'))
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:"
                                              "chiller_type_electric_centrifugal_chiller_water_cooled")
    def test_chiller_type_chiller_electric_centrifugal_chiller_water_cooled(self):
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['chiller_type'] = 'ElectricCentrifugalChiller'
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['condenser_type'] = 'WaterCooled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Chiller:Electric:EIR'].get('Main Chiller'))
        self.assertEqual(
            'WaterCooled',
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['condenser_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:"
                                              "chiller_type_electric_centrifugal_chiller_air_cooled")
    def test_chiller_type_chiller_electric_centrifugal_chiller_air_cooled(self):
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['chiller_type'] = 'ElectricCentrifugalChiller'
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['condenser_type'] = 'AirCooled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Chiller:Electric:EIR'].get('Main Chiller'))
        self.assertEqual(
            'AirCooled',
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['condenser_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:"
                                              "chiller_type_electric_screw_chiller_water_cooled")
    def test_chiller_type_chiller_electric_screw_chiller_water_cooled(self):
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['chiller_type'] = 'ElectricScrewChiller'
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['condenser_type'] = 'WaterCooled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Chiller:Electric:EIR'].get('Main Chiller'))
        self.assertEqual(
            'WaterCooled',
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['condenser_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:"
                                              "chiller_type_electric_screw_chiller_air_cooled")
    def test_chiller_type_chiller_electric_screw_chiller_air_cooled(self):
        # todo_eo: legacy and pyExpandObjects do not prevent the HVACTemplate:Plant:Tower from being created, which
        #  causes a lot of warnings.  Look into modifying the behaviour here
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['chiller_type'] = 'ElectricScrewChiller'
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['condenser_type'] = 'AirCooled'
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertIsNotNone(epjson_output['Chiller:Electric:EIR'].get('Main Chiller'))
        self.assertEqual(
            'AirCooled',
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['condenser_type'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:capacity")
    def test_capacity(self):
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['capacity'] = 2000
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            2000,
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['reference_capacity'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:nominal_cop")
    def test_nominal_cop(self):
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['nominal_cop'] = 3.5
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            3.5,
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['reference_cop'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:priority")
    def test_priority(self):
        # todo_eo: discuss with team that priority requires a string and not integer.  Otherwise, it silently fails
        #  and removes any object that is not 1 priority
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['priority'] = '2'
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Chiller': {
                    'Second Chiller': {
                        "chiller_type": "ElectricReciprocatingChiller",
                        "capacity": "Autosize",
                        "nominal_cop": 3.5,
                        "condenser_type": "WaterCooled",
                        "priority": '1'
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Second Chiller',
            epjson_output['PlantEquipmentList']['Chilled Water Loop All Equipment']['equipment'][0]['equipment_name'])
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:inputs")
    def test_inputs(self):
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['chiller_type'] = 'ElectricReciprocatingChiller'
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['sizing_factor'] = 1.1
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['minimum_part_load_ratio'] = 0.1
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['maximum_part_load_ratio'] = 0.9
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['optimum_part_load_ratio'] = 0.75
        self.base_epjson['HVACTemplate:Plant:Chiller']['Main Chiller']['leaving_chilled_water_lower_temperature_limit'] = 6
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            1.1,
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['sizing_factor'])
        self.assertEqual(
            0.1,
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['minimum_part_load_ratio'])
        self.assertEqual(
            0.9,
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['maximum_part_load_ratio'])
        self.assertEqual(
            0.75,
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['optimum_part_load_ratio'])
        self.assertEqual(
            6,
            epjson_output['Chiller:Electric:EIR']['Main Chiller']['leaving_chilled_water_lower_temperature_limit'])
        return
