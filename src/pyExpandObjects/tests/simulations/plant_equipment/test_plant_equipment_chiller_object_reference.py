from pathlib import Path

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON

test_dir = Path(__file__).parent.parent.parent


class TestSimulationsPlantEquipmentChillerObjectReference(BaseSimulationTest):
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

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:ObjectReference:test_minimum_inputs")
    def test_minimum_inputs(self):
        # todo_eo: legacy fails with IDD message without 'priority', but it is not a required field
        self.base_epjson['HVACTemplate:Plant:Chiller:ObjectReference'].pop('Main Chiller Connection')
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                'HVACTemplate:Plant:Chiller:ObjectReference': {
                    'Main Chiller Connection': {
                        'chiller_name': 'Main Chiller',
                        'priority': 1
                    }
                }
            }
        )
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:PlantEquipment:Chiller:ObjectReference:priority")
    def test_priority(self):
        # todo_eo: discuss with team that priority requires a string and not integer.
        self.base_epjson['HVACTemplate:Plant:Chiller:ObjectReference']['Main Chiller Connection']['priority'] = 2
        self.ej.merge_epjson(
            super_dictionary=self.base_epjson,
            object_dictionary={
                "Chiller:Electric:EIR": {
                    "Second Chiller": {
                        "chilled_water_inlet_node_name": "Second Chiller ChW Inlet",
                        "chilled_water_outlet_node_name": "Second Chiller ChW Outlet",
                        "chiller_flow_mode": "ConstantFlow",
                        "condenser_inlet_node_name": "Second Chiller Cnd Inlet",
                        "condenser_outlet_node_name": "Second Chiller Cnd Outlet",
                        "condenser_type": "WaterCooled",
                        "cooling_capacity_function_of_temperature_curve_name": "Second Chiller RecipCapFT",
                        "design_heat_recovery_water_flow_rate": 0,
                        "electric_input_to_cooling_output_ratio_function_of_part_load_ratio_curve_name": "Second Chiller RecipEIRFPLR",
                        "electric_input_to_cooling_output_ratio_function_of_temperature_curve_name": "Second Chiller RecipEIRFT",
                        "fraction_of_compressor_electric_consumption_rejected_by_condenser": 1,
                        "leaving_chilled_water_lower_temperature_limit": 5.0,
                        "maximum_part_load_ratio": 1.0,
                        "minimum_part_load_ratio": 0.0,
                        "minimum_unloading_ratio": 0.25,
                        "optimum_part_load_ratio": 1.0,
                        "reference_capacity": "Autosize",
                        "reference_chilled_water_flow_rate": "Autosize",
                        "reference_condenser_fluid_flow_rate": "Autosize",
                        "reference_cop": 3.2,
                        "reference_entering_condenser_fluid_temperature": 29.4,
                        "reference_leaving_chilled_water_temperature": 6.67,
                        "sizing_factor": 1.0
                    }
                },
                "Curve:Biquadratic": {
                    "Second Chiller RecipCapFT": {
                        "coefficient1_constant": 0.507883,
                        "coefficient2_x": 0.145228,
                        "coefficient3_x_2": -0.00625644,
                        "coefficient4_y": -0.0011178,
                        "coefficient5_y_2": -0.0001296,
                        "coefficient6_x_y": -0.00028188,
                        "maximum_value_of_x": 10,
                        "maximum_value_of_y": 35,
                        "minimum_value_of_x": 5,
                        "minimum_value_of_y": 24
                    },
                    "Second Chiller RecipEIRFT": {
                        "coefficient1_constant": 1.03076,
                        "coefficient2_x": -0.103536,
                        "coefficient3_x_2": 0.00710208,
                        "coefficient4_y": 0.0093186,
                        "coefficient5_y_2": 0.00031752,
                        "coefficient6_x_y": -0.00104328,
                        "maximum_value_of_x": 10,
                        "maximum_value_of_y": 35,
                        "minimum_value_of_x": 5,
                        "minimum_value_of_y": 24
                    }
                },
                "Curve:Quadratic": {
                    "Second Chiller RecipEIRFPLR": {
                        "coefficient1_constant": 0.088065,
                        "coefficient2_x": 1.137742,
                        "coefficient3_x_2": -0.225806,
                        "maximum_value_of_x": 1,
                        "minimum_value_of_x": 0
                    }
                },
                "HVACTemplate:Plant:Chiller:ObjectReference": {
                    "Second Chiller Connection": {
                        "chiller_name": "Second Chiller",
                        "chiller_object_type": "Chiller:Electric:EIR",
                        "priority": 1
                    }
                }
            })
        base_file_path = self.create_idf_file_from_epjson(epjson=self.base_epjson, file_name='base_pre_input.epJSON')
        self.perform_full_comparison(base_idf_file_path=base_file_path)
        epjson_output = self.ej._get_json_file(test_dir.joinpath('..', 'simulation', 'test', 'test_input_epjson.epJSON'))
        self.assertEqual(
            'Second Chiller',
            epjson_output['PlantEquipmentList']['Chilled Water Loop All Equipment']['equipment'][0]['equipment_name'])
        return
