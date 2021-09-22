from pathlib import Path
import tempfile
from argparse import Namespace
import json

from tests.simulations import BaseSimulationTest
from src.epjson_handler import EPJSON
from src.main import main

test_dir = Path(__file__).parent.parent.parent


class TestSimulationsThermostat(BaseSimulationTest):
    def setUp(self):
        self.ej = EPJSON()
        base_idf_file_path = test_dir.joinpath('..', 'simulation', 'ExampleFiles', 'HVACTemplate-5ZoneVAVWaterCooled.idf')
        base_copy_file_path = self._copy_to_test_directory(base_idf_file_path)
        # read in base file, then edit inputs for alternate tests
        self.base_epjson = self.get_epjson_object_from_idf_file(base_copy_file_path)
        self.base_epjson.pop('Output:Variable')
        return

    def teardown(self):
        return

    @BaseSimulationTest._test_logger(doc_text="Simulation:Thermostat:test_blank")
    def test_blank(self):
        self.base_epjson['HVACTemplate:Thermostat'].pop('All Zones')
        self.base_epjson['HVACTemplate:Thermostat']['All Zones'] = {}
        with tempfile.NamedTemporaryFile(suffix='.epJSON', mode='w') as temp_file:
            json.dump(
                self.base_epjson,
                temp_file)
            temp_file.seek(0)
            output = main(
                Namespace(
                    file=temp_file.name,
                    no_schema=False
                )
            )
        self.assertRegex(output['Output:PreprocessorMessage'], r'In HVACTemplate:Thermostat')
        return
