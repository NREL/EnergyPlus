from pathlib import Path
import subprocess

from tests.simulations import BaseSimulationTest

test_dir = Path(__file__).parent.parent.parent


class TestSimulationCommandLine(BaseSimulationTest):
    def setUp(self):
        return

    def teardown(self):
        return

    def test_no_schema_command_line_args(self):
        sub_process = subprocess.Popen(
            [
                'python',
                str(test_dir / '..' / 'src' / 'main.py'),
                '-ns',
                str(test_dir / '..' / 'simulation' / 'ExampleFiles' /
                    'HVACTemplate-5ZoneVAVWaterCooled.epJSON'),
                '-o',
                str(test_dir / '..' / 'simulation' / 'test')
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT
        )
        process_msg, _ = sub_process.communicate()
        # check console output for errors
        self.assertNotIn('ERROR', str(process_msg))
        return
