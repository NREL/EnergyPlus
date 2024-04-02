import os
from subprocess import check_call, CalledProcessError, STDOUT

from ep_testing.exceptions import EPTestingException
from ep_testing.tests.base import BaseTest


class HVACDiagram(BaseTest):

    def name(self):
        return 'Test running 5ZoneAirCooled.idf, then HVACDiagram and make sure the SVG is created'

    def run(self, install_root: str, verbose: bool, kwargs: dict):
        idf_path = os.path.join(install_root, 'ExampleFiles', '5ZoneAirCooled.idf')
        print('* Running test class "%s" on file "%s"... ' % (self.__class__.__name__, '5ZoneAirCooled.idf'), end='')
        eplus_binary = os.path.join(install_root, 'energyplus')
        dev_null = open(os.devnull, 'w')
        try:
            check_call([eplus_binary, '-D', idf_path], stdout=dev_null, stderr=STDOUT)
            print(' [E+ FINISHED] ', end='')
        except CalledProcessError:
            raise EPTestingException('EnergyPlus failed!')
        hvac_diagram_binary = os.path.join(install_root, 'PostProcess', 'HVAC-Diagram')
        try:
            check_call([hvac_diagram_binary], stdout=dev_null, stderr=STDOUT)
            print(' [HVAC DIAGRAM FINISHED] ', end='')
        except CalledProcessError:
            raise EPTestingException('Transition failed!')
        if os.path.exists('eplusout.svg'):
            print(' [SVG FILE EXISTS] [DONE]!')
        else:
            raise EPTestingException('SVG Did not exist!')
