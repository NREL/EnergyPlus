import os
import sys
sys.path.append(os.path.dirname(os.path.realpath(__file__)))

from energyplus_api import EnergyPlusAPI


api = EnergyPlusAPI()
runtime = api.runtime()
runtime.run_energyplus_fully('/tmp/epdll'.encode('utf-8'))
