import sys
from pyenergyplus.api import EnergyPlusAPI


api = EnergyPlusAPI()
runtime = api.runtime()


def hour_handler():
    print("OH HAI ZONE TIME STEP")
    sys.stdout.flush()


runtime.register_callback_end_zone_timestep_after_zone_reporting(hour_handler)
runtime.run_energyplus('/tmp/epdll'.encode('utf-8'))
