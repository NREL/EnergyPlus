import sys
from pyenergyplus.api import EnergyPlusAPI


def hour_handler():
    print("OH HAI ZONE TIME STEP")
    sys.stdout.flush()


api = EnergyPlusAPI()
api.runtime.callback_end_zone_timestep_after_zone_reporting(hour_handler)
api.runtime.run_energyplus(sys.argv[1:])
