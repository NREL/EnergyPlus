import sys
from pyenergyplus.api import EnergyPlusAPI


api = EnergyPlusAPI()
runtime = api.runtime()


def hour_handler():
    print("OH HAI NEW HOUR")
    sys.stdout.flush()


runtime.register_callback_end_of_hour(hour_handler)
runtime.run_energyplus('/tmp/epdll'.encode('utf-8'))
