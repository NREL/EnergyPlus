import os
import sys
sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'api'))

from energyplus_api import EnergyPlusAPI


api = EnergyPlusAPI()
runtime = api.runtime()
data = api.data_transfer()


def timestep_handler():
    electricity_sensor = data.get_variable_handle(u"ELECTRICITY:FACILITY", u"ENVIRONMENT")
    print("Handle ID: %s" % electricity_sensor)
    electricity = data.get_variable_value(electricity_sensor)
    print("ELECTRICITY USAGE: %s" % electricity)


runtime.register_callback_new_timestep(timestep_handler)
runtime.run_energyplus('/tmp/epdll'.encode('utf-8'))
