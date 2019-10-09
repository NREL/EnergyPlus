import os
import sys
sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'api'))

from energyplus_api import EnergyPlusAPI

import matplotlib.pyplot as plt

api = EnergyPlusAPI()
runtime = api.runtime()
data = api.data_transfer()
ydata = []


def timestep_handler():
    electricity_sensor = data.get_meter_handle(u"ELECTRICITY:FACILITY")
    electricity = data.get_meter_value(electricity_sensor)
    ydata.append(electricity)
    plt.plot(ydata)


runtime.register_callback_new_timestep(timestep_handler)
runtime.run_energyplus('/tmp/epdll'.encode('utf-8'))

# plt.plot(ydata)
# plt.show()
