import os
import sys
sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'api'))

from energyplus_api import EnergyPlusAPI

import matplotlib.pyplot as plt

api = EnergyPlusAPI()
runtime = api.runtime()
data = api.data_transfer()
y_data = []
electricity_sensor = 0

fig = plt.gcf()
fig.show()
fig.canvas.draw()


def timestep_handler():
    global electricity_sensor
    if not electricity_sensor:
        electricity_sensor = data.get_meter_handle(u"ELECTRICITY:FACILITY")
    electricity = data.get_meter_value(electricity_sensor)
    y_data.append(electricity)
    plt.plot(y_data)
    fig.canvas.draw()


runtime.register_callback_new_timestep(timestep_handler)
runtime.run_energyplus('/tmp/epdll'.encode('utf-8'))
