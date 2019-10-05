import os
import sys
sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'api'))

from energyplus_api import EnergyPlusAPI


api = EnergyPlusAPI()
runtime = api.runtime()
data = api.data_transfer()
runtime.api.cInitializeEnergyPlus('/tmp/epdll'.encode('utf-8'))
runtime.api.cInitializeSimulation()
env_count = 0
while True:
    env = runtime.api.cGetNextEnvironment()
    if env == 0:
        env_count += 1
    else:
        break
    skip = runtime.api.cSkipCurrentEnvironment()
    if skip == 0:
        continue
    runtime.api.cBeforeRunEnvironment()
    outdoorTempSensor = data.get_variable_handle(
        "ENVIRONMENT:SITE OUTDOOR AIR DRYBULB TEMPERATURE".encode('utf-8'),
        "ENVIRONMENT".encode('utf-8')
    )
    outdoorDewPointSensor = data.get_variable_handle(
        "ENVIRONMENT:SITE OUTDOOR AIR DEWPOINT TEMPERATURE".encode('utf-8'),
        "ENVIRONMENT".encode('utf-8')
    )
    outdoorDewPointActuator = data.get_actuator_handle(
        "Outdoor Dew Point".encode('utf-8'),
        "Environment".encode('utf-8')
    )
    print("Handle IDs: %s, %s, %s" % (outdoorTempSensor, outdoorDewPointSensor, outdoorDewPointActuator))
    response = data.set_actuator_value(outdoorDewPointActuator, -25)
    if response != 0:
        print("Could not set actuator...")
    runtime.api.cRunEnvironment()
    oa_temp = data.get_variable_value(outdoorTempSensor)
    print("Reading outdoor temp via getVariable, value is: %s" % oa_temp)
    dp_temp = data.get_variable_value(outdoorDewPointSensor)
    print("Actuated Dew Point temp value is: %s" % dp_temp)
    runtime.api.cAfterRunEnvironment()
runtime.api.cWrapUpSimulation()
runtime.api.cWrapUpEnergyPlus()
