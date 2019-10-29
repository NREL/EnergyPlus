import sys
from pyenergyplus.api import EnergyPlusAPI


api = EnergyPlusAPI()
env_count = 0


def time_step_handler():
    global env_count
    env_count += 1
    print("OH HAI NEW TIMESTEP")
    sys.stdout.flush()
    outdoor_temp_sensor = api.exchange.get_variable_handle(
        u"SITE OUTDOOR AIR DRYBULB TEMPERATURE", u"ENVIRONMENT"
    )
    print("Got OA sensor handle = " + str(outdoor_temp_sensor))
    outdoor_dew_point_sensor = api.exchange.get_variable_handle(
        u"SITE OUTDOOR AIR DEWPOINT TEMPERATURE", u"ENVIRONMENT"
    )
    if env_count > 1:
        outdoor_dew_point_actuator = api.exchange.get_actuator_handle(
            "Outdoor Dew Point", "Environment"
        )
        print("Handle IDs: %s, %s, %s" % (outdoor_temp_sensor, outdoor_dew_point_sensor, outdoor_dew_point_actuator))
        response = api.exchange.set_actuator_value(outdoor_dew_point_actuator, -25)
        if response != 0:
            print("Could not set actuator...")
    oa_temp = api.exchange.get_variable_value(outdoor_temp_sensor)
    print("Reading outdoor temp via getVariable, value is: %s" % oa_temp)
    dp_temp = api.exchange.get_variable_value(outdoor_dew_point_sensor)
    print("Actuated Dew Point temp value is: %s" % dp_temp)


api.runtime.callback_end_zone_timestep_after_zone_reporting(time_step_handler)
api.runtime.run_energyplus('/tmp/epdll'.encode('utf-8'))
