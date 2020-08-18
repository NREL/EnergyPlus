import sys
from pyenergyplus.api import EnergyPlusAPI

one_time = True
outdoor_temp_sensor = 0
outdoor_dew_point_sensor = 0
outdoor_dew_point_actuator = 0


def time_step_handler(state):
    global one_time, outdoor_temp_sensor, outdoor_dew_point_sensor, outdoor_dew_point_actuator
    sys.stdout.flush()
    if one_time:
        if api.exchange.api_data_fully_ready(state):
            # val = api.exchange.list_available_api_data_csv()
            # with open('/tmp/data.csv', 'w') as f:
            #     f.write(val.decode(encoding='utf-8'))
            outdoor_temp_sensor = api.exchange.get_variable_handle(
                state, u"SITE OUTDOOR AIR DRYBULB TEMPERATURE", u"ENVIRONMENT"
            )
            outdoor_dew_point_sensor = api.exchange.get_variable_handle(
                state, u"SITE OUTDOOR AIR DEWPOINT TEMPERATURE", u"ENVIRONMENT"
            )
            outdoor_dew_point_actuator = api.exchange.get_actuator_handle(
                state, "Weather Data", "Outdoor Dew Point", "Environment"
            )
            if outdoor_temp_sensor == -1 or outdoor_dew_point_sensor == -1 or outdoor_dew_point_actuator == -1:
                sys.exit(1)
            one_time = False
    api.exchange.set_actuator_value(state, outdoor_dew_point_actuator, -25)
    oa_temp = api.exchange.get_variable_value(state, outdoor_temp_sensor)
    print("Reading outdoor temp via getVariable, value is: %s" % oa_temp)
    dp_temp = api.exchange.get_variable_value(state, outdoor_dew_point_sensor)
    print("Actuated Dew Point temp value is: %s" % dp_temp)


api = EnergyPlusAPI()
state = api.state_manager.new_state()
api.runtime.callback_end_zone_timestep_after_zone_reporting(state, time_step_handler)
api.exchange.request_variable(state, "SITE OUTDOOR AIR DRYBULB TEMPERATURE", "ENVIRONMENT")
api.exchange.request_variable(state, "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", "ENVIRONMENT")
# trim off this python script name when calling the run_energyplus function so you end up with just
# the E+ args, like: -d /output/dir -D /path/to/input.idf
api.runtime.run_energyplus(state, sys.argv[1:])
