import sys
from pyenergyplus.api import EnergyPlusAPI


api = EnergyPlusAPI()
runtime = api.runtime()
# data = api.data_transfer()
# env_count = 0
#
#
# def hour_handler():
#     global env_count
#     env_count += 1
#     print("OH HAI NEW HOUR")
#     sys.stdout.flush()
#     outdoor_temp_sensor = data.get_variable_handle(u"SITE OUTDOOR AIR DRYBULB TEMPERATURE", u"ENVIRONMENT")
#     print("Got OA sensor handle = " + str(outdoor_temp_sensor))
#     outdoor_dew_point_sensor = data.get_variable_handle(u"SITE OUTDOOR AIR DEWPOINT TEMPERATURE", u"ENVIRONMENT")
#     # if env_count > 1:
#     #     outdoor_dew_point_actuator = data.get_actuator_handle("Outdoor Dew Point", "Environment")
#     #     print("Handle IDs: %s, %s, %s" % (outdoor_temp_sensor, outdoor_dew_point_sensor, outdoor_dew_point_actuator))
#     #     response = data.set_actuator_value(outdoor_dew_point_actuator, -25)
#     #     if response != 0:
#     #         print("Could not set actuator...")
#     oa_temp = data.get_variable_value(outdoor_temp_sensor)
#     print("Reading outdoor temp via getVariable, value is: %s" % oa_temp)
#     dp_temp = data.get_variable_value(outdoor_dew_point_sensor)
#     print("Actuated Dew Point temp value is: %s" % dp_temp)
#
#
# runtime.register_callback_end_of_hour(hour_handler)
runtime.run_energyplus('/tmp/epdll'.encode('utf-8'))
