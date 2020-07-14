import sys

from pyenergyplus.api import EnergyPlusAPI

api = EnergyPlusAPI()

heating_ua_sizer = api.autosizing.heating_airflow_ua_sizer()

for elevation in [0.0, 25.0, 50.0]:
    heating_ua_sizer.initialize_for_zone_terminal_fan_coil(elevation, 500.0)
    success = heating_ua_sizer.calculate()
    if success:
        value = heating_ua_sizer.autosized_value()
        print(f"Autosizing succeeded! Value = {value} m3/s")
    else:
        print(f"Autosizing failed!")
        sys.exit(1)
