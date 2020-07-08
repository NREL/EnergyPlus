import sys

from pyenergyplus.api import EnergyPlusAPI

api = EnergyPlusAPI()

heating_ua_sizer = api.autosizing.heating_airflow_ua_sizer()
heating_ua_sizer.initialize(1.0)
success = heating_ua_sizer.calculate()
if success:
    value = heating_ua_sizer.autosized_value()
    print(f"Autosizing succeeded! Value = {value}")
else:
    print(f"Autosizing failed!")
    sys.exit(1)
