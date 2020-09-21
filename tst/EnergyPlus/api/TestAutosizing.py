import sys

from pyenergyplus.api import EnergyPlusAPI

api = EnergyPlusAPI()
state = api.state_manager.new_state()

heating_ua_sizer = api.autosizing.heating_airflow_ua_sizer()

for elevation in [0.0, 25.0, 50.0]:
    heating_ua_sizer.initialize_for_zone(state, heating_ua_sizer.ZoneConfigTerminal, elevation, 500.0)
    success = heating_ua_sizer.size(state)
    if success:
        value = heating_ua_sizer.autosized_value()
        print(f"Autosizing succeeded! Terminal Unit Value = {value} m3/s")
    else:
        print(f"Autosizing failed!")
        sys.exit(1)
    heating_ua_sizer.initialize_for_zone(state, heating_ua_sizer.ZoneConfigInductionUnit, elevation, 500.0)
    success = heating_ua_sizer.size(state)
    if success:
        value = heating_ua_sizer.autosized_value()
        print(f"Autosizing succeeded! Induction Unit Value = {value} m3/s")
    else:
        print(f"Autosizing failed!")
        sys.exit(1)
