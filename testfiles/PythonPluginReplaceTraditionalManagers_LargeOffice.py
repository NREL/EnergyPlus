from pyenergyplus.plugin import EnergyPlusPlugin


class PythonSetpointManagers(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()

        # handles
        self.need_to_get_handles = True
        self.handles = {}

    def get_handles(self):
        self.need_to_get_handles = False

    def handles_are_valid(self):
        handles_are_valid = True

        for (k, v) in self.handles.items():
            if v == -1:
                handles_are_valid = False
                self.api.runtime.issue_severe(f"Handle not found for '{k}'")

        return handles_are_valid

    def vav_5_sched_setpoint(self):
        pass

    def vav_5_mixed_air_managers(self):
        pass

    def vav_1_sched_setpoint(self):
        pass

    def vav_1_mixed_air_managers(self):
        pass

    def vav_3_sched_setpoint(self):
        pass

    def vav_3_mixed_air_managers(self):
        pass

    def vav_2_sched_setpoint(self):
        pass

    def vav_2_mixed_air_managers(self):
        pass

    def cool_sys_1_sched_setpoint(self):
        pass

    def heat_sys_1_sched_setpoint(self):
        pass

    def shw_sys_1_sched_setpoint(self):
        pass

    def vav_5_night_cycle_mgr(self):
        pass

    def vav_1_night_cycle_mgr(self):
        pass

    def vav_2_night_cycle_mgr(self):
        pass

    def vav_3_night_cycle_mgr(self):
        pass

    def on_after_predictor_after_hvac_managers(self) -> int:

        if self.api.exchange.api_data_fully_ready():
            if self.need_to_get_handles:
                self.get_handles()
                if not self.handles_are_valid():
                    return 1
            self.vav_5_sched_setpoint()
            self.vav_5_mixed_air_managers()
            self.vav_1_sched_setpoint()
            self.vav_1_mixed_air_managers()
            self.vav_3_sched_setpoint()
            self.vav_3_mixed_air_managers()
            self.vav_2_sched_setpoint()
            self.vav_2_mixed_air_managers()
            self.cool_sys_1_sched_setpoint()
            self.heat_sys_1_sched_setpoint()
            self.shw_sys_1_sched_setpoint()
            self.vav_5_night_cycle_mgr()
            self.vav_1_night_cycle_mgr()
            self.vav_2_night_cycle_mgr()
            self.vav_3_night_cycle_mgr()

        else:
            return 0
