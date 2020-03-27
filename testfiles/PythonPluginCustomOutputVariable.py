
from pyenergyplus.plugin import EnergyPlusPlugin


class AverageZoneTemps(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.do_setup = True

    def on_end_of_zone_timestep_before_zone_reporting(self) -> int:
        if self.do_setup:
            self.data['dummy_int_var_handle'] = self.api.exchange.get_variable_handle("Site Rain Status", "Environment")
            self.data["zone_volumes"] = []
            self.data["zone_temps"] = []
            zone_names = ["perimeter_zn_" + str(i) for i in range(1, 5)] + ["core_zn"]
            for zone_name in zone_names:
                handle = self.api.exchange.get_internal_variable_handle("Zone Air Volume", zone_name)
                zone_volume = self.api.exchange.get_internal_variable_value(handle)
                self.data["zone_volumes"].append(zone_volume)
                self.data["zone_temps"].append(
                    self.api.exchange.get_variable_handle("Zone Mean Air Temperature", zone_name)
                )
            self.data["avg_temp_variable"] = self.api.exchange.get_global_handle("AverageBuildingTemp")
            self.do_setup = False
        zone_temps = list()
        dummy_val = self.api.exchange.get_variable_value(self.data['dummy_int_var_handle'])
        for t_handle in self.data["zone_temps"]:
            zone_temps.append(self.api.exchange.get_variable_value(t_handle))
        numerator = 0.0
        denominator = 0.0
        for i in range(5):
            numerator += self.data["zone_volumes"][i] * zone_temps[i]
            denominator += self.data["zone_volumes"][i]
        average_temp = numerator / denominator
        self.api.exchange.set_global_value(self.data["avg_temp_variable"], average_temp)
        return 0
