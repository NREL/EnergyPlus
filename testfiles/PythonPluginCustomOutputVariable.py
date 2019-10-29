
from pyenergyplus.plugin import EnergyPlusPlugin


class AverageZoneTemps(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.one_time = True
        self.zone_volumes = []
        self.t_handles = []
        self.avg_temp_variable_handle = None

    def main(self) -> int:
        if self.one_time:
            zone_names = ["perimeter_zn_" + str(i) for i in range(1, 5)] + ["core_zn"]
            for zone_name in zone_names:
                handle = self.exchange.get_internal_variable_handle("Zone Air Volume", zone_name)
                zone_volume = self.exchange.get_internal_variable_value(handle)
                self.zone_volumes.append(zone_volume)
                self.t_handles.append(self.exchange.get_variable_handle("Zone Mean Air Temperature", zone_name))
            self.avg_temp_variable_handle = self.helpers.get_global_handle("AverageBuildingTemp")
            self.one_time = False
        zone_temps = list()
        for t_handle in self.t_handles:
            zone_temps.append(self.exchange.get_variable_value(t_handle))
        numerator = 0.0
        denominator = 0.0
        for i in range(5):
            numerator += self.zone_volumes[i] * zone_temps[i]
            denominator += self.zone_volumes[i]
        average_temp = numerator / denominator
        self.helpers.set_global_value(self.avg_temp_variable_handle, average_temp)
        return 0
