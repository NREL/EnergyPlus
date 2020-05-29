from collections import deque

from pyenergyplus.plugin import EnergyPlusPlugin


class CalculateAverageTrend(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.one_time = True
        self.zone_volumes = []
        self.t_handles = []
        self.avg_temp_variable_handle = None
        self.trend_avg_var_handle = None
        self.last_five_avg_temps = deque(maxlen=5)

    def on_end_of_zone_timestep_before_zone_reporting(self):
        if self.one_time:
            zone_names = ["perimeter_zn_" + str(i) for i in range(1, 5)] + ["core_zn"]
            for zone_name in zone_names:
                handle = self.api.exchange.get_internal_variable_handle("Zone Air Volume", zone_name)
                zone_volume = self.api.exchange.get_internal_variable_value(handle)
                self.zone_volumes.append(zone_volume)
                self.t_handles.append(self.api.exchange.get_variable_handle("Zone Mean Air Temperature", zone_name))
            self.avg_temp_variable_handle = self.api.exchange.get_global_handle("AverageBuildingTemp")
            self.trend_avg_var_handle = self.api.exchange.get_global_handle("TrendAverageTemp")
            self.one_time = False
        zone_temps = list()
        for t_handle in self.t_handles:
            zone_temps.append(self.api.exchange.get_variable_value(t_handle))
        numerator = 0.0
        denominator = 0.0
        for i in range(5):
            numerator += self.zone_volumes[i] * zone_temps[i]
            denominator += self.zone_volumes[i]
        average_temp = numerator / denominator
        self.api.exchange.set_global_value(self.avg_temp_variable_handle, average_temp)
        self.last_five_avg_temps.append(average_temp)
        trend_average = sum(self.last_five_avg_temps) / len(self.last_five_avg_temps)
        self.api.exchange.set_global_value(self.trend_avg_var_handle, trend_average)
        return 0
