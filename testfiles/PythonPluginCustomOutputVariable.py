
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
            perimeter_zone_1_handle = self.exchange.get_internal_variable_handle("Zone Floor Area", "perimeter_zn_1")
            self.zone_volumes.append(self.exchange.get_zone_volume(perimeter_zone_1_handle))
            perimeter_zone_2_handle = self.exchange.get_zone_index("perimeter_zn_2")
            self.zone_volumes.append(self.exchange.get_zone_volume(perimeter_zone_2_handle))
            perimeter_zone_3_handle = self.exchange.get_zone_index("perimeter_zn_3")
            self.zone_volumes.append(self.exchange.get_zone_volume(perimeter_zone_3_handle))
            perimeter_zone_4_handle = self.exchange.get_zone_index("perimeter_zn_4")
            self.zone_volumes.append(self.exchange.get_zone_volume(perimeter_zone_4_handle))
            core_zone = self.exchange.get_zone_index("core_zn")
            self.zone_volumes.append(self.exchange.get_zone_volume(core_zone))
            self.t_handles.append(self.exchange.get_variable_handle("Zone Mean Air Temperature", "Perimeter_zn_1"))
            self.t_handles.append(self.exchange.get_variable_handle("Zone Mean Air Temperature", "Perimeter_zn_2"))
            self.t_handles.append(self.exchange.get_variable_handle("Zone Mean Air Temperature", "Perimeter_zn_3"))
            self.t_handles.append(self.exchange.get_variable_handle("Zone Mean Air Temperature", "Perimeter_zn_4"))
            self.t_handles.append(self.exchange.get_variable_handle("Zone Mean Air Temperature", "Core_Zn"))
            self.avg_temp_variable_handle = self.helpers.get_global_handle("AverageBuildingTemp")
            self.one_time = False
        zone_temps = list()
        zone_temps.append(self.exchange.get_variable_value(self.t_handles[0]))
        zone_temps.append(self.exchange.get_variable_value(self.t_handles[1]))
        zone_temps.append(self.exchange.get_variable_value(self.t_handles[2]))
        zone_temps.append(self.exchange.get_variable_value(self.t_handles[3]))
        zone_temps.append(self.exchange.get_variable_value(self.t_handles[4]))
        numerator = 0.0
        denominator = 0.0
        for i in [0, 1, 2, 3, 4]:
            numerator += self.zone_volumes[i] * zone_temps[i]
            denominator += self.zone_volumes[i]
        average_temp = numerator / denominator
        self.helpers.set_global_value(self.avg_temp_variable_handle, average_temp)
        return 0
