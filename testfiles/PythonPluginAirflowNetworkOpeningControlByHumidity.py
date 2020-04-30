from pyenergyplus.plugin import EnergyPlusPlugin


class RH_OpeningController(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.need_to_get_handles = True
        self.ZoneRH_handle = None
        self.MyOpenFactor_handle = None

    def on_begin_timestep_before_predictor(self) -> int:

        # get handles if needed
        if self.need_to_get_handles:
            self.ZoneRH_handle = self.api.exchange.get_variable_handle(
                "System Node Relative Humidity",
                "Zone 1 Node"
            )

            self.MyOpenFactor_handle = self.api.exchange.get_actuator_handle(
                "AirFlow Network Window/Door Opening",
                "Venting Opening Factor",
                "Zn001:Wall001:Win001"
            )

            self.need_to_get_handles = False

        if self.ZoneRH_handle == -1:
            self.api.runtime.issue_severe("Could not get handle to zone relative humidity")
            return 1

        if self.MyOpenFactor_handle == -1:
            self.api.runtime.issue_severe("Could not get handle to vent opening factor")
            return 1

        zone_rh = self.api.exchange.get_variable_value(self.ZoneRH_handle)

        # calculate
        if zone_rh < 25.0:
            self.api.exchange.set_actuator_value(self.MyOpenFactor_handle, 0.0)
        elif zone_rh > 60.0:
            self.api.exchange.set_actuator_value(self.MyOpenFactor_handle, 1.0)
        else:
            self.api.exchange.set_actuator_value(self.MyOpenFactor_handle, (zone_rh - 25) / (60 - 25))

        return 0
