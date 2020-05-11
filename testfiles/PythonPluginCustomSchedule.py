
from pyenergyplus.plugin import EnergyPlusPlugin


class HeatingSetPoint(EnergyPlusPlugin):

    def actuate(self, x):
        self.api.exchange.set_actuator_value(self.data['actuator_heating'], x)

    def on_begin_timestep_before_predictor(self) -> int:
        if 'handles_done' not in self.data:
            if self.api.exchange.api_data_fully_ready():
                self.data['actuator_heating'] = self.api.exchange.get_actuator_handle(
                    "Schedule:Constant", "Schedule Value", "HTGSETP_SCH"
                )
                self.data['handles_done'] = True
            else:
                return 0
        hour = self.api.exchange.hour()
        day_of_week = self.api.exchange.day_of_week()
        day_of_year = self.api.exchange.day_of_year()
        holiday = self.api.exchange.holiday_index()
        if day_of_week == 1:
            self.actuate(15.6)
        elif holiday == 3 and day_of_year == 21:
            self.actuate(21)
        elif hour < 5:
            self.actuate(15.6)
        elif 5 <= hour < 19 and 2 <= day_of_week <= 6:
            self.actuate(21)
        elif hour < 6 and day_of_week == 7:
            self.actuate(15.6)
        elif 6 <= hour < 17 and day_of_week == 7:
            self.actuate(21)
        elif hour >= 6 and hour >= 17 and day_of_week == 7:
            self.actuate(15.6)
        elif hour >= 19:
            self.actuate(15.6)
        return 0


class CoolingSetPoint(EnergyPlusPlugin):

    def actuate(self, x):
        self.api.exchange.set_actuator_value(self.data['actuator_cooling'], x)

    def on_begin_timestep_before_predictor(self) -> int:
        if 'handles_done' not in self.data:
            if self.api.exchange.api_data_fully_ready():
                self.data['actuator_cooling'] = self.api.exchange.get_actuator_handle(
                    "Schedule:Constant", "Schedule Value", "CLGSETP_SCH"
                )
                self.data['handles_done'] = True
            else:
                return 0
        hour = self.api.exchange.hour()
        day_of_week = self.api.exchange.day_of_week()
        day_of_month = self.api.exchange.day_of_month()
        holiday = self.api.exchange.holiday_index()
        month = self.api.exchange.month()
        if day_of_week == 1:
            self.actuate(30)
        elif holiday == 3 and day_of_month == 21 and month == 1:
            self.actuate(30)
        elif hour < 6:
            self.actuate(30)
        elif (6 <= hour < 22) and 2 <= day_of_week <= 6:
            self.actuate(24)
        elif 6 <= hour < 18 and day_of_week == 7:
            self.actuate(24)
        elif hour >= 6 and hour >= 18 and day_of_week == 7:
            self.actuate(30)
        elif hour > 22:
            self.actuate(30)
        return 0
