
from pyenergyplus.plugin import EnergyPlusPlugin


class HeatingSetPoint(EnergyPlusPlugin):

    def actuate(self, x):
        self.api.exchange.set_actuator_value(self.data['actuator_heating'], x)

    def on_begin_timestep_before_predictor(self) -> int:
        if 'handles_done' not in self.data:
            self.data['actuator_heating'] = self.api.exchange.get_actuator_handle(
                "HTGSETP_SCH", "Schedule:Constant", "Schedule Value"
            )
            self.data['handles_done'] = True
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

    def __init__(self):
        super().__init__()
        print("OK HELLO I AM HERE")
        print(self)
        self.name = "hello"
        print("self has api?: " + str(hasattr(self, "api")))

    def actuate(self, x):
        self.api.exchange.set_actuator_value(self.data['actuator_cooling'], x)

    def on_begin_timestep_before_predictor(self) -> int:
        print("FLKJDSFLKDSJFLKJSDFLKJSDFLKJSDFLKJSDFLKJSDLKJSDFLKJSDFLKJSDF")
        print(self)
        print("self has api?: " + str(hasattr(self, "api")))
        print(self.name)

        if 'handles_done' not in self.data:
            self.data['actuator_cooling'] = self.api.exchange.get_actuator_handle(
                "CLGSETP_SCH", "Schedule:Constant", "Schedule Value"
            )
            self.data['handles_done'] = True
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
