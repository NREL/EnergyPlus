from pyenergyplus.plugin import EnergyPlusPlugin


class HeatingSetPoint(EnergyPlusPlugin):

    def actuate(self, state, x):
        self.api.exchange.set_actuator_value(state, self.data['actuator_heating'], x)

    def on_begin_zone_timestep_before_set_current_weather(self, state) -> int:
        if self.api.exchange.warmup_flag(state):
            return 0
        self.api.exchange.tomorrow_weather_outdoor_dry_bulb_at_time(state, 3, 2)
        return 0

    def on_begin_timestep_before_predictor(self, state) -> int:
        if 'handles_done' not in self.data:
            self.data['actuator_heating'] = self.api.exchange.get_actuator_handle(
                state, "Schedule:Constant", "Schedule Value", "HTGSETP_SCH"
            )
            if self.data['actuator_heating'] == -1:
                self.api.runtime.issue_severe(state, "Could not get handle to heating setpoint schedule")
                return 1
            self.data['handles_done'] = True

        hour = self.api.exchange.hour(state)
        day_of_week = self.api.exchange.day_of_week(state)
        day_of_year = self.api.exchange.day_of_year(state)
        holiday = self.api.exchange.holiday_index(state)
        if day_of_week == 1:
            self.actuate(state, 15.6)
        elif holiday == 3 and day_of_year == 21:
            self.actuate(state, 21)
        elif hour < 5:
            self.actuate(state, 15.6)
        elif 5 <= hour < 19 and 2 <= day_of_week <= 6:
            self.actuate(state, 21)
        elif hour < 6 and day_of_week == 7:
            self.actuate(state, 15.6)
        elif 6 <= hour < 17 and day_of_week == 7:
            self.actuate(state, 21)
        elif hour >= 6 and hour >= 17 and day_of_week == 7:
            self.actuate(state, 15.6)
        elif hour >= 19:
            self.actuate(state, 15.6)
        return 0


class CoolingSetPoint(EnergyPlusPlugin):

    def actuate(self, state, x):
        self.api.exchange.set_actuator_value(state, self.data['actuator_cooling'], x)

    def on_begin_timestep_before_predictor(self, state) -> int:
        if 'handles_done' not in self.data:
            self.data['actuator_cooling'] = self.api.exchange.get_actuator_handle(
                state, "Schedule:Constant", "Schedule Value", "CLGSETP_SCH"
            )
            if self.data['actuator_cooling'] == -1:
                self.api.runtime.issue_severe(state, "Could not get handle to cooling setpoint schedule")
                return 1
            self.data['handles_done'] = True
        hour = self.api.exchange.hour(state)
        day_of_week = self.api.exchange.day_of_week(state)
        day_of_month = self.api.exchange.day_of_month(state)
        holiday = self.api.exchange.holiday_index(state)
        month = self.api.exchange.month(state)
        if day_of_week == 1:
            self.actuate(state, 30)
        elif holiday == 3 and day_of_month == 21 and month == 1:
            self.actuate(state, 30)
        elif hour < 6:
            self.actuate(state, 30)
        elif (6 <= hour < 22) and 2 <= day_of_week <= 6:
            self.actuate(state, 24)
        elif 6 <= hour < 18 and day_of_week == 7:
            self.actuate(state, 24)
        elif hour >= 6 and hour >= 18 and day_of_week == 7:
            self.actuate(state, 30)
        elif hour > 22:
            self.actuate(state, 30)
        return 0


class AverageZoneTemps(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.need_to_get_handles = True
        self.T1_handle = None
        self.T2_handle = None
        self.T3_handle = None
        self.T4_handle = None
        self.T5_handle = None

        self.Zn1vol = None
        self.Zn2vol = None
        self.Zn3vol = None
        self.Zn4vol = None
        self.Zn5vol = None

        self.ave_bldg_temp_handle = None

    def on_end_of_zone_timestep_before_zone_reporting(self, state) -> int:

        # check if API ready
        if self.api.exchange.api_data_fully_ready(state):
            # get handles if needed
            if self.need_to_get_handles:
                self.T1_handle = self.api.exchange.get_variable_handle(state, "Zone Mean Air Temperature", "Perimeter_ZN_1")
                self.T2_handle = self.api.exchange.get_variable_handle(state, "Zone Mean Air Temperature", "Perimeter_ZN_2")
                self.T3_handle = self.api.exchange.get_variable_handle(state, "Zone Mean Air Temperature", "Perimeter_ZN_3")
                self.T4_handle = self.api.exchange.get_variable_handle(state, "Zone Mean Air Temperature", "Perimeter_ZN_4")
                self.T5_handle = self.api.exchange.get_variable_handle(state, "Zone Mean Air Temperature", "Core_ZN")

                Zn1vol_handle = self.api.exchange.get_internal_variable_handle(state, "Zone Air Volume", "Perimeter_ZN_1")
                Zn2vol_handle = self.api.exchange.get_internal_variable_handle(state, "Zone Air Volume", "Perimeter_ZN_2")
                Zn3vol_handle = self.api.exchange.get_internal_variable_handle(state, "Zone Air Volume", "Perimeter_ZN_3")
                Zn4vol_handle = self.api.exchange.get_internal_variable_handle(state, "Zone Air Volume", "Perimeter_ZN_4")
                Zn5vol_handle = self.api.exchange.get_internal_variable_handle(state, "Zone Air Volume", "Core_ZN")

                self.Zn1vol = self.api.exchange.get_internal_variable_value(state, Zn1vol_handle)
                self.Zn2vol = self.api.exchange.get_internal_variable_value(state, Zn2vol_handle)
                self.Zn3vol = self.api.exchange.get_internal_variable_value(state, Zn3vol_handle)
                self.Zn4vol = self.api.exchange.get_internal_variable_value(state, Zn4vol_handle)
                self.Zn5vol = self.api.exchange.get_internal_variable_value(state, Zn5vol_handle)

                self.ave_bldg_temp_handle = self.api.exchange.get_global_handle(state, "AverageBuildingTemp")

                self.need_to_get_handles = False

            # calculations
            T1 = self.api.exchange.get_variable_value(state, self.T1_handle)
            T2 = self.api.exchange.get_variable_value(state, self.T2_handle)
            T3 = self.api.exchange.get_variable_value(state, self.T3_handle)
            T4 = self.api.exchange.get_variable_value(state, self.T4_handle)
            T5 = self.api.exchange.get_variable_value(state, self.T5_handle)

            num = T1 * self.Zn1vol + T2 * self.Zn2vol + T3 * self.Zn3vol + T4 * self.Zn4vol + T5 * self.Zn5vol
            den = self.Zn1vol + self.Zn2vol + self.Zn3vol + self.Zn4vol + self.Zn5vol

            self.api.exchange.set_global_value(state, self.ave_bldg_temp_handle, num / den)

        return 0
