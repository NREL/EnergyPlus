from pyenergyplus.plugin import EnergyPlusPlugin
import math


class Set_Shade_Control_State(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.need_to_get_handles = True
        self.Solar_Beam_Incident_Cos_handle = None
        self.Zn001_Wall001_Win001_Shading_Deploy_Status_actuator_handle = None
        self.Zn001_Wall001_Win001_Shading_Deploy_Status_output_handle = None
        self.Zone_Sensible_Cool_Rate_handle = None
        self.Shade_Status_Off_handle = None
        self.Shade_Status_Interior_Blind_On_handle = None
        self.IncidentAngle_handle = None

    @staticmethod
    def rad_to_deg(rad):
        return 180 * rad / math.pi

    def on_begin_timestep_before_predictor(self, state) -> int:

        # check if API ready
        if self.api.exchange.api_data_fully_ready(state):
            # get handles if needed
            if self.need_to_get_handles:
                self.Solar_Beam_Incident_Cos_handle = self.api.exchange.get_variable_handle(
                    state,
                    "Surface Outside Face Beam Solar Incident Angle Cosine Value",
                    "Zn001:Wall001:Win001"
                )

                self.Zn001_Wall001_Win001_Shading_Deploy_Status_actuator_handle = self.api.exchange.get_actuator_handle(
                    state,
                    "Window Shading Control",
                    "Control Status",
                    "Zn001:Wall001:Win001"
                )

                self.Zn001_Wall001_Win001_Shading_Deploy_Status_output_handle = self.api.exchange.get_global_handle(
                    state,
                    "Zn001_Wall001_Win001_Shading_Deploy_Status")

                self.Zone_Sensible_Cool_Rate_handle = self.api.exchange.get_variable_handle(
                    state,
                    "Zone Air System Sensible Cooling Rate",
                    "WEST ZONE"
                )

                self.Shade_Status_Off_handle = self.api.exchange.get_global_handle(
                    state,
                    "Shade_Status_Off")

                self.Shade_Status_Interior_Blind_On_handle = self.api.exchange.get_global_handle(
                    state,
                    "Shade_Status_Interior_Blind_On")

                self.IncidentAngle_handle = self.api.exchange.get_global_handle(state, "IncidentAngle")

                self.need_to_get_handles = False

            # calculations
            Solar_Beam_Incident_Cos_handle = self.api.exchange.get_variable_value(state, self.Solar_Beam_Incident_Cos_handle)
            IncidentAngleRad = math.acos(Solar_Beam_Incident_Cos_handle)
            IncidentAngle = self.rad_to_deg(IncidentAngleRad)
            self.api.exchange.set_global_value(state, self.IncidentAngle_handle, IncidentAngle)

            Zone_Sensible_Cool_Rate = self.api.exchange.get_variable_value(state, self.Zone_Sensible_Cool_Rate_handle)
            shade_status_off = self.api.exchange.get_global_value(state, self.Shade_Status_Off_handle)
            shade_status_interior_blind_on = self.api.exchange.get_global_value(
                state, self.Shade_Status_Interior_Blind_On_handle)

            if IncidentAngle < 45.0:
                self.api.exchange.set_actuator_value(state, self.Zn001_Wall001_Win001_Shading_Deploy_Status_actuator_handle,
                                                     shade_status_interior_blind_on)
                shade_status_rpt = shade_status_interior_blind_on
            elif Zone_Sensible_Cool_Rate > 20.0:
                self.api.exchange.set_actuator_value(state, self.Zn001_Wall001_Win001_Shading_Deploy_Status_actuator_handle,
                                                     shade_status_interior_blind_on)
                shade_status_rpt = shade_status_interior_blind_on
            else:
                self.api.exchange.set_actuator_value(state, self.Zn001_Wall001_Win001_Shading_Deploy_Status_actuator_handle,
                                                     shade_status_off)
                shade_status_rpt = shade_status_off

            self.api.exchange.set_global_value(state, self.Zn001_Wall001_Win001_Shading_Deploy_Status_output_handle,
                                               shade_status_rpt)
            return 0
        else:
            return 0


class InitializeShadeControlFlags(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.need_to_get_handles = True
        self.Shade_Status_Off_handle = None
        self.Shade_Status_Interior_Blind_On_handle = None

    def on_begin_new_environment(self, state) -> int:

        # these are control flag values used inside EnergyPlus for window shades
        # EMS control of window shading devices involves setting the control values for shading control actuators with
        #  one of these values.  The variable names can be used or replaced, it is the whole number values that trigger
        #  changes in the modeling.
        #  Shades and Blinds are either fully on or fully off, partial positions require multiple windows.
        # the window shading control flag values follow
        #  -1: if window has no shading device
        #   0: if shading device is off
        #   1: if interior shade is on
        #   2: if glazing is switched to darker state
        #   3: if exterior shade is on
        #   6: if interior blind is on
        #   7: if exterior blind is on
        #   8: if between-glass shade is on
        #   9: if between-glass blind is on

        # check if API ready
        if self.api.exchange.api_data_fully_ready(state):
            if self.need_to_get_handles:
                self.Shade_Status_Off_handle = self.api.exchange.get_global_handle(state, "Shade_Status_Off")
                self.Shade_Status_Interior_Blind_On_handle = self.api.exchange.get_global_handle(
                    state, "Shade_Status_Interior_Blind_On")
                self.need_to_get_handles = False
            self.api.exchange.set_global_value(state, self.Shade_Status_Off_handle, 0.0)
            self.api.exchange.set_global_value(state, self.Shade_Status_Interior_Blind_On_handle, 6.0)
            return 0
        else:
            # API not ready, return
            return 0
