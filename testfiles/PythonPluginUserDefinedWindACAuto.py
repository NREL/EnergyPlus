import sys
from pyenergyplus.plugin import EnergyPlusPlugin


class Zone1WinACModel(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()

        # handles
        self.need_to_get_handles = True
        self.prim_air_inlet_temp_handle = None
        self.prim_air_inlet_hum_handle = None
        self.outdoor_air_drybulb_handle = None
        self.outdoor_air_hum_handle = None
        self.sens_q_request_handle = None
        self.oa_des_flow_handle = None
        self.oa_rho_handle = None
        self.des_mdot_handle = None
        self.prim_air_cp_handle = None
        self.des_cool_cap_handle = None
        self.cool_setpoint_temp_handle = None
        self.cool_coil_avail_sched_handle = None

    def on_user_defined_component_model(self) -> int:

        # setup
        if self.need_to_get_handles and self.api.exchange.api_data_fully_ready():
            self.prim_air_inlet_temp_handle = self.api.exchange.get_internal_variable_handle(
                "Inlet Temperature for Primary Air Connection",
                "Zone1WindAC")

            self.prim_air_inlet_hum_handle = self.api.exchange.get_internal_variable_handle(
                "Inlet Humidity Ratio for Primary Air Connection",
                "Zone1WindAC")

            self.outdoor_air_drybulb_handle = self.api.exchange.get_internal_variable_value(
                "Inlet Temperature for Secondary Air Connection",
                "Zone1WindAC")

            self.outdoor_air_hum_handle = self.api.exchange.get_internal_variable_value(
                "Inlet Humidity Ratio for Secondary Air Connection",
                "Zone1WindAC")

            self.sens_q_request_handle = self.api.exchange.get_internal_variable_value(
                "Remaining Sensible Load to Cooling Setpoint",
                "Zone1WindAC")

            self.oa_des_flow_handle = self.api.exchange.get_internal_variable_value(
                "Zone Outdoor Air Design Volume Flow Rate",
                "West Zone")

            self.oa_rho_handle = self.api.exchange.get_internal_variable_value(
                "Inlet Density for Secondary Air Connection",
                "Zone1WindAC")

            self.des_mdot_handle = self.api.exchange.get_internal_variable_value(
                "Final Zone Design Cooling Air Mass Flow Rate",
                "West Zone")

            self.prim_air_cp_handle = self.api.exchange.get_internal_variable_value(
                "Inlet Specific Heat for Primary Air Connection",
                "Zone1WindAC")

            self.des_mdot_handle = self.api.exchange.get_variable_handle(
                "Final Zone Design Cooling Load",
                "West Zone")

            self.cool_setpoint_temp_handle = self.api.exchange.get_variable_handle(
                "Zone Thermostat Cooling Setpoint Temperature",
                "West Zone")

            self.cool_coil_avail_sched_handle = self.api.exchange.get_variable_handle(
                "Schedule Value",
                "COOLINGCOILAVAILSCHED")

        else:
            return 0

        # init block
        prim_air_inlet_temp = self.api.exchange.get_internal_variable_value(self.prim_air_inlet_temp_handle)
        prim_air_inlet_hum = self.api.exchange.get_internal_variable_value(self.prim_air_inlet_hum_handle)
        outdoor_air_drybulb = self.api.exchange.get_internal_variable_value(self.outdoor_air_drybulb_handle)
        outdoor_air_hum = self.api.exchange.get_internal_variable_value(self.outdoor_air_hum_handle)
        sens_q_request = self.api.exchange.get_internal_variable_value(self.sens_q_request_handle)
        oa_des_flow = self.api.exchange.get_internal_variable_value(self.oa_des_flow_handle)
        oa_rho = self.api.exchange.get_internal_variable_value(self.oa_rho_handle)
        des_mdot = self.api.exchange.get_internal_variable_value(self.des_mdot_handle)
        prim_air_cp = self.api.exchange.get_internal_variable_value(self.prim_air_cp_handle)
        fan_eff = 0.5
        fan_delta_p = 75.0
        des_cool_cap = self.api.exchange.get_internal_variable_value(self.des_cool_cap_handle)
        rated_cap = des_cool_cap / 0.75
        rated_eir = 1.0 / 3.0
        cool_setpoint_temp = self.api.exchange.get_variable_value(self.cool_setpoint_temp_handle)

        # sim block
        q_dot_req = 0.0
        if sens_q_request < 0.0:
            q_dot_req = sens_q_request

        if self.api.exchange.get_variable_value(self.cool_coil_avail_sched_handle) == 0.0:
            q_dot_req = 0.0

        prim_air_mdot_in = 0.0
        prim_air_mdot_out = 0.0
        prim_air_outlet_temp = 0.0
        prim_air_outlet_hum = 0.0
        tot_cool_power = 0.0
        elect_power = 0.0
        elect_energy = 0.0
        oa_mdot_in = 0.0
        if q_dot_req == 0.0:
            prim_air_outlet_temp = prim_air_inlet_temp
            prim_air_outlet_hum = prim_air_inlet_hum
            tot_cool_power = 0.0
            elect_power = 0.0
            elect_energy = 0.0
            oa_mdot_in = 0.0
        else:

        return 0
