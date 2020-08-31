from pyenergyplus.plugin import EnergyPlusPlugin


class TowerControl(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()
        self.need_to_get_handles = True
        self.outdoor_air_temp_handle = None
        self.chiller_condenser_loop_handle = None
        self.chiller_cond_pump_handle = None
        self.pump_override_report_handle = None

    def on_inside_hvac_system_iteration_loop(self, state) -> int:

        # api is ready to execute
        if self.api.exchange.api_data_fully_ready(state):

            # get variable handles if needed
            if self.need_to_get_handles:
                self.outdoor_air_temp_handle = self.api.exchange.get_variable_handle(
                    state,
                    "Site Outdoor Air Drybulb Temperature",
                    "Environment")

                self.chiller_condenser_loop_handle = self.api.exchange.get_actuator_handle(state,
                                                                                           "Plant Loop Overall",
                                                                                           "On/Off Supervisory",
                                                                                           "Chiller Plant Condenser Loop")

                self.chiller_cond_pump_handle = self.api.exchange.get_actuator_handle(state,
                                                                                      "Pump",
                                                                                      "Pump Mass Flow Rate",
                                                                                      "Chiller Plant Cnd Circ Pump")

                self.pump_override_report_handle = self.api.exchange.get_global_handle(state, "PumpFlowOverrideReport")

                self.need_to_get_handles = False

            # calculate
            if self.api.exchange.get_variable_value(state, self.outdoor_air_temp_handle) < 6.0:
                self.api.exchange.set_actuator_value(state, self.chiller_condenser_loop_handle, 0.0)
                self.api.exchange.set_actuator_value(state, self.chiller_cond_pump_handle, 0.0)
                self.api.exchange.set_global_value(state, self.pump_override_report_handle, 1.0)
            else:
                self.api.exchange.reset_actuator(state, self.chiller_condenser_loop_handle)
                self.api.exchange.reset_actuator(state, self.chiller_cond_pump_handle)
                self.api.exchange.set_global_value(state, self.pump_override_report_handle, 0.0)

            return 0

        else:
            # api not ready, return
            return 0
