from pyenergyplus.plugin import EnergyPlusPlugin


class UserDefinedCoilInit(EnergyPlusPlugin):

    def __init(self):
        super().__init()

    def on_user_defined_component_model(self) -> int:
        # functional api glycol instance
        glycol = self.api.functional.glycol(u"water")

        # chilled water loop set points
        # local scope only
        cw_loop_exit_temp = 7.0
        cw_loop_delta_temp = 4.0

        # get some properties
        cw_cp = glycol.specific_heat(cw_loop_exit_temp)
        cw_rho = glycol.density(cw_loop_exit_temp)

        # get chilled water loop design vdot
        cw_loop_vdot_design_handle = self.api.exchange.get_internal_variable_handle("Chilled Water Loop",
                                                                                    "Plant Design Volume Flow Rate")
        cw_loop_vdot_design = self.api.exchange.get_internal_variable_value(cw_loop_vdot_design_handle)

        # set chiller design vdot
        chiller_vdot_design_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                           "Plant Connection 1",
                                                                           "Design Volume Flow Rate")
        self.api.exchange.set_actuator_value(chiller_vdot_design_handle, cw_loop_vdot_design)

        # set chiller minimum mdot
        chiller_mdot_min_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                        "Plant Connection 1",
                                                                        "Minimum Mass Flow Rate")
        chiller_mdot_min = 0.0
        self.api.exchange.set_actuator_value(chiller_mdot_min_handle, chiller_mdot_min)

        # set chiller maximum mdot
        chiller_mdot_max_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                        "Plant Connection 1",
                                                                        "Maximum Mass Flow Rate")
        chiller_mdot_max = cw_loop_vdot_design * cw_rho
        self.api.exchange.set_actuator_value(chiller_mdot_max_handle, chiller_mdot_max)

        # reference chiller capacity - local scope
        chiller_cap = cw_cp * cw_rho * cw_loop_delta_temp * cw_loop_vdot_design

        # set minimum chiller capacity
        chiller_cap_min_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                       "Plant Connection 1",
                                                                       "Minimum Loading Capacity")
        chiller_cap_min = 0.0
        self.api.exchange.set_actuator_value(chiller_cap_min_handle, chiller_cap_min)

        # set maximum chiller capacity
        chiller_cap_max_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                       "Plant Connection 1",
                                                                       "Maximum Loading Capacity")
        chiller_cap_max = chiller_cap
        self.api.exchange.set_actuator_value(chiller_cap_max_handle, chiller_cap_max)

        # set optimal chiller capacity
        chiller_cap_opt_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                       "Plant Connection 1",
                                                                       "Optimal Loading Capacity")
        chiller_cap_opt = 0.9 * chiller_cap
        self.api.exchange.set_actuator_value(chiller_cap_opt_handle, chiller_cap_opt)
        return 0
