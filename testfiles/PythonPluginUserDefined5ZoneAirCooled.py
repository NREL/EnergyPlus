from pyenergyplus.plugin import EnergyPlusPlugin


class UserDefinedCoilInit(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()

        # get functional api glycol instance
        self.glycol = self.api.functional.glycol(u"water")

    def on_user_defined_component_model(self) -> int:
        # loop set points
        # local scope only
        cw_loop_exit_temp = 7.0
        cw_loop_delta_temp = 4.0

        # get some properties
        cw_cp = self.glycol.specific_heat(cw_loop_exit_temp)
        cw_rho = self.glycol.density(cw_loop_exit_temp)

        # get design vdot
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


class UserDefinedCoilSim(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()

        # get functional api glycol instance
        self.glycol = self.api.functional.glycol(u"water")

    def on_user_defined_component_model(self) -> int:
        # get loop load request
        cw_load_request_handle = self.api.exchange.get_internal_variable_handle("Central Chiller",
                                                                                "Load Request for Plant Connection 1")
        cw_load = self.api.exchange.get_internal_variable_value(cw_load_request_handle)

        # get loop inlet temp
        cw_inlet_temp_handle = self.api.exchange.get_internal_variable_handle("Central Chiller",
                                                                              "Inlet Temperature for "
                                                                              "Plant Connection 1")
        cw_inlet_temp = self.api.exchange.get_internal_variable_value(cw_inlet_temp_handle)

        # get loop mdot
        cw_mdot_handle = self.api.exchange.get_internal_variable_handle("Central Chiller",
                                                                        "Inlet Mass Flow Rate for Plant Connection 1")
        cw_mdot = self.api.exchange.get_internal_variable_value(cw_mdot_handle)

        # get loop outlet temp actuator handle
        cw_outlet_temp_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                      "Plant Connection 1",
                                                                      "Outlet Temperature")

        # get loop mdot request actuator handle
        cw_mdot_request_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                       "Plant Connection 1",
                                                                       "Mass Flow Rate")

        # get chiller electric energy actuator handle
        chiller_elect_energy_handle = self.api.exchange.get_global_handle("Chiller_ElectEnergy")

        # chiller is off
        if cw_load > -100.0:
            # pass inlet temp through
            self.api.exchange.set_actuator_value(cw_outlet_temp_handle, cw_inlet_temp)

            # set mass flow rate and electricity to 0
            self.api.exchange.set_actuator_value(cw_mdot_request_handle, 0.0)
            self.api.exchange.set_global_value(chiller_elect_energy_handle, 0.0)

            return 0

        # set chiller load
        chiller_cap_max_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                       "Plant Connection 1",
                                                                       "Maximum Loading Capacity")
        chiller_cap_max = self.api.exchange.get_variable_value(chiller_cap_max_handle)

        chiller_load = 0.0
        if abs(cw_load) > chiller_cap_max:
            chiller_load = chiller_cap_max
        else:
            chiller_load = abs(cw_load)

        # set chiller mdot
        chiller_mdot = 0.0
        if cw_mdot == 0.0:
            chiller_mdot_max_handle = self.api.exchange.get_actuator_handle("Central Chiller",
                                                                            "Plant Connection 1",
                                                                            "Maximum Mass Flow Rate")
            chiller_mdot_max = self.api.exchange.get_variable_value(chiller_mdot_max_handle)
            chiller_mdot = chiller_mdot_max
        else:
            chiller_mdot = cw_mdot

        # set outlet temp
        cw_cp = self.glycol.specific_heat(cw_inlet_temp)
        cw_outlet_temp = cw_inlet_temp - (chiller_load / (chiller_mdot * cw_cp))
        self.api.exchange.set_actuator_value(cw_outlet_temp_handle, cw_outlet_temp)

        # set chiller energy
        chiller_power = chiller_load / 4.6
        chiller_elect_energy = chiller_power * 3600.0 * self.api.exchange.system_time_step()
        self.api.exchange.set_global_value(chiller_elect_energy_handle, chiller_elect_energy)

        return 0
