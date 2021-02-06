from pyenergyplus.plugin import EnergyPlusPlugin


class InitChiller(EnergyPlusPlugin):
    CWLoopExitTemp = 7.0
    CWLoopDeltaTemp = 4.0

    def __init__(self):
        # init parent class
        super().__init__()

        # get functional api glycol instance
        self.glycol = None

        # handles
        self.need_to_get_handles = True
        self.handles = {}

    def get_handles(self, state):
        self.handles["h_intvar_design_flow"] = self.api.exchange.get_internal_variable_handle(
            state,
            "Plant Design Volume Flow Rate",
            "Chilled Water Loop"
        )
        self.handles["h_act_chiller_design_flow"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Design Volume Flow Rate",
            "Central Chiller"
        )
        self.handles["h_act_chiller_min_flow"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Minimum Mass Flow Rate",
            "Central Chiller"
        )
        self.handles["h_act_mdot_max"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Maximum Mass Flow Rate",
            "Central Chiller"
        )
        self.handles["h_act_min_cap"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Minimum Loading Capacity",
            "Central Chiller"
        )
        self.handles["h_act_max_cap"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Maximum Loading Capacity",
            "Central Chiller"
        )
        self.handles["h_act_opt_cap"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Optimal Loading Capacity",
            "Central Chiller"
        )
        self.need_to_get_handles = False

    def handles_are_valid(self, state):
        handles_are_valid = True
        for (k, v) in self.handles.items():
            if v == -1:
                handles_are_valid = False
                print(k, v)
                self.api.runtime.issue_severe(state, f"Handle not found for '{k}'")
        return handles_are_valid

    def on_user_defined_component_model(self, state) -> int:
        if not self.glycol:
            self.glycol = self.api.functional.glycol(state, u"water")
        if self.need_to_get_handles:
            self.get_handles(state)
            if not self.handles_are_valid(state):
                return 1
        specific_heat = self.glycol.specific_heat(state, self.CWLoopExitTemp)
        density = self.glycol.density(state, self.CWLoopExitTemp)
        vol_flow_design = self.api.exchange.get_internal_variable_value(state, self.handles["h_intvar_design_flow"])
        self.api.exchange.set_actuator_value(state, self.handles["h_act_chiller_design_flow"], vol_flow_design)
        self.api.exchange.set_actuator_value(state, self.handles["h_act_chiller_min_flow"], 0.0)
        self.api.exchange.set_actuator_value(state, self.handles["h_act_mdot_max"], vol_flow_design * density)
        capacity = specific_heat * density * self.CWLoopDeltaTemp * vol_flow_design
        self.api.exchange.set_actuator_value(state, self.handles["h_act_min_cap"], 0.0)
        self.api.exchange.set_actuator_value(state, self.handles["h_act_max_cap"], capacity)
        self.api.exchange.set_actuator_value(state, self.handles["h_act_opt_cap"], 0.9 * capacity)
        return 0


class SimChiller(EnergyPlusPlugin):
    CWLoopExitTemp = 7.0

    def __init__(self):
        # init parent class
        super().__init__()

        self.glycol = None

        # handles
        self.need_to_get_handles = True
        self.handles = {}

    def get_handles(self, state):
        self.handles["h_act_mdot_max"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Maximum Mass Flow Rate",
            "Central Chiller"
        )
        self.handles["h_act_max_cap"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Maximum Loading Capacity",
            "Central Chiller"
        )
        self.handles["h_intvar_chiller_load"] = self.api.exchange.get_internal_variable_handle(
            state,
            "Load Request for Plant Connection 1",
            "Central Chiller"
        )
        self.handles["h_intvar_inlet_temp"] = self.api.exchange.get_internal_variable_handle(
            state,
            "Inlet Temperature for Plant Connection 1",
            "Central Chiller"
        )
        self.handles["h_intvar_inlet_mass_flow"] = self.api.exchange.get_internal_variable_handle(
            state,
            "Inlet Mass Flow Rate for Plant Connection 1",
            "Central Chiller"
        )
        self.handles["h_act_outlet_temp"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Outlet Temperature",
            "Central Chiller"
        )
        self.handles["h_act_mass_flow_request"] = self.api.exchange.get_actuator_handle(
            state,
            "Plant Connection 1",
            "Mass Flow Rate",
            "Central Chiller"
        )
        self.handles["h_global_chiller_energy"] = self.api.exchange.get_global_handle(
            state,
            "Chiller_ElectEnergy"
        )

        self.need_to_get_handles = False

    def handles_are_valid(self, state):
        handles_are_valid = True
        for (k, v) in self.handles.items():
            if v == -1:
                handles_are_valid = False
                print(k, v)
                self.api.runtime.issue_severe(state, f"Handle not found for '{k}'")
        return handles_are_valid

    def on_user_defined_component_model(self, state) -> int:
        if not self.glycol:
            self.glycol = self.api.functional.glycol(state, u"water")
        if self.need_to_get_handles:
            self.get_handles(state)
            if not self.handles_are_valid(state):
                return 1
        # get load, inlet temp, and mdot
        chiller_load = self.api.exchange.get_internal_variable_value(state, self.handles["h_intvar_chiller_load"])
        inlet_temp = self.api.exchange.get_internal_variable_value(state, self.handles["h_intvar_inlet_temp"])
        mass_flow_rate = self.api.exchange.get_internal_variable_value(state, self.handles["h_intvar_inlet_mass_flow"])

        # leave early if chiller is off
        if chiller_load > -100.0:
            # pass inlet temp through
            self.api.exchange.set_actuator_value(state, self.handles["h_act_outlet_temp"], inlet_temp)
            # set mass flow rate and electricity to 0
            self.api.exchange.set_actuator_value(state, self.handles["h_act_mass_flow_request"], 0.0)
            self.api.exchange.set_global_value(state, self.handles["h_global_chiller_energy"], 0.0)
            return 0

        # set chiller load
        abs_load = abs(self.api.exchange.get_internal_variable_value(state, self.handles["h_intvar_chiller_load"]))
        max_capacity = self.api.exchange.get_actuator_value(state, self.handles["h_act_max_cap"])
        if abs_load > max_capacity:
            q_act = max_capacity
        else:
            q_act = abs_load

        # set chiller mdot
        if mass_flow_rate == 0.0:
            m_dot_act = self.api.exchange.get_actuator_value(state, self.handles["h_act_mdot_max"])
            self.api.exchange.set_actuator_value(state, self.handles["h_act_mass_flow_request"], m_dot_act)
        else:
            m_dot_act = mass_flow_rate

        # set outlet temp
        specific_heat = self.glycol.specific_heat(state, self.CWLoopExitTemp)
        t_out = inlet_temp - (q_act / (m_dot_act * specific_heat))
        self.api.exchange.set_actuator_value(state, self.handles["h_act_outlet_temp"], t_out)

        # set chiller energy
        eir = 1 / 4.6
        power = q_act * eir
        energy = power * 3600.0 * self.api.exchange.system_time_step(state)
        self.api.exchange.set_global_value(state, self.handles["h_global_chiller_energy"], energy)
        return 0
