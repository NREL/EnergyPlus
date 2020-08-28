from pyenergyplus.plugin import EnergyPlusPlugin


class HeatingDispatchValues(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()
        self.need_to_get_handles = True

        self.heat_sys_1_loop_demand_handle = None
        self.boiler_dispatch_handle = None

    def on_user_defined_component_model(self, state) -> int:

        # api is ready to execute
        if self.api.exchange.api_data_fully_ready(state):

            # get variable handles if needed
            if self.need_to_get_handles:
                self.heat_sys_1_loop_demand_handle = self.api.exchange.get_internal_variable_handle(
                    state,
                    "Supply Side Current Demand Rate",
                    "HeatSys1 Operation Scheme"
                )
                self.boiler_dispatch_handle = self.api.exchange.get_actuator_handle(
                    state,
                    "Plant Equipment Operation",
                    "Distributed Load Rate",
                    "HeatSys1 Operation Scheme:HeatSys1 Boiler"
                )
                self.need_to_get_handles = False

            # calculation
            heat_sys_1_loop_demand = self.api.exchange.get_internal_variable_value(state, self.heat_sys_1_loop_demand_handle)
            if heat_sys_1_loop_demand > 0.0:
                self.api.exchange.set_actuator_value(state, self.boiler_dispatch_handle, heat_sys_1_loop_demand)
            else:
                self.api.exchange.set_actuator_value(state, self.boiler_dispatch_handle, 0.0)

            return 0

        else:
            # api not ready, return
            return 0


class InitChillerCapacityAndCoolingDispatchValues(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()
        self.need_to_get_handles = True

        self.total_chiller_capacity_handle = None
        self.chiller_1_capacity_handle = None
        self.chiller_2_capacity_handle = None
        self.cool_sys_1_loop_demand_handle = None
        self.chiller_1_dispatch_handle = None
        self.chiller_2_dispatch_handle = None

    def on_user_defined_component_model(self, state) -> int:

        # api is ready to execute
        if self.api.exchange.api_data_fully_ready(state):

            # get variable handles if needed
            if self.need_to_get_handles:
                self.total_chiller_capacity_handle = self.api.exchange.get_global_handle(state, "totChilCap")
                self.chiller_1_capacity_handle = self.api.exchange.get_internal_variable_handle(
                    state,
                    "Chiller Nominal Capacity",
                    "CoolSys1 Chiller 1"
                )
                self.chiller_2_capacity_handle = self.api.exchange.get_internal_variable_handle(
                    state,
                    "Chiller Nominal Capacity",
                    "CoolSys1 Chiller 2"
                )
                self.cool_sys_1_loop_demand_handle = self.api.exchange.get_internal_variable_handle(
                    state,
                    "Supply Side Current Demand Rate",
                    "CoolSys1 Operation Scheme"
                )
                self.chiller_1_dispatch_handle = self.api.exchange.get_actuator_handle(
                    state,
                    "Plant Equipment Operation",
                    "Distributed Load Rate",
                    "CoolSys1 Operation Scheme:CoolSys1 Chiller 1"
                )
                self.chiller_2_dispatch_handle = self.api.exchange.get_actuator_handle(
                    state,
                    "Plant Equipment Operation",
                    "Distributed Load Rate",
                    "CoolSys1 Operation Scheme:CoolSys1 Chiller 2"
                )
                self.need_to_get_handles = False

            # calculation: Init_Chiller_Capacity_Values
            chiller_1_capacity = self.api.exchange.get_internal_variable_value(state, self.chiller_1_capacity_handle)
            chiller_2_capacity = self.api.exchange.get_internal_variable_value(state, self.chiller_2_capacity_handle)
            total_chiller_capacity = chiller_1_capacity + chiller_2_capacity
            self.api.exchange.set_global_value(state, self.total_chiller_capacity_handle, total_chiller_capacity)

            # calculation: Cooling_dispatch_Values
            cool_sys_1_loop_demand = self.api.exchange.get_internal_variable_value(state, self.cool_sys_1_loop_demand_handle)
            if cool_sys_1_loop_demand < 0.0:
                uniform_plr = min(cool_sys_1_loop_demand / total_chiller_capacity, 1.0)
                self.api.exchange.set_actuator_value(state, self.chiller_1_dispatch_handle, uniform_plr * chiller_1_capacity)
                self.api.exchange.set_actuator_value(state, self.chiller_2_dispatch_handle, uniform_plr * chiller_2_capacity)
            else:
                self.api.exchange.set_actuator_value(state, self.chiller_1_dispatch_handle, 0)
                self.api.exchange.set_actuator_value(state, self.chiller_2_dispatch_handle, 0)

            return 0

        else:
            # api not ready, return
            return 0


class TowerDispatchValues(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()
        self.need_to_get_handles = True

        self.tower_water_sys_loop_demand_handle = None
        self.tower_dispatch_handle = None

    def on_user_defined_component_model(self, state) -> int:

        # api is ready to execute
        if self.api.exchange.api_data_fully_ready(state):

            # get variable handles if needed
            if self.need_to_get_handles:
                self.tower_water_sys_loop_demand_handle = self.api.exchange.get_internal_variable_handle(
                    state,
                    "Supply Side Current Demand Rate",
                    "TowerWaterSys Operation Scheme"
                )
                self.tower_dispatch_handle = self.api.exchange.get_actuator_handle(
                    state,
                    "Plant Equipment Operation",
                    "Distributed Load Rate",
                    "TowerWaterSys Operation Scheme:TowerWaterSys CoolTower"
                )
                self.need_to_get_handles = False

            # calculation
            tower_water_sys_loop_demand = self.api.exchange.get_internal_variable_value(
                state, self.tower_water_sys_loop_demand_handle
            )
            if tower_water_sys_loop_demand < 0.0:
                self.api.exchange.set_actuator_value(state, self.tower_dispatch_handle, tower_water_sys_loop_demand)
            else:
                self.api.exchange.set_actuator_value(state, self.tower_dispatch_handle, 0.0)

            return 0

        else:
            # api not ready, return
            return 0
