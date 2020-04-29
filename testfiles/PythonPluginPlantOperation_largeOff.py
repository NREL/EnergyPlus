from pyenergyplus.plugin import EnergyPlusPlugin

class Init_Chiller_Capacity_Values(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()
        self.need_to_get_handles = True

        self.totChilCap_handle = None
        self.Chil1_Cap_handle = None
        self.Chil2_Cap_handle = None

    def on_user_defined_component_model(self) -> int:

        # api is ready to execute
        if self.api.exchange.api_data_fully_ready():

            # get variable handles if needed
            if self.need_to_get_handles:

                self.totChilCap_handle = self.api.exchange.get_global_handle("totChilCap")

                self.Chil1_Cap_handle = self.api.exchange.get_internal_variable_handle(
                    "Chiller Nominal Capacity",
                    "CoolSys1 Chiller 1"
                )

                self.Chil2_Cap_handle = self.api.exchange.get_internal_variable_handle(
                    "Chiller Nominal Capacity",
                    "CoolSys1 Chiller 2"
                )

            # calculation
            totChilCap = self.api.exchange.get_global_value(self.totChilCap_handle)
            Chil1_Cap = self.api.exchange.get_internal_variable_value(self.Chil1_Cap_handle)
            Chil2_Cap = self.api.exchange.get_internal_variable_value(self.Chil2_Cap_handle)
            self.api.exchange.set_global_value(totChilCap, Chil1_Cap + Chil2_Cap)

            return 0

        else:
            # api not ready, return
            return 0

class Cooling_dispatch_Values(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()
        self.need_to_get_handles = True

        self.CoolSys1_LoopDmnd_handle = None

    def on_user_defined_component_model(self) -> int:

        # api is ready to execute
        if self.api.exchange.api_data_fully_ready():

            # get variable handles if needed

            # calculation

            return 0

        else:
            # api not ready, return
            return 0

class Tower_dispatch_Values(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()
        self.need_to_get_handles = True

        self.TowerWaterSys_LoopDmnd_handle = None
        self.Tower_Disptch_handle = None

    def on_user_defined_component_model(self) -> int:

        # api is ready to execute
        if self.api.exchange.api_data_fully_ready():

            # get variable handles if needed
            if self.need_to_get_handles:

                self.TowerWaterSys_LoopDmnd_handle = self.api.exchange.get_internal_variable_handle(
                    "Supply Side Current Demand Rate",
                    "TowerWaterSys Operation Scheme"
                )

                # print(f"\tTowerWaterSys_LoopDmnd_handle: {self.TowerWaterSys_LoopDmnd_handle}")

                self.Tower_Disptch_handle = self.api.exchange.get_actuator_handle(
                    "Plant Equipment Operation",
                    "Distributed Load Rate",
                    "TowerWaterSys Operation Scheme:TowerWaterSys CoolTower"
                )

            # calculation
            TowerWaterSys_LoopDmnd = self.api.exchange.get_internal_variable_value(self.TowerWaterSys_LoopDmnd_handle)
            if TowerWaterSys_LoopDmnd < 0.0:
                self.api.exchange.set_actuator_value(self.Tower_Disptch_handle, TowerWaterSys_LoopDmnd)
            else:
                self.api.exchange.set_actuator_value(self.Tower_Disptch_handle, 0.0)

            return 0

        else:
            # api not ready, return
            return 0
