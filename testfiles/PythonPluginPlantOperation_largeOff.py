from pyenergyplus.plugin import EnergyPlusPlugin


class DispatchAlgs(EnergyPlusPlugin):

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
