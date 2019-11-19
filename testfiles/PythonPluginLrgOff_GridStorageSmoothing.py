from pyenergyplus.plugin import EnergyPlusPlugin


class CollectTrendVariable(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.handles_set = False
        self.handle_whole_building_demand_power = None
        self.handle_electric_storage_simple_charge_state = None

    def main(self) -> int:
        if not self.handles_set:
            self.handle_whole_building_demand_power = self.api.exchange.get_variable_handle(
                "Facility Total Electric Demand Power", "Whole Building"
            )
            self.handle_electric_storage_simple_charge_state = self.api.exchange.get_variable_handle(
                "Electric Storage Simple Charge State", "Battery"
            )
            self.handles_set = True
        # hour = self.api.exchange.hour()
        # if hour < 7:
        #     self.api.exchange.set_actuator_value(self.handle, 15)
        # elif hour < 18:
        #     self.api.exchange.set_actuator_value(self.handle, 23)
        # else:
        #     self.api.exchange.set_actuator_value(self.handle, 15)
        return 0
