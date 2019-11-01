
from pyenergyplus.plugin import EnergyPlusPlugin


class HeatingSetPoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.handle = None

    def main(self) -> int:
        if not self.handle:
            self.handle = self.api.exchange.get_actuator_handle("HTGSETP_SCH", "Schedule:Constant", "Schedule Value")
        hour = self.api.exchange.hour()
        if hour < 7:
            self.api.exchange.set_actuator_value(self.handle, 15)
        elif hour < 18:
            self.api.exchange.set_actuator_value(self.handle, 23)
        else:
            self.api.exchange.set_actuator_value(self.handle, 15)
        return 0


class CoolingSetPoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.handle = None

    def main(self) -> int:
        if not self.handle:
            self.handle = self.api.exchange.get_actuator_handle("CLGSETP_SCH", "Schedule:Constant", "Schedule Value")
        hour = self.api.exchange.hour()
        if hour < 7:
            self.api.exchange.set_actuator_value(self.handle, 29)
        elif hour < 18:
            self.api.exchange.set_actuator_value(self.handle, 25)
        else:
            self.api.exchange.set_actuator_value(self.handle, 29)
        return 0
