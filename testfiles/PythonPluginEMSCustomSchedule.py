
from pyenergyplus.plugin import EnergyPlusPlugin


class HeatingSetPoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.handle = None

    def main(self) -> int:
        if not self.handle:
            self.handle = self.exchange.get_actuator_handle("Schedule Value", "HTGSETP_SCH")
        hour = self.exchange.hour()
        if hour < 7:
            self.exchange.set_actuator_value(self.handle, 15)
        elif hour < 18:
            self.exchange.set_actuator_value(self.handle, 23)
        else:
            self.exchange.set_actuator_value(self.handle, 15)
        return 0


class CoolingSetPoint(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.handle = None

    def main(self) -> int:
        if not self.handle:
            self.handle = self.exchange.get_actuator_handle("Schedule Value", "CLGSETP_SCH")
        hour = self.exchange.hour()
        if hour < 7:
            self.exchange.set_actuator_value(self.handle, 29)
        elif hour < 18:
            self.exchange.set_actuator_value(self.handle, 25)
        else:
            self.exchange.set_actuator_value(self.handle, 29)
        return 0
