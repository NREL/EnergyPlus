from pyenergyplus.plugin import EnergyPlusPlugin


class BatteryControlDemandDemo(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()

        # handles
        self.need_to_get_handles = True
        self.handles = {}

    def get_handles(self):
        self.need_to_get_handles = False
        self.handles["CurntFacilityElectDemand"] = self.api.exchange.get_variable_handle(
            "Facility Total Electric Demand Power",
            "Whole Building"
        )
        self.handles["CurntFacilityElectDemandTrend"] = self.api.exchange.get_trend_handle(
            "CurntFacilityElectDemandTrend"
        )
        self.handles["dischargeRate"] = self.api.exchange.get_actuator_handle(
            "Electrical Storage",
            "Power Draw Rate",
            "Grid Battery Load Center"
        )
        self.handles["chargeRate"] = self.api.exchange.get_actuator_handle(
            "Electrical Storage",
            "Power Charge Rate",
            "Grid Battery Load Center"
        )

    def handles_are_valid(self):
        handles_are_valid = True
        for (k, v) in self.handles.items():
            if v == -1:
                handles_are_valid = False
                self.api.runtime.issue_severe(f"Handle not found '{k}'")
        return handles_are_valid

    def simulate(self):
        avgFacilDemand = self.api.exchange.get_trend_average(self.handles["CurntFacilityElectDemandTrend"], 1008)
        self.api.exchange.set_actuator_value(self.handles["dischargeRate"], 0.0)
        self.api.exchange.set_actuator_value(self.handles["chargeRate"], 0.0)
        dampenFactor = 0.8
        CurntFacilityElectDemand = self.api.exchange.get_variable_value(self.handles["CurntFacilityElectDemand"])
        if (CurntFacilityElectDemand > (avgFacilDemand * 1.05)):
            dischargeRate = (CurntFacilityElectDemand - avgFacilDemand) * dampenFactor
            self.api.exchange.set_actuator_value(self.handles["dischargeRate"], dischargeRate)
        elif (CurntFacilityElectDemand < (avgFacilDemand * 0.95)):
            chargeRate = (avgFacilDemand - CurntFacilityElectDemand) * dampenFactor
            self.api.exchange.set_actuator_value(self.handles["chargeRate"], chargeRate)

    def on_begin_zone_timestep_before_init_heat_balance(self) -> int:

        if self.need_to_get_handles:
            self.get_handles()
            if not self.handles_are_valid():
                return 1
        self.simulate()
        return 0
