from pyenergyplus.plugin import EnergyPlusPlugin


class BatteryControlDemandDemo(EnergyPlusPlugin):

    def get_handles(self):
        self.data['current_demand'] = self.api.exchange.get_variable_handle(
            "Facility Total Electric Demand Power", "Whole Building"
        )
        self.data["trend"] = self.api.exchange.get_trend_handle("CurntFacilityElectDemandTrend")
        self.data["var"] = self.api.exchange.get_global_handle("CurntFacilityElectDemand")
        self.data["discharge_rate"] = self.api.exchange.get_actuator_handle(
            "Electrical Storage", "Power Draw Rate", "Grid Battery Load Center"
        )
        self.data["charge_rate"] = self.api.exchange.get_actuator_handle(
            "Electrical Storage", "Power Charge Rate", "Grid Battery Load Center"
        )

    def handles_are_valid(self):
        handles = [
            self.data['current_demand'], self.data["trend"],
            self.data["discharge_rate"], self.data["charge_rate"],
            self.data["var"]
        ]
        return all([x > -1 for x in handles])

    def simulate(self):
        avg_facility_demand = self.api.exchange.get_trend_average(self.data["trend"], 1008)
        self.api.exchange.set_actuator_value(self.data["discharge_rate"], 0.0)
        self.api.exchange.set_actuator_value(self.data["charge_rate"], 0.0)
        dampen_factor = 0.8
        current_facility_elect_demand = self.api.exchange.get_variable_value(self.data["current_demand"])
        if current_facility_elect_demand > (avg_facility_demand * 1.05):
            discharge_rate = (current_facility_elect_demand - avg_facility_demand) * dampen_factor
            self.api.exchange.set_actuator_value(self.data["discharge_rate"], discharge_rate)
        elif current_facility_elect_demand < (avg_facility_demand * 0.95):
            charge_rate = (avg_facility_demand - current_facility_elect_demand) * dampen_factor
            self.api.exchange.set_actuator_value(self.data["charge_rate"], charge_rate)
        self.api.exchange.set_global_value(self.data['var'], current_facility_elect_demand)

    def on_begin_zone_timestep_before_init_heat_balance(self) -> int:
        if 'current_demand' not in self.data or self.data['current_demand'] == -1:
            if not self.api.exchange.api_data_fully_ready():
                return 0
            self.get_handles()
            if not self.handles_are_valid():
                return 1
        self.simulate()
        return 0
