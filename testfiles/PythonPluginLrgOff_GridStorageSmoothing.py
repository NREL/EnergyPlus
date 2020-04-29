from pyenergyplus.plugin import EnergyPlusPlugin


class BatteryControlDemandDemo(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()

        # handles
        self.need_to_get_handles = True
        # self.CurntStorageSOC_handle = None
        # self.MaxSOC_handle = None
        self.CurntFacilityElectDemand_handle = None
        self.CurntFacilityElectDemandTrend_handle = None
        self.dischargeRate_handle = None
        self.chargeRate_handle = None

    def get_handles(self):
        self.need_to_get_handles = False

        # self.CurntStorageSOC_handle = self.api.exchange.get_variable_handle(
        #     "Electric Storage Simple Charge State",
        #     "Battery"
        # )
        # self.MaxSOC_handle = self.api.exchange.get_internal_variable_handle(
        #     "Electrical Storage Simple Maximum Capacity",
        #     "Battery"
        # )
        self.CurntFacilityElectDemand_handle = self.api.exchange.get_variable_handle(
            "Facility Total Electric Demand Power",
            "Whole Building"
        )
        self.CurntFacilityElectDemandTrend_handle = self.api.exchange.get_trend_handle(
            "CurntFacilityElectDemandTrend"
        )
        self.dischargeRate_handle = self.api.exchange.get_actuator_handle(
            "Electrical Storage",
            "Power Draw Rate",
            "Grid Battery Load Center"
        )
        self.chargeRate_handle = self.api.exchange.get_actuator_handle(
            "Electrical Storage",
            "Power Charge Rate",
            "Grid Battery Load Center"
        )

    def simulate(self):
        # CurntStorageSOC = self.api.exchange.get_variable_value(self.CurntStorageSOC_handle)
        # MaxSOC = self.api.exchange.get_internal_variable_value(self.MaxSOC_handle)
        # fracSOC = CurntStorageSOC / MaxSOC
        avgFacilDemand = self.api.exchange.get_trend_average(self.CurntFacilityElectDemandTrend_handle, 1008)
        self.api.exchange.set_actuator_value(self.dischargeRate_handle, 0.0)
        self.api.exchange.set_actuator_value(self.chargeRate_handle, 0.0)
        dampenFactor = 0.8
        CurntFacilityElectDemand = self.api.exchange.get_variable_value(self.CurntFacilityElectDemand_handle)
        if (CurntFacilityElectDemand > (avgFacilDemand * 1.05)):
            dischargeRate = (CurntFacilityElectDemand - avgFacilDemand) * dampenFactor
            self.api.exchange.set_actuator_value(self.dischargeRate_handle, dischargeRate)
        elif (CurntFacilityElectDemand < (avgFacilDemand * 0.95)):
            chargeRate = (avgFacilDemand - CurntFacilityElectDemand) * dampenFactor
            self.api.exchange.set_actuator_value(self.chargeRate_handle, chargeRate)

    def on_begin_zone_timestep_before_init_heat_balance(self) -> int:
        if self.api.exchange.api_data_fully_ready():
            if self.need_to_get_handles:
                self.get_handles()
            self.simulate()
            return 0
        else:
            return 0
