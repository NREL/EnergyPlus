from pyenergyplus.plugin import EnergyPlusPlugin


class ManageDemand(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.do_setup = True
        self.need_to_get_handles = True
        self.CurntFacilityElectDemand_handle = None
        self.argTargetDemand_handle = None
        self.argCrntDmnd_handle = None
        self.argTrendDirection_handle = None
        self.argDmndMngrState_handle = None


    def on_begin_timestep_before_predictor(self) -> int:
        if self.do_setup:
            self.data["h_trend"] = self.api.exchange.get_trend_handle("Demand_Mgr_State_Trend") #argDmndMngrState
            self.data["h_trend"] = self.api.exchange.get_trend_handle("FacilityElectTrend") #CurntFacilityElectDemand
        # API is ready to execute
        if self.api.exchange.api_data_fully_ready():
            # get handles if needed
            if self.need_to_get_handles:
                self.CurntFacilityElectDemand_handle = self.api.exchange.get_variable_handle(
                    "Facility Total Electric Demand Power",
                    "Whole Building")

                self.argTargetDemand_handle = self.api.exchange.get_global_handle("argTargetDemand")
                
                self.argCrntDmnd_handle = self.api.exchange.get_global_handle("argCrntDmnd")

                self.argTrendDirection_handle= self.api.exchange.get_global_handle("argTrendDirection")

                self.argDmndMngrState_handle = self.api.exchange.get_global_handle("argDmndMngrState")

                self.need_to_get_handles = False

            # calculations

            # variables: localDemand CurrntTrend


            return 0
        else:
            # API not ready, return
            return 0

