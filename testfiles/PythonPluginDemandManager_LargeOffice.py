from pyenergyplus.plugin import EnergyPlusPlugin


class ManageDemand(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.need_to_get_handles = True

    def on_begin_timestep_before_predictor(self) -> int:
        # API is ready to execute
        if self.api.exchange.api_data_fully_ready():
            # get handles if needed
            if self.need_to_get_handles:

                self.need_to_get_handles = False

            # calculations
            return 0
        else:
            # API not ready, return
            return 0
