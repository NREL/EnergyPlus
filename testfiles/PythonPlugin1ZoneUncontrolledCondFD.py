from pyenergyplus.plugin import EnergyPlusPlugin


class CondFDSurfaceManager(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.cond_fd_ready = False
        self.api_ready = False

    def on_begin_timestep_before_predictor(self, state) -> int:
        # check if API ready
        if self.api.exchange.cond_fd_ready(state):
            self.cond_fd_ready = True

        if self.api.exchange.api_data_fully_ready(state):
            self.api_ready = True

        return 0
