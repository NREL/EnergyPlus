from pyenergyplus.plugin import EnergyPlusPlugin


class CondFDSurfaceManager(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.surf_cond_handle = None
        self.need_to_get_handles = True

    def get_handles(self, state):
        self.surf_cond_handle = self.api.exchange.get_actuator_handle(state,
                                                                      "CondFD Surface Material Layer",
                                                                      "Thermal Conductivity",
                                                                      "ZN001:WALL001:WallStudAndCavityk1")

        self.need_to_get_handles = False

    def on_begin_timestep_before_predictor(self, state) -> int:

        if self.need_to_get_handles:
            if self.api.exchange.cond_fd_ready(state) and self.api.exchange.api_data_fully_ready(state):
                self.get_handles(state)
            else:
                return 0

        print(self.surf_cond_handle)

        return 0
