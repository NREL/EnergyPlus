from pyenergyplus.plugin import EnergyPlusPlugin


class CondFDSurfaceManager(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.cond_fd_ready = False
        self.surf_cond_handle = None

    def on_begin_timestep_before_predictor(self, state) -> int:
        # check if API ready
        if self.api.exchange.cond_fd_ready(state):
            self.surf_cond_handle = self.api.exchange.get_actuator_handle(state,
                                                                          "CondFD Surface Material Layer",
                                                                          "Thermal Conductivity",
                                                                          "ZN001:WALL001:WallStudAndCavityk1")

            self.cond_fd_ready = True
        else:
            return 0

        print(self.surf_cond_handle)

        return 0
