from pyenergyplus.plugin import EnergyPlusPlugin


class CondFDSurfaceManager(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.surf_cond_handle = None
        self.surf_node_handle = None
        self.num_nodes = None
        self.need_to_get_handles = True
        self.node_temp = None

    def get_handles(self, state):
        self.surf_cond_handle = self.api.exchange.get_actuator_handle(state,
                                                                      "CondFD Surface Material Layer",
                                                                      "Thermal Conductivity",
                                                                      "ZN001:WALL001:WallStudAndCavityk1")

        self.surf_node_handle = self.api.exchange.get_internal_variable_handle(state,
                                                                               "CondFD Node Temperature",
                                                                               "ZN001:WALL001:WallStudAndCavityk1:1")

        self.num_nodes = self.api.exchange.get_num_nodes_in_cond_fd_surf_layer(state,
                                                                               "ZN001:WALL001",
                                                                               "WallStudAndCavityk1")

        self.node_temp = self.api.exchange.get_internal_variable_value(state, self.surf_node_handle)

        self.need_to_get_handles = False

        print(f"Layer Handle: {self.surf_cond_handle}")
        print(f"Node Handle: {self.surf_node_handle}")
        print(f"Nodes in layer: {self.num_nodes}")
        print(f"Node temp: {self.node_temp}")

    def on_begin_timestep_before_predictor(self, state) -> int:

        if self.need_to_get_handles:
            if self.api.exchange.cond_fd_ready(state) and self.api.exchange.api_data_fully_ready(state):
                self.get_handles(state)
            else:
                return 0

        return 0
