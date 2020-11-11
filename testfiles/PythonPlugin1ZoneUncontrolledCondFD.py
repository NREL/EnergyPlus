from pyenergyplus.plugin import EnergyPlusPlugin


class Material(object):
    def __init__(self, name):
        self.name = name
        self.num_nodes = 0
        self.nodes = []
        self.cp_handle = None
        self.k_handle = None

    def set_num_nodes(self, num_nodes):
        self.num_nodes = num_nodes

    def set_cp_handle(self, handle):
        self.cp_handle = handle

    def set_k_handle(self, handle):
        self.k_handle = handle


class Construction(object):
    def __init__(self, name):
        self.name = name
        self.material_layers = []

    def set_material_layers(self, layers):
        for idx, l in enumerate(layers):
            self.material_layers.append(Material(l))


class CondFDSurfaceManager(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()

        wall_materials = ["Vinyl&#44 Light", "WallSheathing", "WallStudAndCavityk1", "Drywall 0.5 in."]
        surfaces_names = ["Zn001:Wall001", "Zn001:Wall002", "Zn001:Wall003", "Zn001:Wall004"]

        self.surfaces = []

        for surface_name in surfaces_names:
            constr = Construction(surface_name)
            constr.set_material_layers(wall_materials)
            self.surfaces.append(constr)

        self.zone_air_temp_handle = None
        self.outdoor_air_temp_handle = None

        self.need_to_get_handles = True

        # set flag to use EMS to modify k and cp
        self.use_ems = True

    def get_handles(self, state):

        # get air temperature handles
        self.zone_air_temp_handle = self.api.exchange.get_variable_handle(state,
                                                                          "Zone Mean Air Temperature",
                                                                          "Zone One")
        self.outdoor_air_temp_handle = self.api.exchange.get_variable_handle(state,
                                                                             "Site Outdoor Air Drybulb Temperature",
                                                                             "Environment")
        # get surface handles
        for surface in self.surfaces:
            for material in surface.material_layers:
                # set the total number of nodes in each material layer
                num_nodes = self.api.exchange.get_num_nodes_in_cond_fd_surf_layer(state, surface.name, material.name)
                material.set_num_nodes(num_nodes)

                # set the conductivity handle in each material layer
                k_handle = self.api.exchange.get_actuator_handle(state,
                                                                 "CondFD Surface Material Layer",
                                                                 "Thermal Conductivity",
                                                                 f"{surface.name}:{material.name}")
                material.set_k_handle(k_handle)

                # set the specific heat handle in each material layer
                k_handle = self.api.exchange.get_actuator_handle(state,
                                                                 "CondFD Surface Material Layer",
                                                                 "Specific Heat",
                                                                 f"{surface.name}:{material.name}")
                material.set_cp_handle(k_handle)

        self.need_to_get_handles = False

    def on_begin_timestep_before_predictor(self, state) -> int:
        if self.need_to_get_handles:
            if self.api.exchange.cond_fd_ready(state) and self.api.exchange.api_data_fully_ready(state):
                self.get_handles(state)
            else:
                return 0

        # return early if we're not using EMS
        if not self.use_ems:
            return 0

        # get temperatures
        setpoint = 20
        outdoor_air_temp = self.api.exchange.get_variable_value(state, self.outdoor_air_temp_handle)
        zone_air_temp = self.api.exchange.get_variable_value(state, self.zone_air_temp_handle)

        # check whether we need to actuate the walls
        actuate_walls = False

        # cooling condition
        if (zone_air_temp > setpoint) and (zone_air_temp > outdoor_air_temp):
            actuate_walls = True

        # heating condition
        if (zone_air_temp < setpoint) and (zone_air_temp < outdoor_air_temp):
            actuate_walls = True

        # update k and cp using EMS based on temperature conditions
        for surface in self.surfaces:
            for layer in surface.material_layers:
                if layer.name is "WallStudAndCavityk1":
                    if actuate_walls:
                        k = 10
                        cp = 1500
                    else:
                        k = 2.4
                        cp = 1179
                    self.api.exchange.set_actuator_value(state, layer.k_handle, k)
                    self.api.exchange.set_actuator_value(state, layer.cp_handle, cp)

        return 0
