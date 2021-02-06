from pyenergyplus.plugin import EnergyPlusPlugin


class ZN_1_wall_south_Window_1_Control(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()
        self.need_to_get_handles = True
        self.Win1_Tout_handle = None
        self.Win1_Construct_handle = None
        self.TCwindow_25_handle = None
        self.TCwindow_27_handle = None
        self.TCwindow_29_handle = None
        self.TCwindow_31_handle = None
        self.TCwindow_33_handle = None
        self.TCwindow_35_handle = None
        self.TCwindow_37_handle = None
        self.TCwindow_39_handle = None
        self.TCwindow_41_handle = None
        self.TCwindow_43_handle = None
        self.TCwindow_45_handle = None
        self.TCwindow_50_handle = None
        self.TCwindow_55_handle = None
        self.TCwindow_60_handle = None
        self.TCwindow_65_handle = None
        self.TCwindow_70_handle = None
        self.TCwindow_80_handle = None
        self.TCwindow_85_handle = None

    def on_begin_timestep_before_predictor(self, state) -> int:

        # api is ready to execute
        if self.api.exchange.api_data_fully_ready(state):

            # get variable handles if needed
            if self.need_to_get_handles:
                self.Win1_Tout_handle = self.api.exchange.get_variable_handle(
                    state,
                    "Surface Outside Face Temperature",
                    "Perimeter_ZN_1_wall_south_Window_1")

                self.Win1_Construct_handle = self.api.exchange.get_actuator_handle(state,
                                                                                   "Surface",
                                                                                   "Construction State",
                                                                                   "Perimeter_ZN_1_wall_south_Window_1")

                self.TCwindow_25_handle = self.api.exchange.get_construction_handle(state, "TCwindow_25")

                self.TCwindow_27_handle = self.api.exchange.get_construction_handle(state, "TCwindow_27")

                self.TCwindow_29_handle = self.api.exchange.get_construction_handle(state, "TCwindow_29")

                self.TCwindow_31_handle = self.api.exchange.get_construction_handle(state, "TCwindow_31")

                self.TCwindow_33_handle = self.api.exchange.get_construction_handle(state, "TCwindow_33")

                self.TCwindow_35_handle = self.api.exchange.get_construction_handle(state, "TCwindow_35")

                self.TCwindow_37_handle = self.api.exchange.get_construction_handle(state, "TCwindow_37")

                self.TCwindow_39_handle = self.api.exchange.get_construction_handle(state, "TCwindow_39")

                self.TCwindow_41_handle = self.api.exchange.get_construction_handle(state, "TCwindow_41")

                self.TCwindow_43_handle = self.api.exchange.get_construction_handle(state, "TCwindow_43")

                self.TCwindow_45_handle = self.api.exchange.get_construction_handle(state, "TCwindow_45")

                self.TCwindow_50_handle = self.api.exchange.get_construction_handle(state, "TCwindow_50")

                self.TCwindow_55_handle = self.api.exchange.get_construction_handle(state, "TCwindow_55")

                self.TCwindow_60_handle = self.api.exchange.get_construction_handle(state, "TCwindow_60")

                self.TCwindow_65_handle = self.api.exchange.get_construction_handle(state, "TCwindow_65")

                self.TCwindow_70_handle = self.api.exchange.get_construction_handle(state, "TCwindow_70")

                self.TCwindow_75_handle = self.api.exchange.get_construction_handle(state, "TCwindow_75")

                self.TCwindow_80_handle = self.api.exchange.get_construction_handle(state, "TCwindow_80")

                self.TCwindow_85_handle = self.api.exchange.get_construction_handle(state, "TCwindow_85")

                self.need_to_get_handles = False

            # calculate
            if self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 26.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_25_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 28.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_27_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 30.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_29_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 32.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_31_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 34.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_33_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 36.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_35_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 38.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_37_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 40.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_39_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 42.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_41_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 44.0:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_43_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 47.5:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_45_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 52.5:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_50_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 57.5:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_55_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 62.5:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_60_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 67.5:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_65_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 72.5:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_70_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 77.5:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_75_handle)
            elif self.api.exchange.get_variable_value(state, self.Win1_Tout_handle) <= 82.5:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_80_handle)
            else:
                self.api.exchange.set_actuator_value(state, self.Win1_Construct_handle, self.TCwindow_85_handle)

            return 0

        else:
            # api not ready, return
            return 0
