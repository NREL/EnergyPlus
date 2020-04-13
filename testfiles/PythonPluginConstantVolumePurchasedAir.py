from pyenergyplus.plugin import EnergyPlusPlugin


class Set_Purch_Air(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.need_to_get_handles = True

    def get_handles(self):
        if self.need_to_get_handles and self.api.exchange.api_data_fully_ready():

            self.need_to_get_handles = False
            return 0
        else:
            return 0

    def on_after_predictor_after_hvac_managers(self) -> int:
        self.get_handles()
        return 0


class Set_Shade_Control_State(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.need_to_get_handles = True

    def get_handles(self):
        if self.need_to_get_handles and self.api.exchange.api_data_fully_ready():

            self.need_to_get_handles = False
            return 0
        else:
            return 0

    def on_begin_timestep_before_predictor(self) -> int:
        self.get_handles()
        return 0


class InitializeShadeControlFlags(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.need_to_get_handles = True
        self.Shade_Status_None_handle = None
        self.Shade_Status_Off_handle = None
        self.Shade_Status_Interior_Shade_On_handle = None
        self.Shade_Status_Switchable_Dark_handle = None
        self.Shade_Status_Exterior_Shade_On_handle = None
        self.Shade_Status_Interior_Blind_On_handle = None
        self.Shade_Status_Exterior_Blind_On_handle = None
        self.Shade_Status_Between_Glass_Shade_On_handle = None
        self.Shade_Status_Between_Glass_Blind_On_handle = None

    def get_handles(self):
        if self.need_to_get_handles and self.api.exchange.api_data_fully_ready():
            self.Shade_Status_None_handle = self.api.exchange.get_global_handle(
                "Shade_Status_None")

            attr = "Shade_Status_None"
            print(f"{attr}: {getattr(self, attr)}")

            self.Shade_Status_Off_handle = self.api.exchange.get_global_handle(
                "Shade_Status_Off")

            self.Shade_Status_Interior_Shade_On_handle = self.api.exchange.get_global_handle(
                "Shade_Status_Interior_Shade_On")

            self.Shade_Status_Switchable_Dark_handle = self.api.exchange.get_global_handle(
                "Shade_Status_Switchable_Dark")

            self.Shade_Status_Exterior_Shade_On_handle = self.api.exchange.get_global_handle(
                "Shade_Status_Exterior_Shade_On")

            self.Shade_Status_Interior_Blind_On_handle = self.api.exchange.get_global_handle(
                "Shade_Status_Interior_Blind_On")

            self.Shade_Status_Exterior_Blind_On_handle = self.api.exchange.get_global_handle(
                "Shade_Status_Exterior_Blind_On")

            self.Shade_Status_Between_Glass_Shade_On_handle = self.api.exchange.get_global_handle(
                "Shade_Status_Between_Glass_Shade_On")

            self.Shade_Status_Between_Glass_Blind_On_handle = self.api.exchange.get_global_handle(
                "Shade_Status_Between_Glass_Blind_On")

            self.need_to_get_handles = False
            return 0
        else:
            return 0

    def on_begin_new_environment(self) -> int:
        self.get_handles()

        self.api.exchange.set_global_value(self.Shade_Status_None_handle, -1.0)
        self.api.exchange.set_global_value(self.Shade_Status_Off_handle, 0.0)
        self.api.exchange.set_global_value(self.Shade_Status_Interior_Shade_On_handle, 1.0)
        self.api.exchange.set_global_value(self.Shade_Status_Switchable_Dark_handle, 2.0)
        self.api.exchange.set_global_value(self.Shade_Status_Exterior_Shade_On_handle, 3.0)
        self.api.exchange.set_global_value(self.Shade_Status_Interior_Blind_On_handle, 6.0)
        self.api.exchange.set_global_value(self.Shade_Status_Exterior_Blind_On_handle, 7.0)
        self.api.exchange.set_global_value(self.Shade_Status_Between_Glass_Shade_On_handle, 8.0)
        self.api.exchange.set_global_value(self.Shade_Status_Between_Glass_Blind_On_handle, 9.0)

        return 0
