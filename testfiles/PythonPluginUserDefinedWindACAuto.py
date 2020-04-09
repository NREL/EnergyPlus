from pyenergyplus.plugin import EnergyPlusPlugin
import math


class Zone1WinACModel(EnergyPlusPlugin):

    def __init__(self):
        # init parent class
        super().__init__()

        # handles
        self.need_to_get_handles = True
        self.Zone1WinAC_PrimAir_Tinlet_handle = None
        self.Zone1WinAC_PrimAir_Winlet_handle = None
        self.Zone1WinAC_OA_Tdb_handle = None
        self.Zone1WinAC_OA_W_handle = None
        self.Zone1WinAC_Qdot_Request_handle = None
        self.Zone1_OADesign_Vdot_handle = None
        self.Zone1WinAC_OA_rho_handle = None
        self.Zone1_CoolDesign_Mdot_handle = None
        self.Zone1WinAC_PrimAir_Cp_handle = None
        self.Zone1_CoolDesign_Cap_handle = None
        self.Zone1Cooling_Tstat_handle = None
        self.COOLINGCOILAVAILSCHED_handle = None
        self.psych = self.api.functional.psychrometrics()

    def on_user_defined_component_model(self) -> int:

        # setup
        if self.need_to_get_handles and self.api.exchange.api_data_fully_ready():
            self.Zone1WinAC_PrimAir_Tinlet_handle = self.api.exchange.get_internal_variable_handle(
                "Inlet Temperature for Primary Air Connection",
                "Zone1WindAC")

            self.Zone1WinAC_PrimAir_Winlet_handle = self.api.exchange.get_internal_variable_handle(
                "Inlet Humidity Ratio for Primary Air Connection",
                "Zone1WindAC")

            self.Zone1WinAC_OA_Tdb_handle = self.api.exchange.get_internal_variable_value(
                "Inlet Temperature for Secondary Air Connection",
                "Zone1WindAC")

            self.Zone1WinAC_OA_W_handle = self.api.exchange.get_internal_variable_value(
                "Inlet Humidity Ratio for Secondary Air Connection",
                "Zone1WindAC")

            self.Zone1WinAC_Qdot_Request_handle = self.api.exchange.get_internal_variable_value(
                "Remaining Sensible Load to Cooling Setpoint",
                "Zone1WindAC")

            self.Zone1_OADesign_Vdot_handle = self.api.exchange.get_internal_variable_value(
                "Zone Outdoor Air Design Volume Flow Rate",
                "West Zone")

            self.Zone1WinAC_OA_rho_handle = self.api.exchange.get_internal_variable_value(
                "Inlet Density for Secondary Air Connection",
                "Zone1WindAC")

            self.Zone1_CoolDesign_Mdot_handle = self.api.exchange.get_internal_variable_value(
                "Final Zone Design Cooling Air Mass Flow Rate",
                "West Zone")

            self.Zone1WinAC_PrimAir_Cp_handle = self.api.exchange.get_internal_variable_value(
                "Inlet Specific Heat for Primary Air Connection",
                "Zone1WindAC")

            self.des_mdot_handle_2 = self.api.exchange.get_variable_handle(
                "Final Zone Design Cooling Load",
                "West Zone")

            self.Zone1Cooling_Tstat_handle = self.api.exchange.get_variable_handle(
                "Zone Thermostat Cooling Setpoint Temperature",
                "West Zone")

            self.COOLINGCOILAVAILSCHED_handle = self.api.exchange.get_variable_handle(
                "Schedule Value",
                "COOLINGCOILAVAILSCHED")

            self.OA_Press_handle = self.api.exchange.get_variable_handle(
                "Site Outdoor Air Barometric Pressure",
                "Environment"
            )

        else:
            return 0

        # init block
        Zone1WinAC_PrimAir_Tinlet = self.api.exchange.get_internal_variable_value(self.Zone1WinAC_PrimAir_Tinlet_handle)
        PrimAir_Tinlet = Zone1WinAC_PrimAir_Tinlet

        Zone1WinAC_PrimAir_Winlet = self.api.exchange.get_internal_variable_value(self.Zone1WinAC_PrimAir_Winlet_handle)
        PrimAir_Winlet = Zone1WinAC_PrimAir_Winlet

        Zone1WinAC_OA_Tdb = self.api.exchange.get_internal_variable_value(self.Zone1WinAC_OA_Tdb_handle)
        OA_Tdb = Zone1WinAC_OA_Tdb

        Zone1WinAC_OA_W = self.api.exchange.get_internal_variable_value(self.Zone1WinAC_OA_W_handle)
        OA_W = Zone1WinAC_OA_W

        Zone1WinAC_Qdot_Request = self.api.exchange.get_internal_variable_value(self.Zone1WinAC_Qdot_Request_handle)
        Qdot_Request = Zone1WinAC_Qdot_Request

        Zone1_OADesign_Vdot = self.api.exchange.get_internal_variable_value(self.Zone1_OADesign_Vdot_handle)
        Zone1WinAC_OA_rho = self.api.exchange.get_internal_variable_value(self.Zone1WinAC_OA_rho_handle)
        AirOAMdotDesign = Zone1_OADesign_Vdot * Zone1WinAC_OA_rho

        Zone1_CoolDesign_Mdot = self.api.exchange.get_internal_variable_value(self.Zone1_CoolDesign_Mdot_handle)
        AirSupMdotDesign = Zone1_CoolDesign_Mdot

        Zone1WinAC_PrimAir_Cp = self.api.exchange.get_internal_variable_value(self.Zone1WinAC_PrimAir_Cp_handle)
        AirCp = Zone1WinAC_PrimAir_Cp
        FanEff = 0.5
        FanDeltaP = 75.0
        Zone1_CoolDesign_Cap = self.api.exchange.get_internal_variable_value(self.Zone1_CoolDesign_Cap_handle)
        RatedCap = Zone1_CoolDesign_Cap / 0.75
        RatedEIR = 1.0 / 3.0
        Zone1Cooling_Tstat = self.api.exchange.get_variable_value(self.Zone1Cooling_Tstat_handle)
        ZoneCoolTstat = Zone1Cooling_Tstat

        # sim block
        QdotReq = 0.0
        if Qdot_Request < 0.0:
            QdotReq = Qdot_Request

        if self.api.exchange.get_variable_value(self.COOLINGCOILAVAILSCHED_handle) == 0.0:
            QdotReq = 0.0

        PrimAir_MdotIn = 0.0
        PrimAir_MdotOut = 0.0
        PrimAir_Tout = 0.0
        PrimAir_Wout = 0.0
        tot_cool_Power = 0.0
        ElectPower = 0.0
        ElectEnergy = 0.0
        OA_MdotIn = 0.0
        if QdotReq == 0.0:
            PrimAir_Tout = PrimAir_Tinlet
            PrimAir_Wout = PrimAir_Winlet
            tot_cool_Power = 0.0
            ElectPower = 0.0
            ElectEnergy = 0.0
            OA_MdotIn = 0.0
        else:
            Recirc_Mdot = AirSupMdotDesign - AirOAMdotDesign
            Recirc_H = self.psych.enthalpy(PrimAir_Tinlet, PrimAir_Winlet)
            OA_H = self.psych.enthalpy(OA_Tdb, OA_W)
            Mix_H = ((Recirc_Mdot * Recirc_H) + (AirOAMdotDesign * OA_H)) / AirSupMdotDesign
            Mix_W = ((Recirc_Mdot * PrimAir_Winlet) + (AirOAMdotDesign * OA_W)) / AirSupMdotDesign
            Mix_Tdb = self.psych.dry_bulb(Mix_H, Mix_W)
            Mix_Cp = self.psych.specific_heat(Mix_W)
            OA_Press = self.api.exchange.get_variable_value(self.OA_Press_handle)
            Mix_rho = self.psych.density(OA_Press, Mix_Tdb, Mix_W)
            FanPower = (AirSupMdotDesign * FanDeltaP) / (FanEff * Mix_rho)
            FanPowerToAir = FanPower
            FanDeltaH = FanPowerToAir / AirSupMdotDesign
            FanOut_H = Mix_H + FanDeltaH
            FanOut_W = Mix_W
            FanOut_Tdb = self.psych.dry_bulb(FanOut_H, FanOut_W)
            FanOut_Twb = self.psych.wet_bulb(FanOut_Tdb, FanOut_W, OA_Press)
            RatedCBF = 0.12666
            LogRatedCBF = math.log(RatedCBF)
            A0 = (0.0 - 1.0) * LogRatedCBF * AirSupMdotDesign
            ADiff = (0.0 - 1.0) * (A0 / AirSupMdotDesign)
            CBF = math.exp(ADiff)
            dummy = 0.0

        return 0
