from pyenergyplus.plugin import EnergyPlusPlugin


class CurveOverwriteMGR(EnergyPlusPlugin):

    def __init__(self):
        super().__init__()
        self.need_to_get_handles = True
        self.CoilInletDBT_handle = None
        self.CoilInletW_handle = None
        self.Pressure_handle = None
        self.OAT_handle = None
        self.CurveOverwrite_handle = None

        self.psych = self.api.functional.psychrometrics()

    def on_end_of_zone_timestep_before_zone_reporting(self) -> int:
        # API is ready to execute
        if self.api.exchange.api_data_fully_ready():
            # get handles if needed
            if self.need_to_get_handles:
                self.CoilInletDBT_handle = self.api.exchange.get_variable_handle("System Node Temperature",
                                                                                 "Zone1PTHPFanOutletNode")

                self.CoilInletW_handle = self.api.exchange.get_variable_handle("System Node Humidity Ratio",
                                                                               "Zone1PTHPFanOutletNode")

                self.Pressure_handle = self.api.exchange.get_variable_handle("System Node Pressure",
                                                                             "Zone1PTHPOAInNode")

                self.CurveOverwrite_handle = self.api.exchange.get_actuator_handle("Curve",
                                                                                   "Curve Result",
                                                                                   "HPACCOOLCAPFT")

                self.OAT_handle = self.api.exchange.get_variable_handle("System Node Temperature",
                                                                        "Zone1PTHPOAInNode")

                self.need_to_get_handles = False

            # calculations
            TTmp = self.api.exchange.get_variable_value(self.CoilInletDBT_handle)
            WTmp = self.api.exchange.get_variable_value(self.CoilInletW_handle)
            PTmp = self.api.exchange.get_variable_value(self.Pressure_handle)
            MyWB = self.psych.wet_bulb(TTmp, WTmp, PTmp)
            IVOnea = MyWB
            OAT = self.api.exchange.get_variable_value(self.OAT_handle)
            IVTwo = OAT
            IVThree = IVOnea * IVTwo
            C1 = 0.942567793
            C2 = 0.009543347
            C2a = 0.009543347
            C3 = 0.00068377E0
            C4 = 0.011042676
            C5 = 0.000005249
            C6 = 0.000009720
            CurveInput = C1 + (C2 * IVOnea) + (C3 * IVOnea * IVOnea) - (C4 * IVTwo) + (C5 * IVTwo * IVTwo) - (
                    C6 * IVThree)
            if OAT > 31.0:
                CurveInput = C1 - (C2a * IVOnea) + (C3 * IVOnea * IVOnea) - (C4 * IVTwo) + (C5 * IVTwo * IVTwo) - (
                        C6 * IVThree)

            CurveOverwrite = CurveInput
            self.api.exchange.set_actuator_value(self.CurveOverwrite_handle, CurveOverwrite)

            return 0
        else:
            # API not ready, return
            return 0
