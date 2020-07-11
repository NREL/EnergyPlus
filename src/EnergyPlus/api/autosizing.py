from ctypes import cdll, c_int, c_void_p
from pyenergyplus.common import RealEP


class HeatingAirflowUASizer:
    """
    This sizer class wraps the internal HeatingAirflowUASizer class
    """

    def __init__(self, api: cdll):
        self.api = api
        self.api.sizerHeatingAirflowUANew.argtypes = []
        self.api.sizerHeatingAirflowUANew.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForSingleDuctZoneTerminal.argtypes = [c_void_p, RealEP, RealEP]
        self.api.sizerHeatingAirflowUAInitializeForSingleDuctZoneTerminal.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForZoneInductionUnit.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.sizerHeatingAirflowUAInitializeForZoneInductionUnit.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForZoneFanCoil.argtypes = [c_void_p, RealEP, RealEP]
        self.api.sizerHeatingAirflowUAInitializeForZoneFanCoil.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForSystemOutdoorAir.argtypes = [c_void_p, RealEP, c_int]
        self.api.sizerHeatingAirflowUAInitializeForSystemOutdoorAir.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForSystemMainDuct.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.sizerHeatingAirflowUAInitializeForSystemMainDuct.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForSystemCoolingDuct.argtypes = [c_void_p, RealEP]
        self.api.sizerHeatingAirflowUAInitializeForSystemCoolingDuct.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForSystemHeatingDuct.argtypes = [c_void_p, RealEP]
        self.api.sizerHeatingAirflowUAInitializeForSystemHeatingDuct.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForSystemOtherDuct.argtypes = [c_void_p, RealEP]
        self.api.sizerHeatingAirflowUAInitializeForSystemOtherDuct.restype = c_void_p
        self.api.sizerHeatingAirflowUADelete.argtypes = [c_void_p]
        self.api.sizerHeatingAirflowUADelete.restype = c_void_p
        self.api.sizerHeatingAirflowUACalculate.argtypes = [c_void_p]
        self.api.sizerHeatingAirflowUACalculate.restype = int
        self.api.sizerHeatingAirflowUAValue.argtypes = [c_void_p]
        self.api.sizerHeatingAirflowUAValue.restype = RealEP
        self.instance = self.api.sizerHeatingAirflowUANew()

    def __del__(self):
        self.api.sizerHeatingAirflowUADelete(self.instance)

    def initialize(self, elevation: float = 0.0) -> None:
        """
        Performs initialization of the sizer, eventually it will have lots of args

        :param elevation: Elevation above sea level for evaluating fluid properties, in meters, default zero
        :return: Nothing
        """
        self.api.sizerHeatingAirflowUAInitialize(self.instance, elevation)

    def initialize_for_zone_terminal_single_duct(self, elevation: float, main_flow_rate: float) -> None:
        self.api.sizerHeatingAirflowUAInitializeForSingleDuctZoneTerminal(self.instance, elevation, main_flow_rate)

    def initialize_for_zone_terminal_induction_unit(self, elevation: float, main_flow_rate: float, reheat_mult: float) -> None:
        self.api.sizerHeatingAirflowUAInitializeForZoneInductionUnit(self.instance, elevation, main_flow_rate, reheat_mult)

    def initialize_for_zone_terminal_fan_coil(self, elevation: float, design_heat_volume_flow_rate: float) -> None:
        self.api.sizerHeatingAirflowUAInitializeForZoneFanCoil(self.instance, elevation, design_heat_volume_flow_rate)

    def initialize_for_system_outdoor_air(self, elevation: float, overall_system_mass_flow: float, doas: bool) -> None:
        self.api.sizerHeatingAirflowUAInitializeForSystemOutdoorAir(self.instance, elevation, overall_system_mass_flow, 1 if doas else 0)

    def initialize_for_system_main_duct(self, elevation: float, overall_system_vol_flow: float, min_flow_rate_ratio: float) -> None:
        self.api.sizerHeatingAirflowUAInitializeForSystemMainDuct(self.instance, elevation, overall_system_vol_flow, min_flow_rate_ratio)

    def initialize_for_system_cooling_duct(self, elevation: float) -> None:
        self.api.sizerHeatingAirflowUAInitializeForSystemCoolingDuct(self.instance, elevation)

    def initialize_for_system_heating_duct(self, elevation: float) -> None:
        self.api.sizerHeatingAirflowUAInitializeForSystemHeatingDuct(self.instance, elevation)

    def initialize_for_system_other(self, elevation: float) -> None:
        self.api.sizerHeatingAirflowUAInitializeForSystemOtherDuct(self.instance, elevation)

    def calculate(self) -> bool:
        """
        Performs autosizing calculations with the given initialized values

        :return: True if the sizing was successful, or False if not
        """
        return True if self.api.sizerHeatingAirflowUACalculate(self.instance) == 0 else False

    def autosized_value(self) -> float:
        """
        Returns the autosized value, assuming the calculation was successful

        :return: The autosized value
        """
        return self.api.sizerHeatingAirflowUAValue(self.instance)


class Autosizing:
    """
    A wrapper class for all the autosizing classes, acting as a factory
    """

    def __init__(self, api: cdll):
        self.api = api

    def heating_airflow_ua_sizer(self) -> HeatingAirflowUASizer:
        return HeatingAirflowUASizer(self.api)
