from ctypes import cdll, c_char_p, c_int, c_void_p
from pyenergyplus.common import RealEP


class BaseSizerWorker:
    """
    This class provides utility functions that are common to all sizer types, each sizer just needs one of these.
    """

    def __init__(self, api: cdll):
        api.sizerGetLastErrorMessages.argtypes = [c_void_p]
        api.sizerGetLastErrorMessages.restype = c_char_p

    @staticmethod
    def get_error_messages(api: cdll, instance: c_void_p) -> bytes:
        """
        Lists out any error messages during sizing for this sizer, and clears the error buffer.

        :return: Returns a raw byte string of error messages.
        """
        return api.getLastErrorMessages(instance)


class HeatingAirflowUASizer:
    """
    This sizer class wraps the internal HeatingAirflowUASizer class
    """

	ZoneConfigTerminal = 0
	ZoneConfigInductionUnit = 1
	ZoneConfigFanCoil = 2

	SysConfigOutdoorAir = 0
	SysConfigMainDuct = 1
	SysConfigCoolingDuct = 2
	SysConfigHeatingDuct = 3
	SysConfigOtherDuct = 4
	
    def __init__(self, api: cdll):
        self.api = api
        self.api.sizerHeatingAirflowUANew.argtypes = []
        self.api.sizerHeatingAirflowUANew.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForZone.argtypes = [c_void_p, int, RealEP, RealEP, RealEP]
        self.api.sizerHeatingAirflowUAInitializeForZone.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitializeForSystem.argtypes = [c_void_p, int, RealEP, RealEP, RealEP, int]
        self.api.sizerHeatingAirflowUAInitializeForSystem.restype = c_void_p
        self.api.sizerHeatingAirflowUADelete.argtypes = [c_void_p]
        self.api.sizerHeatingAirflowUADelete.restype = c_void_p
        self.api.sizerHeatingAirflowUASize.argtypes = [c_void_p]
        self.api.sizerHeatingAirflowUASize.restype = int
        self.api.sizerHeatingAirflowUAValue.argtypes = [c_void_p]
        self.api.sizerHeatingAirflowUAValue.restype = RealEP
        self.base_worker = BaseSizerWorker(self.api)
        self.instance = self.api.sizerHeatingAirflowUANew()

    def __del__(self):
        self.api.sizerHeatingAirflowUADelete(self.instance)

    def get_last_error_messages(self):
        return self.base_worker.get_error_messages(self.api, self.instance)

    def initialize_for_zone(self, zone_config: int, elevation: float, representative_flow_rate: float, reheat_multiplier: float = 0.0) -> None:
        self.api.sizerHeatingAirflowUAInitializeForZone(self.instance, zone_config, elevation, representative_flow_rate, reheat_multiplier)

    def initialize_for_system_outdoor_air(self, sys_config: int, elevation: float, representative_flow_rate: float, min_flow_rate_ratio: float, doas: bool) -> None:
        self.api.sizerHeatingAirflowUAInitializeForSystem(self.instance, sys_config, elevation, representative_flow_rate, min_flow_rate_ratio, 1 if doas else 0)

    def size(self) -> bool:
        """
        Performs autosizing calculations with the given initialized values

        :return: True if the sizing was successful, or False if not
        """
        return True if self.api.sizerHeatingAirflowUASize(self.instance) == 0 else False

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
