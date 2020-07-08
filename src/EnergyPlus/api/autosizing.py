from ctypes import cdll, c_void_p
from pyenergyplus.common import RealEP


class HeatingAirflowUASizer:
    """
    This sizer class wraps the internal HeatingAirflowUASizer class
    """

    def __init__(self, api: cdll):
        self.api = api
        self.api.sizerHeatingAirflowUANew.argtypes = []
        self.api.sizerHeatingAirflowUANew.restype = c_void_p
        self.api.sizerHeatingAirflowUAInitialize.argtypes = [c_void_p, RealEP]
        self.api.sizerHeatingAirflowUAInitialize.restype = c_void_p
        self.api.sizerHeatingAirflowUADelete.argtypes = [c_void_p]
        self.api.sizerHeatingAirflowUADelete.restype = c_void_p
        self.api.sizerHeatingAirflowUACalculate.argtypes = [c_void_p]
        self.api.sizerHeatingAirflowUACalculate.restype = int
        self.api.sizerHeatingAirflowUAValue.argtypes = [c_void_p]
        self.api.sizerHeatingAirflowUAValue.restype = RealEP
        self.instance = self.api.sizerHeatingAirflowUANew()

    def __del__(self):
        self.api.sizerHeatingAirflowUADelete(self.instance)

    def initialize(self, temperature: float) -> None:
        """
        Performs initialization of the sizer, eventually it will have lots of args

        :return: Nothing
        """
        self.api.sizerHeatingAirflowUAInitialize(self.instance, temperature)

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
