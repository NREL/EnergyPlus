from ctypes import cdll, c_bool, c_int, c_char_p
from common import RealEP
from typing import Union
import sys


class DataTransfer:

    def __init__(self, api: cdll):
        self.api = api
        self.api.getVariableHandle.argtypes = [c_char_p, c_char_p]
        self.api.getVariableHandle.restype = c_int
        self.api.getMeterHandle.argtypes = [c_char_p]
        self.api.getMeterHandle.restype = c_int
        self.api.getActuatorHandle.argtypes = [c_char_p, c_char_p]
        self.api.getActuatorHandle.restype = c_int
        self.api.getVariableValue.argtypes = [c_int]
        self.api.getVariableValue.restype = RealEP
        self.api.getMeterValue.argtypes = [c_int]
        self.api.getMeterValue.restype = RealEP
        self.api.setActuatorValue.argtypes = [c_int, RealEP]
        self.api.setActuatorValue.restype = c_bool

    def get_variable_handle(self, variable_name: Union[str, bytes], variable_key: Union[str, bytes]):
        if isinstance(variable_name, str):
            variable_name = variable_name.encode('utf-8')
        if isinstance(variable_key, str):
            variable_key = variable_key.encode('utf-8')
        return self.api.getVariableHandle(variable_name, variable_key)

    def get_meter_handle(self, meter_name: Union[str, bytes]):
        if isinstance(meter_name, str):
            meter_name = meter_name.encode('utf-8')
        return self.api.getMeterHandle(meter_name)

    def get_actuator_handle(self, variable_name: Union[str, bytes], variable_key: Union[str, bytes]):
        if isinstance(variable_name, str):
            variable_name = variable_name.encode('utf-8')
        if isinstance(variable_key, str):
            variable_key = variable_key.encode('utf-8')
        return self.api.getActuatorHandle(variable_name, variable_key)

    def get_variable_value(self, variable_handle: int):
        return self.api.getVariableValue(variable_handle)

    def get_meter_value(self, meter_handle: int):
        return self.api.getMeterValue(meter_handle)

    def set_actuator_value(self, variable_handle: int, variable_value: RealEP):
        return self.api.setActuatorValue(variable_handle, variable_value)
