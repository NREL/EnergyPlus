from ctypes import cdll
import os
import sys

from functional import BaseThermalPropertySet
from datatransfer import DataTransfer
from runtime import Runtime


def api_path():
    this_absolute_dir = os.path.dirname(os.path.realpath(__file__))
    if sys.platform.startswith('linux'):
        return os.path.join(this_absolute_dir, 'libenergyplusapi.so')
    elif sys.platform.startswith('darwin'):
        return os.path.join(this_absolute_dir, 'libenergyplusapi.dylib')
    else:  # assume Windows
        return os.path.join(this_absolute_dir, 'EnergyPlusAPI.dll')


class EnergyPlusAPI:
    def __init__(self):
        self.api = cdll.LoadLibrary(api_path())

    def props(self, conductivity: float, density: float, specific_heat: float) -> BaseThermalPropertySet:
        return BaseThermalPropertySet(self.api, conductivity, density, specific_heat)

    def data_transfer(self):
        return DataTransfer(self.api)

    def runtime(self):
        return Runtime(self.api)
