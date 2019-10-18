from ctypes import cdll
import os
import sys

from pyenergyplus.func import Functional
from pyenergyplus.datatransfer import DataTransfer
from pyenergyplus.runtime import Runtime


def api_path():
    this_script_dir = os.path.dirname(os.path.realpath(__file__))
    api_dll_dir = os.path.dirname(os.path.normpath(this_script_dir))
    if sys.platform.startswith('linux'):
        return os.path.join(api_dll_dir, 'libenergyplusapi.so')
    elif sys.platform.startswith('darwin'):
        return os.path.join(api_dll_dir, 'libenergyplusapi.dylib')
    else:  # assume Windows
        return os.path.join(api_dll_dir, 'EnergyPlusAPI.dll')


class EnergyPlusAPI:
    def __init__(self):
        self.api = cdll.LoadLibrary(api_path())

    def functional(self) -> Functional:
        return Functional(self.api)

    def data_transfer(self) -> DataTransfer:
        return DataTransfer(self.api)

    def runtime(self) -> Runtime:
        return Runtime(self.api)

    @staticmethod
    def energyplus_version() -> str:
        return "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}"

    @staticmethod
    def api_version() -> float:
        return 0.1
