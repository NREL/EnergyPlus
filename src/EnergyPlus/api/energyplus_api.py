from ctypes import cdll
import os
import sys

from functional import Functional
from datatransfer import DataTransfer
from runtime import Runtime


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

    def data_transfer(self):
        return DataTransfer(self.api)

    def runtime(self):
        return Runtime(self.api)
