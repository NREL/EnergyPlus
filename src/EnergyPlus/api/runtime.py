from ctypes import cdll, c_int, c_char_p, c_void_p, CFUNCTYPE
from typing import Union


all_callbacks = []  # CFUNCTYPE wrapped Python callbacks need to be kept in memory explicitly, otherwise we may segfault


class Runtime:

    def __init__(self, api: cdll):
        self.api = api
        self.api.cRunEnergyPlus.argtypes = [c_char_p]
        self.api.cRunEnergyPlus.restype = c_int
        self.py_callback_type = CFUNCTYPE(c_void_p)
        self.api.registerRuntimeCallbackFromEndOfHour.argtypes = [self.py_callback_type]
        self.api.registerRuntimeCallbackFromEndOfHour.restype = c_void_p
        # self.api.registerRuntimeCallbackFromBeginNewEvironment.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromBeginNewEvironment.restype = c_void_p
        # self.api.registerRuntimeCallbackFromZoneSizing.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromZoneSizing.restype = c_void_p
        # self.api.registerRuntimeCallbackFromSystemSizing.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromSystemSizing.restype = c_void_p
        # self.api.registerRuntimeCallbackFromBeginTimestepBeforePredictor.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromBeginTimestepBeforePredictor.restype = c_void_p
        # self.api.registerRuntimeCallbackFromEndSystemTimestepBeforeHVACReporting.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromEndSystemTimestepBeforeHVACReporting.restype = c_void_p
        # self.api.registerRuntimeCallbackFromEndSystemTimestepAfterHVACReporting.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromEndSystemTimestepAfterHVACReporting.restype = c_void_p
        # self.api.registerRuntimeCallbackFromBeforeHVACManagers.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromBeforeHVACManagers.restype = c_void_p
        # self.api.registerRuntimeCallbackFromAfterHVACManagers.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromAfterHVACManagers.restype = c_void_p
        # self.api.registerRuntimeCallbackFromHVACIterationLoop.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromHVACIterationLoop.restype = c_void_p
        # self.api.registerRuntimeCallbackFromBeginZoneTimestepBeforeInitHeatBalance.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromBeginZoneTimestepBeforeInitHeatBalance.restype = c_void_p
        # self.api.registerRuntimeCallbackFromBeginZoneTimestepAfterInitHeatBalance.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromBeginZoneTimestepAfterInitHeatBalance.restype = c_void_p
        # self.api.registerRuntimeCallbackFromEndZoneTimestepBeforeZoneReporting.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromEndZoneTimestepBeforeZoneReporting.restype = c_void_p
        # self.api.registerRuntimeCallbackFromBeginNewEnvironmentAfterWarmUp.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromBeginNewEnvironmentAfterWarmUp.restype = c_void_p
        # self.api.registerRuntimeCallbackFromUnitarySizing.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromUnitarySizing.restype = c_void_p
        # self.api.registerRuntimeCallbackFromExternalInterface.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromExternalInterface.restype = c_void_p
        # self.api.registerRuntimeCallbackFromEndZoneTimestepAfterZoneReporting.argtypes = [self.py_callback_type]
        # self.api.registerRuntimeCallbackFromEndZoneTimestepAfterZoneReporting.restype = c_void_p

    def run_energyplus(self, path_to_dir: Union[str, bytes]):
        return self.api.cRunEnergyPlus(path_to_dir)

    def register_callback_end_of_hour(self, f):
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerRuntimeCallbackFromEndOfHour(cb_ptr)

    # def register_callback_new_timestep(self, f):
    #     cb_ptr = self.py_callback_type(f)
    #     all_callbacks.append(cb_ptr)
    #     self.api.registerRuntimeCallbackFromBeginZoneTimestepBeforeInitHeatBalance(cb_ptr)
