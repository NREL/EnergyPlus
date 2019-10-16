from ctypes import cdll, c_int, c_char_p, c_void_p, CFUNCTYPE
from typing import Union


# CFUNCTYPE wrapped Python callbacks need to be kept in memory explicitly, otherwise GC takes it
# This causes undefined behavior but generally segfaults and illegal access violations
# Keeping them referenced in a global array here suffices to keep GC away from it
# And while we _could_ clean up after ourselves, I would imagine users aren't making *so* many callbacks that
# We have anything to worry about here
all_callbacks = []


class Runtime:

    def __init__(self, api: cdll):
        self.api = api
        self.api.cRunEnergyPlus.argtypes = [c_char_p]
        self.api.cRunEnergyPlus.restype = c_int
        self.py_callback_type = CFUNCTYPE(c_void_p)
        self.api.registerRuntimeCallbackFromEndOfHour.argtypes = [self.py_callback_type]
        self.api.registerRuntimeCallbackFromEndOfHour.restype = c_void_p
        self.api.registerRuntimeCallbackFromBeginningOfHour.argtypes = [self.py_callback_type]
        self.api.registerRuntimeCallbackFromBeginningOfHour.restype = c_void_p
        self.api.registerRuntimeCallbackFromBeginningOfZoneTimeStep.argtypes = [self.py_callback_type]
        self.api.registerRuntimeCallbackFromBeginningOfZoneTimeStep.restype = c_void_p
        self.api.registerRuntimeCallbackFromEndOfZoneTimeStep.argtypes = [self.py_callback_type]
        self.api.registerRuntimeCallbackFromEndOfZoneTimeStep.restype = c_void_p

    def run_energyplus(self, path_to_dir: Union[str, bytes]):
        return self.api.cRunEnergyPlus(path_to_dir)

    def register_callback_end_of_hour(self, f):
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerRuntimeCallbackFromEndOfHour(cb_ptr)

    def register_callback_beginning_of_hour(self, f):
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerRuntimeCallbackFromBeginningOfHour(cb_ptr)

    def register_callback_end_of_zone_time_step(self, f):
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerRuntimeCallbackFromEndOfZoneTimeStep(cb_ptr)

    def register_callback_beginning_of_zone_time_step(self, f):
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerRuntimeCallbackFromBeginningOfZoneTimeStep(cb_ptr)

    @staticmethod
    def clear_callbacks(self):
        # Only used if you are running this script continually making many calls into the E+ library in one
        # thread, and you need to clean up.   Generally this shouldn't be necessary.
        # Note this will affect all current Runtime instances in this thread, so use carefully!
        all_callbacks.clear()
