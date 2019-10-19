from ctypes import cdll, c_int, c_char_p, c_void_p, CFUNCTYPE
from typing import Union


# CFUNCTYPE wrapped Python callbacks need to be kept in memory explicitly, otherwise GC takes it
# This causes undefined behavior but generally segfaults and illegal access violations
# Keeping them referenced in a global array here suffices to keep GC away from it
# And while we _could_ clean up after ourselves, I would imagine users aren't making *so* many callbacks that
# We have anything to worry about here
all_callbacks = []


class Runtime:
    """
    This API class enables a client to hook into EnergyPlus at runtime and sense/actuate data in a running simulation.
    The pattern is quite simple: create a callback function in Python, and register it with one of the registration
    methods on this class to allow the callback to be called at a specific point in the simulation.  Inside the callback
    function, the client can get sensor values and set actuator values using the DataTransfer API methods, and also
    look up values and perform calculations using EnergyPlus internal methods via the Functional API methods.
    """

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

    def run_energyplus(self, path_to_dir: Union[str, bytes]) -> int:
        """
        This function enables EnergyPlus to run a simulation.  Currently this API method expects a directory name
        to be passed in.  This directory must contain an in.idf file, and if the file is running a weather file run
        period, there must also be an in.epw file.

        NOTE that currently EnergyPlus does not completely clean up the global state, so calling run_energyplus
        multiple times in a single thread is not advised.  Effort is currently underway to ensure EnergyPlus is fully
        cleaned up after a call to run, but until then, only call this once per thread.

        :param path_to_dir: Path to a directory containing the in.idf (and possibly in.epw) to be run
        :return: An integer exit code from the simulation, zero is success, non-zero is failure
        """
        return self.api.cRunEnergyPlus(path_to_dir)

    def register_callback_end_of_hour(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of each hour.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerRuntimeCallbackFromEndOfHour(cb_ptr)

    def register_callback_beginning_of_hour(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the start of each hour.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerRuntimeCallbackFromBeginningOfHour(cb_ptr)

    def register_callback_end_of_zone_time_step(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of each zone
        time step.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerRuntimeCallbackFromEndOfZoneTimeStep(cb_ptr)

    def register_callback_beginning_of_zone_time_step(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of each zone
        time step.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerRuntimeCallbackFromBeginningOfZoneTimeStep(cb_ptr)

    @staticmethod
    def clear_callbacks():
        """
        This function is only used if you are running this script continually making many calls into the E+ library in
        one thread, and you need to clean up.  Since E+ is currently not able to reliably re-run, this function
        shouldn't be necessary anytime soon, but it's here just in case.

        Note this will affect all current Runtime instances in this thread, so use carefully!

        :return: Nothing
        """
        all_callbacks.clear()
