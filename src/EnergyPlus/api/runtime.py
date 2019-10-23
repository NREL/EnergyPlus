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
        self.api.energyplus.argtypes = [c_char_p]
        self.api.energyplus.restype = c_int
        self.py_callback_type = CFUNCTYPE(c_void_p)
        self.api.registerCallbackFromBeginNewEnvironment.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromBeginNewEnvironment.restype = c_void_p
        self.api.registerCallbackFromAfterNewEnvironmentWarmupComplete.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromAfterNewEnvironmentWarmupComplete.restype = c_void_p
        self.api.registerCallbackFromBeginZoneTimeStepBeforeInitHeatBalance.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromBeginZoneTimeStepBeforeInitHeatBalance.restype = c_void_p
        self.api.registerCallbackFromBeginZoneTimeStepAfterInitHeatBalance.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromBeginZoneTimeStepAfterInitHeatBalance.restype = c_void_p
        self.api.registerCallbackFromBeginTimeStepBeforePredictor.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromBeginTimeStepBeforePredictor.restype = c_void_p
        self.api.registerCallbackFromAfterPredictorBeforeHVACManagers.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromAfterPredictorBeforeHVACManagers.restype = c_void_p
        self.api.registerCallbackFromAfterPredictorAfterHVACManagers.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromAfterPredictorAfterHVACManagers.restype = c_void_p
        self.api.registerCallbackFromInsideSystemIterationLoop.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromInsideSystemIterationLoop.restype = c_void_p
        self.api.registerCallbackFromEndOfZoneTimeStepBeforeZoneReporting.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromEndOfZoneTimeStepBeforeZoneReporting.restype = c_void_p
        self.api.registerCallbackFromEndOfZoneTimeStepAfterZoneReporting.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromEndOfZoneTimeStepAfterZoneReporting.restype = c_void_p
        self.api.registerCallbackFromEndOfSystemTimeStepBeforeHVACReporting.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromEndOfSystemTimeStepBeforeHVACReporting.restype = c_void_p
        self.api.registerCallbackFromEndOfSystemTimeStepAfterHVACReporting.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromEndOfSystemTimeStepAfterHVACReporting.restype = c_void_p
        self.api.registerCallbackFromEndOfZoneSizing.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromEndOfZoneSizing.restype = c_void_p
        self.api.registerCallbackFromEndOfSystemSizing.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromEndOfSystemSizing.restype = c_void_p
        self.api.registerCallbackFromEndOfAfterComponentGetInput.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromEndOfAfterComponentGetInput.restype = c_void_p
        self.api.registerCallbackFromUserDefinedComponentModel.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromUserDefinedComponentModel.restype = c_void_p
        self.api.registerCallbackFromUnitarySystemSizing.argtypes = [self.py_callback_type]
        self.api.registerCallbackFromUnitarySystemSizing.restype = c_void_p

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
        return self.api.energyplus(path_to_dir)

    def register_callback_begin_new_environment(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the beginning of
        each environment.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromBeginNewEnvironment(cb_ptr)

    def register_callback_after_new_environment_warmup_complete(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the warmup of
        each environment.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromAfterNewEnvironmentWarmupComplete(cb_ptr)

    def register_callback_begin_zone_timestep_before_init_heat_balance(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the beginning of the
        zone time step before init heat balance.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromBeginZoneTimeStepBeforeInitHeatBalance(cb_ptr)

    def register_callback_begin_zone_timestep_after_init_heat_balance(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the beginning of the
        zone time step after init heat balance.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromBeginZoneTimeStepAfterInitHeatBalance(cb_ptr)

    def register_callback_begin_system_timestep_before_predictor(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the beginning of
        system time step .

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromBeginTimeStepBeforePredictor(cb_ptr)

    def register_callback_after_predictor_before_hvac_managers(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of the
        predictor step but before HVAC managers.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromAfterPredictorBeforeHVACManagers(cb_ptr)

    def register_callback_after_predictor_after_hvac_managers(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of the
        predictor step after HVAC managers.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromAfterPredictorAfterHVACManagers(cb_ptr)

    def register_callback_inside_system_iteration_loop(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus inside the system
        iteration loop.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromInsideSystemIterationLoop(cb_ptr)

    def register_callback_end_zone_timestep_before_zone_reporting(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of a zone
        time step but before zone reporting has been completed.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromEndOfZoneTimeStepBeforeZoneReporting(cb_ptr)

    def register_callback_end_zone_timestep_after_zone_reporting(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of a zone
        time step and after zone reporting.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromEndOfZoneTimeStepAfterZoneReporting(cb_ptr)

    def register_callback_end_system_timestep_before_hvac_reporting(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of a system
        time step, but before HVAC reporting.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromEndOfSystemTimeStepBeforeHVACReporting(cb_ptr)

    def register_callback_end_system_timestep_after_hvac_reporting(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of a system
        time step and after HVAC reporting.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromEndOfSystemTimeStepAfterHVACReporting(cb_ptr)

    def register_callback_end_zone_sizing(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of the zone
        sizing process.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromEndOfZoneSizing(cb_ptr)

    def register_callback_end_system_sizing(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of the system
        sizing process.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromEndOfSystemSizing(cb_ptr)

    def register_callback_after_component_get_input(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of
        component get input processes.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromEndOfAfterComponentGetInput(cb_ptr)

    def register_callback_user_defined_component_model(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus inside the user defined
        component model.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromUserDefinedComponentModel(cb_ptr)

    def register_callback_unitary_system_sizing(self, f) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus in unitary system sizing.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerCallbackFromUnitarySystemSizing(cb_ptr)

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
