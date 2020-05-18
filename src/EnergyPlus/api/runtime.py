from ctypes import cdll, c_int, c_char_p, c_void_p, CFUNCTYPE
from typing import Union, List
from types import FunctionType


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
        # self.api.energyplus.argtypes = [c_int, POINTER(c_char_p)]  # DEFERRED UNTIL run_energyplus call
        self.api.energyplus.restype = c_int
        self.api.issueWarning.argtypes = [c_char_p]
        self.api.issueWarning.restype = c_void_p
        self.api.issueSevere.argtypes = [c_char_p]
        self.api.issueSevere.restype = c_void_p
        self.api.issueText.argtypes = [c_char_p]
        self.api.issueText.restype = c_void_p
        self.py_progress_callback_type = CFUNCTYPE(c_void_p, c_int)
        self.api.registerProgressCallback.argtypes = [self.py_progress_callback_type]
        self.api.registerProgressCallback.restype = c_void_p
        self.py_message_callback_type = CFUNCTYPE(c_void_p, c_char_p)
        self.api.registerStdOutCallback.argtypes = [self.py_message_callback_type]
        self.api.registerStdOutCallback.restype = c_void_p
        self.py_empty_callback_type = CFUNCTYPE(c_void_p)
        self.api.callbackBeginNewEnvironment.argtypes = [self.py_empty_callback_type]
        self.api.callbackBeginNewEnvironment.restype = c_void_p
        self.api.callbackAfterNewEnvironmentWarmupComplete.argtypes = [self.py_empty_callback_type]
        self.api.callbackAfterNewEnvironmentWarmupComplete.restype = c_void_p
        self.api.callbackBeginZoneTimeStepBeforeInitHeatBalance.argtypes = [self.py_empty_callback_type]
        self.api.callbackBeginZoneTimeStepBeforeInitHeatBalance.restype = c_void_p
        self.api.callbackBeginZoneTimeStepAfterInitHeatBalance.argtypes = [self.py_empty_callback_type]
        self.api.callbackBeginZoneTimeStepAfterInitHeatBalance.restype = c_void_p
        self.api.callbackBeginTimeStepBeforePredictor.argtypes = [self.py_empty_callback_type]
        self.api.callbackBeginTimeStepBeforePredictor.restype = c_void_p
        self.api.callbackAfterPredictorBeforeHVACManagers.argtypes = [self.py_empty_callback_type]
        self.api.callbackAfterPredictorBeforeHVACManagers.restype = c_void_p
        self.api.callbackAfterPredictorAfterHVACManagers.argtypes = [self.py_empty_callback_type]
        self.api.callbackAfterPredictorAfterHVACManagers.restype = c_void_p
        self.api.callbackInsideSystemIterationLoop.argtypes = [self.py_empty_callback_type]
        self.api.callbackInsideSystemIterationLoop.restype = c_void_p
        self.api.callbackEndOfZoneTimeStepBeforeZoneReporting.argtypes = [self.py_empty_callback_type]
        self.api.callbackEndOfZoneTimeStepBeforeZoneReporting.restype = c_void_p
        self.api.callbackEndOfZoneTimeStepAfterZoneReporting.argtypes = [self.py_empty_callback_type]
        self.api.callbackEndOfZoneTimeStepAfterZoneReporting.restype = c_void_p
        self.api.callbackEndOfSystemTimeStepBeforeHVACReporting.argtypes = [self.py_empty_callback_type]
        self.api.callbackEndOfSystemTimeStepBeforeHVACReporting.restype = c_void_p
        self.api.callbackEndOfSystemTimeStepAfterHVACReporting.argtypes = [self.py_empty_callback_type]
        self.api.callbackEndOfSystemTimeStepAfterHVACReporting.restype = c_void_p
        self.api.callbackEndOfZoneSizing.argtypes = [self.py_empty_callback_type]
        self.api.callbackEndOfZoneSizing.restype = c_void_p
        self.api.callbackEndOfSystemSizing.argtypes = [self.py_empty_callback_type]
        self.api.callbackEndOfSystemSizing.restype = c_void_p
        self.api.callbackEndOfAfterComponentGetInput.argtypes = [self.py_empty_callback_type]
        self.api.callbackEndOfAfterComponentGetInput.restype = c_void_p
        # self.api.callbackUserDefinedComponentModel.argtypes = [self.py_empty_callback_type]
        # self.api.callbackUserDefinedComponentModel.restype = c_void_p
        self.api.callbackUnitarySystemSizing.argtypes = [self.py_empty_callback_type]
        self.api.callbackUnitarySystemSizing.restype = c_void_p

    def run_energyplus(self, command_line_args: List[Union[str, bytes]]) -> int:
        """
        This function calls EnergyPlus to run a simulation.  The C API expects to find arguments matching the command
        line string when executing EnergyPlus.  When calling the C API directly, the client must create a list of char
        arguments starting with the program name, followed by all the command line options.  For this Python API, the
        program name is not passed in as an argument, rather only the command line options.

        An example call:
        run_energyplus(['-d', '/path/to/output/directory', '-w', '/path/to/weather.epw', '/path/to/input.idf'])

        :param command_line_args: The command line arguments that would be passed into EnergyPlus if executing directly
                                  from the EnergyPlus executable.
        :return: An integer exit code from the simulation, zero is success, non-zero is failure
        """
        args_with_program_name = [b"energyplus"]  # don't require the program name argument, just add it here
        for cla in command_line_args:
            if isinstance(cla, str):
                prepped_cla = cla.encode('utf-8')
            else:
                prepped_cla = cla
            args_with_program_name.append(prepped_cla)
        # Passing an array arg is a smidge tricky, as you need to cast to a fixed size array.
        # Thus we must dynamically create the argument "type" based on the current number of arguments.
        # The Python type checker may be complaining that c_char_p is actually needing to be an integer;
        # this is something to do with the dynamic nature of the ctypes "type" objects.
        # For now I'm adding this noinspection marker to get it to hush
        # noinspection PyTypeChecker
        cli_arg_type = (c_char_p * len(args_with_program_name))
        # OK, so now that we have the argument "type" set as a fixed length array of char*, we assign it to the
        # DLL function call.  This must be done here because the arg count may change with each call.
        self.api.energyplus.argtypes = [c_int, cli_arg_type]
        # Now cast the actual Python list instance into that dynamically created ctype "type".  This is another
        # line where the type checker is complaining because it things cli_arg_type is actually an "int", and as such,
        # it wouldn't be a callable item.  cli_arg_type is actually a callable type, so I'm muting this warning.
        # noinspection PyCallingNonCallable
        cli_args = cli_arg_type(*args_with_program_name)
        return self.api.energyplus(len(args_with_program_name), cli_args)

    def issue_warning(self, message: Union[str, bytes]) -> None:
        """
        This function allows a script to issue a warning through normal EnergyPlus methods.  The message will be listed
        in the standard EnergyPlus error file once the simulation is complete.  This function has limited usefulness
        when calling EnergyPlus as a library, as errors can be handled by the calling client, however, in a
        PythonPlugin workflow, this can be an important tool to alert the user of issues once EnergyPlus(.exe) has
        finished running.

        Note that the argument passed in here can be either a string or a bytes object, as this wrapper handles
        conversion as needed.

        :param message: The warning message to be listed in the error file.
        :return: Nothing
        """
        if isinstance(message, str):
            message = message.encode('utf-8')
        self.api.issueWarning(message)

    def issue_severe(self, message: Union[str, bytes]) -> None:
        """
        This function allows a script to issue an error through normal EnergyPlus methods.  The message will be listed
        in the standard EnergyPlus error file once the simulation is complete.  This function has limited usefulness
        when calling EnergyPlus as a library, as errors can be handled by the calling client, however, in a
        PythonPlugin workflow, this can be an important tool to alert the user of issues once EnergyPlus(.exe) has
        finished running.

        Note the severe errors tend to lead to EnergyPlus terminating with a Fatal Error.  This can be accomplished
        in PythonPlugin workflows by issuing a severe error, followed by returning 1 from the plugin function.
        EnergyPlus will interpret this return value as a signal to terminate with a fatal error.

        Note that the argument passed in here can be either a string or a bytes object, as this wrapper handles
        conversion as needed.

        :param message: The error message to be listed in the error file.
        :return: Nothing
        """
        if isinstance(message, str):
            message = message.encode('utf-8')
        self.api.issueSevere(message)

    def issue_text(self, message: Union[str, bytes]) -> None:
        """
        This function allows a script to issue a message through normal EnergyPlus methods.  The message will be listed
        in the standard EnergyPlus error file once the simulation is complete.  This function can be used alongside the
        warning and error issuance functions to provide further context to the situation. This function has limited
        usefulness when calling EnergyPlus as a library, as errors can be handled by the calling client, however, in a
        PythonPlugin workflow, this can be an important tool to alert the user of issues once EnergyPlus(.exe) has
        finished running.

        Note that the argument passed in here can be either a string or a bytes object, as this wrapper handles
        conversion as needed.

        :param message: The message to be listed in the error file.
        :return: Nothing
        """
        if isinstance(message, str):
            message = message.encode('utf-8')
        self.api.issueText(message)

    def callback_progress(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of each
        day with a progress (percentage) indicator

        :param f: A python function which takes an integer argument and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_progress_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerProgressCallback(cb_ptr)

    def callback_message(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus when printing anything
        to standard output.  This can allow a GUI to easily show the output of EnergyPlus streaming by.  When used in
        conjunction with the progress callback, a progress bar and status text label can provide a nice EnergyPlus
        experience on a GUI.

        :param f: A python function which takes a string (bytes) argument and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_message_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.registerStdOutCallback(cb_ptr)

    def callback_begin_new_environment(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the beginning of
        each environment.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackBeginNewEnvironment(cb_ptr)

    def callback_after_new_environment_warmup_complete(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the warmup of
        each environment.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackAfterNewEnvironmentWarmupComplete(cb_ptr)

    def callback_begin_zone_timestep_before_init_heat_balance(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the beginning of the
        zone time step before init heat balance.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackBeginZoneTimeStepBeforeInitHeatBalance(cb_ptr)

    def callback_begin_zone_timestep_after_init_heat_balance(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the beginning of the
        zone time step after init heat balance.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackBeginZoneTimeStepAfterInitHeatBalance(cb_ptr)

    def callback_begin_system_timestep_before_predictor(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the beginning of
        system time step .

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackBeginTimeStepBeforePredictor(cb_ptr)

    def callback_after_predictor_before_hvac_managers(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of the
        predictor step but before HVAC managers.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackAfterPredictorBeforeHVACManagers(cb_ptr)

    def callback_after_predictor_after_hvac_managers(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of the
        predictor step after HVAC managers.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackAfterPredictorAfterHVACManagers(cb_ptr)

    def callback_inside_system_iteration_loop(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus inside the system
        iteration loop.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackInsideSystemIterationLoop(cb_ptr)

    def callback_end_zone_timestep_before_zone_reporting(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of a zone
        time step but before zone reporting has been completed.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackEndOfZoneTimeStepBeforeZoneReporting(cb_ptr)

    def callback_end_zone_timestep_after_zone_reporting(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of a zone
        time step and after zone reporting.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackEndOfZoneTimeStepAfterZoneReporting(cb_ptr)

    def callback_end_system_timestep_before_hvac_reporting(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of a system
        time step, but before HVAC reporting.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackEndOfSystemTimeStepBeforeHVACReporting(cb_ptr)

    def callback_end_system_timestep_after_hvac_reporting(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of a system
        time step and after HVAC reporting.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackEndOfSystemTimeStepAfterHVACReporting(cb_ptr)

    def callback_end_zone_sizing(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of the zone
        sizing process.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackEndOfZoneSizing(cb_ptr)

    def callback_end_system_sizing(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of the system
        sizing process.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackEndOfSystemSizing(cb_ptr)

    def callback_after_component_get_input(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus at the end of
        component get input processes.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackEndOfAfterComponentGetInput(cb_ptr)

    # user defined component callbacks are not allowed, they are coupled directly to a specific EMS manager/PythonPlugin
    # def callback_user_defined_component_model(self, f: FunctionType) -> None:

    def callback_unitary_system_sizing(self, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus in unitary system sizing.

        :param f: A python function which takes no arguments and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_empty_callback_type(f)
        all_callbacks.append(cb_ptr)
        self.api.callbackUnitarySystemSizing(cb_ptr)

    @staticmethod
    def clear_callbacks() -> None:
        """
        This function is used if you are running this script continually making multiple calls into the E+ library in
        one thread.  EnergyPlus should be cleaned up between runs.

        Note this will clean all registered callbacks, so functions must be registered again prior to the next run.

        :return: Nothing
        """
        all_callbacks.clear()
