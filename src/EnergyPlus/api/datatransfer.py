from ctypes import cdll, c_bool, c_int, c_char_p
from pyenergyplus.common import RealEP
from typing import Union


class DataTransfer:
    """
    This API class enables data transfer between EnergyPlus and a client.  Output variables and meters are treated as
    "sensor" data.  A client should get a handle (integer) using one of the worker methods then make calls to get data
    using that handle.  Some specific variables in EnergyPlus are controllable as actuators.  These work the same way,
    in that a client should get an integer handle to an actuator and then use the workers to set the value on the
    actuator.

    This data transfer class is used in one of two workflows:

    - When a outside tool is already running EnergyPlus using the Runtime API, and data transfer is to be made during
      callback functions. In this case, the script should create a DataTransfer API class by calling the `data_transfer`
      method on the main API class, never trying to create this class directly.
    - When a Python script is used in the EnergyPlus Python Plugin System, and the user runs the EnergyPlus binary.  In
      this case, the plugin may need access to state data to make control decisions, and this class enables that.  The
      plugin base class automatically creates an instance of this class, so client plugins that inherit that class will
      have a `self.transfer` instance of this class available, and should *not* attempt to create another one.
    """

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

    def get_variable_handle(self, variable_name: Union[str, bytes], variable_key: Union[str, bytes]) -> int:
        """
        Get a handle to an output variable in a running simulation.  For now, this variable *must* be defined as an
        output variable in the input file being run.  In a future version, this restriction may be lifted.

        The arguments passed into this function do not need to be a particular case, as the EnergyPlus API
        automatically converts values to upper-case when finding matches to internal variables in the simulation.

        Note also that the arguments passed in here can be either strings or bytes, as this wrapper handles conversion
        as needed.

        :param variable_name: The name of the variable to retrieve, e.g. "Site Outdoor Air DryBulb Temperature", or
                              "Fan Air Mass Flow Rate"
        :param variable_key: The instance of the variable to retrieve, e.g. "Environment", or "Main System Fan"
        :return: An integer ID for this output variable, or zero if one could not be found.
        """
        if isinstance(variable_name, str):
            variable_name = variable_name.encode('utf-8')
        if isinstance(variable_key, str):
            variable_key = variable_key.encode('utf-8')
        return self.api.getVariableHandle(variable_name, variable_key)

    def get_meter_handle(self, meter_name: Union[str, bytes]) -> int:
        """
        Get a handle to a meter in a running simulation.

        The meter name passed into this function do not need to be a particular case, as the EnergyPlus API
        automatically converts values to upper-case when finding matches to internal variables in the simulation.

        Note also that the meter name passed in here can be either strings or bytes, as this wrapper handles conversion
        as needed.

        :param meter_name: The name of the variable to retrieve, e.g. "Electricity:Facility", or "Fans:Electricity"
        :return: An integer ID for this meter, or zero if one could not be found.
        """
        meter_name = meter_name.upper()
        if isinstance(meter_name, str):
            meter_name = meter_name.encode('utf-8')
        return self.api.getMeterHandle(meter_name)

    def get_actuator_handle(self, actuator_name: Union[str, bytes], actuator_key: Union[str, bytes]) -> int:
        """
        Get a handle to an available actuator in a running simulation.  For now, there *must* be at least one EMS
        related object in the IDF, even just an unused EMS global variable.  This is because the simulation skips much
        EMS code for efficiency if no EMS objects are found in the IDF.  In a future version, this restriction may be
        lifted.

        The arguments passed into this function do not need to be a particular case, as the EnergyPlus API
        automatically converts values to upper-case when finding matches to internal variables in the simulation.

        Note also that the arguments passed in here can be either strings or bytes, as this wrapper handles conversion
        as needed.

        :param actuator_name: The name of the actuator to retrieve, e.g. "Outdoor Dew Point"
        :param actuator_key: The instance of the variable to retrieve, e.g. "Environment"
        :return: An integer ID for this output variable, or zero if one could not be found.
        """
        if isinstance(actuator_name, str):
            actuator_name = actuator_name.encode('utf-8')
        if isinstance(actuator_key, str):
            actuator_key = actuator_key.encode('utf-8')
        return self.api.getActuatorHandle(actuator_name, actuator_key)

    def get_variable_value(self, variable_handle: int) -> float:
        """
        Get the current value of a variable in a running simulation.  The `get_variable_handle` function is first used
        to get a handle to the variable by name.  Then once the handle is retrieved, it is passed into this function to
        then get the value of the variable.

        :param variable_handle: An integer returned from the `get_variable_handle` function.
        :return: Floating point representation of the current variable value
        """
        return self.api.getVariableValue(variable_handle)

    def get_meter_value(self, meter_handle: int) -> float:
        """
        Get the current value of a meter in a running simulation.  The `get_meter_handle` function is first used
        to get a handle to the meter by name.  Then once the handle is retrieved, it is passed into this function to
        then get the value of the meter.

        Note this function is not completed yet.  It currently gives an instant reading of the meter, not an aggregate
        value throughout the simulation.  Use caution

        :param meter_handle: An integer returned from the `get_meter_handle` function.
        :return: Floating point representation of the current meter value
        """
        return self.api.getMeterValue(meter_handle)

    def set_actuator_value(self, actuator_handle: int, actuator_value: RealEP) -> bool:
        """
        Sets the value of an actuator in a running simulation.  The `get_actuator_handle` function is first used
        to get a handle to the actuator by name.  Then once the handle is retrieved, it is passed into this function,
        along with the value to assign, to then set the value of the actuator.

        :param actuator_handle: An integer returned from the `get_actuator_handle` function.
        :param actuator_value: The value to assign to the actuator
        :return: A bool response value for success of setting the actuator value, True is success, False is failure
        """
        return self.api.setActuatorValue(actuator_handle, actuator_value)
