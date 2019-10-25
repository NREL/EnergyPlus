from ctypes import cdll, c_int, c_char_p, c_void_p
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
        self.api.setActuatorValue.restype = c_int
        # some simulation data values are available for plugins or regular runtime calls
        self.api.year.argtypes = []
        self.api.year.restype = c_int
        self.api.month.argtypes = []
        self.api.month.restype = c_int
        self.api.dayOfMonth.argtypes = []
        self.api.dayOfMonth.restype = c_int
        self.api.dayOfWeek.argtypes = []
        self.api.dayOfWeek.restype = c_int
        self.api.dayOfYear.argtypes = []
        self.api.dayOfYear.restype = c_int
        self.api.daylightSavingsTimeIndicator.argtypes = []
        self.api.daylightSavingsTimeIndicator.restype = c_int
        self.api.hour.argtypes = []
        self.api.hour.restype = c_int
        self.api.currentTime.argtypes = []
        self.api.currentTime.restype = c_int
        self.api.minutes.argtypes = []
        self.api.minutes.restype = c_int
        self.api.holidayIndex.argtypes = []
        self.api.holidayIndex.restype = c_int
        self.api.sunIsUp.argtypes = []
        self.api.sunIsUp.restype = c_int
        self.api.isRaining.argtypes = []
        self.api.isRaining.restype = c_int
        self.api.systemTimeStep.argtypes = []
        self.api.systemTimeStep.restype = RealEP
        self.api.currentEnvironmentNum.argtypes = []
        self.api.currentEnvironmentNum.restype = c_int
        self.api.warmupFlag.argtypes = []
        self.api.warmupFlag.restype = c_int
        # there is also some special things that EMS exposed for convenience
        self.api.getZoneIndex.argtypes = [c_char_p]
        self.api.getZoneIndex.restype = c_int
        self.api.getZoneFloorArea.argtypes = [c_int]
        self.api.getZoneFloorArea.restype = RealEP
        self.api.getZoneAirVolume.argtypes = [c_int]
        self.api.getZoneAirVolume.restype = RealEP
        self.api.getZoneMultiplier.argtypes = [c_int]
        self.api.getZoneMultiplier.restype = RealEP
        self.api.getZoneListMultiplier.argtypes = [c_int]
        self.api.getZoneListMultiplier.restype = RealEP
        # these are only meaningful for Python Plugins, so they are declared here, but no Python functions wrap them
        self.api.getPluginGlobalVariableHandle.argtypes = [c_char_p]
        self.api.getPluginGlobalVariableHandle.restype = c_int
        self.api.getPluginGlobalVariableValue.argtypes = [c_int]
        self.api.getPluginGlobalVariableValue.restype = RealEP
        self.api.setPluginGlobalVariableValue.argtypes = [c_int, RealEP]
        self.api.setPluginGlobalVariableValue.restype = c_void_p

    def get_variable_handle(self, variable_name: Union[str, bytes], variable_key: Union[str, bytes]) -> int:
        """
        Get a handle to an output variable in a running simulation.

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
        Get a handle to an available actuator in a running simulation.

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

    def set_actuator_value(self, actuator_handle: int, actuator_value: RealEP) -> int:
        """
        Sets the value of an actuator in a running simulation.  The `get_actuator_handle` function is first used
        to get a handle to the actuator by name.  Then once the handle is retrieved, it is passed into this function,
        along with the value to assign, to then set the value of the actuator.

        :param actuator_handle: An integer returned from the `get_actuator_handle` function.
        :param actuator_value: The value to assign to the actuator
        :return: An integer response value for success of setting the actuator value, 0 is success, failure otherwise
        """
        return self.api.setActuatorValue(actuator_handle, actuator_value)

    def year(self) -> int:
        return self.api.year()

    def month(self) -> int:
        return self.api.month()

    def day_of_month(self) -> int:
        return self.api.dayOfMonth()

    def day_of_week(self) -> int:
        return self.api.dayOfWeek()

    def day_of_year(self) -> int:
        return self.api.dayOfYear()

    def daylight_savings_time_indicator(self) -> int:
        return self.api.daylightSavingsTimeIndicator()

    def hour(self) -> int:
        return self.api.hour()

    def current_time(self) -> int:
        return self.api.currentTime()

    def minutes(self) -> int:
        return self.api.minutes()

    def holiday_index(self) -> int:
        return self.api.holidayIndex()

    def sun_is_up(self) -> int:
        return self.api.sunIsUp()

    def is_raining(self) -> int:
        return self.api.isRaining()

    def system_time_step(self) -> float:
        return self.api.systemTimeStep()

    def current_environment_num(self) -> int:
        return self.api.currentEnvironmentNum()

    def warmup_flag(self) -> int:
        return self.api.warmupFlag()

    def get_zone_index(self, zone_name: Union[str, bytes]) -> int:
        if isinstance(zone_name, str):
            zone_name = zone_name.encode('utf-8')
        return self.api.getZoneIndex(zone_name)

    def get_zone_floor_area(self, zone_handle: int) -> float:
        return self.api.getZoneFloorArea(zone_handle)

    def get_zone_volume(self, zone_handle: int) -> float:
        return self.api.getZoneAirVolume(zone_handle)

    def get_zone_multiplier(self, zone_handle: int) -> float:
        return self.api.getZoneMultiplier(zone_handle)

    def get_zone_list_multiplier(self, zone_handle: int) -> float:
        return self.api.getZoneListMultiplier(zone_handle)
