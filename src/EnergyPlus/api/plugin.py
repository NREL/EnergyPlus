from typing import Union

from pyenergyplus.api import EnergyPlusAPI
from pyenergyplus.datatransfer import DataTransfer


class PluginSpecificAPI(object):
    """
    This class captures API points and other stuff that is only meaningful for the Python Plugin system.
    As such, the Python bindings are not included in the regular API files, just added here.
    """
    def __init__(self, transfer_api: DataTransfer):
        self.transfer = transfer_api

    def get_global_handle(self, var_name: Union[str, bytes]) -> int:
        if isinstance(var_name, str):
            var_name = var_name.encode('utf-8')
        return self.transfer.api.getPluginGlobalVariableHandle(var_name)

    def get_global_value(self, handle: int) -> float:
        return self.transfer.api.getPluginGlobalVariableValue(handle)

    def set_global_value(self, handle: int, value: float) -> None:
        self.transfer.api.setPluginGlobalVariableValue(handle, value)


class EnergyPlusPlugin(object):
    """
    The EnergyPlusPlugin class is the base class for all Python Plugin classes.
    Derived classes should inherit from this class and override at least the main function.

    This base class includes two member variables for accessing the EnergyPlus functional and data transfer APIs:
    self.functional and self.transfer.
    """

    def __init__(self):
        """
        Constructor for the Plugin interface base class.  Does not take any arguments, initializes member variables
        """
        super().__init__()
        api = EnergyPlusAPI()
        self.functional = api.functional(False)
        self.exchange = api.data_transfer()
        self.helpers = PluginSpecificAPI(api)

    def main(self) -> int:
        """
        Performs the main plugin action, calling for sensor data and setting actuator data.
        This function can call other functions as needed, as well as importing other Python libraries.

        **Derived classes must override this function!**

        :return: An integer exit code, for now zero is success and one indicates EnergyPlus should throw a fatal error
        """
        raise NotImplementedError(
            "Encountered EnergyPlusPlugin::main base function -- override this in your plugin class"
        )
