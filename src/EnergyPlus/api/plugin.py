from pyenergyplus.api import EnergyPlusAPI


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
        self.functional = api.functional()
        self.transfer = api.data_transfer()

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

    def ems_program_name(self) -> str:
        """
        If this Python Plugin is to be used as a standard EMS program, this would be the name of the program referenced
        in the input file.  For example, if this was used as a program for a user-defined component, this name would be
        entered in the user-defined component input fields

        If this Python Plugin is not intended for use in that way, but rather as regular plugins, this function does not
        need to be overridden.

        :return: A string program name to be used as an alias in typical EMS program name inputs
        """
        return ""
