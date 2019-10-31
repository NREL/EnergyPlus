
from pyenergyplus.api import EnergyPlusAPI


class EnergyPlusPlugin(object):
    """
    The EnergyPlusPlugin class is the base class for all Python Plugin classes.
    Derived classes should inherit from this class and override at least the main function.
    Optionally, the initialize() method can be overridden to allow for one-time initializations to be made without
    having to override the constructor and set one-time flags, etc.

    This base class instantiates the EnergyPlus API and makes it available to derived classes through the `self.api`
    member variable.  Scripts can then access the functional API through `self.api.functional` and the data exchange
    API through `self.api.exchange`.

    This base class also creates a convenience variable: self.data which is a dictionary.  This is purely a convenience
    to allow derived classes to store data on the class without having to declare a variable in a custom constructor.
    Derived classes can ignore this and store data as they see fit.
    """

    def __init__(self):
        """
        Constructor for the Plugin interface base class.  Does not take any arguments, initializes member variables.

        Note API is available on derived classes through:
        - self.api.functional provides access to a functional API class, instantiated and ready to go
        - self.api.exchange provides access to a data exchange API class, instantiated and ready to go
        """
        super().__init__()
        self.api = EnergyPlusAPI(True)
        self.data = {}

    def initialize(self) -> None:
        """
        Provides a base class function that can be overridden to do some script initialization.  This function is called
        at a point in the EnergyPlus simulation when structures have been populated and so EMS actuators and variables
        can be accessed to get handles.  This function does not need to be overridden, users can still achieve the same
        behavior by looking up handles at later, regular calling points in the simulation.  This is merely a
        convenience.

        :return: Does not return anything
        """
        pass

    def main(self) -> int:
        """
        Performs the main plugin action, calling for sensor data and setting actuator data.
        This function can call other functions as needed, as well as importing other Python libraries.

        **Derived classes MUST override this function!**

        :return: An integer exit code, for now zero is success and one indicates EnergyPlus should throw a fatal error
        """
        raise NotImplementedError(
            "Encountered EnergyPlusPlugin::main base function -- override this in your plugin class"
        )
