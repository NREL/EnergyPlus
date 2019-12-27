from typing import List

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

    def _detect_overridden(self) -> List[str]:
        """
        This function allows for detection of methods that are overridden in derived classes.
        It first collects all the members of the class which share the same name into a single list.
        It then looks up each member name in the EnergyPlusPlugin base class dictionary and the current instance
        dictionary.  If they share the same address, they are the same function, and the derived class must not
        be overriding the base class version.  If they have different addresses, the derived class is overriding it.

        :return: A list of function names which are overridden in the derived class instance of this base class
        """
        common = EnergyPlusPlugin.__dict__.keys() & self.__class__.__dict__.keys()
        diff = [m for m in common if EnergyPlusPlugin.__dict__[m] != self.__class__.__dict__[m]]
        for known_skip in ['__init__', '__doc__', '__module__']:
            if known_skip in diff:
                diff.remove(known_skip)
        return diff

    # \key BeginNewEnvironment
    # \key AfterNewEnvironmentWarmUpIsComplete
    # \key BeginZoneTimestepBeforeInitHeatBalance
    # \key BeginZoneTimestepAfterInitHeatBalance
    # \key BeginTimestepBeforePredictor
    # \key AfterPredictorBeforeHVACManagers
    # \key AfterPredictorAfterHVACManagers
    # \key InsideHVACSystemIterationLoop
    # \key EndOfZoneTimestepBeforeZoneReporting
    # \key EndOfZoneTimestepAfterZoneReporting
    # \key EndOfSystemTimestepBeforeHVACReporting
    # \key EndOfSystemTimestepAfterHVACReporting
    # \key EndOfZoneSizing
    # \key EndOfSystemSizing
    # \key AfterComponentInputReadIn
    # \key UserDefinedComponentModel
    # \key UnitarySystemSizing