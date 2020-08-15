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

    def _detect_overridden(self) -> List[str]:
        """
        This function allows for detection of methods that are overridden in derived classes.
        It first collects all the members of the class which share the same name into a single list.
        It then looks up each member name in the EnergyPlusPlugin base class dictionary and the current instance
        dictionary.  If they share the same address, they are the same function, and the derived class must not
        be overriding the base class version.  If they have different addresses, the derived class is overriding it.

        :return: A list of function names which are overridden in the derived class instance of this base class
        """
        self.data['i'] = 12
        common = EnergyPlusPlugin.__dict__.keys() & self.__class__.__dict__.keys()
        diff = [m for m in common if EnergyPlusPlugin.__dict__[m] != self.__class__.__dict__[m]]
        for known_skip in ['__init__', '__doc__', '__module__']:
            if known_skip in diff:
                diff.remove(known_skip)
        return diff

    def on_begin_new_environment(self, state) -> int:
        pass

    def on_after_new_environment_warmup_is_complete(self, state) -> int:
        pass

    def on_begin_zone_timestep_before_init_heat_balance(self, state) -> int:
        pass

    def on_begin_zone_timestep_after_init_heat_balance(self, state) -> int:
        pass

    def on_begin_timestep_before_predictor(self, state) -> int:
        pass

    def on_after_predictor_before_hvac_managers(self, state) -> int:
        pass

    def on_after_predictor_after_hvac_managers(self, state) -> int:
        pass

    def on_inside_hvac_system_iteration_loop(self, state) -> int:
        pass

    def on_end_of_zone_timestep_before_zone_reporting(self, state) -> int:
        pass

    def on_end_of_zone_timestep_after_zone_reporting(self, state) -> int:
        pass

    def on_end_of_system_timestep_before_hvac_reporting(self, state) -> int:
        pass

    def on_end_of_system_timestep_after_hvac_reporting(self, state) -> int:
        pass

    def on_end_of_zone_sizing(self, state) -> int:
        pass

    def on_end_of_system_sizing(self, state) -> int:
        pass

    def on_end_of_component_input_read_in(self, state) -> int:
        pass

    def on_user_defined_component_model(self, state) -> int:
        pass

    def on_unitary_system_sizing(self, state) -> int:
        pass
