from ctypes import cdll, c_char_p, c_int, c_void_p
from pyenergyplus.common import RealEP


class BaseThermalPropertySet:
    """
    This class is a Python representation of blablah - DONT WORRY ABOUT DOCUMENTING FOR NOW
    """

    def __init__(self, api: cdll, conductivity: float, density: float, specific_heat: float):
        self.api = api
        self.api.newCBaseThermalPropertySet.argtypes = [RealEP, RealEP, RealEP]
        self.api.newCBaseThermalPropertySet.restype = c_void_p
        self.api.delCBaseThermalPropertySet.argtypes = [c_void_p]
        self.api.delCBaseThermalPropertySet.restype = c_void_p
        self.api.cBaseThermalPropertySet_diffusivity.argtypes = [c_void_p]
        self.api.cBaseThermalPropertySet_diffusivity.restype = RealEP
        self.api.cBaseThermalPropertySet_setConductivity.argtypes = [c_void_p, RealEP]
        self.api.cBaseThermalPropertySet_setConductivity.restype = c_void_p
        self.instance = self.api.newCBaseThermalPropertySet(conductivity, density, specific_heat)

    def __del__(self):
        self.api.delCBaseThermalPropertySet(self.instance)

    def diffusivity(self) -> float:
        return self.api.cBaseThermalPropertySet_diffusivity(self.instance)

    def set_conductivity(self, conductivity: float) -> None:
        self.api.cBaseThermalPropertySet_setConductivity(self.instance, conductivity)


class FluidAndPsychProperties:
    """
    This class is a Python representation of blahblah - DONT WORRY ABOUT DOCUMENTING FOR NOW, SPLIT FOR PSYCH/FLUID
    """

    def __init__(self, api: cdll, fluid_name: str):
        self.api = api
        self.fluid_name = fluid_name

        self.api.initializeFunctionalAPI.argtypes = []
        self.api.initializeFunctionalAPI.restype = c_void_p
        self.api.fluidProperty_GetSatPressureRefrig.argtypes = [c_char_p, RealEP, c_int]
        self.api.fluidProperty_GetSatPressureRefrig.restype = RealEP

        self.api.initializeFunctionalAPI()

    def get_sat_press_refrigerant(self) -> float:
        index = 0
        val = self.api.fluidProperty_GetSatPressureRefrig(self.fluid_name.encode('utf-8'), 25.5, index)
        return float(val)


class EnergyPlusVersion:
    """
    Could also just call into the DLL and get the version
    """
    def __init__(self):
        self.ep_version_major = int("${CMAKE_VERSION_MAJOR}")
        self.ep_version_minor = int("${CMAKE_VERSION_MINOR}")
        self.ep_version_patch = int("${CMAKE_VERSION_PATCH}")
        self.ep_version_build = str("${CMAKE_VERSION_BUILD}")

    def __str__(self) -> str:
        return "%s.%s.%s-%s" % (
            self.ep_version_major, self.ep_version_minor, self.ep_version_patch, self.ep_version_build
        )


class Functional:
    """
    This API class enables accessing structures and functionality inside EnergyPlus from an outside client.
    This functional API will be extended over time, but initial targeted functionality includes fluid and refrigerant
    property methods, and surface and geometry classes and methods.

    The Functional API class itself is really just an organizational class that provides access to nested functional
    classes through member functions.  The functional API class is instantiated by the higher level EnergyPlusAPI class,
    and clients should *never* attempt to create an instance manually.  Instead, create an EnergyPlusAPI instance, and
    call the `functional()` member function to create a Functional class instance.  For Python Plugin workflows, the
    EnergyPlusPlugin base class also provides an instance of the Functional base class through the `self.functional`
    member variable.  Clients should use that directly when needing to make functional calls into the library.
    """

    def __init__(self, api: cdll):
        self.api = api

    def base_struct(self, conductivity: float, density: float, specific_heat: float) -> BaseThermalPropertySet:
        """
        Returns a BaseThermalPropertySet instance, which is just a collection of thermal properties.

        :param conductivity: Thermal conductivity, W/m-K
        :param density: Density, kg/m3
        :param specific_heat: Specific Heat, J/kg-K
        :return: An instantiated thermal property structure, ready for use with the initial properties
        """
        return BaseThermalPropertySet(self.api, conductivity, density, specific_heat)

    def fluid_properties(self, fluid_name: str) -> FluidAndPsychProperties:
        """
        Returns a FluidAndPsychProperties instance, which collects fluid property calculation methods

        :param fluid_name: The name of the fluid to instantiate; EnergyPlus only allows a few built in: STEAM...
        :return: An instantiated fluid properties structure created for the given fluid name
        """
        return FluidAndPsychProperties(self.api, fluid_name)

    @staticmethod
    def ep_version() -> EnergyPlusVersion:
        return EnergyPlusVersion()
