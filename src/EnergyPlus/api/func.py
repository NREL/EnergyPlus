from ctypes import cdll, c_char_p, c_int, c_void_p
from pyenergyplus.common import RealEP


class BaseThermalPropertySet:
    """
    This class is a Python representation of blablah
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
    This class is a Python representation of blahblah
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


class Functional:
    """
    This API class does blahblahblah
    """

    def __init__(self, api: cdll):
        self.api = api

    def base_struct(self, conductivity: float, density: float, specific_heat: float) -> BaseThermalPropertySet:
        return BaseThermalPropertySet(self.api, conductivity, density, specific_heat)

    def fluid_properties(self, fluid_name: str) -> FluidAndPsychProperties:
        return FluidAndPsychProperties(self.api, fluid_name)

    @staticmethod
    def energyplus_version() -> str:
        return "${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}"
