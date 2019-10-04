from ctypes import cdll, c_void_p
from common import RealEP


class BaseThermalPropertySet:

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

    def diffusivity(self):
        return self.api.cBaseThermalPropertySet_diffusivity(self.instance)

    def set_conductivity(self, conductivity: float):
        self.api.cBaseThermalPropertySet_setConductivity(self.instance, conductivity)

