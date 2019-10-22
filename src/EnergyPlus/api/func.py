from ctypes import cdll, c_char_p, c_int, c_void_p
from pyenergyplus.common import RealEP


class Glycol:
    """
    This class is a Python representation of the glycol properties calculations inside EnergyPlus.
    For now, the only glycol name allowed is plain water.  This is because other fluids are only
    initialized when they are declared in the input file.  When calling this way, through the API,
    there is no input file, so no other fluids are declared.  This is ripe for a refactor to enable
    additional fluids, but water will suffice for now as an example.
    """

    def __init__(self, api: cdll, glycol_name: bytes):
        self.api = api
        self.api.glycolNew.argtypes = [c_char_p]
        self.api.glycolNew.restype = c_void_p
        self.api.glycolDelete.argtypes = [c_void_p]
        self.api.glycolDelete.restype = c_void_p
        self.api.glycolSpecificHeat.argtypes = [c_void_p, RealEP]
        self.api.glycolSpecificHeat.restype = RealEP
        self.api.glycolDensity.argtypes = [c_void_p, RealEP]
        self.api.glycolDensity.restype = RealEP
        self.api.glycolConductivity.argtypes = [c_void_p, RealEP]
        self.api.glycolConductivity.restype = RealEP
        self.api.glycolViscosity.argtypes = [c_void_p, RealEP]
        self.api.glycolViscosity.restype = RealEP
        self.instance = self.api.glycolNew(glycol_name)

    def __del__(self):
        self.api.glycolDelete(self.instance)

    def specific_heat(self, temperature: float) -> float:
        return self.api.glycolSpecificHeat(self.instance, temperature)

    def density(self, temperature: float) -> float:
        return self.api.glycolDensity(self.instance, temperature)

    def conductivity(self, temperature: float) -> float:
        return self.api.glycolConductivity(self.instance, temperature)

    def viscosity(self, temperature: float) -> float:
        return self.api.glycolViscosity(self.instance, temperature)


class EnergyPlusVersion:
    """
    This is the EnergyPlus version.  Could also call into the DLL but it's the same effect.

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

    def __init__(self, api: cdll, init_ep: bool = True):
        self.api = api
        self.api.initializeFunctionalAPI.argtypes = []
        self.api.initializeFunctionalAPI.restype = c_void_p
        if init_ep:
            self.api.initializeFunctionalAPI()

    def glycol(self, glycol_name: str) -> Glycol:
        """
        Returns a Glycol instance, which allows calculation of glycol properties.

        :param glycol_name: Name of the Glycol, for now only water is allowed
        :return: An instantiated Glycol structure
        """
        if isinstance(glycol_name, str):
            glycol_name = glycol_name.encode('utf-8')
        return Glycol(self.api, glycol_name)

    @staticmethod
    def ep_version() -> EnergyPlusVersion:
        return EnergyPlusVersion()
