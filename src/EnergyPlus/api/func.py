from ctypes import cdll, c_int, c_char_p, c_void_p, CFUNCTYPE
from types import FunctionType
from pyenergyplus.common import RealEP

# CFUNCTYPE wrapped Python callbacks need to be kept in memory explicitly, otherwise GC takes it
# This causes undefined behavior but generally segfaults and illegal access violations
# Keeping them referenced in a global array here suffices to keep GC away from it
# And while we _could_ clean up after ourselves, I would imagine users aren't making *so* many callbacks that
# We have anything to worry about here
error_callbacks = []


class Glycol:
    """
    This class provides access to the glycol property calculations inside EnergyPlus.
    For now, the only glycol name allowed is plain water.  This is because other fluids are only
    initialized when they are declared in the input file.  When calling through the API,
    there is no input file, so no other fluids are declared.  This is ripe for a refactor to enable
    additional fluids, but water will suffice for now.
    """

    def __init__(self, state: c_void_p, api: cdll, glycol_name: bytes):
        """
        Creates a new Glycol instance, should almost certainly always be called from the API's Functional class, not
        directly from user code.  To get a Glycol instance from client code, call api.functional.glycol("name"),
        where state is an active EnergyPlus state returned from a call to `api.state_manager.new_state()`.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`
        :param api: An active CTYPES CDLL instance
        :param glycol_name: The name of the glycol to instantiate -- currently only "water" is supported.
        """
        self.api = api
        self.api.glycolNew.argtypes = [c_void_p, c_char_p]
        self.api.glycolNew.restype = c_void_p
        self.api.glycolDelete.argtypes = [c_void_p, c_void_p]
        self.api.glycolDelete.restype = c_void_p
        self.api.glycolSpecificHeat.argtypes = [c_void_p, c_void_p, RealEP]
        self.api.glycolSpecificHeat.restype = RealEP
        self.api.glycolDensity.argtypes = [c_void_p, c_void_p, RealEP]
        self.api.glycolDensity.restype = RealEP
        self.api.glycolConductivity.argtypes = [c_void_p, c_void_p, RealEP]
        self.api.glycolConductivity.restype = RealEP
        self.api.glycolViscosity.argtypes = [c_void_p, c_void_p, RealEP]
        self.api.glycolViscosity.restype = RealEP
        self.instance = self.api.glycolNew(glycol_name)

    def delete(self, state: c_void_p) -> None:
        """
        Frees the memory of the associated Glycol instance inside the EnergyPlus (C++) state.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :return: Nothing
        """
        self.api.glycolDelete(self.instance)

    def specific_heat(self, state: c_void_p, temperature: float) -> float:
        """
        Returns the specific heat of the fluid at the specified temperature.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`
        :param temperature: Fluid temperature, in degrees Celsius
        :return: The specific heat of the fluid, in J/kg-K
        """
        return self.api.glycolSpecificHeat(self.instance, temperature)

    def density(self, state: c_void_p, temperature: float) -> float:
        """
        Returns the density of the fluid at the specified temperature.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`
        :param temperature: Fluid temperature, in degrees Celsius
        :return: The density of the fluid, in kg/m3
        """
        return self.api.glycolDensity(self.instance, temperature)

    def conductivity(self, state: c_void_p, temperature: float) -> float:
        """
        Returns the conductivity of the fluid at the specified temperature.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`
        :param temperature: Fluid temperature, in degrees Celsius
        :return: The conductivity of the fluid, in W/m-K
        """
        return self.api.glycolConductivity(self.instance, temperature)

    def viscosity(self, state: c_void_p, temperature: float) -> float:
        """
        Returns the dynamic viscosity of the fluid at the specified temperature.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`
        :param temperature: Fluid temperature, in degrees Celsius
        :return: The dynamic viscosity of the fluid, in Pa-s (or kg/m-s)
        """
        return self.api.glycolViscosity(self.instance, temperature)


class Refrigerant:
    """
    This class provides access to the refrigerant property calculations inside EnergyPlus.
    For now, the only refrigerant  name allowed is steam.  This is because other refrigerants are only
    initialized when they are declared in the input file.  When calling through the API, there is no
    input file, so no other refrigerants are declared.  This should be improved through later enhancements,
    but steam will provide a suitable use case for now.
    """

    def __init__(self, state: c_void_p, api: cdll, refrigerant_name: bytes):
        """
        Creates a new Refrigerant instance, should almost certainly always be called from the API's functional class,
        not directly from user code.  To get a Refrigerant instance from client code, call
        api.functional.refrigerant("name"), where state is an active EnergyPlus state returned from a call to
        `api.state_manager.new_state()`.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`
        :param api: An active CTYPES CDLL instance
        :param refrigerant_name: The name of the refrigerant to instantiate -- currently only "steam" is supported.
        """
        self.refrigerant_name = refrigerant_name
        self.api = api
        self.api.refrigerantNew.argtypes = [c_void_p, c_char_p]
        self.api.refrigerantNew.restype = c_void_p
        self.api.refrigerantDelete.argtypes = [c_void_p, c_void_p]
        self.api.refrigerantDelete.restype = c_void_p
        self.api.refrigerantSaturationPressure.argtypes = [c_void_p, c_void_p, RealEP]
        self.api.refrigerantSaturationPressure.restype = RealEP
        self.api.refrigerantSaturationTemperature.argtypes = [c_void_p, c_void_p, RealEP]
        self.api.refrigerantSaturationTemperature.restype = RealEP
        self.api.refrigerantSaturatedEnthalpy.argtypes = [c_void_p, c_void_p, RealEP, RealEP]
        self.api.refrigerantSaturatedEnthalpy.restype = RealEP
        self.api.refrigerantSaturatedDensity.argtypes = [c_void_p, c_void_p, RealEP, RealEP]
        self.api.refrigerantSaturatedDensity.restype = RealEP
        self.api.refrigerantSaturatedSpecificHeat.argtypes = [c_void_p, c_void_p, RealEP, RealEP]
        self.api.refrigerantSaturatedSpecificHeat.restype = RealEP
        self.instance = self.api.refrigerantNew(refrigerant_name)

    def delete(self, state: c_void_p):
        """
        Frees the memory of the associated Refrigerant instance inside the EnergyPlus (C++) state.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :return: Nothing
        """
        self.api.refrigerantDelete(self.instance)

    def saturation_pressure(self, state: c_void_p, temperature: float) -> float:
        """
        Returns the saturation pressure of the refrigerant at the specified temperature.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param temperature: Refrigerant temperature, in Celsius.
        :return: Refrigerant saturation pressure, in Pa
        """
        return self.api.refrigerantSaturationPressure(self.instance, temperature)

    def saturation_temperature(self, state: c_void_p, pressure: float) -> float:
        """
        Returns the saturation temperature of the refrigerant at the specified pressure.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param pressure: Refrigerant pressure, in Pa
        :return: Refrigerant saturation temperature, in Celsius
        """
        return self.api.refrigerantSaturationTemperature(self.instance, pressure)

    def saturated_enthalpy(self, state: c_void_p, temperature: float, quality: float) -> float:
        """
        Returns the refrigerant saturated enthalpy at the specified temperature and quality.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param temperature: Refrigerant temperature, in Celsius
        :param quality: Refrigerant quality, in fractional form from 0.0 to 1.0
        :return: Refrigerant saturated enthalpy, in J/kg
        """
        return self.api.refrigerantSaturatedEnthalpy(self.instance, temperature, quality)

    def saturated_density(self, state: c_void_p, temperature: float, quality: float) -> float:
        """
        Returns the refrigerant density at the specified temperature and quality.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param temperature: Refrigerant temperature, in Celsius
        :param quality: Refrigerant quality, in fractional form from 0.0 to 1.0
        :return: Refrigerant saturated density, in kg/m3
        """
        return self.api.refrigerantSaturatedDensity(self.instance, temperature, quality)

    def saturated_specific_heat(self, state: c_void_p, temperature: float, quality: float) -> float:
        """
        Returns the refrigerant specific heat at the specified temperature and quality.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param temperature: Refrigerant temperature, in Celsius
        :param quality: Refrigerant quality, in fractional form from 0.0 to 1.0
        :return: Refrigerant saturated specific heat, in J/kg-K
        """
        return self.api.refrigerantSaturatedSpecificHeat(self.instance, temperature, quality)


class Psychrometrics:
    """
    This class provides access to the psychrometric functions within EnergyPlus.  Some property calculations are
    available as functions of different independent variable combinations, leading to suffixed function names, such as
    `vapor_density_b` and `relative_humidity_c`.
    """

    def __init__(self, api: cdll):
        """
        Creates a new Psychrometrics instance, should almost certainly always be called from the API's functional class,
        not directly from user code.  To get a Psychrometrics instance from client code, call
        api.functional.psychrometrics(), where state is an active EnergyPlus state returned from a call to
        `api.state_manager.new_state()`.

        :param api: An active CTYPES CDLL instance
        """
        self.api = api
        self.api.psyRhoFnPbTdbW.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.psyRhoFnPbTdbW.restype = RealEP
        self.api.psyHfgAirFnWTdb.argtypes = [c_void_p, RealEP]
        self.api.psyHfgAirFnWTdb.restype = RealEP
        self.api.psyHgAirFnWTdb.argtypes = [c_void_p, RealEP]
        self.api.psyHgAirFnWTdb.restype = RealEP
        self.api.psyHFnTdbW.argtypes = [c_void_p, RealEP, RealEP]
        self.api.psyHFnTdbW.restype = RealEP
        self.api.psyCpAirFnW.argtypes = [c_void_p, RealEP]
        self.api.psyCpAirFnW.restype = RealEP
        self.api.psyTdbFnHW.argtypes = [c_void_p, RealEP, RealEP]
        self.api.psyTdbFnHW.restype = RealEP
        self.api.psyRhovFnTdbWPb.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.psyRhovFnTdbWPb.restype = RealEP
        self.api.psyTwbFnTdbWPb.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.psyTwbFnTdbWPb.restype = RealEP
        self.api.psyVFnTdbWPb.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.psyVFnTdbWPb.restype = RealEP
        self.api.psyWFnTdbH.argtypes = [c_void_p, RealEP, RealEP]
        self.api.psyWFnTdbH.restype = RealEP
        self.api.psyPsatFnTemp.argtypes = [c_void_p, RealEP]
        self.api.psyPsatFnTemp.restype = RealEP
        self.api.psyTsatFnHPb.argtypes = [c_void_p, RealEP, RealEP]
        self.api.psyTsatFnHPb.restype = RealEP
        self.api.psyRhovFnTdbRh.argtypes = [c_void_p, RealEP, RealEP]
        self.api.psyRhovFnTdbRh.restype = RealEP
        self.api.psyRhFnTdbRhov.argtypes = [c_void_p, RealEP, RealEP]
        self.api.psyRhFnTdbRhov.restype = RealEP
        self.api.psyRhFnTdbWPb.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.psyRhFnTdbWPb.restype = RealEP
        self.api.psyWFnTdpPb.argtypes = [c_void_p, RealEP, RealEP]
        self.api.psyWFnTdpPb.restype = RealEP
        self.api.psyWFnTdbRhPb.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.psyWFnTdbRhPb.restype = RealEP
        self.api.psyWFnTdbTwbPb.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.psyWFnTdbTwbPb.restype = RealEP
        self.api.psyHFnTdbRhPb.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.psyHFnTdbRhPb.restype = RealEP
        self.api.psyTdpFnWPb.argtypes = [c_void_p, RealEP, RealEP]
        self.api.psyTdpFnWPb.restype = RealEP
        self.api.psyTdpFnTdbTwbPb.argtypes = [c_void_p, RealEP, RealEP, RealEP]
        self.api.psyTdpFnTdbTwbPb.restype = RealEP

    def density(self, state: c_void_p, barometric_pressure: float, dry_bulb_temp: float, humidity_ratio: float) -> float:
        """
        Returns the psychrometric density at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param barometric_pressure: Barometric pressure, in Pa
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param humidity_ratio: Humidity ratio, in kgWater/kgDryAir
        :return:
        """
        return self.api.psyRhoFnPbTdbW(barometric_pressure, dry_bulb_temp, humidity_ratio)

    def latent_energy_of_air(self, state: c_void_p, dry_bulb_temp: float) -> float:
        """
        Returns the psychrometric latent energy of air at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :return:
        """
        return self.api.psyHfgAirFnWTdb(dry_bulb_temp)

    def latent_energy_of_moisture_in_air(self, state: c_void_p, dry_bulb_temp: float) -> float:
        """
        Returns the psychrometric latent energy of the moisture in air at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :return:
        """
        return self.api.psyHgAirFnWTdb(dry_bulb_temp)

    def enthalpy(self, state: c_void_p, dry_bulb_temp: float, humidity_ratio: float) -> float:
        """
        Returns the psychrometric enthalpy at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param humidity_ratio: Humidity ratio, in kgWater/kgDryAir
        :return:
        """
        return self.api.psyHFnTdbW(dry_bulb_temp, humidity_ratio)

    def enthalpy_b(self, state: c_void_p, dry_bulb_temp: float, relative_humidity_fraction: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric enthalpy at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param relative_humidity_fraction: Psychrometric relative humidity, as a fraction from 0.0 to 1.0.
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyHFnTdbRhPb(dry_bulb_temp, relative_humidity_fraction, barometric_pressure)

    def specific_heat(self, state: c_void_p, humidity_ratio: float) -> float:
        """
        Returns the psychrometric specific heat at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param humidity_ratio: Humidity ratio, in kgWater/kgDryAir
        :return:
        """
        return self.api.psyCpAirFnW(humidity_ratio)

    def dry_bulb(self, state: c_void_p, enthalpy: float, humidity_ratio: float) -> float:
        """
        Returns the psychrometric dry bulb temperature at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param enthalpy: Psychrometric enthalpy, in J/kg
        :param humidity_ratio: Humidity ratio, in kgWater/kgDryAir
        :return:
        """
        return self.api.psyTdbFnHW(enthalpy, humidity_ratio)

    def vapor_density(self, state: c_void_p, dry_bulb_temp: float, humidity_ratio: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric vapor density at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param humidity_ratio: Humidity ratio, in kgWater/kgDryAir
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyRhovFnTdbWPb(dry_bulb_temp, humidity_ratio, barometric_pressure)

    def relative_humidity(self, state: c_void_p, dry_bulb_temp: float, vapor_density: float) -> float:
        """
        Returns the psychrometric relative humidity at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param vapor_density: Psychrometric vapor density, in kg/m3
        :return:
        """
        return self.api.psyRhFnTdbRhov(dry_bulb_temp, vapor_density)

    def relative_humidity_b(self, state: c_void_p, dry_bulb_temp: float, humidity_ratio: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric relative humidity at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param humidity_ratio: Humidity ratio, in kgWater/kgDryAir
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyRhFnTdbWPb(dry_bulb_temp, humidity_ratio, barometric_pressure)

    def wet_bulb(self, state: c_void_p, dry_bulb_temp: float, humidity_ratio: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric wet bulb temperature at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param humidity_ratio: Humidity ratio, in kgWater/kgDryAir
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyTwbFnTdbWPb(dry_bulb_temp, humidity_ratio, barometric_pressure)

    def specific_volume(self, state: c_void_p, dry_bulb_temp: float, humidity_ratio: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric specific volume at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param humidity_ratio: Humidity ratio, in kgWater/kgDryAir
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyVFnTdbWPb(dry_bulb_temp, humidity_ratio, barometric_pressure)

    def saturation_pressure(self, state: c_void_p, dry_bulb_temp: float) -> float:
        """
        Returns the psychrometric saturation pressure at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :return:
        """
        return self.api.psyPsatFnTemp(dry_bulb_temp)

    def saturation_temperature(self, state: c_void_p, enthalpy: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric saturation temperature at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param enthalpy: Psychrometric enthalpy, in J/kg
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyTsatFnHPb(enthalpy, barometric_pressure)

    def vapor_density_b(self, state: c_void_p, dry_bulb_temp: float, relative_humidity_fraction: float) -> float:
        """
        Returns the psychrometric vapor density at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param relative_humidity_fraction: Psychrometric relative humidity, as a fraction from 0.0 to 1.0.
        :return:
        """
        return self.api.psyRhovFnTdbRh(dry_bulb_temp, relative_humidity_fraction)

    def humidity_ratio(self, state: c_void_p, dry_bulb_temp: float, enthalpy: float) -> float:
        """
        Returns the psychrometric humidity ratio at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param enthalpy: Psychrometric enthalpy, in J/kg
        :return:
        """
        return self.api.psyWFnTdbH(dry_bulb_temp, enthalpy)

    def humidity_ratio_b(self, state: c_void_p, dew_point_temp: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric humidity ratio at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dew_point_temp: Psychrometric dew point temperature, in Celsius
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyWFnTdpPb(dew_point_temp, barometric_pressure)

    def humidity_ratio_c(self, state: c_void_p, dry_bulb_temp: float, relative_humidity_fraction: float,
                         barometric_pressure: float) -> float:
        """
        Returns the psychrometric humidity ratio at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param relative_humidity_fraction: Psychrometric relative humidity, as a fraction from 0.0 to 1.0.
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyWFnTdbRhPb(dry_bulb_temp, relative_humidity_fraction, barometric_pressure)

    def humidity_ratio_d(self, state: c_void_p, dry_bulb_temp: float, wet_bulb_temp: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric humidity ratio at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param wet_bulb_temp: Psychrometric wet bulb temperature, in C
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyWFnTdbTwbPb(dry_bulb_temp, wet_bulb_temp, barometric_pressure)

    def dew_point(self, state: c_void_p, humidity_ratio: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric dew point temperature at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param humidity_ratio: Humidity ratio, in kgWater/kgDryAir
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyTdpFnWPb(humidity_ratio, barometric_pressure)

    def dew_point_b(self, state: c_void_p, dry_bulb_temp: float, wet_bulb_temp: float, barometric_pressure: float) -> float:
        """
        Returns the psychrometric dew point temperature at the specified conditions.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param dry_bulb_temp: Psychrometric dry bulb temperature, in C
        :param wet_bulb_temp: Psychrometric wet bulb temperature, in C
        :param barometric_pressure: Barometric pressure, in Pa
        :return:
        """
        return self.api.psyTdpFnTdbTwbPb(dry_bulb_temp, wet_bulb_temp, barometric_pressure)


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
        """
        Returns a string representation of this EnergyPlus version.
        :return: EnergyPlus version, as major.minor.patch-build
        """
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
    use the `functional` member variable to access a Functional class instance.  For Python Plugin workflows, the
    EnergyPlusPlugin base class also provides an instance of the Functional base class through the `self.api.functional`
    member variable.  Clients should use that directly when needing to make functional calls into the library.
    """

    def __init__(self, api: cdll, running_as_python_plugin: bool = False):
        self.api = api
        self.api.initializeFunctionalAPI.argtypes = [c_void_p]
        self.api.initializeFunctionalAPI.restype = c_void_p
        self.initialized = False
        self.plugin_mode = running_as_python_plugin
        self.py_error_callback_type = CFUNCTYPE(c_void_p, c_int, c_char_p)
        self.api.registerErrorCallback.argtypes = [c_void_p, self.py_error_callback_type]
        self.api.registerErrorCallback.restype = c_void_p

    def initialize(self, state: c_void_p) -> None:
        if not self.initialized and not self.plugin_mode:
            self.api.initializeFunctionalAPI()
            self.initialized = True

    def glycol(self, state: c_void_p, glycol_name: str) -> Glycol:
        """
        Returns a Glycol instance, which allows calculation of glycol properties.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param glycol_name: Name of the Glycol, for now only water is allowed
        :return: An instantiated Glycol structure
        """
        self.initialize()
        if isinstance(glycol_name, str):
            glycol_name = glycol_name.encode('utf-8')
        return Glycol(self.api, glycol_name)

    def refrigerant(self, state: c_void_p, refrigerant_name: str) -> Refrigerant:
        """
        Returns a Refrigerant instance, which allows calculation of refrigerant properties.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param refrigerant_name: Name of the Refrigerant, for now only steam is allowed
        :return: An instantiated Refrigerant structure
        """
        self.initialize()
        if isinstance(refrigerant_name, str):
            refrigerant_name = refrigerant_name.encode('utf-8')
        return Refrigerant(self.api, refrigerant_name)

    def psychrometrics(self, state: c_void_p) -> Psychrometrics:
        """
        Returns a Psychrometric instance, which allows calculation of psychrometric properties.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :return: An instantiated Psychrometric structure
        """
        self.initialize()
        return Psychrometrics(self.api)

    @staticmethod
    def ep_version() -> EnergyPlusVersion:
        return EnergyPlusVersion()

    # TODO: Move this to the runtime API section?
    def callback_error(self, state, f: FunctionType) -> None:
        """
        This function allows a client to register a function to be called back by EnergyPlus when an error message
        is added to the error file.  The user can then detect specific error messages or whatever.

        :param state: An active EnergyPlus "state" that is returned from a call to `api.state_manager.new_state()`.
        :param f: A python function which takes an integer severity and a string (bytes) argument and returns nothing
        :return: Nothing
        """
        cb_ptr = self.py_error_callback_type(f)
        error_callbacks.append(cb_ptr)
        self.api.registerErrorCallback(cb_ptr)

    @staticmethod
    def clear_callbacks() -> None:
        """
        This function is only used if you are running this script continually making many calls into the E+ library in
        one thread, each with many new and different error handler callbacks, and you need to clean up.

        Note this will affect all current instances in this thread, so use carefully!

        :return: Nothing
        """
        error_callbacks.clear()
