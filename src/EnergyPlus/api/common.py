from ctypes import c_double


# This is an alias to the EnergyPlus floating point type.  If EnergyPlus were to go to single precision, we may
# need to change this to c_float to allow marshalling data properly
RealEP = c_double


class EnergyPlusException(Exception):
    pass
