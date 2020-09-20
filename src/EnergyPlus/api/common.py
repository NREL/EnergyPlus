from ctypes import c_double
from numbers import Number

# This is an alias to the EnergyPlus floating point type.
# If EnergyPlus were to go to single precision, we may
# need to change this to c_float to allow marshalling data properly
RealEP = c_double


class EnergyPlusException(Exception):
    pass


def is_number(obj) -> bool:
    """
    Check if the python object is a number.
    Returns True when the object is a number, and False if is not.
    Parameters
    ----------
    obj : any type
        The object to check if is a number.

    Returns
    -------
    is_number : bool
        Whether `obj` is a number or not.

    Examples
    --------
    >>> is_number(1)
    True
    >>> is_number(7.15)
    True
    Booleans are normally valid because they are int subclass, but we filter
    them out here
    >>> is_number(False)
    False
    >>> is_number("foo")
    False
    >>> is_number("5")
    False
    >>> is_number(RealEP(5))
    True
    This also works with numpy.number types
    >>> is_number(np.float64(10.0))
    True
    """

    return isinstance(obj, (Number, RealEP)) and not isinstance(obj, bool)
