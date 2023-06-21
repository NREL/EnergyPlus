# Enhancing the Usability of the EnergyPlus API

## Document Tracking

- 2023-06-21 Initial draft

## Background

- API activity is continuing to rise, and a new discussion was opened on E+ to perpetually track user activity/requests
- Every API demo I give hints at ways users want to interact and difficulties they encounter. A few aspects have recurred:
  - Being able to tie together multiple simulation engines and keep E+ in sync (out of scope for this -- improved mechanization)
  - Being able to easily deploy and embed E+ into workflows (out of scope for this -- pip install)
  - Being able to create reusable functionality (that's in this one!)
  - Odd nature of hardwired discrete EMS integer values (that's here too!) 

## Problem Statement

I had always hoped that users would develop their own reusable scripts and packages, which is one reason that users provide custom search paths for Python in their input files.
However, API callback functionality is tied very closely to a single input model because of hardwired object names.
Users can create fully functional generic code and then create thin wrappers that tie the functionality to a single model, but this can be improved.

By providing a set of API functions that provide the current input structure to the user, they can directly write generic code in more situations.
Most users still operate on IDF, so providing the already-interpreted input deck as a Python object will lead to a lower bar for writing generic code.

In addition, there are "magic numbers" in the EMS interface which allow certain control strategies or settings to be applied discretely based on integer values.
Internally to EnergyPlus, these are often stored as an enumeration, and as the numbers have meaning, they must be hardcoded there as well.
These are not currently exposed as symbols through the EnergyPlus API, which means user scripts must hardcode these there too.
To alleviate this, it is proposed that an automated method be created which either:
 - programmatically exposes these internal values through symbols in the API, or
 - allows a call into an API function to get symbolic values for certain discrete settings or configurations. 

## Design of API Functions for Accessing Current Inputs

I want to provide an API endpoint that provides input data as Python objects.
Any functions here would belong in the data exchange API group, as it relies on a currently running simulation.
The inputs, whether EpJSON or IDF (or CBOR, or...), are already interpreted into a JSON object in the EnergyPlus C++ at runtime.
The new endpoints would provide methods to access a list of names for object types, for example:

```python
def get_objects_of_type(object_type: str) -> List[str]:
    """
    Returns a list of object names for the given object type argument.
    For example: get_objects_of_type("Zone") would return a list of zone names
    """
```

## Design of EMS Magic Number Solution

The solution here is a bit less clear.

If the agreed solution is to expose the internal enumerations through a set of symbolic constants in the API layer, then
the result may be a small script that scrapes the EnergyPlus code as a part of the libenergyplusapi target and creates an API file full of symbols.
This file would be packaged like all the other API files and users could import the symbols and avoid having to track magic numbers.
This solution could help future proof the user code in case the symbols changed for some reason, although it is doubtful we would make that much of a breaking change anytime soon.
I would expect the symbols to remain an enum class in the core C++ code, but it would still need to be determined how to pass this information to both C and Python.

Consider the before:

```python
api.exchange.set_actuator_value(state, actuator_handle, 3)
```

versus the after:

```python
api.exchange.set_actuator_value(state, actuator_handle, api.constants.operation_mode.cooling)
```

If the agreed upon solution is to create a worker function inside the EnergyPlus API to "mine" out the magic numbers based on symbolic entries, it actually becomes a bit easier.
Any enumeration that is exposed through EMS actuation would need to be given a symbolic class and key name.
Then a single worker function would be created that takes string views of the class and key name.
Internally, there could be a nested switch block that simply returns the enumerated value as an integer back to the user.
The user would then avoid magic numbers.
Consider the before:

```python
api.exchange.set_actuator_value(state, actuator_handle, 3)
```

versus the after:

```python
operation_mode = api.functional.get_enum_value("OperationMode", "Cooling")
api.exchange.set_actuator_value(state, actuator_handle, operation_mode)
```

This would still require a string in the Python code, but feels a ton better than a magic number.