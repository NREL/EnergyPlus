Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: Output:DebuggingData

This object uses to accept numeric entries, where 1 = Yes and all others = No. This was changed to a choice type, accepting 'Yes' or 'No', and defaulting to 'No'.

cf #7740.

# Object Change: Output:Diagnostics

This object was made unique, and with an extensible "Key" field to add specific diagnostics.

cf #7742.

# Minor Changes:

## ZoneProperty:UserViewFactors:BySurfaceName,

The case was changed from `ZoneProperty:UserViewFactors:bySurfaceName` to `ZoneProperty:UserViewFactors:BySurfaceName` to match conventions.
This will have no effect on EnergyPlus which handles this in a case-insensitive manner.

cf #7499.

## Curve:DoubleExponentialDecay,

Two typos were corrected in field names.

9.3.0:

```
  N4 , \field Coefficient3 C4
  N5 , \field Coefficient3 C5
```

9.4.0:

```
  N4 , \field Coefficient4 C4
  N5 , \field Coefficient5 C5
```
