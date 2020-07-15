Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: ‘Construction:InternalSource’

Summary: A new field was added into the middle of the object, everything else states the same (or shifted back if occurring after the new field).

Fields 1-5 remain the same.

NEW Field F6 (N5): Two-Dimensional Position of Interior Temperature Calculation Request.  Leave blank or assign a zero value (default).

Old Fields 6-15 remain the same, just shifted back one field to Fields 7-16.

# Object Change: 'ZoneHVAC:HybridUnitaryHVAC'

Fields 1-14 remain the same.

Insert field 15, Fan Heat Included in Lookup Tables.

Insert field 16, Fan Heat Gain Location.

Insert field 17, Fan Heat Gain In Airstream Fraction.

Field 18, previous field 15. No other changes.

Field 19, previous field 17. No other changes.

Remaining fields 20 onward are the same as previous fields 18 onwards. No other changes.

# Object Change: Output:DebuggingData

This object uses to accept numeric entries, where 1 = Yes and all others = No. This was changed to a choice type, accepting 'Yes' or 'No', and defaulting to 'No'.

cf #7740.

# Object Change: Output:Diagnostics

This object was made unique, and with an extensible "Key" field to add specific diagnostics.

cf #7742.

# Object Change: ‘ZoneHVAC:LowTemperatureRadiant:VariableFlow’

Summary: A new field was added into the middle of the object, everything else states the same (or shifted back if occurring after the new field).

Fields 1-7 remain the same.

NEW Field F8 (A6): Setpoint Control Type.  Leave blank or assign a value of “HalfFlowPower” (default).

Old Fields 8-29 remain the same, just shifted back one field to Fields 9-30.

# Object Change: ‘ZoneHVAC:LowTemperatureRadiant:Electric’

Summary: A new field was added into the middle of the object, everything else states the same (or shifted back if occurring after the new field).

Fields 1-9 remain the same.

NEW Field F10 (A7): Setpoint Control Type.  Leave blank or assign a value of “HalfFlowPower” (default).

Old Fields 10-11 remain the same, just shifted back one field to Fields 11-12.

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
