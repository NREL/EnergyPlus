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

# Object Change: `PerformancePrecisionTradeoffs'

Fields 1-2 (A1, A2) remain the same.

Change in Field 3 (A3): two more key values---Mode06 and Mode07---are added.

Field 4 (N1) remains the same; however it is no longer the last field. See the newly added Field 5 (N2) below.

Newly added Field 5 (N2): MaxAllowedDelTemp, supplied with minimu, maximum, and default values.
