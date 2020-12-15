Input Changes version 9.4.0 to 9.5.0
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: Construction:AirBoundary

Summary: The fields for *Solar and Daylighting Method* and *Radiant Exchange Method* have been removed. All air boundaries will be modeled using the former "GroupedZones" option. The former options for "InteriorWindow" and "IRTSurface" are no longer available and will generate a transition warning.

Field 1 remains the same.
Fields 2 and 3 are deleted.
Fields 4-6 remain the same, shifting up to new fields 2-4.

See [8370](https://github.com/NREL/EnergyPlus/pull/8370)