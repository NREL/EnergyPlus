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

# Object Change: Construction:InternalSource to ConstructionProperty:InternalHeatSource and Construction

Summary: Construction:InternalSource has been changed to ConstructionProperty:InternalHeatSource. The new object no longer contains the material list, but references a standard Construction object instead.

Old fields are from the incoming Construction:InternalSource object.

New Object ConstructionProperty:InternalHeatSource
New Field 1 (new A1) is modified by adding a suffix to the end "Heat Source".
Field 2 (new A2) is the old Field 1 (old A1)
Fields 3 thru 7 (new N1:N5) are the old Fields 2:6 (old N1:N5)

*The new Construction:InternalHeatSource object has 7 fields.*

New Object Construction
New Field 1 (new A1) is the old Field 1 (old A1)
Fields 2:m (new A2:Am) are the old Fields 7:n (old A2:An)

*If the incoming object has n fields, then the new Construction object will have n-5 fields.*

See [8442](https://github.com/NREL/EnergyPlus/pull/8442)
