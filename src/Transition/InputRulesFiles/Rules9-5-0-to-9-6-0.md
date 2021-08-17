Input Changes version 9.5.0 to 9.6.0
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: AirflowNetwork:MultiZone:ReferenceCrackConditions 

Summary: The field for *Reference Temperature* was changed to a required-field without a default value. In previous versions, the field for *Reference Temperature* was not a required-field and has a default value of 20.0C. The transition rule sets a value of 20.0C for this field if it was left blank in previous versions.

Field 1 remains the same.
Fields 2 remains the same if not blank or is filled in the value of 20.0C if blank.
Fields 3-4 remain the same.

See [8807](https://github.com/NREL/EnergyPlus/pull/8807)
# Object Change: Coil:Cooling:Water:DetailedGeometry

Summary: Appended new optional numeric input field *Design Water Inlet Temperature*.

Field 1-24 remains the same.
New Field F25 (N18): Design Inlet Water Temperature.  Leave blank or assign a typical chilled water supply temperature.

See [8466](https://github.com/NREL/EnergyPlus/pull/8466)

# Object Change: AirLoopHVAC:OutdoorAirSystem

Summary: The fourth field (Availability Manager name) was being ignored by EnergyPlus other than to verify that it was a valid availability manager (if it wasn’t blank).  It was not a required input and E+ wasn’t doing anything with it.  It was decided to remove this field to avoid any confusion.

Field 1-3 remain the same.
Field 4 has been eliminated/deleted and not replace with anything else.

See [8884](https://github.com/NREL/EnergyPlus/pull/8884)

# Object Change: PerformancePrecisionTradeoffs
Summary: A new override option including cubic spline interpolations in replacement of original psychrometric function PsyTsatFnPb was added as Mode06 in this object. The previous options for the override mode of the object including Mode06 and Mode07 in version 9.5 were switched to Mode07 and Mode08 in version 9.6, respectively.

Fields 1-2 remains the same.
Fields 3 has been updated by adding a new override mode option. 
Fields 4-5 remains the same.

See [8946](https://github.com/NREL/EnergyPlus/pull/8946)