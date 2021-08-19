Input Changes version 9.5.0 to 9.6.0
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: AirflowNetwork:MultiZone:ReferenceCrackConditions 

Summary: The field for *Reference Temperature* was changed to a required-field without a default value. In previous versions, the field for *Reference Temperature* was not a required-field and has a default value of 20.0C. The transition rule sets a value of 20.0C for this field if it was left blank in previous versions.

Field 1 remains the same.
Fields 2 remains the same if not blank or is filled in the value of 20.0C if blank.
Fields 3-4 remain the same.

See [PR#8807](https://github.com/NREL/EnergyPlus/pull/8807)
# Object Change: Coil:Cooling:Water:DetailedGeometry

Summary: Appended new optional numeric input field *Design Water Inlet Temperature*.

Field 1-24 remains the same.
New Field F25 (N18): Design Inlet Water Temperature.  Leave blank or assign a typical chilled water supply temperature.

See [PR#8466](https://github.com/NREL/EnergyPlus/pull/8466)

# Object Change: AirLoopHVAC:OutdoorAirSystem

Summary: The fourth field (Availability Manager name) was being ignored by EnergyPlus other than to verify that it was a valid availability manager (if it wasn't blank).  It was not a required input and E+ wasn't doing anything with it.  It was decided to remove this field to avoid any confusion.

Field 1-3 remain the same.
Field 4 has been eliminated/deleted and not replace with anything else.

See [PR#8884](https://github.com/NREL/EnergyPlus/pull/8884)

# Object Change: PerformancePrecisionTradeoffs
Summary: A new override option including cubic spline interpolations in replacement of original psychrometric function PsyTsatFnPb was added as Mode06 in this object. The previous options for the override mode of the object including Mode06 and Mode07 in version 9.5 were switched to Mode07 and Mode08 in version 9.6, respectively.

Fields 1-2 remain the same.
Fields 3 has been updated by adding a new override mode option. 
Fields 4-5 remains the same.

See [PR#8946](https://github.com/NREL/EnergyPlus/pull/8946)

# Object Change: GroundHeatExchanger:System

Summary: A new field was added to specify the method used to compute the g-function values. 

Fields 1-9 remain the same.
New Field F10 (A7) g-Function Calculation Method. Leave blank or assign a value of 'UHFcalc' or 'UBHWTcalc'. Default value is 'UHFcalc'.
Remaining fields remain the same and are shifted.

See [PR#8708](https://github.com/NREL/EnergyPlus/pull/8708)

# Object Changes: All Base Surface and Internal Gains Objects

Summary: 

1. A new field for Space Name was added after Zone Name to all surface objects. 
2. For all internal gains objects, the "Zone or ZoneList Name" field name was changed to "Zone or ZoneList or Space or SpaceList Name".

## epJSON transition rules
**For People, Lights, ElectricEquipment, GasEquipment, HotWaterEquipment, SteamEquipment, and OtherEquipment:**

Field name: "zone_or_zonelist_name" was changed to "zone_or_zonelist_or_space_or_spacelist_name".

**For BuildingSurface:Detailed, Wall:Detailed, RoofCeiling:Detailed, Floor:Detailed, Wall:Exterior, Wall:Adiabatic,
Wall:Underground, Wall:Interzone, Roof, Ceiling:Adiabatic, Ceiling:Adiabatic, Floor:GroundContact, Floor:Adiabatic,
and Floor:Interzone:**

No transition required. A new optional field "space_name" was added.

**For InternalMass:**

No transition required. A new optional field "space_or_spacelist_name" was added.

## idf transition rules
**For People, Lights, ElectricEquipment, GasEquipment, HotWaterEquipment, SteamEquipment, and OtherEquipment:**

No transition required. Field name "Zone or ZoneList Name" changed to "Zone or ZoneList or Space or SpaceList Name".

**For BuildingSurface:Detailed:**

Fields 1-4 remain the same.
Insert new Field 5 (A5) "Space Name". This field is optional and may be left blank.
Remaining fields remain the same and are shifted by one.

**For Wall:Detailed, RoofCeiling:Detailed, Floor:Detailed, Wall:Exterior, Wall:Adiabatic,
Wall:Underground, Wall:Interzone, Roof, Ceiling:Adiabatic, Ceiling:Adiabatic, Floor:GroundContact, Floor:Adiabatic,
and Floor:Interzone:**

Fields 1-3 remain the same.
Insert new Field 4 (A4) "Space Name". This field is optional and may be left blank.
Remaining fields remain the same and are shifted by one.

**For InternalMass:**

Fields 1-3 remain the same.
Insert new Field 4 (A4) "Space or SpaceList Name". This field is optional and may be left blank.
Remaining fields remain the same and are shifted by one.


See [PR#8394](https://github.com/NREL/EnergyPlus/pull/8394)
