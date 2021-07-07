Input Changes version 9.4.0 to 9.5.0
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: AirflowNetwork:MultiZone:ReferenceCrackConditions 

Summary: The field for *Reference Temperature* was changed to a required-field without a default value. In previous versions, the field for *Reference Temperature* was not a required-field and has a default value of 20.0C. The transition rule sets a value of 20.0C for this field if it was left blank in previous versions.

Field 1 remains the same.
Fields 2 remains the same if not blank or is filled in the value of 20.0C if blank.
Fields 3-4 remain the same.

See [8807](https://github.com/NREL/EnergyPlus/pull/8807)
