Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Additional fields for Shadowing/Sun Position Calculations Annual Simulations

The existing Shadowing/Sun Position Calculations Annual Simulations report in the eio output has additional field at the end:
External Shading Calculation Method, Output External Shading Calculation Results, Disable Self-Shading Within Shading Zone Groups, Disable Self-Shading From Shading Zone Groups to Other Zones

See [6390](https://github.com/NREL/EnergyPlus/pull/6390)

### Adds additional records of User-Specified component sizing output to SQLite output file. 

In the Component Sizing table report, autosized fields may report values for "Design Size", "User-Specified", or both.  Previously, for fields with both values, only the "Design Size" value was included in the Component Sizing table in the SQLite output.  Now both values are included in the SQLite output.

See [6147](https://github.com/NREL/EnergyPlus/pull/6147)

### Additions and Changes to LEED Summary Report 

A number of changes have been implemented in the LEED Summary tabular report:

- The "EAp2-4/5. Performance Rating Method Compliance" subtable has additional columns additional fuel, district cooling and district heating instead of generic column for "additional use".

- The "EAp2-4/5. Performance Rating Method Compliance" subtable rows are now driven generically by any end-use subcategory provided by the user. The special logic for certain key values used in end-use subcategories has been removed. The number of rows for the table will change depending on the number of subcategories specified in the input file. The rows are identified by the end-use name followed by two dashes followed by the end-use subcategory name.

- The "EAp2-17a. Energy Use Intensity - Electricity" and "EAp2-17b. Energy Use Intensity - Natural Gas" and "EAp2-18. End Use Percentage" employed special logic for certain key values used in end-use subcategories and that special logic has been removed. In order to make that clear, the rows for interior lighting, fans, and miscellaneous energy use now show the word "all" to make it clear that they include all energy for that end-use category.

- A new subtable has been added called "Schedules-Equivalent Full Load Hours (Schedule Type=Fraction)" which shows the equivalent full load hours and the number of hours with values over 1% in a schedule. A row appears for each schedule that uses the (Schedule Type=Fraction).

- A new subtable has been added called "Schedules-SetPoints (Schedule Type=Temperature)" which shows the first setpoint object that uses the schedule, the month assumed, the value at 11am for the first Wednesday for the month assumed, the number of days with those same 11am values, the value at 11pm for the first Wednesday for the month assumed, the number of days with those same 11am values, the number of days with those same 11pm values. This is used to understand the heating and cooling setpoints for typically occupied and unoccupied hours.



See [6150](https://github.com/NREL/EnergyPlus/pull/6150)


