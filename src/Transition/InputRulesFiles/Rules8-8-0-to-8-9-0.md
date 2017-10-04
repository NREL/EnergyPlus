Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: Schedule:Day:Interval

Field A3 Interpolate to Timestep, change value of "Yes" to "Average"

# Object Change: Schedule:Day:List

Field A3 Interpolate to Timestep, change value of "Yes" to "Average"

# Object Change: Schedule:Compact

For any field A3 and later, change values of "Interpolate:Yes" to "Interpolate:Average"


