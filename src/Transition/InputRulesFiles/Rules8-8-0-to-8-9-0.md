Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `ZoneHVAC:EquipmentList`

Field 1 remains the same.  
After F1, insert new alpha input choice field (A2) 'Load Distribution Scheme' with default key input value of 'SequentialLoad'.
Shift all later fields down by 1. The old input fields F2 - Fn become the new F3-Fn. The new input field becomes F2.

# Object Change: Schedule:Day:Interval

Field A3 Interpolate to Timestep, change value of "Yes" to "Average"

# Object Change: Schedule:Day:List

Field A3 Interpolate to Timestep, change value of "Yes" to "Average"

# Object Change: Schedule:Compact

For any field A3 and later, change values of "Interpolate:Yes" to "Interpolate:Average"


