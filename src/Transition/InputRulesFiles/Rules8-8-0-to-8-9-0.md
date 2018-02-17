Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `ZoneHVAC:EquipmentList`

Field 1 remains the same.  
After F1, insert new alpha input choice field (A2) 'Load Distribution Scheme' with default key input value of 'SequentialLoad'.
Shift all later fields down by 1. The old input fields F2 - Fn become the new F3-Fn+1. The new input field becomes F2.

# Object Change: Schedule:Day:Interval

Field A3 Interpolate to Timestep, change value of "Yes" to "Average"

# Object Change: Schedule:Day:List

Field A3 Interpolate to Timestep, change value of "Yes" to "Average"

# Object Change: Schedule:Compact

For any field A3 and later, change values of "Interpolate:Yes" to "Interpolate:Average"

# Object Change: ElectricEquipment:ITE:AirCooled
Fields 1 and 2 remain tha same
After F2, insert new alpha input choice field (A3) 'Air Flow Calculation Method' with default kay value 'FlowFromSystem'.
Shift all later fields down by 1. The old input fields F3 - Fn become the new F4-Fn+1. The new input field becomes F3.
At the end, there are four new fields, but these don't need to be added - they are required only when using the new option
introduced in new field A3.


