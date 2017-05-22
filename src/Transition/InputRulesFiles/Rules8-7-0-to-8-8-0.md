Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc. 
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `ZoneControl:ContaminantController`

Insert new blank field 6 (A6) (for Maximum Carbon Dioxide Concentration Schedule Name). 
Shift all later fields down by one.

