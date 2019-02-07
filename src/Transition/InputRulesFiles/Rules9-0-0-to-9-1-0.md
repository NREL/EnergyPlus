Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `ZoneHVAC:EquipmentList`

Fields F1-F2 remain the same.
Each extensible block has 2 numeric fields added to the end of it, making the extensible block 6 items long instead of 4. 
The additional fields can be left empty. 

