Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc. 
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `UnitarySystemPerformance:Multispeed`

Fields 1-4 remain the same.  
After F4, insert one new blank field that represents the No Load Supply Air Flow Rate Ratio. 
Shift all later fields down by one.


