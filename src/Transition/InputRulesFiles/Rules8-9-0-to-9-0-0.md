Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.


# Object Change: `Boiler:HotWater`

The only change is for field F7, which was N3.  Logic to apply:

Fields F1-F6 (A1-A4, N1-N2) remain the same.  
Delete F7, which is N3 field. This field was removed as redundant: N3 , \field Design Water Outlet Temperature.
Move up all later numeric fields by one (old N4-N10 become new N3-N9).

