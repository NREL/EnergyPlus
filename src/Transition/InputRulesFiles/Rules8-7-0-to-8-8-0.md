Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc. 
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `AvailabilityManager:NightCycle`

Fields 1-5 remain the same.  
After F5, insert new alpha input choice field (A5) 'Cycling Run Time Control Type' with default key input value of 'FixedRunTime'. 
Shift all later fields down by 1. The old input field F6 - F10 becomes the new F7-11. The new input field becomes F6.

