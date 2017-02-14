Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc. 
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `Coil:Cooling:DX:MultiSpeed`

The only change is for field F16, which is A12.  Logic to apply:

if key = blank, change to = 'NaturalGas'
if key = 'PropaneGas', change to = 'Propane'

All other fields stay the same.

# Object Change: `Coil:Heating:DX:MultiSpeed`

The only change is for field F16, which is A9.  Logic to apply:

if key = blank, change to = 'NaturalGas'
if key = 'PropaneGas', change to = 'Propane'

All other fields stay the same.

# Object Change: `CoolingTower:SingleSpeed`

Fields 1-16 remain the same.  
After F16, insert four new blank fields that represent user-enterable design conditions. 
Shift all later fields down by four.

# Object Change: `CoolingTower:TwoSpeed`

Fields 1-24 remain the same.  
After F24, insert four new blank fields that represent user-enterable design conditions. 
Shift all later fields down by four.

# Object Change: `CoolingTower:VariableSpeed:Merkel`

Fields 1-24 remain the same.  
After F24`, insert four new blank fields that represent user-enterable design conditions. 
Shift all later fields down by four.





