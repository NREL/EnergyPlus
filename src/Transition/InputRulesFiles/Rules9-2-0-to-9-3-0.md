Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `HeatPump:WaterToWater:EIR:Heating`

Object has been renamed to ```PlantLoop:WaterToWater:EIR:Heating```

Fields 1-3 remain the same.
After field 3, insert 'Condenser Type'. The field has two key options of 'WaterSource' and 'AirSource', and defaults to 'WaterSource'. Shift all later fields down by 1. The old input fields F4-F15 become the new F5-F16.

# Object Change: `HeatPump:WaterToWater:EIR:Cooling`

Object has been renamed to ```PlantLoop:WaterToWater:EIR:Cooling```

Fields 1-3 remain the same.
After field 3, insert 'Condenser Type'. The field has two key options of 'WaterSource' and 'AirSource', and defaults to 'WaterSource'. Shift all later fields down by 1. The old input fields F4-F15 become the new F5-F16.
