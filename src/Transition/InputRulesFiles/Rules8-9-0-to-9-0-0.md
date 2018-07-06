Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `AirflowNetwork:Distribution:Component:OutdoorAirFlow`

Field 1 remains the same.  
After F1, insert new alpha input choice field (A2) 'Outdoor Air Mixer Name' with the object name of  'OutdoorAir:Mixer'.
Shift all later fields down by 1. The old input fields F2 - F4 become the new F3-F5. The new input field becomes F2.

Note: The old input file has a single OutdoorAir:Mixer object, so that the name of OutdoorAir:Mixer is unique.

# Object Change: `AirflowNetwork:Distribution:Component:ReliefAirFlow`

Field 1 remains the same.  
After F1, insert new alpha input choice field (A2) 'Outdoor Air Mixer Name' with the object name of  'OutdoorAir:Mixer'.
Shift all later fields down by 1. The old input fields F2 - F4 become the new F3-F5. The new input field becomes F2.

Note: The old input file has a single OutdoorAir:Mixer object, so that the name of OutdoorAir:Mixer is unique.

