Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc. 
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `ZoneHVAC:FourPipeFanCoil`

The only change is for field F7, which is numeric field N4.  Logic to apply:

if input value = blank, change to = 'Autosize'
if input value = 0, change to = 'Autosize'

All other fields stay the same.

# Object Change: `ZoneHVAC:WaterToAirHeatPump`

The only change is for fields F10, F11, and F12 which are numeric fields N4, N5, and N6.  Logic to apply to all three fields:

if input value = blank, change to = 'Autosize'
if input value = 0, change to = 'Autosize'

All other fields stay the same.

# Object Change: `ZoneHVAC:PackagedTerminalAirConditioner`

The only change is for fields F10, F11, and F12 which are numeric fields N4, N5, and N6.  Logic to apply to all three fields:

if input value = blank, change to = 'Autosize'
if input value = 0, change to = 'Autosize'

All other fields stay the same.

# Object Change: `ZoneHVAC:PackagedTerminalHeatPump`

The only change is for fields F10, F11, and F12 which are numeric fields N4, N5, and N6.  Logic to apply to all three fields:

if input value = blank, change to = 'Autosize'
if input value = 0, change to = 'Autosize'

All other fields stay the same.

# Object Change: `ZoneHVAC:TerminalUnit:VariableRefrigerantFlow`

The only change is for fields F9, F10, and F11 which are numeric fields N5, N6, and N7.  Logic to apply to all three fields:

if input value = blank, change to = 'Autosize'
if input value = 0, change to = 'Autosize'

All other fields stay the same.
