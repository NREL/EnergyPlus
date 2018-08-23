Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `Boiler:HotWater`

The only change is for field F7, which was N3.  Logic to apply:

Fields F1-F6 (A1-A4, N1-N2) remain the same.  
Delete F7, which is N3 field. This field was removed as redundant: N3 , \field Design Water Outlet Temperature.
Move up all later numeric fields by one (old N4-N10 become new N3-N9).


(Developer note: to avoid merge conflicts, please choose a random line in the file below (there are like 20 blank lines initially).
 This will reduce the effect of all the rules being written on the same line, causing conflicts.)
 
# Object Change: 'WindowMaterial:ComplexShade'
 
The only change is for field A2 (Layer Type). Logic to apply:
If field contains value Venetian then change it to VenetianHorizontal.
 
# Object Change: 'RunPeriod:CustomRange'

Change the object type to 'RunPeriod'.
All fields remain the same.

# Object Change: 'RunPeriod'
Fields F1-F3 (A1, N1-N2) remain the same
Insert new field F4 (new N3) for Begin Year
  Fill with the value from old field F14 (old N6 Start Year) if non-blank
  elseif old field F12 (old N5 Number of Times Runperiod to be Repeated) is non-blank fill with year corresponding to the "Day of Week for Start Day" (old F6, old A2)
  elseif start day is blank, fill with a year that starts on a Sunday
  else leave blank
Old fields F4-F5 become new fields F5-F6
Insert new field F7 (new N6)for End Year
  If old field F12 (old N5 Number of Times Runperiod to be Repeated) is non-blank fill with new field F4 (Begin Year) plus old F12 value
  unless new field F4 (Begin Year) is blank, then new F7 is also blank
  else leave blank
New field F8 (new A2 Day of Week for Start Day)
  If old field F14 (old N6 Start Year) is non-blank, then fill new F8 with blank
  else new F8 = old F6 (old A2 Day of Week for Start Day)
New F9=F13 = Old F7-F11
  





# Object Change: `FenestrationSurface:Detailed`

Fields F1-F6 (A1-A5, N1) remain the same.  
Delete F7, which is A6 \field Shading Control Name.
Shift all later fields by one.

# Object Change: `Window`

Fields F1-F3 (A1-A3) remain the same.  
Delete F4, which is A4 \field Shading Control Name.
Shift all later fields by one.

# Object Change: `GlazedDoor`

Fields F1-F3 (A1-A3) remain the same.  
Delete F4, which is A4 \field Shading Control Name.
Shift all later fields by one.

# Object Change: `WindowProperty:ShadingControl`

Change object type from `WindowProperty:ShadingControl` to `WindowShadingControl`
Field F1 (A1) remains the same.
Insert new field F2 (N1) as a blank, \field Shading Control Sequence Number.
All remaining old Fields F2-F12 (A2-A10, N1-N2) shift by one to F3-F13 (A2-A10, N2-N3).
Add the following new fields at the end, starting with F14:
F14 (A11) ????? \field Daylighting Control Name.
F15 (A12) "Sequential" \field Multiple Surface Control Type.
F16 and following (A13ff) \field Fenestration Surface n Name: 
a) Loop through all of the old Fenestration:Detailed, Window, and GlazedDoor objects, 
b) If the "Shading Control Name" is not blank, and it matches the name (F1) of this object, then add the surface name as a new field here
Order matters here, so need to maintain the same order that was previously used for control, which was ???








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

