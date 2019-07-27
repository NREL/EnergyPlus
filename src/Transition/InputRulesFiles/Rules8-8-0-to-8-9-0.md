Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `ZoneHVAC:EquipmentList`

Field 1 remains the same.  
After F1, insert new alpha input choice field (A2) 'Load Distribution Scheme' with default key input value of 'SequentialLoad'.
Shift all later fields down by 1. The old input fields F2 - Fn become the new F3-Fn+1. The new input field becomes F2.

# Object Change: Schedule:Day:Interval

Field A3 Interpolate to Timestep, change value of "Yes" to "Average"

# Object Change: Schedule:Day:List

Field A3 Interpolate to Timestep, change value of "Yes" to "Average"

# Object Change: Schedule:Compact

For any field A3 and later, change values of "Interpolate:Yes" to "Interpolate:Average"

# Object Change: ElectricEquipment:ITE:AirCooled
Fields 1 and 2 remain tha same
After F2, insert new alpha input choice field (A3) 'Air Flow Calculation Method' with default kay value 'FlowFromSystem'.
Shift all later fields down by 1. The old input fields F3 - Fn become the new F4-Fn+1. The new input field becomes F3.
At the end, there are four new fields, but these don't need to be added - they are required only when using the new option
introduced in new field A3.


# Transitioning "AirTerminal:SingleDuct:Uncontrolled" to "AirTerminal:SingleDuct:ConstantVolume:NoReheat". Require five steps:

# (1) Object Change: ZoneHVAC:EquipmentList

Field F3(A3): Zone Equipment 1 Object Type, replace "AirTerminal:SingleDuct:Uncontrolled" with "ZoneHVAC:AirDistributionUnit"

Field F4(A4): Zone Equipment 1 Name, create a unique new name. The new name will be created based on the name of "AirTerminal:SingleDuct:Uncontrolled" object". "AirTerminal:SingleDuct:Uncontrolled" object Name + "ADU" -> "SPACE3-1 Direct Air ADU"

save the new name of the "ZoneHVAC:AirDistributionUnit" object

save the "AirTerminal:SingleDuct:Uncontrolled" object Name for later use ( will be used as name for new air terminal no reheat object)

Field F5(N1) - F6(N2), no change.


# (2) Add Object: ZoneHVAC:AirDistributionUnit

Field F1(A1): Name, insert the unique name created for "ZoneHVAC:AirDistributionUnit" in STEP 1 above, example "SPACE3-1 Direct Air ADU"

Field F2(A2): Air Distribution Unit Outlet Node Name, use Zone Supply Air Node Name input field (A3) value of the "AirTerminal:SingleDuct:Uncontrolled" object

Field F3(A3): Air Terminal Object Type, insert "AirTerminal:SingleDuct:ConstantVolume:NoReheat"  

Field F4(A4): Air Terminal Object Type, insert the Name of the "AirTerminal:SingleDuct:Uncontrolled" object, example "SPACE3-1 Direct Air"

Field >=F5(N1): Leave blank. Will use default.



# (3) Modify Object: AirTerminal:SingleDuct:Uncontrolled

#### replace the "AirTerminal:SingleDuct:Uncontrolled" object with "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object

Field F1(A1): Name, no change 

Field F2(A2): Availability Schedule Name, no change. 

Field F3(A3): Air Outlet Node Name, no change.

Field F4(A4): Air Inlet Node Name, create a new inlet node name using the Field F3(A3) value above

          Field F4(A4 = Field F3(A3) + "2AT" -> example, "SPACE3-1 Supply Inlet 2AT", 

          save this new inlet node name for use as the "AirLoopHVAC:ZoneSplitter" or "AirLoopHVAC:SupplyPlenum" outlet node name

Field F5(N1): Maximum Air Flow Rate {m3/s}, no change.

Field F6(A5): Design Specification Outdoor Air Object Name, no change.

Field F7(A6) : Per Person Ventilation Rate Mode, no change.


# (4) Modify Object: AirLoopHVAC:ZoneSplitter

Field F1(A1): Name, no change.

Field F2(A2): Inlet Node Name, no change

(4) modify one of the "Outlet X Node Name" of the "AirLoopHVAC:ZoneSplitter" if it matches the Zone Supply Air Node Name input field value of current "AirTerminal:SingleDuct:Uncontrolled" object

    use the new Air Inlet Node Name created in Step 3 above for "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object

Field >=F3: Outlet X Node Name, modify the matching Outlet X Node Name, should be the same node name as the new Air Inlet Node Name of the "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object, old name + 2AT -> example, "SPACE3-1 Supply Inlet 2AT"

# (5) Modify Object: AirLoopHVAC:SupplyPlenum

    Step 5 is not required if Step 4 is successful. If not do the following:

Field F1(A1): Name, no change.

Field F2(A2): Zone Name, no change

Field F3(A3): Zone Node Name, no change

Field F4(A4): Inlet Node Name, no change

    modify one of the "Outlet X Node Name" of the "AirLoopHVAC:SupplyPlenum" that matches the Zone Supply Air Node Name input field value of current "AirTerminal:SingleDuct:Uncontrolled" object

    use the new Air Inlet Node Name created in Step 3 above for "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object. 

Field >=FX(AX): Outlet X Node Name, modify the matching Outlet X Node Name, should be the same node name as the new Air Inlet Node Name of "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object, old name + 2AT -> example, "SPACE3-1 Supply Inlet 2AT"
