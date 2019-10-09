Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `Foundation:Kiva`
Field 1 remains the same.  
After F1, insert new numeric input field (N1) 'Initial Indoor Air Temperature'.  The field is blank by default.
Shift all later fields down by 1. The old input fields F2-F56 become the new F3-F57.

# Object Change: `RunPeriod`
Field A1 "Name" is now a required field. If blank, add a name. If the old idf has more than one RunPeriod object, all of the Name fields must be unique.

# Object Change: `Schedule:File`
The only change is for Field A4 (Column Separator). If A4 = "Fixed" then replace with "Space". Field A1-A3, A5, and N1-N4 remain the same.

# Object Change: `ThermalStorage:Ice:Detailed`
Fields A5 and A7 change description to Discharging Curve Variable Specifications and Charging Curve Variable Specifications, respectively.  The old options for these fields were either QuadraticLinear or CubicLinear.  

For A5 (field 6), if the field previously had QuadraticLinear, it should be changed to FractionDischargedLMTD.  If A5 was defined as CubicLinear, it should be changed to LMTDMassFlow.  

For A7 (field 7), if the field had previously been QuadraticLinear, it should now say FractionChargedLMTD.  If A7 was CubicLinear, it should be changed to LMTDMassFlow.

# Object Change: `ZoneHVAC:EquipmentList`
Zone Equipment <n></n> Sequential Cooling/Heating Fraction fields are now schedules.

Fields 7 & 8 (old N3 and N4) change from a numeric input to a schedule name.
If the field is blank, then leave it blank.
If the fied is not blank, then add a Schedule:Constant object using the old numeric value and replace the fraction with the new schedule name.
To avoid warnings, add a ScheduleTypeLimits object and reference it in the new schedule objects.

Repeat this for each equipment group of 6 fields; fields 13 & 14, 19 & 20, etc.

# Object Remove and Replace: `AirTerminal:SingleDuct:Uncontrolled` to `AirTerminal:SingleDuct:ConstantVolume:NoReheat`

## (1) Replace Object: replace the "AirTerminal:SingleDuct:Uncontrolled" with "AirTerminal:SingleDuct:ConstantVolume:NoReheat"

Field F1(A1): Name, no change 

Field F2(A2): Availability Schedule Name, no change. 

Insert Field F3(A3): Air Inlet Node Name, create a new inlet node name using the original Field F3(A3) value

          Field F4(A4 = Field F3(A3) + "ATInlet" -> example, "SPACE3-1 Supply Inlet ATInlet", 

          save this new inlet node name for use as the "AirLoopHVAC:ZoneSplitter" or "AirLoopHVAC:SupplyPlenum" outlet node name

Field F4(A4): Air Outlet Node Name, use the old F3(A3).

Field F5(N1) Old Field F4: Maximum Air Flow Rate {m3/s}, no change.

Field F6(A5) Old Field F5: Design Specification Outdoor Air Object Name, no change.

Field F7(A6) Old Field F6: Per Person Ventilation Rate Mode, no change.

Save old Field F7 to use in new ZoneHVAC:AirDistribution object: Design Specification Air Terminal Sizing Object Name


## (2) Object Change: ZoneHVAC:EquipmentList

Field F3(A3): Zone Equipment 1 Object Type, replace "AirTerminal:SingleDuct:Uncontrolled" with "ZoneHVAC:AirDistributionUnit"

Field F4(A4): Zone Equipment 1 Name, create a unique new name. The new name will be created based on the name of "AirTerminal:SingleDuct:Uncontrolled" object". "AirTerminal:SingleDuct:Uncontrolled" object Name + "ADU" -> "SPACE3-1 Direct Air ADU"

save the new name of the "ZoneHVAC:AirDistributionUnit" object

save the "AirTerminal:SingleDuct:Uncontrolled" object Name for later use ( will be used as name for new air terminal no reheat object)

Field F5(N1) - F6(N2), no change.


## (3) Add Object: ZoneHVAC:AirDistributionUnit

Field F1(A1): Name, insert the unique name created for "ZoneHVAC:AirDistributionUnit" in STEP 1 above, example "SPACE3-1 Direct Air ADU"

Field F2(A2): Air Distribution Unit Outlet Node Name, use Zone Supply Air Node Name input field (A3) value of the "AirTerminal:SingleDuct:Uncontrolled" object

Field F3(A3): Air Terminal Object Type, insert "AirTerminal:SingleDuct:ConstantVolume:NoReheat"  

Field F4(A4): Air Terminal Object Type, insert the Name of the "AirTerminal:SingleDuct:Uncontrolled" object, example "SPACE3-1 Direct Air"

Field >=F5(N1): Leave blank. Will use default.

Field >=F6(N2): Leave blank. Will use default.

Field >=F5(N1): Leave blank. Will use default.


## (4) Modify Object: AirLoopHVAC:ZoneSplitter

If a zone splitter references an  "AirTerminal:SingleDuct:Uncontrolled" node name (or a NodeList containing a match) . If it's a NodeList, then create a duplicate NodeList (if necessary) and change
the matching node name in the NodeList. If  duplicate NodeList is made, then change the node name in the splitter to match.

Field F1(A1): Name, no change.

Field F2(A2): Inlet Node Name, no change

    modify one of the "Outlet X Node Name" of the "AirLoopHVAC:ZoneSplitter" if it matches the Zone Supply Air Node Name input field value of current "AirTerminal:SingleDuct:Uncontrolled" object

    use the new Air Inlet Node Name created in Step 3 above for "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object

Field >=F3: Outlet X Node Name, modify the matching Outlet X Node Name, should be the same node name as the new Air Inlet Node Name of the "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object, old name + ATInlet -> example, "SPACE3-1 Supply Inlet ATInlet"

## (5) Modify Object: AirLoopHVAC:SupplyPlenum

If a supply plenum references an  "AirTerminal:SingleDuct:Uncontrolled" node name (or a NodeList containing a match). If it's a NodeList, then create a duplicate NodeList (if necessary) and change
the matching node name in the NodeList. If a duplicate NodeList is made, then change the node name in the splitter to match.

Field F1(A1): Name, no change.

Field F2(A2): Zone Name, no change

Field F3(A3): Zone Node Name, no change

Field F4(A4): Inlet Node Name, no change

    modify one of the "Outlet X Node Name" of the "AirLoopHVAC:SupplyPlenum" that matches the Zone Supply Air Node Name input field value of current "AirTerminal:SingleDuct:Uncontrolled" object

    use the new Air Inlet Node Name created in Step 3 above for "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object. 

Field >=FX(AX): Outlet X Node Name, modify the matching Outlet X Node Name, should be the same node name as the new Air Inlet Node Name of "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object, old name + ATInlet -> example, "SPACE3-1 Supply Inlet ATInlet"

## (6) Modify Object: AirLoopHVAC

If F9 "Demand Side Inlet Node Names" references an  "AirTerminal:SingleDuct:Uncontrolled" node name (or a NodeList containing a match). If it's a NodeList, then create a duplicate NodeList (if necessary) and change
the matching node name in the NodeList. If a duplicate NodeList is made, then change the node name in the splitter to match.


Field F1(A1): Name, no change.

Field F2(A2): Zone Name, no change

Field F3(A3): Zone Node Name, no change

Field F4(A4): Inlet Node Name, no change

    modify one of the "Outlet X Node Name" of the "AirLoopHVAC:SupplyPlenum" that matches the Zone Supply Air Node Name input field value of current "AirTerminal:SingleDuct:Uncontrolled" object

    use the new Air Inlet Node Name created in Step 3 above for "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object. 

Field >=FX(AX): Outlet X Node Name, modify the matching Outlet X Node Name, should be the same node name as the new Air Inlet Node Name of "AirTerminal:SingleDuct:ConstantVolume:NoReheat" object, old name + ATInlet -> example, "SPACE3-1 Supply Inlet ATInlet"

## (7) Change any EMS references
Replace "AirTerminal:SingleDuct:Uncontrolled" with	"AirTerminal:SingleDuct:ConstantVolume:NoReheat"
and replace "AirTerminal:SingleDuct:Uncontrolled Maximum Mass Flow Rate" with 	"AirTerminal:SingleDuct:ConstantVolume:NoReheat Maximum Mass Flow Rate"

## (8) AirflowNetwork:Distribution:Node
If there is an "AirflowNetwork:Distribution:Node" object which references an  "AirTerminal:SingleDuct:Uncontrolled" node name, create a duplicate object
with ATInlet appended to the object name and the node name.

## (9) AirflowNetwork:Distribution:Linkage
If there is an "AirflowNetwork:Distribution:Linkage" object which references an "AirflowNetwork:Distribution:Node" that was duplicated in step 8 then modify the linkage
object and create a new one as shown in this exmple.

### Before
    AirflowNetwork:Distribution:Linkage,
      Zone3SupplyLink,         !- Name
      Zone3SupplyNode,         !- Node 1 Name
      Zone3SupplyRegisterNode, !- Node 2 Name
      Zone3Supply,             !- Component Name
      Attic Zone;              !- Thermal Zone Name

### After
    AirflowNetwork:Distribution:Linkage,
      Zone3SupplyLink ATInlet, !- Name
      Zone3SupplyRegisterNode ATInlet,  !- Node 1 Name
      Zone3SupplyRegisterNode, !- Node 2 Name
      Zone3Supply,             !- Component Name
      Attic Zone;              !- Thermal Zone Name
  
    AirflowNetwork:Distribution:Linkage,
      Zone3SupplyLink,         !- Name
      Zone3SupplyNode,         !- Node 1 Name
      Zone3SupplyRegisterNode ATInlet,  !- Node 2 Name
      Zone3Supply,             !- Component Name
      Attic Zone;              !- Thermal Zone Name


# Object Change: `ObjectNameD`

# Object Change: `ObjectNameE`

# Object Change: `ObjectNameF`
