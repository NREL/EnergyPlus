Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

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


