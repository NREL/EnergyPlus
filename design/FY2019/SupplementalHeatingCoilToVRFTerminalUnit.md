Add Supplemental Heating Coil to ZoneHVAC:TerminalUnit:VariableRefrigerantFlow
=================================================================

**Bereket Nigusse, FSEC**

 - 25 March 2019 - Original NFP
 - N/A - Revision

## Justification for New Feature

ZoneHVAC:TerminalUnit:VariableRefrigerantFlow currently does not support supplemental heating coils. Variable Refrigerant Flow (VRF) Systems cannot meet space heating demand in cold outside conditions. Some VRF technologies support integral supplemental heating coils. Adding supplemental heating coil as an option in the VRF air terminal units makes VRF system meet the entire heating demand instead of specifying standalone zone heating equipment.


## E-mail and  Conference Call Conclusions
N/A


## Overview and Approach
Below is sample ZoneHVAC:TerminalUnit:VariableRefrigerantFlow object in EnergyPlus. This object will be modified to support supplemental heating coil.

```
  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,
    TU 1,                    !- Zone Terminal Unit Name
    VRFAvailSched,           !- Terminal Unit Availability Schedule
    TU 1 Inlet Node,         !- Terminal Unit Air Inlet Node Name
    TU 1 Outlet Node,        !- Terminal Unit Air Outlet Node Name
    autosize,                !- Cooling Supply Air Flow Rate {m3/s}
    autosize,                !- No Cooling Supply Air Flow Rate {m3/s}
    autosize,                !- Heating Supply Air Flow Rate {m3/s}
    autosize,                !- No Heating Supply Air Flow Rate {m3/s}
    autosize,                !- Cooling Outdoor Air Flow Rate {m3/s}
    autosize,                !- Heating Outdoor Air Flow Rate {m3/s}
    autosize,                !- No Load Outdoor Air Flow Rate {m3/s}
    VRFFanSchedule,          !- Supply Air Fan Operating Mode Schedule Name
    drawthrough,             !- Supply Air Fan Placement
    Fan:ConstantVolume,      !- Supply Air Fan Object Type
    TU 1 VRF Supply Fan,     !- Supply Air Fan Object Name
    OutdoorAir:Mixer,        !- Outside Air Mixer Object Type
    TU 1 OA Mixer,           !- Outside Air Mixer Object Name
    COIL:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type
    TU 1 VRF DX Cooling Coil,!- Cooling Coil Object Name
    COIL:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type
    TU 1 VRF DX Heating Coil,!- Heating Coil Object Name
    30,                      !- Zone Terminal Unit On Parasitic Electric Energy Use {W}
    20;                      !- Zone Terminal Unit Off Parasitic Electric Energy Use {W}
```

Two new input fields "Supplemental Heating Coil Object Type" and and "Supplemental Heating Coil Object Name" will be added to the existing VRF air terminal object. It is anticipated to support the four heating coil types.  These two new input fields will be optional:

```
   A15, \field Supplemental Heating Coil Object Type
        \type choice
        \key Coil:Heating:Fuel
        \key Coil:Heating:Electric
        \key Coil:Heating:Water
        \key Coil:Heating:Steam
        \note works with gas, electric, hot water and steam heating coil.
   A16, \field Supplemental Heating Coil Name
        \type object-list
        \object-list HeatingCoilName
        \note Needs to match in the supplemental heating coil object.   
```

When there is remaining heating load not met by the main DX heating coil of a VRF Air Terminal Unit, then the supplemental heating coils will be turned-on to meet the remaining heating load. Below is a modified sample ZoneHVAC:TerminalUnit:VariableRefrigerantFlow object with the two new input fields added.

```
  ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,
    TU 1,                    !- Zone Terminal Unit Name
    VRFAvailSched,           !- Terminal Unit Availability Schedule
    TU 1 Inlet Node,         !- Terminal Unit Air Inlet Node Name
    TU 1 Outlet Node,        !- Terminal Unit Air Outlet Node Name
    autosize,                !- Cooling Supply Air Flow Rate {m3/s}
    autosize,                !- No Cooling Supply Air Flow Rate {m3/s}
    autosize,                !- Heating Supply Air Flow Rate {m3/s}
    autosize,                !- No Heating Supply Air Flow Rate {m3/s}
    autosize,                !- Cooling Outdoor Air Flow Rate {m3/s}
    autosize,                !- Heating Outdoor Air Flow Rate {m3/s}
    autosize,                !- No Load Outdoor Air Flow Rate {m3/s}
    VRFFanSchedule,          !- Supply Air Fan Operating Mode Schedule Name
    drawthrough,             !- Supply Air Fan Placement
    Fan:ConstantVolume,      !- Supply Air Fan Object Type
    TU 1 VRF Supply Fan,     !- Supply Air Fan Object Name
    OutdoorAir:Mixer,        !- Outside Air Mixer Object Type
    TU 1 OA Mixer,           !- Outside Air Mixer Object Name
    COIL:Cooling:DX:VariableRefrigerantFlow,  !- Cooling Coil Object Type
    TU 1 VRF DX Cooling Coil,!- Cooling Coil Object Name
    COIL:Heating:DX:VariableRefrigerantFlow,  !- Heating Coil Object Type
    TU 1 VRF DX Heating Coil,!- Heating Coil Object Name
    Coil:Heating:Electric,   !- Supplemental Heating Coil Object Type
    TU 1 VRF Supp HeatCoil,  !- Supplemental Heating Coil Name
    30,                      !- Zone Terminal Unit On Parasitic Electric Energy Use {W}
    20;                      !- Zone Terminal Unit Off Parasitic Electric Energy Use {W}
```

## Testing/Validation/Data Sources

Create example files and review results.

## Input Output Reference Documentation

Zone terminal units with variable refrigerant flow DX coils are used exclusively with variable refrigerant flow (VRF) air conditioning systems (Ref. \hyperref[airconditionervariablerefrigerantflow]{AirConditioner:VariableRefrigerantFlow} objects). The zone terminal units are connected to a zone using the inlet and exhaust node names specified in a \hyperref[zonehvacequipmentconnections]{ZoneHVAC:EquipmentConnections} object. The zone exhaust node has the same name as the terminal unit air inlet node. The zone inlet node has the same name as the terminal unit air outlet node. The zone terminal unit is also listed in a zone's equipment list and will typically be the first equipment operating for both cooling and heating (i.e., Sequence = 1 in the \hyperref[zonehvacequipmentlist]{ZoneHVAC:EquipmentList}). Other ZoneHVAC equipment may be used in the same zone and should be sequenced to operate after the zone terminal units (i.e., sequence = 2 or higher)

The terminal units operate to satisfy a heating or cooling load in a zone based on a zone thermostat temperature set point. A direct-expansion (DX) cooling and/or DX heating coil is specified depending on the operating mode required. Both a DX cooling and DX heating coil will typically be installed in the terminal unit, however only one may be used if desired. An optional supplemental heating coil can be used to meet the remaining heating load when the DX heating coil cannot meet the entire heating load of a zone during cold outdoor conditions. Outdoor ventilation air is modeled with the use of an optional outside air mixer object. Outside air may be provided to the zone only when the coil is operating or can be supplied continuously even when the coil is not operating.

A supply air fan is also required and can be modeled as either draw through or blow through. The Supply Air Fan Object Type must be \hyperref[fansystemmodel]{Fan:SystemModel}, \hyperref[fanonoff]{Fan:OnOff}, or \hyperref[fanconstantvolume]{Fan:ConstantVolume} if \hyperref[airconditionervariablerefrigerantflow]{AirConditioner:VariableRefrigerantFlow} is used to model the VRF outdoor unit. The Supply Air Fan Object Type must be \hyperref[fansystemmodel]{Fan:SystemModel} or \hyperref[fanvariablevolume]{Fan:VariableVolume} if AirConditioner:VariableRefrigerantFlow:\-FluidTemperatureControl or AirConditioner:VariableRefrigerantFlow:\-FluidTemperatureControl:HR is used to model the VRF outdoor unit.

```
ZoneHVAC:TerminalUnit:VariableRefrigerantFlow,
        \memo Zone terminal unit with variable refrigerant flow (VRF) DX cooling and heating coils
        \memo (air-to-air heat pump). The VRF terminal units are served by an
        \memo AirConditioner:VariableRefrigerantFlow system.
        \min-fields 19
  A1 ,  \field Zone Terminal Unit Name
        \required-field
        \type alpha
        \reference ZoneTerminalUnitNames
        \reference DOAToZonalUnit
        \reference ZoneEquipmentNames
  A2 ,  \field Terminal Unit Availability Schedule
        \type object-list
        \object-list ScheduleNames
        \note The unit is available the entire simulation if this field is left blank
        \note Schedule values of 0 denote the unit is off.
  A3 ,  \field Terminal Unit Air Inlet Node Name
        \required-field
        \type node
        \note the inlet node to the terminal unit
  A4 ,  \field Terminal Unit Air Outlet Node Name
        \required-field
        \type node
        \note the outlet node of the terminal unit
  N1 ,  \field Cooling Supply Air Flow Rate
        \type real
        \units m3/s
        \minimum> 0.0
        \autosizable
  N2 ,  \field No Cooling Supply Air Flow Rate
        \type real
        \units m3/s
        \minimum 0.0
        \autosizable
  N3 ,  \field Heating Supply Air Flow Rate
        \type real
        \units m3/s
        \minimum> 0.0
        \autosizable
  N4 ,  \field No Heating Supply Air Flow Rate
        \type real
        \units m3/s
        \minimum 0.0
        \autosizable
  N5 ,  \field Cooling Outdoor Air Flow Rate
        \type real
        \units m3/s
        \minimum 0.0
        \autosizable
        \note This field is set to zero flow when the VRF terminal unit is connected to
        \note central dedicated outdoor air through air terminal single duct mixer object.
  N6 ,  \field Heating Outdoor Air Flow Rate
        \type real
        \units m3/s
        \minimum 0.0
        \autosizable
        \note This field is set to zero flow when the VRF terminal unit is connected to
        \note central dedicated outdoor air through air terminal single duct mixer object.
  N7 ,  \field No Load Outdoor Air Flow Rate
        \type real
        \units m3/s
        \minimum 0.0
        \autosizable
        \note This field is set to zero flow when the VRF terminal unit is connected to
        \note central dedicated outdoor air through air terminal single duct mixer object.
  A5 ,  \field Supply Air Fan Operating Mode Schedule Name
        \required-field
        \type object-list
        \object-list ScheduleNames
  A6 ,  \field Supply Air Fan Placement
        \type choice
        \key BlowThrough
        \key DrawThrough
        \default BlowThrough
        \note Select fan placement as either blow through or draw through.
  A7 ,  \field Supply Air Fan Object Type
        \type choice
        \key Fan:SystemModel
        \key Fan:OnOff
        \key Fan:ConstantVolume
        \key Fan:VariableVolume
        \note Supply Air Fan Object Type must be Fan:SystemModel, Fan:OnOff, or Fan:ConstantVolume
        \note if AirConditioner:VariableRefrigerantFlow is used to model VRF outdoor unit
        \note Supply Air Fan Object Type must be Fan:SystemModel or Fan:VariableVolume if
        \note AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl or
        \note AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:HR
        \note is used to model VRF outdoor unit
        \default Fan:ConstantVolume
  A8 ,  \field Supply Air Fan Object Name
        \required-field
        \type object-list
        \object-list FansCVandOnOffandVAV
  A9 ,  \field Outside Air Mixer Object Type
        \type choice
        \key OutdoorAir:Mixer
        \note Currently only one type OutdoorAir:Mixer object is available.
        \note If this field is blank, and outside air mixer is not used.
        \note This field should be left blank if the VRF terminal unit is connected to
        \note central dedicated outdoor air through an AirTerminal:SingleDuct:Mixer object.
  A10,  \field Outside Air Mixer Object Name
        \type object-list
        \object-list OutdoorAirMixers
        \note If this field is blank, the OutdoorAir:Mixer is not used.
        \note This optional field specifies the name of the OutdoorAir:Mixer object.
        \note When used, this name needs to match name of the OutdoorAir:Mixer object.
        \note This field should be left blank if the VRF terminal unit is connected to
        \note central dedicated outdoor air through an AirTerminal:SingleDuct:Mixer object.
  A11,  \field Cooling Coil Object Type
        \type choice
        \key Coil:Cooling:DX:VariableRefrigerantFlow
        \key Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl
        \note Cooling Coil Type must be Coil:Cooling:DX:VariableRefrigerantFlow
        \note if AirConditioner:VariableRefrigerantFlow is used
        \note to model VRF outdoor unit
        \note Cooling Coil Type must be
        \note Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl
        \note if AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl or
        \note if AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:HR
        \note is used to model VRF outdoor unit
        \note This field may be left blank if heating-only mode is used
  A12,  \field Cooling Coil Object Name
        \type object-list
        \object-list CoolingCoilsDXVarRefrigFlow
        \object-list CoolingCoilsDXVarRefrigFlowFluidTemperatureControl
        \note Cooling Coil Type must be Coil:Cooling:DX:VariableRefrigerantFlow
        \note This field may be left blank if heating-only mode is used
  A13,  \field Heating Coil Object Type
        \type choice
        \key Coil:Heating:DX:VariableRefrigerantFlow
        \key Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl
        \note Heating Coil Type must be Coil:Heating:DX:VariableRefrigerantFlow
        \note if AirConditioner:VariableRefrigerantFlow is used
        \note to model VRF outdoor unit
        \note Heating Coil Type must be
        \note Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl
        \note if AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl or
        \note if AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:HR
        \note is used to model VRF outdoor unit
        \note This field may be left blank if cooling-only mode is used
  A14,  \field Heating Coil Object Name
        \type object-list
        \object-list HeatingCoilsDXVarRefrigFlow
        \object-list HeatingCoilsDXVarRefrigFlowFluidTemperatureControl
        \note Heating Coil Type must be Coil:Heating:DX:VariableRefrigerantFlow
        \note This field may be left blank if cooling-only mode is used
  A15,  \field Supplemental Heating Coil Object Type
        \type choice
        \key Coil:Heating:Fuel
        \key Coil:Heating:Electric
        \key Coil:Heating:Water
        \key Coil:Heating:Steam
        \note works with gas, electric, hot water and steam heating coil.
  A16,  \field Supplemental Heating Coil Name
        \type object-list
        \object-list HeatingCoilName
        \note Needs to match in the supplemental heating coil object.  
  N8 ,  \field Zone Terminal Unit On Parasitic Electric Energy Use
        \type real
        \units W
        \minimum 0
        \default 0
  N9 ,  \field Zone Terminal Unit Off Parasitic Electric Energy Use
        \type real
        \units W
        \minimum 0
        \default 0
  N10, \field Rated Heating Capacity Sizing Ratio
       \type real
       \units W/W
       \minimum 1.0
       \default 1.0
       \note If this terminal unit's heating coil is autosized, the heating capacity is sized
       \note to be equal to the cooling capacity multiplied by this sizing ratio.
       \note This input applies to the terminal unit heating coil and overrides the sizing
       \note ratio entered in the AirConditioner:VariableRefrigerantFlow object.
  A16, \field Availability Manager List Name
       \note Enter the name of an AvailabilityManagerAssignmentList object.
       \type object-list
       \object-list SystemAvailabilityManagerLists
  A17; \field Design Specification ZoneHVAC Sizing Object Name
       \note Enter the name of a DesignSpecificationZoneHVACSizing object.
       \type object-list
       \object-list DesignSpecificationZoneHVACSizingName.
```

The DX heating coil outlet node name will be the supplemental coil air inlet node name, and the supplemental coil air outlet node name will be the zone air inlet node name if a supplemental heating coil is specified.


## Engineering Reference

Update engineering reference documentation source as needed.

## Example File and Transition Changes

Proposed example file: modify existing example file or add new example file if needed.

Transition rule will be required for `ZoneHVAC:TerminalUnit:VariableRefrigerantFlow`.


## References ##

- N/A.



