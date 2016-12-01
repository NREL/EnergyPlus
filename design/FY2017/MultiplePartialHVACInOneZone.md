Improve Control of Multiple/Partial HVAC in One Zone
====================================================

**Michael J. Witte, GARD Analytics, Inc.**

 - November 30, 2016 - Original NFP
 - Revision Date
 

## Justification for New Feature ##

Currently, EnergyPlus allows more than one type of HVAC equipment to serve the same zone. However, there are limitations on how to share the load both for sizing and operation. Examples of applications where equipment shares the load within a zone include:

 - Single-zone residential model with a window air conditioner in one part of the house and a central system serving the rest of the house.
 - Large retail or warehouse zone with multiple rooftop units of differing type.

For sizing, there are optional inputs which allow scalable sizing of `ZoneHVAC:*` objects using `DesignSpecification:ZoneHVAC:Sizing` or fields which are directly in the ZoneHVAC object.  A companion development project, "Allow Multiple Air Loops to One Thermal Zone" is adding a similar object for AirTerminal:* objects, `DesignSpecification:AirTerminal:Sizing`.

For operation, however, the user can only specify the order of simulation. Each piece of equipment attempts to meet the entire remaining load in a sequential fashion.  For many applications, the equipment shares the load in parallel. This feature will add inputs to specify how the load is to be shared. The equipment will still be simulated in a sequential fashion, but the new inputs will allow each piece of equipment to hold back and meet only part of the load, leaving the remaining load for the next piece of equipment.

## E-mail and  Conference Call Conclusions ##

n/a

## Approach ##

The current `ZoneHVAC:EquipmentList` object controls the order of simulation for heating and for cooling.  
```
  ZoneHVAC:EquipmentList,
    SPACE2-1 Eq,             !- Name
    ZoneHVAC:CoolingPanel:RadiantConvective:Water,  !- Zone Equipment 1 Object Type
    SPACE2 Cooling Panel,    !- Zone Equipment 1 Name
    1,                       !- Zone Equipment 1 Cooling Sequence
    1,                       !- Zone Equipment 1 Heating or No-Load Sequence
    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type
    SPACE2-1 ATU,            !- Zone Equipment 2 Name
    2,                       !- Zone Equipment 2 Cooling Sequence
    2,                       !- Zone Equipment 2 Heating or No-Load Sequence
    ZoneHVAC:Baseboard:RadiantConvective:Water,  !- Zone Equipment 3 Object Type
    SPACE2-1 Baseboard,      !- Zone Equipment 3 Name
    3,                       !- Zone Equipment 3 Cooling Sequence
    3;                       !- Zone Equipment 3 Heating or No-Load Sequence
```

A new pair of fields are proposed for each piece of equipment:

```
   Zone Equipment <x> Cooling Fraction Schedule Name
   Zone Equipment <x> Heating or No-Load Fraction Schedule Name
```
There are two ways that the schedule could be applied.

### Option A - Initial Load 
The zone equipment manager multiplies the initial load by the applicable schedule fraction before calling a given piece of equipment.  Here is an example where the radiant cooling panels meet 25% of the cooling load and the air system meets the remaining load.

```
  ZoneHVAC:EquipmentList,
    SPACE2-1 Eq,             !- Name
    ZoneHVAC:CoolingPanel:RadiantConvective:Water,  !- Zone Equipment 1 Object Type
    SPACE2 Cooling Panel,    !- Zone Equipment 1 Name
    1,                       !- Zone Equipment 1 Cooling Sequence
    1,                       !- Zone Equipment 1 Heating or No-Load Sequence
    Always-0.25,             !- Zone Equipment 1 Cooling Fraction Schedule Name
    ,                        !- Zone Equipment 1 Heating or No-Load Fraction Schedule Name
    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type
    SPACE2-1 ATU,            !- Zone Equipment 2 Name
    2,                       !- Zone Equipment 2 Cooling Sequence
    2,                       !- Zone Equipment 2 Heating or No-Load Sequence
    Always-0.75,              !- Zone Equipment 2 Cooling Fraction Schedule Name
    ;                        !- Zone Equipment 2 Heating or No-Load Fraction Schedule Name
```

If the current cooling load is 1000W, the cooling panel will be passed a load of 0.25 \* 1000=250W. The air distribution unit will be passed a load of 0.75 \* 1000W=750W. 

*Pros* - The schedule fractions are intuitive, 0.25 + 0.75 = 1.0

*Cons* - If the first piece of equipment cannot provide the requested load, the next one will not try to meet to meet the unmet portion. and the zone setpoint will not be met.

### Option B - Remaining Load 
The zone equipment manager multiplies the current remaining load by the applicable schedule fraction before calling a given piece of equipment.  Here is an example where the radiant cooling panels meet 25% of the cooling load and the air system meets the remaining load.

```
  ZoneHVAC:EquipmentList,
    SPACE2-1 Eq,             !- Name
    ZoneHVAC:CoolingPanel:RadiantConvective:Water,  !- Zone Equipment 1 Object Type
    SPACE2 Cooling Panel,    !- Zone Equipment 1 Name
    1,                       !- Zone Equipment 1 Cooling Sequence
    1,                       !- Zone Equipment 1 Heating or No-Load Sequence
    Always-0.25,             !- Zone Equipment 1 Cooling Fraction Schedule Name
    ,                        !- Zone Equipment 1 Heating or No-Load Fraction Schedule Name
    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 2 Object Type
    SPACE2-1 ATU,            !- Zone Equipment 2 Name
    2,                       !- Zone Equipment 2 Cooling Sequence
    2,                       !- Zone Equipment 2 Heating or No-Load Sequence
    Always-1.0,              !- Zone Equipment 2 Cooling Fraction Schedule Name
    ;                        !- Zone Equipment 2 Heating or No-Load Fraction Schedule Name
```

If the current cooling load is 1000W, the cooling panel will be passed a load of 0.25*1000=250W.  If the panel can provide this, then the remaining load of 750W is passed to the air distribution unit. If the cooling panel only provides 200W, then 1000-200=800W will be passed to the air distribution unit.

*Pros* - If the first piece of equipment cannot provide the requested load, the next one tries to meet to meet the unmet portion.

*Cons* - The schedule fractions are not intuitive. Because the schedule is applied to the remaining load, the schedule fractions will not add up to 1.0.

## Testing/Validation/Data Sources ##

Build example files and check results.

## Input Output Reference Documentation ##


### Modified Object: ZoneHVAC:EquipmentList###

*New Field: Zone Equipment <x> Cooling Fraction Schedule Name*

The name of a schedule which specifies the fraction of the current (*OR remaining*) sensible cooling load to be met by equipment <x>.  If this field is left blank, the entire remaining cooling load is passed to this piece of equipment.

*New Field: Zone Equipment <x> Heating or No-Load Fraction Schedule Name*

The name of a schedule which specifies the fraction of the total (*OR remaining*) sensible heating  load to be met by equipment <x>.  If this field is left blank, the entire remaining cooling load is passed to this piece of equipment.

## Outputs Description ##

As proposed in the companion project, a set of general zone-level output variables will be developed report the contribution of each piece of zone equipment.  Proposed output variable names:

Zone HVAC Equipment 1 Sensible Cooling Rate [W]
Zone HVAC Equipment 1 Total Cooling Rate [W]
Zone HVAC Equipment 1 Sensible Heating Rate [W]
Zone HVAC Equipment 1 Total Heating Rate [W]
Zone HVAC Equipment 1 Sensible Cooling Energy [J]
Zone HVAC Equipment 1 Total Cooling Energy [J]
Zone HVAC Equipment 1 Sensible Heating Energy [J]
Zone HVAC Equipment 1 Total Heating Energy [J]

## Engineering Reference ##

The current section "Zone Equipment Simulation" discusses simulation of air terminal units which are part of an air loop in greate detail.  The only statement about other zone equipment is:

"If multiple air-conditioning components are attached to a zone, the components
are simulated in the order specified by the user assigned priority given in the ZoneHVAC:EquipmentList object."

This section will be expanded to include more details of the ZoneHVAC:EquipmentList simulation, including the accounting for initial zone loads, remaining loads, load to heating setpoint and load to cooling setpoint.

## Example File and Transition Changes ##

Proposed example files:

 - Single-zone residential model with window air conditioner and central system.
 - Retail store with two rooftop units, one high-efficiency and one low-efficiency serving the same core zone.

Transition will be required for `ZoneHVAC:EquipmentList`.

## References ##

n/a



