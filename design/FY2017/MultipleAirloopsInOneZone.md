Allow Multiple Air Loops to One Thermal Zone
================

**Michael J. Witte, GARD Analytics, Inc.**

 - October 28, 2016 - NFP
 - Revision Date
 

## Justification for New Feature ##

The current limitation of one airloop (any system built using the `AirloopHVAC` object) serving a given zone has long been a barrier to modeling various system configurations. The most commong situation is a multi-zone DOAS running in parallel with single or multi-zone space conditioning systems.  EnergyPlus currently offers three workarounds for modeling DOAS systems in combination with other space-conditioning equipment:

1. Model the DOAS as an airloop and the space-conditioning equipment as zone HVAC equipment (`ZoneHVAC:*`).
2. Model the space-conditioning equipment as an airloop and the DOAS as zone HVAC equipment (`ZoneHVAC:OutdoorAirUnit`).
3. Model the DOAS and the space-conditioning equipment as a dual-duct airloop where one deck is the DOAS side and the other deck is the space-conditining side. (`AirTerminal:DualDuct:VAV:OutdoorAir`).

All of the current workarounds have limitations and none of them reflect the layout of the real systems.  

Recently, the NREL Residential Group and a consultant for NRCan (Natural Resources Canada) asked how to model this common residential application:

> The configuration in question is one where zone air is re-circulated through a furnace while a DOAS, generally including a HRV/ERV, handles ventilation.  So the conditioning and ventilation loops operate in parallel.   The furnace coil must be centralized to be able to impose a capacity and calculate part-load factors: so re-heating coils are also not acceptable.  

None of the available workarounds will model both systems correctly. In this particular case, one solution would be to build on #3 above with a new terminal unit that has heating and cooling capability on the VAV side.  The other solution is to use #2, but this raises issues of the HRV/ERV effiency and defrost operation as well as fan power.

Rather than develop more workaround components, it is time to remove the restriction of one airloop per zone.

## E-mail and  Conference Call Conclusions ##

insert text

## Overview ##

This new feature will remove the limitation of one airloop (`AirLoopHVAC`) system per zone, and will add options for user-control of sizing and return air flow allocation. Currently, EnergyPlus issues a fatal error if more than one `ZoneHVAC:AirDistributionUnit` or `AirTerminal:SingleDuct:Uncontrolled` are connected to a given zone:

```
** Severe  ** In zone "ZONE ONE" there are too many air terminals served by AirLoopHVAC systems.
**   ~~~   ** A single zone cannot have both an AirTerminal:SingleDuct:Uncontrolled and also a second AirTerminal:* object.
**  Fatal  ** Preceding condition causes termination
```

Also, EnergyPlus currently allows only one Return Air Node per zone.

This feature will remove these limitations and allow any number of airloop systems to serve a given zone.

If time allows, this work will also remove the current requirement that a return path must always be described even if it does not exist in the system.  For example, most DOAS systems and direct-fired heaters only supply outdoor air and do not have any return path.

## Approach ##

### 1. Remove error checks on number of air terminals in a zone ###

### 2. Allow more than one return air node in a zone ###
Change the `Zone Return Air Node Name` field to allow a NodeList, similar to the way that zone inlet nodes are input.


### 3. Revise return air flow and air loop flow balance calculations
Currently, the zone return air node is passive and receives whatever flow is left over from total supply less total exhaust.  There is also an option to control the return air flow using the `Zone Return Air Flow Rate Fraction Schedule Name` and `Zone Return Air Flow Rate Basis Node or NodeList Name` fields.

### 4. Allocate return air heat gains to specific return air nodes
This applies to return air heat gain from lights and loss from refrigeration equipment.

### 4. Revise other places that assume a single airloop is associated with a zone
Preliminary code review shows that there are several places that use `ZoneEquipConfig::AirLoopNum`.

### 5. Allow different sizing specifications for different air terminal units
The present sizing inputs apply a single `Sizing:Zone` object to a given zone to calculate the design load and airflow rates.  Then ZoneHVAC:* equipment (such as fan coils or PTACs) can reference a `DesignSpecification:ZoneHVAC:Sizing` object to scale it's flow rates and capacities based on the results from the `Sizing:Zone` calculations or entirely on user-specified values or values based on floor area.  But multiple air terminals in the same zone will typically need to use different supply air temperatures for sizing.

Here are examples of the current zone sizing objects:

```
  Sizing:Zone,
    West Zone,               !- Zone or ZoneList Name
    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method
    14.,                     !- Zone Cooling Design Supply Air Temperature {C}
    ,                        !- Zone Cooling Design Supply Air Temperature Difference {deltaC}
    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method
    50.,                     !- Zone Heating Design Supply Air Temperature {C}
    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}
    0.009,                   !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}
    0.004,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}
    SZ DSOA West Zone,       !- Design Specification Outdoor Air Object Name
    0.0,                     !- Zone Heating Sizing Factor
    0.0,                     !- Zone Cooling Sizing Factor
    DesignDay,               !- Cooling Design Air Flow Method
    0,                       !- Cooling Design Air Flow Rate {m3/s}
    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}
    ,                        !- Cooling Minimum Air Flow {m3/s}
    ,                        !- Cooling Minimum Air Flow Fraction
    DesignDay,               !- Heating Design Air Flow Method
    0,                       !- Heating Design Air Flow Rate {m3/s}
    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}
    ,                        !- Heating Maximum Air Flow {m3/s}
    ;                        !- Heating Maximum Air Flow Fraction
```

```
  DesignSpecification:ZoneHVAC:Sizing,
    FanCoilDesignSpec1,      !- Name
    SupplyAirFlowRate,       !- Cooling Supply Air Flow Rate Method
    autosize,                !- Cooling Supply Air Flow Rate {m3/s}
    ,                        !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}
    ,                        !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate
    ,                        !- Cooling Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}
    SupplyAirFlowRate,       !- No Load Supply Air Flow Rate Method
    0.0,                     !- No Load Supply Air Flow Rate {m3/s}
    ,                        !- No Load Supply Air Flow Rate Per Floor Area {m3/s-m2}
    ,                        !- No Load Fraction of Cooling Supply Air Flow Rate
    ,                        !- No Load Fraction of Heating Supply Air Flow Rate
    SupplyAirFlowRate,       !- Heating Supply Air Flow Rate Method
    autosize,                !- Heating Supply Air Flow Rate {m3/s}
    ,                        !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}
    ,                        !- Heating Fraction of Heating Supply Air Flow Rate
    ,                        !- Heating Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}
    CoolingDesignCapacity,   !- Cooling Design Capacity Method
    autosize,                !- Cooling Design Capacity {W}
    ,                        !- Cooling Design Capacity Per Floor Area {W/m2}
    ,                        !- Fraction of Autosized Cooling Design Capacity
    CapacityPerFloorArea,    !- Heating Design Capacity Method
    ,                        !- Heating Design Capacity {W}
    156.89549,               !- Heating Design Capacity Per Floor Area {W/m2}
    ;                        !- Fraction of Autosized Heating Design Capacity

```

In the most general sense, an air terminal unit in a zone with multiple air terminal units would require data from both of these objects in order to size correctly - it could have different supply air temperature, OA requirements, and scalable sizing factors. 

The initial proposal is to add a new object named `DesignSpecification:AirTerminal:Sizing` which is a combination of the `Sizing:Zone` object plus the airflow fields from the `DesignSpecification:ZoneHVAC:Sizing` object.

Another option would be to allow names Sizing:Zone object which are not associated with a particular zone.

In either case, a new field would be added at the end of the AirTerminal objects to reference a `DesignSpecification:AirTerminal:Sizing` or `Sizing:Zone` object.


### 6. *Optional* - Allow an airloop with no return path

## Testing/Validation/Data Sources ##

Build example files and check results.

## Input Output Reference Documentation ##

## Input Description ##

### Modified: ZoneHVAC:EquipmentConnections ###
* Change the current `Zone Return Air Node Name` to `Zone Return Air Node or NodeList Name`.

* Change the following fields to apply only to the first zone return air node 

* OR make these an extensible group:

```
Zone Return Air Flow Rate Fraction Schedule Name
Zone Return Air Flow Rate Basis Node or NodeList Name
```

### New: DesignSpecification:AirTerminal:Sizing###

```
  DesignSpecification:AirTerminal:Sizing,
    DOAS Terminal Sizing,      !- Name
    SupplyAirTemperature,    !- Cooling Design Supply Air Temperature Input Method
    14.,                     !- Cooling Design Supply Air Temperature {C}
    ,                        !- Cooling Design Supply Air Temperature Difference {deltaC}
    SupplyAirTemperature,    !- Heating Design Supply Air Temperature Input Method
    50.,                     !- Heating Design Supply Air Temperature {C}
    ,                        !- Heating Design Supply Air Temperature Difference {deltaC}
    0.009,                   !- Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}
    0.004,                   !- Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}
    SZ DSOA West Zone,       !- Design Specification Outdoor Air Object Name
    0.0,                     !- Heating Sizing Factor
    0.0,                     !- Cooling Sizing Factor
    DesignDay,               !- Cooling Design Air Flow Method
    0,                       !- Cooling Design Air Flow Rate {m3/s}
    ,                        !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}
    ,                        !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate
    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}
    ,                        !- Cooling Minimum Air Flow {m3/s}
    ,                        !- Cooling Minimum Air Flow Fraction
    SupplyAirFlowRate,       !- No Load Supply Air Flow Rate Method
    0.0,                     !- No Load Supply Air Flow Rate {m3/s}
    ,                        !- No Load Supply Air Flow Rate Per Floor Area {m3/s-m2}
    ,                        !- No Load Fraction of Cooling Supply Air Flow Rate
    ,                        !- No Load Fraction of Heating Supply Air Flow Rate
    DesignDay,               !- Heating Design Air Flow Method
    0,                       !- Heating Design Air Flow Rate {m3/s}
    ,                        !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}
    ,                        !- Heating Fraction of Heating Supply Air Flow Rate
    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}
    ,                        !- Heating Maximum Air Flow {m3/s}
    ;                        !- Heating Maximum Air Flow Fraction
```


## Outputs Description ##

No new outputs are anticipated

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

No transition is anticipated.

New example files will be made to show various combinations of systems.

## References ##

n/a



