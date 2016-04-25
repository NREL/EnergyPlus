Outdoor Air Controller Makeover - Issue #4663
================

**M.J. Witte, GARD Analytics, Inc.**

 - April 22, 2016


## Background ##

The current combination of Controller:OutdoorAir plus Controller:MechanicalVentilation can be confusing. Common frustrations that users encounter with these objects include:

  
1. Trying to set the OA flow to zero at night when using Controller:MechanicalVentilation
[Issue #4633](https://github.com/NREL/EnergyPlus/issues/4633) requires that the Controller:MechanicalVentilation schedule be set to zero as well as the Controller:OutdoorAir Minimum Outdoor Air Schedule (*if* the Controller:OutdoorAir Minimum Outdoor Air Flow Rate is > 0).

2. Specifying a schedule in DesignSpecification:OutdoorAir only to find that it is ignored by Controller:MechanicalVentilation.

3. Controller:MechanicalVentilation requires a redundant list of DesignSpecification:OutdoorAir and DesignSpecification:ZoneAirDistribution names which typically are already listed in the corresponding Sizing:Zone objects.  (The I/O Ref and an error message for omitted zones says that the information will be found from the Sizing:Zone objects but that code was removed some years back - not sure why.)

4.  Specifying a 100% outdoor air system is not obvious. The Minimum Outdoor Air Flow Rate and Maximum Outdoor Air Flow rate both need to match the system flow rate.  If the correct inputs are set in Sizing:System then these will autosize to the same value, but for some applications this does not work.

5.  Using VRP or DCV with Controller:MechanicalVentilation and autosizing the Controller:MechanicalVentilation Minimum Outdoor Air Flow Rate can result in a Minimum Outdoor Air Flow Rate always greater than the DCV or VRP OA flow, which overrides the DCV or VRP operation.

6. Uncertainties about what limits take precedence over others, especially the Controller:OutdoorAir Maximum Fraction of Outdoor Air Schedule which is described as a hardward limit which should take precedence over all other OA flow calcs, but it does not.

7. Some users want to disable economizer by time of day of year, but there is no option for that now.  They try using the "Time of Day Economizer Control Schedule" but it doesn't turn off the main economizer controls.

## Current Example Objects ##


```

 Controller:OutdoorAir,
    VAV WITH REHEAT_OA_Controller,  !- Name
    VAV WITH REHEAT_OARelief Node,  !- Relief Air Outlet Node Name
    VAV WITH REHEAT Supply Equipment Inlet Node,  !- Return Air Node Name
    VAV WITH REHEAT_OA-VAV WITH REHEAT_CoolCNode,  !- Mixed Air Node Name
    VAV WITH REHEAT_OAInlet Node,  !- Actuator Node Name
    AUTOSIZE,                !- Minimum Outdoor Air Flow Rate {m3/s}
    AUTOSIZE,                !- Maximum Outdoor Air Flow Rate {m3/s}
    NoEconomizer,            !- Economizer Control Type
    ModulateFlow,            !- Economizer Control Action Type
    19.0,                    !- Economizer Maximum Limit Dry-Bulb Temperature {C}
    32000.0,                 !- Economizer Maximum Limit Enthalpy {J/kg}
    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}
    ,                        !- Electronic Enthalpy Limit Curve Name
    0.0,                     !- Economizer Minimum Limit Dry-Bulb Temperature {C}
    NoLockout,               !- Lockout Type
    FixedMinimum,            !- Minimum Limit Type
    MinOA_MotorizedDamper_Sched,  !- Minimum Outdoor Air Schedule Name
    ,                        !- Minimum Fraction of Outdoor Air Schedule Name
    ,                        !- Maximum Fraction of Outdoor Air Schedule Name
    VAV WITH REHEAT_OAMechanical Ventilation,  !- Mechanical Ventilation Controller Name
    ,                        !- Time of Day Economizer Control Schedule Name
    ,                        !- High Humidity Control
    ,                        !- Humidistat Control Zone Name
    ,                        !- High Humidity Outdoor Air Flow Ratio
    ,                        !- Control High Indoor Humidity Based on Outdoor Humidity Ratio
    BypassWhenWithinEconomizerLimits;  !- Heat Recovery Bypass Control Type

 Controller:MechanicalVentilation,
    VAV WITH REHEAT_OAMechanical Ventilation,  !- Name
    ALWAYS_ON,               !- Availability Schedule Name
    Yes,                     !- Demand Controlled Ventilation
    VentilationRateProcedure,!- System Outdoor Air Method
    0.3,                     !- Zone Maximum Outdoor Air Fraction {dimensionless}
    ZN_1_FLR_1_SEC_1,        !- Zone 1 Name
    CM DSOA ZN_1_FLR_1_SEC_1,!- Design Specification Outdoor Air Object Name 1
    ZoneAirDistribution,     !- Design Specification Zone Air Distribution Object Name 1
    ZN_1_FLR_1_SEC_2,        !- Zone 2 Name
    CM DSOA ZN_1_FLR_1_SEC_2,!- Design Specification Outdoor Air Object Name 2
    ZoneAirDistribution,     !- Design Specification Zone Air Distribution Object Name 2
    ZN_1_FLR_1_SEC_3,        !- Zone 3 Name
    CM DSOA ZN_1_FLR_1_SEC_3,!- Design Specification Outdoor Air Object Name 3
    ZoneAirDistribution,     !- Design Specification Zone Air Distribution Object Name 3
    ZN_1_FLR_1_SEC_4,        !- Zone 4 Name
    CM DSOA ZN_1_FLR_1_SEC_4,!- Design Specification Outdoor Air Object Name 4
    ZoneAirDistribution,     !- Design Specification Zone Air Distribution Object Name 4
    ZN_1_FLR_1_SEC_5,        !- Zone 5 Name
    CM DSOA ZN_1_FLR_1_SEC_5,!- Design Specification Outdoor Air Object Name 5
    ZoneAirDistribution;     !- Design Specification Zone Air Distribution Object Name 5

 DesignSpecification:ZoneAirDistribution,
    ZoneAirDistribution,     !- Name
    1.2,                     !- Zone Air Distribution Effectiveness in Cooling Mode {dimensionless}
    0.8,                     !- Zone Air Distribution Effectiveness in Heating Mode {dimensionless}
    ,                        !- Zone Air Distribution Effectiveness Schedule Name
    0.0;                     !- Zone Secondary Recirculation Fraction {dimensionless}

 DesignSpecification:OutdoorAir,
    CM DSOA ZN_1_FLR_1_SEC_1,!- Name
    Sum,                     !- Outdoor Air Method
    0.0100,                  !- Outdoor Air Flow per Person {m3/s-person}
    0.0000,                  !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}
    ,                        !- Outdoor Air Flow per Zone {m3/s}
    ,                        !- Outdoor Air Flow Air Changes per Hour {1/hr}
    MinOA_MotorizedDamper_Sched;  !- Outdoor Air Flow Rate Fraction Schedule Name

```

## Proposed Changes ##

1. Move these three fields from `Controller:MechanicalVentilation` to `Controller:OutdoorAir`:
```
    ALWAYS_ON,               !- Availability Schedule Name
    Yes,                     !- Demand Controlled Ventilation
    VentilationRateProcedure,!- System Outdoor Air Method
```

2. Rename `Controller:MechanicalVentilation` to `Controller:OutdoorAir:ZoneList` and make it optional.  If all of the zones have a Sizing:Zone object, then the designspec info will be taken from there.

3. Change the autosizing for `Controller:OutdoorAir` Minimum Outdoor Air Flow Rate to be zero if one of the advanced methods (VRP, DCV, IAQ) of control is selected.  *Or maybe autosize to the non-per-person flow?*

4. Make the Maximum Fraction of Outdoor Air Schedule Name be king - OA fraction can never be greater than the current schedule value.

5. Change the advanced methods (ZoneSum, VentilationRateProcedure,IndoorAirQualityProcedure, ProportionalControlBasedOnDesignOccupancy,ProportionalControlBasedonOccupancySchedule,IndoorAirQualityProcedureGenericContaminant) to use the `DesignSpecification:OutdoorAir` Outdoor Air Flow Rate Fraction Schedule.  Currently it is ignored. *The primary goal here is to address the original issue that setting this schedule to zero should shut off OA.  There is a question of how this would be applied for the CO2 and IAQP methods.*

6. Add a new outdoor air control type = "100PercentOA" which always delivers 100% OA.

7. Change "Time of Day Economizer Control Schedule" to apply to any type of economizer control, and add a new Economizer Control Type = TimeOfDay.

8. Reorder and rename some of the fields in Controller:OutdoorAir.

9. Refactor MixedAir::CalcOAController to make separate functions for each control type.  Current CalcOAController is over 1000 lines long.

## Modified Objects ##


```

 Controller:OutdoorAir,
    VAV WITH REHEAT_OA_Controller,  !- Name
    VAV WITH REHEAT_OARelief Node,  !- Relief Air Outlet Node Name
    VAV WITH REHEAT Supply Equipment Inlet Node,  !- Return Air Node Name
    VAV WITH REHEAT_OA-VAV WITH REHEAT_CoolCNode,  !- Mixed Air Node Name
    VAV WITH REHEAT_OAInlet Node,  !- Actuator Node Name
    AUTOSIZE,                !- Minimum Outdoor Air Flow Rate {m3/s}
    AUTOSIZE,                !- Maximum Outdoor Air Flow Rate {m3/s}
    FixedMinimum,            !- Minimum Outdoor Air Flow Limit Type
    MinOA_MotorizedDamper_Sched,  !- Minimum Outdoor Air Flow Rate Schedule Name
    VentilationRateProcedure,!- Advanced Outdoor Air Control Type
    ALWAYS_ON,               !- Advanced Control Availability Schedule Name
    Yes,                     !- Advanced Control Demand Controlled Ventilation
    0.3,                     !- Advanced Control Zone Maximum Outdoor Air Fraction
    VAV WITH REHEAT Zone OA List,  !- Advanced Outdoor Air Control Zonelist Name
    NoEconomizer,            !- Economizer Control Type
    ModulateFlow,            !- Economizer Control Action Type
    19.0,                    !- Economizer Maximum Limit Dry-Bulb Temperature {C}
    32000.0,                 !- Economizer Maximum Limit Enthalpy {J/kg}
    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}
    ,                        !- Electronic Enthalpy Limit Curve Name
    0.0,                     !- Economizer Minimum Limit Dry-Bulb Temperature {C}
    NoLockout,               !- Economizer Lockout Type
    ,                        !- Economizer Control Schedule Name
    BypassWhenWithinEconomizerLimits,  !- Heat Recovery Bypass Control Type
    ,                        !- High Humidity Outdoor Air Control
    ,                        !- Humidistat Control Zone Name
    ,                        !- High Humidity Outdoor Air Flow Ratio
    ,             !- Control High Indoor Humidity Based on Outdoor Humidity Ratio
    ,                        !- Minimum Fraction of Outdoor Air Schedule Name
    ;                        !- Maximum Fraction of Outdoor Air Schedule Name

 Controller:OutdoorAir:ZoneList,
    VAV WITH REHEAT Zone OA List,  !- Name
    ZN_1_FLR_1_SEC_1,        !- Zone 1 Name
    CM DSOA ZN_1_FLR_1_SEC_1,!- Design Specification Outdoor Air Object Name 1
    ZoneAirDistribution,     !- Design Specification Zone Air Distribution Object Name 1
    ZN_1_FLR_1_SEC_2,        !- Zone 2 Name
    CM DSOA ZN_1_FLR_1_SEC_2,!- Design Specification Outdoor Air Object Name 2
    ZoneAirDistribution,     !- Design Specification Zone Air Distribution Object Name 2
    ZN_1_FLR_1_SEC_3,        !- Zone 3 Name
    CM DSOA ZN_1_FLR_1_SEC_3,!- Design Specification Outdoor Air Object Name 3
    ZoneAirDistribution,     !- Design Specification Zone Air Distribution Object Name 3
    ZN_1_FLR_1_SEC_4,        !- Zone 4 Name
    CM DSOA ZN_1_FLR_1_SEC_4,!- Design Specification Outdoor Air Object Name 4
    ZoneAirDistribution,     !- Design Specification Zone Air Distribution Object Name 4
    ZN_1_FLR_1_SEC_5,        !- Zone 5 Name
    CM DSOA ZN_1_FLR_1_SEC_5,!- Design Specification Outdoor Air Object Name 5
    ZoneAirDistribution;     !- Design Specification Zone Air Distribution Object Name 5


```

