# Group – Demand Limiting Controls

Demand limiting, or demand management, is a strategy for reducing a building's demand for utilities, e.g., electricity. Utility companies typically charge a monthly fee for "demand charges" that is based on the peak demand during a certain time period. Often the peak demand charge is set by one exceptional day that is much higher than the peak demand for an average day during the month. Therefore, to save utility costs, it is in the interest of building owners to find ways to manage the demand on peak days. While demand management is generally concerned with the demand for electricity, the future will likely see demand management of other utilities, such as natural gas or water.

Demand limiting controls shut off or reduce the power to non-essential loads in order to reduce the overall building demand. Some typical controls:

shut off or dim electric lights, equipment, or HVAC systems

reset the thermostatic set points on HVAC systems

reduce the load of a set of similar components by rotating one or more components "off" for a short time interval

turn on generators to meet some or all of the building's demand.

The demand limiting controls implemented in EnergyPlus are intended to allow some of the more common demand limiting strategies. Currently, only [Exterior:[Lights](#lights)](#exteriorlights), [Lights](#lights), [ElectricEquipment](#electricequipment), and [ZoneControl:Thermostat](#zonecontrolthermostat) objects can be demand limited. Additional components will be demand limited in future releases.

## DemandManagerAssignmentList

The [DemandManagerAssignmentList](#demandmanagerassignmentlist) object is a high level control that makes demand limiting decisions based on a list of possible demand limiting strategies. Each demand limiting strategy is described in a separate DemandManager object. (This is structured like the relationship between the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) and the AvailabilityManager  objects.)

Each [DemandManagerAssignmentList](#demandmanagerassignmentlist) monitors and demand limits one meter. Only electricity meters are currently allowed. In many cases, the meter will be *Electricity:Facility*. A schedule and safety factor are used to set the demand limit above which the DemandManagers become activated. The billing month schedule and demand window are also taken into account when calculating the demand limit.

DemandManagers in the list are activated based on the *Demand Manager Priority*. If the **Sequential** option is used, each manager in the list is activated in sequence from first to last until demand is reduced below the limit or until all managers are activated. If the **All** option is used, all managers are activated simultaneously to achieve the maximum demand reduction.

A DemandManager is skipped if it cannot reduce the demand. Possible reasons that a manager cannot reduce demand include:

not enough load to limit

not available because of its *Availability Schedule*

already activated; load limited during a previous timestep.

### Inputs

#### Field: Name

The name of the [DemandManagerAssignmentList](#demandmanagerassignmentlist) object.

#### Field: Meter Name

This field references the meter that is to be monitored and demand limited. Only electricity meters are currently allowed.

#### Field: Demand Limit Schedule Name

The reference to the schedule object specifying the target demand limits [schedule values should be in Watts].

#### Field: Demand Limit Safety Fraction

This field is a multiplier that is applied to the target demand limit (above). When the metered demand exceeds (target demand limit \* safety fraction), the DemandManagers begin to limit the demand. This helps to ensure that the target limit is not exceeded.

#### Field: Billing Period Schedule Name

The reference to the schedule object that defines the monthly billing periods. The peak demand during the peak period of the billing period typically determines the demand charge. This should reference the same schedule that is used in the *Month Schedule Name* field of the UtilityCost:Tariff object. If left blank, it defaults to the regular divisions between months.

#### Field: Peak Period Schedule Name

The reference to the schedule object that defines the peak/off-peak time-of-use periods. The peak demand during the peak period of the billing period typically determines the demand charge. This should reference the same schedule that is used in the *Time of Use Period Schedule Name* field of the UtilityCost:Tariff object. A value of 1 indicates the peak period; any other value is off-peak. If left blank, it defaults to always on the peak period. The [DemandManagerAssignmentList](#demandmanagerassignmentlist) only attempts to demand limit during peak periods.

#### Field: Demand Window Length

This field is similar to the field of the same name in the UtilityCost:Tariff object. However, the user may not want to limit using the same demand window as the utility company. This field allows the user to input the number of minutes over which to calculate the current demand. The minutes are rounded to match the nearest multiple of time steps.

#### Field: Demand Manager Priority

This field indicates what logic should be used to activate the individual DemandManagers in the list. **Sequential** treats the list of DemandManagers as a sequential priority list, i.e., the first DemandManager in the list is activated first, then the second, etc., until the demand falls below the limit. **All** activates all demand managers to achieve the maximum demand reduction.

#### Field: Demand Manager Type 1

The key word defining the type of DemandManager.

#### Field: Demand Manager Name 1

The name of a DemandManager object defined elsewhere in the input file.

DemandManagers are listed by pairs of data items:  *Demand Manager Type* and *Demand Manager Name*. Ten managers are accomodated in the list by default. The IDD specification, however, is extensible and additional pairs may be added by directly editing the IDD.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

        DemandManagerAssignmentList,
        Demand Manager,          !- Name
        Electricity:Facility,    !- Meter Name
        Limit Schedule,          !- Demand Limit Schedule Name
        1.0,                     !- Demand Limit Safety Fraction
        ,                        !- Billing Period Schedule Name
        ,                        !- Peak Period Schedule Name
        15,                      !- Demand Window Length {minutes}
        Sequential,              !- Demand Manager Priority
        DemandManager:ExteriorLights,  !- DemandManager 1 Object Type
        Ext Lights Manager,      !- DemandManager 1 Name
        DemandManager:ElectricEquipment,  !- DemandManager 2 Object Type
        Eq Mgr Stage 1,          !- DemandManager 2 Name
        DemandManager:ElectricEquipment,  !- DemandManager 3 Object Type
        Eq Mgr Stage 2,          !- DemandManager 3 Name
        DemandManager:ElectricEquipment,  !- DemandManager 4 Object Type
        Eq Mgr Stage 3,          !- DemandManager 4 Name
        DemandManager:Lights,    !- DemandManager 5 Object Type
        Lights Manager,          !- DemandManager 5 Name
        DemandManager:Thermostats,  !- DemandManager 6 Object Type
        Thermostats Manager;     !- DemandManager 6 Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables are reported for the [DemandManagerAssignmentList](#demandmanagerassignmentlist) object:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Demand Manager Meter Demand Power [W]
    Zone,Average,Demand Manager Average Demand Power [W]
    Zone,Average,Demand Manager Peak Demand Power [W]
    Zone,Average,Demand Manager Scheduled Limit Power [W]
    Zone,Average,Demand Manager Demand Limit Power [W]
    Zone,Average,Demand Manager Avoided Demand [W]
    Zone,Average,Demand Manager Over Limit Power [W]
    Zone,Sum,Demand Manager Over Limit Time [hr]
    Zone,Sum,Demand Manager Exterior Energy Iteration Count []
    Zone,Sum,Demand Manager Heat Balance Iteration Count []
    Zone,Sum,Demand Manager HVAC Iteration Count []
~~~~~~~~~~~~~~~~~~~~

#### Demand Manager Meter Demand Power [W]

The current demand for the designated meter.

#### Demand Manager Average Demand Power [W]

The current demand for the designated meter averaged over the *Demand [Window](#window) Length*.

#### Demand Manager Peak Demand Power [W]

The peak demand in the billing month so far.

#### Demand Manager Scheduled Limit Power [W]

The scheduled target demand limit from the *Demand Limit Schedule*.

#### Demand Manager Demand Limit Power [W]

The actual demand limit after multiplication by the *Demand Limit Safety Fraction*.

#### Demand Manager Avoided Demand [W]

The demand that was avoided by the active DemandManagers.

#### Demand Manager Over Limit Power [W]

The difference between the demand limit and the average demand.

#### Demand Manager Over Limit Time [hr]

The number of hours that the demand limit was exceeded.

#### Demand Manager Exterior Energy Iteration Count []

The number times that the exterior energy use was resimulated for demand limiting.

#### Demand Manager Heat Balance Iteration Count []

The number times that the zone heat balance was resimulated for demand limiting.

#### Demand Manager HVAC Iteration Count []

The number times that the HVAC system was resimulated for demand limiting.

## DemandManager:ExteriorLights

The [DemandManager:ExteriorLights](#demandmanagerexteriorlights) object is used for demand limiting [Exterior:Lights](#exteriorlights) objects.

### Inputs

#### Field: Name

The name of the [DemandManager:ExteriorLights](#demandmanagerexteriorlights) object.

#### Field: Availability Schedule Name

The reference to the schedule object specifying the availability of this DemandManager. A schedule value of 0 indicates that the unit is off for that time period. A schedule value greater than 0 indicates that the unit can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Limit Control

This field specifies the type of limiting control. The **Fixed** option reduces the power to the controlled loads by a fixed amount determined by the *Maximum Limit Fraction* field. The **Off** option disables the DemandManager in the simulation.

#### Field: Minimum Limit Duration

The minimum amount of time [minutes] that the DemandManager will continue to demand limit after being activated. This prevents loads from turning on and off every time step.

#### Field: Maximum Limit Fraction

This field specifies a fraction of the *Design Level* power for the load that determines the lowest allowable power level. With *Limit Control* set to **Fixed**, the power is reduced to *Maximum Limit Fraction \* Design Level* whenever the manager is activated. For example, if the *Maximum Limit Fraction* is 0.8, then a 100 W design level load is reduced to 80 W when limiting. A *Maximum Limit Fraction* of zero means that the load can be shut off entirely.

#### Field: Limit Step Change

NOT YET IMPLEMENTED.

#### Field: Selection Control

This field specifies which loads are selected to be limited. The **All** option simultaneously limits all of the loads listed in this DemandManager. The **RotateMany** option limits all loads except for one which rotates sequentially through the loads listed. The **RotateOne** limits only one load which rotates sequentially through the loads listed. The time interval between rotations is set by the *Rotation Duration* field.

#### Field: Rotation Duration

If the **RotateOne** or **RotateMany** option is used for *Selection Control*, this field sets the time interval [minutes] between rotations.

#### Field: Exterior Lights 1-10 Name

The names of [Exterior:Lights](#exteriorlights) objects defined elsewhere in the input file. These are the loads to be limited by this DemandManager.

Ten objects are accomodated in the list by default. The IDD specification, however, is extensible and additional fields may be added by directly editing the IDD.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      DemandManager:ExteriorLights,
        Ext Lights Manager,      !- Name
        ,                        !- Availability Schedule Name
        Fixed,                   !- Limit Control
        60,                      !- Minimum Limit Duration {minutes}
        0.0,                     !- Maximum Limit Fraction
        ,                        !- Limit Step Change
        All,                     !- Selection Control
        ,                        !- Rotation Duration {minutes}
        Exterior Lights;         !- Exterior Lights 1 Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

There are no output variables reported for the [DemandManager:ExteriorLights](#demandmanagerexteriorlights) object.

## DemandManager:Lights

The [DemandManager:[Lights](#lights)](#demandmanagerlights) object is used for demand limiting [Lights](#lights) objects.

### Inputs

#### Field: Name

The name of the [DemandManager:Lights](#demandmanagerlights) object.

#### Field: Availability Schedule Name

The reference to the schedule object specifying the availability of this DemandManager. A schedule value of 0 indicates that the unit is off for that time period. A schedule value greater than 0 indicates that the unit can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Limit Control

This field specifies the type of limiting control. The **Fixed** option reduces the power to the controlled loads by a fixed amount determined by the *Maximum Limit Fraction* field. The **Off** option disables the DemandManager in the simulation.

#### Field: Minimum Limit Duration

The minimum amount of time [minutes] that the DemandManager will continue to demand limit after being activated. This prevents loads from turning on and off every time step.

#### Field: Maximum Limit Fraction

This field specifies a fraction of the *Design Level* power for the load that determines the lowest allowable power level. With *Limit Control* set to **Fixed**, the power is reduced to *Maximum Limit Fraction \* Design Level* whenever the manager is activated. For example, if the *Maximum Limit Fraction* is 0.8, then a 100 W design level load is reduced to 80 W when limiting. A *Maximum Limit Fraction* of zero means that the load can be shut off entirely.

#### Field: Limit Step Change

NOT YET IMPLEMENTED.

#### Field: Selection Control

This field specifies which loads are selected to be limited. The **All** option simultaneously limits all of the loads listed in this DemandManager. The **RotateMany** option limits all loads except for one which rotates sequentially through the loads listed. The **RotateOne** limits only one load which rotates sequentially through the loads listed. The time interval between rotations is set by the *Rotation Duration* field.

#### Field: Rotation Duration

If the **RotateOne** or **RotateMany** option is used for *Selection Control*, this field sets the time interval [minutes] between rotations.

#### Field: Lights 1-10 Name

The names of [Lights](#lights) objects defined elsewhere in the input file. These are the loads to be limited by this DemandManager.

Ten objects are accomodated in the list by default. The IDD specification, however, is extensible and additional fields may be added by directly editing the IDD.

If a global lights statement is used (Ref: [Lights](#lights)), then only that name need be entered and the demand limiting will be applied to all the zones for that lighting. If only one of a set of zones from a global lights statement is desired, then the name to be entered is: <[Zone](#zone) Name> <Name of [Lights](#lights) Global Object>.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      DemandManager:Lights,
        Lights Manager,          !- Name
        ,                        !- Availability Schedule Name
        Fixed,                   !- Limit Control
        60,                      !- Minimum Limit Duration {minutes}
        0.5,                     !- Maximum Limit Fraction
        ,                        !- Limit Step Change
        All,                     !- Selection Control
        ,                        !- Rotation Duration {minutes}
        Space1-1 Lights 1,       !- Lights 1 Name
        Space2-1 Lights 1,       !- Lights 2 Name
        Space3-1 Lights 1,       !- Lights 3 Name
        Space4-1 Lights 1,       !- Lights 4 Name
        Space5-1 Lights 1;       !- Lights 5 Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

There are no output variables reported for the [DemandManager:Lights](#demandmanagerlights) object.

## DemandManager:ElectricEquipment

The [DemandManager:[ElectricEquipment](#electricequipment)](#demandmanagerelectricequipment) object is used for demand limiting [ElectricEquipment](#electricequipment) objects.

### Inputs

#### Field: Name

The name of the [DemandManager:ElectricEquipment](#demandmanagerelectricequipment) object.

#### Field: Availability Schedule Name

The reference to the schedule object specifying the availability of this DemandManager. A schedule value of 0 indicates that the unit is off for that time period. A schedule value greater than 0 indicates that the unit can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Limit Control

This field specifies the type of limiting control. The **Fixed** option reduces the power to the controlled loads by a fixed amount determined by the *Maximum Limit Fraction* field. The **Off** option disables the DemandManager in the simulation.

#### Field: Minimum Limit Duration

The minimum amount of time [minutes] that the DemandManager will continue to demand limit after being activated. This prevents loads from turning on and off every time step.

#### Field: Maximum Limit Fraction

This field specifies a fraction of the *Design Level* power for the load that determines the lowest allowable power level. With *Limit Control* set to **Fixed**, the power is reduced to *Maximum Limit Fraction \* Design Level* whenever the manager is activated. For example, if the *Maximum Limit Fraction* is 0.8, then a 100 W design level load is reduced to 80 W when limiting. A *Maximum Limit Fraction* of zero means that the load can be shut off entirely.

#### Field: Limit Step Change

NOT YET IMPLEMENTED.

#### Field: Selection Control

This field specifies which loads are selected to be limited. The **All** option simultaneously limits all of the loads listed in this DemandManager. The **RotateMany** option limits all loads except for one which rotates sequentially through the loads listed. The **RotateOne** limits only one load which rotates sequentially through the loads listed. The time interval between rotations is set by the *Rotation Duration* field.

#### Field: Rotation Duration

If the **RotateOne** or **RotateMany** option is used for *Selection Control*, this field sets the time interval [minutes] between rotations.

#### Field: Electric Equipment 1-10 Name

The names of [ElectricEquipment](#electricequipment) objects defined elsewhere in the input file. These are the loads to be limited by this DemandManager.

Ten objects are accomodated in the list by default. The IDD specification, however, is extensible and additional fields may be added by directly editing the IDD.

If a global Electric Equipment statement is used (Ref: [ElectricEquipment](#electricequipment)), then only that name need be entered and the demand limiting will be applied to all the zones for that equipment. If only one of a set of zones from a global electric equipment statement is desired, then the name to be entered is: <[Zone](#zone) Name> <Name of [ElectricEquipment](#electricequipment) Global Object>.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      DemandManager:ElectricEquipment,
        Eq Mgr Stage 3,          !- Name
        ,                        !- Availability Schedule Name
        Fixed,                   !- Limit Control
        60,                      !- Minimum Limit Duration {minutes}
        0.0,                     !- Maximum Limit Fraction
        ,                        !- Limit Step Change
        All,                     !- Selection Control
        ,                        !- Rotation Duration {minutes}
        Space5-1 ElecEq 1;       !- Electric Equipment 1 Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

There are no output variables reported for the [DemandManager:ElectricEquipment](#demandmanagerelectricequipment) object.

## DemandManager:Thermostats

The [DemandManager:Thermostats](#demandmanagerthermostats) object is used for demand limiting [ZoneControl:Thermostat](#zonecontrolthermostat) objects.

### Inputs

#### Field: Name

The name of the [DemandManager:Thermostats](#demandmanagerthermostats) object.

#### Field: Availability Schedule Name

The reference to the schedule object specifying the availability of the DemandManager. A schedule value of 0 indicates that the unit is off for that time period. A schedule value greater than 0 indicates that the unit can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Reset Control

This field specifies the type of limiting control. The **Fixed** option resets the thermostats to a fixed temperature determined by the *Maximum Heating Setpoint Reset* and *Maximum Cooling Setpoint Reset* fields. The **Off** option disables the DemandManager in the simulation.

#### Field: Minimum Reset Duration

The minimum amount of time [minutes] that the DemandManager will continue to demand limit after being activated. This prevents thermostats from resetting on and off every time step.

#### Field: Maximum Heating Setpoint Reset

This field specifies the thermostat heating setpoint temperature [C] that represents the largest allowable temperature reset or setback. With *Reset Control* set to **Fixed**, the setpoint temperature is reset to the *Maximum Heating Setpoint Reset* whenever the manager is activated. For example, if the heating setpoint is normally 22 C and the *Maximum Heating Setpoint Reset* is 20 C, then the setpoint is reset to 20 C when limiting.

#### Field: Maximum Cooling Setpoint Reset

This field specifies the thermostat cooling setpoint temperature [C] that represents the largest allowable temperature reset or setback. With *Reset Control* set to **Fixed**, the setpoint temperature is reset to the *Maximum Cooling Setpoint Reset* whenever the manager is activated. For example, if the cooling setpoint is normally 22 C and the *Maximum Cooling Setpoint Reset* is 24 C, then the setpoint is reset to 24 C when limiting.

#### Field: Reset Step Change

NOT YET IMPLEMENTED.

#### Field: Selection Control

This field specifies which loads are selected to be limited. The **All** option simultaneously limits all of the loads listed in this DemandManager. The **RotateMany** option limits all loads except for one which rotates sequentially through the loads listed. The **RotateOne** limits only one load which rotates sequentially through the loads listed. The time interval between rotations is set by the *Rotation Duration* field.

#### Field: Rotation Duration

If the **RotateOne** or **RotateMany** option is used for *Selection Control*, this field sets the time interval [minutes] between rotations.

#### Field: Thermostat 1-10 Name

The names of [ZoneControl:Thermostat](#zonecontrolthermostat) objects defined elsewhere in the input file. These are the loads to be limited by this DemandManager.

Ten objects are accomodated in the list by default. The IDD specification, however, is extensible and additional fields may be added by directly editing the IDD.

If a global thermostat statement is used (Ref: [ZoneControl:Thermostat](#zonecontrolthermostat)), then only that name need be entered and the demand limiting will be applied to all the zones for that thermostatic control. If only one of a set of zones from a global thermostat statement is desired, then the name to be entered is: <[Zone](#zone) Name> <Name of Thermostat Global Object>.

An example IDF showing how this object is used is provided below:

~~~~~~~~~~~~~~~~~~~~

      DemandManager:Thermostats,
        Thermostats Manager,     !- Name
        ,                        !- Availability Schedule Name
        Fixed,                   !- Reset Control
        60,                      !- Minimum Reset Duration {minutes}
        19,                      !- Maximum Heating Setpoint Reset {C}
        26,                      !- Maximum Cooling Setpoint Reset {C}
        ,                        !- Reset Step Change
        All,                     !- Selection Control
        ,                        !- Rotation Duration {minutes}
        SPACE1-1 Control,        !- Thermostat 1 Name
        SPACE2-1 Control,        !- Thermostat 2 Name
        SPACE3-1 Control,        !- Thermostat 3 Name
        SPACE4-1 Control,        !- Thermostat 4 Name
        SPACE5-1 Control;        !- Thermostat 5 Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

There are no output variables reported for the [DemandManager:Thermostats](#demandmanagerthermostats) object.