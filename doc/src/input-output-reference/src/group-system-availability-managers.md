# Group – System  Availability Managers

System Availability Managers are one of the high-level control constructs in EnergyPlus. A System Availability Manager is able to access data from any of the HVAC system nodes and use this data to to make a decision on whether an entire [AirLoopHVAC](#airloophvac) or [PlantLoop](#plantloop) should be on or off. Some of the zone component objects  (e.g. unit heater, unit ventilator, packaged terminal air conditioner, packaged terminal heat pump, zone water source heat pump,window air conditioner, zone terminal unit (variable refrigerant flow), energy recovery ventilator, outdoor air unit, fan coil unit, and ventilated slab) can also individually use availability managers to make a decision regarding whether their fan should be on or off.

System Availability Managers are executed at the start of each HVAC timestep. They reside outside the HVAC system iteration loops. Thus, the System Availability Managers are executed once per HVAC timestep, and they use previous timestep information (except for zone load) to calculate their set points.

Some of the managers monitor the temperature at an air or plant node to determine whether the system should be on or off. If the system is to be controlled on the outdoor dry-bulb temperature, an [OutdoorAir:NodeList](#outdoorairnodelist) object can be used to define a node which will have the current outdoor air conditions.

The output from each System Availability Manager is an availability status flag. This flag can have the values *NoAction*, *ForceOff*, *CycleOn*, or *CycleOnZoneFansOnly*. The availability status flags for the System Availability Managers referenced by an air or plant loop are used to set the availability status flag for each loop. For the air loops and zone components, *ForceOff* takes precedence: if any of the loop's (or zone component's) availability managers are showing status *ForceOff*, the loop (or zone component fan) status will be *ForceOff*. Next in precedence is *CycleOnZoneFansOnly* (only for air loop), **followed by *CycleOn,* and *NoAction*. For the plant loops, there is no precedence among the System Availability Manager status flag values. Instead, the first availability manager giving a status flag value other than *NoAction* sets the status for the loop. The System Availability Managers are executed in [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) order.

The actual action of turning on or off a loop is taken by the loop prime movers: fans for AirLoopHVACs and zone components, and pumps for plant loops. For instance when a fan is deciding whether it is on or off, it checks its on/off schedule and whether the loop availability status flag is *CycleOn* or *ForceOff*. If the schedule is on and the status flag is *ForceOff*, the fan will be off. If the fan schedule says off and the status flag is *CycleOn*, the fan will be on. Thus the availability managers overrule the fan on/off schedule. The availability managers, air loops, and plant loops all have output variables which may be used to verify the action of the loop.

Availability managers for [AirLoopHVAC](#airloophvac) systems also control the availability of zone exhaust fans (Ref. [Fan:ZoneExhaust](#fanzoneexhaust)) and terminal unit fans (Ref. AirTerminal:SingleDuct:SeriesPIU: Reheat, [AirTerminal:SingleDuct:ParallelPIU:Reheat](#airterminalsingleductparallelpiureheat), and AirTerminal:SingleDuct:VAV: Reheat:VariableSpeedFan) in the zones served by the [AirLoopHVAC](#airloophvac) system.

## AvailabilityManager:Scheduled

The simplest System Availability Manager is when the availability is determined by an on/off schedule.The syntax for implementing such an availability manager is shown below. The identifying name refers back to the name recorded in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) statement described in the **Group – Air Distribution** section. The schedule must be the name of a schedule defined elsewhere in the input.

### Inputs

#### Field: Name

The unique, user-assigned name of an instance of a scheduled availability manager. Other objects that use this scheduled availability manager will reference it by this name.

#### Field: Schedule Name

The name of a schedule defined elsewhere in the input. Schedule values greater than zero (usually 1 is used) indicate that the system is on. Schedule values less than or equal to zero (usually 0 is used) denote that the system is off. This schedule overides the central fan schedule for determining whether the fan is on.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:Scheduled,
        VAV Sys 1 Avail, !- Name
        FanAvailSched;   !- Schedule Name
~~~~~~~~~~~~~~~~~~~~

[AvailabilityManager:Scheduled](#availabilitymanagerscheduled) Outputs

The scheduled availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Availability Manager Scheduled Control Status
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager Scheduled Control Status

A value of 1 indicates the manager is signaling *ForceOff*. A value of 2 means the manager is signaling *CycleOn*.

## AvailabilityManager:ScheduledOn

The Schedule On Availability Manager is used when equipment must be turned on during a given time period. This availability manager will not turn the equipment off, a separate availability manager must be used to disable the equipment as necessary. The scheduled on availability is determined by an on schedule. Schedule values other than 0 set a *CycleOn* availability status. Schedule values equal to 0 set a *NoAction* availability status. The syntax for implementing such an availability manager is shown below. The identifying name refers back to the name recorded in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) statement described in the **Group – Air Distribution** section. The schedule must be the name of a schedule defined elsewhere in the input.

### Inputs

#### Field: Name

The unique, user-assigned name of an instance of a scheduled on availability manager. Other objects that use this scheduled on availability manager will reference it by this name.

#### Field: Schedule Name

The name of a schedule defined elsewhere in the input. Schedule values greater than zero (usually 1 is used) indicate that the system is on. Schedule values less than or equal to zero (usually 0 is used) denote that *NoAction* is desired. This schedule overides the central fan schedule for determining whether the fan is on.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:ScheduledOn,
        VAV Sys 1 Avail On,  !- Name
        FanAvailSched;       !- Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The scheduled on availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Availability Manager Scheduled On Control Status
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager Scheduled On Control Status []

A value of 0 indicates the manager is signaling *NoAction*. A value of 2 means the manager is signaling *CycleOn*.

## AvailabilityManager:ScheduledOff

The Schedule Off Availability Manager is used when equipment must be turned off during a given time period. This availability manager will not turn the equipment on, a separate availability manager must be used to enable the equipment as necessary. The scheduled off availability is determined by an off schedule. Schedule values equal to 0 set a *ForceOff* availability status. Schedule values other than 0 (usually a 1 is used) set a *NoAction* availability status. The syntax for mplementing such an availability manager is shown below. The identifying name refers back to the name recorded in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) statement described in the **Group – Air Distribution** section. The schedule must be the name of a schedule defined elsewhere in the input.

### Inputs

#### Field: Name

The unique, user-assigned name of an instance of a scheduled off availability manager. Other objects that use this scheduled off availability manager will reference it by this name.

#### Field: Schedule Name

The name of a schedule defined elsewhere in the input. Schedule values equal to zero indicate that the system is off. Schedule values other than (usually 0 is used) denote that *NoAction* is desired.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:ScheduledOff,
        VAV Sys 1 Avail Off,   !- Name
        FanAvailSched;         !- Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The scheduled off availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Availability Manager Scheduled Off Control Status
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager Scheduled Off Control Status []

A value of 0 indicates the manager is signaling *NoAction*. A value of 1 means the manager is signaling *ForceOff*.

## AvailabilityManager:NightCycle

This manager is used for cycling on an air system when one or more zones become too hot or too cold. The usual situation is that the central air handler is turned off at night. However if the building gets too cold there might be condensation on the walls and other damage. Thus the control system is usually programmed to turn the system on if 1 control thermostat or any thermostat shows a zone temperature of less than a night time set point. Similarly there might be a concern about a building getting too hot. Again the control system is programmed to turn the air handler back on if one or any zone temperature exceeds a night time cooling set point.

This object gives the user considerable flexibility in determining how the night time on/off decision will be made. The manager can look at the temperature in 1 zone or it can sample all the zones connected to the air handler. The user can specify a temperature tolerance and a run time for the system once it switches on. There is also an applicability schedule – the user can schedule whether or not this availability manager itself is being applied.

**Field: Name**

A unique, user-assigned name for an instance of a night cycle availability manager. Other objects that use this availability manager will reference it by this name.

**Field: Applicability Schedule Name**

The name of a schedule defined elsewhere in the input file. This schedule determines whether or not for a given time period this availability manager is to be applied. Schedule values greater than zero (usually 1 is used) indicate the availability manager is to be applied. Schedule values less than or equal to zero (usually 0 is used) denote that the availability manager is not used for this time period.

**Field: Fan Schedule Name**

The name of the central fan on/off schedule for the air system that this availability manager will affect.

**Field: Control Type**

The possible inputs are *StayOff*, *CycleOnAny*, *CycleOnControlZone*, *CycleOnAnyZoneFansOnly*. *StayOff* means the availability manager will have no effect – air handler on/off will be determined by the fan schedule. *CycleOnAny* means that if any zone served by a system whose [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) references this system availability manager has an air temperature outside the cooling or heating set points the central fan will turn on even though the fan schedule indicates the fan is off. *CycleOnControlZone* means the same thing except the availability manager looks at the temperature in only one zone. *CycleOnAnyZoneFansOnly* is the same as *CycleOnAny* except that only the zone fans are cycled on and the central air handler fan is left off. The default is *StayOff*.

**Field: Thermostat Tolerance**

The Thermostat Tolerance defines the amount by which the zone temperature must fall outside the current zone heating and cooling setpoints for the Night Cycle manager to turn the system on. The zone temperature must exceed the cooling set point or fall below the heating set point by 1/2 the Thermostat Tolerance in order for the availability manager to signal that the system should turn on. The Thermostat Tolerance is specified in degrees Celsius. The default is 1 degree C.

**Field: Cycling Run Time**

The time in seconds for which the system will run after it has cycled on. The default is 3600 seconds (1 hour).

**Field: Control [Zone](#zone) Name**

For the option *CycleOnControlZone* this is the name of the control zone.

An example of this statement in an IDF is:

[AvailabilityManager:NightCycle](#availabilitymanagernightcycle),

    VAV Sys 1 Avail,     ! Name

    SysAvailApplicSch,   ! Applicability Schedule Name

    FanAvailSched,       ! Fan Schedule Name

    CycleOnAny,          ! Control Type

    4.0,                 ! Temperature Tolerance {delta C}

    7200.;               ! Cycle Run Time {s}

### Inputs

### Outputs

The night cycle availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Availability Manager Night Cycle Control Status
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager Night Cycle Control Status

A value of 0 indicates the manager is signaling *NoAction*. A value of 2 means the manager is signaling *CycleOn*. A value of 3 says that the manager is signaling *CycleOnZoneFansOnly*.

## AvailabilityManager:DifferentialThermostat

The differential thermostat compares the temperatures at two different system nodes. If the temperature difference (T) is above the *Temperature Difference On Limit*, the system is turned on. If the T is below the *Temperature Difference Off Limit*, the system is turned off. Between the *On Limit* and *Off Limit* the system is either on or off depending on the previous state of the thermostat. Unlike other availability managers, the differential thermostat is always either "On" or "Off"; it does not use the "No Action" state. Therefore, the differential thermostat must be the last manager in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist). Any availability managers after the differential thermostat manager will never be simulated.

One application of the differential thermostat is to ensure a useful heat gain for solar heating systems, i.e. the system is only turned on when there is a significant temperature difference between the solar collector outlet node and the storage tank outlet node.

### Inputs

#### Field: Name

The name of the object. This is referenced in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist).

#### Field: Hot Node Name

The sensor node with the higher temperature.

#### Field: Cold Node Name

The sensor node with the colder temperature.

#### Field: Temperature Difference On Limit

Temperature difference [deltaC] between hot and cold nodes necessary to turn the system on.

#### Field: Temperature Difference Off Limit

Temperature difference [deltaC] between hot and cold nodes necessary to turn the system off. This field defaults to the *Temperature Difference On Limit*.

An example of this object in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:DifferentialThermostat,
      Differential Thermostat Availability Manager,  !- Name
      Collector Outlet Node,            !- Hot Node Name
      Water Heater Source Outlet Node,  !- Cold Node Name
      10.0,  !- Temperature Difference On Limit {delta C}
      2.0;   !- Temperature Difference Off Limit {delta C}
~~~~~~~~~~~~~~~~~~~~

### Outputs

The differential thermostat availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Availability Manager Differential Thermostat Control Status
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager Differential Thermostat Control Status

A value of 1 indicates the manager is signaling *ForceOff*. A value of 2 means the manager is signaling *CycleOn*.

## AvailabilityManager:HighTemperatureTurnOff

This manager turns the system off if the temperature at the sensor node is above the specified setpoint temperature.

### Inputs

#### Field: Name

The name of the object. This is referenced in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist).

#### Field: Sensor Node Name

The air or plant node where the temperature is monitored.

#### Field: Temperature

The setpoint temperature [C] at which the system is turned off.

An example of this object in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:HighTemperatureTurnOff,
      High Temperature Turn Off Availability Manager,  !- Name
      Water Heater Use Outlet Node,  !- Sensor Node Name
      60.0;  !- Temperature {C}
~~~~~~~~~~~~~~~~~~~~

### Outputs

The high temperature turn off availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Availability Manager High Temperature Turn Off Control Status
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager High Temperature Turn Off Control Status []

*A value of 0 indicates the manager is signaling NoAction*. A value of 1 means the manager is signaling *ForceOff.*

## AvailabilityManager:HighTemperatureTurnOn

This manager turns the system on if the temperature at the sensor node is above the specified setpoint temperature.

### Inputs

#### Field: Name

The name of the object. This is referenced in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist).

#### Field: Sensor Node Name

The air or plant node where the temperature is monitored.

#### Field: Temperature

The setpoint temperature [C] at which the system is turned on.

An example of this object in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:HighTemperatureTurnOn,
      High Temperature Turn On Availability Manager,  !- Name
      Outside Air Inlet Node,  !- Sensor Node
      30.0;  !- Temperature {C}
~~~~~~~~~~~~~~~~~~~~

### Outputs

The high temperature turn on availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Availability Manager High Temperature Turn On Control Status
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager High Temperature Turn On Control Status []

*A value of 0 indicates the manager is signaling NoAction*. A value of 2 means the manager is signaling *CycleOn.*

## AvailabilityManager:LowTemperatureTurnOff

This manager turns the system off if the temperature at the sensor node is below the specified setpoint temperature.

### Inputs

#### Field: Name

The name of the object. This is referenced in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist).

#### Field: Sensor Node Name

The air or plant node where the temperature is monitored.

#### Field: Temperature

The setpoint temperature [C] at which the system is turned off.

#### Field: Applicability Schedule Name

The name of a schedule defined elsewhere in the input file. This schedule determines whether or not for a given time period this availability manager is to be applied. Schedule values greater than zero (usually 1 is used) indicate the availability manager is to be applied. Schedule values less than or equal to zero (usually 0 is used) denote that the availability manager is not used for this time period. If this field is left blank, this availability manager is always active.

An example of this object in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:LowTemperatureTurnOff,
      Low Temperature Turn Off Availability Manager,  !- Name
      Outside Air Inlet Node,  !- Sensor Node Name
      0.0,  !- Temperature {C}
      Low Temp Control Schedule;  !- Applicability Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The low temperature turn off availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Availability Manager Low Temperature Turn Off Control Status
~~~~~~~~~~~~~~~~~~~~

Availability Manager Low Temperature Turn Off Control Status []

A value of 0 indicates the manager is signaling NoAction. A value of 1 means the manager is signaling ForceOff.

## AvailabilityManager:LowTemperatureTurnOn

This manager turns the system on if the temperature at the sensor node is below the specified setpoint temperature.

### Inputs

#### Field: Name

The name of the object. This is referenced in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist).

#### Field: Sensor Node Name

The air or plant node where the temperature is monitored.

#### Field: Temperature

The setpoint temperature [C] at which the system is turned on.

An example of this object in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:LowTemperatureTurnOn,
      Low Temperature Turn On Availability Manager,  !- Name
      Collector Outlet Node,  !- Sensor Node Name
      0.0;  !- Temperature {C}
~~~~~~~~~~~~~~~~~~~~

### Outputs

The low temperature turn on availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Availability Manager Low Temperature Turn On Control Status []
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager Low Temperature Turn On Control Status []

A value of 0 indicates the manager is signaling NoAction. A value of 2 means the manager is signaling CycleOn.

## AvailabilityManager:NightVentilation

This manager allows the user to implement a strategy for precooling a building at night using outdoor air. This strategy can consist of running the system fans with the outdoor air dampers open when outdoor conditions are favorable for a precooling strategy. The zone terminal unit air dampers may also be held at their fully open position to minimize fan energy consumption while precooling. Fan energy consumption is the critical parameter in deciding whether a precooling strategy will save energy.

The inputs for the night ventilation manager consist of an applicability schedule name (determining when the manager is active), a fan schedule name (for the fan(s) that the manager can switch on), a ventilation temperature schedule name (one zone must be above this temperature for night venting to be active), a ventilation temperature difference (indoor – outdoor temperature difference for night venting), a low limit temperature (no conditioned zone may fall below this temperature during night venting),  a night venting flow fraction (fraction of the design flow rate at which the fans run during night venting), and the zone name of a control zone.

The night ventilation manager is used in conjunction with another object: [FanPerformance:NightVentilation](#fanperformancenightventilation). The performance of the fan during night venting may be very different than during normal operation (a different fan might even be used). The [FanPerformance:NightVentilation](#fanperformancenightventilation) object allows the user to specify alternate fan performance parameters for use during night ventilation.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a night ventilation availability manager. Other objects that use this availability manager will reference it by this name.

#### Field: Applicability Schedule Name

The name of a schedule defined elsewhere in the input file. This schedule determines whether or not for a given time period this availability manager is to be applied. Schedule values greater than zero (usually 1 is used) indicate the availability manager is to be applied. Schedule values less than or equal to zero (usually 0 is used) denote that the availability manager is not used for this time period.

#### Field: Fan Schedule Name

The name of the central fan on/off schedule for the air system that this availability manager will affect.

#### Field: Ventilation Temperature Schedule Name

The name of a temperature schedule defined elsewhere in the input file. At least one conditioned zone in the forced air system using this availability manager must be above the current temperature specified in this schedule for night ventilation to occur or to continue occurring.

#### Field: Ventilation Temperature Difference

This field specifies an indoor / outdoor temperature difference (in degrees C). The control zone temperature minus the outdoor air temperature must be greater than the Ventilation Temperature Difference for night ventilation to occur or to continue occurring. The default is 2 degrees C.

#### Field: Ventilation Temperature Low Limit

This field specifies a lower limit (in degrees C) on zone temperatures for night ventilation. If any conditioned zone served by the air system using this availability manager is below the ventilation temperature low limit, night ventilation will not occur or will switch off. The default is 15 degrees C.

#### Field: Night Venting Flow Fraction

The fraction (could be greater than 1) of the design flow rate at which the night ventilation will be done. The default is 1.

#### Field: Control Zone Name

The name of the control zone used in the ventilation temperature difference check.

An example of this object in an input file:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:NightVentilation,
        VAV Sys 1 Avail,         !- Name
        NightVentSched,          !- Applicability Schedule Name
        FanAvailSched,           !- Fan Schedule Name
        VentTempSched,           !- Ventilation Temperature Schedule Name
        2.0,                     !- Ventilation Temperature Difference  {C}
        15.,                     !- Ventilation Temperature Low Limit
        0.3333,                  !- Night Venting Flow Fraction
        SPACE3-1;                !- Control Zone Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The night ventilation availability manager has one output variable.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Availability Manager Night Ventilation Control Status []
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager Night Ventilation Control Status []

A value of 0 indicates the manager is signaling NoAction. A value of 2 means the manager is signaling CycleOn.

## AvailabilityManager:HybridVentilation

This availability manager is executed at the start of each HVAC system timestep, before EnergyPlus calculates airflows defined in the objects ZoneInfiltration:\*, ZoneVentilation:\*, [ZoneMixing](#zonemixing), and [ZoneCrossMixing](#zonecrossmixing), and before the AirflowNetwork model calculates multizone airflows. It serves two purposes: 1) prevents simultaneous natural ventilation and HVAC system operation, and 2) allows users to examine various ventilation strategies to maximize natural ventilation in order to reduce heating/cooling loads. The natural ventilation objects controlled by this availability manager have two groups: simple airflow group and AirflowNetwork group. The simple airflow group consists of the ZoneVentilation;\* and [ZoneMixing](#zonemixing) objects. The AirflowNetwork group consists of [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening) and [AirflowNetwork:MultiZone:Component:SimpleOpening](#airflownetworkmultizonecomponentsimpleopening) objects used in the AirflowNetwork model. These two groups are not allowed to work simultaneously (see restrictions in the Airflow Network model section). Basically, this object overrides the controls for these opening objects or ZoneVentilation:\* and [ZoneMixing](#zonemixing) objects, closing the "openings" or shutting off ventilation/mixing airflows under certain conditions and allowing the HVAC system ([AirLoopHVAC](#airloophvac)) to operate. If the availability manager determines that conditions are favorable for natural ventilation, then the HVAC system ([AirLoopHVAC](#airloophvac)) is disabled and the "opening" objects or ventilation/mixing objects are able to operate based on the controls specified for those objects.

Each [AirLoopHVAC](#airloophvac) can have a corresponding hybrid ventilation availability manager. Each hybrid ventilation manager can control natural ventilation in the zones served by the [AirLoopHVAC](#airloophvac). The hybrid ventilation availability manager is triggered by zone air conditions for the controlled zone specified in an object input field. If there is no air loop, hybrid ventilation manager can still be applied to controlled zone specified in the object. To apply hybrid ventilation manager to the controlled zone not served by any air loop, the HVAC air loop name input field must be left blank. Currently, zone component objects such as unit heater, unit ventilator, packaged terminal air conditioner, packaged terminal heat pump, zone water source heat pump, window air conditioner, variable refrigerant flow, energy recovery ventilator, outdoor air unit, fan coil unit, and ventilated slab can individually use hybrid ventilation managers to make a decision regarding whether their fan should be on or off. Also, hybrid ventilation managers can be applied to zones served by ideal load zone components to turn them off when natural ventilation is active. Currently, hybrid ventilation manager is restricted to one per zone. It can either be applied through the air loop or directly to the zone. If hybrid ventilation manager is applied to an air loop and one of the zones served by the air loop also has hybrid ventilation manager, then zone hybrid ventilation manager is disabled. Presently, this availability manager must be used either with the ZoneVentilation:\* and [ZoneMixing](#zonemixing) objects or with the AirflowNetwork model.

The inputs for this availability manager consist of the name for the air loop ([AirLoopHVAC](#airloophvac)) being controlled, the controlled zone name to determine which zone air conditions are used to determine system availability, a ventilation control mode schedule, maximum wind speed and rain indicator to determine whether natural ventilation is allowed or not, a low limit and high limit for dry-bulb temperature, enthalpy and dewpoint temperature to determine the hybrid ventilation control, and a minimum outdoor air ventilation schedule. The last control allows user to use a wind speed modifier to adjust openness when the AirflowNetwork opening objects are selected. The other inputs include how to control simple airflow objects and AirflowNetwork opening objects.

The hybrid ventilation availability manager works independently of all other system availability manager objects, so this manager is not a valid system availability manager type in the [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist) object.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a hybrid ventilation availability manager. Other objects that use this availability manager will reference it by this name.

#### Field: HVAC Air Loop Name

The name of the air loop ([AirLoopHVAC](#airloophvac) object) to be controlled by this system availability manager. If this field is left blank, hybrid ventilation manager will be applied to the controlled zone specified in the field below.

#### Field: Controlled Zone Name

The name of a controlled zone served by the air loop defined in the previous field. The air conditions in this zone are used to determine if natural ventilation should be provided.

#### Field: Ventilation Control Mode Schedule Name 

The name of a schedule defined elsewhere in the input file. This schedule determines whether or not for a given time the hybrid ventilation control mode is to be applied. Schedule values equal to zero indicate no ventilation control, resulting in natural ventilation and HVAC system operation being performed based on their own controls. Schedule values equal to one denote temperature control for either cooling or heating, which is determined internally based on thermostat set point and temperature control type. The temperature control is restricted between the minimum and maximum outdoor temperatures provided in two additional input fields (below). Schedule values equal to two denote enthalpy control, which is restricted between minimum and maximum outdoor enthalpy values given in two additional input fields (below). Schedule values equal to three denote dewpoint control for either dehumidification or humidification. Schedule values equal to four represent outdoor ventilation air control. The outdoor ventilation air control works with AirflowNetwork opening objects only, and is not allowed to work with ZoneVentilation:\* and [ZoneMixing](#zonemixing) objects.

The detailed control logic is given in the EnergyPlus Engineering Reference.

#### Field: Use Weather File Rain Indicators

This logical alpha field indicates whether or not the rain indicator is used to shut off natural ventilation or not. The valid choices for Rain Indicator are Yes and No, with the default being Yes. This can help simulate conditions where one would normally close windows to avoid rain penetration in a space. Any possible rain getting into a space will not be counted as a zone load.

#### Field: Maximum Wind Speed

This is the wind speed (m/s) above which hybrid ventilation is shut off.  This can help simulate conditions where one would normally close windows to avoid wind problems in a space (papers blowing around, etc).

#### Field: Minimum Outdoor Temperature

This is the outdoor temperature (in Celsius) below which hybrid ventilation is shut off when the ventilation control mode = 1 (Temperature). This lower temperature limit is intended to avoid overcooling a space, which could result in a heating load.

#### Field: Maximum Outdoor Temperature

This is the outdoor temperature (in Celsius) above which hybrid ventilation is shut off when the ventilation control mode = 1 (Temperature). This upper temperature limit is intended to avoid overheating a space, which could result in a cooling load.

#### Field: Minimum Outdoor Enthalpy

This is the outdoor enthalpy (in J/kg) below which hybrid ventilation is shut off when the ventilation control mode = 2 (Enthalpy).

#### Field: Maximum Outdoor Enthalpy

This is the outdoor enthalpy (in J/kg) above which hybrid ventilation is shut off when the ventilation control mode = 2 (Enthalpy).

#### Field: Minimum Outdoor Dewpoint

This is the outdoor dewpoint (in Celsius) below which hybrid ventilation is shut off when the ventilation control mode = 3 (Dewpoint). This lower dewpoint temperature limit is intended to avoid dehumidifying a space.

#### Field: Maximum Outdoor Dewpoint

This is the outdoor dewpoint temperature (in Celsius) above which hybrid ventilation is shut off when the ventilation control mode = 3 (Dewpoint). This upper dewpoint temperature limit is intended to avoid humidifying a space.

#### Field: Minimum Outdoor Ventilation Air Schedule Name

The name of a schedule defined elsewhere in the input file. This field applies only if Ventilation Control Mode = 4 (Outdoor Ventilation Air Control). This schedule determines the minimum outdoor ventilation air for a given time in the units of air change per hour (ACH). The program calculates the natural (outdoor) ventilation in the controlled zone first and compares the amount of outdoor air introduced by opening windows or doors and other small openings to this minimum value. If the amount of outdoor air from natural ventilation is less than the minimum value, the natural ventilation is shut off (i.e., the window or door openings are closed) and the HVAC system may operate if needed. Otherwise, the natural ventilation is on and the HVAC system is shut off. The amount of outdoor ventilation air entering the controlled zone is determined as air from outdoors and not from other adjacent zones. Therefore, this option is only applied to a zone having a window or door exposed to outdoors.

#### Field: Opening Factor Function of Wind Speed Curve Name

The name of a linear or quadratic performance curve (ref: Performance Curves) that parameterizes the variation of opening factor as a function of outdoor wind speed. The output of this curve is multiplied by the opening factor of opening objects to give the final openness. This field only works with the AirflowNetwork opening objects.

#### Field: AirflowNetwork Control Type Schedule Name

The name of a schedule defined elsewhere in the input file. This field works with the AirflowNetwork opening objects only. This schedule determines for a given simulation timestep how the opening objects respond to the hybrid ventilation control when the hybrid ventilation control allows the objects to operate.

Schedule values equal to zero indicate individual ventilation control based on the control requirements specified for each individual AirflowNetwork opening object. Schedule values equal to one denote "group" control. The opening objects exposed to outdoors in the controlled zone served by the primary air loop (Ref. Field "HVAC Air Loop Name") will be considered as a master to provide group control.

#### Field: Simple Airflow Control Type Schedule Name

The name of a schedule defined elsewhere in the input file. This field works with ZoneVentilation:\* and [ZoneMixing](#zonemixing) objects only. This schedule determines for a given simulation timestep how the ZoneVentilation:\* and [ZoneMixing](#zonemixing) objects respond to the hybrid ventilation control when the hybrid ventilation control allows the objects to operate.

Schedule values equal to zero indicate the individual ventilation control based on the control requirements from their own objects. Schedule values equal to one denote group control. The ZoneVentilation:\* and [ZoneMixing](#zonemixing) objects in the zones served by the primary air loop defined in a previous input field (Ref. Field "HVAC Air Loop Name") are controlled by a single object, whose name is provided in the following input field.

#### Field: ZoneVentilation Object Name

The name of a ZoneVentilation:\* object whose zone name is the controlled zone name defined in a previous input field for this availability manager object (Ref. Field "Controlled [Zone](#zone) Name"). The controls defined for this specific ZoneVentilation:\* object to enable ventilation air will be applied to other ZoneVentilation:\* and [ZoneMixing](#zonemixing) objects served by the air loop controlled by this availability manager, regardless of the controls defined for the other ZoneVentilation:\* and [ZoneMixing](#zonemixing) objects. In other words, when ventilation is enabled by this specific ZoneVentilation:\* object, the other ZoneVentilation:\* and [ZoneMixing](#zonemixing) objects in the zones served by the primary air loop are also enabled.

> **Note:** A **ZoneInfiltration:\*** object ****indicates any one of **ZoneInfiltration:DesignFlowRate**, **ZoneInfiltration:EffectiveLeakageArea**,and **ZoneInfiltration:FlowCoefficient** objects. A object of **ZoneVentilation:\*** indicates any one of **ZoneVentilation:DesignFlowRate** and **ZoneVentilation:WindandStackOpenArea** objects**.**

### Outputs

The hybrid ventilation availability manager has two output variables.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Availability Manager Hybrid Ventilation Control Mode []
    HVAC,Average,Availability Manager Hybrid Ventilation Control Status []
~~~~~~~~~~~~~~~~~~~~

#### Availability Manager Hybrid Ventilation Control Mode []

This is the hybrid ventilation control mode given in the Ventilation Control Mode Schedule.

#### Availability Manager Hybrid Ventilation Control Status []

This is the hybrid ventilation control status, which can have three integer values: 0, 1, and 2. A zero value indicates no hybrid ventilation control, corresponding to zero value defined in the previous output variable. A value of one indicates that natural ventilation is allowed. A value of two denotes that the natural ventilation is not allowed, so all window/door openings are closed.

## AvailabilityManager:OptimumStart

When night time temperature setback and/or setup are employed, the optimum start availability manager is used to start the HVAC system prior to occupancy to ensure that the space is comfortable when people appear in the building.

### Inputs

#### Field: Name

A unique, user-defined name for an instance of this availability manager.

#### Field: Applicability Schedule Name

This schedule decides whether for a given time period this availability manager is to be applied. Schedule value of 1 specifies the availability manager is to be applied. Schedule value of 0 indicates that the availability manager is not used for this time period

#### Field: Fan Schedule Name

The name of the central fan schedule for the air system that this availability manager will govern.

#### Field: Control Type

The possible inputs are StayOff, MaximumofZoneList, and ControlZone. StayOff means the availability manager will have no effect – air handler on/off will be determined by the fan schedule. ControlZone means a specific zone mentioned in the field will govern the start time. The default is ControlZone. MaximumofZoneList means that from all zones mentioned in the zone list field in this availability manager maximum value of start time will be used to start the fan.

#### Field: Control Zone Name

For Control type ControlZone the name of zone is added here. Start time for this zone will be used for the air loop.

#### Field: Zone List Name

Zones mentioned in this list will be used to determine the start time. Maximum value of start time for the zones mentioned in this list will be used for the air loop.

#### Field: Maximum Value for Optimum Start Time

To limit too early start of the system maximum value for the start time can be entered here. Optional input, defaults to 6 hours. This is the maximum number of hours that a system can start before occupancy.

#### Field: Control Algorithm

**The** four possible inputs are **ConstantTemperatureGradient, AdaptiveTemperatureGradient, AdaptiveASHRAE,** and **ConstantStartTime**. In ConstantTemperatureGradient algorithm the temperature gradient entered by the user will be constant throughout the simulation. The response to heating and cooling operation can be non-linear thus two separate fields for temperature gradients are used.  AdaptiveTemperatureGradient algorithm will modify the temperature gradients based on the arithmetic average of the actual temperature gradients calculated for the specified number of previous days. For this algorithm user should enter two initial start values. AdaptiveASHRAE algorithm implements equations suggested in ASHRAE Handbook – HVAC Applications (2007). This algorithm will calculate the coefficients of the equation within the code and users need not provide temperature response related inputs. ConstantStartTime method calculates the start time based on specified number of hours before occupancy for each day. **The AdaptiveASHRAE algorithm has not been implemented yet.**

#### Field: Constant Temperature Gradient during Cooling

This input is required if ConstantTemperatureGradient algorithm is selected in the above field. Temperature gradient (°C/hour) for cooling operation is entered here. Such gradient may be obtained for existing buildings from EMCS or pre-calculated by other programs. This gradient remains constant throughout the simulation.

#### Field: Constant Temperature Gradient during Heating

This input is required if ConstantTemperatureGradient algorithm is selected in the Control Algorithm field. Temperature gradient (°C/hour) for heating operation is entered here. This gradient remains constant throughout the simulation.

#### Field: Initial Temperature Gradient during Cooling

This input is required if AdaptiveTemperatureGradient algorithm is selected in Control Algorithm field. Initial value of temperature gradient (°C/hour) for cooling operation is entered here. This value is used as initial guess value to start the adaptive method. Depending on performance, this value is modified and used in the simulation.

#### Field: Initial Temperature Gradient during Heating

This input is required if AdaptiveTemperatureGradient algorithm is selected in Control Algorithm field. Initial value of temperature gradient (°C/hour) for heating operation is entered here. This value is used as initial guess value to start the adaptive method. Depending on performance, this value is modified and used in the simulation.

#### Field: Constant Start Time

This input is required if ConstantStartTime algorithm is selected in the Control Algorithm field. Start time, the number of hours before occupancy occurs for a system, for heating and cooling operation is entered here. Irrespective of thermal response, the heating or cooling operation starts before the occupancy at this fixed time. This start time value remains constant throughout the simulation.

#### Field: Number of Previous Days

This input is required if AdaptiveTemperatureGradient algorithm is selected in the Control Algorithm field. The values of optimal start time for number of days entered here will be used to determine the adaptive gradients.

Sample IDFs:

~~~~~~~~~~~~~~~~~~~~

    AvailabilityManager:OptimumStart,
      AdaptiveSetbackRecovery, !- Name
      AppliSch,!- Applicability Schedule Name
      FanSch,!- Fan Schedule Name
      ControlZone,!- Control Type
      MainZone,!- Control Zone Name
      ,!- Zone List Name
      4,      !- Maximum Value for Optimum Start Time
      AdaptiveTemperatureGradient,!- Control Algorithm
      ,!- Constant Temperature Gradient during Cooling
      ,!- Constant Temperature Gradient during Heating
      5,!- Initial Temperature Gradient during Cooling
      5;!- Initial Temperature Gradient during Heating

    AvailabilityManager:OptimumStart,
      SetbackRecovery, !- Name
      AppliSch,!- Applicability Schedule Name
      FanSch,!- Fan Schedule Name
      MaximumofZoneList,!- Control Type
      ,!- Control Zone Name
      ZoneList,!- Zone List Name
      ,      !- Maximum Value for Optimum Start Time
      AdaptiveASHRAE;!- Control Algorithm

~~~~~~~~~~~~~~~~~~~~

### Outputs

The optimum start availability manager has one output variable.

#### Availability Manager Optimum Start Hours Before Occupancy

The unit is in hour.