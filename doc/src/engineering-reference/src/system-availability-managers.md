# System Availability Managers

## Overview

System availability managers, or AvailabilityManagers, are one of the high-level control constructs in EnergyPlus. An AvailabilityManager is able to access data from any of the HVAC system nodes and use this data to set flags telling a central air system or plant loop to turn on or off. Also, some of the HVACZone:\* components can use these same availability managers to turn their supply air fan on or off. Each type of availability manager encapsulates a specific high-level on-off control strategy. For instance, the input object AvailabilityManager:NightVentilation allows the user to specify a control strategy for pre-cooling the building with night-time ventilation.

Availability managers are contained in the program module *SystemAvailabilityManager*. The Availability managers are executed at the start of each HVAC time step by a call to *ManageSystemAvailability*, and they reside outside the HVAC system iteration loops. Thus, the availability managers are executed once per HVAC time step, and they use previous time step information to calculate their control actions.

If a given instance of an AvailabilityManager is specific to an AirLoopHVAC or to a PlantLoop, then in the input, each loop references its availability managers through a AvailabilityManagerAssignmentList. Once the data has been read in and processed, the AvailabilityManager data for each loop is stored in array *PriAirSysAvailMgr* in *DataAirLoop* (for AirLoopHVAC) and in array *PlantAvailMgr*  in *DataPlant* for plant loops.

The availability status for each AirLoopHVAC (*PriAirSysAvailMgr*) is also passed to a zone equipment availability status array (*ZoneEquipAvail*) for possible use by the zones served by that air loop. This sets the availability for certain zone equipment which are assumed to operate in conjunction with the air loop. Specifically, the availability of zone exhaust fans (Ref. Fan:ZoneExhaust) and terminal unit fans (Ref. AirTerminal:SingleDuct:SeriesPIU: Reheat, AirTerminal:SingleDuct: ParallelPIU:Reheat, and AirTerminal:SingleDuct:VAV: Reheat:VariableSpeedFan), if specified in the input, will be the same as the corresponding AirLoopHVAC availability if an AvailabilityManager is specified. Other zone equipment are not affected by the AirLoopHVAC system availability managers (e.g., Window AC, PTAC and PTHP).

However, system availability managers can separately be specified for zone components (currently only allowed for Window AC, PTAC, PTHP, Unit Heater, Unit Ventilator, Zone Water to Air Heat Pump, Zone Terminal Unit (VRF), Standalone Energy Recovery Ventilator, Outdoor Air Unit, Fan Coil Unit, and Ventilated Slab). Similar to AirLoopHVAC or PlantLoop, inputs of these zone components refer to their availability managers through a AvailabilityManagerAssignmentList. System availability managers only operate when zone components are available. If the Window AC (or any other unit) is scheduled off then the system availability manager will not operate until the Window AC is scheduled on. The functioning of the system availability manager for zone components is same as for AirLoopHVAC or PlantLoop. Except night ventilation SAM, all other SAMs can be specified for these zone components.

The actual action of turning on or off a loop is taken by the loop prime movers: fans for AirLoopHVACs and pumps for plant loops. For instance when a fan is deciding whether it is on or off, it checks its on/off schedule and whether the loop availability status flag is *CycleOn* or *ForceOff*. If the schedule is on and the status flag is *ForceOff*, the fan will be off. If the fan schedule says off and the status flag is *CycleOn*, the fan will be on. Thus the availability managers overrule the fan on/off schedule.

## Scheduled

The input object AvailabilityManager:Scheduled provides the simplest availability manager. The sole input for this manager is a schedule name. If the current schedule value is > 0.0, the availability status flag is set to *CycleOn*; otherwise it is set to *ForceOff*.

## Scheduled On

An alternative to the AvailabilityManager:Scheduled object is the *AvailabilityManager:ScheduledOn* that is used specifically to turn equipment on while other availability managers may be used to turn the equipment off. The sole input for this manager is a schedule name. If the current schedule value is > 0.0, the availability status flag is set to *CycleOn*; otherwise it is set to *NoAction*.

## Scheduled Off

An alternative to the AvailabilityManager:Scheduled object is the *AvailabilityManager:ScheduledOff* that is used specifically to turn equipment off while other availability managers may be used to turn the equipment on. The sole input for this manager is a schedule name. If the current schedule value is = 0.0, the availability status flag is set to *ForceOff*; otherwise it is set to *NoAction*.

## Night Cycle

The input object AvailabilityManager:NightCycle is to specify when a system that has been scheduled off during unoccupied hours should cycle on to prevent building temperatures from becoming too hot or too cold. This manager can not be used to force a system off. The inputs are:

A manager applicability schedule;

The name of the fan schedule this manager will override;

The control type: *Stay Off*, *Cycle On Any*, *Cycle On Control Zone*, or *Cycle On Any - Zone Fans Only*;

Thermostat on/off tolerance *T~tol~* (degrees C);

Cycling run time in seconds; used to calculate a stop time (in time steps since the start of the current run period) once the status has become *Cycle On*.

If the fan schedule current value is > 0 or the applicability schedule value is ![](media/image6290.png)  0 or if the program is in warmup, *AvailStatus = NoAction*.

Otherwise,

if current time (in time steps since the start of the run period) is greater than the start time and less than the stop time, *AvailStatus = CycleOn* (or *CycleOnZoneFansOnly* if the control type is *Cycle On Any – Zone Fans only*).

If the current time equals the stop time, *AvailStatus = NoAction*  and the fan schedule will determine if the system is on.

 If the current time is greater than the stop time, the manager can potentially cycle the system on.

For control types *Cycle On Any* and *Cycle On Any – Zone Fans Only* the manger looks at each zone served by the air loop and detects whether the zone temperature at the thermostat is greater than the cooling setpoint plus ½*T~tol~* or less than the heating setpoint minus ½*T~tol~*. If it is, *AvailStatus* is set to *CycleOn* (or *CycleOnZoneFansOnly*). If not, *AvailStatus* is set to *NoAction*.

For control type *Cycle On Control Zone*, the manager does the same check just for the control zone.

Lastly if the new status is CycleOn the start time is reset to the current time and the stop time is reset. When this availability manager is specified for a zone component then the only allowed control types are *Stay Off* *and* *Cycle On Control Zone.*

## Night Ventilation

The input object AvailabilityManager:NightVentilation is a manager that looks at indoor and outdoor conditions to see if night ventilation might be beneficial. In addition to being able to cycle the air system on, this manager can specify what the ventilation air flow rate should be.  The inputs are:

A manager applicability schedule;

The name of the fan schedule this manager will override;

A ventilation temperature schedule;

A ventilation delta T;

A ventilation low limit temperature;

The night ventilation flow fraction;

The name of the control zone.

If the fan schedule current value is > 0 or the applicability schedule value is ![](media/image6291.png)  0 or if the program is in warmup, *AvailStatus = NoAction*.

Otherwise, the manager performs 3 limit checks.

If for all the zones connected to the air loop the room temperature at the thermostat of any zone is greater than the ventilation temperature schedule value, this check is true.

If for all the zones connected to the air loop the room temperature at the thermostat of any zone is less than the ventilation low limit temperature, this check is true.

If the difference between the control zone room temperature at the thermostat and the outside temperature is greater than the specified night venting delta T, this check is true.

If 1) and 3) are true and 2) is false, the status is set to *CycleOn*; otherwise the status is *NoAction*.  If the status is *CycleOn* this manager sets some additional values in the *AirLoopControlInfo* data structure (a flag is set to indicate night ventilation is occurring and a flag is set to indicate that the air system flow rate has been specified by a manager) and in the  *AirLoopFlow* data structure (the system flow fraction is specified). The night ventilation flag indicates to the fan that it should use alternate performance characteristics if they have been specified in a FanPerformance:NightVentilation object.

## Differential Thermostat

The input object AvailabilityManager:DifferentialThermostat is a manager that overrides fan or pump schedules depending on the temperature difference between two nodes. A typical use would be for one node to be an outdoor air node and the other a zone node. The inputs are:

name of the hot node;

name of the cold node;

temperature difference for turning the system on (*DeltaT~on~*);

temperature difference for turning the system off (*DeltaT~off~*).

Note that there is no applicability schedule for this manager. Also, this manager always returns a status of *CycleOn* or *ForceOff*, never *NoAction*.

*DeltaT* = *T~hot node~* – *T~cold node~*

If *DeltaT* >= *DeltaT~on~* then

*AvailStatus = CycleOn*

      Else if *DeltaT* <= *DeltaT~off~* then

*AvailStatus = ForceOff*

Else

*AvailStatus* remains in its previous state.

## High Temperature Turn Off

The input object AvailabilityManager:HighTemperatureTurnOff is used to turn off a central air system or a plant loop if a sensed node temperature exceeds a temperature limit. The inputs are:

name of the sensed node;

limit temperature (*T~u~*).

If *T~sensed node~* >= *T~u~* then

*AvailStatus* = *ForceOff*

Else

*AvailStatus* = *NoAction*

## High Temperature Turn On

The input object AvailabilityManager:HighTemperatureTurnOn is used to turn on a central air system or a plant loop if a sensed node temperature exceeds a temperature limit. The inputs are:

name of the sensed node;

limit temperature (*T~u~*).

If *T~sensed node~* >= *T~u~* then

*AvailStatus* = *CycleOn*

Else

*AvailStatus* = *NoAction*

## Low Temperature Turn Off

The input object AvailabilityManager:LowTemperatureTurnOff is used to turn off a central air system or a plant loop if a sensed node temperature is lower than a temperature limit. The inputs are:

name of the sensed node;

limit temperature (*T~l~*);

applicability schedule name.

If the applicability schedule exists and has a current value of <= 0, the manager returns an availability status of *NoAction*.

Otherwise,

If *T~sensed node~* <= *T~l~*  then

*AvailStatus* = *ForceOff*

Else

*AvailStatus* = *NoAction*

## Low Temperature Turn On

The input object AvailabilityManager:LowTemperatureTurnOn is used to turn on a central air system or a plant loop if a sensed node temperature is less than a temperature limit. The inputs are:

name of the sensed node;

limit temperature (*T~l~*).

If *T~sensed node~* <= *T~l~*  then

*AvailStatus* = *CycleOn*

Else

*AvailStatus* = *NoAction*

## Hybrid Ventilation Control

The input object AvailabilityManager:HybridVentilation serves two purposes: 1) it prevents simultaneous natural ventilation and HVAC system operation, and 2) it allows users to examine various strategies to maximize natural ventilation in order to reduce heating/cooling loads. This availability manager works with either the AirflowNetwork model or the simple airflow objects to provide controlled natural ventilation. The controlled natural ventilation objects referred to here are either AirflowNetwork:Multizone:ComponentDetailedOpening and AirflowNetwork:Multizone:ComponentSimpleOpening objects, or ZoneVentilation and ZoneMixing objects. The simple airflow objects are not allowed to work with the AirflowNetwork objects simultaneously. If there is no air loop, this availability manager can still be applied to controlled zone specified in the object. In order to apply hybrid ventilation manager to the controlled zone not served by any air loop, the HVAC air loop name input field must be left blank. Currently, zone component objects such as unit heater, unit ventilator, packaged terminal air conditioner, packaged terminal heat pump, zone water source heat pump, window air conditioner, variable refrigerant flow, energy recovery ventilator, outdoor air unit, fan coil unit, and ventilated slab can individually use hybrid ventilation managers to make a decision regarding whether their fan should be on or off. Also, hybrid ventilation manager can be applied to zones served by the ideal load zone component to turn them off when natural ventilation is active. Currently, hybrid ventilation manager is restricted to one per zone. It can either be applied through the air loop or directly to the zone. If hybrid ventilation manager is applied to an air loop and one of the zones served by the air loop also has hybrid ventilation manager, then zone hybrid ventilation manager is disabled. This availability manager performs somewhat differently from the other availability managers:

- This availability manager is called before the program estimates (predicts) the cooling or heating load that needs to be met by the cooling/heating systems for the simulation time step. On the other hand, the other availability managers are called after the system cooling/heating load prediction. The main reason for calling AvailabilityManager:HybridVentilation early is that this manager determines whether natural ventilation is allowed or not, and the loads from natural ventilation are used to predict system loads.
- This availability manager has its own control schedule and does not work in tandem with *AvailabilityManager:Scheduled*.
- AvailabilityManager:HybridVentilation works completely independent of the other system availability managers, so this manager is not a valid system availability manager type in the AvailabilityManagerAssignmentList object.

### Control logic

The control logic for each ventilation control mode is described below.

### Temperature control

This control mode checks whether the outdoor air dry-bulb temperature is between the Minimum Outdoor Temperature and Maximum Outdoor Temperature specified. If the outdoor temperature is between the two values then natural ventilation is allowed, else natural ventilation is not allowed.

When natural ventilation is allowed, the control then checks the temperature difference between the zone temperature and the temperature setpoint(s) in the controlled zone based on the specified temperature control type (four available temperature control types) to make a final decision:

*Single Heating Setpoint:*

If the zone temperature is below the setpoint, then the initial decision is overridden and natural ventilation is not allowed. This is intended to avoid overcooling a space, which could result in additional heating load.

*Single Cooling Setpoint:*

If the zone temperature is above the setpoint, then the initial decision is overridden and natural ventilation is not allowed. This is intended to avoid overheating a space, which could result in additional cooling load.

*Single Heating Cooling Setpoint:*

Since this temperature control type requires only a single setpoint, natural ventilation is not allowed. A recurring warning message is issued.

*Dual Setpoint with DeadBand:*

If the zone temperature is beyond the deadband, the initial decision is overridden and natural ventilation is not allowed. This is intended to avoid either overcooling a space, which could result in additional heating load when the zone temperature is below the heating setpoint, or overheating a space, which could result in additional cooling load when the zone temperature is above the cooling setpoint.

### Enthalpy control

This control mode checks whether the outdoor air enthalpy is between the Minimum Outdoor Enthalpy and Maximum Outdoor Enthalpy specified. If the outdoor enthalpy is between the two values then natural ventilation is allowed, else natural ventilation is not allowed.

When natural ventilation is allowed, the control then checks the temperature difference between the zone temperature and the temperature setpoint(s) in the controlled zone based on the specific temperature control type to make a final decision. This procedure is the same as defined above for the temperature control mode.

*Single Heating Setpoint:*

If the zone temperature is below the setpoint, then the initial decision is overridden and natural ventilation is not allowed. This is intended to avoid overcooling a space, which could result in additional heating load.

*Single Cooling Setpoint:*

If the zone temperature is above the setpoint, then the initial decision is overridden and natural ventilation is not allowed. This is intended to avoid overheating a space, which could result in additional cooling load.

*Single Heating Cooling Setpoint:*

Since this temperature control type requires only a single setpoint, natural ventilation is not allowed. A recurring warning message is issued.

*Dual Setpoint with DeadBand:*

If the zone temperature is beyond the deadband, then the initial decision is overridden and natural ventilation is not allowed. This is intended to avoid either overcooling a space, which could result in additional heating load when the zone temperature is below the heating setpoint, or overheating a space, which could result in additional cooling load when the zone temperature is above the cooling setpoint.

### Dew-Point control

This control mode checks whether the outdoor air dewpoint temperature is between the Minimum Outdoor Dew-Point and Maximum Outdoor Dew-Point specified. If the outdoor dewpoint temperature is between the two values then natural ventilation is allowed, else national ventilation is not allowed.

When natural ventilation is allowed and a humidistat is available in the controlled zone, the control then checks the humidity ratio difference between the zone air humidity ratio and humidistat setpoint in the controlled zone to make a final decision. It should be pointed out that the humidistat object provides the setpoint of relative humidity, the value of relative humidity has to be converted into the setpoint of humidity ratio using the actual zone air temperature for comparison. Since the humidistat only provides a single value for relative humidity, there are two possible scenarios:

If the actual zone air humidity ratio is below the humidity ratio setpoint and the controlled zone needs humidification as the first scenario, then the initial decision is overridden and natural ventilation is not allowed. This is intended to avoid dehumidifying a space, which could result in additional humidification load.

If the actual zone air humidity ratio is above the humidity ratio setpoint and the controlled zone needs dehumidification as the second scenario, then the initial decision is overridden and natural ventilation is not allowed. This is intended to avoid humidifying a space, which could result in additional dehumidification load.

### Outdoor ventilation air control

This control mode does not require any additional checks. The control only works with Airflow Network opening objects only, and is not allowed to work with ZoneVentilation and ZoneMixing objects.

### Availability Status

After the hybrid ventilation control determines whether natural ventilation is allowed or not, the availability status flag is set as follows:

If *Natural Ventilation* then

*AvailStatus* = *CycleOn*

Else

*AvailStatus = ForceOn*

## **Optimum Start Controls**

### Overview

Optimum start is often also referred to as optimum recovery time. Commercial buildings are often operated intermittently by lowering heating set-point and increasing cooling set-point during unoccupied period. The building should return to set-point just before occupancy period starts. Therefore it is important to start the operation of heating and cooling plant before building is occupied. If the operation is started too early before the occupants return, energy is wasted. If it is started too late the occupants will be uncomfortable. An optimum start time for operation will save energy without compromising comfort.

![Optimal Start Control](media/optimal-start-control.png)


In practice a few proprietary algorithms are used to predict this start time value for preheating and precooling operation. ASHRAE Handbook refers to work done by (Seem, Armstrong, & Hancock, 1989) which compares seven different equations and suggests a set of equations for preheating and precooling time prediction.

### Algorithms

There are a few algorithms to predict the start time for the HVAC system. Among these algorithms following are the promising ones that can be implemented in a simulation environment.

### Constant Start Time

This is the simplest format of the optimum start algorithm. The HVAC system will start the HVAC system at a fixed number of hours before occupancy as specified by the user.

### Constant Temperature Gradient

In practice most of the HVAC control manufacturers use a temperature gradient as a thermal capacity factor, in which temperature rise per unit time i.e. °C/hour is measured for the unoccupied building and then the difference between space temperature and set-point temperature is divided by this factor to calculate the actual number of hours before occupancy are needed to start the HVAC system.

### Adaptive Temperature Gradient

Adaptive control is used to modify the temperature gradient depending on the time required to meet the set-point on the prior days. The adaptive algorithm takes the arithmetic average of the actual temperature gradients calculated for the specified number of previous days.

### Adaptive ASHRAE Algorithm

The equations suggested in the ASHRAE Handbook are used in combination with a recursive linear least square method to determine the optimum start/recovery time empirically. Depending on heating or cooling operation the start time is categorized as preheating or precooling time. According to (Seem, Armstrong, & Hancock, 1989) the preheating time is a strong function of outdoor temperature whereas precooling time is not strong function of outdoor temperature if space temperature is close or less than the setback temperature. Thus, two different equations are suggested for preheating and precooling time.

- Precooling time

![](media/image6293.png)\


- Preheating time

![](media/image6294.png)\


Where, on *i*^th^ day

*t~i~*= start/recovery time,

*T~z,i~* = zone temperature

*T~o,i~* =outdoor temperature

‘*w~i~*' is a weighting factor which determines weighting given to outside and zone temperature.

![](media/image6295.png)\


*T~unocc~* and *T~occ~*~~are setpoint temperatures during unoccupied (setback) and occupied periods.

Coefficients *a~0~, a~1~, a~2~~~*(and *a~3~*) are separately calculated for heating and cooling operation and are updated from the optimum times from last three days thus; these coefficients carry history and adapt the trend from previous days.

The optimum time for past days is determined using,

![](media/image6296.png)\


*k* =time steps required for recovery

Δ*t* =time-step

*t~opt,(i-1)~*~~= recovery time on (*i*-1)^th^ day

*q~(i-1)~* =energy extracted or added during last time step

*q~max~*~~= maximum capacity of the equipments