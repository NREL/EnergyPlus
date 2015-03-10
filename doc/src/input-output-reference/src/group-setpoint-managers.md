# Group – Setpoint Managers

Setpoint Managers are one of the high-level control constructs in EnergyPlus. A Setpoint Manager is able to access data from any of the HVAC system nodes and use this data to calculate a setpoint (usually a temperature setpoint) for one or more other HVAC system nodes. Setpoints are then used by Controllers or Plant or Condenser Loops as a goal for their control actions.

Setpoint Managers are executed at the start of each HVAC timestep, and they reside outside the HVAC system iteration loops. Thus, the Setpoint Managers are executed once per HVAC timestep, and they use previous timestep information (except for zone load) to calculate their setpoints.

The following situations require the use of a Setpoint Manager:

#. any use of [Controller:WaterCoil](#controllerwatercoil); either in "Temperature" control only or 2 Setpoint Managers for the " TemperatureAndHumidityRatio " control variable option.
#. use of [Controller:OutdoorAir](#controlleroutdoorair) with *Economizer Control Type* set to anything but *NoEconomizer*;
#. use of [Coil:Heating:Gas](#coilheatinggas) or Coil: Heating: Electric with the field *Temperature Setpoint Node Name* set to a node name;
#. any use of [CoilSystem:Cooling:DX](#coilsystemcoolingdx);
#. any use of [Humidifier:Steam:Electric](#humidifiersteamelectric).
#. Any use of [PlantLoop](#plantloop) or [CondenserLoop](#condenserloop) which doesn't use an environment reference such as AIR or GROUND as a set point

Setpoint managers will place a calculated or scheduled setpoint value on the setpoint node and overwrite any previous value. This value may represent one of several control types (e.g., Temperature, HumidityRatio, MassFlowRate, etc.). No two setpoint managers should use the same setpoint node for like control types.

For the case when two setpoint managers place a setpoint on the same setpoint node (e.g., a temperature setpoint placed on the same node by two different setpoint managers), the setpoint value on the node is calculated by the last setpoint manager simulated. Within a specific type of setpoint manager (e.g., Scheduled), the setpoint managers are simulated in the order found in the input data file (idf) when viewed using a text editor. It is also possible for two different types of setpoint managers to place setpoints of like control variable on the same node. In this case, the order of simulation for the type of setpoint manager determines which setpoint manager is the last to write a setpoint value to the node. This conflict in setpoint node data is most likely to cause inaccurate simulation results. For this reason, a conflicting setpoint node name warning message is issued so that the conflict can be resolved.  Duplicate setpoint node names found within a specific node list will not typically cause problems with the simulation (the same value is written twice to the same node). In this case a duplicate setpoint node name warning is issued, however, this could be an error in the spelling of the node name and should be corrected.

Simulation order for setpoint managers:

- SetpointManager:Scheduled
- SetpointManager:Scheduled:DualSetpoint
- SetpointManager:OutdoorAirReset
- SetpointManager:SingleZone:Reheat
- SetpointManager: SingleZone:Heating
- SetpointManager: SingleZone:Cooling
- SetpointManager: SingleZone:Humidity:Minimum
- SetpointManager: SingleZone:Humidity:Maximum
- SetpointManager:Warmest
- SetpointManager:Coldest
- SetpointManager:WarmestTemperatureFlow
- SetpointManager:ReturnAirBypassFlow
- SetpointManager:MultiZone:Cooling:Average
- SetpointManager:MultiZone:Heating:Average
- SetpointManager:MultiZone:MinimumHumidity:Average
- SetpointManager:MultiZone:MaximumHumidity:Average
- SetpointManager:MultiZone:Humidity:Minimum
- SetpointManager:MultiZone:Humidity:Maximum
- SetpointManager:FollowOutdoorAirTemperature
- SetpointManager:FollowSystemNodeTemperature
- SetpointManager:FollowGroundTemperature
- SetpointManager:CondenserEnteringReset
- SetpointManager:CondenserEnteringReset:Ideal
- SetpointManager:MixedAir
- SetpointManager:OutdoorAirPretreat

## SetpointManager:Scheduled

The simplest Setpoint Manager simply uses a schedule to determine one or more setpoints. No node data is used as input. The input consists of the Setpoint Manager name, the control variable, the schedule name, and the name of a node list. The node list contains the name of those nodes at which the setpoint is to be applied. Of course, a node list name can always be simply the name of a single node.

Note that although this object provides a very general method of setting the values of many different control variables on nodes, the component models and system solvers may or may not be able to use them. Therefore, it is important to understand that simply setting a control variable using this object will not necessarily always produce the desired behavior.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a scheduled setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. There are several choices for this type of set point manager. The most common control variable is Temperature but other major choices include: HumidityRatio and MassFlowRate. The versatility of control variables available here (probably) exceeds what is actually available in the component and systems models. However, a large number of possible control variables are made available here for maximum flexibility and because some (but not all) component models will be able to use them. The complete list of key words and the units required in the associated schedules follow.

TemperatureTemperture of fluid at node (°C)

MaximumTemperatureMaximum temperature of fluid at node  (°C)

MinimumTemperatureMinimum temperature of fluid at node  (°C)

HumidityRatioHumidity ratio of fluid at node (kgWater/kgDryAir)

MaximumHumidityRatio  Maximum humidity ratio of fluid at node (kgWater/kgDryAir)

MinimumHumidityRatio   Minimum humidity ratio of fluid at node (kgWater/kgDryAir)

MassFlowRateMass flow rate of fluid at node (kg/s)

MaximumMassFlowRateMaximum mass flow rate of fluid at node (kg/s)

MinimumMassFlowRate Minimum mass flow rate of fluid at node (kg/s)

#### Field: Schedule Name

The name of a schedule whose values are to be used as setpoints on the node or node list. The schedule value for each time period is the setpoint for this type of setpoint manager. The schedule must have values that are in the appropriate units as defined in the previous field.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which setpoints will be established by this setpoint manager.

Following is an example of the input for a Scheduled Setpoint Manager.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:Scheduled,
        Supply Air Temp Manager 1,  !- Name
        Temperature,                !- Control Variable
        Seasonal Reset Supply Air Temp Sch,  !- Schedule Name
        VAV Sys 1 Outlet Node;      !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:Scheduled:DualSetpoint

This setpoint manager places a high and low schedule value on one or more nodes. The input consists of the Setpoint Manager name, the control variable, the high and low set point schedule names, and the name of a node list. The node list contains the name of those nodes at which the setpoint is to be applied. Of course, a node list name can simply be the name of a single node. Currently the DualSetpoint Manager will be used with [PlantLoop](#plantloop) when the Plant Loop Demand Calculation Scheme is set to "DualSetpointDeadband".

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of this scheduled setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. There is only one choice for this type of setpoint manager: Temperature.

#### Field: High Setpoint Schedule Name

The name of a schedule that contains the high setpoint values. The schedule value for each time period is the high setpoint for this type of setpoint manager.

#### Field: Low Setpoint Schedule Name

The name of a schedule that contains the low setpoint values. The schedule value for each time period is the low setpoint for this type of setpoint manager.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Following is an example of the input for a [SetpointManager:Scheduled:DualSetpoint](#setpointmanagerscheduleddualsetpoint) object.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:Scheduled:DualSetpoint,
        Water Loop Setpoint Manager,    !- Name
        Temperature,                    !- Control Variable
        Plant Loop High Temp Schedule,  !- High Setpoint Schedule Name
        Plant Loop Low Temp Schedule,   !- Low Setpoint Schedule Name
        Plant Supply Outlet Node;       !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:OutdoorAirReset

The Outdoor Air Reset Setpoint Manager sets the supply air temperature according to the outdoor air temperature using a reset rule. The reset rule is determined by 2 points: the supply air setpoint temperature at the outdoor high temperature (SATOH) and the supply air setpoint temperature at the outdoor low temperature (SATOL). If the outdoor temperature is above the outdoor high temperature, the supply air temperature is set to SATOH. If the outdoor temperature is below the outdoor low temperature, the supply air temperature is set to SATOL. If the outdoor temperature is between the outdoor high and outdoor low temperatures, the supply air temperature is linearly interpolated between SATOH and SATOL.

The input consists of the setpoint manager name, the type of control variable, a node list name of the nodes affected by the setpoint, and the data for the reset rule: the supply air setpoint temperature at the outdoor low temperature, the outdoor low temperature, the supply air setpoint temperature at the outdoor high temperature, and the outdoor high temperature.

There are optional inputs for a second reset rule. First there is the name of a schedule that indicates which rule to use: a schedule value of 1 means use the first rule; a schedule value of 2 means use the second rule. Next, there are the 4 inputs specifying the second reset rule.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of an outdoor air reset setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. There is only one choice for this type of setpoint manager: Temperature.

#### Field: Setpoint at Outdoor Low Temperature

The supply air temperature setpoint in  ^o^C at the outdoor low temperature for the first reset rule.

#### Field: Outdoor Low Temperature

The outdoor air low temperature in  ^o^C for the first supply air temperature reset rule. Generally, at this outdoor air temperature the supply temperature is at its maximum.

#### Field: Setpoint at Outdoor High Temperature

The supply air temperature setpoint in  ^o^C at the outdoor high temperature for the first reset rule.

#### Field: Outdoor High Temperature

The outdoor air high temperature in ^o^C for the first supply air temperature reset rule. Generally, at this outdoor air temperature the supply temperature is at its minimum.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

#### Field: Schedule Name

The name of a schedule whose values indicate which reset rule to use. Schedule values of 1 indicate that the first reset rule will be used. Schedule values of 2 select the second reset rule.

#### Field: Setpoint at Outdoor Low Temperature 2

The supply air temperature setpoint in ^o^C at the outdoor low temperature for the second reset rule.

#### Field: Outdoor Low Temperature 2

The outdoor air low temperature in ^o^C for the second supply air temperature reset rule. Generally, at this outdoor air temperature the supply temperature is at its maximum.

#### Field: Setpoint at Outdoor High Temperature 2

The supply air temperature setpoint in ^o^C at the outdoor high temperature for the second reset rule.

#### Field: Outdoor High Temperature 2

The outdoor air high temperature in ^o^C for the second supply air temperature reset rule. Generally, at this outdoor air temperature the supply temperature is at its minimum.

Below is an example of the input for an Outdoor Air Reset Setpoint Manager:

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:OutdoorAirReset,
          Supply Air Temp Manager 1,  !- Name
          Temperature,  !- Control Variable
          16.7,  !- Setpoint at Outdoor Low Temperature {C}
          0.0,   !- Outdoor Low Temperature {C}
          12.8,  !- Setpoint at Outdoor High Temperature {C}
          32.0,  !- Outdoor High Temperature {C}
          Supply Air Temp Nodes;  !- Setpoint Node or NodeList Name

    NodeList,
          Supply Air Temp Nodes, !- Name
          Air Loop Outlet Node;  !- Node 1 Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:SingleZone:Reheat

The Single [Zone](#zone) Reheat Setpoint Manager allows the simulation of a single zone reheat system. This setpoint manager detects the control zone load, zone inlet node flow rate, and zone node temperature and calculates a setpoint temperature for the supply air that will satisfy the zone load for the control zone. This setpoint manager creates a variable temperature system. The input consists of the setpoint manager name, the control variable type, the minimum and maximum supply air temperatures, the name of the control zone, the name of the control zone node, the name of the control zone inlet node, and the name of a node or node list containing the nodes whose setpoint temperatures are to be set by this manager.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of this setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. There is only one choice for this type of setpoint manager: Temperature.

#### Field: Minimum Supply Air Temperature

The minimum supply air temperature (in ^o^C) that is allowed for this system (as set by this setpoint manager).

#### Field: Maximum Supply Air Temperature

The maximum supply air temperature (in ^o^C) that is allowed for this system (as set by this setpoint manager)

#### Field: Control Zone Name

The name of the control zone for this single zone reheat system. The heating or cooling load for this zone determines the supply air temperature setpoint.

#### Field: Zone Node Name

The name of the zone node for the control zone.

#### Field: Zone Inlet Node Name

The name of the zone inlet node that is supplying air to the control zone.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below is an example input for a [SetpointManager:SingleZone:Reheat](#setpointmanagersinglezonereheat) object.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:SingleZone:Reheat,
          Supply Air Temp Manager,  !- Name
          Temperature, !- Control Variable
          13., !- Minimum Supply Air Temperature {C}
          45., !- Maximum Supply Air Temperature {C}
          NORTH ZONE,   !- Control Zone Name
          Zone 3 Node,  !- Zone Node Name
          Zone 3 Inlet Node, !- Zone Inlet Node Name
          Supply Air Temp Nodes; !- Setpoint Node or NodeList Name

    NodeList,
          Supply Air Temp Nodes, !- Name
          Heating Coil Air Inlet Node,  !- Node 1 Name
          Air Loop Outlet Node; !- Node 2 Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:SingleZone:Heating

The Single [Zone](#zone) Heating Setpoint Manager allows a component to be controlled based on the load required to meet the zone heating setpoint. Ths setpoint manager detects the control zone load to meet the current heating setpoint, zone inlet node flow rate, and zone node temperature, and calculates a setpoint temperature for the supply air that will satisfy the zone heating load for the control zone. This setpoint manager creates a variable temperature system. The input consists of the setpoint manager name, the controlled variable type, the minimum and maximum supply air temperatures, the name of the control zone, the name of the control zone node, the name of the control zone inlet node, and the name of a node or node list containing the nodes whose setpoint temperatures are to be set by this manager.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a single zone heating setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. There is only one choice for this type of setpoint manager: Temperature.

#### Field: Minimum Supply Air Temperature

The minimum supply air temperature (in ^o^C) that is allowed for this system (as set by this setpoint manager).

#### Field: Maximum Supply Air Temperature

The maximum supply air temperature (in ^o^C) that is allowed for this system (as set by this setpoint manager)

#### Field: Control Zone Name

The name of the control zone for this setpoint manager. The heating load for this zone (to meet the zone heating setpoint) determines the supply air temperature setpoint.

#### Field: Zone Node Name

The name of the zone node for the control zone.

#### Field: Zone Inlet Node Name

The name of the zone inlet node that is supplying air to the control zone.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below is an example input for a [SetpointManager:SingleZone:Heating](#setpointmanagersinglezoneheating) object.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:SingleZone:Heating,
        Zone 1 Heating Supply Air Temp Manager,  !- Name
        Temperature,             !- Control Variable
        7.22,                    !- Minimum Supply Air Temperature {C}
        45.,                     !- Maximum Supply Air Temperature {C}
        Zone 1,                  !- Control Zone Name
        Zone 1 Zone Node,        !- Zone Node Name
        Zone 1 Supply Inlet,     !- Zone Inlet Node Name
        Zone 1 Heating Coil Outlet;  !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:SingleZone:Cooling

The Single [Zone](#zone) Cooling Setpoint Manager allows a component to be controlled based on the load required to meet the zone cooling setpoint. This setpoint manager detects the control zone load to meet the current cooling setpoint, zone inlet node flow rate, and zone node temperature, and calculates a setpoint temperature for the supply air that will satisfy the zone cooling load for the control zone. This setpoint manager creates a variable temperature system. The input consists of the setpoint manager name, the controlled variable type, the minimum and maximum supply air temperatures, the name of the control zone, the name of the control zone node, the name of the control zone inlet node, and the name of a node or node list containing the nodes whose setpoint temperatures are to be set by this manager.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a single zone cooling setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. There is only one choice for this type of setpoint manager: Temperature.

#### Field: Minimum Supply Air Temperature

The minimum supply air temperature (in ^o^C) that is allowed for this system (as set by this setpoint manager).

#### Field: Maximum Supply Air Temperature

The maximum supply air temperature (in ^o^C) that is allowed for this system (as set by this setpoint manager)

#### Field: Control Zone Name

The name of the control zone for this setpoint manager. The cooling load of this zone (to meet the zone cooling set point) determines the supply air temperature setpoint.

#### Field: Zone Node Name

The name of the zone node for the control zone.

#### Field: Zone Inlet Node Name

The name of the zone inlet node that is supplying air to the control zone.

#### Field: Setpoint Node or NodeList Name

The name of a Node List object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below is an example input for a [SetpointManager:SingleZone:Cooling](#setpointmanagersinglezonecooling) object.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:SingleZone:Cooling,
        Zone 1 Cooling Supply Air Temp Manager,  !- Name
        Temperature,             !- Control Variable
        7.22,                    !- Minimum Supply Air Temperature {C}
        45.,                     !- Maximum Supply Air Temperature {C}
        Zone 1,                  !- Control Zone Name
        Zone 1 Zone Node,        !- Zone Node Name
        Zone 1 Supply Inlet,     !- Zone Inlet Node Name
        Zone 1 Cooling Coil Outlet;  !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:SingleZone:Humidity:Minimum

The Single [Zone](#zone) Minimum Humidity Setpoint Manager allows the control of a single zone minimum humidity level. This setpoint manager detects the humidity level in a control zone and, using air and moisture mass balance, calculates the supply air humidity ratio needed to maintain the zone relative humidity at or above a given setpoint. The calculated supply air humidity ratio is then used as the setpoint for the designated setpoint node. A humidifier component placed upstream of the setpoint node can then use the humidity ratio setpoint to control its moisture addition rate.

The use of this object requires that a "[ZoneControl:Humidistat](#zonecontrolhumidistat)" object be specified with a humidifying relative humidity schedule for the controlling zone. The humidistat's controlling zone must correspond with the control zone air node name specified in this setpoint manager.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a single zone minimum humidity setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

Deprecated Field. This field is not used and will be deleted in a future release.

#### Field: Schedule Name

Deprecated Field. This field is not used and will be deleted in a future release.

#### Field: Setpoint Node or NodeList Name

The name of the HVAC system node where the calculated humidity ratio setpoint is set. If the setpoint is being placed on more than one node, this input should be the name of a [NodeList](#nodelist).

#### Field: Control Zone Air Node Name

The name of the zone node for the humidity control zone (as specified in the object [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections)).

An IDF example for this setpoint manager is shown below with the required humidistat:

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:SingleZone:Humidity:Minimum,
        Zone Min Set Point Manager,  !- Name
        ,  !- Control Variable
        ,  !- Schedule Name
        Air Loop Outlet Node,  !- Setpoint Node or NodeList Name
        Zone 2 Node;           !- Control Zone Air Node Name

    ZoneControl:Humidistat,
        Zone 2 Humidistat,       !- Name
        ZONE 2,                  !- Zone Name
        Min rel Hum Set Sch2;    !- Humidifying Relative Humidity Setpoint Schedule Name

~~~~~~~~~~~~~~~~~~~~

## SetpointManager:SingleZone:Humidity:Maximum

The Single [Zone](#zone) Maximum Humidity Setpoint Manager allows the control of a single zone maximum humidity level. This setpoint manager, used in conjunction with object [ZoneControl:Humidistat](#zonecontrolhumidistat), detects the air humidity level in a single control zone and uses air/moisture mass balances to calculate the supply air humidity ratio needed to maintain the zone relative humidity at or below a given setpoint. The calculated supply air humidity ratio is then used as the setpoint for the designated setpoint node. A dehumidification component placed upstream of this node can then use the humidity ratio setpoint to control its moisture removal rate (e.g., desiccant dehumidifiers).

In the case of a chilled water coil which is used for both temperature and high humidity control, this setpoint manager works in conjunction with a [Controller:WaterCoil](#controllerwatercoil) object to determine the minimum supply air temperature required to meet both the temperature (sensible) and humidity (latent) load in the control zone. See object [Controller:WaterCoil](#controllerwatercoil) in the EnergyPlus Engineering Reference for a detailed discussion of how this is achieved.

The use of this object requires that a "[ZoneControl:Humidistat](#zonecontrolhumidistat)" object be specified with a relative humidity setpoint schedule for the controlling zone. The humidistat's controlling zone must correspond with the control zone air node name specified in this setpoint manager..

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a single zone maximum humidity setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

Deprecated Field. This field is not used and will be deleted in a future release.

#### Field: Schedule Name

Deprecated Field. This field is not used and will be deleted in a future release.

#### Field: Setpoint Node or NodeList Name

The name of the HVAC system node where the calculated humidity ratio setpoint is set. If the setpoint is being placed on more than one node, this input should be the name of a [NodeList](#nodelist).

#### Field: Control Zone Air Node Name

The name of the zone air node for the humidity control zone (as specified in the object [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections)).

An example of this object in an input data file (IDF), along with a temperature set point manager, humidistat, and a simple controller, is shown below:

~~~~~~~~~~~~~~~~~~~~

      SetpointManager:SingleZone:Humidity:Maximum,
        Zone Max Set Point Manager,           !- Name
        ,                                     !- Control Variable
        ,                                     !- Schedule Name
        VAV Sys 1 Outlet Node,                !- Setpoint Node or NodeList Name
        Zone 2 Node;                          !- Control Zone Air Node Name

      SetpointManager:Scheduled,
        Scheduled Set Point Manager 1,        !- Name
        Temperature,                          !- Control Variable
        Seasonal Reset Supply Air Temp Sch,   !- Schedule Name
        VAV Sys 1 Outlet Node;                !- Setpoint Node or NodeList Name

      ZoneControl:Humidistat,
        Zone 2 Humidistat,       !- Name
        ZONE 2,                  !- Zone Name
        Min rel Hum Set Sch2;    !- Humidifying Relative Humidity Setpoint Schedule Name

      Controller:WaterCoil,
        Central Cooling Coil Contoller 1,     !- Name
        TEMPandHUMRAT,                        !- Control Variable
        Reverse,                              !- Action
        FLOW,                                 !- Actuator Variable
        VAV Sys 1 Outlet Node,                !- Sensor Node Name
        Main Cooling Coil 1 Water Inlet Node, !- Actuator Node Name
        0.002,                                !- Controller Convergence Tolerance {deltaC}
        0.025,                                !- Maximum Actuated Flow {m3/s}
        0.0;                                  !- Minimum Actuated Flow {m3/s}

~~~~~~~~~~~~~~~~~~~~

## SetpointManager:MixedAir

The Mixed Air Setpoint Manager is meant to be used in conjunction with a [Controller:OutdoorAir](#controlleroutdoorair) object. This setpoint manager is used to establish a temperature setpoint at the mixed air node. The [Controller:OutdoorAir](#controlleroutdoorair) then operates the outdoor air damper to attempt to meet this setpoint.

In EnergyPlus the relief and outdoor air dampers, the economizer, and any outdoor air conditioning equipment form a separate subsystem of an air loop system (ref. [AirLoopHVAC](#airloophvac)). The outdoor air controller operates within the subsystem. Consequently the mixed air temperature setpoint must take into account any downstream system fan heat if it is to meet a desired system supply air leaving temperature. The Mixed Air Setpoint Manager accomplishes this by referencing a supply air setpoint set by another setpoint manager (most likely at the [AirLoopHVAC](#airloophvac) outlet node). The supply fan inlet and outlet nodes are also inputs to the Mixed Air Setpoint Manager. From this information the Mixed Air Setpoint Manager calculates the supply fan air temperature rise, subtracts it from the reference setpoint temperature, and sets the result as the mixed air node setpoint temperature.

Of course any type of setpoint manager can be used to establish a temperature setpoint at the mixed air node. But the Mixed Air Setpoint Manager is likely to be the most useful.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a mixed air setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. There is only one choice for this type of setpoint manager: Temperature.

#### Field: Reference Setpoint Node Name

The name of an HVAC system node where the system supply air temperature is set. Normally this would be the [AirLoopHVAC](#airloophvac) outlet node. The temperature setpoint at this reference node is set by a different setpoint manager.

#### Field: Fan Inlet Node Name

The name of the supply fan inlet node.

#### Field: Fan Outlet Node Name

The name of the supply fan outlet node.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name where temperature setpoints will be established by this setpoint manager.

Below is an example input for a Mixed Air Setpoint Manager.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:SingleZone:Reheat, ! establishes the setpoint at the system outlet node
          Supply Air Temp Manager,  !- Name
          Temperature,  !- Control Variable
          13., !- Minimum Supply Air Temperature
          45., !- Maximum Supply Air Temperature
          NORTH ZONE, !- Control Zone Name
          Zone 3 Node, !- Zone Node Name
          Zone 3 Inlet Node, !- Zone Inlet Node Name
          Supply Air Temp Nodes;  !- Setpoint Node or NodeList Name

    SetpointManager:MixedAir, ! uses the system outlet setpoint to establish the mixed air setpoint
          Mixed Air Temp Manager 1,  !- Name
          Temperature,  !- Control Variable
          Air Loop Outlet Node,          !- Reference Setpoint Node Name
          Heating Coil Air Outlet Node,  !- Fan Inlet Node Name
          Air Loop Outlet Node,          !- Fan Outlet Node Name
          Mixed Air Nodes;               !- Setpoint Node or NodeList Name

    NodeList, Supply Air Temp Nodes,
          Air Loop Outlet Node; ! Setpoint Nodes
    NodeList, Mixed Air Nodes,
          Mixed Air Node; ! Setpoint Nodes
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:OutdoorAirPretreat

The OutdoorAir Pretreat Setpoint Manager is meant to be used in conjunction with an [OutdoorAir:Mixer](#outdoorairmixer) object. The Outdoor air Pretreat Setpoint Manager is used to establish a temperature or humidity ratio setpoint in the outdoor air stream flowing into the "Outdoor Air Stream Node" of an [OutdoorAir:Mixer](#outdoorairmixer) object. This setpoint manager determines the required setpoint in the outdoor air stream to produce the reference setpoint in the mixed air stream after mixing with return air. For example, if the temperature setpoint at the mixed air node is 15°C, the return air temperature is 20°C, and the outdoor air flow fraction is 0.5, the Outdoor Air Pretreat setpoint would be set to 10°C.

Of course any type of setpoint manager can be used to establish setpoints in the outdoor air stream, but this setpoint manager is likely to be the most useful for systems which precondition outdoor air with equipment such as a DX cooling coil or desiccant dehumidifier.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of an outdoor air pretreat setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. The choices are:

- Temperature – references and sets the "System Node Setpoint Temperature" property. This single temperature setpoint is set and used by all temperature setpoint managers and equipment controls except dual-setpoint equipment.
- MaximumHumidityRatio – references and sets the "System Node Humidity Ratio Max" property. This type of reference setpoint may be set using [SetpointManager:SingleZone:Humidity:Maximum](#setpointmanagersinglezonehumiditymaximum), [SetpointManager:MultiZone:MaximumHumidity:Average](#setpointmanagermultizonemaximumhumidityaverage), or [SetpointManager:MultiZone:Humidity:Maximum](#setpointmanagermultizonehumiditymaximum) and is used by equipment such as [Controller:WaterCoil](#controllerwatercoil) (with control variable "TemperatureAndHumidityRatio") and [Dehumidifier:Desiccant:NoFans](#dehumidifierdesiccantnofans).
- MinimumHumidityRatio – references and sets the "System Node Humidity Ratio Min" property. This type of reference setpoint may be set using [SetpointManager:SingleZone:Humidity:Minimum](#setpointmanagersinglezonehumidityminimum), SetpointManager:Multizone:MinimumHumidity:Average or [SetpointManager:MultiZone:Humidity:Minimum](#setpointmanagermultizonehumidityminimum) and is used by equipment such as [Humidifier:Steam:Electric](#humidifiersteamelectric).
- HumidityRatio – references and sets the "System Node Setpoint Humidity Ratio" property. This type of reference setpoint may be set using (no applicable setpoint managers at this time) and is used by equipment such as [Controller:WaterCoil](#controllerwatercoil) (with control variable "HumidityRatio").

The system node properties listed above are all available as an [Output:Variable](#outputvariable) to facilitate tracking these setpoints both at the reference node and the setpoint nodes.

#### Field: Minimum Setpoint Temperature

The minimum temperature (in ^o^C) that is allowed by this setpoint manager. Applicable only if Control variable is Temperature.

#### Field: Maximum Setpoint Temperature

The maximum temperature (in ^o^C) that is allowed by this setpoint manager. Applicable only if Control variable is Temperature.

#### Field: Minimum Setpoint Humidity Ratio

The minimum humidity ratio (kgWater/kgDryAir) that is allowed by this setpoint manager. Applicable only if Control variable is MaximumHumidityRatio, MinimumHumidityRatio, or HumidityRatio. Note that zero is not allowed as the computed setpoint humidity ratio, because zero is used as a special value to indicate that no humidification or dehumidification is needed. If the reference humidity ratio setpoint value is zero, the zero value will be passed directly to the Set Point Node(s).

#### Field: Maximum Setpoint Humidity Ratio

The maximum humidity ratio (kgWater/kgDryAir) that is allowed by this set point manager. Applicable only if Control variable is MaximumHumidityRatio, MinimumHumidityRatio, or HumidityRatio.

#### Field: Reference Setpoint Node Name

The name of an HVAC system node where the desired mixed air condition has been set. Normally this would be the mixed air node leaving the [OutdoorAir:Mixer](#outdoorairmixer). The temperature or humidity setpoint at this reference node must be set by a different setpoint manager of the appropriate type (see Control Variable above).

#### Field: Mixed Air Stream Node Name

The name of the mixed air node. This node is used to obtain the flow rate of the combined air stream. This node is usually the same node at the Reference Setpoint Node Name, but does not have to be.

#### Field: Outdoor Air Stream Node Name

The name of a node in the outdoor air stream. This node is used to obtain the flow rate of the outdoor air stream.

#### Field: Return Air Stream Node Name

The name of a node in the return air stream. This node is used to obtain the flow rate of the return air stream.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature or humidity ratio setpoints will be established by this setpoint manager.

Below is an example input for an Outdoor Air Pretreat Setpoint Manager.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:OutdoorAirPretreat,
        DXSystem 1 Desiccant Set Point Manager,  !- Name
        MaximumHumidityRatio,    !- Control Variable
        -99,                     !- Minimum Setpoint Temperature {C}
        99,                      !- Maximum Setpoint Temperature {C}
        0.00001,                 !- Minimum Setpoint Humidity Ratio (kgWater/kgDryAir)
        1.0,                     !- Maximum Setpoint Humidity Ratio (kgWater/kgDryAir)
        DXSystem 1 Fan Air Inlet Node,  !- Reference Setpoint Node Name
        DXSystem 1 Mixed Air Node,      !- Mixed Air Stream Node Name
        Heat Recovery Outlet Node,      !- Outdoor Air Stream Node Name
        DXSystem 1 Air Loop Inlet Node, !- Return Air Stream Node Name
        Desiccant Process Outlet Node;  !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:Warmest

The Warmest Setpoint Manager resets the cooling supply air temperature of a central forced air HVAC system according to the cooling demand of the warmest zone. For each zone in the system at each system timestep, the manager calculates a supply air temperature that will meet the zone cooling load at the maximum zone supply air flow rate. The lowest of the possible supply air temperatures becomes the new supply air temperature setpoint, subject to minimum and maximum supply air temperature constraints. The resulting temperature setpoint is the highest supply air temperature that will meet the cooling requirements of all the zones. When compared to a fixed cooling supply air temperature setpoint, this strategy minimizes zone reheat coil energy (or overcooling) and central chiller energy consumption (if the chilled water temperature is also reset) at the cost of possible increased fan energy.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a warmest setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. Currently there is only one choice: Temperature.

#### Field: HVAC Air Loop Name

The name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its cooling supply air temperature.

#### Field: Minimum Setpoint Temperature

The minimum allowed setpoint temperature in degrees C. If the calculated setpoint temperature is less than this minimum, the setpoint is set to the minimum.

#### Field: Maximum Setpoint Temperature

The maximum allowed setpoint temperature in degrees C. If the calculated setpoint is greater than this value the setpoint is set to the maximum. This value is also used as the setpoint temperature when none of the zones have a cooling load.

#### Field: Strategy

Currently, the only choice for this field is MaximumTemperature.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below is an example input for a Warmest Setpoint Manager.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:Warmest,
        Supply Air Temperature Manager 1,  !- Name
        Temperature,  !- Control Variable
        VAV Sys 1,  !- HVAC Air Loop Name
        11.2,  !- Minimum Setpoint Temperature
        16.,  !- Maximum Setpoint Temperature
        MaximumTemperature,  !- Strategy
        Supply Air Temp Nodes 1;  !- Setpoint Node or NodeList Name

    SetpointManager:MixedAir,
        Mixed Air Temp Manager 1,  !- Name
        Temperature,  !- Control Variable
        VAV Sys 1 Outlet Node,  !- Reference Setpoint Node Name
        Main Heating Coil 1 Outlet Node,  !- Fan Inlet Node Name
        VAV Sys 1 Outlet Node,  !- Fan Outlet Node Name
        Mixed Air Node 1;       !- Setpoint Node or NodeList Name

    NodeList, Supply Air Temp Nodes 1,
        VAV Sys 1 Outlet Node; ! Setpoint Node
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:Coldest

The Coldest Setpoint Manager is used in dual duct systems to reset the setpoint temperature of the air in the heating supply duct. Usually it is used in conjunction with a [SetpointManager:Warmest](#setpointmanagerwarmest) resetting the temperature of the air in the cooling supply duct. For each zone in the system at each system timestep, the manager calculates a supply air temperature that will meet the zone heating load at the maximum zone supply air flow rate. The highest of the possible supply air temperatures becomes the new supply air temperature setpoint, subject to minimum and maximum supply air temperature constraints. The resulting temperature setpoint is the lowest supply air temperature that will meet the heating requirements of all the zones. When compared to a fixed heating supply air temperature setpoint, this strategy minimizes central boiler energy consumption (if the hot water temperature is also reset or there are variable speed pumps) at the cost of possible increased fan energy (if there is variable volume control in the air system).

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a coldest setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. Currently there is only one choice: Temperature.

#### Field: HVAC Air Loop Name

The name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its heating supply air temperature.

#### Field: Minimum Setpoint Temperature

The minimum allowed setpoint temperature in degrees C. If the calculated setpoint temperature is less than this minimum, the setpoint is set to the minimum. This value is also used as the setpoint temperature when none of the zones have a heating load.

#### Field: Maximum Setpoint Temperature

The maximum allowed setpoint temperature in degrees C. If the calculated setpoint is greater than this value the setpoint is set to the maximum.

#### Field: Strategy

Currently, the only choice for this field is MinimumTemperature.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

Below is an example input for a Coldest Setpoint Manager.

~~~~~~~~~~~~~~~~~~~~

      Controller:Simple,
        Central Cooling Coil Controller 1,  !- Name
        Temperature,  !- Control Variable
        Reverse,  !- Action
        Flow,     !- Actuator Variable
        VAV Sys 1 Cold Outlet Node,            !- Sensor Node Name
        Main Cooling Coil 1 Water Inlet Node,  !- Actuator Node Name
        0.001,    !- Controller Convergence Tolerance {deltaC}
        autosize, !- Maximum Actuated Flow {m3/s}
        0.0;      !- Minimum Actuated Flow {m3/s}

      Controller:WaterCoil,
        Central Heating Coil Controller 1,  !- Name
        Temperature,  !- Control Variable
        Normal,  !- Action
        Flow,    !- Actuator Variable
        VAV Sys 1 Hot Outlet Node,             !- Sensor Node Name
        Main Heating Coil 1 Water Inlet Node,  !- Actuator Node Name
        0.001,     !- Controller Convergence Tolerance {deltaC}
        autosize,  !- Maximum Actuated Flow {m3/s}
        0.0;       !- Minimum Actuated Flow {m3/s}

      SetpointManager:Warmest,
        Supply Air Temperature Manager 1,  !- Name
        Temperature,       !- Control Variable
        VAV Sys 1,  !- HVAC Air Loop Name
        11.2,       !- Minimum Setpoint Temperature {C}
        16.,        !- Maximum Setpoint Temperature {C}
        MaximumTemperature,       !- Strategy
        Supply Air Temp Nodes 1;  !- Setpoint Node or NodeList Name

      NodeList,
        Supply Air Temp Nodes 1,     !- Name
        VAV Sys 1 Cold Outlet Node;  !- Node 1 Name

      SetpointManager:Coldest,
        Supply Air Temperature Manager 2,  !- Name
        Temperature,       !- Control Variable
        VAV Sys 1,  !- HVAC Air Loop Name
        25.,        !- Minimum Setpoint Temperature {C}
        50.,        !- Maximum Setpoint Temperature {C}
        MinimumTemperature,       !- Strategy
        Supply Air Temp Nodes 2;  !- Setpoint Node or NodeList Name

      NodeList,
        Supply Air Temp Nodes 2,    !- Name
        VAV Sys 1 Hot Outlet Node;  !- Node 1 Name

~~~~~~~~~~~~~~~~~~~~

## SetpointManager:ReturnAirBypassFlow

This manager is user in conjunction with a return air bypass configuration. This type of air system is basically a standard single duct system with the addition of a bypass duct that bypasses return air around the main system components – in particular, the central cooling coil. This allows the central cooling coil to sufficiently dehumidify the mixed air; the bypassed air is then added to the supply air stream to bring the supply air temperature up to the desired temperature set point. This scheme is very useful in situations where an higher than normal supply air temperature is used – for instance, in underfloor air distribution systems.

This manager relies on the program to figure out the system configuration and to extract the needed data from the system nodes. All the user needs to input is the name of the air system and a schedule giving the desired supply air temperature setpoint. No node names are required. The manager will establish a bypass air flow rate that upon mixing with the non-bypassed air will yield the temperature given in the schedule.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of an Return Air Bypass Flow setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. Currently there is only one choice:  Flow.

#### Field: HVAC Air Loop Name

The name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its return air bypass flow rate.

#### Field: Temperature Setpoint Schedule Name

The name of a schedule whose values are temperatures in  ^o^C. The schedule value for the time period is the setpoint for this type of setpoint manager. The setpoint is assumed to be at the air handler outlet.

The following shows an example input for [SetpointManager:ReturnAirBypassFlow](#setpointmanagerreturnairbypassflow).

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:ReturnAirBypassFlow,
        RETURN AIR BYPASS Manager 1,  !- Name
        Flow,  !- Control Variable
        VAV Sys 1,  !- HVAC Air Loop Name
        Seasonal Reset Supply Air Temp Sch;  !- Temperature Setpoint Schedule Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:WarmestTemperatureFlow

The Warmest Temperature Flow Setpoint Manager resets the cooling supply air temperature of a central forced air HVAC system according to the cooling demand of the warmest zone. The user can select one of two strategies: *TemperatureFirst* or *FlowFirst*.

For *TemperatureFirst* the manager tries to find the highest setpoint temperature that will satisfy all the zone cooling loads at minimum supply air flow rate. If this setpoint temperature is less than the minimum, the setpoint temperature is set to the minimum, and the supply air flow rate is increased to meet the loads.

For *FlowFirst* the manager tries to find the lowest supply air flow rate that will satisfy all the zone cooling loads at the maximum setpoint temperature. If this flow is greater than the maximum, the flow is set to the maximum and the setpoint temperature is reduced to satisfy the cooling loads.

When compared to a fixed cooling supply air temperature setpoint, the *TemperatureFirst* strategy minimizes fan energy at the cost of possible increased zone reheat coil energy (or overcooling) and central chiller energy consumption, whereas the *FlowFirst* strategy minimizes zone reheat coil energy (or overcooling) and central chiller energy consumption at the cost of possible increased fan energy.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of this setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control Variable

The type of variable that will be controlled. For this setpoint manager, this input should be Temperature.

#### Field: HVAC Air Loop Name

he name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its cooling supply air temperature.

#### Field: Minimum Setpoint Temperature

The minimum allowed setpoint temperature in degrees C. If the calculated setpoint temperature is less than this minimum, the setpoint is set to the minimum.

#### Field: Maximum Setpoint Temperature

The maximum allowed setpoint temperature in degrees C. If the calculated setpoint is greater than this value the setpoint is set to the maximum. This value is also used as the setpoint temperature when none of the zones have a cooling load.

#### Field: Strategy

The choices for this field are *TemperatureFirst* and *FlowFirst*. See discussion above for a description of what each strategy does.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which temperature setpoints will be established by this setpoint manager.

#### Field: Minimum Turndown Ratio

The minimum value of the ratio of the actual air flow rate to the maximum air flow rate, either for the supply fan if there are no VAV terminal boxes, or for the VAV boxes if present.  If there are VAV boxes, it is assumed that the same value of minimum turndown applies to all boxes.

Below is an example input for a [SetpointManager:WarmestTemperatureFlow](#setpointmanagerwarmesttemperatureflow) object.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:WarmestTemperatureFlow,
        Supply Air Temperature Manager 1,  !- Name
        Temperature,  !- Control Variable
        VAV Sys 1,  !- HVAC Air Loop Name
        11.2,  !- Minimum Setpoint Temperature
        16.,   !- Maximum Setpoint Temperature
        TemperatureFirst,  !- Strategy
        Supply Air Temp Nodes 1,  !- Setpoint Node or NodeList Name
        0.3;  !- Minimum Turndown Ratio

    SetpointManager:MixedAir,
        Mixed Air Temp Manager 1,  !- Name
        Temperature,  !- Control Variable
        VAV Sys 1 Outlet Node,  !- Reference Setpoint Node Name
        Main Heating Coil 1 Outlet Node,  !- Fan Inlet Node Name
        VAV Sys 1 Outlet Node,  !- Fan Outlet Node Name
        Mixed Air Node 1;  !- Setpoint Node or NodeList Name

    NodeList, Supply Air Temp Nodes 1,
        VAV Sys 1 Outlet Node; ! Set Point Node
~~~~~~~~~~~~~~~~~~~~

### Outputs

#### Setpoint Manager Warmest Temperature Critical Zone Number []

This is the number of the zone that was the critical zone for the setpoint manager.

#### Setpoint Manager Warmest Temperature Turndown Flow Fraction []

This is the fraction that flow was reduced by the setpoint manager.  This is the actual air flow rate divided by the maximum air flow rate.

## SetpointManager:MultiZone:Cooling:Average

This setpoint manager is used to establish a supply air temperature setpoint for a central forced air HVAC system (air loop) based on the predicted sensible cooling loads and actual supply air mass flow rates for all zones served by the system. For all controlled zones in the air loop (i.e., zones with a thermostat object), the setpoint manager calculates an average supply air temperature that will meet the zone cooling loads based on the actual zone supply air mass flow rates (lagged by one time step). The calculated setpoint temperature is subject to the minimum and maximum setpoint temperature constraints specified by the user. When compared to a fixed cooling supply air temperature setpoint, this strategy may reduce zone reheat coil energy (or overcooling) and central chiller energy consumption (if the chilled water temperature is also reset) at the cost of possible increased fan energy.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of this multizone average cooling setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: HVAC Air Loop Name

The name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its cooling supply air temperature.

#### Field: Minimum Setpoint Temperature

The minimum allowed setpoint temperature in degrees C. If the calculated setpoint temperature is less than this minimum, the setpoint is set to the minimum. The default value is 12C.

#### Field: Maximum Setpoint Temperature

The maximum allowed setpoint temperature in degrees C. If the calculated setpoint temperature is greater than this value, the setpoint is set to this maximum value. The default value is 18C.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes, or the HVAC System Node Name, for which temperature setpoints will be established by this setpoint manager.

Below is an example of this object in an input file.

~~~~~~~~~~~~~~~~~~~~

      SetpointManager:MultiZone:Cooling:Average,
        Cooling Coil Air Sys Branch Set Point,  !- Name
        Main Dual Duct Air Loop, !- HVAC Air Loop Name
        12.0,                    !- Minimum Setpoint Temperature {C}
        22.0,                    !- Maximum Setpoint Temperature {C}
        Cooling Coil Supply Air Temp Nodes;  !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:MultiZone:Heating:Average

This setpoint manager is used to establish a supply air temperature setpoint for a central forced air HVAC system (air loop) based on the predicted sensible heating loads and actual supply air mass flow rates for all zones served by the system. For all controlled zones in the air loop (i.e., zones with a thermostat object), the setpoint manager calculates an average supply air temperature that will meet the zone heating loads based on the actual zone supply air mass flow rates (lagged by one time step). The calculated setpoint temperature is subject to the minimum and maximum setpoint temperature constraints specified by the user. When compared to a fixed heating supply air temperature setpoint, this strategy may reduce central boiler energy consumption (if the hot water temperature is also reset or there are variable speed pumps) at the cost of possible increased fan energy (if there is variable volume control for the air system).

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a multizone average heating setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: HVAC Air Loop Name

The name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its heating supply air temperature.

#### Field: Minimum Setpoint Temperature

The minimum allowed setpoint temperature in degrees C. If the calculated setpoint temperature is less than this minimum, the setpoint is set to the minimum. The default value is 20C.

#### Field: Maximum Setpoint Temperature

The maximum allowed setpoint temperature in degrees C. If the calculated setpoint temperature is greater than this value, the setpoint is set to this maximum value. The default value is 50C.

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes, or the HVAC System Node Name, for which temperature setpoints will be established by this setpoint manager.

Below is an example of this object in an input file.

~~~~~~~~~~~~~~~~~~~~

      SetpointManager:MultiZone:Heating:Average,
        Heating Coil Air Sys Branch Set Point,  !- Name
        Main Dual Duct Air Loop, !- HVAC Air Loop Name
        24.0,                    !- Minimum Setpoint Temperature {C}
        41.0,                    !- Maximum Setpoint Temperature {C}
        Heating Coil Supply Air Temp Nodes;  !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:MultiZone:MinimumHumidity:Average

This setpoint manager allows for controlling the minimum humidity level is multiple zones served by a central forced air HVAC system (air loop). This setpoint manager, used in conjunction with one or more [ZoneControl:Humidistat](#zonecontrolhumidistat) objects, detects the air humidity level in multiple controlled zones served by the HVAC air loop and uses air/moisture mass balances to calculate an average supply air humidity ratio needed to maintain the zone relative humidity levels near their respective humidifying setpoints. The calculated humidity ratio is then used as the minimum humidity ratio setpoint for the designated setpoint node(s). A humidification component (e.g., [Humidifier:Steam:Electric](#humidifiersteamelectric)) placed upstream of the setpoint node can then use the minimum humidity ratio setpoint to control humidification rate. The use of this object requires that a "[ZoneControl:Humidistat](#zonecontrolhumidistat)" object be specified with a humidifying relative humidity schedule for one or more controlled zones served by the HVAC air loop.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of an average minimum humidity (humidifying) multizone setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: HVAC Air Loop Name

The name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its humidification setpoint (minimum air humidity ratio).

#### Field: Minimum Setpoint Humidity Ratio

The minimum humidity ratio (kgWater/kgDryAir) that is allowed by this setpoint manager.  If the calculated setpoint humidity ratio is less than this value, then the setpoint is set to this minimum value. The default value is 0.005 (kgWater/kgDryAir).

#### Field: Maximum Setpoint Humidity Ratio

The maximum humidity ratio (kgWater/kgDryAir) that is allowed by this setpoint manager.  If the calculated setpoint humidity ratio is greater than this value, then the setpoint is set to this maximum value. The default value is 0.012 (kgWater/kgDryAir).

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes, or the HVAC System Node Name, for which humidity ratio setpoints will be established by this setpoint manager.

An example of this object in an input data file (IDF), along with a humidistat object, is shown below.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:MultiZone:MinimumHumidity:Average,
      VAV_1 Humidifier HUMRAT setpoint, !- Name
      VAV_1,                            !- HVAC Air Loop Name
      0.003,                            !- Minimum Setpoint Humidity Ratio (kgWater/kgDryAir)
      0.012,                            !- Maximum Setpoint Humidity Ratio (kgWater/kgDryAir)
      VAV_1 Humidifier-Outlet Nodes List;      !- Setpoint Node or NodeList Name

    ZoneControl:Humidistat,
      ICU_NURSESTN_LOBBY_FLR_2 Humidistat,  !- Name
      ICU_NURSESTN_LOBBY_FLR_2,  !- Zone Name
      MinRelHumSetSch,  !- Humidifying Relative Humidity Setpoint Schedule Name
      MaxRelHumSetSch;  !- Dehumidifying Relative Humidity Setpoint Schedule Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:MultiZone:MaximumHumidity:Average

This setpoint manager allows for controlling the maximum humidity level is multiple zones served by a central forced air HVAC system (air loop). This setpoint manager, used in conjunction with one or more [ZoneControl:Humidistat](#zonecontrolhumidistat) objects, detects the air humidity level in multiple controlled zones served by the HVAC air loop and uses air/moisture mass balances to calculate an average supply air humidity ratio needed to maintain the zone relative humidity levels near their respective dehumidifying setpoints. The calculated humidity ratio is then used as the maximum humidity ratio setpoint for the designated setpoint node(s). A dehumidification component (e.g., desiccant dehumidifiers) placed upstream of the setpoint node can then use the maximum humidity ratio setpoint to control its moisture removal rate. The use of this object requires that a "[ZoneControl:Humidistat](#zonecontrolhumidistat)" object be specified with a dehumidifying relative humidity schedule for one or more controlled zones served by the HVAC air loop.

In the case of a chilled water coil which is used for both temperature and high humidity control, this setpoint manager works in conjunction with a [Controller:WaterCoil](#controllerwatercoil) object to determine the supply air temperature required to meet both the temperature (sensible) and humidity (latent) load in the control zone. See object [Controller:WaterCoil](#controllerwatercoil) in the EnergyPlus Engineering Reference for a detailed discussion of how this is achieved.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of an average maximum humidity ratio (dehumidifying) multizone setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: HVAC Air Loop Name

The name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its dehumidification setpoint (maximum air humidity ratio).

#### Field: Minimum Setpoint Humidity Ratio

The minimum humidity ratio (kgWater/kgDryAir) that is allowed by this setpoint manager.  If the calculated setpoint humidity ratio is less than this value, then the setpoint is set to this minimum value. The default value is 0.008 (kgWater/kgDryAir).

#### Field: Maximum Setpoint Humidity Ratio

The maximum humidity ratio (kgWater/kgDryAir) that is allowed by this set point manager.  If the calculated setpoint humidity ratio is greater than this value, then the setpoint is set to this maximum value. The default value is 0.015 (kgWater/kgDryAir).

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes, or the HVAC System Node Name, for which humidity ratio setpoints will be established by this setpoint manager.

An example of this object in an input data file (IDF), along with a humidistat object, is shown below:

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:MultiZone:MaximumHumidity:Average,
      VAV_1_CoolC HUMRAT setpoint,      !- Name
      VAV_1,                            !- HVAC Air Loop Name
      0.005,                            !- Minimum Setpoint Humidity Ratio (kgWater/kgDryAir)
      0.015,                            !- Maximum Setpoint Humidity Ratio (kgWater/kgDryAir)
      VAV_1 DeHumidifier-Outlet Nodes List;    !- Setpoint Node or NodeList Name

    ZoneControl:Humidistat,
      ICU_NURSESTN_LOBBY_FLR_2 Humidistat,  !- Name
      ICU_NURSESTN_LOBBY_FLR_2,  !- Zone Name
      MinRelHumSetSch,  !- Humidifying Relative Humidity Setpoint Schedule Name
      MaxRelHumSetSch;  !- Dehumidifying Relative Humidity Setpoint Schedule Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:MultiZone:Humidity:Minimum

This setpoint manager allows for controlling the minimum humidity level in multiple zones served by a central forced air HVAC system (air loop). This setpoint manager, used in conjunction with one or more [ZoneControl:Humidistat](#zonecontrolhumidistat) objects, detects the air humidity level in multiple controlled zones served by the HVAC air loop and uses air/moisture mass balances to calculate a supply air minimum humidity ratio based on  a zone with the critical  humidification requirement (i.e., a zone with the highest humidity ratio setpoint) to maintain the zone relative humidity levels near their respective humidifying setpoints. The calculated humidity ratio is then used as the minimum humidity ratio setpoint for the designated setpoint node(s). A humidification component (e.g., [Humidifier:Steam:Electric](#humidifiersteamelectric)) placed upstream of the setpoint node can then use the minimum humidity ratio setpoint to control humidification rate. The use of this object requires that a "[ZoneControl:Humidistat](#zonecontrolhumidistat)" object be specified with a humidifying relative humidity schedule for one or more controlled zones served by the HVAC air loop.  If "[ZoneControl:Humidistat](#zonecontrolhumidistat)" object is defined for only one of the zones served by the air loop then the [SetpointManager:MultiZone:Humidity:Minimum](#setpointmanagermultizonehumidityminimum) perform as SepointManager:SingleZone:Humidity:Minimum.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a multizone minimum humidity (humidifying) setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: HVAC Air Loop Name

The name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its humidification setpoint (minimum air humidity ratio).

#### Field: Minimum Setpoint Humidity Ratio

The minimum humidity ratio (kgWater/kgDryAir) that is allowed by this setpoint manager.  If the calculated setpoint humidity ratio is less than this value, then the setpoint is set to this minimum value. The default value is 0.005 (kgWater/kgDryAir).

#### Field: Maximum Setpoint Humidity Ratio

The maximum humidity ratio (kgWater/kgDryAir) that is allowed by this setpoint manager.  If the calculated setpoint humidity ratio is greater than this value, then the setpoint is set to this maximum value. The default value is 0.012 (kgWater/kgDryAir).

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes, or the HVAC System Node Name, for which humidity ratio setpoints will be established by this setpoint manager.

An example of this object in an input data file (IDF), along with a humidistat object, is shown below.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:MultiZone:Humidity:Minimum,
        Main Humidifier setpoint Mgr,   !- Name
        VAV Sys 1,                      !- HVAC Air Loop Name
        0.003,                          !- Minimum Setpoint Humidity Ratio (kgWater/kgDryAir)
        0.015,                          !- Maximum Setpoint Humidity Ratio (kgWater/kgDryAir)
        Main Humidifier Outlet Node;    !- Setpoint Node or NodeList Name

    ZoneControl:Humidistat,
        Space5-1 Humidistat,     !- Name
        SPACE5-1,                !- Zone Name
        HumidifyingHumSetSch,    !- Humidifying Relative Humidity Setpoint Schedule Name
        DehumidifyingHumSetSch;  !- Dehumidifying Relative Humidity Setpoint Schedule Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:MultiZone:Humidity:Maximum 

This setpoint manager allows for controlling the maximum humidity level in multiple zones served by a central forced air HVAC system (air loop). This setpoint manager, used in conjunction with one or more [ZoneControl:Humidistat](#zonecontrolhumidistat) objects, detects the air humidity level in multiple controlled zones served by the HVAC air loop and uses air/moisture mass balances to calculate the maximum supply air humidity ratio based on  a zone with the critical dehumidification requirement (i.e., a zone with the lowest humidity ratio setpoint) to maintain the zone relative humidity levels near their respective dehumidifying setpoints. The calculated humidity ratio is then used as the maximum humidity ratio setpoint for the designated setpoint node(s). A dehumidification component (e.g., desiccant dehumidifiers) placed upstream of the setpoint node can then use the maximum humidity ratio setpoint to control its moisture removal rate. The use of this object requires that a "[ZoneControl:Humidistat](#zonecontrolhumidistat)" object be specified with a dehumidifying relative humidity schedule for one or more controlled zones served by the HVAC air loop. If "[ZoneControl:Humidistat](#zonecontrolhumidistat)" object is defined for only one of the zones served by the air loop then the [SetpointManager:MultiZone:Humidity:Maximum](#setpointmanagermultizonehumiditymaximum) perform as [SetpointManager:SingleZone:Humidity:Maximum](#setpointmanagersinglezonehumiditymaximum).

In the case of a chilled water coil which is used for both temperature and high humidity control, this setpoint manager works in conjunction with a [Controller:WaterCoil](#controllerwatercoil) object to determine the supply air temperature required to meet both the temperature (sensible) and dehumidification (latent) load in the control zone. See object [Controller:WaterCoil](#controllerwatercoil) in the EnergyPlus Engineering Reference for a detailed discussion of how this is achieved.

### Inputs

#### Field: Name

A unique, user-assigned name for an instance of a multizone maximum humidity ratio (dehumidifying) setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: HVAC Air Loop Name

The name of the [AirLoopHVAC](#airloophvac) object (the central air system) which will use this setpoint manager to set its dehumidification setpoint (maximum air humidity ratio).

#### Field: Minimum Setpoint Humidity Ratio

The minimum humidity ratio (kgWater/kgDryAir) that is allowed by this setpoint manager.  If the calculated setpoint humidity ratio is less than this value, then the setpoint is set to this minimum value. The default value is 0.008 (kgWater/kgDryAir).

#### Field: Maximum Setpoint Humidity Ratio

The maximum humidity ratio (kgWater/kgDryAir) that is allowed by this set point manager.  If the calculated setpoint humidity ratio is greater than this value, then the setpoint is set to this maximum value. The default value is 0.015 (kgWater/kgDryAir).

#### Field: Setpoint Node or NodeList Name

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes, or the HVAC System Node Name, for which humidity ratio setpoints will be established by this setpoint manager.

An example of this object in an input data file (IDF), along with a humidistat object, is shown below.

~~~~~~~~~~~~~~~~~~~~

      SetpointManager:MultiZone:Humidity:Maximum,
        Maximum Mzone HUMRAT setpoint,  !- Name
        VAV Sys 1,                      !- HVAC Air Loop Name
        0.003,                          !- Minimum Setpoint Humidity Ratio (kgWater/kgDryAir)
        0.015,                          !- Maximum Setpoint Humidity Ratio (kgWater/kgDryAir)
        DeHumidifier-Outlet Nodes;      !- Setpoint Node or NodeList Name

    ZoneControl:Humidistat,
        Space5-1 Humidistat,     !- Name
        SPACE5-1,                !- Zone Name
        HumidifyingHumSetSch,    !- Humidifying Relative Humidity Setpoint Schedule Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:FollowOutdoorAirTemperature

This setpoint manager is used to place a temperature setpoint on a system node that is derived from the current outdoor air environmental conditions.  The outdoor air conditions are obtained from the weather information during the simulation.

### Inputs

#### Field: Name

A unique user-defined name for an instance of this setpoint manager.

#### Field: Control Variable

The type of variable that will be controlled.  There are three choices available: Temperature, MaximumTemperature, or MinimumTemperature.

#### Field: Reference Temperature Type

The field specifies the type of temperature value to obtain from the system node referenced in the previous field.  The two available options are OutdoorDryBulb and OutdoorWetBulb.

#### Field: Offset Temperature Difference

This field provides a temperature offset that will be applied to the value of the reference temperature (outdoor air wetbulb/drybulb).  If this value is zero, and the limits are met, then the resulting setpoint will be exactly the same as the outdoor air wetbulb/drybulb temperature.  The sign convention is that a positive value here will increase the resulting setpoint to higher than the outdoor air wetbulb/drybulb.

#### Field: Maximum Setpoint Temperature 

This field provides an upper limit to the resulting setpoint value.

#### Field: Minimum Setpoint Temperature

This field provides a lower limit to the resulting setpoint value.

#### Field: Setpoint Node or NodeList Name 

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which setpoints will be established by this setpoint manager.

An IDF example of use:

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:FollowOutdoorAirTemperature,
      MyCondenserControl,     !- Name
      Temperature,            !- Control Variable
      OutdoorAirDryBulb,      !- Reference Temperature Type
      0.5,                    !- Offset Temperature Difference {deltaC}
      200,                    !- Maximum Setpoint Temperature {C}
      21.1,                   !- Minimum Setpoint Temperature {C}
      CondSupplyOutletNode;   !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:FollowSystemNodeTemperature

This setpoint manager is used to place a temperature setpoint on a system node that is derived from the current temperatures at a separate system node.  The current value of the temperature at a reference node is obtained and used to generate setpoint on a second system node.  If the reference node is also designated to be an outdoor air (intake) node, then this setpoint manager can be used to follow outdoor air conditions that are adjusted for altitude.

### Inputs

#### Field: Name

A unique user-defined name for an instance of this setpoint manager.

#### Field: Control Variable

The type of variable that will be controlled.  There are three choices available: Temperature, MaximumTemperature, or MinimumTemperature.

#### Field: Reference Node Name

The name of a system node where this setpoint manager will obtain a reference temperature to follow.  Note that the temperature to obtained is the current temperature on the node and not the current value of a temperature *setpoint*.

#### Field: Reference Temperature Type

The field specifies the type of temperature value to obtain from the system node referenced in the previous field.  The two available options are NodeDryBulb and NodeWetBulb.

#### Field: Offset Temperature Difference

This field provides a temperature offset that will be applied to the value obtained from the reference system node.  If this value is zero, and the limits are met, then the resulting setpoint will be exactly the same as the reference system node temperature.  The sign convention is that a positive value here will increase the resulting setpoint to higher than the temperature at the reference node.

#### Field: Maximum Limit Setpoint Temperature 

This field provides an upper limit to the resulting setpoint value.

#### Field: Minimum Limit Setpoint Temperature

This field provides a lower limit to the resulting setpoint value.

#### Field: Setpoint System Node or NodeList Name 

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which setpoints will be established by this setpoint manager.

An IDF example of use:

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:FollowSystemNodeTemperature,
      MyCondenserControl,     !- Name
      Temperature,            !- Control Variable
      MyOAinletNode,          !- Reference Node Name
      NodeDryBulb,            !- Reference Temperature Type
      0.5,                    !- Offset Temperature Difference {deltaC}
      200,                    !- Maximum Setpoint Temperature {C}
      24.0,                   !- Minimum Setpoint Temperature {C}
      CondSupplyOutletNode;   !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:FollowGroundTemperature

This setpoint manager is used to place a temperature setpoint on a system node that is derived from a current ground temperature.  The ground temperatures are specified in different Site:GroundTemperature:\* objects and used during the simulation.  This setpoint manager is primarily intended for condenser or plant loops using some type of ground heat exchanger.

### Inputs

#### Field: Name

A unique user-defined name for an instance of this setpoint manager.

#### Field: Control Variable

The type of variable that will be controlled.  There are three choices available: Temperature, MaximumTemperature, or MinimumTemperature.

#### Field: Reference Ground Temperature Object Type

This field is used to specify the type of ground temperature to be used by the setpoint manager.  There are four options, [Site:GroundTemperature:BuildingSurface](#sitegroundtemperaturebuildingsurface), [Site:GroundTemperature:Shallow](#sitegroundtemperatureshallow), [Site:GroundTemperature:Deep](#sitegroundtemperaturedeep), or [Site:GroundTemperature:FCfactorMethod](#sitegroundtemperaturefcfactormethod).  Generally the deep ground temperatures are the most useful for a plant loop serving a vertical borehole ground heat exchanger.

#### Field: Offset Temperature Difference

This field provides a temperature offset that will be applied to the value of the ground temperature.  If this value is zero, and the limits are met, then the resulting setpoint will be exactly the same as the ground temperature.  The sign convention is that a positive value here will increase the resulting setpoint to higher than the ground temperature.

#### Field: Maximum Setpoint Temperature 

This field provides an upper limit to the resulting setpoint value.

#### Field: Minimum Setpoint Temperature

This field provides a lower limit to the resulting setpoint value.

#### Field: Setpoint System Node or NodeList Name 

The name of a [NodeList](#nodelist) object containing the names of the HVAC system nodes or the HVAC System Node Name for which setpoints will be established by this setpoint manager.

An IDF example of use:

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:FollowGroundTemperature,
      MyCondenserControl,          !- Name
      Temperature,                 !- Control Variable
      Site:GroundTemperature:Deep, !- Reference Ground Temperature Object Type
      1.5,                         !- Offset Temperature Difference {deltaC}
      50.0,                        !- Maximum Setpoint Temperature {C}
      10.0,                        !- Minimum Setpoint Temperature {C}
      CondSupplyOutletNode;        !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:CondenserEnteringReset

The object resets the condenser entering water temperature setpoint to the optimal setpoint temperature that will result in minimum net energy consumption for the chiller and cooling tower plant. This chiller-tower optimization scheme uses one curve to determine the optimum condenser entering water temperature for a given time step and two other curves to place boundary conditions on the "optimized" setpoint value. Note that this object will work with only one cooling tower object in a plant loop.

### Inputs

#### Field: Name

A unique, user assigned name for an instance of the optimized condenser entering water setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control variable

The type of variable that will be controlled. The only valid choice for this setpoint manager is **Temperature**.

#### Field: Default Condenser Entering Water Temperature Schedule Name

This schedule should define the default condenser entering water temperature setpoint. This scheduled setpoint value is only used in a given time step if the "optimized" setpoint value (see the Optimized Condenser Entering Water Temperature Curve Name field) does not fall within its prescribed boundary conditions.

#### Field: Minimum Design Wetbulb Temperature Curve Name

The curve name associated with the coefficients in the equation used to determine the minimum design tower wetbulb referenced by the minimum outside air wetbulb temperature curve below.  The value from this curve is compared to the tower design wet bulb each timestep to establish one of the governing boundaries over the optimized condenser entering water temperature setpoint calculation.  This curve must be quad-linear ([Curve:QuadLinear](#curvequadlinear)) and is defined as:

![](media/image397.png)\


where,

OaWb = Outside air wet bulb for current timestep, ˚C

WPLR = Weighted Part Load Ratio, dimensionless

TwrWb = Design tower wet bulb boundary condition, ˚C

NF = Normalized condenser water flow per unit of tower capacity, m3/s-W

#### Field: Minimum Outside Air Wetbulb Temperature Curve Name

The curve name associated with the coefficients in the equation used to determine the minimum actual wetbulb referenced by the optimized condenser entering water temperature curve. The value from this curve is compared the actual outside wet bulb each timestep to establish one of the governing boundaries over the optimized condenser entering water temperature setpoint calculation.  This curve must be quad-linear ([Curve:QuadLinear](#curvequadlinear)) and is defined as:

![](media/image398.png)\


Where

 MinWb = Minimum design wetbulb for current timestep, ˚C

WPLR = Weighted Part Load Ratio, dimensionless

TwrWb = Design tower wet bulb boundary condition, ˚C

NF = Normalized condenser water flow per unit of tower capacity, m3/s-W

#### Field: Optimized Condenser Entering Temperature Curve Name

The curve name associated with the coefficients in the optimized condenser entering water temperature equation. The value from this curve is used to calculate the optimized condenser entering water temperature for each timestep. If this "optimized" setpoint does not fall within the bounds established by the two boundary conditions, then the value from the Default Condenser Entering Water Temperature Schedule is used for the Condenser Entering Water Setpoint for that timestep.  This curve must be quad-linear ([Curve:QuadLinear](#curvequadlinear)) and is defined as:

![](media/image399.png)\


where,

OaWb = Outside air wet bulb for current timestep, ˚C

WPLR = Weighted Part Load Ratio, dimensionless

TwrWb = Design tower wet bulb boundary condition, ˚C

NF = Normalized condenser water flow per unit of tower capacity, m3/s-W

#### Field: Minimum Lift

This field establishes the minimum Lift allowed.  Lift is generally thought of as the difference between condenser refrigerant pressure and the evaporator refrigerant pressure. Using defined pressure and temperature relationships, lift also can be related to the difference between the leaving chilled water and the leaving condenser water temperature. Further, when the leaving condenser water temperature and condenser water flow are constant, the entering condenser temperature can be used as a proxy for lift. Because most condenser water systems are designed for constant flow, entering condenser temperature is the most common metric for lift, and that is what meant here. If the calculated Condenser Entering Water Setpoint falls below (TEvapLvgWater+ MinimumLift), then the Condenser Entering Water Setpoint is reset to equal TEvapLvgWater+ MinimumLift. The units for this field are deg C TD. Default is 11.1 ˚C (20 F).

#### Field: Maximum Condenser Entering Temperature

This field establishes the maximum condenser entering water setpoint temperature allowed.  If the scheduled or calculated setpoint is above TCondEntMax, then TCondEntSetpoint is reset to equal TCondEntMax. The units for this field are deg C.

#### Field: Cooling Tower Design Inlet Air Wet-Bulb Temperature

This field defines the reference wet bulb temperature used to size the cooling tower. Typically, the design condenser entering water temperature equals TwrRefOaWb + TowerApproachTD. The units for this field are deg C.

#### Field: Setpoint Node or Node List Name

This field defines the condenser node being controlled.

Below is an example input for a CondenserEnteringReset Setpoint Manager.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:CondenserEnteringReset,
        Condenser Optimized Setpoint Manager,  !- Name
        Temperature,          !- Control Variable
        DefaultCondEntSch,    !- Default Condenser Entering Water Temperature Schedule Name
        MinDsnWBCurveName,    !- Minimum Design Wetbulb Temperature Curve Name
        MinActWBCurveName,    !- Minimum Outside Air Wetbulb Temperature Curve Name
        OptCondEntCurveName,  !- Optimized Cond Entering Water Temperature Curve Name
        12,                   !- Minimum Lift, C
        32,                   !- Maximum Condenser Entering Water Temp, C
        25.56,                !- Cooling Tower Design Inlet Air Wetbulb Temperature, C
        Cond Water Setpoint Node;  !- Setpoint Node or Node List Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:CondenserEnteringReset:Ideal

The object determines a "near-optimal" condenser water entering setpoint at each time step that will result in minimum net energy consumption for the chiller and cooling tower plant. The "ideal" chiller-tower optimization scheme uses a search algorithm to find the ideal optimal setpoint at a given timestep. Note that this object will work with only one chiller object and only one cooling tower object in a plant loop.

### Inputs

#### Field: Name

A unique, user assigned name for an instance of the optimized condenser entering water setpoint manager. Anywhere in the input that this setpoint manager is used, it is referred to by this name.

#### Field: Control variable

The type of variable that will be controlled. There is only one choice for this type of setpoint manager: **Temperature**.

#### Field: Minimum Lift

**This field establishes the minimum lift allowed.  Lift is generally thought of as the difference between condenser refrigerant pressure and the evaporator refrigerant pressure. Using defined pressure and temperature relationships, lift also can be related to the difference between the leaving chilled water and the leaving condenser water temperature. Further, when the leaving condenser water temperature and condenser water flow are constant, the entering condenser temperature can be used as a proxy for lift. Because most condenser water systems are designed for constant flow, entering condenser temperature is the most common metric for lift, and that is what meant here. If the "optimized" condenser entering water setpoint falls below leaving evaporator water temperature plus minimum lift, then the condenser entering water setpoint is reset to equal this field value plus leaving evaporator water temperature. This determines the minimum boundary of condenser entering water setpoint. The unit for this field is deg C TD. Default is 11.1 deg C (20 deg F).**

#### Field: Maximum Condenser Entering Water Temperature

This field establishes the maximum condenser entering water setpoint temperature allowed.  If the "optimized" condenser entering water setpoint is above this field value, then the condenser entering water setpoint is reset to equal this filed value. The units for this field are deg C. Default is 34 deg C

#### Field: Setpoint Node or Node List Name

This field defines the condenser node being controlled.

Below is an example input for a IdealCondenserEnteringReset Setpoint Manager.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:CondenserEnteringReset:Ideal,
        Condenser Optimized Setpoint Manager,  !- Name
        Temperature,          !- Control Variable
        12,                   !- Minimum Lift, C
        32,                   !- Maximum Condenser Entering Water Temp, C
        Cond Water Setpoint Node;  !- Setpoint Node or Node List Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:SingleZone:OneStageCooling

The singe-zone, one-stage cooling setpoint manager allows a component to be controlled based on thermostatic control using the object [ZoneControl:Thermostat:StagedDualSetpoint](#zonecontrolthermostatstageddualsetpoint).  This setpoint manager differs from other parts of EnergyPlus that control based on predicted loads to setpoint.  This setpoint manager allows modeling on/off cycling of DX coils using [CoilSystem:Cooling:DX](#coilsystemcoolingdx) in continuous fan air handlers.  This setpoint manger detects if the zone is calling for a cooling stage and applies one setpoint value if cooling is called for and another setpoint value if cooling is not called for.

This method of control is much more like real world thermostatic control but it requires short zone timesteps.  Models using this type of control should use 60 timesteps per hour and will run much slower because of that.

### Inputs

#### Field: Name

This alpha field is a unique, user-assigned name for an instance of a single zone one stage cooling setpoint manager.

#### Field: Cooling Stage On Supply Air Setpoint Temperature

This numeric field is the setpoint temperature to apply when the manager intends to turn on cooling, in degrees Celsius.  The default is -99.0°C.

#### Field: Cooling Stage Off Supply Air Setpoint Temperature

This numeric field is the setpoint temperature to apply when the manager intends to turn off cooling, in degrees Celsius.  The value in this field must be higher than the value in the previous field.  The default is 99.0°C.

#### Field: Control Zone Name

This alpha field is the name of the control zone for this setpoint manager.  This zone needs to be controlled using [ZoneControl:Thermostat:StagedDualSetpoint](#zonecontrolthermostatstageddualsetpoint).

#### Field: Setpoint Node or NodeList Name

This alpha field is the name of a [NodeList](#nodelist) objet containing the names of the HVAC system nodes, or the name of a system node, for which temperature setpoints will be established by this setpoint manager.

An example input follows.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:SingleZone:OneStageCooling,
        ZSF2 cooling on_off , !- Name
        -99.0 , !- Cooling Stage On Supply Air Setpoint Temperature
        99.0 , !- Cooling Stage Off Supply Air Setpoint Temperature
        ZSF2 , !- Control Zone Name
        DX Cooling Coil 3 Outlet Node ; !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~

## SetpointManager:SingleZoneOneStageHeating

The singe-zone, one-stage heating setpoint manager allows a component to be controlled based on thermostatic control using the object [ZoneControl:Thermostat:StagedDualSetpoint](#zonecontrolthermostatstageddualsetpoint).  This setpoint manager differs from other parts of EnergyPlus that control based on predicted loads to setpoint.  This setpoint manager allows modeling on/off cycling of heating coils in continuous fan air handlers.  This setpoint manger detects if the zone is calling for a heating stage and applies one setpoint value if heating is called for and another setpoint value if heating is not called for.

This method of control is much more like real world thermostatic control but it requires short zone timesteps.  Models using this type of control should use 60 timesteps per hour and will run much slower because of that.

### Field: Name

This alpha field is a unique, user-assigned name for an instance of a single zone one stage heating setpoint manager.

### Field: Heating Stage On Supply Air Setpoint Temperature

This numeric field is the setpoint temperature to apply when the manager intends to turn on heating, in degrees Celsius.  The default is 99.0°C.

### Field: Heating Stage Off Supply Air Setpoint Temperature

This numeric field is the setpoint temperature to apply when the manager intends to turn off heating, in degrees Celsius.  The value in this field must be lower than the value in the previous field.  The default is -99.0°C.

### Field: Control Zone Name

This alpha field is the name of the control zone for this setpoint manager.  This zone needs to be controlled using [ZoneControl:Thermostat:StagedDualSetpoint](#zonecontrolthermostatstageddualsetpoint).

### Field: Setpoint Node or NodeList Name

This alpha field is the name of a [NodeList](#nodelist) objet containing the names of the HVAC system nodes, or the name of a system node, for which temperature setpoints will be established by this setpoint manager.

An example input follows.

~~~~~~~~~~~~~~~~~~~~

    SetpointManager:SingleZone:OneStageHeating,
        ZNF1 heating on_off , !- Name
        99.0 , !- Heating Stage On Supply Air Setpoint Temperature
        -99.0 , !- Heating Stage Off Supply Air Setpoint Temperature
        ZNF1 , !- Control Zone Name
        Main Heating Coil 2 Air Outlet Node ; !- Setpoint Node or NodeList Name
~~~~~~~~~~~~~~~~~~~~