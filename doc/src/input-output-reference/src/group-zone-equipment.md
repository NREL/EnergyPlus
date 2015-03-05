# Group – Zone Equipment

There are five main zone equipment statements that must be used to describe a block of zone equipment as shown in the below figure "[Zone](#zone) Equipment Input Syntax Map".

Types of [Zone](#zone) equipment are listed below and shown below in figure "[Zone](#zone) Equipment".

Table: Available [Zone](#zone) Equipment Types

**Air Distribution Equipment**|AirTerminal:SingleDuct:Uncontrolled|AirTerminal:SingleDuct:ConstantVolume:Reheat|AirTerminal:SingleDuct:VAV:Reheat|AirTerminal:SingleDuct:VAV:NoReheat|AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan|AirTerminal:DualDuct:ConstantVolume|AirTerminal:DualDuct:VAV|Powered Induction Units (Series and Parallel)
-------------------------------------------|-----------------------------------|--------------------------------------------|---------------------------------|-----------------------------------|--------------------------------------------------|-----------------------------------|------------------------|---------------------------------------------
**Zone Forced Air Units**||ZoneHVAC:IdealLoadsAirSystem|ZoneHVAC:FourPipeFanCoil|ZoneHVAC:WindowAirConditioner|ZoneHVAC:RefrigerationChillerSet|Unit Ventilator/Heater|Air-to-Air Heat Pumps|Unitary Systems (heat only and heat/cool)|Furnaces (heat only and heat/cool)|Energy Recovery Ventilator:Stand Alone|CoilSystem:Cooling:DX
**Radiative/Convective Units**|Baseboard Heaters|Low Temp Radiant Systems|High Temp Radiant System

![Zone Equipment Input Syntax Map](media/zone-equipment-input-syntax-map.jpeg)


![Representative Zone Equipment](media/representative-zone-equipment.png)


The following figure (Air Loop/Zone Equipment Node Diagram) illustrates the connection between the zone equipment and the air loop systems.

![Air Loop/Zone Equipment Node Diagram](media/air-loopzone-equipment-node-diagram.png)


Each zone served by an HVAC system must have three additional statements to complete the zone equipment specification. An [ZoneHVAC:AirDistributionUnit](#zonehvacairdistributionunit) allows equipment typically found within the zone inlet ductwork (such as dampers, reheat coils, etc.) to be attached to the supply air stream for a particular zone. A ZoneControl statement will allow the conditions in the zone to be managed. Finally, a [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) statement describes all of the pertinent information about the zone from an HVAC perspective. Each of these statements is described in more detail below.

## ZoneHVAC:AirDistributionUnit 

The [ZoneHVAC:AirDistributionUnit](#zonehvacairdistributionunit) object gives further information on what air loop equipment (air terminal units) will be serving a particular zone. The [ZoneHVAC:AirDistributionUnit](#zonehvacairdistributionunit) is the part of the system that is supplied from a common main air handler simulated in the Air Loop Simulation and includes the equipment that controls or tempers the air going to each individual zone according to the desired thermostatic control. The current options for [ZoneHVAC:AirDistributionUnit](#zonehvacairdistributionunit) terminal unit types are:

~~~~~~~~~~~~~~~~~~~~

    AirTerminal:DualDuct:ConstantVolume
    AirTerminal:DualDuct:VAV
    AirTerminal:DualDuct:VAV:OutdoorAir
    AirTerminal:SingleDuct:ConstantVolume:Reheat
    AirTerminal:SingleDuct:VAV:Reheat
    AirTerminal:SingleDuct:VAV:NoReheat
    AirTerminal:SingleDuct:SeriesPIU:Reheat
    AirTerminal:SingleDuct:ParallelPIU:Reheat
    AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction
    AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan
    AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat
    AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat
~~~~~~~~~~~~~~~~~~~~

Connections between the air distribution unit, the supply air duct, and the zone are specified in the input syntax for the air distribution unit and the [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter). The input syntax also explicitly defines an outlet identifier. This implies a connection to a zone through a [NodeList](#nodelist) for zone inlets (see the [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) statement). The air distribution unit is limited to one combined component-controller unit; because controls are normally based on the zone thermostat and can work in parallel or series in complex fashion. Since the control and the flow resolution can be complex, each air distribution unit is unique in addressing these combinations and therefore only one is allowed per zone.

The Air Distribution unit also allows the user to specify leaks in the supply air duct system. These inputs are used in the EnergyPlus Simplified [Duct](#duct) Leakage Model (SDLM). This model simulates a specific configuration: supply leaks to a return plenum in a commercial VAV or CV system. The system must have a constant static pressure setpoint. Within these limitations SDLM allows the user to easily evaluate the energy penalty due to duct leakage.

### Inputs

#### Field: Name

Unique identifying name of the air distribution unit.

#### Field: Air Distribution Unit Outlet Node Name

Outlet node name for the air distribution unit to the attached zone.

#### Field: Air Terminal Object Type

Single combined component/controller unit for that attached zone. Selection of components as listed above.

#### Field: Air Terminal Name

The unique identifying component name.

#### Field: Nominal Upstream Leakage Fraction

This is the leakage upstream of the terminal unit as a fraction of the design flow rate through the unit. It is the leakage fraction at the design flow rate. It is used to calculate a leakage flow rate which is then held constant while the system air flow varies. This input is optional; the default is zero.

#### Field: Constant Downstream Leakage Fraction

This is the leakage downstream of the terminal unit as a fraction of the current flow rate through the terminal unit. This fraction is held constant, so the leakage flow rate will vary proportinally with the supply air flow rate. This input is optional; the default is zero.

Two example IDF excerpts (one with duct leakage, one without):

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:AirDistributionUnit,
        SPACE1-1 ATU,            !- Air Distribution Unit Name
        SPACE1-1 In Node,        !- Air Dist Unit Outlet Node Name
        AIRTERMINAL:SINGLEDUCT:VAV:REHEAT,  !- KEY--System Component Type 1
        SPACE1-1 VAV Reheat;     !- Component Name 1

    ZoneHVAC:AirDistributionUnit,
        SPACE4-1 ATU,            !- Air Distribution Unit Name
        SPACE4-1 In Node,        !- Air Dist Unit Outlet Node Name
        AIRTERMINAL:SINGLEDUCT:VAV:REHEAT,  !- KEY--System Component Type 1
        SPACE4-1 VAV Reheat,     !- Component Name 1
        0.05,                    !- upstream nominal leakage fraction
        0.07;                    !- downstream constant leakage fraction
~~~~~~~~~~~~~~~~~~~~

## ZoneHVAC:EquipmentConnections

Finally, the [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) statement defines the remaining details about each thermal zone from an HVAC perspective (besides the controls which were defined above). As with other statements, the first two items in this object are the keyword and an identifying name which links the zone back to its geometrical input, internal gains, etc. and other statements in the HVAC section of the input. The next three items are names of lists (equipment, air inlet nodes, and air exhaust nodes) that are described in more detail below. Note that if there are no air exhaust nodes from the zone that field is left blank. And if there are no air inlet nodes, that field is left blank. Finally, two node names are necessary to complete the zone-HVAC description. The first node is the main air node for the zone upon which the air heat balance is performed. The other node begins the return air path from the zone.

> Note that all nodes mentioned in the [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) input must be unique.  That is, all nodes in all the [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) statements referenced by the "[Zone](#zone) Air Inlet Nodes", "[Zone](#zone) Air Exhaust Nodes", "[Zone](#zone) Air Node Name" and "[Zone](#zone) Return Air Node Name" cannot have any node name appearing more than once.

### Inputs

#### Field: Zone Name

Name links this equipment list back to the heat balance for the zone.

#### Field: Zone Conditioning Equipment List Name

List of zone equipment for this zone in a [ZoneHVAC:EquipmentList](#zonehvacequipmentlist) object. This list will consist of air distribution units or other direct convection or radiant equipment, i.e. window air conditioner, baseboard, fan coils, etc.

#### Field: Zone Air Inlet Node or NodeList Name

There can be more than one air inlet node depending on how many pieces of equipment are in the [ZoneHVAC:EquipmentList](#zonehvacequipmentlist). Generally there will be one air inlet node for each piece of zone equipment that delivers conditioned air to the zone. Components such as electric or hot water baseboards and radiant systems do not require zone air inlet nodes. If there is only one node – its name can be put in this field. If there is more than one node, this must be the name of a node list object (a node list object can also contain only one node name). If this field is not required (as in the baseboard system), it should be blank.

#### Field: Zone Air Exhaust Node or NodeList Name

List of exhaust nodes leaving the zone for exhaust fans, zone energy recovery, etc. However these nodes are also used as sources of zone air for zone components such as fan coil units, unit heaters and ventilators, and window air conditioners. For each such component attached to a zone there should be a unique zone exhaust node acting as the inlet node to the component. If there is only one node – its name can be put in this field. If there is more than one node, this must be the name of a node list object (a node list object can also contain only one node name). If there are no air exhaust nodes, this field should be blank.

#### Field: Zone Air Node Name

The conditions at this node represent the average state of the air in the zone. For zones modeled as fully mixed the conditions at this node are assumed to represent the air in the entire zone. This field is required for all [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) statements.

#### Field: Zone Return Air Node Name

The name of the return air node which attaches the zone to the return air path described above. Even if there is no return air or no return air path a unique name must be entered in this field. The conditions at this node represent the state of the air leaving the zone including any heat gain from light-heat-to-return.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:EquipmentConnections,
        SPACE3-1,              !- Zone Name
        SPACE3-1 Eq,           !- List Name: Zone Equipment
        SPACE3-1 In Nodes,     !- List Name: Zone Air Inlet Nodes
        ,                      !- List Name: Zone Air Exhaust Nodes
        SPACE3-1 Node,         !- Zone Air Node Name
        SPACE3-1 Out Node;     !- Zone Return Air Node Name
~~~~~~~~~~~~~~~~~~~~

## ZoneHVAC:EquipmentList

The first list encountered in the [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) statement is the [ZoneHVAC:EquipmentList](#zonehvacequipmentlist). This object lists all HVAC equipment serving the zone. Each item in the list has four fields associated with it: Object Type, Name, Cooling Sequence and Heating or No-Load Sequence The Object Type and Name identify the specific equipment object. Cooling Sequence and Heating or No-Load Sequence specify the order of simulation for zones with more than one type of HVAC equipment.

Note that a [ZoneHVAC:AirDistributionUnit](#zonehvacairdistributionunit) or [AirTerminal:SingleDuct:Uncontrolled](#airterminalsingleductuncontrolled) must be listed in this statement if there is a forced air system serving the zone from an air loop.

### Inputs

#### Field: Name

Unique identifying name.

#### Field Set (Zone Equipment: Object Type, Name, Cooling Sequence, Heating or No-Load Sequence)

This set is used together in order to sequence the equipment for heating and cooling. The #1 sequence equipment will try to meet the entire demand with its capacity and then pass the results on to the #2 and so on for both heating and cooling. This field set is extensible by duplicating the last four fields.

Equipment is simulated in the order specified by [Zone](#zone) Equipment Cooling Sequence and [Zone](#zone) Equipment Heating or No-Load Sequence, depending on the current thermostat request. For equipment of similar type, assign sequence 1 to the first system intended to serve that type of load, assign sequence 2 to the next system, and so on. For situations where one or more equipment types has limited capacity or limited control capability, order the sequence so that the most controllable piece of equipment runs last. For example, with a dedicated outdoor air system (DOAS), the air terminal for the DOAS should be assigned Heating Sequence = 1 and Cooling Sequence = 1. Any other equipment should be assigned sequence 2 or higher so that it will see the net load after the DOAS air is added to the zone.

#### Field: Zone Equipment <x> Object Type

Type of zone equipment such as air distribution unit, baseboard, window air conditioner, etc. The current legal types are listed in the following table:

Table: Legal [Zone](#zone) Equipment Types ([ZoneHVAC:EquipmentList](#zonehvacequipmentlist))

Legal [Zone](#zone) Equipment Types
--------------------------
AirTerminal:SingleDuct:Uncontrolled
Fan:ZoneExhaust
WaterHeater:HeatPump
ZoneHVAC:AirDistributionUnit
ZoneHVAC:Baseboard:Convective:Electric
ZoneHVAC:Baseboard:Convective:Water
ZoneHVAC:Baseboard:RadiantConvective:Electric
ZoneHVAC:Baseboard:RadiantConvective:Water
ZoneHVAC:Baseboard:RadiantConvective:Steam
ZoneHVAC:Dehumidifier:DX
ZoneHVAC:EnergyRecoveryVentilator
ZoneHVAC:FourPipeFanCoil
ZoneHVAC:HighTemperatureRadiant
ZoneHVAC:IdealLoadsAirSystem
ZoneHVAC:LowTemperatureRadiant:ConstantFlow
ZoneHVAC:LowTemperatureRadiant:Electric
ZoneHVAC:LowTemperatureRadiant:VariableFlow
ZoneHVAC:OutdoorAirUnit
ZoneHVAC:PackagedTerminalAirConditioner
ZoneHVAC:PackagedTerminalHeatPump
ZoneHVAC:RefrigerationChillerSet
ZoneHVAC:UnitHeater
ZoneHVAC:UnitVentilator
ZoneHVAC:WindowAirConditioner
ZoneHVAC:WaterToAirHeatPump
ZoneHVAC:VentilatedSlab

#### Field: Zone Equipment <x> Name

Name of the zone equipment used in the object definition of its type.

#### Field: Zone Equipment <x> Cooling Sequence

Specifies the zone equipment simulation order when the zone thermostat requests cooling.

#### Field: Zone Equipment <x> Heating or No-Load Sequence

Specifies the zone equipment simulation order when the zone thermostat requests heating or no load.

Examples of this statement in an IDF are:

~~~~~~~~~~~~~~~~~~~~

      ZoneHVAC:EquipmentList,
        Zone1Equipment,          !- Name
        ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type
        Zone1TermReheat,         !- Zone Equipment 1 Name
        1,                       !- Zone Equipment 1 Cooling Sequence
        1;                       !- Zone Equipment 1 Heating or No-Load Sequence

      ZoneHVAC:EquipmentList,
        Zone1Equipment,          !- Name
        AirTerminal:SingleDuct:Uncontrolled,  !- Zone Equipment 1 Object Type
        Zone1DirectAir,          !- Zone Equipment 1 Name
        1,                       !- Zone Equipment 1 Cooling Sequence
        1,                       !- Zone Equipment 1 Heating or No-Load Sequence
        ZoneHVAC:WaterToAirHeatPump,  !- Zone Equipment 2 Object Type
        Zone1WTAHP,              !- Zone Equipment 2 Name
        2,                       !- Zone Equipment 2 Cooling Sequence
        2;                       !- Zone Equipment 2 Heating or No-Load Sequence
~~~~~~~~~~~~~~~~~~~~