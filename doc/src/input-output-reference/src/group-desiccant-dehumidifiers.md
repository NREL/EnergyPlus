# Group – Desiccant Dehumidifiers

## Dehumidifier:Desiccant:NoFans

This object models a solid desiccant dehumidifier (excluding associated fans). The process air stream is the air which is dehumidified. The regen air stream is the air which is heated to regenerate the desiccant. This object determines the process air outlet conditions, the load on the regeneration heating coil, the electric power consumption for the wheel rotor motor, and the regeneration air fan mass flow rate. All other heat exchangers are modeled as separate objects connected to the inlet and outlet nodes of the dehumidifier. The solid desiccant dehumidifier is typically used in an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem) object, but can also be specified in any [AirLoopHVAC](#airloophvac). The regeneration heating coil can be Gas, Electric, Steam , or Hot Water coil.  When hot water coil is selected as regeneration heating coil user-defined curves designed for lower temperature operation must be specified in the input field Performance Model Type along with the Nominal Regeneration Temperature input field. The default performance model type is valid for higher nominal regeneration temperature (e.g. 121C).

### Inputs

#### Field: Name

This alpha field contains the identifying name for the desiccant dehumidifier.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the desiccant unit can run during a given time period. A schedule value of 0 indicates that the unit is off for that time period. A schedule value greater than 0 indicates that the unit can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Process Air Inlet Node Name

The name of the node entering the process side of the desiccant wheel.

#### Field: Process Air Outlet Node Name

The name of the node leaving the process side of the desiccant wheel.

#### Field: Regeneration Air inlet Node Name

The name of the node entering the regeneration side of the desiccant wheel after the regeneration coil.

#### Field: Regeneration Fan Inlet Node Name

Node name for air entering the regeneration fan, mass flow is set by this desiccant dehumidifier model.

#### Field: Control Type

Type of setpoint control. Options are

- LeavingMaximumHumidityRatioSetpoint
- SystemNodeMaximumHumidityRatioSetpoint

**LeavingMaximumHumidityRatioSetpoint** means that the unit is controlled to deliver air at the *Leaving Maximum Humidity Ratio Setpoint*, using bypass dampers to prevent overdrying.

**SystemNodeMaximumHumidityRatioSetpoint** means that the unit is controlled to deliver air at the maximum humidity ratio setpoint (System Node Humidity Ratio Max) on the *Process Air outlet node*, using bypass dampers to prevent overdrying. This setpoint must be established using a set point manager which sets the MaximumHumidityRatio control variable:

- SetpointManager:SingleZone:Humidity:Maximum
- SetpointManager:MultiZone:MaximumHumidity:Average
- SetpointManager:MultiZone:Humidity:Maximum

This will also require the use of a **ZoneControl:Humidistat** object. If the dehumidifer is located in the outdoor air stream, it may also be necessary to use **SetpointManager:OutdoorAirPretreat**.

#### Field: Leaving Maximum Humidity Ratio Setpoint

Fixed setpoint for maximum process air leaving humidity ratio. Applicable only when Control Type = LeavingMaximumHumidityRatioSetpoint.

#### Field: Nominal Process Air Flow Rate

 Process air flow rate in m^3^/s at nominal conditions. This field is autosizable.

#### Field: Nominal Process Air Velocity

Process air velocity in m/s at nominal flow. The default value is 3m/s.

#### Field: Rotor Power

Power input to wheel rotor motor in W. If this field is unknown, electricity consumption of the unit can be obtained from nominal power per unit air flow rate below.

#### Field: Regeneration Coil Object Type

Type of heating coil object for regeneration air. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the regeneration air heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object ([Dehumidifier:Desiccant:NoFans](#dehumidifierdesiccantnofans)) itself provides the "controller" function of modulating water flow. The valid choices are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Regeneration Coil Name

Name of heating coil object for regeneration air.

#### Field: Regeneration Fan Object Type

Type of fan object for regeneration air. For UserCurves performance (see below) [Fan:VariableVolume](#fanvariablevolume) and [Fan:ConstantVolume](#fanconstantvolume) are valid. For Default performance (see below) only [Fan:VariableVolume](#fanvariablevolume) is valid.

#### Field: Regeneration Fan Name

Name of fan object for regeneration air.

#### Field: Performance Model Type

Specifies whether the Default performance model or UserCurves curves should be used to model the performance. The default model is a generic solid desiccant wheel using performance curves of the form:

curve = C1 + C2\*edb + C3\*edb\*\*2 + C4\*ew + C5\*ew\*\*2 + C6\*vel + C7\*vel\*\*2 + C8\*edb\*ew + C9\*edb\*\*2\*ew\*\*2 + C10\*edb\*vel + C11\*edb\*\*2\*vel\*\*2 + C12\*ew\*vel + C13\*ew\*\*2\*vel\*\*2 + C14\*ALOG(edb) + C15\*ALOG(ew) + C16\*ALOG(vel) edb = process entering drybulb temperature [C]   ew  = process entering humidity ratio [kgWater/kgDryAir]   vel = process air velocity [m/s]

The Default curves are valid for the following range of process inlet conditions:  dry-bulb temperatures of 1.67C (35F) to 48.9C (120F) and humidity ratios of 0.002857 kgWater/kgDryAir (20 gr/lb) to 0.02857 kgWater/kgDryAir (200 gr/lb). If the process inlet conditions are outside this range, the dehumidifier will not operate.

If UserCurves are specified, then performance is calculated as follows:

Leaving Dry-bulb = (Leaving Dry-Bulb Function of Entering Dry-Bulb and Humidity Ratio Curve) \* (Leaving Dry-Bulb Function of Air Velocity Curve)

Leaving Humidity Ratio = (Leaving Humidity Ratio Function of Entering Dry-Bulb and Humidity Ratio Curve) \* (Leaving Humidity Ratio Function of Air Velocity Curve)

Regeneration Energy = (Regeneration Energy Function of Entering Dry-Bulb and Humidity Ratio Curve) \* (Regeneration Energy Function of Air Velocity Curve)

Regeneration Velocity = (Regeneration Velocity Function of Entering Dry-Bulb and Humidity Ratio Curve) \* (Regeneration Velocity Function of Air Velocity Curve)

The UserCurves are limited to the following range of process inlet conditions (essentially not limited):  dry-bulb temperatures of –73.3C (-100F) to 65.6C (150F) and humidity ratios of 0.0 kgWater/kgDryAir (0 gr/lb) to 0.21273 kgWater/kgDryAir (1490 gr/lb). If the process inlet conditions are outside this range, the dehumidifier will not operate.

When the Default performance model is selected, the remaining fields are ignored.

#### Field: Leaving Dry-Bulb Function of Entering Dry-Bulb and Humidity Ratio Curve Name

*This field is applicable only when* UserCurves *performance model type is specified.*

Leaving dry-bulb of process air as a function of entering dry-bulb and entering humidity ratio, biquadratic curve.

curve = C1 + C2\*edb + C3\*edb\*\*2 + C4\*ew + C5\*ew\*\*2 + C6\*edb\*ew

edb = process entering drybulb temperature [C]

ew  = process entering humidity ratio [kgWater/kgDryAir]

#### Field: Leaving Dry-Bulb Function of Air Velocity Curve Name

*This field is applicable only when* UserCurves *performance model type is specified.*

Leaving dry-bulb of process air as a function of air velocity,  quadratic curve.

curve = C1 + C2\*v + C3\*v\*\*2

v = process air velocity [m/s]

#### Field: Leaving Humidity Ratio Function of Entering Dry-Bulb and Humidity Ratio Curve Name

*This field is applicable only when* UserCurves *performance model type is specified.*

Leaving humidity ratio of process air as a function of entering dry-bulb and entering humidity ratio, biquadratic curve

curve = C1 + C2\*edb + C3\*edb\*\*2 + C4\*ew + C5\*ew\*\*2 + C6\*edb\*ew

edb = process entering drybulb temperature [C]

ew  = process entering humidity ratio [kgWater/kgDryAir]

#### Field: Leaving Humidity Ratio Function of Air Velocity Curve Name

*This field is applicable only when* UserCurves *performance model type is specified.*

Leaving humidity ratio of process air as a function of process air velocity, quadratic curve.

curve = C1 + C2\*v + C3\*v\*\*2

v = process air velocity [m/s]

#### Field: Regeneration Energy Function of Entering Dry-Bulb and Humidity Ratio Curve Name

*This field is applicable only when* UserCurves *performance model type is specified.*

Regeneration energy [J/kg of water removed] as a function of entering dry-bulb and entering humidity ratio, biquadratic curve

curve = C1 + C2\*edb + C3\*edb\*\*2 + C4\*ew + C5\*ew\*\*2 + C6\*edb\*ew

edb = process entering drybulb temperature [C]

ew  = process entering humidity ratio [kgWater/kgDryAir]

#### Field: Regeneration Energy Function of Air Velocity Curve Name

*This field is applicable only when* UserCurves *performance model type is specified.*

Regeneration energy [J/kg of water removed] as a function of process air velocity, quadratic curve.

curve = C1 + C2\*v + C3\*v\*\*2

v = process air velocity [m/s]

#### Field: Regeneration Velocity Function of Entering Dry-Bulb and Humidity Ratio Curve Name

*This field is applicable only when* UserCurves *performance model type is specified.*

Regeneration velocity [m/s] as a function of entering dry-bulb and entering humidity ratio, biquadratic curve

curve = C1 + C2\*edb + C3\*edb\*\*2 + C4\*ew + C5\*ew\*\*2 + C6\*edb\*ew

edb = process entering drybulb temperature [C]

ew  = process entering humidity ratio [kgWater/kgDryAir]

#### Field: Regeneration Velocity Function of Air Velocity Curve Name

*This field is applicable only when* UserCurves *performance model type is specified.*

Regeneration velocity [m/s] as a function of process air velocity, quadratic curve.

curve = C1 + C2\*v + C3\*v\*\*2

v = process air velocity [m/s]

#### Field: Nominal Regeneration Temperature

*This field is applicable only when* UserCurves *performance model type is specified.*

Nominal regeneration temperature upon which the regeneration energy modifier curve is based. This input is ignored when Performance Model Type = Default, which assume a regeneration temperature of 121C.

#### Field: Nominal Power Per Unit Air Flow Rate

This field is nominal power consumption per unit air flow rate. It is used to calculate electricity consumption of the unit when no rotor power is entered.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

      Dehumidifier:Desiccant:NoFans,
        Desiccant 1,             !- Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        Outside Air Inlet Node,  !- Process Air Inlet Node Name
        Desiccant Process Outlet Node,  !- Process Air Outlet Node Name
        Regen Coil Out Node,     !- Regeneration Air Inlet Node Name
        Outside Air Inlet Node 2,!- Regeneration Fan Inlet Node Name
        SystemNodeMaximumHumidityRatioSetpoint,  !- Control Type
        0.007,                   !- Leaving Maximum Humidity Ratio Setpoint {kgWater/kgDryAir}
        1,                       !- Nominal Process Air Flow Rate {m3/s}
        2.5,                     !- Nominal Process Air Velocity {m/s}
        10,                      !- Rotor Power {W}
        Coil:Heating:Gas,        !- Regeneration Coil Object Type
        Desiccant Regen Coil,    !- Regeneration Coil Name
        Fan:VariableVolume,      !- Regeneration Fan Object Type
        Desiccant Regen Fan,     !- Regeneration Fan Name
        UserCurves,              !- Performance Model Type
        Desiccant DryBulb fTW Curve, !- Leaving Dry-Bulb Function of Entering Dry-Bulb and Humidity Ratio
                 !                         Curve Name
        Desiccant DryBulb fV Curve,  !- Leaving Dry-Bulb Function of Air Velocity Curve Name
        Desiccant HumRat fTW Curve,  !- Leaving Humidity Ratio Function of Entering Dry-Bulb and Humidity Ratio Curve Name
        Desiccant HumRat fV Curve,   !- Leaving Humidity Ratio Function of Air Velocity Curve Name
        Desiccant RegenEnergy fTW Curve, !- Regeneration Energy Function of Entering Dry-Bulb and Humidity Ratio Curve Name
        Desiccant RegenEnergy fV Curve,  !- Regeneration Energy Function of Air Velocity Curve Name
        Desiccant RegenVel fTW Curve,    !- Regeneration Velocity Function of Entering Dry-Bulb and Humidity Ratio Curve Name
        Desiccant RegenVel fV Curve,     !- Regeneration Velocity Function of Air Velocity Curve Name
        121,                     !- Nominal Regeneration Temperature {C}
        ;  !- Nominal Power Per Unit Air Flow Rate {W/m3/s}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Dehumidifier Removed Water Mass [kg]
    HVAC,Average,Dehumidifier Removed Water Mass Flow Rate [kg/s]
    HVAC,Average,Dehumidifier Part Load Ratio []
    HVAC,Average,Dehumidifier Electric Power [W]
    HVAC,Sum,Dehumidifier Electric Energy [J]
    HVAC,Average,Dehumidifier Regeneration Specific Energy [J/kgWater]
    HVAC,Average,Dehumidifier Regeneration Rate [W]
    HVAC,Sum,Dehumidifier Regeneration Energy [J]
    HVAC,Average,Dehumidifier Regeneration Air Speed [m/s]
    HVAC,Average,Dehumidifier Regeneration Air Mass Flow Rate [kg/s]
    HVAC,Average,Dehumidifier Process Air Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Dehumidifier Removed Water Mass  [kg]

Mass of water removed from process air stream.

#### Dehumidifier Removed Water Mass Flow Rate [kg/s]

Rate of water removal from process air stream.

#### Dehumidifier Part Load Ratio []

Dehumidifier water removal rate divided by full-load water removal rate.

#### Dehumidifier Electric Power [W]

Dehumidifier rotor electric power.

#### Dehumidifier Electric Energy [J]

Dehumidifier rotor electric energy.

#### Dehumidifier Regeneration Specific Energy [J/kgWater]

Regeneration heating coil energy divided by water removed.

#### Dehumidifier Regeneration Rate [W]

Regeneration heating coil output rate.

#### Dehumidifier Regeneration Energy [J]

Regeneration heating coil output energy.

#### Dehumidifier Regeneration Air Speed[m/s]

Regeneration air velocity.

#### Dehumidifier Regeneration Air Mass Flow Rate [kg/s]

Regeneration air mass flow rate.

#### Dehumidifier Process Air Mass Flow Rate [kg/s]

Process air mass flow rate.

## Dehumidifier:Desiccant:System

The [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) object models the dehumidification of an air stream, normally called the process air stream. A second heated air stream, called the regeneration air stream, is used to remove the collected moisture from the desiccant heat exchanger and this moisture-laden air is then usually exhausted from the building. This [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) object is similar to the [Dehumidifier:Desiccant:NoFans](#dehumidifierdesiccantnofans) object but has some additional modeling capabilities.

The [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) object in EnergyPlus is a compound object that can be placed anywhere in an air loop ([AirLoopHVAC](#airloophvac)). Common locations for this object are in an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem) or in the main air loop ([AirLoopHVAC](#airloophvac)) downstream of a cooling coil (postcooling desiccant dehumidifier). This compound object coordinates the operation of several ‘children' objects: a desiccant heat exchanger, a regeneration air fan, and an optional regeneration air heater. Gas, Electric, Steam, or Hot Water heating coils can be used for regenerator air heaters. If this dehumidifier is placed in the main air loop immediately downstream of a direct expansion (DX) cooling coil, then the dehumidifier's operation can be coordinated with the operation of the companion DX coil and it is also possible to specify that the DX system's condenser waste heat can be used to help regenerate the desiccant heat exchanger. For the case of condenser waste heat regeneration, an optional exhaust fan can also be modeled by this desiccant dehumidifier compound object to help maintain a set point temperature for air entering the regeneration side of the desiccant heat exchanger.

> It is important to note that the optional exhaust air fan is modeled internal to the [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) and a separate fan object should *not* be added to the input data file (idf) for this fan. On the other hand, a separate fan object *is* required in the input data file for the regeneration air fan.

![Schematic of Dehumidifier:Desiccant:System with Draw Through Regeneration Fan Placement](media/schematic-of-dehumidifier-desiccant-system.jpeg)


A schematic of the compound object [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) is shown in Figure 141 with the draw through regeneration air fan placement. Figure 142 shows the [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) object configured with the blow through regeneration air fan placement.

> NOTE: As with any air loop compound object, the [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) object itself is specified on the [AirLoopHVAC](#airloophvac) [Branch](#branch) or in the [AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist) for an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem). The children objects (e.g., desiccant heat exchanger, regeneration air fan, and optional regeneration air heater) must be specified separately in the input data file and their inlet/outlet connections must be as shown in Figure 141 or Figure 142.

![Schematic of Dehumidifier:Desiccant:System with Blow Through Regeneration Fan Placement](media/schematic-of-dehumidifier-desiccant-system-001.jpeg)


Currently the only heat exchanger choice for this object is HeatExchanger:Desiccant: BalancedFlow. So to model a [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) located in an air loop, the input data file should include the following objects:

[Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) (in an air loop ([AirLoopHVAC](#airloophvac)) [Branch](#branch) or [AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist) for an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem))

[HeatExchanger:Desiccant:BalancedFlow](#heatexchangerdesiccantbalancedflow) (desiccant heat exchanger child object)

[HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1](#heatexchangerdesiccantbalancedflowperformancedatatype1) (desiccant heat exchanger data object)

[ZoneControl:Humidistat](#zonecontrolhumidistat), and one of:

- SetpointManager:SingleZone:Humidity:Maximum
- SetpointManager:MultiZone:Humidity:Maximum
- SetpointManager:MultiZone:MaximumHumidity:Average

 (when in an air loop ([AirLoopHVAC](#airloophvac)) [Branch](#branch)), and  [SetpointManager:OutdoorAirPretreat](#setpointmanageroutdoorairpretreat) (when in an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem)) to place a maximum humidity ratio set point on the sensor node, typically the process air outlet node

[Fan:OnOff](#fanonoff) or [Fan:ConstantVolume](#fanconstantvolume) (regeneration air fan)

[Coil:Heating:Electric](#coilheatingelectric) or [Coil:Heating:Gas](#coilheatinggas) (optional regeneration air heater)

[Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed) or [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode) (optional companion cooling coil)

If the user wants to model the [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) in an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem), then the process air path of the dehumidifier should be located in the outdoor air stream and the regeneration air path may be placed in the relief air stream or modeled by the desiccant dehumidifier itself where the first node for the regeneration inlet air stream must be an outdoor air node. If the user wants to model the [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) in an air loop ([AirLoopHVAC](#airloophvac)) [Branch](#branch), then the process air path of the dehumidifier should be located in the air loop [Branch](#branch) object. For this case, the regeneration air stream is modeled by the desiccant dehumidifier object itself (i.e., not part of an air loop [Branch](#branch) statement) and the first node for the regeneration inlet air stream must be an outdoor air node (ref. Figure 141 or Figure 142).

A description of each input field for this object is provided below:

### Inputs

#### Field: Name

A unique, user-assigned name for a particular [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem). Any reference to this dehumidifier by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the dehumidifier can operate during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the dehumidifier can operate. A value less than or equal to 0 (usually 0 is used) denotes that the dehumidifier will not operate (i.e., no heat exchange will take place and the regeneration air fan does not operate). If the field is blank, the schedule has a value of 1 for all time periods. For the case where companion cooling coil regeneration air heating has been specified, the desiccant dehumidifier's exhaust fan serves as the condenser air fan for the cooling coil system so this availability schedule will not disable exhaust fan operation.

#### Field: Desiccant Heat Exchanger Object Type

This alpha field contains the type of desiccant heat exchanger used with this dehumidifier. Currently, the only valid choice is [HeatExchanger:Desiccant:BalancedFlow](#heatexchangerdesiccantbalancedflow).

#### Field: Desiccant Heat Exchanger Name

This alpha field contains the name of the desiccant heat exchanger used with this dehumidifier.

#### Field: Sensor Node Name

This alpha field specifies the name of the air loop node used to control desiccant heat exchanger operation. A set point manager must be used to place a maximum humidity ratio set point on this node (e.g., [SetpointManager:SingleZone:Humidity:Maximum](#setpointmanagersinglezonehumiditymaximum) or [SetpointManager:OutdoorAirPretreat](#setpointmanageroutdoorairpretreat)).

#### Field: Regeneration Air Fan Object Type

This alpha field contains the type of regeneration air fan used. Available fan types are [Fan:OnOff](#fanonoff) and [Fan:ConstantVolume](#fanconstantvolume).

#### Field: Regeneration Air Fan Name

This alpha field contains the name of the regeneration air fan used with this dehumidifier.

#### Field: Regeneration Air Fan Placement

This alpha field specifies the fan configuration used in the desiccant dehumidifier. Valid choices are ‘BlowThrough' and ‘DrawThrough', with a default of ‘DrawThrough' if this field is left blank.

#### Field: Regeneration Air Heater Object Type

This alpha field contains the type of heating coil used to heat the regeneration air stream. This field may be left blank when no regeneration air heater is required. The hot water and steam heating coils require specifying plant loop, branches, and connector objects to support the heating coils, and are placed on the demand side of the plantloop. The hot water flow modulation through the regeneration air heating coil does not require additional controller or [Controller:WaterCoil](#controllerwatercoil) object. The parent object ([Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem)) itself provides the "controller" function of modulating water flow. Valid choices are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric
    Coil:Heating:Gas
    Coil:Heating:Water
    Coil:Heating:Steam
~~~~~~~~~~~~~~~~~~~~

#### Field: Regeneration Air Heater Name

This alpha field contains the name of the heating coil used to heat the regeneration air stream. This field may be left blank when no regeneration air heater is required.

#### Field: Regeneration Inlet Air Setpoint Temperature

This optional numeric field specifies the regeneration air inlet temperature setpoint in Celsius. The regeneration air heater and/or the companion coil regeneration air heating will be controlled to this temperature to the extent possible. This field may be left blank when no regeneration air heater is required or when control of the exhaust fan used with the companion coil regeneration air heating option is not required.

#### Field: Companion Cooling Coil Object Type

This optional alpha field contains the type of companion cooling coil used with this desiccant dehumidifier. The only valid choices are [Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed) and [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode).

#### Field: Companion Cooling Coil Name

This optional alpha field contains the name of the companion cooling coil used with this desiccant dehumidifier. This field may be left blank when no companion cooling coil is being modeled.

#### Field: Companion Cooling Coil Upstream of Dehumidifier Process Inlet

This choice field specifies if the companion cooling coil is located immediately upstream of the dehumidifier's process inlet. Valid choices are Yes and No. If Yes is selected, then the outlet air node for the companion cooling coil must be the same as the dehumidifier's process air inlet node (i.e., the process air inlet node name for the desiccant heat exchanger specified for this desiccant dehumidifier). For this case, the companion cooling coil and the desiccant dehumidifier are assumed to operate "in tandem"; that is, if the simulation determines that the companion cooling coil is unable to meet the humidity set point specified on the sensor node based on its own operation, then the desiccant dehumidifier operates at the same time and for the same duration as the cooling coil to provide improved dehumidification. If No is selected, then the dehumidifier will control to the humidity set point specified on the sensor node to the extent possible. The default value is No if this field is left blank.

#### Field: Companion Coil Regeneration Air Heating

This choice field determines if the companion cooling coil's condenser waste heat is used to heat the regeneration inlet air. Valid choices are Yes and No. The default value is No if this field is left blank.

#### Field: Exhaust Fan Maximum Flow Rate

This optional numeric field contains the maximum fan volumetric flow rate for the exhaust fan in cubic meters per second. As noted previously, this exhaust fan is modeled internally by the [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) object and a separate fan object should NOT be specified in the input data file for this fan. This field is used only when a companion cooling coil is specified and the ‘Companion Coil Regeneration Air Heating' field is set to ‘Yes'. This field must be used in conjunction with the ‘Exhaust Fan Maximum Power' and the ‘Exhaust Fan Power Curve Name' input fields. The model assumes that the exhaust fan will operate as needed to maintain the ‘Regeneration Inlet Air Setpoint Temperature', up to the maximum flow rate specified in this input field. If the desiccant dehumidifier is OFF for a simulation timestep but its companion cooling coil is operating and is specified to provide regeneration air heating, then the exhaust fan operates at this maximum air flow rate (i.e., this fan serves as the condenser fan for the companion cooling coil system when regeneration air heating is specified, so the inputs to the companion cooling coil object should not include the condenser fan energy since the condenser fan energy is modeled by the [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem) object).

#### Field: Exhaust Fan Maximum Power

This optional numeric field contains the maximum power for the exhaust fan in Watts (i.e., at the Exhaust Fan Maximum Flow Rate). This field is used only when a companion cooling coil is used and the ‘Companion Coil Regeneration Air Heating' field is set to ‘Yes'. This field must be used in conjunction with the ‘Exhaust Fan Maximum Flow Rate' and the ‘Exhaust Fan Power Curve Name' input fields.

#### Field: Exhaust Fan Power Curve Name

This optional alpha field contains the name of the exhaust fan power modifier curve. This field is used only when a companion cooling coil is used and the ‘Companion Coil Regeneration Air Heating' field is set to ‘Yes'. This field must be used in conjunction with the ‘Exhaust Fan Maximum Flow Rate' and the ‘Exhaust Fan Maximum Power' input fields. If this field is blank, the exhaust fan operates (when required) at the maximum power specified in the field above. The curve object type for this Exhaust Fan Power Curve Name must be [Curve:Cubic](#curvecubic) or [Curve:Quadratic](#curvequadratic). The curve object ([Curve:Cubic](#curvecubic) or [Curve:Quadratic](#curvequadratic)) defines the change in exhaust fan power as a function of the ratio of the actual exhaust air flow rate divided by the maximum flow rate.

Following is an example input for this object:

~~~~~~~~~~~~~~~~~~~~

      Dehumidifier:Desiccant:System,
        Desiccant 1,             !- Name
        FanAvailSched,           !- Availability Schedule Name
        HeatExchanger:Desiccant:BalancedFlow,  !- Desiccant Heat Exchanger Object Type
        Desiccant Heat Exchanger 1,  !- Desiccant Heat Exchanger Name
        HX Process Outlet Node,  !- Sensor Node Name
        Fan:ConstantVolume,      !- Regeneration Air Fan Object Type
        Desiccant Regen Fan,     !- Regeneration Air Fan Name
        DrawThrough,             !- Regeneration Air Fan Placement
        Coil:Heating:Gas,        !- Regeneration Air Heater Object Type
        Desiccant Regen Coil,    !- Regeneration Air Heater Name
        46.111111,               !- Regeneration Inlet Air Setpoint Temperature {C}
        Coil:Cooling:DX:SingleSpeed,  !- Companion Cooling Coil Object Type
        Desiccant DXSystem Cooling Coil,  !- Companion Cooling Coil Name
        Yes,              !- Companion Cooling Coil Upstream of Dehumidifier Process Inlet
        Yes,                     !- Companion Coil Regeneration Air Heating
        1.05,                    !- Exhaust Fan Maximum Flow Rate {m3/s}
        50,                      !- Exhaust Fan Maximum Power {W}
        EXHAUSTFANPLF;           !- Exhaust Fan Power Curve Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Dehumidifier Removed Water Mass [kg]
    HVAC,Average,Dehumidifier Removed Water Mass Flow Rate [kg/s]
    HVAC,Average,Dehumidifier Part Load Ratio []
    HVAC,Average,Dehumidifier Exhaust Fan Electric Power [W]
    HVAC,Sum,Dehumidifier Exhaust Fan Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Dehumidifier Removed Water Mass  [kg]

This output is the mass of water removed from the process air stream in kilograms for the timestep being reported.

#### Dehumidifier Removed Water Mass Flow Rate [kg/s]

This output is the average rate of water removal from the process air stream in kilograms per second for the timestep being reported.

#### Dehumidifier Part Load Ratio [ ]

This output is the fraction of time that the desiccant heat exchanger (and associated regeneration air heater and fans, if appropriate) operate for the timestep being reported.

#### Dehumidifier Exhaust Fan Electric Power [W]

This output is the average electric consumption rate for the exhaust fan in Watts for the timestep being reported.

#### Dehumidifier Exhaust Fan Electric Energy [J]

This output is the electric consumption for the exhaust fan in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, EndUseKey = Cooling, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).