# Group – Controllers

## Controls (Air Loop and Zone Equipment)

A controller mimics the function of an actual physical controller in a somewhat rudimentary way. It can sense one node variable, compare it with its setpoint, and determine the desired value for another node variable. It should be noted that a controller cannot span a loop manager boundary (but a Setpoint Manager can). Thus, in an air loop ([AirLoopHVAC](#airloophvac)), the sensed node and the controlled device must be in the air loop simulation. This means that a single zone system cannot be modeled with a simple controller sensing zone temperature and controlling coil water flow. Instead this must be modeled as a Setpoint Manager sensing a zone temperature and resetting the supply air temperature setpoint each timestep. This is artificial but should simplify the control modeling task. It should also be noted that there are various types of controllers and that each controller might hook into loops in a slightly different way. As a result, each controller type is described separately below.

## Controller:WaterCoil

This controller is really a solution inverter. For a water coil the simulation cannot be inverted where the mass flow rate of the water through the coil can be solved directly given an air temperature. Thus, this "controller" will numerically step through all of the water flow possibilities by an interval-halving technique until the mass flow rate is determined to meet the specified outlet air temperature within a specified user tolerance.

As the reader probably noted when reading the descriptions of the coil syntax shown earlier in this section, there were no controls attached directly to a particular component. This is because the input can be simplified somewhat by entering node names to be controlled. This avoids having to search through multiple lists of component types for the sake of simply controlling components. The [Controller:WaterCoil](#controllerwatercoil) shown below is a way of controlling variables at one node based on conditions at another node. After the identifying name for the controller, the user must define which control variable the controller is managing. These options include Temperature, Humidity Ratio, TemperatureAndHumidityRatio), or Flow.

The next parameter in the input syntax is the Action of the control, which determines how the controlled variable (e.g., mass flow rate through a water coil) is changed based on the control signal. The following input parameter is the actuator variable, which is currently limited to mass flow through a water coil.

The next two parameters in the input syntax are node names corresponding to the node that is being sensed and the actuated node which controls the water mass flow through the coil. For example, in the case of a cooling coil the control variable might be the outlet air temperature of the coil while the actuated variable might be the water flow rate through the coil. These two parameters are followed by the controller convergence tolerance. Finally, the last two input parameters represent the maximum and minimum values allowed for the actuated variable at the actuated node.

### Inputs

#### Field: Name

This is the unique name of the controller.

#### Field: Control Variable

This was setup to be generic but to date has only been used for temperature control, or temperature and humidity ratio control, of a water coil in the air loop simulation. The keyword Temperature is used for air temperature control and is normally specified for the coil's outlet air node. The keyword TemperatureAndHumidityRatio is used for controlling both air temperature and high humidity levels, and is normally specified for a cooling coil's outlet air node. The keyword HumidityRatio is used for humidity control and would normally be specified for a dehumidifier outlet node. These two keywords require a [ZoneControl:Humidistat](#zonecontrolhumidistat) object and a maximum humidity setpoint manager object ("SetPointManager:SingleZone:Humidity:Maximum", "SetPointManager:MultiZone:MaximumHumidity:Average" or "SetPointManager:MultiZone:Humidity:Maximum"). If the coil is located in the outdoor air stream, it may also be necessary to use [SetpointManager:OutdoorAirPretreat](#setpointmanageroutdoorairpretreat).

#### Field: Action

The next input refers to the "action" of the control. This can be best described by an example. In a coil where water mass flow rate is to be controlled, a coil will increase the mass flow rate through the coil when more heating or cooling is requested. In a heating coil, this increases the value of heat transfer from the water to the air stream. As a result, this is considered a "Normal" action controller. In a cooling coil, an increase in water mass flow rate through the coil decreases the value of heat transfer from the water to the air stream (absolute value increases, but since cooling is traditionally described as a negative number, an increase in absolute value results in a decrease in the actual heat transfer value). Thus, the cooling coil controller has "Reverse" action since an increase in flow rate results in a decrease in heat transfer.

#### Field: Actuator Variable

This was again meant to be more generic but currently has only been used to control the water mass flow rate of a heating or cooling coil. This actuator variable must be set to the keyword Flow to control the water mass flow rate.

#### Field: Sensor Node Name

Name of the node where the temperature, humidity ratio or flow is sensed.

#### Field: Actuator Node Name

Name of the actuated node that controls the water mass flow rate through the coil.

#### Field: Controller Convergence Tolerance

The coil is controlled by knowing the outlet temperature and/or humidity ratio specified by the setpoint managers, and setting the outlet conditions from the coil to meet these setpoints. The hot and chilled water coils use complex models that cannot be inverted directly. Therefore, to determine the correct mass flow rate for the hot or cold water the models are inverted numerically using an iterative procedure. The iterative solution uses an interval-halving routine and needs a termination criteria that is set with the Controller Convergence Tolerance parameter. The convergence tolerance is the maximum difference between the actual temperature at the setpoint node and the setpoint temperature. This control offset is set to a small temperature difference, such as 0.01 to denote 1/100 degree C. The default is 0.1 degree C.

#### Field: Maximum Actuated Flow

This is the maximum water flow (m^3^/sec) through the coil. Set to the maximum design water flow for the coil.

#### Field: Minimum Actuated Flow

Set to the minimum design water flow (m^3^/sec) for the water coil, normally a shut off valve that is set to zero.

An example of this object in an IDF, along with appropriate setpoint managers, is shown below:

~~~~~~~~~~~~~~~~~~~~

      Controller:WaterCoil,
        Central Cooling Coil Contoller 1,     !- Name
        TemperatureAndHumidityRatio,          !- Control Variable
        Reverse,                              !- Action
        Flow,                                 !- Actuator Variable
        VAV Sys 1 Outlet Node,                !- Sensor Node Name
        Main Cooling Coil 1 Water Inlet Node, !- Actuator Node Name
        0.002,                                !- Controller Convergence Tolerance {deltaC}
        0.025,                                !- Maximum Actuated Flow {m3/s}
        0.0;                                  !- Minimum Actuated Flow {m3/s}

      SetpointManager:SingleZone:Humidity:Maximum,
        Zone Max Set Point Manager,           !- Name
        ,                                     !- Control Variable
        ,                                     !- Schedule name
        VAV Sys 1 Outlet Node,                !- Setpoint Node or NodeList Name
        Zone 2 Node;                          !- Control Zone Air Node Name

      SetpointManager:Scheduled,
        Scheduled Set Point Manager 1,        !- Name
        Temperature,                          !- Control Variable
        Seasonal Reset Supply Air Temp Sch,   !- Schedule Name
        VAV Sys 1 Outlet Node;                !- Setpoint Node or NodeList Name

~~~~~~~~~~~~~~~~~~~~

## Controller:OutdoorAir

A mixed air box has its own controller type called [Controller:OutdoorAir](#controlleroutdoorair). The purpose of the outdoor air controller is to provide outdoor air for ventilation and also provide free cooling (through additional outdoor air and/or bypassing an air-to-air heat exchanger) whenever possible. The outdoor air controller includes a number of user-selectable limit controls. If any of the selected limits are exceeded, the outdoor airflow rate is set to the minimum.

If all the limits are satisfied, the outdoor air controller does the following for continuous air flow systems: if the outdoor air temperature is greater than or equal to the mixed air temperature setpoint, the outdoor air flow rate is set to the maximum; if the outdoor air temperature is less than the mixed air temperature setpoint, the outdoor air controller will modulate the outdoor air flow so that the mixed air temperature will match the mixed air setpoint temperature.

A time-of-day schedule may also be used to simulate an increase in outdoor air flow rate for "push-button" type economizer applications. When the schedule permits (i.e., schedule values are greater than 0), the outdoor air flow rate is increased to the user-specified maximum outdoor air flow rate.

The outdoor air controller can also account for changes in the outdoor air flow rate during times when indoor humidity levels are high. A zone humidistat must be used with this control option. During high indoor humidity, the outdoor air flow rate is modified in response to a high indoor humidity condition. If high humidity control is based on the outdoor air humidity ratio and the outdoor humidity ratio is greater than the indoor humidity ratio, high humidity control is terminated. When the economizer is used in conjunction with the high humidity control option, high humidity control has priority and controls the change in air flow rates. The Night Ventilation Availability Manager has priority over high humidity control and will use the controller's maximum outdoor air flow rate when this Availability Manager cycles the fan on (Ref. System Availability Managers – [AvailabilityManager:NightVentilation](#availabilitymanagernightventilation))

The mixed air box connections are defined separately in the **OutdoorAir:Mixer** object.

Although the mixer will commonly be connected directly to the outdoor air, other components may be placed on the outdoor air path upstream of the mixer. When this is the case, any modulation will be determined by the conditions at the inlet node of the mixer rather than the outdoor air. This means that the controller will account for any heat recovery or other preheating/precooling components that may modify the condition of outdoor air before it reaches the mixer.

If all the limits are satisfied, the outdoor air controller does the following for cycling fan systems: the outdoor air flow rate is set to the maximum when the fan cycles on. If the limits are not satisfied, the outdoor air flow rate is at the minimum when the fan cycles on.

### Inputs

#### Field: Name

The unique user-assigned name for an instance of an outdoor air controller. Any other object referencing this outdoor air controller will use this name.

#### Field: Relief Air Outlet Node Name

The name of the relief air node of the outdoor air mixer associated with this outdoor air controller.

#### Field: Return Air Node Name

The name of the return air node of the outdoor air mixer associated with this outdoor air controller.

#### Field: Mixed Air Node Name

Name of the node where the mixed air setpoint is set. The outdoor air controller senses the temperature at this node and attempts to control that temperature to the node setpoint.

#### Field: Actuator Node Name

The name of the node that is associated with the outdoor air damper. This should be the outermost air node on the outdoor air path connected to the outdoor air stream for the mixer associated with this outdoor air controller.

#### Field: Minimum Outdoor Air Flow Rate

Input for this field is the minimum outdoor air flow rate for the system in cubic meters per second.

#### Field: Maximum Outdoor Air Flow Rate

Input for this field is the maximum outdoor air flow rate for the system in cubic meters per second.

#### Field: Economizer Control Type

The options for this field are

- FixedDryBulb
- DifferentialDryBulb
- FixedEnthalpy
- DifferentialEnthalpy
- ElectronicEnthalpy
- FixedDewPointAndDryBulb
- DifferentialDryBulbAndEnthalpy
- NoEconomizer

Choosing **NoEconomizer** means the economizer will not operate and the outdoor airflow rate will be at the minimum for the entire simulation.

Choosing **FixedDryBulb** means the economizer will set the outdoor airflow rate at minimum if the outdoor air temperature is higher than a specified dry-bulb temperature limit.

Choosing **DifferentialDryBulb** will trigger the outdoor airflow to minimum when the dry-bulb temperature of outdoor air is higher than the dry-bulb temperature of the return air.

**FixedEnthalpy** checks the upper limit of the enthalpy given as a field input against the enthalpy content of outdoor air and will set the outdoor airflow rate to minimum if the latter is greater than the former.

**DifferentialEnthalpy** does the same thing but compares the return air enthalpy with the enthalpy of outdoor air. When the enthalpy of outdoor air is greater than the enthalpy of the return air, the outdoor air flow rate is set to minimum.

Choosing **ElectronicEnthalpy** enables the simulation to calculate the humidity ratio limit of outdoor air based on the dry-bulb temperature of outdoor air and a quadratic/cubic curve, and compare it to the actual outdoor air humidity ratio. If the actual outdoor humidity ratio is greater than the calculated humidity ratio limit, then the outdoor airflow rate is set to minimum.

Choosing **FixedDewPointAndDryBulb** compares both the outdoor dewpoint temperature and the outdoor dry-bulb temperature to their specified high limit values. If either outdoor temperature exceeds the high limit value, the outdoor airflow rate is set to minimum.

Another option **DifferentialDryBulbAndEnthalpy** enables the control strategy to be based on both the DifferentialDryBulb and DifferentialEnthalpy economizer control strategies.

In addition to all economizer control types listed above, each control type checks for user-entered values for the upper limit of dry-bulb temperature, enthalpy limit, humidity ratio limit and dewpoint limit. The outdoor air flow rate is set to minimum if any of these entered limits are exceeded.

The default for this field is **NoEconomizer**.

#### Field: Economizer Control Action Type

There are two choices for this Field: **MinimumFlowWithBypass** and **ModulateFlow**, with the default being **ModulateFlow** if this input field is left blank.

**ModulateFlow** means the outdoor air flow rate will be increased to meet the mixed air setpoint temperature, subject to the limits imposed via other inputs for this object (e.g., Economizer Maximum Limit Dry-Bulb Temperature, Maximum Outdoor Air Flow Rate, etc.).

**MinimumFlowWithBypass** is used exclusively in conjunction with air-to-air heat exchanger:objects (Ref. HeatExchanger:\*) for providing free cooling operation in the absence of a conventional air-side economizer (i.e., when outdoor air flow rate is not increased during economizer mode). The MinimumFlowWithBypass choice forces the outdoor air flow rate to always remain at the minimum. However, when high humidity control is used, the outdoor air flow rate is set to the product of the maximum outdoor air flow rate multiplied by the high humidity outdoor air flow ratio. The heat exchanger uses the limit checking in the outdoor air controller to decide whether or not to bypass the outdoor air around the heat exchanger – or turn off the wheel motor in the case of a rotary heat exchanger. Heat exchange is also suspended when high humidity control is active.

The **ModulateFlow** option can also be used with the [HeatExchanger:AirToAir:FlatPlate](#heatexchangerairtoairflatplate) or [HeatExchanger:AirToAir:SensibleAndLatent](#heatexchangerairtoairsensibleandlatent) objects.

#### Field: Economizer Maximum Limit Dry-Bulb Temperature

Input for this field is the outdoor air temperature high limit (ºC) for economizer operation. If the outdoor air temperature is above this limit, the outdoor airflow rate will be set to the minimum. This field is required if Economizer Control Type ‘FixedDryBulb' or ‘FixedDewPointAndDryBulb' has been specified.

No input (blank) in this field means that there is no outdoor air temperature high limit control. This limit applies to the conditions at the Actuator Node regardless of whether or not there are any other components in the outdoor air path upstream of the mixer.

#### Field: Economizer Maximum Limit Enthalpy

Input for this field is the outdoor air enthalpy limit (in J/kg) for economizer operation. If the outdoor air enthalpy is above this value, the outdoor airflow rate will be set to the minimum. This field is required if Economizer Control Type ‘FixedEnthalpy' has been specified.

No input (blank) in this field means that there is no outdoor air enthalpy limit control. This limit applies to the conditions at the Actuator Node regardless of whether or not there are any other components in the outdoor air path upstream of the mixer.

#### Field: Economizer Maximum Limit Dewpoint Temperature

Input for this field is the outdoor air dewpoint limit (ºC) for economizer operation. If the outdoor air dewpoint temperature is above this value, the outdoor airflow rate will be set to the minimum. This field is required if the Economizer Control Type ‘'FixedDewPointAndDryBulb' has been specified.

No input (blank) in this field means that there is no outdoor air dewpoint limit control. This limit applies to the conditions at the Actuator Node regardless of whether or not there are any other components in the outdoor air path upstream of the mixer.

#### Field: Electronic Enthalpy Limit Curve Name

Input for this field is the name of a quadratic or cubic curve which provides the maximum outdoor air humidity ratio (function of outdoor air dry-bulb temperature) for economizer operation. If the outdoor air humidity ratio is greater than the curve's maximum humidity ratio (evaluated at the outdoor air dry-bulb temperature), the outdoor air flow rate will be set to the minimum. This limit applies to the conditions at the Actuator Node regardless of whether or not there are any other components in the outdoor air path upstream of the mixer. No input (blank) in this field means that there is no electronic enthalpy limit control.

#### Field: Economizer Minimum Limit Dry-Bulb Temperature

Input for this field is the outdoor air temperature low limit (ºC) for economizer operation. If the outdoor air temperature is below this limit, the outdoor airflow rate will be set to the minimum.

No input (blank) in this field means that there is no outdoor air temperature low limit control. This limit applies to the conditions at the Actuator Node regardless of whether or not there are any other components in the outdoor air path upstream of the mixer.

#### Field: Lockout Type

Choices for this field are NoLockout, LockoutWithHeating, and LockoutWithCompressor. This field is used for packaged systems with DX coils. LockoutWithHeating means that if the packaged unit is in heating mode, the economizer is locked out – i.e., the economizer dampers are closed and there is minimum outdoor air flow. LockoutWithCompressor means that in addition to locking out the economizer when the unit is in heating mode the economizer is locked out when the DX unit compressor is operating to provide cooling. In other words, the economizer must meet the entire cooling load – it isn't allowed to operate in conjunction with the DX cooling coil. This option (LockoutWithCompressor) is sometimes called a "nonintegrated" economizer.

The default is NoLockout.

#### Field: Minimum Limit Type

Choices for this field are FixedMinimum or ProportionalMinimum. FixedMinimum means that the minimum outdoor airflow rate is fixed no matter what the actual system flow rate is. ProportionalMinimum means the minimum outdoor airflow rate varies in proportion to the total system air flow rate. The default is ProportionalMinimum.

#### Field: Minimum Outdoor Air Schedule Name

The name of a schedule which uses decimal values (0.0 – 1.0).  These values are multiplied by the minimum outdoor air flow rate. This schedule is useful for reducing the outdoor air flow rate to zero during unoccupied or start up hours. If this field is not entered, the minimum outdoor air flow rate either remains constant during the simulation period (Minimum Outdoor Air Control Type = FixedMinimum) or varies in proportion to the supply air flow rate (Minimum Outdoor Air Control Type = ProportionalMinimum).

#### Field: Minimum Fraction of Outdoor Air Schedule Name

**The name of a schedule which uses decimal values (0.0 – 1.0) These values are multiplied by the** total (design) air flow rate.  This is an alternate method for specifying the minimum outdoor air amount.  (The other method is the **Minimum Outdoor Air Schedule** *described above.)  Note:  This field overrides* **Minimum Outdoor Air Schedule** *and* **Minimum Outdoor Air Flowrate.**

#### Field: Maximum Fraction of Outdoor Air Schedule Name

This is an optional field with the schedule values in decimal fractions (0.0-1.0). This field enables the user to limit the maximum of amount of outdoor air permissible and thus also limits the permissible maximum outdoor air into the system.

If both the above schedules are given values of 1.0, the system works at 100% outdoor air, so in a system if 100% outdoor air is required regardless of any other condition, these fields are helpful.

#### Field: Mechanical Ventilation Controller Name

This optional field is the name of the mechanical ventilation controller object to be used in conjunction with this outdoor air controller. The [Controller:MechanicalVentilation](#controllermechanicalventilation) object allows the user to define the minimum outdoor air flow rate based on air flow per unit floor area and air flow per person (occupant) for the zones being served by the air loop that utilizes this controller.

This feature allows the user to perform a first-order evaluation of carbon dioxide(CO~2~)-based demand controlled ventilation (outdoor ventilation varied according to occupancy levels).

If a valid name for a [Controller:MechanicalVentilation](#controllermechanicalventilation) object is entered in this field, the minimum outdoor air flow rate delivered will be the greater of:

- the minimum outdoor air flow rate calculated by the fields Minimum Outdoor Air Flow Rate, Minimum Limit Type, and Minimum Outdoor Air Schedule Name as defined for this outdoor air controller, or
- the outdoor air flow rate calculated using the [Controller:MechanicalVentilation](#controllermechanicalventilation) object named in this input field.

Leaving this input field blank will bypass the [Controller:MechanicalVentilation](#controllermechanicalventilation) object calculations and the minimum outdoor air flow rate will be based on the other inputs associated with this outdoor air controller object. Actual outdoor air flow rates may be higher than the minimum if free cooling is available and the object inputs are properly selected. Regardless, the maximum outdoor air flow rate is limited by the field Maximum Outdoor Air Flow Rate.

#### Field: Time of Day Economizer Control Schedule Name

This alpha field is the name of a schedule which controls the outdoor air flow rate based on a time-of-day economizer. Schedule values equal to 0 disable this feature. Schedule values greater than 0 cause the outdoor air flow rate to increase to the maximum. When an economizer is used in conjunction with the high humidity control option, high humidity control has priority.

#### Field: High Humidity Control

This choice field establishes whether or not the outdoor air flow rate is modified in response to high indoor relative humidity. Valid choices are Yes and No. If Yes is selected, the outdoor air flow rate may be modified when the indoor relative humidity is above the humidstat setpoint. If No is selected, this option is disabled and the following three fields are not used.

#### Field: Humidistat Control Zone Name

This input defines the zone name where the humidistat is located. This is the same name used in the [ZoneControl:Humidistat](#zonecontrolhumidistat) object. This field is required when the input field High Humidity Control is specified as Yes.

#### Field: High Humidity Outdoor Air Flow Ratio

This input is the ratio of the modified outdoor air flow rate to the maximum outdoor air flow rate. When the high humidity control algorithm determines that the outdoor air flow rate will be changed (i.e., increased or decreased), the operating outdoor air flow rate is equal to the maximum outdoor air flow rate multiplied by this ratio. The minimum value for this field is 0. If this field is blank, the default value is 1. This field is used only when the input field High Humidity Control is specified as Yes. When an economizer is used in conjunction with the high humidity control option, high humidity control has priority.

#### Field: Control High Indoor Humidity Based on Outdoor Humidity Ratio

This choice field determines if high humidity control is activated based on high indoor relative humidity alone or is activated only when the indoor relative humidity is above the humidstat setpoint *and* the indoor humidity ratio is greater than the outdoor humidity ratio. Valid choices are **Yes** and **No**. If No is selected, high humidity control is active any time the zone humidistat senses a moisture load. If Yes is selected, the model also verifies that the outdoor humidity ratio is less than the humidistat's zone air humidity ratio. This field is used only when the input field High Humidity Control is specified as Yes. The default value is **Yes**.

#### Field: Heat Recovery Bypass Control Type

This choice field determines if specialized control is used to optimize the use of heat recovery. Valid choices are **BypassWhenWithinEconomizerLimits** and **BypassWhenOAFlowGreaterThanMinimum**. If BypassWhenWithinEconomizerLimits is selected, heat recovery is disabled any time the controller determines that the economizer is active (i.e., all controls are within limits). If BypassWhenOAFlowGreaterThanMinimum is selected, the model first verifies that the economizer is active and then checks to see if the outdoor air flow rate is greater than the minimum, if so heat recovery is set to bypass whether or not a heat exchanger is used in the outdoor air system. When this option is used with Time of Day Economizer Control or High Humidity Control, this option has priority. The model then compares the mixed air temperature at minimum outdoor air flow with the heat exchanger off to the mixed air temperature set point. If the mixed air temperature at minimum outdoor air flow with the heat exchanger off is less than the mixed air temperature set point, the outdoor air flow rate is set to the minimum. The model then checks to see if an air loop heating coil is operating. If any airloop heating coil turns on, the heat exchanger is also turned on and the outdoor air flow is set to the minimum. This action is meant to minimize heating energy (this action may also disable the heating coil on subsequent iterations, see output variable Air System Outdoor Air Heat Recovery Bypass Heating Coil Activity Status). This means that heat recovery is active any time the outdoor air flow rate is equal to the minimum even when the economizer requests free cooling. For this case, the use of supply air temperature control for the heat exchanger is recommended (Ref HeatExchanger). The default value is **BypassWhenWithinEconomizerLimits**.

An Example IDF specification:

~~~~~~~~~~~~~~~~~~~~

      Controller:OutdoorAir,
        OA Controller 1,         !- Name
        Relief Air Outlet Node,  !- Relief Air Outlet Node Name
        Outdoor Air Mixer Inlet Node,  !- Return Air Node Name
        Mixed Air Node,          !- Mixed Air Node Name
        Outdoor Air Inlet Node,  !- Actuator Node Name
        0.25,                    !- Minimum Outdoor Air Flow Rate {m3/s}
        1.6,                     !- Maximum Outdoor Air Flow Rate {m3/s}
        ElectronicEnthalpy,      !- Economizer Control Type
        ModulateFlow,            !- Economizer Control Action Type
        23.,                 !- Economizer Maximum Limit Dry-Bulb Temperature {C}
        ,                    !- Economizer Maximum Limit Enthalpy {J/kg}
        13.5,                !- Economizer Maximum Limit Dewpoint Temperature {C}
        ElectronicEnthalpyCurveA,!- Electronic Enthalpy Limit Curve Name
        14.,                 !- Economizer Minimum Limit Dry-Bulb Temperature {C}
        NoLockout,               !- Lockout Type
        FixedMinimum,            !- Minimum Limit Type
        OAFractionSched,         !- Minimum Outdoor Air Schedule Name
        ,                        !- Minimum Fraction of Outdoor Air Schedule Name
        ,                        !- Maximum Fraction of Outdoor Air Schedule Name
        ,                        !- Mechanical Ventilation Controller Name
        TimeOfDayEconomizerSch,  !- Time of Day Economizer Control Schedule Name
        Yes,                     !- High Humidity Control
        EAST ZONE,               !- Humidistat Control Zone Name
        0.9,                     !- High Humidity Outdoor Air Flow Ratio
        Yes;      !- Control High Indoor Humidity Based on Outdoor Humidity Ratio

      Curve:Cubic,
        ElectronicEnthalpyCurveA,!- Name
        0.01342704,              !- Coefficient1 Constant
        -0.00047892,             !- Coefficient2 x
        0.000053352,             !- Coefficient3 x**2
        -0.0000018103,           !- Coefficient4 x**3
        16.6,                    !- Minimum Value of x
        29.13;                   !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~

### Outputs

Note that the key value for these outputs is the [AirLoopHVAC](#airloophvac) name, not the name of the [Controller:OutdoorAir](#controlleroutdoorair).

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Air System Outdoor Air Economizer Status []
    HVAC,Average,Air System Outdoor Air Heat Recovery Bypass Status []
    HVAC,Average,Air System Outdoor Air Heat Recovery Bypass Heating Coil Activity Status []
    HVAC,Average,Air System Outdoor Air Heat Recovery Bypass Minimum Outdoor Air Mixed Air Temperature [C]
    HVAC,Average,Air System Outdoor Air High Humidity Control Status []
    HVAC,Average,Air System Outdoor Air Flow Fraction []
    HVAC,Average,Air System Outdoor Air Minimum Flow Fraction []
    HVAC,Average,Air System Outdoor Air Mass Flow Rate [kg/s]
    HVAC,Average,Air System Mixed Air Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Air System Outdoor Air Economizer Status []

Reports the average operating status of an air economizer over the reporting interval. The economizer status is set to 1 when the conditions are favorable for the economizer to operate (i.e., none of the control limits have been exceeded). While conditions may be favorable for economizer operation, it does not guarantee that the air-side economizer has increased outdoor air flow above the minimum level since the actual outdoor air flow rate is also governed by other controls (e.g., mixed air set point tempeature, time of day economizer control, maximum humidity setpoint, etc.). This variable is set to 0 if conditions disable economizer operation or NoEconomizer (Economizer Control Type) is specified.

#### Air System Outdoor Air Heat Recovery Bypass Status []

This indicates if the controls have determined if the bypass mode for heat recovery is in effect or not.

#### Air System Outdoor Air Heat Recovery Bypass Heating Coil Activity Status []

Reports the operating status of any heating coil in the air loop. If the heating coil is active, the heat exchanger will be activated (no air bypassed) and the heating energy will be reduced or eliminated. While conditions may be favorable for economizer operation, it does not guarantee that the air-side economizer has increased outdoor air flow above the minimum level since the actual outdoor air flow rate is also governed by other controls (e.g., mixed air set point temperature, time of day economizer control, maximum humidity setpoint, etc.). This variable is set to 0 if conditions disable economizer operation. This output variable is only available when using Heat Recovery Bypass Control Type = BypassWhenOAFlowGreaterThanMinimum.

#### Air System Outdoor Air Heat Recovery Bypass Minimum Outdoor Air Mixed Air Temperature [C]

Reports the outdoor air mixer's mixed air node temperature at minimum outdoor air flow rate when the heat exchanger is disabled (off). This temperature is calculated as the return air temperature multiplied by the return air mass flow rate plus the mixer's inlet node temperature multipled by the minimum outdoor air flow rate. This quantity is then divided by the mixed air mass flow rate. If this temperature is less than the outdoor air mixer's mixed air node set point temperature, and the Heat Recovery Bypass Control Type = BypassWhenOAFlowGreaterThanMinimum, the outdoor air flow rate is set to the minimum.  This output variable is only available when using Heat Recvoery Bypass Control Type = BypassWhenOAFlowGreaterThanMinimum.

#### Air System Outdoor Air High Humidity Control Status []

Reports the average operating status of the controller's high humidity control over the reporting interval. The high humidity control status is set to 1 when the controller determines that a zone high humidity condition exists *according to the settings specified in the controller*. This variable is set to 0 if conditions disable high humidity control operation or High Humidity Control is specified as No.

#### Air System Outdoor Air Flow Fraction []

Reports the average actual outdoor air fraction for the outdoor air controller over the reporting interval.

#### Air System Outdoor Air Minimum Flow Fraction []

Reports the average minimum limit of the outdoor air fraction for the outdoor air controller over the reporting interval.

#### Air System Outdoor Air Mass Flow Rate [kg/s]

Reports the average outdoor air mass flow rate introduced by the outdoor air controller over the reporting interval.

#### Air System Mixed Air Mass Flow Rate [kg/s]

Reports the average mixed air mass flow rate of the HVAC air loop associated with this outdoor air controller over the reporting interval.

## Controller:MechanicalVentilation

This object is used in conjunction with an outdoor air controller (Ref. [Controller:OutdoorAir](#controlleroutdoorair), Field: Mechanical Ventilation Controller Name) to establish the minimum outdoor air flow rate provided by a mixed air box.

Ventilation standards provide guidance on appropriate levels of outdoor ventilation air required for acceptable indoor air quality. The Ventilation Rate Procedure (VRP) of ASHRAE Standard 62.1-2007/2010 (www.ashrae.org) requires outdoor ventilation rates to be determined based on the floor area of each occupied zone plus the number of people in each zone and considers the zone air distribution effectiveness and system ventilation efficiency. The outdoor air ventilation rate can be reset dynamically as operating conditions change (e.g., variations in occupancy). The [Controller:MechanicalVentilation](#controllermechanicalventilation) object implements the VRP for calculating these outdoor air ventilation requirements and resetting them based on varying occupancy levels and zone diversification. This is particularly useful for large air distribution systems that serve a number of different zone types with varying occupancy levels. This object can also be used to model the Indoor Air Quality Procedure (IAQP) as defined in Standard 62.1

### Inputs

#### The first five inputs for this object are the name, the availability schedule, the zone outdoor air method, the system outdoor air method, and the zone maximum outdoor air fraction. The next three input fields define the zone name (or zone list name), the design specification outdoor air object name, and the design specification zone air distribution object name to be applied to this zone (or zone list). The last three fields are extensible

#### *Field: Name*

The unique user assigned name for an instance of mechanical ventilation. Any other object referencing this mechanical ventilation object will use this name.

#### *Field: Availability* Schedule Name

The name of a schedule whose values are greater than 0 when mechanical ventilation, as calculated by this object, is desired. If the schedule's value is 0.0, then mechanical ventilation is not available and flow will not be requested. If the schedule's value is > 0.0 (usually 1 is used), mechanical ventilation is available. If this field is blank, the schedule has values of 1 for all time periods. This schedule is useful for purging the building of contaminants prior to occupancy (i.e., ventilation rate per unit floor area will be provided even if the occupancy is zero).

#### *Field: Demand Controlled Ventilation*

This field indicates whether the air loop is capable of doing demand controlled ventilation (DCV) to vary the amount of outdoor air based on actual number of occupants in spaces. Two choices: Yes and No. Default is No.

#### Field: System Outdoor Air  Method

The method used to calculate the system minimum outdoor air flow. Several choices are allowed: **ZoneSum**, **VentilationRateProcedure,** **IndoorAirQualityProcedure,  ProportionalControl,** and **IndoorAirQualityProcedureGenericContaminant**.  ZoneSum sums the outdoor air flows across all zones served by the system. VentilationRateProcedure (VRP) uses the multi-zone equations defined in 62.1-2007 to calculate the system outdoor air flow. VRP considers zone air distribution effectiveness and zone diversification of outdoor air fractions. IndoorAirQualityProcedure (IAQP) is the other procedure defined in ASHRAE Standard 62.1-2007 for calculate the amount of outdoor air necessary to maintain the levels of indoor air carbon dioxide at or below the setpoint defined in the [ZoneControl:ContaminantController](#zonecontrolcontaminantcontroller) object. Appendix A of the ASHRAE 62.1-2010 user's manual discusses another method for implementing CO~2~-based DCV in a single zone system. This method (Proportional Control) calculates the required outdoor air flow rate which varies in proportion to the percentage of the CO~2~ signal range. The IndoorAirQualityProcedure-GenericContaminant method calculates the amount of outdoor air necessary to maintain the levels of indoor air generic contaminant at or below the setpoint defined in the [ZoneControl:ContaminantController](#zonecontrolcontaminantcontroller) object.

> Note: When System Outdoor Air Method = IndoorAirQualityProcedure or IndoorAirQualityProcedureGenericContaminant is specified, only the [Zone](#zone) <x> Name fields are used. The other field inputs described below are not used.

#### *Field: Zone Maximum Outdoor Air Fraction*

This positive numeric input is the zone maximum outdoor air fraction. For VAV systems, when a zone requires outdoor air higher than the user specified [Zone](#zone) Maximum Outdoor Air Fraction, the zone supply air flow will be increased (if damper not fully open yet) to cap the outdoor air fraction at the maximum value. This allows the system level outdoor air flow to be reduced while the total supply air flow increases to meet zone outdoor air requirement. Valid values are from 0 to 1.0. Default is 1.0 which indicates zones can have 100% outdoor air maintaining backward compatibility. This inputs work for single and dual duct VAV systems.

#### *Field Set (Zone Name, Design Specification Outdoor Air Object Name,* and Design Specification Zone Air Distribution Object Name)

The following three fields are needed to define the parameters for the ventilation. This object is extensible by duplicating these three fields.

#### *Field:* Zone <x> Name

The zone name or zone list to apply the ventilation rates specified in the [DesignSpecification:OutdoorAir](#designspecificationoutdoorair) object.

#### *Field: Design Specification Outdoor Air Object Name <x>*

The name of the [DesignSpecification:OutdoorAir](#designspecificationoutdoorair) object, defining the amount of outdoor air, that applies to the zone or zone list. If this field is blank, the corresponding [DesignSpecification:OutdoorAir](#designspecificationoutdoorair) object for the zone will come from the [DesignSpecification:OutdoorAir](#designspecificationoutdoorair) object referenced by the [Sizing:Zone](#sizingzone) object for the same zone. If no such zone match is found, default values from the IDD will be used for the [DesignSpecification:OutdoorAir](#designspecificationoutdoorair) object.

#### *Field: Design Specification* Zone Air Distribution Object Name <x>

The name of the [DesignSpecification:ZoneAirDistribution](#designspecificationzoneairdistribution) object, defining the air distribution effectiveness and secondary recirculation air fraction, that applies to the zone or zone list. If this field is blank, the corresponding [DesignSpecification:ZoneAirDistribution](#designspecificationzoneairdistribution) object for the zone will come from the [DesignSpecification:ZoneAirDistribution](#designspecificationzoneairdistribution) object referenced by the [Sizing:Zone](#sizingzone) object for the same zone. If no such zone match is found, default values from the IDD will be used for the [DesignSpecification:ZoneAirDistribution](#designspecificationzoneairdistribution) object.

As described previously, the [Controller:MechanicalVentilation](#controllermechanicalventilation) object works in conjunction with [Controller:OutdoorAir](#controlleroutdoorair). As such, the minimum quantity of outdoor air delivered via the mixed air box will be the greater of:

- the minimum outdoor air flow rate calculated by the fields Minimum Outdoor Air Flow Rate, Minimum Limit and Minimum Outdoor Air Schedule Name in the associated [Controller:OutdoorAir](#controlleroutdoorair) object, or
- the outdoor air flow rate calculated by the VRP (details in the Engineering Reference) with inputs specified in this object and the referenced [DesignSpecification:OutdoorAir](#designspecificationoutdoorair) and [DesignSpecification:ZoneAirDistribution](#designspecificationzoneairdistribution) objects.

The actual outdoor air flow rate may be higher than the minimum if free cooling is available. Regardless, the outdoor air flow rate will not exceed the Maximum Outdoor Air Flow Rate specified in the associated [Controller:OutdoorAir](#controlleroutdoorair) object.

An example input for this object is shown below:

~~~~~~~~~~~~~~~~~~~~

      Controller:MechanicalVentilation,
        VentObject,               !- Name
        VentSchedule,             !- Availability Schedule Name
        VentilationRateProcedure, !- System Outdoor Air Method
        1.0,                      !- Zone Maximum Outdoor Air Fraction
        Resistive Zone,           !- Zone 1 Name
        DSOA1,                    !- Design Specification Outdoor Air Object Name 1
        DSZADO1,                  !- Design Specification Zone Air Distribution Object Name 1
        DCV Zone List,            !- Zone 2 Name
        DSO_ZList,                !- Design Specification Outdoor Air Object Name 2
        ;                         !- Design Specification Zone Air Distribution Object Name 2

      ZoneList,
        DCV Zone List,            !- Zone List Name
        East Zone,                !- Zone Name 1
        North Zone;               !- Zone Name 2
    DesignSpecification:OutdoorAir,
    DSOA1,                   !- Name
    SUM,                     !- Outdoor Air Method
    0.00236,                 !- Outdoor Air Flow per Person
    0.000305,                !- Outdoor Air Flow per Zone Floor Area
    0.0,                     !- Outdoor Air Flow per Zone
    0.0,                     !- Outdoor Air Flow Air Changes per Hour
    ;                        !- Outdoor Air Flow Rate Fraction Schedule Name

    DesignSpecification:ZoneAirDistribution,
        DSZADO1,                 !- Name
        1.2,                     !- Zone Air Distribution Effectiveness in Cooling Mode
        1.0,                     !- Zone Air Distribution Effectiveness in Heating Mode
        ,                        !- Zone Air Distribution Effectiveness Schedule Name
        0.3;                     !- Zone Secondary Recirculation Fraction
~~~~~~~~~~~~~~~~~~~~

## ZoneHVAC:EnergyRecoveryVentilator:Controller

This controller object is used exclusively by the stand alone energy recovery ventilator ([ZoneHVAC:EnergyRecoveryVentilator](#zonehvacenergyrecoveryventilator), see Figure 143). The purpose of this controller is to simulate economizer operation for the stand alone ERV and provide free cooling whenever possible or modify the outdoor air flow rate during high indoor humidity conditions. During economizer operation, if all of the limits are satisfied, the controller activates economizer mode (fully bypassing the fixed-plate air-to-air heat exchanger or stopping the rotation of a rotary heat exchanger). If any of the selected limits are exceeded, economizer operation is terminated. A time-of-day schedule may also be used to simulate a "push-button" type economizer controller.

In addition, the outdoor air flow rate may be modified and heat exchange suspended in response to high indoor relative humidities. When the indoor relative humidity exceeds the zone humidistat's relative humidity set point, high humidity control is activated. When activated, if high humidity control is based on the outdoor air humidity ratio and the outdoor humidity ratio is greater than the indoor humidity ratio, high humidity control is terminated.

![Schematic of the ZoneHVAC:EnergyRecoveryVentilator compound object](media/schematic-of-the-zonehvac.jpeg)


### Inputs

#### Field: Name

A unique user-assigned name for the stand alone ERV controller. Any reference to this controller by another object will use this name.

#### Field: Temperature High Limit

The input for this field is the outdoor air temperature high limit (°C) for economizer operation. If the outdoor air temperature is above this limit, economizer (free cooling) operation is terminated. No input (blank) in this field means that there is no outdoor air temperature high limit control.

#### Field: Temperature Low Limit

The input for this field is the outdoor air temperature low limit (°C) for economizer operation. If the outdoor air temperature is below this limit, economizer (free cooling) operation is terminated. No input (blank) in this field means that there is no outdoor air temperature low limit control.

#### Field: Enthalpy High Limit

The input for this field is the outdoor air enthalpy limit (in J/kg) for economizer operation. If the outdoor air enthalpy is above this value, economizer (free cooling) operation is terminated. No input (blank) in this field means that there is no outdoor air economizer limit control.

#### Field: Dewpoint Temperature Limit

Input for this field is the outdoor air dewpoint limit (°C) for economizer operation. If the outdoor air dewpoint temperature is above this value, the outdoor airflow rate will be set to the minimum. No input (blank) in this field means that there is no outdoor air dewpoint limit control. This limit applies to the conditions at the Actuated Node regardless of whether or not there are any other components on the outdoor air path upstream of the mixer.

#### Field: Electronic Enthalpy Limit Curve Name

Input for this field is the name of a quadratic or cubic curve which provides the maximum outdoor air humidity ratio (function of outdoor air dry-bulb temperature) for economizer operation. If the outdoor air humidity ratio is greater than the curve's maximum humidity ratio (evaluated at the outdoor air dry-bulb temperature), the outdoor air flow rate will be set to the minimum. This limit applies to the conditions at the Actuated Node regardless of whether or not there are any other components on the outdoor air path upstream of the mixer. No input (blank) in this field means that there is no electronic enthalpy limit control.

#### Field: Exhaust Air Temperature Limit

This input establishes whether or not there is a limit control on the exhaust air temperature. The choices are **ExhaustAirTemperatureLimit** or **NoExhaustAirTemperatureLimit**. If **ExhaustAirTemperatureLimit** is chosen, the controller deactivates economizer mode whenever the outdoor air temperature is greater than the exhaust air temperature. If **NoExhaustAirTemperatureLimit** is chosen, no limit check on the exhaust air temperature is performed.

#### Field: Exhaust Air Enthalpy Limit

This input establishes whether or not there is a limit control on the exhaust air enthalpy. The choices are **ExhaustAirEnthalpyLimit** or **NoExhaustAirEnthalpyLimit**. If **ExhaustAirEnthalpyLimit** is chosen, the controller deactivates economizer mode whenever the outdoor air enthalpy is greater than the exhaust air enthalpy. If **NoExhaustAirEnthalpyLimit** is chosen, no limit check on the exhaust air enthalpy is performed.

#### Field: Time of Day Economizer Flow Control Schedule Name

This alpha field is the name of a schedule which controls the change in air flow rate based on time-of-day. Schedule values equal to 0 disable this feature. Schedule values greater than 0 activate the economizer. Note that heat exchange between the air streams is suspended when the economizer is active. This schedule can be used with or without the high humidity control option. When an economizer is used in conjunction with the high humidity control option, high humidity control has priority.

#### Field: High Humidity Control Flag

This optional choice field establishes whether or not the supply and exhaust air flow rates are modified in response to high indoor relative humidity. Valid choices are Yes and No. If Yes is selected, the supply and exhaust air flow rates may be modified when the indoor relative humidity is above the humidstat set point. If No is selected, this option is disabled and the following three fields are not used. Note that heat exchange between the air streams is suspended during times when high humidity control is active. The default value is No.

#### Field: Humidistat Control Zone Name

This optional input defines the zone name where the humidistat is located. This is the same zone name used in the [Zone](#zone) Control:Humidistat object. This field is required when the High Humidity Control Flag is specified as Yes.

#### Field: High Humidity Outdoor Air Flow Ratio

This optional input is the ratio of the modified supply (outdoor) air flow rate to the supply air flow rate specified in the Energy Recovery Ventilator:Stand Alone ERV object. When the high humidity control algorithm determines that the supply air flow rate will be changed (i.e. increased or decreased), the operating supply air flow rate is equal to the supply air flow rate specified in the Energy Recovery Ventilator:Stand Alone ERV object multiplied by this ratio. The minimum value for this field is 0. This field is used to modify both the supply and exhasut air flow rates when high humidity control is active. The supply and exhasut air fan volumetric flow rates must be able to account for the increase in air flow when this input is greater than 1. The default value is 1.

#### Field: Control High Indoor Humidity based on Outdoor Humidity Ratio

This optional choice field determines if high humidity control is activated based on high indoor relative humidity alone or is activated only when the indoor relative humidity is above the humidstat set point *and* the outdoor humidity ratio is less than the indoor humidity ratio. Valid choices are Yes and No. If No is selected, high humidity control is active any time the zone humidistat senses a moisture load. If yes is selected, the model also verifies that the outdoor humidity ratio is less than the humidistat's zone air humidity ratio. This field is used only when the High Humidity Control Flag is specified as Yes. The default value is Yes.

Following is an example input for this stand alone ERV controller object:

~~~~~~~~~~~~~~~~~~~~

    ZoneHVAC:EnergyRecoveryVentilator:Controller,
        ERV OA Controller 1,            !- ERV controller name
        19.,                            !- Temperature high limit {C}
        14.,                            !- Temperature low limit {C}
        ,                               !- Enthalpy high limit {J/kg}
        15.55,                          !- dew point temperature limit (C)
        ElectronicEnthalpyCurveA,       !- electronic enthalpy limit curve name
        NoExhaustAirTemperatureLimit,   !- Exhaust air temperature limit
        NoExhaustAirEnthalpyLimit,      !- Exhaust air enthalpy limit
        OutsideAirFlowSchedule,         !- Time of Day Economizer Flow Control Schedule Name
        Yes,                            !- High Humidity Control Flag
        East Zone,                      !- Humidistat Control Zone Name
        1.2,                            !- High Humidity Outdoor air Flow Ratio
        Yes;              !- Control High Indoor Humidity based on Outdoor Humidity Ratio

    Curve:Cubic,
        ElectronicEnthalpyCurveA, !- Name
        0.01342704,               !- Coefficient1 Constant
       -0.00047892,               !- Coefficient2 x
        0.000053352,              !- Coefficient3 x**2
       -0.0000018103,             !- Coefficient4 x**3
        16.6,                     !- Minimum Value of x
        29.13;                    !- Maximum Value of x
~~~~~~~~~~~~~~~~~~~~