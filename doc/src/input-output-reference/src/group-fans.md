# Group – Fans

The following fans may be defined either in the air loop or as a zone equipment component: [Fan:ConstantVolume](#fanconstantvolume), [Fan:OnOff](#fanonoff), [Fan:VariableVolume](#fanvariablevolume), [Fan:ZoneExhaust](#fanzoneexhaust), and [FanPerformance:NightVentilation](#fanperformancenightventilation). The data that are common to these fan types include an identifying name, an availability schedule name, a total efficiency rating, a rated pressure rise, and inlet and outlet air node names. In the case of a variable volume fan, additional input includes parameters for modeling fan performance over a range of fan speeds. See the engineering documentation for the variable speed fan for a further description of what these coefficients represent. Commonly-used values for different variable volume systems are shown in the following table.

Table: Fan Coefficient Values

Type of Fan|Fan Coeff. 1|Fan Coeff. 2|Fan Coeff. 3|Fan Coeff. 4|Fan Coeff. 5
-----------|------------|------------|------------|------------|------------
Inlet Vane Dampers|0.35071223|0.30850535|-0.54137364|0.87198823|0.000
Discharge Dampers|0.37073425|0.97250253|-0.34240761|0.000|0.000
Var. Speed Motor|0.0015302446|0.0052080574|1.1086242|-0.11635563|0.000

## Fan:ConstantVolume

This object models a constant air volume fan that is intended to operate continuously based on a time schedule. This fan will not cycle on and off based on cooling/heating load or other control signals (Ref: [Fan:OnOff](#fanonoff)).

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a [Fan:ConstantVolume](#fanconstantvolume). Any reference to this fan by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the fan can run during a given time period. A schedule value of 0 indicates that the fan is off for that time period. A schedule value greater than 0 indicates that the fan can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods. Applicable availability managers (ref. [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)) may override this schedule by forcing the fan to be on or off.

#### Field: Fan Total Efficiency

This value is the overall efficiency of the fan, i.e., the ratio of the power delivered to the fluid to the electrical input power. It is the product of the motor efficiency and the impeller efficiency. The motor efficiency is the power delivered to the shaft divided by the electrical power input to the motor. The impeller efficiency is power delivered to the fluid (air) divided by the shaft power. The power delivered to the fluid is the mass flow rate of the air multiplied by the pressure rise divided by the air density. This input value must be between 0 and 1.The default is 0.7.

#### Field: Pressure Rise

The pressure rise in Pascals at full flow and standard (sea level) conditions (20°C and 101325 Pa).

#### Field: Maximum Flow Rate

The full load air volumetric flow rate (m^3^/sec) at standard temperature and pressure (dry air at 20°C drybulb). The program does use local barometric pressure to account for altitude using equation for "standard atmospheric" pressure on p 6.1 of the ASHRAE 1997 HOF (SI edition) to initialize the air systems being simulated.

p=101325\*(1-2.25577E-05\*Z)\*\*5.2559

where p=pressure in Pa and Z=altitude in m

#### Field: Motor Efficiency

The shaft power divided by the electrical power consumed. Must be between 0 and 1. The default is 0.9.

#### Field: Motor In Airstream Fraction

The fraction of the motor heat that is added to the air stream. A value of 0 means that the motor is completely outside the air stream. A value of 1 means that all of the motor heat loss will go into the air stream and act to cause a temperature rise. Must be between 0 and 1. The default is 1.0.

#### Field: Air Inlet Node Name

The name of the HVAC system node which supplies the inlet air conditions to the fan.

#### Field: Air Outlet Node Name

The name of the HVAC system node to which the fan sends its outlet air.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Central System", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the fan will be assigned to the "General" end-use subcategory.

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Fan Electric Power[W]
    HVAC,Average,Fan Rise in Air Temperature [deltaC]
    HVAC,Sum,Fan Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Fan Electric Power [W]

This output field contains the average electricity consumption rate for the fan in Watts for the timestep being reported.

#### Fan Rise in Air Temperature [deltaC]

This output field contains the average rise in air temperature across the fan (outlet air temperature minus inlet air temperature) in degrees Celsius for the timestep being reported.

#### Fan Electric Energy [J]

This output field contains the electricity consumption of the fan in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key= Fans, Group Key= System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

## Fan:OnOff

This object models a constant air volume fan that is intended to cycle on and off in tandem with a cooling or heating system (i.e., AUTO fan control mode). The fan can also operate continuously like [Fan:ConstantVolume](#fanconstantvolume). If modeling continuous operation and this object is used as part of a system that utilizes [Coil:Heating:Gas](#coilheatinggas), [Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed) or [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed), the user should confirm proper air flow rates (coil and fan max flows are equal) and that the coil part-load fraction correlation(s) are appropriate (e.g., part-load fraction is less than or equal to 1 for all values of coil part-load ratio). If modeling multi-speed fan operation, this object must be used as part of a compound object that allows multiple fan speeds (e.g., [AirLoopHVAC:Unitary:Furnace:HeatCool](#airloophvacunitaryfurnaceheatcool), [ZoneHVAC:PackagedTerminalAirConditioner](#zonehvacpackagedterminalairconditioner), etc.). In this case, the ratio of the compound object air flow rate to the fan's maximum air flow rate is used to determine the power at alternate fan speeds. The optional input for Fan Power Ratio Function of Speed Ratio Curve Name must be entered to model multi-speed fan operation. An optional fan total efficiency ratio curve is also available to model efficiency differences at alternate fan speeds.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a [Fan:OnOff](#fanonoff). Any reference to this fan by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the fan can run during a given time period. A schedule value of 0 indicates that the fan is off for that time period. A schedule value greater than 0 indicates that the fan can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods. Applicable availability managers (ref. [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)) may override this schedule by forcing the fan to be on or off.

#### Field: Fan Total Efficiency

This value is the overall efficiency of the fan, i.e., the ratio of the power delivered to the fluid to the electrical input power. It is the product of the motor efficiency and the impeller efficiency. The motor efficiency is the power delivered to the shaft divided by the electrical power input to the motor. The impeller efficiency is power delivered to the fluid (air) divided by the shaft power. The power delivered to the fluid is the mass flow rate of the air multiplied by the pressure rise divided by the air density. This input value must be between 0 and 1.The default is 0.6.

#### Field: Pressure Rise

The pressure rise in Pascals at full flow and standard (sea level) conditions (20°C and 101325 Pa).

#### Field: Maximum Flow Rate

The full load air volumetric flow rate (m^3^/sec) at standard temperature and pressure (dry air at 20°C drybulb). The program does use local barometric pressure to account for altitude using equation for "standard atmospheric" pressure on p 6.1 of the ASHRAE 1997 HOF (SI edition) to initialize the air systems being simulated.

p=101325\*(1-2.25577E-05\*Z)\*\*5.2559

where p=pressure in Pa and Z=altitude in m

#### Field: Motor Efficiency

The shaft power divided by the electrical power consumed. Must be between 0 and 1. The default is 0.8.

#### Field: Motor In Airstream Fraction

The fraction of the motor heat that is added to the air stream. A value of 0 means that the motor is completely outside the air stream. A value of 1 means that all of the motor heat loss will go into the air stream and act to cause a temperature rise. Must be between 0 and 1. The default is 1.0.

#### Field: Air Inlet Node Name

The name of the HVAC system node which supplies the inlet air conditions to the fan.

#### Field: Air Outlet Node Name

The name of the HVAC system node to which the fan sends its outlet air.

#### Field: Fan Power Ratio Function of Speed Ratio Curve Name

Enter the name of an exponent performance curve. This optional alpha field must be used to simulate multi-speed fan motors. This curve represents the ratio of actual fan power to rated fan power when a change in fan speed occurs. Leave this field blank when simulating constant-speed fan motors.

#### Field: Fan Efficiency Ratio Function of Speed Ratio Curve Name

Enter the name of a quadratic or cubic performance curve. This optional alpha field is used to simulate multi-speed fan motors. This curve represents the ratio of actual fan total efficiency to rated fan total efficiency when a change in fan speed occurs. Leave this field blank when simulating constant-speed fan motors.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Main Fans", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the fan will be assigned to the "General" end-use subcategory.

Following is an example input for an OnOff Fan.

~~~~~~~~~~~~~~~~~~~~

    Fan:OnOff,
          Supply Fan 1,          ! Fan Name
          FanAndCoilAvailSched,  ! Fan Schedule
          0.7,                   ! Fan Total Efficiency
          600.0,                 ! Delta Pressure [N/M^2]
          1.3,                   ! Max Flow Rate  [m^3/Sec]
          0.9,                   ! Motor Efficiency
          1.0,                   ! Motor in Airstream Fraction (1.0 means motor in air stream)
          Air Loop Inlet Node, Cooling Coil Air Inlet Node;  !Inlet Node, Outlet Node
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Fan Electric Power[W]
    HVAC,Average,Fan Rise in Air Temperature [deltaC]
    HVAC,Sum,Fan Electric Energy [J]
    HVAC,Average,Fan Runtime Fraction []
~~~~~~~~~~~~~~~~~~~~

#### Fan Electric Power [W]

This output field contains the average electricity consumption rate for the fan in Watts for the timestep being reported.

#### Fan Rise in Air Temperature [deltaC]

This output field contains the average rise in air temperature across the fan (outlet air temperature minus inlet air temperature) in degrees Celsius for the timestep being reported.

#### Fan Electric Energy [J]

This output field contains the electricity consumption of the fan in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key= Fans, Group Key= System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Fan Runtime Fraction []

This output field contains the fraction of time that this fan operated for the timestep being reported.

## Fan:VariableVolume

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a [Fan:VariableVolume](#fanvariablevolume). Any reference to this fan by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the fan can run during a given time period. A schedule value of 0 indicates that the fan is off for that time period. A schedule value greater than 0 indicates that the fan can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods. Applicable availability managers (ref. [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)) may override this schedule by forcing the fan to be on or off.

#### Field: Fan Total Efficiency

This value is the overall efficiency of the fan, i.e., the ratio of the power delivered to the fluid to the electrical input power. It is the product of the motor efficiency and the impeller efficiency. The motor efficiency is the power delivered to the shaft divided by the electrical power input to the motor. The impeller efficiency is power delivered to the fluid (air) divided by the shaft power. The power delivered to the fluid is the mass flow rate of the air multiplied by the pressure rise divided by the air density. This input value must be between 0 and 1. The default is 0.7.

#### Field: Pressure Rise

The pressure rise in Pascals at full flow and standard (sea level) conditions (20°C and 101325 Pa).

#### Field: Maximum Flow Rate

The full load air volumetric flow rate (m^3^/sec) at standard temperature and pressure (dry air at 20°C drybulb). The program does use local barometric pressure to account for altitude using equation for "standard atmospheric" pressure on p 6.1 of the ASHRAE 1997 HOF (SI edition) to initialize the air systems being simulated.

p=101325\*(1-2.25577E-05\*Z)\*\*5.2559

where p=pressure in Pa and Z=altitude in m

#### Field: Fan Power Minimum Flow Rate Input Method

This field is a key/choice field that tells which of the next two fields is filled and is descriptive of how the minimum flow rate is specified for calculating the fan power.  The key/choices are:

- Fraction
- With this choice, the fan power will be calculated using the value specified in the Fan Power Minimum Flow Fraction field.  (The Fan Power Minimum Flow Fraction field should be filled.)
- FixedFlowRate
- With this choice, the fan power will be calculated using the value specified in the Fan Power Minimum Air Flow Rate field.  (The Fan Power Minimum Air Flow Rate field should be filled.)
- The default is Fraction.

#### Field: Fan Power Minimum Flow Fraction

The minimum air volumetric flow rate for fan power, specified as a fraction of maximum system air flow rate. Must be between 0 and 1. Note that this field is only used to calculate the fan power. This field does not enforce the system air flow rate during simulation. The default is 0.25.

#### Field: Fan Power Minimum Air Flow Rate

The minimum air volumetric flow rate for fan power, specified as a constant minimum air flow rate (m3/sec). Note that this field is only used to calculate the fan power. This field does not enforce the system air flow rate during simulation.

#### Field: Motor Efficiency

The shaft power divided by the electrical power consumed. Must be between 0 and 1. The default is 0.9.

#### Field: Motor In Airstream Fraction

The fraction of the motor heat that is added to the air stream. A value of 0 means that the motor is completely outside the air stream. A value of 1 means that all of the motor heat loss will go into the air stream and act to cause a temperature rise. Must be between 0 and 1. The default is 1.0.

#### Field: Fan Power Coefficient 1

The constant coefficient (C~1~) in a fourth order polynomial curve giving the fraction of full load power (PLF) as a function of flow fraction (FF). Flow fraction is the air mass flow rate divided by the maximum air mass flow rate. The curve is:

PLF = C~1~ + C~2~^.^ FF + C~3~^.^ FF^2 +^C~4~^.^ FF^3^ + C~5~^.^ FF^4^

#### Field: Fan Power Coefficient 2

The linear coefficient (C~2~) in a fourth order polynomial curve giving the fraction of full load power (PLF) as a function of flow fraction (FF). Flow fraction is the air mass flow rate divided by the maximum air mass flow rate. The curve is:

PLF = C~1~ + C~2~^.^ FF + C~3~^.^ FF^2 +^C~4~^.^ FF^3^ + C~5~^.^ FF^4^

#### Field: Fan Power Coefficient 3

The quadratic coefficient (C~3~) in a fourth order polynomial curve giving the fraction of full load power (PLF) as a function of flow fraction (FF). Flow fraction is the air mass flow rate divided by the maximum air mass flow rate. The curve is:

PLF = C~1~ + C~2~^.^ FF + C~3~^.^ FF^2 +^C~4~^.^ FF^3^ + C~5~^.^ FF^4^

#### Field: Fan Power Coefficient 4

The cubic coefficient (C~1~) in a fourth order polynomial curve giving the fraction of full load power (PLF) as a function of flow fraction (FF). Flow fraction is the air mass flow rate divided by the maximum air mass flow rate. The curve is:

PLF = C~1~ + C~2~^.^ FF + C~3~^.^ FF^2 +^C~4~^.^ FF^3^ + C~5~^.^ FF^4^

#### Field: Fan Power Coefficient 5

The coefficient C~5~ in a fourth order polynomial curve giving the fraction of full load power (PLF) as a function of flow fraction (FF). Flow fraction is the air mass flow rate divided by the maximum air mass flow rate. The curve is:

PLF = C~1~ + C~2~^.^ FF + C~3~^.^ FF^2 +^C~4~^.^ FF^3^ + C~5~^.^ FF^4^

#### Field: Air Inlet Node Name

The name of the HVAC system node which supplies the inlet air conditions to the fan.

#### Field: Air Outlet Node Name

The name of the HVAC system node to which the fan sends its outlet air.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Central System", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the fan will be assigned to the "General" end-use subcategory.

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Fan Electric Power[W]
    HVAC,Average,Fan Rise in Air Temperature [deltaC]
    HVAC,Sum,Fan Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Fan Electric Power [W]

This output field contains the average electricity consumption rate for the fan in Watts for the timestep being reported.

#### Fan Rise in Air Temperature [deltaC]

This output field contains the average rise in air temperature across the fan (outlet air temperature minus inlet air temperature) in degrees Celsius for the timestep being reported.

#### Fan Electric Energy [J]

This output field contains the electricity consumption of the fan in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key= Fans, Group Key= System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

## Fan:ZoneExhaust

This fan object differs from the other fans in that it stands on its own in a zone rather than serving as one part of an HVAC air system.  This object appears directly in a [ZoneHVAC:EquipmentList](#zonehvacequipmentlist) object and all the controls are contained within the fan object.  The zone exhaust fan model provides a way to include the electrical power used by the fan.  It can also impact air flows in central air handlers by decreasing the flow of return air and sometimes increasing the outdoor air flow rate.

There are several control options available for the exhaust fan including:  an on/off availability schedule, interaction with system availability managers, minimum zone air temperature control limits and a variable flow fraction schedule.

The way in which the exhaust fan impacts central air system can be controlled by declaring what portion of the flow has been "balanced" by simple airflow from infiltration, ventilation, or mixing.  However it is important to note that presence of an exhaust fan does not by itself drive any simple airflow such as infiltration, ventilation, or zone mixing.  There is no comprehensive automatic mass balancing between air system flows, exhaust flows, and the separate simple airflows.  For balancing, the simple airflows need to have their own input objects that need to be coordinated with the exhaust fan.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a [Fan:ZoneExhaust](#fanzoneexhaust). Any reference to this fan by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the fan can run during a given time period. A schedule value of 0 indicates that the fan is off for that time period. A schedule value greater than 0 indicates that the fan can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods. Applicable availability managers (ref. [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)) may override this schedule by forcing the fan to be on or off.

#### Field: Fan Total Efficiency

This value is the overall efficiency of the fan, i.e., the ratio of the power delivered to the fluid to the electrical input power. It is the product of the motor efficiency and the impeller efficiency. The motor efficiency is the power delivered to the shaft divided by the electrical power input to the motor. The impeller efficiency is power delivered to the fluid (air) divided by the shaft power. The power delivered to the fluid is the mass flow rate of the air multiplied by the pressure rise divided by the air density. This input value must be between 0 and 1. The default is 0.6.

#### Field: Pressure Rise

The pressure rise in Pascals at full flow and standard (sea level) conditions (20°C and 101325 Pa).

#### Field: Maximum Flow Rate

The full load air volumetric flow rate (m^3^/sec) at standard temperature and pressure (dry air at 20°C drybulb). The program does use local barometric pressure to account for altitude using equation for "standard atmospheric" pressure on p 6.1 of the ASHRAE 1997 HOF (SI edition) to initialize the air systems being simulated.

p=101325\*(1-2.25577E-05\*Z)\*\*5.2559

where p=pressure in Pa and Z=altitude in m

#### Field: Air Inlet Node Name

The name of the HVAC system node which supplies the inlet air conditions to the fan.  This node should be listed as a zone exhaust node in an associated [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) object.

#### Field: Air Outlet Node Name

The name of the HVAC system node to which the fan sends its outlet air.

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Kitchen Exhaust", "Fume Hoods", etc. A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the fan will be assigned to the "General" end-use subcategory.

#### Field: Flow Fraction Schedule Name

This field is optional.  If it is not used then the fan operates at the maximum flow rate.  If a schedule is input here, then it should contain fractional values between 0.0 and 1.0, inclusive.  The flow rate that the fan operates will be this fraction times the maximum flow rate.  This allows a variable speed exhaust fan to be modeled according to a schedule.

#### Field: System Availability Manager Coupling Mode

This field is optional. If if is not used then the exhaust fan is assumed to be integrated with the central air handler's system availability manager.  This field can be used to control if the exhaust fan should operate independently or not.  For example, when a night cycle availability manager turns on the central air system for freeze protection, this field can be used to control if the zone exhaust fans should also run at the same time or not.  The key choice "Coupled" indicates that the exhaust fan should be integrated with the system availability manager so that the fan runs when the air system is forced to run.   The key choice "Decoupled" indicates that the exhaust fan should operate on its own and ignore the system availability manager's requests so that the exhaust fan can remain off when the air system runs.  The default is Coupled.

#### Field: Minimum Zone Temperature Limit Schedule Name

This field is optional. If it is not used then there will be no temperature-related control over the operation of the exhaust fan.  If the field is used, then enter the name of a schedule with values for zone temperature values (°C).  The fan's control will be based on a comparison between the current zone air temperature and the schedule values.  If the zone is warmer than the scheduled limit, then the fan will operate.  When balancing with simple ventilation, this feature can be used to coordinate exhaust fan operation with ZoneVentilation:\* controls for minimum indoor temperature.

#### Field: Balanced Exhaust Fraction Schedule Name

This field is optional.  If it is not used, then all the exhaust air flow is assumed to be unbalanced by any simple airflows, such as infiltration, ventilation, or zone mixing.  Unbalanced exhaust is then modeled as being provided by the outdoor air system in the central air system. The modeling of unbalanced will reduce the flow rates at the zone's return air node by the flow rate that is being exhausted and will insure that the outdoor air flow rate is sufficient to serve the exhaust.  If this field is used, then enter the name of a schedule with fractional values between 0.0 and 1.0, inclusive.  This fraction is applied to the exhaust fan flow rate and the model tracks the portion of the exhaust that is "balanced."  Balanced exhaust is then modeled as being provided by simple airflows and does not impact the central air system return air or outdoor air flow rates.  For example, if a kitchen zone with an exhaust fan is designed to draw half of its make up air from a neighboring dining room and the other half from the outdoor air system, then a schedule value of 0.5 could be used here.

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Fan Electric Power [W]
    HVAC,Average,Fan Rise in Air Temperature[deltaC]
    HVAC,Sum,Fan Electric Energy [J]
    HVAC,Average,Fan Unbalanced Air Mass Flow Rate [kg/s]
    HVAC,Average,Fan Balanced Air Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

#### Fan Electric Power [W]

This output field contains the average electricity consumption rate for the fan in Watts for the timestep being reported.

#### Fan Rise in Air Temperature [deltaC]

This output field contains the average rise in air temperature across the fan (outlet air temperature minus inlet air temperature) in degrees Celsius for the timestep being reported.

#### Fan Electric Energy [J]

This output field contains the electricity consumption of the fan in Joules for the timestep being reported. This output is also added to an output meter with Resource Type = Electricity, End Use Key= Fans, Group Key= System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Fan Unbalanced Air Mass Flow Rate [kg/s]

#### Fan Balanced Air Mass Flow Rate [kg/s]

These two output variables are available when the exhaust fan uses the input field called Balanced Exhaust Fraction Schedule Name.  The balanced air flow is the result of the current flow rate times the balance fraction.  The unbalanced air flow is the difference between the current flow rate and the balanced flow rate.  These outputs are the resulting flow rates in kg/s.

Examples of [Fan:ConstantVolume](#fanconstantvolume), [Fan:VariableVolume](#fanvariablevolume), [Fan:ZoneExhaust](#fanzoneexhaust), and , [Fan:OnOff](#fanonoff),  fans in an IDF are:

~~~~~~~~~~~~~~~~~~~~

    Fan:ConstantVolume,
          Supply Fan 1,  !- Name
          FanAndCoilAvailSched,        !- Availability Schedule Name
          0.7,           !- Fan Total Efficiency
          600.0,         !- Pressure Rise {Pa}
          1.3,           !- Maximum Flow Rate {m3/sec}
          0.9,           !- Motor Efficiency
          1.0,           !- Motor in Airstream Fraction
          Air Loop Inlet Node, Cooling Coil Air Inlet Node;  !- Air Inlet Node Name, Air Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Fan:VariableVolume,
          Var Vol Supply Fan 1,  !- Name
          FanAndCoilAvailSched,  !- Availability Schedule Name
          0.7,           !- Fan Total Efficiency
          600.0,         !- Pressure Rise {Pa}
          1.3,           !- Maximum Flow Rate {m3/s}
          0.20,          !- Minimum Flow Rate {m3/s}
          0.9,           !- Motor Efficiency
          1.0,           !- Motor in Airstream Fraction
          0.35071223,    !- Fan Coefficient 1
          0.30850535,    !- Fan Coefficient 2
         -0.54137364,    !- Fan Coefficient 3
          0.87198823,    !- Fan Coefficient 4
          0.000,         !- Fan Coefficient 5
          Air Loop Inlet Node, Cooling Coil Air Inlet Node;  !- Air Inlet Node Name, Air Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Fan:ZoneExhaust,
      Zone 2 Exhaust Fan,              !- Name
      FanAndCoilAvailSched,            !- Availability Schedule Name
      0.6,                             !- Fan Total Efficiency
      125,                             !- Pressure Rise {Pa}
      0.1,                             !- Maximum Flow Rate {m3/s}
      Zone 2 Exhaust Node,             !- Air Inlet Node Name
      Zone 2 Exhaust Fan Outlet Node,  !- Air Outlet Node Name
      Kitchen Exhaust;                 !- End-Use Subcategory
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Fan:OnOff,
        AHU 1 Supply Fan,        !- Name
        FanAvailSched,           !- Availability Schedule Name
        0.7,                     !- Fan Total Efficiency
        600.0,                   !- Pressure Rise {Pa}
        2.0,                     !- Maximum Flow Rate {m3/s}
        0.9,                     !- Motor Efficiency
        1.0,                     !- Motor in Airstream Fraction
        AHU 1 Air Loop Inlet,    !- Air Inlet Node Name
        AHU 1 Supply Fan Outlet; !- Air Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

## FanPerformance:NightVentilation

This object is used for specifying an alternate set of performance parameters for a fan. These alternate parameters are used when a system manager (such as *AvailabilityManager:NightVentilation*) sets a specified flow rate for a central forced air system. At this time, it can be used with [Fan:ConstantVolume](#fanconstantvolume), [Fan:VariableVolume](#fanvariablevolume), [Fan:ZoneExhaust](#fanzoneexhaust), and , [Fan:OnOff](#fanonoff) fans, but not with [Fan:ComponentModel](#fancomponentmodel) fans. The fan model checks whether a fixed flow rate has been set; if it has the fan model will use these alternate performance parameters. Note that it is assumed that the fan will run at a fixed speed in the alternate mode. The inputs needed by this object are the fan name, fan total efficiency, pressure rise, flow rate, motor efficiency, and motor in airstream fraction.

### Inputs

#### Field: Fan Name

This is the name of a fan defined elsewhere in the input file. The night vent performance parameters will be applied to the named fan when a system manager has set the air system flow rate.

#### Field: Fan Total Efficiency

This value is the overall efficiency of the fan, i.e., the ratio of the power delivered to the fluid to the electrical input power. It is the product of the motor efficiency and the impeller efficiency. The motor efficiency is the power delivered to the shaft divided by the electrical power input to the motor. The impeller efficiency is power delivered to the fluid (air) divided by the shaft power. The power delivered to the fluid is the mass flow rate of the air multiplied by the pressure rise divided by the air density. This input value must be between 0 and 1. This is a required field with no default.

#### Field: Pressure Rise

The pressure rise in Pascals at full flow and standard (sea level) conditions (20°C and 101325 Pa).

#### Field: Maximum Flow Rate

The design volumetric flow rate of the fan (m^3^/sec) at standard conditions. This input is not currently used by the night ventilation manager. The flow rate during night ventilation is specified using the SystemAvailabilityManager:NightVentilation "Night Venting Flow Fraction" field. This fraction is multiplied times the fan object's design flow rate.

#### Field: Motor Efficiency

The shaft power divided by the electrical power consumed. Must be between 0 and 1. This is a required field with no default.

#### Field: Motor in Airstream Fraction

The fraction of the motor heat that is added to the air stream. A value of 0 means that the motor is completely outside the air stream. A value of 1 means that all of the motor heat loss will go into the air stream and act to cause a temperature rise. Must be between 0 and 1. The default is 1.0.

An example of use in an IDF:

~~~~~~~~~~~~~~~~~~~~

      Fan:VariableVolume,
        Supply Fan 1,            !- Name
        FanAvailSched,           !- Availability Schedule Name
        0.7,                     !- Fan Efficiency
        600.0,                   !- Pressure Rise {Pa}
        autosize,                !- Maximum Flow Rate {m3/s}
        autosize,                !- Minimum Flow Rate {m3/s}
        0.9,                     !- Motor Efficiency
        1.0,                     !- Motor In Airstream Fraction
        0.35071223,              !- Fan Coefficient 1
        0.30850535,              !- Fan Coefficient 2
        -0.54137364,             !- Fan Coefficient 3
        0.87198823,              !- Fan Coefficient 4
        0.000,                   !- Fan Coefficient 5
        Main Heating Coil 1 Outlet Node,  !- Air Inlet Node Name
        VAV Sys 1 Outlet Node;   !- Air Outlet Node Name

      FanPerformance:NightVentilation,
        Supply Fan 1,            !- Fan Name
        0.7,                     !- Fan Total Efficiency
        67.0,                    !- Pressure Rise {Pa}
        autosize,                !- Maximum Flow Rate {m3/s}
        0.9,                     !- Motor Efficiency
        1.0;                     !- Motor in Airstream Fraction
~~~~~~~~~~~~~~~~~~~~

## Fan:ComponentModel

The [Fan:ComponentModel](#fancomponentmodel) fan is a more detailed fan type that can be defined in the air loop for central constant-air-volume (CAV) and variable-air-volume (VAV) systems. It includes inputs that describe the air-distribution system as well as the fan, its drive belt (if used), its motor, and its variable-frequency-drive (if used). See the engineering documentation for further descriptions about the inputs for this fan type.

### Inputs

#### Field: Name

The required unique user-assigned alpha name for an instance of a [Fan:ComponentModel](#fancomponentmodel). Any reference to this fan by another object will use this name.

#### Field: Air Inlet Node Name

The required alpha name of the HVAC system node which supplies the inlet air conditions to the fan.

#### Field: Air Outlet Node Name

The required alpha name of the HVAC system node to which the fan sends its outlet air.

#### Field: Availability Schedule Name

The required alpha name of the schedule (ref: Schedule) that denotes whether the fan can run during a given time period. A schedule value of 0 indicates that the fan is off for that time period. A schedule value greater than 0 indicates that the fan can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods. Applicable availability managers (ref. [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)) may override this schedule by forcing the fan to be on or off.

#### Field: Maximum Flow Rate

The full-load volumetric airflow (m^3^/sec) through the fan at standard temperature and pressure (dry air at 20°C dry-bulb). To initialize the air systems being simulated, the program uses local barometric pressure adjusted for altitude, based on the equation for "standard atmospheric" pressure on p.6.1 of the 1997 ASHRAE Handbook of Fundamentals (SI edition):

p=101325 \* (1 - 2.25577E-05 \* Z)\*\*5.2559

where p = pressure in Pa and Z = altitude in m. Can be autosized.

Specified or autosized maximum airflow rate (including effects of scaling by Field: Fan Sizing Factor) along with corresponding fan static pressure rise and fan shaft power are reported in the .eio file as, respectively, Design Fan Airflow [m3/s], Design Fan Static Pressure Rise [Pa], and Design Fan Shaft Power [W].

#### Field: Minimum Flow Rate

The minimum volumetric airflow (m^3^/sec) through the fan at standard temperature and pressure (see Maximum Flow Rate field above for condition details). Can be autosized.

#### Field: Fan Sizing Factor

The numeric dimensionless factor (*F~fan~*) used to multiply the specified or autosized full-load volumetric airflow (see Maximum Flow Rate field above for details) for fan sizing. If specified, minimum value is 1.0. Default is 1.0 if field is blank.

#### Field: Fan Wheel Diameter

The required numeric outer diameter of the fan wheel (*D~fan~*, m). This value is determined from manufacturer's data. In general, larger diameter fans have higher maximum efficiency than smaller diameter fans of the same type (Ref: AMCA Standard 205-10: Energy Efficiency Classification for Fans). Must be greater than zero.

#### Field: Fan Outlet Area

The required numeric outlet area of the fan (*A~fan,out~*, m^2^). This value is determined from manufacturer's data. It is used to convert fan total pressure rise to fan static pressure rise. Fan static pressure rise is the fan total pressure rise minus the fan outlet velocity pressure; it is not the difference between fan outlet and inlet static pressures (Ref: ANSI/AMCA Standard 210-07, ANSI/ASHRAE Standard 51-07: Laboratory Methods of Testing Fans for Certified Aerodynamic Performance Rating). Must be greater than zero.

#### Field: Maximum Fan Static Efficiency

The required numeric maximum ratio (*η~fan,max~*) between the power delivered to the air (*H~air~*, W) and the fan shaft input power (*H~fan~*, W). For this parameter, *H~air~* is the volumetric airflow through the fan multiplied by the fan static pressure rise. Maximum fan static efficiency is determined from analyses of manufacturer's data using:

![](media/image388.png)\


where Δ*P~fan~* is fan static pressure rise (Pa) and *Q~fan~* is airflow through the fan (m^3^/sec). Typically, "do not select" curves on fan performance maps of pressure rise versus flow correspond to or are near maximum efficiency. Must be greater than zero and less than or equal to 1.0.

Calculated fan static efficiency at design flow condition (including part-load effects of oversized fan) is reported in the .eio file as Design Fan Efficiency [-].

#### Field: Euler Number at Maximum Fan Static Efficiency

The required numeric Euler number (*Eu~max~*), which is also called the throttling or pressure coefficient, and is the ratio of pressure forces to inertial forces. The Euler number is determined from analyses of manufacturer's data using:

![](media/image389.png)\


where Δ*P~fan~* is fan static pressure rise (Pa; see *Fan Pressure Rise Curve Name* field), *D~fan~* is wheel diameter (m), *ρ* is the manufacturer's reference air density (kg/m^3^), and *Q~fan~* is airflow through the fan (m^3^/sec). *Eu~max~* is calculated using any pair of pressure rise and airflow values that correspond with maximum fan static efficiency for the specified fan. Must be greater than zero.

#### Field: Maximum Dimensionless Fan Airflow

The required numeric maximum dimensionless airflow (*φ~max~*) through the fan, which corresponds to the maximum ratio between the airflow through the fan (*Q~fan~*, m^3^/sec) and the fan shaft rotational speed (*ω~fan~*, rpm) for the specified fan wheel diameter (*D~fan~*, m). *φ~max~* is determined from manufacturer's data using:

![](media/image390.png)\


*φ~max~* occurs at minimum *Eu*, which corresponds to maximum speed (high flow) with zero pressure rise. The factor (*30/π*) converts revolutions per minute (rpm) to rad/s. Must be greater than zero.

#### Field: Motor Fan Pulley Ratio

The numeric dimensionless ratio of the motor pulley diameter to the fan pulley diameter (*D~motor,pulley~ / D~fan,pulley~*). If specified, must be greater than zero. This ratio can be adjusted to account for belt slip if the fractional slip is known (multiply the drive ratio with no slip by 1+s, where s is the belt fractional slip). Default is 1.0 if field is blank (leave blank if no belt; i.e., direct drive). Can be autosized (assumes no slip).

Specified or autosized motor/fan pulley diameter ratio is reported in the .eio file as Drive Ratio [-]. Autosized ratio is based on fan speed in revolutions per minute (rpm), calculated at design flow condition, divided by Field: Motor Maximum Speed.

#### Field: Belt Maximum Torque

The required numeric maximum output torque capacity of the fan drive belt (*τ~belt,max~*, N·m). If specified, must be greater than zero. Can be autosized. Use autosize if no belt (i.e., direct drive).

Specified or autosized belt maximum output torque (including effects of scaling by Field: Belt Sizing Factor) is reported in the .eio file as Design Belt Output Torque [N·m]. Also, calculated maximum belt efficiency corresponding to Design Fan Shaft Power, along with belt efficiency at design flow condition (including part-load effects of oversized belt), are reported in the .eio file as, respectively, Maximum Belt Efficiency [-] and Design Belt Efficiency [-].

#### Field: Belt Sizing Factor

The numeric dimensionless factor (*F~belt~*) used to multiply the specified or autosized fan shaft maximum output torque (*τ~belt,max~*). If specified, minimum value is 1.0. Default is 1.0 if field is blank.

#### Field: Belt Fractional Torque Transition

The numeric transition point (*x~belt,trans~*) between performance curves for Regions 1 and 2 for the drive belt normalized part-load efficiency. Must be between 0.0 and 1.0. Default is 0.167 (corresponds to generic V-belt) if field is blank.

#### Field: Motor Maximum Speed

The required numeric maximum rotational speed of the fan motor shaft (*ω~motor,max~*) in revolutions per minute (rpm). Typical values for motors supplied by 60 Hz power are near 900, 1200, 1800, and 3600 rpm. Must be greater than zero.

#### Field: Maximum Motor Output Power

The required numeric maximum output power (input power to the fan drive belt) by the motor (*H~belt,max~*, W). If specified, must be greater than zero. Can be autosized. In the case of direct drive, *H~belt,max~* corresponds to the maximum fan shaft power (*H~fan,max~*).

Specified or autosized maximum motor output power (including effects of scaling by Field: Motor Sizing Factor) is reported in the .eio file as Design Motor Output Power [W]. Also, calculated maximum motor efficiency corresponding to Design Motor Output Power, along with motor efficiency at design flow condition (including part-load effects of oversized motor), are reported in the .eio file as, respectively, Maximum Motor Efficiency [-] and Design Motor Efficiency [-]. Note that maximum motor efficiency often occurs at less than full load.

#### Field: Motor Sizing Factor

The numeric dimensionless sizing factor (*F~motor~*) used to multiply the specified or autosized fan motor output power (*H~belt,max~*). If specified, minimum value is 1.0. Default is 1.0.

#### Field: Motor In Airstream Fraction

The numeric fraction of the combined motor and belt heat that is added to the air stream. A value of 0.0 means that the motor and belt are completely outside the air stream. A value of 1.0 means that all of the motor and belt heat loss will go into the air stream and act to cause an air enthalpy rise. Must be between 0.0 and 1.0. Default is 1.0.

#### Field: VFD Efficiency Type

The alpha basis for calculating fan variable-frequency-drive (VFD) efficiency: "Power", which corresponds to a function of the fraction of full-load motor input power (*H~motor~* / *H~motor,max~*), or "Speed", which corresponds to a function of the fraction of full-load speed (*ω~motor~ / ω~motor,max~*). If this field is blank, then it is assumed that the VFD efficiency is 0.97. If no VFD is used, then specify "Power" and also specify a VFD efficiency curve with a constant value of 1.0 (see VFD Efficiency Curve Name field for details).

#### Field: Maximum VFD Output Power

The required numeric maximum output power (input power to the fan motor) by the variable frequency drive (*H~motor,max~*, W). If specified, must be greater than zero. Can be autosized.

Specified or autosized maximum VFD output power (including effects of scaling by Field: VFD Sizing Factor) and corresponding VFD input power are reported in the .eio file as, respectively, Design VFD Output Power [W] and Rated Power [W]. Also, calculated VFD efficiency corresponding to Design VFD Output Power (including part-load effects of oversized VFD) along with corresponding combined system efficiency (fan, belt, motor, and VFD efficiencies multiplied together) at design flow condition are reported in the .eio file as, respectively, Design VFD Efficiency [-] and Design Combined Efficiency [-].

#### Field: VFD Sizing Factor

The numeric dimensionless factor (*F~VFD~*) used to multiply the specified or autosized motor maximum input power (*H~motor,max~*). If specified, minimum value is 1.0. Default is 1.0 if field is blank.

#### Field: Fan Pressure Rise Curve Name

The required alpha name of the fan total pressure rise performance curve (ref: [Curve:FanPressureRise](#curvefanpressurerise) in Performance Curves) that parameterizes the variation of fan total pressure rise (Δ*P~fan,tot~*, Pa) as a function of volumetric flow through the fan (*Q~fan~*, m^3^/s) and duct static pressure set point (*P~sm~*, Pa). The fan outlet velocity pressure is subtracted from the output of this curve to determine fan static pressure rise, which is then used to calculate a dimensionless Euler number at each time step. The Euler number is in turn used to determine fan efficiency, speed, and torque (the Euler number is defined in the *Euler Number at Maximum Fan Static Efficiency* field). This curve should be valid for the range of volumetric flows, distribution system leakage, duct static pressures, and static pressures surrounding the ducts anticipated for the simulation period.

#### Field: Duct Static Pressure Reset Curve Name

The required alpha name of the performance curve that parameterizes the variation of the duct static pressure set point (*P~sm~*, Pa) as a function of volumetric flow through the fan (*Q~fan~*, m^3^/s), which is used so that the resistance associated with VAV box damper operation is reduced.

The output of this curve is used to calculate the duct static pressure set point at each time step. This curve should be valid for the range of duct static pressure set points and volumetric flows, anticipated for the simulation period.

For an ad hoc linear duct static pressure reset scheme, the relation (ref: [Curve:Linear](#curvelinear) in Performance Curves) between duct static pressure (*P~sm~*, Pa) and flow through the fan (*Q~fan~*, m^3^/s) for *Q~fan,min~* ≤ *Q~fan~* ≤ *Q~fan,max~* is:

![](media/image391.png)\


where ![](media/image392.png)  and ![](media/image393.png)

For *Q~fan~* < *Q~fan,min~*, *P~sm~* = *P~sm,min~*; for *Q~fan~* > *Q~fan,max~*, *P~sm~* = *P~sm,max~*

The minimum and maximum fan airflows (*Q~fan,min~* and *Q~fan,max~*) correspond respectively to the minimum and maximum duct static pressure set points (*P~sm,min~* and *P~sm,max~*).

If no duct static pressure reset scheme is used and the duct static pressure set point is constant, then parameter *C~2~* is set to zero and *C~1~* represents the constant duct static pressure set point.

#### Field: Normalized Fan Static Efficiency Curve Name– Non-Stall Region

The required alpha name of the exponential-modified skew normal performance curve (ref: [Curve:ExponentialSkewNormal](#curveexponentialskewnormal) in Performance Curves) that parameterizes the normalized fan static efficiency (*η~fan~* (*x~fan~*) */ η~fan,max~*) at each time step for the normal operating (non-stall) region of the fan performance map as a function of *x~fan~*, which is defined as log-base-10 of Eu at the fan flow and pressure rise operating point divided by Eu at maximum fan static efficiency [*log~10~*(*Eu / Eu~max~*)]. In this region, *x~fan~*<=0.

The output of this curve is used to calculate the fan efficiency *η~fan~* (*x~fan~*) **at each time step by modifying *η~fan,max~* (see *Maximum Fan Static Efficiency* field). This curve should have a maximum of 1.0 and should be valid for the range of volumetric flows and fan pressure rises anticipated for the simulation period.

#### Field: Normalized Fan Static Efficiency Curve Name– Stall Region

The required alpha name of the exponential-modified skew normal performance curve (ref: [Curve:ExponentialSkewNormal](#curveexponentialskewnormal) in Performance Curves) that parameterizes the normalized fan static efficiency (*η~fan~* (*x~fan~*) */ η~fan,max~*) at each time step for the stall region of the fan performance map as a function of *x~fan~* (see *Normalized Fan Static Efficiency Curve Name– Non-Stall Region* field). In this region, *x~fan~*>0.

The output of this curve is used to calculate the fan efficiency *η~fan~* (*x~fan~*) **at each time step by modifying *η~fan,max~* (see *Maximum Fan Static Efficiency* field). This curve should have a maximum of 1.0 and should be valid for the range of volumetric flows and fan pressure rises anticipated for the simulation period.

#### Field: Normalized Dimensionless Airflow Curve Name – Non-Stall Region

The required alpha name of the sigmoid performance curve (ref: [Curve:Sigmoid](#curvesigmoid) in Performance Curves) that parameterizes the normalized dimensionless airflow through the fan (*φ* (*x~fan~*) */ φ~max~*) at each time step for the normal operating (non-stall) region of the fan performance map as a function of *x~fan~*, which is defined as log-base-10 of Eu at the fan flow and pressure rise operating point divided by Eu at maximum fan static efficiency [*log~10~*(*Eu / Eu~max~*)]. In this region, *x~fan~* <=0.

The output of this curve is used to calculate the dimensionless airflow *φ* (*x~fan~*) **at each time step by modifying *φ~max~* (see *Maximum Dimensionless Fan Airflow* field). This curve should have a maximum of 1.0 and should be valid for the range of volumetric flows and fan pressure rises anticipated for the simulation period.

#### Field: Normalized Dimensionless Airflow Curve Name – Stall Region

The required alpha name of the sigmoid performance curve (ref: [Curve:Sigmoid](#curvesigmoid) in Performance Curves) that parameterizes the normalized dimensionless airflow through the fan (*φ* (*x~fan~*) */ φ~max~*) at each time step for the stall region of the fan performance map as a function of *x~fan~* (see *Normalized Dimensionless Airflow Curve Name – Non-Stall Region* field). In this region, *x~fan~* >0.

The output of this curve is used to calculate the dimensionless airflow *φ* (*x~fan~*) **at each time step by modifying *φ~max~* (see *Maximum Dimensionless Fan Airflow* field). This curve should have a maximum of 1.0 and should be valid for the range of volumetric flows and fan pressure rises anticipated for the simulation period.

#### Field: Maximum Belt Efficiency Curve Name

The alpha name of the quartic polynomial performance curve (ref: [Curve:Quartic](#curvequartic) in Performance Curves) that determines the maximum fan drive belt efficiency in logarithmic space (*η~belt,max,ln~*) as a function of *x~belt,max~*. The curve is:

*![](media/image394.png)*

where *x~belt,max~* *= ln*(*F~belt~\*H~fan,max~*) with *H~fan,max~* expressed in terms of hp.

*Note that η~belt,max~ = exp*(*η~belt,max,ln~*).

The output of this curve must be greater than zero and less than or equal to 1.0. If *η~belt,max~* is known, it is represented by coefficient *C~1~*(*=ln*(*η~belt,max~*)). In this case, coefficients *C~2~* through *C~5~* are set to zero. If this field is left blank (e.g., there is no belt), the model assumes that the output of the modifier curve is 1.0 for the entire simulation (maximum belt efficiency = 1.0).

#### Field: Normalized Belt Efficiency Curve Name – Region 1

The alpha name of the single rectangular hyperbola type 2 performance curve (ref: [Curve:RectangularHyperbola2](#curverectangularhyperbola2) in Performance Curves) that determines the normalized (par-load) fan drive belt efficiency (*η~belt~* (*x*~belt~) **/ *η~belt,max~*) as a function of *x~belt~*. Normalized belt efficiency is represented by a segmented curve with three different regions. The curve for Region 1 (0 <= *x~belt~* < *x~belt,trans~*) is:

*η~belt~* (*x*~belt~) **/ *η~belt,max~ =* (*C~1~·\* x~belt~*) */* (*C~2~+x~belt~*) *+ C~3~·\* x~belt~*

where *x~belt~ =* *τ~belt~* / *τ~belt,max~; τ~belt~* is the belt output torque that corresponds to the calculated power input to the fan shaft (*H~fan~*, W) by the drive belt and the calculated fan shaft speed (*ω~fan~*, rpm).

The output of this curve is used to calculate the belt efficiency *η~belt~* (*x~belt~*) **in Region 1 **at each time step by modifying *η~belt,max~* (see *Maximum Belt Efficiency* *Curve Name* field). The output of this curve must be greater than zero and less than or equal to 1.0 and should be valid for the range of volumetric flows and fan pressure rises anticipated for the simulation period.

If this field is left blank, the model assumes that the output of the modifier curve is 1.0 for the entire simulation (i.e., constant belt efficiency at *η~belt,max~* in Region 1).

#### Field: Normalized Belt Efficiency Curve Name – Region 2

The alpha name of the exponential decay performance curve (ref: [Curve:ExponentialDecay](#curveexponentialdecay) in Performance Curves) that determines the normalized (part-load) fan drive belt efficiency (*η~belt~* (*x*~belt~) **/ *η~belt,max~*) as a function of *x~belt~*. Normalized belt efficiency is represented by a segmented curve with three different regions. The curve for Region 2 (*x~belt,trans~* <= *x~belt~* <= 1) is:

*η~belt~* (*x*~belt~) **/ *η~belt,max~ = C~1~ + C~2~ \* exp*^(^*^C^3^\*x^belt*^)^

where *x~belt~ =* *τ~belt~* / *τ~belt,max~; τ~belt~* is the belt output torque that corresponds to the calculated power input to the fan shaft (*H~fan~*, W) by the drive belt and the calculated fan shaft speed (*ω~fan~*, rpm).

The output of this curve is used to calculate the belt efficiency *η~belt~* (*x~belt~*) **in Region 2 **at each time step by modifying *η~belt,max~* (see *Maximum Belt Efficiency* *Curve Name* field). The output of this curve must be greater than zero and less than or equal to 1.0 and should be valid for the range of volumetric flows and fan pressure rises anticipated for the simulation period.

If this field is left blank, the model assumes that the output of the modifier curve is 1.0 for the entire simulation (i.e., constant belt efficiency at *η~belt,max~* in Region 2).

#### Field: Normalized Belt Efficiency Curve Name – Region 3

The alpha name of the single rectangular hyperbola type 2 performance curve (ref: [Curve:RectangularHyperbola2](#curverectangularhyperbola2) in Performance Curves) that determines the normalized (part-load) fan drive belt efficiency (*η~belt~* (*x*~belt~) **/ *η~belt,max~*) as a function of *x~belt~*. Normalized belt efficiency is represented by a segmented curve with three different regions. The curve for Region 3 (*x~belt~* > 1) is:

*η~belt~* (*x*~belt~) **/ *η~belt,max~ =* (*C~1~·\* x~belt~*) */* (*C~2~+x~belt~*) *+ C~3~·\* x~belt~*

where *x~belt~ =* *τ~belt~* / *τ~belt,max~; τ~belt~* is the belt output torque that corresponds to the calculated power input to the fan shaft (*H~fan~*, W) by the drive belt and the calculated fan shaft speed (*ω~fan~*, rpm).

The output of this curve is used to calculate the belt efficiency *η~belt~* (*x~belt~*) **in Region 3 **at each time step by modifying *η~belt,max~* (see *Maximum Belt Efficiency* *Curve Name* field). The output of this curve must be greater than zero and less than or equal to 1.0 and should be valid for the range of volumetric flows and fan pressure rises anticipated for the simulation period.

If this field is left blank, the model assumes that the output of the modifier curve is 1.0 for the entire simulation (i.e., constant belt efficiency at *η~belt,max~* in Region 3).

#### Field: Maximum Motor Efficiency Curve Name

The alpha name of the single rectangular hyperbola type 1 performance curve (ref: Curve: RectangularHyperbola1 in Performance Curves) that determines the maximum fan motor efficiency (*η~motor,max~*) as a function of *x~motor,max~*. The curve is:

*η~motor,max~ =* (*C~1~ \* x~motor,max~*) */* (*C~2~ + x~motor,max~*) *+ C~3~*

where *x~motor,max~=ln*(*F~motor~ \* H~belt,max~*) with *H~belt,max~* expressed in terms of hp. *H~belt,max~* is the maximum output power from the motor to the belt, which corresponds to the calculated maximum power input to the fan shaft (*H~fan,max~*, W).

The output of this curve must be greater than zero and less than or equal to 1.0. If *η~motor,max~* is known, it is represented by coefficient *C~3~*. In this case, coefficients *C~1~* and *C~2~* are set to zero.

If this field is left blank, the model assumes that the output of the modifier curve is 1.0 for the entire simulation (maximum motor efficiency = 1.0).

#### Field: Normalized Motor Efficiency Curve Name

The name of the HVAC system node to which the fan sends its outlet air.

The alpha name of the single rectangular hyperbola type 2 performance curve (ref: [Curve:RectangularHyperbola2](#curverectangularhyperbola2) in Performance Curves) that determines the normalized (part-load) fan motor efficiency (*η~motor~* (*x*~motor~) **/ *η~motor,max~*) as a function of the motor load fraction *x~motor~*. The curve is:

*η~motor~* (*x~motor~*) */ η~motor,max~ =* (*C~1~ \* x~motor~*) */* (*C~2~ + x~motor~*) *+* (*C~3~·\* x~motor~*)

where *x~motor~ = H~belt~* / *H~belt,max~. H~belt~* is the calculated output power from the motor to the belt (W), which corresponds to the calculated power input to the fan shaft (*H~fan~*, W).

The output of this curve is used to calculate the motor efficiency (*η~motor~* (*x~motor~*)) **at each time step by modifying *η~motor,max~* (see *Maximum Motor Efficiency Curve Name* field). The output of this curve must be greater than zero and less than or equal to 1.0 and should be valid for the range of volumetric flows and fan pressure rises anticipated for the simulation period.

If this field is left blank, the model assumes that the output of the modifier curve is 1.0 for the entire simulation (i.e., constant motor efficiency at *η~motor,max~*).

#### Field: VFD Efficiency Curve Name

The alpha name of the single rectangular hyperbola type 2 performance curve (e.g., [Curve:RectangularHyperbola2](#curverectangularhyperbola2) in Performance Curves) that determines the VFD efficiency (*η~VFD~* (*x*~VFD~)) as a function of the fractional input power of the motor or fractional motor speed (*x~VFD~*). An example of the curve is:

*η~VFD~* (*x~VFD~*) *=* (*C~1~ \* x~VFD~*) */* (*C~2~+x~VFD~*) *+ C~3~ \* x~VFD~*

where *x~VFD~ = H~motor~* / *H~motor,max~* or *ω~motor~ / ω~motor,max~*

The output of this curve is used to calculate the VFD efficiency *η~VFD~* (*x~VFD~*) **at each time step. The output of this curve must be greater than zero and less than or equal to 1.0 and should be valid for the range of volumetric flows and fan pressure rises anticipated for the simulation period.

If this field is left blank, the model assumes that the output of the modifier curve is 0.97 for the entire simulation (i.e., constant VFD efficiency of 0.97).

#### Field: End-Use Subcategory

Allows you to specify a user-defined end-use subcategory, e.g., "Central System". A new meter for reporting is created for each unique subcategory  (ref: [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). Subcategories are also reported in the ABUPS table. If this field is omitted or blank, the fan will be assigned to the "General" end-use subcategory.

An example of use in an IDF:

~~~~~~~~~~~~~~~~~~~~

    Fan:ComponentModel,
        Supply Fan 1,                    ! Fan Name
        Main Heating Coil 1 Outlet Node, ! Inlet Node Name
        VAV Sys 1 Outlet Node,           ! Outlet Node Name
        FanAvailSched,                   ! Fan Schedule
        autosize,                        ! Maximum Flow Rate [m3/s]
        autosize,                        ! Minimum Flow Rate [m3/s]
        1.0,                             ! Fan Sizing Factor [-]
        0.3048,                          ! Fan Wheel Diameter [m]
        0.0873288576,                    ! Fan Outlet Area [m2]
        0.514,                           ! Maximum Fan Static Efficiency [-]
        9.76,                            ! Euler Number at Maximum Fan Static Efficiency [-]
        0.160331811647483,               ! Maximum Dimensionless Fan Airflow [-]
        autosize,                        ! Motor/Fan Pulley Ratio [-]
        autosize,                        ! Belt Maximum Torque [N m]
        1.0,                             ! Belt Sizing Factor [-]
        0.167,                           ! Belt Fractional Torque Transition [-]
        1800,                            ! Motor Maximum Speed [rpm]
        autosize,                        ! Maximum Motor Output Power [W]
        1.0,                             ! Motor Sizing Factor [-]
        1.0,                             ! Motor In Airstream Fraction [-]
        Power,                           ! VFD Efficiency Type
        autosize,                        ! Maximum VFD Output Power [W]
        1.0,                             ! VFD Sizing Factor [-]
        VSD Example,                     ! Fan Pressure Rise Curve Name
        DiagnosticSPR,                   ! Duct Static Pressure Reset Curve Name
        FanEff120CPLANormal,             ! Fan Efficiency Curve Name – Non-Stall
        FanEff120CPLAStall,              ! Fan Efficiency Curve Name - Stall
        FanDimFlowNormal,                ! Dimensionless Airflow Curve Name-Non-Stall
        FanDimFlowStall,                 ! Dimensionless Airflow Curve Name-Stall
        BeltMaxEffMedium,                ! Maximum Belt Efficiency Curve Name
        BeltPartLoadRegion1,             ! Normalized Belt Efficiency Curve Name
        BeltPartLoadRegion2,             ! Normalized Belt Efficiency Curve Name
        BeltPartLoadRegion3,             ! Normalized Belt Efficiency Curve Name
        MotorMaxEffAvg,                  ! Maximum Motor Efficiency Curve Name
        MotorPartLoad,                   ! Normalized Motor Efficiency Curve Name
        VFDPartLoad;                     ! VFD Efficiency Curve Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Fan Electric Power[W]
    HVAC,Average,Fan Rise in Air Temperature [deltaC]
    HVAC,Sum,Fan Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Fan Electric Power [W]

This output field contains the average electricity consumption rate for the fan in Watts for the timestep being reported.

#### Fan Rise in Air Temperature [deltaC]

This output field contains the average rise in air temperature across the fan (outlet air temperature minus inlet air temperature) in degrees Celsius for the timestep being reported.

#### Fan Electric Energy [J]

This output field contains the electricity consumption of the fan in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key= Fans, Group Key= System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Other Outputs

Several parameters input by the user or calculated during component sizing for the design condition (maximum system airflow) are reported separately in the <filename>.eio file. These parameters include fan airflow and pressure rise; fan shaft input, motor output, VFD output, and VFD input (rated) power; pulley drive ratio; belt output torque; and fan, belt, motor, VFD, and combined system efficiencies. They can be identified by lines in the .eio file beginning with "Component Sizing Information, [Fan:ComponentModel](#fancomponentmodel)". The same values are also reported under the ComponentSizingSummary heading in the <filename>Table.html file.