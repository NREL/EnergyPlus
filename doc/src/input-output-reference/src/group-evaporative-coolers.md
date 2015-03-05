# Group – Evaporative Coolers

This group of objects describes the properties and configuration for the evaporative coolers models for the HVAC section.

## EvaporativeCooler:Direct:CelDekPad

The direct stage, shown in the figure below, consists of a rigid media evaporative pad, with water recirculated from a reservoir.  The water is pumped from the reservoir to a water distribution header, for water feed by gravity from above the media.  The evaporative pad provides the area for the adiabatic saturation of the air.  While the process provides a lower dry-bulb temperature, the moisture content of the leaving air is higher than the entering condition.  The direct stage is used for comfort cooling in a building where adding humidity to the air can be tolerated.

![Direct Stage Evaporative Cooler](media/direct-stage-evaporative-cooler.png)


The thermodynamic process is a simultaneous heat and mass transfer, or adiabatic cooling, and follows a constant enthalpy line on the psychrometric chart, it is shown in the figure below as a process from A to B.  Since the deviation of the constant wet-bulb line and the constant enthalpy line is small, it is assumed that the wet-bulb temperature is constant across the direct evaporative stage.

![Psychrometric Chart -- Constant Enthalpy](media/psychrometric-chart-constant-enthalpy.png)


If the direct evaporative process were 100% efficient, the leaving dry-bulb temperature would equal the entering wet-bulb temperature.  The efficiency of the direct evaporative process is less than 100% and by defining saturation efficiency (![](media/image402.png) se) for the direct stage or evaporative pad, the leaving dry-bulb temperature can be expressed by the following equation.

![](media/image403.png)\


### Inputs

#### Field: Name

A unique identifying name for each evaporative cooler.

#### Field: Availability Schedule Name

The name of a schedule which defines when the evaporative cooler is available. A schedule value of 0 indicates that the evaporative cooler is off for that time period. A schedule value greater than 0 indicates that the evaporative cooler can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Direct Pad Area

The face area of the evaporative pad in m^2^.  With the area and mass flow rate, the air velocity is calculated and is used to determine the saturation efficiency. This field is autosizable.

#### Field: Direct Pad Depth

The depth of the evaporative pad in meters.  The pad depth is used to determine the saturation efficiency. This field is autosizable.

#### Field: Recirculating Water Pump Power Consumption

This field is used to specify the power consumed by the evaporative cooler recirculating pump in Watts.

#### Field: Air Inlet Node Name

The name of the evaporative cooler air inlet node from the Air Loop Simulation.

#### Field: Air Outlet Node Name

The name of the evaporative cooler air outlet node from the Air Loop Simulation.

#### Field: Control Type

This input field is currently unused and can be left blank.

#### Field: Water Supply Storage Tank Name

This field is optional.  It is used to describe where the cooler obtains water used for evaporative cooling.  If blank or omitted, then the cooler will obtain water directly from the mains.  If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the cooler will obtain its water from that tank. If a tank is specified, the cooler will attempt to obtain all the water it uses from the tank.  However if the tank cannot provide all the water the cooler needs, then the cooler will still operate and obtain the rest of the water it needs from the mains.

An IDF example showing how this object is:

~~~~~~~~~~~~~~~~~~~~

      EvaporativeCooler:Direct:CelDekPad,
        Evaporative Cooler,      !- Name
        System Availability Schedule,  !- Availability Schedule Name
        0.6,                     !- Direct Pad Area {m2}
        0.2,                     !- Direct Pad Depth {m}
        225,                     !- Recirculating Water Pump Power Consumption {W}
        Evap Cooler Inlet Node,  !- Air Inlet Node Name
        Supply Outlet Node;      !- Air Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The output variables that are available for this direct evaporative cooler are shown below:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Evaporative Cooler Wet Bulb Effectiveness
    HVAC,Average, Evaporative Cooler Electric Power[W]
    HVAC,Sum, Evaporative Cooler Electric Energy [J]
    HVAC,Sum, Evaporative Cooler Water Volume[m3]
    HVAC,Sum,Evaporative Cooler Mains Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Storage Tank Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Mains Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Evaporative Cooler Wet Bulb Effectiveness

The effectivenss, or saturation efficiency, is the temperature change of the supply air divided by the difference between the inlet air dry-bulb and wet-bulb temperatures. In other words, it is a measure of the approach to the inlet air wet-bulb temperature.

#### Evaporative Cooler Electric Power[W]

#### Evaporative Cooler Electric Energy [J]

These output variables report the electric power and electric energy required to operate the water pump.

#### Evaporative Cooler Water Volume [m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Mains Water Volume [m3]

This is the source of the water consumed.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Storage Tank Water Volume [m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Water Volume [m3]

This is the water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Mains Water Volume [m3]

This is the source (mains) of water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.

## EvaporativeCooler:Direct:ResearchSpecial

This cooler is similar in principal to the [EvaporativeCooler:Direct:CelDekPad](#evaporativecoolerdirectceldekpad). The model differs in that it gives the user a simple way of specify the cooler effectiveness. Using the ResearchSpecial input object also allows the cooler to control the amount of cooling based on node setpoints (controlled by SetpointManagers). This avoid problems from over cooling when conditions are such that loads are low and cooling power is high. Water pump power is assumed to vary linearly when the cooler is operating at less than full capacity.

### Inputs

#### Field: Name

A unique identifying name for each cooler.

#### Field: Availability Schedule Name

The name of a schedule that defines when the evaporative cooler is available. A schedule value of 0 indicates that the evaporative cooler is off for that time period. A schedule value greater than 0 indicates that the evaporative cooler can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Cooler Effectiveness

This field specifies the effectiveness that is applied to the wetbulb depression to determine the conditions leaving the cooler. This model assumes that the effectiveness is constant.

#### Field: Recirculationg Water Pump Power Consumption

This field is used to specify the power consumed by the water pump that circulates water in Watts.

#### Field: Air Inlet Node Name

The name of the air inlet node for the primary air flow path through the cooler.

#### Field: Air Outlet Node Name

The name of the air outlet node for the primary air flow path through the cooler.

#### Field: Sensor Node Name

This field specifies the name of a node that will provide system air temperature setpoint information.  A separate SetpointManager object should be setup to update this node.

#### Field: Water Supply Storage Tank Name

This field is optional. It is used to describe where the cooler obtains water used for evaporative cooling. If blank or omitted, then the cooler will obtain water directly from the mains. If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the cooler will obtain its water from that tank. If a tank is specified, the cooler will attempt to obtain all the water it uses from the tank. However, if the tank cannot provide all the water the cooler needs, then the cooler will still operate and obtain the rest of the water it needs from the mains.

#### Field: Drift Loss Fraction

This field is optional and can be used to model additional water consumed by the cooler from drift.  Drift is water that leaves the cooling media as droplets and does not evaporate into the process air stream.  For example, water may get blown off the evaporative media by winds and escape the air system.  The value entered here is a simple fraction of the water consumed by the cooler for normal process evaporation.  The amount of drift is this fraction times the water evaporated for the normal cooling process.  This field can be left blank and then there will be no added water consumption from drift.

#### Field: Blowdown Concentration Ratio

This field is optional and can be used to model additional water consumed by the cooler from blowdown.  Blowdown is water that is intentionally drained from the cooler's sump to offset the build up of solids in the water that would otherwise occur because of evaporation.  The value entered here is dimensionless.  It can be characterized as the ratio of solids in the blowdown water to solids in the make up water.  Typical values are 3 to 5.  The default is 3.0.

An example IDF entry is

~~~~~~~~~~~~~~~~~~~~

    EvaporativeCooler:Direct:ResearchSpecial,
      Direct Evap Cooler, !- Name
      ALWAYS_ON, !- Availability Schedule Name
      0.7 , !- Cooler Effectiveness
      30.0 , !- Recirculating Water Pump Power Consumption
      OAIndRDD Evap Cooler- OADirect Evap CoolerNode , !- Air Inlet Node Name
      OADirect Evap Cooler- OAMixing BoxNode, !- Air Outlet Node Name
      OADirect Evap Cooler- OAMixing BoxNode, !- Sensor Node Name
      , !- Water Supply Storage Tank Name
      0.0, !- Drift Loss Fraction
      3; !- Blowdown Concentration Ratio
~~~~~~~~~~~~~~~~~~~~

### Outputs

The output variables that are available for this direct evaporative cooler are shown below:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Evaporative Cooler Electric Power[W]
    HVAC,Sum, Evaporative Cooler Electric Energy [J]
    HVAC,Sum, Evaporative Cooler Water Volume[m3]
    HVAC,Sum,Evaporative Cooler Mains Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Storage Tank Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Mains Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Evaporative Cooler Electric Power[W]

#### Evaporative Cooler Electric Energy [J]

These output variables report the electric power and electric energy required to operate the water pump.

#### Evaporative Cooler Water Volume [m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Mains Water Volume [m3]

This is the source of the water consumed.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Storage Tank Water Volume [m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Water Volume [m3]

This is the water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Mains Water Volume [m3]

This is the source (mains) of water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.

## EvaporativeCooler:Indirect:CelDekPad

The dry coil indirect evaporative cooler, shown in the figure below, has a rigid media pad, similar to the direct evaporative stage, where the adiabatic cooling takes place. The secondary air leaves the rigid media pad and enters an air to air heat exchanger where it cools the supply air flowing through the heat exchanger tubes. The moist secondary air is then exhausted to the environment. The secondary air stream has its own fan and consists of a rigid media evaporative pad, with water recirculated from a reservoir. The water is pumped from the reservoir to a water distribution header, for water feed by gravity from above the media. The evaporative pad provides the area for the adiabatic saturation of the air.

![Evaporative Cooler -- Indirect Dry Coil](media/evaporative-cooler-indirect-dry-coil.png)


The process that the secondary air goes through, A to C to D, is shown by the dashed lines in  the following figure. Process A to C is adiabatic cooling in the rigid media pad. Then the air enters the shell side of the heat exchanger and is sensibly heated from C to D by the warm supply air passing through the tube side. The secondary air inlet is modeled as a separate stream of outdoor air and the user has the option of defining the name of an outdoor air node.

![Secondary Air Process -- Indirect Dry Coil Evap Cooler](media/secondary-air-process-indirect-dry-coil-evap.png)


The advantage of the dry coil heat exchanger is that the heat exchanger does not have the evaporation taking place on the outside of the tubes, thus no mineral deposits are left on the heat exchange surface to reduce the efficiency of the heat exchanger. The rigid media pads are designed to flush the mineral deposits to the sump, so the saturation efficiency of the pad stays relatively constant.

### Inputs

#### Field: Name

A unique identifying name for each evaporative cooler.

#### Field: Availability Schedule Name

The name of a schedule which defines when the evaporative cooler is available. A schedule value of 0 indicates that the evaporative cooler is off for that time period. A schedule value greater than 0 indicates that the evaporative cooler can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Direct Pad Area

The face area of the evaporative pad in m^2^. With the area and mass flow rate, the air velocity is calculated and is used to determine the saturation efficiency on the secondary side of the evaporative cooler. This field is autosizable.

#### Field: Direct Pad Depth

The depth of the evaporative pad in meters. The pad depth is used to determine the saturation efficiency on the secondary side of the evaporative cooler. This field is autosizable.

#### Field: Recirculating Water Pump Power Consumption

This field is used to specify the power consumed by the evaporative cooler recirculating pump in Watts.

#### Field: Secondary Fan Flow Rate

This field is used to specify the secondary fan flow rate and is specified in m^3^/sec.

#### Field: Secondary Fan Total Efficiency

This value is the overall efficiency of the fan, i.e., the ratio of the power delivered to the fluid to the electrical input power. It is the product of the motor efficiency and the impeller efficiency. The motor efficiency is the power delivered to the shaft divided by the electrical power input to the motor. The impeller efficiency is power delivered to the fluid (air) divided by the shaft power. The power delivered to the fluid is the mass flow rate of the air multiplied by the pressure rise divided by the air density. This input value must be between 0 and 1.

#### Field: Secondary Fan Delta Pressure

This field is used to specify the delta pressure across the secondary stage of the evaporative cooler in Pascals.

#### Field: Indirect Heat Exchanger Effectiveness

This field is used to specify the effectiveness of the indirect heat exchanger between the primary and secondary air flow.

#### Field: Primary Air Inlet Node Name

The name of the evaporative cooler's primary air inlet node from the Air Loop Simulation. This is the air flow being cooled indirectly.

#### Field: Primary Air Outlet Node Name

The name of the evaporative cooler's primary air outlet node from the Air Loop Simulation.

#### Field: Control Type

This input field is currently unused and can be left blank.

#### Field: Water Supply Storage Tank Name

This field is optional. It is used to describe where the cooler obtains water used for evaporative cooling. If blank or omitted, then the cooler will obtain water directly from the mains. If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the cooler will obtain its water from that tank. If a tank is specified, the cooler will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the cooler needs, then the cooler will still operate and obtain the rest of the water it needs from the mains.

#### Field: Secondary Air Inlet Node Name

This field is optional. It is used to explicitly define an outdoor air node for the inlet for secondary air stream. Defining an outdoor air node here allows using the height-dependent model for outdoor air conditions.

And an IDF example showing how this object is specified:

~~~~~~~~~~~~~~~~~~~~

     EvaporativeCooler:Indirect:CelDekPad,
        IndirectEvapCooler1,     !- Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        0.6,                     !- Direct Pad Area {m2}
        0.2,                     !- Direct Pad Depth {m}
        225.,                    !- Recirculating Water Pump Power Consumption {W}
        1.0,                     !- Secondary Fan Flow Rate {m3/s}
        0.7,                     !- Secondary Fan Total Efficiency
        200.0,                   !- Secondary Fan Delta Pressure {Pa}
        0.67,                    !- Indirect Heat Exchanger Effectiveness
        EvapCoolerIndirectInletAirNode,  !- Primary Air Inlet Node Name
        EvapCoolerDirectInletAirNode,  !- Primary Air Outlet Node Name
        ,                      !- Control Type
        ,                        !- Water Supply Storage Tank Name
        Secondary side OA inlet node;  !- Secondary Air Inlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The output variables that are available for the indirect dry evaporative cooler are shown below:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Evaporative Cooler Wetbulb Effectiveness
    HVAC,Average, Evaporative Cooler Total Stage Effectiveness
    HVAC,Average, Evaporative Cooler Electric Power[W]
    HVAC,Sum, Evaporative Cooler Electric Energy [J]
    HVAC,Sum, Evaporative Cooler Water Volume[m3]
    HVAC,Sum,Evaporative Cooler Mains Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Storage Tank Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Mains Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Evaporative Cooler Wetbulb Effectiveness

The dry evaporation saturation efficiency is the saturation efficiency of the secondary or wet side air stream defined as the temperature change of the supply air divided by the difference between the outdoor dry-bulb and wet-bulb temperatures. In other words, it is a measure of the approach to the outdoor wet-bulb temperature.

#### Evaporative Cooler Total Stage Effectiveness

The total stage efficiency includes the sensible heat exchanger effectiveness of the heat exchanger in the supply air stream. It is the saturation efficiency multiplied by the heat exchanger effectiveness.

#### Evaporative Cooler Electric Power[W]

#### Evaporative Cooler Electric Energy [J]

These output variables report the electric power and energy consumed by the secondary air fan and the sump pump.

#### Evaporative Cooler Water Volume[m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Mains Water Volume [m3]

This is the source of the water consumed.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Storage Tank Water Volume [m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Water Volume [m3]

This is the water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Mains Water Volume [m3]

This is the source (mains) of water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.

## EvaporativeCooler:Indirect:WetCoil

The wetted coil evaporative cooler shown in the figure below, has water sprayed directly on the tubes of the heat exchanger where latent cooling takes place. The vaporization of the water on the outside of the heat exchanger tubes allows the simultaneous heat and mass transfer which removes heat from the supply air on the tube side. Then the moist secondary air is exhausted. The secondary air stream has its own fan.

![Evaporative Cooler – Indirect Wet Coil](media/evaporative-cooler-indirect-wet-coil.png)


The process that the secondary air goes through, A to C on the following figure, is a path of simultaneous heat and mass transfer, but it does not follow a line of constant enthalpy as in the direct stage. The process is not adiabatic due to the heat gain from the supply air flowing through the tubes of the heat exchanger.

![Secondary Air Process – Indirect Wet Coil Evap Cooler](media/secondary-air-process-indirect-wet-coil-evap.png)


The wet coil heat exchanger can have a higher stage efficiency than the dry coil due to a higher heat transfer rate on the outside of the heat exchanger tubes. Over the operating lifetime of the heat exchanger, the vaporization taking place on the heat exchange surface can leave mineral deposits which will decrease the effectiveness of the heat exchanger.

### Inputs

#### Field: Name

A unique identifying name for each evaporative cooler.

#### Field: Availability Schedule Name

The name of a schedule which defines when the evaporative cooler is available. A schedule value of 0 indicates that the evaporative cooler is off for that time period. A schedule value greater than 0 indicates that the evaporative cooler can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Coil Maximum Efficiency

The maximum efficiency of the stage is a combination of the efficiency due to the simultaneous heat and mass transfer on the outside of the tube and the efficiency of the heat exchanger. This value can be higher than the dry coil overall efficiency since the convective coefficients on the outside of the tube are larger.

#### Field: Coil Flow Ratio

The Coil Flow Ratio is determined from performance data. The Coil Flow Ratio tells how quickly the efficiency of the stage would decrease with a mismatch of the supply and secondary flows.

#### Field: Recirculating Water Pump Power Consumption

This field is used to specify the power consumed by the evaporative cooler recirculating pump in Watts.

#### Field: Secondary Fan Flow Rate

This field is used to specify the secondary fan flow rate and is specified in m^3^/sec.

#### Field: Secondary Fan Total Efficiency

This value is the overall efficiency of the fan, i.e., the ratio of the power delivered to the fluid to the electrical input power. It is the product of the motor efficiency and the impeller efficiency. The motor efficiency is the power delivered to the shaft divided by the electrical power input to the motor. The impeller efficiency is power delivered to the fluid (air) divided by the shaft power. The power delivered to the fluid is the mass flow rate of the air multiplied by the pressure rise divided by the air density. This input value must be between 0 and 1..

#### Field: Secondary Fan Delta Pressure

This field is used to specify the delta pressure across the secondary stage of the evaporative cooler in Pascals.

#### Field: Primary Air Inlet Node Name

The name of the evaporative cooler air inlet from the Air Loop Simulation.

#### Field: Primary Air Outlet Node Name

The name of the evaporative cooler air outlet from the Air Loop Simulation.

#### Field: Control Type

This input field is currently unused and can be left blank.

#### Field: Water Supply Storage Tank Name

This field is optional. It is used to describe where the cooler obtains water used for evaporative cooling. If blank or omitted, then the cooler will obtain water directly from the mains. If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the cooler will obtain its water from that tank. If a tank is specified, the cooler will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the cooler needs, then the cooler will still operate and obtain the rest of the water it needs from the mains.

#### Field: Secondary Air Inlet Node Name

This field is optional. It is used to explicitly define an outdoor air node for the inlet for secondary air stream. Defining an outdoor air node here allows using the height-dependent model for outdoor air conditions.

#### Field: Drift Loss Fraction

This field is optional and can be used to model additional water consumed by the cooler from drift.  Drift is water that leaves the cooling media as droplets and does not evaporate into the process air stream.  For example, water may get blown off the evaporative media by winds and escape the air system.  The value entered here is a simple fraction of the water consumed by the cooler for normal process evaporation.  The amount of drift is this fraction times the water evaporated for the normal cooling process.  This field can be left blank and then there will be no added water consumption from drift.

#### Field: Blowdown Concentration Ratio

This field is optional and can be used to model additional water consumed by the cooler from blowdown.  Blowdown is water that is intentionally drained from the cooler's sump to offset the build up of solids in the water that would otherwise occur because of evaporation.  The value entered here is dimensionless.  It can be characterized as the ratio of solids in the blowdown water to solids in the make up water.  Typical values are 3 to 5.  The default is 3.0.

And an IDF example showing how this object is specified:

~~~~~~~~~~~~~~~~~~~~

    EvaporativeCooler:Indirect:WetCoil,
        IndirectEvapCooler1,     !- Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        0.8,                     !- Coil Maximum Efficiency
        0.16,                    !- Coil Flow Ratio
        225.,                    !- Recirculating Water Pump Power Consumption {W}
        1.0,                     !- Secondary Fan Flow Rate {m3/s}
        0.7,                     !- Secondary Fan Total Efficiency
        200.0,                   !- Secondary Fan Delta Pressure {Pa}
        EvapCoolerIndirectInletAirNode,  !- Primary Air Inlet Node Name
        EvapCoolerDirectInletAirNode,  !- Primary Air Outlet Node Name
        ,                        !- Control Type
        ,                        !- Water Supply Storage Tank Name
        Secondary side OA inlet node;  !- Secondary Air Inlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The output variables that are available for the wet indirect evaporative cooler are shown below:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Evaporative Cooler Total Stage Effectiveness
    HVAC,Average, Evaporative Cooler Electric Power[W]
    HVAC,Sum, Evaporative Cooler Electric Energy [J]
    HVAC,Sum, Evaporative Cooler Water Volume[m3]
    HVAC,Sum,Evaporative Cooler Mains Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Storage Tank Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Mains Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Evaporative Cooler Total Stage Effectiveness []

The Total Stage Efficiency is defined as the temperature change of the supply air divided by the difference between the outdoor dry-bulb and wet-bulb temperatures, including the effect of the reduction in flow because of the secondary air stream. In other words, it is a measure of the approach to the outdoor wet-bulb temperature.

#### Evaporative Cooler Electric Power [W]

#### Evaporative Cooler Electric Energy [J]

These output variables report the electric power and energy that are consumed by the secondary air fan and the sump pump.

#### Evaporative Cooler Water Volume [m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Mains Water Volume [m3]

This is the source of the water consumed.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Storage Tank Water Volume [m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Water Volume [m3]

This is the water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Mains Water Volume [m3]

This is the source (mains) of water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.

## EvaporativeCooler:Indirect:ResearchSpecial

This cooler is similar in principal to the [EvaporativeCooler:Indirect:CelDekPad](#evaporativecoolerindirectceldekpad) and [EvaporativeCooler:Indirect:WetCoil](#evaporativecoolerindirectwetcoil) (see Figure 147, Figure 148, and Figure 149). The model differs in that it gives the user more flexibility to specify the source of secondary air. The cooler effectiveness with respect to wetbulb depression is allowed to go beyond 1.0. Using the ResearchSpecial input object also allows the cooler to control the amount of cooling based on node setpoints (controlled by SetpointManagers). This avoid problems from over cooling when conditions are such that loads are low and cooling power is high. Fan power is assumed to vary linearly when the cooler is operating at less than full capacity.

### Inputs

#### Field: Name

A unique identifying name for each cooler.

#### Field: Availability Schedule Name

The name of a schedule that defines when the evaporative cooler is available. A schedule value of 0 indicates that the evaporative cooler is off for that time period. A schedule value greater than 0 indicates that the evaporative cooler can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Cooler Maximum Effectiveness

This field specifies the maximum effectiveness that is applied to the wetbulb depression to determine the conditions leaving the cooler. This effectiveness is a complicated function of the efficiency with which heat and mass are transferred on the secondary side and the efficiency of heat exchange between the secondary and primary flows. The model assumes that the effectiveness is constant.

#### Field: Cooler Flow Ratio

Not used in this model.

#### Field: Recirculating Water Pump Power Consumption

This field is used to specify the power consumed by the water pump that circulates water in Watts. The pump power and energy consumption is reduced by cycling when the amount of cooling needs to be restricted for control purposes.

#### Field: Secondary Fan Flow Rate

This field is used to specify the secondary fan flow rate and is specified in m^3^/s. This flow rate would typically be similar in magnitude to the flow through the primary side. This field can be autosized. When it is autosized, the program detects if the component is in the main air loop or on an outdoor air path.  If it is on the main air loop, then the flow rate is set to the [AirLoopHVAC](#airloophvac) system's design supply air flow rate (which is the maximum required for heating and cooling).  If it is on the outdoor air path, then the flow rate is set to the larger of either the design minimum outdoor air flow rate or one-half of the main air loop design flow rate. The flow rate is used to determine parasitic fan energy and does not impact the modeling of cooler effectiveness. The flow rate (and fan power) is effectively reduced by cycling when the amount of cooling needs to be restricted for control purposes.

#### Field: Secondary Fan Total Efficiency

This value is the overall efficiency of the fan, i.e., the ratio of the power delivered to the fluid to the electrical input power. It is the product of the motor efficiency and the impeller efficiency. The motor efficiency is the power delivered to the shaft divided by the electrical power input to the motor. The impeller efficiency is power delivered to the fluid (air) divided by the shaft power. The power delivered to the fluid is the mass flow rate of the air multiplied by the pressure rise divided by the air density. This input value must be between 0 and 1.

#### Field: Secondary Fan Delta Pressure

This field is used to specify the pressure difference in Pascals experienced by the secondary fan as it moves air through the wet side of the cooler.

#### Field: Primary Air Inlet Node Name

The name of the air inlet node for the primary air flow path through the cooler.

#### Field: Primary Air Outlet Node Name

The name of the air outlet node for the primary air flow path through the cooler.

#### Field: Control Type

This input field is not used by this model. But using this model does implement controlling of the primary outlet temperature.

#### Field: Dewpoint Effectiveness Factor

This field specifies an effectiveness that is applied to the dewpoint depression to determine a bound for the conditions leaving the cooler. The model uses the warmer of the two temperatures determined from wetbulb depression and dewpoint depression.

#### Field: Secondary Air Inlet Node Name

This field specifies the name of the node providing air to the secondary/wet side of the cooler. Typically this node could appear in an outdoor air node list or be part of an air system loop.

#### Field: Sensor Node Name

This field specifies the name of a node that will provide system air temperature setpoint information. A separate SetpointManager object should be setup to update this node.

#### Field: Relief Air Inlet Node Name

This field is optional, but can be used to feed two sources of secondary air into the wet side of the cooler. Typical use is to run the air system relief air into the system. The model first uses all of the air flow available from this node and then adds the air flow from the secondary air inlet node to make up the total defined by Secondary Fan Flow Rate.

#### Field: Water Supply Storage Tank Name

This field is optional. It is used to describe where the cooler obtains water used for evaporative cooling. If blank or omitted, then the cooler will obtain water directly from the mains. If the name of a [WaterUse:Storage](#waterusestorage) object is used here, then the cooler will obtain its water from that tank. If a tank is specified, the cooler will attempt to obtain all the water it uses from the tank. However, if the tank cannot provide all the water the cooler needs, then the cooler will still operate and obtain the rest of the water it needs from the mains.

#### Field: Drift Loss Fraction

This field is optional and can be used to model additional water consumed by the cooler from drift.  Drift is water that leaves the cooling media as droplets and does not evaporate into the process air stream.  For example, water may get blown off the evaporative media by winds and escape the air system.  The value entered here is a simple fraction of the water consumed by the cooler for normal process evaporation.  The amount of drift is this fraction times the water evaporated for the normal cooling process.  This field can be left blank and then there will be no added water consumption from drift.

#### Field: Blowdown Concentration Ratio

This field is optional and can be used to model additional water consumed by the cooler from blowdown.  Blowdown is water that is intentionally drained from the cooler's sump to offset the build up of solids in the water that would otherwise occur because of evaporation.  The value entered here is dimensionless.  It can be characterized as the ratio of solids in the blowdown water to solids in the make up water.  Typical values are 3 to 5.  The default is 3.0.

An IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

     EvaporativeCooler:Indirect:ResearchSpecial,
        Indirect Evap ZN1,       !- Name
        On_Except_Winter_Night,  !- Availability Schedule Name
        1.2,                     !- Cooler Maximum Effectiveness
        ,                        !- Cooler Flow Ratio
        30,                      !- Recirculating Water Pump Power Consumption {W}
        autosize,                !- Secondary Fan Flow Rate {m3/s}
        0.7,                     !- Secondary Fan Total Efficiency
        300,                     !- Secondary Fan Delta Pressure {Pa}
        Mixed Air Node ZN1,      !- Primary Air Inlet Node Name
        Evap Cooler Outlet Node ZN1,  !- Primary Air Outlet Node Name
        ,                        !- Control Type
        0.9,                     !- Dewpoint Effectiveness Factor
        Purge Air Inlet ZN1,     !- Secondary Air Inlet Node Name
        Air Loop Outlet Node ZN1,!- Sensor Node Name
        Relief Air Outlet Node ZN1;  !- Relief Air Inlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The output variables that are available for the [EvaporativeCooler:Indirect:ResearchSpecial](#evaporativecoolerindirectresearchspecial) object are shown below:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Evaporative Cooler Total Stage Effectiveness
    HVAC,Average,Evaporative Cooler Part Load Ratio
    HVAC,Average,Evaporative Cooler Dewpoint Bound Status
    HVAC,Sum,Evaporative Cooler Electric Energy [J]
    HVAC,Average,Evaporative Cooler Electric Power [W]
    HVAC,Sum,Evaporative Cooler Storage Tank Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Water Volume [m3]
    HVAC,Sum,Evaporative Cooler Starved Mains Water Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Evaporative Cooler Total Stage Effectiveness []

The Total Stage Efficiency is defined as the temperature change of the supply air divided by the difference between the outdoor dry-bulb and wet-bulb temperatures, including the effect of the reduction in flow because of the secondary air stream. In other words, it is a measure of the approach to the outdoor wet-bulb temperature.

#### Evaporative Cooler Part Load Ratio []

This output variable provides the part load fraction of the indirect cooler.  The ResearchSpecial cooler model is able to modulate to meet a temperature set point to avoid over cooling.  This output variable is the fraction formed by the ratio of the capacity needed over the maximum cooling capacity available.  A value of 1.0 corresponds to full capacity cooling.

#### Evaporative Cooler Dewpoint Bound Status []

This output variable is a flag that indicates if the modeling was based on dewpoint effectivenss rather than wetbulb effectiveness  The ResearchSpecial model is usually based on wet-bulb approach, but since values in excess of 1.0 are allowed, there is a secondary constraint imposed by dewpoint.  If the dewpoint effectiveness was applied, then this flag variable will have the value 1.0, otherwise it is 0.0.

#### Evaporative Cooler Electric Power [W]

#### Evaporative Cooler Electric Energy [J]

These output variables report the electric power and energy that are consumed by the secondary air fan and the sump pump.

#### Evaporative Cooler Water Volume [m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Mains Water Volume [m3]

This is the source of the water consumed.  This output variable appears when mains water is supplied to the cooler.

#### Evaporative Cooler Storage Tank Water Volume [m3]

The water consumption is the water evaporated from the pad.  This water consumption is only from the direct thermodynamics of water evaporation and does not include other sources of consumption such as drift or concentration blow down.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Water Volume [m3]

This is the water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.

#### Evaporative Cooler Starved Mains Water Volume [m3]

This is the source (mains) of water consumed by the evaporative cooler that could not accually be met by the storage tank.  This output variable appears when storage tank water is supplied to the cooler.