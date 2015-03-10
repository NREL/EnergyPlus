# Group – Heating and Cooling Coils

Several different coils may be added to zone equipment and air loops. This includes simple heating (gas, electric, and hot water) and a simple water-cooling coil as well as a more detailed flat fin water-cooling coil model. In general, the heating coil description can be used for a heating coil, a reheat coil, or a preheat coil in the air loop simulation or for zone equipment. Figure 135 is an example of a heating and cooling coil in the air loop simulation in a dual duct system. This does show the basic node structure that will be referenced in the input description. The figure does show water coils since they are the most complex to input in the simulation compared to the Electric and Gas coils which only have air connections.

![Example Air Loop Heating & Cooling Coil](media/example-air-loop-heating-cooling-coil.jpeg)


## Coil:Cooling:Water

The water cooling coil ([Coil:Cooling:Water](#coilcoolingwater)) has the ability to give detailed output with simplified inputs, inputting complicated coil geometry is not required by the user for this model instead the coil is sized in terms of auto-sizeable thermodynamic inputs. The coil requires thermodynamic inputs such as temperatures, mass flow rates and humidity ratios.

The coil is sized using auto-sized/user design input conditions and the UA values are calculated from the design conditions. A rough estimate of the coil area is provided along with percentage of surface wet and/or dry. This model uses the NTU-effectiveness approach to model heat transfer and has two types of flow arrangements cross-flow or counter-flow.

The basic underlying idea is - use auto sizable thermodynamic design inputs, calculate the coil UA's, use these UA values and operating conditions from the nodes connections, calculate the outlet stream conditions, and calculate the heat transfer rates.

See section "Cooling Coil Model" in the EnergyPlus Engineering Document for further details regarding this model.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a cooling coil. Any reference to this cooling coil by another object will use this name.

#### Field: Availability Schedule Name

Schedule that defines when the coil is available. The name of the schedule (ref: Schedule) that denotes whether the cooling coil can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during a given time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit is off. If this field is blank, the schedule has a value of 1 for all time periods.

#### Field: Design Water Flow Rate

The maximum possible water volume flow rate (m^3^/sec) through the coil. This is an auto sizable design input.

#### Field: Design Air Flow Rate

The maximum possible air volume flow rate (m^3^/sec) through the coil. This is an auto sizable design input.

#### Field: Design Inlet Water Temperature

The inlet water temperature for the design flow (C). This is an auto sizable design input.

#### Field: Design Inlet Air Temperature 

The inlet air temperature for the design flow (C). This is an auto sizable design input.

#### Field: Design Outlet Air Temperature

The outlet air condition desired for design flow (C). This is an auto sizable design input.

#### Field: Design Inlet Air Humidity Ratio

The highest value of humidity ratio possible for the Design inlet air stream (kgWater/kgDryAir). This is an auto sizable input.

#### Field: Design Outlet Air Humidity Ratio

The value of humidity ratio for the Design outlet air stream (kgWater/kgDryAir), it is an auto sizable input.

#### Field: Water Inlet Node Name

The name of the water coil inlet from the chilled water loop, i.e. Cooling Coil Water Inlet Node. It is from this node the operating inputs for water are transferred to the coil.

#### Field: Water Outlet Node Name

The name of the water coil outlet from the chilled water loop, i.e. Cooling Coil Water Outlet Node. It is from this node the operating output for water are reported to the coil.

#### Field: Air Inlet Node Name

The name of the air inlet to the water coil, i.e. Cooling Coil Air Inlet Node. It is from this node the operating inputs for air are transferred to the coil.

#### Field: Air Outlet Node Name

The name of the air outlet from the water coil, i.e. Cooling Coil Air Outlet Node. It is from this node the operating output for airside is reported to the coil.

#### Field: Type of Analysis

The coil has two modes of operation, termed as **SimpleAnalysis** and **DetailedAnalysis**. The difference between the two modes being, the simple mode reports the value of surface area fraction wet of the coil as dry or wet. The detailed mode give the exact value, however the execution time in detailed mode is noticeably higher.

#### Field: Heat Exchanger Configuration

The coil is operable in two modes, Cross Flow for general A/C applications and Counter flow mode. Air-conditioning systems generally use cross flow heat exchangers, hence the default is set to cross flow.

#### Field: Condensate Collection Water Storage Tank Name

This field is optional. It is used to describe where condensate from the coil is collected. If blank or omitted, then any coil condensate is discarded. Enter the name of Water Storage Tank object defined elsewhere and the condensate will then be collected in that tank.

Examples when auto sized in an IDF are as below:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:Water,
        Main Cooling Coil 1,    !- Coil Name
        CoolingCoilAvailSched,  !- Availability Schedule Name
        autosize,     !- Design Water Volume Flow Rate of Coil {m3/s}
        autosize,     !- Design Air Flow Rate of Coil {m3/s}
        autosize,     !- Design Inlet Water Temperature {C}
        autosize,     !- Design Inlet Air Temperature {C}
        autosize,     !- Design Outlet Air Temperature {C}
        autosize,     !- Design Inlet Air Humidity Ratio (kgWater/kgDryAir)
        autosize,     !- Design Outlet Air Humidity Ratio (kgWater/kgDryAir)
        Main Cooling Coil 1 Water Inlet Node,   !- Coil_Water_Inlet_Node
        Main Cooling Coil 1 Water Outlet Node,  !- Coil_Water_Outlet_Node
        Mixed Air Node 1,                       !- Coil_Air_Inlet_Node
        Main Cooling Coil 1 Outlet Node;        !- Coil_Air_Outlet_Node
~~~~~~~~~~~~~~~~~~~~

Examples when values (hard-sized) are input in an IDF are as below:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:Water,
        Main Cooling Coil 1,    !- Coil Name
        CoolingCoilAvailSched,  !- Availability Schedule Name
        0.0022,    !- Design Water Volume Flow Rate of Coil {m3/s}
        1.45,      !- Design Air Flow Rate of Coil {m3/s}
        6.1,       !- Design Inlet Water Temperature {C}
        25,        !- Design Inlet Air Temperature {C}
        10,        !- Design Outlet Air Temperature {C}
        0.012,     !- Design Inlet Air Humidity Ratio
        0.008,     !- Design Outlet Air Humidity Ratio
        Main Cooling Coil 1 Water Inlet Node,   !- Coil_Water_Inlet_Node
        Main Cooling Coil 1 Water Outlet Node,  !- Coil_Water_Outlet_Node
        Mixed Air Node 1,                       !- Coil_Air_Inlet_Node
        Main Cooling Coil 1 Outlet Node;        !- Coil_Air_Outlet_Node
~~~~~~~~~~~~~~~~~~~~

### Outputs

Following are the list of possible output output variables from this coil model:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Cooling Coil Total Cooling Energy [J]
    HVAC,Sum,Cooling Coil Sensible Cooling Energy [J]
    HVAC,Average,Cooling Coil Total Cooling Rate [W]
    HVAC,Average,Cooling Coil Sensible Cooling Rate [W]
    HVAC,Average,Cooling Coil Wetted Area Fraction []
    HVAC,Average,Cooling Coil Condensate Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Coil Source Side Heat Transfer Energy [J]
    HVAC,Sum,Cooling Coil Condensate Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Coil Total Cooling Energy (J)

Cooling Coil Total Cooling Energy is the total amount of heat transfer taking place in the coil at the operating conditions.

#### Cooling Coil Sensible Cooling Energy (J)

Cooling Coil Sensible Cooling Energy is the total amount of Sensible heat transfer taking place in the coil at the operating conditions. It only takes into account temperature difference in the inlet and outlet air streams at operating conditions.

#### Cooling Coil Total Cooling Rate (W)

Cooling Coil Total Cooling Rate is the Rate of heat transfer taking place in the coil at the operating conditions. The units are (J/sec) or Watts.

#### Cooling Coil Sensible Cooling Rate (W)

Cooling Coil Sensible Cooling Rate is the Rate of Sensible heat transfer taking place in the coil at the operating conditions.

#### Cooling Coil Wetted Area Fraction []

It defines the fraction of total surface area of coil which is wet due to moisture condensation on the surface of the coil. Value varies between 0.0 and 1.0.

In addition, if a Water Storage Tank is used to collect coil condensate, then the following outputs will be available.

#### Cooling Coil Condensate Volume Flow Rate [m3/s]

#### Cooling Coil Condensate Volume [m3]

These reports provide the rate and amount of condensate from the coil. Condensate is water condensed out of the air as a result of cooling. The condensate volume is also reported on the meter for "OnSiteWater."

#### Cooling Coil Source Side Heat Transfer Energy [J]

This is the energy extracted from the chilled water serving the coil, in Joules.

## Coil:Heating:Water

This simple heating coil model only does sensible heating of the air.  The simple heating coil uses the Effectiveness-NTU algorithm and assumes a cross-flow heat exchanger.

### Inputs

#### Field: Name

A unique identifying name for each coil.

#### Field: Availability Schedule Name

Schedule that defines when the coil is available. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during a given time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit is off. If this field is blank, the schedule has a value of 1 for all time periods.

#### Field: U-Factor Times Area Value

The UA value for the coil needed for the Effectiveness-NTU heat exchanger model.  An estimate of the UA can be obtained from:

![](media/image337.png)\


where q is the heat transferred from water to the air in watts; T~water, avg~ is the average water temperature in degrees C; and T~air, avg~ is the average air temperature in degrees C. Or the LMTD temperature difference can be used. This field is used when *Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate*.This field is autosizable.

#### Field: Maximum Water Flow Rate

The maximum possible water flow rate (m^3^/sec) through the coil. This field is used when *Coil Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate*. This field is autosizable.

#### Field: Water Inlet Node Name

The name of the coil's water inlet node from the hot water loop.

#### Field: Water Outlet Node Name

The name of the coil's water outlet node from the hot water loop.

#### Field: Air Inlet Node Name

The name of the air inlet node to the water coil.

#### Field: Air Outlet Node Name

The name of the air outlet node from the water coil.

#### Field: Performance Input Method

The user can choose either *UFactorTimesAreaAndDesignWaterFlowRate* or *NominalCapacity*. If *UFactorTimesAreaAndDesignWaterFlowRate* is selected, the user must input values for UA of the Coil and Max Water FlowRate of the Coil (and Rated Capacity is ignored). If *NominalCapacity* is chosen, the user must input a Rated Capacity for the coil; UA of the Coil and Max Water FlowRate of the Coil will be ignored. Rated capacity is defined as the heating capacity in watts of the coil at the rating points (i.e., the rated inlet and outlet water/air temperatures defined in the input fields below). The rated capacity is used to calculate a water mass flow rate and a UA for the coil. The default is *NominalCapacity*.

To autosize the capacity, choose *UfactorTimesAreaAndDesignWaterFlowRate* and put autosize as the inputs for *U-Factor Times Area Value, Maximum Water Flow Rate,* and *Rated Capacity*. The program will use the Sizing inputs to size the coil. The rated temperatures (see below) are ignored in autosizing. These are used only when the user is specifying coil performance using the *NominalCapacity* input method.

#### Field: Gross Rated Heating Capacity

The heating capacity of the coil in watts at the rated inlet and outlet air and water temperatures.  The gross rated heating capacity does not account for the effect of supply air fan heat. This field is used when the *Performance Input Method* = *Nominal Capacity*. This field is autosizable. The rating points are given in the four subsequent input fields.

#### Field: Rated Inlet Water Temperature

The inlet water temperature (degrees C) corresponding to the rated heating capacity. The default is 82.2 degrees C (180 degrees F).

#### Field: Rated Inlet Air Temperature

The inlet air temperature (degrees C) corresponding to the rated heating capacity. The default is 16.6 degrees C (60 degrees F).

#### Field: Rated Outlet Water Temperature

The outlet water temperature (degrees C) corresponding to the rated heating capacity. The default is 71.1 degrees C (160 degrees F).

#### Field: Rated Outlet Air Temperature

The outlet air temperature (degrees C) corresponding to the nominal heating capacity. The default is 32.2 degrees C (90 degrees F).

#### Field: Rated Ratio for Air and Water Convection

This is the ratio of convective heat transfers between air side and water side of the heating coil at the rated operating conditions.  The default is .5.  This ratio describes the geometry and the design of the coil and is defined by:

![](media/image338.png)\


where,

![](media/image339.png)  is the fin efficiency, (dimensionless)

*h* is the surface convection heat transfer coefficient

*A* is the surface area

An example input of the object is:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Water,
        SPACE3-1 Zone Coil,      !- Coil Name
        ReheatCoilAvailSched,    !- Availability Schedule Name
        ,                        !- UA of the Coil {W/K}
        ,                        !- Max Water Flow Rate of Coil {m3/s}
        SPACE3-1 Zone Coil Water In Node,  !- Coil_Water_Inlet_Node
        SPACE3-1 Zone Coil Water Out Node,  !- Coil_Water_Outlet_Node
        SPACE3-1 Zone Coil Air In Node,  !- Coil_Air_Inlet_Node
        SPACE3-1 In Node,        !- Coil_Air_Outlet_Node
        NominalCapacity,         !- Coil Performance Input Method
        10000.,                  !- Gross Rated Heating Capacity
        0.55;                    !- Rated Ratio for Air and Water Convection
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum, Heating Coil Heating Energy [J]
    HVAC,Sum,Heating Coil Source Side Heat Transfer Energy [J]
    HVAC,Average,Heating Coil Heating Rate [W]
    HVAC,Average, Heating Coil U Factor Times Area Value [W/K]
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Heating Energy [J]

Heating Coil Heating Energy is the total amount of heat transfer taking place in the coil at the operating conditions.

#### Heating Coil Heating Rate [W]

Heating Coil Heating Rate is the Rate of heat transfer taking place in the coil at the operating conditions. The units are (J/sec) or Watts.

#### Heating Coil U Factor Times Area Value [W/K]

This characterizes the overall heat transfer "UA" value, or U-factor times Area.  The simple heating coil model adjusts UA value based on inlet temperatures and flow rates and this output contains the results from that adjustment.

#### Heating Coil Source Side Heat Transfer Energy [J]

This is the same has the Heating Coil Heating Energy but it is also metered as a plant loop heating demand. This represents the heat in Joules extracted from the hot water serving the coil.

## Coil:Heating:Steam

The simple steam to air heating coil model only does sensible heating of the air. The steam to air coils condenses the steam and sub cools steam at loop pressure and discharges the condensate through steam traps at low pressure condensate line.

### Inputs

#### Field: Name

A unique identifying name for each steam coil.

#### Field: Availability Schedule Name

Schedule that defines when the coil is available. If the schedule's value is less than or equal to  0.0, then the coil is not available and flow will not be requested. If the schedule's value is > 0.0 (usually 1 is used), the coil is available. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Maximum Steam Flow Rate

The maximum possible steam volumetric flow rate in m^3^/s through the steam heating coil. The steam volumetric flow rate is calculated at 100C and 101325 Pa. This field is autosizable.

#### Field: Degree of SubCooling

Ideally the steam trap located at the outlet of steam coil should remove all the condensate immediately, however there is a delay in this process in actual systems which causes the condensate to SubCool by certain degree in the coil before leaving the coil, this SubCool occurs in the steam coil and this SubCool-heat is added to the zone. The minimum value is 2 Celsius and default is 5 Celsius.

#### Field: Degree of Loop SubCooling

This essentially represents the heat loss to the atmosphere due to uninsulated condensate return piping to the boiler. Condensate return piping operates at atmospheric pressure and is not insulated. The condensate sub cools to certain degree before it is pumped back to the boiler. The minimum value is 10 Celsius and default is 20 Celsius.

#### Field: Water Inlet Node Name

The name of the steam coil inlet from the steam loop, i.e. Steam Coil steam inlet node.

#### Field: Water Outlet Node Name

The name of the steam coil outlet to the condensate loop, i.e. Steam Coil condensate outlet node.

#### Field: Air Inlet Node Name

The name of the air inlet to the steam coil, i.e. Steam Coil Air Inlet Node.

#### Field: Air Outlet Node Name

The name of the air outlet from the steam coil, i.e. Steam Coil Air Outlet Node.

#### Field: Coil Control Type

Choice of either **ZoneLoadControl** steam coil or **TemperatureSetpointControl** steam coil. A zone coil is load controlled and an air loop steam coil is temperature controlled.

#### Field: Temperature Setpoint Node Name

If the coil is used in the air loop simulation and is temperature controlled using a Set Point Manager (i.e., the previous file is TemperatureSetpointConrtol), then the node that is the control node needs to be specified here. If the coil is used in an air terminal unit, the coil is load controlled and a control node set point is not required (i.e., the previous field is ZoneLoadControl).

An example of a Steam Coil input statement (one each for Temperature Controlled and Load Controlled) from an IDF is given below:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Steam,
        VAV SYS 1 Heating Coil,          !- Coil Name
        ReheatCoilAvailSched,            !- Availability Schedule Name
        0.06,                            !- Max Steam volume Flow rate
        5.0,                             !- Deg of Subcooling Desired
        15.0,                    !- Loop Subcooling Desired
        VAV SYS 1 Heating Coil Steam Inlet,  !- Coil Steam Inlet Node
        VAV SYS 1 Heating Coil Steam Outlet, !- Coil Water Outlet Node
        VAV SYS 1 Cooling Coil Outlet,    !- Coil Air Inlet Node
        VAV SYS 1 Heating Coil Outlet,    !- Coil Air Outlet Node
        TemperatureSetPointControl,       !- field Coil Control Type
        VAV SYS 1 Heating Coil Outlet;    !- field Coil Temp Setpoint Node

    Coil:Heating:Steam,
        SPACE1-1 Reheat Coil,               !- Coil Name
        ReheatCoilAvailSched,               !- Availability Schedule Name
        autosize,                       !- Max Steam volume Flow rate
        5.0,                       !- Deg of Subcooling Desired
        15.0,                       !- Loop Subcooling Desired
        SPACE1-1 Reheat Coil Steam Inlet,   !- Coil Steam Inlet Node
        SPACE1-1 Reheat Coil Steam Outlet,  !- Coil Water Outlet Node
        SPACE1-1 Damper Outlet,             !- Coil Air Inlet Node
        SPACE1-1 Supply Inlet,  !- Coil Air Outlet Node
        ZoneLoadControl;     !- field Coil Control Type
~~~~~~~~~~~~~~~~~~~~

## Heating Coil (Steam) Outputs:

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum, Heating Coil Heating Energy [J]
    HVAC,Average,Total Steam Coil Heating Rate [W]
    HVAC,Average,Heating Coil Steam Trap Loss Rate [W]
    HVAC, Average, Heating Coil Steam Inlet Temperature [C]
    HVAC, Average, Heating Coil Steam Outlet Temperature [C]
    HVAC, Average, Heating Coil Steam Mass Flow Rate [kg/s]
~~~~~~~~~~~~~~~~~~~~

### Heating Coil Heating Energy [J]

Heating Coil Heating Energy is the total amount of heat transfer taking place in the coil at the operating conditions.

### Heating Coil Heating Rate [W]

Heating Coil Heating Rate is the Rate of heat transfer taking place in the coil at the operating conditions. The units are (J/sec) or Watts.

### Heating Coil Steam Trap Loss Rate [W]

Loop losses represent the unavoidable loss due to degree of sub cooling in the condensate return piping back to the boiler and the loss occurring due to flashing of steam across the steam trap due to pressure differential between the steam and the condensate side.

### Heating Coil Steam Inlet Temperature [C]

### Heating Coil Steam Outlet Temperature [C]

### Heating Coil Steam Mass Flow Rate [kg/s]

These outputs are the Steam inlet and condensate outlet temperatures and steam flow rate for the boiler.

## Coil:Heating:Electric

The electric heating coil is a simple capacity model with a user-inputted efficiency. In many cases, this efficiency for the electric coil will be 100%. This coil will be simpler than shown in Figure 135 since it will only have air nodes to connect it in the system. The coil can be used in the air loop simulation or in the zone equipment as a reheat coil. Depending on where it is used determines if this coil is temperature or capacity controlled. If used in the air loop simulation it will be controlled to a specified temperature scheduled from the SetPoint Manager. If it is used in zone equipment, it will be controlled from the zone thermostat by meeting the zone demand.

### Inputs

#### Field: Name

A unique identifying name for each coil.

#### Field: Availability Schedule Name

Schedule that defines when the coil is available. If the schedule's value is 0.0, then the coil is not available and flow will not be requested. If the schedule's value is > 0.0 (usually 1 is used), the coil is available. If this field is blank, the schedule has values of 1 for all time periods. Schedule values must be >= 0 and <= 1.

#### Field: Efficiency

This is user-inputted efficiency (decimal units, not percent) and can account for any loss. In most cases for the electric coil, this will be 100%.

#### Field: Nominal Capacity

This is the maximum capacity of the coil (W). This controlled coil will only provide the needed capacity to meet the control criteria whether it is temperature or capacity controlled. This field is autosizable.

#### Field: Air Inlet Node Name

The name of the air inlet to the electric coil, i.e. Heating Coil Air Inlet Node.

#### Field: Air Outlet Node Name

The name of the air outlet from the electric coil, i.e. Heating Coil Air Outlet Node.

#### Field: Temperature Setpoint Node Name

If the coil is used in the air loop simulation and is temperature controlled using a Set Point Manager, then the node that is the control node needs to be specified here. If the coil is used in an air terminal unit, the coil is load controlled and a control node set point is not required.

An example of IDF usage:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Electric, AHU Reheater,           !- Name 2,                      !- Availability Schedule Name 0.99,                   !- Efficiency 600000,                 !- Nominal Capacity {W} DOAS Supply Fan Outlet, !- Air Inlet Node Name AHU Reheater Outlet,    !- Air Outlet Node Name AHU Reheater Outlet;    !- Temperature Setpoint Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Heating Coil Air Heating Energy [J]
    HVAC,Average,Heating Coil Air Heating Rate [W]
    HVAC,Sum,Heating Coil Electric Energy [J]
    HVAC,Average,Heating Coil Electric Power [W]
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Air Heating Energy (J)

Heating Coil Air Heating Energy is the total amount of heat transfer taking place in the coil at the operating conditions.

#### Heating Coil Air Heating Rate [W]

Heating Coil Air Heating Rate is the Rate of heat transfer taking place in the coil at the operating conditions. The units are (J/sec) or Watts.

#### Heating Coil Electric Energy [J]

Heating Coil electric consumption after the efficiency of the coil has been taken into account in Joules for the timestep reported.

#### Heating Coil Electric Power [W]

This field is the average Heating Coil electric power after the efficiency of the coil has been taken into account in Watts for the timestep reported.

## Coil:Heating:Electric:MultiStage

The multi stage electric heating coil is a simple capacity model with a user-inputted efficiencies at different stages. In many cases, the efficiencies for the electric coil will be 100%. This coil will only have air nodes to connect it in the system. The coil can be used in the air loop simulation or in the zone equipment as a reheat coil. Depending on where it is used determines if this coil is temperature or capacity controlled. If used in the air loop simulation it will be controlled to a specified temperature scheduled from the SetPoint Manager. If it is used in zone equipment, it will be controlled from the zone thermostat by meeting the zone demand. For the time being, this coil model can only be called by the parent object [AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed](#airloophvacunitaryheatpumpairtoairmultispeed).

### Inputs

#### Field: Name

A unique identifying name for each coil.

#### Field: Availability Schedule Name

Schedule that defines when the coil is available. If the schedule's value is 0.0, then the coil is not available and flow will not be requested. If the schedule's value is > 0.0 (usually 1 is used), the coil is available. If this field is blank, the schedule has values of 1 for all time periods. Schedule values must be >= 0 and <= 1.

#### Field: Air Inlet Node Name

The name of the air inlet to the electric coil, i.e. Heating Coil Air Inlet Node.

#### Field: Air Outlet Node Name

The name of the air outlet from the electric coil, i.e. Heating Coil Air Outlet Node.

#### Field: Temperature Setpoint Node Name

If the coil is used in the air loop simulation and is temperature controlled using a Set Point Manager, then the node that is the control node needs to be specified here. If the coil is used in an air terminal unit, the coil is load controlled and a control node set point is not required. At present, the multistage electric heating coil does not model temperature setoint control.

#### Field: Stage 1 Efficiency

This is stage 1 user-inputted efficiency (decimal units, not percent) and can account for any loss. In most cases for the electric coil, this will be 100%.

#### Field: Stage 1 Nominal Capacity

This is stage 1 capacity of the coil (W). This field is autosizable.

#### Field: Stage 2 Efficiency

This is stage 2 user-inputted efficiency (decimal units, not percent) and can account for any loss. In most cases for the electric coil, this will be 100%.

#### Field: Stage 2 Nominal Capacity

This is stage 2 capacity of the coil (W). This field is autosizable.

#### Field: Stage 3 Efficiency

This is stage 3 user-inputted efficiency (decimal units, not percent) and can account for any loss. In most cases for the electric coil, this will be 100%.

#### Field: Stage 3 Nominal Capacity

This is stage 3 capacity of the coil (W). This field is autosizable.

#### Field: Stage 4 Efficiency

This is stage 4 user-inputted efficiency (decimal units, not percent) and can account for any loss. In most cases for the electric coil, this will be 100%.

#### Field: Stage 4 Nominal Capacity

This is stage 4 capacity of the coil (W). This field is autosizable.

An example in IDF form:

~~~~~~~~~~~~~~~~~~~~

      Coil:Heating:Electric:MultiStage,
        Heat Pump Heating Coil 1,         !- Name
        FanAndCoilAvailSched,             !- Availability Schedule Name
        Heating Coil Air Inlet Node,      !- Air Inlet Node Name
        SuppHeating Coil Air Inlet Node,  !- Air Outlet Node Name
        ,                        !- Temp Setpoint node name
        3,                       !- Number of Stages
        1,                       !- Stage 1 Efficiency
        Autosize,                !- Stage 1 Nominal Capacity {W}
        1,                       !- Stage 2 Efficiency
        Autosize,                !- Stage 2 Nominal Capacity {W}
        1,                       !- Stage 3 Efficiency
        Autosize;                !- Stage 3 Nominal Capacity {W}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Heating Coil Energy[J]
    HVAC,Average,Heating Coil Rate[W]
    HVAC,Sum,Heating Coil Electric Consumption [J]
    HVAC,Average,Heating Coil Electric Power [W]
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Energy (J)

Heating Coil Energy is the total amount of heat transfer taking place in the coil at the operating conditions.

#### Heating Coil Rate[W]

Heating Coil Rate is the Rate of heat transfer taking place in the coil at the operating conditions. The units are (J/sec) or Watts.

#### Heating Coil Electric Consumption [J]

Heating Coil electric consumption after the efficiency of the coil has been taken into account in Joules for the timestep reported.

#### Heating Coil Electric Power [W]

This field is the average Heating Coil electric power after the efficiency of the coil has been taken into account in Watts for the timestep reported.

## Coil:Heating:Desuperheater

A simplified approach is used to determine the performance of this air heating coil. The model assumes that the heating energy provided by this coil is reclaimed from the superheated refrigerant gas leaving a compressor (i.e., a desuperheating refrigerant-to-air heating coil) and does not impact the performance of the compressor. This coil can be used in air loop simulations but can't be used by certain compound objects (e.g., [AirLoopHVAC:UnitaryHeatPump:AirToAir](#airloophvacunitaryheatpumpairtoair), [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair), or [Dehumidifier:Desiccant:NoFans](#dehumidifierdesiccantnofans)) or any air distribution equipment (e.g., [AirTerminal:SingleDuct:ConstantVolume:Reheat](#airterminalsingleductconstantvolumereheat), [AirTerminal:SingleDuct:VAV:Reheat](#airterminalsingleductvavreheat), or [AirTerminal:SingleDuct:SeriesPIU:Reheat](#airterminalsingleductseriespiureheat)).

The desuperheater heating coil input requires a coil name, an availability schedule, and a heat reclaim recovery efficiency. The reclaim recovery efficiency determines the amount of heat available for use by this heating coil. Approximately 25-30% of the energy rejected by typical refrigeration system condensers is to reduce the superheated refrigerant vapor temperature to the condensing temperature. Recovery efficiencies higher than 30% may cause the refrigerant gas to condense which in turn impacts the performance of the refrigeration system. For this reason, the maximum heat reclaim recovery efficiency for this coil is 30% for most sources of waste heat, including refrigeration compressor racks. The one exception to this 30% limit is a condenser that is part of a detailed refrigeration system. In a detailed refrigeration system, the portion of the rejected heat that lies within the superheated region is explicitly calculated. Therefore, the desuperheater coils supplied by a condenser attached to a detailed refrigeration system are subject to a maximum reclaim recovery efficiency of 90% of the heat within the superheated region.

The next two input items for the desuperheater heating coil are the node names for the inlet and outlet air streams. The following two input fields define the source of heating energy for the coil. This desuperheater heating coil may only be used with direct expansion (DX) cooling or refrigeration equipment. The first of these two inputs is the heating source object type while the second defines the name of the heating source. For proper modeling, the desuperheater heating coil must be placed downstream of a DX cooling coil when reclaiming heat from that cooling coil. Desuperheating heating coil placement is unrestricted when reclaiming heat from a  [Refrigeration:CompressorRack](#refrigerationcompressorrack)  or Refrigeration:Condenser.

The next input field is optional and defines the set point node name if the desuperheater heating coil is to be controlled based on temperature. When a load-based control scheme is used, this field is left blank. A final optional input is used to model parasitic electric energy use of auxiliary equipment associated with the desuperheater heating coil (e.g., solenoid valve).

### Inputs

#### Field: Name

This alpha field defines a unique user-assigned name for an instance of a desuperheater heating coil. Any reference to this desuperheater heating coil by another object will use this name.

#### Field: Availability Schedule Name

This alpha field defines the name of the schedule (ref: Schedule) that denotes whether the desuperheater heating coil can run during a given time period. Schedule values must range from 0 to 1. A schedule value greater than 0 indicates that the coil can operate during the time period. A value equal to 0 denotes that the coil must be off for that time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Heat Reclaim Recovery Efficiency

This numeric field defines the ratio of recovered waste heat from the superheated refrigerant gas to the total rejected waste heat from the heating source (as if no heat reclaim occurred). Values can range from 0.0 up to a maximum of 0.9 if the source is a refrigeration condenser and 0.3 for all other waste heat sources. If this input field is left blank, the default value is 0.8 for a refrigeration condenser source type and 0.25 for all other sources.

#### Field: Air Inlet Node Name

This alpha field defines the name of the HVAC system node from which the desuperheater heating coil draws its inlet air.

#### Field: Air Outlet Node Name

This alpha field defines the name of the HVAC system node to which the desuperheater heating coil sends its outlet air.

#### Field: Heating Source Object Type

This alpha field defines the source of superheated refrigerant gas from which the desuperheater heating coil recovers energy. Valid choices are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:SingleSpeed
    Coil:Cooling:DX:TwoSpeed
    Coil:Cooling:DX:TwoStageWithHumidityControlMode
    Refrigeration:CompressorRack
    Refrigeration:Condenser:AirCooled
    Refrigeration:Condenser:EvaporativeCooled
    Refrigeration:Condenser:WaterCooled
~~~~~~~~~~~~~~~~~~~~

When the heating coil source is a DX Coil, the air loop's supply air fan control mode may be auto fan (cycling fan cycling coil), constant fan, or variable volume. When the heating source is a compressor rack for refrigerated cases or a refrigeration condenser, the supply air fan control mode should be either variable volume or constant fan.

> NOTE: Use of the desuperheater heating coil in variable air volume systems should be done with caution since the model assumption of a fixed heat reclaim recovery efficiency may not be valid if the air flow rate over the coil varies significantly.

#### Field: Heating Source Name

This alpha field defines the name of the desuperheater heating coil source (e.g., the name of a specific valid coil (as mentioned in the previous field) which provides waste heat to this desuperheater heating coil).

> NOTE: When the heating source is a  Refrigeration Compressor rack, the heat rejection location in the [Refrigeration:CompressorRack](#refrigerationcompressorrack) object must be "Outdoors". If the compressor rack heat rejection location is "[Zone](#zone)", the total amount of heat rejection available for reclaim (e.g., by this desuperheater heating coil) is set to zero by the compressor rack object and the simulation proceeds.

#### Field: Temperature Setpoint Node Name

This optional alpha field defines the name of the HVAC system node used for temperature-based control (e.g., controlling the heating coil's outlet air dry-bulb temperature to a setpoint). If the desuperheater heating coil is temperature controlled through the use of a Set Point Manager, then the control node specified in the Set Point Manager will be entered here. If the desuperheater heating coil is controlled based on a certain heating load to be met (e.g., using this heating coil as part of [AirLoopHVAC:Unitary:Furnace:HeatCool](#airloophvacunitaryfurnaceheatcool) for high humidity control), this field should be left blank.

#### Field: Parasitic Electric Load

This optional numeric field defines the parasitic electric load (in Watts) due to control valves or other devices specific to the desuperheater heating coil. The load is applied whenever the coil is heating the air. The model assumes that this electric load is small and does not contribute to heating the air.

Following is an example input for a desuperheater heating coil.

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:Desuperheater,
      DesuperheaterCoil,                  !- Coil Name
      FanAndCoilAvailSched,               !- Availability Schedule
      0.3,                                !- Heat Reclaim Recovery Efficiency
      Cooling Coil Air Outlet Node,       !- Coil Air Inlet Node Name
      Air Loop Outlet Node,               !- Coil Air Outlet Node Name
      Coil:Cooling:DX:SingleSpeed, !- Heating Source Type
      Furnace ACDXCoil 1,                 !- Heating Source Name
      ,                                   !- Coil Temperature Setpoint Node Name
      0.1;                                !- Parasitic Electric Load {W}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Heating Coil Rate [W]
    HVAC,Sum,Heating Coil Energy [J]
    HVAC,Average,Heating Coil Electric Power [W]
    HVAC,Sum,Heating Coil Electric Consumption [J]
    HVAC,Average,Heating Coil Runtime Fraction []
    HVAC,Average,Heating Coil Air Heating Rate [W]
    HVAC,Sum,Heating Coil Air Heating Energy [J]
    HVAC,Average,Heating Coil Electric Power [W]
    HVAC,Sum,Heating Coil Electric Energy [J]
    HVAC,Average,Heating Coil Runtime Fraction
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Air Heating Rate [W]

This output is the average heating rate of the desuperheater heating coil in Watts over the timestep being reported. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil.

#### Heating Coil Air Heating Energy [J]

This output is the total heating output of the desuperheater heating coil in Joules over the timestep being reported. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = HeatingCoils, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Electric Power [W]

This output is the average electric consumption rate for the parasitic load associated with the desuperheater heating coil in Watts.

#### Heating Coil Electric Energy [J]

This output is the electric consumption of the desuperheater heating coil parasitic load in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Runtime Fraction []

This is the runtime fraction of the desuperheater heating coil for the timestep being reported. Since the desuperheater heating coil can only provide heat when the heat source object is active, the runtime fraction of the desuperheater heating coil will always be less than or equal to the runtime fraction of the heat source object.

## Coil:Cooling:DX:VariableRefrigerantFlow

The variable refrigerant flow (VRF) DX cooling coil model is nearly identical to the single-speed DX cooling coil model (Ref. [Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed)). For this reason, an adaptation of the single-speed DX cooling coil model is used to model the variable-speed compression system used in VRF AC systems. The model inputs are quite similar to the input requirements for the single-speed DX cooling coil model, however, the location of a majority of the inputs have been moved to the variable refrigerant flow air conditioner object since multiple DX cooling coils will use the same DX compression system (Ref. [AirConditioner:VariableRefrigerantFlow](#airconditionervariablerefrigerantflow)).

### Inputs

#### Field: Coil Name

This alpha field defines a unique user-assigned name for an instance of a VRF DX cooling coil. Any reference to this DX cooling coil by another object will use this name. This cooling coil name must be entered in the [AirConditioner:VariableRefrigerantFlow](#airconditionervariablerefrigerantflow) object. No other sytem type uses this specific coil.

#### Field: Availability Schedule Name

This alpha field defines the name of the DX cooling coil availabiltiy schedule. Schedule values of 0 denote the DX cooling coil is off. A schedule value greater than 0 indicates that the coil can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Gross Rated Total Cooling Capacity

This numeric field defines the gross rated total cooling capacity of the DX cooling coil in watts. The total cooling capacity should be a "gross", i.e., the effect of supply air fan heat NOT accounted for.

#### Field: Gross Ratio Sensible Heat Ratio

This numeric field defines the gross sensible heat ratio (sensible capacity divided by total cooling capacity) of the DX cooling coil at rated conditions. Both the sensible and total cooling capacities used to define the Rated SHR should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for)

#### Field: Rated Air Flow Rate

The air volume flow rate, in m^3^ per second, across the DX cooling coil at rated conditions. The rated air volume flow rate should be between 0.00004027 m^3^/s and 0.00006041 m^3^/s per watt of rated total cooling capacity (300 to 450 cfm/ton). The gross rated total cooling capacity and gross rated SHR should be performance information for the unit with at this rated air volume flow rate.

#### Field: Cooling Capacity Ratio Modifier Function of Temperature Curve Name

This alpha field defines the cooling capacity ratio modifier as a function of indoor wet-bulb temperature or indoor wet-bulb and outdoor dry-bulb temperatures. This curve is a linear, quadratic, or cubic curve if the cooling capacity is soley a function of indoor wet-bulb temperature (i.e., the indoor terminal units weighted average inlet air wet-bulb temperatures). Without specific manufacturers data indicating otherwise, the use of a single independent variable is recommended for this coil type. If, however, the user has reason to believe the cooling capacity is both a function of indoor wet-bulb temperature and outdoor dry-bulb temperature (and has manufacturers data to create the performance curve), a bi-quadratic equation using weighted average indoor wet-bulb temperature and condenser entering air dry-bulb temperature as the independent variables may be used. See the Engineering Reference for more discussion on using this input field.

#### Field: Cooling Capacity Modifier Curve Function of Flow Fraction Name

This alpha field defines the name of a linear, quadratic or cubic performance curve (ref:  Performance Curves) that parameterizes the variation of total cooling capacity as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated total cooling capacity and the total cooling capacity modifier curve (function of temperature) to give the gross total cooling capacity at the specific temperature and air flow conditions at which the coil is operating. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Coil Air Inlet Node Name

This alpha field defines the name of the air inlet node entering the DX cooling coil.

#### Field: Coil Air Outlet Node Name

This alpha field defines the name of the air outlet node exiting the DX cooling coil.

#### Field: Name of Water Storage Tank for Condensate Collection

This field is optional. It is used to describe where condensate from the coil is collected. If blank or omitted, then any coil condensate is discarded. Enter the name of Water Storage Tank object defined elsewhere and the condensate will then be collected in that tank.

Following is an example input for a [Coil:Cooling:DX:VariableRefrigerantFlow](#coilcoolingdxvariablerefrigerantflow) object.

~~~~~~~~~~~~~~~~~~~~

    COIL:Cooling:DX:VariableRefrigerantFlow,
        TU1 VRF DX Cooling Coil, !- Coil Name
        VRFAvailSched,           !- Availability Schedule Name
        autosize,                !- Gross Rated Total Cooling Capacity {W}
        autosize,                !- Gross Rated Sensible Heat Ratio
        autosize,                !- Rated Air Flow Rate {m3/s}
        VRFTUCoolCapFT,          !- Cooling Capacity Ratio Modifier Function of Temperature Curve Name
        VRFACCoolCapFFF,         !- Cooling Capacity Modifier Curve Function of Flow Fraction Name
        TU1 VRF DX CCoil Inlet Node,  !- Coil Air Inlet Node
        TU1 VRF DX CCoil Outlet Node;  !- Coil Air Outlet Node
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Cooling Coil Total Cooling Rate [W]
    HVAC,Sum, Cooling Coil Total Cooling Energy [J]
    HVAC,Average, Cooling Coil Sensible Cooling Rate [W]
    HVAC,Sum, Cooling Coil Sensible Cooling Energy [J]
    HVAC,Average, Cooling Coil Latent Cooling Rate [W]
    HVAC,Sum, Cooling Coil Latent Cooling Energy [J]
    HVAC,Average, Cooling Coil Runtime Fraction []

    Evaporative-cooled condenser:
    HVAC,Average,Cooling Coil Condensate Volume Flow Rate [m3/s]
    Zone,Meter,Condensate:OnSiteWater [m3]
    HVAC,Sum,Cooling Coil Condensate Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Coil Total Cooling Rate [W]

This field is the total (sensible and latent) cooling rate output of the DX coil in Watts. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Total Cooling Energy [J]

This is the total (sensible plus latent) cooling output of the DX coil in Joules over the time step being reported. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = CoolingCoils, Group Key = System (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Cooling Coil Sensible Cooling Rate [W]

This output is the moist air sensible cooling rate output of the DX coil in Watts. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Sensible Cooling Energy [J]

This is the moist air sensible cooling output of the DX coil in Joules for the time step being reported. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Latent Cooling Rate [W]

This is the latent cooling rate output of the DX coil in Watts. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Latent Cooling Energy [J]

This is the latent cooling output of the DX coil in Joules for the time step being reported. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Runtime Fraction []

This is the runtime fraction of the DX coil compressor and condenser fan(s) for the time step being reported.

#### Cooling Coil Condensate Volume Flow Rate [m3/s]

#### Cooling Coil Condensate Volume [m3]

These outputs are the rate and volume of water collected as condensate from the coil. These reports only appear if a water storage tank is named in the input object.

## Coil:Heating:DX:VariableRefrigerantFlow

The variable refrigerant flow (VRF) DX heating coil model uses performance information at rated conditions along with performance curves for variations in total capacity, energy input ratio and part load fraction to determine performance at part-load conditions. The impacts of defrost operation is modeled based a combination of user inputs and empirical models taken from the air-to-air heat pump algorithms in DOE-2.1E.

The VRF DX heating coil input requires an availability schedule, the gross rated heating capacity and the rated air volume flow rate. The rated air volume flow rate should be between 0.00008056 m^3^/s and 0.00002684 m^3^/s per watt of gross rated heating capacity.

Two performance curves are required. The first performance curve defines the heating capacity as a function of indoor air dry-bulb and outdoor condenser entering air wet-bulb temperature. This curve object is specified in the variable refrigerant flow air-to-air heat pump object. The second performance curve is specific the each VRF DX heating coil and defines the change in heating capacity as a function of air flow fraction. Each of these performance curves are further discussed here.

#. Heating capacity modifier curve (function of temperature, specified in Heat Pump)

- The heating capacity modifier curve (function of temperature) can be a function of both the outdoor wet-bulb temperature and indoor air dry-bulb temperature. Users have the choice of a bi-quadratic curve with two independent variables or a tri-quadratic curve with three independent variable. The tri-quadratic curve is recommended if sufficient manufacturer data is available as it provides sensitivity to the combined total capacity of all indoor units connected to the heat pump condenser and a more realistic output. The output of this curve is multiplied by the gross rated heating capacity to give the gross heating capacity at specific temperature operating conditions (i.e., at an outdoor or indoor air temperature different from the rating point temperature) and the combination ratio of the installed system.

#. Heating capacity modifier curve (function of flow fraction, specified in DX coil)

- The heating capacity modifier curve (function of flow fraction) is a quadratic or cubic curve with the independent variable being the ratio of the actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated heating capacity and the heating capacity modifier curve (function of temperature) to give the gross heating capacity at the specific temperature and air flow conditions at which the coil is operating.

### Inputs

#### Field: Name

This alpha field defines a unique user-assigned name for an instance of a VRF DX heating coil. Any reference to this DX heating coil by another object will use this name.

#### Field: Availability Schedule Name

This alpha field defines the name of the schedule (ref: Schedule) that denotes whether the DX heating coil can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Gross Rated Heating Capacity

This numeric field defines the total, full load gross heating capacity in watts of the DX coil unit at rated conditions (outside air dry-bulb temperature of 8.33 C, outside air wet-bulb temperature of 6.11 C, heating coil entering air dry-bulb temperature of 21.11 C, heating coil entering air wet-bulb temperature of 15.55 C, and a heating coil air flow rate defined by field "rated air flow volume" below). The value entered here must be greater than 0. The gross total heating capacity should not account for the effect of supply air fan heat.

#### Field: Rated Air Flow Rate

This numeric field defines the volume air flow rate, in m^3^ per second, across the DX heating coil at rated conditions. The value entered here must be greater than 0. The rated air volume flow rate should be between 0.00004027 m^3^/s and 0.00006041 m^3^/s per watt of gross rated heating capacity. The gross rated heating capacity and the gross rated COP should be performance information for the unit with outside air dry-bulb temperature of 8.33 C, outside air wet-bulb temperature of 6.11 C, heating coil entering air dry-bulb temperature of 21.11 C, heating coil entering air wet-bulb temperature of 15.55 C, and the rated air volume flow rate defined here.

#### Field: Coil Air Inlet Node

This alpha field defines the name of the HVAC system node from which the DX heating coil draws its inlet air.

#### Field: Coil Air Outlet Node

This alpha field defines the name of the HVAC system node to which the DX heating coil sends its outlet air.

#### Field: Heating Capacity Ratio Modifier Function of Temperature Curve Name

This alpha field defines the heating capacity ratio modifier as a function of indoor dry-bulb temperature or indoor dry-bulb and outdoor wet-bulb temperatures. This curve is a linear, quadratic, or cubic curve if the heating capacity is soley a function of indoor dry-bulb temperature (i.e., the indoor terminal units weighted average inlet air dry-bulb temperatures). Without specific manufacturers data indicating otherwise, the use of a single independent variable is recommended for this coil type. If, however, the user has reason to believe the heating capacity is both a function of indoor dry-bulb temperature and outdoor wet-bulb temperature (and has manufacturers data to create the performance curve), a bi-quadratic equation using weighted average indoor dry-bulb temperature and condenser entering air wet-bulb temperature as the independent variables may be used. See the Engineering Reference for more discussion on using this input field.

> Note: The choice of using either outdoor dry-bulb temperature or outdoor wet-bulb temperature as the independent variable in this performance curve is set in the parent object AirConditioner: VariableRefrigerantFlow.

#### Field: Heating Capacity Ratio Modifier Function of Flow Fraction Curve Name

This alpha field defines the name of a linear, quadratic or cubic performance curve (ref:  Performance Curves) that parameterizes the variation of heating capacity as a function of the ratio of actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated heating capacity and the heating capacity modifier curve (function of temperature) to give the gross heating capacity at the specific temperature and air flow conditions at which the coil is operating. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the rated air flow rate.

Following is an example input for the object.

~~~~~~~~~~~~~~~~~~~~

    COIL:Heating:DX:VariableRefrigerantFlow,
        TU1 VRF DX Heating Coil, !- Coil Name
        VRFAvailSched,           !- Availability Schedule
        autosize,                !- Gross Rated Heating Capacity {W}
        autosize,                !- Rated Air Flow Rate {m3/s}
        TU1 VRF DX CCoil Outlet Node,  !- Coil Air Inlet Node
        TU1 VRF DX HCoil Outlet Node,  !- Coil Air Outlet Node
        VRFTUHeatCapFT,          !- Heating Capacity Ratio Modifier Function of Temperature Curve Name
        VRFACCoolCapFFF;         !- Total heating capacity modifier curve Function of Flow Fraction
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average, Heating Coil Total Heating Rate [W]
    HVAC,Sum, Heating Coil Total Heating Energy [J]
    HVAC,Average, Heating Coil Runtime Fraction []
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Total Heating Rate [W]

This field is the total heating rate output of the DX coil in Watts. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil.

#### Heating Coil Total Heating Energy [J]

This is the total heating output of the DX coil in Joules over the time step being reported. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = HeatingCoils, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Runtime Fraction []

This is the runtime fraction of the DX coil compressor and condenser fan(s) for the time step being reported.

## Coil:Heating:Gas

The gas heating coil is a simple capacity model with a user inputted gas burner efficiency. The default for the gas burner efficiency is 80%. This coil will be simpler than shown in Figure 135 since it will only have air nodes to connect it in the system. The coil can be used in the air loop simulation or in the zone equipment as a reheat coil. Depending on where it is used determines if this coil is temperature or capacity controlled. If used in the air loop simulation it will be controlled to a specified temperature scheduled from the Setpoint Manager. If it is used in zone equipment, it will be controlled from the zone thermostat by meeting the zone demand.

### Inputs

#### Field: Name

A unique identifying name for each coil.

#### Field: Availability Schedule Name

Schedule that defines when the coil is available. If the schedule's value is 0.0, then the coil is not available and flow will not be requested. If the schedule's value is > 0.0 (usually 1 is used), the coil is available. If this field is blank, the schedule has values of 1 for all time periods. Schedule values must be >= 0 and <= 1.

#### Field: Gas Burner Efficiency

This is user inputted gas burner efficiency (decimal, not percent) and is defaulted to 80%.

#### Field: Nominal Capacity

This is the maximum capacity of the coil (W). This controlled coil will only provide the needed capacity to meet the control criteria whether it is temperature or capacity controlled. This field is autosizable.

#### Field: Air Inlet Node Name

The name of the air inlet to the gas coil, i.e. Heating Coil Air Inlet Node.

#### Field: Air Outlet Node Name

The name of the air outlet from the gas coil, i.e. Heating Coil Air Outlet Node.

#### Field: Temperature Setpoint Node Name

If the coil is used in the air loop simulation and is temperature controlled using a Set Point Manager, then the node that is the control node needs to be specified here. If the coil is used in an air terminal unit, the coil is load controlled and a control node set point is not required.

#### Field: Parasitic Electric Load

This is the parasitic electric load associated with the gas coil operation, such as an inducer fan, etc.. This will be modified by the PLR (or coil runtime fraction if a part-load fraction correlation is provided in the next input field) to reflect the time of operation in a simulation timestep.

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of gas consumption rate by the heating coil as a function of the part load ratio (PLR, sensible heating load/nominal capacity of the heating coil). For any simulation timestep, the nominal gas consumption rate (heating load/burner efficiency) is divided by the part-load fraction (PLF) if a part-load curve has been defined. The part-load curve accounts for efficiency losses due to transient coil operation.

The part-load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the heating coil runs continuously for the simulation timestep). For PLR values between 0 and 1 ( 0 <= PLR < 1), the following rules apply:

PLF >= 0.7   and   PLF >= PLR

If PLF < 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the heating coil is defined a PLR/PLF. If PLF < PLR, then a warning message is issues and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional gas heating coil (e.g., residential furnace) would be:

PLF = 0.8 + 0.2(PLR)

#### Field: Parasitic Gas Load

This numeric field is the parasitic gas load associated with the gas coil's operation (Watts), such as a standing pilot light. The model assumes that this parasitic load is consumed only for the portion of the simulation timestep where the gas heating coil is not operating.

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Heating Coil Air Heating Energy [J]
    HVAC,Average,Heating Coil Air Heating Rate [W]
    HVAC,Sum,Heating Coil Gas Energy [J]
    HVAC,Average,Heating Coil Gas Rate [W]
    HVAC,Sum,Heating Coil Electric Energy [J]
    HVAC,Average,Heating Coil Electric Power [W]
    HVAC,Average,Heating Coil Runtime Fraction []
    HVAC,Sum,Heating Coil Ancillary Gas Energy [J]
    HVAC,Average,Heating Coil Ancillary Gas Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Air Heating Energy [J]

This field is the total heating output of the coil in Joules over the timestep being reported. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = HeatingCoils, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Air Heating Rate [W]

This field is the average heating rate output of the coil in Watts over the timestep being reported.

#### Heating Coil Gas Energy [J]

This field is the gas consumption of the heating coil in Joules over the timestep being reported, including the impacts of part-load performance if a part load fraction correlation is specified. This output is also added to a meter with Resource Type = Gas, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Gas Rate [W]

This field is the average gas consumption rate of the coil in Watts over the timestep being reported, including the impacts of part-load performance if a part load fraction correlation is specified.

#### Heating Coil Electric Energy [J]

This field is the electric consumption of the heating coil auxiliaries in Joules over the timestep being reported (e.g., inducer fan). This output is also added to a meter with Resource Type = Electricity, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Electric Power [W]

This field is the average electric consumption rate of the heating coil auxiliaries (e.g., inducer fan) in Watts over the timestep being reported.

#### Heating Coil Runtime Fraction []

This field is the runtime fraction of the coil over the timestep being reported.

#### Heating Coil Ancillary Gas Energy [J]

This field is the parasitic gas consumption of the heating coil in Joules over the timestep being reported (e.g., standing pilot light). The model assumes that the parasitic load is accumulated only for the portion of the simulation timestep where the gas heating coil is not operating. This output is also added to a meter with Resource Type = Gas, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Ancillary Gas Rate [W]

This field is the average parasitic gas consumption rate of the heating coil (e.g., standing pilot light) in Watts over the timestep being reported. The model assumes that the parasitic load is present only for the portion of the simulation timestep where the gas heating coil is not operating.

## Coil:Heating:Gas:MultiStage

The multi stage gas heating coil is a simple capacity model with a user inputted gas burner efficiencies at different stages. This coil will only have air nodes to connect it in the system. The coil can be used in the air loop simulation or in the zone equipment as a reheat coil. Depending on where it is used determines if this coil is temperature or capacity controlled. If used in the air loop simulation it will be controlled to a specified temperature scheduled from the Setpoint Manager. If it is used in zone equipment, it will be controlled from the zone thermostat by meeting the zone demand. For the time being, this coil model can only be called by the parent object [AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed](#airloophvacunitaryheatpumpairtoairmultispeed).

### Inputs

#### Field: Name

A unique identifying name for each coil.

#### Field: Availability Schedule Name

Schedule that defines when the coil is available. If the schedule's value is 0.0, then the coil is not available and flow will not be requested. If the schedule's value is > 0.0 (usually 1 is used), the coil is available. If this field is blank, the schedule has values of 1 for all time periods. Schedule values must be >= 0 and <= 1.

#### Field: Air Inlet Node Name

The name of the air inlet to the gas coil, i.e. Heating Coil Air Inlet Node.

#### Field: Air Outlet Node Name

The name of the air outlet from the gas coil, i.e. Heating Coil Air Outlet Node.

#### Field: Temperature Setpoint Node Name

If the coil is used in the air loop simulation and is temperature controlled using a Set Point Manager, then the node that is the control node needs to be specified here. If the coil is used in an air terminal unit, the coil is load controlled and a control node set point is not required. At present, the multistage gas heating coil does not model temperature setoint control.

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of gas consumption rate by the heating coil as a function of the part load ratio (PLR, sensible heating load/nominal capacity of the heating coil). For any simulation timestep, the nominal gas consumption rate (heating load/burner efficiency) is divided by the part-load fraction (PLF) if a part-load curve has been defined. The part-load curve accounts for efficiency losses due to transient coil operation.

The part-load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the heating coil runs continuously for the simulation timestep). For PLR values between 0 and 1 ( 0 <= PLR < 1), the following rules apply:

PLF >= 0.7   and   PLF >= PLR

If PLF < 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the heating coil is defined a PLR/PLF. If PLF < PLR, then a warning message is issues and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional gas heating coil (e.g., residential furnace) would be:

PLF = 0.8 + 0.2(PLR)

#### Field: Parasitic Gas Load

This numeric field is the parasitic gas load associated with the gas coil's operation (Watts), such as a standing pilot light. The model assumes that this parasitic load is consumed only for the portion of the simulation timestep where the gas heating coil is not operating.

#### Field: Stage 1 Gas Burner Efficiency

This is user inputted stage 1 gas burner efficiency (decimal, not percent) and is defaulted to 80%.

#### Field: Stage 1 Nominal Capacity

This is the stage 1 capacity of the coil (W). This controlled coil will only provide the needed capacity to meet the control criteria whether it is temperature or capacity controlled. This field is autosizable.

#### Field: Stage 1 Parasitic Electric Load

This is the stage 1 parasitic electric load associated with the gas coil operation, such as an inducer fan, etc.. This will be modified by the PLR (or coil runtime fraction if a part-load fraction correlation is provided in the next input field) to reflect the time of operation in a simulation timestep.

#### Field: Stage 2 Gas Burner Efficiency

This is user inputted stage 2 gas burner efficiency (decimal, not percent) and is defaulted to 80%.

#### Field: Stage 2 Nominal Capacity

This is the stage 2 capacity of the coil (W). This controlled coil will only provide the needed capacity to meet the control criteria whether it is temperature or capacity controlled. This field is autosizable.

#### Field: Stage 2 Parasitic Electric Load

This is the stage 2 parasitic electric load associated with the gas coil operation, such as an inducer fan, etc.. This will be modified by the PLR (or coil runtime fraction if a part-load fraction correlation is provided in the next input field) to reflect the time of operation in a simulation timestep.

#### Field: Stage 3 Gas Burner Efficiency

This is user inputted stage 3 gas burner efficiency (decimal, not percent) and is defaulted to 80%.

#### Field: Stage 3 Nominal Capacity

This is the stage 3 capacity of the coil (W). This controlled coil will only provide the needed capacity to meet the control criteria whether it is temperature or capacity controlled. This field is autosizable.

#### Field: Stage 3 Parasitic Electric Load

This is the stage 3 parasitic electric load associated with the gas coil operation, such as an inducer fan, etc.. This will be modified by the PLR (or coil runtime fraction if a part-load fraction correlation is provided in the next input field) to reflect the time of operation in a simulation timestep.

#### Field: Stage 4 Gas Burner Efficiency

This is user inputted stage 4 gas burner efficiency (decimal, not percent) and is defaulted to 80%.

#### Field: Stage 4 Nominal Capacity

This is the stage 4 capacity of the coil (W). This controlled coil will only provide the needed capacity to meet the control criteria whether it is temperature or capacity controlled. This field is autosizable.

#### Field: Stage 4 Parasitic Electric Load

This is the stage 4 parasitic electric load associated with the gas coil operation, such as an inducer fan, etc.. This will be modified by the PLR (or coil runtime fraction if a part-load fraction correlation is provided in the next input field) to reflect the time of operation in a simulation timestep.

An example in IDF form:

~~~~~~~~~~~~~~~~~~~~

      Coil:Heating:Gas:MultiStage,
        Heat Pump Heating Coil 1,         !- Name
        FanAndCoilAvailSched,             !- Availability Schedule Name
        Heating Coil Air Inlet Node,      !- Air Inlet Node Name
        SuppHeating Coil Air Inlet Node,  !- Air Outlet Node Name
        ,                        !- Temp Setpoint node name
        ,                        !- Part Load Fraction Correlation Curve Name
        ,                        !- Parasitic Gas Load
        3,                       !- Number of Speeds
        0.92,                    !- Speed 1 Gas burner Efficiency
        Autosize,                !- Speed 1 Nominal Capacity {W}
        ,                        !- Stage 1 Parasitic Electric Load {W}
        0.88,                    !- Speed 2 Gas burner Efficiency
        Autosize,                !- Speed 2 Nominal Capacity {W}
        ,                        !- Stage 2 Parasitic Electric Load {W}
        0.84,                    !- Speed 3 Gas burner Efficiency
        Autosize,                !- Speed 3 Nominal Capacity {W}
        ;                        !- Stage 3 Parasitic Electric Load {W}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Heating Coil Energy[J]
    HVAC,Average,Heating Coil Rate[W]
    HVAC,Sum,Heating Coil Gas Consumption [J]
    HVAC,Average,Heating Coil Gas Consumption Rate [W]
    HVAC,Sum,Heating Coil Electric Consumption [J]
    HVAC,Average,Heating Coil Electric Power [W]
    HVAC,Average,Heating Coil Runtime Fraction
    HVAC,Sum,Heating Coil Parasitic Gas Consumption [J]
    HVAC,Average,Heating Coil Parasitic Gas Consumption Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Energy [J]

This field is the total heating output of the coil in Joules over the timestep being reported. This output is also added to an output meter with Resource Type = EnergyTransfer, End Use Key = HeatingCoils, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Rate [W]

This field is the average heating rate output of the coil in Watts over the timestep being reported.

#### Heating Coil Gas Consumption [J]

This field is the gas consumption of the heating coil in Joules over the timestep being reported, including the impacts of part-load performance if a part load fraction correlation is specified. This output is also added to an output meter with Resource Type = Gas, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Gas Consumption Rate [W]

This field is the average gas consumption rate of the coil in Watts over the timestep being reported, including the impacts of part-load performance if a part load fraction correlation is specified.

#### Heating Coil Electric Consumption [J]

This field is the electric consumption of the heating coil auxiliaries in Joules over the timestep being reported (e.g., inducer fan). This output is also added to an output meter with Resource Type = Electricity, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Electric Power [W]

This field is the average electric consumption rate of the heating coil auxiliaries (e.g., inducer fan) in Watts over the timestep being reported.

#### Heating Coil Parasitic Gas Consumption [J]

This field is the parasitic gas consumption of the heating coil in Joules over the timestep being reported (e.g., standing pilot light). The model assumes that the parasitic load is accumulated only for the portion of the simulation timestep where the gas heating coil is not operating. This output is also added to an output meter with Resource Type = Gas, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Parasitic Gas Consumption Rate [W]

This field is the average parasitic gas consumption rate of the heating coil (e.g., standing pilot light) in Watts over the timestep being reported. The model assumes that the parasitic load is present only for the portion of the simulation timestep where the gas heating coil is not operating.

## Coil:Cooling:Water:DetailedGeometry

This detailed flat fin coil model is for continuous plate fins. First, found in Type 12 from MODSIM, but now programmed directly from Elmahdy, A.H. and Mitalas, G.P. Then there was a discontinuity in their original model that was fixed in the EnergyPlus implementation. Now this model can be used in an interval halving solution technique for controlling this coil without the problems of non-convergence.

"A Model for Cooling and Dehumidifying Coils for Use in Energy Requirements for Buildings"  ASHRAE Transactions, Vol. 83, Part 2, pp. 103-117 (1977). For fin efficiency see K.A. Gardner, "Efficiency of Extended  ," Transactions ASME, Vol. 67, pp. 621-631, 1945.

The following figures illustrate the geometry and circuits in a cooling coil.

![Geometry of a Cooling Coil (CC)](media/geometry-of-a-cooling-coil-cc.jpeg)


![Number of Coolant Circuits (CCNCC)](media/number-of-coolant-circuits-ccncc.jpeg)


### Inputs

#### Field: Name

A unique identifying name for each coil.

#### Field: Availability Schedule Name

Schedule that defines when the coil is available. If the schedule's value is 0.0, then the coil is not available and flow will not be requested. If the schedule's value is > 0.0 (usually 1 is used), the coil is available. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Maximum Water Flow Rate

The maximum possible water flow rate (m^3^/sec) through the coil.

#### Field: Tube Outside Surface Area

The outside surface area (m^2^) of the tubes that is exposed to air (i.e. the outside area of the unfinned tubes minus the area of tubes covered by the fins).

#### Field: Total Tube Inside Area

The total surface area (m^2^) inside the tubes (water side).

#### Field: Fin Surface Area

The total surface area (m^2^) of the fins attached to the coil.

#### Field: Minimum Air Flow Area

The minimum cross sectional area (m^2^) available for air passage. Frequently calculated as

Amin = (Amin/Afr)\*Afr

where Afr is the frontal area of the heat exchanger, and (Amin/Afr) is the ratio of the minimum airflow area to frontal area.

#### Field: Coil Depth

The distance (m) from the front of the coil to the back of the coil in the airflow direction. Also called the fin depth. Illustrated in the figure (Figure 136. Geometry of a Cooling Coil (CC)).

#### Field: Fin Diameter

The outside diameter (m) of the fins. Used instead of COIL HEIGHT

#### Field: Fin Thickness

Thickness (m) of the air side fins.

#### Field: Tube Inside Diameter

The inside diameter (m) of the tubes.

#### Field: Tube Outside Diameter

The outside diameter (m) of the tubes.

#### Field: Tube Thermal Conductivity

The thermal conductivity (W/m-K) of the tube material.

#### Field: Fin Thermal Conductivity

The thermal conductivity (W/m-K) of the fin material.

#### Field: Fin Spacing

The spacing (m) of the fins, centerline to centerline.

#### Field: Tube Depth Spacing

The spacing (m) of the tube rows, centerline to centerline. Also called tube longitudinal spacing.

#### Field: Number of Tube Rows

The number of tube rows in the direction of the airflow.

#### Field: Number of Tubes per Row

The number of tubes per row. (NTPR in the above diagram)

#### Field: Water Inlet Node Name

The name of the water coil inlet from the chilled water loop, i.e. Cooling Coil Water Inlet Node.

#### Field: Water Outlet Node Name

The name of the water coil outlet from the chilled water loop, i.e. Cooling Coil Water Outlet Node.

#### Field: Air Inlet Node Name

The name of the air inlet to the water coil, i.e. Cooling Coil Air Inlet Node.

#### Field: Air Outlet Node Name

The name of the air outlet from the water coil, i.e. Cooling Coil Air Outlet Node.

#### Field: Condensate Collection Water Storage Tank Name

This field is optional. It is used to describe where condensate from the coil is collected. If blank or omitted, then any coil condensate is discarded. Enter the name of Water Storage Tank object defined elsewhere and the condensate will then be collected in that tank.

Examples of these statements in an IDF are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:Water:DetailedGeometry,
          Detailed Cooling Coil,  !Name of cooling coil
          CoolingCoilAvailSched,  !Cooling Coil Schedule
          0.0011,          !Max Water Flow Rate of Coil m3/sec
          6.23816,         !Tube Outside Surf Area
          6.20007018,      !Tube Inside Surf Area
          101.7158224,     !Fin Surf Area
          0.300606367,     !Min Air Flow Area
          0.165097968,     !Coil Depth
          0.43507152,      !Coil Height
          0.001499982,     !Fin Thickness
          0.014449958,     !Tube Inside Diameter
          0.015879775,     !Tube Outside Diameter
          386.0,           !Tube Thermal Conductivity
          204.0,           !Fin Thermal Conductivity
          0.001814292,     !Fin Spacing
          0.02589977,      !Tube Depth
          6,               !Number of Tube Rows
          16,              !Number of Tubes per Row
          NODE_32,NODE_33, !Coil Water Side Inlet & Outlet Node
          NODE_5, NODE_6;  !Coil Air Side Inlet & Outlet Node
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Sum,Cooling Coil Total Cooling Energy [J]
    HVAC,Sum,Cooling Coil Sensible Cooling Energy [J]
    HVAC,Average,Cooling Coil Total Cooling Rate [W]
    HVAC,Average,Cooling Coil Sensible Cooling Rate [W]
    HVAC,Average,Cooling Coil Condensate Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Coil Source Side Heat Transfer Energy [J]
    HVAC,Sum,Cooling Coil Condensate Volume [m3]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Coil Total Cooling Energy (J)

Cooling Coil Total Cooling Energy is the total amount of heat transfer taking place in the coil at the operating conditions.

#### Cooling Coil Sensible Cooling Energy (J)

Cooling Coil Sensible Cooling Energy is the total amount of Sensible heat transfer taking place in the coil at the operating conditions. It only takes into account temperature difference in the inlet and outlet air streams at operating conditions.

#### Cooling Coil Total Cooling Rate (W)

Cooling Coil Total Cooling Rate is the Rate of heat transfer taking place in the coil at the operating conditions. The units are (J/sec) or Watts.

#### Cooling Coil Sensible Cooling Rate (W)

Cooling Coil Sensible Cooling Rate is the Rate of Sensible heat transfer taking place in the coil at the operating conditions.

In addition, if a Water Storage Tank is used to collect coil condensate, then the following outputs will be available.

#### Cooling Coil Condensate Volume Flow Rate [m3/s]

#### Cooling Coil Condensate Volume [m3]

These reports provide the rate and amount of condensate from the coil. Condensate is water condensed out of the air as a result of cooling. The condensate volume is also reported on the  meter for "OnSiteWater."

#### Cooling Coil Source Side Heat Transfer Energy [J]

This is the energy extracted from the chilled water serving the cooling coil, in Joules.

## Coil:Cooling:DX:SingleSpeed

This DX cooling coil input requires an availability schedule, the gross rated total cooling capacity, the gross rated SHR, the gross rated COP, and the rated air volume flow rate. The latter 4 inputs determine the coil performance at the rating point (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb and air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb). The rated air volume flow rate should be between .00004027 m^3^/s and .00006041 m^3^/s per watt of gross rated total cooling capacity (300 to 450 cfm/ton).

The rated volumetric air flow to total cooling capacity ratio for 100% dedicated outdoor air (DOAS) application DX cooling coils should be between 0.00001677 (m3/s)/W (125 cfm/ton) and 0.00003355 (m3/s)/W (250 cfm/ton).

This model requires 5 curves as follows:

#. The total cooling capacity modifier curve (function of temperature) is a biquadratic curve with two independent variables: wet-bulb temperature of the air entering the cooling coil, and dry-bulb temperature of the air entering the air-cooled condenser coil (wet-bulb temperature if modeling an evaporative-cooled condenser). The output of this curve is multiplied by the gross rated total cooling capacity to give the gross total cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures).
#. The total cooling capacity modifier curve (function of flow fraction) is a quadratic or cubic curve with the independent variable being the ratio of the actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated total cooling capacity and the total cooling capacity modifier curve (function of temperature) to give the gross total cooling capacity at the specific temperature and air flow conditions at which the coil is operating.
#. The energy input ratio (EIR) modifier curve (function of temperature) is a biquadratic curve with two independent variables: wet-bulb temperature of the air entering the cooling coil, and dry-bulb temperature of the air entering the air-cooled condenser coil (wet-bulb temperature if modeling an evaporative-cooled condenser). The output of this curve is multiplied by the rated EIR (inverse of the rated COP) to give the EIR at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures).
#. The energy input ratio (EIR) modifier curve (function of flow fraction) is a quadratic or cubic curve with the independent variable being the ratio of the actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the rated EIR (inverse of the rated COP) and the EIR modifier curve (function of temperature) to give the EIR at the specific temperature and air flow conditions at which the coil is operating.
#. The part load fraction correlation (function of part load ratio) is a quadratic or cubic curve with the independent variable being part load ratio (sensible cooling load / steady-state sensible cooling capacity). The output of this curve is used in combination with the rated EIR and EIR modifier curves to give the "effective" EIR for a given simulation timestep. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling. The curve should be normalized to a value of 1.0 when the part-load ratio equals 1.0 (i.e., the compressor(s) run continuously for the simulation timestep).

The curves are simply specified by name. Curve inputs are described in the curve manager section of this document (see Performance Curves in this document).

The next four input fields are optional and relate to the degradation of latent cooling capacity when the supply air fan operates continuously while the cooling coil/compressor cycle on and off to meet the cooling load. The fan operating mode is determined in the partent object and is considered to either be constant (e.g. [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled (e.g. [AirLoopHVAC:UnitaryHeatCool](#airloophvacunitaryheatcool)). When scheduled, the schedule value must be greater than 0 to calculate degradation of latent cooling capacity. At times when the parent object's supply air fan operating mode schedule is 0, latent degradation will be ignored. When modeling latent capacity degradation, these next four input fields must all have positive values.

The next input specifies the outdoor air node used to define the conditions of the air entering the outdoor condenser. If this field is left blank, the outdoor temperature entering the condenser is taken directly from the weather data. If this field is not blank, the node name specified must be listed in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor temperature from the weather data. Alternately, the node name must be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor temperature entering the condenser is taken directly from the weather data.

The next input describes the type of outdoor condenser coil used with the DX cooling coil (Air Cooled or Evap Cooled). The following three inputs are required when modeling an evaporative-cooled condenser: evaporative condenser effectiveness, evaporative condenser air volume flow rate, and the power consumed by the evaporative condenser pump. Crankcase heater capacity and cutout temperature are entered in the next two input fields. These two fields for this object define the name of the water storage tank for supply and condensate collection. See section "DX Cooling Coil Model" in the EnergyPlus Engineering Document for further details regarding this model.

The last two input fields following the Basin Heater Operating Schedule Name are the Sensible Heat Ratio (SHR) modifier cruve names for temperature and flow fraction.  These two input fields  are optional and used only when a user intends to override SHR calculated using the apparatus dew point (ADP) and bypass factor (BF) method.  See section "SHR Calculation Using User Specified SHR Modifier Curves" in the EnergyPlus Engineering Document for further details.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a DX cooling coil. Any reference to this DX coil by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the DX cooling coil can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during a given time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Gross Rated Total Cooling Capacity

The total, full load gross cooling capacity (sensible plus latent) in watts of the DX coil unit at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate" below). Capacity should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for). When used in a heat pump, the gross rated total cooling capacity should be within 20% of the gross rated heating capacity, otherwise a warning message is issued.

#### Field: Gross Rated Sensible Heat Ratio

The sensible heat ratio (SHR= gross sensible cooling capacity divided by gross total cooling capacity) of the DX cooling coil at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb), and a cooling coil air flow rate defined by field "rated air flow rate" below). Both the sensible and total cooling capacities used to define the Rated SHR should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for).

#### Field: Gross Rated Cooling COP

The coefficient of performance is the ratio of the gross total cooling capacity in watts to electrical power input in watts of the DX cooling coil unit at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/ 23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate" below). The input power includes electric power for the compressor(s) and condenser fan(s) but does not include the power consumption of the supply air fan. The gross COP should NOT account for the supply air fan. If this input field is left blank, the default value is 3.0.

#### Field: Rated Air Flow Rate

The air volume flow rate, in m^3^ per second, across the DX cooling coil at rated conditions. The rated air volume flow rate should be between 0.00004027 m^3^/s and 0.00006041 m^3^/s per watt of gross rated total cooling capacity (300 to 450 cfm/ton). For DOAS applications the rated air volume flow rate should be between 0.00001677 m^3^/s and 0.00003355 m^3^/s per watt of gross rated total cooling capacity (125 to 250 cfm/ton).  The gross rated total cooling capacity, gross rated SHR and gross rated COP should be performance information for the unit with air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and the rated air volume flow rate defined here.

#### Field: Rated Evaporator Fan Power Per Volume Flow Rate

This field is the electric power for the evaporator (cooling coil) fan per air volume flow rate through the coil at the rated conditions in W/(m^3^/s). The default value is 773.3 W/(m^3^/s) (365 W/1000 cfm) if this field is left blank. If a value is entered, it must be >= 0.0 and <= 1250 W/(m^3^/s). This value is only used to calculate Seasonal Energy Efficiency Ratio (SEER), Energy Efficiency Ratio (EER), Integrated Energy Efficiency Ratio (IEER) and the Standard Rating (Net) Cooling Capacity which will be outputs in the EnergyPlus eio file (ref. EnergyPlus Engineering Reference, Single Speed DX Cooling Coil, Standard Ratings). This value is not used for modeling the evaporator (cooling coil) fan during simulations; instead, it is used for calculating SEER, EER, IEER and Standard Rating Cooling Capacity to assist the user in verifying their inputs for modeling this type of equipment.

#### Field: Air Inlet Node Name

The name of the HVAC system node from which the DX cooling coil draws its inlet air.

#### Field: Air Outlet Node Name

The name of the HVAC system node to which the DX cooling coil sends its outlet air.

#### Field: Total Cooling Capacity Function of Temperature Curve Name

The name of a biquadratic performance curve (ref: Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the wet-bulb temperature of the air entering the cooling coil, and the dry-bulb temperature of the air entering the air-cooled condenser coil (wet-bulb temperature if modeling an evaporative-cooled condenser). The output of this curve is multiplied by the gross rated total cooling capacity to give the gross total cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Total Cooling Capacity Function of Flow Fraction Curve Name

The name of a quadratic or cubic performance curve (ref:  Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated total cooling capacity and the total cooling capacity modifier curve (function of temperature) to give the gross total cooling capacity at the specific temperature and air flow conditions at which the coil is operating. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Energy Input Ratio Function of Temperature Curve Name

The name of a biquadratic performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the wet-bulb temperature of the air entering the cooling coil, and the dry-bulb temperature of the air entering the air-cooled condenser coil (wet-bulb temperature if modeling an evaporative-cooled condenser). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP) to give the EIR at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to a value of 1.0 at the rating point.

#### Field: Energy Input Ratio Function of Flow Fraction Curve Name

The name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR and the EIR modifier curve (function of temperature) to give the EIR at the specific temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the DX unit as a function of the part load ratio (PLR, sensible cooling load/steady-state sensible cooling capacity). The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling.

The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply:

PLF >= 0.7   and   PLF >= PLR

If PLF < 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, then a warning message is issued and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional, single-speed DX cooling coil (e.g., residential unit) would be:

PLF = 0.85 + 0.15(PLR)

If the user wishes to model no efficiency degradation due to compressor cycling, the part load fraction correlation should be defined as follows:

PLF = 1.0 + 0.0(PLR)

#### Field: Nominal Time for Condensate Removal to Begin

The nominal time (in seconds) after startup for condensate to begin leaving the coil's condensate drain line at the coil's rated airflow and temperature conditions, starting with a dry coil. Nominal time is equal to the ratio of the energy of the coil's maximum condensate holding capacity (J) to the coil's steady-state latent capacity (W). Suggested value is 1000; zero value means the latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]), and this field as well as the next three input fields for this object must have positive values in order to model latent capacity degradation.

#### Field: Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity

Ratio of the initial moisture evaporation rate from the cooling coil (when the compressor first turns off, in Watts) and the coil's steady-state latent capacity (Watts) at rated airflow and temperature conditions. Suggested value is 1.5; zero value means the latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]); and this field, the previous field and the next two fields must have positive values in order to model latent capacity degradation.

#### Field: Maximum Cycling Rate

The maximum on-off cycling rate for the compressor (cycles per hour), which occurs at 50% run time fraction. Suggested value is 3; zero value means latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]); and this field, the previous two fields and the next field must have positive values in order to model latent capacity degradation.

#### Field: Latent Capacity Time Constant

Time constant (in seconds) for the cooling coil's latent capacity to reach steady state after startup. Suggested value is 45: supply air fan operating mode must be continuous. That is, the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects (e.g., AirloopHVAC:UnitaryHeatCool), and this field as well as the previous three input fields for this object must have positive values in order to model latent capacity degradation.

#### Field: Condenser Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the outdoor condenser. If this field is left blank, the outdoor air temperature entering the condenser (dry-bulb or wet-bulb) is taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data.

#### Field: Condenser Type

The type of condenser used by the DX cooling coil. Valid choices for this input field are **AirCooled** or **EvaporativelyCooled**. The default for this field is **AirCooled**.

#### Field: Evaporative Condenser Effectiveness

The effectiveness of the evaporative condenser, which is used to determine the temperature of the air entering the outdoor condenser coil as follows:

![](media/image342.png)\


where

*T~cond inlet~* = the temperature of the air entering the condenser coil (C)

*T~wb,o~*~~= the wet-bulb temperature of the outdoor air (C)

*T~db,o~*~~= the dry-bulb temperature of the outdoor air (C)

The resulting condenser inlet air temperature is used by the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature). The default value for this field is 0.9, although valid entries can range from 0.0 to 1.0. This field is not used when Condenser Type = Air Cooled.

If the user wants to model an air-cooled condenser, they should simply specify AirCooled in the field Condenser Type. In this case, the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature) input fields for this object should reference performance curves that are a function of outdoor dry-bulb temperature.

If the user wishes to model an evaporative-cooled condenser AND they have performance curves that are a function of the wet-bulb temperature of air entering the condenser coil, then the user should specify Condenser Type = Evap Cooled and the evaporative condenser effectiveness value should be entered as 1.0. In this case, the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature) input fields for this object should reference performance curves that are a function of the wet-bulb temperature of air entering the condenser coil.

If the user wishes to model an air-cooled condenser that has evaporative media placed in front of it to cool the air entering the condenser coil, then the user should specify Condenser Type = Evap Cooled. The user must also enter the appropriate evaporative effectiveness for the media. In this case, the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature) input fields for this object should reference performance curves that are a function of outdoor dry-bulb temperature. Be aware that the evaporative media will significantly reduce the dry-bulb temperature of the air entering the condenser coil, so the Total Cooling Capacity and EIR Modifier Curves must be valid for the expected range of dry-bulb temperatures that will be entering the condenser coil.

#### Field: Evaporative Condenser Air Flow Rate

The air volume flow rate, in m^3^ per second, entering the evaporative condenser. This value is used to calculate the amount of water used to evaporatively cool the condenser inlet air. The minimum value for this field must be greater than zero, and this input field is autosizable (equivalent to 0.000144 m^3^/s per watt of rated total cooling capacity [850 cfm/ton]). This field is not used when Condenser Type = Air Cooled.

#### Field: Evaporative Condenser Pump Rated Power Consumption

The rated power of the evaporative condenser water pump in Watts. This value is used to calculate the power required to pump the water used to evaporatively cool the condenser inlet air. The default value for this input field is zero, but it is autosizable (equivalent to 0.004266 W per watt [15 W/ton] of rated total cooling capacity). This field is not used when Condenser Type = Air Cooled.

#### Field: Crankcase Heater Capacity

This numeric field defines the crankcase heater capacity in Watts. When the outdoor air dry-bulb temperature is below the value specified in the input field "Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation" (described below), the crankcase heater is enabled during the time that the compressor is not running. If this cooling coil is used as part of an air-to-air heat pump (Ref. [AirLoopHVAC:UnitaryHeatPump:AirToAir](#airloophvacunitaryheatpumpairtoair) or PackageTerminal: HeatPump:AirToAir), the crankcase heater defined for this DX cooling coil is ignored and the crankcase heater power defined for the DX heating coil (Ref. [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed)) is enabled during the time that the compressor is not running for either heating or cooling. The value for this input field must be greater than or equal to 0, and the default value is 0. To simulate a DX cooling coil without a crankcase heater, enter a value of 0.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the compressor's crankcase heater is disabled. The value for this input field must be greater than or equal to 0.0°C, and the default value is 10°C.

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the coil obtains water used for evaporative cooling of its condenser. If blank or omitted, then the unit will obtain water directly from the mains. If the name of a Water Storage Tank object is used here, then the unit will obtain its water from that tank. If a tank is specified, the unit will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the condenser needs, then the unit will still operate and obtain the rest of the water it needs from the mains (referred to as StarvedWater).

#### Field: Condensate Collection Water Storage Tank Name

This field is optional. It is used to describe where condensate from the coil is collected. If blank or omitted, then any coil condensate is discarded. Enter the name of Water Storage Tank object defined elsewhere and the condensate will then be collected in that tank.

#### Field: Basin Heater Capacity

This numeric field contains the capacity of the DX coil's electric evaporative cooler basin heater in watts per degree Kelvin. This field only applies for Condenser Type = EvaporativelyCooled. This field is used in conjunction with the Basin Heater Setpoint Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the outdoor dry-bulb temperature. The basin heater only operates when the DX coil is off, regardless of the basin heater schedule described below. The basin heater capacity must be greater than or equal to zero, with a default value of zero if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (˚C) for the basin heater described in the previous field. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater is active when the outdoor air dry-bulb temperature falls below this setpoint temperature, as long as the DX coil is off. This set point temperature must be greater than or equal to 2˚C, and the default value is 2˚C if this field is left blank.

#### Field: Basin Heater Operating Schedule Name

This alpha field contains the name of the basin heater operating schedule. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0. The basin heater operates when scheduled on and the outdoor air dry-bulb temperature is below the set point temperature described in the previous field. If this field is left blank, the basin heater is available to operate throughout the simulation. Regardless of this schedule, the basin heater may only operate when the DX coil is off.

#### Field: Sensible Heat Ratio Function of Temperature Curve Name

The name of a biquadratic normalized curve (Ref: Performance Curves) that parameterizes the variation of the sensible heat ratio (SHR) as a function of DX cooling coil entering air wet-bulb and dry-bulb temperatures. The output of this curve is multiplied by the rated SHR and the SHR modifier curve (function of flow fraction) to give the SHR at the specific coil entering air temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 at the rated condition. This input field is optional.

#### Field: Sensible Heat Ratio Function of Flow Fraction Curve Name

The name of a quadratic or cubic normalized curve (Ref: Performance Curves) that parameterizes the variation of the sensible heat ratio (SHR) as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the rated SHR and the SHR modifier curve (function of temperature) to give the SHR at the specific temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate. This input field is optional.

Following is an example input for a [Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed) coil.

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:SingleSpeed,
           Zone1WindACDXCoil,        ! Coil Name
           FanAndCoilAvailSched,     ! Availability Schedule
           10548,      ! Gross Rated Total Cooling Capacity
           0.75,       ! Gross Rated Sensible Heat Ratio
           3.0,        ! Gross Rated Cooling COP
           0.637,      ! Rated Air Flow Rate (m3/s)
           773.3,      ! Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}
           Zone1WindACFanOutletNode, ! Coil Air Inlet Node
           Zone1WindACAirOutletNode, ! Coil Air Outlet Node
           WindACCoolCapFT,          ! Total Cooling Capacity Modifier Curve (function of temperature)
           WindACCoolCapFFF,         ! Total Cooling Capacity Modifier Curve (function of flow fraction)
           WindACEIRFT,              ! Energy Input Ratio Modifier Curve (function of temperature)
           WindACEIRFFF,             ! Energy Input Ratio Modifier Curve (function of flow fraction)
           WindACPLFFPLR,            ! Part Load Fraction Correlation (function of part load ratio)
           1000.,             ! Nominal Time for Condensate Removal to Begin {s}
           1.5,               ! Ratio of Initial Moisture Evaporation Rate and Steady-state Latent Capacity
           3.0,               ! Maximum ON/OFF Cycling Rate {cycles/hr}
           45.0,              ! Latent Capacity Time Constant {s}
           ,                  ! Condenser Air Inlet Node Name
           AirCooled,        ! Condenser Type
           ,                  ! Evaporative Condenser Effectiveness
           ,                  ! Evaporative Condenser Air Volume Flow Rate {m3/s}
           ,                  ! Evaporative Condenser Pump Rated Power Consumption {W}
           30.,               ! Crankcase Heater Capacity {W}
           10.;               ! Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation {C}
~~~~~~~~~~~~~~~~~~~~

## Coil:Cooling:DX:TwoSpeed

This component models a two-speed (or variable speed) DX compressor and fan. The method is based on the model used for the cycling, single speed DX unit. The single speed unit is described by single full load capacity, SHR, COP, and air flow rate at rated conditions. Off rated full load performance is obtained by the use of 4 modifier curves. At partial load the unit cycles on/off and the cycling losses are described by a part load fraction curve.

The multispeed unit is described by specifying the performance at two states: high speed compressor, high speed fan; and low speed compressor, low speed fan. When the unit load is above the high speed capacity, the unit runs with high speed compressor and fan. When the load on the unit is below the high speed capacity but above the low speed capacity, the unit will run with performance intermediate between high speed and low speed. When the load is less than the low speed capacity, the unit will cycle on/off just like the single speed unit.

The multispeed unit model requires 2 full sets of performance data. There must be a high and low speed capacity, SHR, COP, and evaporator air flow rate; as well as high and low speed performance curves – total cooling capacity modifier curve (function of temperature) and energy input ratio modifier curve (function of temperature).

The multispeed DX component should be used for all cases in which a DX VAV system is being simulated. Obviously this model – in which performance is obtained by interpolating between 2 specified states - is an oversimplification of how real multi-speed and variable speed DX cooling units are controlled. But detailed descriptions of how actual units perform and are controlled are not available. This model should give a good average prediction of multispeed and variable speed DX cooling unit performance. The last four input fields following the Basin Heater Operating Schedule Name are the Sensible Heat Ratio (SHR) modifier cruve names for temperature and flow fraction for high and low speed DX cooling coils.  These four input fields  are optional and used only when a user intends to override SHR calculated using the apparatus dew point (ADP) and bypass factor (BF) method.  See section "SHR Calculation Using User Specified SHR Modifier Curves" in the EnergyPlus Engineering Document for further details..

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a multispeed DX cooling coil. Any reference to this DX coil by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the DX cooling coil can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: High Speed Gross Rated Total Cooling Capacity

The total, full load gross cooling capacity (sensible plus latent) in watts of the DX coil unit for high speed compressor and high speed fan at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate" below). Capacity should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for).

#### Field: High Speed Gross Rated Sensible Heat Ratio

The sensible heat ratio (gross sensible capacity divided by gross total cooling capacity) of the DX cooling coil  for high speed compressor and high speed fan at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate" below). Both the sensible and total cooling capacities used to define the Rated SHR should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for).

#### Field: High Speed Gross Rated Cooling COP

The coefficient of performance is the ratio of the gross total cooling capacity in watts to electrical power input in watts) of the DX cooling coil unit for high speed compressor and high speed fan at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate" below). The input power includes electric power for the compressor(s) and condenser fan(s) but does not include the power consumption of the supply air fan. The gross COP should NOT account for the supply air fan. If this input field is left blank, the default value is 3.0.

#### Field: High Speed Rated Air Flow Rate

The high speed air volume flow rate, in m^3^ per second, across the DX cooling coil at rated conditions. The rated air volume flow rate should be between 0.00004027 m^3^/s and 0.00006041 m^3^/s per watt of gross rated total cooling capacity. For DOAS applications the rated air volume flow rate should be between 0.00001677 m^3^/s and 0.00003355 m^3^/s per watt of gross rated total cooling capacity (125 to 250 cfm/ton).  The gross rated total cooling capacity, gross rated SHR and gross rated COP should be performance information for the unit with air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and the rated air volume flow rate defined here.

#### Field: Unit Internal Static Air Pressure

If this coil is used with a [Fan:VariableVolume](#fanvariablevolume) to model a packaged variable-air-volume unit, then ratings for standard rated net capacity, EER, and IEER will be calculated per ANSI/AHRI Standard 340/360-2007 with Addenda 1 and 2.  This field is to specify the internal static air pressure, in units of Pascals, associated with the unit's supply air flow for rating purposes.  This field does not affect the performance during operation.  This field is optional. If this field is used, then the internal static air pressure is used with the associated fan characteristics when calculating standard rated net capacity, EER, and IEER.  If this field is not used, then the standard ratings are still performed but use a default for specific fan power of 773.3 (W/(m^3^/s)).  The air pressure drop/rise input here should be "internal" in the sense that it is for the entire package of unitary equipment as it would be tested in a laboratory (including other non-cooling sections inside the package for filters, dampers, and.or heating coils) but none of the "external" pressure drop for distributing supply air throughout the building.  This is different from the input field called Pressure Rise in the fan object which includes both the external static pressure and the internal static pressure.  The results of standard rating calculations are reported to the EIO file and to predefined output tables called "DX Cooling Coils" and "VAV DX Cooling Standard Rating Details."

#### Field: Air Inlet Node

The name of the HVAC system node from which the DX cooling coil  draws its inlet air.

#### Field: Air Outlet Node

The name of the HVAC system node to which the DX cooling coil  sends its outlet air.

#### Field: Total Cooling Capacity Function of Temperature Curve Name

The name of a biquadratic performance curve (ref: Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the wet-bulb temperature of the air entering the cooling coil, and the dry-bulb temperature of the air entering the air-cooled condenser (wet-bulb temperature if modeling an evaporative-cooled condenser). The output of this curve is multiplied by the gross rated total cooling capacity to give the gross total cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to have the value of 1.0 at the rating point. This curve is used for performance at the high speed compressor, high speed fan operating point.

#### Field: Total Cooling Capacity Function of Flow Fraction Curve Name

The name of a quadratic performance curve (ref:  Performance Curves) that parameterizes the variation of gross total cooling capacity as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated total cooling capacity and the total cooling capacity modifier curve (function of temperature) to give the gross total cooling capacity at the specific temperature and air flow conditions at which the coil is operating. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the rated air flow rate. This curve is applied only at the high speed compressor, high speed fan operating point. There is no corresponding curve for the low speed operating point.

#### Field: Energy Input Ratio Function of Temperature Curve Name

The name of a biquadratic performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the wet-bulb temperature of the air entering the cooling coil and the dry-bulb temperature of the air entering the air-cooled condenser (wet-bulb temperature if modeling an evaporative-cooled condenser). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP) to give the EIR at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to a value of 1.0 at the rating point. This curve is used for performance at the high speed compressor, high speed fan operating point.

#### Field: Energy Input Ratio Function of Flow Fraction Curve Name

The name of a quadratic performance curve (Ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR and the EIR modifier curve (function of temperature) to give the EIR at the specific temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate. This curve is applied only at the high speed compressor, high speed fan operating point. There is no corresponding curve for the low speed operating point.

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the DX unit as a function of the part load ratio (PLR, sensible cooling load/steady-state sensible cooling capacity). The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling.

The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply:

PLF >= 0.7   and   PLF >= PLR

If PLF < 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, then a warning message is issued and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional, single-speed DX cooling coil (e.g., residential unit) would be:

PLF = 0.85 + 0.15(PLR)

If the user wishes to model no efficiency degradation due to compressor cycling, the part load fraction correlation should be defined as follows:

PLF = 1.0 + 0.0(PLR)

#### Field: Low Speed Gross Rated Total Cooling Capacity

The total, full load gross total cooling capacity (sensible plus latent) in watts of the DX coil unit for low speed compressor and low speed fan at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate, low speed" below). Capacity should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for).

#### Field: Low Speed Gross Rated Sensible Heat Ratio

The sensible heat ratio (SHR= gross sensible capacity divided by gross total cooling capacity) of the DX cooling coil  for low speed compressor and low speed fan at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate, low speed" below). Both the sensible and total cooling capacities used to define the Rated SHR should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for).

#### Field: Low Speed Gross Rated Cooling COP

The coefficient of performance is the ratio of  gross total cooling capacity in watts to electrical power input in watts) of the DX cooling coil unit for low speed compressor and low speed fan at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air volume flow rate, low speed" below). The input power includes power for the compressor(s) and condenser fan(s) but does not include the power consumption of the supply air fan. The gross COP should NOT account for the supply air fan. If this input field is left blank, the default value is 3.0.

#### Field: Low Speed Rated Air Flow Rate

The low speed volume air flow rate, in m^3^ per second, across the DX cooling coil at rated conditions. The rated air volume flow rate should be between 0.00004027 m^3^/s and 0.00006041 m^3^/s per watt of the gross rated total cooling capacity. For DOAS applications the rated air volume flow rate should be between 0.00001677 m^3^/s and 0.00003355 m^3^/s per watt of gross rated total cooling capacity (125 to 250 cfm/ton).  The gross rated total cooling capacity, gross rated SHR and gross rated COP should be performance information for the unit with air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and the rated air volume flow rate defined here.

#### Field: Low Speed Total Cooling Capacity Function of Temperature Curve Name

The name of a biquadratic performance curve (ref: Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the wet-bulb temperature of the air entering the cooling coil, and the dry-bulb temperature of the air entering the air-cooled condenser (wet-bulb temperature if modeling an evaporative-cooled condenser). The output of this curve is multiplied by the gross rated total cooling capacity to give the gross total cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to have the value of 1.0 at the rating point. This curve is used for performance at the low speed compressor, low speed fan operating point.

#### Field: Low Speed Energy Input Ratio Function of Temperature Curve Name

The name of a biquadratic performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the wetbulb temperature of the air entering the cooling coil and the drybulb temperature of the air entering the air-cooled condenser (wetbulb temperature if modeling an evaporative-cooled condenser). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP) to give the EIR at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to a value of 1.0 at the rating point. This curve is used for performance at the low speed compressor, low speed fan operating point.

#### Field: Condenser Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the outdoor condenser. If this field is left blank, the outdoor air temperature entering the condenser (dry-bulb or wet-bulb) is taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data.

#### Field: Condenser Type

The type of condenser used by the multi-speed DX cooling coil. Valid choices for this input field are **AirCooled** or **EvaporativelyCooled**. The default for this field is **AirCooled**.

#### Field: High Speed Evaporative Condenser Effectiveness

The effectiveness of the evaporative condenser at high compressor/fan speed, which is used to determine the temperature of the air entering the outdoor condenser coil as follows:

![](media/image343.png)\


where

*T~cond inlet~* = the temperature of the air entering the condenser coil (C)

*T~wb,o~*~~= the wet-bulb temperature of the outdoor air (C)

*T~db,o~*~~= the dry-bulb temperature of the outdoor air (C)

The resulting condenser inlet air temperature is used by the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature). The default value for this field is 0.9, although valid entries can range from 0.0 to 1.0. This field is not used when Condenser Type = Air Cooled.

If the user wants to model an air-cooled condenser, they should simply specify AirCooled in the field Condenser Type. In this case, the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature) input fields for this object should reference performance curves that are a function of outdoor dry-bulb temperature.

If the user wishes to model an evaporative-cooled condenser AND they have performance curves that are a function of the wet-bulb temperature of air entering the condenser coil, then the user should specify Condenser Type = Evap Cooled and the evaporative condenser effectiveness value should be entered as 1.0. In this case, the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature) input fields for this object should reference performance curves that are a function of the wet-bulb temperature of air entering the condenser coil.

If the user wishes to model an air-cooled condenser that has evaporative media placed in front of it to cool the air entering the condenser coil, then the user should specify Condenser Type = Evap Cooled. The user must also enter the appropriate evaporative effectiveness for the media. In this case, the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature) input fields for this object should reference performance curves that are a function of outdoor dry-bulb temperature. Be aware that the evaporative media will significantly reduce the dry-bulb temperature of the air entering the condenser coil, so the Total Cooling Capacity and EIR Modifier Curves must be valid for the expected range of dry-bulb temperatures that will be entering the condenser coil.

#### Field: High Speed Evaporative Condenser Air Flow Rate

The air volume flow rate, in m^3^ per second, entering the evaporative condenser at high compressor/fan speed. This value is used to calculate the amount of water used to evaporatively cool the condenser inlet air. The minimum value for this field must be greater than zero, and this input field is autosizable (equivalent to 0.000144 m^3^/s per watt of rated high-speed total cooling capacity [850 cfm/ton]). This field is not used when Condenser Type = Air Cooled.

#### Field: High Speed Evaporative Condenser Pump Rated Power Consumption

The rated power of the evaporative condenser water pump in Watts at high compressor/fan speed. This value is used to calculate the power required to pump the water used to evaporatively cool the condenser inlet air. The default value for this input field is zero, but it is autosizable (equivalent to 0.004266 W per watt [15 W/ton] of rated high-speed total cooling capacity). This field is not used when Condenser Type = Air Cooled.

#### Field: Low Speed Evaporative Condenser Effectiveness

The effectiveness of the evaporative condenser at low compressor/fan speed, which is used to determine the temperature of the air entering the outdoor condenser coil as follows:

![](media/image344.png)\


where

*T~cond inlet~* = the temperature of the air entering the condenser coil (C)

*T~wb,o~*~~= the wet-bulb temperature of the outdoor air (C)

*T~db,o~*~~= the dry-bulb temperature of the outdoor air (C)

The resulting condenser inlet air temperature is used by the Total Cooling Capacity Modifier Curve, low speed (function of temperature) and the Energy Input Ratio Modifier Curve, low speed (function of temperature). The default value for this field is 0.9, although valid entries can range from 0.0 to 1.0. This field is not used when Condenser Type = Air Cooled. See field "Evaporative Condenser Effectiveness, High Speed" above for further information.

#### Field: Low Speed Evaporative Condenser Air Flow Rate

The air volume flow rate, in m^3^ per second, entering the evaporative condenser at low compressor/fan speed. This value is used to calculate the amount of water used to evaporatively cool the condenser inlet air. The minimum value for this field must be greater than zero, and this input field is autosizable (equivalent to 0.000048 m^3^/s per watt of rated high-speed total cooling capacity [280 cfm/ton]). This field is not used when Condenser Type = Air Cooled.

#### Field: Low Speed Evaporative Condenser Pump Rated Power Consumption

The rated power of the evaporative condenser water pump in Watts at low compressor/fan speed. This value is used to calculate the power required to pump the water used to evaporatively cool the condenser inlet air. The default value for this input field is zero, but it is autosizable (equivalent to 0.001422 W per watt [5 W/ton] of rated high-speed total capacity). This field is not used when Condenser Type = Air Cooled.

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the coil obtains water used for evaporative cooling. If blank or omitted, then the cooler will obtain water directly from the mains. If the name of a Water Storage Tank object is used here, then the cooler will obtain its water from that tank. If a tank is specified, the coil will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the cooler needs, then the cooler will still operate and obtain the rest of the water it needs from the mains (referred to as StarvedWater).

#### Field: Condensate Collection Water Storage Tank Name

This field is optional. It is used to describe where condensate from the coil is collected. If blank or omitted, then any coil condensate is discarded. Enter the name of Water Storage Tank object defined elsewhere and the condensate will then be collected in that tank.

#### Field: Basin Heater Capacity

This numeric field contains the capacity of the DX coil's electric evaporative cooler basin heater in watts per degree Kelvin. This field only applies for Condenser Type = EvaporativelyCooled. This field is used in conjunction with the Basin Heater Setpoint Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the outdoor dry-bulb temperature. The basin heater only operates when the DX coil is off, regardless of the basin heater schedule described below. The basin heater capacity must be greater than or equal to zero, with a default value of zero if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (˚C) for the basin heater described in the previous field. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater is active when the outdoor air dry-bulb temperature falls below this setpoint temperature, as long as the DX coil is off. This set point temperature must be greater than or equal to 2˚C, and the default value is 2˚C if this field is left blank.

#### Field: Basin Heater Operating Schedule Name

This alpha field contains the name of the basin heater operating schedule. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0. The basin heater operates when scheduled on and the outdoor air dry-bulb temperature is below the set point temperature described in the previous field. If this field is left blank, the basin heater is available to operate throughout the simulation. Regardless of this schedule, the basin heater may only operate when the DX coil is off.

#### Field: Sensible Heat Ratio Function of Temperature Curve Name

The name of a biquadratic normalized curve (Ref: Performance Curves) that parameterizes the variation of the sensible heat ratio (SHR) as a function of DX cooling coil entering air wet-bulb and dry-bulb temperatures. The output of this curve is multiplied by the rated SHR and the SHR modifier curve (function of flow fraction) to give the SHR at the specific coil entering air temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 at the rated condition. This input field is optional.

#### Field: Sensible Heat Ratio Function of Flow Fraction Curve Name

The name of a quadratic or cubic normalized curve (Ref: Performance Curves) that parameterizes the variation of the sensible heat ratio (SHR) as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the rated SHR and the SHR modifier curve (function of temperature) to give the SHR at the specific temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate. This input field is optional.

#### Field: Low Sensible Heat Ratio Function of Temperature Curve Name

The name of a biquadratic normalized curve (Ref: Performance Curves) that parameterizes the variation of the sensible heat ratio (SHR) as a function of DX cooling coil entering air wet-bulb and dry-bulb temperatures. The output of this curve is multiplied by the rated SHR and the SHR modifier curve (function of flow fraction) to give the SHR at the specific coil entering air temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 at the rated condition. This input field is optional.

#### Field: Low Sensible Heat Ratio Function of Flow Fraction Curve Name

The name of a quadratic or cubic normalized curve (Ref: Performance Curves) that parameterizes the variation of the sensible heat ratio (SHR) as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the rated SHR and the SHR modifier curve (function of temperature) to give the SHR at the specific temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate. This input field is optional.

Following are example inputs for the object.

~~~~~~~~~~~~~~~~~~~~

      Coil:Cooling:DX:TwoSpeed,
        Main Cooling Coil 1,     !- Name
        CoolingCoilAvailSched,   !- Availability Schedule Name
        autosize,                !- Gross Rated High Speed Total Cooling Capacity {W}
        0.68,                    !- Gross Rated High Speed Sensible Heat Ratio
        3.0,                     !- Gross Rated High Speed Cooling COP
        autosize,                !- Rated High Speed Air Flow Rate {m3/s}
        ,                        !- Unit Internal Static Air Pressure Drop {Pa}
        Mixed Air Node 1,        !- Air Inlet Node Name
        Main Cooling Coil 1 Outlet Node,  !- Air Outlet Node Name
        VarSpeedCoolCapFT,       !- Total Cooling Capacity Function of Temperature Curve Name
        PackagedRatedCoolCapFFlow,  !- Total Cooling Capacity Function of Flow Fraction Curve Name
        VarSpeedCoolEIRFT,       !- Energy Input Ratio Function of Temperature Curve Name
        PackagedRatedCoolEIRFFlow,  !- Energy Input Ratio Function of Flow Fraction Curve Name
        VarSpeedCyclingPLFFPLR,  !- Part Load Fraction Correlation Curve Name
        autosize,                !- Rated Low Speed Total Cooling Capacity {W}
        0.69,                    !- Rated Low Speed Sensible Heat Ratio
        4.2,                     !- Rated Low Speed COP
        autosize,                !- Rated Low Speed Air Flow Rate {m3/s}
        VarSpeedCoolCapLSFT,     !- Low Speed Total Cooling Capacity Function of Temperature Curve Name
        VarSpeedCoolEIRLSFT,     !- Low Speed Energy Input Ratio Function of Temperature Curve Name
        Main Cooling Coil 1 Condenser Node;  !- Condenser Air Inlet Node Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Cooling:DX:TwoSpeed,
        Main Cooling Coil 1,     !- Name
        CoolingCoilAvailSched,   !- Availability Schedule Name
        autosize,                !- High Speed Rated Total Cooling Capacity {W}
        0.68,                    !- High Speed Rated Sensible Heat Ratio
        3.0,                     !- High Speed Rated Cooling COP
        autosize,                !- High Speed Rated Air Flow Rate {m3/s}
        ,                        !- Unit Internal Static Air Pressure Drop {Pa}
        Mixed Air Node 1,        !- Air Inlet Node Name
        Main Cooling Coil 1 Outlet Node,  !- Air Outlet Node Name
        VarSpeedCoolCapFT,       !- Total Cooling Capacity Function of Temperature Curve Name
        PackagedRatedCoolCapFFlow,  !- Total Cooling Capacity Function of Flow Fraction Curve Name
        VarSpeedCoolEIRFT,       !- Energy Input Ratio Function of Temperature Curve Name
        PackagedRatedCoolEIRFFlow,  !- Energy Input Ratio Function of Flow Fraction Curve Name
        VarSpeedCyclingPLFFPLR,  !- Part Load Fraction Correlation Curve Name
        autosize,                !- Low Speed Rated Total Cooling Capacity {W}
        0.69,                    !- Low Speed Rated Sensible Heat Ratio
        4.2,                     !- Low Speed Rated Cooling COP
        autosize,                !- Low Speed Rated Air Flow Rate {m3/s}
        VarSpeedCoolCapLSFT,     !- Low Speed Total Cooling Capacity Function of Temperature Curve Name
        VarSpeedCoolEIRLSFT,     !- Low Speed Energy Input Ratio Function of Temperature Curve Name
        Main Cooling Coil 1 Condenser Node;  !- Condenser Air Inlet Node Name
        ,                        !- Condenser Type
        ,                        !- High Speed Evaporative Condenser Effectiveness {dimensionless}
        ,                        !- High Speed Evaporative Condenser Air Flow Rate {m3/s}
        ,                        !- High Speed Evaporative Condenser Pump Rated Power Consumption {W}
        ,                        !- Low Speed Evaporative Condenser Effectiveness {dimensionless}
        ,                        !- Low Speed Evaporative Condenser Air Flow Rate {m3/s}
        ,                        !- Low Speed Evaporative Condenser Pump Rated Power Consumption {W}
        ,                        !- Supply Water Storage Tank Name
        ,                        !- Condensate Collection Water Storage Tank Name
        ,                        !- Basin Heater Capacity {W/K}
        ,                        !- Basin Heater Setpoint Temperature {C}
        ,                        !- Basin Heater Operating Schedule Name
        DOAS DX Coil SHR-FT,     !- High Speed Sensible Heat Ratio Function of Temperature Curve Name
        DOAS DX Coil SHR-FF,     !- High Speed Sensible Heat Ratio Function of Flow Fraction Curve Name
        DOAS DX Coil SHR-FT,     !- Low Speed Sensible Heat Ratio Function of Temperature Curve Name
        DOAS DX Coil SHR-FF;     !- Low Speed Sensible Heat Ratio Function of Flow Fraction Curve Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Curve:Quadratic,
      DOAS DX Coil SHR-FF,                                     !- Name
      0.9317,                                                  !- Coefficient1 Constant
     -0.0077,                                                  !- Coefficient2 x
      0.0760,                                                  !- Coefficient3 x**2
      0.69,                                                    !- Minimum Value of x
      1.30;                                                    !- Maximum Value of x

    Curve:Biquadratic,
      DOAS DX Coil SHR-FT,                                     !- Name
      1.3294540786,                                            !- Coefficient1 Constant
      -0.0990649255,                                           !- Coefficient2 x
      0.0008310043,                                            !- Coefficient3 x**2
      0.0652277735,                                            !- Coefficient4 y
      -0.0000793358,                                           !- Coefficient5 y**2
      -0.0005874422,                                           !- Coefficient6 x*y
      24.44,                                                   !- Minimum Value of x
      26.67,                                                   !- Maximum Value of x
      29.44,                                                   !- Minimum Value of y
      46.1,                                                    !- Maximum Value of y
      0.6661,                                                  !- Minimum Curve Output
      1.6009,                                                  !- Maximum Curve Output
      Temperature,                                             !- Input Unit Type for X
      Temperature,                                             !- Input Unit Type for Y
      Dimensionless;                                           !- Output Unit Type
~~~~~~~~~~~~~~~~~~~~

## Coil:Cooling:DX:TwoStageWithHumidityControlMode

The multimode DX coil is functionally equivalent to [Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed) but with multiple performance modes. It is capable of modeling two-stage DX units and units with an enhanced dehumidification mode such as coil bypass or subcool reheat. This object contains one-time specifications for the DX unit such as node names and crankcase heater specifications. It references one or more [CoilPerformance:DX:Cooling](#coilperformancedxcooling) objects which define the performance for each mode of operation. It can have up to 4 performance modes to accommodate a 2-stage 2-mode unit.

The multimode DX coil can be used only as a component of [CoilSystem:Cooling:DX](#coilsystemcoolingdx) or [AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass](#airloophvacunitaryheatcoolvavchangeoverbypass) (parent object). These parent objects pass a load and dehumidification mode to this coil. If the coil has 2 capacity stages, the multimode coil model determines the stage sequencing.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a DX cooling coil. Any reference to this DX coil by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the DX cooling coil can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during a given time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Air Inlet Node Name

The name of the HVAC system node from which the DX cooling coil draws its inlet air.

#### Field: Air Outlet Node Name

The name of the HVAC system node to which the DX cooling coil sends its outlet air.

#### Field: Crankcase Heater Capacity

This numeric field defines the crankcase heater capacity in Watts. When the outdoor air dry-bulb temperature is below the value specified in the input field "Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation" (described below), the crankcase heater is enabled during the time that the compressor is not running. If this cooling coil is used as part of an air-to-air heat pump (Ref. [AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass](#airloophvacunitaryheatcoolvavchangeoverbypass)), the crankcase heater defined for this DX cooling coil is ignored and the crankcase heater power defined for the DX heating coil (Ref. [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed)) is enabled during the time that the compressor is not running for either heating or cooling. The value for this input field must be greater than or equal to 0, and the default value is 0. To simulate a DX cooling coil without a crankcase heater, enter a value of 0.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the compressor's crankcase heater is disabled. The value for this input field must be greater than or equal to 0.0°C, and the default value is 10°C.

#### Field: Number of Capacity Stages

This integer field defines the number of capacity stages. The value for this input field must be either 1 or 2, and the default value is 1. Larger DX units often have two capacity stages, which are often two completely independent compressor/coil circuits with the evaporator coils arranged in parallel in the supply air stream. 2-stage operation affects cycling losses and latent degradation due to re-evaporation of moisture with continuous fan operation.

#### Field: Number of Enhanced Dehumidification Modes

This integer field defines the number of enhanced dehumidification modes available. The value for this input field must be 0 or 1, and the default value is 0. If the DX unit can switch operating modes to increase dehumidification based on a humidistat signal, then set this to 1. This field just specified the availability of enhanced dehumidification. Actual control of the operating mode is handled by the coil's parent component.

#### Field: Normal Mode Stage 1 Coil Performance Object Type

#### Field: Normal Mode Stage 1 Coil Performance Object Name

This pair of fields specifies the object type and name for the coil performance object which specifies the DX coil performance for stage 1 operation without enhanced dehumidification (normal mode). The only valid performance object type is [CoilPerformance:DX:Cooling](#coilperformancedxcooling).

#### Field: Normal Mode Stage 1+2 Coil Performance Object Type

#### Field: Normal Mode Stage 1+2 Coil Performance Object Name

This pair of fields specifies the object type and name for the coil performance object which specifies the DX coil performance for stage 1+2 operation (both stages active) without enhanced dehumidification (normal mode). The only valid performance object type is [CoilPerformance:DX:Cooling](#coilperformancedxcooling).

#### Field: Dehumidification Mode 1 Stage 1 Coil Performance Object Type

#### Field: Dehumidification Mode 1 Stage 1 Coil Performance Object Name

This pair of fields specifies the object type and name for the coil performance object which specifies the DX coil performance for stage 1 operation with enhanced dehumidification active. The only valid performance object type is [CoilPerformance:DX:Cooling](#coilperformancedxcooling).

#### Field: Dehumidification Mode 1 Stage 1+2 Coil Performance Object Type

#### Field: Dehumidification Mode 1 Stage 1+2 Coil Performance Object Name

This pair of fields specifies the object type and name for the coil performance object which specifies the DX coil performance for stage 1+2 operation (both stages active) with enhanced dehumidification active. The only valid performance object type is [CoilPerformance:DX:Cooling](#coilperformancedxcooling).

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the coil obtains water used for evaporative cooling. If blank or omitted, then the cooler will obtain water directly from the mains. If the name of a Water Storage Tank object is used here, then the cooler will obtain its water from that tank. If a tank is specified, the coil will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the cooler needs, then the cooler will still operate and obtain the rest of the water it needs from the mains (referred to as StarvedWater).

#### Field: Condensate Collection Water Storage Tank Name

This field is optional. It is used to describe where condensate from the coil is collected. If blank or omitted, then any coil condensate is discarded. Enter the name of Water Storage Tank object defined elsewhere and the condensate will then be collected in that tank.

#### Field: Basin Heater Capacity

This numeric field contains the capacity of the DX coil's electric evaporative cooler basin heater in watts per degree Kelvin. This field only applies for Condenser Type = EvaporativelyCooled. This field is used in conjunction with the Basin Heater Setpoint Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the outdoor dry-bulb temperature. The basin heater only operates when the DX coil is off, regardless of the basin heater schedule described below. The basin heater capacity must be greater than or equal to zero, with a default value of zero if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (˚C) for the basin heater described in the previous field. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater is active when the outdoor air dry-bulb temperature falls below this setpoint temperature, as long as the DX coil is off. This set point temperature must be greater than or equal to 2˚C, and the default value is 2˚C if this field is left blank.

#### Field: Basin Heater Operating Schedule Name

This alpha field contains the name of the basin heater operating schedule. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0. The basin heater operates when scheduled on and the outdoor air dry-bulb temperature is below the set point temperature described in the previous field. If this field is left blank, the basin heater is available to operate throughout the simulation. Regardless of this schedule, the basin heater may only operate when the DX coil is off.

Following is an example IDF use of the object:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:TwoStageWithHumidityControlMode,
      DOAS Cooling Coil,                  !- Name
      HVACTemplate-Always 1,              !- Availability Schedule Name
      DOAS Supply Fan Outlet,             !- Air Inlet Node Name
      DOAS Cooling Coil Outlet,           !- Air Outlet Node Name
      ,                                   !- Crankcase Heater Capacity
      ,                                   !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater
      2,                                  !- Number of Capacity Stages
      1,                                  !- Number of Enhanced Dehumidification Modes
      CoilPerformance:DX:Cooling,         !- Normal Mode Stage 1 Coil Performance Object Type
      DOAS Standard Perf 1,               !- Normal Mode Stage 1 Coil Performance Name
      CoilPerformance:DX:Cooling,         !- Normal Mode Stage 1+2 Coil Performance Object Type
      DOAS Standard Perf 1+2,             !- Normal Mode Stage 1+2 Coil Performance Name
      CoilPerformance:DX:Cooling,         !- Dehumidification Mode 1 Stage 1 Coil Performance Object Type
      DOAS Dehumid Perf 1,                !- Dehumidification Mode 1 Stage 1 Coil Performance Name
      CoilPerformance:DX:Cooling,         !- Dehumidification Mode 1 Stage 1+2 Coil Performance Object Type
      DOAS Dehumid Perf 1+2;              !- Dehumidification Mode 1 Stage 1+2 Coil Performance Name
~~~~~~~~~~~~~~~~~~~~

## Coil:Cooling:DX:MultiSpeed

This component models a DX cooling unit with multiple discrete levels of cooling capacity. Depending on input choices, the user can model a single compressor with multiple operating speeds, or a unit with a single cooling coil fed by multiple compressors (e.g., row split or intertwined coil circuiting). Currently, this cooling coil can only be referenced by a AirLoopHVAC:UnitaryHeatPump:AirToAir:Multispeed object. Refer to [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode) if the user wishes to model a cooling coil with discrete levels of cooling and the possibility of air bypass during low speed operation (e.g. face-split coil circuiting), or if cooling coil operation based on dehumidification requirements is desired.

The multispeed DX cooling coil can have from two to four operating speeds. When the coil operates at Speed 1 (the lowest speed), its performance is very similar to the single speed DX coil where the impacts of part-load ratio and latent capacity degradation can be included. When the coil operates at higher speeds (above Speed 1), the linear approximation methodology is applied. The coil outputs at two consecutive speeds are linearly interpolated to meet the required cooling capacity during an HVAC system timestep. When the coil performs above the lowest speed, the user can chose if they want to include part-load ratio and latent capacity degradation impacts at the higher speeds.

The multispeed unit is described by specifying the performance at different operating speeds. Each speed has its own set of input specifications: full load capacity, SHR, COP and air flow rate at rated conditions, along with modifier curves to determine performance when actual operating conditions are different from the rated conditions.

The coil operates to meet the sensible capacity being requested. When this requested capacity is above the sensible capacity of the highest operating speed, the coil runs continuously at the highest speed. When the requested capacity is between the sensible capacities of two consecutive speeds, the unit will operate a portion of the time at each speed to meet the request. When the requested capacity is less than the low speed (Speed 1) capacity, the unit will cycle on/off as needed.

### Inputs

#### Field: Name

A unique user-assigned name for an instance of a multispeed DX cooling coil. Any reference to this DX coil by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the DX cooling coil can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Air Inlet Node Name

The name of the HVAC system node from which the DX cooling coil draws its inlet air.

#### Field: Air Outlet Node Name

The name of the HVAC system node to which the DX cooling coil sends its outlet air.

#### Field: Condenser Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the outdoor condenser. If this field is left blank, the outdoor air temperature entering the condenser (dry-bulb or wet-bulb) is taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data.

#### Field: Condenser Type

#### **The type of condenser used by the multispeed DX cooling coil. Valid choices for this input field are** *AirCooled* **or** *EvaporativelyCooled***. The default for this field is** *AirCooled***.**

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the coil obtains water used for evaporative cooling. If blank or omitted, then the evaporative cooler will obtain water directly from the mains. If the name of a Water Storage Tank object is used here, then the cooler will obtain its water from that tank. If a tank is specified, the coil will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the cooler needs, then the cooler will still operate and obtain the rest of the water it needs from the mains (referred to as StarvedWater).

#### Field: Condensate Collection Water Storage Tank Name

This field is optional. It is used to describe where condensate from the coil is collected. If blank or omitted, then any coil condensate is discarded. Enter the name of Water Storage Tank object defined elsewhere and the condensate will then be collected in that tank.

#### Field: Apply Part Load Fraction to Speeds Greater than 1

This field determines whether part-load impacts on coil energy use are applied when the coil is operating at speeds greater than speed 1. The allowed choices are Yes or No, with the default being No if this field is left blank. Other input fields in this object allow the user to specify a part-load fraction correlation for each speed to account for compressor start up losses (cycle on/off). For the case of a single multi-speed compressor, the part load losses may only be significant when the compressor cycles between speed 1 and off, but the losses may be extremely small when the compressor operates between speed 1 and speed 2 (or between speeds 2 and 3, etc.). In this case, the user may chose to specify NO for this input field to neglect part-load impacts on energy use at higher operating speeds. If part-load impacts on coil energy use are thought to be significant (e.g., interwined cooling coil with multiple compressors feeding individual refrigerant circuits), then the user may chose to specify YES and the part-load fraction correlations specified for speeds 2 through 4 will be applied as appropriate. The selection for this input field does not affect part-load impacts when the compressor cycles between speed 1 and off  (i.e., the part-load fraction correlation for speed 1 is always applied).

#### Field: Apply Latent Degradation to Speeds Greater than 1

This field determines whether latent capacity degradation is applied when the coil is operating at speeds greater than speed 1. The allowed choices are Yes or No, with the default being No if this field is left blank. Other input fields in this object allow the user to specify latent capacity degradation at each speed.

The latent capacity degradation model only applies when the ContinuousFanWithCyclingCompressor supply air fan operating mode is specified, to account for moisture evaporation from the wet cooling coil when the compressor cycles off but the supply air fan continues to operate. For the case of a single multi-speed compressor, latent capacity degradation may only be significant when the compressor cycles between speed 1 and off, but the losses may be extremely small when the compressor operates between speed 1 and speed 2 (or between speeds 2 and 3, etc.). In this case, the user may chose to specify NO for this input field to neglect latent capacity degradation impacts at higher operating speeds. If latent capacity degradation is thought to be significant (e.g., interwined or row-split cooling coil with multiple compressors feeding individual refrigerant circuits), then the user may chose to specify YES and the latent capacity degradation model will be applied for speeds 2 through 4 as appropriate. The selection for this input field does not affect latent capacity degradation between speed 1 and off.

#### Field: Crankcase Heater Capacity

This numeric field defines the crankcase heater capacity in Watts. When the outdoor air dry-bulb temperature is below the value specified in the input field "Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation" (described below), the crankcase heater is enabled during the time that the compressor is not running. The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0. To simulate a unit without a crankcase heater, enter a value of 0.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the compressor's crankcase heater is disabled. The value for this input field must be greater than or equal to 0.0°C. If this input field is left blank, the default value is 10°C.

#### Field: Basin Heater Capacity

This numeric field contains the capacity of the DX coil's electric evaporative cooler basin heater in watts per degree Kelvin. This field only applies for Condenser Type = EvaporativelyCooled. This field is used in conjunction with the Basin Heater Setpoint Temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the outdoor dry-bulb temperature. The basin heater only operates when the DX coil is off, regardless of the basin heater schedule described below. The basin heater capacity must be greater than or equal to zero, with a default value of zero if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (˚C) for the basin heater described in the previous field. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater is active when the outdoor air dry-bulb temperature falls below this setpoint temperature, as long as the DX coil is off. This set point temperature must be greater than or equal to 2˚C, and the default value is 2˚C if this field is left blank.

#### Field: Basin Heater Operating Schedule Name

This alpha field contains the name of the basin heater operating schedule. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0. The basin heater operates when scheduled on and the outdoor air dry-bulb temperature is below the set point temperature described in the previous field. If this field is left blank, the basin heater is available to operate throughout the simulation. Regardless of this schedule, the basin heater may only operate when the DX coil is off.

#### Field: Fuel Type

This alpha field determines the type of fuel that this cooling coil uses.  This field has seven choices: Electricity, NaturalGas, PropaneGas, Coal, Diesel, Gasoline, FuelOil#1, FuelOil#2, OtherFuel1 and OtherFuel2. The default is NaturalGas.

#### Field: Number of Speeds

This field specifies the number of sets of data being entered for rated specifications, performance curves, evaporative condenser data, latent degradation data, and waste heat specifications for each cooling speed. The rated specifications consist of gross rated capacity, gross rated SHR, gross rated COP, and rated air flow rate. The performance curves consist of a total capacity modifier curve as a function of temperature, total capacity modifier curve as a function of flow fraction, energy input ratio modifier curve as a function of temperature, energy input ratio modifier curve as a function of flow fraction, and part load fraction correlation as a function of part load ratio. The evaporative condenser data consists of effectiveness, condenser air volume flow rate, and rated pump power consumption. The latent degradation data consists of nominal time for condensate removal to begin, ratio of initial moisture evaporation rate and steady-state latent capacity, maximum On/Off cycling rate, and latent capacity time constant. The latent degradation data are only applied if the supply air fan operation mode is specified as ContinuousFanWithCyclingCompressor. The waste heat specifications include the fraction of energy input to the cooling coil at the fully loaded and rated conditions, and a temperature modifier. The minimum number of speeds for cooling is 2 and the maximum number is 4. The number of speeds should be the same as the number of speeds for cooling defined in its parent object ([AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed](#airloophvacunitaryheatpumpairtoairmultispeed)). The first set of performance inputs is for Speed 1 and should be for low speed, and the last set of performance inputs should be for high speed. For example, if only three cooling speeds are defined, the first set should be for low speed (Speed 1), the second set should be for medium speed (Speed 2), and the third set should be for high speed (Speed 3). In this example, any performance inputs for Speed 4 would be neglected (since this input field specifies that the coil only has three cooling speeds).

#### Field Group: Rated Specification, Performance Curves, Latent Capacity Degradation Inputs, and Evaporative Cooled Condenser Data

The performance for each cooling speed must be specified as shown below. All inputs for Speed 1 are required first, followed by the inputs for Speed 2, Speed 3 and Speed 4.

#### Field: Speed <x> Gross Rated Total Cooling Capacity

The total, full load gross cooling capacity (sensible plus latent) in watts of the DX coil unit for Speed <x> operation at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "Rated Air Flow Rate, Speed <x>" below). Capacity should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for).

#### Field: Speed <x> Gross Rated Sensible Heat Ratio

The sensible heat ratio (SHR= gross sensible capacity divided by gross total cooling capacity) of the DX cooling coil for Speed <x> operation at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "Rated Air Flow Rate, Speed <x>" below). Both the sensible and total cooling capacities used to define the Rated SHR should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for).

#### Field: Speed <x> Gross Rated Cooling COP

The coefficient of performance is the ratio of the gross total cooling capacity in watts to electrical power input in watts) of the DX cooling coil unit for Speed <x> operation at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "Rated Air Flow Rate, Speed <x>" below). The input power includes power for the compressor(s) and condenser fan(s) but does not include the power consumption of the supply air fan. The gross COP should NOT account for the supply air fan. If this input field is left blank, the default value is 3.0.

#### Field: Speed <x> Rated Air Flow Rate

The volumetric air flow rate for Speed <x>, in m^3^ per second, across the DX cooling coil at rated conditions. The rated air volume flow rate for Speed <x> should be between 0.00004027 m^3^/s and 0.00006041 m^3^/s per watt of the gross rated total cooling capacity for Speed <x>. The gross rated total cooling capacity, gross rated SHR and gross rated COP for Speed <x> should be performance information for the unit with air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and the rated air volume flow rate defined here.

**Field: Speed <X> Rated Evaporator Fan Power Per Volume Flow Rate**

This field is the electric power for the evaporator (cooling coil) fan per air volume flow rate through the coil at the rated conditions for Speed <x> in W/(m3/s). The default value is 773.3 W/(m3/s) (365 W/1000 cfm) if this field is left blank. If a value is entered, it must be >= 0.0 and <= 1250 W/(m3/s). This value is only used to calculate Seasonal Energy Efficiency Ratio (SEER), and the Standard Rating (Net) Cooling Capacity which will be outputs in the EnergyPlus eio file (ref. EnergyPlus Engineering Reference, Multi-Speed DX Cooling Coil, Standard Ratings). This value is not used for modeling the evaporator (cooling coil) fan during simulations; instead, it is used for calculating SEER and Standard Rating Cooling Capacity to assist the user in verifying their inputs for modeling this type of equipment.

#### Field: Speed <x> Total Cooling Capacity Function of Temperature Curve Name

The name of a biquadratic performance curve (ref: Performance Curves) that parameterizes the variation of the gross total cooling capacity for Speed <x> as a function of the wet-bulb temperature of the air entering the cooling coil, and the dry-bulb temperature of the air entering the air-cooled condenser (wet-bulb temperature if modeling an evaporative-cooled condenser). The output of this curve is multiplied by the gross rated total cooling capacity for Speed <x> to give the gross total cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Speed <x> Total Cooling Capacity Function of Flow Fraction Curve Name

The name of a quadratic performance curve (ref:  Performance Curves) that parameterizes the variation of the gross total cooling capacity for Speed <x> as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate for Speed <x> (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated total cooling capacity and the total cooling capacity modifier curve (function of temperature) to give the gross total cooling capacity for Speed <x> at the specific temperature and air flow conditions at which the coil is operating. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the rated air flow rate for Speed <x>.

#### Field: Speed <x> Energy Input Ratio Function of Temperature Curve Name

The name of a biquadratic performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) for Speed <x> as a function of the wetbulb temperature of the air entering the cooling coil and the drybulb temperature of the air entering the air-cooled condenser (wetbulb temperature if modeling an evaporative-cooled condenser). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR for Speed <x> (inverse of rated COP for Speed <x>) to give the EIR for Speed <x> at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to a value of 1.0 at the rating point.

#### Field: Speed <x> Energy Input Ratio Function of Flow Fraction Curve Name

The name of a quadratic performance curve (Ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) for Speed <x> as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate for Speed <x> (i.e., fraction of full load flow). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR and the EIR modifier curve (function of temperature) to give the EIR for Speed <x> at the specific temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate for Speed <x>.

#### Field: Speed <x> Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the DX unit as a function of the part load ratio (PLR, sensible cooling load/steady-state sensible cooling capacity for Speed <x>). The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep for Speed <x>. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling.

The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply:

PLF >= 0.7   and   PLF >= PLR

If PLF < 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, then a warning message is issued and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional DX cooling coil (Speed <x>) would be:

PLF = 0.85 + 0.15(PLR)

If the user wishes to model no efficiency degradation due to compressor cycling, the part load fraction correlation should be defined as follows:

PLF = 1.0 + 0.0(PLR)

#### Field: Speed <x> Nominal Time for Condensate Removal to Begin

For Speed <x>, the nominal time (in seconds) after startup for condensate to begin leaving the coil's condensate drain line at the coil's rated airflow and temperature conditions, starting with a dry coil. Nominal time is equal to the ratio of the energy of the coil's maximum condensate holding capacity (J) to the coil's steady-state latent capacity (W). Suggested value is 1000; zero value means the latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]), and this field as well as the next three input fields for this object must have positive values in order to model latent capacity degradation for Speed <x>.

#### Field: Speed <x> Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity

For Speed <x>, the ratio of the initial moisture evaporation rate from the cooling coil (when the compressor first turns off, in Watts) and the coil's steady-state latent capacity (Watts) for Speed <x> at rated airflow and temperature conditions. Suggested value is 1.5; zero value means the latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]); and this field, the previous field and the next two fields must have positive values in order to model latent capacity degradation for Speed <x>.

#### Field: Speed <x> Maximum Cycling Rate

For Speed <x>, the maximum on-off cycling rate for the compressor (cycles per hour), which occurs at 50% run time fraction. Suggested value is 3; zero value means latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]); and this field, the previous two fields and the next field must have positive values in order to model latent capacity degradation for Speed <x>.

#### Field: Speed <x> Latent Capacity Time Constant

For Speed <x>, the time constant (in seconds) for the cooling coil's latent capacity to reach steady state after startup. Suggested value is 45; zero value means latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]), and this field as well as the previous three input fields for this object must have positive values in order to model latent capacity degradation for Speed <x>.

#### Field: Speed <x> Rated Waste Heat Fraction of Power Input

The fraction of energy input to the cooling coil that is available as recoverable waste heat at full load and rated conditions for Speed <x>.

#### Field: Speed <x> Waste Heat Function of Temperature Curve Name

The name of a biquadratic performance curve (ref: Performance Curves) that parameterizes the variation of the waste heat recovery as a function of outdoor dry-bulb temperature and the entering coil dry-bulb temperature at Speed <x>. The output of this curve is multiplied by the rated waste heat fraction at specific temperature operating conditions (i.e., at temperatures different from the rating point). The curve is normalized to a value of 1.0 at the rating point. When the fuel type is electricity, this field can remain blank since it is ignored by the program in this instance.

#### Field: Speed <x> Evaporative Condenser Effectiveness

The effectiveness of the evaporative condenser at Speed <x>, which is used to determine the temperature of the air entering the outdoor condenser coil as follows:

![](media/image345.png)\


where

*T~cond inlet~* = the temperature of the air entering the condenser coil (C)

*T~wb,o~*~~= the wet-bulb temperature of the outdoor air (C)

*T~db,o~*~~= the dry-bulb temperature of the outdoor air (C)

The resulting condenser inlet air temperature is used by the Total Cooling Capacity Modifier Curve, Speed <x> (function of temperature) and the Energy Input Ratio Modifier Curve, Speed <x> (function of temperature). The default value for this field is 0.9, although valid entries can range from 0.0 to 1.0. This field is not used when Condenser Type = Air Cooled.

If the user wants to model an air-cooled condenser, they should simply specify AirCooled in the field Condenser Type. In this case, the Total Cooling Capacity Modifier Curve, Speed <x> (function of temperature) and the Energy Input Ratio Modifier Curve, Speed <x> (function of temperature) input fields for this object should reference performance curves that are a function of outdoor dry-bulb temperature.

If the user wishes to model an evaporative-cooled condenser AND they have performance curves that are a function of the wet-bulb temperature of air entering the condenser coil, then the user should specify Condenser Type = Evap Cooled and the evaporative condenser effectiveness value should be entered as 1.0. In this case, the Total Cooling Capacity Modifier Curve, Speed <x> (function of temperature) and the Energy Input Ratio Modifier Curve, Speed <x> (function of temperature) input fields for this object should reference performance curves that are a function of the wet-bulb temperature of air entering the condenser coil.

If the user wishes to model an air-cooled condenser that has evaporative media placed in front of it to cool the air entering the condenser coil, then the user should specify Condenser Type = Evap Cooled. The user must also enter the appropriate evaporative effectiveness for the media. In this case, the Total Cooling Capacity Modifier Curve, Speed <x> (function of temperature) and the Energy Input Ratio Modifier Curve, Speed <x> (function of temperature) input fields for this object should reference performance curves that are a function of outdoor dry-bulb temperature. Be aware that the evaporative media will significantly reduce the dry-bulb temperature of the air entering the condenser coil, so the Total Cooling Capacity and EIR Modifier Curves for Speed <x> must be valid for the expected range of dry-bulb temperatures that will be entering the condenser coil.

#### Field: Speed <x> Evaporative Condenser Air Flow Rate

The air volume flow rate, in m^3^ per second, entering the evaporative condenser at Speed <x>. This value is used to calculate the amount of water used to evaporatively cool the condenser inlet air. The minimum value for this field must be greater than zero, and this input field is autosizable (equivalent to 0.000114 m^3^/s per watt of rated total cooling capacity for Speed <x> [850 cfm/ton]). This field is not used when Condenser Type = Air Cooled.

#### Field: Speed <x> Rated Evaporative Condenser Pump Power Consumption

The rated power of the evaporative condenser water pump in Watts at Speed <x>. This value is used to calculate the power required to pump the water used to evaporatively cool the condenser inlet air. The default value for this input field is zero, but it is autosizable (equivalent to 0.004266 W per watt [15 W/ton] of rated total capacity for Speed <x>). This field is not used when Condenser Type = Air Cooled.

Following is an example input for this multispeed DX cooling coil.

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:MultiSpeed,
        Heat Pump ACDXCoil 1,    !- Coil Name
        FanAndCoilAvailSched,    !- Availability Schedule
        DX Cooling Coil Air Inlet Node,  !- Coil Air Inlet Node
        Heating Coil Air Inlet Node,     !- Coil Air Outlet Node
        Outdoor Condenser Air Node,      !- Condenser Air Inlet Node Name
        AirCooled,              !- Condenser Type
        ,                        !- Name of Water Storage Tank for Supply
        ,                        !- Name of Water Storage Tank for Condensate Collection
        No,                      !- Apply Part Load Fraction to Speeds greater than 1
        No,                      !- Apply latent degradation to Speeds greater than 1
        200.0,                   !- Crankcase Heater Capacity {W}
        10.0,                    !- Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation
        ,                        !- Basin Heater Capacity {W/K}
        ,                        !- Basin Heater Setpoint Temperature {C}
        ,                        !- Basin Heater Operating Schedule Name
        NaturalGas,              !- Fuel Type
        4,                       !- Number of speeds
        7500,                    !- Gross Rated Total Cooling Capacity, Speed 1 {W}
        0.75,                    !- Gross Rated Sensible Heat Ratio, Speed 1 {dimensionless}
        3.0,                     !- Gross Rated Cooling COP, Speed 1 {dimensionless}
        0.40,                    !- Rated Air Flow Rate, Speed 1 {m3/s}
        ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate, Speed 1 {W/(m3/s)}
        HPACCoolCapFT Speed 1,   !- Total Cooling Capacity Modifier Curve, Speed 1 (temperature)
        HPACCoolCapFF Speed 1,   !- Total Cooling Capacity Modifier Curve, Speed 1 (flow fraction)
        HPACCOOLEIRFT Speed 1,   !- Energy Input Ratio Modifier Curve, Speed 1 (temperature)
        HPACCOOLEIRFF Speed 1,   !- Energy Input Ratio Modifier Curve, Speed 1 (flow fraction)
        HPACCOOLPLFFPLR Speed 1, !- Part Load Fraction Correlation, Speed 1 (part load ratio)
        1000.0,                  !- Nominal Time for Condensate Removal to Begin, Speed 1 {s}
        1.5,                     !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent
        3.0,                     !- Maximum ON/OFF Cycling Rate, Speed 1 {cycles/hr}
        45.0,                    !- Latent Capacity Time Constant, Speed 1 {s}
        0.2,                     !- Rated waste heat fraction of power input, Speed 1 {dimensionless}
        HAPCCoolWHFT Speed 1,    !- Waste heat modifier curve, Speed 1 (temperature)
        0.9,                     !- Evaporative Condenser Effectiveness, Speed 1 {dimensionless}
        0.05,                    !- Evaporative Condenser Air Volume Flow Rate, Speed 1 {m3/s}
        50,                      !- Evaporative Condenser Pump Rated Power Consumption, Speed 1 {W}
        17500,                   !- Gross Rated Total Cooling Capacity, Speed 2 {W}
        0.75,                    !- Gross Rated Sensible Heat Ratio, Speed 2 {dimensionless}
        3.0,                     !- Gross Rated Cooling COP, Speed 2 {dimensionless}
        0.85,                    !- Rated Air Flow Rate, Speed 2 {m3/s}
        ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate, Speed 2 {W/(m3/s)}
        HPACCoolCapFT Speed 2,   !- Total Cooling Capacity Modifier Curve, Speed 2 (temperature)
        HPACCoolCapFF Speed 2,   !- Total Cooling Capacity Modifier Curve, Speed 2 (flow fraction)
        HPACCOOLEIRFT Speed 2,   !- Energy Input Ratio Modifier Curve, Speed 2 (temperature)
        HPACCOOLEIRFF Speed 2,   !- Energy Input Ratio Modifier Curve, Speed 2 (flow fraction)
        HPACCOOLPLFFPLR Speed 1, !- Part Load Fraction Correlation, Speed 2 (part load ratio)
        1000.0,                  !- Nominal Time for Condensate Removal to Begin, Speed 2
        1.5,                     !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent
        3.0,                     !- Maximum ON/OFF Cycling Rate, Speed 2
        45.0,                    !- Latent Capacity Time Constant, Speed 2
        0.2,                     !- Rated waste heat fraction of power input, Speed 2 {dimensionless}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

        HAPCCoolWHFT Speed 2,    !- Waste heat modifier curve, Speed 2 (temperature)
        0.9,                     !- Evaporative Condenser Effectiveness, Speed 2 {dimensionless}
        0.1,                     !- Evaporative Condenser Air Volume Flow Rate, Speed 2 {m3/s}
        60,                      !- Evaporative Condenser Pump Rated Power Consumption, Speed 2 {W}
        25500,                   !- Gross Rated Total Cooling Capacity, Speed 3 {W}
        0.75,                    !- Gross Rated Sensible Heat Ratio, Speed 3 {dimensionless}
        3.0,                     !- Gross Rated Cooling COP, Speed 3 {dimensionless}
        1.25,                    !- Rated Air Flow Rate, Speed 3 {m3/s}
        ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate, Speed 3 {W/(m3/s)}
        HPACCoolCapFT Speed 3,   !- Total Cooling Capacity Modifier Curve, Speed 3 (temperature)
        HPACCoolCapFF Speed 3,   !- Total Cooling Capacity Modifier Curve, Speed 3 (flow fraction)
        HPACCOOLEIRFT Speed 3,   !- Energy Input Ratio Modifier Curve, Speed 3 (temperature)
        HPACCOOLEIRFF Speed 3,   !- Energy Input Ratio Modifier Curve, Speed 3 (flow fraction)
        HPACCOOLPLFFPLR Speed 1, !- Part Load Fraction Correlation, Speed 3 (part load ratio)
        1000.0,                  !- Nominal Time for Condensate Removal to Begin, Speed 3 {s}
        1.5,                     !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent
        3.0,                     !- Maximum ON/OFF Cycling Rate, Speed 3 {cycles/hr}
        45.0,                    !- Latent Capacity Time Constant, Speed 3 {s}
        0.2,                     !- Rated waste heat fraction of power input, Speed 3 {dimensionless}
        HAPCCoolWHFT Speed 3,    !- Waste heat modifier curve, Speed 3 (temperature)
        0.9,                     !- Evaporative Condenser Effectiveness, Speed 3 {dimensionless}
        0.2,                     !- Evaporative Condenser Air Volume Flow Rate, Speed 3 {m3/s}
        80,                      !- Evaporative Condenser Pump Rated Power Consumption, Speed 3 {W}
        35500,                   !- Gross Rated Total Cooling Capacity, Speed 4 {W}
        0.75,                    !- Gross Rated Sensible Heat Ratio, Speed 4 {dimensionless}
        3.0,                     !- Gross Rated Cooling COP, Speed 4 {dimensionless}
        1.75,                    !- Rated Air Flow Rate, Speed 4 {m3/s}
        ,                        !- Rated Evaporator Fan Power Per Volume Flow Rate, Speed 4 {W/(m3/s)}
        HPACCoolCapFT Speed 4,   !- Total Cooling Capacity Modifier Curve, Speed 4 (temperature)
        HPACCoolCapFF Speed 4,   !- Total Cooling Capacity Modifier Curve, Speed 4 (flow fraction)
        HPACCOOLEIRFT Speed 4,   !- Energy Input Ratio Modifier Curve, Speed 4 (temperature)
        HPACCOOLEIRFF Speed 4,   !- Energy Input Ratio Modifier Curve, Speed 4 (flow fraction)
        HPACCOOLPLFFPLR Speed 1, !- Part Load Fraction Correlation, Speed 4 (part load ratio)
        1000.0,                  !- Nominal Time for Condensate Removal to Begin, Speed 4 {s}
        1.5,                     !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent   !- Capacity, Speed 4 {dimensionless}
        3.0,                     !- Maximum ON/OFF Cycling Rate, Speed 4 {cycles/hr}
        45.0,                    !- Latent Capacity Time Constant, Speed 4 {s}
        0.2,                     !- Rated waste heat fraction of power input, Speed 4 {dimensionless}
        HAPCCoolWHFT Speed 4,    !- Waste heat modifier curve, Speed 4 (temperature)
        0.9,                     !- Evaporative Condenser Effectiveness, Speed 4 {dimensionless}
        0.3,                     !- Evaporative Condenser Air Volume Flow Rate, Speed 4 {m3/s}
        100;                     !- Evaporative Condenser Pump Rated Power Consumption, Speed 4 {W}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Coil Total Cooling Rate [W]
    HVAC,Sum,Cooling Coil Total Cooling Energy [J]
    HVAC,Average,Cooling Coil Sensible Cooling Rate [W]
    HVAC,Sum,Cooling Coil Sensible Cooling Energy [J]
    HVAC,Average,Cooling Coil Latent Cooling Rate [W]
    HVAC,Sum,Cooling Coil Latent Cooling Energy [J]
    HVAC,Average,Cooling Coil Electric Power[W]
    HVAC,Sum,Cooling Coil Electric Energy [J]
    HVAC,Average,Cooling Coil Runtime Fraction []

    If not part of AirLoopHVAC:UnitaryHeatPump:AirToAir (if part of a heat pump, crankcase heater is reported only for the heating coil):
    HVAC,Average,Cooling Coil Crankcase Heater Electric Power[W]
    HVAC,Sum,Cooling Coil Crankcase Heater Electric Energy [J]

    Evaporative-cooled condenser:
    HVAC,Average,Cooling Coil Condenser Inlet Temperature [C]
    HVAC,Sum,Cooling Coil Evaporative Condenser Water Volume[m3]
    HVAC,Average,Cooling Coil Evaporative Condenser Pump Electric Power[W]
    HVAC,Sum,Cooling Coil Evaporative Condenser Pump Electric Energy [J]
    HVAC,Average,Cooling Coil Basin Heater Electric Power[W]
    HVAC,Sum,Cooling Coil Basin Heater Electric Energy [J]
    HVAC,Sum,Cooling Coil Evaporative Condenser Mains Supply Water Volume [m3]

    Additional variables for Coil:Cooling:DX:TwoStageWithHumidityControlMode only:
    HVAC,Average,Cooling Coil Stage 2 Runtime Fraction []
    HVAC,Average,Cooling Coil Dehumidification Mode []

    Additional variables when condensate is collected using a storage tank:
    HVAC,Average,Cooling Coil Condensate Volume Flow Rate [m3/s]
    Zone,Meter,Condensate:OnSiteWater [m3]
    HVAC,Sum,Cooling Coil Condensate Volume [m3]

    Additional variables for Coil:Cooling:DX:Multispeed:
    If Fuel Type is not Electricity:
    HVAC,Average,DX Cooling Coil <Fuel Type> Power[W]
    HVAC,Sum,DX Cooling Coil <Fuel Type> Consumption[J]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Coil Total Cooling Rate [W]

This field is the total (sensible and latent) cooling rate output of the DX coil in Watts. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Total Cooling Energy [J]

This is the total (sensible plus latent) cooling output of the DX coil in Joules over the timestep being reported. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = CoolingCoils, Group Key = System (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Cooling Coil Sensible Cooling Rate [W]

This output is the moist air sensible cooling rate output of the DX coil in Watts. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Sensible Cooling Energy [J]

This is the moist air sensible cooling output of the DX coil in Joules for the timestep being reported. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Latent Cooling Rate [W]

This is the latent cooling rate output of the DX coil in Watts. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Latent Cooling Energy [J]

This is the latent cooling output of the DX coil in Joules for the timestep being reported. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Electric Power [W]

This output is the electricity consumption rate of the DX coil compressor and condenser fan(s) in Watts. This value is calculated for each HVAC system timestep, and the results are averaged for the timestep being reported.

#### Cooling Coil Electric Energy [J]

This is the electricity consumption of the DX coil compressor and condenser fan(s) in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Cooling, Group Key = System (Ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Cooling Coil Runtime Fraction []

This is the runtime fraction of the DX coil compressor and condenser fan(s) for the timestep being reported.

#### Cooling Coil Crankcase Heater Electric Power[W]

This is the average electricity consumption rate of the DX coil compressor's crankcase heater in Watts for the timestep being reported. If the DX Cooling Coil is used in a heat pump, the crankcase heater is reported only for the heating coil.

#### Cooling Coil Crankcase Heater Electric Energy [J]

This is the electricity consumption of the DX coil compressor's crankcase heater in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Cooling, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This output variable appears only when the DX Cooling Coil is not used as part of a heat pump, otherwise the crankcase heater is reported only for the heating coil.

#### Cooling Coil Condenser Inlet Temperature [C]

This is the inlet air temperature to the condenser coil in degrees C. This value can represent the outdoor air dry-bulb temperature, wet-bulb temperature, or somewhere in between from the weather data being used, depending on the value used in the input field "Evaporative Condenser Effectiveness". The temperature reported here is used in the various modifier curves related to temperature (e.g., Total Cooling Capacity Modifier Curve [function of temperature]). This output variable appears only when the DX Cooling Coil is not used as part of a heat pump, otherwise the crankcase heater is reported only for the heating coil.

#### Cooling Coil Evaporative Condenser Water Volume [m3]

This output is the amount of water used to evaporatively cool the condenser coil inlet air, in cubic meters. This output is also added to a meter with Resource Type = Water, End Use Key = Cooling, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This output variable appears only when the DX Cooling Coil is evaporatively cooled.

#### Cooling Coil Evaporative Condenser Mains Supply Water Volume [m3]

This is the volume of water drawn from mains water service for the evaporatively cooled condenser.

#### Cooling Coil Evaporative Condenser Pump Electric Power [W]

This is the average electricity consumption rate of the evaporative condenser water pump in Watts for the timestep being reported. This output variable appears only when the DX Cooling Coil is evaporatively cooled.

#### Cooling Coil Evaporative Condenser Pump Electric Energy [J]

This is the electricity consumption rate of the evaporative condenser water pump in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Cooling, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This output variable appears only when the DX Cooling Coil is evaporatively cooled.

#### Cooling Coil Stage 2 Runtime Fraction []

This is the runtime fraction of the stage 2 DX coil compressor and condenser fan(s) for the timestep being reported. Applicable only for COIL [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode) when 2 capacity stages are specified. For 2-stage systems, Cooling Coil Runtime Fraction is the stage 1 runtime fraction. These runtime fractions overlap, because stage 2 will not run unless stage 1 is already running. For example, a system where stage 1 is 60% of total capacity is passed a load of 70%. The Cooling Coil Runtime Fraction (stage 1) will be 1.0, and the Cooling Coil Stage 2 Runtime Fraction will be 0.25 [(70%-60%)/(100%-60%)].

#### Cooling Coil Dehumidification Mode []

This is the dehumidification mode for the timestep being reported. Applicable only for [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode) when enhanced dehumidification mode is available. A value of 0 indicates normal mode (extra dehumidification not active). A value of 1 indicates dehumidification mode 1 is active. Note that this is an averaged variable, so fractional values are likely to be reported for reporting frequencies longer than "detailed".

#### Cooling Coil Condensate Volume Flow Rate [m3/s]

#### Cooling Coil Condensate Volume [m3]

These outputs are the rate and volume of water collected as condensate from the coil. These reports only appear if a water storage tank is named in the input object.

#### Cooling Coil Evaporative Condenser Mains Supply Water Volume  [m3]

This is the water consumed by the DX Cooling Coil evaporatively cooled condenser that is met by the mains water. This output variable appears only when the DX Cooling Coil is evaporatively cooled.

#### Cooling Coil Basin Heater Electric Power [W]

This is the average electricity consumption rate of the basin heater in Watts for the timestep being reported. This output variable appears only when the DX Cooling Coil is evaporatively cooled and the Basin Heater Capacity is greater than 0.

#### Cooling Coil Basin Heater Electric Energy [J]

This is the electricity consumption rate of the basin heater in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Cooling, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). This output variable appears only when the DX Cooling Coil is evaporatively cooled and the Basin Heater Capacity is greater than 0.

#### Cooling Coil <Fuel Type> Power [W]

This output variable appears only when using the Coil:Cooling:DX:Multispeed object and a fuel type other than electricity is used. This variable describes the input fuel type power for the cooling coil in Watts, averaged during the timestep being reported.

#### Cooling Coil <Fuel Type> Energy [J]

This output variable appears only when using the Coil:Cooling:DX:Multispeed object and a fuel type other than electricity is used. This variable describes the input fuel type consumption for the multispeed cooling coil in the unit of Joules, summed for the timestep being reported. The electric consumption is excluded..This output is added to a meter with Resource Type = <Fuel Type>, End Use Key = Cooling, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

> Note: <Fuel Type> in the above two output variables depends on the user specified input for the Fuel Type field. In addition to Electricity, valid fuel types are NaturalGas, Propane, FuelOil#1, FuelOil#2, Coal, Diesel, Gasoline, OtherFuel1 and OtherFuel2.

## Coil:Cooling:DX:VariableSpeed

The Variable-Speed DX Cooling Coil is a collection of performance curves that represent the cooling coil at various speed levels. The performance curves should be generated from a Reference Unit data. This is an equation-fit model that resembles a black box with no usage of heat transfer equations. On the other hand, the model uses the bypass factor approach to calculate sensible heat transfer rate, similar to the one used in the single-speed DX coil. The number of speed levels can range from 1 to 10. The cooling coil has two indoor air side connections, and one optional condenser air node connection. The user needs to specify a nominal speed level, at which the gross rated total cooling capacity, and rated volumetric air rate are sized. The rated capacity and rated volumetric flow rate represent the real situation in the air loop, and are used to determine and flow rates at various speed levels in the parent objects, e.g. of [AirLoopHVAC:UnitaryHeatCool](#airloophvacunitaryheatcool), [ZoneHVAC:PackagedTerminalAirConditioner](#zonehvacpackagedterminalairconditioner), [AirLoopHVAC:UnitaryHeatPump:AirToAir](#airloophvacunitaryheatpumpairtoair) and [ZoneHVAC:PackagedTerminalHeatPump](#zonehvacpackagedterminalheatpump). It shall be mentioned that the performance correction curves, i.e. the temperature and flow fraction correction curves, should be normalized to the capacity and flow rate at each individual speed and at the rated conditions, similar to the performance curves used in the single-speed DX coil. However, the performance values, e.g. capacities, COPs, SHRs and flow rates at individual speed levels, should be given regarding a specific unit from the Reference Unit catalog data. In the following content, the statement started with "Reference Unit" means the actual Reference Unit catalog data. The rated conditions for obtaining the capacities, COPs and SHRs  are at indoor dry-bulb temperature of 26.67 ˚C (80 ˚F), wet bulb temperature of 19.44 ˚C (67 ˚F),  and the condenser entering air temperature of 35 ˚C (95 ˚F). Some equations are provided below to help explain the function of the various performance curves and data fields.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the variable-speed cooling coil.

#### Field: Air Inlet Node Name

This alpha field contains the cooling coil load side inlet node name.

#### Field: Air Outlet Node Name

This alpha field contains the cooling coil load side outlet node name.

#### Field: Number of Speeds

This numeric field contains the maximum number of speed levels that the module uses. The number of speeds, for which the user input the performance data and curves, should be equal or higher than the maximum number. The performance inputs at higher speed levels are ignored.

#### Field: Nominal Speed Level

This numeric field defines the nominal speed level, at which the rated capacity and rated air rate are correlated.

#### Field: Gross Rated Total Cooling Capacity at Selected Nominal Speed Level

This numeric field contains the gross rated total cooling capacity at the nominal speed level.  This field is autosizable. The gross rated total cooling capacity is used to determine a capacity scaling factor, as compared to the Reference Unit capacity at the nominal speed level.

![](media/image346.png)\


And then, this scaling factor is used to determine capacities at rated conditions for other speed levels, as below,

![](media/image347.png)\


#### Field: Rated Air Flow Rate at Selected Nominal Speed Level

This numeric field contains the rated volumetric air flow rate on the load side of the DX unit, corresponding to the nominal speed level. This field is autosizable. The value is used to determine an internal scaling factor, and calculate the air flow rates in the parent objects.  It is recommended that the ratio of the rated volumetric air flow rate to the rated capacity is the same as the unit performance from the Reference Unit data.

![](media/image348.png)\


And the volumetric air flow rates in the parent objects are calculated as below,

![](media/image349.png)\


#### Field: Nominal Time for Condensate Removal to Begin

This numeric field defines the nominal time (in seconds) after startup for condensate to begin leaving the coil's condensate drain line at the coil's rated airflow and temperature conditions, starting with a dry coil. Nominal time is equal to the ratio of the energy of the coil's maximum condensate holding capacity (J) to the coil's steady-state latent capacity (W). Suggested value is 1000; zero value means the latent degradation model is disabled. The default value for this field is zero.

#### Field: Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity

This numeric field defines ratio of the initial moisture evaporation rate from the cooling coil (when the compressor first turns off, in Watts) and the coil's steady-state latent capacity (Watts) at rated airflow and temperature conditions. Suggested value is 1.5; zero value means the latent degradation model is disabled. The default value for this field is zero.

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the unit as a function of the part load ratio (PLR, sensible or latent load/steady-state sensible or latent cooling capacity for Speed 1), in the case that the unit operates under the lowest speed, i.e. on/off. The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep for Speed 1. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling. The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep).

#### Field: Condenser Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the outdoor condenser. If this field is left blank, the outdoor air temperature entering the condenser (dry-bulb or wet-bulb) is taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data.

#### Field: Condenser Type

The type of condenser used by the DX cooling coil. Valid choices for this input field are AirCooled or EvaporativelyCooled. The default for this field is AirCooled.

#### Field: Evaporative Condenser Pump Rated Power Consumption

The rated power of the evaporative condenser water pump in Watts. This value is used to calculate the power required to pump the water used to evaporatively cool the condenser inlet air. The default value for this input field is zero, but it is autosizable (equivalent to 0.004266 W per watt [15 W/ton] of rated total cooling capacity). This field is not used when Condenser Type = Air Cooled.

#### Field: Crankcase Heater Capacity

This numeric field defines the crankcase heater capacity in Watts. When the outdoor air drybulb temperature is below the value specified in the input field "Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation" (described below), the crankcase heater is enabled during the time that the compressor is not running. If this cooling coil is used as part of an air-to-air heat pump, the crankcase heater defined for this DX cooling coil is ignored and the crankcase heater power defined for the DX heating coil (Ref. [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed)) is enabled during the time that the compressor is not running for either heating or cooling. The value for this input field must be greater than or equal to 0, and the default value is 0. To simulate a DX cooling coil without a crankcase heater, enter a value of 0.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the compressor's crankcase heater is disabled. The value for this input field must be greater than or equal to 0.0°C, and the default value is 10°C.

#### Field: Supply Water Storage Tank Name

This field is optional. It is used to describe where the coil obtains water used for evaporative cooling of its condenser. If blank or omitted, then the unit will obtain water directly from the mains. If the name of a Water Storage Tank object is used here, the unit will obtain its water from that tank. If a tank is specified, the unit will attempt to obtain all the water it uses from the tank. However if the tank cannot provide all the water the condenser needs, then the unit will still operate and obtain the rest of the water it needs from the mains (referred to as StarvedWater).

#### Field: Condensate Collection Water Storage Tank Name

This field is optional. It is used to describe where condensate from the coil is collected. If blank or omitted, then any coil condensate is discarded. Enter the name of Water Storage Tank object defined elsewhere and the condensate will then be collected in that tank.

#### Field: Basin Heater Capacity

This numeric field contains the capacity of the DX coil's electric evaporative cooler basin

heater in watts per degree Kelvin. This field only applies for Condenser Type = EvaporativelyCooled. This field is used in conjunction with the Basin Heater Setpoint temperature described in the following field. The basin heater electric power is equal to this field multiplied by the difference between the basin heater set point temperature and the outdoor dry-bulb temperature. The basin heater only operates when the DX coil is off, regardless of the basin heater schedule described below. The basin heater capacity must be greater than or equal to zero, with a default value of zero if this field is left blank.

#### Field: Basin Heater Setpoint Temperature

This numeric field contains the set point temperature (˚C) for the basin heater described in the previous field. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater is active when the outdoor air dry-bulb temperature falls below this setpoint temperature, as long as the DX coil is off. This set point temperature must be greater than or equal to 2˚C, and the default value is 2˚C if this field is left blank.

#### Field: Basin Heater Operating Schedule Name

This alpha field contains the name of the basin heater operating schedule. This field only applies for Condenser Type = EvaporativelyCooled. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0. The basin heater operates when scheduled on and the outdoor air dry-bulb temperature is below the set point temperature described in the previous field. If this field is left blank, the basin heater is available to operate throughout the simulation. Regardless of this schedule, the basin heater may only operate when the DX coil is off.

#### Field Group: Rated specification, performance curves

The performance for each cooling speed must be specified as shown below. They should be directly given from the Reference Unit catalog data. All inputs for Speed 1 are required, followed by the optional inputs for other speeds.

#### Field: Speed <x> Reference Unit Gross Rated Total Cooling Capacity

This numeric field defines the total, full load gross cooling capacity in watts of the air-to-air cooling coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0. Capacity should not account for supply air fan heat.

#### Field: Speed <x> Reference Unit Gross Rated Sensible Heat Ratio

This numeric field defines sensible heat transfer ratio (SHR = gross sensible cooling capacity divided by gross total cooling capacity) of the cooling coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0.0 and less than 1.0. This value should be obtained from the Reference Unit data.

#### Field: Speed <x> Reference Unit Gross Rated Cooling COP

This numeric field defines the coefficient of performance (COP= the gross total cooling capacity in watts divided by electrical power input in watts) of the cooling coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0. The input power includes power for the compressor(s), condenser fan and accessories, but does not include the supply air fan. The gross COP should Not account for the supply air fan.

#### Field: Speed <x> Reference Unit Rated Air Flow Rate

This numeric field defines the volumetric air flow rate, in m^3^ per second, across the cooling coil at rated conditions for Speed <x> operation. The value entered here should be directly from the Reference Unit data, corresponding to the given cooling capacity and COP at the speed, as above.

#### Field: Speed <x> Reference Unit Rated Condenser Air Flow Rate

This numeric field defines the condenser volumetric air flow rate, in m^3^ per second, across the condenser coil at rated conditions for Speed <x> operation. The value entered here should be directly from the Reference Unit data. This field is used to calculate water evaporation rate for an evaporatively-cooled condenser. For an air-cooled condenser, this input is not used.

#### Field: Speed <x> Reference Unit Rated Pad Effectiveness of Evap Precooling

This numeric field defines the effectiveness of condenser evaporative precooling pad at rated condition. The values of effectiveness are given at individual speed levels, since varied condenser air flow rates impact the effectiveness.

#### Field: Speed <x> Total Cooling Capacity Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the both the indoor wet-bulb and source side entering air temperature, from the Reference Unit. The output of this curve is multiplied by the gross rated total cooling capacity at the speed to give the gross total cooling capacity at specific temperature operating conditions (i.e., at an indoor air wet-bulb temperature or outdoor entering air temperature different from the rating point temperature). It should be noted that the curve is normalized to the cooling capacity at Speed<x> from the Reference Unit data, and have the value of 1.0 at the rating point.

#### Field: Speed <x> Total Cooling Capacity Function of Air Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the ratio of actual air flow rate across the cooling coil to the design air flow rate (i.e., fraction of full load flow at Speed <x>, from the Reference Unit data). The curve is normalized to have the value of 1.0 when the actual air flow rate equals the design air flow rate, at Speed <x>.

#### Field: Speed <x> Energy Input Ratio Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the both the indoor air wet-bulb and condenser entering air temperatures The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP at Speed <x> from the Reference Unit data) to give the EIR at specific temperature operating conditions (i.e., at an indoor air wet-bulb temperature or condenser entering air temperature different from the rating point temperature). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Speed <x> Energy Input Ratio Function of Air Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the cooling coil to the design air flow rate (i.e., fraction of full load flow, at Speed <x> from the Reference Unit data). The EIR is the inverse of the COP. This curve is normalized to a value of 1.0 when the actual air flow rate equals the design air flow rate.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

     Coil:Cooling:DX:VariableSpeed,
        Heat Pump ACDXCoil 1,            !- Name
        DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name
        Heating Coil Air Inlet Node,     !- Air Outlet Node Name
        10.0,                   !- Number of Speeds {dimensionless}
        10.0,                   !- Nominal Speed Level {dimensionless}
        32000,                  !- Gross Rated Total Cooling Capacity {W}
        1.7,                    !- Rated Air Flow Rate {m3/s}
        0.0,                    !- Nominal Time for Condensate to Begin Leaving the Coil {s}
        0.0,  !- Initial Moisture Evaporation Rate Divided by Steady-State AC Latent Capacity {dimensionless}
        HPACCOOLPLFFPLR,           !- Part Load Fraction Correlation Curve Name
        ,    ! - Condenser Air Inlet Node Name
        AirCooled,    ! - Condenser Type
        ,    ! - Evaporative Condenser Pump Rated Power Consumption
        200.0,                ! - Crankcase Heater Capacity, {w}
        10.0,                   !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}
        , ! - Supply Water Storage Tank Name
        ,  ! - Condensate Collection Water Storage Tank Name
        ,                       ! - Basin Heater Capacity
        , ! - Basin Heater Setpoint Temperature
        ,                       ! - Basin Heater Operating Schedule Name
        1524.1,                 !- Speed 1 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 1 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 1 Gross Rated Cooling COP {dimensionless}
        0.1359072,              !- Speed 1 Rated Air Flow Rate {m3/s}
        0.26,                   ! - Speed 1 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       ! - Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name
        1877.9,                 !- Speed 2 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 2 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 2 Gross Rated Cooling COP {dimensionless}
        0.151008,               !- Speed 2 Rated Air Flow Rate {m3/s}
        0.30,                   ! - Speed 2 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       ! - Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name
        2226.6,                 !- Speed 3 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 3 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 3 Gross Rated Cooling COP {dimensionless}
        0.1661088,              !- Speed 3 Rated Air Flow Rate {m3/s}
        0.33,                   !- Speed 3 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       !- Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name
        2911.3,                 !- Speed 4 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 4 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 4 Gross Rated Cooling COP {dimensionless}
        0.1963104,              !- Speed 4 Rated Air Flow Rate {m3/s}
        0.38,                   !- Speed 4 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       !- Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name
        3581.7,                 !- Speed 5 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 5 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 5 Gross Rated Cooling COP {dimensionless}
        0.226512,               !- Speed 5 Rated Air Flow Rate {m3/s}
        0.44,                   !- Speed 5 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       !- Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name
        4239.5,                 !- Speed 6 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 6 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 6 Gross Rated Cooling COP {dimensionless}
        0.2567136,              !- Speed 6 Rated Air Flow Rate {m3/s}
        0.50,                   ! - Speed 6 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       ! - Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name
        4885.7,                 !- Speed 7 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 7 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 7 Gross Rated Cooling COP {dimensionless}
        0.2869152,              !- Speed 7 Rated Air Flow Rate {m3/s}
        0.57,                   ! - Speed 7 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       ! - Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name
        5520.7,                 !- Speed 8 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 8 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 8 Gross Rated Cooling COP {dimensionless}
        0.3171168,              !- Speed 8 Rated Air Flow Rate {m3/s}
        0.63,                   ! - Speed 8 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       ! - Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name
        6144.8,                 !- Speed 9 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 9 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 9 Rated Cooling COP {dimensionless}
        0.3473184,              !- Speed 9 Rated Air Flow Rate {m3/s}
        0.69,                   ! - Speed 9 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       ! - Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF,         !- Energy Input Ratio Function of Flow Fraction Curve Name
        6758.0,                 !- Speed 10 Gross Rated Total Cooling Capacity {w}
        0.75,                   !- Speed 10 Gross Rated Sensible Heat Ratio {dimensionless}
        4.0,                    !- Speed 10 Gross Rated Cooling COP {dimensionless}
        0.37752,                !- Speed 10 Rated Air Flow Rate {m3/s}
        0.74,                   ! - Speed 10 Rated Condenser Air Flow Rate {m3/s}, for evaporatively cooled
        ,                       ! - Evaporative precooling effectiveness
        HPACCoolCapFT,          !- Total Cooling Capacity Function of Temperature Curve Name
        HPACCoolCapFFF,         !- Total Cooling Capacity Function of Flow Fraction Curve Name
        HPACCOOLEIRFT,          !- Energy Input Ratio Function of Temperature Curve Name
        HPACCOOLEIRFFF;         !- Energy Input Ratio Function of Flow Fraction Curve Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Coil Electric Power [W]
    HVAC,Average,Cooling Coil Total Cooling Rate [W]
    HVAC,Average,Cooling Coil Sensible Cooling Rate [W]
    HVAC,Average,Cooling Coil Source Side Heat Transfer Rate [W]
    HVAC,Average,Cooling Coil Part Load Ratio []
    HVAC,Average, Cooling Coil Runtime Fraction []
    HVAC,Average, Cooling Coil Air Mass Flow Rate [kg/s]
    HVAC,Average,Cooling Coil Air Inlet Temperature [C]
    HVAC,Average,Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,Cooling Coil Air Outlet Temperature [C]
    HVAC,Average,Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,Cooling Coil Upper Speed Level []
    HVAC,Average,Cooling Coil Neighboring Speed Levels Ratio []
    HVAC,Average,VSAirtoAirHP Recoverable Waste Heat [W]
    HVAC,Sum,Cooling Coil Electric Energy [J]
    HVAC,Sum,Cooling Coil Total Cooling Energy [J]
    HVAC,Sum,Cooling Coil Sensible Cooling Energy [J]
    HVAC,Sum,Cooling Coil Latent Cooling Energy [J]
    HVAC,Sum,Cooling Coil Source Side Heat Transfer Energy [J]
    HVAC,Average,Cooling Coil Crankcase Heater Electric Power [W]
    HVAC,Sum,Cooling Coil Crankcase Heater Electric Energy [J]
    HVAC,Average,Cooling Coil Condensate Volume Flow Rate [m3/s]
    HVAC,Sum,Cooling Coil Condensate Volume [m3]
    HVAC,Average,Cooling Coil Condenser Inlet Temperature [C]
    HVAC,Sum,Cooling Coil Evaporative Condenser Water Volume [m3]
    HVAC,Sum,Cooling Coil Evaporative Condenser Mains Water Volume [m3]
    HVAC,Average,Cooling Coil Evaporative Condenser Pump Electric Power[W]
    HVAC,Sum,Cooling Coil Evaporative Condenser Pump Electric Energy [J]
    HVAC,Average,Cooling Coil Basin Heater Electric Power [W]
    HVAC,Sum,Cooling Coil Basin Heater Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Coil Electric Power [W]

This output variable is the average electric consumption rate of the heat pump in Watts over the timestep being reported.

#### Cooling Coil Total Cooling Rate [W]

The output variable is the average total cooling load provide by the heat pump which includes the sensible and latent load in Watts over the timestep being reported.

#### Cooling Coil Sensible Cooling Rate [W]

The output variable is the average sensible cooling load provide by the heat pump in Watts over the timestep being reported.

#### Cooling Coil Source Side Heat Transfer Rate [W]

The output variable is the average heat rejected to the water at the heat pump condenser in Watts over the timestep being reported.

#### Cooling Coil Part Load Ratio []

This output variable is the ratio of the part-load capacity to the steady state capacity of the VSAirtoAirHP coil. For the cycling fan mode, the runtime fraction for the heat pump compressor may be different from the compressor part-load ratio reported here due to the part-load performance of the VSAirtoAirHP coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate.

#### Cooling Coil Runtime Fraction []

This output variable is the function of the part load ratio (PLR, part-load capacity/ steady state capacity). The runtime fraction, or duty factor, accounts for efficiency losses due to compressor cycling.

#### Cooling Coil Air Mass Flow Rate [kg/s]

The output variable is the average air mass flow rate on the load side going through the heat pump over the timestep being reported.

#### Cooling Coil Air Inlet Temperature [C]

The output variable is the average entering air dry-bulb temperature over the timestep being reported.

#### Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average entering air dry humidity ratio over the timestep being reported.

#### Cooling Coil Air Outlet Temperature [C]

The output variable is the average leaving air dry-bulb temperature over the timestep being reported.

#### Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average leaving air dry humidity ratio over the timestep being reported.

#### Cooling Coil Upper Speed Level []

The output variable is the average upper speed level, for interpolating performances between two neighboring speed levels.

#### Cooling Coil Neighboring Speed Levels Ratio []

The output variable is the average speed ratio, for interpolating performances between two neighboring speed levels.

#### Cooling Coil Electric Energy [J]

The output variable is the electric consumption of the heat pump in Joules over the timestep being reported.

#### Cooling Coil Total Cooling Energy [J]

The output variable is the total cooling output of the coil in Joules over the timestep being reported.

#### Cooling Coil Sensible Cooling Energy [J]

The output variable is the total sensible cooling output of the coil in Joules over the timestep being reported

#### Cooling Coil Latent Cooling Energy [J]

#### Cooling Coil Latent Cooling Rate [W]

These output variables are the total latent cooling output of the coil in Joules or Watts over the timestep being reported.

#### Cooling Coil Source Side Heat Transfer Energy [J]

The output variable is the total source side heat transfer of the coil in Joules over the timestep being reported.

#### Cooling Coil Crankcase Heater Electric Power [W]

The output variable is the average power used for crankcase heater, in Watts over the timestep being reported.

#### Cooling Coil Crankcase Heater Electric Energy [J]

The output variable is the total electric energy usage of the coil for crankcase heater, in Joules over the timestep being reported.

#### Cooling Coil Condensate Volume Flow Rate [m3/s]

The output variable is the average water condensate volumetric flow rate from the cooling coil, in m^3^/s over the timestep being reported, if choosing to use CondensatetoTank.

#### Cooling Coil Condensate Volume [m3]

The output variable is the total water condensate volume from the cooling coil, in m^3^ over the timestep being reported.

#### Cooling Coil Condenser Inlet Temperature [C]

The output variable is the average air temperature entering the condenser coil, in °C over the timestep being reported.

#### Cooling Coil Evaporative Condenser Water Volume [m3]

The output variable is the total water volume consumed for condenser evaporative pre-cooling, in m^3^ over the timestep being reported.

#### Cooling Coil Evaporative Condenser Mains Water Volume [m3]

The output variable is the total water volume for condenser evaporative pre-cooling, obtained from the Mains Water supply, in m^3^ over the timestep being reported.

#### Cooling Coil Evaporative Condenser Pump Electric Power[W]

The output variable is the average power consumption rate of the evaporative condenser pump, in Watts over the timestep being reported.

#### Cooling Coil Evaporative Condenser Pump Electric Energy [J]

The output variable is the total power consumption of the evaporative condenser pump, in Joules over the timestep being reported.

#### Cooling Coil Basin Heater Electric Power [W]

The output variable is the average power consumption rate by the basin heater, in Watts over the timestep being reported.

#### Cooling Coil Basin Heater Electric Energy [J]

The output variable is the total power consumption by the basin heater, in Joules over the timestep being reported.

## CoilPerformance:DX:Cooling

This coil performance object is used to specify DX coil performance for one mode of operation for a [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode). A single [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode) object will reference one to four [CoilPerformance:DX:Cooling](#coilperformancedxcooling) objects depending on the number of available stages and dehumidification modes as specified in the two stage DX object. For example, a standard 2-stage DX system will use two of these performance objects, one to defined the capacity and performance for stage 1 operation, and a second one for stage 1+2 (both stages active) operation. In nearly all cases, the Rated Air Volume Flow Rate will be the same for all performance objects associated with a given multimode DX coil. If bypass is specified, the Rated Air Volume Flow Rate includes both the bypassed flow and the flow through the active coil.

This DX coil model is identical to [Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed)  with addition of bypass and multi-stage capabilities. This DX cooling coil model and input are quite different from that for the heating and cooling water coils. The simple water coils use an NTU-effectiveness heat exchanger model. The single speed DX coil model uses performance information at rated conditions along with curve fits for variations in total capacity, energy input ratio and part-load fraction to determine performance at part-load conditions. Sensible/latent capacity splits are determined by the rated sensible heat ratio (SHR) and the apparatus dewpoint/bypass factor (ADP/BF) approach. This approach is analogous to the NTU-effectiveness calculations used for sensible-only heat exchanger calculations, extended to a cooling and dehumidifying coil.

An alternative to ADP/BF method for sensible/latent capacity split is to use SHR modifier curves for temperature and flow fraction.  These two optional input fields are used only when a user specified SHR calculation method desired over the (ADP/BF) method.  Sensible heat ratio calculated using these two SHR modifier curves override the value calculated by ADP/BF method. See section "SHR Calculation Using User Specified SHR Modifier Curves" in the EnergyPlus Engineering Document for further details.

The DX cooling coil input requires the gross rated total cooling capacity, the gross rated SHR, the gross rated COP, the rated air volume flow rate, and the fraction of air flow which is bypassed around the coil. The first 4 inputs determine the coil performance at the rating point (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb and air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb). The rated air volume flow rate (less any bypassed fraction) should be between .00004027 m^3^/s and .00006041 m^3^/s per watt of gross rated total cooling capacity (300 to 450 cfm/ton). The rated volumetric air flow to gross total cooling capacity ratio for 100% dedicated outdoor air (DOAS) application DX cooling coils should be between 0.00001677 (m3/s)/W (125 cfm/ton) and 0.00003355 (m3/s)/W (250 cfm/ton).

This model requires 5 curves as follows:

#. The total cooling capacity modifier curve (function of temperature) is a biquadratic curve with two independent variables: wet-bulb temperature of the air entering the cooling coil, and dry-bulb temperature of the air entering the air-cooled condenser coil (wet-bulb temperature if modeling an evaporative-cooled condenser). The output of this curve is multiplied by the gross rated total cooling capacity to give the gross total cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures).
#. The total cooling capacity modifier curve (function of flow fraction) is a quadratic or cubic curve with the independent variable being the ratio of the actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated total cooling capacity and the total cooling capacity modifier curve (function of temperature) to give the gross total cooling capacity at the specific temperature and air flow conditions at which the coil is operating.
#. The energy input ratio (EIR) modifier curve (function of temperature) is a biquadratic curve with two independent variables: wet-bulb temperature of the air entering the cooling coil, and dry-bulb temperature of the air entering the air-cooled condenser coil (wet-bulb temperature if modeling an evaporative-cooled condenser). The output of this curve is multiplied by the rated EIR (inverse of the rated COP) to give the EIR at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures).
#. The energy input ratio (EIR) modifier curve (function of flow fraction) is a quadratic or cubic curve with the independent variable being the ratio of the actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the rated EIR (inverse of the rated COP) and the EIR modifier curve (function of temperature) to give the EIR at the specific temperature and air flow conditions at which the coil is operating.
#. The part load fraction correlation (function of part load ratio) is a quadratic or cubic curve with the independent variable being part load ratio (sensible cooling load / steady-state sensible cooling capacity). The output of this curve is used in combination with the rated EIR and EIR modifier curves to give the "effective" EIR for a given simulation timestep. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling. The curve should be normalized to a value of 1.0 when the part-load ratio equals 1.0 (i.e., the compressor(s) run continuously for the simulation timestep).

The curves are simply specified by name. Curve inputs are described in the curve manager section of this document (see Performance Curves in this document).

The next four input fields are optional and relate to the degradation of latent cooling capacity when the supply air fan operates continuously while the cooling coil/compressor cycle on and off to meet the cooling load. The fan operating mode is either considered to be constant (e.g. [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in the parent object (e.g. [AirLoopHVAC:UnitaryHeatCool](#airloophvacunitaryheatcool)). When scheduled, the schedule value must be greater than 0 to calculate degradation of latent cooling capacity. At times when the parent object's supply air fan operating mode schedule is 0, latent degradation will be ignored. When used, these next four input fields must all have positive values in order to model latent capacity degradation.

The next input specifies the outdoor air node used to define the conditions of the air entering the outdoor condenser. If this field is not blank, the node name specified must be listed in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor temperature from the weather data. Alternately, the node name must be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor temperature entering the condenser is taken directly from the weather data. This field may also be left blank, if this is the case then the outdoor temperature entering the condenser is taken directly from the weather data.

The next input describes the type of outdoor condenser coil used with the DX cooling coil (Air Cooled or Evap Cooled). The following three inputs are required when modeling an evaporative-cooled condenser: evaporative condenser effectiveness, evaporative condenser air volume flow rate, and the power consumed by the evaporative condenser pump. See section "DX Cooling Coil Model" in the EnergyPlus Engineering Document for further details regarding this model.

### Inputs

#### Field: Name

This alpha field is a unique user-assigned name for an instance of DX cooling coil performance. Any reference to this DX coil performance object by another object will use this name.

#### Field: Gross Rated Total Cooling Capacity

The total, full load gross cooling capacity (sensible plus latent) in watts of the DX coil unit at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate" below). Capacity should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for).

#### Field: Gross Rated Sensible Heat Ratio

The sensible heat ratio (SHR=gross sensible capacity divided by gross total cooling capacity) of the DX cooling coil at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate" below). Both the sensible and total cooling capacities used to define the Rated SHR should be "gross" (i.e., the effect of supply air fan heat is NOT accounted for).

#### Field: Gross Rated Cooling COP

The coefficient of performance is the ratio of the gross total cooling capacity in watts to electrical power input in watts) of the DX cooling coil unit at rated conditions (air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/ 23.9°C wetbulb, and a cooling coil air flow rate defined by field "rated air flow rate" below). The input power includes electric power for the compressor(s) and condenser fan(s) but does not include the power consumption of the supply air fan. If this input field is left blank, the default value is 3.0.

#### Field: Rated Air Flow Rate

The air volume flow rate, in m^3^ per second, across the DX cooling coil at rated conditions. The gross rated total cooling capacity, gross rated SHR and gross rated COP should be performance information for the unit with air entering the cooling coil at 26.7°C drybulb/19.4°C wetbulb, air entering the outdoor condenser coil at 35°C drybulb/23.9°C wetbulb, and the rated air volume flow rate defined here.

#### Field: Fraction of Air Flow Bypassed Around Coil

This numeric field specifies the fraction of the Rated Air Volume Flow Rate which bypasses the active cooling coil for this performance mode. The remaining portion of the flow should be between 0.00004027 m3/s and .00006041 m3/s per watt of gross rated total cooling capacity (300 to 450 cfm/ton) for this performance mode. For DOAS applications the remaining portion of rated air volume flow rate should be between 0.00001677 m^3^/s and 0.00003355 m^3^/s per watt of gross rated total cooling capacity (125 to 250 cfm/ton).  This is used to model face-split coils on multi-stage units or bypass dampers. If total flow rate varies during simulation, the same fraction is bypassed. This input may range from 0.0 to <1.0. The default is 0.0. For a multi-stage face-split coil in which stage 1 is 60% of total capacity, this field would be set to 0.4 for the Stage 1 performance and set to 0.0 for the Stage 1+2 performance. For a DX system which activates a bypass damper for improved dehumidification, this field would be set to 0.0 for normal mode performance and set to something greater than zero for enhanced dehumidification mode performance.

#### Field: Total Cooling Capacity Function of Temperature Curve Name

The name of a **biquadratic** performance curve (ref: Performance Curves) that parameterizes the variation of the total gross cooling capacity as a function of the wet-bulb temperature of the air entering the cooling coil, and the dry-bulb temperature of the air entering the air-cooled condenser coil (wet-bulb temperature if modeling an evaporative-cooled condenser). The output of this curve is multiplied by the gross rated total cooling capacity to give the gross total cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Total Cooling Capacity Function of Flow Fraction Curve Name

The name of a **quadratic** or **cubic** performance curve (ref:  Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated total cooling capacity and the total cooling capacity modifier curve (function of temperature) to give the gross total cooling capacity at the specific temperature and air flow conditions at which the coil is operating. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Energy Input Ratio Function of Temperature Curve Name

The name of a **biquadratic** performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the wet-bulb temperature of the air entering the cooling coil, and the dry-bulb temperature of the air entering the air-cooled condenser coil (wet-bulb temperature if modeling an evaporative-cooled condenser). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP) to give the EIR at specific temperature operating conditions (i.e., at temperatures different from the rating point temperatures). The curve is normalized to a value of 1.0 at the rating point.

#### Field: Energy Input Ratio Function of Flow Fraction Curve Name

The name of a **quadratic** or **cubic** performance curve (Ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR and the EIR modifier curve (function of temperature) to give the EIR at the specific temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a **quadratic** or **cubic** performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the DX unit as a function of the part load ratio (PLR, sensible cooling load/steady-state sensible cooling capacity). The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling.

The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply:

PLF >= 0.7   and   PLF >= PLR

If PLF < 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, then a warning message is issued and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional, single-speed DX cooling coil (e.g., residential unit) would be:

PLF = 0.85 + 0.15(PLR)

If the user wishes to model no efficiency degradation due to compressor cycling, the part load fraction correlation should be defined as follows:

PLF = 1.0 + 0.0(PLR)

#### Field: Nominal Time for Condensate Removal to Begin

The nominal time (in seconds) after startup for condensate to begin leaving the coil's condensate drain line at the coil's rated airflow and temperature conditions, starting with a dry coil. Nominal time is equal to the ratio of the energy of the coil's maximum condensate holding capacity (J) to the coil's steady-state latent capacity (W). Suggested value is 1000; zero value means the latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]), and this field as well as the next three input fields for this object must have positive values in order to model latent capacity degradation.

#### Field: Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity

Ratio of the initial moisture evaporation rate from the cooling coil (when the compressor first turns off, in Watts) and the coil's steady-state latent capacity (Watts) at rated airflow and temperature conditions. Suggested value is 1.5; zero value means the latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]); and this field, the previous field and the next two fields must have positive values in order to model latent capacity degradation.

#### Field: Maximum Cycling Rate

The maximum on-off cycling rate for the compressor (cycles per hour), which occurs at 50% run time fraction. Suggested value is 3; zero value means latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]); and this field, the previous two fields and the next field must have positive values in order to model latent capacity degradation.

#### Field: Latent Capacity Time Constant

Time constant (in seconds) for the cooling coil's latent capacity to reach steady state after startup. Suggested value is 45; zero value means latent degradation model is disabled. The default value for this field is zero. The supply air fan operating mode must be continuous (i.e., the supply air fan operating mode may be specified in other "parent" objects and is assumed continuous in some objects (e.g., [CoilSystem:Cooling:DX](#coilsystemcoolingdx)) or can be scheduled in other objects [e.g., AirloopHVAC:UnitaryHeatCool]), and this field as well as the previous three input fields for this object must have positive values in order to model latent capacity degradation.

#### Field: Condenser Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the outdoor condenser. If this field is left blank, the outdoor air temperature entering the condenser (dry-bulb or wet-bulb) is taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data.

#### Field: Condenser Type

The type of condenser used by the DX cooling coil. Valid choices for this input field are **AirCooled** or **EvaporativelyCooled**. The default for this field is **AirCooled**.

#### Field: Evaporative Condenser Effectiveness

The effectiveness of the evaporative condenser, which is used to determine the temperature of the air entering the outdoor condenser coil as follows:

![](media/image350.png)\


where

*T~cond inlet~* = the temperature of the air entering the condenser coil (C)

*T~wb,o~*~~= the wet-bulb temperature of the outdoor air (C)

*T~db,o~*~~= the dry-bulb temperature of the outdoor air (C)

The resulting condenser inlet air temperature is used by the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature). The default value for this field is 0.9, although valid entries can range from 0.0 to 1.0. This field is not used when Condenser Type = Air Cooled.

If the user wants to model an air-cooled condenser, they should simply specify AirCooled in the field Condenser Type. In this case, the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature) input fields for this object should reference performance curves that are a function of outdoor dry-bulb temperature.

If the user wishes to model an evaporative-cooled condenser AND they have performance curves that are a function of the wet-bulb temperature of air entering the condenser coil, then the user should specify Condenser Type = Evap Cooled and the evaporative condenser effectiveness value should be entered as 1.0. In this case, the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature) input fields for this object should reference performance curves that are a function of the wet-bulb temperature of air entering the condenser coil.

If the user wishes to model an air-cooled condenser that has evaporative media placed in front of it to cool the air entering the condenser coil, then the user should specify Condenser Type = Evap Cooled. The user must also enter the appropriate evaporative effectiveness for the media. In this case, the Total Cooling Capacity Modifier Curve (function of temperature) and the Energy Input Ratio Modifier Curve (function of temperature) input fields for this object should reference performance curves that are a function of outdoor dry-bulb temperature. Be aware that the evaporative media will significantly reduce the dry-bulb temperature of the air entering the condenser coil, so the Total Cooling Capacity and EIR Modifier Curves must be valid for the expected range of dry-bulb temperatures that will be entering the condenser coil.

#### Field: Evaporative Condenser Air Flow Rate

The air volume flow rate, in m^3^ per second, entering the evaporative condenser. This value is used to calculate the amount of water used to evaporatively cool the condenser inlet air. The minimum value for this field must be greater than zero, and this input field is autosizable (equivalent to 0.000144 m^3^/s per watt of rated total cooling capacity [850 cfm/ton]). This field is not used when Condenser Type = Air Cooled.

#### Field: Evaporative Condenser Pump Rated Power Consumption

The rated power of the evaporative condenser water pump in Watts. This value is used to calculate the power required to pump the water used to evaporatively cool the condenser inlet air. The default value for this input field is zero, but it is autosizable (equivalent to 0.004266 W per watt [15 W/ton] of rated total cooling capacity). This field is not used when Condenser Type = AirCooled.

#### Field: Sensible Heat Ratio Function of Temperature Curve Name

The name of a biquadratic normalized curve (Ref: Performance Curves) that parameterizes the variation of the sensible heat ratio (SHR) as a function of DX cooling coil entering air wet-bulb and dry-bulb temperatures. The output of this curve is multiplied by the rated SHR and the SHR modifier curve (function of flow fraction) to give the SHR at the specific coil entering air temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 at the rated condition. This input field is optional.

#### Field: Sensible Heat Ratio Function of Flow Fraction Curve Name

The name of a quadratic or cubic normalized curve (Ref: Performance Curves) that parameterizes the variation of the sensible heat ratio (SHR) as a function of the ratio of actual air flow rate across the cooling coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the rated SHR and the SHR modifier curve (function of temperature) to give the SHR at the specific temperature and air flow conditions at which the cooling coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate. This input field is optional.

Following is an example input for a [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode) with 2 capacity stages and one enhanced dehumidification mode so it requires four  [CoilPerformance:DX:Cooling](#coilperformancedxcooling) objects.

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:TwoStageWithHumidityControlMode,
        DXSystem 1 Cooling Coil, !- Coil Name
        OfficeHeatCoolAvail,     !- Availability Schedule
        DXSystem 1 Mixed Air Node,  !- Coil Air Inlet Node
        DXSystem 1 Fan Air Inlet Node,  !- Coil Air Outlet Node
        ,                        !- Crankcase Heater Capacity {W}
        ,                        !- Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation {C}
        2,                       !- Number of Capacity Stages
        1,                       !- Number of Enhanced Dehumidification Modes
        CoilPerformance:DX:Cooling,  !- Normal Mode Stage 1 Coil Performance Object Type
        DXSystem 1 DX Coil Standard Mode-Stage 1,  !- Normal Mode Stage 1 Coil Performance Object Name
        CoilPerformance:DX:Cooling,  !- Normal Mode Stage 1+2 Coil Perf Object Type
        DXSystem 1 DX Coil Standard Mode-Stage 1&2,  !- Normal Mode Stage 1+2 Coil Perf Object Name
        CoilPerformance:DX:Cooling,  !- Dehumid Mode 1 Stage 1 Coil Perf Object Type
        DXSystem 1 DX Coil SubCoolReheat Mode-Stage 1,  !- Dehumid Mode 1 Stage 1 Coil Perf Object Name
        CoilPerformance:DX:Cooling,  !- Dehumid Mode 1 Stage 1+2 Coil Perf Object Type
        DXSystem 1 DX Coil SubCoolReheat Mode-Stage 1&2;  !- Dehumid Mode 1 Stage 1+2 Coil Perf Object Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    CoilPerformance:DX:Cooling,
        DXSystem 1 DX Coil Standard Mode-Stage 1,  !- Coil Performance Specification Name
        21327.57812,             !- Gross Rated Total Cooling Capacity {W}
        0.68,                    !- Gross Rated Sensible Heat Ratio
        3.56,                    !- Gross Rated Cooling COP
        1.695372105,             !- Rated Air Flow Rate {m3/s}
        0.4,                     !- Fraction of Air Flow Bypassed Around Coil
        HPACCoolCapFT,           !- Total Cooling Capacity Modifier Curve (function of temperature)
        HPACCoolCapFFF,          !- Total Cooling Capacity Modifier Curve (function of flow fraction)
        HPACCOOLEIRFT,           !- Energy Input Ratio Modifier Curve (function of temperature)
        HPACCOOLEIRFFF,          !- Energy Input Ratio Modifier Curve (function of flow fraction)
        HPACCOOLPLFFPLR,         !- Part Load Fraction Correlation (function of part load ratio)
        1000,                    !- Nominal Time for Condensate Removal to Begin {s}
        1.5, !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent Capacity {dimensionless}
        3,                       !- Maximum ON/OFF Cycling Rate {cycles/hr}
        45,                      !- Latent Capacity Time Constant {s}
        ,                        !- Condenser Air Inlet Node Name
        ,                        !- Condenser Type
        ,                        !- Evaporative Condenser Effectiveness
        ,                        !- Evaporative Condenser Air Flow Rate
        ,                        !- Evaporative Condenser Pump Rated Power Consumption
        DOAS DX Coil SHR-FT,     !- Sensible Heat Ratio Function of Temperature Curve Name
        DOAS DX Coil SHR-FF;     !- Sensible Heat Ratio Function of Flow Fraction Curve Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    CoilPerformance:DX:Cooling,
        DXSystem 1 DX Coil Standard Mode-Stage 1&2,  !- Coil Performance Specification Name
        35545.96484,             !- Gross Rated Total Cooling Capacity {W}
        0.68,                    !- Gross Rated Sensible Heat Ratio
        3.56,                    !- Gross Rated Cooling COP
        1.695372105,             !- Rated Air Flow Rate {m3/s}
        0.0,                     !- Fraction of Air Flow Bypassed Around Coil
        HPACCoolCapFT,           !- Total Cooling Capacity Modifier Curve (function of temperature)
        HPACCoolCapFFF,          !- Total Cooling Capacity Modifier Curve (function of flow fraction)
        HPACCOOLEIRFT,           !- Energy Input Ratio Modifier Curve (function of temperature)
        HPACCOOLEIRFFF,          !- Energy Input Ratio Modifier Curve (function of flow fraction)
        HPACCOOLPLFFPLR,         !- Part Load Fraction Correlation (function of part load ratio)
        1000,                    !- Nominal Time for Condensate Removal to Begin {s}
        1.5, !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent Capacity {dimensionless}
        3,                       !- Maximum ON/OFF Cycling Rate {cycles/hr}
        45,                      !- Latent Capacity Time Constant {s}
        ,                        !- Condenser Air Inlet Node Name
        ,                        !- Condenser Type
        ,                        !- Evaporative Condenser Effectiveness
        ,                        !- Evaporative Condenser Air Flow Rate
        ,                        !- Evaporative Condenser Pump Rated Power Consumption
        DOAS DX Coil SHR-FT,     !- Sensible Heat Ratio Function of Temperature Curve Name
        DOAS DX Coil SHR-FF;     !- Sensible Heat Ratio Function of Flow Fraction Curve Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    CoilPerformance:DX:Cooling,
        DXSystem 1 DX Coil SubCoolReheat Mode-Stage 1,  !- Coil Performance Specification Name
        19962.61328,             !- Gross Rated Total Cooling Capacity (gross) {W}
        0.60,                    !- Gross Rated SHR
        3.31,                    !- Gross Rated Cooling COP
        1.695372105,             !- Rated Air Flow Rate {m3/s}
        0.4,                     !- Fraction of Air Flow Bypassed Around Coil
        SubCoolReheatCoolCapFT,  !- Total Cooling Capacity Modifier Curve (function of temperature)
        SubCoolReheatCoolCapFFF, !- Total Cooling Capacity Modifier Curve (function of flow fraction)
        SubCoolReheatCOOLEIRFT,  !- Energy Input Ratio Modifier Curve (function of temperature)
        SubCoolReheatCoolEIRFFF, !- Energy Input Ratio Modifier Curve (function of flow fraction)
        SubCoolReheatCoolPLFFPLR, !- Part Load Fraction Correlation (function of part load ratio)
        1000,                    !- Nominal Time for Condensate Removal to Begin {s}
        1.5, !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent Capacity {dimensionless}
        3,                       !- Maximum ON/OFF Cycling Rate {cycles/hr}
        45,                      !- Latent Capacity Time Constant {s}
        ,                        !- Condenser Air Inlet Node Name
        ,                        !- Condenser Type
        ,                        !- Evaporative Condenser Effectiveness
        ,                        !- Evaporative Condenser Air Flow Rate
        ,                        !- Evaporative Condenser Pump Rated Power Consumption
        DOAS DX Coil SHR-FT,     !- Sensible Heat Ratio Function of Temperature Curve Name
        DOAS DX Coil SHR-FF;     !- Sensible Heat Ratio Function of Flow Fraction Curve Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    CoilPerformance:DX:Cooling,
        DXSystem 1 DX Coil SubCoolReheat Mode-Stage 1&2,  !- Coil Performance Specification Name
        33271.01953,             !- Gross Rated Total Cooling Capacity (gross) {W}
        0.60,                    !- Gross Rated SHR
        3.31,                    !- Gross Rated Cooling COP
        1.695372105,             !- Rated Air Flow Rate {m3/s}
        0.0,                     !- Fraction of Air Flow Bypassed Around Coil
        SubCoolReheatCoolCapFT,  !- Total Cooling Capacity Modifier Curve (function of temperature)
        SubCoolReheatCoolCapFFF, !- Total Cooling Capacity Modifier Curve (function of flow fraction)
        SubCoolReheatCOOLEIRFT,  !- Energy Input Ratio Modifier Curve (function of temperature)
        SubCoolReheatCoolEIRFFF, !- Energy Input Ratio Modifier Curve (function of flow fraction)
        SubCoolReheatCoolPLFFPLR, !- Part Load Fraction Correlation (function of part load ratio)
        1000,                    !- Nominal Time for Condensate Removal to Begin {s}
        1.5, !- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent Capacity {dimensionless}
        3,                       !- Maximum ON/OFF Cycling Rate {cycles/hr}
        45,                      !- Latent Capacity Time Constant {s}
        ,                        !- Condenser Air Inlet Node Name
        ,                        !- Condenser Type
        ,                        !- Evaporative Condenser Effectiveness
        ,                        !- Evaporative Condenser Air Flow Rate
        ,                        !- Evaporative Condenser Pump Rated Power Consumption
        DOAS DX Coil SHR-FT,     !- Sensible Heat Ratio Function of Temperature Curve Name
        DOAS DX Coil SHR-FF;     !- Sensible Heat Ratio Function of Flow Fraction Curve Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The [CoilPerformance:DX:Cooling](#coilperformancedxcooling) object does not have specific output variables. To request reports, use the parent object [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode) output variable options (ref. DX Cooling Coil Outputs). When requesting specific output variables by name, use the name of the parent object as the Key Value for [Output:Variable](#outputvariable) reporting objects (ref. [Output:Variable](#outputvariable) object). The name of the [CoilPerformance:DX:Cooling](#coilperformancedxcooling) object cannot be used as a Key Value in the [Output:Variable](#outputvariable) object.

## Coil:Heating:DX:SingleSpeed

The single speed heating DX coil model uses performance information at rated conditions along with curve fits for variations in total capacity, energy input ratio and part load fraction to determine performance at part-load conditions. The impacts of various defrost strategies (reverse cycle, resistive, timed or on-demand) are modeled based on a combination of user inputs and empirical models taken from the air-to-air heat pump algorithms in DOE-2.1E.

The single speed heating DX coil input requires an availability schedule, the gross rated heating capacity, the gross rated COP and the rated air volume flow rate. The latter 3 inputs determine the coil performance at the rating point (outdoor air dry-bulb temperature of 8.33 C, outdoor air wet-bulb temperature of 6.11 C, coil entering air dry-bulb temperature of 21.11 C, coil entering air wet-bulb temperature of 15.55 C). The rated air volume flow rate should be between .00004027 m^3^/s and .00006041 m^3^/s per watt of gross rated heating capacity.

Up to 6 curves are required depending on the defrost strategy selected.

#. The heating capacity modifier curve (function of temperature) can be a function of both the outdoor and indoor air dry-bulb temperature or only the outdoor air dry-bulb temperature. User has the choice of a bi-quadratic curve with two independent variables or a quadratic curve as well as a cubic curve with a single independent variable. The bi-quadratic curve is recommended if sufficient manufacturer data is available as it provides sensitivity to the indoor air dry-bulb temperature and a more realistic output. The output of this curve is multiplied by the gross rated heating capacity to give the gross heating capacity at specific temperature operating conditions (i.e., at an outdoor or indoor air temperature different from the rating point temperature).
#. The heating capacity modifier curve (function of flow fraction) is a quadratic or cubic curve with the independent variable being the ratio of the actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated heating capacity and the heating capacity modifier curve (function of temperature) to give the gross heating capacity at the specific temperature and air flow conditions at which the coil is operating.
#. The energy input ratio (EIR) modifier curve (function of temperature) can be a function of both the outdoor and indoor air dry-bulb temperature or only the outdoor air dry-bulb temperature. User has the choice of a bi-quadratic curve with two independent variables or a quadratic curve as well as a cubic curve with a single independent variable. The bi-quadratic curve is recommended if sufficient manufacturer data is available as it provides sensitivity to the indoor air dry-bulb temperature and a more realistic output. The output of this curve is multiplied by the rated EIR (inverse of the rated COP) to give the EIR at specific temperature operating conditions (i.e., at an outdoor or indoor air temperature different from the rating point temperature).
#. The energy input ratio (EIR) modifier curve (function of flow fraction) is a quadratic or cubic curve with the independent variable being the ratio of the actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the rated EIR (inverse of the rated COP) and the EIR modifier curve (function of temperature) to give the EIR at the specific temperature and air flow conditions at which the coil is operating.
#. The part load fraction correlation (function of part load ratio) is a quadratic or cubic curve with the independent variable being part load ratio (sensible heating load / steady-state heating capacity). The output of this curve is used in combination with the rated EIR and EIR modifier curves to give the "effective" EIR for a given simulation timestep. The part load fraction correlation accounts for efficiency losses due to compressor cycling.
#. The defrost energy input ratio (EIR) modifier curve (function of temperature) is a bi-quadratic curve with two independent variables: outdoor air dry-bulb temperature and the heating coil entering air wet-bulb temperature. The output of this curve is multiplied by the heating coil capacity, the fractional defrost time period and the runtime fraction of the heating coil to give the defrost power at the specific temperatures at which the coil is operating. This curve is only required when a reverse-cycle defrost strategy is specified.

The curves are simply specified by name. Curve inputs are described in the curve manager section of this document (ref. Performance Curves).

The next input item for the coil is the supply air fan operation mode. Either the supply air fan runs continuously while the DX coil cycles on/off, or the fan and coil cycle on/off together. The next two inputs define the minimum outdoor dry-bulb temperature that the heat pump compressor will operate and the maximum outdoor dry-bulb temperature for defrost operation. Crankcase heater capacity and cutout temperature are entered in the following two inputs. The final four inputs cover the type of defrost strategy (reverse-cycle or resistive), defrost control (timed or on-demand), the fractional defrost time period (timed defrost control only), and the resistive defrost heater capacity if a resistive defrost strategy is selected.

### Inputs

#### Field: Name

This alpha field defines a unique user-assigned name for an instance of a DX heating coil. Any reference to this DX heating coil by another object will use this name.

#### Field: Availability Schedule Name

This alpha field defines the name of the schedule (ref: Schedule) that denotes whether the DX heating coil can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Gross Rated Heating Capacity

This numeric field defines the total, full load gross heating capacity in watts of the DX coil unit at rated conditions (outdoor air dry-bulb temperature of 8.33 C, outdoor air wet-bulb temperature of 6.11 C, heating coil entering air dry-bulb temperature of 21.11 C, heating coil entering air wet-bulb temperature of 15.55 C, and a heating coil air flow rate defined by field "rated air flow volume" below). The value entered here must be greater than 0. Capacity should not account for supply air fan heat. The gross rated heating capacity should be within 20% of the gross rated total cooling capacity, otherwise a warning message is issued. The gross heating capacity should NOT include the effect of supply air fan heat.

#### Field: Gross Rated Heating COP

This numeric field defines the coefficient of performance (COP= the gross heating capacity in watts divided by electrical power input in watts) of the DX heating coil unit at rated conditions (outdoor air dry-bulb temperature of 8.33 C, outdoor air wet-bulb temperature of 6.11 C, coil entering air dry-bulb temperature of 21.11 C, coil entering air wet-bulb temperature of 15.55 C, and a heating coil air flow rate defined by field "rated air flow volume rate" below). The value entered here must be greater than 0. The input power includes power for the compressor(s) and outdoor fan(s) but does not include the power consumption of the indoor supply air fan. The gross COP should NOT account for the supply air fan.

#### Field: Rated Air Flow Rate

This numeric field defines the volume air flow rate, in m^3^ per second, across the DX heating coil at rated conditions. The value entered here must be greater than 0. The rated air volume flow rate should be between 0.00004027 m^3^/s and 0.00006041 m^3^/s per watt of the gross rated heating capacity. The gross rated heating capacity and gross rated COP should be performance information for the unit with outdoor air dry-bulb temperature of 8.33 C, outdoor air wet-bulb temperature of 6.11 C, heating coil entering air dry-bulb temperature of 21.11 C, heating coil entering air wet-bulb temperature of 15.55 C, and the rated air volume flow rate defined here.

#### Field: Rated Evaporator Fan Power Per Volume Flow Rate

This field is the electric power for the evaporator (heating coil) fan per air volume flow rate through the coil at the rated conditions in W/(m^3^/s). The default value is 773.3 W/(m^3^/s) (365 W/1000 cfm) if this field is left blank. If a value is entered, it must be >= 0.0 and <= 1250 W/(m^3^/s). This value is only used to calculate High Temperature Heating Standard (Net) Rating Capacity, Low Temperature Heating Standard (Net) Rating Capacity and Heating Seasonal Performance Factor (HSPF) which will be outputs in the EnergyPlus eio file (ref. EnergyPlus Engineering Reference, Single Speed DX Heating Coil, Standard Ratings). This value is not used for modeling the evaporator (heating coil) fan during simulations; instead, it is used for calculating the above standard ratings to assist the user in verifying their inputs for modeling this type of equipment.

#### Field: Air Inlet Node Name

This alpha field defines the name of the HVAC system node from which the DX heating coil draws its inlet air.

#### Field: Air Outlet Node Name

This alpha field defines the name of the HVAC system node to which the DX heating coil sends its outlet air.

#### Field: Heating Capacity Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic, quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of the total heating capacity as a function of the both the indoor and outdoor air dry-bulb temperature or just the outdoor air dry-bulb temperature depending on the type of curve selected. The bi-quadratic curve is recommended if sufficient manufacturer data is available as it provides sensitivity to the indoor air dry-bulb temperature and a more realistic output. The output of this curve is multiplied by the gross rated heating capacity to give the gross total heating capacity at specific temperature operating conditions (i.e., at an indoor air dry-bulb temperature or outdoor air dry-bulb temperature different from the rating point temperature). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Heating Capacity Function of Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (ref:  Performance Curves) that parameterizes the variation of total heating capacity as a function of the ratio of actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated heating capacity and the heating capacity modifier curve (function of temperature) to give the gross heating capacity at the specific temperature and air flow conditions at which the coil is operating. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Energy Input Ratio Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic, quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the both the indoor and outdoor air dry-bulb temperature or just the outdoor air dry-bulb temperature depending on the type of curve selected. The bi-quadratic curve is recommended if sufficient manufacturer data is available as it provides sensitivity to the indoor air dry-bulb temperature and a more realistic output. The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP) to give the EIR at specific temperature operating conditions (i.e., at an indoor air dry-bulb temperature or outdoor air dry-bulb temperature different from the rating point temperature). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Energy Input Ratio Function of Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR and the EIR modifier curve (function of temperature) to give the EIR at the specific temperature and air flow conditions at which the coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the DX unit as a function of the part load ratio (PLR, sensible cooling load/steady-state sensible cooling capacity). The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling.

The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply:

PLF >= 0.7   and   PLF >= PLR

If PLF < 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, then a warning message is issued and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional, single-speed DX cooling coil (e.g., residential unit) would be:

PLF = 0.85 + 0.15(PLR)

If the user wishes to model no efficiency degradation due to compressor cycling, the part load fraction correlation should be defined as follows:

PLF = 1.0 + 0.0(PLR)

#### Field: Defrost Energy Input Ratio Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) during reverse-cycle defrost periods as a function of the outdoor air dry-bulb temperature and the wet-bulb temperature of the air entering the indoor coil. The EIR is the inverse of the COP. The output of this curve is multiplied by the coil capacity, the fractional defrost time period and the runtime fraction of the heating coil to give the defrost power at the specific temperatures at which the indoor and outdoor coils are operating. This curve is only required when a reverse-cycle defrost strategy is selected. The curve is normalized to a value of 1.0 at the rating point conditions.

#### Field: Minimum Outdoor Dry-Bulb Temperature for Compressor Operation

This numeric field defines the minimum outdoor air dry-bulb temperature where the heating coil compressor turns off. The temperature for this input field must be greater than or equal to –20 C. If this input field is left blank, the default value is -8 C.

#### Field: Outdoor Dry-Bulb Temperature to Turn On Compressor

This numeric field defines the outdoor air dry-bulb temperature when the compressor is automatically turned back on following an automatic shut off because of low outdoor temperature. This field is only used for the calculation heating seasonal performance factor (HSPF) of heating coil. If this field is not provided, outdoor bin temperature is always considered to be greater than this temperature and ‘Minimum Outdoor dry-bulb Temperature for Compressor Operation' field described above.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Defrost Operation

This numeric field defines the outdoor air dry-bulb temperature above which outdoor coil defrosting is disabled. The temperature for this input field must be greater than or equal to 0 C and less than or equal to 7.22 C. If this input field is left blank, the default value is 5 C.

#### Field: Crankcase Heater Capacity

This numeric field defines the crankcase heater capacity in Watts. When the outdoor air dry-bulb temperature is below the value specified in the input field "Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation" (described below), the crankcase heater is enabled during the time that the compressor is not running. If this heating coil is used as part of an air-to-air heat pump (Ref. [AirLoopHVAC:UnitaryHeatPump:AirToAir](#airloophvacunitaryheatpumpairtoair) or [ZoneHVAC:PackagedTerminalHeatPump](#zonehvacpackagedterminalheatpump)), the crankcase heater defined for this DX heating coil is enabled during the time that the compressor is not running for either heating or cooling (and the crankcase heater power defined in the DX cooling coil object is disregarded in this case). The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0. To simulate a unit without a crankcase heater, enter a value of 0.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the compressor's crankcase heater is disabled. The value for this input field must be greater than or equal to 0.0 C. If this input field is left blank, the default value is 10 C.

#### Field: Defrost Strategy

This alpha field has two choices: reverse-cycle or resistive. If the reverse-cycle strategy is selected, the heating cycle is reversed periodically to provide heat to melt frost accumulated on the outdoor coil. If a resistive defrost strategy is selected, the frost is melted using an electric resistance heater. If this input field is left blank, the default defrost strategy is reverse-cycle.

#### Field: Defrost Control

This alpha field has two choices: timed or on-demand. If timed control is selected, the defrost time period is calculated based on a fixed value or compressor runtime whether or not frost has actually accumulated. For timed defrost control, the fractional amount of time the unit is in defrost is entered in the input field "Defrost Time Period Fraction" described below. If on-demand defrost control is selected, the defrost time period is calculated based on outdoor weather (humidity ratio) conditions. Regardless of which defrost control is selected, defrost does not occur above the user specified outdoor temperature entered in the input field "Maximum Outdoor Dry-bulb Temperature for Defrost Operation" described above. If this input field is left blank, the default defrost control is timed.

#### Field: Defrost Time Period Fraction

This numeric field defines the fraction of compressor runtime when the defrost cycle is active, and only applies to "timed" defrost (see Defrost Control input field above). For example, if the defrost cycle is active for 3.5 minutes for every 60 minutes of compressor runtime, then the user should enter 3.5/60 = 0.058333. The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0.058333.

#### Field: Resistive Defrost Heater Capacity

This numeric field defines the capacity of the resistive defrost heating element in Watts. This input field is used only when the selected defrost strategy is ‘resistive' (see input field "Defrost Strategy" above). The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0.

#### Field: Region Number

This optional numeric field defines the region number which is used to calculate HSPF of heating coil. The value for this input field must be between 1 and 6. If this input field is left blank, the default value is 4.

#### Field: Evaporator Air Inlet Node Name

This optional alpha field specifies the outdoor air node name used to define the conditions of the air entering the outdoor evaporator. If this field is left blank, the outdoor air temperature entering the evaporator is taken directly from the weather data. If this field is not blank, the node name specified must also be specified in an [OutdoorAir:Node](#outdoorairnode) object where the height of the node is taken into consideration when calculating outdoor air temperature from the weather data. Alternately, the node name may be specified in an [OutdoorAir:NodeList](#outdoorairnodelist) object where the outdoor air temperature is taken directly from the weather data.

Following is an example input for the object.

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:DX:SingleSpeed,
      Heat Pump DX Heating Coil 1,   ! Name of heating coil
      FanAndCoilAvailSched,          ! Heating coil schedule
      35000,                         ! Gross Rated Heating Capacity
      2.75,                          ! Gross Rated Heating COP
      1.7,                           !rated air flow rate (m3/s)
      ,                            !rated evaporator fan power per volume flow rate (m3/s)
      Heating Coil Air Inlet Node,   !heating coil air side inlet node
      SuppHeating Coil Air Inlet Node, !Heating coil air side outlet node
      HPACHeatCapFT,   ! heating cap modifier curve (temperature, C)
      HPACHeatCapFFF,  ! heating cap modifier curve (flow fraction)
      HPACHeatEIRFT,   ! energy input ratio modifier curve (temperature, C)
      HPACHeatEIRFFF,  ! energy input ratio modifier curve (flow fraction)
      HPACCOOLPLFFPLR, ! part load fraction modifier curve (function of part load ratio)
      ,         ! defrost EIR modifier curve (temp, C) not required for resistive defrost
      -5.0,            ! minimum OAT for compressor operation  (C)
      ,                ! outdoor dry-bulb temperature to turn on compressor  (C)
      5.0,             ! maximum outdoor dry-bulb temp for defrost operation (C)
      200.0,           ! Crankcase heater capacity (W)
      10.0,            ! Maximum outdoor temp for crankcase heater operation (C)
      Resistive,       ! Defrost strategy (resistive or reverse-cycle)
      Timed,           ! Defrost control (timed or on-demand)
      0.166667,       ! Defrost time period fraction (used for timed defrost control only)
      20000;! Resistive defrost heater capacity (used for resistive defrost strategy only)
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Heating Coil Total Heating Rate [W]
    HVAC,Sum,Heating Coil Total Heating Energy [J]
    HVAC,Average,Heating Coil Electric Power[W]
    HVAC,Sum,Heating Coil Electric Energy [J]
    HVAC,Average,Heating Coil Defrost Electric Power[W]
    HVAC,Sum,Heating Coil Defrost Electric Energy [J]
    HVAC,Average,Heating Coil Crankcase Heater Electric Power[W]
    HVAC,Sum,Heating Coil Crankcase Heater Electric Energy [J]
    HVAC,Average,Heating Coil Runtime Fraction []
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Total Heating Rate [W]

This field is the total heating rate output of the DX coil in Watts. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil.

#### Heating Coil Total Heating Energy [J]

This is the total heating output of the DX coil in Joules over the timestep being reported. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = CoolingCoils, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Electric Power [W]

This output is the electricity consumption rate of the DX coil compressor and outdoor fan(s) in Watts. This rate is applicable when the unit is providing heating to the conditioned zone(s), and excludes periods when the unit is in reverse-cycle defrost mode.

#### Heating Coil Electric Energy [J]

This is the electricity consumption of the DX coil compressor and condenser fan(s) in Joules for the timestep being reported. This consumption is applicable when the unit is providing heating to the conditioned zone(s), and excludes periods when the unit is in reverse-cycle defrost mode. This output is also added to a meter with Resource Type = Electricity, End Use Key = Cooling, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Defrost Electric Power [W]

This is the electricity consumption rate of the DX coil unit in Watts when the unit is in defrost mode (reverse-cycle or resistive).

#### Heating Coil Defrost Electric Energy [J]

This is the electricity consumption of the DX coil unit in Joules for the timestep being reported. This consumption is applicable when the unit is in defrost mode (reverse-cycle or resistive).

#### Heating Coil Crankcase Heater Electric Power [W]

This is the average electricity consumption rate of the DX coil compressor's crankcase heater in Watts for the timestep being reported.

#### Heating Coil Crankcase Heater Electric Energy [J]

This is the electricity consumption of the DX coil compressor's crankcase heater in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Miscellaneous, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Runtime Fraction []

This is the runtime fraction of the DX heating coil compressor and outdoor fan(s) for the timestep being reported.

## Coil:Heating:DX:MultiSpeed

### Inputs

## Coil:Heating:DX:MultiSpeed

This component models a DX heating unit with multiple discrete levels of heating capacity. Currently, this heating coil can only be referenced by a [AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed](#airloophvacunitaryheatpumpairtoairmultispeed) compound object. The multispeed DX heating coil can have from two to four operating speeds. When the coil operates at Speed 1 (the lowest speed), its performance is very similar to the [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed) object where the impacts of part-load ratio can be included. When the coil operates at higher speeds (above Speed 1), the linear approximation methodology is applied. The coil outputs at two consecutive speeds are linearly interpolated to meet the required heating capacity during an HVAC system timestep. When the coil performs above the lowest speed, the user can choose if they want to include part-load ratio impacts at the higher speeds.

The multispeed unit is described by specifying the performance at different operating speeds. Each speed has its own set of input specifications: full load capacity, COP and air flow rate at rated conditions, along with modifier curves to determine performance when actual operating conditions are different from the rated conditions.

The coil operates to meet the sensible capacity being requested. When this requested capacity is above the sensible capacity of the highest operating speed, the coil runs continuously at the highest speed. When the requested capacity is between the sensible capacities of two consecutive speeds, the unit will operate a portion of the time at each speed to meet the request. When the requested capacity is less than the low speed (Speed 1) capacity, the unit will cycle on/off as needed.

The next input defines the minimum outdoor dry-bulb temperature where the compressor will operate. The followed two inputs are related to crankcase heater operation: capacity and maximum outdoor dry-bulb temperature for crankcase heater operation. The next six inputs cover defrost operation: defrost EIR modifier curve, the maximum outdoor dry-bulb temperature for defrost operation, the type of defrost strategy (reverse-cycle or resistive), defrost control (timed or on-demand), the fractional defrost time period (timed defrost control only), and the resistive defrost heater capacity if a resistive defrost strategy is selected. The activation of defrost is dependent on outdoor conditions. The capacity reduction and energy use modification are independent of speed. The defrost EIR modifier is described below:

The defrost energy input ratio (EIR) modifier curve (function of temperature) is a bi-quadratic curve with two independent variables: outdoor air dry-bulb temperature and the heating coil entering air wet-bulb temperature. The output of this curve is multiplied by the heating coil capacity, the fractional defrost time period and the runtime fraction of the heating coil to give the defrost power at the specific temperatures at which the coil is operating. This curve is only required when a reverse-cycle defrost strategy is specified.

The next input allows the user to choose whether to apply the part load fraction correlation to speeds greater than 1 or not. The following input is the type of fuel.

Then the number of speed for heating is entered. The rest of inputs are speed dependent. Each set of data consists of gross rated heating capacity, gross rated COP, and the rated air volume flow rate. These three inputs determine the coil performance at the rating point (outdoor air dry-bulb temperature of 8.33°C, outdoor air wet-bulb temperature of 6.11°C, coil entering air dry-bulb temperature of 21.11°C, coil entering air wet-bulb temperature of 15.55°C). The rated air volume flow rate should be between .00004027 m^3^/s and .00006041 m^3^/s per watt of gross rated heating capacity. The rated waste heat fraction is needed to calculate how much waste heat is available at the rated conditions. In addition, up to 6 modifier curves are required per speed.

#. The heating capacity modifier curve (function of temperature) can be a function of both the outdoor and indoor air dry-bulb temperature or only the outdoor air dry-bulb temperature. User has the choice of a bi-quadratic curve with two independent variables or a quadratic curve as well as a cubic curve with a single independent variable. The bi-quadratic curve is recommended if sufficient manufacturer data is available as it provides sensitivity to the indoor air dry-bulb temperature and a more realistic output. The output of this curve is multiplied by the gross rated heating capacity to give the gross total heating capacity at specific temperature operating conditions (i.e., at an outdoor or indoor air temperature different from the rating point temperature).
#. The heating capacity modifier curve (function of flow fraction) is a quadratic or cubic curve with the independent variable being the ratio of the actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated heating capacity and the heating capacity modifier curve (function of temperature) to give the gross heating capacity at the specific temperature and air flow conditions at which the coil is operating.
#. The energy input ratio (EIR) modifier curve (function of temperature) can be a function of both the outdoor and indoor air dry-bulb temperature or only the outdoor air dry-bulb temperature. User has the choice of a bi-quadratic curve with two independent variables or a quadratic curve as well as a cubic curve with a single independent variable. The bi-quadratic curve is recommended if sufficient manufacturer data is available as it provides sensitivity to the indoor air dry-bulb temperature and a more realistic output. The output of this curve is multiplied by the rated EIR (inverse of the rated COP) to give the EIR at specific temperature operating conditions (i.e., at an outdoor or indoor air temperature different from the rating point temperature).
#. The energy input ratio (EIR) modifier curve (function of flow fraction) is a quadratic or cubic curve with the independent variable being the ratio of the actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the rated EIR (inverse of the rated COP) and the EIR modifier curve (function of temperature) to give the EIR at the specific temperature and air flow conditions at which the coil is operating.
#. The part load fraction correlation (function of part load ratio) is a quadratic or cubic curve with the independent variable being part load ratio (sensible heating load / steady-state heating capacity). The output of this curve is used in combination with the rated EIR and EIR modifier curves to give the "effective" EIR for a given simulation timestep. The part load fraction correlation accounts for efficiency losses due to compressor cycling.
#. The waste heat modifier curve (function of temperature) is a bi-quadratic curve with two independent variables: outdoor air dry-bulb temperature and the heating coil entering air dry-bulb temperature. The output of this curve is multiplied by the heating input energy, the waste heat fraction of heat input to give the recoverable waste heat.

The curves are simply specified by name. Curve inputs are described in the curve manager section of this document (ref. Performance Curves).

### Inputs

#### Field: Name

This alpha field defines a unique user-assigned name for an instance of a multispeed DX heating coil. Any reference to this DX heating coil by another object will use this name. The only allowed parent is [AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed](#airloophvacunitaryheatpumpairtoairmultispeed).

#### Field: Availability Schedule Name

This alpha field defines the name of the schedule (ref: Schedule) that denotes whether the multispeed DX heating coil can run during a given time period. A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during the time period. A value less than or equal to 0 (usually 0 is used) denotes that the unit must be off for the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Air Inlet Node name

This alpha field defines the name of the HVAC system node from which the DX heating coil draws its inlet air.

#### Field: Air Outlet Node Name

This alpha field defines the name of the HVAC system node to which the DX heating coil sends its outlet air.

#### Field: Minimum Outdoor Dry-Bulb Temperature for Compressor Operation

This numeric field defines the minimum outdoor air dry-bulb temperature where the heating coil compressor turns off. The temperature for this input field must be greater than or equal to –20°C. If this input field is left blank, the default value is -8°C.

#### Field: Crankcase Heater Capacity

This numeric field defines the crankcase heater capacity in Watts. When the outdoor air dry-bulb temperature is below the value specified in the input field "Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation" (described below), the crankcase heater is enabled during the time that the compressor is not running. The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0. To simulate a unit without a crankcase heater, enter a value of 0.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the compressor's crankcase heater is disabled. The value for this input field must be greater than or equal to 0.0°C. If this input field is left blank, the default value is 10°C.

#### Field: Defrost Energy Input Ratio Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) during reverse-cycle defrost periods as a function of the outdoor air dry-bulb temperature and the wet-bulb temperature of the air entering the indoor coil. The EIR is the inverse of the COP. The output of this curve is multiplied by the coil capacity, the fractional defrost time period and the runtime fraction of the heating coil to give the defrost power at the specific temperatures at which the indoor and outdoor coils are operating. This curve is only required when a reverse-cycle defrost strategy is selected. The curve is normalized to a value of 1.0 at the rating point conditions.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Defrost Operation

This numeric field defines the outdoor air dry-bulb temperature above which outdoor coil defrosting is disabled. The temperature for this input field must be greater than or equal to 0°C and less than or equal to 7.22°C. If this input field is left blank, the default value is 5°C.

#### Field: Defrost Strategy

This alpha field has two choices: reverse-cycle or resistive. If the reverse-cycle strategy is selected, the heating cycle is reversed periodically to provide heat to melt frost accumulated on the outdoor coil. If a resistive defrost strategy is selected, the frost is melted using an electric resistance heater. If this input field is left blank, the default defrost strategy is reverse-cycle.

#### Field: Defrost Control

This alpha field has two choices: timed or on-demand. If timed control is selected, the defrost time period is calculated based on a fixed value or compressor runtime whether or not frost has actually accumulated. For timed defrost control, the fractional amount of time the unit is in defrost is entered in the input field "Defrost Time Period Fraction" described below. If on-demand defrost control is selected, the defrost time period is calculated based on outdoor weather (humidity ratio) conditions. Regardless of which defrost control is selected, defrost does not occur above the user specified outdoor temperature entered in the input field "Maximum Outdoor Dry-bulb Temperature for Defrost Operation" described above. If this input field is left blank, the default defrost control is timed.

#### Field: Defrost Time Period Fraction

This numeric field defines the fraction of compressor runtime when the defrost cycle is active, and only applies to "timed" defrost (see Defrost Control input field above). For example, if the defrost cycle is active for 3.5 minutes for every 60 minutes of compressor runtime, then the user should enter 3.5/60 = 0.058333. The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0.058333.

#### Field: Resistive Defrost Heater Capacity

This numeric field defines the capacity of the resistive defrost heating element in Watts. This input field is used only when the selected defrost strategy is ‘resistive' (see input field "Defrost Strategy" above). The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0.

#### Field: Apply Part Load Fraction to Speeds Greater than 1

This field determines whether part-load impacts on coil energy use are applied when the coil is operating at speeds greater than speed 1. The allowed choices are Yes or No, with the default being No if this field is left blank. Other input fields in this object allow the user to specify a part-load fraction correlation for each speed to account for compressor start up losses (cycle on/off). For the case of a single multi-speed compressor, the part load losses may only be significant when the compressor cycles between speed 1 and off, but the losses may be extremely small when the compressor operates between speed 1 and speed 2 (or between speeds 2 and 3, etc.). In this case, the user may chose to specify **No** for this input field to neglect part-load impacts on energy use at higher operating speeds. If part-load impacts on coil energy use are thought to be significant (e.g., interwined cooling coil with multiple compressors feeding individual refrigerant circuits), then the user may chose to specify **Yes** and the part-load fraction correlations specified for speeds 2 through 4 will be applied as appropriate. The selection for this input field does not affect part-load impacts when the compressor cycles between speed 1 and off  (i.e., the part-load fraction correlation for speed 1 is always applied).

#### Field: Fuel Type

This alpha field determines the type of fuel that the chiller uses.  This field has seven choices: Electricity, NaturalGas, PropaneGas, Coal, Diesel, Gasoline, FuelOil#1, FuelOil#2, OtherFuel1 and OtherFuel2. The default is NaturalGas.

#### Field: Number of Speeds

This field specifies the number of sets of data being entered for rated specifications, performance curves, and waste heat specifications for each cooling speed. The rated specifications consist of gross rated capacity, gross rated COP, and rated air flow rate. The performance curves consist of a total capacity modifier curve as a function of temperature, total capacity modifier curve as a function of flow fraction, energy input ratio modifier curve as a function of temperature, energy input ratio modifier curve as a function of flow fraction, and part load fraction correlation as a function of part load ratio. The waste heat specifications include the fraction of energy input to the heating coil at the fully loaded and rated conditions, and a temperature modifier. The minimum number of speeds for heating is 2 and the maximum number is 4. The number of speeds should be the same as the number of speeds for heating defined in its parent object ([AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed](#airloophvacunitaryheatpumpairtoairmultispeed)). The first set of performance inputs is for Speed 1 and should be for low speed, and the last set of performance inputs should be for high speed. For example, if only three heating speeds are defined, the first set should be for low speed (Speed 1), the second set should be for medium speed (Speed 2), and the third set should be for high speed (Speed 3). In this example, any performance inputs for Speed 4 would be neglected (since this input field specifies that the coil only has three heating speeds).

#### Field Group: Rated Specification, Performance Curves, and Waste Heat Data

The performance for each heating speed must be specified as shown below. All inputs for Speed 1 are required first, followed by the inputs for Speed 2, Speed 3 and Speed 4.

#### Field: Speed <x> Gross Rated Heating Capacity

This numeric field defines the total, full load gross heating capacity in watts of the DX coil unit at rated conditions for Speed <x> operation (outdoor air dry-bulb temperature of 8.33°C, outdoor air wet-bulb temperature of 6.11°C, heating coil entering air dry-bulb temperature of 21.11°C, heating coil entering air wet-bulb temperature of 15.55°C, and a heating coil air flow rate defined by field "Rated Air Flow Rate, Speed <x>" below). The value entered here must be greater than 0. The gross heating capacity should not account for the effect of supply air fan heat.

#### Field: Speed <x> Gross Rated Heating COP

This numeric field defines the coefficient of performance (COP=gross heating capacity in watts divided by electrical power input in watts) of the DX heating coil unit at rated conditions for Speed <x> operation (outdoor air dry-bulb temperature of 8.33°C, outdoor air wet-bulb temperature of 6.11°C, coil entering air dry-bulb temperature of 21.11°C, coil entering air wet-bulb temperature of 15.55°C, and a heating coil air flow rate defined by field "Speed <x> Rated Air Flow Rate" below). The value entered here must be greater than 0. The input power includes power for the compressor(s) and outdoor fan(s) but does not include the power consumption of the indoor supply air fan. The gross heating capacity is the value entered above in the field "Gross Rated Heating Capacity". The gross COP should NOT account for the supply air fan.

#### Field: Speed <x> Rated Air Flow Rate

This numeric field defines the volume air flow rate, in m^3^ per second, across the DX heating coil at rated conditions for Speed <x> operation. The value entered here must be greater than 0. The rated air volume flow rate should be between 0.00004027 m^3^/s and 0.00006041 m^3^/s per watt of gross rated heating capacity. The gross rated heating capacity and gross rated COP should be performance information for the unit with outdoor air dry-bulb temperature of 8.33°C, outdoor air wet-bulb temperature of 6.11°C, heating coil entering air dry-bulb temperature of 21.11°C, heating coil entering air wet-bulb temperature of 15.55°C, and the rated air volume flow rate defined here.

#### Field: Speed <x> Heating Capacity Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic, quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the total heating capacity as a function of the both the indoor and outdoor air dry-bulb temperature or just the outdoor air dry-bulb temperature depending on the type of curve selected. The bi-quadratic curve is recommended if sufficient manufacturer data is available as it provides sensitivity to the indoor air dry-bulb temperature and a more realistic output. The output of this curve is multiplied by the gross rated heating capacity to give the gross total heating capacity at specific temperature operating conditions (i.e., at an indoor air dry-bulb temperature or outdoor air dry-bulb temperature different from the rating point temperature). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Speed <x> Heating Capacity Function of Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref:  Performance Curves) that parameterizes the variation of heating capacity as a function of the ratio of actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The output of this curve is multiplied by the gross rated heating capacity and the gross heating capacity modifier curve (function of temperature) to give the gross heating capacity at the specific temperature and air flow conditions at which the coil is operating. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Speed <x> Energy Input Ratio Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic, quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the both the indoor and outdoor air dry-bulb temperature or just the outdoor air dry-bulb temperature depending on the type of curve selected. The bi-quadratic curve is recommended if sufficient manufacturer data is available as it provides sensitivity to the indoor air dry-bulb temperature and a more realistic output. The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP) to give the EIR at specific temperature operating conditions (i.e., at an indoor air dry-bulb temperature or outdoor air dry-bulb temperature different from the rating point temperature). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Speed <x> Energy Input Ratio Function of Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the heating coil to the rated air flow rate (i.e., fraction of full load flow). The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR and the EIR modifier curve (function of temperature) to give the EIR at the specific temperature and air flow conditions at which the coil is operating. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Speed <x>Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (Ref: Performance Curves) that parameterizes the variation of electrical power input to the DX unit as a function of the part load ratio (PLR, sensible cooling load/steady-state sensible cooling capacity). The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling.

The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply:

PLF >= 0.7   and   PLF >= PLR

If PLF < 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, then a warning message is issued and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional DX heating coil (Speed <x>) would be:

PLF = 0.85 + 0.15(PLR)

If the user wishes to model no efficiency degradation due to compressor cycling, the part load fraction correlation should be defined as follows:

PLF = 1.0 + 0.0(PLR)

####  Field: Speed <x> Rated Waste Heat Fraction of Power Input

The fraction of heat input to heating that is available as recoverable waste heat at full load and rated conditions for Speed <x> operation.

#### Field: Speed <x> Waste Heat Function of Temperature Curve Name

The name of a bi-quadratic performance curve (ref: Performance Curves) that parameterizes the variation of the waste heat recovery as a function of outdoor dry-bulb temperature and the entering coil dry-bulb temperature for Speed <x>. The output of this curve is multiplied by the rated recoverable waste heat at specific temperature operating conditions (i.e., at temperatures different from the rating point). The curve is normalized to a value of 1.0 at the rating point. When the fuel type is electricity, the field is either left blank or ignored by the program.

Following is an example input for a multi-speed heating DX coil.

~~~~~~~~~~~~~~~~~~~~

      COIL:Heating:DX:MultiSpeed,
        Heat Pump DX Heating Coil 1,  !- Name of heat pump heating coil
        FanAndCoilAvailSched,    !- Availability Schedule
        Heating Coil Air Inlet Node,  !- Coil Air Inlet Node
        SuppHeating Coil Air Inlet Node,  !- Coil Air Outlet Node
        -8.0,                    !- Minimum Outdoor Dry-bulb Temperature for Compressor Operation {C}
        200.0,                   !- Crankcase Heater Capacity {W}
        10.0,                    !- Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation {C}
        HPACDefrostCAPFT,        !- Defrost energy input ratio modifier curve (temperature)
        7.22,                    !- Maximum Outdoor Dry-bulb Temperature for Defrost Operation
        reverse-cycle,           !- Defrost Strategy
        timed,                   !- Defrost Control
        0.058333,                !- Defrost Time Period Fraction
        2000.0,                  !- Resistive Defrost Heater Capacity {W}
        No,                      !- Apply Part Load Fraction to Speeds greater than 1
        NaturalGas,              !- Fuel type
        4,                       !- Number of speeds
        7500,                    !- Gross Rated Heating Capacity, Speed 1 {W}
        2.75,                    !- Gross Rated Heating COP, Speed 1
        0.45,                    !- Rated Air Flow Rate, Speed 1 {m3/s}
        HPACHeatCapFT Speed 1,   !- Heating Capacity Modifier Curve, Speed 1 (temperature)
        HPACHeatCapFF Speed 1,   !- Heating capacity modifier curve, Speed 1 (flow fraction)
        HPACHeatEIRFT Speed 1,   !- Energy input ratio modifier curve, Speed 1 (temperature)
        HPACHeatEIRFF Speed 1,   !- Energy input ratio modifier curve, Speed 1 (flow fraction)
        HPACHeatPLFFPLR Speed 1, !- Part load fraction correlation, Speed 1 (part load ratio)
        0.2,                     !- Rated waste heat fraction of power input, Speed 1
        HAPCHeatWHFT Speed 1,    !- Waste heat modifier curve, Speed 1 (temperature)
        17500,                   !- Gross Rated Heating Capacity, Speed 2 {W}
        2.75,                    !- Gross Rated Heating COP, Speed 2
        0.85,                    !- Rated Air Flow Rate, Speed 2 {m3/s}
        HPACHeatCapFT Speed 2,   !- Heating Capacity Modifier Curve, Speed 2 (temperature)
        HPACHeatCapFF Speed 2,   !- Heating capacity modifier curve, Speed 2 (flow fraction)
        HPACHeatEIRFT Speed 2,   !- Energy input ratio modifier curve, Speed 2 (temperature)
        HPACHeatEIRFF Speed 2,   !- Energy input ratio modifier curve, Speed 2 (flow fraction)
        HPACHeatPLFFPLR Speed 2, !- Part load fraction correlation, Speed 2 (part load ratio)
        0.2,                     !- Rated waste heat fraction of power input, Speed 2
        HAPCHeatWHFT Speed 2,    !- Waste heat modifier curve, Speed 2 (temperature)

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

        25500,                   !- Gross Rated Heating Capacity, Speed 3 {W}
        2.75,                    !- Gross Rated Heating COP, Speed 3
        1.25,                    !- Rated Air Flow Rate, Speed 3 {m3/s}
        HPACHeatCapFT Speed 3,   !- Heating Capacity Modifier Curve, Speed 3 (temperature)
        HPACHeatCapFF Speed 3,   !- Heating capacity modifier curve, Speed 3 (flow fraction)
        HPACHeatEIRFT Speed 3,   !- Energy input ratio modifier curve, Speed 3 (temperature)
        HPACHeatEIRFF Speed 3,   !- Energy input ratio modifier curve, Speed 3 (flow fraction)
        HPACHeatPLFFPLR Speed 3, !- Part load fraction correlation, Speed 3 (part load ratio)
        0.2,                     !- Rated waste heat fraction of power input, Speed 3
        HAPCHeatWHFT Speed 3,    !- Waste heat modifier curve, Speed 3 (temperature)
        35500,                   !- Gross Rated Heating Capacity, Speed 4 {W}
        2.75,                    !- Gross Rated Heating COP, Speed 4
        1.75,                    !- Rated Air Flow Rate, Speed 4 {m3/s}
        HPACHeatCapFT Speed 4,   !- Heating Capacity Modifier Curve, Speed 4 (temperature)
        HPACHeatCapFF Speed 4,   !- Heating capacity modifier curve, Speed 4 (flow fraction)
        HPACHeatEIRFT Speed 4,   !- Energy input ratio modifier curve, Speed 4 (temperature)
        HPACHeatEIRFF Speed 4,   !- Energy input ratio modifier curve, Speed 4 (flow fraction)
        HPACHeatPLFFPLR Speed 4, !- Part load fraction correlation, Speed 4 (part load ratio)
        0.2,                     !- Rated waste heat fraction of power input, Speed 4
        HAPCHeatWHFT Speed 4;    !- Waste heat modifier curve, Speed 4 (temperature)
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Heating Coil Total Heating Rate [W]
    HVAC,Sum,Heating Coil Total Heating Energy [J]
    HVAC,Average,Heating Coil Electric Power[W]
    HVAC,Sum,Heating Coil Electric Energy [J]
    HVAC,Average,Heating Coil Defrost Electric Power[W]
    HVAC,Sum,Heating Coil Defrost Electric Energy [J]
    HVAC,Average,Heating Coil Defrost Gas Rate [W]
    HVAC,Sum,Heating Coil Defrost Gas Energy [J]
    HVAC,Average,Heating Coil Crankcase Heater Electric Power[W]
    HVAC,Sum,Heating Coil Crankcase Heater Electric Energy [J]
    HVAC,Average,Heating Coil Runtime Fraction []
    If Fuel Type is not Electricity:
    HVAC,Average, Heating Coil <Fuel Type> Rate [W]
    HVAC,Sum, Heating Coil <Fuel Type> Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Total Heating Rate [W]

This field is the total heating rate output of the DX coil in Watts. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil.

#### Heating Coil Total Heating Energy [J]

This is the total heating output of the DX coil in Joules over the timestep being reported. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil. This output is also added to a meter with Resource Type = EnergyTransfer, End Use Key = HeatingCoil, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Electric Power [W]

This output variable is the input fuel type power for the heating coil in the unit of Watts, averaged during the report period. If Fuel Type is not Electricity, the value is zero.

#### Cooling Coil Electric Energy [J]

This output variable is the input fuel type consumption for the multispeed heating coil in the unit of Joules, summed during the report period. This output is also added to a meter with Resource Type = Electricity, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). If Fuel Type is not Electricity, the value is zero.

#### Heating Coil Defrost Electric Power [W]

This is the electricity consumption rate of the DX coil unit in Watts when the unit is in defrost mode (reverse-cycle or resistive). The variable is available when the defrost mode is resistive or the fuel type is electricity.

#### Heating Coil Defrost Electric Energy [J]

This is the electricity consumption of the DX coil unit in Joules for the timestep being reported. This consumption is applicable when the unit is in defrost mode (reverse-cycle or resistive). The variable is available when the defrost mode is resistive or the fuel type is electricity.

#### Heating Coil Defrost <Fuel Type> Rate [W]

This is the fuel consumption rate of the DX coil unit in Watts when the unit is in defrost mode (reverse-cycle). The variable is available when the defrost mode is reverse-cycle and the fuel type is non-electricity.

#### Heating Coil Defrost <Fuel Type> Energy [J]

This is the fuel consumption of the DX coil unit in Joules for the timestep being reported. This consumption is applicable when the unit is in defrost mode (reverse-cycle). The variable is available when the defrost mode is reverse-cycle and the fuel type is non-electricity.

#### Heating Coil Crankcase Heater Electric Power [W]

This is the average electricity consumption rate of the DX coil compressor's crankcase heater in Watts for the timestep being reported. When a companion cooling coil exits, the crankcase heater power of the companion cool coil is alos reported in this variable.

#### Heating Coil Crankcase Heater Electric Energy [J]

This is the electricity consumption of the DX coil compressor's crankcase heater in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). When a companion cooling coil exits, the crankcase heater power of the companion cool coil is alos reported in this variable.

#### Heating Coil Runtime Fraction []

This is the runtime fraction of the DX heating coil compressor and outdoor fan(s) for the timestep being reported. When the heating speed is above 1, this output is the run time fraction for the higher speed.

#### Heating Coil <Fuel Type> Rate [W]

This output variable is the input fuel type power for the heating coil in the unit of Watts, averaged during the report period. The electric power is excluded. If Fuel Type is Electricity, this output variable is not reported.

#### Cooling Coil <Fuel Type> Energy [J]

This output variable is the input fuel type consumption for the multispeed heating coil in the unit of Joules, summed during the report period. The electric consumption is excluded. This output is also added to a meter with Resource Type = <Fuel Type>, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects). If Fuel Type is Electricity, this output variable is not reported.

> Note: The Fuel Type defined in the above two output variables depends on the input in the fuel type filed. In addition to Electricity, Valid fuel types are NaturalGas, Propane, FuelOil#1, FuelOil#2, Coal, Diesel, Gasoline, OtherFuel1 and OtherFuel2.

## Coil:Heating:DX:VariableSpeed

The Variable-Speed Air-to-Air Heating DX Coil is a collection of performance curves that represent the heating coil at various speed levels. The performance curves should be generated from the heat pump Reference Unit catalog data. This is an equation-fit model that resembles a black box with no usage of heat transfer equations. The number of speed levels can range from 1 to 10. The heating coil has two air side node connections. The user needs to specify a nominal speed level, at which the gross rated capacity and rated volumetric air flow rate are sized. The gross rated capacity and rated volumetric flow rate represent the real situation in the air loop, and are used to determine the flow rates at various speed levels in the parent objects, e.g. [AirLoopHVAC:UnitaryHeatPump:AirToAir](#airloophvacunitaryheatpumpairtoair), [ZoneHVAC:PackagedTerminalHeatPump](#zonehvacpackagedterminalheatpump), etc. It shall be mentioned that the performance correction curves, i.e. the temperature and flow fraction correction curves, should be normalized to the capacity and flow rate at each individual speed and at the rated operating conditions, similar to the performance curves used in the single-speed DX coil. On the other hand, the performance values at individual speed levels, e.g. capacities, COPs and flow rates, should be given regarding a specific unit from the Reference Unit catalog data. In the following content, the statement started with "Reference Unit" means the actual Reference Unit catalog data. The rated conditions for obtaining the capacities and COPs  are at indoor dry-bulb temperature of 21.1 ˚C (70 ˚F) and the source side entering air temperature of 8.3 ˚C (47 ˚F). Some equations are provided below to help explain the function of the various performance curves and data fields.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the variable speed heating coil.

#### Field: Air Inlet Node Name

This alpha field contains the heating coil load side inlet node name.

#### Field: Air Outlet Node Name

This alpha field contains the heating coil load side outlet node name.

#### Field: Number of Speeds

This numeric field contains the maximum number of speed levels that the module uses. The number of speeds, for which the user input the performance data and curves, has to be equal to or higher than the maximum number. The performance inputs at higher speeds are ignored.

#### Field: Nominal Speed Level

This numeric field defines the nominal speed level, at which the rated capacity, rated air and volumetric flow rate are correlated.

#### Field: Gross Rated Heating Capacity at Selected Nominal Speed Level

This numeric field contains the rated capacity at the nominal speed level.  This field is autosizable. The gross rated heating capacity is used to determine a capacity scaling factor, as compared to the Reference Unit capacity at the nominal speed level.

![](media/image351.png)\


And then, this scaling factor is used to determine capacities at rated conditions for other speed levels, as below,

![](media/image352.png)\


#### Field: Rated Volumetric Air Flow Rate

This numeric field contains the rated volumetric air flow rate on the load side of the heat pump corresponding to the nominal speed level. This field is autosizable. The value is used to determine an internal scaling factor, and calculate the air flow rates in the parent objects.  It is recommended that the ratio of the rated volumetric air flow rate to the rated capacity is the same as the unit performance from the Reference Unit data.

![](media/image353.png)\


And the volumetric air flow rates in the parent objects are calculated as below,

#### !!! {:error "Image in Header" :alt "" :img "media/image354.png"} !!! 

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the unit as a function of the part load ratio (PLR, heating load/steady-state heating capacity for Speed 1), in the case that the unit operates under the lowest speed, i.e. on/off. The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep for Speed 1. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling. The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep).

#### Field: Defrost Energy Input Ratio Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic performance curve (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) during reverse-cycle defrost periods as a function of the outdoor air dry-bulb temperature and the wet-bulb temperature of the air entering the indoor coil. The EIR is the inverse of the COP. The output of this curve is multiplied by the coil capacity at the maximum speed level, the fractional defrost time period and the runtime fraction of the heating coil to give the defrost power at the specific temperatures at which the indoor and outdoor coils are operating. This curve is only required when a reverse cycle defrost strategy is selected. The curve is normalized to a value of 1.0 at the rating point conditions.

#### Field: Minimum Outdoor Dry-Bulb Temperature for Compressor Operation

This numeric field defines the minimum outdoor air dry-bulb temperature where the heating coil compressor turns off. If this input field is left blank, the default value is -8 °C.

#### Field: Outdoor Dry-Bulb Temperature to Turn On Compressor

This numeric field defines the outdoor temperature when the compressor is automatically turned back on following an automatic shut off because of low outdoor dry-bulb temperature. This field is only used for the calculation of HSPF. If this field is not provided, then outdoor bin temperature used in the HSPF calculation is always considered to be greater than this temperature and 'Minimum Outdoor Dry-Bulb Temperature for Compressor Operation' field described above. This assumption is based on AHRI standard 210/240 (2008) and can introduce significant error in the final value of HSPF.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Defrost Operation

This numeric field defines the outdoor air dry-bulb temperature above which outdoor coil defrosting is disabled. The temperature for this input field must be greater than or equal to 0 °C and less than or equal to 7.22 °C. If this input field is left blank, the default value is 5 °C.

#### Field: Crankcase Heater Capacity

This numeric field defines the crankcase heater capacity in Watts. When the outdoor air drybulb temperature is below the value specified in the input field "Maximum Outdoor Dry-bulb Temperature for Crankcase Heater Operation" (described below), the crankcase heater is enabled during the time that the compressor is not running. The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0. To simulate a unit without a crankcase heater, enter a value of 0.

#### Field: Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation

This numeric field defines the outdoor air dry-bulb temperature above which the compressor's crankcase heater is disabled. The value for this input field must be greater than or equal to 0.0 °C. If this input field is left blank, the default value is 10 °C.

#### Field: Defrost Strategy

This alpha field has two choices: reverse-cycle or resistive. If the reverse-cycle strategy is selected, the heating cycle is reversed periodically to provide heat to melt frost accumulated on the outdoor coil. If a resistive defrost strategy is selected, the frost is melted using an electric resistance heater. If this input field is left blank, the default defrost strategy is reverse cycle.

#### Field: Defrost Control

This alpha field has two choices: timed or on-demand. If timed control is selected, the defrost time period is calculated based on a fixed value or compressor runtime whether or not frost has actually accumulated. For timed defrost control, the fractional amount of time the unit is in defrost is entered in the input field "Defrost Time Period Fraction" described below. If on-demand defrost control is selected, the defrost time period is calculated based on outdoor weather (humidity ratio) conditions. Regardless of which defrost control is selected, defrost does not occur above the user specified outdoor temperature entered in the input field "Maximum Outdoor Dry-bulb Temperature for Defrost Operation" described above. If this input field is left blank, the default defrost control is timed.

#### Field: Defrost Time Period Fraction

This numeric field defines the fraction of compressor runtime when the defrost cycle is active, and only applies to "timed" defrost (see Defrost Control input field above). For example, if the defrost cycle is active for 3.5 minutes for every 60 minutes of compressor runtime, then the user should enter 3.5/60 = 0.058333. The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0.058333.

#### Field: Resistive Defrost Heater Capacity

This numeric field defines the capacity of the resistive defrost heating element in Watts. This input field is used only when the selected defrost strategy is ‘resistive' (see input field "Defrost Strategy" above). The value for this input field must be greater than or equal to 0. If this input field is left blank, the default value is 0.

#### Field Group: Rated specification and performance curves

The performance for each heating speed must be specified as shown below. They should be directly given from the Reference Unit data. All inputs for Speed 1 are required, followed by the optional inputs for other speeds.

#### Field: Speed <x> Reference Unit Gross Rated Heating Capacity

This numeric field defines the total, full load gross heating capacity in watts of the air-to-air heating coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0. The gross heating capacity should not account for the effects of supply air fan heat.

#### Field: Speed <x> Reference Unit Gross Rated Heating COP

This numeric field defines the coefficient of performance (COP=gross heating capacityin watts divided by electrical power input in watts) of the heating coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0. The input power includes  power for the compressor(s), the outdoor coil fan and accessories, but does not include the power consumption of the indoor supply air fan. The gross COP should NOT account for the supply air fan.

#### Field: Speed <x> Reference Unit Rated Air Flow Rate

This numeric field defines the volume air flow rate, in m^3^ per second, across the heating coil at rated conditions for Speed <x> operation. The value entered here should be directly from the Reference Unit data, corresponding to the given gross rated heating capacity and gross rated heating COP at the speed, as above.

#### Field: Speed <x> Heating Capacity Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the total heating capacity as a function of the indoor dry-bulb and source side entering air temperature, from the Reference Unit. The output of this curve is multiplied by the gross rated heating capacity at the speed to give the gross heating capacity at specific temperature operating conditions (i.e., at an indoor air dry-bulb temperature or entering air temperature different from the rating point temperature). It should be noted that the curve is normalized to the heating capacity at Speed<x> from the Reference Unit data, and have the value of 1.0 at the rating point.

#### Field: Speed <x> Heating Capacity Function of Air Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the gross heating capacity as a function of the ratio of actual air flow rate across the heating coil to the design air flow rate (i.e., fraction of full load flow), at Speed <x> from the Reference Unit data. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the design air flow rate at Speed <x>.

#### Field: Speed <x> Energy Input Ratio Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the both the indoor air dry-bulb and entering air temperatures. The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP at Speed <x> from the Reference Unit data) to give the EIR at specific temperature operating conditions (i.e., at an indoor air dry bulb temperature or entering air temperature different from the rating point temperature). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Speed <x> Energy Input Ratio Function of Air Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the heating coil to the design air flow rate (i.e., fraction of full load flow, at Speed <x> from the Reference Unit data). The EIR is the inverse of the COP. This curve is normalized to a value of 1.0 when the actual air flow rate equals the design air flow rate.

An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:DX:VariableSpeed,
        Heat Pump DX Heating Coil 1,     !- Name
        Heating Coil Air Inlet Node,     !- Air Inlet Node Name
        SuppHeating Coil Air Inlet Node,  !- Air Outlet Node Name
        10.0,                   !- Number of Speeds {dimensionless}
        10.0,                   !- Nominal Speed Level {dimensionless}
        35000,                  !- Gross Rated Heating Capacity {W}
        1.7,                     !- Rated Air Flow Rate {m3/s}
        HPACCOOLPLFFPLR,         !- Part Load Fraction Correlation Curve Name
        ,                        !- Defrost Energy Input Ratio Function of Temperature Curve Name
        -5.0,                    !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}
        ,                        !- Outdoor Dry-Bulb Temperature to Turn Back On Compressor{C}
        5.0,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}
        200.0,                   !- Crankcase Heater Capacity {W}
        10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}
        Resistive,               !- Defrost Strategy
        TIMED,                   !- Defrost Control
        0.166667,                !- Defrost Time Period Fraction
        20000,                   !- Resistive Defrost Heater Capacity {W}
        1838.7,                  !- Speed 1 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 1 Gross Rated Heating COP {dimensionless}
        0.1661088,               !- Speed 1 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
        2295.5,                  !- Speed 2 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 2 Gross Rated Heating COP {dimensionless}
        0.179322,                !- Speed 2 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
        2751.3,                  !- Speed 3 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 3 Gross Rated Heating COP {dimensionless}
        0.1925352,               !- Speed 3 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
        3659.6,                  !- Speed 4 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 4 Gross Rated Heating COP {dimensionless}
        0.2189616,               !- Speed 4 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

        4563.7,                  !- Speed 5 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 5 Gross Rated Heating COP {dimensionless}
        0.245388,                !- Speed 5 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Total Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Total Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
        5463.3,                  !- Speed 6 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 6 Gross Rated Heating COP {dimensionless}
        0.2718144,               !- Speed 6 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
        6358.4,                  !- Speed 7 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 7 Gross Rated Heating COP {dimensionless}
        0.2982408,               !- Speed 7 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
        7248.5,                  !- Speed 8 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 8 Gross Rated Heating COP {dimensionless}
        0.3246672,               !- Speed 8 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
        8133.6,                  !- Speed 9 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 9 Gross Rated Heating COP {dimensionless}
        0.3510936,               !- Speed 9 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
        9013.2,                  !- Speed 10 Gross Rated Heating Capacity {w}
        5.0,                     !- Speed 10 Gross Rated Heating COP {dimensionless}
        0.37752,                 !- Speed 10 Rated Air Flow Rate {m3/s}
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF;          !- Energy Input Ratio Function of Flow Fraction Curve Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Heating Coil Electric Power [W]
    HVAC,Average,Heating Coil Heating Rate [W]
    HVAC,Average,Heating Coil Sensible Heating Rate [W]
    HVAC,Average,Heating Coil Source Side Heat Transfer Rate [W]
    HVAC,Average,Heating Coil Part Load Ratio []
    HVAC,Average, Heating Coil Runtime Fraction []
    HVAC,Average, Heating Coil Air Mass Flow Rate [kg/s]
    HVAC,Average,Heating Coil Air Inlet Temperature [C]
    HVAC,Average,Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,Heating Coil Air Outlet Temperature [C]
    HVAC,Average,Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,Heating Coil Upper Speed Level []
    HVAC,Average,Heating Coil Neighboring Speed Levels Ratio []
    HVAC,Average,VSAirtoAirHP Recoverable Waste Heat [W]
    HVAC,Sum,Heating Coil Electric Energy [J]
    HVAC,Sum,Heating Coil Heating Energy [J]
    HVAC,Sum,Heating Coil Source Side Heat Transfer Energy [J]
    HVAC,Average,Heating Coil Defrost Electric Power [W]
    HVAC,Sum,Heating Coil Defrost Electric Energy [J]
    HVAC,Average,Heating Coil Crankcase Heater Electric Power[W]
    HVAC,Sum,Heating Coil Crankcase Heater Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Electric Power [W]

This output variable is the average electric consumption rate of the heat pump in Watts over the timestep being reported.

#### Heating Coil Heating Rate [W]

The output variable is the average total heating capacity provide by the heat pump in Watts over the timestep being reported.

#### Heating Coil Sensible Heating Rate [W]

The output variable is the average sensible heating capacity provide by the heat pump in Watts over the timestep being reported. For heating mode, the sensible capacity is equal to the total capacity.

#### Heating Coil Source Side Heat Transfer Rate [W]

The output variable is the average heat absorbed at the heat pump evaporator in Watts over the timestep being reported.

#### Heating Coil Part Load Ratio  []

This output variable is the ratio of the part-load capacity to the steady state capacity of the heating coil. For the cycling fan mode, the runtime fraction for the heat pump compressor may be different from the compressor part-load ratio reported here due to the part-load performance of the heating coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate.

#### Heating Coil Runtime Fraction []

This output variable is the function of the part load ratio (PLR, part-load capacity/ steady state capacity). The duty factor or part load fraction accounts for efficiency losses due to compressor cycling.

#### Heating Coil Air Mass Flow Rate [kg/s]

The output variable is the average air mass flow rate on the load side going through the heat pump over the timestep being reported.

#### Heating Coil Air Inlet Temperature [C]

The output variable is the average entering air dry-bulb temperature over the timestep being reported.

#### Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average entering air dry humidity ratio over the timestep being reported.

#### Heating Coil Air Outlet Temperature [C]

The output variable is the average leaving air dry-bulb temperature over the timestep being reported.

#### Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average leaving air dry humidity ratio over the timestep being reported.

#### Heating Coil Upper Speed Level []

The output variable is the average upper speed level, for interpolating performances between two neighboring speed levels.

#### Heating Coil Neighboring Speed Levels Ratio []

The output variable is the average speed ratio, for interpolating performances between two neighboring speed levels.

#### Heating Coil Electric Energy [J]

The output variable is the total electric consumption of the heat pump in Joules over the timestep being reported.

#### Heating Coil Heating Energy [J]

The output variable is the total cooling output of the coil in Joules over the timestep being reported.

#### Heating Coil Source Side Heat Transfer Energy [J]

The output variable is the total source side heat transfer of the coil in Joules over the timestep being reported.

#### Heating Coil Defrost Electric Power[W]

The output variable is the average electric power used for defrosting, in Watts over the timestep being reported.

#### Heating Coil Defrost Electric Energy [J]

The output variable is the total electric defrosting energy usage of the coil in Joules over the timestep being reported.

#### Heating Coil Crankcase Heater Electric Power [W]

The output variable is the average power used for crankcase heater, in Watts over the timestep being reported.

#### Heating Coil Crankcase Heater Electric Energy [J]

The output variable is the total electric energy usage of the coil for crankcase heater, in Joules over the timestep being reported.

## Coil:WaterHeating:Desuperheater

A simplified approach is used to determine the performance of this water heating coil. The model assumes that the heating energy provided by this coil is reclaimed from the superheated refrigerant gas leaving a compressor (i.e., a desuperheating refrigerant-to-water heating coil) and does not impact the performance of the compressor. This coil must be used with a water heater tank (e.g., [WaterHeater:Mixed](#waterheatermixed)) which can supply heated potable water and/or hot water for use in a plant loop (e.g., hydronic air reheat coils).

Except for detailed refrigeration system models, the amount of available superheat is simply a percentage of the total heat being rejected by the DX system's condenser. Approximately 25-30% of the energy rejected by typical refrigeration system condensers is to reduce the superheated refrigerant vapor temperature to the condensing temperature. Recovery efficiencies higher than 30% may cause the refrigerant gas to condense which in turn impacts the performance of the refrigeration system. For this reason, the maximum heat reclaim recovery efficiency for this coil is 30% for most sources of waste heat, including refrigeration compressor racks. The one exception to this 30% limit is a condenser that is part of a detailed refrigeration system. In a detailed refrigeration system, the portion of the rejected heat that lies within the superheated region is explicitly calculated. Therefore, the desuperheater coils supplied by a condenser attached to a detailed refrigeration system are subject to a maximum reclaim recovery efficiency of 90% of the heat within the superheated region.

The model includes the ability to modify the heat reclaim recovery efficiency based on variations in inlet water temperature and outdoor air dry-bulb temperature.

This coil model performs the following major functions:

- calculates the amount of heat delivered to the water tank
- calculates the electricity consumption of the integral water pump and on/off-cycle parasitic loads

The input fields for this object are described below in detail:

### Inputs

#### Field: Name

This alpha field contains a unique user-assigned name for an instance of a desuperheater water heating coil. Any reference to this desuperheater coil by another object will use this name.

#### Field: Availability Schedule Name

This alpha field contains the name of the schedule (ref: Schedule) that denotes whether the desuperheater coil is available to operate during a given time period. A schedule value equal to 0 denotes that the desuperheater coil is off for that time period. A value other than 0 denotes that the desuperheater coil is available to operate during that time period. During times when the desuperheater coil is scheduled off, the heater (element or burner) in the water tank object operates based on its tank set point temperature schedule and the desuperheater coil's parasitic electric power is also off for that time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Setpoint Temperature Schedule Name

This alpha field contains the name of the schedule (ref: Schedule) that specifies the set point (or "cut-out") temperature for the desuperheater coil. Temperature values used in this schedule should be in degrees Celsius. The desuperheater coil turns off when the tank water reaches this set point temperature. Once the desuperheater coil is off, the tank water temperature floats downward until it falls below the set point temperature minus the dead band temperature difference defined below (i.e., the "cut-in" temperature). At this point, the desuperheater coil turns on and remains on until the desuperheater coil set point temperature is reached.

#### Field: Dead Band Temperature Difference

This numeric field contains the dead band temperature difference in degrees Celsius. The desuperheater coil "cut-in" temperature is defined as the desuperheater coil set point temperature defined above minus this dead band temperature difference. The desuperheater coil turns on when the water temperature in the tank falls below the "cut-in" temperature. The desuperheater coil remains on until the water temperature in the tank rises above the desuperheater coil set point ("cut-out") temperature defined above. The dead band temperature difference must be greater than 0°C and less than or equal to 20°C. If this field is left blank, the default value is 5°C.

> Desuperheater water heating coils are typically used to offset energy consumption by the water tank's heater (element or burner). Therefore, the cut-in temperature for the desuperheater coil (set point minus dead band temperature difference) is usually higher than the set point temperature for the heater (element or burner) in the associated water heater tank object. At times when the water heater tank set point temperature is greater than the cut-in temperature of the desuperheater coil, the model disables the desuperheater coil and the tank's heater is used to heat the water.

#### Field: Rated Heat Reclaim Recovery Efficiency

This numeric field defines the ratio of recovered waste heat from the superheated refrigerant gas to the total rejected waste heat from the heating source (as if no heat reclaim occurred). Input values must be greater than 0 up to a  maximum value is 0.3 (with a defaults of 0.25) for most sources of waste heat, including refrigeration compressor racks. The one exception to this 0.3 limit is a source that is a condenser that is part of a detailed refrigeration system. In a detailed refrigeration system, the portion of the rejected heat that lies within the superheated region is explicitly calculated. Therefore, the desuperheater coils supplied by a condenser attached to a detailed refrigeration system are subject to a maximum reclaim recovery efficiency of 0.9. with a  default value is 0.8.

#### Field: Rated Inlet Water Temperature

This numeric field defines the coil inlet water temperature, in degrees Celsius, that corresponds to the rated heat reclaim recovery efficiency. Also see field "Heat Reclaim Efficiency Modifier Curve Name (function of temperature)" below.

#### Field: Rated Outdoor Air Temperature

This numeric field defines the outdoor air dry-bulb temperature, in degrees Celsius, that corresponds to the rated heat reclaim recovery efficiency. The outdoor air dry-bulb temperature impacts the desuperheater coil refrigerant temperature and the amount of heat available for reclaim. Also see field "Heat Reclaim Efficiency Modifier Curve Name(function of temperature)" below.

#### Field: Maximum Inlet Water Temperature for Heat Reclaim

This numeric field defines the maximum coil inlet water temperature in degrees Celsius. Any time the inlet water temperature to the desuperheater coil is above this maximum allowed temperature, heat reclaim is restricted so that the tank water does not exceed this temperature.

#### Field: Heat Reclaim Efficiency Function of Temperature Curve Name

This alpha field specifies the name of a bi-quadratic curve object (ref: Performance Curves) that defines the variation in heat reclaim efficiency as a function of inlet fluid (air and water) temperatures. The bi-quadratic curve uses the coil inlet water temperature and outdoor air dry-bulb temperature (entering the DX coil condenser) as the independent variables. The output of this curve is multiplied by the rated heat reclaim recovery efficiency to give the heat reclaim efficiency at specific operating conditions (i.e., at temperatures different from the rating point temperatures). The curve should be normalized to have the value of 1.0 at the rating point temperatures. If this field is left blank, the heat reclaim efficiency remains constant (curve value assumed to be 1.0 for all conditions). The model restricts the product of the output of this curve and the rated heat reclaim recovery efficiency to a maximum of 0.9 for refrigeration condenser sources and 0.3 for all other sources..

#### Field: Water Inlet Node Name

This alpha field defines the name of the water node from which the desuperheater heating coil draws its inlet water. This node name must also match the source side outlet node name for the water heater tank used with this coil (ref. Water Heaters).

#### Field: Water Outlet Node Name

This alpha field defines the name of the water node to which the desuperheater heating coil sends its outlet water. This node name must also match the source side inlet node name for the water heater tank used with this coil (ref. Water Heaters).

#### Field: Tank Object Type

This alpha (choice) field contains the type of water heater tank used by this desuperheater heating coil. Currently, the only valid choice is [WaterHeater:Mixed](#waterheatermixed).

#### Field: Tank Name

This alpha field contains the name of the specific water heater tank ([WaterHeater:Mixed](#waterheatermixed) object) used by this desuperheater heating coil.

#### Field: Heating Source Object Type

This alpha (choice) field defines the source of superheated refrigerant gas from which the desuperheater water heating coil recovers energy through heat reclaim. Valid choices are:

This alpha (choice) field defines the source of superheated refrigerant gas from which the desuperheater water heating coil recovers energy through heat reclaim. Valid choices are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:SingleSpeed
    Coil:Cooling:DX:TwoSpeed
    Coil:Cooling:DX:TwoStageWithHumidityControlMode
    Refrigeration:CompressorRack
    Refrigeration:Condenser:AirCooled
    Refrigeration:Condenser:EvaporativeCooled
    Refrigeration:Condenser:WaterCooled
~~~~~~~~~~~~~~~~~~~~

#### Field: Heating Source Name

This alpha field defines the name of the desuperheater coil's heat source (e.g., the name of a specific coil of the type mentioned in the previous field which provides waste heat to this desuperheater water heating coil).

#### Field: Water Flow Rate

This numeric field defines the desuperheater coil's water flow rate in cubic meters per second. The model assumes that this flow rate is constant (throughout the simulation period) when the desuperheater coil operates, and that it corresponds to the heat reclaim recovery efficiency performance specified by the user. This water flow rate must be greater than zero.

#### Field: Water Pump Power

This numeric field defines the coil's water circulation pump power in Watts. This is the operating pump power as installed. Values must be greater than or equal to 0. If this field is left blank, the default value is 0. A warning message will be issued if the ratio of water pump power to desuperheater water volumetric flow rate exceeds 7.9264E6 W/m^3^/s, but the simulation will continue using the user-defined values. The model assumes that this integral pump (i.e., no need to define a separate pump object) cycles on and off with the desuperheater heating coil.

#### Field: Fraction of Pump Heat to Water

This numeric field defines the fraction of water circulation pump heat that is transferred to the water. The pump is assumed to be downstream of the desuperheater water heating coil, and this field is used to determine the desuperheater water outlet temperature. Values must be greater than or equal to 0 and less than or equal to 1. If this field is left blank, the default value is 0.2.

#### Field: On-Cycle Parasitic Electric Load

This optional numeric field contains the on-cycle parasitic electric power in Watts. This is the parasitic electric power consumed by controls or other electrical devices associated with the desuperheater water heating coil. This parasitic electric load is consumed whenever the desuperheater coil is operating, and the model assumes that this parasitic power does not contribute to heating the water nor does it affect the zone air heat balance. The minimum value for this field is 0.0, and the default value is also 0.0 if this field is left blank.

#### Field: Off-Cycle Parasitic Electric Load

This optional numeric field contains the off-cycle parasitic electric power in Watts. This is the parasitic electric power consumed by controls or other electrical devices associated with the desuperheater water heating coil. This parasitic electric load is consumed whenever the desuperheater coil is available but is not operating, and the model assumes that this parasitic power does not contribute to heating the water nor does it affect the zone air heat balance. The minimum value for this field is 0.0, and the default value is also 0.0 if this field is left blank.

Following is an example input for a desuperheater water heating coil.

~~~~~~~~~~~~~~~~~~~~

    Coil:WaterHeating:Desuperheater,
        WaterHeatingCoil,        !- Coil Name
        DesuperheaterSched,      !- Availability Schedule Name
        DesuperheaterTempSch,    !- Desuperheater Coil Set Point Temperature Schedule Name
        5.0,                     !- Dead Band Temperature Difference {deltaC}
        0.25,                    !- Rated Heat Reclaim Recovery Efficiency
        50.0,                    !- Rated Inlet Water Temperature {C}
        35.0,                    !- Rated Outdoor Air Temperature {C}
        60.0,                    !- Maximum Inlet Water Temperature for Heat Reclaim {C}
        HEffFTemp,               !- Heat Reclaim Efficiency Modifier Curve Name (function of temperature)
        WaterHeatingCoilInletNode,   !- Desuperheater Water Inlet Node Name
        WaterHeatingCoilOutletNode,  !- Desuperheater Water Outlet Node Name
        WaterHeater:Mixed,      !- Water Heater Tank Type
        WaterHeatingCoilTank,    !- Water Heater Tank Name
        Coil:Cooling:DX:SingleSpeed,  !- Heating Source Type
        Furnace ACDXCoil 1,      !- Heating Source Name
        0.0001,                  !- Desuperheater Water Volumetric Flow Rate {m3/s}
        100.0,                   !- Water Pump Power {W}
        0.2,                     !- Fraction of Pump Heat to Water
        10.0,                    !- On-Cycle Parasitic Electric Load {W}
        10.0;                    !- Off-Cycle Parasitic Electric Load {W}
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Water Heater Part Load Ratio
    HVAC,Average,Water Heater Heating Rate [W]
    HVAC,Sum,Water Heater Heating Energy [J]
    HVAC,Average,Water Heater Pump Electric Power [W]
    HVAC,Sum,Water Heater Pump Electric Energy [J]
     HVAC,Average,Water Heater Heat Reclaim Efficiency Modifier Multiplier []
    HVAC,Average,Water Heater On Cycle Parasitic Electric Power [W]
    HVAC,Sum,Water Heater On Cycle Parasitic Electric Energy [J]
    HVAC,Average,Water Heater Off Cycle Parasitic Electric Power [W]
    HVAC,Sum,Water Heater Off Cycle Parasitic Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

Water Heater Part Load Ratio []This output field contains the part load ratio of the desuperheater heating coil for the timestep being reported. This ratio represents the fraction of the timestep that the desuperheater heating coil is operating.

#### Water Heater Heating Rate [W]

This output field contains the average heating rate output of the desuperheater heating coil in Watts for the timestep being reported. This value includes the portion of circulation pump heat attributed to heating the water.

#### Water Heater Heating Energy [J]

This output field contains the total heating output of the desuperheater heating coil in Joules for the timestep being reported. This value includes the portion of circulation pump heat attributed to heating the water.

#### Water Heater Pump Electric Power [W]

This output field contains the average electricity consumption rate for the water circulation pump in Watts for the timestep being reported.

#### Water Heater Pump Electric Energy [J]

This output field contains the electricity consumption of the water circulation pump in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key= DHW, Group Key= Plant (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Water Heater Heat Reclaim Efficiency Modifier Multiplier []

This output field contains the average output of the Heat Reclaim Efficiency Modifier Curve (function of temperature) for the timestep being reported.

#### Water Heater On Cycle Parasitic Electric Power [W]

#### Water Heater On Cycle Parasitic Electric Energy [J]

#### Water Heater Off Cycle Parasitic Electric Power [W]

#### Water Heater Off Cycle Parasitic Electric Energy [J]

These outputs are the parasitic electric power and consumption associated with the desuperheater water heating coil. Specific outputs represent parasitic electrical usage during the coil on and off cycles. These outputs represent electronic controls or other electric component. The model assumes that the parasitic power does not contribute to heating the water nor does it impact the zone air heat balance. The parasitic electric consumption outputs are also added to a meter with Resource Type = Electricity, End Use Key = DHW, Group Key = Plant (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

## CoilSystem:Cooling:DX

The [CoilSystem:Cooling:DX](#coilsystemcoolingdx) object is a "virtual" container component that consists of a DX cooling coil component and its associated controls, as shown in the Figure below. This control object supports several different types of DX cooling coils (see field Cooling Coil Object Type).

This component may be used as a cooling coil in constant volume or variable volume systems, as blow through or draw through, with or without humidity controls. Unlike AirLoopHVAC:Unitary system types, this component controls only the DX coil, not the supply fan.  [CoilSystem:Cooling:DX](#coilsystemcoolingdx) is added to a system by placing it in an air loop branch (see [Branch](#branch) object) or in an [AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist) or in a [ZoneHVAC:OutdoorAirUnit:EquipmentList](#zonehvacoutdoorairunitequipmentlist) . It requires one or more setpoint manager (see SetpointManager:\*) objects to specify temperature and/or humidity setpoints (unless it is used in a [ZoneHVAC:OutdoorAirUnit](#zonehvacoutdoorairunit) which has its own temperature setpoints).  This object is the one that is listed in the [Branch](#branch) or equipment list object rather than the coil itself.  A constant volume or variable volume fan is modeled separately from this cooling system. These are the only fan types allowed for this system type (ref. [Fan:ConstantVolume](#fanconstantvolume) and [Fan:VariableVolume](#fanvariablevolume)).  Cycling fan operation is not available with this model. The [CoilSystem:Cooling:DX](#coilsystemcoolingdx) object can also be placed on dedicated outdoor air system (DOAS) airloop branches or in arloop branches where the air flow to capacity ratio range is between 100 – 300 cfm/ton. 100% DOAS DX cooling coils operate in lower flow to capacity ratio range compared to regular DX cooling coils. The [CoilSystem:Cooling:DX](#coilsystemcoolingdx) is selected to operate in DOAS application or in low flow to capacity ratio range by specifying "YES" to the input field "Use Outdoor Air DX Cooling Coil". If this optional input field is left blank or specified as "NO", then the coil is modeled as regular DX cooling coil. If the [CoilSystem:Cooling:DX](#coilsystemcoolingdx) object is in an [AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist) or in a [ZoneHVAC:OutdoorAirUnit:EquipmentList](#zonehvacoutdoorairunitequipmentlist) then it is treated as 100% DOAS DX cooling coil only if the choice input field "Use Outdoor Air DX Cooling Coil" is set too "YES". All the control options of the regular DX cooling coils are available to DOAS DX coils as well. Heating DX coils in DOAS airloop operate at the same flow to capacity ratio limits as the DOAS DX cooling coils.

![Schematic of CoilSystem:Cooling:DX Object in an Air Loop for a Blow Through Application](media/schematic-of-coilsystem-cooling-dx-object-in.png)


### Inputs

#### Field: Name

This alpha field contains the identifying name for this component.

#### Field: Availability Schedule Name

This alpha field contains the schedule name that contains information on the availability of the DX coil to operate. A schedule value of 0 indicates that the coil is off for that time period. A schedule value greater than 0 indicates that the coil can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: DX Cooling Coil System Inlet Node Name

This alpha field contains the identifying name given to the DX cooling coil inlet node, as specified in the DX cooling coil object.

#### Field: DX Cooling Coil System Outlet Node Name

This alpha field contains the identifying name given to the DX cooling coil outlet node, as specified in the cooling coil object.

#### Field: DX Cooling Coil System Sensor Node Name

This alpha field contains the identifying name given to the DX cooling coil control node, this is the node at which the temperature set point is specified by the set point manager.

#### Field: Cooling Coil Object Type

This alpha field specifies the type of DX cooling coil. The valid choices for this field are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:DX:SingleSpeed
    CoilSystem:Cooling:DX:HeatExchangerAssisted
    Coil:Cooling:DX:TwoSpeed
    Coil:Cooling:DX:TwoStageWithHumidityControlMode
    Coil:Cooling:DX:VariableSpeed
~~~~~~~~~~~~~~~~~~~~

#### Field: Cooling Coil Name

This alpha field contains the identifying name of the DX cooling coil.

As shown in the example below, correct specification of this system requires specification of the DX Cooling Coil object in addition to the [CoilSystem:Cooling:DX](#coilsystemcoolingdx) object.

#### Field: Dehumidification Control Type

This alpha field contains the type of dehumidification control. The following options are valid for this field:

- **None** - meet sensible load only, no active dehumidification control
- **MultiMode** - activate enhanced dehumidification mode as needed and operate to meet the sensible load. If a sensible load exists, the system will operate to meet that sensible load and may not meet the latent load. If no sensible load exists, and Run on Latent Load = Yes, the system will operate to meet the entire latent load. This option is used to model DX equipment with a switchable option such as subcool reheat. It is valid only with Cooling coil type= [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode) or [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted). If the Run on Latent Load option below is set to Yes, this option may require the use of a heating coil and heating coil outlet air temperature set point manager downstream of this cooling coil to maintain the temperature set point.
- **CoolReheat** - cool beyond the dry-bulb setpoint as required to meet the humidity setpoint. It is valid only with Cooling coil type=Coil:Cooling:DX:TwoStageWithHumidityControlMode. This option requires the use of a heating coil and heating coil outlet air temperature set point manager downstream of this cooling coil to maintain the temperature set point.

The default is **None**. For all dehumidification controls, the max humidity setpoint on the control node is used. This must be set using a **ZoneControl:Humidistat** **ZoneControl:Humidistat** and one of:

- **SetpointManager:SingleZone:Humidity:Maximum**
- **SetpointManager:MultiZone:Humidity:Maximum**
- **SetpointManager:MultiZone:MaximumHumidity:Average**

objects and **SetpointManager:OutdoorAirPretreat** (optional) objects. When extra dehumidification is required, the equipment may not be able to meet the humidity setpoint if its full capacity is not adequate.

#### Field: Run on Sensible Load

This alpha field specifies if the unit will operate to meet a sensible load as determined by the inlet node dry-bulb temperature and the dry-bulb temperature setpoint on the control node. There are two valid choices, **Yes** or **No**. If **Yes**, unit will run if there is a sensible load. If **No**, unit will not run if there is only a sensible load. The default is **Yes**.

#### Field: Run on Latent Load

This alpha field specifies if the unit will operate to meet a latent load as determined by the inlet node humidity ratio and the max humidity setpoint on the control node. There are two valid choices, **Yes** or **No**. If **Yes**, unit will run if there is a latent load. If both a sensible and latent load exist, the system will operate to maintain the temperature set point. When only a latent load exists, the system will operate to meet the humidity ratio set point and requires the use of a heating coil and heating coil outlet air temperature set point manager downstream of this cooling coil to maintain the temperature set point. If **No**, unit will not run if there is only a latent load. The default is **No**.

#### Field: Use Outdoor Air DX Cooling Coil

This input field enables the Coil System DX Coil to be used for low air flow to capacity ratio range ( 100 – 300 cfm/ton).  This flow to capacity ratio range is common in 100% dedicated outdoor air system (DOAS) applications. Other airloop or zone HVAC systems may use this input filed if they operate at such a low flow to capacity ratio range. There are two valid choices, **Yes** or **No**. If **Yes**, the DX cooling coil is forced to operate in this flow to capacity ratio range or runs as 100% DOAS DX coil. If **No**, DX coil is used as regular DX coil. This input field is optional.

#### Field: Outdoor Air DX Cooling Coil Leaving Minimum Air Temperature

This input field is the DX cooling coil leaving supply air minimum temperature specified for frost control.  The DX cooling coil leaving air temperature is not allowed to exceed this minimum coil leaving air temperature.  The DX cooling coil frost controller adjusts or limits the desired coil outlet air setpoint temperature when the coil outlet temperature exceeds this minimum temperature limit specified.  This input field is optional and only used along with in the input field above. The minimum and maximum values of this input field are 0.0C and 7.2C, and the default value is 2.0°C.

An example IDF specification:

~~~~~~~~~~~~~~~~~~~~

    CoilSystem:Cooling:DX,
        DX Cooling Coil System 1,!- Name
        CoolingCoilAvailSched,   !- Availability Schedule Name
        Cooling Coil Air Inlet Node,  !- DX Cooling Coil System Inlet Node Name
        Air Loop Outlet Node,    !- DX Cooling Coil System Outlet Node Name
        Air Loop Outlet Node,    !- DX Cooling Coil System Sensor Node Name
        Coil:Cooling:DX:SingleSpeed,  !- Cooling Coil Object Type
        ACDXCoil 1,              !- Cooling Coil Name
        None,                    !- Dehumidification Control Type
        Yes,                     !- Run on Sensible Load
        No,                      !- Run on Latent Load
        Yes,                     !- Use DOAS DX Cooling Coil
        2.0;                     !- DOAS DX Cooling Coil Leaving Minimum Air Temperature

    Coil:Cooling:DX:SingleSpeed,
        ACDXCoil 1,              !- Name
        CoolingCoilAvailSched,   !- Availability Schedule Name
        25000,                   !- Gross Rated Total Cooling Capacity {W}
        0.75,                    !- Gross Rated Sensible Heat Ratio
        3.0,                     !- Gross Rated Cooling COP
        1.3,                     !- Rated Air Flow Rate {m3/s}
        Cooling Coil Air Inlet Node,  !- Air Inlet Node Name
        Air Loop Outlet Node,    !- Air Outlet Node Name
        WindACCoolCapFT,         !- Total Cooling Capacity Function of Temperature Curve Name
        WindACCoolCapFFF,        !- Total Cooling Capacity Function of Flow Fraction Curve Name
        WindACEIRFT,             !- Energy Input Ratio Function of Temperature Curve Name
        WindACEIRFFF,            !- Energy Input Ratio Function of Flow Fraction Curve Name
        WindACPLFFPLR;           !- Part Load Fraction Correlation Curve Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    All DX Coils:
    HVAC,Average,Coil System Frost Control Status

    Coil Type= Coil:Cooling:DX:TwoStageWithHumidityControlMode
    HVAC,Average,Coil System Cycling Ratio
    HVAC,Average,Coil System Compressor Speed Ratio

    Coil types=Coil:Cooling:DX:SingleSpeed & CoilSystem:Cooling:DX:HeatExchangerAssisted
    HVAC,Average,Coil System Part Load Ratio
~~~~~~~~~~~~~~~~~~~~

#### Coil System Cycling Ratio

The system may operate for the entire system timestep, but to meet the load the compressor can cycle on and off. This reports the fraction of the system timestep that the compressor is operating. (1.0 is continuous, 0.0 is off).

#### Coil System Compressor Speed Ratio

This is the ratio of time in a system timestep that the compressor is at rated speed. The compressor speed ratio reports (1.0 is max, 0.0 is min) and any value in between as it is averaged over the timestep.

#### Coil System Part Load Ratio

The DX system can operate with a cycling compressor or a varying speed compressor, This variable reports the fraction of the Full Load that is met during the system timestep. This can differ from the cycling part load ratio or the compressor speed ratio. (1.0 is Full Load and 0.0 is no load)

#### Coil System Frost Control Status

This is a flag indicating whether frost control is active at current time step or not.  Frost control is activated or enforced when the sensible load control requires DX cooling coil outlet air temperature below the user specified minimum temperature or when the dehumidification load control requires DX cooling coil outlet air humidity ratio below the saturation humidity ratio corresponding to the user specified minimum temperature for frost control. Frost control status of zero means no active frost control, a value of 1 or 2 indicates that frost control is active.  If frost control status is 1, then the frost control was enforced when the cooling coil is run to meet sensible load.  If the frost control status value is 2, then the control was enforced when the cooling coil is run to meet latent load. When frost control is active the DX cooling coil setpoint value is modified based on user specified limit.

## CoilSystem:Heating:DX

The [CoilSystem:Heating:DX](#coilsystemheatingdx) object is a "virtual" container component for a DX heating coil that provides the controls needed to operate the coil.  Only single speed DX air-to-air heating coils are supported.

This component may be used as a heating coil in constant volume or variable volume air handlers.  It can also be used in an outside air system (by including it in an [AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist) object) or in a zone outdoor air unit (by including it in an [ZoneHVAC:OutdoorAirUnit:EquipmentList](#zonehvacoutdoorairunitequipmentlist) object).  This object is the one that is listed in the [Branch](#branch) or equipment list object rather than the coil itself.

The inlet and outlet nodes for the DX heat pump system are defined in the heating coil object.  The control node is always the outlet node of the coil.  This DX heat pump heating system requires that a (drybulb) temperature setpoint be placed on the outlet node using either a setpoint manager or the energy management system.  The coil is controlled to attempt to meet that setpoint using a part load ratio modeling approach.

This model only supports continuous fan and cycling compressor operation -- cycling fan modeling is not available with this model.

### Inputs

#### Field: Name

This alpha field contains the identifying name for this component.

#### Field: Availability Schedule Name

This alpha field contains the schedule name that contains information on the availability of the DX coil to operate. A schedule value of 0 indicates that the coil is off for that time period. A schedule value greater than 0 indicates that the coil can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Heating Coil Object Type

This alpha field specifies the type of DX heating coil.  This model currently supports only single speed DX heat pump heating coils and the only options for this field are [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed) and [Coil:Heating:DX:VariableSpeed](#coilheatingdxvariablespeed).

#### Field: Heating Coil Name

This alpha field specifies the unique name of the DX heating coil.  This field references the name of a [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed) that needs to be defined elsewhere in the input file.

An example of a DX heating coil system follows.

~~~~~~~~~~~~~~~~~~~~

    CoilSystem:Heating:DX,
        HeatPump DX Coil 1, !- Name
        FanAndCoilAvailSched , !- Availability Schedule Name
        Coil:Heating:DX:SingleSpeed,  !- Heating Coil Object Type
        Heat Pump DX Heating Coil 1;  !- Heating Coil Name

      Coil:Heating:DX:SingleSpeed,
        Heat Pump DX Heating Coil 1,  !- Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        autosize,                !- Gross Rated Heating Capacity {W}
        2.75,                    !- Gross Rated Heating COP {W/W}
        autosize,                !- Rated Air Flow Rate {m3/s}
        Heating Coil Air Inlet Node,  !- Air Inlet Node Name
        SuppHeating Coil Air Inlet Node,  !- Air Outlet Node Name
        HPACHeatCapFT,           !- Heating Capacity Function of Temperature Curve Name
        HPACHeatCapFFF,          !- Heating Capacity Function of Flow Fraction Curve Name
        HPACHeatEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name
        HPACHeatEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name
        HPACCOOLPLFFPLR,         !- Part Load Fraction Correlation Curve Name
        ,                        !- Defrost Energy Input Ratio Function of Temperature Curve Name
        -8.0,                    !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}
        5.0,                     !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}
        200.0,                   !- Crankcase Heater Capacity {W}
        10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}
        Resistive,               !- Defrost Strategy
        TIMED,                   !- Defrost Control
        0.166667,                !- Defrost Time Period Fraction
        autosize,                !- Resistive Defrost Heater Capacity {W}
        Heat Pump 1 Evaporator Node;
~~~~~~~~~~~~~~~~~~~~

### Outputs

#### Coil System Part Load Ratio

The DX system can operate with a cycling compressor or a varying speed compressor, This variable reports the fraction of the Full Load that is met during the system timestep. This can differ from the cycling part load ratio or the compressor speed ratio. (1.0 is Full Load and 0.0 is no load)

## CoilSystem:Cooling:DX:HeatExchangerAssisted

The heat exchanger-assisted DX cooling coil is a "virtual" component consisting of a direct expansion (DX) cooling coil and an air-to-air heat exchanger as shown in Figure 139 below. The air-to-air heat exchanger pre-conditions the air entering the cooling coil, and reuses this energy to post-condition the supply air leaving the cooling coil. This heat exchange process improves the latent removal performance of the cooling coil by allowing it to dedicate more of its cooling capacity toward dehumidification (lower sensible heat ratio).

This compound object models the basic operation of an air-to-air heat exchanger in conjunction with a cooling coil. The heat exchanger-assisted DX cooling coil does not have an operating schedule of its own; its operating schedule is governed by the availability schedules for the DX cooling coil and the air-to-air heat exchanger. This compound object is used in place of where a DX cooling coil object would normally be used by itself.

To model a heat exchanger-assisted DX cooling coil, the input data file should include the following objects:

- [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted) object
- Air-to-air heat exchanger object ([HeatExchanger:AirToAir:FlatPlate](#heatexchangerairtoairflatplate), [HeatExchanger:AirToAir:SensibleAndLatent](#heatexchangerairtoairsensibleandlatent), or [HeatExchanger:Desiccant:BalancedFlow](#heatexchangerdesiccantbalancedflow))
- DX cooling coil object ([Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed))

In terms of controlling the operation of the heat exchanger, the heat exchanger is assumed to always provide its heat transfer when the associated DX cooling coil is operating and no high humidity control mechanism is specified. However, the heat exchanger's energy transfer may be controlled (i.e., turned on and off) based on a zone air humidity level using either a humidistat (ref. [AirLoopHVAC:Unitary:Furnace:HeatCool](#airloophvacunitaryfurnaceheatcool) or [AirLoopHVAC:UnitaryHeatCool](#airloophvacunitaryheatcool)) or a humidistat and a maximum humidity set point manager to place a humidity ratio set point on the appropriate control node (ref. [CoilSystem:Cooling:DX](#coilsystemcoolingdx)). This model may also be used with the unitary changeover bypass system and the unitary air-to-air heat pump system (ref. [AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass](#airloophvacunitaryheatcoolvavchangeoverbypass) and [AirLoopHVAC:UnitaryHeatPump:AirToAir](#airloophvacunitaryheatpumpairtoair)); however, the heat exchanger is assumed to always provide its heat transfer when the cooling coil operates and cannot be turned on and off based on a zone air humidity set point. Two zone air conditioners may also use this model for improved dehumidification. The first type is the packaged terminal heat pump (ref. [ZoneHVAC:PackagedTerminalHeatPump](#zonehvacpackagedterminalheatpump)) where the heat exchanger's heat transfer is always active whenever the cooling coil operates. The second type is the window air conditioner (ref. [ZoneHVAC:WindowAirConditioner](#zonehvacwindowairconditioner)) where the heat exchanger's heat transfer is always active when the cooling coil operates and no high humidity control mechanism is specified, OR the heat exchanger's heat transfer may be controlled based on zone air humidity level if a humidistat and high humidity set point manager are specified (maximum humidity ratio set point placed on the heat exchanger's exhaust air outlet node, ref. Figure 139).

Links to the cooling coil and air-to-air heat exchanger specifications are provided in the input data syntax for this compound object. A description of each input field for this compound object is provided below.

![Schematic of the CoilSystem:Cooling:DX:HeatExchangerAssisted compound object](media/schematic-of-the-coilsystem-cooling-dx.jpeg)


> NOTE: Node naming shown in Figure 139 is representative for [HeatExchanger:AirToAir:SensibleAndLatent](#heatexchangerairtoairsensibleandlatent). For [HeatExchanger:AirToAir:FlatPlate](#heatexchangerairtoairflatplate), the exhaust air nodes are referred to as ‘secondary air' nodes. For [HeatExchanger:Desiccant:BalancedFlow](#heatexchangerdesiccantbalancedflow), the supply air nodes are referred to as ‘regeneration air' nodes and the exhaust air nodes as ‘process air' nodes.

### Inputs

#### Field: Name

A unique user-assigned name for the heat exchanger-assisted DX cooling coil. Any reference to this compound component by another object will use this name.

#### Field: Heat Exchanger Object Type

This alpha field denotes the type of heat exchanger being modeled. Valid choices are:

~~~~~~~~~~~~~~~~~~~~

    HeatExchanger:AirToAir:FlatPlate
    HeatExchanger:AirToAir:SensibleAndLatent
    HeatExchanger:Desiccant:BalancedFlow
~~~~~~~~~~~~~~~~~~~~

#### Field: Heat Exchanger Name

This alpha field denotes the name of the air-to-air heat exchanger being modeled.

#### Field: Cooling Coil Object Type

This alpha field denotes the type of DX cooling coil being modeled. The only valid choice is [Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed).

#### Field: Cooling Coil Name

This alpha field denotes the name of the DX cooling coil being modeled.

Following is an example input for this compound object:

~~~~~~~~~~~~~~~~~~~~

    CoilSystem:Cooling:DX:HeatExchangerAssisted,
        HeatExchangerAssistedCoolingCoil,        ! Name of the heat exchanger assisted cooling coil
        HeatExchanger:AirToAir:SensibleAndLatent,       ! Heat exchanger type
        Air to Air Heat Exchanger 1,             ! Heat exchanger name
        Coil:Cooling:DX:SingleSpeed,    ! Cooling coil type
        DX Cooling Coil 1;                       ! Cooling coil name

      HeatExchanger:AirToAir:SensibleAndLatent,
        Air to Air Heat Exchanger 1,       !- Heat exchanger name
        FanAndCoilAvailSched,              !- Availability schedule name
        1.3,                               !- Nominal supply air flow rate {m3/s}
        .2,                                !- Sensible effectiveness at 100% airflow heating condition
        .0,                                !- Latent effectiveness at 100% airflow heating condition
        .23,                               !- Sensible effectiveness at 75% airflow heating condition
        .0,                                !- Latent effectiveness at 75% airflow heating condition
        .2,                                !- Sensible effectiveness at 100% airflow cooling condition
        .0,                                !- Latent effectiveness at 100% airflow cooling condition
        .23,                               !- Sensible effectiveness at 75% airflow cooling condition
        .0,                                !- Latent effectiveness at 75% airflow cooling condition
        HeatExchangerSupplyAirInletNode,   !- Supply air inlet node name
        DX Cooling Coil Air Inlet Node,    !- Supply air outlet node name
        HeatExchangerExhaustAirInletNode,  !- Exhaust air inlet node name
        HeatExchangerExhaustAirOutletNode, !- Exhaust air outlet node name
        50.0,                              !- Nominal electric power {W}
        No,                                !- Supply air outlet temperature control
        Rotary,                            !- Heat exchanger type
        None;                              !- Frost control type
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

      Coil:Cooling:DX:SingleSpeed,
        DX Cooling Coil 1,                !- Coil Name
        FanAndCoilAvailSched,             !- Availability Schedule
        25000,                            !- Gross Rated Total Cooling Capacity {W}
        0.75,                             !- Gross Rated Sensible Heat Ratio
        3.0,                              !- Gross Rated Cooling COP
        1.3,                              !- Rated Air Flow Rate {m3/s}
        DX Cooling Coil Air Inlet Node,   !- Coil Air Inlet Node
        HeatExchangerExhaustAirInletNode, !- Coil Air Outlet Node
        ACCoolCapFT,          !- Total Cooling Capacity Modifier Curve (function of temperature)
        ACCoolCapFFF,         !- Total Cooling Capacity Modifier Curve (function of flow fraction)
        ACEIRFT,              !- Energy Input Ratio Modifier Curve (function of temperature)
        ACEIRFFF,             !- Energy Input Ratio Modifier Curve (function of flow fraction)
        ACPLFFPLR;            !- Part Load Fraction Correlation (function of part load ratio)
~~~~~~~~~~~~~~~~~~~~

### Outputs

No variables are reported for this compound object. However, outputs are provided by the cooling coil and heat exchanger that are specified.

## CoilSystem:Cooling:Water:HeatExchangerAssisted

The heat exchanger-assisted water cooling coil is a "virtual" component consisting of a chilled-water cooling coil and an air-to-air heat exchanger as shown in Figure 140 below. The air-to-air heat exchanger precools the air entering the cooling coil, and reuses this energy to reheat the supply air leaving the cooling coil. This heat exchange process improves the latent removal performance of the cooling coil by allowing it to dedicate more of its cooling capacity toward dehumidification (lower sensible heat ratio).

![Schematic of the CoilSystem:Cooling:Water:HeatExchangerAssisted compound object](media/schematic-of-the-coilsystem-cooling-dx.jpeg)


> Note: Node naming shown in Figure 140 is representative for [HeatExchanger:AirToAir:SensibleAndLatent](#heatexchangerairtoairsensibleandlatent). For [HeatExchanger:AirToAir:FlatPlate](#heatexchangerairtoairflatplate), the exhaust air nodes are referred to as ‘secondary air' nodes.

This compound object models the basic operation of an air-to-air heat exchanger in conjunction with a chilled-water cooling coil. The heat exchanger-assisted water cooling coil does not have an operating schedule of its own; its operating schedule is governed by the availability schedules for the chilled-water cooling coil and the air-to-air heat exchanger. Heat exchange will occur whenever the heat exchanger is available to operate (via its availability schedule) and a temperature difference exists between the two air streams -- there is currently no method to enable or disable heat exchange based on zone air humidity level. This compound object is used in place of where a chilled-water cooling coil object would normally be used by itself.

To model a heat exchanger-assisted water cooling coil, the input data file should include the following objects:

- [CoilSystem:Cooling:Water:HeatExchangerAssisted](#coilsystemcoolingwaterheatexchangerassisted) compound object
- Air-to-air heat exchanger object ([HeatExchanger:AirToAir:FlatPlate](#heatexchangerairtoairflatplate) or [HeatExchanger:AirToAir:SensibleAndLatent](#heatexchangerairtoairsensibleandlatent))
- Chilled-water cooling coil object ([Coil:Cooling:Water](#coilcoolingwater) or [Coil:Cooling:Water:DetailedGeometry](#coilcoolingwaterdetailedgeometry))

Links to the cooling coil and air-to-air heat exchanger specifications are provided in the input data syntax for this compound object. A description of each input field for this compound object is provided below.

### Inputs

#### Field: Name

A unique user-assigned name for the heat exchanger-assisted water cooling coil. Any reference to this compound component by another object (e.g., [ZoneHVAC:UnitVentilator](#zonehvacunitventilator), [ZoneHVAC:FourPipeFanCoil](#zonehvacfourpipefancoil), component in an air loop [Branch](#branch) object) will use this name.

#### Field: Heat Exchanger Object Type

This alpha field denotes the type of heat exchanger being modeled. Valid choices are:

~~~~~~~~~~~~~~~~~~~~

    HeatExchanger:AirToAir:FlatPlate
    HeatExchanger:AirToAir:SensibleAndLatent
~~~~~~~~~~~~~~~~~~~~

#### Field: Heat Exchanger Name

This alpha field denotes the name of the air-to-air heat exchanger being modeled.

#### Field: Cooling Coil Object Type

This alpha field denotes the type of chilled-water cooling coil being modeled. Valid choices are:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:Water
    Coil:Cooling:Water:DetailedGeometry
~~~~~~~~~~~~~~~~~~~~

#### Field: Cooling Coil Name

This alpha field denotes the name of the chilled-water cooling coil being modeled.

Following is an example input for this compound object:

~~~~~~~~~~~~~~~~~~~~

    CoilSystem:Cooling:Water:HeatExchangerAssisted,
        Heat Exchanger Assisted Cooling Coil 1, !- Name of the heat exchanger assisted cooling coil
        HeatExchanger:AirToAir:FlatPlate,   !- Heat exchanger type
        Heat Exchanger Assisting Cooling Coil,  !- Heat exchanger name
        Coil:Cooling:Water:DetailedGeometry,         !- Cooling coil type
        Main Cooling Coil 1;                    !- Cooling coil name

    HeatExchanger:AirToAir:FlatPlate,
        Heat Exchanger Assisting Cooling Coil,  !- Name
        CoolingCoilAvailSched,   !- Availability Schedule Name
        Counter Flow,            !- flow arrangement
        Yes,                     !- Economizer lockout
        1.0,                     !- hA ratio
        1.32,                    !- Nominal supply air flow rate {m3/s}
        24.0,                    !- Nominal supply air inlet temperature {C}
        21.0,                    !- Nominal supply air outlet temperature {C}
        1.32,                    !- Nominal secondary air flow rate {m3/s}
        12.0,                    !- Nominal secondary air inlet temperature {C}
        100.0,                   !- Nominal electric power {W}
        Mixed Air Node 1,        !- Supply air inlet node
        Main Cooling Coil 1 Inlet Node,  !- Supply air outlet node
        Main Cooling Coil 1 Outlet Node, !- Secondary air inlet node
        Main Heating Coil 1 Inlet Node;  !- Secondary air outlet node

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:Water:DetailedGeometry,
        Main Cooling Coil 1,     !- Coil Name
        CoolingCoilAvailSched,   !- Availability Schedule Name
        autosize,                !- Max Water Flow Rate of Coil {m3/s}
        autosize,                !- Tube Outside Surf Area {m2}
        autosize,                !- Total Tube Inside Area {m2}
        autosize,                !- Fin Surface Area {m2}
        autosize,                !- Minimum Air Flow Area {m2}
        autosize,                !- Coil Depth {m}
        autosize,                !- Fin Diameter {m}
        ,                        !- Fin Thickness {m}
        ,                        !- Tube Inside Diameter {m}
        ,                        !- Tube Outside Diameter {m}
        ,                        !- Tube Thermal Conductivity {W/m-K}
        ,                        !- Fin Thermal Conductivity {W/m-K}
        ,                        !- Fin Spacing {m}
        ,                        !- Tube Depth Spacing {m}
        ,                        !- Number of Tube Rows
        autosize,                !- Number of Tubes per Row
        Main Cooling Coil 1 Water Inlet Node,  !- Coil_Water_Inlet_Node
        Main Cooling Coil 1 Water Outlet Node, !- Coil_Water_Outlet_Node
        Main Cooling Coil 1 Inlet Node,   !- Coil_Air_Inlet_Node
        Main Cooling Coil 1 Outlet Node;  !- Coil_Air_Outlet_Node
~~~~~~~~~~~~~~~~~~~~

### Outputs

No variables are reported for this compound object. However, outputs are provided by the cooling coil and heat exchanger that are specified.

## Coil:WaterHeating:AirToWaterHeatPump

EnergyPlus can model a heat pump water heater (HPWH) consisting of a water heater tank (e.g., [WaterHeater:Mixed](#waterheatermixed)), a direct expansion (DX) "coil" (i.e., an air-to-water DX compression system which includes a water heating coil, air coil, compressor, and water pump), and a fan to provide air flow across the air coil associated with the DX compression system. These objects work together to model a system which heats water using zone air, outdoor air, or a combination of zone and outdoor air as the primary heat source.

The [WaterHeater:HeatPump](#waterheaterheatpump) compound object, water heater tank object (e.g., [WaterHeater:Mixed](#waterheatermixed)), and fan object (e.g., [Fan:OnOff](#fanonoff)) are defined elsewhere in this reference document. [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump) object described here models an air-to-water DX compression system to determine its air-side and water-side performance. This DX coil object calculates the air-side sensible and latent cooling capacity at the specific operating conditions for each simulation timestep, as well as the condenser's water-side temperature difference at a given condenser water flow rate.

The heat pump water heater DX coil model performs the following major functions:

calculates the electric consumption of the DX compressor and water pump

calculates the amount of heat delivered to the water tank

calculates the electric consumption of the compressor's crankcase heater

calculates the air-side performance of the DX coil

The input fields for this object are described below in detail:

### Inputs

#### Field: Name

This alpha field defines a unique user-assigned name for an instance of a heat pump water heater DX coil. Any reference to this coil by another object (e.g., [WaterHeater:HeatPump](#waterheaterheatpump)) will use this name.

#### Field: Rated Heating Capacity

This numeric field defines the DX coil heating capacity in Watts at the rated evaporator inlet air temperatures, rated condenser inlet water temperature, rated evaporator air flow rate, and rated condenser water flow rate specified below. Values must be greater than 0. This value represents water heating capacity, and it may or may not include the impact of condenser pump heat (see field Condenser Pump Heat Included in Rated Heating Capacity below).

#### Field: Rated COP

This numeric field defines the DX coil's water heating coefficient of performance (COP=water heating capacity in watts divided by electrical power input in watts) at rated conditions (rated inlet temperatures and flow rates specified below). This input not only determines the electric energy use of the heat pump DX coil, but also the amount of total air cooling provided by the evaporator. The rated COP includes compressor power, and may or may not include condenser pump power or evaporator fan power (see field Evaporator Fan Power Included in Rated COP and field Condenser Pump Power Included in Rated COP). Values must be greater than 0. If this field is left blank, the default value is 3.2.

#### Field: Rated Sensible Heat Ratio

This numeric field defines the air-side sensible heat ratio (SHR=sensible cooling capacity divided by total cooling capacity) of the DX coil at rated conditions (rated inlet temperatures and flow rates specified below). This value should not include the effect of evaporator fan heat. Values must be greater than or equal to 0.5, and less than or equal to 1.0. The default value is 0.85.

#### Field: Rated Evaporator Inlet Air Dry-Bulb Temperature 

This numeric field defines the evaporator inlet air dry-bulb temperature, in degrees Celsius, that corresponds to rated coil performance (heating capacity, COP and SHR). Values must be greater than 5°C. If this field is left blank, the default value is 19.7°C.

#### Field: Rated Evaporator Inlet Air Wet-Bulb Temperature 

This numeric field defines the evaporator inlet air wet-bulb temperature, in degrees Celsius, that corresponds to rated coil performance (heating capacity, COP and SHR). Values must be greater than 5°C. If this field is left blank, the default value is 13.5°C.

#### Field: Rated Condenser Inlet Water Temperature 

This numeric field defines the condenser inlet water temperature, in degrees Celsius, that corresponds to rated coil performance (heating capacity, COP and SHR). Values must be greater than 25°C. If this field is left blank, the default value is 57.5°C.

#### Field: Rated Evaporator Air Flow Rate

This numeric field defines the evaporator air volume flow rate in cubic meters per second at rated conditions. Values must be greater than 0. If this field is left blank or autocalculated  (field value = **autocalculate**), the default value is 5.035E-5 m^3^/s/W (31.25 cfm/MBH) multiplied by the Rated Heating Capacity specified above. When autocalculating the rated evaporator air volumetric flow rate, a zone sizing object is not required.

#### Field: Rated Condenser Water Flow Rate

This numeric field defines the condenser water volumetric flow rate in cubic meters per second at rated conditions. Values must be greater than 0. If this field is left blank or autocalculated (field value = **autocalculate**), the default value is 4.487E-8 m^3^/s/W (0.208 gpm/MBH) multiplied by the Rated Heating Capacity specified above. When autocalculating the rated condenser water volumetric flow rate, a zone sizing object is not required. A warning message will be issued if the ratio of Rated Condenser Water Volumetric Flow Rate to Rated Heating Capacity is less than 1.79405E-8 m^3^/s/W (0.083 gpm/MBH) or greater than 8.97024E-8 m^3^/s/W (0.417 gpm/MBH), but the simulation will continue.

#### Field: Evaporator Fan Power Included in Rated COP

This choice field specifies if evaporator fan power is included in the rated COP defined above. This input impacts the calculation of compressor electric power and total air cooling provided by the evaporator for each simulation timestep. If Yes is selected, the evaporator fan power is subtracted from the total electric heating power when calculating total evaporator cooling capacity. If No is selected, it is assumed that the total heating power does not include evaporator fan power. If this field is left blank, the default is Yes. See the Engineering Reference section for [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump) for further details.

#### Field: Condenser Pump Power Included in Rated COP

This choice field specifies if condenser pump power is included in the rated COP defined above. This input impacts the calculation of compressor electric power which then impacts the total air cooling provided by the evaporator for each simulation timestep. If Yes is selected, the condenser pump power is subtracted from the total electric heating power when calculating total evaporator cooling capacity. If No is selected, it is assumed that the total heating power does not include the condenser pump. If this field is left blank, the default is No. See Engineering Reference section for [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump) for further details.

#### Field: Condenser Pump Heat Included in Rated Heating Capacity and Rated COP

This choice field specifies if condenser pump heat is included in the rated heating capacity and rated COP defined above. This input impacts the calculation of compressor electric power and total air cooling provided by the evaporator for each simulation timestep. If Yes is selected, the condenser pump heat is already included in the rated heating capacity and rated COP. If No is selected, it is assumed that the rated heating capacity and rated COP do not include the condenser pump heat, and pump heat is added to the total water heating capacity based on the Condenser Water Pump Power and Fraction of Condenser Pump Heat to Water fields below. If this field is left blank, the default is No. See Engineering Reference section for [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump) for further details.

#### Field: Condenser Water Pump Power

This numeric field defines the DX coil's condenser pump power in Watts. This is the operating pump power as installed. Values must be greater than or equal to 0. If this field is left blank, the default value is 0. A warning message will be issued if the ratio of Condenser Water Pump Power to Rated Heating Capacity exceeds 0.1422 W/W (41.67 Watts/MBH), but the simulation will continue.

#### Field: Fraction of Condenser Pump Heat to Water

This numeric field defines the fraction of condenser pump heat that is transferred to the condenser water. The pump is assumed to be downstream of the condenser water coil, and this field is used to determine the water temperature at the condenser outlet node when the field Condenser Pump Power Included in Rated Heating Capacity is set to No. Values must be greater than or equal to 0 and less than or equal to 1. If this field is left blank, the default value is 0.2.

#### Field: Evaporator Air Inlet Node Name

This alpha field defines the name of the air node from which the evaporator coil draws its inlet air.

#### Field: Evaporator Air Outlet Node Name

This alpha field defines the name of the air node to which the evaporator coil sends its outlet air.

#### Field: Condenser Water Inlet Node Name

This alpha field defines the name of the node from which the DX coil condenser draws its inlet water. This node name must also match the source side outlet node name for the water heater tank connected to this DX coil (ref: Water Heaters).

#### Field: Condenser Water Outlet Node Name

This alpha field defines the name of the node to which the heat pump condenser sends it outlet water. This node name must also match the source side inlet node name for the water heater tank connected to this DX coil (ref: Water Heaters).

#### Field: Crankcase Heater Capacity

This numeric field defines the compressor's crankcase heater capacity in Watts. The crankcase heater only operates when the compressor is off and the air surrounding the compressor is below the Maximum Ambient Temperature for Crankcase Heater Operation specified below.

#### Field: Maximum Ambient Temperature for Crankcase Heater Operation

This numeric field defines the maximum ambient temperature for crankcase heater operation in degree Celsius. The crankcase heater only operates when the air surrounding the compressor is below this maximum temperature value and the compressor is off  The ambient temperature surrounding the compressor is set by the Heat Pump:Water Heater parent object (field Compressor Location).

#### Field: Evaporator Air Temperature Type for Curve Objects

This choice field specifies the air temperature type used for the heating capacity and COP modifier curve objects below. The valid selections are Dry-bulb Temperature and Wet-bulb Temperature. If dry-bulb temperature is selected, the inlet air dry-bulb temperature entering the heat pump DX coil and fan section is used to evaluate the curve objects. If wet-bulb temperature is selected, the inlet air wet-bulb temperature entering the heat pump DX coil and fan section is used to evaluate the curve objects. If this field is left blank and the following curve names are defined, the default value is wet-bulb temperature. If the following curve names are not defined, this field is not used.

#### Field: Heating Capacity Function of Temperature Curve Name

This alpha field specifies the name of a **biquadratic** or **cubic** performance curve object (ref: Performance Curves) that defines the variation in DX coil heating capacity as a function of inlet fluid (air and water) temperatures. The biquadratic curve uses evaporator inlet air temperature (dry-bulb or wet-bulb temperature based on the field Evaporator Air Temperature Type for Curve Objects defined above) and condenser inlet water temperature as the independent variables. The cubic curve uses evaporator inlet air (dry-bulb or wet-bulb) temperature as the independent variable. The output of this curve is multiplied by the rated heating capacity to give the heating capacity at specific operating conditions (i.e., at temperatures different from the rating point temperatures). The curve should be normalized to have the value of 1.0 at the rating point temperatures. If this field is left blank, the heating capacity remains constant (curve value assumed to be 1.0 for all conditions).

#### Field: Heating Capacity Function of Air Flow Fraction Curve Name

This alpha field specifies the name of a **quadratic** or **cubic** performance curve object (ref: Performance Curves) that defines the variation in DX coil heating capacity as a function of the ratio of actual air flow rate across the evaporator coil to the rated evaporator air flow rate. The output of this curve is multiplied by the rated heating capacity and the heating capacity modifier curve (function of temperature) to give the DX coil heating capacity at the specific inlet fluid temperatures and air flow rate at which the coil is operating. The curve should be normalized to have the value of 1.0 at the rated evaporator air flow rate (air flow fraction of 1.0). If this field is left blank, the heating capacity remains constant (curve value assumed to be 1.0 for all air flow rates).

#### Field: Heating Capacity Function of Water Flow Fraction Curve Name

This alpha field specifies the name of a **quadratic** or **cubic** performance curve object (ref: Performance Curves) that defines the variation in DX coil heating capacity as a function of the ratio of actual water flow rate through the condenser to the rated condenser water flow rate. The output of this curve is multiplied by the rated heating capacity and the output from the two other heating capacity modifier curves (function of temperature and function of air flow fraction) to give the DX coil heating capacity at the specific inlet fluid temperatures and flow rates at which the coil is operating. The curve should be normalized to have the value of 1.0 at the rated condenser water flow rate (water flow fraction of 1.0). If this field is left blank, the heating capacity remains constant (curve value assumed to be 1.0 for all water flow rates).

#### Field: Heating COP Function of Temperature Curve Name

This alpha field specifies the name of a **biquadratic** or **cubic** performance curve object (ref: Performance Curves) that defines the variation in DX coil heating COP as a function of inlet fluid (air and water) temperatures. The biquadratic curve uses evaporator inlet air temperature (dry-bulb or wet-bulb temperature based on the field Evaporator Air Temperature Type for Curve Objects defined above) and condenser inlet water temperature as the independent variables. The cubic curve uses evaporator inlet air (dry-bulb or wet-bulb) temperature as the independent variable. The output of this curve is multiplied by the rated COP to give the heating COP at specific operating conditions (i.e., at temperatures different from the rating point temperatures). The curve should be normalized to have the value of 1.0 at the rating point temperatures. If this field is left blank, the COP remains constant (curve value assumed to be 1.0 for all conditions).

#### Field: Heating COP Function of Air Flow Fraction Curve Name

This alpha field specifies the name of a **quadratic** or **cubic** performance curve object (ref: Performance Curves) that defines the variation in DX coil heating COP as a function of the ratio of actual air flow rate across the evaporator coil to the rated evaporator air flow rate. The output of this curve is multiplied by the rated COP and the heating COP modifier curve (function of temperature) to give the heating COP at the specific inlet fluid temperatures and air flow rate at which the coil is operating. The curve should be normalized to have the value of 1.0 at the rated evaporator air flow rate (air flow fraction of 1.0). If this field is left blank, the heating COP remains constant (curve value assumed to be 1.0 for all air flow rates).

#### Field: Heating COP Function of Water Flow Fraction Curve Name

This alpha field specifies the name of a **quadratic** or **cubic** performance curve object (ref: Performance Curves) that defines the variation in DX coil heating COP as a function of the ratio of actual water flow rate through the condenser to the rated condenser water flow rate.. The output of this curve is multiplied by the rated COP and the output from the two other heating COP modifier curves (function of temperature and function of air flow fraction) to give the DX coil heating COP at the specific inlet fluid temperatures and flow rates at which the coil is operating. The curve should be normalized to have the value of 1.0 at the rated condenser water flow rate (water flow fraction of 1.0). If this field is left blank, the heating COP remains constant (curve value assumed to be 1.0 for all water flow rates).

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a **quadratic** or **cubic** performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the DX unit as a function of the part load ratio (PLR, sensible cooling load/steady-state sensible cooling capacity). The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling.

The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply:

PLF >= 0.7   and   PLF >= PLR

If PLF < 0.7 a warning message is issued, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, then a warning message is issued and the runtime fraction of the coil is limited to 1.0.

A typical part load fraction correlation for a conventional, single-speed DX cooling coil (e.g., residential unit) would be:

PLF = 0.85 + 0.15(PLR)

If the user wishes to model no efficiency degradation due to compressor cycling, the part load fraction correlation should be defined as follows:

PLF = 1.0 + 0.0(PLR)

Following is an example input for the [Coil:WaterHeating:AirToWaterHeatPump](#coilwaterheatingairtowaterheatpump) object.

~~~~~~~~~~~~~~~~~~~~

    Coil:WaterHeating:AirToWaterHeatPump,
        Zone4HPWHDXCoil,             !- Coil Name
        4000.0,                      !- Heating Capacity {W}
        3.2,                         !- Rated COP {W/W}
        0.6956,                      !- Rated SHR (gross)
        19.7,                        !- Rated Evaporator Inlet Air Dry-Bulb Temperature {C}
        13.5,                        !- Rated Evaporator Inlet Air Wet-Bulb Temperature {C}
        57.5,                        !- Rated Condenser Inlet Water Temperature {C}
        autocalculate,               !- Rated Evaporator Air Volumetric Flow Rate {m3/s}
        autocalculate,               !- Rated Condenser Water Volumetric Flow Rate {m3/s}
        No,                          !- Evaporator Fan Power Included in Rated COP
        No,                          !- Condenser Pump Power Included in Rated COP
        No,                          !- Condenser Pump Heat Included in Rated Heating Capacity and Rated COP
        150.0,                       !- Condenser Water Pump Power {W}
        0.1,                         !- Fraction of Condenser Pump Heat to Water
        Zone4AirOutletNode,          !- Evaporator Air Inlet Node Name
        Zone4DXCoilAirOutletNode,    !- Evaporator Air Outlet Node Name
        Zone4WaterInletNode,         !- Condenser Water Inlet Node Name
        Zone4WaterOutletNode,        !- Condenser Water Outlet Node Name
        100.0,                       !- Crankcase Heater Capacity {W}
        5.0,                         !- Maximum Ambient Temperature for Crankcase Heater Operation {C}
        wet-bulb temperature,        !- Evaporator Air Temperature Type for Curve Objects
        HPWHHeatingCapFTemp,         !- Heating Capacity Modifier Curve Name (function of temperature)
        ,                            !- Heating Capacity Modifier Curve Name (function of air flow fraction)
        ,                            !- Heating Capacity Modifier Curve Name (function of water flow fraction)
        HPWHHeatingCOPFTemp,         !- Heating COP Modifier Curve Name (function of temperature)
        ,                            !- Heating COP Modifier Curve Name (function of air flow fraction)
        ,                            !- Heating COP Modifier Curve Name (function of water flow fraction)
        HPWHPLFFPLR;                 !- Part Load Fraction Correlation Name (function of part load ratio)
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Coil Total Cooling Rate [W]
    HVAC,Sum,Cooling Coil Total Cooling Energy [J]
    HVAC,Average,Cooling Coil Sensible Cooling Rate [W]
    HVAC,Sum,Cooling Coil Sensible Cooling Energy [J]
    HVAC,Average,Cooling Coil Latent Cooling Rate [W]
    HVAC,Sum,Cooling Coil Latent Cooling Energy [J]
    HVAC,Average, Cooling Coil Runtime Fraction []
    HVAC,Average,DX Cooling Coil Crankcase Heater Electric Power [W]
    HVAC,Sum, Cooling Coil Crankcase Heater Electric Energy [J]
    HVAC,Average,Cooling Coil Total Water Heating Rate [W]
    HVAC,Sum,Cooling Coil Total Water Heating Energy [J]
    HVAC,Average,Cooling Coil Water Heating Electric Power[W]
    HVAC,Sum,Cooling Coil Water Heating Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Coil Total Cooling Rate [W]

This output field is the average total (sensible and latent) cooling rate output of the DX coil in Watts for the timestep being reported. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Total Cooling Energy [J]

This output field is the total (sensible plus latent) cooling output of the DX coil in Joules for the timestep being reported. This is determined by the coil inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Sensible Cooling Rate [W]

This output field is the average moist air sensible cooling rate output of the DX coil in Watts for the timestep being reported. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Sensible Cooling Energy [J]

This output field is the moist air sensible cooling output of the DX coil in Joules for the timestep being reported. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### DX Coil Latent Cooling Rate [W]

This output field is the average latent cooling rate output of the DX coil in Watts for the timestep being reported. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Latent Cooling Energy [J]

This output field is the latent cooling output of the DX coil in Joules for the timestep being reported. This is determined by the inlet and outlet air conditions and the air mass flow rate through the coil.

#### Cooling Coil Runtime Fraction  []

This output field is the average runtime fraction of the DX coil compressor for the timestep being reported. This also represents the runtime fraction of the condenser water pump.

#### Cooling Coil Crankcase Heater Electric Power[W]

This output field is the average electricity consumption rate of the DX coil compressor's crankcase heater in Watts for the timestep being reported. The crankcase heater operates only when the compressor is off and the air surrounding the compressor is below the Maximum Ambient Temperature for Crankcase Heater Operation, otherwise this output variable is set equal to 0.

#### Cooling Coil Crankcase Heater Electric Energy [J]

This output field is the total electricity consumption of the DX coil compressor's crankcase heater in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = DHW, Group Key = Plant (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Cooling Coil Total Water Heating Rate [W]

This output field is the average water heating rate output of the DX coil (condenser coil plus condenser water pump) in Watts for the timestep being reported. This is determined using the inlet and outlet water temperatures and the water mass flow rate through the condenser coil.

#### Cooling Coil Total Water Heating Energy [J]

This output field is the total water heating output of the DX coil (condenser coil plus condenser water pump) in Joules for the timestep being reported. This is determined using the inlet and outlet water temperatures and the water mass flow rate through the condenser coil.

#### Cooling Coil Water Heating Electric Power[W]

This output field is the average electricity consumption rate of the DX coil compressor and condenser pump in Watts for the timestep being reported.

#### Cooling Coil Water Heating Electric Energy [J]

This output field is the electricity consumption of the DX coil compressor and condenser pump in Joules for the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = DHW, Group Key = Plant (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

## Coil:Cooling:WaterToAirHeatPump:ParameterEstimation

The **Coil:Cooling:WaterToAirHeatPump:ParameterEstimation** coil is a deterministic model that requires parameters to describe the operating conditions of the heat pump's components. The parameters are generated from the manufacturer catalog data using multi-variable optimization method. In addition, the cooling coil model can be used for 3 type of compressors: **reciprocating**, **rotary** and **scroll**. Descriptions and strength of each respective model is available in the following references:

Jin, Hui. 2002. Parameter Estimation Based Models of Water Source Heat Pumps. Phd. Thesis, Department of Mechanical and Aerospace Engineering, Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

Tang,C. C. 2005. Modeling Packaged Heat Pumps in Quasi-Steady State Energy Simulation Program. M.S. Thesis. Department of Mechanical and Aerospace Engineering, Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

### Inputs

#### Field: Name

This alpha field contains the identifying name for the coil. Any reference to this coil by another object (e.g., [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair)) will use this name.

#### Field: Compressor Type

Type of compressor mode used for the heat pump. Choices available are reciprocating, rotary and scroll compressor. Note that the parameters vary for different compressor.

#### Field: Refrigerant Type

This alpha field contains the type of refrigerant used by the heat pump.

#### Field: Design Source Side Flow Rate

This numeric field defines the water flow rate though the coil in m3/sec

#### Field: Nominal Cooling Coil Capacity

This numeric field defines the nominal cooling capacity for the WatertoAirHP cooling coil in Watts.

#### Field: Nominal Time for Condensate Removal to Begin

This numeric field defines the nominal time (in seconds) after startup for condensate to begin leaving the coil's condensate drain line at the coil's rated airflow and temperature conditions, starting with a dry coil. Nominal time is equal to the ratio of the energy of the coil's maximum condensate holding capacity (J) to the coil's steady-state latent capacity (W). Suggested value is 1000; zero value means the latent degradation model is disabled. The default value for this field is zero.

#### Field: Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity

This numeric field defines ratio of the initial moisture evaporation rate from the cooling coil (when the compressor first turns off, in Watts) and the coil's steady-state latent capacity (Watts) at rated airflow and temperature conditions. Suggested value is 1.5; zero value means the latent degradation model is disabled. The default value for this field is zero.

#### Field: High Pressure Cutoff

This numeric field defines the compressor's maximum allowable pressure in Pascal (N/m2)

#### Field: Low Pressure Cutoff 

This numeric field defines the compressor's minimum allowable pressure in Pascal (N/m2)

#### Field: Water Inlet Node Name

This alpha field contains the cooling coil source side inlet node name.

#### Field: Water Outlet Node Name

This alpha field contains the cooling coil source side outlet node name.

#### Field: Air Inlet Node Name

This alpha field contains the cooling coil air inlet node name.

#### Field: Air Outlet Node Name

This alpha field contains the cooling coil air outlet node name.

#### Cooling Coil Parameters

Depending on the type of compressor and the source side fluid specified, the type of parameters and values also differs. An Excel Spreadsheet is developed to estimate the parameters based on the manufacturer data. The general parameters are listed first followed by specific parameters required by the respective compressor model. Lastly, parameters are listed based on the type of source side fluid used.

#### Field: Load Side Total Heat Transfer Coefficient 

This numeric field defines the estimated parameter **load side total heat transfer coefficient** in W/K. This field was previously known as Parameter 1.

#### Field: Load Side Outside Surface Heat Transfer Coefficient 

This numeric field defines the estimated parameter **load side outside surface heat transfer coefficient** in W/K. This field was previously known as Parameter 2.

#### Field: Superheat Temperature at the Evaporator Outlet 

This numeric field defines the estimated parameter **superheat temperature** at the evaporator outlet in ˚C. This field was previously known as Parameter 3.

#### Field: Compressor Power Losses 

This numeric field defines the estimated parameter **compressor power losses**, which accounts for the loss of work due to mechanical and electrical losses in the compressor in Watts. This field was previously known as Parameter 4.

#### Field: Compressor Efficiency 

This numeric field defines the estimated parameter of the **compressor's efficiency**. The compressor efficiency is formulated as the equation below:

![](media/image357.png)\


This field was previously know as Parameter 5.

#### Field: Compressor Piston Displacement 

This numeric field defines the estimated parameter **piston displacement of the compressor** in m3/s. This field was part of what was previously known as Parameter 6. It should be used when the Compressor type is either Reciprocating and Rotary. The field should be left blank when Compressor type is Scroll.

#### Field: Compressor Suction/Discharge Pressure Drop 

This numeric field defines the estimated parameter **pressure drop** at the compressor suction and discharge in Pascals (N/m2). This field was part of what was previously known as Parameter 7. It should be used when the Compressor type is either Reciprocating and Rotary. The field should be left blank when Compressor type is Scroll.

#### Field: Compressor Clearance Factor

This numeric field defines the estimated parameter **clearance factor of the compressor**. This parameter is dimensionless. This field was part of what was previously known as Parameter 8. It should only be used when the Compressor type is Reciprocating. The field should be left blank when Compressor type is Scroll or Rotary.

#### Field: Refrigerant Volume Flow Rate 

This numeric field defines the **refrigerant volume flow rate** at the beginning of the compression [m3/s]. This field was part of what was previously known as Parameter 6. It should only be used when the Compressor type is Scroll. The field should be left blank when Compressor type is Reciprocating or Rotary.

#### Field: Volume Ratio

This numeric field defines the built-in-**volume ratio**. This field was part of what was previously known as Parameter 7. It should only be used when the Compressor type is Scroll. The field should be left blank when Compressor type is Reciprocating or Rotary.

#### Field: Leak Rate Coefficient

This numeric field defines the **leak rate coefficient** for the relationship between pressure ratio and leakage rate. This field was part of what was previously known as Parameter 8. It should only be used when the Compressor type is Scroll. The field should be left blank when Compressor type is Reciprocating or Rotary.

#### Field: Source Side Heat Transfer Coefficient

This numeric field defines the estimated parameter **source side heat transfer coefficient** in W/K. This field was part of what was previously known as Parameter 9. It should only be used when the Source Side Fluid Name is Water.

#### Field: Source Side Heat Transfer Resistance1

This numeric field defines the estimated parameter **source side heat transfer resistance** 1. Unit is dimensionless. This field was part of what was previously known as Parameter 9. It should only be used when the Source Side Fluid Name is an antifreeze.

#### Field: Source Side Heat Transfer Resistance2

This numeric field defines the estimated parameter **source side heat transfer resistance** 2. Unit is W/K. This field was previously known as Parameter 10. It should only be used when the Source Side Fluid Name is an antifreeze.

Following is an example for COIL:WaterToAirHP:ParameterEstimation:Cooling coil input

~~~~~~~~~~~~~~~~~~~~

      Coil:Cooling:WaterToAirHeatPump:ParameterEstimation,
        Heat Pump Cooling Mode AHU1,  !- Name
        Scroll,                  !- Compressor Type
        R22,                     !- Refrigerant Type
        0.0015,                  !- Design Source Side Flow Rate {m3/s}
        38000,                   !- Nominal Cooling Coil Capacity {W}
        0,                       !- Nominal Time for Condensate Removal to Begin {s}
        0,     !- Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}
        3000000,                 !- High Pressure Cutoff {Pa}
        0,                       !- Low Pressure Cutoff {Pa}
        AHU1 Water to Air Heat Pump Source Side1 Inlet Node,  !- Water Inlet Node Name
        AHU1 Water to Air Heat Pump Source Side1 Outlet Node,  !- Water Outlet Node Name
        AHU 1 Supply Fan Outlet, !- Air Inlet Node Name
        Cooling Coil Air Outlet Node AHU1,  !- Air Outlet Node Name
        3.78019E+03,             !- Load Side Total Heat Transfer Coefficient {W/K}
        3.41103E+03,             !- Load Side Outside Surface Heat Transfer Coefficient {W/K}
        1.57066E+00,             !- Superheat Temperature at the Evaporator Outlet {C}
        2.23529E+03,             !- Compressor Power Losses {W}
        1.34624E+00,             !- Compressor Efficiency
        ,                        !- Compressor Piston Displacement {m3/s}
        ,                        !- Compressor Suction/Discharge Pressure Drop {Pa}
        ,                        !- Compressor Clearance Factor {dimensionless}
        9.74424E-03,             !- Refrigerant Volume Flow Rate {m3/s}
        2.30803E+00,             !- Volume Ratio {dimensionless}
        2.06530E-02,             !- Leak Rate Coefficient
        1.92757E+03,             !- Source Side Heat Transfer Coefficient {W/K}
        ,                        !- Source Side Heat Transfer Resistance1 {dimensionless}
        ;                        !- Source Side Heat Transfer Resistance2 {W/K}
~~~~~~~~~~~~~~~~~~~~

## Coil:Cooling:WaterToAirHeatPump:EquationFit

The [Coil:Cooling:WaterToAirHeatPump:EquationFit](#coilcoolingwatertoairheatpumpequationfit) is a simple curve-fit model that requires coefficients generated from the heat pump catalog data. This is an equation-fit model that resembles a black box with no usage of heat transfer equations. The performance of the heat pump is modeled using curves fitted from the catalog data. Description of the equation-fit model is available in the following reference:

Tang,C. C. 2005. Modeling Packaged Heat Pumps in Quasi-Steady State Energy Simulation Program. M.S. Thesis. Department of Mechanical and Aerospace Engineering, Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

### Inputs

#### Field: Name

This alpha field contains the identifying name for the coil. Any reference to this coil by another object (e.g., [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair)) will use this name.

#### Field: Water Inlet Node Name

This alpha field contains the cooling coil source side inlet node name.

#### Field: Water Outlet Node Name

This alpha field contains the cooling coil source side outlet node name.

#### Field: Air Inlet Node Name

This alpha field contains the cooling coil air inlet node name.

#### Field: Air Outlet Node Name

This alpha field contains the cooling coil air outlet node name.

#### Field: Rated Air Flow Rate

This numeric field contains the rated volumetric air flow rate on the load side of the heat pump in m3/s. This field is autosizable.

#### Field: Rated Water Flow Rate

This numeric field contains the rated volumetric water flow rate on the source side of the heat pump in m3/s. This field is autosizable.

#### Field: Gross Rated Total Cooling Capacity

This numeric field contains the gross rated total cooling capacity of the heat pump in W. This field is autosizable.The gross rated total cooling capacity should be within 20% of the gross rated heating capacity, otherwise a warning message is issued. The gross rated total cooling capacity should not account for the effect of supply air fan heat.

#### Field: Gross Rated Sensible Cooling Capacity

This numeric field contains the gross rated sensible capacity of the heat pump in W. This field is autosizable. The gross rated sensible cooling capacity should not account for the effect of supply air fan heat.

#### Field: Rated Cooling Coefficient of Performance

This numeric field contains the rated cooling coefficient of performance of the heat pump.

#### Field: Total Cooling Capacity Coefficient 1 to 5

These numeric fields contain the first to fifth coefficient for the heat pump total cooling capacity.

#### Field: Sensible Cooling Capacity Coefficient 1 to 6 

These numeric fields contain the first to sixth coefficient for the heat pump sensible cooling capacity.

#### Field: Cooling Power Consumption Coefficient 1 to 5

These numeric fields contain the first to fifth coefficient for the heat pump power consumption.

#### Field: Nominal Time for Condensate Removal to Begin

This numeric field defines the nominal time (in seconds) after startup for condensate to begin leaving the coil's condensate drain line at the coil's rated airflow and temperature conditions, starting with a dry coil. Nominal time is equal to the ratio of the energy of the coil's maximum condensate holding capacity (J) to the coil's steady-state latent capacity (W). Suggested value is 1000; zero value means the latent degradation model is disabled. The default value for this field is zero.

#### Field: Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity

This numeric field defines ratio of the initial moisture evaporation rate from the cooling coil (when the compressor first turns off, in Watts) and the coil's steady-state latent capacity (Watts) at rated airflow and temperature conditions. Suggested value is 1.5; zero value means the latent degradation model is disabled. The default value for this field is zero.

Following is an example of the input for Coil:WaterToAirHP:EquationFit:Cooling coil

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:WaterToAirHeatPump:EquationFit,
        Heat Pump Cooling Mode,!- Name of Coil
        Water to Air Heat Pump Source Side1 Inlet Node,!- Coil Water Inlet Node Name
        Water to Air Heat Pump Source Side1 Outlet Node,!- Coil Water Outlet Node Name
        Cooling Coil Air Inlet Node,!- Coil Air Inlet Node Name
        Heating Coil Air Inlet Node,!- Coil Air Outlet Node Name
        4.6015E-01,!- Rated Air Volumetric Flow Rate
        2.8391E-04,!- Rated Water Volumetric Flow Rate
        23125.59,!- Gross Rated Total Cooling Capacity
        16267.05,!- Gross Rated Sensible Cooling Capacity
        4.7,!- Rated Cooling Coefficient of Performance
        -0.68126221,!- Total Cooling Capacity Coefficient 1
        1.99529297,!- Total Cooling Capacity Coefficient 2
        -0.93611888,!- Total Cooling Capacity Coefficient 3
        0.02081177,!- Total Cooling Capacity Coefficient 4
        0.008438868,!- Total Cooling Capacity Coefficient 5
        2.24209455,!- Sensible Cooling Capacity Coefficient 1
        7.28913391,!- Sensible Cooling Capacity Coefficient 2
        -9.06079896,!- Sensible Cooling Capacity Coefficient 3
        -0.36729404,!- Sensible Cooling Capacity Coefficient 4
        0.218826161,!- Sensible Cooling Capacity Coefficient 5
        0.00901534,!- Sensible Cooling Capacity Coefficient 6
        -3.20456384,!- Cooling Power Consumption Coefficient 1
        0.47656454,!- Cooling Power Consumption Coefficient 2
        3.16734236,!- Cooling Power Consumption Coefficient 3
        0.10244637,!- Cooling Power Consumption Coefficient 4
        -0.038132556,!- Cooling Power Consumption Coefficient 5
        0,!- Nominal Time for Condensate Removal to Begin
        0;!- Ratio of Initial Moisture Evaporation Rate and Steady-state Latent Capacity
~~~~~~~~~~~~~~~~~~~~

### Outputs

[Coil:Cooling:WaterToAirHeatPump:ParameterEstimation](#coilcoolingwatertoairheatpumpparameterestimation) and [Coil:Cooling:WaterToAirHeatPump:EquationFit](#coilcoolingwatertoairheatpumpequationfit) have the same output variables listed as follows;

~~~~~~~~~~~~~~~~~~~~

    HVAC, Average, Cooling Coil Electric Power [W]
    HVAC, Average, Cooling Coil Total Cooling Rate [W]
    HVAC, Average, Cooling Coil Sensible Cooling Rate [W]
    HVAC, Average, Cooling Coil Source Side Heat Transfer Rate [W]
    HVAC, Average, Cooling Coil Part Load Ratio []
    HVAC, Average, Cooling Coil Runtime Fraction []

    HVAC, Average, Cooling Coil Air Mass Flow Rate [kg/s]
    HVAC, Average, Cooling Coil Air Inlet Temperature [C]
    HVAC, Average, Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]
    HVAC, Average, Cooling Coil Air Outlet Temperature [C]
    HVAC, Average, Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]

    HVAC, Average, Cooling Coil Source Side Mass Flow Rate [kg/s]
    HVAC, Average, Cooling Coil Source Side Inlet Temperature [C]
    HVAC, Average, Cooling Coil Source Side Outlet Temperature [C]

    HVAC, Sum, Cooling Coil Electric Energy [J]
    HVAC, Sum, Cooling Coil Total Cooling Energy [J]
    HVAC, Sum, Cooling Coil Sensible Cooling Energy [J]
    HVAC, Sum, Cooling Coil Latent Cooling Energy [J]
    HVAC, Average, Cooling Coil Latent Cooling Rate [W]
    HVAC, Sum, Cooling Coil Source Side Heat Transfer Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Coil Electric Power [W]

This output variable is the average electric consumption rate of the heat pump in Watts over the timestep being reported.

#### Cooling Coil Total Cooling Rate [W]

The output variable is the average total cooling load provide by the heat pump which includes the sensible and latent load in Watts over the timestep being reported.

#### Cooling Coil Sensible Cooling Rate [W]

The output variable is the average sensible cooling load provide by the heat pump in Watts over the timestep being reported.

#### Cooling Coil Source Side Heat Transfer Rate [W]

The output variable is the average heat rejected to the water at the heat pump condenser in Watts over the timestep being reported.

#### Cooling Coil Part Load Ratio []

This output variable is the ratio of the part-load capacity to the steady state capacity of the WatertoAirHP coil. For the cycling fan mode, the runtime fraction for the heat pump compressor may be different from the compressor part-load ratio reported here due to the part-load performance of the WatertoAirHP coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate.

#### Cooling Coil Runtime Fraction []

This output variable is the function of the part load ratio (PLR, part-load capacity/ steady state capacity). The duty factor or part load fraction accounts for efficiency losses due to compressor cycling.

#### Cooling Coil Air Mass Flow Rate [kg/s]

The output variable is the average air mass flow rate going through the heat pump over the timestep being reported.

#### Cooling Coil Air Inlet Temperature [C]

The output variable is the average entering air dry-bulb temperature over the timestep being reported.

#### Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average entering air dry humidity ratio over the timestep being reported.

#### Cooling Coil Air Outlet Temperature [C]

The output variable is the average leaving air dry-bulb temperature over the timestep being reported.

#### Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average leaving air dry humidity ratio over the timestep being reported.

#### Cooling Coil Source Side Mass Flow Rate [kg/s]

The output variable is the average water mass flow rate going through the heat pump over the timestep being reported.

#### Cooling Coil Source Side Inlet Temperature [C]

The output variable is the average entering water temperature over the timestep being reported.

#### Cooling Coil Source Side Outlet Temperature [C]

The output variable is the average leaving water temperature over the timestep being reported.

#### Cooling Coil Electric Energy [J]

The output variable is the electric consumption of the heat pump in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Cooling, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Cooling Coil Total Cooling Energy [J]

The output variable is the total cooling output of the coil in Joules over the timestep being reported. Resource Type = EnergyTransfer, End Use Key = CoolingCoils, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Cooling Coil Sensible Cooling Energy [J]

The output variable is the total sensible cooling output of the coil in Joules over the timestep being reported

#### Cooling Coil Latent Cooling Energy [J]

#### Cooling Coil Latent Cooling Rate [W]

These output variables are the total latent cooling output of the coil in Joules or Watts over the timestep being reported.

#### Cooling Coil Source Side Heat Transfer Energy [J]

The output variable is the total source side heat transfer of the coil in Joules over the timestep being reported

## Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit

The Variable-Speed Water-to-Air Cooling Equation Fit Coil is a collection of performance curves that represent the cooling coil at various speed levels. The performance curves should be generated from the heat pump Reference Unit data. This is an equation-fit model that resembles a black box with no usage of heat transfer equations. On the other hand, the model uses the bypass factor approach to calculate sensible heat transfer rate, similar to the one used in the single-speed DX coil. The number of speed levels can range from 2 to 10. The cooling coil has four node connections, i.e. two air sides and two water sides. The user needs to specify a nominal speed level, at which the gross rated total cooling capacity, rated volumetric air and water flow rates are sized. The rated capacity, rated volumetric flow rates represent the real situation in the air and water loops, and are used to determine and flow rates at various speed levels in the parent objects, e.g. [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair) and [ZoneHVAC:WaterToAirHeatPump](#zonehvacwatertoairheatpump). It shall be mentioned that the performance correction curves, i.e. the temperature and flow fraction correction curves, should be normalized to the capacity and flow rates at each individual speed and at the rated conditions, similar to the performance curves used in the DX coils. However, the performance values, e.g. gross capacities, gross COPs, gross SHRs and flow rates at individual speed levels, should be given regarding a specific unit from the Reference Unit catalog data. In the following content, the statement started with "Reference Unit" means the actual Reference Unit catalog data. The rated conditions for obtaining the capacities, COPs and SHRs  are at indoor dry-bulb temperature of 26.67 ˚C (80 ˚F), wet bulb temperature of 19.44 ˚C (67 ˚F),  and the source side entering water temperature of 29.4 ˚C (85 ˚F). Some equations are provided below to help explain the function of the various performance curves and data fields. For a detailed description of the algorithm and how the curves are used in the calculations, please see the Engineering Reference.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the variable speed cooling coil. Any reference to this coil by another object (e.g., [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair)) will use this name.

#### Field: Water Inlet Node Name

This alpha field contains the cooling coil source side inlet node name.

#### Field: Water Outlet Node Name

This alpha field contains the cooling coil source side outlet node name.

#### Field: Air Inlet Node Name

This alpha field contains the cooling coil load side inlet node name.

#### Field: Air Outlet Node Name

This alpha field contains the cooling coil load side outlet node name.

#### Field: Number of Speeds

This numeric field contains the maximum number of speed levels that the module uses. The number of speeds, for which the user input the performance data and curves, should be equal or higher than the maximum number. The performance inputs at higher speed levels are ignored.

#### Field: Nominal Speed Level

This numeric field defines the nominal speed level, at which the rated capacity, rated air and water volumetric flow rates are correlated.

#### Field: Gross Rated Total Cooling Capacity at Selected Nominal Speed Level

This numeric field contains the gross rated total cooling capacity at the nominal speed level.  This field is autosizable. The gross rated total cooling capacity is used to determine a capacity scaling factor, as compared to the Reference Unit capacity at the nominal speed level.

![](media/image358.png)\


And then, this scaling factor is used to determine capacities at rated conditions for other speed levels, as below,

![](media/image359.png)\


#### Field: Rated Volumetric Air Flow Rate

This numeric field contains the rated volumetric air flow rate on the load side of the heat pump corresponding to the nominal speed level. This field is autosizable. The value is used to determine an internal scaling factor, and calculate the air flow rates in the parent objects.  It is recommended that the ratio of the rated volumetric air flow rate to the rated capacity is the same as the unit performance from the Reference Unit data.

![](media/image360.png)\


And the volumetric air flow rates in the parent objects are calculated as below,

![](media/image361.png)\


#### Field: Rated Volumetric Water Flow Rate

This numeric field contains the rated volumetric water flow rate on the source side of the heat pump at the nominal speed level. This field is autosizable. The value is used to determine an internal scaling factor, and calculate the water flow rates in the water loop.  It is recommended that the ratio of the rated volumetric water flow rate to the rated capacity is the same as the unit performance from the Reference Unit data.

![](media/image362.png)\


And the required volumetric water flow rates at the speed levels in the parent objects, other than the nominal speed, are calculated as below,

![](media/image363.png)\


#### Field: Nominal Time for Condensate Removal to Begin

This numeric field defines the nominal time (in seconds) after startup for condensate to begin leaving the coil's condensate drain line at the coil's rated airflow and temperature conditions, starting with a dry coil. Nominal time is equal to the ratio of the energy of the coil's maximum condensate holding capacity (J) to the coil's steady-state latent capacity (W). Suggested value is 1000; zero value means the latent degradation model is disabled. The default value for this field is zero.

#### Field: Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity

This numeric field defines ratio of the initial moisture evaporation rate from the cooling coil (when the compressor first turns off, in Watts) and the coil's steady-state latent capacity (Watts) at rated airflow and temperature conditions. Suggested value is 1.5; zero value means the latent degradation model is disabled. The default value for this field is zero.

#### Field: Flag for Using Hot Gas Reheat, 0 or 1

This numeric field dictates whether to use the recoverable waste heat for reheating the supply air, downstream of the coil. The value "1" means using the hot gas reheating, and "0" means not using. If the hot gas reheating is turned on, the recoverable waste heat is subtracted from both the total cooling capacity and the sensible cooling capacity. The default value for this field is zero.

#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the unit as a function of the part load ratio (PLR, sensible or latent load/steady-state sensible or latent cooling capacity for Speed 1), in the case that the unit operates under the lowest speed, i.e. on/off. The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep for Speed 1. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling. The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply:

PLF >= 0.7 and PLF >= PLR

If PLF < 0.7, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, the runtime fraction of the coil is limited to 1.0. A typical part load fraction correlation would be:

![](media/image364.png)\


If the user wishes to model no efficiency degradation due to compressor cycling, the part load fraction correlation should be defined as follows:

![](media/image365.png)\


#### Field Group: rated specification, performance curves, and waste heat data

The performance for each cooling speed must be specified as shown below. They should be directly given from the Reference Unit catalog data. All inputs for Speed 1 are required, followed by the optional inputs for other speeds.

#### Field: Speed <x> Reference Unit Gross Rated Total Cooling Capacity

This numeric field defines the total, full load cooling capacity in watts of the water-to-air cooling coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0. The gross rated total capacity should not account for the effect of supply air fan heat.

#### Field: Speed <x> Reference Unit Gross Rated Sensible Heat Ratio

This numeric field defines sensible heat transfer ratio (SHR = gross sensible cooling capacity divided by gross total cooling capacity) of the cooling coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0.0 and less than 1.0. This value should be obtained from the Reference Unit data. The gross rated SHR should not account for the effect of supply air fan heat.

#### Field: Speed <x> Reference Unit Gross Rated Cooling COP

This numeric field defines the coefficient of performance (COP=gross total cooling capacity in watts divided by electrical power input in watts) of the cooling coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0. The input power includes power for the compressor(s) and accessories, but does not include the power consumptions of the indoor supply air fan and water pump. The gross COP should not account for the supply air fan.

#### Field: Speed <x> Reference Unit Rated Air Flow Rate

This numeric field defines the volumetric air flow rate, in m^3^ per second, across the cooling coil at rated conditions for Speed <x> operation. The value entered here should be directly from the Reference Unit data, corresponding to the given gross rated total cooling capacity and gross rated cooling COP at the speed, as above.

#### Field: Speed <x> Reference Unit Rated Water Flow Rate

This numeric field defines the volumetric water flow rate, in m^3^ per second, flowing at the source side of the cooling coil at rated conditions for Speed <x> operation. The value entered here should be directly from the Reference Unit data, corresponding to the given gross rated total cooling capacity and gross rated cooling COP at the speed, as above.

#### Field: Speed <x> Total Cooling Capacity Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the both the indoor wet-bulb and source side entering water temperature, from the Reference Unit. The output of this curve is multiplied by the gross rated total cooling capacity at the speed to give the gross total cooling capacity at specific temperature operating conditions (i.e., at an indoor air wet-bulb temperature or entering water temperature different from the rating point temperature). It should be noted that the curve is normalized to the cooling capacity at Speed<x> from the Reference Unit data, and have the value of 1.0 at the rating point.

#### Field: Speed <x> Total Cooling Capacity Function of Air Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the ratio of actual air flow rate across the cooling coil to the design air flow rate (i.e., fraction of full load flow at Speed <x>, from the Reference Unit data). The curve is normalized to have the value of 1.0 when the actual air flow rate equals the design air flow rate, at Speed <x>.

#### Field: Speed <x> Total Cooling Capacity Function of Water Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the gross total cooling capacity as a function of the ratio of actual water flow rate across the cooling coil to the design water flow rate (i.e., fraction of full load flow), at Speed <x>, from the Reference Unit data. The curve is normalized to have the value of 1.0 when the actual water flow rate equals the design air flow rate, at Speed <x>.

The actual total cooling capacity at Speed <x>, considering variations in temperatures, air and water flow rates is calculated as below:

![](media/image366.png)\


#### Field: Speed <x> Energy Input Ratio Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic, quadratic or cubic performance curve for

Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the both the indoor air wet-bulb and entering water temperatures The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP at Speed <x> from the Reference Unit data) to give the EIR at specific temperature operating conditions (i.e., at an indoor air wet-bulb temperature or entering water temperature different from the rating point temperature). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Speed <x> Energy Input Ratio Function of Air Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the cooling coil to the design air flow rate (i.e., fraction of full load flow, at Speed <x> from the Reference Unit data). The EIR is the inverse of the COP. This curve is normalized to a value of 1.0 when the actual air flow rate equals the design air flow rate.

#### Field: Speed <x> Energy Input Ratio Function of Water Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual water flow rate across the cooling coil to the design water flow rate (i.e., fraction of full load flow, at Speed <x> from the Reference Unit data). This curve is normalized to a value of 1.0 when the actual air flow rate equals the design air flow rate.

The actual EIR at Speed <x>, considering variations in temperatures, air and water flow rates is calculated as below:

![](media/image367.png)\


And, the actual power consumption is calculated:

![](media/image368.png)\


#### Field: Speed <x> Reference Unit Waste Heat Fraction of Power Input

The fraction of heat input to cooling that is available as recoverable waste heat at full load and rated conditions for Speed <x> operation. The part of heat is not discharged to the water loop. And it can be used for hot gas reheating during dehumidification operation, by setting 1 to Flag for Using Hot Gas Reheat. ****

#### Field: Speed <x> Waste Heat Function of Temperature Curve Name

The name of a bi-quadratic performance curve (ref: Performance Curves) that parameterizes the variation of the waste heat recovery as a function of indoor wet-bulb temperature and the entering water temperature for Speed <x> from the Reference Unit data. The output of this curve is multiplied by the design recoverable waste heat at specific temperature operating conditions (i.e., at temperatures different from the rating point). The curve is normalized to a value of 1.0 at the rating point.

The actual recoverable waste heat at Speed <x> is calculated as below:

![](media/image369.png)\


An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit,
        Sys 1 Heat Pump Cooling Mode,         !- Name
        Sys 1 Water to Air Heat Pump Source Side1 Inlet Node,  !- Water Inlet Node Name
        Sys 1 Water to Air Heat Pump Source Side1 Outlet Node, !- Water Outlet Node Name
        Sys 1 Cooling Coil Air Inlet Node,    !- Air Inlet Node Name
        Sys 1 Heating Coil Air Inlet Node,    !- Air Outlet Node Name
        10.0,                 !- Number of Speeds
        10.0,                 !- Nominal Speed Level
        Autosize,                   !- Gross Rated Total Cooling Capacity {W}
        Autosize,                       !- Rated Air Flow Rate {m3/s}
        Autosize,                   !- Rated Water Flow Rate {m3/s}
        0.0, !Nominal time for Condensate to Begin Leaving the Coil
        0.0, !Initial Moisture Evaporation Rate Divided by Steady-State AC Latent Capacity
        1,         !Flag for Using Hot Gas Reheat or Not, 0 = false; 1 = true
        VS Energy Part Load Fraction 1,  !Energy Part Load Fraction Curve
        1524.1,         !-Speed 1 Reference Unit Gross Rated Total Cooling Capacity
        0.75,                   !-Speed 1 Reference Unit Gross Sensible Heat Ratio
        4.0,                    !-Speed 1 Reference Unit Gross Cooling COP
        0.1359072,              !-Speed 1 Reference Unit Rated air flow rate
        0.000381695,            !-Speed 1 Reference Unit Rated water flow rate
        TC VS Temp1 Test,       !-Speed 1 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test,   !-Speed 1 Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test,   !-Speed 1 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test,       !-Speed 1 Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test,   !-Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test,   !-Speed 1 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,   !-Speed 1 Waste Heat fraction to power input
        wasteHeat VS Temp1 Test, !-Speed 1 Waste Heat Function of Temperature Curve Name
        1877.9,          !-Speed 2 Reference Unit Gross Rated Total Cooling Capacity
        0.75,                    !-Speed 2 Reference Unit Gross Sensible Heat Ratio
        4.0,                     !-Speed 2 Reference Unit Gross Cooling COP
        0.151008,                !-Speed 2 Reference Unit Rated air flow rate
        0.000381695,             !-Speed 2 Reference Unit Rated water flow rate
        TC VS Temp1 Test,        !-Speed 2 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test,   !-Speed 2  Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test,   !-Speed 2  Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test,       !-Speed 2  Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test,   !-Speed 2  Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test,   !-Speed 2  Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,   !-Speed 2  Waste Heat fraction to power input
        wasteHeat VS Temp1 Test, !-Speed 2  Waste Heat Function of Temperature Curve Name
        2226.6,          !-Speed 3 Reference Unit Gross Rated Total Cooling Capacity
        0.75,                    !-Speed 3 Reference Unit Gross Sensible Heat Ratio
        4.0,                     !-Speed 3 Reference Unit Gross Cooling COP
        0.1661088,                !-Speed 3 Reference Unit Rated air flow rate
        0.000381695,              !-Speed 3 Reference Unit Rated water flow rate
        TC VS Temp1 Test,         !-Speed 3 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test, !-Speed 3 Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test,  !-Speed 3 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test, !-Speed 3 Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test,   !-Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test, !-Speed 3 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,         !-Speed 3 Waste Heat fraction to power input
        wasteHeat VS Temp1 Test,!-Speed 3 Waste Heat Function of Temperature Curve Name
        2911.3,        !-Speed 4 Reference Unit Gross Rated Total Cooling Capacity
        0.75,                  !-Speed 4 Reference Unit Gross Sensible Heat Ratio
        4.0,                   !-Speed 4 Reference Unit Gross Cooling COP
        0.1963104,             !-Speed 4 Reference Unit Rated air flow rate
        0.000381695,           !-Speed 4 Reference Unit Rated water flow rate
        TC VS Temp1 Test,      !-Speed 4 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test, !-Speed 4 Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test, !-Speed 4 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test, !-Speed 4 Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test, !-Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test, !-Speed 4 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1, !-Speed 4 Waste Heat fraction to power input
        wasteHeat VS Temp1 Test, !-Speed 4 Waste Heat Function of Temperature Curve Name
        3581.7,        !- Speed 5 Reference Unit Gross Rated Total Cooling Capacity
        0.75,                  !- Speed 5 Reference Unit Gross Sensible Heat Ratio
        4.0,                   !- Speed 5 Reference Unit Gross Cooling COP
        0.226512,              !- Speed 5 Reference Unit Rated air flow rate
        0.000381695,           !- Speed 5 Reference Unit Rated water flow rate
        TC VS Temp1 Test,      !- Speed 5 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test, !- Speed 5 Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test, !- Speed 5 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test, !- Speed 5 Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test, !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test, !- Speed 5 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1, !- Speed 5 Waste Heat fraction to power input
        wasteHeat VS Temp1 Test, !- Speed 5 Waste Heat Function of Temperature Curve Name
        4239.5,       !-Speed 6 Reference Unit Gross Total Cooling Capacity
        0.75,                  !-Speed 6 Reference Unit Gross Sensible Heat Ratio
        4.0,                   !-Speed 6 Reference Unit Gross Cooling COP
        0.2567136,             !-Speed 6 Reference Unit Rated air flow rate
        0.000381695,           !-Speed 6 Reference Unit Rated water flow rate
        TC VS Temp1 Test,      !-Speed 6 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test, !-Speed 6 Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test, !-Speed 6 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test, !-Speed 6 Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test, !-Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test, !-Speed 6 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1, !-Speed 6 Waste Heat fraction to power input
        wasteHeat VS Temp1 Test, !-Speed 6 Waste Heat Function of Temperature Curve Name
        4885.7,        !-Speed 7 Reference Unit Gross Rated Total Cooling Capacity
        0.75,                  !-Speed 7 Reference Unit Gross Sensible Heat Ratio
        4.0,                   !-Speed 7 Reference Unit Gross Cooling COP
        0.2869152,             !-Speed 7 Reference Unit Rated air flow rate
        0.000381695,           !-Speed 7 Reference Unit Rated water flow rate
        TC VS Temp1 Test,      !-Speed 7 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test, !-Speed 7 Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test, !-Speed 7 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test, !-Speed 7 Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test, !-Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test, !-Speed 7 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1, !-Speed 7 Waste Heat fraction to power input
        wasteHeat VS Temp1 Test, !-Speed 7 Waste Heat Function of Temperature Curve Name
        5520.7,        !-Speed 8 Reference Unit Gross Rated Total Cooling Capacity
        0.75,                   !-Speed 8 Reference Unit Gross Sensible Heat Ratio
        4.0,                    !-Speed 8 Reference Unit Gross Cooling COP
        0.3171168,              !-Speed 8 Reference Unit Rated air flow rate
        0.000381695,            !-Speed 8 Reference Unit Rated water flow rate
        TC VS Temp1 Test,       !-Speed 8 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test,  !-Speed 8 Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test, !-Speed 8 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test, !-Speed 8 Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test, !-Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test, !-Speed 8 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1, !-Speed 8 Waste Heat fraction to power input
        wasteHeat VS Temp1 Test, !-Speed 8 Waste Heat Function of Temperature Curve Name
        6144.8,          !-Speed 9 Reference Unit Gross Rated Total Cooling Capacity
        0.75,                    !-Speed 9 Reference Unit Gross Sensible Heat Ratio
        4.0,                     !-Speed 9 Reference Unit Gross Cooling COP
        0.3473184,               !-Speed 9 Reference Unit Rated air flow rate
        0.000381695,             !-Speed 9 Reference Unit Rated water flow rate
        TC VS Temp1 Test,        !-Speed 9 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test,  !-Speed 9 Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test,  !-Speed 9 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test,  !-Speed 9 Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test,  !-Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test,  !-Speed 9 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,  !-Speed 9 Waste Heat fraction to power input
        wasteHeat VS Temp1 Test, !-Speed 9 Waste Heat Function of Temperature Curve Name
        6758.0,         !-Speed 10 Reference Unit Gross Rated Total Cooling Capacity
        0.75,                    !-Speed 10 Reference Unit Gross Sensible Heat Ratio
        4.0,                     !-Speed 10 Reference Unit Gross Cooling COP
        0.37752,                 !-Speed 10 Reference Unit Rated air flow rate
        0.000381695,             !-Speed 10 Reference Unit Rated water flow rate
        TC VS Temp1 Test,        !-Speed 10 Total Cooling Capacity Function of Temperature Curve Name
        TC VS AirFrac Test,   !-Speed 10 Total Cooling Capacity Function of Air Flow Fraction Curve Name
        TC VS WaterFrac Test,   !-Speed 10 Total Cooling Capacity Function of Water Flow Fraction Curve Name
        EIR VS Temp1 Test,   !-Speed 10 Energy Input Ratio Function of Temperature Curve Name
        EIR VS AirFrac Test,   !-Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIR VS WaterFrac Test, !-Speed 10 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1, !-Speed 10 Waste Heat fraction to power input
        wasteHeat VS Temp1 Test; !-Speed 10 Waste Heat Function of Temperature Curve Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC, Average, Cooling Coil Electric Power [W]
    HVAC, Average, Cooling Coil Total Cooling Rate [W]
    HVAC, Average, Cooling Coil Sensible Cooling Rate [W]
    HVAC, Average, Cooling Coil Source Side Heat Transfer Rate [W]
    HVAC, Average, Cooling Coil Part Load Ratio []
    HVAC, Average, Cooling Coil Runtime Fraction []
    HVAC, Average, Cooling Coil Air Mass Flow Rate [kg/s]
    HVAC, Average, Cooling Coil Air Inlet Temperature [C]
    HVAC, Average, Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]
    HVAC, Average, Cooling Coil Air Outlet Temperature [C]
    HVAC, Average, Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]
    HVAC, Average, Cooling Coil Source Side Mass Flow Rate [kg/s]
    HVAC, Average, Cooling Coil Source Side Inlet Temperature [C]
    HVAC, Average, Cooling Coil Source Side Outlet Temperature [C]
    HVAC, Average, Cooling Coil Upper Speed Level []
    HVAC, Average, Cooling Coil Neighboring Speed Levels Ratio []
    HVAC, Average, Cooling Coil Recoverable Heat Transfer Rate [W]
    HVAC, Sum, Cooling Coil Electric Energy [J]
    HVAC, Sum, Cooling Coil Total Cooling Energy [J]
    HVAC, Sum, Cooling Coil Sensible Cooling Energy [J]
    HVAC, Sum, Cooling Coil Latent Cooling Energy [J]
    HVAC, Average, Cooling Coil Latent Cooling Rate [W]
    HVAC, Sum, Cooling Coil Source Side Heat Transfer Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Coil Electric Power [W]

This output variable is the average electric consumption rate of the heat pump in Watts over the timestep being reported.

#### Cooling Coil Total Cooling Rate [W]

The output variable is the average total cooling load provide by the heat pump which includes the sensible and latent load in Watts over the timestep being reported.

#### Cooling Coil Sensible Cooling Rate [W]

The output variable is the average sensible cooling load provide by the heat pump in Watts over the timestep being reported.

#### Cooling Coil Source Side Heat Transfer Rate [W]

The output variable is the average heat rejected to the water at the heat pump condenser in Watts over the timestep being reported.

#### Cooling Coil Part Load Ratio []

This output variable is the ratio of the part-load capacity to the steady state capacity of the VSWatertoAirHP coil. For the cycling fan mode, the runtime fraction for the heat pump compressor may be different from the compressor part-load ratio reported here due to the part-load performance of the VSWatertoAirHP coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate.

#### Cooling Coil Runtime Fraction []

This output variable is the function of the part load ratio (PLR, part-load capacity/ steady state capacity). The duty factor or runtime fraction accounts for efficiency losses due to compressor cycling.

#### Cooling Coil Air Mass Flow Rate [kg/s]

The output variable is the average air mass flow rate going through the heat pump over the timestep being reported.

#### Cooling Coil Air Inlet Temperature [C]

The output variable is the average entering air dry-bulb temperature over the timestep being reported.

#### Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average entering air dry humidity ratio over the timestep being reported.

#### Cooling Coil Air Outlet Temperature [C]

The output variable is the average leaving air dry-bulb temperature over the timestep being reported.

#### Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average leaving air dry humidity ratio over the timestep being reported.

#### Cooling Coil Source Side Mass Flow Rate [kg/s]

The output variable is the average water mass flow rate going through the heat pump over the timestep being reported.

#### Cooling Coil Source Side Inlet Temperature [C]

The output variable is the average entering water temperature over the timestep being reported.

#### Cooling Coil Source Side Outlet Temperature [C]

The output variable is the average leaving water temperature over the timestep being reported.

#### Cooling Coil Upper Speed Level []

The output variable is the average upper speed level, for interpolating performances between two neighboring speed levels.

#### Cooling Coil Neighboring Speed Levels Ratio []

The output variable is the average speed ratio, for interpolating performances between two neighboring speed levels.

#### Cooling Coil Recoverable Heat Transfer Rate [W]

This output variable is the average recoverable waste heat rate of the heat pump in Watts over the timestep being reported.

#### Cooling Coil Electric Energy [J]

The output variable is the electric consumption of the heat pump in Joules over the timestep being reported.

#### Cooling Coil Total Cooling Energy [J]

The output variable is the total cooling output of the coil in Joules over the timestep being reported.

#### Cooling Coil Sensible Cooling Energy [J]

The output variable is the total sensible cooling output of the coil in Joules over the timestep being reported

#### Cooling Coil Latent Cooling Energy [J]

#### Cooling Coil Latent Cooling Rate [W]

These output variables are the total latent cooling output of the coil in Joules or Watts over the timestep being reported.

#### Cooling Coil Source Side Heat Transfer Energy [J]

The output variable is the total source side heat transfer of the coil in Joules over the timestep being reported.

## Coil:Heating:WaterToAirHeatPump:ParameterEstimation

The Water to Air heat pump heating coil is a deterministic model that requires parameters to describe the operating conditions of the heat pump's components. The parameters are generated from the manufacturer catalog data using multi-variable optimization method. However, the parameters required for WatertoAirHP heating coil is similar to WatertoAirHP cooling except (source side outside heat transfer coefficient) parameter is not required for heating. In addition, the heating coil model can be used for 3 type of compressors which are **reciprocating**, **rotary** and **scroll**.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the coil. Any reference to this coil by another object (e.g., [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair)) will use this name.

#### Field: Compressor Type

Type of compressor mode used for the heat pump. Choices available are reciprocating, rotary and scroll compressor. Note that the parameters vary for different compressor.

#### Field: Refrigerant Type

This alpha field contains the type of refrigerant used by the heat pump.

#### Field: Design Source Side Flow Rate

This numeric field defines the water flow rate though the coil in m3/sec

#### Field: Gross Rated Heating Capacity

This numeric field defines the gross heating capacity for the WatertoAirHP heating coil at the rated condition in Watts. The gross heating capacity should Not inlcude the ffect of supply air fan heat.

#### Field: High Pressure Cutoff

This numeric field defines the compressor's maximum allowable pressure in Pascal (N/m2)

#### Field: Low Pressure Cutoff 

This numeric field defines the compressor's minimum allowable pressure in Pascal (N/m2)

#### Field: Water Inlet Node Name

This alpha field contains the heating coil source side inlet node name.

#### Field: Water Outlet Node Name

This alpha field contains the heating coil source side outlet node name.

#### Field: Air Inlet Node Name

This alpha field contains the heating coil air inlet node name.

#### Field: Air Outlet Node Name

This alpha field contains the heating coil air outlet node name.

#### Heating Coil Parameters

Depending on the type of compressor specified, the type of parameters and values also differs. An Excel Spreadsheet is in the final stage of development that is capable of estimating the parameters based on the manufacturer data. The general parameters are listed first followed by specific parameters required by the respective compressor model. Lastly, parameters are listed based on the type of source side fluid used.

#### Field: Load Side Total Heat Transfer Coefficient

This numeric field defines the estimated parameter **load side total heat transfer coefficient** in W/K. This field was previously known as Parameter 1.

#### Field: Superheat Temperature at the Evaporator Outlet

This numeric field defines the estimated parameter **superheat temperature** at the evaporator outlet in ˚C. This field was previously known as Parameter 2.

#### Field: Compressor Power Losses

This numeric field defines the estimated parameter **compressor power losses**, which accounts for the loss of work due to mechanical and electrical losses in the compressor in Watts. This field was previously known as Parameter 3.

#### Field: Compressor Efficiency

This numeric field defines the estimated parameter of the **compressor's efficiency**. The compressor efficiency is formulated as the equation below:

![](media/image370.png)\


This field was previously know as Parameter 4.

#### Field: Compressor Piston Displacement

This numeric field defines the estimated parameter **piston displacement of the compressor** in m3/s. This field was part of what was previously known as Parameter 5. It should be used when the Compressor type is either Reciprocating and Rotary. The field should be left blank when Compressor type is Scroll.

#### Field: Compressor Suction/Discharge Pressure Drop

This numeric field defines the estimated parameter **pressure drop at the compressor suction** and discharge in Pascals (N/m2). This field was part of what was previously known as Parameter 6. It should be used when the Compressor type is either Reciprocating and Rotary. The field should be left blank when Compressor type is Scroll.

#### Field: Compressor Clearance Factor

This numeric field defines the estimated parameter **clearance factor of the compressor**. This parameter is dimensionless. This field was part of what was previously known as Parameter 7. It should only be used when the Compressor type is Reciprocating. The field should be left blank when Compressor type is Scroll or Rotary.

#### Field: Refrigerant Volume Flow Rate 

This numeric field defines the **refrigerant volume flow rate** at the beginning of the compression [m3/s]. This field was part of what was previously known as Parameter 6. It should only be used when the Compressor type is Scroll. The field should be left blank when Compressor type is Reciprocating or Rotary.

#### Field: Volume Ratio

This numeric field defines the **built-in-volume ratio**. This field was part of what was previously known as Parameter 6. It should only be used when the Compressor type is Scroll. The field should be left blank when Compressor type is Reciprocating or Rotary.

#### Field: Leak Rate Coefficient

This numeric field defines the coefficient for the relationship between **pressure ratio and leakage rate**. This field was part of what was previously known as Parameter 7. It should only be used when the Compressor type is Scroll. The field should be left blank when Compressor type is Reciprocating or Rotary.

#### Field: Source Side Heat Transfer Coefficient

This numeric field defines the estimated parameter **source side heat transfer coefficient** in W/K. This field was part of what was previously known as Parameter 8. It should only be used when the Source Side Fluid Name is Water.

#### Field: Source Side Heat Transfer Resistance1

This numeric field defines the estimated parameter **source side heat transfer resistance 1**. Unit is dimensionless. This field was part of what was previously known as Parameter 8. It should only be used when the Source Side Fluid Name is an antifreeze.

#### Field: Source Side Heat Transfer Resistance2

This numeric field defines the estimated **parameter source side heat transfer resistance 2**. Unit is W/K. This field was previously known as Parameter 9. It should only be used when the Source Side Fluid Name is an antifreeze.

Following is an example input for Coil:WatertoAirHP:ParameterEstimation:Heating

~~~~~~~~~~~~~~~~~~~~

      Coil:Heating:WaterToAirHeatPump:ParameterEstimation,
        Heat Pump HEATING Mode AHU1,  !- Name
        Scroll,                  !- Compressor Type
        R22,                     !- Refrigerant Type
        0.0015,                  !- Design Source Side Flow Rate {m3/s}
        38000,                   !- Gross Rated Heating Capacity {W}
        3000000,                 !- High Pressure Cutoff
        0,                       !- Low Pressure Cutoff {Pa}
        AHU1 Water to Air Heat Pump Source Side2 Inlet Node,  !- Water Inlet Node Name
        AHU1 Water to Air Heat Pump Source Side2 Outlet Node,  !- Water Outlet Node Name
        Cooling Coil Air Outlet Node AHU1,  !- Air Inlet Node Name
        Heating Coil Air Outlet Node AHU1,  !- Air Outlet Node Name
        5.79505E+03,             !- Load Side Total Heat Transfer Coefficient {W/K}
        1.56696E+00,             !- Superheat Temperature at the Evaporator Outlet {C}
        3.91338E+03,             !- Compressor Power Losses {W}
        1.26356E+00,             !- Compressor Efficiency
        ,                        !- Compressor Piston Displacement {m3/s}
        ,                        !- Compressor Suction/Discharge Pressure Drop {Pa}
        ,                        !- Compressor Clearance Factor {dimensionless}
        6.99573E-03,             !- Refrigerant Volume Flow Rate {m3/s}
        4.96167E+00,             !- Volume Ratio {dimensionless}
        2.07750E-06,             !- Leak Rate Coefficient {dimensionless}
        4.00322E+03,             !- Source Side Heat Transfer Coefficient {W/K}
        ,                        !- Source Side Heat Transfer Resistance1 {dimensionless}
        ;                        !- Source Side Heat Transfer Resistance2 {W/K}
~~~~~~~~~~~~~~~~~~~~

## Coil:Heating:WaterToAirHeatPump:EquationFit

The Water to Air Heating Equation Fit Coil is a simple curve-fit model that requires coefficients generated from the heat pump catalog data. This is an equation-fit model that resembles a black box with no usage of heat transfer equations. The performance of the heat pump is modeled using curves fitted from the catalog data.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the coil. Any reference to this coil by another object (e.g., [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair)) will use this name.

#### Field: Water Inlet Node Name

This alpha field contains the heating coil source side inlet node name.

#### Field: Water Outlet Node Name

This alpha field contains the heating coil source side outlet node name.

#### Field: Air Inlet Node Name

This alpha field contains the heating coil air inlet node name.

#### Field: Air Outlet Node Name

This alpha field contains the heating coil air outlet node name.

#### Field: Rated Air Flow Rate

This numeric field contains the rated volumetric air flow rate on the load side of the heat pump in m3/s. This field is autosizable.

#### Field: Rated Water Flow Rate

This numeric field contains the rated volumetric water flow rate on the source side of the heat pump in m3/s. This field is autosizable.

#### Field: Gross Rated Heating Capacity 

This numeric field contains the gross heating capacity of the heat pump at the rated condition in W. The gross heating capacity should not account for the effect of supply air fan heat. This field is autosizable.

#### Field: Gross Rated Heating COP

This numeric field contains the gross rated heating coefficient of performance of the heat pump. The gross rated COP of the heat pump should not account for the effect of supply air fan heat.

#### Field: Heating Capacity Coefficient 1-5 

These numeric fields contain the first to fifth coefficient for the heat pump capacity.

#### Field: Heating Power Consumption Coefficient 1-5

These numeric fields contain the first to fifth coefficient for the heat pump power consumption.

### Outputs

[Coil:Heating:WaterToAirHeatPump:ParameterEstimation](#coilheatingwatertoairheatpumpparameterestimation) and [Coil:Heating:WaterToAirHeatPump:EquationFit](#coilheatingwatertoairheatpumpequationfit) have the same output variables listed as follows;

~~~~~~~~~~~~~~~~~~~~

    HVAC, Average, Heating Coil Electric Power [W]
    HVAC, Average, Heating Coil Heating Rate [W]
    HVAC, Average, Heating Coil Sensible Heating Rate [W]
    HVAC, Average, Heating Coil Source Side Heat Transfer Rate [W]
    HVAC, Average, Heating Coil Part Load Ratio
    HVAC, Average, Heating Coil Runtime Fraction []

    HVAC, Average, Heating Coil Air Mass Flow Rate [kg/s]
    HVAC, Average, Heating Coil Air Inlet Temperature [C]
    HVAC, Average, Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]
    HVAC, Average, Heating Coil Air Outlet Temperature [C]
    HVAC, Average, Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]

    HVAC, Average, Heating Coil Source Side Mass Flow Rate [kg/s]
    HVAC, Average, Heating Coil Source Side Inlet Temperature [C]
    HVAC, Average, Heating Coil Source Side Outlet Temperature [C]

    HVAC, Sum, Heating Coil Electric Energy [J]
    HVAC, Sum, Heating Coil Heating Energy [J]
    HVAC, Sum, Heating Coil Source Side Heat Transfer Energy [J]

~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Electric Power [W]

This output variable is the average electric consumption rate of the heat pump in Watts over the timestep being reported.

#### Heating Coil Heating Rate [W]

The output variable is the average total heating capacity provide by the heat pump which includes the sensible and latent capacity in Watts over the timestep being reported.

#### Heating Coil Sensible Heating Rate [W]

The output variable is the average sensible heating capacity provide by the heat pump in Watts over the timestep being reported. For heating mode, the sensible capacity is equal to the total capacity.

#### Heating Coil Source Side Heat Transfer Rate [W]

The output variable is the average heat absorbed at the heat pump evaporator in Watts over the timestep being reported.

#### Heating Coil Part Load Ratio []

This output variable is the ratio of the part-load capacity to the steady state capacity of the WatertoAirHP coil. For the cycling fan mode, the runtime fraction for the heat pump compressor may be different from the compressor part-load ratio reported here due to the part-load performance of the WatertoAirHP coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate.

#### Heating Coil Runtime Fraction []

This output variable is the function of the part load ratio (PLR, part-load capacity/ steady state capacity). The duty factor or part load fraction accounts for efficiency losses due to compressor cycling.

#### Heating Coil Air Mass Flow Rate [kg/s]

The output variable is the average air mass flow rate going through the heat pump over the timestep being reported.

#### Heating Coil Air Inlet Temperature [C]

The output variable is the average entering air dry-bulb temperature over the timestep being reported.

#### Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average entering air dry humidity ratio over the timestep being reported.

#### Heating Coil Air Outlet Temperature [C]

The output variable is the average leaving air dry-bulb temperature over the timestep being reported.

#### Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average leaving air dry humidity ratio over the timestep being reported.

#### Heating Coil Source Side Mass Flow Rate [kg/s]

The output variable is the average water mass flow rate going through the heat pump over the timestep being reported.

#### Heating Coil Source Side Inlet Temperature [C]

The output variable is the average entering water temperature over the timestep being reported.

#### Heating Coil Source Side Outlet Temperature [C]

The output variable is the average leaving water temperature over the timestep being reported.

#### Heating Coil Electric Energy [J]

The output variable is the total electric consumption of the heat pump in Joules over the timestep being reported. This output is also added to a meter with Resource Type = Electricity, End Use Key = Heating, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Heating Energy [J]

The output variable is the total cooling output of the coil in Joules over the timestep being reported. Resource Type = EnergyTransfer, End Use Key = HeatingCoils, Group Key = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heating Coil Source Side Heat Transfer Energy [J]

The output variable is the total source side heat transfer of the coil in Joules over the timestep being reported.

## Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit

The Variable-Speed Water-to-Air Heating Equation Fit Coil is a collection of performance curves that represent the heating coil at various speed levels. The performance curves should be generated from the heat pump Reference Unit catalog data. This is an equation-fit model that resembles a black box with no usage of heat transfer equations. The number of speed levels can range from 2 to 10. The heating coil has four node connections, i.e. two air sides and two water sides. The user needs to specify a nominal speed level, at which the rated capacity, rated volumetric air and water flow rates are sized. The rated capacity, rated volumetric flow rates represent the real situation in the air and water loops, and are used to determine the flow rates at various speed levels in the parent objects, e.g. [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair) and [ZoneHVAC:WaterToAirHeatPump](#zonehvacwatertoairheatpump). It shall be mentioned that the performance correction curves, i.e. the temperature and flow fraction correction curves, should be normalized to the capacity and flow rates at each individual speed and at the rated operating conditions, similar to the performance curves used in the DX coils. On the other hand, the performance values at individual speed levels, e.g. capacities, COPs and flow rates, should be given regarding a specific unit from the Reference Unit catalog data. In the following content, the statement started with "Reference Unit" means the actual Reference Unit catalog data. The rated conditions for obtaining the capacities and COPs  are at indoor dry-bulb temperature of 21.1 ˚C (70 ˚F) and the source side entering water temperature of 21.1 ˚C (70 ˚F). Some equations are provided below to help explain the function of the various performance curves and data fields. For a detailed description of the algorithm and how the curves are used in the calculations, please see the Engineering Reference.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the variable speed heating coil. Any reference to this coil by another object (e.g., [AirLoopHVAC:UnitaryHeatPump:WaterToAir](#airloophvacunitaryheatpumpwatertoair)) will use this name.

#### Field: Water Inlet Node Name

This alpha field contains the heating coil source side inlet node name.

#### Field: Water Outlet Node Name

This alpha field contains the heating coil source side outlet node name.

#### Field: Air Inlet Node Name

This alpha field contains the heating coil load side inlet node name.

#### Field: Air Outlet Node Name

This alpha field contains the heating coil load side outlet node name.

#### Field: Number of Speeds

This numeric field contains the maximum number of speed levels that the module uses. The number of speeds, for which the user input the performance data and curves, has to be equal to or higher than the maximum number. The performance inputs at higher speeds are ignored.

#### Field: Nominal Speed Level

This numeric field defines the nominal speed level, at which the rated capacity, rated air and water volumetric flow rates are correlated.

#### Field: Gross Rated Heating Capacity at Selected Nominal Speed Level

This numeric field contains the gross rated capacity at the nominal speed level.  This field is autosizable. The gross rated heating capacity is used to determine a capacity scaling factor, as compared to the Reference Unit capacity at the nominal speed level.

![](media/image371.png)\


And then, this scaling factor is used to determine capacities at rated conditions for other speed levels, as below,

![](media/image372.png)\


#### Field: Rated Volumetric Air Flow Rate

This numeric field contains the rated volumetric air flow rate on the load side of the heat pump corresponding to the nominal speed level. This field is autosizable. The value is used to determine an internal scaling factor, and calculate the air flow rates in the parent objects.  It is recommended that the ratio of the rated volumetric air flow rate to the rated capacity is the same as the unit performance from the Reference Unit data.

![](media/image373.png)\


And the volumetric air flow rates in the parent objects are calculated as below,

![](media/image374.png)\


#### Field: Rated Volumetric Water Flow Rate

This numeric field contains the rated volumetric water flow rate on the source side of the heat pump at the nominal speed level. This field is autosizable. The value is used to determine an internal scaling factor, and calculate the water flow rates at other speed levels.  It is recommended that the ratio of the rated volumetric water flow rate to the rated capacity is the same as the unit performance from the Reference Unit data.

![](media/image375.png)\


And the required volumetric water flow rates in the water loop are calculated as below,

![](media/image376.png)\


#### Field: Part Load Fraction Correlation Curve Name

This alpha field defines the name of a quadratic or cubic performance curve (Ref: Performance Curves) that parameterizes the variation of electrical power input to the unit as a function of the part load ratio (PLR, heating load/steady-state heating capacity for Speed 1), in the case that the unit operates under the lowest speed, i.e. on/off. The product of the rated EIR and EIR modifier curves is divided by the output of this curve to give the "effective" EIR for a given simulation timestep for Speed 1. The part load fraction (PLF) correlation accounts for efficiency losses due to compressor cycling. The part load fraction correlation should be normalized to a value of 1.0 when the part load ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation timestep). For PLR values between 0 and 1 (0 <= PLR < 1), the following rules apply:

PLF >= 0.7 and PLF >= PLR

If PLF < 0.7, the program resets the PLF value to 0.7, and the simulation proceeds. The runtime fraction of the coil is defined as PLR/PLF. If PLF < PLR, the runtime fraction of the coil is limited to 1.0. A typical part load fraction correlation would be:

![](media/image377.png)\


If the user wishes to model no efficiency degradation due to compressor cycling, the part load fraction correlation should be defined as follows:

![](media/image378.png)\


#### Field Group: Rated Specification, Performance Curves, and Waste Heat Data

The performance for each heating speed must be specified as shown below. They should be directly given from the Reference Unit data. All inputs for Speed 1 are required, followed by the optional inputs for other speeds.

#### Field: Speed <x> Reference Unit Gross Rated Heating Capacity

This numeric field defines the total, full load gross heating capacity in watts of the water-to-air heating coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0. Capacity should not account for the effect of supply air fan heat.

#### Field: Speed <x> Reference Unit Gross Rated Heating COP

This numeric field defines the coefficient of performance (COP= gross heating capacity in watts divided by electrical power input in watts) of the heating coil unit at rated conditions for Speed <x> operation. The value entered here must be greater than 0. The input power includes  power for the compressor(s) and accessories, but does not include the power consumption of the indoor supply air fan and water pump. The gross COP should not account for the supply air fan.

#### Field: Speed <x> Reference Unit Rated Air Flow Rate

This numeric field defines the volume air flow rate, in m^3^ per second, across the heating coil at rated conditions for Speed <x> operation. The value entered here should be directly from the Reference Unit data, corresponding to the given gross rated heating capacity and gross rated heating COP at the speed, as above.

#### Field: Speed <x> Reference Unit Rated Water Flow Rate

This numeric field defines the volume water flow rate, in m^3^ per second, flowing at the source side of the heating coil at rated conditions for Speed <x> operation. The value entered here should be directly from the Reference Unit data, corresponding to the given gross rated heating capacity and gross rated heating COP at the speed, as above.

#### Field: Speed <x> Heating Capacity Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the total heating capacity as a function of the indoor dry-bulb and source side entering water temperature, from the Reference Unit. The output of this curve is multiplied by the gross rated heating capacity at the speed to give the gross total heating capacity at specific temperature operating conditions (i.e., at an indoor air dry-bulb temperature or entering water temperature different from the rating point temperature). It should be noted that the curve is normalized to the heating capacity at Speed<x> from the Reference Unit data, and have the value of 1.0 at the rating point.

#### Field: Speed <x> Heating Capacity Function of Air Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the gross total heating capacity as a function of the ratio of actual air flow rate across the heating coil to the design air flow rate (i.e., fraction of full load flow), at Speed <x> from the Reference Unit data. The curve is normalized to have the value of 1.0 when the actual air flow rate equals the design air flow rate at Speed <x>.

#### Field: Speed <x> Heating Capacity Function of Water Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of total heating capacity as a function of the ratio of actual water flow rate across the heating coil to the design water flow rate (i.e., fraction of full load flow), at Speed <x> from the Reference Unit data. The curve is normalized to have the value of 1.0 when the actual water flow rate equals the design air flow rate at Speed <x>.

The actual total heating capacity at Speed <x>, considering variations in temperatures, air and water flow rates is calculated as below:

![](media/image379.png)\


#### Field: Speed <x> Energy Input Ratio Function of Temperature Curve Name

This alpha field defines the name of a bi-quadratic, quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the both the indoor air dry-bulb and entering water temperatures The EIR is the inverse of the COP. The output of this curve is multiplied by the rated EIR (inverse of rated COP at Speed <x> from the Reference Unit data) to give the EIR at specific temperature operating conditions (i.e., at an indoor air dry bulb temperature or entering water temperature different from the rating point temperature). The curve is normalized to have the value of 1.0 at the rating point.

#### Field: Speed <x> Energy Input Ratio Function of Air Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the heating coil to the design air flow rate (i.e., fraction of full load flow, at Speed <x> from the Reference Unit data). The EIR is the inverse of the COP. This curve is normalized to a value of 1.0 when the actual air flow rate equals the rated air flow rate.

#### Field: Speed <x> Energy Input Ratio Function of Water Flow Fraction Curve Name

This alpha field defines the name of a quadratic or cubic performance curve for Speed <x> (ref: Performance Curves) that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual water flow rate across the heating coil to the rated water flow rate (i.e., fraction of full load flow, at Speed <x> from the Reference Unit data). This curve is normalized to a value of 1.0 when the actual air flow rate equals the design air flow rate.

The actual EIR at Speed <x>, considering variations in temperatures, air and water flow rates is calculated as below:

![](media/image380.png)\


And, the actual power consumption is calculated:

![](media/image381.png)\


#### Field: Speed <x> Rated Waste Heat Fraction of Power Input

The fraction of heat input to heating that is available as recoverable waste heat at full load and rated conditions for Speed <x> operation. The part of heat is not delivered to the indoor side.

#### Field: Speed <x> Waste Heat Function of Temperature Curve Name

The name of a bi-quadratic performance curve (ref: Performance Curves) that parameterizes the variation of the waste heat recovery as a function of indoor dry-bulb temperature and the entering water temperature for Speed <x> from the Reference Unit data. The output of this curve is multiplied by the rated recoverable waste heat at specific temperature operating conditions (i.e., at temperatures different from the rating point). The curve is normalized to a value of 1.0 at the rating point.

The actual recoverable waste heat at Speed <x> is calculated as below:

![](media/image382.png)\


An example of this statement in an IDF is:

~~~~~~~~~~~~~~~~~~~~

    Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit,
        Sys 1 Heat Pump Heating Mode,  !- Name
        Sys 1 Water to Air Heat Pump Source Side2 Inlet Node,  !- Water Inlet Node Name
        Sys 1 Water to Air Heat Pump Source Side2 Outlet Node,  !- Water Outlet Node Name
        Sys 1 Heating Coil Air Inlet Node,  !- Air Inlet Node Name
        Sys 1 SuppHeating Coil Air Inlet Node,  !- Air Outlet Node Name
        10.0,!- Number of Speeds
        10.0,!- Nominal Speed Level
        Autosize,                !- Rated Heating Capacity {W}
        Autosize,                     !- Rated Air Flow Rate {m3/s}
        Autosize,                 !- Rated Water Flow Rate {m3/s}
        VS Energy Part Load Fraction 1,    !- Energy part load fraction curve
        1838.7,             !- Speed 1 Reference Unit Gross Rated Heating Capacity
        5.0,                             !- Speed 1 Reference Unit Gross Rated Heating COP
        0.1661088,                             !- Speed 1 Reference Unit Rated Air Flow Rate
        0.000381695,                             !- Speed 1 Reference Unit Rated Water Flow Rate
        Heating VS Temp1 Test,    !- Speed 1 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test,  !- Speed 1 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 1 Heating Capacity Function of Water Flow Fraction Curve Name
        EIRH VS Temp1 Test,       !- Speed 1 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test,     !- Speed 1 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test,   !- Speed 1 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,                      !- Speed 1 Waste Heat Fraction to Power Input
        Heating wasteHeat VS Temp1 Test,   !- Speed 1 Waste Heat Function of Temperature Curve Name
        2295.5,           !- Speed 2 Reference Unit Gross Rated Heating Capacity
        5.0,                      !- Speed 2 Reference Unit Gross Rated Heating COP
        0.179322,                 !- Speed 2 Reference Unit Rated air flow rate
        0.000381695,              !- Speed 2 Reference Unit Rated water flow rate
        Heating VS Temp1 Test,    !- Speed 2 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test,  !- Speed 2 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 2 Heating Capacity Function of Water Flow Fraction Curve Name
        EIRH VS Temp1 Test,    !- Speed 2 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test,    !- Speed 2 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test,   !- Speed 2 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,    !- Speed 2 Waste Heat Fraction to Power Input
        Heating wasteHeat VS Temp1 Test,  !- Speed 2 Waste Heat Function of Temperature Curve Name
        2751.3,           !- Speed 3 Reference Unit Gross Rated Heating Capacity
        5.0,                       !- Speed 3 Reference Unit Gross COP
        0.1925352,                 !- Speed 3 Reference Unit Rated Air Flow Rate
        0.000381695,               !- Speed 3 Reference Unit Rated Water Flow Rate
        Heating VS Temp1 Test,     !- Speed 3 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test,   !- Speed 3 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 3 Heating Capacity Function of Water Flow Fraction Curve Name
        EIRH VS Temp1 Test,       !- Speed 3 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test,     !- Speed 3 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test,   !- Speed 3 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,    !- Speed 3 Waste Heat fraction to power input
        Heating wasteHeat VS Temp1 Test, !- Speed 3 Waste Heat Function of Temperature Curve Name
        3659.6,            !- Speed 4 Reference Unit Gross Rated Heating Capacity
        5.0,                        !- Speed 4 Reference Unit Gross Rated Heating COP
        0.2189616,                  !- Speed 4 Reference Unit Rated air flow rate
        0.000381695,                !- Speed 4 Reference Unit Rated water flow rate
        Heating VS Temp1 Test,      !- Speed 4 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test,    !- Speed 4 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 4 Heating Capacity Function of Water Flow Fraction Curve Name
        EIRH VS Temp1 Test,    !- Speed 4 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test,    !- Speed 4 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test,   !- Speed 4 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,    !- Speed 4 Waste Heat fraction to power input
        Heating wasteHeat VS Temp1 Test, !- Speed 4 Waste Heat Function of Temperature Curve Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

        4563.7,           !- Speed 5 Reference Unit Gross Rated Heating Capacity
        5.0,                       !- Speed 5 Reference Unit Gross Rated Heating COP
        0.245388,                  !- Speed 5 Reference Unit Rated air flow rate
        0.000381695,               !- Speed 5 Reference Unit Rated water flow rate
        Heating VS Temp1 Test,     !- Speed 5 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test,   !- Speed 5 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 5 Heating Capacity Function of Water Flow Fraction Curve Name
        EIRH VS Temp1 Test,    !- Speed 5 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test,    !- Speed 5 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test,   !- Speed 5 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,    !- Speed 5 Waste Heat fraction to power input
        Heating wasteHeat VS Temp1 Test,  !- Speed 5 Waste Heat Function of Temperature Curve Name
        5463.3,                !- Speed 6 Reference Unit Gross Rated Heating Capacity
        5.0,                              !- Speed 6 Reference Unit Gross Rated Heating COP
        0.2718144,                        !- Speed 6 Reference Unit Rated air flow rate
        0.000381695,                      !- Speed 6 Reference Unit Rated water flow rate
        Heating VS Temp1 Test,            !- Speed 6 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test, !- Speed 6 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 6 Heating Capacity Function of Water Flow Fraction Curve Name
        EIRH VS Temp1 Test,       !- Speed 6 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test,    !- Speed 6 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test,   !- Speed 6 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,    !- Speed 6 Waste Heat fraction to power input
        Heating wasteHeat VS Temp1 Test, !- Speed 6 Waste Heat Function of Temperature Curve Name
        6358.4,          !- Speed 7 Reference Unit Gross Rated Heating Capacity
        5.0,                      !- Speed 7 Reference Unit Gross Rated Heating COP
        0.2982408,                !- Speed 7 Reference Unit Rated air flow rate
        0.000381695,              !- Speed 7 Reference Unit Rated water flow rate
        Heating VS Temp1 Test,    !- Speed 7 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test,  !- Speed 7 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 7 Heating Capacity Function of Water Flow Fraction Curve Name
        EIRH VS Temp1 Test,       !- Speed 7 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test,     !- Speed 7 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test,   !- Speed 7 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,    !- Speed 7 Waste Heat fraction to power input
        Heating wasteHeat VS Temp1 Test,!- Speed 7 Waste Heat Function of Temperature Curve Name
        7248.5,          !- Speed 8 Reference Unit Gross Rated Heating Capacity
        5.0,                      !- Speed 8 Reference Unit Gross Rated Heating COP
        0.3246672,                !- Speed 8 Reference Unit Rated air flow rate
        0.000381695,              !- Speed 8 Reference Unit Rated water flow rate
        Heating VS Temp1 Test,    !- Speed 8 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test,  !- Speed 8 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 8 Heating Capacity Function of Water Flow Fraction Curve Name
        EIRH VS Temp1 Test,    !- Speed 8 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test,    !- Speed 8 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test,   !- Speed 8 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,    !- Speed 8 Waste Heat fraction to power input
        Heating wasteHeat VS Temp1 Test,  !- Speed 8 Waste Heat Function of Temperature Curve Name
        8133.6,          !- Speed 9 Reference Unit Gross Heating Capacity
        5.0,                      !- Speed 9 Reference Unit Gross Rated Heating COP
        0.3510936,                !- Speed 9 Reference Unit Rated air flow rate
        0.000381695,              !- Speed 9 Reference Unit Rated water flow rate
        Heating VS Temp1 Test,    !- Speed 9 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test,  !- Speed 9 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 9 Heating Capacity Function of Water Flow Fraction Curve Name
        EIRH VS Temp1 Test,    !- Speed 9 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test,    !- Speed 9 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test,   !- Speed 9 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1,    !- Speed 9 Waste Heat fraction to power input
        Heating wasteHeat VS Temp1 Test, !- Speed 9 Waste Heat Function of Temperature Curve Name
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

        9013.2,       !- Speed 10 Reference Unit Gross Rated Heating Capacity
        5.0,                   !- Speed 10 Reference Unit Gross Rated Heating COP
        0.37752,               !- Speed 10 Reference Unit Rated air flow rate
        0.000381695,           !- Speed 10 Reference Unit Rated water flow rate
        Heating VS Temp1 Test, !- Speed 10 Heating Capacity Function of Temperature Curve Name
        Heating VS AirFrac Test, !- Speed 10 Heating Capacity Function of Air Flow Fraction Curve Name
        Heating VS WaterFrac Test,!- Speed 10 Heating Capacity Function of Water Flow Fraction Curve
        EIRH VS Temp1 Test, !- Speed 10 Energy Input Ratio Function of Temperature Curve Name
        EIRH VS AirFrac Test, !- Speed 10 Energy Input Ratio Function of Air Flow Fraction Curve Name
        EIRH VS WaterFrac Test, !- Speed 10 Energy Input Ratio Function of Water Flow Fraction Curve Name
        0.1, !- Speed 10 Waste Heat fraction to power input
        Heating wasteHeat VS Temp1 Test; !- Speed 10 Waste Heat Function of Temperature Curve Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC, Average, Heating Coil Electric Power [W]
    HVAC, Average, Heating Coil Heating Rate [W]
    HVAC, Average, Heating Coil Sensible Heating Rate [W]
    HVAC, Average, Heating Coil Source Side Heat Transfer Rate [W]
    HVAC, Average, Heating Coil Part Load Ratio
    HVAC, Average, Heating Coil Runtime Fraction []
    HVAC, Average, Heating Coil Air Mass Flow Rate [kg/s]
    HVAC, Average, Heating Coil Air Inlet Temperature [C]
    HVAC, Average, Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]
    HVAC, Average, Heating Coil Air Outlet Temperature [C]
    HVAC, Average, Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]
    HVAC, Average, Heating Coil Source Side Mass Flow Rate [kg/s]
    HVAC, Average, Heating Coil Source Side Inlet Temperature [C]
    HVAC, Average, Heating Coil Source Side Outlet Temperature [C]
    HVAC, Average, Heating Coil Upper Speed Level []
    HVAC, Average, Heating Coil Neighboring Speed Levels Ratio []
    HVAC, Average, Heating Coil Recoverable Heat Transfer Rate [W]
    HVAC, Sum, Heating Coil Electric Energy [J]
    HVAC, Sum, Heating Coil Heating Energy [J]
    HVAC, Sum, Heating Coil Source Side Heat Transfer Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Heating Coil Electric Power [W]

This output variable is the average electric consumption rate of the heat pump in Watts over the timestep being reported.

#### Heating Coil Heating Rate [W]

The output variable is the average total heating capacity provide by the heat pump in Watts over the timestep being reported.

#### Heating Coil Sensible Heating Rate [W]

The output variable is the average sensible heating capacity provide by the heat pump in Watts over the timestep being reported. For heating mode, the sensible capacity is equal to the total capacity.

#### Heating Coil Source Side Heat Transfer Rate [W]

The output variable is the average heat absorbed at the heat pump evaporator in Watts over the timestep being reported.

#### Heating Coil Part Load Ratio []

This output variable is the ratio of the part-load capacity to the steady state capacity of the VSWatertoAirHP coil. For the cycling fan mode, the runtime fraction for the heat pump compressor may be different from the compressor part-load ratio reported here due to the part-load performance of the VSWatertoAirHP coil (delay at start-up to reach steady-state output). In general, runtime fractions are reported by individual components where appropriate.

#### Heating Coil Runtime Fraction []

This output variable is the function of the part load ratio (PLR, part-load capacity/ steady state capacity). The duty factor or part load fraction accounts for efficiency losses due to compressor cycling.

#### Heating Coil Air Mass Flow Rate [kg/s]

The output variable is the average air mass flow rate going through the heat pump over the timestep being reported.

#### Heating Coil Air Inlet Temperature [C]

The output variable is the average entering air dry-bulb temperature over the timestep being reported.

#### Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average entering air dry humidity ratio over the timestep being reported.

#### Heating Coil Air Outlet Temperature [C]

The output variable is the average leaving air dry-bulb temperature over the timestep being reported.

#### Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]

The output variable is the average leaving air dry humidity ratio over the timestep being reported.

#### Heating Coil Source Side Mass Flow Rate [kg/s]

The output variable is the average water mass flow rate going through the heat pump over the timestep being reported.

#### Heating Coil Source Side Inlet Temperature [C]

The output variable is the average entering water temperature over the timestep being reported.

#### Heating Coil Source Side Outlet Temperature [C]

The output variable is the average leaving water temperature over the timestep being reported.

#### Heating Coil Upper Speed Level []

The output variable is the average upper speed level, for interpolating performances between two neighboring speed levels.

#### Heating Coil Neighboring Speed Levels Ratio []

The output variable is the average speed ratio, for interpolating performances between two neighboring speed levels.

#### Heating Coil Recoverable Heat Transfer Rate [W]

This output variable is the average recoverable waste heat rate of the heat pump in Watts over the timestep being reported.

#### Heating Coil Electric Energy [J]

The output variable is the total electric consumption of the heat pump in Joules over the timestep being reported.

#### Heating Coil Heating Energy [J]

The output variable is the total cooling output of the coil in Joules over the timestep being reported.

#### Heating Coil Source Side Heat Transfer Energy [J]

The output variable is the total source side heat transfer of the coil in Joules over the timestep being reported.

## Coil:Cooling:DX:SingleSpeed:ThermalStorage

This object models a special direct expansion (DX) cooling coil that includes methods of storing thermal energy within the cooling coil package.  This packaged thermal storage cooling coil model includes all of the equipment needed to charge and discharge and the Thermal Energy Storage (TES) tank itself.  This coil object is used with [CoilSystem:Cooling:DX](#coilsystemcoolingdx) object and a constant volume supply fan, but all other subcomponents are contained within the package.

Some types of devices may have a process air cooling section that actually has two coils, one an evaporator and the second some other kind of coil that cools by discharging the TES tank.  There may also be two separate heat engines inside the package with two compressors.  The model wraps these complexities inside of the model which considers just one process air cooling section that combines both types of coils and one heat rejection condenser section that rejects heat from both engines.  The operating modes that are likely to have two heat engines offer dual sets of rating points and performance curves to model the energy and capacity implications separately.

The model offers various modes of operation that can be controlled over time using either a schedule or EMS actuators.  The user can choose which of the five operating modes are present in the device.  Each of the modes has its own separate set of performance characteristics and curves and the model can change abruptly from one mode to the next.  For example when using the schedule control, the TES coil can be put into Charge Only Mode for some number of hours at night, operated in Cooling Only Mode during the mild morning hours, and run in Discharge Only Mode during the hottest part of the day.  Custom control routines can be programmed using EMS to obtain a dynamic, supervisory control algorithm.

Off Mode.  This mode is always present.  Although the coil will not do any cooling, the state of the TES tank is still modeled since it can continue to exchange heat with the surrounding ambient air.

Cooling Only Mode.  This mode is where the coil cools air at the evaporator using a model that is very similar to the regular, single speed DX cooling coil.  The same five performance curves are used to describe cooling capacity and electric power use.  The sensible heat ratio is described using two performance curves in the same as is available for 100% outdoor air coils. The TES tank is neither charged or discharged but it continues to exchange heat with the surrounding ambient air.

Cooling And Charge Mode.  This mode is where the coil both cools air at the evaporator and cools the TES tank at the same time.  The model supports separate performance characteristics when there two separate heat engines, one for evaporator cooling and one for charging the TES tank.

Cooling and Discharge Mode.  This mode is where the coil cools process air and heat is rejected to both the condenser section and the TES tank at the same time. The model supports separate performance characteristics when there two separate heat engines, one for evaporator cooling and one for TES cooling.

Charge Only Mode.  This mode is where the coil charges the TES tank but does not provide any cooling at the process air cooling section.

Discharge Only Mode.  This mode is where the coil cools process air by discharging the TES tank.

This cooling coil can be autosized.  The cooling capacity for the cooling only mode is sized to meet the air system's final design cooling capacity.  The cooling capacity for the other modes, and the storage capacity, can all be scaled relative to that one capacity using sizing factors that can be input by the user.

### Inputs

#### Field:  Name

A unique name for this TES coil object, assigned by user.

#### Field:  Availability Schedule Name

The name of a schedule that determines whether the TES coil is available for cooling during a given time period.  A schedule value greater than 0 (usually 1 is used) indicates that the unit can be on during a given time period.  A value less than or equal to 0 (usually 0 is used) denotes that unit must be in "off" mode.  If this field is blank, the schedule has values of 1 for all time periods.

#### Field:  Operating Mode Control Method

This field determines how the TES coil is to be controlled in terms of which operating mode is in effect for a given time period.  There are two choices, "ScheduledModes" or "EMSControlled."  Choosing ScheduledModes indicates that the operating mode is determined by the values in a schedule that is named in the following input field.  Choosing EMSControlled indicates that the operating mode is determined by the state of an EMS actuator called "[Coil:Cooling:DX:SingleSpeed:ThermalStorage](#coilcoolingdxsinglespeedthermalstorage)" with the control type "Operating Mode."

#### Field:  Operation Mode Control Schedule Name

This field is used (and required) if the operating model control method is set to ScheduledModes in the previous input field.  The control schedule consists of a series of integer values that indicate what mode the TES coil should operate in for a given time period. The values for various operating modes have been programmed to be as follows:

Off Mode

Cooling Only Mode

Cooling And Charge Mode

Cooling And Discharge Mode

Charge Only Mode

Discharge Only Mode

#### Field:  Storage Type

This field is used to determine what type of material is used for thermal storage.  There are two basic types of thermal storage material, fluid or ice.  For fluid tanks, the material can be water or a user-defined fluid such as a glycol and the storage of thermal energy accompanies changes in the temperature of the fluid in the tank.  For ice tanks, the material is water ice and the storage of thermal energy accompanies changes in the fraction of ice.  This input field has three possible choices:  **Water**, **UserDefinedFluidType**, Or **Ice**.  Choose **Water** for a fluid tank TES based on water.  Choose **UserDefinedFluidType** for a fluid tank TES based on custom or glycol type fluid.  Choose **Ice** for ice-based TES tank.

#### Field:  User Defined Fluid Type

This field is used to declare what type of user defined fluid is contained in the TES tank.  This input field is only used (and required) if the previous field is set to "UserDefinedFluidType." Water, EthyleneGlycol, and PropoleneGlycol have fluid properties built-in to EnergyPlus.  Using a [FluidProperties:GlycolConcentration](#fluidpropertiesglycolconcentration) object allows specifying a mixture of water and a glycol and the name of one can be entered here.  For other types of fluids, a complete set of fluid property data is needed, see [FluidProperties:Name](#fluidpropertiesname), FluidProperties:Temperature, etc.

#### Field:  Fluid Storage Volume [m^3^]

This field is used to describe the size of fluid-based TES tank, in m^3^.  The storage volume can be automatically calculated based on the cooling capacity and sizing factor.

#### Field:  Ice Storage Capacity [GJ]

This field is used to describe the size of ice-based TES tank, in GJ.  The storage capacity can be automatically calculated based on the cooling capacity and a sizing factor.

#### Field:  Storage Capacity Sizing Factor [hr]

This field is used if one of the previous two fields is set to autocalculate.  The value entered here is a time duration, in hours.  This time period is used for calculating a storage capacity.  The basic idea is that storage be sized such that the TES can provide cooling at rated capacity for this amount of time.  The rated capacity used in the sizing calculation is the Discharge Only Mode Rated Storage Discharging Capacity unless the Discharge Only mode is not available in which case it is the Cooling Only Mode Rated Total Evaporator Cooling Capacity.  This sizing factor approach allows scaling the storage size relative to the TES coil's capacity.  The sizing factor is applied for an ice-based TES by simply multiplying the rated capacity by the time duration (converted to seconds).  For fluid-based TES, a change in fluid temperature of 10°C is assumed to calculate the tank volume.

#### Field:  Storage Tank Ambient Temperature Node Name

This field is used to assign the environmental conditions surrounding the TES coil.  The thermal storage tank exchanges heat with its surroundings and the boundary conditions for those surrounding are taken from this node named in this field. Typically this is the name of a node declared to be an outdoor air node.  This field is required.

#### Field:  Storage Tank to Ambient U-value Times Area Heat Transfer Coefficient [W/K]

This field is used to characterize the rate at which heat is exchanged between the TES tank and the surrounding ambient conditions, in W/K.  This is an overall "UA" value for the tank where the U-factor and surface area are combined into one coefficient.  Heat loss or gain to the TES tank is modeled using ![](media/image383.png) . This field is required.

#### Field:  Fluid Storage Tank Rating Temperature [C]

This field is used to define what temperature is used for rating conditions when using a fluid storage tank.  This field is only used for Storage Type of Water or UserDefinedFluidType.  The temperature here is used for declaring the state of the TES fluid tank that corresponds to "Rated" conditions.  The temperature entered here is used to define fluid properties and to characterize the performance curves that depend on the state of the TES.

#### Field:  Rated Evaporator Air Flow Rate [m^3^/s]

This field is the air volume flow rate through the coil, in m^3^/s, at rating conditions.  This is the rated air flow rate through the evaporator (and any other air cooling devices that are in series with the main evaporator).  The coil can be operated with a different flow rate than this rated flow rate and the performance of the unit scales accordingly using the curves that are of the type "Function of Flow Fraction Curve."  All of the other "rated" values for capacity, COP, and SHR values, for all the various modes, should be determined at the same air flow rate used here.  This field can be autosized.

#### Field:  Evaporator Air Inlet Node Name

This field is the name of an HVAC system node that the coil draws in as its inlet air.  This for the evaporator's connection to air system but it may include other air cooling devices that are in series with the evaporator and contained within the packaged TES coil. This node must be unique across the model. This field is required.

#### Field:  Evaporator Air Outlet Node Name

This field is the name of an HVAC system node that the coils sends its outlet air.  This for the evaporator's connection to air system but it may include other air cooling devices that are in series with the evaporator and contained within the packaged TES coil.  This node must be unique across the model. This field is required.

#### Field:  Cooling Only Mode Available

This field is used to indicate if the packaged TES coil includes a mode with only cooling and no equipment is interacting with TES tank to charge or discharge it.  The choices are "Yes" or "No." This field is required.

#### Field:  Cooling Only Mode Rated Total Evaporator Cooling Capacity [W]

This field is used to specify the total, full load cooling capacity (sensible plus latent), in Watts, of the TES coil at rated conditions, while operating in Cooling Only Mode.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb and air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, and the air flow rate specified in Rated Evaporator Air Flow Rate.  Capacity should be "gross" (i.e. supply air fan heat is NOT included).  This total cooling capacity is a central value for TES coil in the sense that it is the basis for various autocalculated sizes for the rest of the model which can be scaled off of this one value. This field is required if Cooling Only Mode is available or if another operating mode's capacity, or storage capacity, will be autocalculated from this value.  This field is autosizable.

#### Field:  Cooling Only Mode Rated Sensible Heat Ratio

This field is used to specify the sensible heat ratio (SHR) at rating conditions while operating in Cooling Only Mode.  SHR is the sensible cooling capacity divided by the total (sensible plus latent) cooling capacity and is dimensionless.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, and cooling coil air flow rate specified in Rated Evaporator Air Flow Rate.  Both the sensible and total cooling capacities used to define the rated SHR should be gross and not include supply fan heat.  The packaged TES coil model uses SHR curves to modify the rated SHR as conditions move away from the rating conditions. This field is only used if Cooling Only Mode is available. If this input is left blank, the default is 0.7.

#### Field:  Cooling Only Mode Rated COP

This field is used to specify the coefficient of performance (COP) at rating conditions while operating in Cooling Only Mode.  COP is the total cooling power output in watts divided by the electric power input in watts and is dimensionless. The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, and cooling coil air flow rate specified in Rated Evaporator Air Flow Rate.  The electric input power includes power for the compressor(s), condenser fan(s), and internal controls but does not include the electric power for the supply fan (which is modeled separately in EnergyPlus).  The total cooling power output is the same as the value for the Cooling Only Mode Rated Total Evaporator Cooling Capacity and is gross cooling without the fan heat.  This field is only used if Cooling Only Mode is available. If this input is left blank, the default is 3.0.

#### Field:  Cooling Only Mode Total Evaporator Cooling Capacity Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the total cooling capacity as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil and (2) the drybulb temperature of the air entering the condenser section.  The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness). The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 19.4°C and y= 35.0°C. The result of the curve is multiplied by Cooling Only Mode Rated Total Evaporator Cooling Capacity to model capacity at temperatures away from the rating point.  This field is required if Cooling Only Mode is available.

#### Field:  Cooling Only Mode Total Evaporator Cooling Capacity Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the total cooling capacity as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the Cooling Only Mode Rated Total Evaporator Cooling Capacity to model capacity at air flow rates away from the rating point.  This field is required if Cooling Only Mode is available.

#### Field:  Cooling Only Mode Energy Input Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil and (2) the drybulb temperature of the air entering the condenser section.  The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 19.4°C and y= 35.0°C. The result of this curve is multiplied by the inverse of the Cooling Only Mode Rated COP to model electric energy consumption at temperatures away from the rating point.  This field is required if Cooling Only Mode is available.

#### Field:  Cooling Only Mode Energy Input Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the inverse of the Cooling Only Mode Rated COP to model electric energy consumption at air flow rates away from the rating point.  This field is required if Cooling Only Mode is available.

#### Field:  Cooling Only Mode Part Load Fraction Correlation Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) as a function of the part load ratio (PLR).  PLR is the ratio of current cooling load to the current cooling capacity.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is the part load fraction (PLF) and the runtime fraction of the coil is defined as PLR divided by PLF.  The runtime fraction is then multiplied by the energy input ratio to model electric energy consumption at part load to account for inefficiencies because of compressor cycling.  This field is required if Cooling Only Mode is available.

#### Field:  Cooling Only Mode Sensible Heat Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the sensible heat ratio (SHR) as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil and (2) the drybulb temperature of the air entering the evaporator section. The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the evaporator section.  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 19.4°C and y= 26.7°C.  The result of the curve is multiplied by the Cooling Only Mode Rated Sensible Heat Ratio to model the SHR at coil entering temperatures that differ from the rating point.  This field is required if Cooling Only Mode is available.

#### Field:  Cooling Only Mode Sensible Heat Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the sensible heat ratio (SHR) as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate. The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0.  The result of the curve is multiplied by the Cooling Only Mode Rated Sensible Heat Ratio to model the SHR at coil air flow rates that differ from the rating point. This field is required if Cooling Only Mode is available.

#### Field:  Cooling And Charge Mode Available

This field is used to indicate if the packaged TES coil includes a mode with both cooling at the coil and charging of the TES tank at the same time. The choices are "Yes" or "No."  This field is required.

#### Field:  Cooling And Charge Mode Rated Total Evaporator Cooling Capacity [W]

This field is used to specify the total, full load cooling capacity (sensible plus latent), in Watts, of the TES coil at rated conditions, while operating in Cooling And Charge Mode.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb and air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, the air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  Capacity should be "gross" (i.e. supply air fan heat is NOT included).  The Cooling and Charge Mode has two capacities, this first one is for "cooling" and would typically be for a heat engine operating between the condenser and the evaporator.  This field is required if Cooling And Charge Mode is available.

This field is autocalculatable.  When autocalculating the capacity, the following sizing factor field is used to scale this capacity relative to the Cooling Only Mode Rated Total Evaporator Cooling Capacity.

#### Field:  Cooling And Charge Mode Capacity Sizing Factor

This field is used if the previous input field is set to "autocalculate."  This sizing factor is multiplied by the Cooling Only Mode Rated Total Evaporator Cooling Capacity to obtain a scaled value for Cooling and Charge Mode Rated Total Evaporator Cooling Capacity, in Watts. If this field is left blank the default values is 0.5.

#### Field:  Cooling And Charge Mode Rated Storage Charging Capacity [W]

This field is used to specify the total, full load charging capacity, in Watts, of the TES coil at rated conditions, while operating in Cooling And Charge Mode.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb and air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, the air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  Capacity should be "net" (i.e. any ancillary equipment inside the package needed for charging is included).  The Cooling and Charge Mode has two capacities, this second one is for "charging" and would typically be for a heat engine operating between the condenser and the TES tank.  This field is required if Cooling And Charge Mode is available.

This field is autocalculatable.  When autocalculating the capacity, the following sizing factor field is used to scale this capacity relative to the Cooling Only Mode Rated Total Evaporator Cooling Capacity.

#### Field:  Cooling And Charge Mode Storage Capacity Sizing Factor

This field is used if the previous input field is set to "autocalculate."  This sizing factor is multiplied by the Cooling Only Mode Rated Total Evaporator Cooling Capacity to obtain a scaled value for Cooling and Charge Mode Rated Storage Charging Capacity, in Watts. If this field is left blank the default values is 0.5.

#### Field:  Cooling And Charge Mode Rated Sensible Heat Ratio

This field is used to specify the sensible heat ratio (SHR) at rating conditions while operating in Cooling And Charge Mode.  SHR is the sensible cooling capacity divided by the total (sensible plus latent) cooling capacity and is dimensionless.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, and cooling coil air flow rate specified in Rated Evaporator Air Flow Rate.  Both the sensible and total cooling capacities used to define the rated SHR should be gross and not include supply fan heat.  The packaged TES coil model uses SHR curves to modify the rated SHR as conditions move away from the rating conditions.  This field is only used if Cooling And Charge Mode is available. If this input is left blank, the default is 0.7.

#### Field:  Cooling And Charge Mode Cooling Rated COP

This field is used to specify the coefficient of performance (COP) at rating conditions while operating in Cooling And Charge Mode to cool air at the evaporator.  COP is the total cooling power output in watts divided by the electric power input in watts and is dimensionless. The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, cooling coil air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  The electric input power includes power for the compressor(s), condenser fan(s), and internal controls but does not include the electric power for the supply fan (which is modeled separately in EnergyPlus).  The total cooling power output is the same as the value for the Cooling And Charge Mode Rated Total Evaporator Cooling Capacity and is gross cooling without the fan heat.  The Cooling And Charge Mode has two COP values and this first COP is for "cooling" and would typically be for a heat engine operating between the condenser and the evaporator.  This field is only used if Cooling And Charge Mode is available. If this input is left blank, the default is 3.0.

#### Field:  Cooling And Charge Mode Charging Rated COP

This field is used to specify the coefficient of performance (COP) at rating conditions while operating in Cooling And Charge Mode to charge the TES.  COP is the total cooling power output in watts divided by the electric power input in watts and is dimensionless. The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, cooling coil air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  The electric input power includes power for the compressor(s), condenser fan(s), and internal controls.  The charging power output is the same as the value for the Cooling And Charge Mode Rated Storage Charging Capacity and is net charging including any internal equipment.  The Cooling And Charge Mode has two COP values and this second COP is for "charging" and would typically be for a heat engine operating between the condenser and the TES tank.  This field is only used if Cooling And Charge Mode is available. If this input is left blank, the default is 3.0.

#### Field:  Cooling And Charge Mode Total Evaporator Cooling Capacity Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the total cooling capacity as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the condenser section, and (3) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.  The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C, y=35.0°C, and z=0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of the curve is multiplied by Cooling And Charge Mode Rated Total Evaporator Cooling Capacity to model capacity at temperatures away from the rating point.  This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Total Evaporator Cooling Capacity Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the total cooling capacity as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0.  The result of the curve is multiplied by the Cooling And Charge Mode Rated Total Evaporator Cooling Capacity to model capacity at air flow rates away from the rating point.  This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Evaporator Energy Input Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for evaporator cooling as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the condenser section, and (3) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types. The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C, y=35.0°C, and z=0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType.  The performance curve is normalized to have the value of 1.0 at the rating point. The result of this curve is multiplied by the inverse of the Cooling And Charge Mode Cooling Rated COP to model electric energy consumption at temperatures away from the rating point. This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Evaporator Energy Input Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for evaporator cooling as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0.  The result of the curve is multiplied by the inverse of the Cooling And Charge Mode Cooling Rated COP to model electric energy consumption at air flow rates away from the rating point.  This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Evaporator Part Load Fraction Correlation Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) (for evaporator cooling) as a function of the part load ratio (PLR).  PLR is the ratio of current cooling load to the current cooling capacity.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0.  The result of the curve is the part load fraction (PLF) and the runtime fraction of the coil is defined as PLR divided by PLF.  The runtime fraction is then multiplied by the energy input ratio to model electric energy consumption at part load to account for inefficiencies because of compressor cycling.  This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Storage Charge Capacity Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the storage charging capacity as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the condenser section, and (3) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.  The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C, y=35.0°C, and z=0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType.  The result of the curve is multiplied by Cooling And Charge Mode Rated Storage Charging Capacity to model capacity at temperatures away from the rating point.  This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Storage Charge Capacity Function of Total Evaporator PLR Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the storage charging capacity as a function of part load ratio (PLR) at the evaporator.  PLR is the ratio of current evaporator cooling load to the current evaporator cooling capacity.  This curve allows increasing the storage charging capacity when loads at the evaporator are low.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0.  The result of the curve is multiplied by Cooling And Charge Mode Rated Storage Charging Capacity to model capacity at evaporator PLR less than 1.0.  This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Storage Energy Input Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for charging as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the condenser section, and (3) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types. The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C, y = 35.0°C, and z = 0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of this curve is multiplied by the inverse of the Cooling And Charge Mode Charging Rated COP to model electric energy consumption at temperatures away from the rating point. This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Storage Energy Input Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for charging as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable). The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the inverse of the Cooling And Charge Mode Charging Rated COP to model electric energy consumption at air flow rates away from the rating point.  This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Storage Energy Part Load Fraction Correlation Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) (for storage charging) as a function of the part load ratio (PLR).  PLR is the ratio of current cooling load to the current cooling capacity (at the evaporator).  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0.  The result of the curve is the part load fraction (PLF) and the runtime fraction is defined as PLR divided by PLF.  The runtime fraction is then multiplied by the energy input ratio to model electric energy consumption at part load to account for inefficiencies because of compressor cycling.  This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Sensible Heat Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the sensible heat ratio (SHR) as a function of temperature and optionally state of TES tank.  The user can enter the name of a curve or table object that has either two or three independent variables.

For a curve or table with two independent variables the SHR is a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil and (2) the drybulb temperature of the air entering the evaporator section. The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the evaporator section.

For a curve or table with three independent variables the SHR is a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the evaporator section and (3) the state of TES tank (in C or fraction). The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" and "y" values for the curve are the same as for two independent variables while the "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.

The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 19.4°C, y = 26.7°C, and z = 0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType.  The result of the curve is multiplied by the Cooling And Charge Mode Rated Sensible Heat Ratio to model the SHR at coil entering temperatures that differ from the rating point.  This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Charge Mode Sensible Heat Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the sensible heat ratio (SHR) as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate. The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable). The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the Cooling And Charge Mode Rated Sensible Heat Ratio to model the SHR at coil air flow rates that differ from the rating point. This field is required if Cooling And Charge Mode is available.

#### Field:  Cooling And Discharge Mode Available

This field is used to indicate if the packaged TES coil includes a mode with both cooling at the coil and discharging of the TES tank at the same time. The choices are "Yes" or "No."  This field is required.

#### Field:  Cooling And Discharge Mode Rated Total Evaporator Cooling Capacity [W]

This field is used to specify the total, full load cooling capacity (sensible plus latent), in Watts, of the TES evaporator coil at rated conditions, while operating in Cooling And Discharge Mode.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb and air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, the air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  Capacity should be "gross" (i.e. supply air fan heat is NOT included).  The Cooling and Discharge Mode has two capacities (and may have two separate cooling coils contained within), this first one is for "Evaporator cooling" and would typically be for a heat engine operating between the condenser and the evaporator.  This field is required if Cooling And Discharge Mode is available.

This field is autocalculatable.  When autocalculating the capacity, the following sizing factor field is used to scale this capacity relative to the Cooling Only Mode Rated Total Evaporator Cooling Capacity.

#### Field:  Cooling And Discharge Mode Evaporator Capacity Sizing Factor

This field is used if the previous input field is set to "autocalculate."  This sizing factor is multiplied by the Cooling Only Mode Rated Total Evaporator Cooling Capacity to obtain a scaled value for Cooling and Discharge Mode Rated Total Evaporator Cooling Capacity, in Watts. If this field is left blank the default values is 1.0.

#### Field:  Cooling And Discharge Mode Rated Storage Discharging Capacity

This field is used to specify the total, full load discharging capacity, in Watts, of the TES coil at rated conditions, while operating in Cooling And Discharge Mode.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb and air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, the air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  Capacity should be "net" (i.e. any ancillary equipment inside the package needed for discharging is included).  The Cooling and Discharge Mode has two capacities, this second one is for "discharging" and would typically be for a heat transfer loop operating between the TES tank and a coil in series with the evaporator.  This field is required if Cooling And Discharge Mode is available.

This field is autocalculatable.  When autocalculating the capacity, the following sizing factor field is used to scale this capacity relative to the Cooling Only Mode Rated Total Evaporator Cooling Capacity.

#### Field:  Cooling And Discharge Mode Storage Discharge Capacity Sizing Factor

This field is used if the previous input field is set to "autocalculate."  This sizing factor is multiplied by the Cooling Only Mode Rated Total Evaporator Cooling Capacity to obtain a scaled value for Cooling and Discharge Mode Rated Storage Discharging Capacity, in Watts. If this field is left blank the default value is 1.0.

#### Field:  Cooling And Discharge Mode Rated Sensible Heat Ratio

This field is used to specify the sensible heat ratio (SHR) at rating conditions while operating in Cooling And Discharge Mode.  SHR is the sensible cooling capacity divided by the total (sensible plus latent) cooling capacity and is dimensionless.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, cooling coil air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  Both the sensible and total cooling capacities used to define the rated SHR should be gross and not include supply fan heat.  The packaged TES coil model uses SHR curves to modify the rated SHR as conditions move away from the rating conditions.  Cooling and discharge mode may have two separate coils in series all contained within the package and this SHR should be for the combined performance of the entire package. This field is only used if Cooling And Discharge Mode is available. If this input is left blank, the default is 0.7.

#### Field:  Cooling And Discharge Mode Cooling Rated COP

This field is used to specify the coefficient of performance (COP) at rating conditions while operating in Cooling And Discharge Mode to cool air at the evaporator.  COP is the total cooling power output in watts divided by the electric power input in watts and is dimensionless. The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, cooling coil air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  The electric input power includes power for the compressor(s), condenser fan(s), and internal controls but does not include the electric power for the supply fan (which is modeled separately in EnergyPlus).  The total cooling power output is the same as the value for the Cooling And Discharge Mode Rated Total Evaporator Cooling Capacity and is gross cooling without the fan heat.  The Cooling And Discharge Mode has two COP values and this first COP is for "evaporator cooling" and would typically be for a heat engine operating between the condenser and the evaporator.  This field is only used if Cooling And Discharge Mode is available. If this input is left blank, the default is 3.0.

#### Field:  Cooling And Discharge Mode Discharging Rated COP

This field is used to specify the coefficient of performance (COP) at rating conditions while operating in Cooling And Discharge Mode to discharge the TES.  COP is the total cooling power output in watts divided by the electric power input in watts and is dimensionless. The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb, cooling coil air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  The electric input power includes power for the compressor or circulation pumps and internal controls.  The discharging power output is the same as the value for the Cooling And Discharge Mode Rated Storage Discharging Capacity and is net discharging including any internal equipment.  The Cooling And Discharge Mode has two COP values and this second COP is for "discharging" and would typically be for a heat transfer loop operating between the TES tank and a coil in series with the evaporator.  This field is only used if Cooling And Discharge Mode is available. If this input is left blank, the default is 3.0.

#### Field:  Cooling And Discharge Mode Total Evaporator Cooling Capacity Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the total cooling capacity as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the condenser section, and (3) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.  The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C, y = 35.0°C, and z = 0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of the curve is multiplied by Cooling And Discharge Mode Rated Total Evaporator Cooling Capacity to model capacity at temperatures away from the rating point.  This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Total Evaporator Cooling Capacity Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the total cooling capacity as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable). The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the Cooling And Discharge Mode Rated Total Evaporator Cooling Capacity to model capacity at air flow rates away from the rating point.  This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Evaporator Energy Input Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for evaporator cooling as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the condenser section, and (3) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types. The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C, y = 35.0°C, and z = 0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of this curve is multiplied by the inverse of the Cooling And Discharge Mode Cooling Rated COP to model electric energy consumption at temperatures away from the rating point. This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Evaporator Energy Input Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for evaporator cooling as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the inverse of the Cooling And Discharge Mode Cooling Rated COP to model electric energy consumption at air flow rates away from the rating point.  This field is required if Cooling And discharge Mode is available.

#### Field:  Cooling And Discharge Mode Evaporator Part Load Fraction Correlation Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) (for evaporator cooling) as a function of the part load ratio (PLR).  PLR is the ratio of current cooling load to the current cooling capacity.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is the part load fraction (PLF) and the runtime fraction of the coil is defined as PLR divided by PLF.  The runtime fraction is then multiplied by the energy input ratio to model electric energy consumption at part load to account for inefficiencies because of compressor cycling.  This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Storage Discharge Capacity Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the storage discharging capacity as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the condenser section, and (3) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.  The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C, y = 35.0°C, and z = 0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of the curve is multiplied by Cooling And Discharge Mode Rated Storage Discharging Capacity to model capacity at temperatures away from the rating point.  This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Storage Discharge Capacity Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the storage discharge capacity as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the Cooling And Discharge Mode Rated Storage Discharging Capacity to model capacity at air flow rates away from the rating point.  This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Storage Discharge Capacity Function of Total Evaporator PLR Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the storage discharging capacity as a function of part load ratio (PLR) at the evaporator.  PLR is the ratio of current evaporator cooling load to the current evaporator cooling capacity.  This curve allows varying the storage discharging capacity based on loads at the evaporator.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by Cooling And Discharge Mode Rated Storage Discharging Capacity to model capacity at different evaporator PLR values.  This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Storage Energy Input Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for discharging as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the condenser section, and (3) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types. The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C, y = 35.0°C, and z = 0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of this curve is multiplied by the inverse of the Cooling And Discharge Mode Discharging Rated COP to model electric energy consumption at temperatures away from the rating point. This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Storage Energy Input Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for discharging as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the inverse of the Cooling And Discharge Mode Discharging Rated COP to model electric energy consumption at air flow rates away from the rating point.  This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Storage Energy Part Load Fraction Correlation Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) (for storage discharging) as a function of the part load ratio (PLR).  PLR is the ratio of current cooling load to the current cooling capacity (at the evaporator).  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is the part load fraction (PLF) and the runtime fraction is defined as PLR divided by PLF.  The runtime fraction is then multiplied by the energy input ratio to model electric energy consumption at part load to account for inefficiencies because of compressor cycling.  This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Sensible Heat Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the sensible heat ratio (SHR) as a function of temperature and optionally state of TES tank.  The user can enter the name of a curve or table object that has either two or three independent variables.

For a curve or table with two independent variables the SHR is a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil and (2) the drybulb temperature of the air entering the evaporator section. The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the evaporator section.

For a curve or table with three independent variables the SHR is a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the evaporator section and (3) the state of TES tank (in C or fraction). The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" and "y" values for the curve are the same as for two independent variables while the "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.

The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 19.4°C, y = 26.7°C, and z = 0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType.  The result of the curve is multiplied by the Cooling And Discharge Mode Rated Sensible Heat Ratio to model the SHR at coil entering temperatures that differ from the rating point.  This field is required if Cooling And Discharge Mode is available.

#### Field:  Cooling And Discharge Mode Sensible Heat Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the sensible heat ratio (SHR) as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate. The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the Cooling And Discharge Mode Rated Sensible Heat Ratio to model the SHR at coil air flow rates that differ from the rating point. This field is required if Cooling And Discharge Mode is available.

#### Field:  Charge Only Mode Available

This field is used to indicate if the packaged TES coil includes a mode with only charging of the TES tank. The choices are "Yes" or "No."  This field is required.

#### Field:  Charge Only Mode Rated Storage Charging Capacity [W]

This field is used to specify the total, full load charging capacity, in Watts, of the TES coil at rated conditions, while operating in Charge Only Mode.  The rating conditions are air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  Capacity should be "net" (i.e. any ancillary equipment inside the package needed for charging is included).  The Charge Only Mode capacity would typically be for a heat engine operating between the condenser and the TES tank.  This field is required if Charge Only Mode is available.

This field is autocalculatable.  When autocalculating the capacity, the following sizing factor field is used to scale this capacity relative to the Cooling Only Mode Rated Total Evaporator Cooling Capacity.

#### Field:  Charge Only Mode Capacity Sizing Factor

This field is used if the previous input field is set to "autocalculate."  This sizing factor is multiplied by the Cooling Only Mode Rated Total Evaporator Cooling Capacity to obtain a scaled value for Charge Only Mode Rated Storage Charging Capacity, in Watts. If this field is left blank the default values is 1.0.

#### Field:  Charge Only Mode Charging Rated COP

This field is used to specify the coefficient of performance (COP) at rating conditions while operating in Charge Only Mode to charge the TES.  COP is the total cooling power output in watts divided by the electric power input in watts and is dimensionless. The rating conditions are air entering the outdoor condenser section at 35°C drybulb and 23.9°C wetbulb and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  The electric input power includes power for the compressor(s), condenser fan(s), and internal controls.  The charging power output is the same as the value for the Charge Only Mode Rated Storage Charging Capacity and is net charging including any internal equipment.  This field is only used if Charge Only Mode is available. If this input is left blank, the default is 3.0.

#### Field:  Charge Only Mode Storage Charge Capacity Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the storage charging capacity as a function of (1) the drybulb temperature of the air entering the condenser section, and (2) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "y" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.  The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 35.0°C, and y = 0.5 for Ice storage type or y = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType.  The result of the curve is multiplied by Charge Only Mode Rated Storage Charging Capacity to model capacity at temperatures away from the rating point.  This field is required if Charge Only Mode is available.

#### Field:  Charge Only Mode Storage Energy Input Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for charging as a function of (1) the drybulb temperature of the air entering the condenser section, and (2) the state of TES tank (in C or fraction). The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the drybulb temperature of air entering the condenser section (which if using evaporatively-cooled condenser the temperature will be adjusted to approach the wetbulb temperature depending on effectiveness).  The "y" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.  The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 35.0°C, and y = 0.5 for Ice storage type or y = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of this curve is multiplied by the inverse of the Charge Only Mode Charging Rated COP to model electric energy consumption at temperatures away from the rating point. This field is required if Charge Only Mode is available.

#### Field:  Discharge Only Mode Available

This field is used to indicate if the packaged TES coil includes a mode with only discharging of the TES tank. The choices are "Yes" or "No."  This field is required.

#### Field:  Discharge Only Mode Rated Storage Discharging Capacity [W]

This field is used to specify the total, full load discharging capacity, in Watts, of the TES coil at rated conditions, while operating in Discharge Only Mode.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, the air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  Capacity should be "net" (i.e. any ancillary equipment inside the package needed for discharging is included) with regard to TES discharge, and "gross" with regard to supply fan heat.  Discharge Only Mode would typically be for a heat transfer loop operating between the TES tank and a coil in series with the evaporator.  This field is required if Discharge Only Mode is available.

This field is autocalculatable.  When autocalculating the capacity, the following sizing factor field is used to scale this capacity relative to the Cooling Only Mode Rated Total Evaporator Cooling Capacity.

#### Field:  Discharge Only Mode Capacity Sizing Factor

This field is used if the previous input field is set to "autocalculate."  This sizing factor is multiplied by the Cooling Only Mode Rated Total Evaporator Cooling Capacity to obtain a scaled value for Discharge Only Mode Rated Storage Discharging Capacity, in Watts. If this field is left blank the default value is 1.0.

#### Field:  Discharge Only Mode Rated Sensible Heat Ratio

This field is used to specify the sensible heat ratio (SHR) at rating conditions while operating in Discharge Only Mode.  SHR is the sensible cooling capacity divided by the total (sensible plus latent) cooling capacity and is dimensionless.  The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, cooling coil air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  Both the sensible and total cooling capacities used to define the rated SHR should be gross and not include supply fan heat.  The packaged TES coil model uses SHR curves to modify the rated SHR as conditions move away from the rating conditions.  This field is only used if Discharge Only Mode is available. If this input is left blank, the default is 0.7.

#### Field:  Discharge Only Mode Rated COP

This field is used to specify the coefficient of performance (COP) at rating conditions while operating in Discharge Only Mode to discharge the TES.  COP is the total cooling power output in watts divided by the electric power input in watts and is dimensionless. The rating conditions are air entering the cooling coil at 26.7°C drybulb, 19.4°C wetbulb, cooling coil air flow rate specified in Rated Evaporator Air Flow Rate, and the state of TES at either the Fluid Storage Tank Rating Temperature (for water or fluid storage type) or an ice fraction of 0.5 (for ice storage type).  The electric input power includes power for the compressor or circulation pumps and internal controls.  The discharging power output is the same as the value for the Discharge Only Mode Rated Storage Discharging Capacity and is net discharging including any internal equipment.  This field is only used if Discharge Only Mode is available. If this input is left blank, the default is 3.0.

#### Field:  Discharge Only Mode Storage Discharge Capacity Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the storage discharging capacity as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, and (2) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.  The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C and y = 0.5 for Ice storage type or y = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of the curve is multiplied by Discharge Only Mode Rated Storage Discharging Capacity to model capacity at temperatures away from the rating point.  This field is required if Discharge Only Mode is available.

#### Field:  Discharge Only Mode Storage Discharge Capacity Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the storage discharge capacity as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable). The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the Cooling Discharge Only Mode Rated Storage Discharging Capacity to model capacity at air flow rates away from the rating point.  This field is required if Discharge Only Mode is available.

#### Field:  Discharge Only Mode Energy Input Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for discharging as a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, and (2) the state of TES tank (in C or fraction).  The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.  The performance curve is normalized to have the value of 1.0 at the rating point which is defined to be x = 19.4°C and y = 0.5 for Ice storage type or y = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of this curve is multiplied by the inverse of the Discharge Only Mode Rated COP to model electric energy consumption at temperatures away from the rating point. This field is required if Discharge Only Mode is available.

#### Field:  Discharge Only Mode Energy Input Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) for discharging as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the inverse of the Discharge Only Mode Rated COP to model electric energy consumption at air flow rates away from the rating point.  This field is required if Discharge Only Mode is available.

#### Field:  Discharge Only Mode Part Load Fraction Correlation Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the energy input ratio (EIR) (for storage discharging) as a function of the part load ratio (PLR).  PLR is the ratio of current cooling load to the current cooling capacity.  The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable).  The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is the part load fraction (PLF) and the runtime fraction is defined as PLR divided by PLF.  The runtime fraction is then multiplied by the energy input ratio to model electric energy consumption at part load to account for inefficiencies because of compressor cycling.  This field is required if Discharge Only Mode is available.

#### Field:  Discharge Only Mode Sensible Heat Ratio Function of Temperature Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the sensible heat ratio (SHR) as a function of temperature and optionally state of TES tank.  The user can enter the name of a curve or table object that has either two or three independent variables.

For a curve or table with two independent variables the SHR is a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil and (2) the drybulb temperature of the air entering the evaporator section. The performance curve can be any curve or table based on two independent variables, x and y, including:  [Curve:Biquadratic](#curvebiquadratic), [Table:TwoIndependentVariables](#tabletwoindependentvariables), [Curve:Bicubic](#curvebicubic), and [Curve:QuadraticLinear](#curvequadraticlinear).  The "x" values for the performance curve are the wetbulb temperature of air entering the evaporator section.  The "y" values for the performance curve are the drybulb temperature of air entering the evaporator section.

For a curve or table with three independent variables the SHR is a function of (1) the wetbulb temperature of air entering the evaporator section of the TES coil, (2) the drybulb temperature of the air entering the evaporator section and (3) the state of TES tank (in C or fraction). The performance curve can be any curve or table based on three independent variables, x, y, and z, including:  [Curve:Triquadratic](#curvetriquadratic) and [Table:MultiVariableLookup](#tablemultivariablelookup).  The "x" and "y" values for the curve are the same as for two independent variables while the "z" values are the state of the TES tank which are the tank's temperature for water or fluid storage type or the storage fraction for ice storage types.

The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 19.4°C, y = 26.7°C, and z = 0.5 for Ice storage type or z = Fluid Storage Tank Rating Temperature when storage type is Water or UserDefinedFluidType. The result of the curve is multiplied by the Discharge Only Mode Rated Sensible Heat Ratio to model the SHR at coil entering temperatures that differ from the rating point.  This field is required if Discharge Only Mode is available.

#### Field:  Discharge Only Mode Sensible Heat Ratio Function of Flow Fraction Curve Name

This field is the name of a separate performance curve object that parameterizes the variation of the sensible heat ratio (SHR) as a function of the ratio of actual air flow rate across the cooling coil to the value of the Rated Evaporator Air Flow Rate. The performance curve can be any curve or table based on one independent variable, x, including:  [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic), [Curve:Exponent](#curveexponent), [Curve:ExponentialSkewNormal](#curveexponentialskewnormal), [Curve:Sigmoid](#curvesigmoid), Curve:RectuangularHyperbola1, [Curve:RectangularHyperbola2](#curverectangularhyperbola2), [Curve:ExponentialDecay](#curveexponentialdecay), [Curve:DoubleExponentialDecay](#curvedoubleexponentialdecay), and [Table:OneIndependentVariable](#tableoneindependentvariable). The performance curve is normalized to have a value of 1.0 at the rating point which is defined to be x = 1.0. The result of the curve is multiplied by the Discharge Only Mode Rated Sensible Heat Ratio to model the SHR at coil air flow rates that differ from the rating point. This field is required if Discharge Only Mode is available.

#### Field:  Ancillary Electric Power [W]

This field is the electric power level for miscellaneous ancillary controls and standby draws, in Watts.  This power is not linked to any particular operating mode and will always be "on," except when the device is scheduled to not be available by the Availability Schedule.  This field is optional.

#### Field:  Cold Weather Operation Minimum Outdoor Air Temperature [C]

This field is the outdoor temperature at which the device operates additional electric components to protect from cold weather, in Degrees Celsius.  When the outdoor temperature is below this value, the power draw specified in the next field will be turned on.  The "outdoor" temperature is obtained from the node specified in the input field called Storage Tank Ambient Temperature Node Name.

#### Field:  Cold Weather Operation Ancillary Power [W]

This field is the electric power level for cold weather protection.  Cold weather protection is in effect whenever the outdoor temperature is lower than the limit set in the previous field, except when the device is scheduled to not be available by the Availability Schedule.

#### Field:  Condenser Air Inlet Node Name

This field is the name of system node that serves as the inlet to the condenser section of the packaged TES coil.  This node is typically specified to be an outdoor air node.  The conditions exposed to the condenser are obtained from this system node.  This field is required.

#### Field:  Condenser Air Outlet Node Name

This field is the name of system node that serves as the outlet to the condenser section of the packaged TES coil.  This node is typically not connected to anything else.  The conditions leaving the condenser are applied to this system node.  This field is required.

#### Field:  Condenser Design Air Flow Rate [m^3^/s]

This field is the rate of air flow through the condenser section, in m^3^/s.  The model assumes constant, single-speed condenser fans.  The flow rate is not used to determine coil operation but is used to determine the conditions leaving the condenser section.  This field is required, for both air-cooled and evaporatively-cooled condenser types.

This field is autocalulatable. When autocalculated, the design flow rate is determined using the sizing factor in the following input field.

#### Field:  Condenser Air Flow Sizing Factor

This field is used if the previous input field is set to "autocalculate."  This sizing factor is multiplied by the Rated Evaporator Air Flow Rate to obtain a scaled value for Condenser Design Air Flow Rate, in m^3^/s. If this field is left blank the default value is 1.0.

#### Field:  Condenser Type

This field is the type of condenser used by the TES coil. There are two options, **AirCooled** or **EvaporativelyCooled**.  The default is AirCooled. The next six put fields are used when the condenser section is evaporatively cooled.

#### Field:  Evaporative Condenser Effectiveness

This field is the wetbulb effectiveness for the evaporatively-cooled condenser. The effectiveness is used to model the temperature of air exposed to the condenser section as follows:

![](media/image384.png)\


where

![](media/image385.png) = the temperature of air entering the condenser section, in °C. This value will be used when evaluating performance curves that depend on the drybulb entering the condenser section.

![](media/image386.png) = the wetbulb temperature of outdoor air, in °C

![](media/image387.png) = the drybulb temperature of outdoor air, in °C.

This field is required if the condenser type is set to EvaporativelyCooled. If the field is left blank then a default of 0.7 is used.

#### Field:  Evaporative Condenser Pump Rated Power Consumption [W]

This field is the rated power of the evaporative condenser water pump, in Watts.  This value is used to calculate the power required to pump the water used to evaporatively cool the condenser inlet air. The default is zero, but this field is autosizable using a sizing factor of 0.004266 W of electricity per W of cooling. This field is only used when the condenser type is set to EvaporativelyCooled.

#### Field:  Basin Heater Capacity [W/K]

This input field is the capacity of the evaporative cooler water basin heater for freeze protection in Watts per Kelvin. This field is only used when the condenser type is set to EvaporativelyCooled. This field is used with the following field to determine the electricity consumption rate for freeze protection of the water in a basin needed for evaporative cooling. The basin heater electric power is equal to this field multiplied by the difference between the Basin Heater Setpoint Temperature and the outdoor drybulb temperature.  The default is zero.

#### Field:  Basin Heater Setpoint Temperature [C]

This input field contains the setpoint temperature for basin heater operation, in °C. This field is only used when the condenser type is set to EvaporativelyCooled. The basin heater is active when the outdoor drybulb temperatures falls below this setpoint temperature. The default is 2.0°C.

#### Field:  Basin Heater Availability Schedule Name

This alpha field contains the name of the basin heater operating schedule. This field is only used when the condenser type is set to EvaporativelyCooled. The basin heater operating schedule is assumed to be an on/off schedule and the heater is available to operate any time the schedule value is greater than 0.  The basin heater operates whenever the schedule is on and the outdoor air drybulb is lower than the setpoint temperature in the previous field. If the field is left blank, the basin heater is available to operate throughout the simulation.

#### Field:  Supply Water Storage Tank Name

This optional field is used to describe where the TES coil obtains water used for evaporative cooling of its condenser.  If blank or omitted, then the unit will obtain water directly from the mains.  If the name of a Water Storage Tank object is used here, then the unit will attempt to obtain all its water from that tank.  However, if the tank cannot provide all the water the condenser needs, then the unit will still operate and obtains the rest of the water it needs form the mains (referred to as Starved Water).

#### Field:  Condensate Collection Water Storage Tank Name

This optional field is used to describe where condensate from the coil is collected. If blank or omitted, then any coil condensate is discarded.  Enter the name of a Water Storage Tank defined elsewhere and the condensate will be collected in that tank.

#### Field:  Storage Tank Plant Connection Inlet Node Name

This is the name of a system node that is the inlet to the TES tank.  This field is optional and is only used if the TES tank is directly connected to a plant loop.

#### Field:  Storage Tank Plant Connection Outlet Node Name

This is the name of a system node that is the inlet to the TES tank.  This field is optional and is only used if the TES tank is directly connected to a plant loop.

#### Field:  Storage Tank Plant Connection Design Flow Rate

This field is the design flow rate for the plant connection to the TES tank, in m^3^/s.  The TES tank will make a passive request for this amount of flow.  This field is required if the storage tank is connected to plant.

#### Field:  Storage Tank Plant Connection Heat Transfer Effectiveness

This field specifies the heat transfer effectiveness between the plant connection and the TES tank. If the effectiveness is set to 1 then ideal heat transfer occurs, as if the fluids were completely mixed and the fluid leaving the tank is the same temperature as the tank.  If the effectiveness is less than 1.0, then the leaving fluid temperature approaches that of the tank as would be the case with a heat exchanger.  If left blank, the default is 0.7.

#### Field:  Storage Tank Minimum Operating Limit Fluid Temperature [C]

This field is used for fluid-based TES tank (Storage Type of Water or UserDefinedFluidType) to set the a lower limit on the operating temperatures, in Degrees Celsius.  This value represents the temperature of the fluid-based TES tank when fully charged.  This field is optional.  When left blank, the model uses the lowest temperature for which fluid properties are defined.

#### Field:: Storage Tank Maximum Operating Limit Fluid Temperature [C]

This field is used for fluid-based TES tank (Storage Type of Water or UserDefinedFluidType) to set the an upper limit on the operating temperatures, in Degrees Celsius.  This value represents the temperature of the fluid-based TES tank when fully discharged.  This field is optional.  When left blank, the model uses the highest temperature for which fluid properties are defined.

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Cooling Coil Operating Mode Index []
    HVAC,Average,Cooling Coil Total Cooling Rate [W]
    HVAC,Sum,Cooling Coil Total Cooling Energy [J]
    HVAC,Average,Cooling Coil Sensible Cooling Rate [W]
    HVAC,Sum,Cooling Coil Sensible Cooling Energy [J]
    HVAC,Average,Cooling Coil Latent Cooling Rate [W]
    HVAC,Sum,Cooling Coil Latent Cooling Energy [J]
    HVAC,Average,Cooling Coil Electric Power [W]
    HVAC,Sum,Cooling Coil Electric Energy [J]
    HVAC,Average,Cooling Coil Runtime Fraction []
    HVAC,Sum,Cooling Coil Cold Weather Protection Electric Energy [J]
    HVAC,Average,Cooling Coil Cold Weather Protection Electric Power [W]
    HVAC,Average,Cooling Coil Thermal Storage Mechanical Heat Transfer Rate [W]
    HVAC,Sum,Cooling Coil Thermal Storage Mechanical Heat Transfer Energy [J]
    HVAC,Average,Cooling Coil Thermal Storage Ambient Heat Transfer Rate [W]
    HVAC,Sum,Cooling Coil Thermal Storage Ambient Heat Transfer Energy [J]
    HVAC,Average,Cooling Coil Ice Thermal Storage End Fraction []
    HVAC,Average,Cooling Coil Condenser Inlet Temperature [C]
    HVAC,Sum,Cooling Coil Evaporative Condenser Water Volume [m3]
    HVAC,Sum,Cooling Coil Evaporative Condenser Mains Supply Water Volume [m3]
    HVAC,Average,Cooling Coil Evaporative Condenser Pump Electric Power [W]
    HVAC,Sum,Cooling Coil Evaporative Condenser Pump Electric Energy [J]
    HVAC,Average,Cooling Coil Basin Heater Electric Power [W]
    HVAC,Sum,Cooling Coil Basin Heater Electric Energy [J]
    HVAC,Average,Cooling Coil Thermal Storage Plant Heat Transfer Rate [W]
    HVAC,Sum,Cooling Coil Thermal Storage Plant Heat Transfer Energy [J]
    HVAC,Average,Cooling Coil Fluid Thermal Storage End Temperature [C]
~~~~~~~~~~~~~~~~~~~~

#### Cooling Coil Operating Mode Index [ ]

This output variable reports the operating mode for the cooling coil.  The numbers in this output are integer codes that correspond to the operating modes as described in the following table.  These codes match the values used for input in schedules and EMS actuators to control operation.

Code|Operating Mode
----|--------------
0|Off
1|Cooling Only
2|Cooling and Charge
3|Cooling and Discharge
4|Charge Only
5|Discharge Only

#### Cooling Coil Total Cooling Rate [W]

This output is the total (sensible and latent) cooling rate of the coil in Watts.

#### Cooling Coil Total Cooling Energy [J]

This output is the total (sensible and latent) cooling energy transfer by the coil in Joules.  This output is metered as EnergyTransfer for CoolingCoils.

#### Cooling Coil Sensible Cooling Rate [W]

This output is the moist air sensible cooling rate of the coil in Watts.

#### Cooling Coil Sensible Cooling Energy [J]

This output is the moist air sensible cooling energy transfer by the coil in Joules.

#### Cooling Coil Latent Cooling Rate [W]

This output is the latent cooling rate of the coil in Watts.

#### Cooling Coil Latent Cooling Energy [J]

This output is the latent cooling energy transfer by the coil in Joules.

#### Cooling Coil Electric Power [W]

This output is the electricity consumption rate, in Watts, of the compressor(s), pump(s), condenser fan(s) that are inside the package to drive heat transfer for the cooling coil.  This does not include ancillary electrical consumption for such things as cold weather protection, off cycle ancillary power, or supply fans.

#### Cooling Coil Electric Energy [J]

This output is the electricity consumption, in Joules, of the compressor(s), pump(s), condenser fan(s) that are inside the package to drive heat transfer for the cooling coil.  This does not include ancillary electrical consumption for such things as cold weather protection, off cycle ancillary power, or supply fans.

#### Cooling Coil Runtime Fraction []

This output is the runtime fraction of cooling coil.

#### Cooling Coil Cold Weather Protection Electric Power [W]

#### Cooling Coil Cold Weather Protection Electric Energy [J]

These are the power and energy outputs associated with Cold Weather Operation Ancillary Power, in Watts and Joules respectively.

#### Cooling Coil Thermal Storage Mechanical Heat Transfer Rate [W]

#### Cooling Coil Thermal Storage Mechanical Heat Transfer Energy [J]

These are the power and energy outputs for the coil's heat exchange with the TES tank, in Watts and Joules respectively.  The sign convection is that negative values are cooling the TES tank, or charging it, and positive values are heating the TES tank, or discharging.  This heat transfer is driven by the mechanical systems used inside the package to charge or discharge the TES tank.

#### Cooling Coil Thermal Storage Ambient Heat Transfer Rate [W]

#### Cooling Coil Thermal Storage Ambient Heat Transfer Energy [J]

These are the power and energy outputs for the TES tank's heat exchange with the surrounding ambient conditions, in Watts and Joules respectively.  The sign convection is that negative values are cooling the TES tank, or charging it, and positive values are heating the TES tank, or discharging.  This heat transfer is driven by the temperature difference between the media in the tank and the surrounding ambient and is governed by the UA factor.

#### Cooling Coil Ice Thermal Storage End Fraction []

This output is the state of the TES tank as fraction of the storage capacity.  Because the tank storage model is dynamic, this value corresponds to the point in time right at the end of the timestep.  This output variable is only available for a storage type of "Ice."

#### Cooling Coil Fluid Thermal Storage End Temperature [C]

This output is the state of the TES tank as the temperature of the fluid in the tank, in °C.  Because the tank storage model is dynamic, this value corresponds to the point in time right at the end of the timestep.  This output variable is only available for a storage type of "Water" or "UserDefinedFluidType.'

#### Cooling Coil Condenser Inlet Temperature [C]

This output is the inlet temperature entering the condenser section of an evaporatively cooled condenser, in °C. This temperature is for the air stream after the evaporative assist and before entering the condenser.  This is the temperature used for condenser entering drybulb conditions when evaluating performance curves for evaporatively cooled condensers. This output is only available for evaporatively cooled condensers.

#### Cooling Coil Evaporative Condenser Water Volume [m^3^]

This output is the amount of water used to evaporatively cool the condenser inlet air, in cubic meters.  This output is only available for evaporatively cooled condensers.

#### Cooling Coil Evaporative Condenser Mains Supply Water Volume [m^3^]

This output is the volume of water drawn from mains water service for the evaporatively cooled condenser. This output is only available for evaporatively cooled condensers.

#### Cooling Coil Evaporative Condenser Pump Electric Power [W]

This output is the average electricity consumption rate of the evaporative condenser water pump, in Watts. This output is only available for evaporatively cooled condensers.

#### Cooling Coil Evaporative Condenser Pump Electric Energy [J]

This output is the electricity consumption of the evaporative condenser water pump, in Joules. This output is only available for evaporatively cooled condensers.

#### Cooling Coil Basin Heater Electric Power [W]

This output is the average electricity consumption rate of the basin heater, in Watts. This output is only available for evaporatively cooled condensers.

#### Cooling Coil Basin Heater Electric Energy [J]

This output is the electricity consumption of the basin heater, in Joules. This output is only available for evaporatively cooled condensers.

#### Cooling Coil Thermal Storage Plant Heat Transfer Rate [W]

#### Cooling Coil Thermal Storage Plant Heat Transfer Energy [J]

These are the power and energy outputs for the TES tank's heat exchange with the plant loop, in Watts and Joules respectively.  This output is only available if the plant connection to the tank is used.  The sign convection is that negative values are cooling the TES tank, or charging it, and positive values are heating the TES tank, or discharging.  This heat transfer is driven by the temperature difference between the media in the tank and the plant loop fluid and is governed by the effectiveness and plant fluid mass flow rate.