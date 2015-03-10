# Group – Pumps

EnergyPlus plant and condenser loops need pump components to drive liquid flow around the loop.  There are various types of pump models available.

## Pump:VariableSpeed

Pumps are needed in both plant and condenser loops. The syntax for a variable speed pump is given below.

### Inputs

#### Field: Name

This alpha field contains the identifying a unique name for the pump.

#### Field: Inlet Node Name

This alpha field contains the identifying name for the pump inlet node.

#### Field: Outlet Node Name

This alpha field contains the identifying name for the pump outlet node.

#### Field: Rated Flow Rate

This numeric field contains the pump's rated volumetric flow rate in cubic meters per second.

#### Field: Rated Pump Head

This numeric field contains the pump's rated head pressure in Pascals.

#### Field: Rated Power Consumption

This numeric field contains the pump's rated power consumption in Watts.  If the user is performing a pressure simulation on the loop in which this pump is found, this value would only be used to estimate pump efficiency.  During reported calculations, the pump would use the loop pressure drop and current flow conditions along with efficiency to calculate pump power dynamically.

#### Field: Motor Efficiency

This numeric field contains the pump's efficiency in decimal form (0 = 0%, 1 = 100%).

#### Field: Fraction of Motor Inefficiencies to Fluid Stream

This numeric field contains the pump's fraction of power loss to the fluid.

#### Field: Coefficient 1 of the Part Load Performance Curve

This numeric field contains the first coefficient in the part load ratio curve. The fraction of full load power is determined by the cubic equation:

![](media/image330.png)\


where C~1~,C~2~,C~3~,and C~4~ are Coefficients 1 – 4 (below) and PLR is the Part Load Ratio.

#### Field: Coefficient 2 of the Part Load Performance Curve

This numeric field contains the second coefficient in the part load ratio curve.

#### Field: Coefficient 3 of the Part Load Performance Curve

This numeric field contains the third coefficient in the part load ratio curve.

#### Field: Coefficient 4 of the Part Load Performance Curve

This numeric field contains the fourth coefficient in the part load ratio curve.

#### Field: Minimum Flow Rate

This field contains the minimum volumetric flow rate while operating in variable flow capacity rate in cubic meters per second.

#### Field: Pump Control Type

This is a choice field of Continuous or Intermittent. A variable speed pump is defined with maximum and minimum flow rates that are the physical limits of the device. If there is no load on the loop and the pump is operating intermittently, then the pump can shutdown. For any other condition such as the loop having a load and the pump is operating intermittently or the pump is continuously operating (regardless of the loading condition), the pump will operate and select a flow somewhere between the minimum and maximum limits. In these cases where the pump is running, it will try to meet the flow request made by demand side components. Applicable availability managers (ref. [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)) may override this control by forcing the pump to be on or off.

#### Field: Pump Flow Rate Schedule Name

The schedule values modify the value for Rated Flow Rate of the pump on a time basis. The default is that the pump is ON and runs according to its other operational requirements specified above. This schedule is for special, not typical pump operations.

#### Field: Pump Curve Name

This references a pump curve to be used during pump flow-loop head resolution.  This is only applicable for loop simulations which include pressure components on at least a single branch.  More information regarding pressure system simulation is available in the engineering reference.

#### Field: Impeller Diameter

For pressure simulation cases, this is the impeller diameter used during calculations which is used in non-dimensionalizing the pump curve described in the previous field.  The units of this value are meters.

#### Field: VFD Control Type

This string defines which type of VFD control type to perform.  PressureSetPointControl is a realistic approach which requires inputs of pressure range schedules to control the pump rpm to maintain a certain pressure drop (head).  ManualControl is an idealized control strategy in which the pump RPM is maintained at a scheduled value throughout the simulation, abiding by other flow restrictions in the loop.

#### Field: Pump RPM Schedule Name

For VFD Control Type = ManualControl, this defines the pump RPM schedule to use during the simulation.  For VFD Control Type = PressureSetPointControl, this field is not interpreted.  The value of the schedule is RPM.

#### Field: Minimum Pressure Schedule

For VFD Control Type = PressureSetPointControl, this field defines the minimum pressure range setpoint value, or the lower bound of pressure drop (head) to use when determining the required pump speed.  For VFD Control Type = ManualControl, this field is not interpreted.  The value of the schedule is Pascals.

#### Field: Maximum Pressure Schedule

For VFD Control Type = PressureSetPointControl, this field defines the maximum pressure range setpoint value, or the upper bound of pressure drop (head) to use when determining the required pump speed.  For VFD Control Type = ManualControl, this field is not interpreted.  The value of the schedule is Pascals.

#### Field: Minimum RPM Schedule

For VFD Control Type = PressureSetPointControl, this field defines the minimum allowable RPM, or the lower bound of pump speed to use when determining the required pump speed.  For VFD Control Type = ManualControl, this field is not interpreted.  The value of the schedule is RPM.

#### Field: Maximum RPM Schedule

For VFD Control Type = PressureSetPointControl, this field defines the maximum allowable RPM, or the upper bound of pump speed to use when determining the required pump speed.  For VFD Control Type = ManualControl, this field is not interpreted.  The value of the schedule is RPM.

#### Field: Zone Name

This field is optional. It can be used to input the name of the [Zone](#zone) in which the pump is located in the model.  If the pump is outdoors, or skin losses are not to be modeled, then leave this field blank.  If a valid [Zone](#zone) name is entered, then the portion of electrical power consumed by the pump that is not added to the working fluid is added to the surrounding [Zone](#zone).

#### Field: Skin Loss Radiative Fraction

This field is optional.  If a [Zone](#zone) is named in the previous field and pump losses are to be added to a surrounding thermal zone, then this input determines the split between thermal radiation and thermal convection for the heat losses from the pump.  If it is left blank then all the losses will be convective.

Examples of this object in the IDF follow.

~~~~~~~~~~~~~~~~~~~~

      Pump:VariableSpeed,
        HW Circ Pump,          !- Name
        HW Supply Inlet Node,  !- Inlet Node Name
        HW Pump Outlet Node,   !- Outlet Node Name
        autosize,              !- Rated Flow Rate {m3/s}
        179352,                !- Rated Pump Head {Pa}
        autosize,              !- Rated Power Consumption {W}
        0.9,                   !- Motor Efficiency
        0.0,                   !- Fraction of Motor Inefficiencies to Fluid Stream
        0,                     !- Coefficient 1 of the Part Load Performance Curve
        1,                     !- Coefficient 2 of the Part Load Performance Curve
        0,                     !- Coefficient 3 of the Part Load Performance Curve
        0,                     !- Coefficient 4 of the Part Load Performance Curve
        0,                     !- Minimum Flow Rate {m3/s}
        Intermittent;          !- Pump Control Type

      Pump:VariableSpeed,
        Circ Pump,             !- Name
        CW Supply Inlet Node,  !- Inlet Node Name
        CW Pump Outlet Node,   !- Outlet Node Name
        .0011,                 !- Rated Flow Rate {m3/s}
        300000,                !- Rated Pump Head {Pa}
        500,                   !- Rated Power Consumption {W}
        .87,                   !- Motor Efficiency
        0.0,                   !- Fraction of Motor Inefficiencies to Fluid Stream
        0,                     !- Coefficient 1 of the Part Load Performance Curve
        1,                     !- Coefficient 2 of the Part Load Performance Curve
        0,                     !- Coefficient 3 of the Part Load Performance Curve
        0,                     !- Coefficient 4 of the Part Load Performance Curve
        0,                     !- Minimum Flow Rate {m3/s}
        Intermittent;          !- Pump Control Type
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Pump Electric Power [W]
    HVAC,Sum,Pump Electric Energy [J]
    HVAC,Average,Pump Shaft Power [W]
    HVAC,Average,Pump Fluid Heat Gain Rate [W]
    HVAC,Sum,Pump Fluid Heat Gain Energy [J]
    HVAC,Average,Pump Outlet Temperature [C]
    HVAC,Average,Pump Mass Flow Rate [kg/s]
    HVAC,Average,Pump Zone Total Heating Rate [W]
    HVAC,Sum,Pump Zone Total Heating Energy [J]
    HVAC,Average,Pump Zone Convective Heating Rate [W]
    HVAC,Average,Pump Zone Radiative Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Pump Electric Power [W]

#### Pump Electric Energy [J] 

These outputs are the electric power input to the pump motor. Consumption is metered on Pumps:Electricity, Electricity:Plant, and Electricity:Facility.

#### Pump Shaft Power [W]

This is the shaft power delivered from the motor to the pump.

#### Pump Fluid Heat Gain Rate [W]

#### Pump Fluid Heat Gain Energy [J] 

These outputs are the energy added to the fluid as heat. For the current algorithm, this is equal to Pump Shaft Power, because the loops are closed and all energy added to the fluid will ultimately become heat due to friction.

#### Pump Outlet Temperature [C]

#### Pump Mass Flow Rate [kg/s]

These outputs are the water outlet temperature and mass flow rate.

#### Pump Zone Total Heating Rate [W]

#### Pump Zone Total Heating Energy [J]

These outputs are the thermal losses from the pump to the surrounding [Zone](#zone).  They are only available if a [Zone](#zone) was named in the pump's input.  These indicate the amount of heat added to the zone from the pump's inefficiencies. They are the total heat loss including both convection and radiation.

#### Pump Zone Convective Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of convective gains directly to the zone air. It is only available if a [Zone](#zone) was named in the pump input.

#### Pump Zone Radiative Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of thermal radiation gains directly to the inside face of the zone's surfaces. It is only available if a [Zone](#zone) was named in the pump input.

## Pump:ConstantSpeed

Pumps may be found in both plant and condenser loops. The syntax for a constant speed pump is given below.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the pump.

#### Field: Inlet Node Name

This alpha field contains the identifying name for the pump inlet node.

#### Field: Outlet Node Name

This alpha field contains the identifying name for the pump outlet node.

#### Field: Rated Flow Rate

This numeric field contains the pump's rated volumetric flow rate in cubic meters per second.

#### Field: Rated Pump Head

This numeric field contains the pump's rated head pressure in Pascals.

#### Field: Rated Power Consumption

This numeric field contains the pump's rated power consumption in Watts.  If the user is performing a pressure simulation on the loop in which this pump is found, this value would only be used to estimate pump efficiency.  During reported calculations, the pump would use the loop pressure drop and current flow conditions along with efficiency to calculate pump power dynamically.

#### Field: Motor Efficiency

This numeric field contains the pump's efficiency in decimal form (0 = 0%, 1 = 100%).

#### Field: Fraction of Motor Inefficiencies to Fluid Stream

This numeric field contains the pump's fraction of power loss to the fluid.

#### Field: Pump Control Type

This is a choice field of Continuous or Intermittent. The operation of a constant speed pump is fairly straightforward. If the user designates a constant speed pump that is operating continuously, the pump will run regardless of whether or not there is a load. This may have the net effect of adding heat to the loop if no equipment is turned on. If the pump is constant speed and operates intermittently, the pump will run at its capacity if a load is sensed and will shut off if there is no load on the loop. Applicable availability managers (ref. [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)) may override this control by forcing the pump to be on or off.

#### Field: Pump Flow Rate Schedule Name

The schedule values modify the Rated Flow Rate of the pump on a time basis. The default is that the pump is ON and runs according to its other operational requirements specified above. This schedule is for special, not typical pump operations.

#### Field: Pump Curve Name

This field is only used if the user is performing a pressure-based simulation in the plantloop of the "LoopFlowCorrection" type (see [PlantLoop](#plantloop) section for how to enable this).

This field references a separate pressure head curve that the user has input separately in the input file.  The pressure curve represents a non-dimensional regression of pressure-flow rate data.  The equation should be of the following form:

![](media/image331.png)\


In this equation, the two dimensionless parameters, X and X are defined in terms of physical pump and flow parameters as:

![](media/image332.png)\


![](media/image333.png)\


Where "Delta P" is the pressure drop, mdot is the fluid mass flow rate, rho is a representative fluid density, N is the rotational speed of the pump, and D is the pump impeller diameter.

Since the user may not need a full fourth order expansion of this, the user may use linear (first order), quadratic (second order), cubic (third order), or full quartic (fourth order).  These are available in the objects [Curve:Linear](#curvelinear), [Curve:Quadratic](#curvequadratic), [Curve:Cubic](#curvecubic), [Curve:Quartic](#curvequartic).

Once this curve is input properly (along with the following two fields), the pump will respond to the plant loop pressure drop when resolving the flow rate.  Note that this simulation method will likely result in a plant loop that does not exactly hit the setpoint as the current simulation method does.  This is due to the loop flow rate now being "pressure-based" and not "decision-based".

#### Field: Impeller Diameter

This field is only used if the user is performing a pressure-based simulation in the plantloop of the "LoopFlowCorrection" type (see [PlantLoop](#plantloop) section for how to enable this).

To "re-dimensionalize" the dimensionless pump curve, the impeller diameter must be known.  This value is entered in meters.

#### Field: Rotational Speed

This field is only used if the user is performing a pressure-based simulation in the plantloop of the "LoopFlowCorrection" type (see [PlantLoop](#plantloop) section for how to enable this).

To "re-dimensionalize" the dimensionless pump curve, the rotational speed must be known.  This value is entered in RPM (revolutions per minute).

#### Field: Zone Name

This field is optional. It can be used to input the name of the [Zone](#zone) in which the pump is located in the model.  If the pump is outdoors, or skin losses are not to be modeled, then leave this field blank.  If a valid [Zone](#zone) name is entered, then the portion of electrical power consumed by the pump that is not added to the working fluid is added to the surrounding [Zone](#zone).

#### Field: Skin Loss Radiative Fraction

This field is optional.  If a [Zone](#zone) is named in the previous field and pump losses are to be added to a surrounding thermal zone, then this input determines the split between thermal radiation and thermal convection for the heat losses from the pump.  If it is left blank then all the losses will be convective.

An example of this object follows.

~~~~~~~~~~~~~~~~~~~~

    Pump:ConstantSpeed,
           Circ Pump,             !- Name
           CW Supply Inlet Node,  !- Inlet Node Name
           PumpChiller Node,      !- Outlet Node Name
           0.0011,                !- Rated Flow Rate
           300000,                !- Rated Pump Head
           700,                   !- Rated Power Consumption
           0.87,                  !- Motor Efficiency
           0.0,                   !- Fraction of Motor Inefficiencies to Fluid Stream
           Intermittent;          !- Pump Control Type
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Pump Electric Power [W]
    HVAC,Sum,Pump Electric Energy [J]
    HVAC,Average,Pump Shaft Power [W]
    HVAC,Average,Pump Fluid Heat Gain Rate [W]
    HVAC,Sum,Pump Fluid Heat Gain Energy [J]
    HVAC,Average,Pump Outlet Temperature [C]
    HVAC,Average,Pump Mass Flow Rate [kg/s]
    HVAC,Average,Pump Zone Total Heating Rate [W]
    HVAC,Sum,Pump Zone Total Heating Energy [J]
    HVAC,Average,Pump Zone Convective Heating Rate [W]
    HVAC,Average,Pump Zone Radiative Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Pump Electric Power [W]

#### Pump Electric Energy [J] 

These outputs are the electric power input to the pump motor. Consumption is metered on Pumps:Electricity, Electricity:Plant, and Electricity:Facility.

#### Pump Shaft Power [W]

This is the shaft power delivered from the motor to the pump.

#### Pump Fluid Heat Gain Rate [W]

#### Pump Fluid Heat Gain Energy [J] 

These outputs are the energy added to the fluid as heat. For the current algorithm, this is equal to Pump Shaft Power, because the loops are closed and all energy added to the fluid will ultimately become heat due to friction.

#### Pump Outlet Temperature [C]

#### Pump Mass Flow Rate [kg/s]

These outputs are the water outlet temperature and mass flow rate.

#### Pump Zone Convective Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of convective gains directly to the zone air. It is only available if a [Zone](#zone) was named in the pump input.

#### Pump Zone Radiative Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of thermal radiation gains directly to the inside face of the zone's surfaces. It is only available if a [Zone](#zone) was named in the pump input.

#### Pump Zone Total Heating Energy [J]

#### Pump Zone Total Heating Rate [W]

These outputs are the thermal losses from the pump to the surrounding [Zone](#zone).  They are only available if a [Zone](#zone) was named in the pump's input.  These indicate the amount of heat added to the zone from the pump's inefficiencies. They are the total heat loss including both convection and radiation.

## Pump:VariableSpeed:Condensate

The [Pump:VariableSpeed:Condensate](#pumpvariablespeedcondensate) object can only be used in steam system simulation for the condensate side of the loop. This is a variable speed pump, which pumps the condensate back to the steam boiler. The syntax for condensate pump is given below.

### Inputs

#### Field: Name

This alpha field contains the identifying unique name for the pump.

#### Field: Inlet Node Name

This alpha field contains the identifying name for the pump inlet node.

#### Field: Outlet Node Name

This alpha field contains the identifying name for the pump outlet node.

#### Field: Rated Flow Rate

This numeric field contains the pump's rated volumetric flow rate in cubic meters per second.

#### Field: Rated Pump Head

This numeric field contains the pump's rated head in Pascals.

#### Field: Rated Power Consumption

This numeric field contains the pump's rated power consumption in Watts.  If the user is performing a pressure simulation on the loop in which this pump is found, this value would only be used to estimate pump efficiency.  During reported calculations, the pump would use the loop pressure drop and current flow conditions along with efficiency to calculate pump power dynamically.

#### Field: Motor Efficiency

This numeric field contains the pump's efficiency in decimal form (0 = 0%, 1 = 100%).

#### Field: Fraction of Motor Inefficiencies to Fluid Stream

This numeric field contains the pump's fraction of power loss to the fluid.

#### Field: Coefficient 1 of the Part Load Performance Curve

This numeric field contains the first coefficient in the part load ratio curve. The fraction of full load power is determined by the cubic equation:

![](media/image334.png)\


where C~1~,C~2~,C~3~,and C~4~ are Coefficients 1 – 4 (below) and PLR is the Part Load Ratio.

#### Field: Coefficient 2 of the Part Load Performance Curve

This numeric field contains the second coefficient in the part load ratio curve.

#### Field: Coefficient 3 of the Part Load Performance Curve

This numeric field contains the third coefficient in the part load ratio curve.

#### Field: Coefficient 4 of the Part Load Performance Curve

This numeric field contains the fourth coefficient in the part load ratio curve.

#### Field: Pump Flow Rate Schedule Name

The schedule values modify the Rated Flow Rate of the pump on a time basis the default is that the pump is ON and runs according to its other operational requirements specified above. The schedule is for special, not typical pump operations.

#### Field: Zone Name

This field is optional. It can be used to input the name of the [Zone](#zone) in which the pump is located in the model.  If the pump is outdoors, or skin losses are not to be modeled, then leave this field blank.  If a valid [Zone](#zone) name is entered, then the portion of electrical power consumed by the pump that is not added to the working fluid is added to the surrounding [Zone](#zone).

#### Field: Skin Loss Radiative Fraction

This field is optional.  If a [Zone](#zone) is named in the previous field and pump losses are to be added to a surrounding thermal zone, then this input determines the split between thermal radiation and thermal convection for the heat losses from the pump.  If it is left blank then all the losses will be convective.

Examples of this object in the IDF follow.

~~~~~~~~~~~~~~~~~~~~

    Pump:VariableSpeed:Condensate,
        Steam Boiler Plant Steam Circ Pump,         !- Name
        Steam Boiler Plant Steam Supply Inlet Node, !- Inlet Node Name
        Steam Boiler Plant Steam Pump Outlet Node,  !- Outlet Node Name
        0.36,                !- Rated Flow Rate {m3/s}
        179352,              !- Rated Pump Head {Pa}
        400,                 !- Rated Power Consumption {W}
        0.9,                 !- Motor Efficiency
        0.0,                 !- Fraction of Motor Inefficiencies to Fluid Stream
        0,                   !- Coefficient 1 of the Part Load Performance Curve
        1,                   !- Coefficient 2 of the Part Load Performance Curve
        0,                   !- Coefficient 3 of the Part Load Performance Curve
        0;                   !- Coefficient 4 of the Part Load Performance Curve

    Pump:VariableSpeed:Condensate,
        Steam Boiler Plant Steam Circ Pump,         !- Name
        Steam Boiler Plant Steam Supply Inlet Node, !- Inlet Node Name
        Steam Boiler Plant Steam Pump Outlet Node,  !- Outlet Node Name
        0.36,                !- Rated Flow Rate {m3/s}
        179352,              !- Rated Pump Head {Pa}
        autosize,            !- Rated Power Consumption {W}
        0.9,                 !- Motor Efficiency
        0.0,                 !- Fraction of Motor Inefficiencies to Fluid Stream
        0,                   !- Coefficient 1 of the Part Load Performance Curve
        1,                   !- Coefficient 2 of the Part Load Performance Curve
        0,                   !- Coefficient 3 of the Part Load Performance Curve
        0;                   !- Coefficient 4 of the Part Load Performance Curve
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Pump Electric Power [W]
    HVAC,Sum,Pump Electric Energy [J]
    HVAC,Average,Pump Shaft Power [W]
    HVAC,Average,Pump Fluid Heat Gain Rate [W]
    HVAC,Sum,Pump Fluid Heat Gain Energy [J]
    HVAC,Average,Pump Outlet Temperature [C]
    HVAC,Average,Pump Mass Flow Rate [kg/s]
    HVAC,Average, Pump Zone Total Heating Rate [W]
    HVAC, Sum, Pump Zone Total Heating Energy [J]
    HVAC, Average, Pump Zone Convective Heating Rate [W]
    HVAC, Average, Pump Zone Radiative Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Pump Electric Power [W]

#### Pump Electric Energy [J] 

These outputs are the electric power input to the pump motor. Consumption is metered on Pumps:Electricity, Electricity:Plant, and Electricity:Facility.

#### Pump Shaft Power [W]

This is the shaft power delivered from the motor to the pump.

#### Pump Fluid Heat Gain Rate [W]

#### Pump Fluid Heat Gain Energy [J] 

These outputs are the energy added to the fluid as heat. For the current algorithm, this is equal to Pump Shaft Power, because the loops are closed and all energy added to the fluid will ultimately become heat due to friction.

#### Pump Outlet Temperature [C]

#### Pump Mass Flow Rate [kg/s]

These outputs are the water outlet temperature and mass flow rate.

#### Pump Zone Total Heating Rate [W]

#### Pump Zone Total Heating Energy [J]

These outputs are the thermal losses from the pump to the surrounding [Zone](#zone).  They are only available if a [Zone](#zone) was named in the pump's input.  These indicate the amount of heat added to the zone from the pump's inefficiencies. They are the total heat loss including both convection and radiation.

#### Pump Zone Convective Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of convective gains directly to the zone air. It is only available if a [Zone](#zone) was named in the pump input.

#### Pump Zone Radiative Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of thermal radiation gains directly to the inside face of the zone's surfaces. It is only available if a [Zone](#zone) was named in the pump input.

## HeaderedPumps:ConstantSpeed

Headered pumps are components with more than one identical pumps connected in parallel. The syntax for [HeaderedPumps:ConstantSpeed](#headeredpumpsconstantspeed) is as follows.

### Inputs

#### Field: Name

This alpha field contains the identifying name for the pump bank.

#### Field: Inlet Node Name

This alpha field contains the identifying name for the pump bank inlet node.

#### Field: Outlet Node Name

This alpha field contains the identifying name for the pump bank outlet node.

#### Field: Total Rated Flow Rate

This numeric field contains the pump bank's rated volumetric flow rate in cubic meters per second. This is equal to the product of number of pumps and the flow rate of each individual pump. The field can be autosized.

#### Field: Number of Pumps in Bank

The numeric field specifies the number of pumps present in the pump bank. All these pumps are connected in parallel internally.

#### Field: Flow Sequencing Control Scheme

The alpha field specifies the scheme for sequencing flow. Currently the only choice is Sequential, where a pump in the pump bank will be turned ON only after fully loading the previous pump.

#### Field: Rated Pump Head

This numeric field contains the pump's rated head pressure in Pascals.

#### Field: Rated Power Consumption

This numeric field contains the pump bank's total power consumption in Watts. This field is equal to the product of number of pumps and the rated power consumption of each individual pump.  If the user is performing a pressure simulation on the loop in which this pump bank is found, this value would only be used to estimate pump efficiency.  During reported calculations, the pump would use the loop pressure drop and current flow conditions along with efficiency to calculate pump power dynamically.

#### Field: Motor Efficiency

This numeric field contains the pump's efficiency in decimal form (0 = 0%, 1 = 100%).

#### Field: Fraction of Motor Inefficiencies to Fluid Stream

This numeric field contains the pump's fraction of power loss to the fluid.

#### Field: Pump Control Type

This is a choice field of Continuous or Intermittent. The operation of a constant speed pump bank is fairly straightforward. If the user designates a constant speed pump bank that is operating continuously, the pump bank will run regardless of whether or not there is a load. This may have the net effect of adding heat to the loop if no equipment is turned on. If the pump bank is constant speed and operates intermittently, the pump bank will run at its capacity if a load is sensed and will shut off if there is no load on the loop. Applicable availability managers (ref. [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)) may override this control by forcing the pump to be on or off.

#### Field: Pump Flow Rate Schedule Name

The schedule values modify the Total Rated Flow Rate of the pump on a time basis. The default is that the pump is ON and runs according to its other operational requirements specified above. This schedule is for special, not typical pump operations.

#### Field: Zone Name

This field is optional. It can be used to input the name of the [Zone](#zone) in which the pump is located in the model.  If the pump is outdoors, or skin losses are not to be modeled, then leave this field blank.  If a valid [Zone](#zone) name is entered, then the portion of electrical power consumed by the pump that is not added to the working fluid is added to the surrounding [Zone](#zone).

#### Field: Skin Loss Radiative Fraction

This field is optional.  If a [Zone](#zone) is named in the previous field and pump losses are to be added to a surrounding thermal zone, then this input determines the split between thermal radiation and thermal convection for the heat losses from the pump.  If it is left blank then all the losses will be convective.

An example for constant speed pump bank follows.

~~~~~~~~~~~~~~~~~~~~

      HeaderedPumps:ConstantSpeed,
        Chilled Water Headered Pumps,  !- Name
        CW Supply Inlet Node,          !- Inlet Node Name
        CW Pumps Outlet Node,          !- Outlet Node Name
        autosize,                      !- Total Rated Flow Rate
        2,                             !- Number of Pumps in Bank
        SEQUENTIAL,                    !- Flow Sequencing Control Scheme
        179352,                        !- Rated Pump Head
        autosize,                      !- Rated Power Consumption
        0.9,                           !- Motor Efficiency
        0.0,                           !- Fraction of Motor Inefficiencies to Fluid Stream
        INTERMITTENT,                  !- Pump Control Type
        CoolingPumpAvailSched;         !- Pump Flow Rate Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Pump Electric Power [W]
    HVAC,Sum, Pump Electric Energy [J]
    HVAC,Average,Pump Shaft Power [W]
    HVAC,Average, Pump Fluid Heat Gain Rate [W]
    HVAC,Sum, Pump Fluid Heat Gain Energy [J]
    HVAC,Average, Pump Outlet Temperature [C]
    HVAC,Average,Pump Mass Flow Rate [kg/s]
    HVAC,Average,Number of Pumps Operating []
    HVAC, Average, Pump Zone Total Heating Rate [W]
    HVAC, Sum, Pump Zone Total Heating Energy [J]
    HVAC, Average, Pump Zone Convective Heating Rate [W]
    HVAC, Average, Pump Zone Radiative Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Pump Electric Power [W]

#### Pump Electric Energy [J] 

These outputs are the electric power input to the pump motor. Consumption is metered on Pumps:Electricity, Electricity:Plant, and Electricity:Facility.

#### Pump Shaft Power [W]

This is the shaft power delivered from the motor to the pump.

#### Pump Fluid Heat Gain Rate [W]

#### Pump Fluid Heat Gain Energy [J] 

These outputs are the energy added to the fluid as heat. For the current algorithm, this is equal to Pump Shaft Power, because the loops are closed and all energy added to the fluid will ultimately become heat due to friction.

#### Pump Outlet Temperature [C]

#### Pump Mass Flow Rate [kg/s]

These outputs are the water outlet temperature and mass flow rate.

#### Pump Operating Pumps Count []

This output tells the number of pumps in the pump bank operating at any reporting frequency.

#### Pump Zone Total Heating Rate [W]

#### Pump Zone Total Heating Energy [J]

These outputs are the thermal losses from the pump to the surrounding [Zone](#zone).  They are only available if a [Zone](#zone) was named in the pump's input.  These indicate the amount of heat added to the zone from the pump's inefficiencies. They are the total heat loss including both convection and radiation.

#### Pump Zone Convective Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of convective gains directly to the zone air. It is only available if a [Zone](#zone) was named in the pump input.

#### Pump Zone Radiative Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of thermal radiation gains directly to the inside face of the zone's surfaces. It is only available if a [Zone](#zone) was named in the pump input.

## HeaderedPumps:VariableSpeed

Headered pumps are components with more than one identical pumps connected in parallel. Currently this object may be found only in plant loops. The syntax for a [HeaderedPumps:VariableSpeed](#headeredpumpsvariablespeed) pump is given below.

### Inputs

#### Field: Name

This alpha field contains the identifying unique name for the pump bank.

#### Field: Inlet Node Name

This alpha field contains the identifying name for the pump bank inlet node.

#### Field: Outlet Node Name

This alpha field contains the identifying name for the pump bank outlet node.

#### Field: Total Rated Flow Rate

This numeric field contains the pump bank's rated volumetric flow rate in cubic meters per second. This is equal to the product of number of pumps and the flow rate of each individual pump. The field can be autosized.

#### Field: Number of Pumps in Bank

The numeric field specifies the number of pumps present in the pump bank. All these pumps are connected in parallel internally.

#### Field: Flow Sequencing Control Scheme

The alpha field specifies the scheme for sequencing flow. Currently the only choice is Sequential, where a pump in the pump bank will be turned ON only after fully loading the previous pump.

#### Field: Rated Pump Head

This numeric field contains the pump's rated head in Pascals.

#### Field: Rated Power Consumption

This numeric field contains the pump bank's total power consumption in Watts. This field is equal to the product of number of pumps and the rated power consumption of each individual pump.  If the user is performing a pressure simulation on the loop in which this pump bank is found, this value would only be used to estimate pump efficiency.  During reported calculations, the pump would use the loop pressure drop and current flow conditions along with efficiency to calculate pump power dynamically.

#### Field: Motor Efficiency

This numeric field contains the efficiency of the individual pump (which will be equivalent to pump bank efficiency) in decimal form (0 = 0%, 1 = 100%).

#### Field: Fraction of Motor Inefficiencies to Fluid Stream

This numeric field contains the individual pump's fraction of power loss to the fluid.

#### Field: Coefficient 1 of the Part Load Performance Curve

This numeric field contains the first coefficient (*C~1~*) **in the part load ratio curve. The fraction of full load power is determined by the cubic equation:

![](media/image335.png)\


where C~1~,C~2~,C~3~,and C~4~ are Coefficients 1 – 4 (below) and PLR is the Part Load Ratio.

#### Field: Coefficient 2 of the Part Load Performance Curve

This numeric field contains the second coefficient (*C~2~*) **in the part load ratio curve.

#### Field: Coefficient 3 of the Part Load Performance Curve

This numeric field contains the third coefficient (*C~3~*) **in the part load ratio curve.

#### Field: Coefficient 4 of the Part Load Performance Curve

This numeric field contains the fourth coefficient (*C~4~*) **in the part load ratio curve.

#### Field: Minimum Flow Rate

The numeric field specifies the minimum flow allowed for the pump bank as a fraction of the nominal flow. If the requested flow is less than minimum flow pump bank runs at minimum flow.

#### Field: Pump Control Type

This is a choice field of Continuous or Intermittent. A variable speed pump bank is defined with maximum and minimum flow rates that are the physical limits of the device. If there is no load on the loop and the pump bank is operating intermittently, then the bank can shutdown. For any other condition such as the loop having a load and the pump bank is operating intermittently or the pump bank is continuously operating (regardless of the loading condition), the bank will operate and select a flow somewhere between the minimum and maximum limits. In these cases where the pump bank is running, it will try to meet the flow request made by demand side components. Applicable availability managers (ref. [AvailabilityManagerAssignmentList](#availabilitymanagerassignmentlist)) may override this control by forcing the pump to be on or off.

#### Field: Pump Flow Rate Schedule Name

Modifies the Rated Volumetric Flow Rate of the pump on a time basis. The default is that the pump is ON and runs according to its other operational requirements specified above. This schedule is for special, not typical pump operations.

#### Field: Zone Name

This field is optional. It can be used to input the name of the [Zone](#zone) in which the pump is located in the model.  If the pump is outdoors, or skin losses are not to be modeled, then leave this field blank.  If a valid [Zone](#zone) name is entered, then the portion of electrical power consumed by the pump that is not added to the working fluid is added to the surrounding [Zone](#zone).

#### Field: Skin Loss Radiative Fraction

This field is optional.  If a [Zone](#zone) is named in the previous field and pump losses are to be added to a surrounding thermal zone, then this input determines the split between thermal radiation and thermal convection for the heat losses from the pump.  If it is left blank then all the losses will be convective.

An example of this object follows.

~~~~~~~~~~~~~~~~~~~~

      HeaderedPumps:VariableSpeed,
        Chilled Water Headered Pumps, !- Name
        CW Supply Inlet Node,    !- Inlet Node Name
        CW Pumps Outlet Node,    !- Outlet Node Name
        autosize,                !- Total Rated Flow Rate {m3/s}
        2,                       !- Number of Pumps in Bank
        SEQUENTIAL,              !- Flow Sequencing Control Scheme
        179352,                  !- Rated Pump Head {Pa}
        autosize,                !- Rated Power Consumption {W}
        0.9,                     !- Motor Efficiency
        0.0,                     !- Fraction of Motor Inefficiencies to Fluid Stream
        0,                       !- Coefficient 1 of the Part Load Performance Curve
        1,                       !- Coefficient 2 of the Part Load Performance Curve
        0,                       !- Coefficient 3 of the Part Load Performance Curve
        0,                       !- Coefficient 4 of the Part Load Performance Curve
        0.1,                     !- Minimum Flow Rate {m3/s}
        INTERMITTENT,            !- Pump Control Type
        CoolingPumpAvailSched;   !- Pump Flow Rate Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Pump Electric Power [W]
    HVAC,Sum, Pump Electric Energy [J]
    HVAC,Average,Pump Shaft Power [W]
    HVAC,Average, Pump Fluid Heat Gain Rate [W]
    HVAC,Sum, Pump Fluid Heat Gain Energy [J]
    HVAC,Average, Pump Outlet Temperature [C]
    HVAC,Average,Pump Mass Flow Rate [kg/s]
    HVAC,Average,Number of pumps operating []
    HVAC, Average, Pump Zone Total Heating Rate [W]
    HVAC, Sum, Pump Zone Total Heating Energy [J]
    HVAC, Average, Pump Zone Convective Heating Rate [W]
    HVAC, Average, Pump Zone Radiative Heating Rate [W]
~~~~~~~~~~~~~~~~~~~~

#### Pump Electric Power [W]

#### Pump Electric Energy [J] 

These outputs are the electric power input to the pump motor. Consumption is metered on Pumps:Electricity, Electricity:Plant, and Electricity:Facility.

#### Pump Shaft Power [W]

This is the shaft power delivered from the motor to the pump.

#### Pump Fluid Heat Gain Rate [W]

#### Pump Fluid Heat Gain Energy [J] 

These outputs are the energy added to the fluid as heat. For the current algorithm, this is equal to Pump Shaft Power, because the loops are closed and all energy added to the fluid will ultimately become heat due to friction.

#### Pump Outlet Temperature [C]

#### Pump Mass Flow Rate [kg/s]

These outputs are the water outlet temperature and mass flow rate.

#### Pump Operating Pumps Count []

This output tells the number of pumps in the pump bank operating at any reporting frequency.

#### Pump Zone Total Heating Rate [W]

#### Pump Zone Total Heating Energy [J]

These outputs are the thermal losses from the pump to the surrounding [Zone](#zone).  They are only available if a [Zone](#zone) was named in the pump's input.  These indicate the amount of heat added to the zone from the pump's inefficiencies. They are the total heat loss including both convection and radiation.

#### Pump Zone Convective Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of convective gains directly to the zone air. It is only available if a [Zone](#zone) was named in the pump input.

#### Pump Zone Radiative Heating Rate [W]

This output is the thermal loss from the pump to the surrounding [Zone](#zone) in the form of thermal radiation gains directly to the inside face of the zone's surfaces. It is only available if a [Zone](#zone) was named in the pump input.