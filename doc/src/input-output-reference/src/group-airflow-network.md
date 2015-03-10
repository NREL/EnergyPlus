# Group – Airflow Network

## Overview

The Airflow Network model provides the ability to simulate multizone airflows driven by wind and also by a forced air distribution system. The model can also simulate the heat and moisture gains or losses from the air distribution system itself (e.g., ductwork). When modeling an air distribution system, the current version of the Airflow Network model is restricted to a single forced air system (i.e., [AirLoopHVAC](#airloophvac) object) with a constant volume supply air fan ([Fan:ConstantVolume](#fanconstantvolume) or [Fan:OnOff](#fanonoff)). The capabilities of the model are to:

- Simulate zone pressures due to envelope leakage and forced air distribution during HVAC system fan operation
- Simulate node pressures in a forced air distribution system during HVAC system fan operation
- Calculate multizone airflows due to forced air, wind, and surface leakage, including adjacent zones and outdoors, during HVAC system fan operation
- Simulate distribution system airflows, including supply and return air leaks, during HVAC system fan operation
- Simulate air distribution system node temperatures and humidity ratios during HVAC system fan operation
- Calculate duct conduction losses during HVAC system fan operation
- Calculate vapor diffusion losses of ducts during HVAC system fan operation
- Calculate sensible and latent loads on the surrounding zones due to supply and return air leaks in the air distribution system during HVAC system fan operation
- Simulate zone pressures due to envelope leakage driven by wind when the HVAC system fan is off or if no air distribution system is specified
- Calculate multizone airflows due to wind and surface leakage, including adjacent zones and outdoors when the HVAC system fan is off or if no air distribution system is specified
- Allow zone exhaust fans to be included as part of the airflow network
- For airflow networks with a forced air distribution system, calculate zone sensible and latent loads for two different supply air fan operation modes as required: cycling fan, cycling compressor (CyclingFanAndCompressor) and continuous fan, cycling compressor (ContinuousFanWithCyclingCompressor)

## Summary of Objects

Before describing the Airflow Network input objects in detail, we list all of the objects and then give a short description of what the objects do.

The Airflow Network input objects are:

**AirflowNetwork:SimulationControl**

**AirflowNetwork:Multizone:Zone**

**AirflowNetwork:Multizone:Surface**

**AirflowNetwork:MultiZone:Component:SimpleOpening**

**AirflowNetwork:MultiZone:Component:DetailedOpening**

**AirflowNetwork:MultiZone:Component:HorizontalOpening**

**AirflowNetwork:MultiZone:Surface:Crack**

**AirflowNetwork:MultiZone:ReferenceCrackConditions**

**AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea**

**AirflowNetwork:MultiZone:Component:ZoneExhaustFan**

**AirflowNetwork:MultiZone:ExternalNode**

**AirflowNetwork:MultiZone:WindPressureCoefficientArray**

**AirflowNetwork:MultiZone:WindPressureCoefficientValues**

**AirflowNetwork:Distribution:Node**

**AirflowNetwork:Distribution:Component:Leak**

**AirflowNetwork:Distribution:Component:LeakageRatio**

**AirflowNetwork:Distribution:Component:Duct**

**AirflowNetwork:Distribution:Component:ConstantPressureDrop**

**AirflowNetwork:Distribution:Component:ConstantVolumeFan**

**AirflowNetwork:Distribution:Component:Coil**

**AirflowNetwork:Distribution:Component:HeatExchanger**

**AirflowNetwork:Distribution:Component:TerminalUnit**

**AirflowNetwork:Distribution:Linkage**

- **AirflowNetwork:SimulationControl** defines basic run parameters for the air flow calculations and specifies whether wind pressure coefficients are input by the user or, for rectangular buildings, calculated by the program.
- The **AirflowNetwork:Multizone:Zone** object specifies the ventilation control that applies to all of the openable exterior and interior windows and doors in the corresponding thermal zone. Surface-level ventilation control can be used to override the zone-level ventilation control if required (see AirflowNetwork: Multizone:Surface object below).
- **AirflowNetwork:Multizone:Surface** object indicates whether a heat transfer surface (wall, window, etc.) has a crack or opening and references an **AirflowNetwork:MultiZone:Surface:Crack,AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea,AirflowNetwork:MultiZone:Component:SimpleOpening,AirflowNetwork:MultiZone:Component:DetailedOpening,AirflowNetwork:MultiZone:Component:HorizontalOpeningor AirflowNetwork:MultiZone:Component:ZoneExhaustFan** object that gives the air flow characteristics of that crack, opening, or zone exhaust fan. The AirflowNetwork:Multizone:Surface object can also be used to specify individual ventilation control for openable exterior and interior windows and doors.
- **AirflowNetwork:MultiZone:ReferenceCrackConditions** is used to normalize crack information that is based on measurements of crack air flow.
- **AirflowNetwork:Distribution:Node** represents air distribution system nodes for the AirflowNetwork model. A set of an [AirLoopHVAC](#airloophvac) and [ZoneHVAC:EquipmentList](#zonehvacequipmentlist) nodes is a subset of the AirflowNetwork:Distribution:Nodes.
- **AirflowNetwork:Distribution:Component** objects consist of **Leak, LeakageRatio, [Duct](#duct), ConstantPressureDrop, ConstantVolumeFan, Coil,** and **TerminalUnit.** The components provide a relationship between pressure and airflow. The **Leak** and **Leakage** components can be used to simulate supply and/or return leaks in an air distribution system**.** The **[Duct](#duct)** and **ConstantPressureDrop components** can be used to deliver forced air into conditioned spaces. The ****components **ConstantVolumeFan, Coil, and TerminalUnit** reference normal EnergyPlus objects. The Airflow Network model gets information from these objects to perform an airflow network simulation.
- **AirflowNetwork:Distribution:Linkage** object represents a connection between two node objects and an AirflowNetwork component. The node objects can be an [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode), [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) or AirflowNetwork:Multizone:Zone.

If you input wind pressure coefficients, **AirflowNetwork:Multizone:Surface** also has an associated **AirflowNetwork:MultiZone:ExternalNode**, that, via the **AirflowNetwork:Multizone:WindPressureCoefficientArray** and **AirflowNetwork:MultiZone:WindPressureCoefficientValues** objects, gives the wind pressure distribution vs. wind direction for that node and, implicitly, for the cracks and openings in the exterior surfaces associated with that node.

Figure 93 shows the relationships among AirflowNetwork:Multizone objects and between AirflowNetwork:Multizone objects and regular EnergyPlus objects. In this figure an arrow from object A to object B means A references B, i.e., one of the inputs in A is the name of object B. For example, one input for AirflowNetwork: Multizone:Surface is the name of a heat transfer surface, and another is the name of a crack or opening object. The arrow between AirflowNetwork:Multizone:Surface and [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) is shown dashed to indicate that this reference is not used when wind pressure coefficients are calculated by the program rather than being input by the user.

Figure 93 also shows the relationships among AirflowNetwork:Distribution objects and between AirflowNetwork:Distribution objects and regular EnergyPlus objects. The [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) objects link two nodes from [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) and/or AirflowNetwork:Multizone:[Zone](#zone) objects with a component defined in the object AirflowNetwork:Distribution:Component. The solid arrows show a reference from object A to object B. The dashed arrows indicate the components which can be used in a linkage object. The red arrows pointing to the [Zone](#zone) object indicate the components that interact with the zone air. For example, the temperature in a zone where a supply leak terminates is used to calculate duct leakage energy loss. The temperature in a zone where a duct component is located is also used to calculate duct conduction loss.

![Relationships among AirflowNetwork objects (right-hand side) and between AirflowNetwork objects and regular EnergyPlus objects. An arrow from object A to object B means that A references B.](media/relationships-among-airflownetwork-objects.jpeg)


Much of the information needed for the air flow calculation is automatically extracted from the building description for thermal modeling. This includes things like the volume and neutral height of the zones, and the orientation and location of the building surfaces that contain cracks or openings through which air flows. From all of this information the program creates a "pressure-flow network" that is solved each timestep using iterative solution methods to obtain the unknown pressures and air flows.

Figure 94 shows a plan view of a very simple air flow network that you can construct using the above AirflowNetwork objects. There are three thermal zones, Zone-1, Zone-2 and Zone-3. There are openable exterior windows—Window-1, Window-2 and Window-3—and openable interior doors—Door-12 and Door-23. Two External Nodes are indicated. ExternalNode-1 is associated with the façade that contains Window-1 and Window-2. ExternalNode-2 is associated with the façade containing Window-3.

One possible air flow pattern is shown in this figure. The actual air flow pattern in a particular timestep, and the size of the flows, depends on many factors, such as (1) What is the wind pressure distribution seen by the exterior windows? (2) Are the exterior windows open or closed, and if open, how far are they open? (3) Are the interior doors open or closed? (4) What are the air temperature differences between zones and between zones and the outdoor air (which affect buoyancy flows)?

![Plan view of a simple air flow network showing a possible air flow pattern in which all of the windows and doors are open.](media/plan-view-of-a-simple-air-flow-network.png)


Figure 94 shows a possible air flow pattern in which all of the windows and doors are open. Associated with the external nodes are wind pressure coefficient distributions as a function of wind direction that are input using two AirflowNetwork:Multizone:Wind Pressure Coefficient objects. The nature of the air flows through the windows and doors is specified using [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening) and [AirflowNetwork:MultiZone:Component:SimpleOpening](#airflownetworkmultizonecomponentsimpleopening) objects. The Airflow Network model calculates the flows each system timestep depending on various factors, including wind direction and speed, size and vertical position of openings, outdoor air temperature, and zone air temperatures.

## Airflow Network Example Files (included in the installation)

AirflowNetwork3zVent.idf

AirflowNetwork3zVentAutoWPC.idf

AirflowNetwork_Simple_House.idf

AirflowNetwork_Simple_SmallOffice.idf

AirflowNetwork_Multizone_House.idf

AirflowNetwork_MultiZone_House_OvercoolDehumid.idf

AirflowNetwork_Multizone_House_TwoSpeed.idf

AirflowNetwork_Multizone_SmallOffice.idf

AirflowNetwork_Multizone_SmallOffice_CoilHXAssistedDX.idf

AirflowNetwork_MultiZone_SmallOffice_GenericContam.idf

AirflowNetwork_Multizone_SmallOffice_HeatRecoveryHXSL.idf

AirflowNetwork_Multizone_SmallOffice_VAV.idf

AirflowNetwor_Multizone_HorizontalOpening.idf

CrossVent_1Zone_AirflowNetwork.idf

DisplacementVent_Nat_AirflowNetwork.idf

HybridVentilationControl.idf

## What the Airflow Network Model Can and Cannot Do

Here is a list of some of the things that the Airflow Network calculation can and cannot model.

### Can Do

#. Air flow through cracks in exterior or interzone surfaces.
#. Air flow through cracks around windows and doors when closed.
#. Natural ventilation (i.e., air flow through open or partially open exterior windows and doors).
#. [Zone](#zone) level control of natural ventilation (all windows/doors in a zone that are defined with a component opening object have identical controls).
#. Individual surface control of natural ventilation for a subsurface (window, door, or glassdoor).
#. Modulation of natural ventilation to prevent large zone air temperature swings.
#. Interzone air flow (i.e., air flow through open interzone windows and doors, and through cracks in interzone surfaces).
#. Dependence of air flow on buoyancy effects and wind pressure.
#. Dependence of wind pressure on wind speed, wind direction and surface orientation.
#. Supply and return air leaks in an air distribution system.
#. Account for the effect of supply-air and/or return-air leakage on zone pressure when a forced air distribution system is present and is operating.
#. When duct leakage is modeled and the HVAC system is on, interzone airflow or infiltration/exfiltration can occur due to changes in zone pressure.
#. Bi-directional flow through large openings. See discussion below under [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening), AirflowNetwork:Multizone:Component:HorizontalOpening, and AirflowNetwork:Multizone:Component:SimpleOpening.
#. Calculate air flows and pressures in ducts or other components of a forced air distribution system.
#. Calculate zone loads when the supply air fan cycles on and off during a system timestep using the CyclingFanAndCompressor fan operation mode ([Fan:OnOff](#fanonoff)).
#. Determine the impact of zone exhaust fans on air flows, pressures, air temperatures/humidity levels and energy consumption.

### Cannot Do or Restricted

#. The model is restricted to using a constant volume fan ([Fan:ConstantVolume](#fanconstantvolume) and [Fan:OnOff](#fanonoff)) and cannot model variable volume fan equipment.
#. Air circulation and/or air temperature stratification within a thermal zone. For example, you should not try to divide a high space, such as an atrium, into subzones separated by artificial horizontal surfaces that have cracks or openings with the expectation that AirflowNetwork will give you a realistic temperature in each subzone and/or a realistic air flow between subzones.
#. The model is restricted to eleven types of coils that can be in the air distribution system ([Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed), [Coil:Heating:Gas](#coilheatinggas), [Coil:Heating:Electric](#coilheatingelectric),  [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed), [Coil:Cooling:Water](#coilcoolingwater), [Coil:Heating:Water](#coilheatingwater), [Coil:Cooling:Water:DetailedGeometry](#coilcoolingwaterdetailedgeometry), [Coil:Cooling:DX:TwoStageWithHumidityControlMode](#coilcoolingdxtwostagewithhumiditycontrolmode), [Coil:Cooling:DX:MultiSpeed](#coilcoolingdxmultispeed), [Coil:Heating:DX:MultiSpeed](#coilheatingdxmultispeed), and [Coil:Heating:Desuperheater](#coilheatingdesuperheater)).
#. The model is restricted to a single type of air distribution equipment terminal units ([AirTerminal:SingleDuct:ConstantVolume:Reheat](#airterminalsingleductconstantvolumereheat)).
#. Pollutant transport cannot currently be modeled.
#. Supply and return leaks are not allowed in an [AirLoopHVAC](#airloophvac). They can only be modeled in the [Zone](#zone) Equipment portion of the air loop (i.e., return leaks may be modeled between the zone return node and the zone mixer inlet or the zone mixer outlet and the zone equipment loop outlet; and supply leaks may be modeled between the zone equipment loop inlet and the [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) inlet node or the [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) outlet node and the zone supply node).
#. An air distribution system must be located inside the building (i.e., the ducts must pass through zones within the building).
#. [Zone](#zone) exhaust fans must be defined in [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) objects.

**The input specifications consist of five main sections: [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object, AirflowNetwork multizone data objects, AirflowNetwork distribution node objects, AirflowNetwork distribution component objects,** and **AirflowNetwork distribution linkage objects.** Each of these object types is described in detail below.

## AirflowNetwork:SimulationControl

The basic run parameters for this model are defined in this unique object which has the following input specifications:

### Inputs

#### Field: Name

This is a unique character string associated with this instance of the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object. At this time, only one [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object can be specified in an input data file (idf).

#### **Field: AirflowNetwork Control**

The following selections are available to control the Airflow Network simulation:

**MultizoneWithDistribution:** Multizone air flow calculations are performed during all simulation timesteps, including the impacts of the air distribution system when a HVAC system fan is operating. Any **ZoneInfiltration:\***, **ZoneVentilation:\***, **ZoneMixing** and **ZoneCrossMixing** objects specified in the input data file are not simulated.

**MultizoneWithoutDistribution**: Multizone air flow calculations are performed during all simulation timesteps, but the air distribution system portion of the network is not modeled even if it is specified in the input data file. Any **ZoneInfiltration:\***, **ZoneVentilation:\***, **ZoneMixing** and **ZoneCrossMixing** objects specified in the input data file are not simulated.

**MultizoneWithDistributionOnlyDuringFanOperation:** Multizone air flow calculations, including the impacts of the air distribution system, are only performed when the HVAC system fan is operating. Any **ZoneInfiltration:\***, **ZoneVentilation:\***, **ZoneMixing** and **ZoneCrossMixing** objects specified in the input data file are used when the HVAC system fan is OFF (if none are specified, then no air flow calculations are performed when the fan is OFF).

**NoMultizoneOrDistribution:** No multizone air flow calculations (with or without the air distribution system portion of the network) are performed during the simulation. Any **ZoneInfiltration:\***, **ZoneVentilation:\***, **ZoneMixing** and **ZoneCrossMixing** objects specified in the input data file are simulated (if none are specified, then no air flow calculations are performed). Note: Having an input data file with no [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) objects gives the same impact – no multizone air flow calculations. However, this choice is provided as a convenience to the user to easily disable the multizone air flow calculations for an input data file that already contains AirflowNetwork objects.

> **Note:** A **ZoneInfiltration:\*** object ****indicates any one of **ZoneInfiltration:DesignFlowRate**, **ZoneInfiltration:EffectiveLeakageArea**,and **ZoneInfiltration:FlowCoefficient** objects. A object of **ZoneVentilation:\*** indicates any one of **ZoneVentilation:DesignFlowRate** and **ZoneVentilation:WindandStackOpenArea** objects**.**

#### Field: Wind Pressure Coefficient Type

Determines whether the wind pressure coefficients are input by the user or calculated. The choices are **Input** or **SurfaceAverageCalculation**, with the default being SurfaceAverageCalculation.

If INPUT, you must enter an [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray) object, one or more [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) objects, and one or more [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) objects.

The second choice, SurfaceAverageCalculation, should only be used for **rectangular** buildings. In this case surface-average wind pressure coefficients vs. wind direction are calculated by the program for the four vertical facades and the roof based on user entries for "[Building](#building) Type," "Azimuth Angle of Long Axis of [Building](#building)," and "Ratio of [Building](#building) Width Along Short Axis to Width Along Long Axis" (see description of these fields below). With this choice you do **not** have to enter any of the following objects: AirflowNetwork:MultiZone: Wind Pressure Coefficient Array, [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) and [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues).

#### Field: AirflowNetwork Wind Pressure Coefficient Array Name

This is the name of the [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray) object that contains wind directions corresponding to the wind pressure coefficients given in the [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) objects.

A value need be entered only if Wind Pressure Coefficient Type = INPUT (see description of previous field).

#### Field: Height Selection for Local Wind Pressure Calculation

Determines whether the local wind pressure is calculated based on either given external node heights or surface opening heights. The choices are **ExternalNode** or **OpeningHeight**, with the default being OpeningHeight. The local outdoor wind speed calculation procedure is given in the section of "Local Wind Speed Calculation" in the Engineering Reference. The calculation procedure requires the height input.

If **ExternalNode**, the heights given in the [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) objects are used to calculate local wind pressures based on the given height local wind speed.  Used only if Wind Pressure Coefficient Type = INPUT (see description of previous field).

If **OpeningHeight**, the number of the [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) objects has to be equal to the number of external surfaces defined in the AirflowNetwork:MultiZone:Surface objects. The centroids in the z direction of the AirflowNetwork:MultiZone:Surface objects are the heights used in the local wind pressure calculation with the given height wind speed. The input is required if Wind Pressure Coefficient Type = INPUT (see description of previous field).

**If Wind Pressure Coefficient Type = SurfaceAverageCalculation**, a value in this field is not required and a blank may be entered. The default choice is used internally to generate the [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) objects ****

#### Field: Building Type

Used only if Wind Pressure Coefficient Type = SurfaceAverageCalculation. The choices for [Building](#building) Type are LowRise and HighRise, with the default being LowRise.

LowRise corresponds to a rectangular building whose height is less than three times the width of the footprint (*w~short~* in Figure 95) and is less than three times the length of the footprint (*w~long~*~~in the same figure).

HighRise corresponds to a rectangular building whose height is more than three times the width of the footprint (*w~short~* in Figure 95) or is more than three times the length of the footprint (*w~long~*~~in the same figure).

#### Field: Maximum Number of Iterations

The maximum number of iterations allowed in finding an AirflowNetwork solution. If the number of iterations at each simulation timestep is above the maximum number of iterations defined by this field, the program could not find the solution and a Severe error is issued and the program is aborted. The default value is 500.

#### Field: Initialization Type

Designates which method is used for AirflowNetwork initialization. The choices for Initialization Type are LinearInitializationMethod and ZeroNodePressures, with the default being ZeroNodePressures.

#### Field: Relative Airflow Convergence Tolerance

The solution is assumed to have converged when ![](media/image211.png) is less than the value specified for this input field. This convergence criteria is equivalent to the ratio of the absolute value of the sum of all network airflows (![](media/image212.png) ) to the sum of network airflow magnitudes (![](media/image213.png) ). The default value is 1.0x10^-4^.

#### Field: Absolute Airflow Convergence Tolerance

The solution is assumed to have converged when the summation of the absolute value of all network airflows (![](media/image214.png) ) is less than the value specified for this input field. The default value is 1.0x10^-6^.

#### Field: Convergence Acceleration Limit

If the ratio of successive pressure corrections is less than this limit, use Steffensen acceleration algorithm (Ref. AirflowNetwork Model in the EnergyPlus Engineering Reference). The range for this field is -1 to 1, with the default value being -0.5.

#### Field: Azimuth Angle of Long Axis of Building

Gives the orientation of a rectangular building for calculating wind pressure coefficients. This is the smaller of the angles, measured clockwise, between North and the long axis of the building (see Figure 95). Used only if Wind Pressure Coefficient Type = SurfaceAverageCalculation. The range for this input is 0 to 180, with the default value being 0.

#### Field: Ratio of Building Width Along Short Axis to Width Along Long Axis

This is the aspect ratio of a rectangular footprint. It is given by the width of the footprint along its short axis divided by the width along the long axis (see Figure 95). If the footprint is square, the value of this field is 1.0. Used only if Wind Pressure Coefficient Type = SurfaceAverageCalculation. The range for this input is > 0 to 1, with the default value being 1.

![Footprint of a rectangular building showing variables used by the program to calculate surface-average wind pressure coefficients. The angle  is the "Azimuth Angle of Long Axis of Building."  w~short~/w~long~ is the "Ratio of Building Width Along Short Axis to Width Along Long Axis."](media/footprint-of-a-rectangular-building-showing.png)


#### Field: Height Dependence of External Node Temperature

This is an optional field. Input is Yes or No. The default is No. Yes is that external node temperature is dependent on node height. No means that external node temperature is calculated with zero height.

An IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:SimulationControl,
        AirflowNetwork_All,      !- Name
        MultiZoneWithDistribution,  !- AirflowNetwork Control
        Input,                   !- Wind Pressure Coefficient Type
        Every 30 Degrees,        !- AirflowNetwork Wind Pressure Coefficient Array Name
        OpeningHeight,           !- Height Selection for Local Wind Speed Calculation
        LowRise,                 !- Building Type
        500,                     !- Maximum Number of Iterations {dimensionless}
        ZeroNodePressures,       !- Initialization Type
        1.0E-05,                 !- Relative Airflow Convergence Tolerance {dimensionless}
        1.0E-06,                 !- Absolute Airflow Convergence Tolerance {kg/s}
        -0.5,                    !- Convergence Acceleration Limit {dimensionless}
        0.0,                     !- Azimuth Angle of Long Axis of Building {deg}
        1.0;                     !- Ratio of Building Width Along Short Axis to Width Along Long Axis
~~~~~~~~~~~~~~~~~~~~

AirflowNetwork:Multizone data objects are used to calculate multizone airflows. This section describes the input requirements for the following objects:

AirflowNetwork:MultiZone:Zone

AirflowNetwork:Multizone data objects are used to calculate multizone airflows. This section describes the input requirements for the following objects:

AirflowNetwork:MultiZone:Zone

AirflowNetwork:MultiZone:Surface

AirflowNetwork:MultiZone:Surface:Crack

AirflowNetwork:MultiZone:ReferenceCrackConditions

AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea

AirflowNetwork:MultiZone:Component:DetailedOpening

AirflowNetwork:MultiZone:Component:SimpleOpening

AirflowNetwork:MultiZone:Component:HorizontalOpening

AirflowNetwork:MultiZone:Component:ZoneExhaustFan

AirflowNetwork:MultiZone:ExternalNode

AirflowNetwork:MultiZone:WindPressureCoefficientArray

AirflowNetwork:MultiZone:WindPressureCoefficientValues

A detailed description for each of these objects is provided below.

## AirflowNetwork:Multizone:Zone

This object allows control of natural ventilation through exterior and interior openings in a zone, where "opening" is defined as an openable window or door. (Note that only window, door or glass door subsurfaces in a zone that are specified using [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening), [AirflowNetwork:MultiZone:Component:HorizontalOpening](#airflownetworkmultizonecomponenthorizontalopening) or AirflowNetwork:Multizone:Component:SimpleOpening and have an associated AirflowNetwork:Multizone:Surface object are considered to be openings). The control will be applied in the same way to all of the openings in the zone.

This object is required to perform Airflow Network calculations. Note that ventilation control for all openings is provided at the zone level as default and individual ventilation control of a surface opening can be used to override the zone-level control (see the AirflowNetwork:Multizone:Surface object description below).

### Field: Zone Name

The name of the EnergyPlus thermal zone corresponding to the AirflowNetwork zone.

### Field: Ventilation Control Mode

Specifies the type of zone-level natural ventilation control.

Let T~out~ equal the outdoor air temperature, T~zone~ equal the previous timestep's zone air temperature, T~set~ equal the Vent Temperature Schedule value, H~zone~ equal the specific enthalpy of zone air from the previous timestep, and H~out~ equal the specific enthalpy of outdoor air. Then the four allowed choices for Ventilation Control Mode are:

**NoVent**: All of the zone's openable windows and doors are closed at all times independent of indoor or outdoor conditions. The Venting Availability Schedule is ignored in this case. This is the default value for this field.

**Temperature**: All of the zone's openable windows and doors are opened if T~zone~ > T~out~ **and** T~zone~ > T~set~ **and** Venting Availability Schedule (see below) allows venting.

**Enthalpy:** All of the zone's openable windows and doors are opened if H~zone~ > H~out~ **and** T~zone~ > T~set~ **and** Venting Availability Schedule allows venting.

**Constant**: Whenever this object's Venting Availability Schedule allows venting, all of the zone's openable windows and doors are open, independent of indoor or outdoor conditions. Note that "Constant" here means that the size of each opening is fixed while venting; the air flow through each opening can, of course, vary from timestep to timestep.

**ASHRAE55Adaptive**: All of the zone's operable windows and doors are opened if the operative temperature is greater than the comfort temperature (central line) calculated from the ASHRAE Standard 55-2010 adaptive comfort model and ****Venting Availability Schedule allows venting.

**CEN15251Adaptive:** All of the zone's operable windows and doors are opened if the operative temperature is greater than the comfort temperature (central line) calculated from the CEN15251 adaptive comfort model and ****Venting Availability Schedule allows venting.

### Field: Ventilation Control Zone Temperature Setpoint Schedule Name

The name of a schedule of zone air temperature set points that controls the opening of windows and doors in the thermal zone to provide natural ventilation. This setpoint is the temperature above which all the openable windows and doors in the zone will be opened if the conditions described in the previous field Ventilation Control Mode are met.

The Ventilation Control [Zone](#zone) Temperature Setpoint Schedule Name applies only to windows and doors in the zone that are specified using [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening), [AirflowNetwork:MultiZone:Component:HorizontalOpening](#airflownetworkmultizonecomponenthorizontalopening) or [AirflowNetwork:MultiZone:Component:SimpleOpening](#airflownetworkmultizonecomponentsimpleopening) and have an associated AirflowNetwork:MultiZone:Surface object.

(The discussion under the field Window/Door Opening Factor in the AirflowNetwork:MultiZone:Surface object describes how the actual opening area of a window or door in a particular timestep is determined.)

*Modulation of Openings*

The following five fields can be used to modulate the window/door openings when Ventilation Control Mode = Temperature or Enthalpy. These fields determine a factor between 0 and 1 that multiplies the opening factor of each window and door in the zone according to the control action shown in Figure 96 for Ventilation Control Mode = Temperature and in Figure 97 for Ventilation Control Mode = Enthalpy. Modulation of the openings can reduce the large temperature swings that can occur if the windows/doors are open too far when they are venting, especially when there is a large inside-outside temperature difference.

The modulation takes the following form when Ventilation Control Mode = Temperature:

T~zone~ - T~out~    **[Lower Value on Inside/Outside Temperature Difference for Modulating the Venting Open Factor]**      Multiplication factor = 1.0

[Lower Value on Inside/Outside Temperature Difference for Modulating the Venting Open Factor] < T~zone~ - T~out~ < [Upper Value on Inside/Outside Temperature Difference for Modulating the Venting Open Factor]    ****Multiplication factor varies linearly from 1.0 to [Limit Value on Multiplier for Modulating Venting Open Factor]

T~zone~ - T~out~    **[Upper Value on Inside/Outside Temperature Difference for Modulating the Venting Open Factor]**     Multiplication factor = [Limit Value on Multiplier for Modulating Venting Open Factor]

One way of "tuning" the following modulation control parameters is to perform a sensitivity analysis for winter and/or summer design days to determine what combination of values causes the biggest reduction in zone air temperature fluctuations due to venting.

Note that the default values for the following fields are such that, if none of the fields are specified, the default values are assigned.

### Field: Minimum Venting Open Factor

See Figure 96 or Figure 97. This field applies only if Ventilation Control Mode = Temperature or Enthalpy. This value may be from zero to 1.0, with the default being 0.0.

### Field: Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor

See Figure 96. This field applies only if Ventilation Control Mode = Temperature. This value may be from zero to less than 100˚C, with the default being 0˚C. The value for this field must be less than the value specified for the following field.

### Field: Indoor and Outdoor Temperature Difference Upper Limit for Minimun Venting Open Factor

See Figure 96. This field applies only if Ventilation Control Mode = Temperature. This value must be greater than 0˚C, with the default being 100˚C. The value for this field must be greater than the value specified for the previous field..

### Field: Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor

See Figure 97. This field applies only if Ventilation Control Mode = Enthalpy. This value may be from zero to less than 300,000 J/kg, with the default being 0 J/kg. The value for this field must be less than the value specified for the following field.

### Field: Indoor and Outdoor Enthalpy Difference Upper Limit for Minimun Venting Open Factor

See Figure 97. This field applies only if Ventilation Control Mode = Enthalpy. This value must be greater than zero, with the default being 300,000 J/kg. The value for this field must be greater than the value specified for the previous field.

### Field: Venting Availability Schedule Name

The name of a schedule that specifies when venting is available. A zero or negative schedule value means venting is not allowed. A value greater than zero means venting can occur if other venting control conditions (specified by Ventilation Control Mode and Vent Temperature Schedule Name) are satisfied. This schedule name should not be confused with Vent Temperature Schedule Name.

If a Venting Availability Schedule Name is not specified, it is assumed that venting is always available.

Using Venting Availability Schedule allows you to turn off venting at certain times of the day (at night, for example), of the week (on weekends, for example), or of the year (during the winter, for example).

If used with Ventilation Control Mode = Constant, the ventilation rate is constant only when this schedule allows venting; otherwise the ventilation rate is set to zero.

If Ventilation Control Mode = NoVent, this schedule has no effect.

![Modulation of venting area according to inside-outside temperature difference.](media/modulation-of-venting-area-according-to.png)


![Modulation of venting area according to inside-outside enthalpy difference.](media/modulation-of-venting-area-according-to-001.png)


> **Note:** In order to establish an airflow network, each AirflowNetwork:Multizone:[Zone](#zone) object must have at least two surfaces defined with AirflowNetwork:Multizone:Surface objects, so that air can flow from one zone into other zones (or to outdoors) through the network (air mass flow conserved). In addition, for all AirflowNetwork:Multizone:Surface objects facing the same [Zone](#zone) (ref. [BuildingSurface:Detailed](#buildingsurfacedetailed)), at least two different environments must be defined for the other side of these surfaces (e.g., an external node and an adjacent zone, two adjacent zones, or two external nodes).

An IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:Zone,
        RESISTIVE ZONE,          !- Name of Associated Thermal Zone
        Temperature,             !- Ventilation Control Mode
        WindowVentSched,         !- Vent Temperature Schedule Name
        0.3,                     !- Limit Value on Multiplier for Modulating Venting Open Factor
                                 !- {dimensionless}
        5.0,                     !- Lower Value on Inside/Outside Temperature Difference for
                                 !- Modulating the Venting Open Factor {deltaC}
        10.0,                    !- Upper Value on Inside/Outside Temperature Difference for
                                 !- Modulating the Venting Open Factor {deltaC}
        0.0,                     !- Lower Value on Inside/Outside Enthalpy Difference for Modulating
                                 !- the Venting Open Factor {J/kg}
        300000.0,                !- Upper Value on Inside/Outside Enthalpy Difference for Modulating
                                 !- the Venting Open Factor {J/kg}
        VentingSched;            !- Venting Availability Schedule Name
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:Multizone:Surface

The AirflowNetwork:Multizone:Surface object specifies the properties of a surface "linkage" through which air flows. This linkage is always associated with a heat transfer surface (wall, roof, floor, or a ceiling) or subsurface (door, glass door, or window) with both faces exposed to air. The linkage specifies two connected nodes: two zone nodes defined in AirflowNetwork:Multizone:Zone objects based on inside and outside face environment for an interior surface, or a zone node defined in an AirflowNetwork:Multizone:Zone object based on inside face environment and an external node defined in an [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) object for an exterior surface. The associated leakage component for this surface can be a crack (or surface effective leakage area) in an exterior or interior heat transfer surface or subsurface, or an exterior or interior window, door or glass door (heat transfer subsurface) that can be opened to allow air flow. The allowed surface air leakage components are:

AirflowNetwork:MultiZone:Surface:Crack

AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea

AirflowNetwork:MultiZone:Component:DetailedOpening

AirflowNetwork:MultiZone:Component:HorizontalOpening

AirflowNetwork:MultiZone:Component:SimpleOpening

AirflowNetwork:MultiZone:Component:ZoneExhaustFan

The two "opening" components are used to modulate openness based on required conditions.

The AirflowNetwork:Multizone:Surface object allows a heat transfer surface or subsurface to have one crack (or one surface effective leakage area object), or a subsurface (i.e., window, door or glass door) to have one opening (detailed or simple).

An interior heat transfer surface ([BuildingSurface:Detailed](#buildingsurfacedetailed)) whose surface name is used as the input for the Outside Boundary Condition Object field represents a floor without ground contact and is not allowed as an AirflowNetwork:Multizone:Surface. A heat transfer surface defined in the BuildingSurface:Detailed:ExteriorNaturalVentedCavity is also not allowed.

### Field: Surface Name

This is the name of the corresponding surface (wall, roof, ceiling, floor, window, door or glass door).

Information on this surface is used by the program as follows:

#. For a linkage associated with an exterior heat transfer surface: air flow through this linkage is between the outside environment and the thermal zone to which the surface belongs.
#. For a linkage associated with an interior (i.e., interzone) heat transfer surface: air flow through this linkage is between the thermal zones separated by the surface (i.e., the thermal zone associated with the inside face environment and the thermal zone associated with the outside face environment).
#. This heat transfer surface determines the height of the linkage, which is used in calculating buoyancy-related flow through the linkage.

> **Note:** It is possible to define an interzone surface twice in EnergyPlus, once in each of the zones that the surface separates. Previously this was a requirement of EnergyPlus (prior to version 2.0), but now it is optional and the user also has the option of only defining the surface once (EnergyPlus defines the second surface automatically within the program). For each interzone surface, use only one (of possible two) interzone surface names in the AirflowNetwork:Multizone:Surface object for "Surface Name." **Do not** enter two AirflowNetwork:Multizone:Surface objects corresponding to the two possible interzone names. This would cause the air flow through the surface to be counted twice.

### Field: Leakage Component Name 

The name of the AirflowNetwork:MultiZone:Surface:Crack,AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea,AirflowNetwork:MultiZone:Component:SimpleOpening,AirflowNetwork:MultiZone:Component:HorizontalOpening,AirflowNetwork:MultiZone:Component:DetailedOpeningor [AirflowNetwork:MultiZone:Component:ZoneExhaustFan](#airflownetworkmultizonecomponentzoneexhaustfan) object associated with this air flow linkage.

If the name of an opening component (i.e. [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening). [AirflowNetwork:MultiZone:Component:HorizontalOpening](#airflownetworkmultizonecomponenthorizontalopening),  or [AirflowNetwork:MultiZone:Component:SimpleOpening](#airflownetworkmultizonecomponentsimpleopening) is given here, then the Surface Name in the previous field must be that of a window, door or glass door heat transfer subsurface. Otherwise an error message will be reported.

If the name of an [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack) object or [AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea](#airflownetworkmultizonesurfaceeffectiveleakagearea) object is given here, the program will position the crack at the average height of the associated heat transfer surface or subsurface. The user can define multiple heat transfer surfaces (e.g., split a wall into several surfaces) to be more precise in establishing the crack location. Similarly, the user can define multiple heat transfer surfaces if a wall, for example, has multiple cracks or openings that need to be defined individually.

If the name of an [AirflowNetwork:MultiZone:Component:ZoneExhaustFan](#airflownetworkmultizonecomponentzoneexhaustfan) is given here, then the Surface Name in the previous field must be that of an exterior heat transfer surface. The zone name defined in the [Zone](#zone) Name field for this heat transfer surface must be the same zone name defined in the [ZoneHVAC:EquipmentConnections](#zonehvacequipmentconnections) object (which references a [ZoneHVAC:EquipmentList](#zonehvacequipmentlist) containing the name of the corresponding zone exhaust fan). Otherwise an error message will be reported. When this zone exhaust fan is operating for a simulation timestep, all surface-level controls described below are ignored for that timestep.

### Field: External Node Name

The name of the associated [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) object, which determines the wind pressure coefficients for the heat transfer surface. Used only if Surface Name is for an exterior surface.

If Wind Pressure Coefficient Type = SurfaceAverageCalculation in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object, this field is not used and a blank may be entered. If the surface is an interior (i.e., interzone) surface, leave this field blank.

### Field: Window/Door Opening Factor, or Crack Factor

If this linkage is associated with an [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening) or [AirflowNetwork:MultiZone:Component:SimpleOpening](#airflownetworkmultizonecomponentsimpleopening) object (which means it is an openable window or door), then this field is called "Window/Door Opening Factor" and represents the value of the Opening Factor that is in effect when the Vent Temperature Schedule (defined in the AirflowNetwork:Multizone:Zone object) indicates that this window or door is open.

The AirflowNetwork model uses a combination of factors to determine the actual opening area for a window or door when it is venting. For example, consider a window that is 1.5m high and 2.0m wide (excluding frame). Assume that the [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening) for this window has Type of Large Vertical Opening = 1 (non-pivoting window), Height Factor = 0.5 and Width Factor = 0.8. Then when the window is fully open, the opening area = height of opening (0.5x1.5) times width of opening (0.8x2.0) = 0.75x1.6 = 1.2 m^2^. If the Window/Door Opening Factor is 0.75, then the opening area = 0.75x1.2 = 0.9 m^2^.

If, in addition, the window is in a thermal zone for which opening modulation has been specified (ref: AirflowNetwork:Multizone:Zone) and the multiplication factor due to modulation is 0.3 in a particular timestep, then the actual opening factor that timestep = 0.3x0.75 = 0.225 and the actual opening area that timestep = 0.3x0.9 = 0.27 m^2^.

If this linkage is associated with an AirflowNetwork:MultiZone:Surface:Crack ****object, the following crack air flow equation is used.

![](media/image218.png)\


Where

*Q*    = air mass flow (kg/s)

*C~Q~*  = air mass flow coefficient (kg/s @ 1 Pa)

*C~T~*  = reference condition temperature correction factor (dimensionless). See [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack) object.

![](media/image219.png) = pressure difference across crack (Pa)

*n*    = air flow exponent (dimensionless)

*The following fields control venting. They are used only when Name of Associated Heat Transfer Surface is that of an openable exterior or interior window, door or glass door. They only apply to openings, and do not apply to surface cracks, effective leakage area or zone exhaust fans. If none of these fields is specified, or if Ventilation Control Mode = ZoneLevel, venting is controlled by the AirflowNetwork:Multizone:Zone object for the thermal zone containing the window or door (ref: AirflowNetwork:Multizone:Zone Data).*

### Field: Ventilation Control Mode

Specifies the type of surface-level natural ventilation control.

Let T~out~ equal the outdoor air temperature, T~zone~ equal the previous timestep's zone air temperature, T~set~ equal the Vent Temperature Schedule value, H~zone~ equal the specific enthalpy of zone air from the previous timestep, and H~out~ equal the specific enthalpy of outdoor air. Then the four allowed choices for Ventilation Control Mode are:

**NoVent**: The openable window or door associated with this surface is closed at all times independent of indoor or outdoor conditions. The Venting Availability Schedule is ignored in this case.

**Temperature**: The openable window or door associated with this surface is opened if T~zone~ > T~out~ **and** T~zone~ > T~set~ **and** Venting Availability Schedule (see below) allows venting.

**Enthalpy:** The openable window or door associated with this surface is opened if H~zone~ > H~out~ **and** T~zone~ > T~set~ **and** Venting Availability Schedule allows venting.

**Constant**: Whenever this object's Venting Availability Schedule allows venting, the openable window or door associated with this surface is open, independent of indoor or outdoor conditions. Note that "Constant" here means that the size of this opening is fixed while venting; the air flow through this opening can, of course, vary from timestep to timestep.

**ASHRAE55Adaptive**: The openable window or door associated with this surface is opened if the operative temperature is greater than the comfort temperature (central line) calculated from the ASHRAE Standard 55-2010 adaptive comfort model **and** Venting Availability Schedule allows venting.

**CEN15251Adaptive:** The openable window or door associated with this surface is opened if the operative temperature is greater than the comfort temperature (central line) calculated from the CEN15251 adaptive comfort model **and** Venting Availability Schedule allows venting.

**ZoneLevel**: Venting of the window or door is not controlled individually, but is controlled instead at the zone level. This means that the venting is determined by the AirflowNetwork:Multizone:Zone object for the thermal zone containing the window or door (ref: AirflowNetwork:Multizone:Zone object). This is the default value for this field.

**AdjacentTemperature**: This choice is used for an interior surface only. The openable interior window or door associated with this surface is opened if T~zone~ > T~adjacent zone~ **and** T~zone~ > T~set~ **and** Venting Availability Schedule (see below) allows venting, where T~adjacent zone~ is the adjacent zone temperature.

**AdjacentEnthalpy:** This choice is also used for an interior surface only. The interior openable window or door associated with this surface is opened if H~zone~ > H~adjacent zone~ **and** T~zone~ > T~set~ **and** Venting Availability Schedule allows venting, where H~adjacent zone~ is the adjacent zone specific enthalpy.

### Field: Ventilation Control Zone Temperature Setpoint Schedule Name

The name of a schedule of zone air temperature set points that controls the opening of a window or door associated with this surface to provide natural ventilation. This setpoint is the temperature above which this openable window or door will be opened if the conditions described in the previous field Ventilation Control Mode are met.

The Ventilation Control [Zone](#zone) Temperature Setpoint Schedule applies only to a window or door attached to this surface that is specified using [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening) or [AirflowNetwork:MultiZone:Component:SimpleOpening](#airflownetworkmultizonecomponentsimpleopening).

(The discussion under the field Window/Door Opening Factor in this object describes how the actual opening area of a window or door in a particular timestep is determined.)

*Modulation of Openings*

The following five fields can be used to modulate this window/door opening when Ventilation Control Mode = Temperature or Enthalpy. These fields determine a factor between 0 and 1 that multiplies the opening factor of this window or door according to the control action shown in Figure 96 for Ventilation Control Mode = Temperature and in Figure 97 for Ventilation Control Mode = Enthalpy. Modulation of this opening can reduce the large temperature swings that can occur if the window/door is open too far when it is venting, especially when there is a large inside-outside temperature difference.

The modulation takes the following form when Ventilation Control Mode = Temperature:

T~zone~ - T~out~    **[Lower Value on Inside/Outside Temperature Difference for Modulating the Venting Open Factor]**      Multiplication factor = 1.0

[Lower Value on Inside/Outside Temperature Difference for Modulating the Venting Open Factor] < T~zone~ - T~out~ < [Upper Value on Inside/Outside Temperature Difference for Modulating the Venting Open Factor]    ****Multiplication factor varies linearly from 1.0 to [Limit Value on Multiplier for Modulating Venting Open Factor]

T~zone~ - T~out~    **[Upper Value on Inside/Outside Temperature Difference for Modulating the Venting Open Factor]**     Multiplication factor = [Limit Value on Multiplier for Modulating Venting Open Factor]

One way of "tuning" the following modulation control parameters is to perform a sensitivity analysis for winter and/or summer design days to determine what combination of values causes the biggest reduction in zone air temperature fluctuations due to venting.

Note that the default values for the following fields are such that, if none of the fields are specified, modulation will not occur.

### Field: Minimum Venting Open Factor

See Figure 96 or Figure 97. This field applies only if Ventilation Control Mode = Temperature or Enthalpy. This value may be from zero to 1.0, with the default being 0.0.

### Field: Indoor and Outdoor Temperature Difference Lower Limit For Maximum Venting Open Factor

See Figure 96. This field applies only if Ventilation Control Mode = Temperature. This value may be from zero to less than 100°C, with the default being 0°C. The value for this field must be less than the value specified for the following field.

### Field: Indoor and Outdoor Temperature Difference Upper Limit for Minimun Venting Open Factor

See Figure 96. This field applies only if Ventilation Control Mode = Temperature. This value must be greater than 0°C, with the default being 100°C. The value for this field must be greater than the value specified for the previous field.

### Field: Indoor and Outdoor Enthalpy Difference Lower Limit For Maximum Venting Open Factor

See Figure 97. This field applies only if Ventilation Control Mode = Enthalpy. This value may be from zero to less than 300,000 J/kg, with the default being 0 J/kg. The value for this field must be less than the value specified for the following field.

### Field: Indoor and Outdoor Enthalpy Difference Upper Limit for Minimun Venting Open Factor

See Figure 97. This field applies only if Ventilation Control Mode = Enthalpy. This value must be greater than zero, with the default being 300,000 J/kg. The value for this field must be greater than the value specified for the previous field.

### Field: Venting Availability Schedule Name

The name of a schedule that specifies when venting is available. A zero or negative schedule value means venting is not allowed. A value greater than zero means venting can occur if other venting control conditions (specified by Ventilation Control Mode and Vent Temperature Schedule Name) are satisfied. This schedule name should not be confused with Vent Temperature Schedule Name.

If a Venting Availability Schedule Name is not specified, it is assumed that venting is always available.

Using Venting Availability Schedule allows you to turn off venting at certain times of the day (at night, for example), week (on weekends, for example), or year (during the winter, for example).

If used with Ventilation Control Mode = Constant, the ventilation rate is constant only when this schedule allows venting; otherwise the ventilation rate is set to zero.

If Ventilation Control Mode = NoVent, this schedule has no effect.

> **Note:** In order to establish an airflow network, each AirflowNetwork:Multizone:[Zone](#zone) object must have at least two surfaces defined with AirflowNetwork:Multizone:Surface objects, so that air can flow from one zone into other zones (or to outdoors) through the network (air mass flow conserved). In addition, for all AirflowNetwork:Multizone:Surface objects facing the same [Zone](#zone) Name (ref. [BuildingSurface:Detailed](#buildingsurfacedetailed)), at least two different environments must be defined for the other side of these surfaces (e.g., an external node and an adjacent zone, two adjacent zones, or two external nodes).

IDF examples are provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:Surface,
        Zn001:Wall001,           !- Name of Associated Heat Transfer Surface
        CR-1,                    !- Leakage Component Name
        SFacade,                 !- External Node Name
        1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}

    AirflowNetwork:MultiZone:Surface,
        Zn001:Wall001:Win001,    !- Name of Associated Heat Transfer Surface
        WiOpen1,                 !- Leakage Component Name
        SFacade,                 !- External Node Name
        0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:Surface,
        Zn003:Wall003,           !- Name of Associated Heat Transfer Surface
        Zone3 Exhaust Fan,       !- Leakage Component Name
        EFacade,                 !- External Node Name
        1.0;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}
    AirflowNetwork:MultiZone:Surface,
        Zn001:Wall001:Win002,    !- Name of Associated Heat Transfer Surface
        WiOpen2,                 !- Leakage Component Name
        WFacade,                 !- External Node Name
    0.5;                     !- Window/Door Opening Factor, or Crack Factor {dimensionless}
        Temperature,             !- Ventilation Control Mode
        WindowVentSched,         !- Vent Temperature Schedule Name
        0.3,                     !- Limit Value on Multiplier for Modulating Venting Open Factor
                                 !- {dimensionless}
        5.0,                     !- Lower Value on Inside/Outside Temperature Difference for
                                 !- Modulating the Venting Open Factor {deltaC}
        10.0,                    !- Upper Value on Inside/Outside Temperature Difference for
                                 !- Modulating the Venting Open Factor {deltaC}
        0.0,                     !- Lower Value on Inside/Outside Enthalpy Difference for Modulating
                                 !- the Venting Open Factor {J/kg}
        300000.0,                !- Upper Value on Inside/Outside Enthalpy Difference for Modulating
                                 !- the Venting Open Factor {J/kg}
        VentingSched;            !- Venting Availability Schedule Name
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:ReferenceCrackConditions

This object specifies the reference conditions for temperature, humidity, and pressure which correspond to the [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack) object.

### Inputs

#### Field: Name

The name of this Reference Crack Conditons object. This name is referenced by an [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack) object.

#### Field: Reference Temperature

The reference temperature in °C under which the Surface Crack Data were obtained. The default value is 20°C.

#### Field: Reference Barometric Pressure

The reference barometric pressure in Pa under which the Surface Crack Data were obtained. The default value is 101325 Pa.

#### Field: Reference Humidity Ratio

The reference humidity ratio in kgWater/kgDryAir under which the Surface Crack Data were obtained. The default value is 0 kgWater/kgDryAir.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:ReferenceCrackConditions,
        ReferenceCrackConditions,      !- Name of Reference Crack Conditions
        20.0,                          !- Reference Temperature for Crack Data {C}
        101325,                        !- Reference Barometric Pressure for Crack Data {Pa}
        0.0;                           !- Reference Humidity Ratio for Crack Data {kgWater/kgDryAir}
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:Surface:Crack

This object specifies the properties of air flow through a crack and the associated measurement conditions. The following power law form is used that gives air flow through the crack as a function of the pressure difference across the crack:

![](media/image220.png)\


Where

*Q*    = air mass flow (kg/s)

*C~Q~*  = air mass flow coefficient (kg/s-Pa^n^ @ 1 Pa)

*C~T~*  = reference condition temperature correction factor (dimensionless)

![](media/image221.png) = pressure difference across crack (Pa)

*n*    = air flow exponent (dimensionless)

![](media/image222.png)\


where

ρ = Air density at the specific air temperature and humidity ratio conditions [kg/m^3^]

ν = Air kinetic viscosity at the specific air temperature condition [m^2^/s]

ρ~o~~~= Air density at the reference air conditions provided by the object [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) specified in the field Reference Crack Conditions [kg/m^3^]

ν~o~~~= Air kinetic viscosity at the reference air temperature provided by the object [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) specified in the field Reference Crack Conditions [m^2^/s]

> Note: The correction factor shown above is use for this particular component as specified.

### Inputs

#### Field: Name 

This is a name for this [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack) object. It is referenced by an AirflowNetwork:Multizone:Surface object.

#### Field: Air Mass Flow Coefficient at Reference Conditions

The value of the air mass flow coefficient,![](media/image223.png) , in the crack air flow equation. It has units of kg/s at 1Pa. This value must be greater than zero.

#### Field: Air Mass Flow Exponent

The value of the exponent, *n*, in the crack air flow equation. The valid range is 0.5 to 1.0, with the default value being 0.65.

#### Field: Reference Crack Conditions

The name of the [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) object which specifies the conditions under which the air mass flow coefficient was measured. If the user omits this field and only one [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) object is defined in the input data file, then those reference crack conditions will be used. If the user omits this field and either zero or more than one [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) objects are defined in the input data file, then the default conditions for the AirflowNetwork:Multizone: Reference Crack Conditions object will be used.

An IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:Surface:Crack,
        CR-1,                     !- Name of Surface Crack Component
        0.01,                     !- Air Mass Flow Coefficient at Reference Conditions {kg/s}
        0.667,                    !- Air Mass Flow Exponent {dimensionless}
        ReferenceCrackConditions; !- Reference Crack Conditions
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea

The effective leakage area (ELA) object is used to define surface air leakage. It has

five fields. The relationship between pressure and airflow may be expressed as:

![](media/image224.png)\


where

![](media/image225.png) = Air mass flow rate [kg/s]

*ELA*= Effective leakage area [m^2^]

ρ= Air density [kg/m^3^]

![](media/image226.png) = Reference pressure difference [Pa]

![](media/image227.png) = Pressure difference across this component [Pa]

C~d~= Discharge coefficient [dimensionless]

n= Air mass flow exponent [dimensionless]

### Inputs

#### Field: Name 

This is a name for this [AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea](#airflownetworkmultizonesurfaceeffectiveleakagearea) object. It is referenced by an AirflowNetwork:Multizone:Surface object.

#### Field: Effective Leakage Area

This numeric field is used to input the effective leakage area  in square meters. The effective leakage area is used to characterize openings for infiltration calculations (ASHRAE Handbook of Fundamentals, 1997, pp 25.18). This value must be greater than zero.

#### Field: Discharge Coefficient

This numeric field is used to input the discharge coefficient. This value must be greater than zero, with a default value of 1.0.

#### Field: Reference Pressure Difference

This numeric field is used to input the reference pressure difference [Pa]. This value must be greater than zero, with a default value of 4.0 Pa.

#### Field: Air Mass Flow Exponent

This numeric field is used to input the pressure difference exponent. The valid range of the exponent is from 0.5 to 1.0, with a default value of 0.65.

> Note: There are two common sets of reference conditions: C~d~ = 1.0 and P = 4 Pa, or C~d~ = 0.6 and P = 10 Pa

An IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea,
        SurfaceELR,              !- Name of surface effective leakage area component
        0.07,                    !- Effective leakage area {dimensionless}
        1.00,                    !- Discharge coefficient {dimensionless}
        4.0,                     !- Reference pressure difference {Pa}
        0.65;                    !- Air mass flow exponent {dimensionless}
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:Component:DetailedOpening

This object specifies the properties of air flow through windows and doors (window, door and glass door heat transfer subsurfaces) when they are closed or open. The fields are similar to those for AirflowNetwork:Multizone:SurfaceCrack object when the window or door is closed, but additional fields are required to describe the air flow characteristics when the window or door is open. These additional fields include opening type, opening dimensions, degree of opening, and opening schedule.

The AirflowNetwork model assumes that open windows or doors are vertical or close to vertical; for this reason they are called "Large Vertical Openings." Such openings can have air flow moving simultaneously in two different directions depending on stack effects and wind conditions (for example, flow from inside to outside at the top of a window and from outside to inside at the bottom). AirflowNetwork models such two-directional flow, but only for vertical openings.

It is assumed that the air flow through a window opening is unaffected by the presence of a shading device such as a shade or blind on the window. Also, the calculation of conductive heat transfer and solar gain through a window or door assumes that the window or door is closed.

The AirflowNetwork model does not have a model for bi-directional flow through large horizontal openings. For this reason, **AirflowNetwork:MultiZone:Component:DetailedOpening should not be used for horizontal openings**. The best modeling technique in this case is to put an [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack) object in a horizontal surface and use a large air mass flow coefficient. Crack flow is assumed to be uni-directional in any given timestep (but can reverse flow direction from timestep to timestep).

A subsurface multiplier may be used to represent multiple subsurfaces and calculates total air flow when the subsurface (window, glassdoor, or door) is either closed or open. The total airflow across the surface is equal to the airflow based on the surface geometry multiplied by the subsurface multiplier.

### Inputs

#### Field: Name

The name of this [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening) object. It is referenced by an AirflowNetwork:Multizone:Surface object.

#### Field: Air Mass Flow Coefficient When Opening is Closed

Crack flow is assumed when the window or door is closed. The units for this air mass flow coefficient (![](media/image228.png) ) are different from the units for ![](media/image229.png) (kg/s at 1 Pa pressure difference) defined in an [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack) object. There is no default but the entered value must be greater than zero. The program will automatically generate four cracks around the perimeter of the window or door--one along the bottom, one along the top, and one on each side. The temperature correction factor used in the [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack) object is not used for this component to calculate air mass flow rate.

#### Field: Air Mass Flow Exponent When Opening Is Closed

Crack flow is assumed when the window or door is closed. In this case, the value of this field is the exponent, *n*, in the crack air flow equation. The valid range for this exponent is 0.5 to 1.0, with the default value being 0.65.

#### Field: Type of Rectanguler Large Vertical Opening (LVO)

This alpha field specifies the type of rectangular window or door. (Open windows or doors are also called Large Vertical Openings (LVOs). The choices for the opening type are **NonPivoted** (LVO Type 1) and **HorizontallyPivoted** (LVO Type 2) with the default being **NonPivoted**. The NonPivoted type represents a regular window or door. The HorizontallyPivoted type represents a window with a horizontal axis ((i.e., a horizontally-pivoting window) and cannot be used for a door.

#### Field: Extra Crack Length or Height of Pivoting Axis

Specifies window or door characteristics that depend on the LVO type.

For LVO Type 1 (rectangular non-pivoted windows and doors) this field is the extra crack length in meters due to multiple openable parts, if present. "Extra" here means in addition to the length, calculated by the program, of the cracks on the top, bottom and sides of the window/door.

For LVO Type 2 (rectangular horizontally-pivoted windows) this field gives the height of the pivoting axis measured from the bottom of the glazed part of the window (m).

#### Field: Number of Sets of Opening Factor Data

This is the number of the following sets of data for opening factor, discharge coefficient, width factor, height factor, and start height factor. From two to four of these sets must be defined. The first set should be for Opening Factor = 0.0 and the last set should be for Opening Factor = 1.0. For example, if only two sets are defined, the first set should be for Opening Factor = 0.0 and the second set should be for Opening Factor = 1.0, as shown below in the IDF example below.

An "opening factor" refers to the amount that a window or door is opened. The program linearly interpolates each timestep between the values of discharge coefficient, width factor, etc., in these sets using the opening factor for the window or door for the timestep. (See discussion under the field Window/Door Opening Factor in the AirflowNetwork:Multizone:Zone object for a description of how the AirflowNetwork model determines the time-step value of the opening factor.)

#### Field Group: Opening Factor, Discharge Coefficient, Width Factor, Height Factor, Start Height Factor

Each field is described for as many groups as required in the previous field (number of sets of opening factor data). As the final field has specific requirements, this field (n) will be described.

#### Field: Opening Factor 11

The first opening factor of a window or door. This value must be 0.0. The default value is also 0.0.

For LVO Type 1 (rectangular non-pivoted window or door), the Opening Factor corresponds to the fraction of window or door that is opened.

For LVO Type 2 (rectangular horizontally-pivoted windows), the Opening Factor is determined by the window opening angle. For example, an opening angle of 45° corresponds to an Opening Factor of 0.50 since the maximum opening angle is 90°.

#### Field: Discharge Coefficient for Opening Factor 1

The discharge coefficient of the window or door for Opening Factor 1. The range is greater than 0.0 to less than or equal to 1.0. The default value is 0.001. The Discharge Coefficient indicates the fractional effectiveness for air flow through a window or door at that Opening Factor.

> **Note:** In the following, "window width" and "window height" are glazing dimensions; they do not include the frame, if present.

#### Field: Width Factor for Opening Factor 1

The Width Factor of the rectangular window or door for Opening Factor 1. The Width Factor is the opening width divided by the window or door width (see Figure 98). The range is 0.0 to 1.0. The default value is 0.0. Note that the width factor applies to rectangular windows or doors where the width is assumed constant along the entire height of the opening.

#### Field: Height Factor for Opening Factor 1

The Height Factor of the rectangular window or door for Opening Factor 1. The Height Factor is the opening height divided by the window or door height (see Figure 98). The range is 0.0 to 1.0. The default value is 0.0. Note that the height factor applies to rectangular windows or doors where the height is assumed constant along the entire width of the opening.

#### Field: Start Height Factor for Opening Factor 1

The Start Height Factor of the window or door for Opening Factor 1. The Start Height Factor is the Start Height divided by the window or door height (see Figure 98). The range is 0.0 to 1.0. The default is 0. Start Height is the distance between the bottom of the window or door and the bottom of the window or door opening. The sum of the Height Factor and the Start Height Factor must be less than 1.0 in order to have the opening within the window or door dimensions.

![Window (or door) showing geometrical factors associated with an opening through which air flows.](media/window-or-door-showing-geometrical-factors.png)


#### Field: Opening Factor <n>

When Number of Sets of Opening Factor Data = n, the value of Opening Factor n must be set to 1.0.

#### Field: Discharge Coefficient for Opening Factor <n> 

The discharge coefficient of the window or door for Opening Factor n. The range is greater than 0.0 to less than or equal to 1.0. The default value is 1.0.

#### Field: Width Factor for Opening Factor <n>

The Width Factor of the rectangular window or door for Opening Factor n. The Width Factor is the opening width divided by the window or door width (see Figure 98). The range is 0.0 to 1.0. The default value is 1.0.

#### Field: Height Factor for Opening Factor <n>

The Height Factor of the rectangular window or door for Opening Factor n. The Height Factor is the opening height divided by the window or door height (see Figure 98). The range is 0.0 to 1.0. The default value is 1.0.

#### Field: Start Height Factor for Opening Factor <n>

The Start Height Factor of the window or door for Opening Factor n. The Start Height Factor is the Start Height divided by the window or door height (see Figure 98). The range is 0.0 to 1.0. The default is 0.

When the opening factor value (as described under the field Window/Door Opening Factor in the AirflowNetwork:Multizone:Surface object) is between two Opening Factor field values, the values of Discharge Coefficient, Width Factor, Height Factor, and Start Height Factor are linearly interpolated.

An IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:Component:DetailedOpening,
        WiOpen1,                 !- Detailed Opening Name
        0.001,                   !- Air Mass Flow Coefficient When Opening is Closed {kg/s-m}
        0.667,                   !- Air Mass Flow Exponent When Opening is Closed {dimensionless}
        NonPivoted,             !- Type of Large Vertical Opening (LVO)
        0.0,                     !- Extra crack length for LVO type 1 with multiple openable parts,
                                 !- or Height of pivoting axis for LVO type 2 {m}
        2,                       !- Number of Sets of Opening Factor Data
        0.0,                     !- Opening factor 1 {dimensionless}
        0.5,                     !- Discharge coefficient for opening factor 1 {dimensionless}
        0.0,                     !- Width factor for opening factor 1 {dimensionless}
        1.0,                     !- Height factor for opening factor 1 {dimensionless}
        0.0,                     !- Start height factor for opening factor 1 {dimensionless}
        1.0,  0.6,  1.0,  1.0,  0.0;  !-  Set of values for opening factor 2
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:Component:HorizontalOpening

This object specifies the properties of air flow through windows, doors and glass doors (heat transfer subsurfaces defined as a subset of [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) objects) when they are closed or open. This AirflowNetwork model assumes that these openings are horizontal or close to horizontal and are interzone surfaces. The second and third input fields are similar to those for [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack), when the window or door is closed, but additional information is required to describe the air flow characteristics when the window or door is open. This additional information is specified in the last two input fields. The airflow across the opening consists of two types of flows: forced and buoyancy. The forced flow is caused by the pressure difference between two zones, while the buoyancy flow only occurs when the air density in the upper zone is greater than the air density in the lower zone. This opening also allows for the possibility of two-way flow when forced and buoyancy flows co-exist. This object's openness can also be modulated based on the same opening factor control as an [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening) object. However, the opening factor is only applied to the subsurface width. The opening width is equal to opening factor multiplied by the subsurface width.

A subsurface multiplier may be used to represent multiple subsurfaces and calculates total air flow when the subsurface (window, glassdoor, or door) is either closed or open. The total airflow across the surface is equal to the airflow based on the surface geometry multiplied by the subsurface multiplier.

### Inputs

#### Field: Name

This is a name for this [AirflowNetwork:MultiZone:Component:HorizontalOpening](#airflownetworkmultizonecomponenthorizontalopening) object. It is referenced by an AirflowNetwork:Multizone:Surface object.

#### Field: Air Mass Flow Coefficient When Opening is Closed

The value of the air mass flow coefficient, ![](media/image231.png) , in the horizontal opening air flow equation. It has units of kg/s-m at 1Pa. The temperature correction factor is not applied to the mass flow calculation. This is a required input field and the entered value must be greater than zero.

#### Field: Air Mass Flow Exponent When Opening is Closed

The value of the exponent, *n*, in the crack air flow equation. The valid range is 0.5 to 1.0, with the default value being 0.65.

#### Field: Sloping Plane Angle

This numeric field is used to represent the angle between the horizontal plane and a sloped plane under the horizontal opening. Sloping plane angle = 90 is equivalent to fully open. The valid range is >0 to 90, with the default value being 90.

#### Field: Discharge Coefficient

This numeric field is used to input the discharge coefficient. This is a required field and the entered value must be greater than zero.

An IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:Component:HorizontalOpening,
        HrOpen,                  !- Name
        0.001,                   !- Air Mass Flow Coefficient When Opening is Closed {kg/s-m}
        0.667,                   !- Air Mass Flow Exponent When Opening is Closed {dimensionless}
        90.0,                    !- Sloping Plane Angle
        0.2;                     !- Discharge Coefficient
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:Component:SimpleOpening

This object specifies the properties of air flow through windows, doors and glass doors (heat transfer subsurfaces) when they are closed or open. The AirflowNetwork model assumes that open windows or doors are vertical or close to vertical. The second and third fields are similar to those for [AirflowNetwork:MultiZone:Surface:Crack](#airflownetworkmultizonesurfacecrack), when the window or door is closed, but additional information is required to describe the air flow characteristics when the window or door is open. This additional information is specified in the last two fields. Compared to the object [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening), which requires more inputs at different opening factors, this object needs comparatively less inputs. For this reason it is called a simple opening. This opening also allows for the possibility of two-way flow due to temperature and resulting density differences. Therefore, it is possible to have a positive pressure difference at the top of the opening, and a negative pressure difference at the bottom (or vice versa) when the neutral height is between the bottom and top heights of the associated surface. This object's openness can also be modulated based on the same opening factor control as an [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening) object. However, the opening factor is only applied to the subsurface width. The opening width is equal to opening factor multiplied by the subsurface width.

A subsurface multiplier may be used to represent multiple subsurfaces and calculates total air flow when the subsurface (window, glassdoor, or door) is either closed or open. The total airflow across the surface is equal to the airflow based on the surface geometry multiplied by the subsurface multiplier.

### Inputs

#### Field: Name

This is a name for this [AirflowNetwork:MultiZone:Component:SimpleOpening](#airflownetworkmultizonecomponentsimpleopening) object. It is referenced by an AirflowNetwork:Multizone:Surface object.

#### Field: Air Mass Flow Coefficient When Opening is Closed

The value of the air mass flow coefficient, ![](media/image232.png) , in the simple opening air flow equation. It has units of kg/s-m at 1Pa. The temperature correction factor is not applied for mass flow calculation.

#### Field: Air Mass Flow Exponent When Opening is Closed

The value of the exponent, *n*, in the crack air flow equation. The valid range is 0.5 to 1.0, with the default value being 0.65.

#### Field: Minimum Density Difference for Two-Way Flow

This numeric field is used to input the minimum density difference above which two-way or one way flow may occur due to stack effect with the window being open. Two-way flow occurs only when the neutral plane is within the opening. Density differences less than this value result in one-way flow only with the window being closed. The minimum value for this field is greater than zero.

#### Field: Discharge Coefficient

This numeric field is used to input the discharge coefficient. This value must be greater than zero.

An IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

     AirflowNetwork:MultiZone:Component:SimpleOpening,
        WiOpen2,                 !- Simple Opening Name
        0.001,                   !- Air Mass Flow Coefficient When Opening Is Closed {kg/s-m}
        0.650,                   !- Air Mass Flow Exponent When Opening Is Closed {dimensionless}
        0.0001,                  !- Minimum density difference for two-way flow (kg/m3)
        1.0;                     !- Discharge coefficient (dimensionless)
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:Component:ZoneExhaustFan

This object specifies the properties of air flow through an exterior heat transfer surface with a zone exhaust fan. The zone exhaust fan turns on or off based on the availability schedule defined in the corresponding [Fan:ZoneExhaust](#fanzoneexhaust) object. When the exhaust fan mass flow rate is greater than zero, the airflow network model treats this object as a constant volume fan. When the fan is off based on the availability schedule, the model treats this object as a crack.

When the fan is on, the air mass flow rate modeled for the airflow network is based on the value defined in the Maximum Flow Rate field of the [Fan:ZoneExhaust](#fanzoneexhaust) object. The airflow direction is from the corresponding zone to outdoors.

When the fan is off, the following power law form is used that gives air flow through the crack as a function of the pressure difference across the crack:

![](media/image233.png)\


Where

*Q*    = air mass flow (kg/s)

*C~Q~*  = air mass flow coefficient (kg/s-Pa^n^ @ 1 Pa)

*C~T~*  = reference condition temperature correction factor (dimensionless)

![](media/image234.png) = pressure difference across crack (Pa)

*n*    = air flow exponent (dimensionless)

![](media/image235.png)\


where

ρ = Air density at the specific air temperature and humidity ratio conditions [kg/m^3^]

ν = Air kinetic viscosity at the specific air temperature condition [m^2^/s]

ρ~o~~~= Air density at the reference air conditions provided by the object [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) specified in the field Reference Crack Conditions [kg/m^3^]

ν~o~~~= Air kinetic viscosity at the reference air temperature provided by the object [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) specified in the field Reference Crack Conditions [m^2^/s]

> Note: The correction factor shown above is used when the exhaust fan is off. The airflow direction is based on the pressure difference between the zone and outdoors.

### Inputs

#### Field: Name

This is the name for this instance of the [AirflowNetwork:MultiZone:Component:ZoneExhaustFan](#airflownetworkmultizonecomponentzoneexhaustfan) object. This name must be the same name defined in the [Fan:ZoneExhaust](#fanzoneexhaust) object. It is referenced by an AirflowNetwork:Multizone:Surface object.

#### Field: Air Mass Flow Coefficient When the Zone Exhaust Fan is Off at Reference Conditions

The value of the air mass flow coefficient,![](media/image236.png) , in the crack air flow equation. It has units of kg/s at 1Pa. This value must be greater than zero. The value is used when the fan is off.

#### Field: Air Mass Flow Exponent When the Zone Exhaust Fan is Off

The value of the exponent, *n*, in the crack air flow equation. The valid range is 0.5 to 1.0, with the default value being 0.65. The value is used when the fan is off.

#### Field: Reference Crack Conditions

The name of the [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) object which specifies the conditions under which the air mass flow coefficient was measured. If the user omits this field and only one [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) object is defined in the input data file, then those reference crack conditions will be used. If the user omits this field and either zero or more than one [AirflowNetwork:MultiZone:ReferenceCrackConditions](#airflownetworkmultizonereferencecrackconditions) objects are defined in the input data file, then the default conditions for the AirflowNetwork:Multizone: Reference Crack Conditions object will be used.

An IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:Component:ZoneExhaustFan,
        Zone3 Exhaust Fan,       !- Name
        0.01,     !- Air Mass Flow Coefficient When the Zone Exhaust Fan is Off at Reference Conditions {kg/s}
        0.667;    !- Air Mass Flow Exponent When the Zone Exhaust Fan is Off{dimensionless}
        ReferenceCrackConditions; !- Reference Crack Conditions
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:ExternalNode

External nodes in the AirflowNetwork model define environmental conditions outside of the building. These conditions include wind pressure coefficients that vary from façade to façade and can be highly dependent on the building geometry.

[AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) objects do **not** have to be entered if Wind Pressure Coefficient Type = SurfaceAverageCalculation in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object.

### Inputs

#### Field: Name

The external node name is associated with a particular building façade. This name is referenced by the External Node Name field of an [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) object (which gives wind pressure coefficients for the façade as a function of angle of wind incident on the façade) and by the External Node Name field of an AirflowNetwork:MultiZone:Surface object.

#### Field: External Node Height

Designates the reference height, in meters, used to calculate relative pressure. The default value is 0 meters.

#### Field: Wind Pressure Coefficient Values Object Name

The name of a specific [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) object (which gives wind pressure coefficients for the façade as a function of angle of wind incident on the façade).

IDF examples are provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:ExternalNode,
        NFacade,                 !- Name
        1.524,                   !- External Node Height {m}
        NFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name

    AirflowNetwork:MultiZone:ExternalNode,
        EFacade,                 !- Name
        1.524,                   !- External Node Height {m}
        EFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name

    AirflowNetwork:MultiZone:ExternalNode,
        SFacade,                 !- Name
        1.524,                   !- External Node Height {m}
        SFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name

    AirflowNetwork:MultiZone:ExternalNode,
        WFacade,                 !- Name
        1.524,                   !- External Node Height {m}
        WFacade_WPCValue;        !- Wind Pressure Coefficient Values Object Name

    AirflowNetwork:MultiZone:ExternalNode,
        Horizontal,              !- Name
    3.028,                   !- External Node Height {m}
    Horizontal_WPCValue;     !- Wind Pressure Coefficient Values Object Name
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:WindPressureCoefficientArray

The reference height and wind directions are first specified under the [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray) object. The user may specify up to 36 different wind directions in ascending order. These are then referenced by [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) objects defined for each [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode).

The [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray) object is unique and needs to be entered only if Wind Pressure Coefficient Type = INPUT in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object. If Wind Pressure Coefficient Type = SurfaceAverageCalculation, this object is not required.

### Inputs

#### Field: Name

The name of this [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray) object. This name is referenced by each [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) object which, for each [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode), gives the wind pressure coefficients at each of the wind directions listed in the [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray). This name is also referenced by the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object, indicating that this [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray) and the pressure coefficients in the associated [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) objects will be used in the air flow simulation.

#### Field: Wind Direction 1-Wind Direction N

Each field references the wind direction corresponding to the first through the Nth WPC value in each of the [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) objects. *N* can be as high as 36.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:WindPressureCoefficientArray,
        Every 30 Degrees,        !- WPC Array Name
        0,                       !- Wind Direction #1 {deg}
        30,                      !- Wind Direction #2 {deg}
        60,                      !- Wind Direction #3 {deg}
        90,                      !- Wind Direction #4 {deg}
        120,                     !- Wind Direction #5 {deg}
        150,                     !- Wind Direction #6 {deg}
        180,                     !- Wind Direction #7 {deg}
        210,                     !- Wind Direction #8 {deg}
        240,                     !- Wind Direction #9 {deg}
        270,                     !- Wind Direction #10 {deg}
        300,                     !- Wind Direction #11 {deg}
        330;                     !- Wind Direction #12 {deg}
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:MultiZone:WindPressureCoefficientValues

This object specifies up to 36 wind pressure coefficients (WPCs) for an [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode). These coefficients are defined for each of the wind directions defined in the unique [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray) object. In the air flow calculation, interpolation of the specified WPC values is done for time-step values of wind direction.

[AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) objects need to be entered only if the Wind Pressure Coefficient Type = INPUT in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object. If Wind Pressure Coefficient Type = SurfaceAverageCalculation, this object is not required and is not used.

### Inputs

#### Field: Name

The name of this WindPressureCoefficientValues object. This name can be referenced by multiple [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) objects.

#### Field: WindPressureCoefficientArray Name

Name of the associated [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray), which lists the wind direction corresponding to each wind pressure coefficient value in this [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) object.

#### Field: Wind Pressure Coefficient Value 1 to Wind Pressure Coefficient Value N

The WPC (wind pressure coefficient) value for the building façade indicated by the External Node Name field above. This WPC value corresponds to the first wind direction in the [AirflowNetwork:MultiZone:WindPressureCoefficientArray](#airflownetworkmultizonewindpressurecoefficientarray). Note that WPC values can be positive, negative or zero.

#### Obtaining WPC values

WPC values can be obtained from wind tunnel measurements, CFD calculations, or from published values for different building shapes.

For **rectangular buildings** EnergyPlus will automatically calculate surface-averaged Cp values for the walls and roof of the building if you specify Wind Pressure Coefficient Type = SurfaceAverageCalculation in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object. In this case you do not have to enter any [AirflowNetwork:MultiZone:WindPressureCoefficientValues](#airflownetworkmultizonewindpressurecoefficientvalues) objects.

Wind pressure coefficients are reported in the eplusout.eio either from inputs using "INPUT" as the choice for the Wind Pressure Coefficient Type field defined in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object, or from internal calculation using SurfaceAverageCalculation as the choice for the Wind Pressure Coefficient Type field defined in the same object. Below is an output example from the eplusout.eio file:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork Model:Wind Direction, 0.0,30.0,60.0,90.0,120.0,150.0,180.0,210.0,240.0,270.0,300.0,330.0
    ! <AirflowNetwork Model:Wind Pressure Coefficients, WPC Name, Wind Pressure Coefficients #1 to n (dimensionless)>
    AirflowNetwork Model:Wind Pressure Coefficients, NFACADE, 0.60,0.48,4.00E-002,-0.56,-0.56,-0.42,-0.37,-0.42,-0.56,-0.56,4.00E-002,0.48
    AirflowNetwork Model:Wind Pressure Coefficients, EFACADE, -0.56,4.00E-002,0.48,0.60,0.48,4.00E-002,-0.56,-0.56,-0.42,-0.37,-0.42,-0.56
    AirflowNetwork Model:Wind Pressure Coefficients, SFACADE, -0.37,-0.42,-0.56,-0.56,4.00E-002,0.48,0.60,0.48,4.00E-002,-0.56,-0.56,-0.42
    AirflowNetwork Model:Wind Pressure Coefficients, WFACADE, -0.56,-0.56,-0.42,-0.37,-0.42,-0.56,-0.56,4.00E-002,0.48,0.60,0.48,4.00E-002
~~~~~~~~~~~~~~~~~~~~

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:MultiZone:WindPressureCoefficientValues,
        NFacade_WPCValue,        !- Name
        Every 30 Degrees,        !- AirflowNetwork:MultiZone:WindPressureCoefficientArray Name
        0.60,                    !- Wind Pressure Coefficient Value 1 {dimensionless}
        0.48,                    !- Wind Pressure Coefficient Value 2 {dimensionless}
        0.04,                    !- Wind Pressure Coefficient Value 3 {dimensionless}
        -0.56,                   !- Wind Pressure Coefficient Value 4 {dimensionless}
        -0.56,                   !- Wind Pressure Coefficient Value 5 {dimensionless}
        -0.42,                   !- Wind Pressure Coefficient Value 6 {dimensionless}
        -0.37,                   !- Wind Pressure Coefficient Value 7 {dimensionless}
        -0.42,                   !- Wind Pressure Coefficient Value 8 {dimensionless}
        -0.56,                   !- Wind Pressure Coefficient Value 9 {dimensionless}
        -0.56,                   !- Wind Pressure Coefficient Value 10 {dimensionless}
        0.04,                    !- Wind Pressure Coefficient Value 11 {dimensionless}
        0.48;                    !- Wind Pressure Coefficient Value 12 {dimensionless}
~~~~~~~~~~~~~~~~~~~~

The previous sections of this AirflowNetwork model discussion describe input objects used for multizone airflow calculations. The following sections describe input objects used for air distribution system simulations. These objects work when control option "MultiZone with Distribution" or "MultiZone with Distribution Only During Fan Operation" is defined in the AirflowNetwork Control field in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object.

The first section presents the input object for distribution system nodes. Although thermal zones are required to perform air distribution system simulations, the thermal zones are already defined in the multizone input section (described previously), so that there is no need to repeat the inputs for thermal zones when modeling an air distribution system. The same is also true for surface air leakage. This section has only one object: [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode).

## AirflowNetwork:Distribution:Node

The [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) object is used to represent air distribution system nodes for the AirflowNetwork model. The EnergyPlus nodes defined in an [AirLoopHVAC](#airloophvac) are a subset of the nodes used to simulate the distribution system using the AirflowNetwork model. For example, the inlet node of a fan and the outlet node of a coil defined in an [AirLoopHVAC](#airloophvac) must be defined as nodes using the [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) object. A set of EnergyPlus [Zone](#zone) Equipment nodes is also a subset of the AirflowNetwork:Distribution:Nodes. For example, zone inlet and outlet nodes must be defined as nodes using the AirflowNetwork:Distribution: Node object. In addition, although mixers and splitters are defined as objects with inlet and outlet nodes within EnergyPlus, the [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) object treats mixers and splitters as single nodes. The node objects are referenced by [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) objects.

In summary, all nodes used to define an [AirLoopHVAC](#airloophvac) (except splitters, mixers, and outdoor air systems which are treated as single nodes) and its connections to a thermal zone must be specified as AirflowNetwork:Distribution:Nodes. If distribution system air leaks are to be modeled, additional AirflowNetwork:Distribution:Nodes may be defined along with AirflowNetwork:Distribution:Components (e.g., leak or leak ratio) to define the air leakage characteristics.

> Note: Supply and return leaks are not allowed in an [AirLoopHVAC](#airloophvac). They can only be modeled in the [Zone](#zone) Equipment Loop (i.e., return leaks may be modeled between the zone return node and the zone mixer inlet or the zone mixer outlet and the zone equipment loop outlet; and supply leaks may be modeled between the zone equipment loop inlet and the [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) inlet node or the [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) outlet node and the zone supply node).

### Inputs

#### Field: Name

The name of an air distribution system node. This node name is referenced by an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) and in the output listing. Each node should have a unique name within the [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) objects (however, the node name may be used elsewhere as regular EnergyPlus node names such as the fan inlet node or coil outlet node).

#### Field:Component Name or Node Name

Designates node names defined in another EnergyPlus object, so that the [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) object is able to get input parameters and node conditions from the associated EnergyPlus node or object. The actual node name is entered here and represents a node already defined in an [AirLoopHVAC](#airloophvac) or zone equipment loop. This field is left blank if the EnergyPlus Node Type field below is entered as Mixer, Splitter, Outdoor air System, or Other.

#### Field: Component Object Type or Node Type

This choice field distinguishes the node type for the EnergyPlus node or object name defined above. Five node types are available:

- **[AirLoopHVAC:ZoneMixer](#airloophvaczonemixer)**: Represents an [AirLoopHVAC:ZoneMixer](#airloophvaczonemixer) object defined in EnergyPlus
- **[AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter)**: Represents an [AirLoopHVAC:ZoneSplitter](#airloophvaczonesplitter) object defined in EnergyPlus
- **[AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem)**: Represents an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem) object used in EnergyPlus
- **OAMixerOutdoorAirStreamNode**: Represents an external node name specified as an Outdoor Air Stream Node Name in the [OutdoorAir:Mixer](#outdoorairmixer) object when the [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem) object is used.
- **[OutdoorAir:NodeList](#outdoorairnodelist)**: Represents an external node name defined in the [OutdoorAir:NodeList](#outdoorairnodelist) object when the [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem) object and an exhaust energy recovery system (air-to-air heat exchanger) are used.
- **OutdoorAir**: Represents an external node name defined in the [OutdoorAir:Node](#outdoorairnode) object when the [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem) and an exhaust energy recovery system (air-to-air heat exchanger) are used.
- **Other**: Represents a type not already defined above.

> Note: Both the [OutdoorAir:NodeList](#outdoorairnodelist) and [OutdoorAir:Node](#outdoorairnode) node types represent a node to outdoor air conditions. Either one of these node types can be used to represent an external node when an air-to-air heat exchanger is used to recover energy from the exhaust air stream as part of an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem) object. Node type OAMixerOutdoorAirStreamNode does not represent an external node when an [OutdoorAir:NodeList](#outdoorairnodelist) or [OutdoorAir:Node](#outdoorairnode) object is specified. If no exhaust heat recovery system (i.e., air-to-air heat exchanger) is specified in the [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem), the node type OAMixerOutdoorAirStreamNode represents an external node.

#### Field: Node Height

Designates the reference height in meters used to calculate relative pressure. The default value is 0 meters.

IDF examples are provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Node,
        EquipmentInletNode,      !- Name
        Zone Equipment Inlet Node,  !- Component Name or Node Name
        Other,                   !- Component Object Type or Node Type
        3.0;                     !- Node Height {m}

    AirflowNetwork:Distribution:Node,
        SupplyMainNode,          !- Name
        ,                        !- Component Name or Node Name
        Other,                   !- Component Object Type or Node Type
        3.0;                     !- Node Height {m}

    AirflowNetwork:Distribution:Node,
        MainSplitterNode,        !- Name
        ,                        !- Component Name or Node Name
        AirLoopHVAC:ZoneSplitter,  !- Component Object Type or Node Type
        3.0;                     !- Node Height {m}
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Node,
        MainSplitterNode,        !- Name of Node
        ,                        !- Name of Associated EnergyPlus Node or Object
        AirLoopHVAC:ZoneSplitter,  !- EnergyPlus Object or Node Type
        3.0;                     !- Node Height {m}
~~~~~~~~~~~~~~~~~~~~

The next section describes AirflowNetwork Distribution Components, with 7 available types listed below. All required fields for each component represent a relationship between pressure difference and airflow. The components are referenced in [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) objects.

- AirflowNetwork:Distribution:Component:Leak
- AirflowNetwork:Distribution:Component:LeakageRatio
- AirflowNetwork:Distribution:Component:Duct
- AirflowNetwork:Distribution:Component:ConstantVolumeFan
- AirflowNetwork:Distribution:Component:Coil
- AirflowNetwork:Distribution:Component:HeatExchanger
- AirflowNetwork:Distribution:Component:TerminalUnit
- AirflowNetwork:Distribution:Component:ConstantPressureDrop

## AirflowNetwork:Distribution:Component:Leak

This component may be also called a power law component and is used to represent a supply or return air leak in an air distribution system. Its relationship between pressure difference and airflow may be expressed as:

![](media/image237.png)\


where

![](media/image238.png)  = Air mass flow rate through the component [kg/s]

C = Air mass flow coefficient (kg/s at 1 Pa pressure difference)

![](media/image239.png)  = Total pressure loss across the element [Pa]

n = Air mass flow exponent

C~T~~~= Temperature correction factor

![](media/image240.png)\


where

ρ = Air density at the specific air temperature and humidity ratio conditions [kg/m^3^]

ν = Air kinetic viscosity at the specific air temperature condition [m^2^/s]

ρ~o~~~= Air density at air conditions of 20°C, 0 kg/kg and 101325 Pa [kg/m^3^]

ν~o~~~= Air kinetic viscosity at an air temperature of 20°C [m^2^/s]

> Note: The correction factor shown above is use for this particular component as specified.

### Inputs

#### Field: Name

A unique name identifying a supply or return air leak in an air distribution system. This unique name will be referenced by an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object to represent a component leak.

#### Field: Air Mass Flow Coefficient

This numeric field is defined as the air mass flow coefficient at 1 Pa pressure difference across this component. Valid entries must be greater than zero.

#### Field: Air Mass Flow Exponent

This numeric field is defined as the pressure difference exponent across the component. Valid entries are from 0.5 to 1.0, with the default value being 0.65.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Component:Leak,
        MainSupplyLeak,          !- Name of Supply or Return Leak
        0.0001,                  !- Air Mass Flow Coefficient {kg/s}
        0.65;                    !- Air Mass Flow Exponent {dimensionless}

    AirflowNetwork:Distribution:Component:Leak,
        ZoneSupplyLeak,          !- Name of Supply or Return Leak
        0.01,                    !- Air Mass Flow Coefficient {kg/s}
        0.65;                    !- Air Mass Flow Exponent {dimensionless}
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:Distribution:Component:LeakageRatio

The leakage ratio component is generally used to define supply and return leaks with respect to a constant fan flow. This object requires 5 inputs. The relationship between pressure and airflow may be expressed as a power law element:

![](media/image241.png)\


where

 = Air density [kg/m^3^]

![](media/image242.png)  = Total pressure loss across the element [Pa]

n = Air mass flow exponent

C~equ~= Equivalent air mass flow coefficient

![](media/image243.png)\


where

r = Effective leakage ratio [dimensionless]

Q~r~= Maximum airflow rate [m^3^/s]

![](media/image244.png)  = Reference pressure difference [Pa]

n = Air mass flow exponent [dimensionless]

The above calculation is valid only for a HVAC system using a constant volume supply fan: [Fan:ConstantVolume](#fanconstantvolume) or [Fan:OnOff](#fanonoff).

### Inputs

#### Field: Name

A unique name identifying a supply or return air leak (ratio with respect to the constant volume fan flow rate) in an air distribution system. This unique name will be referenced in an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object to represent a component.

#### Field: Effective Leakage Ratio

This numeric field is used to input the effective leakage ratio. This value must be greater than zero and less than or equal to 1. The effective leakage ratio is used to characterize supply and return leaks with respect to the constant fan flow (supply or return air leak flow rate divided by the maximum air flow rate for the fan). When a VAV system is used with Supply Fan Object Type = [Fan:VariableVolume](#fanvariablevolume) in the [AirflowNetwork:Distribution:Component:Fan](#airflownetworkdistributioncomponentfan) object, the ratio for a supply leak is defined as the value of supply air leak flow rate divided by the supply fan air flow rate at the current time step. The inputs of the rest of 3 fields are not used.

#### Field: Maximum Flow Rate

This numeric field is used to input the maximum flow rate for a constant volume fan [m^3^/s]. This value must be greater than 0 m^3^/s. When a VAV system is used with Supply Fan Object Type = [Fan:VariableVolume](#fanvariablevolume) in the [AirflowNetwork:Distribution:Component:Fan](#airflownetworkdistributioncomponentfan) object, this input is not used, because the real supply fan flow rate at the current time step is used to calculate the amount of a supply leak.

#### Field: Reference Pressure Difference

This numeric field is used to input the reference pressure difference [Pa]. This value must be greater than 0 Pa. When a VAV system is used with Supply Fan Object Type = [Fan:VariableVolume](#fanvariablevolume) in the [AirflowNetwork:Distribution:Component:Fan](#airflownetworkdistributioncomponentfan) object, this input is not used, because the real supply fan flow rate at the current time step is used to calculate the amount of a supply leak.

#### Field: Air Mass Flow Exponent

This numeric field is used to input the pressure difference exponent. The value of the exponent can be from 0.5 to 1.0, with the default value being 0.65. When a VAV system is used with Supply Fan Object Type = [Fan:VariableVolume](#fanvariablevolume) in the [AirflowNetwork:Distribution:Component:Fan](#airflownetworkdistributioncomponentfan) object, this input is not used, because the real supply fan flow rate at the current time step is used to calculate the amount of a supply leak.

> Note: The reference pressure difference is defined as the difference between pressures of originate and terminate nodes for supply and return leaks. In general, it may require that a simulation be performed with an initial guess for reference pressure difference using design day conditions. After obtaining pressures at the leakage nodes, more realistic reference pressure differences can be entered. It should be pointed out that since pressures at the nodes vary with temperature and other conditions, the effective leakage ratio is only an estimate. In other words, the exact leakage ratio may not be available.

> When a VAV system is used with Supply Fan Object Type = [Fan:VariableVolume](#fanvariablevolume) in the [AirflowNetwork:Distribution:Component:Fan](#airflownetworkdistributioncomponentfan) object, the [AirflowNetwork:Distribution:Component:LeakageRatio](#airflownetworkdistributioncomponentleakageratio) object has to be used to define a supply leak. The other components cannot be used. In addition, the first node in a [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) to define a supply leak has to be the node in the duct to originate a supply leak, and the second node has to be the node of a zone to terminate a supply leak. In other words, the supply leak has to flow from node 1 to node 2. **** When this object is used to define a return leak, the first node in a [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) has to be a thermal zone to originate a leak, and the second node has to be the node of a duct to terminate a leak.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Component:LeakageRatio,
        Zone1SupplyLeakELA,      !- Name of Effective Leakage Ratio
        0.043527,                !- Effective Leakage Ratio {dimensionless}
        1.0,                     !- Maximum Flow Rate {m3/s}
        20.0,                    !- Reference Pressure Difference {Pa}
        0.65;                    !- Air Mass Flow Exponent {dimensionless}
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:Distribution:Component:Duct

This object represents a duct component and requires 9 input fields, one alpha field and 8 numeric fields. The relationship between pressure and airflow across the component may be expressed as (2001 ASHRAE Handbook of Fundamentals, Chapter 34):

![](media/image245.png)\


where

![](media/image246.png)  = Mass flow rate of air through the component [kg/s]

= Air density [kg/m^3^]

A = Cross sectional area [m^2^]

![](media/image247.png)  = Total pressure loss across the component [Pa]

L = [Duct](#duct) length [m]

D = Hydraulic diameter [m]

C~d~= Dynamic loss coefficient due to fitting [dimensionless]

f = Friction factor

The friction factor can be calculated using the nonlinear Colebrook equation (ASHRAE Handbook of Fundamentals, 1997. p. 2.9, Eq. 29b)

![](media/image248.png)\


where

 = Surface roughness [m]

Re = Reynolds number = ![](media/image249.png)

### Inputs

#### Field: Name 

A unique name for an AirflowNetwork duct component in an air distribution system. This unique name will be referenced by an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object to represent a component.

#### Field: Duct Length

This numeric field is used to input duct length [m]. This value must be greater than zero.

#### Field: Hydraulic Diameter

This numeric field is used to input hydraulic diameter, which is defined as:

![](media/image250.png)\


where

D~h~= Hydraulic diameter [m]

A = [Duct](#duct) cross sectional area [m^2^]

P = Perimeter of cross section [m]

#### Field: Cross Section Area

This numeric field is used to input cross section area [m^2^]. The model assumes that this element has no area change along its length. Otherwise, effective cross sectional area is required.

#### Field: Surface Roughness

This numeric field is used to input surface roughness [m]. This value must be greater than zero, and the default value is 0.0009 m.

#### Field: Coefficient for Local Dynamic Loss Due to fitting

This numeric field is defined as a coefficient for dynamic loss [dimensionless]. It represents dynamic loss due to fittings (such as an elbow).

#### Field: Overall Heat Transmittance Coefficient (U-Factor) from Air to Air

This numeric field is defined as the overall heat transmittance coefficient (U value, W/m^2^-K) from air to air, including film coefficients at both surfaces.

#### Field: Overall Moisture Transmittance Coefficient from Air to Air

This numeric field is defined as the overall moisture transmittance coefficient (kg/m^2^) from air to air, including film coefficients at both surfaces.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Component:Duct,
        MainTruck1,              !- Name of Duct Component
        3.0,                     !- Duct Length {m}
        0.6,                     !- Hydraulic Diameter {m}
        0.2827,                  !- Cross Section Area {m2}
        0.0009,                  !- Surface Roughness {m}
        0.01,                    !- Coefficient for local dynamic loss due to fitting {dimensionless}
        0.772,                   !- Overall heat transmittance coefficient (U value) from air to air
                                 !- {W/m2-K}
        0.0001;                  !- Overall moisture transmittance coefficient from air to air {kg/m2}
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:Distribution:Component:Fan

This component represents a constant volume fan in the air distribution system ([AirLoopHVAC](#airloophvac)). The air flow rate and air conditions (temperature and humidity) are obtained from the associated [Fan:ConstantVolume](#fanconstantvolume), [Fan:OnOff](#fanonoff), or [Fan:VariableVolume](#fanvariablevolume) object.

### Inputs

#### Field: Fan Name

The name identifying an AirflowNetwork constant volume fan in an air distribution system. This name must be the same as the name of the associated [Fan:ConstantVolume](#fanconstantvolume),  [Fan:OnOff](#fanonoff) or [Fan:VariableVolume](#fanvariablevolume) object. This name will be referenced by an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object to represent a component.

#### Field: Supply Fan Object Type

This choice field defines the type of fan. The only valid choices are [Fan:OnOff](#fanonoff),  [Fan:ConstantVolume](#fanconstantvolume), and [Fan:VariableVolume](#fanvariablevolume), with the default being [Fan:ConstantVolume](#fanconstantvolume). Both cycling and continuous fan operating modes are allowed for [Fan:OnOff](#fanonoff). Only the continuous fan operating mode is allowed for [Fan:ConstantVolume](#fanconstantvolume). The variable airflow rate is allowed for [Fan:VariableVolume](#fanvariablevolume).

> Note: Make sure that the volumetric air flow rates for the fan, coils, and parent components (e.g., unitary system or furnace) are the same so that fan energy and air distribution system losses/gains are properly calculated.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Component:Fan,
        Supply Fan 1,            !- Name of Constant Volume Fan
        Fan:ConstantVolume;  !- Supply fan type
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:Distribution:Component:Coil

This component represents a cooling or heating coil. The main purpose for this object is to get calculated values (air flow and temperature/humidity conditions) from the associated coil models.

### Inputs

#### Field: Coil Name 

The name identifying an AirflowNetwork cooling coil or heating coil defined in an air loop. This name must be the same name as the associated coil object. This unique name will be referenced by an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object to represent a component.

#### Field: Coil Object Type 

This field requires input of the coil type used in the AirflowNetwork model. The available choices are:

- Coil:Cooling:DX:SingleSpeed
- Coil:Heating:Gas
- Coil:Heating:Electric
- Coil:Heating:DX:SingleSpeed
- Coil:Cooling:Water
- Coil:Heating:Water
- Coil:Cooling:Water:DetailedGeometry
- Coil:Cooling:DX:TwoStageWithHumidityControlMode
- Coil:Cooling:DX:MultiSpeed
- Coil:Heating:DX:MultiSpeed
- Coil:Heating:Desuperheater

#### Field: Air Path Length

This numeric field is used to input air path length for the coil [m]. This value must be greater than 0 meters.

#### Field: Air Path Hydraulic Diameter

This numeric field is used to input hydraulic diameter of a coil's air path, which is defined as:

![](media/image251.png)\


where

D~h~= Hydraulic diameter [m]

A = [Duct](#duct) cross section area [m^2^]

P = Perimeter of cross section [m]

For this component, the relationship between airflow and pressure is similar to the component [AirflowNetwork:Distribution:Component:Duct](#airflownetworkdistributioncomponentduct). However, the model assumes very small surface roughness (10^-4^) and no local dynamic loss due to fittings for this component. Therefore, this component only requires two numerical fields. Heat and moisture exchange from surroundings is ignored.

> Note: Make sure that the volumetric air flow rates for the fan, coils, and parent components (e.g., unitary system or furnace) are the same so that fan energy and air distribution system losses/gains are properly calculated.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Component:Coil,
        ACDXCoil 1,                            !- Name of Associated EnergyPlus Coil
        Coil:Cooling:DX:SingleSpeed,           !- EnergyPlus Coil Type
        0.1,                                   !- Air Path Length {m}
    1.00;                                  !- Air Path Hydraulic Diameter {m}

    AirflowNetwork:Distribution:Component:Coil,
        HP Heating Coil 1,                     !- Name of Associated EnergyPlus Coil
        Coil:Heating:DX:SingleSpeed,           !- EnergyPlus Coil Type
        0.1,                                   !- Air Path Length {m}
        1.00;                                  !- Air Path Hydraulic Diameter {m}
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:Distribution:Component:HeatExchanger

This component represents an air-to-air heat exchanger typically used in combination with a cooling coil to enhance dehumidification or in an outside air system to recover energy from exhaust air to pretreat incoming outdoor ventilation air. The cooling coils with enhanced dehumidification are defined in the two objects [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted) and [CoilSystem:Cooling:Water:HeatExchangerAssisted](#coilsystemcoolingwaterheatexchangerassisted) using one of three heat exchanger objects specified below. The exhaust air energy recovery system also has the same restriction using the one of three heat exchanger objects. The main purpose for this object is to obtain calculated values (air flow and temperature/humidity conditions) from the associated heat exchanger models for the airflow network calculations.

### Inputs

#### Field: Heat Exchanger Name 

The name identifying an AirflowNetwork heat exchanger defined in an air loop. This name must be the same name that is used in the associated heat exchanger object. This unique name will be referenced by an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object to represent a component.

#### Field: Heat Exchanger Object Type 

This field requires input of the heat exchanger type. The available choices are:

-    HeatExchanger:AirToAir:FlatPlate
-    HeatExchanger:AirToAir:SensibleAndLatent
-    HeatExchanger:Desiccant:BalancedFlow

#### Field: Air Path Length

This numeric field is used to input air path length for the heat exchanger coil [m]. This value must be greater than 0 meters.

#### Field: Air Path Hydraulic Diameter

This numeric field is used to input hydraulic diameter of a heat exchanger coil's air path, which is defined as:

![](media/image252.png)\


where

D~h~= Hydraulic diameter [m]

A = [Duct](#duct) cross section area [m^2^]

P = Perimeter of cross section [m]

For this component, the relationship between airflow and pressure is similar to the component [AirflowNetwork:Distribution:Component:Duct](#airflownetworkdistributioncomponentduct). However, the model assumes very small surface roughness (10^-4^) and no local dynamic loss due to fittings for this component. Therefore, this component only requires two numerical fields. Heat and moisture exchange from surroundings are ignored.

> Note: When a heat exchanger is used as a component of either [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted) or [CoilSystem:Cooling:Water:HeatExchangerAssisted](#coilsystemcoolingwaterheatexchangerassisted), the heat exchanger acts as two components in an air primary loop. For example, an air-to-air heat exchanger has a component connected to the supply air side (equivalent to a supply coil) and a component connected to exhaust air side (equivalent to an exhaust coil). The desiccant heat exchanger has a component connected to regeneration air side (equivalent to a regeneration air coil) and a component connected to process air side (equivalent to a process air coil). Therefore, each air-to-air heat exchanger used in this configuration requires two linkage objects (instead of only one linkage object as required for other AirflowNetwork components).

> When a heat exchanger is used in an exhaust air energy recovery system (i.e., in an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem) object to recover waste heat from exhaust air to pretreat incoming outdoor ventilation air), the heat exchanger is treated as a single component. The AirflowNetwork model only connects the two nodes associated with the incoming outdoor ventilation air, while the two exhaust nodes are not defined as part of the AirflowNetwork model. Therefore, each heat exchanger component used in an exhaust air energy recovery system has only one linkage object, similar to AirflowNetwork coil components.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Component:HeatExchanger,
        OA Heat Recovery 1,      !- HeatExchanger Name
        HeatExchanger:AirToAir:SensibleAndLatent,  !- HeatExchanger Object Type
        0.1,                     !- Air Path Length {m}
        1.00;                    !- Air Path Hydraulic Diameter {m}

    AirflowNetwork:Distribution:Component:HeatExchanger,
        Desiccant Heat Exchanger 1, !- HeatExchanger Name
        HeatExchanger:Desiccant:BalancedFlow,  !- HeatExchanger Object Type
        0.1,                     !- Air Path Length {m}
        1.00;                    !- Air Path Hydraulic Diameter {m}
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:Distribution:Component:TerminalUnit

This component represents a terminal unit for reheating the incoming supply air. The main purpose is to get calculated values from the terminal unit models.

### Inputs

#### Field: Terminal Unit Name 

A name identifying an AirflowNetwork terminal unit defined in a zone equipment list. This name must be the same as the associated terminal unit object. This unique name will be referenced by an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object to represent a component.

#### Field: Terminal Unit Object Type 

This field requires input of the terminal unit type used in the AirflowNetwork model. The available types are [AirTerminal:SingleDuct:ConstantVolume:Reheat](#airterminalsingleductconstantvolumereheat) and [AirTerminal:SingleDuct:VAV:Reheat](#airterminalsingleductvavreheat). The [AirTerminal:SingleDuct:ConstantVolume:Reheat](#airterminalsingleductconstantvolumereheat) type is used when the Supply Fan Object Type in the [AirflowNetwork:Distribution:Component:Fan](#airflownetworkdistributioncomponentfan) is either [Fan:ConstantVolume](#fanconstantvolume) or [Fan:OnOff](#fanonoff). The [AirTerminal:SingleDuct:VAV:Reheat](#airterminalsingleductvavreheat) type is used when the Supply Fan Object Type in the [AirflowNetwork:Distribution:Component:Fan](#airflownetworkdistributioncomponentfan) is [Fan:VariableVolume](#fanvariablevolume) only.

#### Field: Air Path Length

This numeric field is used to input the air path length for the terminal unit [m]. This value must be greater than 0 meters.

#### Field: Air Path Hydraulic Diameter

This numeric field is used to input hydraulic diameter for the terminal unit's air path, which is defined as:

![](media/image253.png)\


where

D~h~= Hydraulic diameter [m]

A = [Duct](#duct) cross section area [m^2^]

P = Perimeter of cross section [m]

It should be noted that the relationship for this component between airflow and pressure is similar to the component [AirflowNetwork:Distribution:Component:Duct](#airflownetworkdistributioncomponentduct). However, the model assumes very small surface roughness (10^-4^) and no local dynamic loss due to fittings for this component. Therefore, this component only requires two numerical fields. Heat and moisture exchange from surroundings is ignored.

> Note: The [AirflowNetwork:Distribution:Component:TerminalUnit](#airflownetworkdistributioncomponentterminalunit) object is used to represent an [AirTerminal:SingleDuct:ConstantVolume:Reheat](#airterminalsingleductconstantvolumereheat) or an [AirTerminal:SingleDuct:VAV:Reheat](#airterminalsingleductvavreheat) object in an AirflowNetwork simulation. The [AirflowNetwork:Distribution:Component:TerminalUnit](#airflownetworkdistributioncomponentterminalunit) should not be used to represent any other air terminal unit types. When the [ZoneHVAC:EquipmentList](#zonehvacequipmentlist) object specifies an Air:Terminal:SingleDuct:Uncontrolled object, the [AirflowNetwork:Distribution:Component:Duct](#airflownetworkdistributioncomponentduct) object should be used instead.

> The [AirTerminal:SingleDuct:VAV:Reheat](#airterminalsingleductvavreheat) object has two components: a damper and a reheat coil. When the [AirTerminal:SingleDuct:VAV:Reheat](#airterminalsingleductvavreheat) type is used, two objects of [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) have to be used to make two links, one of which is a link to connect two nodes for a damper, and the other is a link to connect two nodes for a reheat coil.

> When a VAV system is used with Supply Fan Object Type = [Fan:VariableVolume](#fanvariablevolume) in the [AirflowNetwork:Distribution:Component:Fan](#airflownetworkdistributioncomponentfan) object, the type of all terminals has to be [AirTerminal:SingleDuct:VAV:Reheat](#airterminalsingleductvavreheat). The object of Air:Terminal:SingleDuct:Uncontrolled is not allowed.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Component:TerminalUnit,
        Reheat Zone 1,                    !- Name of Associated Energyplus Terminal Unit
        AirTerminal:SingleDuct:ConstantVolume:Reheat,  !- EnergyPlus Terminal Unit Type
        0.1,                              !- Air Path Length {m}
        0.44;                             !- Air Path Hydraulic Diameter {m}
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:Distribution:Component:ConstantPressureDrop

This component represents a constant pressure drop component. It is generally used to simulate a constant pressure drop filter. The mathematical equation may be written as:

![](media/image254.png)\


### Inputs

#### Field: Name

A unique name identifying an AirflowNetwork constant pressure drop component in an air distribution system. This unique name will be referenced by an AirflowNetwork: Distribution:Linkage object to represent a component.

#### Field: Pressure Difference across the Component

This numeric field is used to input the pressure difference across the element [Pa].

> Note: This object should be used with caution. Each node connected to this object cannot be a node for a mixer or splitter, a node in an [AirLoopHVAC](#airloophvac), or a node in a zone configuration loop. It is recommended that duct components be specified at both ends of this object.

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Component:ConstantPressureDrop,
     SupplyCPDComp, ! Name of Constant Pressure Drop Component
     1.0;           ! Pressure Difference Across the Component [Pa]
~~~~~~~~~~~~~~~~~~~~

## AirflowNetwork:Distribution:Linkage

The [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) represents a connection between two [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) objects and an AirflowNetwork component defined above. In addition, the relative height from node height to linkage height for each node is required.

### Inputs

#### Field: Name

The name identifies the linkage for later reference and in the output listing. Each linkage should have a unique name.

#### Field: Node 1 Name

Designates a node name where airflow starts. The node name should be defined in an [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) object.

#### Field: Node 2 Name

Designates a node name where airflow ends. The node name should be defined in an [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) object.

#### Field: Component Name

Designates an AirflowNetwork component name associated with the two nodes. The component name should be one of the AirflowNetwork:Distribution:Component… object names.

#### Field: Thermal Zone Name

Designates a thermal zone where the linkage is located. The information provides the ambient conditions for duct elements to calculate duct conduction losses (only used if component is [AirflowNetwork:Distribution:Component:Duct](#airflownetworkdistributioncomponentduct)).

An IDF example is provided below:

~~~~~~~~~~~~~~~~~~~~

    AirflowNetwork:Distribution:Linkage,
        Main Link 1,             !- Name of Linkage
        EquipmentInletNode,      !- Node 1 Name
        SupplyMainNode,          !- Node 2 Name
        MainTruck1,              !- Component Name
        Attic Zone;              !- Thermal Zone Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The AirflowNetwork nodes in the following output variables includes zones defined in AirflowNetwork:Multizone:Zone objects, external nodes defined in [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) objects, and nodes defined in [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) objects.

The AirflowNetwork linkage used in following output variables includes surfaces defined in AirflowNetwork:Multizone:Surface objects, and linkages defined in [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) objects. The surface linkages represent airflows through surface cracks or openings between two zones or between a zone and outdoors. The distribution linkages represent airflows in an air distribution system.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,AFN Node Temperature [C]
    HVAC,Average,AFN Node Humidity Ratio [kgWater/kgDryAir]
    HVAC,Average,AFN Node Total Pressure [Pa]
    HVAC,Average,AFN Node Wind Pressure [Pa]
    HVAC,Average,AFN Node CO2 Concentration [ppm]
    HVAC,Average,AFN Node Generic Air Contaminant Concentration [ppm]
    HVAC,Average,AFN Linkage Node 1 to Node 2 Mass Flow Rate [kg/s]
    HVAC,Average,AFN Linkage Node 2 to Node 1 Mass Flow Rate [kg/s]
    HVAC,Average,AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s]
    HVAC,Average,AFN Linkage Node 2 to Node 1 Volume Flow Rate [m3/s]
    HVAC,Average,AFN Linkage Node 1 to Node 2 Pressure Difference [Pa]
    HVAC,Average,AFN Surface Venting Window or Door Opening Factor []
    HVAC,Average,AFN Surface Venting Window or Door Opening Modulation Multiplier []
    HVAC,Average,AFN Surface Venting Inside Setpoint Temperature [C]
    HVAC,Average,AFN Surface Venting Availability Status []
    HVAC,Average,AFN Zone Infiltration Sensible Heat Gain Rate [W]
    HVAC,Sum,AFN Zone Infiltration Sensible Heat Gain Energy [J]
    HVAC,Average,AFN Zone Mixing Sensible Heat Gain Rate [W]
    HVAC,Sum,AFN Zone Mixing Sensible Heat Gain Energy [J]
    HVAC,Average,AFN Zone Infiltration Sensible Heat Loss Rate [W]
    HVAC,Sum,AFN Zone Infiltration Sensible Heat Loss Energy [J]
    HVAC,Average,AFN Zone Mixing Sensible Heat Loss Rate [W]
    HVAC,Sum,AFN Zone Mixing Sensible Heat Loss Energy [J]
    HVAC,Average,AFN Zone Infiltration Latent Heat Gain Rate [W]
    HVAC,Sum,AFN Zone Infiltration Latent Heat Gain Energy [J]
    HVAC,Average,AFN Zone Infiltration Latent Heat Loss Rate [W]
    HVAC,Sum,AFN Zone Infiltration Latent Heat Loss Energy [J]
    HVAC,Average,AFN Zone Mixing Latent Heat Gain Rate [W]
    HVAC,Sum,AFN Zone Mixing Latent Heat Gain Energy [J]
    HVAC,Average,AFN Zone Mixing Latent Heat Loss Rate [W]
    HVAC,Sum,AFN Zone Mixing Latent Heat Loss Energy [J]
    HVAC,Average,AFN Zone Duct Leaked Air Sensible Heat Gain Rate [W]
    HVAC,Sum,AFN Zone Duct Leaked Air Sensible Heat Gain Energy [J]
    HVAC,Average,AFN Zone Duct Leaked Air Sensible Heat Loss Rate [W]
    HVAC,Sum,AFN Zone Duct Leaked Air Sensible Heat Loss Energy [J]
    HVAC,Average,AFN Zone Duct Leaked Air Latent Heat Gain Rate [W]
    HVAC,Sum,AFN Zone Duct Leaked Air Latent Heat Gain Energy [J]
    HVAC,Average,AFN Zone Duct Leaked Air Latent Heat Loss Rate [W]
    HVAC,Sum,AFN Zone Duct Leaked Air Latent Heat Loss Energy [J]
    HVAC,Average,AFN Zone Duct Conduction Sensible Heat Gain Rate [W]
    HVAC,Sum,AFN Zone Duct Conduction Sensible Heat Gain Energy [J]
    HVAC,Average,AFN Zone Duct Conduction Sensible Heat Loss Rate [W]
    HVAC,Sum,AFN Zone Duct Conduction Sensible Heat Loss Energy [J]
    HVAC,Average,AFN Zone Duct Diffusion Latent Heat Gain Rate [W]
    HVAC,Sum,AFN Zone Duct Diffusion Latent Heat Gain Energy [J]
    HVAC,Average,AFN Zone Duct Diffusion Latent Heat Loss Rate [W]
    HVAC,Sum,AFN Zone Duct Diffusion Latent Heat Loss Energy [J]
    HVAC,Average,AFN Distribution Sensible Heat Gain Rate [W]
    HVAC,Sum,AFN Distribution Sensible Heat Gain Energy [J]
    HVAC,Average,AFN Distribution Sensible Heat Loss Rate [W]
    HVAC,Sum,AFN Distribution Sensible Heat Loss Energy [J]
    HVAC,Average,AFN Distribution Latent Heat Gain Rate [W]
    HVAC,Sum,AFN Distribution Latent Heat Gain Energy [J]
    HVAC,Average,AFN Distribution Latent Heat Loss Rate [W]
    HVAC,Sum,AFN Distribution Latent Heat Loss Energy [J]
    HVAC,Sum,AFN Zone Infiltration Volume [m3]
    HVAC,Sum,AFN Zone Infiltration Mass [kg]
    HVAC,Average,AFN Zone Infiltration Air Change Rate [ach]
    HVAC,Sum,AFN Zone Mixing Volume [m3]
    HVAC,Sum,AFN Zone Mixing Mass [kg]

    The following are reported only when a Fan:OnOff object is used:
    HVAC,Average,AFN Zone Average Pressure [Pa]
    HVAC,Average,AFN Zone On Cycle Pressure [Pa]
    HVAC,Average,AFN Zone Off Cycle Pressure [Pa]
    HVAC,Average,AFN Linkage Node 1 to 2 Average Mass Flow Rate [kg/s]
    HVAC,Average,AFN Linkage Node 2 to 1 Average Mass Flow Rate [kg/s]
    HVAC,Average,AFN Linkage Node 1 to 2 Average Volume Flow Rate [m3/s]
    HVAC,Average,AFN Linkage Node 2 to 1 Average Volume Flow Rate [m3/s]
    HVAC,Average,AFN Surface Average Pressure Difference [Pa]
    HVAC,Average,AFN Surface On Cycle Pressure Difference [Pa]
    HVAC,Average,AFN Surface Off Cycle Pressure Difference [Pa]
~~~~~~~~~~~~~~~~~~~~

#### AFN Node Temperature [C]

This is the AirflowNetwork node temperature output in degrees C. When a [Fan:OnOff](#fanonoff) object is used and is scheduled to operate in the cycling fan operation mode, this value for [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) objects reflects the temperature when the fan is operating (ON).

#### AFN Node Humidity Ratio [kgWater/kgDryAir]

This is the AirflowNetwork node humidity ratio output in kgWater/kgDryAir. When a [Fan:OnOff](#fanonoff) object is used and is scheduled to operate in the cycling fan operation mode, this value for [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) objects reflects the humidity ratio when the fan is operating (ON).

#### AFN Node Total Pressure [Pa]

This is the AirflowNetwork node total pressure in Pa with respect to outdoor barometric pressure. The total pressure is the sum of static pressure, dynamic pressure, and elevation impact at the node's relative height. When a [Fan:OnOff](#fanonoff) object is used and is scheduled to operate in the cycling fan operation mode, the value for [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) objects reflects the total pressure when the fan is operating (ON). The total pressures for nodes associate with AirflowNetwork:Multizone:Zone objects are reported in different output variables (below).

#### AFN Node CO2 Concentration [ ppm]

This is the AirflowNetwork node carbon dioxide concentration level in parts per million (ppm). When a [Fan:OnOff](#fanonoff) object is used and is scheduled to operate in the cycling fan operation mode, this value for [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) objects reflects the carbon dioxide when the fan is operating (ON).

#### AFN Node Generic Air Contaminant Concentration [ppm]

This is the AirflowNetwork node generic contaminant concentration level in parts per million (ppm). When a [Fan:OnOff](#fanonoff) object is used and is scheduled to operate in the cycling fan operation mode, this value for [AirflowNetwork:Distribution:Node](#airflownetworkdistributionnode) objects reflects the carbon dioxide when the fan is operating (ON).

#### AFN Zone Average Pressure [Pa]

This is the AirflowNetwork average zone total pressure in Pa with respect to outdoor barometric pressure. This output is only available when a [Fan:OnOff](#fanonoff) object is used in the air distribution system. The average zone pressure is weighted by the system fan part-load ratio using the calculated zone pressures during the fan on and off periods for the system timestep. The system fan part-load ratio is defined as the ratio of the air distribution system mass flow rate (average for the simulation timestep) to the system design mass flow rate.

Average zone pressure = ([Zone](#zone) pressure during on cycle \* Part-load ratio) + [Zone](#zone) pressure during off cycle \* (1.0 – Part-load ratio)

#### AFN Zone On Cycle Pressure [Pa]

This is the AirflowNetwork zone total pressure in Pa with respect to outdoor barometric pressure when the air distribution system fan is operating (ON). This output is only available when a [Fan:OnOff](#fanonoff) object is used in the air distribution system. When the fan part-load ratio is equal to 0.0, this pressure value will be zero because the air distribution system is not simulated when the fan is off for the entire timestep.

#### AFN Zone Off Cycle Pressure [Pa]

This is the AirflowNetwork zone total pressure in Pa with respect to outdoor barometric pressure when the air distribution system fan is not operating (OFF). This output is only available when a [Fan:OnOff](#fanonoff) object is used in the air distribution system. Even if the fan part-load ratio is equal to 1.0, the pressure calculated as if the fan were not operating (OFF) is reported.

#### AFN Node Wind Pressure [Pa]

This is the AirflowNetwork wind pressure output in Pa. The wind pressure depends on several factors, including wind speed, wind direction, the wind-pressure coefficient (Cp) values for the [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) associated with the heat transfer surface and the site wind conditions.

When Wind Pressure Coefficient Type = "Input" in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object, the output represents external node pressures driven by wind defined in an [AirflowNetwork:MultiZone:ExternalNode](#airflownetworkmultizoneexternalnode) object. When Wind Pressure Coefficient Type = "SurfaceAverageCalculation" in [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol), the program assumes five external nodes:

- FACADE1Representing north orientation
- FACADE2Representing east orientation
- FACADE3Representing south orientation
- FACADE4Representing west orientation
- ROOFRepresenting horizontal orientation

In this case, the output represents the wind pressures for the five external nodes defined above.

#### AFN Linkage Node 1 to Node 2 Mass Flow Rate [kg/s]

This is the AirflowNetwork linkage mass flow rate output in kg/s in the direction from Node 1 to Node 2. It reports surface airflows through a crack or opening, and through linkages defined in an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object. The surface linkage is divided into two types of surfaces, exterior surface and interior surface. Node 1 for an exterior surface linkage is a thermal zone and Node 2 is an external node. The value of AFN Linkage Node 1 to Node 2 Mass Flow Rate represents the flow rate from a thermal zone to outdoors. The flow direction through an interior surface crack or opening is defined from a thermal zone defined by a surface's [Zone](#zone) Name (Node 1) to an adjacent thermal zone defined by a surface's OutsideFaceEnvironment (Node 2). For an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object, the value represents the air mass flow rate flowing from Node 1 to Node 2.

It should be pointed out that in general, each linkage has one directional flow at any given time, either from Node 1 to 2 or from Node 2 to 1. However, there are three components which may have flows in both directions simultaneously: [AirflowNetwork:MultiZone:Component:DetailedOpening](#airflownetworkmultizonecomponentdetailedopening), [AirflowNetwork:MultiZone:Component:SimpleOpening](#airflownetworkmultizonecomponentsimpleopening), and [AirflowNetwork:MultiZone:Component:HorizontalOpening](#airflownetworkmultizonecomponenthorizontalopening).

When a [Fan:OnOff](#fanonoff) object is used, the air mass flow rates reported for the [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) objects are the values when the fan is operating (ON). It is assumed that the air mass flow rates when the fan is off are zero for the distribution system air linkage objects. The air mass flow rates for the AirflowNetwork:Multizone:Surface object are reported in different output variables (below).

#### AFN Linkage Node 2 to Node 1 Mass Flow Rate [kg/s]

This is the AirflowNetwork linkage mass flow rate output in kg/s in the direction from Node 2 to Node 1. It reports airflows from surfaces through a crack or opening, and from linkages defined in an [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) object. Node 1 and Node 2 for a surface or subsurface are defined in the same manner as AFN Linkage Node 1 to Node 2 Mass Flow Rate.

When a [Fan:OnOff](#fanonoff) object is used, the air mass flow rates reported for the [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) objects are the values when the fan is operating (ON). It is assumed that the air mass flow rates when the fan is off are zero for the distribution system air linkage objects. The air mass flow rates for the AirflowNetwork:Multizone:Surface object are reported in different output variables (below).

#### AFN Linkage Node 1 to 2 Average Mass Flow Rate [kg/s]

This is the AirflowNetwork linkage average mass flow rate in kg/s in the direction from Node 1 to Node 2 defined in the AirflowNetwork:Multizone:Surface objects. This output is only available when a [Fan:OnOff](#fanonoff) object is used in the air distribution system. The average mass flow rate is weighted by the system fan part-load ratio using the calculated air mass flow rates during the fan on and off periods for the system timestep. The system fan part-load ratio is defined as the ratio of the air distribution system mass flow rate (average for the simulation timestep) to the system design mass flow rate.

Average surface mass flow rate = (Surface mass flow rate during on cycle \* Part-load ratio) + Surface mass flow rate during off cycle \* (1.0 – Part-load ratio)

#### AFN Linkage Node 2 to 1 Average Mass Flow Rate [kg/s]

This is the AirflowNetwork linkage average mass flow rate in kg/s in the direction from Node 2 to Node 1 defined in the AirflowNetwork:Multizone:Surface objects. This output is only available when a [Fan:OnOff](#fanonoff) object is used in the air distribution system. The average mass flow rate is weighted by the system fan part-load ratio using the calculated air mass flow rates during the fan on and off periods for the system timestep. The system fan part-load ratio is defined as the ratio of the air distribution system mass flow rate (average for the simulation timestep) to the system design mass flow rate.

#### AFN Linkage Node 1 to Node 2 Volume Flow Rate [m^3^/s]

This is the AirflowNetwork linkage volume flow rate output in m^3^/s in the direction from the Node 1 to Node 2. It is defined in the same manner as AFN Linkage Node 1 to Node 2 Mass Flow Rate.

When a [Fan:OnOff](#fanonoff) object is used, the air volume flow rates reported for the [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) objects are the values when the fan is operating (ON). It is assumed that the air volume flow rates when the fan is off are zero for the distribution system air linkage objects. The air volume flow rates for the AirflowNetwork:Multizone:Surface object are reported in different output variables (below).

#### AFN Linkage Node 2 to Node 1 Volume Flow Rate [m^3^/s]

This is the AirflowNetwork linkage volume flow rate output in m^3^/s in the direction from Node 2 to Node 1. It is defined in the same manner as AFN Linkage Node 2 to Node 1 Mass Flow Rate.

When a [Fan:OnOff](#fanonoff) object is used, the air volume flow rates reported for the [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) objects are the values when the fan is operating (ON). It is assumed that the air volume flow rates when the fan is off are zero for the distribution system air linkage objects. The air volume flow rates for the AirflowNetwork:Multizone:Surface object are reported in different output variables (below).

#### AFN Linkage Node 1 to 2 Average Volume Flow Rate [m^3^/s]

This is the AirflowNetwork linkage average volume flow rate in m^3^/s in the direction from Node 1 to Node 2 defined in the AirflowNetwork:Multizone:Surface objects. This output is only available when a [Fan:OnOff](#fanonoff) object is used. The average volume flow rate is weighted by the system fan part-load ratio using the calculated air volume flow rates during the fan on and off periods for the system timestep.

Average surface volume flow rate = (Surface volume flow rate during on cycle \* Part-load ratio) + Surface volume flow rate during off cycle \* (1.0 – Part-load ratio)

#### AFN Linkage Node 2 to 1 Average Volume Flow Rate [m^3^/s]

This is the AirflowNetwork linkage average volume flow rate in m^3^/s in the direction from Node 2 to Node 1 defined in the AirflowNetwork:Multizone:Surface objects. This output is only available when a [Fan:OnOff](#fanonoff) object is used. The average volume flow rate is weighted by the system fan part-load ratio using the calculated air volume flow rates during the fan on and off periods for the system timestep.

#### AFN Linkage Node 1 to Node 2 Pressure Difference [Pa]

This is the pressure difference across a linkage in Pa. The linkage includes both objects: AirflowNetwork:Multizone:Surface and AirflowNetwork: Distribution:Linkage.

When a [Fan:OnOff](#fanonoff) object is used, the pressure differences reported for the [AirflowNetwork:Distribution:Linkage](#airflownetworkdistributionlinkage) objects are the values calculated when the fan is operating (ON). It is assumed that the pressure differences when the fan is off are zero for the distribution system air linkage objects. The pressure differences defined in the AirflowNetwork:Multizone:Surface are reported in different output variables (below).

#### AFN Surface Average Pressure Difference [Pa]

This is the average pressure difference across a linkage in Pa for the AirflowNetwork:Multizone:Surface objects only when a [Fan:OnOff](#fanonoff) object is used. The average pressure difference is weighted by the system fan part-load ratio using the calculated pressure differences during the fan on and off periods for the system timestep. The system fan part-load ratio is defined as the ratio of the air distribution system mass flow rate (average for the simulation timestep) to the system design mass flow rate.

Surface Average Pressure Difference = (Surface Average Pressure Difference during on cycle \* Part-load ratio) + Surface Average Pressure Difference during off cycle \* (1.0 - Part-load ratio)

#### AFN Surface On Cycle Pressure Difference [Pa]

This is the pressure difference across a linkage in Pa for the AirflowNetwork:Multizone:Surface objects only when the air distribution system fan is operating (ON). This output is only available when a [Fan:OnOff](#fanonoff) object is used. When the fan part-load ratio is equal to 0.0, this pressure difference value will be zero because the air distribution system is not simulated when the fan is off for the entire timestep.

#### AFN Surface Off Cycle Pressure Difference [Pa]

This is the pressure difference across a linkage in Pa for the AirflowNetwork:Multizone:Surface objects only when the air distribution system fan is not operating (OFF). This output is only available when a [Fan:OnOff](#fanonoff) object is used. Even if the fan part-load ratio is equal to 1.0, the pressure difference calculated as if the fan were not operating (OFF) is reported.

#### AFN Surface Venting Window or Door Opening Factor []

The current time-step value of the venting opening factor for a particular window or door. When the window or door is venting, this is the input value of the opening factor (see AirflowNetwork:Multizone:Surface, Window/Door Opening Factor) times the multiplier for venting modulation (see description of next output variable, "Opening Factor Multiplier for AirflowNetwork Venting Modulation"). For example, if  the input Window/Door opening factor is 0.5 and the modulation multiplier is 0.7, then the value of this output variable will be 0.5x0.7 = 0.35.

#### AFN Surface Venting Window or Door Opening Modulation Multiplier  []

This is the multiplier on a window or door opening factor when venting modulation is in effect. See "Modulation of Openings" under AirflowNetwork:Multizone:Zone for a description of how the multiplier is determined.

When modulation is in effect the value of the multiplier is between 0.0 and 1.0. When modulation does not apply the value of the multiplier may be –1.0. When modulation applies but the surface is not venting, the value is –1.0. This is summarized in the following table. In this table, "[Zone](#zone)" means a thermal zone for which AirflowNetwork:Multizone:[Zone](#zone) has been specified. See object AirflowNetwork: Multizone:[Zone](#zone) for definition of "Ventilation Control Mode."

Table: Value of opening factor multiplier for different venting conditions.

**Is surface in a Zone?**|**Ventilation Control Mode**|**Is surface venting?**|**Value of opening factor multiplier**
--------------------------------------|-----------------------------------------|------------------------------------|---------------------------------------------------
Yes|**Temperature**|Yes|0.0 to 1.0

**
No|-1.0

**Enthalpy**|Yes|0.0 to 1.0

**
No|-1.0

**Constant**|Yes|1.0

**NoVent**|No|-1.0

#### AFN Surface Venting Inside Setpoint Temperature [C]

The time-step value of the venting setpoint temperature for the zone to which the surface belongs. This setpoint is determined from the Vent Temperature Schedule input (ref: AirflowNetwork:Multizone:Zone).

#### AFN Surface Venting Availability Status [ ]

A value of 1.0 means venting through the surface can occur if venting control conditions are satisfied. A value of 0.0 means venting through the surface cannot occur under any circumstances. This value is determined by the Venting Availability Schedule input (ref: AirflowNetwork:Multizone:Zone or AirflowNetwork:Multizone: Surface).

#### AFN Zone Infiltration Sensible Heat Gain Rate [W]

The average convective sensible heat gain rate, in Watts, to the zone air corresponding to the [Zone](#zone) Infiltration Volume averaged over the reporting period. This value is calculated for each timestep when the outdoor dry-bulb temperature is higher than the zone temperature; otherwise, the sensible gain rate is set to 0.

When a [Fan:OnOff](#fanonoff) object is used, this reported value is weighted by the system run time fraction using the calculated infiltration sensible gain rate during the system on and off cycles for the reporting period:

Infiltration Sensible Gain Rate = (Infiltration Sensible Gain Rate during on cycle \* Run time fraction) + Infiltration Sensible Gain Rate during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Infiltration Sensible Heat Gain Energy [J]

The average convective sensible heat gain, in Joules, to the zone air corresponding to the [Zone](#zone) Infiltration Volume averaged over the reporting period. This value is calculated for each timestep when the outdoor dry-bulb temperature is higher than the zone temperature; otherwise, the sensible gain rate is set to 0.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated infiltration sensible gain during the system on and off cycles for the reporting period:

Infiltration Sensible Gain = (Infiltration Sensible Gain during on cycle \* Run time fraction) + Infiltration Sensible Gain during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Infiltration Sensible Heat Loss Rate [W]

The average convective sensible heat loss rate, in Watts, to the zone air corresponding to the [Zone](#zone) Infiltration Volume averaged over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated infiltration sensible loss rate during the system on and off cycles for the reporting period:

Infiltration Sensible Loss Rate = (Infiltration Sensible Loss Rate during on cycle \* Run time fraction) + Infiltration Sensible Loss Rate during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Infiltration Sensible Heat Loss Energy [J]

The average convective sensible heat loss, in Joules, to the zone air corresponding to the [Zone](#zone) Infiltration Volume averaged over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated infiltration sensible loss rate during the system on and off cycles for the reporting period:

Infiltration Sensible Loss = (Infiltration Sensible Loss during on cycle \* Run time fraction) + Infiltration Sensible Loss during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Infiltration Latent Heat Gain Rate [W]

The average convective latent heat gain rate, in Watts, to the zone air corresponding to the [Zone](#zone) Infiltration Volume averaged over the reporting period, when the outdoor humidity ratio is higher than the zone air humidity ratio.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated infiltration latent gain rate during the system on and off cycles for the reporting period:

Infiltration Latent Gain Rate = (Infiltration Latent Gain Rate during on cycle \* Run time fraction) + Infiltration Latent Gain Rate during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Infiltration Latent Heat Gain Energy [J]

The total convective latent heat gain, in Joules, to the zone air corresponding to the [Zone](#zone) Infiltration Volume summed over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated infiltration latent gain during the system on and off cycles for the reporting period:

Infiltration Latent Gain = (Infiltration Latent Gain during on cycle \* Run time fraction) + Infiltration Latent Gain during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Infiltration Latent Heat Loss Rate [W]

The average convective latent heat loss rate, in Watts, to the zone air corresponding to the [Zone](#zone) Infiltration Volume averaged over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated infiltration latent loss rate during the system on and off cycles for the reporting period:

Infiltration Latent Loss Rate = (Infiltration Latent Gain Loss Rate during on cycle \* Run time fraction) + Infiltration Latent Loss Rate during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Infiltration Latent Heat Loss Energy [J]

The total convective latent heat loss, in Joules, to the zone air corresponding to the [Zone](#zone) Infiltration Volume summed over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated infiltration latent loss during the system on and off cycles for the reporting period:

Infiltration Latent Loss = (Infiltration Latent Gain Loss during on cycle \* Run time fraction) + Infiltration Latent Loss during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Mixing Sensible Heat Gain Rate [W]

The average convective sensible heat gain rate, in Watts, to the zone air corresponding to the [Zone](#zone) Mixing Volume averaged over the reporting period. The mixing-volume is defined as incoming volume flow from other adjacent zones where the air temperature is higher than the temperature in this zone. For example, there are two zones ([Zone](#zone) 2 and [Zone](#zone) 3) adjacent to this zone ([Zone](#zone) 1). [Zone](#zone) 1 receives airflows from both [Zone](#zone) 2 and [Zone](#zone) 3. The air temperature is 21°C in [Zone](#zone) 1. The air temperatures are 20°C in [Zone](#zone) 2 and 22°C in [Zone](#zone) 3. The sensible gain rate only includes heat gain from [Zone](#zone) 3 with respect to [Zone](#zone) 1. The energy received from [Zone](#zone) 2 is considered as a sensible loss, instead of a gain, because the air temperature in [Zone](#zone) 2 is lower than in [Zone](#zone) 1.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated mixing sensible gain rate during the system on and off cycles for the reporting period:

Mixing Sensible Gain Rate = (Mixing Sensible Gain Rate during on cycle \* Run time fraction) + Mixing Sensible Gain Rate during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Mixing Sensible Heat Loss Rate [W]

The average convective sensible heat loss rate, in Watts, to the zone air corresponding to the [Zone](#zone) Mixing Volume averaged over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated mixing sensible loss rate during the on and off cycles for the reporting period:

Mixing Sensible Loss Rate = (Mixing Sensible Loss Rate during on cycle \* Run time fraction) + Mixing Sensible Loss Rate during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Mixing Sensible Heat Gain Energy [J]

The total convective sensible heat gain, in Joules, to the zone air corresponding to the [Zone](#zone) Mixing Volume summed over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated mixing sensible gain during the on and off cycles for the reporting period:

Mixing Sensible Gain = (Mixing Sensible Gain during on cycle \* Run time fraction) + Mixing Sensible Gain during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Mixing Sensible Heat Loss Energy [J]

The total convective sensible heat loss, in Joules, to the zone air corresponding to the [Zone](#zone) Mixing Volume summed over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated mixing sensible loss during the on and off cycles for the reporting period:

Mixing Sensible Loss = (Mixing Sensible Loss during on cycle \* Run time fraction) + Mixing Sensible Loss during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Mixing Latent Heat Gain Rate [W]

The average convective latent heat gain rate, in Watts, to the zone air corresponding to the [Zone](#zone) Mixing Volume averaged over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated mixing latent gain rate during the on and off cycles for the reporting period:

Mixing Latent Gain Rate = (Mixing Latent Gain Rate during on cycle \* Run time fraction) + Mixing Latent Gain Rate during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Mixing Latent Heat Gain Energy [J]

The total convective latent heat gain, in Joules, to the zone air corresponding to the [Zone](#zone) Mixing Volume summed over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated mixing latent gain during the on and off cycles for the reporting period:

Mixing Latent Gain = (Mixing Latent Gain during on cycle \* Run time fraction) + Mixing Latent Gain during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Mixing Latent Heat Loss Rate [W]

The average convective latent heat loss rate, in Watts, to the zone air corresponding to the [Zone](#zone) Mixing Volume averaged over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated mixing latent loss rate during the on and off cycles for the reporting period:

Mixing Latent Loss Rate = (Mixing Latent Loss Rate during on cycle \* Run time fraction) + Mixing Latent Loss Rate during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Mixing Latent Heat Loss Energy [J]

The total convective latent heat loss, in Joules, to the zone air corresponding to the [Zone](#zone) Mixing Volume summed over the reporting period.

When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system run time fraction using the calculated mixing latent loss during the on and off cycles for the reporting period:

Mixing Latent Loss = (Mixing Latent Loss during on cycle \* Run time fraction) + Mixing Latent Loss during off cycle \* (1.0 – Run time fraction)

#### AFN Zone Mixing CO2 Mass Flow Rate [kg/s]

This is a sum of mass flow rates from adjacent zones multiplied by the corresponding zone carbon dioxide concentration level to the receiving zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the mixing mass flow rate calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Mixing Generic Air Contaminant Mass Flow Rate [kg/s]

This is a sum of mass flow rates from adjacent zones multiplied by the corresponding zone generic contaminant concentration level to the receiving zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the mixing mass flow rate calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Duct Leaked Air Sensible Heat Gain Rate [W]

This is the average sensible heat gain rate, in Watts, to a specific zone due to supply air leaks from the forced air distribution system. This value is averaged over the reporting period. A sensible heat gain occurs when duct air is warmer than zone air. It should be pointed out that when multiple supply air leaks are present in a single zone, the output value is the summation of all the supply air leak gains in this zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Leaked Air Sensible Heat Gain Energy [J]

This is the total sensible heat gain, in Joules, to a specific zone due to supply air leaks summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Leaked Air Sensible Heat Loss Rate [W]

This is the average sensible heat loss rate, in Watts, to a specific zone due to supply air leaks from the forced air distribution system. This value is averaged over the reporting period. A sensible heat loss occurs when duct air is cooler than zone air. It should be pointed out that when multiple supply air leaks are present in this zone, the output value is the summation of all the supply air leak losses in this zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Leaked Air Sensible Heat Loss Energy [J]

This is the total sensible heat loss, in Joules, to a specific zone due to supply air leaks summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Leaked Air Latent Heat Gain Rate [W]

This is the average latent heat gain rate, in Watts, to a specific zone due to supply air leaks from the forced air distribution system for the reported time period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Leaked Air Latent Heat Gain Energy [J]

This is the total latent heat gain, in Joules, to a specific zone due to supply air leaks summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Leaked Air Latent Heat Loss Rate [W]

This is the average latent heat loss rate, in Watts, to a specific zone due to supply air leaks from the forced air distribution system for the reported time period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Leaked Air Latent Heat Loss Energy [J]

This is the total latent heat loss, in Joules, to a specific zone due to supply air leaks summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Conduction Sensible Heat Gain Rate [W]

This is the average sensible heat gain rate, in Watts, of duct conduction to a specific zone where the ducts are located. This value is averaged over the reporting period. A sensible heat gain occurs when duct air is warmer than the zone air. It should be pointed out that when ducts are located in different zones, the total duct conduction loss should be the summation of the duct conduction losses in these zones. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Conduction Sensible Heat Gain Energy [J]

This is the total sensible heat gain, in Joules, to a specific zone due to duct conduction summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Conduction Sensible Heat Loss Rate [W]

This is the average sensible heat loss rate, in Watts, of duct conduction to a specific zone where the ducts are located. This value is averaged over the reporting period. A sensible heat loss occurs when duct air is cooler than the zone air. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Conduction Sensible Heat Loss Energy [J]

This is the total sensible heat loss, in Joules, to a specific zone due to duct conduction summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Diffusion Latent Heat Gain Rate [W]

This is the average latent heat gain rate, in Watts, of vapor diffusion through the walls of the air distribution system to a specific zone where the ducts are located. This value is averaged over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Diffusion Latent Heat Gain Energy [J]

This is the total latent heat gain, in Joules, to a specific zone due to duct vapor diffusion summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Diffusion Latent Heat Loss Rate [W]

This is the average latent heat loss rate, in Watts, of duct vapor diffusion to a specific zone where the ducts are located. This value is averaged over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Duct Diffusion Latent Heat Loss Energy [J]

This is the total latent heat loss, in Joules, to a specific zone due to duct vapor diffusion summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Distribution Sensible Heat Gain Rate [W]

This is the average total sensible heat gain rate, in Watts, in a specific zone caused by the forced air distribution system. The total sensible gain rate is the sum of duct leakage sensible gain rate and duct conduction sensible gain rate. This value is averaged over the reporting period. The multizone airflow sensible gain rate is excluded in this output variable. The output of multizone airflow sensible gain is reported in the previously-described output variables AFN [Zone](#zone) Infiltration Sensible Heat Gain Rate and AFN [Zone](#zone) Mixing Sensible Heat Gain Rate. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Distribution Sensible Heat Gain Energy [J]

This is the total sensible heat gain, in Joules, in a specific zone caused by the forced air distribution system. The total sensible gain is the sum of duct leakage sensible gain and duct conduction sensible gain. This value is summed over the reporting period. The multizone airflow sensible gain is excluded in this output variable. The output of multizone airflow sensible gain is reported in the previously-described output variables AFN [Zone](#zone) Infiltration Sensible Heat Gain Energy and AFN [Zone](#zone) Mixing Sensible Heat Gain Energy. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Distribution Sensible Heat Loss Rate [W]

This is the average total sensible heat loss rate, in Watts, in a specific zone caused by the forced air distribution system. The total sensible loss rate is the sum of duct leakage sensible loss rate and duct conduction sensible loss rate. This value is averaged over the reporting period. The multizone airflow sensible loss rate is excluded in this output variable. The output of multizone airflow sensible loss rate is reported in the previously-described output variables AFN [Zone](#zone) Infiltration Sensible Heat Loss Rate and AFN [Zone](#zone) Mixing Sensible Heat Loss Rate. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Distribution Sensible Heat Loss Energy [J]

This is the total sensible heat loss, in Joules, in a specific zone caused by the forced air distribution system. The total sensible loss is the sum of duct leakage sensible loss and duct conduction sensible loss. This value is summed over the reporting period. The multizone airflow sensible loss is excluded in this output variable. The output of multizone airflow sensible loss is reported in the previously-described output variables AFN [Zone](#zone) Infiltration Sensible Heat Loss Energy and AFN [Zone](#zone) Mixing Sensible Heat Loss Energy. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Distribution Latent Heat Gain Rate [W]

This is the average total latent heat gain rate, in Watts, in a specific zone caused by the forced air distribution system. The total latent gain rate is the sum of duct leakage latent gain rate and duct conduction latent gain rate. This value is averaged over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Distribution Latent Heat Gain Energy [J]

This is the total latent heat gain, in Joules, in a specific zone caused by the forced air distribution system. The total latent gain is the sum of duct leakage latent gain and duct diffusion latent gain. This value is summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Distribution Latent Heat Loss Rate [W]

This is the average total latent heat loss rate, in Watts, in a specific zone caused by the forced air distribution system. The total latent loss rate is a sum of duct leakage latent loss rate and duct diffusion latent loss rate. This value is averaged over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Distribution Latent Heat Loss Energy [J]

This is the total latent heat loss, in Joules, in a specific zone caused by the forced air distribution system. The total latent loss is the sum of duct leakage latent loss and duct diffusion latent loss. This value is summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

> **NOTE:** The following output variables should not be confused with similar output variables for the infiltration, mixing, and cross mixing objects (Ref. Infiltration Output, Mixing Output, or Cross Mixing Output). The output variables described below refer to infiltration, mixing, and cross-mixing when an Airflow Network Simulation is performed. The following output variables are always used to describe infiltration, mixing, and cross mixing when the AirflowNetwork Control field in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object is set to "MultizoneWithoutDistribution" or "MultizoneWithDistribution". In this case the output variables for the infiltration, mixing, and cross mixing objects will always be 0.

> In contrast, the following output variables are only used to describe infiltration, mixing, and cross mixing when the AirflowNetwork Control field in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object is set to "MultizoneAirflowWithDistributionOnlyDuringFanOperation" and the fan is operating. When the fan is not operating, the output variables for the infiltration, mixing, and cross mixing objects are used.

> **In the case where the AirflowNetwork Control field in the [AirflowNetwork:SimulationControl](#airflownetworksimulationcontrol) object is set to "NoMultizoneOrDistribution", the following output variables are** not used and the output variables for the infiltration, mixing, and cross mixing objects are used instead.

#### AFN Zone Infiltration Sensible Heat Gain Energy [J]

The total convective sensible heat gain, in Joules, to the zone air corresponding to the [Zone](#zone) Infiltration Volume summed over the reporting period. This value is calculated for each timestep when the outdoor dry-bulb temperature is higher than the zone temperature, otherwise the sensible gain is set to 0. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Infiltration Sensible Heat Loss Energy [J]

The total convective sensible heat loss, in Joules to the zone air corresponding to the [Zone](#zone) Infiltration Volume summed over the reporting period. When a [Fan:OnOff](#fanonoff) object is used, the reported value is for the system on cycle.

#### AFN Zone Infiltration Volume [m3]

The volume of outdoor air flow into the zone from window/door openings and cracks in the exterior surfaces of the zone (i.e., the sum of ventilation and crack flows from the exterior into the zone). The zone air density is used to calculate the zone infiltration volume based on the mass flow rate. Note that AirflowNetwork [Zone](#zone) Infiltration Volume will be zero if all of the flows through the zone's exterior surfaces are out of the zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the infiltration volume calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Infiltration Mass [kg]

The mass of air corresponding to the AirflowNetwork [Zone](#zone) Infiltration Volume. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the infiltration mass calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Infiltration Air Change Rate [ach]

The number of air changes per hour produced by outdoor air flow into the zone from window/door openings and cracks in the exterior surfaces of the zone (i.e. the sum of ventilation and crack flows from the exterior into the zone). The target zone air density is used to calculate the zone infiltration air change rate based on the mass flow rate Note that, like [Zone](#zone) Infiltration Volume, [Zone](#zone) Infiltration Air Change Rate will be zero if all flows through the zone's exterior surfaces are out of the zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the infiltration air change rate calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Mixing Volume [m3]

This is a measure of interzone air flow for each thermal zone. It is the volume of air flow into the zone from adjacent zones through window/door openings and cracks in the interior heat transfer surfaces of the zone. The target zone air density is used to calculate the zone mixing volume based on the mass flow rate. This variable does not include flows that are from the zone to adjacent zones. Note that [Zone](#zone) Mixing Volume will be zero if all flows through the zone's interior surfaces are out of the zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the mixing volume calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Mixing Mass [kg]

The mass of air corresponding to the AFN [Zone](#zone) Mixing Volume. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the mixing mass calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Mixing Mass Flow Rate [kg/s]

This is a sum of mass flow rates from adjacent zones to the receiving zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the mixing mass flow rate calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Outdoor Air CO2 Mass Flow Rate [kg/s]

This is a sum of mass flow rates from outdoors multiplied by the outdoor carbon dioxide concentration level to the receiving zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the outdoor mass flow rate calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Outdoor Air Generic Air Contaminant Mass Flow Rate [kg/s]

This is a sum of mass flow rates from outdoors multiplied by the outdoor generic air contaminant concentration level to the receiving zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the outdoor mass flow rate calculated during the fan on and off periods for the simulation timestep

#### AFN Zone Outdoor Air Mass Flow Rate [kg/s]

This is a sum of mass flow rates from outdoors to the receiving zone. When a [Fan:OnOff](#fanonoff) object is used, the reported value is weighted by the system fan part-load ratio using the outdoor mass flow rate calculated during the fan on and off periods for the simulation timestep.

#### AFN Zone Total CO2 Mass Flow Rate [kg/s]

This is a sum of mass flow rates from adjacent zones or outdoors multiplied by the carbon dioxide concentration differences between the corresponding zone and the receiving zone.

#### AFN Zone Total Generic Air Contaminant Mass Flow Rate [kg/s]

This is a sum of mass flow rates from adjacent zones or outdoors multiplied by the generic contaminant concentration differences between the corresponding zone and the receiving zone.