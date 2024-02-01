Duct model road map
================

** Lixing Gu **

** Florida Solar Energy Center**

 - Original draft, 1/20/24

## Background ##

The AFN model is able to simulate duct conduction and leakage losses. The driving force is the pressure difference. NREL wants duct model can be performed wihtout using the AFN model. In other words, duct losses can be simulated without any pressure calculation and under AirLoopHVAC configurations. The following required may be met:

1. The proposed new objects may be compatible with the AFN objects. In other words, no duplication is required.
2. The new model should be able to simulate duct conduction and leakage losses.

Since the process is very complicated, a roadmap is needed to provide dicrection how to implement the new feature. It may involve changes of existing AFN objects.

## Roadmap ##

The roadmap presents my view to implement simplified duct model without using the AFN model. The proposed new feature should meet the above requirements and include a new object and possible modifications of existing objects. The present document addresses the possible inputs.

The following figure represents proposed duct configuration.

![duct model](DuctLoss.PNG)

### General inputs ###

This section presents inputs used in both losses of conduction and leakage.

#### A new object ####

A new object of Duct:Loss to cover additional duct inputs for the simplified duct model is provided below.

	Duct:Loss,
      \min-fields 3
        \memo the object is used to calculate duct conduction and leakage losses without using AFN model.
   	A1,  \field Name
        \required-field
   	A2,  \field AirLoopHAVC Name
        \required-field
        \type object-list
        \object-list AirPrimaryLoops
   	A3;  \field AirflowNetwork:Distribution:Linkage Name
        \required-field
        \type object-list
        \object-list AirflowNetworkDistributionLinkageNames

An optional field may help users to understand the object function:

   	A4;  \field Loss type
      \type choice
      \key Conduction
      \key Leakage
      \default Conduction

#### Existing object AirflowNetwork:Distribution:Node ####

The added choice is highlighted in red.

	AirflowNetwork:Distribution:Node,
      \min-fields 4
      \memo This object represents an air distribution node in the AirflowNetwork model.
 	A1 , \field Name
      \required-field
      \type alpha
      \reference AirflowNetworkNodeAndZoneNames
      \note Enter a unique name for this object.
 	A2 , \field Component Name or Node Name
      \type alpha
      \note Designates node names defined in another object. The node name may occur in air branches.
      \note Enter a node name to represent a node already defined in an air loop.
      \note Leave this field blank if the Node or Object Type field below is entered as
      \note AirLoopHVAC:ZoneMixer, AirLoopHVAC:ZoneSplitter, AirLoopHVAC:OutdoorAirSystem, or Other.
 	A3 , \field Component Object Type or Node Type
      \type choice
      \key AirLoopHVAC:ZoneMixer
      \key AirLoopHVAC:ZoneSplitter
      \key AirLoopHVAC:OutdoorAirSystem
      \key OAMixerOutdoorAirStreamNode
      \key OutdoorAir:NodeList
      \key OutdoorAir:Node
      \key Other
<span style="color:red">

      \key Zone
</span>

      \default Other
      \note Designates Node type for the Node or Component Name defined in the field above.
      \note AirLoopHVAC:ZoneMixer -- Represents a AirLoopHVAC:ZoneMixer object.
      \note AirLoopHVAC:ZoneSplitter -- Represents a AirLoopHVAC:ZoneSplitter object.
      \note AirLoopHVAC:OutdoorAirSystem -- Represents an AirLoopHVAC:OutdoorAirSystem object.
      \note OAMixerOutdoorAirStreamNode -- Represents an external node used in the OutdoorAir:Mixer
      \note OutdoorAir:NodeList -- Represents an external node when a heat exchanger is used before
      \note the OutdoorAir:Mixer
      \note OutdoorAir:Node -- Represents an external node when a heat exchanger is used before
      \note the OutdoorAir:Mixer
      \note Other -- none of the above, the Node name already defined in the previous field is part
      \note of an air loop.
      \note Zone -- Enter a zone name for duct simple model to calculate duct leakage loss.
 	N1 ; \field Node Height
      \type real
      \units m
      \default 0.0
      \note Enter the reference height used to calculate the relative pressure.


Note for AirflowNetwork:Distribution:Node: 

1. Component Name or Node Name

The field of Component Name or Node Name is either Air Node name or Zone name. If a Zone name or an outdoor air node is entered, this node is used as a return leakage source or a supply leak target.

2. N1 field are ignored

Since the proposed feature is used for duct energy losses from conduction and leakage, N1 field is not used in energy loss calculation. The outdoor air node is allowed as either leakage source or target.

3. A new choice of A3 field is added as zone name. When AFN is fully implemented, the zone name is defined in AirflowNetwork:MultiZone:Zone. For the simplified duct model without using AFN, the added new choice can be used to define a zone for duct leakage calculation. Therefore, there is no need to use AirflowNetwork:MultiZone:Zone.  

#### Existing object AirflowNetwork:Distribution:Linkage ####

	AirflowNetwork:Distribution:Linkage,
      \min-fields 4
      \memo This object defines the connection between two nodes and a component.
 	A1 , \field Name
      \required-field
      \type alpha
      \note Enter a unique name for this object.
      \reference AirflowNetworkDistributionLinkageNames
 	A2 , \field Node 1 Name
      \required-field
      \type object-list
      \object-list AirflowNetworkNodeAndZoneNames
      \note Enter the name of zone or AirflowNetwork Node or Air Node.
 	A3 , \field Node 2 Name
      \required-field
      \type object-list
      \object-list AirflowNetworkNodeAndZoneNames
      \object-list AirNodeAndZoneNames
      \note Enter the name of zone or AirflowNetwork Node or Air Node.
 	A4 , \field Component Name
      \required-field
      \type object-list
      \object-list AirflowNetworkComponentNames
      \object-list FansCVandOnOffandVAV
      \object-list AFNCoilNames
      \object-list AFNHeatExchangerNames
      \object-list AFNTerminalUnitNames
      \note Enter the name of an AirflowNetwork component. A component is one of the
      \note following AirflowNetwork:Distribution:Component objects: Leak, LeakageRatio,
      \note Duct, ConstantVolumeFan, Coil, TerminalUnit, ConstantPressureDrop, or HeatExchanger.
 	A5 ; \field Thermal Zone Name
      \type object-list
      \object-list ZoneNames
      \note Only used if component = AirflowNetwork:Distribution:Component:Duct
      \note The zone name is where AirflowNetwork:Distribution:Component:Duct is exposed. Leave this field blank if the duct
      \note conduction loss is ignored.

Note for AirflowNetwork:Distribution:Linkage: 

1. Conduction

If duct conduction loss is simulated, the selected Component Name should be AirflowNetwork:Distribution:Component:Duct. Node 1 and Node 2 names should be Air Node names, provided in the list of AirflowNetwork:Distribution:Node.

2. Leakage

If duct leakage loss is simulated, the selected Component Name should be AirflowNetwork:Distribution:Component:LeakageRatio. The input of Effective Leakage Ratio field should be a fraction of mass flow rate of the AirLoopHVAC. The other field inputs are not used. One of nodes should be either zone name or outdoor air node name.

If a supply leak is defined, Node 1 name should be an air node, Node 2 name should be either a zone name or an outdoor air node name.

If a return leak is defined, Node 1 name should be either a zone name or an outdoor air node name, Node 2 name should be an air node.

3. Restriction

Although there is no restriction of the number of ducts and locations, it is proposed for Phase 1 to have limits as follows. 

3.1 There is a single duct used for SupplyTrunk and ReturnTrunk.

The connection of SupplyTrunk is between the inlet node (AirloopHVAC Demand Side Inlet Node) of AirLoopHVAC and the AirLoopHVAC:ZoneSplitter inlet node. If AirLoopHVAC:ZoneMixer is available, The connection of ReturnTrunk is between AirLoopHVAC:ZoneMixer outlet node and the outlet node (AirloopHVAC Demand Side Outlet Node) of AirLoopHVAC.

3.2 Each branch has a single duct.

The connection of SupplyBranch is between AirLoopHVAC:ZoneSplitter outlet node and one of the Air terminal inlet node. If AirLoopHVAC:ZoneMixer is available, The connection of ReturnBranch is between the zone outlet node and the AirLoopHVAC:ZoneMixer inlet node.

### Conduction loss ###

The AirflowNetwork:Distribution:Component:Duct object is used to calculate duct conduction loss.

#### Existing object for duct conduction loss as AirflowNetwork:Distribution:Component:Duct ####

	AirflowNetwork:Distribution:Component:Duct,
      \min-fields 8
      \memo This object defines the relationship between pressure and air flow through the duct.
 	A1 , \field Name
      \required-field
      \type alpha
      \reference AirflowNetworkComponentNames
      \note Enter a unique name for this object.
 	N1 , \field Duct Length
      \required-field
      \type real
      \units m
      \minimum> 0.0
      \note Enter the length of the duct.
 	N2 , \field Hydraulic Diameter
      \required-field
      \type real
      \units m
      \minimum> 0.0
      \note Enter the hydraulic diameter of the duct.
      \note Hydraulic diameter is defined as 4 multiplied by cross section area divided by perimeter
 	N3 , \field Cross Section Area
      \required-field
      \type real
      \units m2
      \minimum> 0.0
      \note Enter the cross section area of the duct.
 	N4 , \field Surface Roughness
      \type real
      \units m
      \default 0.0009
      \minimum> 0.0
      \note Enter the inside surface roughness of the duct.
 	N5 , \field Coefficient for Local Dynamic Loss Due to Fitting
      \type real
      \units dimensionless
      \default 0.0
      \minimum 0.0
      \note Enter the coefficient used to calculate dynamic losses of fittings (e.g. elbows).
<span style="color:red">
 
   	A2,  \field Construction Name
        \required-field
        \type object-list
        \object-list ConstructionNames
</span>

 	N6 , \field Overall Moisture Transmittance Coefficient from Air to Air
      \type real
      \units kg/m2
      \minimum> 0.0
      \default 0.001
      \note Enter the overall moisture transmittance coefficient
      \note including moisture film coefficients at both surfaces.
 	N7 , \field Outside Convection Coefficient
      \note optional. convection coefficient calculated automatically, unless specified
      \type real
      \units W/m2-K
      \minimum> 0.0
 	N8 ; \field Inside Convection Coefficient
      \note optional. convection coefficient calculated automatically, unless specified
      \type real
      \units W/m2-K
      \minimum> 0.0

Note:

1. Replace N6 by A2

Heat Transmittance Coefficient (U-Factor) for Duct Wall Construction field is replaced by construction name. Since inside and outside film coefficients are provided, there is no reason to requires U factor, if the construction object can be use.

2. Keep N6 Overall Moisture Transmittance Coefficient from Air to Air

Although I can require to add one more field for moisture diffusivity in the Material object, this property is driven by humidity ratio. As we know, heat transfer is driven by the temperature difference, while the mositure transfer is driven by partial vapor pressure difference. The humidity ratio, as an independent variable, may not be proper.

Let's keep the field for the time being. We may need to think to use the partial vapor pressure as driving force to simulate moisture performance across walls. 

3. The Component:Duct is the only component listed in the Component Name defined in the Linkage object for conduction loss calculation.

4. The Component:LeakageRatio is the only component listed in the Component Name defined in the Linkage object for conduction leakage calculation.


An example of objects used to calculate duct condiuction loss in an IDF is:

\begin{lstlisting}

	Duct:Loss,
      Main duct,    !- Name
      Main AirLoopHVAC,    !- AirLoopHAVC Name
   	  Mail Duct Linkage;  \field AirflowNetwork:Distribution:Linkage Name

  	AirflowNetwork:Distribution:Node,
      EquipmentOutletNode,      !- Name
      Equipment outlet node,  !- Component Name or Node Name
      Other,                   !- Component Object Type or Node Type
      3.0;                     !- Node Height {m}

  	AirflowNetwork:Distribution:Node,
      SplitterInletNode,      !- Name
      ZoneSplitter Inlet Node,  !- Component Name or Node Name
      Other,                   !- Component Object Type or Node Type
      3.0;                     !- Node Height {m}

  	AirflowNetwork:Distribution:Component:Duct,
      AirLoopSupply,           !- Name
      0.1,                     !- Duct Length {m}
      1.0,                     !- Hydraulic Diameter {m}
      0.7854,                  !- Cross Section Area {m2}
      ,                  !- Surface Roughness {m}
      ,                    !- Coefficient for Local Dynamic Loss Due to Fitting {dimensionless}
      Duct Wall,                !- Construction Name
      0.0001,                  !- Overall Moisture Transmittance Coefficient from Air to Air {kg/m2}
      0.006500,                !- Outside Convection Coefficient {W/m2-K}
      0.032500;                !- Inside Convection Coefficient {W/m2-K}

  	AirflowNetwork:Distribution:Linkage,
      Mail Duct Linkage,             !- Name
      EquipmentOutletNode,      !- Node 1 Name
      SplitterInletNode,     !- Node 2 Name
      AirLoopSupply,              !- Component Name
      Attic Zone;              !- Thermal Zone Name

\end{lstlisting}

### Leakage losses without mass flow changes ###

Assumptions:

1. The leakage loss calculation is based on energy transfer. In other words, no mass flows are involved in both AirLoopHVAC and zones. For example, when a supply leak is applied, the supply fan flow rate remains the same, the changes may be outlet equivalent temperatures and humidity ratios

2. Any energy losses for supply leak occur in the source duct and target zone or outdoor air node only. No more extension to other ducts and zones.

3. Any energy losses for return leak occur in the source zone or outdoor air node and and target duct only. No more extension to other ducts and zones.

4. Any air movement between zones caused by duct leakage can be accomplished by make up air.

#### Existing objects ####

AirflowNetwork:Distribution:Node and AirflowNetwork:Distribution:Linkage are the same as above

No change of AirflowNetwork:Distribution:Component:LeakageRatio.

Note for AirflowNetwork:Distribution:Component:LeakageRatio

Field Effective Leakage Ratio is used as leakage, a fraction of AirLoopHVAC flow. The inputs of the rest of fields are not used.

#### Makeup air ####

An important factor for duct leakage is to introduce make up flow due to supply and return leaks. Since we don't use pressure to calculate make up airflow impact, we will allow users to specify make up flows and direction using existing AFN object, so that make up airflows can flow from outdoor to a zone, and from a zone to another zone. The requirements are as follows:

1. The Node 1 name and Node 2 name in the AirflowNetwork:Distribution:Linkage object have to be either zone names for both fields or a zone name and an outdoor node name. The Node 1 name represents flow starting point, and the Node 2 name represents flow ending points. The flow direction for a linkage with a zone name and an outdoor node name should be from outdoor to a zone, equivalent to air infiltration. When both zone names are specified, the equivalent object should Zobe Mixing. 

2. Exfiltration is not used in energy calculation. In other words, an outdoor air node can not be specified as Node 2 name.

An example of objects used to calculate duct leak loss in an IDF is:

\begin{lstlisting}

	Duct:Loss,
      Main duct leak,    !- Name
      Main AirLoopHVAC,    !- AirLoopHAVC Name
   	  Mail Duct Leak;  \field AirflowNetwork:Distribution:Linkage Name

  	AirflowNetwork:Distribution:Node,
      SplitterInletNode,      !- Name
      ZoneSplitter Inlet Node,  !- Component Name or Node Name
      Other,                   !- Component Object Type or Node Type
      3.0;                     !- Node Height {m}

  	AirflowNetwork:Distribution:Node,
      Attic Zone,      !- Name
      Attic zone Name,  !- Component Name or Node Name
      Other,                   !- Component Object Type or Node Type
      3.0;                     !- Node Height {m}

  	AirflowNetwork:Distribution:Component:LeakageRatio,
      ZoneSupplyELR,          !- Name
      0.05,                    !- Effective Leakage Ratio {dimensionless}
      1.9,                     !- Maximum Flow Rate {m3/s}
      59.0,                    !- Reference Pressure Difference {Pa}
      0.65;                    !- Air Mass Flow Exponent {dimensionless}

  	AirflowNetwork:Distribution:Linkage,
      Main Duct Leak,             !- Name
      SplitterInletNode,      !- Node 1 Name
      Attic Zone,     !- Node 2 Name
      ZoneSupplyELR,              !- Component Name
      ;              !- Thermal Zone Name

! Make up air

  	AirflowNetwork:Distribution:Node,
      OutdoorAirNode,      !- Name
      Outdoor Air Node,  !- Component Name or Node Name
      Other,                   !- Component Object Type or Node Type
      3.0;                     !- Node Height {m}

  	AirflowNetwork:Distribution:Node,
      Living Zone,      !- Name
      Living zone Name,  !- Component Name or Node Name
      Other,                   !- Component Object Type or Node Type
      3.0;                     !- Node Height {m}

  	AirflowNetwork:Distribution:Component:LeakageRatio,
      MakeupAirELR,          !- Name
      0.05,                    !- Effective Leakage Ratio {dimensionless}
      1.9,                     !- Maximum Flow Rate {m3/s}
      59.0,                    !- Reference Pressure Difference {Pa}
      0.65;                    !- Air Mass Flow Exponent {dimensionless}

  	AirflowNetwork:Distribution:Linkage,
      Duct Leak Makeup,             !- Name
      OutdoorAirNode,      !- Node 1 Name
      Living Zone,     !- Node 2 Name
      MakeupAirELR,              !- Component Name
      ;              !- Thermal Zone Name

\end{lstlisting}

### Trigger ###

The duct model can be trigged by two choices. The first choice is to use a new object as Duct:Loss. The second choice is add more choices in the AirflowNetwork Control field of the AirflowNetwork:SimulationControl object. I prefer the first choice. The main reason is that the simplified duct mode does not use the AFN model. Instead, the model only uses the existing AFN objects. 

#### Can be triggered by new chocies of AirflowNetwork Control ####

There are many choices defined in the AirflowNetwork Control field in the AirflowNetwork:SimulationControl object.

Three more choices are:

ConductionLossWithoutAFN
LeakageLossWithoutAFN
ConductionAndLeakageLossWithoutAFN

	AirflowNetwork:SimulationControl,
      \min-fields 16
      \unique-object
      \memo This object defines the global parameters used in an Airflow Network simulation.
 	A1 , \field Name
      \required-field
      \note Enter a unique name for this object.
 	A2 , \field AirflowNetwork Control
      \type choice
      \key MultizoneWithDistribution
      \key MultizoneWithoutDistribution
      \key MultizoneWithDistributionOnlyDuringFanOperation
      \key NoMultizoneOrDistribution
	ConductionLossWithoutAFN
	LeakageLossWithoutAFN
	ConductionAndLeakageLossWithoutAFN
      \default NoMultizoneOrDistribution
      \note NoMultizoneOrDistribution: Only perform Simple calculations (objects ZoneInfiltration:*,
      \note ZoneVentilation:*, ZoneMixing, ZoneCrossMixing, ZoneRefrigerationDoorMixing,
      \note ZoneAirBalance:OutdoorAir, ZoneEarthtube, ZoneThermalChimney, and ZoneCoolTower:Shower);
      \note MultizoneWithoutDistribution: Use AirflowNetwork objects to simulate multizone
      \note Airflows driven by wind during simulation time,
      \note and objects of ZoneInfiltration:*, ZoneVentilation:*, ZoneMixing, ZoneCrossMixing
      \note ZoneRefrigerationDoorMixing, ZoneAirBalance:OutdoorAir, ZoneEarthtube,
      \note ZoneThermalChimney, and ZoneCoolTower:Shower are ignored;
      \note MultizoneWithDistributionOnlyDuringFanOperation: Perform distribution system
      \note calculations during system fan on time
      \note and Simple calculations during system Fan off time;
      \note MultizoneWithDistribution: Perform distribution system calculations during system
      \note fan on time and multizone Airflow driven by wind during system fan off time.
      \note ConductionLossWithoutAFN: Perform conduction loss simulation without AFN
      \note LeakageLossWithoutAFN: Perform leakage loss simulation without AFN
      \note ConductionAndLeakageLossWithoutAFN: Perform conduction and leakage loss simulation without AFN
 	A3 , \field Wind Pressure Coefficient Type
      \type choice
      \key Input
      \key SurfaceAverageCalculation
      \default SurfaceAverageCalculation
      \note Input: User must enter AirflowNetwork:MultiZone:WindPressureCoefficientArray,
      \note AirflowNetwork:MultiZone:ExternalNode, and
      \note AirflowNetwork:MultiZone:WindPressureCoefficientValues objects.
      \note SurfaceAverageCalculation: used only for rectangular buildings.
      \note If SurfaceAverageCalculation is selected,
      \note AirflowNetwork:MultiZone:WindPressureCoefficientArray, AirflowNetwork:MultiZone:ExternalNode,
      \note and AirflowNetwork:MultiZone:WindPressureCoefficientValues objects are not used.
 	A4 , \field Height Selection for Local Wind Pressure Calculation
      \type choice
      \key ExternalNode
      \key OpeningHeight
      \default OpeningHeight
      \note If ExternalNode is selected, the height given in the
      \note AirflowNetwork:MultiZone:ExternalNode object will be used.
      \note If OpeningHeight is selected, the surface opening height (centroid) will be used to
      \note calculate local wind pressure
      \note This field is ignored when the choice of the Wind Pressure Coefficient Type field is
      \note SurfaceAverageCalculation.
 	A5 , \field Building Type
      \note Used only if Wind Pressure Coefficient Type = SurfaceAverageCalculation,
      \note otherwise this field may be left blank.
      \type choice
      \key LowRise
      \key HighRise
      \default LowRise
 	N1 , \field Maximum Number of Iterations
      \type integer
      \units dimensionless
      \default 500
      \minimum> 10
      \maximum 30000
      \note Determines the maximum number of iterations used to converge on a solution. If this limit
      \note is exceeded, the program terminates.
 	A6 , \field Initialization Type
      \type choice
      \key LinearInitializationMethod
      \key ZeroNodePressures
      \default ZeroNodePressures
 	N2 , \field Relative Airflow Convergence Tolerance
      \type real
      \units dimensionless
      \default 1.E-4
      \minimum> 0
      \note This tolerance is defined as the absolute value of the sum of the mass Flow Rates
      \note divided by the sum of the absolute value of the mass Flow Rates. The mass Flow Rates
      \note described here refer to the mass Flow Rates at all Nodes in the AirflowNetwork model.
      \note The solution converges when both this tolerance and the tolerance in the next field
      \note (Absolute Airflow Convergence Tolerance) are satisfied.
 	N3 , \field Absolute Airflow Convergence Tolerance
      \type real
      \units kg/s
      \default 1.E-6
      \minimum> 0
      \note This tolerance is defined as the absolute value of the sum of the mass flow rates. The mass
      \note flow rates described here refer to the mass flow rates at all nodes in the AirflowNetwork
      \note model. The solution converges when both this tolerance and the tolerance in the previous
      \note field (Relative Airflow Convergence Tolerance) are satisfied.
 	N4 , \field Convergence Acceleration Limit
      \type real
      \units dimensionless
      \note Used only for AirflowNetwork:SimulationControl
      \minimum -1
      \maximum 1
      \default -0.5
 	N5 , \field Azimuth Angle of Long Axis of Building
      \type real
      \units deg
      \minimum 0.0
      \maximum 180.0
      \default 0.0
      \note Degrees clockwise from true North.
      \note Used only if Wind Pressure Coefficient Type = SurfaceAverageCalculation.
 	N6 , \field Ratio of Building Width Along Short Axis to Width Along Long Axis
      \type real
      \minimum> 0.0
      \maximum 1.0
      \default 1.0
      \note Used only if Wind Pressure Coefficient Type = SurfaceAverageCalculation.
 	A7 , \field Height Dependence of External Node Temperature
      \note If Yes, external node temperature is height dependent.
      \note If No, external node temperature is based on zero height.
      \type choice
      \key Yes
      \key No
      \default No
 	A8 , \field Solver
      \note Select the solver to use for the pressure network solution
      \type choice
      \key SkylineLU
      \key ConjugateGradient
      \default SkylineLU
 	A9 , \field Allow Unsupported Zone Equipment
      \note Set this input to Yes to have zone equipment that are currently unsupported in the AirflowNetwork model
      \note allowed in the simulation if present. Setting this field to Yes, allows the following equipment
      \note to be modeled along an AirflowNetwork model: ZoneHVAC:Dehumidifier, ZoneHVAC:EnergyRecoveryVentilator,
      \note WaterHeater:HeatPump:*.
      \type choice
      \key Yes
      \key No
      \default No
 	A10; \field Do Distribution Duct Sizing Calculation
      \note Controls duct sizing. See AirflowNetwork:Distribution:DuctSizing for sizing options.
      \type choice
      \key Yes
      \key No
      \default No

#### Self start with new obejcts only ####

<span style="color:red">
I prefer to use this approach:
</span>

	Duct:Loss objects are used without any modifications of AirflowNetwork:SimulationControl. The objects can be accomplished all 3 choices.

1. ConductionLossWithoutAFN

When all nodes defined in the AirflowNetwork:Distribution:Linkage are air nodes, and all the components defined in the Component Name fields are AirflowNetwork:Distribution:Component:Duct, the simplified duct conduction loss calculation will be performed.

  
2. LeakageLossWithoutAFN

When one of two nodes in all the AirflowNetwork:Distribution:Linkage is either a zone name or outdoor air node, and all the component defined in the Component Name fields are AirflowNetwork:Distribution:Component:LeakageRatio, the simplified duct leakage loss calculation will be performed.

3. ConductionAndLeakageLossWithoutAFN

When all the components defined in the Component Name fields are AirflowNetwork:Distribution:Component:Duct and AirflowNetwork:Distribution:Component:LeakageRatio, the simplified duct conduction and leakage loss calculation will be performed.

### Leakage losses with mass flow changes ###

The inputs of objects in this section should be the same as the section of Leakage losses without mass flow changes. The differences between two sections are internal code implementation. The changes will be addressed in Design document.