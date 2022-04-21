AirflowNetwork Duct Autosizing
================

**Lixing Gu**

**Florida Solar Energy Center**

 - First draft, 04/13/22
 - 
 

## Justification for New Feature ##

Duct sizing is one of the remaining barriers to greater usage of the AFN distribution feature. For simpler and smaller models, inclusion of the pressure network may require more inputs and information than the base energy model itself. Removing duct size inputs will be especially helpful in schema-driven modeling efforts (e.g. the approach used in several audit-related interfaces) that may need a legitimate distribution system but lack detailed inputs to construct one. Adding this feature would greatly simplify the required inputs to create a fully featured residential AFN model with distribution.

## E-mail and  Conference Call Conclusions ##

NA

## Overview ##

Due to complexity of this new feature, it will implement in different phases. Phase 1 will be a preliminary phase of airflow network duct autosizing. In this phase, the task will cover autosize duct diameter for a given duct length in simulations with a single air loop and a single conditioned zone. This will answer questions and reveal issues that will inform subsequent phases.

## Approach ##

In order to implement the task, several assumptions are provided below.

###Assumptions###

Here are assumptions used to implement Phase 1. Figure 1 shows schematic air distribution system with trunk and branch ducts.

![Figure 1](AirDistribution.PNG)

** Figure 1. Air dictribution system with trunk and branch ducts **

####Supply duct####

The supply ducts will be divided into two sections based on current duct configuration: Truck and Branch. The truck section includes ducts between Return Air Path Outlet Node in Demand Side Inlet Node in AirLoopHVAC, and Inlet Node in AirLoopHVAC:ZoneSplitter. The branch section includes ducts between Outlet Nodes in AirLoopHVAC:ZoneSplitter, and Inlet Node in ZoneTernminal Units defined in ZoneHVAC:AirDistributionUnit. 
  
####Return duct####

The return ducts will be divided into two sections based on current duct configurqation: Truck and Branch. The truck section includes ducts between Demand Side Outlet Node in AirLoopHVAC, and Outlet Node in AirLoopHVAC:ZoneMixer. The branch section includes ducts between Inlet Nodes in AirLoopHVAC:ZoneMixer, and Zone Return Air Node defined in ZoneHVAC:EquipmentConnections.

####Mass or volumetric flow rates####

The mass flow rates are available from either system sizing or user inputs. Each truck has the same mass flow rate as max fan flow rate. Each branch has the same flow rate as max flow rate from the corresponding ZoneTernminal Unit.

All ducts in the same truck or branch have the same size as diameter, due to the same mass flow rate and velocity. 

####Relationship between total pressure and static pressure####

The total pressure drop between inlet and outlet for a duct is presented below:

&Delta;P = (P<sub>1</sub> + &rho;V<sub>1</sub><sup>2</sup> / 2) - (P<sub>2</sub> + &rho;V<sub>2</sub><sup>2</sup> / 2) + &rho;g (z<sub>1</sub> - z<sub>2</sub>)

where

&Delta;P = Total pressure drop between points 1 and 2

P<sub>1</sub>, P<sub>2</sub> = Entry and exit static pressure

V<sub>1</sub>, V<sub>2</sub> = Entry and exit velocities

&rho; = Air density

g = Acceleration of gravity

z<sub>1</sub>, z<sub>2</sub> = Entry and exit elevations

When entry and exit velocities and elevations are the same, the total pressure difference is equal to the static pressure difference. The assumption will be used for duct sizing. 

###Duct losses ###

The total pressure loss in either a truck or a branch can be calculated using Darcy-Weisbach Equation (Eq. 34 in Chpater 21, 2017 ASHRAE HOF)

&Delta;P = \( (1000fL) / D<sub>h</sub> + &Sigma;C ) * (&rho;V<sup>2</sup> / 2) \)

where

&Delta;P = Total pressure loss in a truck or branch

f = Friction factor

L = Total length in a truck or branch

D<sub>h</sub> = Hydraulic diameter

V = Velocity

&rho; = Air density

C = Local loss coefficient for all ducts

where the friction factor can be represented by Colebrrook's equation:

1/ (f)<sup>1/2</sup> = -2 log\( &epsilon;/ (3.7 * D<sub>h</sub>) + 2.51 /(Re (f)<sup>1/2</sup>)\)

Re = 66.4 *D<sub>h</sub> *V

For a round duct:

V = Q/A = Q / (D<sub>h</sub><sup>2</sup> * &pi; / 4)

When &Delta;P given, and the relationship between D<sub>h</sub> and V is also given, there is only a single unknow D<sub>h</sub>. The value can be obtained through iteration. 

When the max velocity and mass flow rate are given, the hydraulic diameter can be calculated, so that the total pressure drop can be calculated.


###No hard sizes ###

When sizing is requested, no hard input values of hydrolic diameter will be allowed for all ducts. In other words, sizing will be applied to all ducts.

###Discussions###

####New fields ####

The proposed new fields can be added at the end of the AirflowNetwork:SimulationControl object as optional. Or a new sizing object may be created: Sizing:AFN:Ducts with the same fields.

## Testing/Validation/Data Sources ##

insert text

## Input Output Reference Documentation ##

insert text

## Input Description ##

Six new optional fields are proposed in the AirflowNetwork:SimulationControl object. The new fields cover duct sizing type, and total pressure loses for supply and retun ducts. Both supply and return ducts are divided into two sections: trunk and branch. All branches have the same pressure drop to represent equal friction method. The new fields in the object are highlighted in red. 

There are several choices for trunk sizing: maximum velocity, pressure loss, or combination. The combination uses the pressure loss first. Then velocity will be checked. If the velocity is less than the maximum velocity, the pressure loss method results will be used. Otherwise, the maximum velocity will be used to calculate duct diameter.

The pressure loss method will be applied to all branches, so that all ducts have the same pressure losses. The approach is equivalent to equal friction method.

	! Basic parameters
	AirflowNetwork:SimulationControl,
      \min-fields 12
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
      \note allowed in the simulation if present. Setting this field to Yes, allows the following equipments
      \note to be modeled along an AirflowNetwork model: ZoneHVAC:Dehumidifier, ZoneHVAC:EnergyRecoveryVentilator,
      \note WaterHeater:HeatPump:*.
      \type choice
      \key Yes
      \key No
      \default No
<span style="color:red">

 	A10, \field Duct Sizing Type
      \type choice
      \key None
      \key MaximumVelocity
      \key PressureLoss
      \key PressureLossWithMaximumVelocity
      \default None
 	N11 , \field Maximum Airflow Velocity
      \type real
      \units m/s
      \minimum >0.0
      \maximum 25.0
      \default 5.0
      \note Used only if Duct Sizing Type = MaximumVelocity or PressureLossWithMaximumVelocity.
      \note When MaximumVelocity is entered, duct diameter is calculated at D = flow rate / 
      \note cross section area.
      \note When PressureLossWithMaximumVelocity is entered, duct diameter is calculated based on 
      \note PressureLoss. The value is used to check to ensure the final velocity is less than
      \note the maximum value. If greater, final value will be obtained from MaximumVelocity.
      \note This field is apply for truck size, while branch size is based on total pressure drop.
 	N12 , \field Total Pressure Loss Across Supply Truck
      \type real
      \units Pa
      \minimum >0.0
      \note Used only if Duct Sizing Type = PressureLoss or PressureLossWithMaximumVelocity.
      \note When PressureLoss is entered, duct diameter is calculated using Colebrook's equation  
      \note When PressureLossWithMaximumVelocity is entered, duct diameter is calculated based on 
      \note PressureLoss. The value is used to check to ensure the final velocity is less than
      \note the maximum value. If greater, final value will be obtained from MaximumVelocity.
      \note This field is apply for truck size, while branch size is based on total pressure drop.
 	N13 , \field Total Pressure Loss Across Supply Branch
      \type real
      \units Pa
      \minimum >0.0
      \note Duct diameter is calculated using Colebrrook's equation  
 	N14 , \field Total Pressure Loss Across Return Trunk
      \type real
      \units Pa
      \minimum >0.0
      \note Duct diameter is calculated using Colebrrook's equation  
 	N15 , \field Total Pressure Loss Across Return Branch
      \type real
      \units Pa
      \minimum >0.0
      \note Duct diameter is calculated using Colebrrook's equation  
</span>

## Outputs Description ##

Duct sizes will be added in the output of eio.

## Engineering Reference ##

insert text

## Example File and Transition Changes ##

An existing eample file will be modified by adding duct sizing capability. If not found, a new example file will be created and uploded in GitHub

## References ##

insert text



