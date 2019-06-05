# NFP: New Contaminant Transport Feature #

## Justification for New Feature ##
The current contaminant transport feature lacks a number of features (e.g.
generalized sources and sinks, filtration and path removal) and is not tightly
enough integrated with AirflowNetwork to facilitate advanced calculations (e.g.
non-trace contaminant transport). The lack of filtration is particularly
limiting. Many species/materials of interest may be filtered (or removed with a
similar process), including particulate matter, CO2, and water vapor. Similarly,
the lack of more source and sink options tends to limit simulation to very
basic situations. In many cases (particularly for those with limited data on 
the details of the building), these limitations are not important because the
level of detail is simply not needed. For more advanced simulations,
particularly of innovative HVAC systems, this limitation bars use of EnergyPlus
for most users.

## Overview ##
The new feature will enable more advanced simulations of contaminant transport
with a procedure  that is tightly integrated with the AirflowNetwork (AFN)
feature of EnergyPlus. This restriction is required to guarantee conservation
of mass and avoid complications due to EnergyPlus's flexibiltiy (Lorenzetti and
Wray, 2013). Initially, a simple filter object will be made available and
connected to the AFN feature through modifications to the appropriate linkage
objects. Source and sink objects will be connected to the AFN feature through
modification of the AFN node object. While the eventual goal will be to
include simulation of non-trace species, the initial development will focus on
trace simulations for simplicity.

## Approach ##
For simulation of transport of trace contaminants, conservation of mass for a
transported material is written for each zone as:

```
dM/dt = sum(FF_j) + G - R*M
```

where `M` is the mass of material in a zone, the `FF_j`s are the fluxes of mass
of material into and out of the zone, `G` is the generation of material in the
zone, and `R` is the removal rate of material (by non-flux processes) from the
zone. Filtration
processes are embedded in the `FF_j`s and the equation is typically divided by
mass of air in the zone to obtain an equation in terms of concentrations. The
resulting equation is

```
dC/dt = sum(F_j*(1-n_j)C_j) + G - R*C = RHS
```

where `n_j` is the filtration efficiency along path `j`, the `F_j`s are the mass
flow of air along path `j`, and  `C_j` is the concentration at the upstream end
of the flow along the path. The right hand side terms are lumped together as
`RHS` for purposes of the explanations below. For trace contaminants, this
equation is coupled to the airflow calculation in one direction: the airflow
calculation determines the `F_j`s, but the transport of material has no impact
upon the airflows. 

To advance the conservation equations through time, there are a number of
different options. For now, the first order finite difference discretization in
time will be used in three different ways. First, the implicit Euler method will
be implemented as follows, with subscript indicating time:

```
M_(t+h) = M_t + h*RHS_(t+h)  
```

where `h` is the timestep. This method is implicit in that the right hand side
is evaluated at time `t+h`, which requires a solution of simultaneous equations.Fortunately, the methods that are used to solve the airflow problem (e.g. the
skyline approach) may also be used here. The Crank-Nicolson method is an
alternative semi-implicit approach uses information from both `t` and `t+h`:

```
M_(t+h) = M_t + 0.5*h*(RHS_(t+h) + RHS_t)
```

This method also requires solution of simultaneous equations. Finally, an
explicit Euler method will be implemented as

```
M_(t+h) = M_t + h*RHS_(t)  
```

This approach is limited in step size, while the other two are stable for all
step sizes (though in some cases one will do better than the other). The main
advantage of the explicit Euler approach is that it does not require solution
of simulataneous equations.

## Engineering Reference ##
The above "Approach" section will be adapted to the Engineering reference formatand expanded to include additional references.

## I/O Reference ##
The I/O Reference will be modified to describe the modified and additional
inputs.

The zone object will be modified to include sources and sinks:

```
AirflowNetwork:MultiZone:Zone,
      \min-fields 8
      \extensible:1
      \memo This object is used to simultaneously control a thermal zone's window and door openings,
      \memo both exterior and interior.
  A1, \field Zone Name
      \required-field
      \reference AirFlowNetworkMultizoneZones
      \type object-list
      \object-list ZoneNames
      \note Enter the zone name where ventilation control is required.

  ...

  A6, \field Occupant Ventilation Control Name
      \type object-list
      \object-list AirflowNetworkOccupantVentilationControlNames
      \note Enter the name where Occupancy Ventilation Control is required.
  A7, \field Contaminant Source Sink 1
      \begin-extensible
      \type object-list
      \object-list AirflowNetworkContaminantSourceSinks
  A8, \field Contaminant Source Sink 2
      \begin-extensible
      \type object-list
      \object-list AirflowNetworkMaterialSourceSinks
  ...
```

The distribution linkage object will be modified to include a filter component.

```
AirflowNetwork:Distribution:Linkage,
      \min-fields 4
      \extensible:1
      \memo This object defines the connection between two nodes and a component.
 A1 , \field Name
      \required-field
      \type alpha
      \note Enter a unique name for this object.

 ...

 A5 , \field Thermal Zone Name
      \type object-list
      \object-list ZoneNames
      \note Only used if component = AirflowNetwork:Distribution:Component:Duct
      \note The zone name is where AirflowNetwork:Distribution:Component:Duct is exposed. Leave this field blank if the duct
      \note conduction loss is ignored.
 A6 , \field Filter Component 1
      \begin-extensible
      \type object-list
      \object-list AirflowNetworkFilters
```

The simulation control object will be modified to include a simulation option:

```
AirflowNetwork:SimulationControl,
      \min-fields 12
      \unique-object
      \memo This object defines the global parameters used in an Airflow Network simulation.
 A1 , \field Name
      \required-field
      \note Enter a unique name for this object.

 ...

 A8 , \field Solver
      \note Select the solver to use for the pressure network solution
      \type choice
      \key SkylineLU
      \key ConjugateGradient
      \default SkylineLU
 A9 ; \field Transport Simulation Type
      \note The type of transport simulation desired. 
      \note Selecting None will disable simulation entirely.
      \choice
      \key None
      \key ImplicitEuler
      \key ExplicitEuler
      \key CrankNicolson
      \default None
```

The additional IDD objects are:

* A material object that describes a contaminant material

```
AirflowNetwork:Material,
      \min-fields 2
 A1,  \field Name
      \required-field
      \type alpha
      \reference AirflowNetworkMaterials
      \note A unique name for the material.
 N1;  \field Ambient Concentration
      \required-field
      \type real
      \minimum 0
      \note The default/ambient concentration of the material.
```

* A very simple filter object

```
AirflowNetwork:SimpleFilter,
      \min-fields 3
 A1,  \field Name
      \required-field
      \type alpha
      \reference AirflowNetworkFilters
      \note A unique name for the filter.
 A2,  \field Material
      \required-field
      \type object-list
      \object-list AirflowNetworkMaterials
 N1;  \field Removal Efficiency
      \required-field
      \type real
      \minimum 0
      \maximum 1
      \note The removal efficiency of the material.
```

* A constant coefficient source/sink object

```
AirflowNetwork:SourceSink:ConstantCoefficient,
      \min-fields 3
 A1,  \field Name
      \required-field
      \type alpha
      \reference AirflowNetworkMaterialSourceSinks
      \note A unique name for the source/sink.
 A2,  \field Material
      \required-field
      \type object-list
      \object-list AirflowNetworkMaterials
 N1,  \field Generation Rate
      \required-field
      \type real
      \minimum 0
      \note The generation rate (in kg/s) of the material.
 N2,  \field Removal Rate
      \type real
      \minimum 0
      \note The generation rate (in kg/s) of the material.
```

## Output Details ##
TBD

## Example File and Transition Changes ##
An example file will be created that demonstrates the new feature.

## References ##
Lorenzetti, David M, and Craig P Wray. "Air-Handling System Modeling in EnergyPlus: Recommendations for Meeting Stakeholder Needs." 2014. LBNL-6863E.
