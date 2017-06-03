GroundDomain Objects Testing and Reporting Framework
================
                                     
**Matt Mitchell, OSU**
 - Original Draft: 5/23/16
                                     
## Justification for Feature Update
Several new features have been added to EnergyPlus recently to enhance ground simulation capabilities by coupling the zone directly to the finite difference ground domain simulation. These new objects bypass the ground and basement processors and directly simulate energy transfer to/from the ground in "real-time" as the zone is simulated. The expected result is that this will give a more accurate representation of the energy transfer associated with the ground contacting surfaces. The new features, however, are lacking in testing and validation. Some questions have arisen to regarding the accuracy of the simulation methods and results. This project is the first step in resolving these questions by providing a testing framework and a select number of test cases for validation.

## Conference Call Conclusions
N/A

## Other Conference Call Topics (not in scope of current proposal)
N/A

## Overview
Testing and validation of the GroundDomain objects is an important part of maintaining EnergyPlus's credibility among the building energy modeling community. This project proposes to develop the general framework for testing the GroundDomain simulation objects and comparing simulation results against validation data as outlined below.

### Testing Framework

1. __Generate fresh build of EnergyPlus:__ This is currently being accomplished by the CI machines.
2. __Perform simulation:__ Input and weather files for each simulation test case will be generated once and stored in the EnergyPlus git repository. These simulations will be a part of the regression and integration tests performed automatically by the CI machines.
3. __Process output results:__ The output results from simulation may require additional processing beyond what is reported in the output variables. The data file output from EnergyPlus will be processed as needed.
4. __Generate plots and report information:__ Plots from the simulations will be created to compare the simulation and the validation cases. 
5. __Generate report:__ A report detailing the comparison between the simulation case and test case will be automatically generated at each major release, or on-demand.

Most of the underlying work has been completed for the testing framework. For the EnergyPlus project, by enabling the "Build_Validation_Reports" option, validation reports for several different test cases are compiled, simulations run, results plotted, and documentation generated. The build option currently uses the previous markdown documentation system. Some work will be required to adapt this system to use the new LaTeX documentation.

### Proposed Validation Cases

The following test cases are proposed as the initial validation cases.

1. GC30b -- Steady state. 1:1 Aspect ratio. Small slab (12 x 12). h_in = h_out = 100
2. GC40b -- Harmonic. 1:1 Aspect ratio. Small slab (12 x 12). h_in = h_out = 100
3. GC45b -- Harmonic. 9:1 Aspect ratio. Rectangular slab (36 x 4). h_in = h_out = 100
4. GC50b -- Harmonic. 1:1 Aspect ratio. Large slab (80 x 80). h_in = h_out = 100


## Implementation
The CMake and Python source code will be modified and/or added so the EnergyPlus project can be configured to simulate, post-process, and generate reports for the GroundDomain objects.

LaTeX documentation will be generated for each test case and configured to build as part of the testing framework.

Some changes to the C++ code may be added, as per the recommendation of the reviewers, to apply the correct boundary conditions to the GroundDomain models. 


## Possible input scheme
In order to apply appropriate boundary conditions, the following objects may be needed in EnergyPlus. A number of tests specify constant convection coefficients for the ground surface. Ground domain edge boundary conditions may also be required to be set to adiabatic to simulate proximity to similar structures. This object may be added to control the boundary conditions for the GroundDomain objects. Adding this object is expected to be a small task.

```
Site:GroundDomain:BoundaryConditions,
	!- Name of object
	!- Name of GroundDomain
	!- Ground surface boundary condition (ConstConvCoefficient, WeatherFile)
	!- Ground surface convection coefficient
	!- Edge boundary condition (Adiabatic, FarfieldModel)
```

Some simulations require the cell(s) under the wall have a vertical adiabatic boundary condition. This creates a longer heat transfer path between the zone and the environment outside of the zone. This longer heat transfer path can create differences in results between the model and validation simulations (BESTEST). Adding this model is expected to be a larger task which will require changes to the mesh generation routines.

```
Site:GroundDomain:Wall,
	!- Name of object
	!- Name of GroundDomain
	!- Wall Thickness
	!- Underwall boundary condition (Adiabatic, AveZoneEnvrnTemp, Zone)
```

## Testing/Validation/Data Source(s):
Neymark, J & Judkoff, R. 2008. International Energy Agency Building Energy Simulation Test and Diagnostic Method (IEA BESTEST). In-Depth Diagnostic Cases for Ground Coupled Heat Transfer Related to Slab-On-Grade Construction. Technical Report NREL/TP-550-43388. NREL, Golden CO.

## IO Ref (draft):
Will write after we determine whether above objects are needed/wanted.

## EngRef (draft):
No changes expected.

## Example File and Transition changes:
No changes expected.









