Improving Urban Building Energy Modeling
================
**Tianzhen Hong, Xuan Luo, Lawrence Berkeley National Laboratory**
 
- May 30, 2017

# Justification for New Feature 

Building energy efficiency is getting greater attention in urban planning and city GHG emissions reduction. Energy performance in buildings affects the development of energy standards and policies that address energy-related issues at urban scales. However, urban building energy modeling has always been a challenging task due to the huge number of buildings at the urban scale, coupling between buildings, and the trade-off between accuracy and speed in the simulation process. As more urban data related to the basic building information, mutual shading, microclimate, and occupant activities are collected and synthesized in the GIS platforms, allowing the community to city scale building energy modeling and simulation, the simulation engine itself also requires enhancements in terms of accuracy and performance to suit large-scale simulations. We propose enhancements in several areas to improve the use of EnergyPlus for urban scale building energy modeling and simulation, including (1) external shading, (2) heat exchange between buildings, (3) urban micro-climate, (4) district energy systems, and (5) computing performance. These enhancements will be implemented in phases (considering available resources and timeline) with Phase I (September/October 2017 release) to cover (1), (2) and (3); Phase II for (5); and potentially Phase III for (4).  

# Team Discussion so far
Michael Witte provided inputs to the district energy system modeling. Amir Roth suggested to investigate the 64-to-32 bit conversion for EnergyPlus and provided feedback on the original proposal.

# Overview 
### Task 1 - External Shading
Accurate calculation of solar shading on building exterior surfaces is of great importance in whole building energy simulation. Modeling a building in the urban context may involve many shading surfaces from adjacent buildings for a target building, which can significantly slow down EnergyPlus simulations due to the currently implemented shading calculation algorithms. Furthermore, for urban scale building simulation which involves many buildings, there can be more efficient ways to do the shading calculations than the current way of doing shading calculations for every building separately. There is also the potential of using GPU and other parallel computing technologies for building shading calculations.

Various simulation tools, such as Radiance and Daysim, employing state-of-the-art ray-tracing simulation techniques, can be used to pre-calculate the shading fractions for each exterior building surface. However, in the current implementation, EnergyPlus doesn’t allow overriding exterior shading calculations with external data either by EMS or co-simulation interface, considering a large amount of information and the complexity of calculating the dynamic shading. Allowing this external input would enable a huge potential to speed up EnergyPlus in urban-scale simulations. Another benefit is to enable reusing the shading results for parametric runs which usually do not change external shading. Adding this feature to EnergyPlus would also be a benefit for OpenStudio by running radiance simulations before the EnergyPlus simulation and reusing the radiance calculations. Thus, we propose to add this feature to support linking external shading calculations with EnergyPlus.

### Task 2 – Heat Exchange between Buildings
Heat exchange between buildings is one key factor in understanding energy flows at the urban scale, including the long-wave radiant exchange between exterior surfaces of buildings, and between exterior surfaces of buildings and shades. However, these parts of heat exchange are often over-simplified in building energy modeling tools, as is the case in EnergyPlus, and thus the real temperatures at surfaces can only be reproduced with limited accuracy. In EnergyPlus, long-wave radiation heat exchange for a surface is calculated through the summation of radiation gain from the ground, sky, and air (Figure 1). A major assumption of this approach is that the modeled building's surfaces and those of adjacent buildings are at a uniform temperature and the long-wave radiation exchange is negligible – a situation that is an oversimplification in an urban context where urban canyon effect can be significant.

![LWR](EnergyPlusCurrentLWR.png)

***Figure 1 Illustration of EnergyPlus long-wave radiation heat exchange calculation***

We propose to modify the heat balance of the building’s exterior surfaces to explicitly consider their long-wave radiant exchange. The long-wave radiant exchange between building’s exterior surfaces and shading surfaces would not be considered in this proposal mainly due to temperatures of shades are not calculated in EnergyPlus as shades do not have specified thermal capacity.
Computational Fluid Dynamic (CFD) tools, simulating urban micro-climate, would need inputs of the building surface temperatures as boundary conditions. Thus CFD-based urban climate tools can be coupled with EnergyPlus for an integrated energy simulation. This is a very important subject in urban-scale energy modeling, considering the urban canyon effect and urban heat island effect, which also influences the building’s energy demand and indoor occupant thermal comfort.

### Task 3 – Urban Micro-Climate
Urban scale energy modeling is an emerging field that seeks to model multiple collections of buildings and the surrounding urban micro-climate. The microclimate around a building, establishing through the interactions with other buildings or the natural environment, is a significant factor influencing building energy consumption. On the one hand, the energy consumption of urban buildings is affected by the surrounding microclimate which differs from standard weather data, and the mutual obstructions between buildings which decrease sunlight and wind potentials for internal solar gains and passive cooling. On the other hand, the building construction itself affects both outdoor and indoor microclimate. As Figure 2 shows, the building model in EnergyPlus serves as the boundary condition in urban atmosphere models and the buildings exchange mass (air flow) and heat with the surrounding environment, including exhaust air from fans, DX condensing units, cooling towers, boilers, etc. 

![EplusUrban](EnergyPlusUrbanClimate.png)

***Figure 2 EnergyPlus data exchange with urban microclimate model***

The current implementation of EnergyPlus assumes a default vertical temperature gradient. However, it does not provide a link to allow co-simulation with urban microclimate models to override local environmental data at zone and surface levels, such as the outside air temperature, humidity, wind pressure and velocity. We propose to add a link to EnergyPlus for overriding these on-site environmental data, allowing the user to consider the local temperature due to urban heat island effect, the heat rejection from buildings via cooling towers, condensers, building surfaces, etc. This would enable co-simulation of the interaction between buildings and the urban micro-climate.

### Task 4 – District Heating and Cooling (DHC) Systems
In the current implementation of EnergyPlus, building loads can be reported as district heating and cooling energy using ZoneHVAC:IdealLoadsAirSystem or using full HVAC systems that are served by district heating/cooling systems.  Specifically, the load on a plant loop has to be reported out, and then entered as a load profile to the central plant model. EnergyPlus uses DistrictHotWater and DistrictChilledWater as boiler and chiller types for the central plant as well as fuel sources for the Exterior:FuelEquipment, and energy use is then reported as DistrictHeating and DistrictCooling. However, the calculation doesn’t account for energy and heat loss in pumping and controls of DHC systems, and this can be a key factor leading to discrepancies between the simulated and actual district energy performance. Furthermore, the integration of DHC systems with thermal storage, CHP, geothermal, and cooling with lake or bay water should also be taken into consideration to allow flexibility in modeling and simulation of advanced DHC systems. We propose to minimally enhance the modeling of DHC systems but focus on enabling a co-simulation interface between EnergyPlus and other DHC modeling tools (e.g., Modelica-based DHC tools).

### Task 5 – Computing Performance
Computing performance is a key challenge for urban-scale modeling and simulation as tens of thousands of building models need to be simulated simultaneously, while EnergyPlus reveals a relatively unsatisfying performance in I/O when running in very large batches of energy models. High performance computing (HPC) with numerous processors to simulate multiple buildings in parallel would alleviate the computing burden by distributing the workload. However, EnergyPlus was developed as a desktop application and not yet supercomputer ready. The current gap mainly lies in particularly the I/O intensive nature of the software engine, delivering very poor scaling performance over particular platforms regarding the bandwidth and the file-system saturation. 

First of all, lots of intermediate output and debugging messages are generated along with the simulation. The simulation results of a typical execution trace for a single simulation of EnergyPlus are written in a sub-directory, and more than 12 files are created, amounting at different sizes. Besides, constant moving and soft linking of the files are carried out as the simulation workflow executes, and these would to a great extent affect the computing performance of the simulation tool. Moreover, in terms of coupling with urban-climate simulation tools which may require in-memory data exchange in real time, the current output architecture of EnergyPlus may create huge overhead in data writing to physical files. 

Table 1 shows the I/O overhead of a test case performed on a desktop with 4-core Intel® Core™ i7 processor. The test compares the computing performance and storage usage of a DOE’s reference hospital building between two cases: (1) annual simulation with no written output variables and (2) annual simulation with reporting of zone and surface environmental variables for each time step. Each simulation generates 15 output files. With a full concerned output of 1453 variables, the simulation run time extends by 84%, and the large output files create a burden for storage and data query. 

***Table 1 EnergyPlus I/O overhead tests***

|  | No. of Output Variables | EnergyPlus 8.7.0 Run Time | Output files total size |
| ------ | ------ | ------ | ------ |
| RefBldgHospitalWithNoOutput | 0 | 4min 20ec | 12.2 MB |
| RefBldgHospitalWithFullOutput | 1453 | 7min 18sec | 3254 MB |

Apart from this, EnergyPlus currently uses double-precision 64-bit floating-point calculations almost exclusively. However, there is nothing about building physics that fundamentally requires 64-bit floating point. If converted to 32-bit, the memory and performance benefits of such a conversion could be significant. Memory requirements would be reduced by 50%, making better use of pages and caches, and allowing us to run multiple EnergyPlus copies on multi-threaded processors more easily. Output files will become smaller and reading/writing them will become faster, especially in hybrid binary formats like SQL and HDF5. In particular, when getting around to vectorization, the code will run twice as fast.

To alleviate the bottleneck of I/O performance, we propose to investigate and implement the listed enhancements to the to EnergyPlus to enable HPC: 

  - Adding a user-specifiable flag to minimize filesystem burden by reducing the file I/O in the simulation program to only files that are needed for main energy and performance results as well as for co-simulation applications, and piping the run-time alert messages to files in the HPC environment.
  - Adopting the Hierarchical Data Format 5 (HDF5), a unique open source technology suite for managing data collections of all sizes and complexity, as an alternative output format.  
  - Converting EnergyPlus to 32-bit floating-point calculation.
  - Potentially employing shared in-memory data storage and exchange to speed up co-simulation between EnergyPlus and other domain models (e.g., urban climate CFD) for exascale coupled urban systems simulation.

# Approaches 
The new features require a number of actions to be taken, which are described below.

**1. Support links to override environmental data from external calculations**

Throughout the EnergyPlus implementation, we could extend several external links to support overriding environmental data from external calculations, including:

**(1)	Scheduled surface shaded fractions**

We would add a variable __Field: External Shading Is Scheduled__ and a variable __Field: Surface External Shaded Fraction Schedule Name__ in the object __BuildingSurface:Detailed__ to specify the schedule. The variable __Field: External Shading Is Scheduled__ is the indicator of whether the external shading is pre-calculated and overridden in the simulation. If Yes is chosen, the schedule field can be linked to a Schedule object __Schedule:File__. The __Schedule:File__ object points to a CSV text file that has 8760-8784 hours of surface external shaded fraction data, which is pre-calculated and stored in the text file. The __Schedule__ object would be set up in initialization, and the results can be loaded at runtime.

	BuildingSurface:Detailed,
	       \memo Allows for detailed entry of building heat transfer surfaces.  Does not include subsurfaces such as windows or doors.
	       \extensible:3 -- duplicate last set of x,y,z coordinates (last 3 fields), remembering to remove ; from "inner" fields.
	       \format vertices
	       \min-fields 19
	  A1 , \field Name
	  …
	  A11, \field External Shading Is Scheduled
	       \type choice
	       \key No
	       \key Yes
	       \default No
	       \note If Yes, Surface External Shaded Fraction Schedule Name is required.
	  A12, \field Surface External Shaded Fraction Schedule Name
	       \type object-list
	       \object-list ScheduleNames
	       \note Used only if External Shading Is Scheduled = Yes.
	       \note Required if External Shading Is Scheduled = Yes.
	       \note Unit of schedule values should be watt.

**(2)	Surface long-wave radiation  from other buildings**

Apart from long-wave radiation from sky and ground currently considered in EnergyPlus implementation, for building energy modeling at a community or city scale with multiple buildings, the long-ware radiation from other building surfaces should also be considered (Figure 3). 

![ProposedLWR](EnergyPlusProposedLWR.png) 

***Figure 3 Long-wave radiation calculation considering surrounding surfaces***

The energy balance of an exterior building surface from its surrounding surfaces can be written as:

$$q_{LWR}= \epsilon\sigma[F_{s1}(T_{s1}^4-T_{surf}^4)+F_{s2}(T_{s2}^4-T_{surf}^4)+...+ F_{sn}(T_{sn}^4-T_{surf}^4)]$$

where,

ε=longware emittance of the surface,

σ=Stefan-Boltzmann constant,

$$T_{surf}$$=Outside surface temperature of the exterior surface,

$$T_{si}$$=Outside surface temperature of the surrounding surface i,

$$F_{si}$$=View factor of surrounding surface i to the exterior surface.

However, to avoid the complexity of iteratively calculating the long-wave radiation between building surfaces, we would simplify the case by using the pre-calculated surface temperature at the last time step for the current time step surface heat balance calculation. Considering the temperature of building exterior surfaces would not change too much between the last and current time steps, this simplification may sacrifice little accuracy but significantly improve the computing performance. To enable this, in Energy+.idd, we would add a variable __Field: Number of Surrounding Surfaces__ in the object __BuildingSurface:Detailed__ to indicate the number of surrounding building surfaces to the calculated exterior surfaces. We would add a list of variables defining the view factor and the outside surface temperature schedule of these surrounding surfaces, such as __Field: Surrounding Surface 1 View Factor__ and __Field: Surrounding Surface 1 Temperature Schedule Name__. View factors are assumed to be constant values, which can be overwritten in the EnergyPlus initialization stage by reading the calculation results of the external LWR calculation tool. The outside surface temperature can be exchanged with the external tool and be overwritten (by using the schedule) at each time step in EnergyPlus run time. 

	BuildingSurface:Detailed,
	       \memo Allows for detailed entry of building heat transfer surfaces.  Does not include subsurfaces such as windows or doors.
	       \extensible:3 -- duplicate last set of x,y,z coordinates (last 3 fields), remembering to remove ; from "inner" fields.
	       \format vertices
	       \min-fields 19
	  A1 , \field Name
	  …
	  N363, \field Number of Surrounding Surfaces       
	       \autocalculatable
	       \minimum 0
	       \maximum 30
	       \default 0
	  N364, \field Surrounding Surface 1 View Factor
	       \type real
	       \note from the exterior of the surface     
	       \minimum 0.0
	       \maximum 1.0
	  N365, \field Surrounding Surface 1 Temperature Schedule Name
	       \note Schedule values should be in C
	       \type object-list
	       \object-list ScheduleNames
	  N366, \field Surrounding Surface 2 View Factor
	       \type real
	       \note from the exterior of the surface     
	       \minimum 0.0
	       \maximum 1.0
	  N367; \field Surrounding Surface 2 Temperature Schedule Name
	       \note Schedule values should be in C
	       \type object-list
	       \object-list ScheduleNames
	  …


**2. Adding LocalOutdoorAir:Node object to allow local outdoor air nodes for zones and surfaces**

We propose to modify the __OutdoorAir:Node__ object to enable local outdoor air nodes at each zone level and each surface level, which can be used to provide local ambient air conditions. If __Field:IsLocalNode__ is defined as YES, each __OutdoorAir:Node__ object is linked to a zone or surface object. Each object should define its own environmental variable fields such as temperature, humidity ratio, wind speed and direction, and should have its own relative coordinates (Figure 4).

![LocalNode](EnergyPlusLocalAirNode.png) 

***Figure 4 Local air node at zone and surface level***

We would modify the Energy+.idd to add fields to the __OutdoorAir:Node__ object with the variables mentioned above:

	OutdoorAir:Node,
	       \memo This object sets the temperature and humidity conditions
	       \memo for an outdoor air node.  It allows the height above ground to be
	       \memo specified.  This object may be used more than once.
	       \memo The same node name may not appear in both an OutdoorAir:Node object and
	       \memo an OutdoorAir:NodeList object.
	       \memo If defined as local node, this object sets the temperature and humidity conditions
	       \memo for an outdoor air node.  It allows the (x,y,z) coordinate of 
	       \memo the node to be specified.
	  A1,  \field Name
	       \required-field
	       \type node
	  A2,  \field IsLocalNode
	       \required-field
	       \type choice
	       \key No
	       \key Yes
	       \default No
	       \note If Yes, N2-N8 is required.
	  A3,  \field Zone / Surface Object Name
	       \note Name of the corresponding zone / surface object
	       \required-field
	       \type object-list
	       \object-list ZoneAndZoneListNames, SurfaceAndSurfaceListNames 
	  N1 ; \field Height Above Ground
	       \note A value less than zero indicates that the height will be ignored and the weather file conditions will be used.
	       \type real
	       \units m
	       \default -1.0        
	  N2,  \field Outdoor Air Temperature
	       \type real
	       \units C
	       \minimum -100
	       \maximum 100
	  N3,  \field Outdoor Air Humidity Ratio
	       \type real
	       \units kgWater/kgDryAir
	       \minimum> 0
	  N4,  \field Outdoor Wind Speed
	       \type real
	       \units m/s
	       \minimum 0.0
	       \maximum 40.0
	  N5,  \field Outdoor Wind Direction
	       \units deg
	       \minimum 0
	       \maximum 360
	       \note North=0.0 East=90.0
	       \note 0 and 360 are the same direction.
	       \type real
	  N6 , \field X coordinate
	       \note X coordinate
	       \type real
	       \units m
	       \default X coordinate of the center of the zone or surface object
	  N7 , \field Y coordinate
	       \note Y coordinate
	       \type real
	       \units m
	       \default Y coordinate of the center of the zone or surface object
	  N8 ; \field Z coordinate
	       \note Z coordinate
	       \type real
	       \units m

The local environmental conditions can be exchanged with or overwritten by external simulation or calculation results at each time step in run time.

**3. Converting to 32-bit floating-point calculation**

We propose to convert the current 64-bit floating-point calculations to 32-bit FP in EnergyPlus. The task may include:

(a)	Change the data-type declarations throughout the code wherever required;
(b)	Modify data structures with fixed size and alignment suitable for 32-bit calculation;
(c)	Fix the allocation and deallocation of memory use wherever required;
(d)	Test the calculation accuracy, bitwise reproducibility, and computing performance gain with benchmark suite and metrics.

**4. Minimizing the runtime I/O burden**

We propose to minimize the file system burden of I/O by taking the following two steps:

(a)	Allow user input of toggle on and off the intermediate and final outputs, and reduce the file I/O in the simulation program to only the files based on users’ needs;
(b)	Allow user input to pipe the run-time alert messages to files in the HPC environment.

**5.	Adopting HDF5 as an alternative output format**

Hierarchical Data Format 5 (HDF5) is a unique open source technology suite for managing data collections of all sizes and complexity. It was specifically designed for high volume and/or complex data and for flexible, efficient storage and I/O. HDF5 files can contain binary data (in many representations) and allow direct access to parts of the file without first parsing the entire contents. HDF5 also allows hierarchical data objects to be expressed in a natural manner (similar to directories and files), in contrast to the tables in a relational database. 

HDF5 consists of:

+ A File Format for storing HDF5 data.
+ A Data Model for logically organizing and accessing HDF5 data from an application.
+ The Software (libraries, language interfaces, and tools) for working with this format.

The HDF5 File Format is defined by and adheres to the HDF5 File Format Specification, which specifies the bit-level organization of an HDF5 file on storage media. The HDF5 Data Model, also known as the HDF5 Abstract (or Logical) Data Model consists of the building blocks for data organization and specification in HDF5.
We propose to adopt the HDF5 File Format as an alternative output format to store the output variables, and utilize the HDF5 Data Model to reform the output structure and architecture in EnergyPlus. 

# Testing/Validation/Data Sources

TBD

# Input Output Reference Documentation

TBD

# Engineering Reference

TBD

# Output Details and Examples

TBD

# Example Files and Transition Changes

No transition changes are envisioned as a result of the new features (but this can change based on feedback from the team). New example files will be developed to demonstrate the new features.

# References

n/a


