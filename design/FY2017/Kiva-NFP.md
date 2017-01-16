# Kiva(TM) integration for foundation heat transfer in EnergyPlus

## Justification for New Feature

Simulating the multi-dimensional heat flow around building foundaitons is a complicated problem that has been handled in many ways in EnergyPlus. Users are often confronted with choosing between fast or accurate methods. As a rule, the user experience for defining foundation surfaces should:

1. Require little-to-no additional effort than defining other surface boundary conditions.
2. Require little-to-no additional computation time than other conduction calculations (e.g. CTFs). Relatively speaking, it shouldn't be more than half the computation time for a six-surface, single-zone simulation.
3. Provide the capability to characterize common foundation design alternatives, namely interior and exterior, vertical and horizontal insulation placement, and whole-slab insulation.

Foundation heat transfer in EnergyPlus is currently characterized by defining one of four boundary conditions for below-grade surfaces:

- Ground
- OtherSideCoefficients (as a pre-processed schedule [this includes the expand objects versions of Ground Preprocessor boundary conditions]).
- OtherSideConditionsModel (referencing the SurfaceProperty:OtherSideConditionsModel, which references UndergroundPipingSystem [PipingSystem:\*] or GroundCoupledSurface [Site:GroundDomain\*] objects )
- GroundFCfactorMethod

Each of these methods suffers from one or more of the following issues, in that they are:

- inaccurate,
- slow,
- difficult to use, or
- lacking in capability.

Kiva is fast, accurate, and highly capable. Usability depends on how the input interface (i.e. the IDF objects) are designed. More intuitive input objects are also proposed here.

A comprison of Kiva to these other methods is shown below:

| Method:                                       | GroundTemperatures | GroundFCfactorMethod | GroundDomain | Kiva |
|-----------------------------------------------|:------------------:|:--------------------:|:------------:|:----:|
| Dimensions                                    |          1         |           1          |       3      |   2  |
| Computation Time (s)                          |     Negligible     |      Negligible      |     ~350     |  ~5  |
| Mesh Independent (within 3%)                  |         NA         |          NA          |      No      |  Yes |
| Pre-initialization                            |         NA         |          NA          |      No      |  Yes |
| Moisture Content                              |         NA         |          NA          |      Yes     |  No  |
| Single Domain (i.e., no artificial interface) |  No (preprocessed) |   No (preprocessed)  |      No      |  Yes |
| BESTESTed                                     |         No         |          No          |     Yes?     |  Yes |
| Thermal Bridging (around foundation wall)     |         No         |          No          |      No      |  Yes |
| Interior Horizontal                           |         No         |          No          |      Yes     |  Yes |
| Interior Vertical (a.k.a "Gap")               |         No         |          No          |      No      |  Yes |
| Exterior Horizontal                           |         No         |          No          |      No      |  Yes |
| Exterior Vertical                             |         No         |          No          |      Yes     |  Yes |
| Full Slab                                     |         Yes        |          No          |      Yes     |  Yes |

## Overview

Kiva is an open source C++ project developed by Big Ladder Software:

[http://bigladdersoftware.com/projects/kiva/](http://bigladdersoftware.com/projects/kiva/)

Kiva is the product of Neal Kruis's dissertation where he demonstrated that accurate foundation heat transfer calculations can be performed quickly (on the order of 5 seconds) without any noticeable loss of accuracy relative to a mesh-independent, fully three-dimensional simulation.

The computational core of Kiva can be compiled as an independent library and linked to the EnergyPlus executable.

## Input Output Reference Documentation: Foundation

Foundation objects describe boundary conditions for ground-coupled foundation surfaces. Surfaces with the "Outside Boundary Condition" defined as "Foundation", may also refer to a Foundation object in the "Outside Boundary Condition Object" field (if unspecified, a default Foundation object will be created and applied).

The inputs from Foundation objects are translated into the Kiva foundation heat transfer methodology. Kiva generates a 2-dimensional heat transfer calculation to represent heat flow from the zone into the ground. Foundation surfaces do not use the same HeatBalanceAlgorithm as the rest of the model.

Foundation objects are used to describe the 2-dimensional features that cannot be captured by the typical 1-dimensional constructions used in EnergyPlus. Figure 1 illustrates the 2-dimensional context of these features as interpreted by Kiva.

![Figure 1: 2-dimensional context for Foundation input](http://kiva.readthedocs.io/en/latest/_images/context.png)

This context allows for a finer description of the structural and insulation components of a foundation that impact heat transfer (Figure 2).

![Figure 2: Structural and insulation components of Foundation](http://kiva.readthedocs.io/en/latest/_images/components.png)

The dimensions are continuously variable allowing users to specify any combination of:

- Foundation type: by varying the depth of the slab relative to the wall top. (Note: if there are no associated wall surfaces, the foundation is assumed to be slab-on-grade)
- Insulation location: by varying the depth, width, and thickness of the various insulation components
- Above-grade exposure: by varying the height of the wall top relative to the exterior grade.

<!-- TODO add figures illustrating alternatives -->

### Example IDF

```
BuildingSurface:Detailed,
  Slab Floor,         !- Name
  Floor,              !- Surface Type
  Slab Construction,  !- Construction Name
  Living Room,        !- Zone Name
  Foundation,         !- Outside Boundary Condition
  Slab Details,       !- Outside Boundary Condition Object
  No,                 !- Sun Exposure
  No,                 !- Wind Exposure
  0.0,                !- View Factor to Ground
  4,                  !- Number of Vertices
  0.0, 0.0, 0.0,      !- Vertex 1
  0.0, 20.0, 0.0,     !- Vertex 2
  20.0, 20.0, 0.0,    !- Vertex 3
  20.0, 0.0, 0.0;     !- Vertex 4

Foundation,
  Slab Details,              !- Name
  XPS,                       !- Interior Horizontal Insulation Material Name
  0.2,                       !- Interior Horizontal Insulation Depth
  0.6,                       !- Interior Horizontal Insulation Width
  XPS,                       !- Interior Vertical Insulation Material Name
  0.2,                       !- Interior Vertical Insulation Depth
  ,                          !- Exterior Horizontal Insulation Material Name
  ,                          !- Exterior Horizontal Insulation Depth
  ,                          !- Exterior Horizontal Insulation Width
  ,                          !- Exterior Vertical Insulation Material Name
  ,                          !- Exterior Vertical Insulation Depth
  0.2,                       !- Wall Height Above Grade
  Slab Footer Construction,  !- Footer Wall Construction
  0.3,                       !- Footer Depth
  0.864,                     !- Soil Conductivity
  1510,                      !- Soil Density
  1260,                      !- Soil Specific Heat
  0.9,                       !- Ground Solar Absorptivity
  0.9,                       !- Ground Thermal Absorptivity
  0.03,                      !- Ground Surface Roughness
  40,                        !- Far-Field Width
  Autocalculate,             !- Deep-Ground Boundary Condition
  40,                        !- Deep-Ground Depth
  0.02,                      !- Minimum Cell Dimension
  1.50,                      !- Maximum Cell Growth Coefficient
  Accelerated,               !- Initialization Method
  1;                         !- Years of Warmup

Material,
  XPS,    !- Name
  Rough,  !- Roughness
  0.05,   !- Thickness
  0.029,  !- Conductivity
  28,     !- Density
  1450,   !- Specific Heat
  0.9,    !- Thermal Absorptance
  0.7,    !- Solar Absorptance
  0.7;    !- Visible Absorptance


Material,
  Concrete,  !- Name
  Rough,     !- Roughness
  0.3,       !- Thickness
  1.98,      !- Conductivity
  1900,      !- Density
  1800,      !- Specific Heat
  0.9,       !- Thermal Absorptance
  0.7,       !- Solar Absorptance
  0.7;       !- Visible Absorptance

Construction,
  Slab Footer Construction,  !- Name
  Concrete;                  !- Outside Layer Name
```

### Input Description

#### Field: Name

The unique identifier of the Foundation object. Referenced by a the "Outside Boundary Condition Object" field in a surface object.

#### Field: Interior Horizontal Insulation Material Name

A reference to a material object associated with the interior horizontal insulation. If left blank, no interior horizontal insulation will be used. Default: blank.

#### Field: Interior Horizontal Insulation Depth

Distance from the wall top to the top of interior horizontal insulation, in m. Required if Interior Horizontal Insulation Material Name is defined.

#### Field: Interior Horizontal Insulation Width

Extent of insulation as measured from the wall interior to the edge of interior horizontal insulation, in m. Required if Interior Horizontal Insulation Material Name is defined.

#### Field: Interior Vertical Insulation Material Name

A reference to a material object associated with the interior vertical insulation. If left blank, no interior vertical insulation will be used. Default: blank.

#### Field: Interior Vertical Insulation Depth

Extent of insulation as measured from the wall top to the bottom edge of the interior vertical insulation, in m. Required if Interior Vertical Insulation Material Name is defined.

#### Field: Exterior Horizontal Insulation Material Name

A reference to a material object associated with the exterior horizontal insulation. If left blank, no exterior horizontal insulation will be used. Default: blank.

#### Field: Exterior Horizontal Insulation Depth

Distance from the wall top to the top of exterior horizontal insulation, in m. Required if Exterior Horizontal Insulation Material Name is defined.

#### Field: Exterior Horizontal Insulation Width

Extent of insulation as measured from the wall exterior to the edge of exterior horizontal insulation, in m. Required if Exterior Horizontal Insulation Material Name is defined.

#### Field: Exterior Vertical Insulation Material Name

A reference to a material object associated with the exterior vertical insulation. If left blank, no exterior vertical insulation will be used. Default: blank.

#### Field: Exterior Vertical Insulation Depth

Extent of insulation as measured from the wall top to the bottom edge of the exterior vertical insulation, in m. Required if Exterior Vertical Insulation Material Name is defined.

#### Field: Wall Height Above Grade

Distance from the exterior grade to the wall top, in m. Default: 0.2 m

#### Field: Footer Wall Construction

Defines the construction of the foundation wall below the associated surfaces. This is required for slab foundations where the foundation wall is not exposed to the zone (and has no below grade surface to otherwise assign a construction. For foundations with below-grade walls, this construction must be the same construction as the wall surfaces (or left blank). Default: Same construction as any associated below-grade wall surfaces, or 0.3 m wide poured concrete (conductivity = 1.98 W/m-K, density = 1900 kg/m3, specific heat = 1800 J/kg-K).

#### Field: Footer Depth

Distance from the wall bottom to the bottom of the foundation footer, in m. Default: 0.3 m.

<!-- TODO insert figure -->

#### Field: Soil Conductivity

The thermal conductivity of the soil, in W/m-K. Default: 0.864 W/m-K.

#### Field: Soil Density

The bulk density of the soil, in kg/m3. Default: 1510 kg/m3.

#### Field: Soil Specific Heat

The specific heat of the soil, in J/kg-K. Default: 1260 J/kg-K

<!-- TODO insert figure -->

#### Field: Ground Solar Absorptivity

Solar absorptivity of the exterior grade surface. Default: 0.9.

#### Field: Ground Thermal Absorptivity

Long-wave absorptivity (emissivity) of the exterior grade surface. Default: 0.9.

#### Field: Ground Surface Roughness

Long-wave emissivity of the exterior grade surface. Default: 0.03 m.

#### Field: Far-Field Width

The distance from the wall interior to the zero horizontal heat flux (i.e. adiabatic) far-field boundary, in m. This distance represents either the distance halfway between this foundation and a similar foundation of a neighboring building, or a distance adequately far from the foundation such that it is isolated from the effects of the boundary (typically >= 40 m). Default: 40 m.

#### Field: Deep-Ground Boundary Condition

Defines the type of boundary condition to apply at the Deep-Ground Depth. Options are:

- ZeroFlux
- GroundWater
- Autocalculate

ZeroFlux applies a zero vertical heat flux (i.e. adiabatic) boundary condition. GroundWater applies a constant temperature boundary condition, with a temperature equal to the average outdoor air dry-bulb temperature from the environment(s). Auto applies either boundary condition depending on the elevation of the building site (Williams and Williamson, 1989):

$$d_{wt}=0.1022\cdot d_{elev}$$

If $d_{wt} \le$ 40 m., the GroundWater boundary is applied, otherwise a ZeroFlux boundary is applied at 40 m.

Default: Autocalculate.

#### Field: Deep-Ground Depth

The distance from the exterior grade to the deep ground boundary, in m. This distance represents either the distance to the ground water level, or a distance adequately far from the foundation such that it is isolated from the effects of the boundary (typically >= 40 m). Default 40 m, or the distance determined by the "Auto" Deep-Ground Boundary Condition.

#### Field: Minimum Cell Dimension

The minimum cell dimension, in m, used in the Kiva discretization. Default: 0.02 m.

#### Field: Maximum Cell Growth Coefficient

The maximum ratio of growth between neighboring cells in the direction away from the near-field area of interest. Default: 1.50.

#### Field: Initialization Method

Defines the method used to initialize the ground temperatures. Options are:

- Accelerated
- SteadyState
- Kusuda

Default: Accelerated

#### Field: Years of Warmup

Number of years to simulate the ground before beginning of the run period. This is before the warm-up period defined by the Building object. Default: 1 year.

### Output Description

Output for surfaces with Foundation boundary condition type will include all opaque surface output variables except:

- Surface Outside Face variables (since there is no "Outside Face")
- Surface Heat Storage variables (since this definition depends on an "Outside Face")
- Surface Internal Source Location Temperature (Kiva doesn't handle internal sources yet, but this is a possible future enhancement)

## New Input Objects

IDD definitions will be created after input fields are finalized.

## Approach

Kiva will take boundary conditions from EnergyPlus:

- Zone temperature (from previous timestep)
- Weather data
- Solar position
- Zone radiation (solar, IR, etc.)

and return the convective heat gains and surface temperatures for the associated surfaces. No CTF calculations will be performed.

Kiva explicitly handles single zone foundations with a single set of wall and slab constructions. However, multiple instances of Kiva can be run simultaneously (i.e., in parallel) to approximate more complex foundation descriptions.

The two-dimensional approximation method employed by Kiva relies on knowing the footprint shape, area, and exposed perimeter of each instance. The appropriate footprint shape, area, and exposed perimeter for each instance will be defined within the context of the overall geometry of the Foundation surfaces.

In some cases, a single Foundation boundary condition might require multiple Kiva instances:

**Multiple Foundations:** If a building has multiple foundations, e.g. a basement and a slab or even multiple slabs, they are simulated as separate instances of Kiva.

**Multiple Zones:** If the surfaces in several zones reference the same Foundation object, each zone will be calculated using separate Kiva instances.

**Walk-Out Basements:** A separate Kiva instance will be run for each set of walls with the same height associated with the same Foundation object. The heat flux through the associated floor will be weighted according to the fraction of the total exposed perimeter represented by the wall.

**Multiple Floor Surfaces:** If a slab has multiple foundation constructions (e.g., partially carpeted) each surface must reference a separate Foundation object.

### Geometry

Kiva defines the foundations using a 2-dimensional regular grid. Many of the Kiva inputs will be inferred from the geometry of the corresponding Foundation surfaces even if the geometry defined in EnergyPlus doesn't strictly align with the coordinate system used by Kiva. This inferences help improve the overall usability of the Foundation objects in attempt to handle as many situations as possible. If an inference cannot be made, the limitation will result in a fatal error.

**Footprint Shape:** Determined from the composite shape of the floor surfaces.

**Exposed Perimeter:** Determined from the composite shape of the Foundation surfaces.

**Foundation Depth:** Determined by the average z value of the associated floor surface(s) relative to the height of the associated wall surface(s). Produce warning if floor isn't flat.

**Foundation Wall Height:** Determined by the z-coordinates of each surface.

**Core Surfaces:** Slabs with no exposed perimeter will use the results for the slab core (measured at a distance from the perimeter corresponding to the radius of concentric circles of the same areas as the core surface and all surfaces). Basements and crawlspaces must have exposed perimeter. Likewise, the perimeter surfaces will exclude the heat flux from the core. Core and perimeter surfaces must have identical Foundation objects. (Note: This is a challenging task and may be deferred depending on scope.)


### Warm-Up and Design Days

As the ground has time constants with effects on the order of years, the traditional "warm-up" period in EnergyPlus (of repeating a single day) is not long enough to capture these effects. Furthermore, repeating a single day cannot capture the seasonal history of the ground. Testing may reveal issues that need to be adjusted solely for the warm-up of the ground.

Each Kiva instance can be "warmed-up" independently from EnergyPlus warm-up, but separate warm-ups may make it difficult to make the warm-up period converge. The independent warm-up of the ground depends on an assumed indoor temperature and boundary conditions. We can use thermostat settings from controlled zones or an assumed 21 C for uncontrolled zones. Perhaps this should be a user input into the Foundation object?

For shorter runs, e.g., design days, the history of the ground may depend on annual weather information that isn't necessarily available for the given day. In these cases, it's possible to initialize the ground with a steady-state solution.

### Limitations:

- Exactly one floor surface must reference each Foundation object.
- All wall surfaces associated with a given Foundation object must have the same construction.

## Source Code Repository

This will be included as a CMake external project downloaded from GitHub similar to the Window Calculation Engine (following LBNL's precedent).

## Testing/Validation/Data Sources

The 2-dimensional Kiva method has been tested against the BESTEST Ground coupled cases with accuracy within 3% of the reference solutions.

## References

[1] N. Kruis and M. Krarti, "Kiva: A Numerical Framework for Improving Foundation Heat Transfer Calculations," Journal of Building Performance Simulation, vol. 8, no. 6, pp. 449-468, Dec. 2015.

[2] N. Kruis and M. Krarti, "Three-Dimensional Accuracy with Two-Dimensional Computation Speed: Using the Kiva Numerical Framework to Improve Foundation Heat Transfer Calculations," Journal of Building Performance Simulation, in-publication.

[3] T. Williams and A. Williamson, "Estimating Water-Table Altitudes for Regional Ground-Water Flow Modeling, U.S. Gulf Coast," Ground Water, vol. 27, no. 3, pp. 333-340, Dec. 1989.

## E-mail and Conference Call Conclusions

No communications yet.
