Allow Multiple Ground Surface Temperature and Reflectance Objects
======================

**Bereket Nigusse**

**Florida Solar Energy Center**

 - First draft: April 26, 2022
 - Modified Date: NA

## Justification for New Feature ##

Currently EnergyPlus only allows one single ground surface with user defined ground solar reflectance and ground temperature. Each exterior surface has a single view factor to ground. A real building usually sees multiple types of ground surfaces, e.g., bare soil, grass, sidewalks, driveways, water surface, which may have different solar reflectance and ground temperature.

**- this feature is intended for use with exterior surfaces only **
`
## E-mail and  Conference Call Conclusions ##

NA

## Overview ##

### Current Code ###

(1) Exterior surface LWR exchange with the ground is calculated using outside air dryblub temperature (default)
(2) Currently ground surface temperature can be specified using `SurfaceProperty:SurroundingSurfaces` object 
(3) Exiting model uses global Site:GroundReflectance object

**- This enhancement allows each exterior surfaces to see different ground surface temperature and reflectance objects.

## Implementation Approach ##

*(1) This new feature can be implemented using the following two existing Objects:
`SurfaceProperty:LocalEnvironment` and `SurfaceProperty:SurroundingSurfaces`
*(2) Requires modifying SurfaceProperty:SurroundingSurfaces` object
*(3) Adds new input fields for Ground Reflectance Schedule Name
*(4) Adds new input fields for Surrounding Surface X Reflectance Schedule Name
*(4) View factor weighed ground reflectance will be calculated for each exterior surface

### Exsting SurfaceProperty:SurroundingSurfaces ###



![Figure 1](GroundSurfaceProperties_I.PNG)

** Figure 1. Surface Property Object **
 
SurfaceProperty:SurroundingSurfaces,
       \min-fields 10
       \memo This object defines a list of surrounding surfaces for an exterior surface.
       \extensible:4 -- duplicate last set of surrounding surface properties (the last four fields), remembering to remove ; from "inner" fields.
   A1, \field Name
       \required-field
       \type alpha
       \reference SurroundingSurfacesNames
   N1, \field Sky View Factor
       \minimum 0.0
       \maximum 1.0
       \default 0.5
       \note optional
   A2, \field Sky Temperature Schedule Name
       \type object-list
       \object-list ScheduleNames
       \note Schedule values are real numbers, -100.0 to 100.0, units C
       \note optional
   N2, \field Ground View Factor
       \minimum 0.0
       \maximum 1.0
       \default 0.5
       \note optional
   A3, \field Ground Temperature Schedule Name
       \type object-list
       \object-list ScheduleNames
       \note Schedule values are real numbers, -100.0 to 100.0, units C
       \note optional
`   A4, \field Ground Reflectance Schedule Name
`       \required-field
`       \type object-list
`       \object-list ScheduleNames
`       \note Schedule values are fractions, 0.0 to 1.0, dimensionless
   A5, \field Surrounding Surface 1 Name
       \begin-extensible
       \required-field
       \type alpha
   N3, \field Surrounding Surface 1 View Factor
       \required-field
       \minimum 0.0
       \maximum 1.0
       \default 0.0
   A6, \field Surrounding Surface 1 Temperature Schedule Name
       \required-field
       \type object-list
       \object-list ScheduleNames
       \note Schedule values are real numbers, -100.0 to 100.0, units C	   
`   A7, \field Surrounding Surface 1 Reflectance Schedule Name
`       \required-field
`       \type object-list
`       \object-list ScheduleNames
`       \note Schedule values are fractions, 0.0 to 1.0, dimensionless`
 
        ...
		
  A32, \field Surrounding Surface 10 Name
       \type alpha
  N12, \field Surrounding Surface 10 View Factor
       \minimum 0.0
       \maximum 1.0
       \default 0.0
  A33, \field Surrounding Surface 10 Temperature Schedule Name
       \type object-list
       \object-list ScheduleNames
       \note Schedule values are real numbers, -100.0 to 100.0, units C
`  A34; \field Surrounding Surface 10 Reflectance Schedule Name
`       \type object-list
`       \object-list ScheduleNames
`       \note Schedule values are fractions, 0.0 to 1.0, dimensionless
`       \note optional	

SurfaceProperty:LocalEnvironment,
       \min-fields 3
       \memo This object defines the local environment properties of an exterior surface.
       \memo One or more environment properties have to be defined and linked to the exterior surface.
   A1, \field Name
       \required-field
       \type alpha
       \reference SurfaceLocalEnvironmentNames
   A2, \field Exterior Surface Name
       \type object-list
       \object-list SurfaceNames
       \note Enter the name of an exterior surface object
   A3, \field External Shading Fraction Schedule Name
       \type object-list
       \object-list ScheduleNames
       \note Enter the name of a Schedule object
   A4, \field Surrounding Surfaces Object Name
       \type object-list
       \object-list SurroundingSurfacesNames
       \note Enter the name of a SurfaceProperty:SurroundingSurfaces object
   A5; \field Outdoor Air Node Name
       \type object-list
       \object-list OutdoorAirNodeNames
       \note Enter the name of an OutdoorAir:Node object
	   
## Testing/Validation/Data Source(s): ##

Demonstrate that the new approach duplicates the current results using exact set of inputs
Unit tests will be added to demonstrate the new feature.

## Input Output Reference Documentation ##

Update the documentations for changed sections

## Engineering Reference ##

As needed.

## Example File and Transition Changes ##

An example file will be modified to demonstrate the use of multiple ground surface objects. Simulation results will be examined and sample results will be provided.

Transition is required.

## Proposed Report Variables: ##

As needed.


## References ##

N/A
