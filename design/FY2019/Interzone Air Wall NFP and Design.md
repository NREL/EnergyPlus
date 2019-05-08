Air Thermal Boundary - aka Interzone Air Wall
================

**Michael J. Witte, GARD Analytics, Inc.**

 - Original May 8, 2019
 - Revision Date
 

## Table of Contents ##

[Justification for New Feature](#justification-for-new-feature)

[E-mail and  Conference Call Conclusions](#e-mail-and-conference-call-conclusions)

[Approach](#approach)

[Testing/Validation/Data Sources](#testingvalidationdata-sources)

[Input Output Reference Documentation](#input-output-reference-documentation)

[Engineering Reference](#engineering-reference)

[Example File and Transition Changes](#example-file-and-transition-changes)

[References](#references)

[Design](#design)

## Justification for New Feature ##

Open boundaries between thermal zones occur frequently in buildings. Examples include:
 - Open plan offices with core and perimeter zones.
 - Open doorway from one space to another, e.g. hallway opening into a larger space.
 - Service opening, e.g. serving window between kitchen and dining area.
 - High spaces where the occupied zone is conditioned differently from the rest, e.g. atrium or auditorium

Open thermal boundaries allow the following heat transfer to occur:
 - Solar gains and daylighting to pass through as if there is no boundary
 - Radiant exhange between building surfaces as if there is no boundary.
 - Distribute radiant internal  gains as if there is no boundary.
 - Allow airflow and/or mixing to occur between the adjacent zones

This type of boundary occurs frequently in energy models, and is often called an "air wall." 
EnergyPlus has no direct way to model such a boundary. Common workarounds include some 
combination of the following:
 - Near-perfect interzone window
 - Surface using `Material:InfraredTransparent`
 - Surface using a very thin highly conductive material
 - `ZoneMixing` and `ZoneCrossMixing`
 - AirflowNetwork openings

All of these options have serious limitations, especially the lack of solar/daylight penetration and 
no direct radiant exchange across the boundary. 

## E-mail and  Conference Call Conclusions ##

n/a

## Approach ##

The proposed approach is to remove the air wall surface entirely from most of the heat transfer calculations
and create combined superzones for solar, daylighting, and radiant exchange calculations. Air exchange is
more complicated.

### Radiant Exchange ###
For surface-to-surface radiant exchange, all surfaces in the zones connected by airwalls will be combined into
a superzone for the radiant exchange calculations. Radiant gains from lights, people, HVAC equipment, etc. will 
be distributed to all surfaces in the superzone.

#### Pitfalls ####
 - Radiant exchange calculations become very slow for zones with a large number of surfaces. This approach
 will exaggerate that problem.
 - Within the superzone it is likely that many surfaces will not have a direct view to all of the surfaces
 in the combined zone. Presently there is no algorithm to exclude certain views. View factors could be determined
 external to EnergyPlus and imported using `ZoneProperty:UserViewFactors:bySurfaceName`.
 - Input processing will need to allow view factors (for radiant HVAC equipment) to cross zone boundaries.
 - Re-simulation for radiant HVAC equipment will involve all zones in the superzone, causing extra computation time.
 - Reporting for zone-level radiant gains may be confusing. For example, radiant gains which have a source in Zone A
 may be distributed to other zones as well as Zone A. Even though the source of the gain is Zone A, the other zones
 should be reporting their share of the distributed gain.
 
### Solar Gains and Daylighting ###
For solar distribution and daylighting calculations, the zones connected by airwalls will be combined into a superzone.

#### Pitfalls ####
 - For `FullInteriorAndExterior` solar distribution options, the convex zone limitation will likely be tripped
 by the combined zones. The ultimate goal would be to crack this nut once and for all for any situation (airwalls or not).
 But the fallback position is to figure out a way to trap excess solar that has made it past an interior obstruction and
 remove it - if possible.
 - Reporting for solar gains may be confusing. For example, window heat gain calculations will need to
 account for transmitted solar which passes through to another zone (and the reverse).

### Air Exchange ###
For simple air exchange (not AirflowNetwork) the user will be expected to input `ZoneMixing` or `ZoneCrossMixing`
objects the same as always. For simulations using AirflowNetwork, it gets a little more complicated. For smaller openings,
such as open doorways, the current `AirflowNetwork:MultiZone:Component:DetailedOpening`, 
`AirflowNetwork:MultiZone:Component:SimpleOpening` and `AirflowNetwork:MultiZone:Component:HorizontalOpening`
objects should be adequate. For very large openings (horizontal and vertical) another approach will be required. One
possibility is to use a superzone for the AirflowNetwork calculations. Or perhaps a new opening type could be developed
to approximate the connection.

#### Pitfalls ####
 - It's unclear what the values should be for the simple mixing models across an open boundary.
 - AirflowNetwork reporting may be confusing if a superzone approach is used.
 
## Testing/Validation/Data Sources ##

Energy balances and comparisons with simlations using detailed zoning and combined zones will be used to confirm
that the results are reasonable. Various levels of reporing including design component loads will all need to
be consistent with each other.

## Input Output Reference Documentation ##

The proposed input mechanism is to modify the `BuildingSurface:Detailed` and `FenestrationSurface:Detailed` objects to allow a new
choice of *AirWall* for Surface Type. When this is selected, the Construction input is left blank (or ignored with a warning).
 Also, the only allowed choices for Outside Boundary Condition will be *Surface* and *Zone*.

## Input Description ##

**BuildingSurface:Detailed**

Field: Surface Type

. . . *AirWall* indicates an open boundary between two zones. When this option is selected, Construction Name is ignored and the
Outside Boundary Condition must be either *Surface* or *Zone*.

Field: Construction Name

. . . This field is ignored if the Surface Type is *AirWall*.

Field: Outside Boundary Condition

. . . If Surface Type is *AirWall* the only valid options are *Surface* and *Zone*.

**FenestrationSurface:Detailed**

Field: Surface Type

. . . *AirWall* indicates an open boundary between two zones. When this option is selected, Construction Name is ignored and the
Outside Boundary Condition must be either *Surface* or *Zone*.

Field: Construction Name

. . . This field is ignored if the Surface Type is *AirWall*.


## Outputs Description ##

Various output descriptions may need notes to explain what happens when energy crosses an airwall boundary.

## Engineering Reference ##

TBD

## Example File and Transition Changes ##

One or more example files will be developed which use airwalls in various ways.

## References ##

None.

## Design ##

The overall plan is to tackle radiant exchange first, then solar/daylighting, then air exchange.

### Surface Class ###
Anywhere that `Surface(n).Class` is used will be evaluated to add a case or if block for 
the new `SurfaceClass_AirWall`.

### SuperZones ###
For the superzone concept, a second zone number field will be added to the surface data
structure(s). For solar, daylighting, and radiant exchange, the alternate superzone number will
be used to determine which surfaces to use and how they interact.

Will this work? Probably not initially. There are places that assume all of the surfaces in a
zone are adjacent to each other in the `Surface` data structure. e.g.

```
        for (ISurf = Zone(ZoneNum).SurfaceFirst; ISurf <= Zone(ZoneNum).SurfaceLast; ++ISurf) {
```

This example is from daylighting, so it will need to be changed. Other examples of this usage occur
in convection coefficients and other places where the current method will not be impacted. 
This can be addressed by using zone-level lists of surface numbers (which is already being 
developed in a separate effort).

### Reporting ###
Various zone-level outputs will be reviewed to determine the proper accounting of 
solar and radiant gains. Reporting at the superzone level has been considered, but if at all 
possible, it would be preferable to keep reporting at the original zone level only.
