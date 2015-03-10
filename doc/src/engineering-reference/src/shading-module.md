# Shading Module

## Shading and Sunlit Area Calculations

When assessing heat gains in buildings due to solar radiation, it is necessary to know how much of each part of the building is shaded and how much is in direct sunlight. As an example, the figure below shows a flat roofed, L-shaped structure with a window in each of the visible sides. The sun is to the right so that walls 1 and 3 and windows a and c are completely shaded, and wall 4 and window d are completely sunlit. Wall 2 and window b are partially shaded. The sunlit area of each surface changes as the position of the sun changes during the day. The purpose of the EnergyPlus shadow algorithm is to compute these **sunlit areas**.  Predecessors to the EnergyPlus shadowing concepts include the BLAST and TARP shadowing algorithms.

The shadow algorithm is based on coordinate transformation methods similar to Groth and Lokmanhekim and the shadow overlap method of Walton.

Using the **ShadowCalculation** object, you can set how often the shadowing calculations are performed. Having them calculated each day is obviously the most accurate but may also be the most time consuming.  Using a greater length of time (number of days) before calculating again can yield speedier results.  For lengths of time greater than one day, the solar position values (e.g. equation of time, sun position angles) are averaged over that time period for the shadowing calculations.

![Overall Shadowing Scheme Depiction](media/overall-shadowing-scheme-depiction.jpeg)


## Solar Position

Current solar position is described in terms of three direction cosines that are convenient for determining the angle of incidence of the sun's rays on a building surface.  The following procedure is used to determine the direction cosines.  The values of the solar declination angle, , and the equation of time, , are based on *Astronomical Algorithms*, Meeus.  Solar declination is a function of local/site latitude.

The fractional year is calculated, in radians:

![](media/image581.png)\


From this fractional year, the equation of time and solar declination angle are calculated.  For each time step (time value = fractional hour), the hour angle is calculated from:

![](media/image582.png)\


TimeZoneMeridian is the standard meridian for the location's time zone {GMT +/-}.

Solar HourAngle (*H*) gives the apparent solar time for the current time period (degrees); HourAngle is positive before noon, negative after noon.  It is common astronomical practice to express the hour angle in hours, minutes and seconds of time rather than in degrees.  You can convert the hour angle displayed from EnergyPlus to time by dividing by 15.  (Note that 1 hour is equivalent to 15 degrees; 360 of the Earth's rotation takes place every 24 hours.)  The relationship of angles in degrees to time is shown in the following table:

Table: Relationship of Angles (degrees) to Time

**Unit of Angle**|**Equivalent time**
------------------------------|--------------------------------
1 radian|3.819719 hours
1 degree|4 minutes
1 arcmin|4 seconds
1 arcsec|0.066667 seconds

The Solar Altitude Angle () is the angle of the sun above the horizontal (degrees).  The Solar Azimuth Angle () is measured from the North (clockwise) and is expressed in degrees.  This is shown more clearly in the following figure.

![Solar Position Illustration](media/solar-position-illustration.jpeg)


## Surface Geometry

Shadow calculations first require that the building surfaces be described geometrically. Surfaces are described by the coordinates of their vertices in a three dimensional Cartesian coordinate system. This Right-hand coordinate system has the X-axis pointing east, the Y-axis pointing north, and the Z-axis pointing up (see figure below). The azimuth angle () of a surface is the angle from the north axis to the projection onto the X-Y plane of a normal to the surface (clockwise positive). The surface tilt angle () is the angle between the Z-axis and the normal to the surface. The vertices are recorded in counter-clockwise sequence (as the surface is viewed from outside its zone).

During surface entry, surfaces are checked for convex or non-convex shape. If non-convex and inappropriate (used as a receiving surface) then a severe error is produced telling the user that shadowing calculations may be inaccurate.

Similarly collinear points (or as noted below, points within 1 mm distance) are removed unless removing would make an illegal surface (less than 3 points). But degenerate collinear surfaces *should* be removed – they make the shadowing routines do extra work which takes extra time.

Collinear – points that essentially form a "line" rather than a surface shape.

Resolution of 1mm or less – near collinear points.

> Note that the resolution on surfaces/shadowing is 1 mm – using resolution beyond that will result in truncation of the shadowing.

![EnergyPlus Coordinate System](media/energyplus-coordinate-system.png)


The GlobalGeometryRules object specifies to EnergyPlus how the surface vertices will be presented in the input file.  Of pertinent interest here is that the user may specify the vertices in either "relative" or "world" coordinates.  Regardless of input specifications, when vertices are reported, they are reported in world coordinates, starting at the upper-left-corner (4-sided surface) and are listed counter-clockwise.

### Relative Coordinate Transformation

When vertices are specified in "relative" coordinates, there can be a "building" north axis as well as a "zone" north axis.  The building north axis/coordinate system is a rotation of ~b~ degrees from the global/world coordinate system.  The global coordinates of zone origins are related to the building relative coordinates by:

![](media/image585.png)\


![](media/image586.png)\


![](media/image587.png)\


Where

zo – represents Zone Origin

br – represents the Zone Origin as input (relative to building origin)

The zone may also be rotated ~z~ degrees relative to the building coordinates. Origins of zone surfaces are then given relative to the zone coordinate system. The global coordinates of the surface origins are calculated by:

![](media/image588.png)\


![](media/image589.png)\


![](media/image590.png)\


A surface azimuth angle relative to the zone coordinate system (~s~) is converted to a global azimuth by:

![](media/image591.png)\


The surface tilt angle () is not changed by these rotations about the Z-axis.

The coordinates of the surface vertices are given in a coordinate system in the plane of the surface relative to the second vertex as shown for surfaces in Figure 40. The X-axis of the surface coordinate system is a horizontal line through the second vertex. The global coordinates of the surface vertices are given by:

![](media/image592.png)\


![](media/image593.png)\


![](media/image594.png)\


### World Coordinates  Relative Coordinates

Vertices in the global coordinate system can be transformed to the coordinate system relative to a given surface by

![](media/image595.png)\


![](media/image596.png)\


![](media/image597.png)\


![](media/image598.png)\


![](media/image599.png)\


![](media/image600.png)\


## Shadow Projection

All architectural forms are represented by plane polygons. This can give good accuracy even for curved surfaces: a sphere can be approximated by the 20 nodes of an icosahedron with only 3 percent error in the shadow area cast by the sphere. Consider how a solid object, which is composed of a set of enclosing plane polygons, casts a shadow. Figure 41 shows a box shaped structure on a horizontal surface. The structure consists of a top (surface 1) and four vertical surfaces (2 and 3 visible to the observer and 4 and 5 not visible). The sun is positioned behind and to the right of the structure and a shadow is cast onto the horizontal surface (the ground).

Surfaces 1, 4, and 5 are in sunlight; 2 and 3 are in shade. It is possible to think of the structure's shadow as the combination of shadows cast by surfaces 1, 2, 3, 4 and 5 or by 1, 4 and 5, or by surfaces 2 and 3.  This last combination of shadow casting surfaces is the simplest. In the EnergyPlus shadow algorithm every surface is considered to be one of the surfaces that enclose a solid, and only those surfaces that are not sunlit at a given hour are considered shadowing surfaces.

![Basic shadowing concept structure](media/basic-shadowing-concept-structure.jpeg)


The expressions in equation  are the direction cosines of the surface:

![](media/image602.png)\


![](media/image603.png)\


![](media/image604.png)\


The cosine of the angle of incidence of the sun's rays on the surface are given by the dot product of surface and sun direction cosines.

![](media/image605.png)\


If ![](media/image606.png)  is less than zero, the sun is behind the surface.

A shadow is projected from the vertices of the shadowing polygon (SP) along the direction of the sun's rays to the plane of the shadow receiving polygon (RP).  If any vertices of the SP are below the plane of the RP (z < 0), a false shadow is cast as in Figure 42.  The "submerged" portion of the SP must be clipped off before projection.

![Illustration of Shadow Clipping](media/illustration-of-shadow-clipping.jpeg)


This is done by finding, through linear interpolation, the points on the perimeter of the SP, which intersect the plane of the RP. These points become new vertices of the SP, which together with the other positive vertices define a clipped SP that casts only a real shadow.

A vertex located at (x, y, z) relative to the RP coordinate system casts a shadow to a point in the plane of the RP given by

![](media/image608.png)\


![](media/image609.png)\


where

![](media/image610.png)\


and

![](media/image611.png)\


More explicitly, a casting surface – a shadow casting surface or general casting surface – is one that casts a shadow on other surfaces. A receiving surface – a shadow receiving surface – is one that receives shadows from other surfaces (i.e. casting surfaces). A back surface – an inside surface – is one that may be partially sunlit/receive solar transmission for interior solar distribution.

## Homogeneous Coordinates

Two-dimensional homogeneous coordinate techniques are used to determine the vertices of shadow overlaps. In homogeneous coordinates, points and lines are represented by a single form that allows simple vector operations between those forms [Newman-Sproul].  A point (X, Y) is represented by a three element vector (x, y, w) where x = w\*X, y = w\*Y, and w is any real number except zero. A line is also represented by a three element vector (a, b, c). The directed line (a, b, c) from point (x~1~, y~1~, w~1~) to point (x~2~, y~2~, w~2~) is given by:

![](media/image612.png)\


The sequence in the cross product is a convention to determine sign. The condition that a point (x, y, w) lie on a line (a, b, c) is that

![](media/image613.png)\


The point is normalized by dividing by w. Then if

![](media/image614.png)\


the point is to the left of the line.  If it is less than zero, the point is to the right of the line. The intercept (x, y, w) of line (a~1~, b~1~, c~1~) and line (a~2~, b~2~, c~2~) is given by:

![](media/image615.png)\


Note that the use of homogeneous coordinates as outlined above provides a consistent method and notation for defining points and lines, for determining intercepts, and for determining whether a point lies to the left, to the right, or on a line.  Normalization provides the means for transforming to and from homogeneous notation and Cartesian coordinates.  Thus, if (X, Y) is a Cartesian coordinate pair, its homogeneous coordinates are (X, Y, 1).  Similarly, the homogeneous coordinates (x, y, w) can be transformed to the Cartesian point with coordinates (x/w, y/w).

## Polygon Clipping Algorithms

Two methods for polygon clipping (treating of overlapping shadows) are currently in use in EnergyPlus.

- Convex Weiler - Atherton
- Sutherland – Hodgman

The original EnergyPlus method for polygon clipping is a special version of the Weiler-Atherton model (Weiler, Atherton, 1977). It was developed to be sufficiently general to clip concave polygons with holes. The implementation in the current version of EnergyPlus, however, does not support concave shadowing surfaces or holes. The relative computational complexity is preserved – the algorithm is carried out in four steps. For example, if A and B are polygons (see Figure 43).

#. A call to INCLOS determines which vertices of X lie within Y.
#. A second call determines which vertices of Y lie within X.
#. If neither polygon is contained completely within the other, INTCPT is called to collect points of intersection between X and Y.
#. Since the points are usually gathered out of order, they must then be oriented.

The Sutherland-Hodgman algorithm (Sutherland, Hodgman, 1974) is less complex compared to the Weiler-Atherton method and is well-suited to clipping convex polygons. In actuality, only convex shading surfaces are currently supported by EnergyPlus. Let X be a polygon called the "subject polygon" (SP) and Y be a polygon called the "clipping polygon" (CP). The method performs the computation by iterating over the edges of the CP and removing points from the SP that fall in the clipping plane, i.e. points that fall to the left of the edge of the CP. Intersections between the clip edge and the edges of the SP are added appropriately, and points falling outside of the clipping plane, i.e. to the right of the edge of the CP, are added the output polygon as well. This resultant polygon is stored and the process is repeated for the rest of the clip edges in CP. The process is analogous to cutting off pieces of the SP one-by-one with respect to each edge of the CP. The result is ordered and identical to the polygon produced by the Weiler-Atherton method.

## Overlapping Shadows

After transforming the shadows onto the plane of the receiving surface, the basic job of the shadow algorithm is to determine the area of the overlap between the polygons representing the shadows and the polygon representing the receiving surface. Concave surfaces are supported only for exterior wall heat transfer surfaces, when using SutherlandHodgman option. Concave shading devices are not supported by the this option. Neither concave shading devices nor concave exterior wall heat transfer surfaces are supported by the ConvexWeilerAtherton clipping routine.

When only convex shading devices are considered, this provides a great simplification. The overlap between two convex polygons (i.e. projections of shading devices via the direction of the sun) is another convex polygon. Coordinate and projection transformations of a convex polygon produce another convex polygon. Any non-convex polygon can be constructed as the union of convex ones.

For ConvexWeilerAtherton, there is considerable simplification if only convex (no interior angle > 180 ) polygons are considered. The overlap between two convex polygons is another convex polygon. Coordinate and projection transformations of a convex polygon produce another convex polygon. Any non-convex polygon can be constructed as a sum of convex ones.

The vertices that define the overlap between two convex polygons, A and B, consist of:

- the vertices of A enclosed by B
- the vertices of B enclosed by A
- and the intercepts of the sides of A with the sides of B

In Figure 43, point a is the result of rule 1, point c is the result of rule 2, and points b and d result from rule 3. The overlap of A and B is the polygon a-b-c-d. Figure 44 shows an overlap where all of the vertices of B are enclosed by A.  Figure 45 shows an overlap defined only by the intercepts of A and B. Figure 46 shows a more complex overlap.

![Point a – Vertex of A Enclosed by B](media/point-a-vertex-of-a-enclosed-by-b.jpeg)


Coordinate transformation retains the order of the vertices of a polygon, while a projection reverses the order. The sequence of vertices of the receiving polygons should be reversed so it and all shadow polygons will have the same sequence.

![Surface A Totally Overlaps Surface B.](media/surface-a-totally-overlaps-surface-b..jpeg)


A point is enclosed by a clockwise, convex polygon if the point lies to the right of all sides (or does not lie to the left of any side) of the polygon.  The intercept of two sides may not lie beyond the ends of either side. These are "line segments" rather than "lines". It is possible to tell if line segments A and B intercept within their end points by noting that the ends of A must lie on both sides of B, and the ends of B must lie on both sides of A. This should be done before the intercept is calculated.

![Figure Formed from Intercept Overlaps Between A and B](media/figure-formed-from-intercept-overlaps-between.jpeg)


Once the vertices are determined, they must be sorted into clockwise order for the area to be computed.  Given a closed, planar polygon of n sequential vertices (x~1~, y~1~), (x~2~, y~2~) …, (x~n~, y~n~), its **area** is given:

![](media/image619.png)\


where (x~n+1~,y~n+1~)= (x~1~, y~1~)

The area is positive if the vertices are counter-clockwise and negative if they are clockwise.

![Complex Overlapping Condition](media/complex-overlapping-condition.jpeg)


If two shadows overlap the receiving surface, they may also overlap each other as in Figure 47. The vertices of this overlap can be computed.  The areas of all overlaps can be computed.  The total sunlit area can be expressed as the sum of all polygon areas given a proper sign on each of the areas.

The following convention was adopted:

Table: Surface / Area Characteristic / Convention

Surface Characteristic|Area Convention
----------------------|---------------
receiving surface|positive (A)
overlap between shadow and receiving|negative (B & C)
overlap between two shadows|positive (D)

and so on through multiple overlaps where the sign of the overlap area is the product of the signs of the overlapping areas.

![Multiple Shadow Overlaps](media/multiple-shadow-overlaps.jpeg)


Partially transparent shadowing surfaces can also be modeled by giving a transparency () to every shadowing polygon. Let  of the receiving polygon be one. Then the  of every overlap of polygons i and j is the product of ~i~ and ~j~ The shaded area is then computed by summing A~i~(1 - ~i~) for all overlap polygons.

It is easy to determine the sunlit area of a window once all the shadow and overlap vertices on the wall have been computed. Consider wall 2 of Figure 38. First, the wall is considered a simple rectangle and the window on it is ignored. The shadow overlapping is performed and the sunlit portion of the gross wall area is computed. Then the window rectangle is overlapped with the shadow to determine its sunlit area. The sunlit area of the window is subtracted from the gross wall sunlit area to determine the net wall sunlit area. During this calculation it is not necessary to recompute the shadows, because they were precisely determined on the wall.

When the SutherlandHodgman option is selected, the overlap is computed using the Sutherland-Hodgman algorithm for polygon clipping when. Let X be a polygon called the "subject polygon" (SP) and Y be a polygon called the "clipping polygon" (CP). The method performs the computation by iterating over the edges of the CP and removing points from the SP that fall in the clipping plane, i.e. points that fall to the left of the edge of the CP. If it is to the left of any edge, it the point does not overlap with the CP. Intersections between the clip edge and the edges of the SP are added appropriately, and points falling outside of the clipping plane, i.e. to the right of the edge of the CP, are added the output polygon as well. This resultant polygon is stored and the process is repeated for the rest of the clip edges in CP. The process is analogous to cutting off pieces of the SP one-by-one with respect to each edge of the CP. Note that the SP may be concave, but the CP may not. This means that the exterior wall surfaces may be concave, while shading devices may not be concave.

## Solar Gains

The total solar gain on any exterior surface is a combination of the absorption of direct and diffuse solar radiation given by

![](media/image622.png)\


where

a =solar absorptance of the surface

A =angle of incidence of the sun's rays

S =area of the surface

S~s~ = sunlit area

I~b~ =intensity of beam (direct) radiation

I~s~ =intensity of sky diffuse radiation

I~g~ =intensity of ground reflected diffuse radiation

F~ss~ = angle factor between the surface and the sky

F~sg~ = angle factor between the surface and the ground

For the surface of a building located on a featureless plain

![](media/image623.png)\


and

![](media/image624.png)\


If the surface is shaded the program modifies *F~ss~* by a correction factor that takes into account the radiance distribution of the sky (see "Shadowing of Sky Diffuse Solar Radiation").

Shading of ground diffuse solar radiation is not calculated by the program. It is up to the user to estimate the effect of this shading and modify the input value of *F~sg~*accordingly.

## Solar Distribution

As discussed in the Input Output Reference (Object: Building), the field Solar Distribution, in the "Building" input object, determines how EnergyPlus will treat beam solar radiation entering a zone through exterior windows.  There are five choices: **MinimalShadowing**, **FullExterior**, **FullInteriorAndExterior, FullExteriorWithReflections**, and **FullInteriorAndExteriorWithReflections**.

### MinimalShadowing

In this case, there is no exterior shadowing except from window and door reveals. All beam solar radiation entering the zone is assumed to fall on the floor, where it is absorbed according to the floor's solar absorptance. Any reflected by the floor is added to the transmitted diffuse radiation, which is assumed to be uniformly distributed on all interior surfaces. If no floor is present in the zone, the incident beam solar radiation is absorbed on all interior surfaces according to their absorptances. The zone heat balance is then applied at each surface and on the zone's air with the absorbed radiation being treated as a flux on the surface.

### FullExterior 

In this case, shadow patterns on exterior surfaces caused by detached shading, wings, overhangs, and exterior surfaces of all zones are computed. As for MinimalShadowing, shadowing by window and door reveals is also calculated. Beam solar radiation entering the zone is treated as for MinimalShadowing.

### FullExteriorWithReflections

This case is the same interior distribution as the preceding option but uses exterior reflections as well (see the section Solar Radiation Reflected from Exterior Surfaces for further explanation).

### FullInteriorAndExterior 

This is the same as FullExterior except that instead of assuming all transmitted beam solar falls on the floor the program calculates the amount of beam radiation falling on each surface in the zone, including floor, walls and windows, by projecting the sun's rays through the exterior windows, taking into account the effect of exterior shadowing surfaces and window shading devices.

If this option is used, you should be sure that the surfaces of the zone totally enclose a space. This can be determined by viewing the **eplusout.dxf** file with a program like AutoDesk's Volo View Express. You should also be sure that the zone is **convex**. Examples of convex and non-convex zones are shown in Figure 48. The most common non-convex zone is an L-shaped zone. (A formal definition of convex is that any straight line passing through the zone intercepts at most two surfaces.)  If the zone's surfaces do not enclose a space or if the zone is not convex you should use Solar Distribution = **FullExterior** instead of **FullInteriorAndExterior**.

If you use **FullInteriorAndExterior** the program will calculate how much beam radiation falling on an interior window is absorbed by the window, how much is reflected back into the zone, and how much is transmitted into the adjacent zone. (Interior windows are assumed to have no shading device).

If you use **FullInteriorAndExterior** the program will also calculate how much beam radiation falling on the inside of an exterior window (from other windows in the zone) is absorbed by the window, how much is reflected back into the zone, and how much is transmitted to the outside. In this calculation the effect of an interior or exterior shading device, if present, is accounted for.

### FulInteriorAndlExteriorWithReflections

This case is the same interior distribution as the preceding option but uses exterior reflections as well (see Solar Radiation Reflected from Exterior Surfaces for further explanation).

![Illustration of Convex and Non-convex Zones](media/illustration-of-convex-and-non-convex-zones.png)


## Details of the Interior Solar Distribution Calculation

EnergyPlus calculates the distribution of short-wave radiation in the interior of each thermal zone. This radiation consists of beam solar radiation, diffuse solar radiation, and short-wave radiation from electric lights. The program determines the amount of this radiation that is (1) absorbed on the inside face of opaque surfaces, (2) absorbed in the glass and shading device layers of the zone's exterior and interior windows, (3) transmitted through the zone's interior windows to adjacent zones, and (4) transmitted back out of the exterior windows. The effects of movable shading devices on the exterior windows are taken into account; *the program does not allow shading devices on interior windows*. Most of this calculation is done in subroutine CalcInteriorSolarDistribution in the SolarShading module.

### Initial Distribution of Diffuse Solar Transmitted through Exterior and Interior Windows

Diffuse solar (from sky and ground sources) transmitted through exterior windows is first distributed to the interior heat transfer surfaces in the zone containing the exterior windows. This initial distribution apportions the transmitted diffuse solar to interior surfaces using the approximate view factors described above in "LW Radiation Exchange Among Zone Surfaces." The amount of this initially distributed diffuse solar absorbed by each interior surface, and each window material layer, is calculated and later added to the "short-wave radiation absorbed" values described below. The amount of this initially distributed diffuse solar that is reflected is accumulated for each zone and redistributed uniformly as part of the QD calculation described below. The amount of this initially distributed diffuse solar that is transmitted by interior windows to adjacent zones is initially distributed to the interior heat transfer surfaces in the adjacent zone in the same manner as just described.

This new treatment of diffuse solar is intended to more accurately account for the initial absorption, transmittance, and reflection of short-wave radiation prior to the uniform distribution described below.

### Interior Solar Radiation Absorbed by Opaque Surfaces

The short-wave radiation absorbed on the inside face of an opaque surface (floor, wall or ceiling) is given by

![](media/image626.png)\


where

*SurfNum* = surface number

*ZoneNum =* number of zone that surface belongs to

*QS(ZoneNum)* = short-wave diffuse irradiance in the zone [W/m^2^]

*AbsIntSurf(SurfNum)* = inside solar absorptance of the surface

*AISurf(SurfNum)* = inside beam solar irradiance factor for the surface [-]

*BeamSolarRad* = outside beam normal solar irradiance [W/m^2^]

#### Interior Diffuse Radiation

*QS* is assumed to be uniformly distributed throughout the zone. It is calculated as follows. Let *Q~sw~* be the total diffuse short-wave radiation entering the zone or originating in the zone. Since *Q~sw~* is ultimately absorbed or transmitted by zone heat transfer surfaces, summing over these surfaces gives the following energy balance equation:

![](media/image627.png)\


where

*i* = zone surface number counter

*N~surf~* = number of heat transfer surfaces in zone

*A~i~*~~= surface area [m^2^]

![](media/image628.png) = inside solar absorptance for an opaque surface, or, for a window, = back diffuse transmittance plus back diffuse system absorptance of glass layers and shading device layers (if present)

Solving this equation for *QS* gives:

![](media/image629.png)\


where

![](media/image630.png)\


*Q~sw~* is given by

 ![](media/image631.png)

where

*ZoneIntGain(ZoneNum)%QLTSW* = short-wave radiation into zone from general (overhead) electric lighting [W]

*ZoneIntGain(*ZoneNum*)%T_QLTSW* = short-wave radiation into zone from task electric lighting [W]

*QD(ZoneNum)* = diffuse solar radiation entering or originating in zone [W]

*QD(ZoneNum)* is given by:

![](media/image632.png)\


where

*BeamSolarRad* is the outside beam normal solar irradiance [W/m^2^]

*DBZone(ZoneNum)* is the diffuse solar radiation originating from beam solar that passes through the exterior windows in the zone and reflects diffusely from inside zone surfaces plus beam solar entering the zone as diffuse radiation from windows with shading devices or diffusing glass (all divided by BeamSolarRad)   [m^2^]

*InitialDifSolDistReflectedW(ZoneNum)* is the diffuse solar radiation originating from sky and sun related diffuse solar transmitted through the zone's exterior or interior windows into the zone, and reflected diffusely from inside zone surfaces.

*DBZone(ZoneNum)* is calculated as:

![](media/image633.png)\


where

*BTOTZone* = total beam solar incident on the zone's exterior windows that is transmitted as beam or diffuse.

*BABSZone =* total beam solar absorbed inside the zone.

*BTOTZone* is given by:

![](media/image634.png)\


*+ Diffuse entering zone from beam reflected by window inside reveal surfaces*

*+ Diffuse transmitted by windows from beam reflected by outside reveal surfaces*

*–  Beam absorbed by window inside reveal surfaces*

Here,

*TBmAll* = beam-to-beam plus beam-to-diffuse transmittance of window

*SunlitFract* = fraction of window irradiated by sun

*CosInc* = cosine of solar incidence angle on window

*Area* = glazed area of window [m^2^]

*InOutProjSLFracMult* = shadowing factor due to inside and outside projections of window frame and/or divider (= 1.0 if there is no frame or divider).

*BABSZone* is given by the following sum (see Figure 49):

*BABSZone* =  *Beam absorbed by opaque inside surfaces*

*+ Beam transmitted through the zone's interior windows +*

*+ Beam transmitted back out of the zone's exterior windows +*

*+ Beam absorbed by the zone's exterior and interior windows +*

*+ Beam absorbed by inside daylighting shelves*

![Vertical section through a two-zone building showing where transmitted beam solar falls. Some of the beam solar from exterior window EW is absorbed by the floor, D, interior wall, B, and interior window, IW. Some is transmitted by IW to the adjacent zone, Z2. Aoverlap is the irradiated area of a surface projected back onto the plane of EW. Beam reflected by D, B and IW contributes to the interior short-wave radiation flux in Z1.](media/vertical-section-through-a-two-zone-building.png)


If zone *ZoneNum* shares interior windows with other zones, *QS(ZoneNum)* is modified to take into account short-wave radiation received from the other zones through these windows:

![](media/image636.png)\


where

*FractDifShortZtoZ(OtherZoneNum,ZoneNum)* = "diffuse solar exchange factor" = fraction of short-wave radiation in *OtherZoneNum* that is transmitted to *ZoneNum*. This factor is calculated in subroutine ComputeDifSolExcZonesWIZWindows taking into account multiple reflection between zones. For example, for two zones means that some of the radiation transmitted from Zone1 to Zone2 is reflected back to Zone1, and some of this is in turn reflected back to Zone2, etc.

#### Interior Beam Radiation

The inside beam solar irradiance factor in  is given by:

![](media/image637.png) where

*i* = exterior window number

*N~extwin~ =* number of exterior windows in zone

*CosInc~i~* = cosine of angle of incidence of beam on exterior window *i*

*TBm~i~* = beam-to-beam transmittance of exterior window *i* at *CosInc~i~*~~

*Aoverlap~i~(SurfNum)* = beam solar irradiated area of surface *SurfNum* projected back onto the plane of exterior window *i* (the *Aoverlap*'s for an exterior window sum up to the glazed area of the window). These overlap areas (Figure 49) are determined with the EnergyPlus shadowing routines by considering a zone's exterior window as a "sending" surface and the inside faces of the zone's other surfaces as "receiving" surfaces (see "Shading Module"). The overlap areas for a particular exterior window depend on the sun position, the geometry of the window, the geometry of the interior surfaces, and the location of the window with respect to the interior surfaces.

*AbsIntSurf(SurfNum)* = inside face solar absorptance of surface *SurfNum*

*A(SurfNum)* = area of surface *SurfNum* [m^2^]

### Interior Solar Radiation Absorbed by Windows

The interior short-wave radiation absorbed by layer *l* (glass, shade or blind) of a window is equal to:

![](media/image638.png) where

![](media/image639.png) = the system diffuse solar absorptance of layer *l* for irradiance from the back side

![](media/image640.png) = the system beam solar absorptance of layer *l* for irradiance from the back side

*A(SurfNum)* = glazing area [m^2^]

Note that as of Version 2.1, the initially distributed diffuse solar absorbed by each surface (as described above under "Initial Distribution of Diffuse Solar Transmitted through Exterior and Interior Windows") is added to this uniformly distributed short-wave radiation.

### Interior Solar Radiation Transmitted by Interior Windows

#### Interior Diffuse Radiation Transmitted by Interior Windows

The interior diffuse short-wave radiation transmitted by an interior window to the adjacent zone is given by

![](media/image641.png)\


where

![](media/image642.png) = diffuse transmittance of the interior window

#### Interior Beam Radiation Transmitted by Interior Windows

The interior beam solar radiation transmitted by an interior window to the adjacent zone is

![](media/image643.png)\


where ![](media/image644.png) is the beam-to-beam transmittance of the interior window at the angle of incidence of beam solar from the exterior window on the interior window. The program does not track where this radiation falls in the adjacent zone: it is counted as diffuse radiation in that zone. Therefore,

![](media/image645.png)\


## Ground Reflectances

Ground reflectance values (Ref Object: Site:GroundReflectance) are used to calculate the ground reflected solar amount.  This fractional amount (entered monthly) is used in the following equation:

![](media/image646.png)\


Of course, the Ground Reflected Solar is never allowed to be negative.  The Snow Ground Reflectance Modifier can further modify the ground reflectance when snow is on the ground.  If the user enters 0.0 for each month, no ground reflected solar is used.

## Ground Reflectances (Snow)

When snow is on the ground, ground reflectances may change.  (Ref Object: Site:GroundReflectance:SnowModifier).  This object allows the user to specify two values, Ground Reflected Solar Modifier and Daylighting Ground Reflected Solar Modifier.

Ground Reflected Solar Modifier is used to modified the basic monthly ground reflectance when snow is on the ground (from design day input or weather data values). Values can range from 0.0 to 1.0.

![](media/image647.png)\


Daylighting Ground Reflected Solar Modifier is used to modified the basic monthly ground reflectance when snow is on the ground (from design day input or weather data values). Values can range from 0.0 to 1.0.

![](media/image648.png)\


## References

ASHRAE. 2005. Handbook of Fundamentals, Chapter 31, Atlanta: ASHRAE.

ASHRAE. 2007. HVAC Applications, Chapter 33, Atlanta, ASHRAE.

Zhang, Qingyuan, Joe Huang, and Siwei Lang. 2002. "Development of Typical Year Weather Data for Chinese Locations", American Society of Heating Refrigeration and Air-Conditioning Engineers, ASHRAE Transactions, Vol 108, Part 2.

Threlkeld, J.L. and R.C. Jordan. 1958. Direct solar radiation available on clear days. ASHRAE Transactions 64:45.

Groth, C. C., and Lokmanhekim, M. 1969. "Shadow  A New Technique for the Calculation of Shadow Shapes and Areas by Digital Computer," Second Hawaii International Conference on System Sciences, Honolulu, HI, January 2224, 1969.

Walton, G.N. 1983. "The Thermal Analysis Research Program Reference Manual Program (TARP)", National Bureau of Standards (now National Institute of Standards and Technology).

Walton, G. N. 1978. "The Application of Homogeneous Coordinates to Shadowing Calculations", American Society of Heating Refrigeration and Air-Conditioning Engineers, ASHRAE Transactions, Vol 84, Part I.

Meeus, Jean. 2000. Astronomical Algorithms, Willmann-Bell.

Newman, M. W., and Sproul, R. F. 1973. Principles of Interactive Computer Graphics, McGrawHill.

Polygon area derived from Green's Theorem.  Graphic Gems repository.

Weiler, Kevin, Atherton, Peter. "Hidden Surface Removal Using Polygon Area Sorting." Program of Computer Graphics, Cornell University. Ithaca, NY: 1977. http://www.cs.drexel.edu/~david/Classes/CS430/HWs/p214-weiler.pdf

Sutherland, I.E., and Hodgman, G.W. 1974. "Reentrant Polygon Clipping", Communication of Association for Computing Machinery (CACM), vol. 17, pp. 32-42.

Maillot,Patrick-Gilles. **"**A New, Fast Method For 2D Polygon Clipping: Analysis and Software Implementation." Sun Microsystems, inc. Mountain View, CA: 1992. http://pmaillot.chez.com/2dpclip.pdf

Wisstein, Eric W. "Convex Polygon" From Mathworld- A Wolfram Web Resource. http://mathworld.wolfram.com/ConvexPolygon.html