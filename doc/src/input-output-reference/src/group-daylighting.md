# Group – Daylighting

Daylighting can be invoked in EnergyPlus by the use of two objects: **Daylighting:Controls** and **Daylighting:DELight:Controls**. These are described in the following object descriptions.

> Note that [Daylighting:Controls](#daylightingcontrols) and [Daylighting:DELight:Controls](#daylightingdelightcontrols) objects may be intermixed in a single IDF but may not be used in the same zone.

## Daylighting:Controls

In this method daylighting illuminance levels are calculated and then used to determine how much the electric lighting can be reduced. The daylight illuminance level in a zone depends on many factors, including sky condition; sun position; calculation point; location, size, and glass transmittance of windows; window shading devices; and reflectance of interior surfaces. Reduction of electric lighting depends on daylight illuminance level, illuminance set point, fraction of zone controlled and type of lighting control.

### Inputs

#### Field: Zone name

The name of the zone to which the following daylighting-related input applies.

#### Field: Total Daylighting Reference Points

*Allowed values are 1 or 2. This the number of reference points in the zone at which horizontal daylighting illuminance will be calculated based on input for the following fields. It is assumed that the photocells that control the overhead electric lighting respond to the light levels at the specified reference points.*

#### Fields: (X,Y,Z) of First Reference Point

Required if Total Daylighting Reference Points = 1 or 2. These three fields are the X, Y and Z values of the First Reference point in the coordinate system you specified for daylighting reference points in the [GlobalGeometryRules](#globalgeometryrules) object. Figure 53 shows an example using the relative coordinate (to zone) system. Z is typically at desk height (0.8 m).

If you want to divide a thermal zone into two independently-controlled lighting zones, then Total Daylighting Reference Points should be set to 2 and (X,Y,Z) of the Second Reference Point (see following input fields) should also be specified.

#### Fields: (X,Y,Z) of Second Reference Point

The X, Y, and Z coordinates, in the same coordinate system, of the second daylighting reference point. Required if Total Daylighting Reference Points = 2. See Figure 53 for an example.

![Example showing location of daylighting reference points in the zone coordinate system (relative) of a rectangular zone with three windows. (a) Perspective view, (b) plan view, (c) elevation view. All dimensions are in meters.](media/example-showing-location-of-daylighting.png)


#### Field: Fraction of Zone Controlled by First Reference Point

The fraction of the zone's floor-area whose electric lighting is controlled by the daylight illuminance at the First Reference Point. If there is only one reference point then a fraction equal to

- (Fraction of [Zone](#zone) Controlled by First Reference Point) 

 is assumed to have no lighting control.

#### Field: Fraction of Zone Controlled by Second Reference Point

The fraction of the zone's floor-area whose electric lighting is controlled by the daylight illuminance at the Second Reference Point. Required if Total Daylighting Reference Points = 2. A fraction equal to

1.0 – ([Fraction of [Zone](#zone) Controlled by First Reference Point] + [Fraction of [Zone](#zone) Controlled by Second Reference Point])

is assumed to have no lighting control.

#### Field: Illuminance SetPoint at First Reference Point

The desired lighting level (in lux) at the First Reference Point. This is the lighting level that would be produced at this reference point at night if the overhead electric lighting were operating at full input power. Recommended values depend on type of activity; they may be found, for example, in the Lighting Handbook of the Illuminating Engineering Society of North America. A typical value for general office work (excluding computer terminals) is 500 lux.

#### Field: Illuminance SetPoint at Second Reference Point

The desired lighting level (in lux) at the Second Reference Point. Required if Total Daylighting Reference Points = 2. This is the lighting level that would be produced at this reference point at night if the overhead electric lighting were operating at full input power.

#### Field: Lighting Control Type

The type of overhead electric lighting control. All reference points specified are assumed to have this type of control.

For Lighting Control Type = 1 (continuous), the overhead lights dim continuously and linearly from (maximum electric power, maximum light output) to (minimum electric power, minimum light output) as the daylight illuminance increases. The lights stay on at the minimum point with further increase in the daylight illuminance.

For Lighting Control Type = 2 (stepped), the electric power input and light output vary in discrete, equally spaced steps. The number of steps is given by Number of Steps (Excluding Off) of Stepped Control. For example, if Number of Steps = 3 and Illuminance Setpoint = 600, then the following table shows the fraction of the lights that are on vs. daylight illuminance.

Table: Stepped Lighting Control Example

**Example of a Stepped Lighting Control System with Three Steps**
------------------------------------------------------------------------------
**Daylight illuminance**|**Fraction of lights that are on**
0-200|1.0
200-400|2/3
400-600|1/3
600 and above|0.0

Lighting Control Type = 3 (continuous/off) is the same as Lighting Control Type = 1 except that the lights switch off completely when the minimum dimming point is reached.

#### Field: Glare Calculation Azimuth Angle of View Direction Clockwise from Zone y-Axis

Daylight glare from a window depends on occupant view direction. It is highest when you look directly at a window and decreases as you look away from a window. This field specifies the view direction for calculating glare. It is the angle, measured clockwise in the horizontal plane, between the zone y-axis and the occupant view direction.

#### Field: Maximum Allowable Discomfort Glare Index

If a daylit zone has windows with shading devices (except exterior screens), the shades will be deployed if the daylight glare at the First Reference Point exceeds the value of this field. To get this type of glare control you have to specify Trigger = Glare, GlareOrSolarOnWindow, GlareOrHorizontalSolar, GlareOrOutsideAirTemp, GlareOrZoneAirTemp or GlareOrZoneLoad in [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) for one or more windows in the zone (see [WindowProperty:ShadingControl](#windowpropertyshadingcontrol)).

If a zone has two or more windows with glare control, the shading devices will be deployed one by one in the order in which the windows are input until the glare level at each reference point falls below Maximum Allowable Discomfort Glare Index or is as close as possible to it.

The following table gives recommended values of Maximum Allowable Discomfort Glare Index.

Table: Recommended Values -- Discomfort Glare Index

Recommended Values of Maximum Allowable Discomfort Glare Index
--------------------------------------------------------------
Activity or zone type|Maximum Allowable Discomfort Glare Index
---------------------|----------------------------------------
Art Galleries|16
*Factories*|
  Rough work|28
  Engine assembly|26
  Fine assembly|24
  Instrument assembly|22
Hospital wards|18
Laboratories|22
Museums|20
Offices|22
School classrooms|20

#### Field: Minimum Input Power Fraction for Continuous Dimming Control

For Lighting Control Type = 1 (continuous), the lowest power the lighting system can dim down to, expressed as a fraction of maximum input power (see figure, below). For Lighting Control Type = 3 (continuous/off) this is the power fraction reached just before the lights switch off completely.

![Illustration of continuous dimming relationship](media/illustration-of-continuous-dimming.png)


The figure above shows the relationship between electric light output and electrical input.

#### Field: Minimum Light Output Fraction for Continuous Dimming Control

For Lighting Control Type = 1 (continuous), the lowest lighting output the lighting system can dim down to, expressed as a fraction of maximum light output (see figure, above). This is the fractional light output that the system produces at minimum input power. For Lighting Control Type = 3 (continuous/off) this is the light output fraction reached just before the lights switch off completely.

#### Field: Number of Stepped Control Steps

The number of steps, excluding off, in a stepped lighting control system (see figure, below). Required and must be >0 if Lighting Control Type = 2. The steps are assumed to be equally spaced.

![Stepped lighting control with Number of Steps = 3.](media/stepped-lighting-control-with-number-of-steps.png)


#### Field: Probability Lighting will be Reset When Needed in Manual Stepped Control

May be specified if a stepped lighting control system (Lighting Control Type = 2) is manually operated, such as in a simple, one-step (on-off) system. Gives the probability the occupants of a daylit zone will set the electric lights to the correct level to obtain the required illuminance. The rest of the time the lights are assumed to be set one step too high. For example, consider an on-off lighting system (Number of Steps = 1) with a set point of 600 lux and 0.7 reset probability. Then, when daylighting exceeds 600 lux, the electric lights will be off 70% of the time and on 30% of the time.

#### Field: Availability Schedule Name

This alpha field is optional and may be used to define the name of a schedule that denotes whether or not the control is available to operate during a given time period. If the schedule value is greater than zero, then the controls will be available to operate for that time period.  If the schedule value is zero or less, then the controls will not be available. If this field is left blank, the controls will be always available. For example, this can be useful for scheduling the daylighting controls off during design days so that the daylighting controls do not affect lighting power during sizing.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Daylighting:Controls, ! Zone Name
        1,                ! Total Daylighting Reference Points
        3.048,3.048,0.9,  ! X,Y,Z coordinates of first reference point {m}
        0.0,0.0,0.0,      ! X,Y,Z coordinates of second reference point, if present {m}
        0.99,             ! Fraction of zone controlled by first reference point
        0.0,              ! Fraction of zone controlled by second reference point, if present
        500.,             ! Illuminance setpoint at first reference point {lux}
        500.,             ! Illuminance setpoint at second reference point, if present {lux}
        1,                ! Lighting control type {1=continuous,2=stepped,3=continuous/off}
        180.0,            ! Azimuth angle of view direction for glare calculation {deg},
                          !   measured clockwise from zone y-axis. Value of 180 gives view
                          !   (from reference point) that is directly at window, maximizing glare.
        20.0,             ! Maximum discomfort glare index for window shade control
        0.3,              ! Minimum input power fraction for continuous dimming control
        0.2,              ! Minimum light output fraction for continuous dimming control
        0,                ! Number of steps, excluding off, for stepped control
        1.0;              ! Probability electric lighting will be reset when needed (used only
                          !   for stepped control, to simulate manual on/off control)
~~~~~~~~~~~~~~~~~~~~

## Guidelines for Daylighting Modeling (Detailed Method)

Following are some guidelines for preparing EnergyPlus input to model the effects of daylighting. Before studying these guidelines, however, you should read the description of each input field under [Daylighting:Controls](#daylightingcontrols), and review the IDF example, above, and the sample daylighting input, PurchAirWithDaylighting.idf.

### Use of Window Multipliers

If an exterior wall in a daylit zone has a number of identical windows, the windows should be entered separately rather than using a window multiplier (ref: [FenestrationSurface:Detailed](#fenestrationsurfacedetailed)). Using a multiplier would give an incorrect illuminance calculation since individual windows would not be positioned correctly on the wall.

### Use of Zone Multipliers

A zone multiplier should not be used on a daylit zone if the windows in the zone are shadowed by exterior obstructions like trees or neighboring buildings, or by obstructions caused by other parts of building. The reason for this is that the shadowing on the windows of the multiplied zone may be different from zone to zone depending on exactly how the shadows fall on the zones. However, a zone multiplier may be used on a daylit zone if the shadowing is by overhangs, fins and/or window setback.

### Thermal Zoning for Daylighting

To correctly calculate both direct and inter-reflected daylight illuminance you should try to model a thermal zone consisting of several similar rooms separated by interior walls as a representative room with a zone multiplier (ref: [Zone](#zone)). An example of this is shown in Figure 56. Room-1 is the representative room, with a zone multiplier = 4.

Interior walls IW-1, IW-2 and IW-3 should be treated as adiabatic, i.e., they should have Outside Boundary Condition = Surface and Outside Boundary Condition Object = IW-1, IW-2, or IW-3, respectively (ref: [BuildingSurface:Detailed](#buildingsurfacedetailed)). Similarly, if the ceiling and floor of Room-1 are interior surfaces, they should be treated as adiabatic.

![For daylighting purposes the thermal zone enclosed by the dashed boundary line should be modeled as a typical zone (Room-1) with a zone multiplier of 4.](media/for-daylighting-purposes-the-thermal-zone.png)


Sometimes a representative room cannot be found. Figure 57 shows a section of a building with four rooms having different daylighting characteristics because of different floor area, orientation and/or window size. In this case lumping the rooms into a single thermal zone would give nonsensical daylighting illuminance values because of the presence of the interior walls, which EnergyPlus ignores when calculating illuminance reaching a reference point directly from a window (i.e., without reflection). The solution in this case is to describe each room as a separate thermal zone with its own daylighting reference points, and input the interior walls because these will participate in the calculation of inter-reflected illuminance.

![Rooms A, B, C and D have different daylighting characteristics. If lumped into a single thermal zone the daylighting calculation will be less accurate because the blockage of direct light by the interior walls between these rooms is modeled with some simplifications (see Interior Obstructions below). To get a good daylighting calculation each room should be input as a separate thermal zone.](media/rooms-a-b-c-and-d-have-different-daylighting.png)


### Multiple Lighting Zones

The detailed daylighting calculation allows a thermal zone to be divided into two independently-controlled lighting zones. (See the fields: Fraction of [Zone](#zone) Controlled by First Reference Point, Fraction of [Zone](#zone) Controlled by Second Reference Point, Illuminance SetPoint at First Reference Point, and Illuminance SetPoint at Second Reference Point.) An example is shown in Figure 58, where a relatively deep thermal zone has two lighting zones of equal area.

![Two independently-controlled lighting zones, each with 50% of the area of the thermal zone.](media/two-independently-controlled-lighting-zones.png)


### Fins, Overhangs and Other Exterior Obstructions

The daylighting calculation accounts for the presence of exterior obstructions in determining the amount of light that strikes the windows. For daylighting purposes exterior obstructions fall into three categories:

Fins, overhangs, trees, neighboring buildings, etc., entered with the objects [Shading:Zone:Detailed](#shadingzonedetailed), Shading:Site:Detailed, or [Shading:Building:Detailed](#shadingsitedetailed-shadingbuildingdetailed).

[Building](#building) surfaces like walls and roofs (in an L-shaped building, for example).

Surfaces associated with window setback.

Category (1) obstructions can have an associated solar transmittance schedule (see description of the field Transmittance Schedule Name for these shading surfaces). If this schedule is not specified, the surface is opaque (has zero transmittance).

The daylighting calculation takes the transmittance into account in determining, for example, how much light enters a window through a translucent awning. It is assumed that the solar and visible transmittance is the same and that the surfaces are non-diffusing, i.e., they do not change the direction of transmitted light. Ref:Daylighting:Controls.

Category (2) and (3) surfaces are assumed to be opaque.

Surfaces in all three categories are assumed to be black, i.e., they do not reflect light, unless you set if "Reflections" option is chosen  in the [Building](#building) object. Then obstructions can reflect light, such as the top of an overhang reflecting light onto the window above. Ref: [Building](#building), Field: Solar Distribution (with reflections options).

### Interior Obstructions

The daylighting calculation accounts for the presence of interior obstructions that lie between a window and a reference point. Unlike exterior obstructions, which can be light transmitting, interior obstructions are assumed to be opaque. Interior obstructions can reduce or eliminate the light directly reaching the reference point through a window.

However, interior obstructions that are walls, ceilings or floors are reflecting so they  contribute to the inter-reflected component of daylight illuminance as do the non-obstructing zone surfaces.

Interior obstructions can belong to the Shading object series. This type of obstruction is assumed to be non-reflecting so does not contribute to the inter-reflected component of daylight illuminance.

An example of an interior obstruction that is a wall in an L-shaped room is shown in Figure 59. Here, wall A (or, equivalently, Wall B) prevents light from directly getting to the Reference Point from the window.

![Wall A (or Wall B) is an interior obstruction that prevents light from directly reaching the daylighting reference point from the window.](media/wall-a-or-wall-b-is-an-interior-obstruction.png)


### Double Façades: Daylighting through Interior Windows

The [Daylighting:Controls](#daylightingcontrols) method, with no additional user input,  calculates the contribution of daylight that passes into a target zone through interior windows. The origin of this daylight is exterior windows in adjacent zones that share interior windows with the target zone. This capability is aimed at daylighting through a **double façade** (also called "double envelope" or "double skin.")

This is illustrated in Figure 60, which shows a double-façade buffer zone, Z0, with exterior windows EW1 and EW2. Z0 shares interior windows IW1, IW2 and IW3 with daylit occupied zones ZD1, ZD2 and ZD3, respectively ("daylit" here means the zone has an associated [Daylighting:Controls](#daylightingcontrols) object). The daylight illuminance at reference points RP1, RP2 and RP3 comes from the interior windows and has two main sources:

Daylight that passes through both an exterior window and an interior window in Z0 and reaches a reference point without reflection. This is called "direct illuminance."

Daylight from Z0  that passes through an interior window and reaches a reference point by inter-reflection in the daylit zone. Because the program calculates this source from the interior solar distribution in Z0 it is recommended that the most accurate calculation of this distribution be made, which occurs if Solar Distribution = FullInteriorAndExterior in the [Building](#building) object.

A third possible source is neglected because it is generally small in double-façade cases. This is daylight that is reflected from the surfaces of Z0, passes through an interior window and then reaches a reference point without inter-reflection in the daylit zone.

PurchAirWithDoubleFacadeDaylighting.idf is an input example of daylighting through an interior window.

![Vertical section through a double-façade building showing daylighting through interior windows. The dashed lines show that (1) reference point RP1 receives direct light from exterior window EW1 via interior window IW1; (2) RP2~~receives direct light from EW1 and EW2 via IW2; and (3) RP3 receives direct light from EW2 via IW3.](media/vertical-section-through-a-double-faade.png)


### Interior Window Daylighting Configurations that EnergyPlus Can Calculate

Figure 61 shows schematically the general configuration of daylighting through interior windows that can be calculated with EnergyPlus. Here, daylit zone ZD has one or more interior windows that are adjacent to other zones, each of which has one or more exterior windows. ZD itself may or may not have exterior windows. If it does, than the daylight illuminance from its exterior and interior windows will be additive. The zones adjacent to ZD may or may not be daylit and may or may not have other interior windows that are not adjacent to ZD. (The program does not consider the illuminance in ZD from electric lighting in adjacent zones.)

![General configuration of daylighting through interior windows that can be calculated with EnergyPlus. IW = interior window, EW = exterior window.](media/general-configuration-of-daylighting-through.png)


### Interior Window Daylighting Configurations that EnergyPlus Cannot Calculate

Figure 62 shows schematically a configuration of daylighting through interior windows that cannot be calculated with EnergyPlus. Here, zone Z has an interior window that is adjacent to zone Z1 which in turn has an interior window adjacent to zone Z2. However, the daylight from the exterior window in Z2 that enters Z after passing through Z1 is not calculated because Z2 is not adjacent to Z.

![Configuration in which daylighting of zone Z through its interior window  cannot be calculated with EnergyPlus. IW = interior window, EW = exterior window.](media/configuration-in-which-daylighting-of-zone-z.png)


### Restrictions on Shading Devices

There are two restrictions on the use of exterior-window shading devices when a daylit zone has interior windows:

#. If two daylit zones share an interior window, neither zone can have an exterior window whose shading device does glare control (i.e., the [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) for the exterior window has Glare Control Is Active = Yes).
#. If two daylit zones share an interior window, neither zone can have an exterior window with a [WindowProperty:ShadingControl](#windowpropertyshadingcontrol) that has Shading Control Type = MeetDaylightIlluminanceSetpoint.

## DElight Daylighting Method

The DElight method of analyzing daylighting in buildings is very similar to that used in the Detailed method. For each point in time, DElight calculates the interior daylighting illuminance at specified reference points and then determines how much the electric lighting can be reduced while still achieving a combined daylighting and electric lighting illuminance target. The daylight illuminance level in a zone depends on many factors, including exterior light sources; location, size, and visible light transmittance of simple and complex fenestration systems; reflectance of interior surfaces; and location of calculation reference points. The subsequent reduction of electric lighting depends on daylight illuminance level, illuminance set point, fraction of zone controlled, and type of lighting control.

There are two primary differences between the Detailed and DElight methods of calculating interior illuminance levels. The first is that DElight includes the capability of analyzing complex fenestration systems that include geometrically complicated shading systems (e.g., roof monitors) and/or optically complicated glazings (e.g., prismatic or holographic glass). The second key difference is that DElight uses a radiosity method to calculate the effects of light reflection inside a zone. These methods are discussed in more detail in the engineering documentation.

There are other important differences between the two methods. One is the inability of DElight to perform the type of dynamic shading controls possible using the Detailed method at each point in time during the thermal simulation (e.g., changes in electrochromic glazing transmittances and blind slat angles). Another is the DElight ability to include more than two reference points in its interior illuminance and electric lighting reduction calculations. A third is the current lack of visual quality (e.g., glare) calculations performed by DElight. Fourth, the modeling of interior obstructions is different in the two methods. In the DElight method interior obstructions block interreflections but do not block the intial direct illuminance. In the Detailed method, interior obstructions block the initial direct illuminance but do not block interreflections. See the engineering documentation for more details.

Input for invoking the DElight method involves three object types: **Daylighting:DELight:Controls**, **Daylighting:DELight:ReferencePoint**, and **Daylighting:DELight:ComplexFenestration**. Each of these objects is described below.

## Daylighting:DELight:Controls

The first input object required for invoking the DElight method is the [Daylighting:DELight:Controls](#daylightingdelightcontrols) object, which defines the parameters of each daylighting zone within a building. This object must be associated with a specific thermal zone within the building for which the reduction in electric lighting due to daylight illuminance will be accounted.

### Inputs

#### Field: Name

User name of the DElight daylighting zone to which the following input applies.

#### Field: Zone Name

The name of the thermal [Zone](#zone) hosting this DElight daylighting zone. This must be a valid name that has been associated with a thermal [Zone](#zone) contained in the same EnergyPlus input data file.

#### Field: Lighting Control Type

The type of overhead electric lighting control. All reference points specified are assumed to have this type of control.

For Lighting Control Type = 1 (continuous), the overhead lights dim continuously and linearly from (maximum electric power, maximum light output) to (minimum electric power, minimum light output) as the daylight illuminance increases. The lights stay on at the minimum point with further increase in the daylight illuminance.

For Lighting Control Type = 2 (stepped), the electric power input and light output vary in discrete, equally spaced steps. The number of steps is given by Number of Steps (Excluding Off) of Stepped Control. For example, if Number of Steps = 3 and Illuminance Setpoint = 600, then the following table shows the fraction of the lights that are on vs. daylight illuminance.

Table: Stepped Lighting Control Example

**Example of a Stepped Lighting Control System with Three Steps**
------------------------------------------------------------------------------
**Daylight illuminance**|**Fraction of lights that are on**
0-200|1.0
200-400|2/3
400-600|1/3
600 and above|0.0

Lighting Control Type = 3 (continuous/off) is the same as Lighting Control Type = 1 except that the lights switch off completely when the minimum dimming point is reached.

#### Field: Minimum Input Power Fraction for Continuous Dimming Control

For Lighting Control Type = 1 (continuous), the lowest power the lighting system can dim down to, expressed as a fraction of maximum input power (see figure, below). For Lighting Control Type = 3 (continuous/off) this is the power fraction reached just before the lights switch off completely.

![Illustration of continuous dimming relationship](media/illustration-of-continuous-dimming.png)


The figure above shows the relationship between electric light output and electrical input.

#### Field: Minimum Light Output Fraction for Continuous Dimming Control

For Lighting Control Type = 1 (continuous), the lowest lighting output the lighting system can dim down to, expressed as a fraction of maximum light output (see figure, above). This is the fractional light output that the system produces at minimum input power. For Lighting Control Type = 3 (continuous/off) this is the power fraction reached just before the lights switch off completely.

#### Field: Number of Steps (Excluding Off) for Stepped Control

The number of steps, excluding off, in a stepped lighting control system (see figure, below). Required and must be >0 if Lighting Control Type = 2. The steps are assumed to be equally spaced.

![Stepped lighting control with Number of Steps = 3.](media/stepped-lighting-control-with-number-of-steps.png)


#### Field: Probability Lighting Will Be Reset When Needed in Manual Stepped Control

May be specified if a stepped lighting control system (Lighting Control Type = 2) is manually operated, such as in a simple, one-step (on-off) system. Gives the probability the occupants of a daylit zone will set the electric lights to the correct level to obtain the required illuminance. The rest of the time the lights are assumed to be set one step too high. For example, consider an on-off lighting system (Number of Steps = 1) with a set point of 600 lux and 0.7 reset probability. Then, when daylighting exceeds 600 lux, the electric lights will be off 70% of the time and on 30% of the time.

#### Field: Gridding Resolution

*The maximum surface area for nodes in gridding (subdividing) all surfaces in the DElight zone*. All reflective and transmitting surfaces will be subdivided into approximately square nodes that do not exceed this maximum. Higher resolution subdivisions require greater calculation times, but generally produce more accurate results. This same gridding resolution is also used to subdivide any Complex Fenestration System surfaces. It is advisable to perform at least one simulation of new input using a small gridding resolution such as 0.1m2 to compare these results against simulation runs at lower resolution (i.e., higher maximum area nodal grids) to get a sense of possible levels of error.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Daylighting:DELight:Controls,
        DElight NORTH ZONE, !- DElight Zone Name
        NORTH ZONE,         !- Zone Name
        1,     !- Lighting control type
        0.3,   !- Minimum input power fraction for continuous dimming control
        0.2,   !- Minimum light output fraction for continuous dimming control
        0,     !- Number of steps (excluding off) for stepped control
        1.0,   !- Probability lighting will be reset when needed in manual stepped control
        1.0;   !- Gridding Resolution {m2}
~~~~~~~~~~~~~~~~~~~~

## Daylighting:DELight:ReferencePoint

The second input object required for invoking the DElight method is the [Daylighting:DELight:ReferencePoint](#daylightingdelightreferencepoint) object, which defines the parameters of each Reference Point within the associated DElight daylighting zone. This object must be associated with a specific [Daylighting:DELight:Controls](#daylightingdelightcontrols) object instance. There may be up to a maximum of 100 Reference Points for each DElight daylighting zone. Each Reference Point that is input does NOT need to be included in the control of the electric lighting system within the zone. This is determined by the fraction of the zone controlled by each Reference Point, which can be input as 0. Note that the sum of all Reference Point control fractions must equal 1 to obtain correct overall results.

### Inputs

#### Field: Name

User name of the DElight daylighting Reference Point to which the following input applies.

#### Field: DElight Name

The name of the [Daylighting:DELight:Controls](#daylightingdelightcontrols) object instance hosting this Reference Point. This must be a valid name that has been associated with a DElight daylighting [Zone](#zone) contained in the same EnergyPlus input data file.

#### Fields: (X,Y,Z) of Reference Point

These three fields are the X, Y and Z values of the Reference point in the coordinate system you specified for daylighting reference points in the [GlobalGeometryRules](#globalgeometryrules) object. Figure 65 shows an example using the relative coordinate (to zone) system. Z is typically at work surface height (e.g., 0.8m for a desk).

![Example showing location of two daylighting reference points in the zone coordinate system (relative) of a rectangular zone with three windows. (a) Perspective view, (b) plan view, (c) elevation view. All dimensions are in meters.](media/example-showing-location-of-daylighting.png)


#### Field: Fraction of Zone Controlled by Reference Point

The zone floor-area fraction of the electric lighting that is controlled by the daylight illuminance at this Reference Point. If the sum of the values for this field for all Reference Points input for a given daylighting zone is less than 1.0, then the remaining fraction (i.e., 1.0 – Sum) is assumed to not have lighting control.

Note that Reference Points may be input with a 0.0 value for this fraction. In this case, daylight factors and interior illuminance values will be calculated for the Reference Point, but it will play no role in controlling the electric lighting within the daylighting zone to which it belongs.

#### Field: Illuminance Setpoint at Reference Point

The desired lighting level (in lux) at this Reference Point. This is assumed to be the lighting level that would be produced at this reference point at night if the overhead electric lighting were operating at full input power. Recommended values depend on the type of activity and may be found, for example, in the Lighting Handbook of the Illuminating Engineering Society of North America. A typical value for general office work (excluding computer terminals) is 500 lux.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    Daylighting:DELight:ReferencePoint,
        RefPt 2,            !- Reference Point Name
        DElight NORTH ZONE, !- DElight Zone Name
        5.0,  !- X-coordinate of reference point {m}
        3.0,  !- Y-coordinate of reference point {m}
        0.9,  !- Z-coordinate of reference point {m}
        0.5,  !- Fraction of zone controlled by reference point
        500.; !- Illuminance setpoint at reference point {lux}
~~~~~~~~~~~~~~~~~~~~

## Daylighting:DELight:ComplexFenestration

The third input object related to the DElight method is the Daylighting:DElight: Complex Fenestration object. The DElight daylighting analysis method can be applied to daylighting zones that contain only simple fenestration systems such as windows and skylights that are standard EnergyPlus sub-surfaces. In this situation, no Daylighting:DElight: Complex Fenestration object would be input.

In addition to analyzing simple fenestration systems, DElight includes the capability of analyzing complex fenestration systems such as geometrically complicated static shading systems (e.g., roof monitors) and/or optically complicated glazings (e.g., prismatic or holographic glass). This capability is based on characterizing these complex fenestration systems (CFS) using bi-directional transmittance distribution functions (BTDF). In general, BTDF data for a specific CFS must be either measured or simulated (e.g., using ray-tracing techniques) prior to employing DElight to analyze it within EnergyPlus. The current implementation of DElight CFS calculations within EnergyPlus supports two approaches to the input of BTDF, an analytical approach and a file-based approach. The details of inputting these two approaches are described below under the User Complex Fenestration Type field.

Two analytical CFS BTDF types are currently supported, window and light shelf. The file-based approach requires that a user has access to a data file containing raw BTDF data that DElight reads as additional input during its analysis calculations. BTDF data files are described separately since it is anticipated that individual EnergyPlus users will not create these data files themselves.

The methods related to characterizing and analyzing CFS using BTDF are still evolving. DElight is an early implementation of CFS analysis methods. These methods, and the input associated with them here, will likely change in the future.

### Inputs

#### Field: User Name

User name of the DElight daylighting Complex Fenestration to which the following input applies.

#### Field: Complex Fenestration Type

Type name of the DElight daylighting Complex Fenestration system to be analyzed. This type name must take one of the following two forms.

> BTDF^GEN^Analytical Type^Normal Visible Transmittance^Dispersion Angle

> BTDF^FILE^Filename

The first form above is for supported analytical CFS types which currently include WINDOW and LIGHTSHELF. While these analytical types are relatively simple, they represent flexible ways to explore diffusing CFS systems and the impact of light shelves in redirecting light through an aperture. Each of these types also requires the visible transmittance of the CFS at normal incidence angle, and a dispersion angle (in degrees) that represents the "spread" of transmitted light. A small dispersion angle of 10 corresponds to clear glazing while a large angle of 90 corresponds to perfectly diffusing glazing. The "^" symbol must be used as a delimiter between sub-fields within this Complex Fenestration type name string as shown in the IDF example for WINDOW below, and in the DElight sample input data files.

The second form above is for CFS types for which there is pre-measured or pre-simulated BTDF data. In this case the Filename sub-field must be a valid data file name that is associated with an existing BTDF dataset that DElight can use in its calculations.

#### Field: Building Surface Name

The name of the heat transfer surface object instance hosting this Complex Fenestration, analogous to the [Building](#building) Surface Name field for subsurfaces such as Windows. This must be a valid name that has been associated with a heat transfer surface contained in the same EnergyPlus input data file.

#### Field: Window Name

The name of the [Window](#window) (ref: [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) object) instance that will be used to account for the geometry, and the solar/thermal gains/losses, of the Complex Fenestration system surface. This must be a valid name that has been associated with a [Window](#window) contained in the same EnergyPlus input data file. The geometry for the Complex Fenestration is taken from the geometry input for this standard EnergyPlus subsurface, hence the term "Doppelganger."

Note that DElight only deals with the visible spectrum of light transmitted through a Complex Fenestration. To account for the solar/thermal influences of a Complex Fenestration, a geometrically coincident subsurface that will be accounted for by methods already within EnergyPlus must be defined in the input data file. This is an interim solution to the issue of accounting for solar/thermal influences that will likely change as techniques analogous to the daylighting analysis of BTDF are developed.

#### Field: Fenestration Rotation

The in-plane counter-clockwise rotation angle between the Complex Fenestration optical reference direction and the base edge of the Doppelganger Surface geometry. The Complex Fenestration optical reference direction is the direction of the zero azimuth angle for the BDTF dataset. This Rotation angle will typically be zero when the Doppelganger surface is rectangular and its width edge is aligned with the Complex Fenestration optical reference direction.

An IDF example for an analytical WINDOW type CFS:

~~~~~~~~~~~~~~~~~~~~

    Daylighting:DELight:ComplexFenestration,
        Window CFS,       !- DElight Complex Fenestration User Name
        BTDF^GEN^WINDOW^1.0^20.0,     !- Complex Fenestration Type
        ZN003:WALL001,  !- Complex Fenestration Host Surface
        Zn003:Wall001:Doppel001, !- Doppelganger Surface Name
        0.0;            !- Fenestration Rotation {deg}
~~~~~~~~~~~~~~~~~~~~

### Outputs

In addition to the daylighting-specific outputs for DElight listed below, two ASCII text files are created during an EnergyPlus run that includes DElight analysis.  Following completion of an EnergyPlus run, these files are given names that consist of the project name appended with DElight.in and DElight.out.  The format of these files is described in the Output Details and Examples document.

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Daylighting Reference Point Illuminance [lux]
    Zone,Average,Daylighting Lighting Power Multiplier []
~~~~~~~~~~~~~~~~~~~~

#### Daylighting Reference Point Illuminance [lux]

The total daylight illuminance at a DElight reference point from all of the exterior windows in a daylit zone.

#### Daylighting Lighting Power Multiplier []

The amount by which the overhead electric lighting power in a zone is multiplied due to usage of DElight calculated daylighting to dim or switch electric lights. For example, if the multiplier is M and the electric power without dimming is P, then the electric power with dimming is M\*P. The multiplier varies from 0.0, which corresponds to maximum dimming (zero electric lighting), to 1.0, which corresponds to no dimming.

### Outputs

The following daylighting-specific outputs are available for Daylighting:Controls:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Site Exterior Beam Normal Illuminance [lux]
    Zone,Average,Site Exterior Horizontal Beam Illuminance [lux]
    Zone,Average,Site Exterior Horizontal Sky Illuminance [lux]
    Zone,Average,Site Beam Solar Radiation Luminous Efficacy [lum/W]
    Zone,Average,Site Sky Diffuse Solar Radiation Luminous Efficacy [lum/W]
    Zone,Average,Site Daylighting Model Sky Clearness []
    Zone,Average,Site Daylighting Model Sky Brightness []
    Surface,Average,Daylighting Window Reference Point 1 View Luminance [cd/m2]
    Surface,Average,Daylighting Window Reference Point 1 Illuminance [lux]
    Zone,Average,Daylighting Reference Point 1 Illuminance [lux]
    Zone,Average,Daylighting Reference Point 1 Glare Index []
    Zone,Sum,Daylighting Reference Point 1 Glare Index Setpoint Exceeded Time [hr]
    Zone,Sum,Daylighting Reference Point 1 Daylight Illuminance Setpoint Exceeded Time [hr]
    Surface,Average,Daylighting Window Reference Point 2 View Luminance [cd/m2]
    Surface,Average,Daylighting Window Reference Point 2 Illuminance [lux]
    Zone,Average,Daylighting Reference Point 2 Illuminance [lux], if applicable
    Zone,Average,Daylighting Reference Point 2 Glare Index [], if applicable
    Zone,Sum,Daylighting Reference Point 2 Glare Index Setpoint Exceeded Time [hr]
    Zone,Sum,Daylighting Reference Point 2 Daylight Illuminance Setpoint Exceeded Time [hr]
    Zone,Average,Daylighting Lighting Power Multiplier []
~~~~~~~~~~~~~~~~~~~~

#### Site Exterior Beam Normal Illuminance [lux]

Beam normal illuminance of the sun at the earth's surface, measured in lux (lumens/m^2^)

#### Site Exterior Horizontal Beam Illuminance [lux]

Beam illuminance on an unobstructed horizontal plane at the earth's surface. Equals "Site Exterior Beam Normal Illuminance" times sine of solar altitude.

#### Site Exterior Horizontal Sky Illuminance [lux]

Illuminance from sky solar radiation on an unobstructed horizontal plane at the earth's surface. The total exterior horizontal illuminance is the sum of "Site Exterior Horizontal Beam Illuminance" and "Site Exterior Horizontal Sky Illuminance."

#### Site Beam Solar Radiation Luminous Efficacy [lum/W]

A measure of the visible light content of beam solar radiation; equal to the number of lumens per watt of beam solar radiation. Depends on atmospheric conditions (moisture, turbidity, cloudiness) and solar altitude.

#### Site Sky Diffuse Solar Radiation Luminous Efficacy [lum/W]

A measure of the visible light content of sky diffuse solar radiation; equal to the number of lumens per watt of sky diffuse solar radiation. Depends on atmospheric conditions (moisture, turbidity, cloudiness) and solar altitude.

#### Site Daylighting Model Sky Clearness [ ]

Clearness of sky. One of the factors used to determine sky type and luminous efficacy of solar radiation (see *EnergyPlus Engineering Document*). Sky Clearness close to 1.0 corresponds to an overcast sky. Sky Clearness > 6 is a clear sky.

#### Site Daylighting Model Sky Brightness [ ]

Brightness of sky. One of the factors used to determine sky type and luminous efficacy of solar radiation (see *EnergyPlus Engineering Document*).

#### Daylighting Window Reference Point 1 View Luminance [cd/m2]

The area-averaged luminance of an exterior window as viewed from the first reference point in the daylit zone containing the window. In general, higher window luminance values are associated with higher daylight glare values. (Printed only for exterior windows in daylit zones without interior windows.)

#### Daylighting Window Reference Point 1 Illuminance [lux]

The contribution from a particular exterior window to the daylight illuminance at the first reference point in the daylit zone containing the window. (Not printed for exterior windows in daylit zones with interior windows.)

#### Daylighting Reference Point 1 Illuminance [lux]

The total daylight illuminance at the first reference point from all of the windows in a daylit zone.

#### Daylighting Reference Point 1 Glare Index []

The daylight glare index at the first reference point in a daylit zone.

#### Daylighting Reference Point 1 Glare Index Setpoint Exceeded Time [hr]

The number of hours when the calculated daylight glare index at the first reference point exceeds the glare index setpoint.

#### Daylighting Reference Point 1 Daylight Illuminance Setpoint Exceeded Time [hr]

The number of hours when the calculated daylight illuminance at the first reference point exceeds the daylight illuminance setpoint.

#### Daylighting Reference Point 2 Illuminance [lux]

The total daylight illuminance at the second reference point from all of the windows in a daylit zone.

#### Daylighting Window Reference Point 2 View Luminance [cd/m2]

The area-averaged luminance of an exterior window as viewed from the second reference point, if present, in the daylit zone containing the window. Note that, for a bare window, this value can be different from the corresponding value at the first reference point since different regions of the sky and ground will be visible from the two reference points, and different ranges of angles of incidence and associated glass transmittance values will be involved. (Not printed for exterior windows in daylit zones with interior windows.)

#### Daylighting Window Reference Point 2 Illuminance [lux]

The contribution from a particular exterior window to the daylight illuminance at the second reference point, if present, in the daylit zone containing the window. (Not printed for exterior windows in daylit zones with interior windows.)

#### Daylighting Reference Point 2 Glare Index []

The daylight glare index at the second reference point, if present, in a daylit zone.

#### Daylighting Reference Point 2 Glare Index Setpoint Exceeded Time [hr]

The number of hours when the calculated daylight glare index at the second reference point exceeds the glare index setpoint.

#### Daylighting Reference Point 2 Daylight Illuminance Setpoint Exceeded Time [hr]

The number of hours when the calculated daylight illuminance at the second reference point exceeds the daylight illuminance setpoint.

#### Daylighting Lighting Power Multiplier []

The amount by which the overhead electric lighting power in a zone is multiplied due to usage of daylighting to dim or switch electric lights. For example, if the multiplier is M and the electric power without dimming is P, then the electric power with dimming is M\*P. The multiplier varies from 0.0, which corresponds to maximum dimming (zero electric lighting), to 1.0, which corresponds to no dimming.

## Output:IlluminanceMap

The [Output:IlluminanceMap](#outputilluminancemap) object expands on the reporting capabilities of the daylighting simulation. For any zone simulated with [Daylighting:Controls](#daylightingcontrols), the illuminance map can generate up to 2,500 points of additional daylighting illuminance values. The resulting map is output as a comma delimited text file that can be imported into a spreadsheet program for rapid visualization of the daylighting illuminance patterns in a zone. The values are produced on an hourly basis. The Z height of the map is constant (parallel to a flat floor). More than one illuminance map can be created for a zone.

### Inputs

#### Field: Name

The name of the map object.

#### Field: Zone Name

Reference to a zone with [Daylighting:Controls](#daylightingcontrols).

#### Field: Z Height

The height or elevation of the grid of daylighting points.

#### Field: X Minimum Coordinate

The minimum X coordinate boundary for the map.

#### Field: X Maximum Coordinate

The maximum X coordinate boundary for the map.

#### Field: Number of X Grid Points

The number of daylighting reference points in the X direction from the minimum to the maximum boundaries. The maximum number of points that can be generated is 2500 (Number of X Grid Points X Number of Y Grid Points).

#### Field: Y Minimum Coordinate

The minimum Y coordinate boundary for the map.

#### Field: Y Maximum Coordinate

The maximum Y coordinate boundary for the map.

#### Field: Number of Y Grid Points

The number of daylighting reference points in the Y direction from the minimum to the maximum boundaries. The maximum number of points that can be generated is 2500 (Number of X Grid Points X Number of Y Grid Points).

> Note:  Daylighting factors cannot be accurately calculated for reference points that are very close to a wall or window (less than 0.15 m or 6 inches). An error is reported for a reference point that is too close to a window, but no error is reported for a point that is too close to a wall.

~~~~~~~~~~~~~~~~~~~~

    Output:IlluminanceMap,
      Daylit Map,      ! Map Name
      Daylit Zone,     ! Zone Name
      0,               ! Z Height [m]
      0.1,             ! X Minimum Coordinate [m]
      4.9,             ! X Maximum Coordinate [m]
      10,              ! Number of X Grid Points
      0.1,             ! Y Minimum Coordinate [m]
      9.9,             ! Y Maximum Coordinate [m]
      10;              ! Number of Y grid Points
~~~~~~~~~~~~~~~~~~~~

Since not all zones are rectangular, it is possible to have map points that are outside the zone. Any illuminance registered at these points is inaccurate and, additionally, a "\*" marks these values for easy observance.

## OutputControl:IlluminanceMap:Style

This object specifies the "style" for the illuminance map output (described in the Output Details and Examples document). As described early in the document (see: EnergyPlus Output Processing), the user may select the "style" for the daylighting illuminance map output file (eplusmap.<ext>).

### Inputs

#### Field: Column Separator

For this field, the desired separator for columns is entered. "Comma" creates comma separated fields/columns in the outputs (eplusmap.csv file is created). "Tab" creates tab separated fields/columns in the outputs (eplusmap.tab file is created). "Fixed" creates space separated fields/columns in the outputs (eplusmap.txt file is created) but these are not necessarily lined up for easy printing.

Note that both tab and comma separated files easily import into Excel™ or other spreadsheet programs. The tab delimited files can also be viewed by text editors, word processing programs and easily converted to "tables" within those programs.

## Daylighting Devices

Daylighting devices work in conjunction with the [Daylighting:Controls](#daylightingcontrols) object to simulate components that can improve daylighting in a zone.

Daylighting devices are also tightly integrated into the zone heat balance. The thermal effects of these devices are simulated with or without the use of a [Daylighting:Controls](#daylightingcontrols) object.

There are two types of daylighting device in EnergyPlus:  tubular daylighting devices and daylighting shelves.

## DaylightingDevice:Tubular

Tubular daylighting devices (TDDs), also known as tubular skylights or light pipes, are used to bring natural exterior daylight into the hard-to-reach, interior spaces of a building.

TDDs consist of three components: a dome, a pipe, and a diffuser.

![Tubular daylighting device diagram.](media/tubular-daylighting-device-diagram..jpeg)


In EnergyPlus each of these components corresponds to an object in the input file.

The dome and diffuser are defined in the same way as windows using the [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) object. The *Surface Type* field must be specified as TubularDaylightDome or TubularDaylightDiffuser accordingly.

The location and orientation of the dome surface affect the total amount of daylight collected.

The *Base Surface* of the diffuser object determines to which zone the daylighting is delivered. The location and orientation of the diffuser surface affect the amount of daylight received at the [Daylighting:Controls](#daylightingcontrols) reference points.

Although the object definition is the same as for a window, there are several restrictions on TubularDaylightDome and TubularDaylightDiffuser objects:

- Shading control devices are not allowed.
- Frames and dividers are not allowed.
- Multipliers must be 1.0.
- Dome, diffuser, and pipe areas (as given by diameter) must be approximately equal.
- Outside face environment objects are not allowed.
- Dome and diffuser constructions cannot be more than one layer.

Since commercial TDDs are assumed to be cylindrical in shape, it is recommended that the circular areas of the actual dome and diffuser be approximated with a square of equivalent area for the TubularDaylightDome and TubularDaylightDiffuser objects. Although it is possible to use a triangular surface instead, a square is a much better geometric approximation of a circle.

Note that the TubularDaylightDome surface is allowed to have a different position and tilt from the roof base surface. If the actual TDD projects some height above the roof surface, the TubularDaylightDome coordinates should be located accordingly.

![Tubular daylighting device DXF output.](media/tubular-daylighting-device-dxf-output..png)


The TubularDaylightDome surface automatically casts a shadow on the roof base surface. However, since the pipe is not represented by a surface, it will not cast a shadow. If this effect must be simulated, one or more SURFACE:SHADING objects can be used.

The dome and diffuser objects are connected by the DAYLIGHTING DEVICE:TUBULAR object:

### Inputs

#### Field: Name

The name of the TDD object.

#### Field: Dome Name

Reference to a [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) object with *Surface Type* TubularDaylightDome.

#### Field: Diffuser Name

Reference to a [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) object with *Surface Type* TubularDaylightDiffuser.

#### Field: Construction Name

The construction of the TDD pipe. The visible and solar absorptance of the inside material layer determines the reflectivity of the inner mirrored surface of the TDD. This is very important for the overall transmittance of the TDD.

#### Field: Diameter

The diameter [m] of the TDD pipe. The area of the pipe must match the areas of the dome and diffuser.

#### Field: Total Length

The total length [m] of the TDD pipe between the dome and the diffuser, including the exterior part above the roof. The exterior length is determined internally by subtracting the transition zone lengths from the total length.

#### Field: Effective Thermal Resistance

The effective thermal resistance [m^2^-K/W], i.e. R-value, of the TDD between the exterior dome surface and the interior diffuser surface.

#### Field: Transition Zone<#>  Name

Name of zone that the TDD pipe passes through before reaching the delivery zone.

#### Field: Transition Zone <#> Length

Length of pipe [m] in the transition zone. This is used for determining heat gains due to solar absorbed by the pipe in each transition zone. The distribution of absorbed solar gains can be customized by adjusting the length of pipe in each transition zone.   The transition zone gain is proportional to the length of pipe in the zone. If no transition zones are specified, all solar absorbed by the pipe is lost to the exterior environment.

The *Transition [Zone](#zone) Name* and *Transition [Zone](#zone) Length* fields can be repeated for additional transition zones.

~~~~~~~~~~~~~~~~~~~~

    DaylightingDevice:Tubular,
      Pipe1,                    !- Object Name
      Dome1,                    !- Dome Name
      Diffuser1,                !- Diffuser Name
      Reflective Aluminum,      !- Construction Name
      0.3556,                   !- Diameter [m] (approximately 14", a standard size)
      1.4,                      !- Total Length [m] (subtract zone lengths to get outdoor exposed length)
      0.28,                     !- Effective Thermal Resistance [m2-K/W] between TubularDaylightDome and
     !                                TubularDaylightDiffuser
      Attic Zone,               !- Transition Zone 1 Name
      1.1;                      !- Transition Zone 1 Length [m]

     FenestrationSurface:Detailed,
      Dome1,  !- Subsurface Name
      TubularDaylightDome,  !- Surface Type
      Clear Acrylic Dome,  !- Construction Name (only 1 layer allowed in construction)
      Attic Roof,  !- Base Surface Name
      ,  !- Outside Face Environment (not allowed for TubularDaylightDome)
      0.0,  !- VF to Ground
      ,  !- Window Shading Control (not allowed for TubularDaylightDome)
      ,  !- Frame/Divider Name (not allowed for TubularDaylightDome)
      1.0,  !- Multiplier (must be 1.0 for TubularDaylightDome)
      4,  !- Number of Vertices
      2.3425,  3.1575,  3.9,
      2.3425,  2.8425,  3.9,
      2.6575,  2.8425,  3.9,
      2.6575,  3.1575,  3.9;

     FenestrationSurface:Detailed,
      Diffuser1,  !- Subsurface Name
      TubularDaylightDiffuser,  !- Surface Type
      Frosted Acrylic Diffuser,  !- Construction Name (only 1 layer allowed in construction)
      Daylit Zone Ceiling,  !- Base Surface Name
      ,  !- Outside Face Environment (not allowed for TubularDaylightDiffuser)
      0.0,  !- VF to Ground
      ,  !- Window Shading Control (not allowed for TubularDaylightDiffuser)
      ,  !- Frame/Divider Name (not allowed for TubularDaylightDiffuser)
      1.0,  !- Multiplier (must be 1.0 for TubularDaylightDiffuser)
      4,  !- Number of Vertices
      2.3425,  3.1575,  2.5,
      2.3425,  2.8425,  2.5,
      2.6575,  2.8425,  2.5,
      2.6575,  3.1575,  2.5;
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Tubular Daylighting Device Transmitted Solar Radiation Rate [W]
    Zone,Average,Tubular Daylighting Device Pipe Absorbed Solar Radiation Rate [W]
    Zone,Average,Tubular Daylighting Device Heat Gain Rate [W]
    Zone,Average,Tubular Daylighting Device Heat Loss Rate [W]
    Zone,Average,Tubular Daylighting Device Beam Solar Transmittance []
    Zone,Average,Tubular Daylighting Device Beam Visible Transmittance []
    Zone,Average,Tubular Daylighting Device Diffuse Solar Transmittance []
    Zone,Average,Tubular Daylighting Device Diffuse Visible Transmittance []
~~~~~~~~~~~~~~~~~~~~

#### Tubular Daylighting Device Beam Solar Transmittance []

This is the transmittance of beam solar radiation through the TDD.

#### Tubular Daylighting Device Beam Visible Transmittance []

This is the transmittance of beam visible radiation, or daylight, through the TDD.

#### Tubular Daylighting Device Diffuse Solar Transmittance []

This is the transmittance of diffuse solar radiation through the TDD.

#### Tubular Daylighting Device Diffuse Visible Transmittance []

This is the transmittance of diffuse visible radiation, or daylight, through the TDD.

#### Tubular Daylighting Device Heat Gain Rate [W]

This is the rate of heat gain to the zone by the TDD, in Watts.

#### Tubular Daylighting Device Heat Loss Rate [W]

This is the rate of heat loss from the zone by the TDD, in Watts.

#### Tubular Daylighting Device Pipe Absorbed Solar Radiation Rate [W]

This is the rate at which solar radiation is absorbed by the pipe in the TDD, in Watts.

#### Tubular Daylighting Device Transmitted Solar Radiation Rate [W]

This is the rate at which solar radiation is transmitted by the TDD, in Watts.

In addition, several surface and window variables are also reported for the TubularDaylightDome and TubularDaylightDiffuser objects. For the TubularDaylightDome:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Surface Outside Face Sunlit Area [m2]
    Zone,Average,Surface Outside Face Sunlit Fraction []
    Zone,Average,Surface Outside Face Incident Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Incident Beam Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Incident Sky Diffuse Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Incident Ground Diffuse Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Beam Solar Incident Angle Cosine Value[]
    Zone,Average,Surface Window Transmitted Solar Radiation Rate [W]
    Zone,Average,Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]
~~~~~~~~~~~~~~~~~~~~

For the TubularDaylightDiffuser:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Surface Outside Face Incident Solar Radiation Rate per Area[W/m2] (incident inside of pipe)
    Zone,Average,Surface Window Transmitted Solar Radiation Rate [W] (same as Tubular Daylighting Device Transmitted Solar Radiation Rate)
    Zone,Average,Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]
~~~~~~~~~~~~~~~~~~~~

## DaylightingDevice:Shelf

Daylighting shelves, or simply light shelves, are another device for bringing more daylight into a building. Installed as an accessory to a window, daylighting shelves work by reflecting exterior light onto the ceiling of a room. Daylighting shelves can have an inside shelf, an outside shelf, or both.

![Daylighting shelf diagram.](media/daylighting-shelf-diagram..jpeg)


The inside shelf redistributes light that would have entered the zone anyway. Instead of entering as a beam, all light is reflected onto the zone ceiling and is converted to diffuse shortwave radiation.

The outside shelf changes the amount of light entering the zone. If the shelf surface is more reflective than the ground, it can increase the amount of light incident on the upper part of the window. However, the shading effect of the outside shelf on the lower part of the window must also be considered as it can easily negate any gain achieved in the upper part of the window. All light reflected from the outside shelf that enters the upper window is assumed to strike the ceiling.

![Daylighting shelf DXF output.](media/daylighting-shelf-dxf-output..png)


In EnergyPlus a daylighting shelf is simulated using a [DaylightingDevice:Shelf](#daylightingdeviceshelf) object in combination with up to three other objects: a window, a heat transfer surface, and an attached shading surface.

The window must be divided into two window surfaces: an upper window and a lower window. The upper window interacts directly with the daylighting shelf object and is not allowed to have any shading control devices or frames or dividers. The lower window does not interact with the daylighting shelf object, but does receive shading from the outside shelf. There are no restrictions on the lower window.

The inside shelf is defined as a regular heat transfer surface in the zone. However, the surface must have the *Outside Boundary Condition* field set to Surface with itself as the other zone surface. Shading and thermal mass effects are taken into account.

The outside shelf is defined as a [Shading:Zone:Detailed](#shadingzonedetailed) object. The visible and solar absorptance of the outside material layer determines the reflectivity of the shelf.

> NOTE:  Unlike a regular [Shading:Zone:Detailed](#shadingzonedetailed) object, the vertices of the outside shelf surface must be ordered so that the outward normal vector points *upward*, i.e. toward the upper window. This is necessary in order for the outside shelf to properly receive sunlight and shading from other surfaces. A mirror shading surface with the outward normal vector pointing in the opposite direction is automatically created by the program to shade the lower window.

The inside shelf and outside shelf are both optional. However, if neither shelf is specified, the daylighting shelf object has no effect on the simulation.

### Inputs

#### Field: Name

The name of the daylighting shelf object.

#### Field: Window Name

Reference to a [FenestrationSurface:Detailed](#fenestrationsurfacedetailed) object with *Surface Type* WINDOW.

#### Field: Inside Shelf Name

Reference to a [BuildingSurface:Detailed](#buildingsurfacedetailed) object. This field is optional. If used, this surface must have OTHERZONESURFACE specified for the *Outside Face Environment* field and the referenced other zone surface must be itself. The number of vertices of this surface object must be 4.

#### Field: Outside Shelf Name

Reference to a [Shading:Zone:Detailed](#shadingzonedetailed) object. This field is optional. If used, the number of vertices of this surface object must be 4.

#### Field: Outside Shelf Construction Name

Reference to a CONSTRUCTION object. This field is required if an outside shelf is specified. The visible and solar absorptance of the outside material layer determines the shelf reflectivity.

#### Field: View Factor To Outside Shelf

User defined value for the view factor from the window to the outside shelf. This field is optional. If not specified, an exact view factor is calculated for two perpendicular rectangles of the same width having a shared edge. If the given surfaces do not meet these geometric requirements, it is necessary to specify a value here.

> NOTE:  It is up to the user to adjust the view factor to ground of the upper window to account for the part of the view blocked by the outside shelf. The calculated *View Factor To Outside Shelf* is reported in the eio file for this purpose. For the typical case where the shelf is parallel to the ground and the upper window is perpendicular to the ground, the view factor to ground is simply:  0.5 – *View Factor To Outside Shelf*.

~~~~~~~~~~~~~~~~~~~~

    DaylightingDevice:Shelf,
      Shelf,  !- Name
      Daylit Upper Window,  !- Window Name
      Inside Shelf,  !- Inside Shelf Name
      Outside Shelf,  !- Outside Shelf Name
      Shelf Construction;  !- Outside Shelf Construction Name (required if outside shelf specified)
      ! 0.29;  !- View Factor To Outside Shelf (optional)

    FenestrationSurface:Detailed,
      Daylit Upper Window,  !- Subsurface Name
      Window,  !- Surface Type
      Standard Window,  !- Construction Name
      Daylit South Wall,  !- Base Surface Name
      ,  !- Outside Face Environment
      0.211,  !- VF to Ground (user must adjust to account for view factor to outside shelf)
      ,  !- Window Shading Control (not allowed)
      ,  !- Frame/Divider Name (not allowed)
      1.0,  !- Multiplier (must be 1.0)
      4,  !- Number of Vertices
      1.0,  0.0,  2.8,
      1.0,  0.0,  2.0,
      4.0,  0.0,  2.0,
      4.0,  0.0,  2.8;

     BuildingSurface:Detailed,
      Inside Shelf,  !- Surface Name
      Wall,  !- Surface Type
      Shelf Construction,  !- Construction Name
      Daylit Zone,  !- Zone Name
      OtherZoneSurface,  !- Exterior Conditions (must be OtherZoneSurface)
      Inside Shelf,  !- Target (must be itself)
      NoSun,  !- Solar Exposure
      NoWind,  !- Wind Exposure
      0.0,  !- VF to Ground
      4,  !- Number of Vertices
      1.0,  0.0,  2.0,
      4.0,  0.0,  2.0,
      4.0,  1.0,  2.0,
      1.0,  1.0,  2.0;

     Shading:Zone:Detailed,
      Outside Shelf,  !- Surface Name
      Daylit South Wall,  !- Base Surface Name
      ,  !- Shading Transmittance Schedule (default is always opaque)
      4,  !- Number of Vertices
      1.0,  0.0,  2.0,  !- Outward normal vector must point up toward the upper window
      1.0, -1.0,  2.0,
      4.0, -1.0,  2.0,
      4.0,  0.0,  2.0;
~~~~~~~~~~~~~~~~~~~~

### Outputs

The view factor to outside shelf calculation shows up in the .eio file along with the associated window and window view factors to sky and ground:

~~~~~~~~~~~~~~~~~~~~

    ! <Shelf Details>,Name,View Factor to Outside Shelf,Window Name,Window View Factor to Sky,Window View Factor to Ground
    SHELF,0.29,DAYLIT UPPER WINDOW,0.50,0.21
~~~~~~~~~~~~~~~~~~~~

This variable reports the calculated *View Factor To Outside Shelf* so that the user can correctly adjust the view factor to ground of the upper window.

The usual window and surface variables are relevant for the upper window:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Surface Outside Face Sunlit Area [m2]
    Zone,Average,Surface Outside Face Sunlit Fraction []
    Zone,Average,Surface Outside Face Incident Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Incident Beam Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Incident Sky Diffuse Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Incident Ground Diffuse Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Beam Solar Incident Angle Cosine Value[]
    Zone,Average,Surface Window Transmitted Solar Radiation Rate [W]
    Zone,Average,Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]
    Zone,Average,Surface Window Heat Gain Rate [W]
    Zone,Average,Surface Window Heat Loss Rate [W]
~~~~~~~~~~~~~~~~~~~~

The following surface variables are reported for the outside shelf surface, if specified:

~~~~~~~~~~~~~~~~~~~~

    Zone,Average,Surface Outside Face Sunlit Area [m2]
    Zone,Average,Surface Outside Face Sunlit Fraction []
    Zone,Average,Surface Outside Face Incident Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Incident Beam Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Incident Sky Diffuse Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Incident Ground Diffuse Solar Radiation Rate per Area[W/m2]
    Zone,Average,Surface Outside Face Beam Solar Incident Angle Cosine Value[]
~~~~~~~~~~~~~~~~~~~~

## DaylightingDevice:LightWell

This object is used to model the impacts on daylighting of a "light well" that might be associated with exterior windows such as skylights.  The light well model attenuates the light transmitted by the skylight. The attenuation is characterized by the **well efficiency**, which is the ratio of the amount of light leaving the well to the amount of light entering the well. The well efficiency varies from close to 1.0  to close to zero if there is high attenuation. The well efficiency is used only in the EnergyPlus detailed daylighting calculation, where it multiplies the beam and diffuse light transmitted by the skylight. (The well efficiency is not used in calculating the solar gain through the skylight.)

The input object describes the light well using basic characteristics of the geometry along with the visible reflectance of the well's side walls.  The following figure diagrams how the geometry is characterized.

![Skylight with light well: (a) perspective view, (b) vertical section.If the bottom of the light well is a rectangle of side lengths c and d, as shown in (a), then the perimeter of the bottom of the well = 2(c+d) and the area = cd (see description of field names for the Light Well object).](media/skylight-with-light-well-a-perspective-view-b.png)


### Inputs

#### Field: Exterior Window Name

The name of the exterior window that this Light Well is associated with. Generally this is a skylight in a roof. However, light wells can be applied to an exterior window of any slope. Light wells can be assigned to both rectangular and triangular exterior windows, but they should not be assigned to interior windows. Note that the sides of the light well can be sloped and the bottom of the light well can be any shape, not  just rectangular.

#### Field: Height of Well

The distance from the bottom of the skylight to the bottom of the well. If the skylight and well bottom are not coplanar, this is the distance from the center of the bottom of the skylight to center of the bottom of the well. See Figure 70.

#### Field: Perimeter of Bottom of Well

The perimeter of the bottom opening of the well. See Figure 70.

#### Field: Area of Bottom of Well

The area of the bottom opening of the well. A warning will be issued if this area is less that the area of the skylight, including frame if present. See Figure 70.

#### Field: Visible Reflectance of Well Walls

The visible reflectance of the side walls of the well. If the walls do not all have the same reflectance, an area-weighted average value can be used.  This is the well-wall reflectance expressed as a fraction.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    DaylightingDevice:LightWell,  ! The well is 60% reflecting, is 1.2m high and has a 2.5m x 5.5m bottom opening.
     Skylight-1, !- Name of Exterior Window that this Light Well Applies To
     1.2,        !- Height of Well(m) 16.0,       !- Perimeter of Bottom of Well (m) 13.75,      !- Area of Bottom of Well (m2)
     0.60;       !- Visible Reflectance of Well Walls
~~~~~~~~~~~~~~~~~~~~

## Daylighting Modeling Options

The following table shows what input objects/fields to use to model different daylighting options. It also gives the name of an example input, if available, that demonstrates the option.

Table: Daylighting Modeling Options

Option|Object/Field or Output Variable|Input File
------|-------------------------------|----------
Allow a thermal zone to be **daylit**|Daylighting:Controls, [Daylighting:DELight:Controls](#daylightingdelightcontrols) |PurchAirWithDaylighting.idf, DElight-Detailed-Comparison.idf
Specify visible transmittance of the glazing|WindowMaterial:Glazing|PurchAirWithDaylighting.idf
Specify the visible transmittance of a shading device|WindowMaterial:Shade, [WindowMaterial:Screen](#windowmaterialscreen) or WindowMaterial:Blind|PurchAirWindowBlind.idf
Use a shading device to control glare|WindowMaterial:Shade or [WindowMaterial:Blind](#windowmaterialblind); WindowProperty:ShadingControl\*|PurchAirWindowBlind.idf
Use electrochromic glazing to control glare|WindowProperty:ShadingControl/Shading Type = SwitchableGlazing, Shading Control Type = On If High Glare, Glare Control Is Active = Yes\*|PurchAirWithDaylighting.idf
Adjust electrochromic glazing to just meet daylighting illuminance setpoint|WindowProperty:ShadingControl/Shading Type = SwitchableGlazing, Shading Control Type = MeetDaylightIlluminanceSetpoint\*|
Print an illuminance map|Daylighting:Illuminance Map\*|
Control electric lighting response to daylight illuminance level|Daylighting:Controls/Lighting Control Type = Continuous, Stepped or Continuous/Off, plus other fields; or ….|Daylighting:DELight:Controls/Lighting Control Type = Continuous, Stepped or Continuous/Off, plus other fields|PurchAirWithDaylighting.idf; DElight-Detailed-Comparison.idf
Add a light shelf (with Daylighting:Controls)|DaylightingDevice:Shelf \*|DaylightingDeviceShelf.idf
Add a light shelf (with Daylighting:DELight:Controls)|Daylighting:DELight:Controls|DElightCFSLightShelf.idf
Add a light pipe|DaylightingDevice:Tubular \*|DaylightingDeviceTubular.idf
Model a skylight having a light well|DaylightingDevice:LightWell \*|
Model daylighting through double facade|See "Double Facades: Daylighting through Interior Windows"\*|PurchAirWithDoubleFacadeDaylighting.idf
Add diffusing (translucent) glass|WindowMaterial:Glazing/Solar Diffusing = Yes\*|
Model complex fenestration|Daylighting:DELight:Controls|DElightCFSWindow.idf
Get a radiosity-based interior light inter-reflection calculation|Daylighting:DELight:Controls|DElight-Detailed-Comparison.idf
Find effect on daylighting of **solar reflected** from overhangs, neighboring buildings, etc.|Building/SolarDistribution uses "withReflections" option|
Find visible transmittance effect of dirt on window|WindowMaterial:Glazing/Dirt Correction Factor for Solar and Visible Transmittance\*|

\*Used only with Daylighting:Controls