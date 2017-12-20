Window Shade Order
================

**Jason Glazer, GARD Analytics**

 - December 29, 2017
 

## Justification for New Feature ##

When windows shades (including shades, blinds and switchable glazing) are deployed for glare control or daylighing, they are deployed in the order that the windows appear in the input file rather than a user specified order. 

## E-mail and  Conference Call Conclusions ##

Lots of approaches are possible based on discussion between Jason Glazer, Tianzhen Hong, Mike Witte and Dan Macumber.

## Overview ##

Zones often have more than one window. Currently, EnergyPlus applies window shades in the order of their appearance in the input file. The first shade is applied, and if it does not meet the shading control requirement the second shade is applied, and so on. Users do not have a way to specify how multiple shades can be applied in a certain order or in certain sets. 

Currently, each FenestrationSurface:Detailed or Window or GlazedDoor has a field called "Shading Control Name" that references the name of WindowProperty:ShadingControl. Multiple FenestrationSurface:Detailed objects can reference the same WindowProperty:ShadingControl

The WindowProperty:ShadingControl has a field called "Shading Control Type" which has several options but control of window shades occurs with the following options:

- OnIfHighGlare

- MeetDaylightIlluminanceSetpoint (which only applies to SwitchableGlazing)

This also applies whenever the "Glare Control Is Active" field is set to Yes.

To allow the window shade order to be specified by the user, the referencing between FenestrationSurface:Detailed and WindowProperty:ShadingControl will be reversed with WindowProperty:ShadingControl references a list of FenestrationSurface:Detailed objects in the order that they should be deployed. Since the WindowProperty:ShadingControl is no longer a window property, it will be renamed "WindowShadingControl".

## Approach ##

The DayltgInteriorIllum() routine which is in DaylightingManager.cc will be updated to traverse the list of SurfaceWindows in an order established by the new object.

## Testing/Validation/Data Sources ##

Test cases that use multiple windows where the order of the window shading deployment will be constructed to test if the new algorithm works correctly.

## Input Output Reference Documentation ##

Remove "Shading Control Type" field from FenestrationSurface:Detailed, Window and GlazedDoor objects.

Rename the WindowProperty:ShadingControl object to WindowShadingControl


###~~WindowProperty:ShadingControl~~

###WindowShadingControl

Window shading with coverings like drapes, blinds, screens or pull-down shades can be used to reduce the
amount of solar radiation entering the window or reduce daylighting glare. It can also be used to reduce
heat loss through the window (movable insulation). Leaving the window covering open in the winter can
maximize solar heat gain and thereby reduce heating loads.

With ~~WindowProperty:ShadingControl~~ __WindowShadingControl__ —which is referenced by windows and glass doors (ref: FenestrationSurface:
Detailed with Type = Window or GlassDoor)–you specify the type and location of the shading
device, what variable or combination of variables controls deployment of the shading device, and what the
control setpoint is. If the shading device is a blind, you also specify how the slat angle is controlled.

NOTE: ~~WindowProperty:ShadingControl~~ __WindowShadingControl__ does not work with complex fenestration systems. Controlled
complex fenestration systems can be made only with Energy Management Systems objects. Inserting
~~WindowProperty:ShadingControl~~ __WindowShadingControl__ in FenestrationSurface:Detailed while using complex fenestration systems
will be ignored by program.

As shown in Figure 1.36, a shading device can be inside the window (Shading Type = InteriorShade or
InteriorBlind), outside the window (Shading Type = ExteriorShade or ExteriorBlind), or between panes
of glass (Shading Type = BetweenGlassShade or BetweenGlassBlind). The exception is window screens
which can only be outside the window (Shading Type = ExteriorScreen).

When a shading device is present it is either retracted or activated. When it is retracted it covers
none of the window. When it is activated it covers the entire glazed part of the window (but not the
frame). Whether the shading device is retracted or activated in a particular timestep depends on the
control mechanism: see “Shading Control Type,” below. To model a case in which the shading device,
when activated, covers only part of the window you will have to divide the window into two separate
windows, one with the shading device and one without the shading device.

A shading device can also be of a kind in which the optical properties of the glazing switch from one
set of values to another in order to increase or decrease solar or visible transmittance (Shading Type =
SwitchableGlazing).

There are two ways of specifying the actual shading device:

• Specify “Name of Construction with Shading”

This is the name of a window Construction that has the shading device as one of its layers. The
thermal and solar-optical properties of the shading device are given by the shading material referenced
in that Construction (ref: Construction, WindowMaterial:Shade, WindowMaterial:Screen and
WindowMaterial:Blind). To use this method you have to define two Constructions for the window,
one without the shading device and one with it. See Example 1, below.

The Construction without the shading device is referenced in the FenestrationSurface:Detailed input
for the window (see IDF example, below). The Construction with the shading device is referenced
by the window’s ~~WindowProperty:ShadingControl~~ __WindowShadingControl__.

For Shading Type = InteriorShade, InteriorBlind, ExteriorShade, ExteriorScreen and ExteriorBlind
these two Constructions must be identical expect for the presence of the shading layer in the shaded
Construction, otherwise you will get an error message. You will also get an error message if the
Construction referenced by the window has a shading layer.

• Specify the “Material Name of the Shading Device”

This is the name of a WindowMaterial:Shade, WindowMaterial:Screen or WindowMaterial:Blind.
This method can be used with Shading Type = InteriorShade, InteriorBlind, ExteriorShade and
ExteriorBlind. It cannot be used with Shading Type = BetweenGlassShade, BetweenGlassBlind, or
SwitchableGlazing. If Shading Type = InteriorShade or ExteriorShade, then you specify the name
of a WindowMaterial:Shade. If Shading Type = InteriorBlind or ExteriorBlind, then you specify the
name of a WindowMaterial:Blind. If Shading Type = ExteriorScreen, then you specify the name of
a WindowMaterial:Screen. See Example 2, below. This method is simpler to use since you don’t
have to specify two Constructions that differ only by the shading layer.

When this method is used, the program will automatically create a shaded window construction
by adding a shading layer to the outside or inside of the construction corresponding to the window
referencing the ~~WindowProperty:ShadingControl~~ __WindowShadingControl__. The name, created by the program, of this shaded
construction is composed as follows: if the name of the window construction is CCC and the material
name of the shading device is DDD, then the shaded construction name is CCC:DDD:INT for an
interior shading device and CCC:DDD:EXT for an exterior shading device.

This method is the required if you want to add a shading device to a construction brought in from
a WINDOW Data File (ref:Construction:WindowDataFile).

Note that if both “Name of Construction with Shading” and “Material Name of Shading Device”
are specified, the former takes precedence.

Most Shading Control Types allow you to specify a schedule that determines when the control is active.
One example is a control that is active seasonally. For example, to deploy shading only in the summer
when the incident solar is high enough, use Shading Control Type = OnIfHighSolarOnWindow with a
schedule that is 1 during the summer months and 0 otherwise and specify Shading Control Is Scheduled
= YES.

In addition, most Shading Control Types also allow you to specify that glare control is active in addition
to the specified Control Type. For example, you might want to deploy shading when the solar incident on
a window is too high OR the glare from the window is too high. This type of joint control requires that
the window be in a daylit zone, that the maximum allowed glare be specified in the Daylighting object for
the zone, and that Glare Control Is Active = YES in ~~WindowProperty:ShadingControl~~ __WindowShadingControl__.

If Shading Type = InteriorBlind, ExteriorBlind or BetweenGlassBlind you can use ~~WindowProperty:ShadingControl~~ __WindowShadingControl__ to specify how the slat angle of the blind is controlled when the blind is in place.

A special type of ~~WindowProperty:ShadingControl~~ __WindowShadingControl__ is SwitchableGlazing. An example is electrochromic
glazing in which the transmittance and reflectance of the glass is controlled electronically. For example,
you could have electrochromic glazing switch from clear (high transmittance) to dark (low transmittance)
to control solar gain. If you choose the Shading Type = SwitchableGlazing option for ~~WindowProperty:ShadingControl~~ __WindowShadingControl__, the unswitched (clear) state is specified by the Construction referenced by
the window and the switched (dark) state is specified by the Construction referenced by ~~WindowProperty:ShadingControl~~ __WindowShadingControl__for that window. For example, if you specify Shading Type = SwitchableGlazing and
Shading Control Type = OnIfHighSolarOnWindow, then the glazing will switch to the dark state whenever
the solar radiation striking the window exceeds the Setpoint value.

For Shading Type = SwitchableGlazing the state of the window is either clear (unswitched) or dark
(fully switched) for all Shading Control Types except MeetDaylightIlluminanceSetpoint. In this case,
the transmittance of the glazing is adjusted to just meet the daylight illuminance set point at the first
daylighting reference point (see Daylighting). This type of control assures that there is just enough solar
gain to meet the daylighting requirements in a zone, and no more, thus reducing the cooling load.

###Inputs

**Field: Name**

Name of the window shading control. It is referenced by a window (ref: Field: Shading Control Name).

**__Field: Shading Control Sequence Number__**

__If multiple WindowShadingControl objects are used than the order that they deploy the window shades can be set with this field. The first WindowShadingControl should be 1 and subsequent WindowShadingControl should 2, 3, etc.__

**Field: Shading Type**

The type of shading device. The choices are:

- InteriorShade: A diffusing shade is on the inside of the window. (In the shaded Construction the
shading layer must be a WindowMaterial:Shade.)
- ExteriorShade: A diffusing shade is on the outside of the window. (In the shaded Construction the
shading layer must be a WindowMaterial:Shade.)
- BetweenGlassShade: A diffusing shade is between two glass layers. (In the shaded Construction the
shading layer must be a WindowMaterial:Shade.) This shading type is allowed only for double- and tripleglazing. For triple-glazing the shade must be between the two inner glass layers.
- ExteriorScreen: An insect screen is on the outside of the window. (In the shaded Construction the
shading layer must be a WindowMaterial:Screen.)
- InteriorBlind: A slat-type shading device, such as a Venetian blind, is on the inside of the window. (In
the shaded Construction the shading layer must be a WindowMaterial:Blind.)
- ExteriorBlind: A slat-type shading device is on the outside of the window. (In the shaded Construction
the shading layer must be a WindowMaterial:Blind.)
- BetweenGlassBlind: A slat-type shading device is between two glass layers. (In the shaded Construction
the shading layer must be a WindowMaterial:Blind.) This shading type is allowed only for double- and
triple-glazing. For triple-glazing the blind must be between the two inner glass layers.
- SwitchableGlazing: Shading is achieved by changing the characteristics of the window glass, such as by
darkening it.

**Field: Construction with Shading Name**

Name of the window Construction that has the shading in place. The properties of the shading device
are given by the shading material referenced in that Construction (ref: Construction, WindowMaterial:
Shade, WindowMaterial:Screen and WindowMaterial:Blind). For Shading Type = SwitchableGlazing,
this is the name of the Construction that corresponds to the window in its fully-switched (darkest) state.

Specifying “Name of Construction with Shading” is required if Shading Type = BetweenGlassShade,
BetweenGlassBlind, or SwitchableGlazing. For other Shading Types, you may alternatively specify “Material
Name of Shading Device” (see below).

**Field: Shading Control Type**

Specifies how the shading device is controlled, i.e., it determines whether the shading device is “on”
or “off.” For blinds, screens and shades, when the device is “on” it is assumed to cover all of the window
except its frame; when the device is “off” it is assumed to cover none of the window (whether “on” or “off”
the shading device is assumed to cover none of the wall that the window is on).

For switchable glazing, “on” means that the glazing is in the fully-switched state and “off” means that
it is in the unswitched state; for example, for electrochromic glazing, “on” means the glazing is in its
darkest state and “off” means it is in its lightest state.

The choices for Shading Control Type are the following. If SetPoint is applicable its units are shown
in parentheses.

- AlwaysOn: Shading is always on.
- AlwaysOff : Shading is always off.

The following six control types are used primarily to reduce zone cooling load due to window solar
gain.

- OnIfScheduleAllows: Shading is on if schedule value is non-zero. Requires that Schedule Name be
specified and Shading Control Is Scheduled = Yes.

Note: For exterior window screens AlwaysOn, AlwaysOff, and OnIfScheduleAllows are the only valid
shading control types.

- OnIfHighSolarOnWindow: Shading is on if beam plus diffuse solar radiation incident on the window
exceeds SetPoint (W/m2) and schedule, if specified, allows shading.
- OnIfHighHorizontalSolar: Shading is on if total (beam plus diffuse) horizontal solar irradiance exceeds
SetPoint (W/m2) and schedule, if specified, allows shading.
- OnIfHighOutdoorAirTemperature: Shading is on if outside air temperature exceeds SetPoint (C) and
schedule, if specified, allows shading.
- OnIfHighZoneAirTemperature: Shading is on if zone air temperature in the previous timestep exceeds
SetPoint (C) and schedule, if specified, allows shading.
- OnIfHighZoneCooling: Shading is on if zone cooling rate in the previous timestep exceeds SetPoint
(W) and schedule, if specified, allows shading.
- OnIfHighGlare: Shading is on if the total daylight glare index at the zone’s first daylighting reference
point from all of the exterior windows in the zone exceeds the maximum glare index specified in the daylighting
input for zone (ref: Group – Daylighting). Applicable only to windows in zones with daylighting.

Note: Unlike other Shading Control Types, glare control is active whether or not a schedule is specified.

- MeetDaylightIlluminanceSetpoint: Used only with ShadingType = SwitchableGlazing in zones with
daylighting controls. In this case the transmittance of the glazing is adjusted to just meet the daylight
illuminance set point at the first daylighting reference point. Note that the daylight illuminance set
point is specified in the Daylighting:Controls object for the Zone; it is not specified as a ~~WindowProperty:ShadingControl~~ __WindowShadingControl__ SetPoint. When the glare control is active, if meeting the daylight illuminance set
point at the first daylighting reference point results in higher discomfort glare index (DGI) than the specified
zone’s maximum allowable DGI for either of the daylight reference points, the glazing will be further
dimmed until the DGI equals the specified maximum allowable value.

The following three control types can be used to reduce zone heating load during the winter by reducing
window conductive heat loss at night and leaving the window unshaded during the day to maximize solar
gain. They are applicable to any Shading Type except ExteriorScreen but are most appropriate for interior
or exterior shades with high insulating value (“movable insulation”). “Night” means the sun is down and
“day” means the sun is up.

- OnNightIfLowOutdoorTempAndOffDay: Shading is on at night if the outside air temperature is less
than SetPoint (C) and schedule, if specified, allows shading. Shading is off during the day.
- OnNightIfLowInsideTempAndOffDay: Shading is on at night if the zone air temperature in the previous
timestep is less than SetPoint (C) and schedule, if specified, allows shading. Shading is off during the day.
- OnNightIfHeatingAndOffDay: Shading is on at night if the zone heating rate in the previous timestep
exceeds SetPoint (W) and schedule, if specified, allows shading. Shading is off during the day.

The following two control types can be used to reduce zone heating and cooling load. They are
applicable to any Shading Type except ExteriorScreen but are most appropriate for translucent interior or
exterior shades with high insulating value (“translucent movable insulation”).

- OnNightIfLowOutdoorTempAndOnDayIfCooling: Shading is on at night if the outside air temperature
is less than SetPoint (C). Shading is on during the day if the zone cooling rate in the previous timestep is
non-zero. Night and day shading is subject to schedule, if specified.
- OnNightIfHeatingAndOnDayIfCooling: Shading is on at night if the zone heating rate in the previous
timestep exceeds SetPoint (W). Shading is on during the day if the zone cooling rate in the previous
timestep is non-zero. Night and day shading is subject to schedule, if specified.

The following control types can be used to reduce zone cooling load. They are applicable to any Shading
Type except ExteriorScreen but are most appropriate for interior or exterior blinds, interior or exterior
shades with low insulating value, or switchable glazing.

- OffNightAndOnDayIfCoolingAndHighSolarOnWindow: Shading is off at night. Shading is on during
the day if the solar radiation incident on the window exceeds SetPoint (W/m2) and if the zone cooling rate
in the previous timestep is non-zero. Daytime shading is subject to schedule, if specified.
- OnNightAndOnDayIfCoolingAndHighSolarOnWindow: Shading is on at night. Shading is on during
the day if the solar radiation incident on the window exceeds SetPoint (W/m2) and if the zone cooling
rate in the previous timestep is non-zero. Day and night shading is subject to schedule, if specified. (This
Shading Control Type is the same as the previous one, except the shading is on at night rather than off.)
- OnIfHighOutdoorAirTempAndHighSolarOnWindow: Shading is on if the outside air temperature exceeds
the Setpoint ( C ) and if if the solar radiation incident on the window exceeds SetPoint 2 (W/m2).
- OnIfHighOutdoorAirTempAndHighHorizontalSolar: Shading is on if the outside air temperature exceeds
the Setpoint ( C ) and if if the horizontal solar radiation exceeds SetPoint 2 (W/m2).

**Field: Schedule Name**

Required if Shading Control Is Scheduled = Yes. If schedule value > 0 , shading control is active, i.e.,
shading can be on only if the shading control test passes. If schedule value = 0, shading is off whether or
not the control test passes. If Schedule Name is not specified, shading control is assumed to be active at
all times.

**Field: Setpoint**

The setpoint for activating window shading. The units depend on the type of trigger:

- W/m2 for solar-based controls
- W for cooling- or heating-based controls
- Degrees C for temperature-based controls

SetPoint is unused for Shading Control Type = OnIfScheduleAllows, OnIfHighGlare and DaylightIlluminance.

**Field: Shading Control Is Scheduled**

Accepts values YES and NO. The default is NO. Not applicable for Shading Control Type = OnIfHigh-
Glare and should be blank in that case.

If YES, Schedule Name is required and that schedule determines whether the shading control specified
by Shading Control Type is active or inactive (see Schedule Name, above).

If NO, Schedule Name is not applicable (should be blank) and the shading control is unscheduled.
Shading Control Is Scheduled = YES is required if Shading Control Type = OnIfScheduleAllows.

**Field: Glare Control Is Active**

Accepts values YES and NO. The default is NO.

If YES and the window is in a daylit zone, shading is on if the zone’s discomfort glare index exceeds the
maximum discomfort glare index specified in the Daylighting object referenced by the zone. For switchable
windows with MeetDaylightIlluminanceSetpoint shading control, if Glare Control is active, the windows
are always continuously dimmed as necessary to meet the zone’s maximum allowable DGI while providing
appropriate amount of daylight for the zone.

The glare test is OR’ed with the test specified by Shading Control Type. For example, if Glare Control
Is Active = YES and Shading Control Type = OnIfHighZoneAirTemp, then shading is on if glare is too
high OR if the zone air temperature is too high.

Glare Control Is Active = YES is required if Shading Control Type = OnIfHighGlare.

**Field: Shading Device Material Name**

The name of a WindowMaterial:Shade, WindowMaterial:Screen or WindowMaterial:Blind. Required
if “Name of Construction with Shading” is not specified. Not applicable if Shading Type = Between-
GlassShade, BetweenGlassBlind or SwitchableGlazing and should be blank in this case. If both “Name
of Construction with Shading” and “Material Name of Shading Device” are entered the former takes
precedence.

**Field: Type of Slat Angle Control for Blinds**

Applies only to Shading Type = InteriorBlind, ExteriorBlind or BetweenGlassBlind. Specifies how the
slat angle is controlled. The choices are FixedSlatAngle, ScheduledSlatAngle and BlockBeamSolar.

If FixedSlatAngle (the default), the angle of the slat is fixed at the value input for the WindowMaterial:
Blind that is contained in the construction specified by Name of Construction with Shading or is
specified by Material Name of Shading Device.

If ScheduledSlatAngle, the slat angle varies according to the schedule specified by Slat Angle Schedule
Name, below.

If BlockBeamSolar, the slat angle is set each timestep to just block beam solar radiation. If there is
no beam solar on the window the slat angle is set to the value input for the WindowMaterial:Blind that is
contained in the construction specified by Name of Construction with Shading or is specified by Material
Name of Shading Device. The BlockBeamSolar option prevents beam solar from entering the window
and causing possible unwanted glare if the beam falls on work surfaces while at the same time allowing
near-optimal indirect radiation for daylighting.

**Field: Slat Angle Schedule Name**
This is the name of a schedule of slat angles that is used when Type of Slat Angle Control for Blinds
= ScheduledSlatAngle. You should be sure that the schedule values fall within the range given by the
Minimum Slat Angle and Maximum Slat Angle values entered in the corresponding WindowMaterial:Blind.
If not, the program will force them into this range.

**Field: Setpoint 2**

Used only as the second setpoint for the following two-setpoint control types: OnIfHighOutdoorAirTempAndHighSolarOnWindow,
OnIfHighOutdoorAirTempAndHighHorizontalSolar, OnIfHigh-
ZoneAirTempAndHighSolarOnWindow, and OnIfHighZoneAirTempAndHighHorizontalSolar

**__Field: Daylighting Controls Object Name__**

__Reference to the Daylighting:Controls object that provides the glare and illuminance control to the zone.__

**__Field: Multiple Surface Control Type __**

__The field can have one of two options:__

- __Sequential - this means that the following list of fenestration surfaces are controlled individually in the order specified__

- __Group - this means that the entire list is controlled simultaneously and if glare control is needed the entire group of window shades are deployed together a the same time__

**__Field: Fenestration Surface 1 Name __**

__The name of the FenestrationSurface:Detailed, Window, or GlazedDoor objects in order of priority to be controlled. This field can be repeated for each fenestration surface with shading. The object is extensible so that additional fields of fenestration surface names can be added to the object. __


## Outputs Description ##

No output changes are expected.

## Engineering Reference ##

The section on Glare Control Logic in the Daylighting and Window Calculations section of the Engineering Reference will be updated. The current text reads:


**Glare Control Logic**

If glare control has been specified and the glare index at either reference point exceeds a userspecified
maximum value, GI;max, then the windows in the zone are shaded one by one in attempt
to bring the glare at both points below GI;max. (Each time a window is shaded the glare and
illuminance at each reference point is recalculated.) The following logic is used:

1) If there is only one reference point, shade a window if it is unshaded and shading it decreases
the glare, even if it does not decrease the glare below GI;max. Note that if a window has already
been shaded, say to control solar gain, it will be left in the shaded state.

2) If there are two reference points, then:

- If glare is too high at both points, shade the window if it decreases glare at both points.
If glare is too high only at the first point, shade the window if the glare at the first point decreases,
and the glare at the second point stays belowGI;max*.

- If glare is too high only at the second point, shade the window if the glare at the second point
decreases, and the glare at the first point stays below GI;max.

3) Shades are closed in the order of window input until glare at both points is below GI;max,
or until there are no more windows left to shade.


Bullet (3) will be rewritten to read:


3) Shades are closed in the order ~~of window input~~ __specified by order of fenestration objects listed in  the WindowShadingControl oejbect__ until glare at both points is below GI;max, or until there are no more windows left to shade.



## Example File and Transition Changes ##

Signaticant transition changes will be necessary since the referencing between objects is changing as well as the addition of new fields.

## References ##

None.


