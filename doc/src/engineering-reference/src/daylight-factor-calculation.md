# Daylight Factor Calculation

Table: Variables in Daylighting Calculations

Mathematical variable|Description|Units||FORTRAN variable
---------------------|-----------|-----||----------------
E~h,sky~|Exterior horizontal illuminance due to light from the sky|lux|GILSK
E~h,sun~|Exterior horizontal illuminance due to light from the sun|lux|GILSU
d~sky~, d~sun~|Interior illuminance factor due to sky, sun related light|-|DFACSK, DFACSU|
w~sky~, w~sun~|Window luminance factor due to sky, sun related light|cd/lm|SFACSK, SFACSU
b~sky~, b~sun~|Window background luminance factor due to sky, sun related light|cd/lm|BFACSK, BFACSU
N|Number of exterior windows in a zone|-|NWD
θ~sky~, φ~sky~|Azimuth and altitude angles of a point in the sky|radians|THSKY, PHSKY
ψ~cs~|Clear sky luminance distribution|cd/m^2^|-
ψ~ts~|Clear turbid sky luminance distribution|cd/m^2^|-
ψ~is~|Intermediate sky luminance distribution|cd/m^2^|-
ψ~os~|Overcast sky luminance distribution|cd/m^2^|-
φ~sun~|Altitude angle of the sun|radians or degrees|PHSUN
γ|Angle between point in the sky and the sun; or angle between vertical and ray from reference point to window element|radians|G
L~z~|Sky zenith luminance|cd/m^2^|ZENL
m|Optical air mass of the atmosphere|m|AM
h|Building altitude|m|Elevation
E~h,k~|Exterior horizontal illuminance for sky type k|lux|-
N~θ~, N~φ~|Number of azimuth, altitude steps for sky integration||NTH, NPH
![](media/image688.png) |Vector from zone origin to  reference point|m|RREF
![](media/image689.png) |Vector from zone origin to window element|m|RWIN
dΩ|Solid angle subtended by window element|steradians|DOMEGA
L~w~|Luminance of a window element as seen from reference point|cd/m^2^|WLUMSK, WLUMSU
L~w,shade~|Luminance of window element with shade in place|cd/m^2^|WLUMSK, WLUMSU
dE~h~|Horizontal illuminance at reference point from window element|lux|-
dx, dy|Size of window element|m|DWX, DWY
D|Distance from reference point to window element|m|DIS
B|Angle between window element's outward normal and ray from reference point to window element|radians|-
![](media/image690.png) |Unit vector from reference point to window element|-|RAY
![](media/image691.png) |Unit vector normal to window element, pointing away from zone|-|WNORM
![](media/image692.png) |Unit vector along window y-axis|-|W21
![](media/image693.png) |Unit vector along window x-axis|-|W23
τ~vis~|Glass visible transmittance|-|TVISB
L|Luminance of sky or obstruction |cd/m^2^|ELUM, -
Φ~FW~|Downgoing luminous flux from a window|lm|FLFW--
Φ~CW~|Upgoing luminous flux from a window|lm|FLCW--
F~1~|First-reflected flux|lm|-
ρ~FW~|Area-weighted reflectance of floor and upper part of walls|-|SurfaceWindow%RhoFloorWall
ρ~CW~|Area-weighted reflectance of ceiling and upper part of walls|-|SurfaceWindow%RhoCeilingWall
E~r~|Average internally-reflected illuminance|lux|EINTSK, EINTSU
A|Total inside surface area of a zone|m^2^|ATOT
ρ|Area-weighted average reflectance of zone interior surfaces|-|ZoneDaylight%AveVisDiffReflect
θ, φ|Azimuth and altitude angle of a sky or ground element|radians|TH, PH
L(θ,φ)|Luminance of sky or ground element at (θ,φ)|cd/m^2^|HitPointLum--
A~w~|Area of glazed part of window|m^2^|Surface%Area
β|Angle of incidence, at center of window,  of light from a sky or ground element|radians|-
T(β)|Glazing visible transmittance at incidence angle β||TVISBR
dΦ~inc~|Luminous flux incident on window from sky or ground element|lm|-
dΦ|Luminous flux from sky or ground element transmitted through window|lm|-
dΦ~FW~, dΦ~CW~|Luminous flux from sky or ground element transmitted through window and goind downwar, upward|lm|-
θ~min~, θ~max~|Azimuth angle integration limits|radians|THMIN, THMAX
φ~w~|Window normal altitude angle|radians|-
Φ~sh~, Φ~unsh~,|Transmitted flux through window with shade, without shade|lm|-
Φ~CW,sh~, Φ~FW,sh~|Upgoing and downgoing portions of transmitted flux through window with shade|lm|-
Φ~CW,unsh~, Φ~FW,unsh~|Upgoing and downgoing portions of transmitted flux through window without shade|lm|-
f|Fraction of hemisphere seen by the inside of window that lies above the window midplane|-|SurfaceWindow%FractionUpgoing
Φ~inc~|Flux incident on glazing from direct sun|lm|-
f~sunlit~|Fraction of glazing that is sunlit |-|SunLitFrac
Φ|Transmitted flux from direct sun|-|-
L~sh~|Luminance of window with shade|cd/m^2^|-
L~b~|Window background luminance|cd/m^2^|BLUM
G|Discomfort glare constant|-|GTOT
G~i~|Discomfort glare constant from window i|-|-
ω|Solid angle subtended by window with respect to reference point|steradians|SolidAngAtRefPt
Ω|Solid angle subtended by window with respect to reference point, modified to take direction of occupant view into account|steradians|SolidAngAtRefPtWtd
N~x~~,~ N~y~|Number of elements in x and y direction that window is divided into for glare calculation|-|NWX, NWY
p(x~R~, y~R~)|Position factor for horizontal and vertical displacement ratios x~R~ and y~R~|-|DayltgGlarePositionFactor
p~H~|Hopkinson position factor|-|DayltgGlarePositionFactor
L~b~|Window background luminance|cd/m^2^|BLUM
E~b~|Illuminance on window background|lm|-
E~r~|Total internally-reflected component of daylight illuminance|lm|-
E~s~|Illuminance setpoint|lm|IllumSetPoint
G~I~|Glare index|-|GLINDX

## Overview

There are three types of daylight factors: interior illuminance factors, window luminance factors, and window background luminance factors. To calculate these factors the following steps are carried out for each hourly sun position on the sun paths for the design days and for representative days during the simulation run period:

Calculate exterior horizontal daylight illuminance from sky and sun for standard (CIE) clear and overcast skies.

Calculate interior illuminance, window luminance and window background luminance for each window/reference-point combination, for bare and for shaded window conditions (if a shading device has been specified), for overcast sky and for standard clear sky.

Divide by exterior horizontal illuminance to obtain daylight factors.

## Interior Illuminance Components

To calculate daylight factors, daylight incident on a window is separated into two components: (1) light that originates from the *sky* and reaches the window directly or by reflection from exterior surfaces; and (2) light that originates from the *sun* and reaches the window directly or by reflection from exterior surfaces. Light from the window reaches the workplane directly or via reflection from the interior surfaces of the room.

For fixed sun position, sky condition (clear or overcast) and room geometry, the sky-related interior daylight will be proportional to the exterior horizontal illuminance, *E~h,sky~*, due to light from the sky. Similarly, the sun-related interior daylight will be proportional to the exterior horizontal solar illuminance, *E~h,sun~*.

## Daylight Factors

The following daylight factors are calculated:

![](media/image694.png)\


![](media/image695.png)\


![](media/image696.png)\


![](media/image697.png)\


![](media/image698.png)\


![](media/image699.png)\


For a daylit zone with *N* windows these six daylight factors are calculated for each of the following combinations of reference point, window, sky-condition/sun-position and shading device:

![](media/image700.png)\


## Sky Luminance Distributions

The luminance distribution of the sky is represented as a superposition of four standard CIE skies using the approach described in (Perez et al. 1990). The standard skies are as follows.

### Clear Sky

The clear sky luminance distribution has the form (Kittler, 1965; CIE, 1973)

![](media/image701.png)\


Here, *L~z~* is the zenith luminance (i.e., the luminance of the sky at a point directly overhead). In the calculation of daylight factors, which are ratios of interior and exterior illumination quantities that are both proportional to *L~z~*, the zenith luminance cancels out. For this reason we will use *L~z~* = 1.0 for all sky luminance distributions.

The various angles, which are defined in the building coordinate system, are shown in Figure 48. The angle, *γ*, between sun and sky element is given by

![](media/image702.png)\


The general characteristics of the clear-sky luminance distribution are a large peak near the sun; a minimum at a point on the other side of  the zenith from the sun, in the vertical plane containing the sun; and an increase in luminance as the horizon is approached.

### Clear Turbid Sky

The clear turbid sky luminance distribution has the form [Matsuura, 1987]

![](media/image703.png)\


### Intermediate Sky

The intermediate sky luminance distribution has the form [Matsuura, 1987]

![](media/image704.png)\


where

![](media/image705.png) ![](media/image706.png)

![](media/image707.png)\


![](media/image708.png)\


![Angles appearing in the expression for the clear-sky luminance distribution.](media/angles-appearing-in-the-expression-for.png)


### Overcast Sky

The overcast sky luminance distribution has the form [Moon & Spencer, 1942]

![](media/image710.png)\


Unlike the clear sky case, the overcast sky distribution does not depend on the solar azimuth or the sky azimuth. Note that at fixed solar altitude the zenith (![](media/image711.png) ) is three times brighter than the horizon (![](media/image712.png) ).

## Direct Normal Solar Illuminance

For purposes of calculating daylight factors associated with beam solar illuminance, the direct normal solar illuminance is taken to be 1.0 W/m^2^. The actual direct normal solar illuminance, determined from direct normal solar irradiance from the weather file and empirically-determined luminious efficacy, is used in the time-step calculation.

## Exterior Horizontal Illuminance

The illuminance on an unobstructed horizontal plane due to diffuse radiation from the sky is calculated for each of the four sky types by integrating over the appropriate sky luminance distribution:

![](media/image713.png)\


where *k* = *cs*, *ts*, *is* or *os*. The integral is evaluated as a double summation:

![](media/image714.png)\


where

![](media/image715.png)\


![](media/image716.png)  and ![](media/image717.png)  were found to give a ![](media/image718.png) accuracy in the calculation of ![](media/image719.png) .

## Direct Component of Interior Daylight Illuminance

The direct daylight illuminance at a reference point from a particular window is determined by dividing the window into an x-y grid and finding the flux reaching the reference point from each grid element. The geometry involved is shown in Figure 58. The horizontal illuminance at the reference point, ![](media/image720.png) , due to a window element is

![](media/image721.png)\


where *L~w~* is the luminance of the window element as seen from the reference point.

The subtended solid angle is approximated by

![](media/image722.png)\


where

![](media/image723.png)\


*CosB* is found from

![](media/image724.png)\


where

![](media/image725.png)\


![](media/image726.png)\


Equation  becomes exact as ![](media/image727.png) and is accurate to better than about 1% for ![](media/image728.png) .

The net illuminance from the window is obtained by summing the contributions from all the window elements:

![](media/image729.png)\


In performing the summation, window elements that lie below the workplane (![](media/image730.png) ) are omitted since light from these elements cannot reach the workplane directly.

![Geometry for calculation of direct component of daylight illuminance at a reference point. Vectors R~ref~, W~1~, W~2~, W~3~ and R~win~ are in the building coordinate system.](media/geometry-for-calculation-of-direct-component.png)


### Unshaded Window

For the unshaded window case, the luminance of the window element is found by projecting the ray from reference point to window element and determining whether it intersects the sky or an exterior obstruction such as an overhang. If *L* is the corresponding luminance of the sky or obstruction, the window luminance is

![](media/image732.png)\


where ![](media/image733.png) is the visible transmittance of the glass for incidence angle *B*.

Exterior obstructions are generally opaque (like fins, overhangs, neighboring buildings, and the building's own wall and roof surfaces) but can be transmitting (like a tree or translucent awning). Exterior obstructions are assumed to be non-reflecting. If *L~sky~* is the sky luminance and *~obs~* is the transmittance of the obstruction (assumed independent of incidence angle), then *L* = *L~sky~~obs~*. Interior obstructions are assumed to be opaque (*~obs~* = 0).

### Shaded Window

For the window-plus-shade case the shade is assumed to be a perfect diffuser, i.e., the luminance of the shade is independent of angle of emission of light, position on shade, and angle of incidence of solar radiation falling on the shade. Closely-woven drapery fabric and translucent roller shades are closer to being perfect diffusers than Venetian blinds or other slatted devices, which usually have non-uniform luminance characteristics.

The calculation of the window luminance with the shade in place, *L~w,sh~*, is described in [Winkelmann, 1983]. The illuminance contribution at the reference point from a shaded window element is then given by Eq. (152) with ![](media/image734.png) .

## Internally-Reflected Component of Interior Daylight Illuminance

Daylight reaching a reference point after reflection from interior surfaces is calculated using the *split-flux* method [Hopkinson et al., 1954], [Lynes, 1968]. In this method the daylight transmitted by the window is split into two parts—a downward-going flux, ![](media/image735.png) (lumens), which falls on the floor and portions of the walls below the imaginary horizontal plane passing through the center of the window (*window midplane*), and an upward-going flux, ![](media/image736.png) , that strikes the ceiling and portions of the walls above the window midplane. A fraction of these fluxes is absorbed by the room surfaces. The remainder, the first-reflected flux, *F~1~*, is approximated by

![](media/image737.png)\


where *ρ~FW~* is the area-weighted average reflectance of the floor and those parts of the walls below the window midplane, and *ρ~CW~* is the area-weighted average reflectance of the ceiling and those parts of the walls above the window midplane.

To find the final average internally-reflected illuminance, *E~r~*, on the room surfaces (which in this method is uniform throughout the room) a flux balance is used. The total reflected flux absorbed by the room surfaces (or lost through the windows) is *AE~r~(1-ρ)*, where *A* is the total inside surface area of the floor, walls, ceiling and windows in the room, and *ρ* is the area-weighted average reflectance of the room surfaces, including windows. From conservation of energy

![](media/image738.png)\


or

![](media/image739.png)\


This procedure assumes that the room behaves like an integrating sphere with perfectly diffusing interior surfaces and with no internal obstructions. It therefore works best for rooms that are close to cubical in shape, have matte surfaces (which is usually the case), and have no internal partitions. Deviations from these conditions, such as would be the case for rooms whose depth measured from the window-wall is more than three times greater than ceiling height, can lead to substantial inaccuracies in the split-flux calculation.

## Transmitted Flux from Sky and Ground

The luminous flux incident on the center of the window from a luminous element of sky or ground at angular position ![](media/image740.png) , of luminance ![](media/image741.png) , and subtending a solid angle ![](media/image742.png)  is

![](media/image743.png)\


The transmitted flux is

![](media/image744.png)\


where *T(β)* is the window transmittance for light at incidence angle *β*. This transmittance depends on whether or not the window has a shade.

For an unshaded window the total downgoing transmitted flux is obtained by integrating over the part of the exterior hemisphere seen by the window that lies above the window midplane. This gives

![](media/image745.png)\


The upgoing flux is obtained similarly by integrating over the part of the exterior hemisphere that lies below the window midplane:

![](media/image746.png)\


where ![](media/image747.png) is the angle the window outward normal makes with the horizontal plane.

For a window with a diffusing shade the total transmitted flux is

![](media/image748.png)\


The downgoing and upgoing portions of this flux are

![](media/image749.png)\


where *f*, the fraction of the hemisphere seen by the inside of the window that lies above the window midplane, is given by

![](media/image750.png)\


For a vertical window (![](media/image751.png) ) the up- and down-going transmitted fluxes are equal:

![](media/image752.png) .

For a horizontal skylight (![](media/image753.png) ):

![](media/image754.png)\


The limits of integration of *θ* in Equations (153), (154) and (155) depend on ![](media/image755.png) . From [Figure 12 - Winkelmann, 1983] we have

![](media/image756.png)\


which gives

![](media/image757.png)\


Thus

![](media/image758.png)\


## Transmitted Flux from Direct Sun

The flux incident on the window from direct sun is

![](media/image759.png)\


The transmitted flux is

![](media/image760.png)\


where T is the net transmittance of the window glazing (plus shade, if present).

For an unshaded window all of the transmitted flux is downward since the sun always lies above the window midplane. Therefore

![](media/image761.png)\


For a window with a diffusing shade

![](media/image762.png)\


## Luminance of Shaded Window

The luminance of a shaded window is determined at the same time that the transmitted flux is calculated. It is given by

![](media/image763.png)\


## Daylight Discomfort Glare

The discomfort glare at a reference point due to luminance contrast between a window and the interior surfaces surrounding the window is given by [Hopkinson, 1970] and [Hopkinson, 1972]:

![](media/image764.png)\


where

*G* = discomfort glare constant

*L~w~* = average luminance of the window as seen from the reference point

*Ω* = solid angle subtended by window, modified to take direction of occupant view into account

*L~b~* = luminance of the background area surrounding the window

By dividing the window into *N~x~* by *N~y~* rectangular elements, as is done for calculating the direct component of interior illuminance, we have

![](media/image765.png)\


where *L~w~(i,j)* is the luminance of element *(i,j)* as seen from the reference point.

Similarly,

![](media/image766.png)\


where *dω(i,j)* is the solid angle subtended by element *(i,j)* with respect to the reference point.

The modified solid angle is

 ![](media/image767.png)

where p is a "position factor" [Petherbridge & Longmore, 1954] that accounts for the decrease in visual excitation as the luminous element moves away from the line of sight. This factor depends on the horizontal and vertical displacement ratios, *x~R~* and *y~R~* (Figure 59),  given by

![](media/image768.png)\


where

![](media/image769.png)\


![Geometry for calculation of displacement ratios used in the glare formula.](media/geometry-for-calculation-of-displacement.png)


The factor *p* can be obtained from graphs given in [Petherbridge & Longmore, 1954] or it can be calculated from tabulated values of *p~H~*, the Hopkinson position factor [Hopkinson, 1966], since ![](media/image771.png) . The values resulting from the latter approach are given in Table 26. Interpolation of this table is used in EnergyPlus to evaluate *p* at intermediate values of *x~R~* and *y~R~*.

Table: Position factor for glare calculation

|-----------------------------------------------------
|**x~R~: Horizontal Displacement Factor**
|**0**|**0.5**|**1.0**|**1.5**|**2.0**|**2.5**|**3.0**|**>3.0**
**y~R~:**|**VerticalDisplacement Factor**|**0**|1.00|0.492|0.226|0.128|0.081|0.061|0.057|0
|**0.5**|0.123|0.119|0.065|0.043|0.029|0.026|0.023|0
|**1.0**|0.019|0.026|0.019|0.016|0.014|0.011|0.011|0
|**1.5**|0.008|0.008|0.008|0.008|0.008|0.006|0.006|0
|**2.0**|0|0|0.003|0.003|0.003|0.003|0.003|0
|**>2.0**|0|0|0|0|0|0|0|0

The background luminance is

![](media/image772.png)\


where *ρ~b~*~~is approximated by the average interior surface reflectance of the entire room and

![](media/image773.png)\


where *E~r~* is the total internally-reflected component of daylight illuminance produced by all the windows in the room and *E~s~* is the illuminance setpoint at the reference point at which glare is being calculated. A precise calculation of *E~b~* is not required since the glare index (see next section) is logarithmic. A factor of two variation in *E~b~* generally produces a change of only 0.5 to 1.0 in the glare index.

### Glare Index

The net daylight glare at a reference point due to all of the windows in a room is expressed in terms of a *glare index* given by

![](media/image774.png)\


where *G~i~* is the glare constant at the reference point due to the *i^th^*^^window