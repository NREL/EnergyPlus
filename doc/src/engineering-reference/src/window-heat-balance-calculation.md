# Window Heat Balance Calculation

Table: Fortran Variables used in Window Heat Balance Calculations

Mathematical variable|Description|Units||FORTRAN variable
---------------------|-----------|-----||----------------
N|Number of glass layers|-|nlayer
σ|Stefan-Boltzmann constant||sigma
ε~i~|Emissivity of face i|-|emis
k~i~|Conductance of glass layer i|W/m^2^-K|scon
h~o~, h~i~|Outside, inside air film convective conductance|W/m^2^-K|hcout, hcout
h~j~|Conductance of gap j|W/m^2^-K|hgap
T~o~, T~i~|Outdoor and indoor air temperatures|K|tout, tin
E~o~, E~i~|Exterior, interior long-wave radiation incident on window|W/m^2^|outir, rmir
θ~i~|Temperature of face i|K|thetas
S~i~|Radiation (short-wave, and long-wave from zone internal sources) absorbed by face i|W/m^2^|AbsRadGlassFace
I^ext^~bm~|Exterior beam normal solar irradiance|W/m^2^|BeamSolarRad
I^ext^~dif~|Exterior diffuse solar irradiance on glazing|W/m^2^|-
I^int^~sw~|Interior short-wave radiation (from lights and from reflected diffuse solar) incident on glazing from inside|W/m^2^|QS
I^int^~lw~|Long-wave radiation from lights and equipment incident on glazing from inside|W/m^2^|QL
φ|Angle of incidence|radians|-
A^f^~j~|Front beam solar absorptance of glass layer j|-|-
A~j~^f,dif^, A~j~^b,dif^|Front and back diffuse solar absorptance of glass layer j|-|AbsDiff, AbsDiffBack
A, B|Matrices used to solve glazing heat balance equations|W/m^2^, W/m^2^-K|Aface, Bface
h~r,i~|Radiative conductance for face i|W/m^2^-K|hr(i)
Δθ~i~|Difference in temperature of face i between successive iterations|K|-

## The Glazing Heat Balance Equations

The window glass face temperatures are determined by solving the heat balance equations on each face every time step. For a window with *N* glass layers there are 2*N* faces and therefore 2*N* equations to solve. Figure 95 shows the variables used for double glazing (*N*=2).

![Glazing system with two glass layers showing variables used in heat balance equations.](media/glazing-system-with-two-glass-layers-showing.png)


The following assumptions are made in deriving the heat balance equations:

#. The glass layers are thin enough (a few millimeters) that heat storage in the glass can be neglected; therefore, there are no heat capacity terms in the equations.
#. The heat flow is perpendicular to the glass faces and is one dimensional. See "Edge of Glass Corrections," below, for adjustments to the gap conduction in multi-pane glazing to account for 2-D conduction effects across the pane separators at the boundaries of the glazing.
#. The glass layers are opaque to IR. This is true for most glass products. For thin plastic suspended films this is not a good assumption, so the heat balance equations would have to be modified to handle this case.
#. The glass faces are isothermal. This is generally a good assumption since glass conductivity is very high.
#. The short wave radiation absorbed in a glass layer can be apportioned equally to the two faces of the layer.

The four equations for double-glazing are as follows. The equations for single glazing (*N*=1) and for *N*=3 and *N*=4 are analogous and are not shown.

![](media/image1432.png)\


![](media/image1433.png)\


![](media/image1434.png)\


![](media/image1435.png)\


### Absorbed Radiation

*S~i~* in Equations  to  is the radiation (short-wave and long-wave from zone lights and equipment) absorbed on the *i*^th^ face. Short-wave radiation (solar and short-wave from lights) is assumed to be absorbed uniformly along a glass layer, so for the purposes of the heat balance calculation it is split equally between the two faces of a layer. Glass layers are assumed to be opaque to IR so that the thermal radiation from lights and equipment is assigned only to the inside (room-side) face of the inside glass layer. For *N* glass layers *S~i~*~~is given by

![](media/image1436.png)\


![](media/image1437.png)\


Here

![](media/image1438.png) = exterior beam normal solar irradiance

![](media/image1439.png) = exterior diffuse solar incident on glazing from outside

![](media/image1440.png) = interior short-wave radiation (from lights and from reflected diffuse solar) incident on glazing from inside

![](media/image1441.png) = long-wave radiation from lights and equipment incident on glazing from inside

![](media/image1442.png) = emissivity (thermal absorptance) of the room-side face of the inside glass layer

## Room-Side Convection

The correlation for room-side convection coefficient, ![](media/image1443.png) , is from ISO 15099 section 8.3.2.2.  (Prior to EnergyPlus version 3.1, the value for ![](media/image1444.png)  was modeled using the "Detailed" algorithm for opaque surface heat transfer, e.g. for a vertical surface ![](media/image1445.png) ; see section Detailed Natural Convection Algorithm).  The ISO 15099 correlation is for still room air and is determined in terms of the Nusselt number, ![](media/image1446.png) , where

![](media/image1447.png)\


where,

![](media/image1448.png)  is the thermal conductivity of air, and

![](media/image1449.png)  is the height of the window.

The Rayleigh number based on height, ![](media/image1450.png) , is calculated using,

![](media/image1451.png)\


where,

![](media/image1452.png)  is the density of air

![](media/image1453.png)  is the acceleration due to gravity,

![](media/image1454.png)  is the specific heat of air,

![](media/image1455.png)  is the dynamic viscosity of air, and

![](media/image1456.png)  is the mean film temperature in Kelvin given by,

![](media/image1457.png)\


There are four cases for the Nusselt correlation that vary by the tilt angle in degrees, ![](media/image1458.png) , and are based on heating conditions.  For cooling conditions (where ![](media/image1459.png) ) the tilt angle is complemented so that ![](media/image1460.png)

Case A. ![](media/image1461.png)

![](media/image1462.png)\


Case B. ![](media/image1463.png)

![](media/image1464.png)\


![](media/image1465.png)\


![](media/image1466.png)\


Case C. ![](media/image1467.png)

![](media/image1468.png)\


Case D. ![](media/image1469.png)

![](media/image1470.png)\


The material properties are evaluated at the mean film temperature.  Standard EnergyPlus pyschrometric functions are used for ![](media/image1471.png)  and ![](media/image1472.png) .  Thermal conductivity is calculated using,

![](media/image1473.png) .

Kinematic viscosity is calculated using,

![](media/image1474.png) .

This correlation depends on the surface temperature of the room-side glazing surface and is therefore included inside the window heat balance interation loop.

## Solving the Glazing Heat Balance Equations

The equations are solved as follows:

#. Linearize the equations by defining ![](media/image1475.png) .  For example, Equation  becomes

![](media/image1476.png)\


#. Write the equations in the matrix form ![](media/image1477.png)
#. Use previous time step's values of ![](media/image1478.png) as initial values for the current time step. For the first time step of a design day or run period the initial values are estimated by treating the layers as a simple RC network.
#. Save the ![](media/image1479.png)  for use in the next iteration: ![](media/image1480.png)
#. Using ![](media/image1481.png) , reevaluate the room-side face surface convection coefficient ![](media/image1482.png)
#. Using the ![](media/image1483.png)  to evaluate the radiative conductances ![](media/image1484.png)
#. Find the solution ![](media/image1485.png) by LU decomposition
#. Perform relaxation on the the new ![](media/image1486.png) **:** ![](media/image1487.png)
#. Go to step 4

Repeat steps 4 to 9 until the difference, ![](media/image1488.png) , between values of the ![](media/image1489.png)  in successive iterations is less than some tolerance value. Currently, the test is

![](media/image1490.png)\


If this test does not pass after 100 iterations, the tolerance is increased to 0.2K. If the test still fails the program stops and an error message is issued.

The value of the inside face temperature, ![](media/image1491.png) , determined in this way participates in the zone heat balance solution (see Outdoor/Exterior Convection) and thermal comfort calculation (see Occupant Thermal Comfort).

## Edge-Of-Glass Effects

Table: Fortran Variables used in Edge of Glass calculations

Mathematical variable|Description|Units||FORTRAN variable
---------------------|-----------|-----||----------------
![](media/image1492.png) |Area-weighted net conductance of glazing including edge-of-glass effects|W/m^2^-K|-
A~cg~|Area of center-of-glass region|m^2^|CenterGlArea
A~fe~|Area of frame edge region|m^2^|FrameEdgeArea
A~de~|Area of divider edge region|m^2^|DividerEdgeArea
A~tot~|Total glazing area|m^2^|Surface%Area
h~cg~|Conductance of center-of-glass region (without air films)|W/m^2^-K|-
h~fe~|Conductance of frame edge region (without air films)|W/m^2^-K|-
h~de~|Conductance of divider edge region (without air films)|W/m^2^-K|-
h~ck~|Convective conductance of gap k|W/m^2^-K|-
h~rk~|Radiative conductance of gap k|W/m^2^-K|-
η|Area ratio|-|-
α|Conductance ratio|-|FrEdgeToCenterGlCondRatio, DivEdgeToCenterGlCondRatio

Because of thermal bridging across the spacer separating the glass layers in multi-pane glazing, the conductance of the glazing near the frame and divider, where the spacers are located, is higher than it is in the center of the glass. The area-weighted net conductance (without inside and outside air films) of the glazing in this case can be written

![](media/image1493.png)\


where

*h~cg~* = conductance of center-of-glass region (without air films)

*h~fe~*~~= conductance of frame edge region (without air films)

*h~de~* = conductance of divider edge region (without air films)

*A~cg~* = area of center-of-glass region

*A~fe~* = area of frame edge region

*A~de~* = area of divider edge region

*A~tot~* = total glazing area = ![](media/image1494.png)

The different regions are shown in Figure 96:

![](media/image1495.png)\


Figure 96:  Different types of glass regions.

Equation  can be rewritten as

![](media/image1496.png)\


where

![](media/image1497.png)\


![](media/image1498.png)\


![](media/image1499.png)\


![](media/image1500.png)\


![](media/image1501.png)\


The conductance ratios ![](media/image1502.png)  and ![](media/image1503.png) are user inputs obtained from Window 5. They depend on the glazing construction as well as the spacer type, gap width, and frame and divider type.

In the EnergyPlus glazing heat balance calculation effective gap convective conductances are used to account for the edge-of-glass effects. These effective conductances are determined as follows for the case with two gaps (triple glazing). The approach for other numbers of gaps is analogous.

Neglecting the very small resistance of the glass layers, the center-of-glass conductance (without inside and outside air films) can be written as

![](media/image1504.png)\


where

![](media/image1505.png) convective conductance of the *k^th^*^^gap

![](media/image1506.png) radiative conductance of the *k^th^*^^gap

![](media/image1507.png)\


![](media/image1508.png) emissivity of the faces bounding the gap

![](media/image1509.png) temperature of faces bounding the gap (K)

Equation  then becomes

![](media/image1510.png)\


We can also write ![](media/image1511.png) in terms of effective convective conductances of the gaps as

![](media/image1512.png)\


Comparing Eqs.  and  we obtain

![](media/image1513.png)\


Using ![](media/image1514.png)  gives

![](media/image1515.png)\


This is the expression used by EnergyPlus for the gap convective conductance when a frame or divider is present.

## Apportioning of Absorbed Short-Wave Radiation in Shading Device Layers

If a shading device has a non-zero short-wave transmittance then absorption takes place throughout the shading device layer. The following algorithm is used to apportion the absorbed short-wave radiation to the two faces of the layer. Here *f~1~* is the fraction assigned to the face closest to the incident radiation and *f~2~* is the fraction assigned to the face furthest from the incident radiation.

 ![](media/image1516.png)

Otherwise

![](media/image1517.png)\


## Window  Frame and Divider Calculation

For the zone heat balance calculation the inside surface temperature of the frame and that of the divider are needed. These temperatures are determined by solving the heat balance equations on the inside and outside surfaces of the frame and divider.

Table: Fortran Variables used in Window/Frame and Divider calculations

Mathematical variable|Description|Units||FORTRAN variable
---------------------|-----------|-----||----------------
Q~ExtIR,abs~|IR from the exterior surround absorbed by outside frame surfaces|W|-
Q~IR,emitted~|IR emitted by outside frame surfaces|W|-
Q~conv~|Convection from outside air to outside frame surfaces|W|-
Q~cond~|Conduction through frame from inside frame surfaces to outside frame surfaces|W|-
Q~abs~|Solar radiation plus outside glass IR absorbed by outside of frame|W|-
Q^dif^~abs,sol~|Diffuse solar absorbed by outside frame surfaces, per unit frame face area|W/ m^2^|-
Q^bm^~abs,sol~|Beam solar absorbed by outside frame surfaces, per unit frame face area|W/ m^2^|-
I^dif^~ext~|Diffuse solar incident on window|W/ m^2^|-
I^bm^~ext~|Direct normal solar irradiance|W/ m^2^|-
α^fr^~sol~|Solar absorptance of frame|-|FrameSolAbsorp
R~gl~^f,dif^|Front diffuse solar reflectance of glazing|-|
R~gl~^f,bm^|Front beam solar reflectance of glazing|-|
cos(β~face~)|Cosine of angle of incidence of beam solar on frame outside face ||CosIncAng
Cos(β~h~)|Cosine of angle of incidence of beam solar on frame projection parallel to window x-axis|-|CosIncAngHorProj
Cos(β~v~)|Cosine of angle of incidence of beam solar on frame projection parallel to window y-axis|-|CosIncAngVertProj
f~sunlit~|Fraction of window that is sunlit|-|SunlitFrac
A~f~|Area of frame's outside face (same as area of frame's inside face)|m^2^|-
A~p1~, A~p2~|Area of frame's outside and inside projection faces|m^2^|-
F~f~|Form factor of frame's outside or inside face for IR|-|-
F~p1~, F~p2~|Form factor of frame outside projection for exterior IR; form factor of frame inside projection for interior IR |-|-
E~o~|Exterior IR incident on window plane|W/m^2^|outir
E~i~|Interior IR incident on window plane|W/m^2^|SurroundIRfromParentZone
ε~1~, ε~2~|Outside, inside frame surface emissivity|-|FrameEmis
θ~1~, θ~2~|Frame outside, inside surface temperature|K|FrameTempSurfOut, FrameTempSurfIn
T~o~, T~i~|Outdoor and indoor air temperatures|K|tout, tin
h~o,c~, h~i,c~|Frame outside and inside air film convective conductance|W/m2-K|HOutConv, HInConv
k|Effective inside-surface to outside-surface conductance of frame per unit area of frame projected onto window plane|W/m^2^-K|FrameConductance, FrameCon
S~1~|Q~abs~/A~f~|W/m^2^-K|FrameQRadOutAbs
S~2~|Interior short-wave radiation plus interior IR from internal sources absorbed by inside of frame divided by A~f~|W/m^2^-K|FrameQRadInAbs
η~1~, η~2~|A~p1~/A~f~, A~p2~/A~f~|-|-
H|Height of glazed portion of window|m|Surface%Height
W|Width of glazed portion of window|m|Surface%Width
w~f~, w~d~|Frame width, divider width|m|FrameWidth, DividerWidth
p~f1~, p~f2~|Frame outside, inside projection|m|FrameProjectionOut, FrameProjectionIn
N~h~, N~v~|Number of horizontal, vertical dividers|-|HorDividers, VertDividers
T~o,r~, T~i,r~|Frame outside, inside radiative temperature|K|TOutRadFr, TInRadFr
h~o,r~, h~i,r~|Frame outside, inside surface radiative conductance|W/m^2^-K|HOutRad, HInRad
A|Intermediate variable in frame heat balance solution|K|Afac
C|Intermediate variable in frame heat balance solution|-|Efac
B, D|Intermediate variables in frame heat balance solution|-|Bfac, Dfac

### Frame Temperature Calculation

Figure 97 shows a cross section through a window showing frame and divider. The outside and inside frame and divider surfaces are assumed to be isothermal. The frame and divider profiles are approximated as rectangular since this simplifies calculating heat gains and losses (see "Error Due to Assuming a Rectangular Profile," below).

![Cross section through a window showing frame and divider (exaggerated horizontally).](media/cross-section-through-a-window-showing-frame.png)


### Frame Outside Surface Heat Balance

The outside surface heat balance equation is

![](media/image1519.png)\


where

![](media/image1520.png)  = IR from the exterior surround (sky and ground) absorbed by outside frame surfaces

![](media/image1521.png) = IR emitted by outside frame surfaces

![](media/image1522.png) = convection from outside air to outside frame surfaces

![](media/image1523.png) = conduction through frame from inside frame surfaces to outside frame surfaces

![](media/image1524.png) = solar radiation (from sun, sky and ground) plus IR from outside window surface absorbed by outside frame surfaces (see "Calculation of Absorbed Solar Radiation," below).

The first term can be written as the sum of the exterior IR absorbed by the outside face of the frame and the exterior IR absorbed by the frame's outside projection surfaces.

![](media/image1525.png)\


where *ε~1~* is the outside surface emissivity.

The exterior IR incident on the plane of the window, *E~o~*, is the sum of the IR from the sky, ground and obstructions. For the purposes of the frame heat balance calculation it is assumed to be isotropic. For isotropic incident IR, *F~f~* = 1.0 and *F~p1~* = 0.5, which gives

![](media/image1526.png)\


The IR emitted by the outside frame surfaces is

![](media/image1527.png)\


The convective heat flow from the outside air to the outside frame surfaces is

![](media/image1528.png)\


The conduction through the frame from inside to outside is

![](media/image1529.png)\


Note that *A~f~* is used here since the conductance, *k*, is, by definition, per unit area of frame projected onto the plane of the window.

Adding these expressions for the *Q* terms and dividing by *A~f~* gives

![](media/image1530.png)\


where *S~1~* = *Q~abs~/A~f~*  and

![](media/image1531.png)\


We linearize Eq.  as follows.

Write the first two terms as

![](media/image1532.png)\


and define a radiative temperature

![](media/image1533.png)\


This gives

![](media/image1534.png)\


which, within a few percent, equals

![](media/image1535.png)\


Defining an outside surface radiative conductance as follows

![](media/image1536.png)\


then gives

![](media/image1537.png)\


The final outside surface heat balance equation in linearized form is then

![](media/image1538.png)\


### Frame Inside Surface Heat Balance

A similar approach can be used to obtain the following linearized *inside* surface heat balance equation:

![](media/image1539.png)\


where

![](media/image1540.png)\


![](media/image1541.png)\


and *E~i~* is the interior IR irradiance incident on the plane of the window.

Solving Eqs.  and  simultaneously gives

![](media/image1542.png)\


with

![](media/image1543.png)\


![](media/image1544.png)\


![](media/image1545.png)\


![](media/image1546.png)\


### Calculation of Solar Radiation Absorbed by Frame

The frame outside face and outside projections and inside projections absorb beam solar radiation (if sunlight is striking the window) and diffuse solar radiation from the sky and ground. For the outside surfaces of the frame, the absorbed diffuse solar per unit frame face area is

![](media/image1547.png)\


If there is no exterior window shade,  *I^dif^~ext~* includes the effect of diffuse solar reflecting off of the glazing onto the outside frame projection, i.e.,

![](media/image1548.png)\


The beam solar absorbed by the outside face of the frame, per unit frame face area is

![](media/image1549.png)\


The beam solar absorbed by the frame outside projection parallel to the window x-axis is

![](media/image1550.png)\


Here it is assumed that the sunlit fraction, *f~sunlit~*, for the window can be applied to the window frame. Note that at any given time beam solar can strike only one of the two projection surfaces that are parallel to the window x-axis. If there is no exterior window shade, *I^bm^~ext~* includes the effect of beam solar reflecting off of the glazing onto the outside frame projection, i.e.,

![](media/image1551.png)\


The beam solar absorbed by the frame outside projection parallel to the window y-axis is

![](media/image1552.png)\


Using a similar approach, the beam and diffuse solar absorbed by the *inside* frame projections is calculated, taking the transmittance of the glazing into account.

### Error Due to Assuming a Rectangular Profile

Assuming that the inside and outside frame profile is rectangular introduces an error in the surface heat transfer calculation if the profile is non-rectangular. The percent error in the calculation of convection and emitted IR is approximately 100![](media/image1553.png) , where *L~profile,rect~* is the profile length for a rectangular profile (*w~f~* + *p~f1~* for outside of frame or *w~f~* + *p~f2~* for inside of frame) and *L~profile,actual~* is the actual profile length. For example, for a circular profile *vs* a square profile the error is about 22%. The error in the calculation of absorbed beam radiation is close to zero since the beam radiation intercepted by the profile is insensitive to the shape of the profile. The error in the absorbed diffuse radiation and absorbed IR depends on details of the shape of the profile. For example, for a circular profile vs. a square profile the error is about 15%.

### Divider Temperature Calculation

The divider inside and outside surface temperatures are determined by a heat balance calculation that is analogous to the frame heat balance calculation described above.

## Beam Solar Reflection from Window Reveal Surfaces

This section describes how beam solar radiation that is reflected from window reveal surfaces is calculated. Reflection from outside reveal surfaces—which are associated with the setback of the glazing from the outside surface of the window's parent wall—increases the solar gain through the glazing. Reflection from inside reveal surfaces—which are associated with the setback of the glazing from the inside surface of the window's parent wall—decreases the solar gain to the zone because some of this radiation is reflected back out of the window.

The amount of beam solar reflected from reveal surfaces depends, among other things, on the extent to which reveal surfaces are shadowed by other reveal surfaces. An example of this shadowing is shown in Figure 98. In this case the sun is positioned such that the top reveal surfaces shadow the left and bottom reveal surfaces. And the right reveal surfaces shadow the bottom reveal surfaces. The result is that the left/outside, bottom/outside, left/inside and bottom/inside reveal surfaces each have sunlit areas. Note that the top and right reveal surfaces are facing away from the sun in this example so their sunlit areas are zero.

![Example of shadowing of reveal surfaces by other reveal surfaces.](media/example-of-shadowing-of-reveal-surfaces-by.png)


The size of the shadowed areas, and the size of the corresponding illuminated areas, depends on the following factors:

- The sun position relative to the window
- The height and width of the window
- The depth of the outside and inside reveal surfaces

We will assume that the reveal surfaces are perpendicular to the window plane and that the window is rectangular. Then the above factors determine a unique shadow pattern. From the geometry of the pattern the shadowed areas and corresponding illuminated areas can be determined. This calculation is done in subroutine CalcBeamSolarReflectedFromWinRevealSurface in the SolarShading module. The window reveal input data is specified in the WindowProperty:FrameAndDivider object expect for the depth of the outside reveal, which is determined from the vertex locations of the window and its parent wall.

If an exterior shading device (shade, screen or blind) is in place it is assumed that it blocks beam solar before it reaches outside or inside reveal surfaces. Correspondingly, it is assumed that an interior or between-glass shading device blocks beam solar before it reaches inside reveal surfaces.

Representative shadow patterns are shown in Figure 99 for a window with no shading device, and without and with a frame. The case with a frame has to be considered separately because the frame can cast an additional shadow on the inside reveal surfaces.

The patterns shown apply to both vertical and horizontal reveal surfaces. It is important to keep in mind that, for a window of arbitrary tilt, if the left reveal surfaces are illuminated the right surfaces will not be, and vice versa. And if the bottom reveal surfaces are illuminated the top surfaces will not be, and vice versa. (Of course, for a vertical window, the top reveal surfaces will never be illuminated by beam solar if the reveal surfaces are perpendicular to the glazing, as is being assumed.

For each shadow pattern in Figure 99, equations are given for the shadowed areas ![](media/image1555.png)  and *![](media/image1556.png)  of the outside and inside reveal surfaces, respectively. The variables in these equations are the following (see also* Figure 100):

![](media/expression-for-area-of-shaded-regions-for.png)         = depth of outside reveal, measured from the outside plane of the glazing to the edge of the reveal, plus one half of the glazing thickness.

![](media/expression-for-area-of-shaded-regions-for-001.png)         = depth of inside reveal (or, for illumination on bottom reveal surfaces,  inside sill depth), measured from the inside plane of the glazing to the edge of the reveal or the sill, plus one half of the glazing thickness.

![](media/image1559.png)         = window height for vertical reveal surfaces or window width for horizontal reveal surfaces

![](media/image1560.png)           = vertical solar profile angle for shadowing on vertical reveal surfaces or horizontal solar profile angle for shadowing on horizontal reveal surfaces.

![](media/image1561.png)  = distance from outside (inside) surface of frame to glazing midplane.

![](media/image1562.png)       = depth of shadow cast by top reveal on bottom reveal, or by left reveal on right reveal, or by right reveal on left reveal.

![](media/image1563.png)      = depth of shadow cast by frame.

For simplicity it is assumed that, for the case without a frame, the shadowed and illuminated areas extend into the glazing region. For this reason, ![Expression for area of shaded regions for different shadow patterns: (a) window without frame, (b) window with frame](media/expression-for-area-of-shaded-regions-for.png) and ![Expression for area of shaded regions for different shadow patterns: (a) window without frame, (b) window with frame](media/expression-for-area-of-shaded-regions-for-001.png) are measured from the midplane of the glazing. For the case with a frame, the beam solar absorbed by the surfaces formed by the frame outside and inside projections perpendicular to the glazing is calculated as described in "Window Frame and Divider Calculation: Calculation of Solar Radiation Absorbed by Frame."


![](media/image1564.jpeg)\


![](media/image1565.jpeg)\


![Vertical section through a vertical window with outside and inside reveal showing calculation of the shadows cast by the top reveal onto the inside sill and by the frame onto the inside sill.](media/vertical-section-through-a-vertical-window.png)


The following logic gives expressions for the shadowed areas for all possible shadow patterns. Here:

d1 = ![](media/expression-for-area-of-shaded-regions-for.png)

d2 = ![](media/image1567.png)

P1 = ![](media/image1568.png)

P2 = ![](media/image1569.png)

f1 = ![](media/image1570.png)

f2 = ![](media/image1571.png)

d2prime = ![](media/image1572.png)

d2prime2 = ![](media/image1573.png)

d12 = ![](media/image1574.png)

TanAlpha = ![](media/image1575.png)

A1sh = ![](media/image1555.png)

*A2sh* = ![](media/image1556.png)

L = ![](media/image1559.png)

L1 = average distance to frame of illuminated area of outside reveal (used to calculate view factor to frame).

L2 = average distance to frame of illuminated area of inside reveal (used to calculate view factor to frame).

~~~~~~~~~~~~~~~~~~~~

     IF(window does not have a frame) THEN
          IF(d2prime <= d2) THEN
         IF(d12*TanAlpha <= L) THEN
           A1sh = 0.5*TanAlpha*d1**2
           A2sh = d2prime*L + 0.5*TanAlpha*d12**2 - A1sh
         ELSE  ! d12*TanAlpha > L
           IF(d1*TanAlpha <= L) THEN
             A1sh = 0.5*TanAlpha*d1**2
             A2sh = d2*L - 0.5*TanAlpha*(L/TanAlpha - d1)**2
           ELSE  ! d1*TanAlpha > L
             A1sh = d1*L - (0.5/TanAlpha)*L**2
             A2sh = d2*L
           END IF
         END IF
       ELSE  ! d2prime > d2
         A2sh = d2*L
         IF(d2prime < d1+d2) THEN
           IF(d12*TanAlpha <= L) THEN
             A1sh = L*(d2prime-d2) + 0.5*TanAlpha*d12**2
           ELSE  ! d12*TanAlpha > L
             A1sh = d1*L - 0.5*L**2/TanAlpha
           END IF
         ELSE  ! d2prime >= d1+d2
           A1sh = d1*L
         END IF
       END IF
     ELSE  ! Window has a frame
      f1 = d1-P1
      f2 = d2-P2
      d2prime2 = FrameWidth/TanGamma
      IF(vertical reveal) THEN  ! Vertical reveal
        IF(InsReveal+0.5*GlazingThickness <= P2) d2 = P2 + 0.001
      ELSE                      ! Horizontal
        IF(bottom reveal surfaces may be illuminated) THEN
          ! Bottom reveal surfaces may be illuminated
          IF(InsSillDepth+0.5*GlazingThickness<=P2) d2= P2 + 0.001
        ELSE
          ! Top reveal surfaces may be illuminated
          IF(InsReveal+0.5*GlazingThickness <= P2) d2 = P2 + 0.001
        END IF
      END IF
      IF(d2prime <= f2) THEN
        ! Shadow from opposing reveal does not go beyond inside    !  surface of frame
        IF(d12*TanAlpha <= L) THEN
          A1sh = 0.5*TanAlpha*f1**2
          L1   = f1*(f1*TanAlpha/(6*L)+0.5)
          IF(d2-(d2prime+d2prime2+P2) >= 0.) THEN
            A2sh = (d2prime+d2prime2)*L + &
              0.5*TanAlpha*((d1+d2-d2prime)**2-d1+p2+d2prime2)**2)
            L2   = d2prime2 + 0.5*(d2-(d2prime+d2prime2+P2))
          ELSE  ! d2-(d2prime+d2prime2+P2) < 0.
            ! Inside reveal is fully shadowed by frame and/or         !opposing reveal
            A2sh = f2*L
            L2   = f2
          END IF
        ELSE  ! d12*TanAlpha >= L
          IF((d1+P2)*TanAlpha <= L) THEN
            A1sh = 0.5*TanAlpha*f1**2
            L1 = f1*((f1*TanAlpha)/(6*L) + 0.5)
            IF((d1+P2+d2prime2)*TanAlpha >= L) THEN
              A2sh = f2*L
              L2   = f2
            ELSE  ! (d1+P2+d2prime2)*TanAlpha < L
              A2sh = f2*L - 0.5*(L-(d1+P2)*TanAlpha)**2/TanAlpha &            + d2prime2*(L-(d1+P2+d2prime2/2)*TanAlpha)
              L2 = d2prime2 + (L/TanAlpha - (d1+P2+d2prime2))/3
            END IF
          ELSE  ! (d1+P2)*TanAlpha > L
            L2 = f2
            A2sh = f2*L
            IF(f1*TanAlpha <= L) THEN
              A1sh = 0.5*TanAlpha*f1**2
              L1 = f1*((f1*TanAlpha)/(6*L) + 0.5)
            ELSE  ! f1*TanAlpha > L
              A1sh = f1*L - 0.5*L**2/TanAlpha
              L1 = f1-(L/TanAlpha)/3
            END IF
          END IF
        END IF
      ELSE      ! d2prime > f2 -- Shadow from opposing reveal goes beyond    ! inside of frame
        A2sh = f2*L
        L2 = f2
        IF(d2prime >= d1+d2) THEN
          A1sh = 0.0
          L1   = f1
        ELSE  ! d2prime < d1+d2
          IF(d2prime <= d2+P1) THEN
            IF(f1*TanAlpha <= L) THEN
              A1sh = 0.5*TanAlpha*f1**2
              L1 = f1*((f1*TanAlpha)/(6*L) + 0.5)
            ELSE  ! f1*TanAlpha > L
              A1sh = f1*L - 0.5*L**2/TanAlpha
                  L1   = f1 - (L/TanAlpha)/3
                END IF
              ELSE  ! d2prime > d2+P1
                IF(d12*TanAlpha <= L) THEN
                  A1sh = L*(d2prime-(d2+P1)) + 0.5*TanAlpha*d12**2
                  L1   = (L*(f1-d12/2)-d12*TanAlpha* &                (f1/2-d12/3))/(L-d12*TanAlpha/2)
                ELSE  ! d12*TanAlpha > L
                  A1sh = f1*L - 0.5*L**2/TanAlpha
                  L1   = f1 - (L/TanAlpha)/3
                END IF
              END IF
            END IF
          END IF
          FracToGlassOuts = 0.5*(1.0 - ATAN(FrameWidth/L1)/PiOvr2)
          FracToGlassIns  = 0.5*(1.0 - ATAN(FrameWidth/L2)/PiOvr2)
        END IF  ! End of check if window has frame
~~~~~~~~~~~~~~~~~~~~

The beam solar reflected from a sunlit region of area ![](media/image1576.png) is given by

![](media/image1577.png)\


where

![](media/image1578.png) = reflected solar radiation [W]

![](media/image1579.png) = beam normal irradiance [W/m^2^]

![](media/image1576.png) = sunlit area [m^2^]

![](media/image1580.png) = beam solar angle of incidence on reveal surface

![](media/image1581.png) = solar absorptance of reveal surface

All reflected radiation is assumed to be isotropic diffuse. For outside reveal surfaces it is assumed that ![](media/image1582.png)  goes toward the window and ![](media/image1582.png)  goes to the exterior environment. Of the portion that goes toward the window a fraction ![](media/image1583.png)  goes toward the frame, if present, and ![](media/image1584.png)  goes toward the glazing.

The view factor ![](media/image1583.png)  to the frame calculated by assuming that the illuminated area can be considered to be a line source. Then the area-weighted average distance, ![](media/image1585.png) , of the source to the frame is calculated from the shape of the illuminated area (see above psuedo-code). Then ![](media/image1583.png)  is related as follows to the average angle subtended by the frame of width ![](media/image1586.png) :

![](media/image1587.png)\


For the portion going towards the frame, ![](media/image1588.png) is absorbed by the frame (where ![](media/image1589.png) is the solar absorptance of the frame) and contributes to the frame heat conduction calculation. The rest, ![](media/image1590.png) , is assumed to be reflected to the exterior environment.

If the glazing has diffuse transmittance ![](media/image1591.png) , diffuse front reflectance ![](media/image1592.png) , and layer front absorptance ![](media/image1593.png) , then, of the portion, ![](media/image1594.png) , that goes toward the glazing, ![](media/image1595.png) is transmitted to the zone, ![](media/image1596.png) is absorbed in glass layer ![](media/image1597.png) and contributes to the glazing heat balance calculation, and ![](media/image1598.png) is reflected to the exterior environment.

The beam solar absorbed by an outside reveal surface is added to the other solar radiation absorbed by the outside of the window's parent wall.

For inside reveal surfaces it is assumed that ![](media/image1582.png)  goes towards the window and ![](media/image1582.png)  goes into the zone. Of the portion that goes toward the window a fraction ![](media/image1599.png)  goes toward the frame, if present, and ![](media/image1600.png) goes toward the glazing (![](media/image1601.png)  is calculated using a method analogous to that used for ![](media/image1583.png) ). For the portion going towards the frame, ![](media/image1602.png) is absorbed by the frame and contributes to the frame heat conduction calculation. The rest, ![](media/image1603.png) , is assumed to be reflected back into the zone.

If the glazing has diffuse back reflectance ![](media/image1604.png) , and layer back absorptance ![](media/image1605.png) , then, of the portion ![](media/image1606.png)  that goes toward the glazing, ![](media/image1607.png) is transmitted back out the glazing, ![](media/image1608.png) is absorbed in glass layer ![](media/image1597.png) and contributes to the glazing heat balance calculation, and ![](media/image1609.png) is reflected into the zone.

The beam solar absorbed by an inside reveal surface is added to the other solar radiation absorbed by the inside of the window's parent wall.

## Shading Device Thermal Model

Shading devices in EnergyPlus can be on the exterior or interior sides of the window or between glass layers. The window shading device thermal model accounts for the thermal interactions between the shading layer (shade, screen or blind) and the adjacent glass, and between the shading layer and the room (for interior shading) or the shading layer and the outside surround (for exterior shading).

An important feature of the shading device thermal model is calculating the natural convection airflow between the shading device and glass. This flow affects the temperature of the shading device and glazing and, for interior shading, is a determinant of the convective heat gain from the shading layer and glazing to the zone air. The airflow model is based on one described in the ISO Standard 15099, "Thermal Performance of Windows, Doors and Shading Devices—Detailed Calculations" [ISO15099, 2001]. (Between-glass forced airflow is also modeled; see "Airflow Windows.")

The following effects are considered by the shading device thermal model:

- For interior and exterior shading device: Long-wave radiation (IR) from the surround absorbed by shading device, or transmitted by the shading device and absorbed by the adjacent glass. For interior shading the surround consists of the other zone surfaces. For exterior shading the surround is the sky and ground plus exterior shadowing surfaces and exterior building surfaces "seen" by the window.
- Inter-reflection of IR between the shading device and adjacent glass.
- Direct and diffuse solar radiation absorbed by the shading device.
- Inter-reflection of solar radiation between shading layer and glass layers.
- Convection from shading layer and glass to the air in the gap (or, for between-glass shading, gaps) between the shading layer and adjacent glass, and convection from interior shading layer to zone air or from exterior shading layer to outside air.
- Natural convection airflow in the gap (or, for between-glass shading, gaps) between shading layer and adjacent glass induced by buoyancy effects, and the effect of this flow on the shading-to-gap and glass-to-gap convection coefficients.
- For interior shading, convective gain (or loss) to zone air from gap airflow.

In the following it is assumed that the shading device, when in place, covers the glazed part of the window (and dividers, if present) and is parallel to the glazing. For interior and exterior shading devices it is assumed that the shading layer is separated from the glazing by an air gap. A between-glass shading layer is assumed to be centered between two glass layers and separated from the adjacent glass layers by gaps that is filled with the same gas. If the window has a frame, it is assumed that the shading device does *not* cover the frame.

## Heat Balance Equations for Shading Device and Adjacent Glass

If a window shading device is deployed the heat balance equations for the glass surfaces facing the shading layer are modified, and two new equations, one for each face of the shading layer, are added. Figure 101 illustrates the case of double glazing with an interior shading device.

![Glazing system with two glass layers and an interior shading layer showing variables used in heat balance equations.](media/glazing-system-with-two-glass-layers-and-an.png)


The heat balance equation for the glass surface facing the gap between glass and shading layer (called in the following, "gap") is

![](media/image1611.png)\


where

*τ~sh~* = IR diffuse transmittance of shading device

*ε~sh~* = diffuse emissivity of shading device

*ρ~sh~* = IR diffuse reflectance of shading device ( = 1 - ( *τ~sh~* + *ε~sh~*))

*θ~5~* = temperature of the surface of the shading layer that faces the gap (K).

The term 1 – *ρ~4~ ρ~sh~* accounts for the inter-reflection of IR radiation between glass and shading layer.

The convective heat transfer from glass layer #2 to the air in the gap is

![](media/image1612.png)\


where

*T~gap~* = effective mean temperature of the gap air (K).

*h~cv~* = convective heat transfer coefficient from glass or shading layer to gap air (W/m^2^K).

The corresponding heat transfer from shading layer to gap air is

![](media/image1613.png)\


The convective heat transfer coefficient is given by

![](media/image1614.png)\


where

*h~c~* = surface-to-surface heat transfer coefficient for non-vented (closed) cavities (W/m^2^K)

*v* = mean air velocity in the gap (m/s).

The quantities *h~cv~* and *T~gap~* depend on the airflow velocity in the gap, which in turn depends on several factors, including height of shading layer, glass/shading layer separation (gap depth), zone air temperature for interior shading or outside air temperature for exterior shading, and shading layer and glass face temperatures. The calculation of *h~cv~* and *T~gap~* is described in the following sections.

The heat balance equation for the **shading layer surface facing the gap is

![](media/image1615.png)\


where

*k~sh~* = shading layer conductance (W/m^2^K).

*θ~6~* = temperature of shading layer surface facing the zone air (K).

*S~sh,1~* = solar radiation plus short-wave radiation from lights plus IR radiation from lights and zone equipment absorbed by the gap-side face of the shading layer (W/m^2^K).

The heat balance equation for the shading layer surface facing the zone air is

![](media/image1616.png)\


where

*S~sh,2~* = solar radiation plus short-wave radiation from lights plus IR radiation from lights and zone equipment absorbed by the zone-side face of the shading layer (W/m^2^K).

## Solving for Gap Airflow and Temperature

For interior and exterior shading devices a pressure-balance equation is used to determine gap air velocity, gap air mean equivalent temperature and gap outlet air temperature given values of zone air temperature (or outside temperature for exterior shading), shading layer face temperatures and gap geometry. The pressure balance equates the buoyancy pressure acting on the gap air to the pressure losses associated with gap airflow between gap inlet and outlet [ISO15099, 2001]. The variables used in the following analysis of the interior shading case are shown in Figure 102.

![Vertical section (a) and perspective view (b) of glass layer and interior shading layer showing variables used in the gap airflow analysis. The opening areas A~bot~, A~top~, A~l~, A~r~ and A~h~ are shown schematically.](media/vertical-section-a-and-perspective-view-b-of.png)


### Pressure Balance Equation

The pressure balance equation for airflow through the gap is

![](media/image1618.png)\


Here, *Δp~T~* is the driving pressure difference between room air and gap air. It is given by

![](media/image1619.png)\


where

*ρ~0~* = density of air at temperature *T~0~* (kg/m^3^)

*T~0~* = reference temperature (283K)

*g* = acceleration due to gravity (m/s^2^)

*H* = height of shading layer (m)

*φ* = tilt angle of window (vertical = 90^o^)

*T~gap~*~~= effective mean temperature of the gap air (K)

*T~gap,in~* = gap inlet temperature ( = zone air temperature for interior shading) (K)

The *Δp~B~* term is due to the acceleration of air to velocity *v* (Bernoulli's law). It is given by

![](media/image1620.png)\


where  *ρ* is the gap air density evaluated at *T~gap~* (kg/m^3^).

The *Δp~HP~* term represents the pressure drop due to friction with the shading layer and glass surfaces as the air moves through the gap. Assuming steady laminar flow, it is given by the Hagen-Poiseuille law for flow between parallel plates [Munson et al. 1998]:

![](media/image1621.png)\


where  *μ*  is the viscosity of air at temperature *T~gap~* (Pa-s).

The  *Δp~Z~* term is the sum of the pressure drops at the inlet and outlet openings:

![](media/image1622.png)\


Here, the inlet pressure drop factor, *Z~in~*, and the outlet pressure drop factor, *Z~out~*, are given by

![](media/image1623.png)\


where

*A~eq,in~* = equivalent inlet opening area (m^2^)

*A~eq,out~* = equivalent outlet opening area (m^2^)

*A~gap~* = cross-sectional area of the gap = *sW* (m^2^)

If  T~gap~ > T~gap,in~

![](media/image1624.png)\


If  T~gap~ ≤ T~gap,in~

![](media/image1625.png)\


Here, the area of the openings through which airflow occurs (see Figure 102 and Figure 103) are defined as follows:

A~bot~ = area of the bottom opening (m^2^)

A~top~ = area of the top opening (m^2^)

A~l~ = area of the left-side opening (m^2^)

A~r~= area of the right-side opening (m^2^)

A~h~ = air permeability of the shading device expressed as the total area of openings ("holes") in the shade surface (these openings are assumed to be uniformly distributed over the shade) (m^2^)

Figure 103 shows examples of A~bot~, A~top~, A~l~ and A~r~ for different shading device configurations.  These areas range from zero to a maximum value equal to the associated shade/screen/blind-to-glass cross-sectional area; i.e., A~bot~ and A~top~ ≤ sW,  A~l~ and A~r~ ≤ sH.

![Examples of openings for an interior shading layer covering glass of height H and width W. Not to scale. (a) Horizontal section through shading layer with openings on the left and right sides (top view). (b) Vertical section through shading layer with openings at the top and bottom (side view).](media/examples-of-openings-for-an-interior-shading.png)


Expression for the Gap Air Velocity

 Expressing Equation  in terms of  v  yields the following quadratic equation:

![](media/image1627.png)\


Solving this gives

![](media/image1628.png)\


The choice of the root of the quadratic equation is dictated by the requirement that *v* = 0 if *T~gap,in~* = *T~gap~*.

Gap Outlet Temperature and Equivalent Mean Air Temperature

The temperature of air in the gap as a function of distance, *h*, from the gap inlet (Figure 104) is

![](media/image1629.png)\


where

![](media/image1630.png)\


is the average temperature of the glass and shading layer surfaces facing the gap (K).

*H~0~* = characteristic height (m), given by

![](media/image1631.png)\


where *C~p~* is the heat capacity of air.

The gap outlet temperature is given by

![](media/image1632.png)\


The thermal equivalent mean temperature of the gap air is

![](media/image1633.png)\


![Variation of gap air temperature with distance from the inlet for upward flow.](media/variation-of-gap-air-temperature-with.png)


Solution Sequence for Gap Air Velocity and Outlet Temperature

The routine WinShadeGapFlow is called within the glazing heat balance iterative loop in SolveForWindowTemperatures to determine *v* and *T~gap,out~*. The solution sequence in WinShadeGapFlow is as follows:

At start of iteration, guess *T~gap~* as ((*T~gl~* + *T~sh~*)/2 + *T~gap,in~*)/2. Thereafter use value from previous iteration.

Get still-air conductance, *h~c~*, by calling WindowGasConductance and NusseltNumber.

Get *v* from Equation

Get *h~cv~* from Equation

Get *T~ave~* from Equation

Get *T~gap,out~* from Equation

Get new value of *T~gap~* from Equation

The values of *h~cv~* and *T~gap~* so determined are then used in the window heat balance equations to find new values of the face temperatures of the glass and shading layers. These temperatures are used in turn to get new values of *h~cv~* and *T~gap~* until the whole iterative process converges.

Convective Heat Gain to Zone from Gap Airflow

The heat added (or removed) from the air as it passes through the gap produces a convective gain (or loss) to the zone air given by

![](media/image1635.png)\


This can also be expressed as

![](media/image1636.png)\


where the air mass flow rate in the gap is given by

![](media/image1637.png)\


## Heat Balance Equations for Between-Glass Shading Device

In EnergyPlus shading devices are allowed between the two glass panes of double glazing and between the two inner glass panes of triple glazing. Figure 105 shows the case of a between-glass shading device in double glazing.

![Glazing system with two glass layers and a between-glass shading device showing variables used in the heat balance equations.](media/glazing-system-with-two-glass-layers-and-a.png)


The heat balance equations for the two glass surfaces facing the shading device are the following.

For face #2:

![](media/image1639.png)\


where

![](media/image1640.png)\


![](media/image1641.png)  effective mean air temperature in gap 1 (K)

![](media/image1642.png)  convective heat transfer coefficient from glass or shading layer to gas in gap 1 (W/m^2^K)

For face #3:

![](media/image1643.png)\


where

![](media/image1644.png)\


![](media/image1645.png)  effective mean air temperature in gap 2 (K)

![](media/image1646.png)  convective heat transfer coefficient from glass or shading layer to gas in gap 2 (W/m^2^K)

The heat balance equations for the shading layer faces are:

For face #5:

![](media/image1647.png)\


For face #6:

![](media/image1648.png)\


The convective heat transfer coefficients are given by

![](media/image1649.png)\


where

![](media/image1650.png)      surface-to-surface heat transfer coefficients for gap #1 and #2,  respectively,  when these gaps are non-vented (closed).

![](media/image1651.png) air velocity in the gaps (m/s). It is assumed that the gap widths are equal, so that the velocity of flow in the gaps is equal and opposite, i.e., when the airflow is upward in gap #1 it is downward in gap #2 and vice-versa.

In analogy to the interior or exterior shading device case, the air velocity is determined by solving the following pressure balance equation:

![](media/image1652.png)\


where the driving pressure difference between gap #1 and #2 is

![](media/image1653.png)\


The pressure drops on the right-hand side of this equation are:

![](media/image1654.png)\


where *i* = gap number (1 or 2).

It can be shown that ![](media/image1655.png) . Then, inserting these pressure drop expressions into , we obtain the following expression for the airflow velocity:

![](media/image1656.png)\


The choice of the sign of the square root term is dictated by the requirement that ![](media/image1657.png)  if ![](media/image1658.png) , i.e., ![](media/image1659.png) .

Given ![](media/image1660.png)  we can now calculate ![](media/image1661.png) and ![](media/image1662.png) , which gives ![](media/image1663.png) . The procedure is as follows. We have

![](media/image1664.png)\


where ![](media/image1665.png)  and ![](media/image1666.png)  with ![](media/image1667.png) . Since ![](media/image1668.png)  this gives:

  ![](media/image1669.png)

Similarly,

![](media/image1670.png)\


Solving these simultaneous equations gives:

 ![](media/image1671.png)

Using these in

![](media/image1672.png)\


gives

 ![](media/image1673.png)

with

![](media/image1674.png)\


Similarly, from

![](media/image1675.png)\


we get

![](media/image1676.png)\


The overall solution sequence is as follows.

At start of iteration guess ![](media/image1677.png)  and ![](media/image1678.png) . Then

#. Get ![](media/image1679.png) using ![](media/image1680.png) .
#. Get still-air conductances ![](media/image1681.png) by calling WindowGasConductance and NusseltNumber.
#. Get ![](media/image1660.png)  from Equation
#. Get ![](media/image1682.png) from Equation
#. Get ![](media/image1683.png)
#. Get *H~o,1~*, *H~o,2~*, ![](media/image1684.png) and ![](media/image1685.png) .
#. Get ![](media/image1686.png) ,![](media/image1687.png) from Equations  and

The values ![](media/image1688.png)  and ![](media/image1689.png) are then used in the face heat balance equations to find new values of the face temperatures ![](media/image1690.png) and ![](media/image1691.png) . These are used in turn to get new values of ![](media/image1692.png)  and ![](media/image1693.png) until the whole iterative process converges.

## Airflow Windows

In airflow windows forced air flows in the gap between adjacent layers of glass. Such windows are also known as "heat-extract windows" and "climate windows."

Five configurations of airflow windows are modeled (Figure 106) that depend on the source and destination of forced air. The allowed combinations of Airflow Source and Airflow Destination are:

InsideAir  OutsideAir

InsideAir  InsideAir

InsideAir  ReturnAir

OutsideAir  InsideAir

OutsideAir  OutsideAir

![Gap airflow configurations for airflow windows. From "Active facades," Version no. 1, Belgian Building Research Institute, June 2002.](media/gap-airflow-configurations-for-airflow.png)


A common application of airflow windows is to reduce the zone cooling load by exhausting indoor air through the window, thereby picking up and rejecting heat from the glazing (Figure 106).

Figure 107 shows the variables used in the heat balance equations for forced airflow in a double-glazed window.

![Glazing system with forced airflow between two glass layers showing variables used in the heat balance equations.](media/glazing-system-with-forced-airflow-between.png)


The heat balance equation for the left-hand glass surface facing the gap in Figure 107 is:

![](media/image1696.png)\


The corresponding equation for the right-hand glass surface facing the gap is:

![](media/image1697.png)\


Here,

*T~gap~*~~= effective mean temperature of the gap air (K)

*h~cv~* = convective heat transfer coefficient from glass to gap air (W/m2K).

The convective heat transfer coefficient is given by

![](media/image1698.png)\


where

*h~c~* = glass-to-glass heat transfer coefficient for non-vented (closed) cavity (W/m^2^K)

*v* = mean air velocity in the gap (m/s).

The air velocity is determined by the gap cross-sectional area in the flow direction and the air flow rate, which is an input value that is constant or can vary according to a user-specified schedule:

![](media/image1699.png)\


where

*F* = airflow rate (m^3^/s)

*A~gap~* = gap cross-sectional area in direction of flow (m2)

It is assumed that the airflow is uniform across the width of the window.

The mean temperature of the gap air is given by the following expression, whose derivation follows that for  **for the case of an interior shading device:**

![](media/image1700.png)\


where

![](media/image1701.png)\


*H* = glazing height (m)

![](media/image1702.png)\


*T~gap,in~*~~=  gap air inlet temperature (*T~i~* if the airflow source is indoor air, *T~o~* if the airflow source is outside air) (K)

The outlet air temperature is given by

![](media/image1703.png)\


The equations for glass face #1 and #4 are the same as those for no airflow in the gap (Equations  **and ).**

The convective heat gain to the zone air due to the gap airflow when the airflow destination is indoor air is

![](media/image1704.png)\


where

*C~p,i~*~~= heat capacity of the indoor air (J/kg-K)

*C~p,out~*= heat capacity of the gap outlet air (J/kg-K)

and where the air mass flow rate in the gap is

![](media/image1705.png)\


### Fan Energy

The fan energy used to move air through the gap is very small and is ignored.

### Airflow Window with Between-Glass Shading Device

Figure 108 shows the case of a double-glazed airflow window with a between glass shading device. The heat balance equations in this case are the same as those for the between-glass shading device with natural convection (Figure 105 and following equations) except that now

![](media/image1706.png)\


where *A~gap~* = *sW* is the cross-sectional area of the gap on either side of the shading device. It is assumed that the shading device is centered between the two panes of glass so that the airflow, *F*, is divided equally between the two gaps.

The convective heat gain to the zone air due to the airflow through the two gaps when the airflow destination is indoor air is

![](media/image1707.png)\


where the average temperature of the two outlet air streams is

![](media/image1708.png)\


and

*C~p,ave,out~*~~= heat capacity of the outlet air evaluated at *T~gap,ave,out~* (J/kg-K)

![Airflow window with between-glass shading device showing variables used in the heat balance equations.](media/airflow-window-with-between-glass-shading.png)


## Evacuated Glazing Unit (EGU)

Evacuated glazing Units (EGU) are an emerging technology, developed as a concept some 20-30 years ago, but only now approaching wide-spread commercialization and adoption.

### **Evacuated glazing unit (EGU): cavity thermal conductance**

Thermal Conductance of the space in an evacuated glazing unit (EGU) is the sum of the conductance of the low pressure gas (air) and radiation conductance.

![](media/image1710.png)\


**Conductance of the low-pressure gas**

Conductance of low pressure gasses is calculated using formula by Corrucini (Corruccini, R. (1959)).

![](media/image1711.png)\


![](media/image1712.png)\


![](media/image1713.png)\


Where:

![](media/image1714.png)\


~~~~ = Accommodation coefficients of the gas molecules with the two surfaces.  These values depend on the temperature, surface conditions, etc.  For the present configuration and conditions, it is expected that *a* is approximately 0.5.  If conservative value is needed than value of 1.0 could be used.  With ~~~~ = 0.5, a = 0.333

= Specific heat ratio, ~air~ = 1.40.  Table 2 lists specific heat ratios for other gasses.

*R*= Universal gas constant, *R* = 8,314.462175 J/mol·K

*M*= Molecular Weight, *M*~air~ = 28.97 [mol/g]

*T*= (*T*~1~ + *T*~2~)/2 [K]

*P*= Pressure of the gas [N/m^2^]

From the paper Collins and Robinson (Collins, R., & Robinson, S. (1991)), B is set at approximately 50 for Air, if pressure is given in torr.  Therefore according to Collins and Robinson, for air and approximate conditions of EGU:

*C*~COND~ ≈ 50·P

Where P is in torr (i.e., mm Hg).

*Note: Conversion from Pa to torr is accomplished by multiplying value in torr by 133.28.*

Using formula 2 and assuming *T*~1~ to be 20 ºC and *T*~2~ to be -18 ºC (expected temperatures of glass surfaces in EGU, if one glass surface is low-e and unit is exposed to NFRC standard environmental conditions), and using SI system of units, the B is calculated as 54.4, which is very close to the value of 50, proposed by Collins and Robinson

*C*~COND~ ≈ 54.4·P

We will use exact value, calculated by the formula, so values of will be input into the calculations, which enables more flexible model that can account for special treatment of glass surfaces.

### Radiation conductance

Radiation conductance for the two parallel plates is given by:

![](media/image1715.png)\


Where:

~1~= emissivity of the first facing glass surface, [ - ]

~2~= emissivity of the second facing glass surface, [ - ]

= Stefan-Boltzmann Constant, 5.67 x 10^-8^, [W/(m^2^·K^4^)]

T~1~= Temperature of the first facing glass surface, [K]

T~2~= Temperature of the second facing glass surface. [K]

Assuming glass surface temperatures of 20 ºC and -18 ºC, respectively, the following radiation conductances are obtained for the three different glass emissivities:

Clear Glass (~1~ = ~2~ = 0.84): C~RAD~ = 3.4 W/m^2^K

Hard Coat Low-e (~1~ or ~2~ = 0.15): C~RAD~ = 0.68 W/m^2^K

Soft Coat Low-e (~1~ or ~2~ = 0.04): C~RAD~ = 0.19 W/m^2^K

**Note:** *C~RAD~ of 0.09 is theoretically possible using best low-e technology today (i.e., ~1~ or ~2~ = 0.02).*

**Note:** *Low-e values above are typical values, which will vary by manufacturer.  Some more recent hard coat low-e values are at or below 0.1.*

It should also be noted that values above are based on the fix set of temperatures, while in reality temperatures will depend on the environmental conditions and surface emissivities (e.g., it cannot be expected that clear glass will have same *T~1~* and *T~2~* as low-e glass).

Equation  is precise formulation for two parallel plates at the constant temperature.  Simplified equation under these conditions is given in the form of:

![](media/image1716.png)\


Where:

*T~m~* =mean temperature, [K]

![](media/image1717.png)\


Calculation of the U-factor

![](media/image1718.png)\


![](media/image1719.png)\


![](media/image1720.png)\


Where:

*t*~glass~ = glass thickness; [m]

*k*~glass~ = glass conductivity; *k*~glass~ = 1 W/(m·K)

*R*~glass~ = 0.003 m^2^K/W (for 3 mm glass pane)

*R*~o~ ≈ 0.033 m^2^K/W

*R*~i~ ≈ 0.14 m^2^K/W

U-factor of EGU without any pillars (pretending that this is possible) would be calculated using *C*~RAD~ only.  From above radiation conductance calculations:

*Clear Glass: U = 2.64 W/(m^2^·K) [0.464 Btu/(hr·ft^2^·ºF)]*

*Hard Coat Low-e: U = 0.62 W/(m^2^·K) [0.109 Btu/(hr·ft^2^·ºF)]*

*Soft Coat Low-e:U = 0.19 W/(m^2^·K) [0.034 Btu/(hr·ft^2^·ºF)]*

Adding conductance of the air at 0.001 torr (*C*~COND~ = 0.08 W/(m^2^·K), these values become:

*Clear Glass: U = 2.66 W/(m^2^·K) [0.468 Btu/(hr·ft^2^·ºF)]*

*Hard Coat Low-e: U = 0.68 W/(m^2^·K) [0.120 Btu/(hr·ft^2^·ºF)]*

*Soft Coat Low-e:U = 0.27 W/(m^2^·K) [0.048 Btu/(hr·ft^2^·ºF)]*

In contrast, the U-factor of the same configuration with the air at atmospheric pressure will be (For the space width of 50 m, C~COND~ ≈ 450 W/m^2^K):

*Clear Glass: U = 5.52 W/(m^2^·K) [0.468 Btu/(hr·ft^2^·ºF)]*

*Hard Coat Low-e: U = 5.52 W/(m^2^·K) [0.120 Btu/(hr·ft^2^·ºF)]*

*Soft Coat Low-e:U = 5.52 W/(m^2^·K) [0.048 Btu/(hr·ft^2^·ºF)]*

It is clear that emissivity of the glass surface makes no difference, because of the dominant conductance of the air space.  Also, it is worth noting that the U-factor of such configuration is very close to the U-factor of single glazing.

### Evacuated glazing unit (EGU): Glass support element thermal conductance

Glass panes in the EGU are separated by an array of small support elements.  Typically, these support elements have a cylindrical shape and are often referred to as "pillars."  Typical geometry of the pillar is 0.5-1.0 mm diameter and 0.05 mm (50 m) height.  They are typically spaced 1-2 in. apart in a form of square or staggered matrix.

### Calculation of the U-factor

The conductance of these elements can be measured or numerically modeled to determine accurate thermal performance.  Approximate method also exists and is based on the combination of modeling and analytical work for the conduction through small cylinders in contact with infinite parallel plates with thickness much larger than cylinder height.

The following formula can be used to determine conductance of the single pillar, *C*~p~ (Collins and Fisher-Cripps 1991):

![](media/image1721.png)\


Where:

*k*= conductivity of glass, W/(m·K)

*a*= radius of the pillar (m)

*h*= pillar height, m

For the square array of support pillars (Collins and Fischer-Cripps 1991) proposes the following formula for their conductance, *C*~pa~:

![](media/image1722.png)\


Where:

= pillar spacing, m

This formula is approximate and does not include effect of the conductivity of the pillar, but it gives good approximation for common materials used in this technology, since conductivity of the pillar does not play substantial role for non-insulating pillars (where "non-insulating" would mean that conductivity of the pillar is equal or higher than the conductivity of the glass pane.

The U-factor of the EGU with support pillars is then:

![](media/image1723.png)\


Where:

![](media/image1724.png)\


### References

Collins, R., & Fischer-Cripps, A. 1991. "Design of Support Pillar Arrays in Flat Evacuated Windows.". Aust. J. Phys.

Collins, R., & Robinson, S. 1991. *"Evacuated Glazing".* Solar Energy. Vol. 47, No. 1, pp. 27-38.

Corruccini, R. 1959. "Gaseous Heat Conduction at Low Pressures and Temperatures". Vacuum. Vol. 7-8, pp.19-29.

## Thermal Performance of Deflected Insulated Glazing Unit (IGU)

### Introduction

Deflection of insulated glazing unit (IGU) can result in thermal performance degradation or improvement due to the reduction or increase of gap space width. Convection of the gas fill is affected by changed gap space and due to modified convection pattern and shorter or longer thermal path at the center of the glazing unit can result in increased or decreased thermal performance. For the most part, U-factor is mostly affected as a direct result of changed thermal performance; however note that solar heat gain through the window (SHGC) can also be affected because of the effect of inward flowing fraction of absorbed solar radiation, which is affected by thermal performance of the IGU.

Deflection in sealed IGU is caused by the difference in gas pressure in IGU gap vs. outdoor/indoor pressure. Indoor and outdoor pressure can be considered equal, since indoor building environment is in pretty good contact with outdoor environment. We will call this pressure an atmospheric pressure, *P~a~*. The differences in pressure between atmospheric and gap pressure is due to several factors, listed here:

#. Difference in atmospheric pressure between IGU fabrication location and end use location
#. Difference in temperature during fabrication and actual operating conditions for the glazing. It should be noted that initial temperature can be higher than ambient temperature during fabrication process, due to elevated sealant temperatures, which can raise local temperatures within the IGU.
#. Unbalanced gas fill leakage through the sealants, resulting in lower gap pressure and inward deflection.
#. Wind or static load pressure

Effects 1 and 2 will be modeled using equations presented below, while effect 3 does not have credible mathematical model. However, cumulative deflection, resulting from all three effects can be measured in the field and its effect on thermal performance can be modeled by specifying center glazing deflection.

Wind or static load pressure effects on deflection is not included in this model at this time, but will be considered for future versions.

Recognizing that indoor and outdoor air pressure could be different, such as in hot box test environment, future plans for the extension of the model will include option to specify different values for indoor and outdoor pressure. Another future improvement to the model will also include linking certain air gaps with indoor or outdoor environment, meaning that respective pressures in linked spaces will be set to equal.

### Mathematical Model

Mathematical model described in detail here is based on the research work by Bernier and Bourret (1997) and Timoshenko and Woinowsky-Krieger (1959). Bernier and Bourret (1997) of the Ecole Polytechnique Montréal adopted Timoshenko and Woinowsky-Krieger (1959) model for calculating flat plate deflection subjected to the differential pressure field (static), while their original contribution was to develop correlations for changes in thermal performance, based on IGU deflection at the center of glazing location. In addition to adopting Bernier and Bourret (1997) model here, we have also developed model for calculating change in thermal performance of deflected units when this deflection is measured in the field. Therefore, the mathematical formulation, presented here is divided into two sections; 1) calculation of the deflection and resulting thermal performance caused by pressure and temperature effects and 2) calculation of the thermal performance of the IGU when the deflection is measured.

### Calculation of the deflection and thermal performance caused by pressure and temperature effects

If coordinate system is set as shown in Figure 109 and Figure 110, it is possible to calculate deflection distribution at each point of pane by using following equation:

![Deflection Coordinate System - 2D](media/deflection-coordinate-system-2d.png)


![](media/image1726.png)\


**Where,**

![](media/image1727.png)\


Where,

*E* = Young's modulus (7.2 x 10^10^) [Force per unit Area; SI: Pa, IP: psi]

*t* = thickness of glazing pane [Length; SI: m, IP: in.]

 = poison's ratio (0.22 for glass) [Non-Dimensional]

![Deflection Coordinate System - 3D](media/deflection-coordinate-system-3d.png)


Δ*P~i~* = *P~gap(i)~* - *P~gap~~(i-1)~* (for i-th pane) [Force per unit Area; SI: Pa, IP: psi]

Δ*P~i~* = *P~gap(1)~* - *P~a~* (first pane) [Force per unit Area; SI: Pa, IP: psi]

Δ*P~i~* = *P~a~ - P~gap(n-1)~*(last pane) [Force per unit Area; SI: Pa, IP: psi]

Where,

*P~a~ =* atmospheric pressure. [Force per unit Area; SI: Pa, IP: psi]

![](media/image1729.png)\


Where,

*P~ini~* = Initial pressure. Applies to all gaps as a single value (input data - measured or otherwise) [Force per unit Area; SI: Pa, IP: psi]

*T~ini~* = Initial temperature. Applies to all gaps as a single value (input data - measured or otherwise) [Degree Temperature; SI: K, IP: R]

*V~ini(i)~* = Initial volume for i-th gap. [Length\*Length\*Length; SI: m^3^, IP: in^3^]

![](media/image1730.png)\


Where,

*L~i~* = non-deflected glazing gap width (for i-th gap) [Length; SI: m, IP: in.]

*W* = IGU width [Length; SI: m, IP: in.]

*H* = IGU height [Length; SI: m, IP: in.]

*T~gap~~(i)~* = temperature of the gap between two glass panes (calculated using center of glazing thermal calculation algorithm, as described in ISO 15099 (ISO 2003). This value is first calculated using non-deflected state and is recalculated after the resulting deflection is calculated. This process is repeated until temperature at next iteration does not differ by more than 0.1 ºC

*V~gap~~(i)~* = volume of the IGU gap in deflected state [Lentgh\*Length\*Length; SI: m^3^, IP: in^3^]

![](media/image1731.png)\


Where,

![](media/image1732.png)  is mean deflection value for i-th pane. [Length; SI: m, IP: in.]

Deflection of each pane can be positive or negative and is done solely to establish reference. Current frame of reference is that positive deflection means that pane is deflecting towards left side, while negative deflection means that pane is deflecting towards right side . Whether the deflection is in the direction of reducing the gap width or increasing it, it will be the result of pressure difference, as described in . When pressure in the glazing unit is higher than surrounding environmental pressure, the deflection will be towards increasing gap width (i.e., ballooning), while the opposite situation will result in decreasing gap width (i.e., vacuuming)

![Deflection Direction Convention](media/deflection-direction-convention.png)


The important part of calculating deflection of the IGU is to determine mean deflection value for each glazing pane. Mean deflection value is used to calculate gap volume in deflected state . Mean deflection of glazing pane can be calculated by integrating :

![](media/image1734.png)\


Which is identical with the following expression:

![](media/image1735.png)\


and because integral of sin(x) is equal with –cos(x), above equation will become:

![](media/image1736.png)\


Finally, because ![](media/image1737.png)  and ![](media/image1738.png)  values are always equal to -1 for the given range of m and n, above equation will became:

![](media/image1739.png)\


After calculating mean pane deflection the following equation is used to calculate mean gap width:

![](media/image1740.png)\


*Where,*

*L~r~~(i)~ = Mean gap "i" width after incorporating glazing deflection. This mean gap width is used to recalculate thermal performance of deflected IGU.*

![](media/image1741.png)  *****= mean glazing deflection for each pane "i".*

Calculation of the deflection at the center of glazing and mean glazing deflection for each pane is an iterative process, where the initial temperature distribution is calculated for non-deflected state, then deflection is calculated based on this temperature distribution, new temperature distribution is calculated for this deflected state, then temperatures from previous iteration are compared to the current iteration and the process is repeated until the difference is no larger than 0.1 ºC.

At the end of calculations, program will calculate and return maximum deflection value for each pane (i.e., center of glazing deflection). If we label maximum deflection of each pane as *L~D~~(i),~~max~*, we can calculate this value by substituting *x*=*W*/2 and *y*=*H*/2 in equation  to determine deflection at the center point. Therefore,

![](media/image1742.png)\


For glazing systems with more than two glazing layers, meaning multiple gas filled gaps, the deflection will be calculated for each glazing pane assuming that the pressure in a gap is independent from each other and calculated separately, unless spaces are "linked" together (e.g., stretched film middle glazing that has hole for equalizing pressure).

### Non-Linked Gaps in 3 or more glazing layer system:

The procedure shown above generally applies to the 3 or more layer glazing system, with the exception that neighboring pressures are no longer *P~a~*, but rather could be *P~a~* on one side and *P~gap~* on the other, or have *P~gap~* on both sides, as shown in Figure 111 for gap "*i*". Center of glazing thermal calculation will determine new temperature distribution, after deflection is calculated for each glazing and will be used to determine new *P~gap~*, as per the procedure above.

### Linked Gaps in 3 or more glazing layer system:

When one or more gaps are linked together, their pressure is assumed to be identical (e.g., in triple glazing IGU *P~gap,1~* = *P~gap,2~*.)  This pressure is calculated from temperatures of bounding glazing for linked gaps (e.g., for triple glazing IGU, glazing 1 and 3) and using neighboring pressures outside of those bounding glazing (e.g., for triple glazed IGU, *P~a~* on both sides).

*Note: This feature is not implemented in WINDOW 7.1. It is considered for future enhancements to the program.*

### Gap(s) Linked to Indoor or Outdoor Environment:

If one or more glazing gaps are linked to either indoor or outdoor environment its pressure is fixed to *P~a~*. In combination situations, such as two or more gaps linked together with one of them being linked to indoor or outdoor environment, they will all have fixed pressure of *P~a~*.

### Calculation of the thermal performance caused by measured deflection

When deflection is measured, it is normally measured at the point of maximum deflection. Maximum deflection occurs at center of the IGU (at *W*/2 and *H*/2).

Measured value is typically gap width at the point of maximum deflection, which we can label *L~G~~(i)~*. For i-th measured gap the width is equal to:

![](media/image1743.png)\


![Sketch of the non-symetrically Deflected Glazing Panes](media/sketch-of-the-non-symetrically-deflected.png)


If we label ratio of mean deflection and maximum deflection as R~(i)~, then:

![](media/image1745.png)\


Important thing to note is that ratios (![](media/image1747.png) ) for all gaps in glazing system are equal.

![](media/image1748.png)\


Replacing  and  into  the following equation is obtained:

![](media/image1749.png)\


Combining  with  we get the following expression for the mean gap width:

![](media/image1750.png)\


Number of equations given in expression  is equal to n-1, where n is number of panes. Therefore, we need one more equation to complete the system of equations that would allow us to solve for all independent variables. To get the last equation we can rewrite  in slightly different manner:

![](media/image1751.png)\


Where coefficient K combines all constant terms, while ![](media/image1752.png)  is given by  and ![](media/image1753.png)  is calculated by,  and . Summing over all deflections, ![](media/image1754.png)  the following equation is obtained:

![](media/image1755.png)\


Note that sum of all ![](media/image1756.png)  is equal to zero since outside pressure is equal to inside. Therefore, the remaining equation that completes the set of equations is:

![](media/image1757.png)\


### Solving system of equations

In order to solve system of equations we will present  in slightly different manner:

![](media/image1758.png)\


Which in developed form will look like this:

![](media/image1759.png)\


![](media/image1760.png)\


![](media/image1761.png)\


![](media/image1762.png)\


In order to express each ![](media/image1763.png)  as dependence from ![](media/image1764.png)  (deflection of inside/last pane) we will need to make sum from first to last, then from second to last, third to last and so on. This procedure will create following set of equations:

![](media/image1765.png)\


![](media/image1766.png)\


![](media/image1767.png)\


![](media/image1768.png)\


Now replacing this set of equations back to :

![](media/image1769.png)\


Which solving by ![](media/image1770.png)  leads to the following equation:

![](media/image1771.png)\


Calculating ![](media/image1772.png)  value from this equation and substituting it in  will enable calculation of the deflection of remaining panes.

### References

Arasteh, D.K., J.C. Kohler and B.T. Griffith. Draft 2009. Modeling Windows in EnergyPlus with only U, SHGC, and optionally VT. LBNL report.  Full reference to be determined.

Arasteh, D. J. 2009. Modeling Windows in EnergyPlus with only U, SHGC, and optionally VT. Lawrence Berkeley National Laboratory.

Arasteh, D.K., M.S. Reilly and M.D. Rubin. 1989. A versatile procedure for calculating heat transfer through windows. American Society of Heating, Refrigeration and Air-Conditioning Engineers, ASHRAE Transactions, Vol. 95, Pt. 2.

Bernier, M., & Bourret, B. January 1997. "Effects of Glass Plate Curvature on the U-Factor of Sealed Insulated Glazing Units". Atlanta, GA: ASHRAE Transactions. Vol. 103, Pt. 1. American Society for Heating, Refrigerating and Air-Conditioning Engineers.

Collins, R., & Fischer-Cripps, A. 1991. "Design of Support Pillar Arrays in Flat Evacuated Windows.". Aust. J. Phys.

Collins, R., & Robinson, S. 1991. "Evacuated Glazing". Solar Energy. Vol. 47, No. 1, pp. 27-38.

Corruccini, R. (1959). "Gaseous Heat Conduction at Low Pressures and Temperatures". Vacuum. Vol. 7-8, pp.19-29.

Finlayson, E.U., D.K. Arasteh, C. Huizenga, M.D. Rubin and M.S. Reilly. 1993. WINDOW 4.0: documentation of calculation procedures. Lawrence Berkeley National Laboratory report no. LBL-33943.

ISO. 2003. ISO 15099:2003. Thermal performance of windows, doors, and shading devices – Detailed calculations. International Organization for Standardization.

Klems, J. H. 1994A. "A New Method for Predicting the Solar Heat Gain of Complex Fenestration Systems: I. Overview and Derivation of the Matrix Layer Calculation.". ASHRAE Transactions. 100 (pt.1): 1073-1086.

Klems, J. H. 1994B. "A New Method for Predicting the Solar Heat Gain of Complex Fenestration Systems: II. Detailed Description of the Matrix Layer Calculation.". ASHRAE Transactions. 100 (pt.1): 1073-1086.

Klems, J. H. 1995. "Measurements of Bidirectional Optical Properties of Complex Shading Devices.". ASHRAE Transactions. 101 (pt 1; Symposium Paper CH-95-8-1 (RP-548)): 791-801.

Klems, J. H. 1996. "A Comparison between Calculated and Measured SHGC for Complex Glazing Systems.". ASHRAE Transactions. 102 (Pt. 1; Symposium Paper AT-96-16-1): 931-939.

Klems, J. H. 1996. "Calorimetric Measurements of Inward-Flowing Fraction for Complex Glazing and Shading Systems.". ASHRAE Trans. 102(Pt. 1; Symposium Paper AT-96-16-3): 947-954.

Munson, B.R, D.F. Young and T.H. Okiishi. 1998. "Fundamentals of Fluid Mechanics," Third Edition Update, John Wiley & Sons, Inc.

Papamichael, K. J. 1998. "Determination and Application of Bidirectional Solar-Optical Properties of Fenestration Systems.". Cambridge, MA: 13th National Passive Solar Conference.

Simmler, H., U. Fischer and F. Winkelmann. 1996. Solar-Thermal Window Blind Model for DOE-2. Lawrence Berkeley National Laboratory, Simulation Research Group internal report, (unpublished).

Timoshenko, S., & Kreiger-Woinowsky, S. 1959. "Theory of Plates and Shells" 2nd Edition. McGraw-Hill Company.

## Equivalent Layer Fenestration Model

The section describes the equivalent layer fenestration optical and thermal model. The Equivalent Layer fenestration model can have four types of attachments: drapes, venetian blinds, roller blinds and insect screens. In this model shading layers are assumed to be uniform and can be represented by an equivalent homogenous layer that has spatially-averaged "effective" optical and thermal properties (ASHRAE 1311-RP). Likewise, venetian blinds can be characterized using effective optical and thermal properties. When solar radiation strikes a window surface some fraction of the incident solar radiation passes unobstructed through openings in a shading layer and the remaining fraction is intercepted by the structure of the layer. The intercepted radiation is partly absorbed, partly reflected and partly transmitted. These reflected and transmitted components of the scattered solar radiation are assumed to be uniformly diffuse. Shading layers, because of their openness, generally transmit longwave radiation, and the effective infrared properties of shades account for that. Using effective optical properties and a beam/diffuse split of solar radiation at each layer, the equivalent layer approach can represent multi-layer systems. This representation provides virtually unlimited flexibility to combine different types of shading layers in a fenestration. The equivalent layer window model requires a few set of optical data to characterize a particular layer and this set of data is used to calculate effective layer properties. For instance, the effective solar optical properties of a venetian blind can be calculated as a function of slats optical properties and geometry. Also, it is possible to adjust slat angle at each time step in response to the changing angular position of the sun. Moreover, the model provides control strategies as a function of slat angle that can be changed at each time step as needed. Likewise, effective properties of a pleated drape are calculated as a function of fabric properties and a specified value of fullness. The only input data needed to fully characterize drapery fabrics, roller blinds and insect screens are material openness as area fraction, and the transmittance and reflectance at normal incidence. Shade openness area fraction is the same as the beam-beam transmittance at normal incidence. In multilayer fenestration, each layer is separated by a gap. A gap in equivalent layer model is defined by specifying the fill gas and the gap spacing. Currently five gas types are allowed: Air, Argon, Xenon, Krypton and Custom. The convective heat transfer coefficient in a gap is calculated depending on the spacing, the temperatures of the layers and the fill gas properties. Equivalent-layer concept – offers wide range of multiple glazing and shading layers combination and can simulate multi-layer complex fenestration systems. The effective layer properties of venetian blinds, pleated drapes, roller blinds, and insect screens are calculated from geometric layer models and material properties. A set of empirical correlations for estimating off-normal material properties were developed under ASHRAE research project (ASHRAE 1311-RP).

### The Equivalent Layer Analysis

The equivalent layer windows system is treated as a series of parallel layers separated by gaps as shown in Figure 113. This multi-layer structure has been used in several computer programs and the underlying theory has been documented (ASHRAE 1311-RP).

![Multi-layer fenestration analysis structure (ASHRAE 1311-RP)](media/multi-layer-fenestration-analysis-structure.png)


Equivalent layer fenestration model uses two-step analysis. First, the flux of absorbed solar radiation at each layer, S~i~, caused by the incident radiation flux, I~sol~, is determined using net radiation analysis. Second, an energy balance is applied at each layer, accounting for heat transfer and the known set of absorbed solar radiations S~i~ values, in order to solve for the set of layer temperatures, T~i~, and the corresponding heat fluxes. The fenestration model also accounts for the diathermanous shade layers in the longwave radiant exchange. The latter can be significant for shading layers. Glass is considered opaque with respect to longwave radiation.

### Equivalent Layer Optical Model

The multilayer optical model is based on an algorithm originally developed by Edwards (1977) and extended by Wright and Kotey (2006). The algorithm models the interaction of incident solar radiation with a glazing system composed of any number of parallel, planar, specular glazing layers. The shading layers scatter portion of the incident solar radiation diffusely, and the model tracks the beam and diffuse components of solar radiation as they interact with a multi-layer system of glazing and shading layers. The conceptual arrangement for tracking beam and diffuse solar flux components is illustrated in Figure 114. Analysis yields beam-beam, beam-diffuse and diffuse-diffuse fluxes, providing full detail concerning the quantities of reflected, transmitted and absorbed radiation.

![Solar analysis of the multi-layer glazing/shading system showing beam and diffuse fluxes (ASHRAE 1311-RP)](media/solar-analysis-of-the-multi-layer.png)


The beam and diffuse characterization of solar radiation demands an expanded set of solar optical properties (Wright and Kotey 2006). The quantities of interest for single layer are shown in Figure 115.

Where,

 **=transmittance of a glazing or a shading layer, (-)

 **=reflectance of a glazing or a shading layer, (-)

 **=thermal emissivity of a glazing or a shading layer, (-)

 **=incident angle, (degree)

 **=slat angle, (degree)

 **=profile angle, (degree)

*J*=radiosity of a layer surface, (W/m2)

*T*=temperature of a layer, (C)

Subscripts

*f*=front side of a layer

*b*=back side of a layer

*bb*=beam-beam optical property. Represents a fraction of the beam radiation incident at a given layer and leaves the layer without being scattered.

*bd*=beam-diffuse optical property. Represents a fraction of the beam radiation incident at a given layer and leaves the layer diffusely

*dd*=diffuse-diffuse optical property. Represents a diffuse radiation incident at a given layer and leaves the layer as diffuse

*i=layer index, (-)*

Superscripts

*m*=represents material property (e.g., fabric material)

*str*=represents an apparent material structure property of roller blinds

*w*=represents an apparent wire property

![Twelve solar properties assigned at each layer (ASHRAE 1311-RP)](media/twelve-solar-properties-assigned-at-each.png)


Each glazing or shading layer, in general, require eleven set of solar properties (Wright and Kotey 2006). Only beam-beam properties are needed for common glazing layers. Each shading layer is represented by a few set of component properties and geometries that are converted to the full set of layer optical and thermal properties by equivalent layer models (ASHRAE 1311-RP). The models also make account for off-normal incidence of solar radiation and can calculate the diffuse component from normal incidence values using numerical integration.

### Equivalent Layer Thermal Model

A surface energy balance is applied at each layer and the resulting set of equation is solved for layer temperatures and heat transfer fluxes. A schematic drawing of the multi-layer illustration and variables designation is shown in Figure 116. The net radiation formulation based on the radiosities, Jf,i and Jb,i the radiant flux leaving the front and back surfaces of the i^th^ layer, respectively, is the used as a solution technique. The net radiant heat flux across a gap can be expressed as the difference between the radiosities of the bounding surfaces. The net radiation model analysis yields the layer temperatures and corresponding heat transfer coefficients that are used to determine U-factor and SHGC. The thermal analysis is done in "ASHWAT_Thermal" routine.

![Radiosity model used in thermal analysis of the multi-layer glazing/shading system (ASHRAE 1311-RP)](media/radiosity-model-used-in-thermal-analysis-of.png)


### Glazing Layer Analysis

The Equivalent-Layer window model allows a wide range of options regarding the selection of glazing layers and gaps combinations. It also allows mixing glazing and shading layers in any sequence. For instance, it allows placing a venetian blind or a roller blind in between glazing layers. Only beam-beam solar properties at normal incidence, infrared transmittance and infrared emissivity are required for common glazing types. Off-normal solar properties at a given sun position are estimated by adjusting the normal incidence values in "ASHWAT_OffNormalProperties" routine. The model assumes that the ratio between normal and off-normal transmittance is the same for the glazing layer in question and a reference piece of uncoated 6 mm glass with a moderate tint (ASHRAE 1311-RP). A similar procedure is used to convert solar reflectance from normal to off-normal.

### Gap Layer Analysis

Gaps can be specified as sealed, vented indoors or vented outdoors. The sealed gap is considered as enclosed cavity. Vented gaps is specified for inner and outer most gaps only and when the gas type is AIR. The vented gap model assumes that air flow at the perimeter of the window is not restricted. Any fill gas can be specified by molecular mass and thermo-physical properties of viscosity, specific heat and thermal conductivity. This is done by providing "a", "b" and "c" coefficients of the quadratic equation of the form, p = a + bT + cT^2^, where the T (K) is temperature and p is the property being evaluated. EnergyPlus has builtin-data available for common fill gas components including Air, Argon, Krypton and Xenon. Users specify CUSTOM gas by defining the coefficients as an input.

### Shade Layer Analysis

Equivalent layer fenestration model has a complete set of solar and longwave models for the four shading layer types: drapes, venetian blinds, roller blinds and insect screens developed under ASHRAE 1311-RP. And semi-empirical models are used to evaluate the off-normal properties of drape, roller blind and insect screen materials. The effective layer properties of venetian blinds and the effect of pleating in drapes are determined using a more fundamental net radiation scheme. The openness fraction, A~o~ and beam-beam solar transmittance at normal incidence, *~bb~* (=0) really represent a geometric quantity and it has been confirmed that they can be used interchangeably (ASHRAE 1311-RP). Openness is simply the fraction of a material, by area, that is open. In equivalent layer fenestration model, beam-beam transmittance at normal incidence (i.e., openness fraction), beam-diffuse transmittance and reflectance at normal incidence are required to characterize drapery fabric, roller blind and insect screen material. A conventional venetian blind in equivalent layer model can be characterized by specifying the geometry, solar reflectance and transmittance, and emissivity of the slats. The off-normal solar properties of drape, roller blind and insect screen materials were formulated based on measurements (ASHRAE 1311-RP). The longwave properties of the of the drape fabrics, roller blinds and insect screens can be calculated from the material emissivity and openness of the fabric (ASHRE 1311-RP). The optical model development for venetian blinds is presented by Yahoda and Wright (2004 and 2005), and Kotey et al. (2008).

### Drapes and Curtains

The off-normal optical properties of drapery fabric is determined from user specified optical properties at normal incidence (=0) using empirical correlations (Kotey et al., 2009a). The input data required to characterize drapery fabric are the beam-beam transmittance of the material at normal incidence *~bb~* (=0) = Ao, the beam-diffuse transmittance of the material at normal incidence*~bd~* (=0), and the beam-diffuse (total) reflectance of the material~bt~ (=0). The off-normal properties are calculated as follows.

Off-normal Transmittance:

![](media/image1777.png)\


![](media/image1778.png)\


![](media/image1779.png)\


![](media/image1780.png)\


![](media/image1781.png)\


The off-normal reflectance:

![](media/image1782.png)\


![](media/image1783.png)\


The apparent yarn reflectance is given by:

![](media/image1784.png)\


The above set of equations for drapery fabrics are used subject to the condition that the solar absorptance of the fabric, at normal incidence, is not less than 1% (ASHRE 1311-RP). The diffuse-diffuse material properties, for Equivalent layer window model, are determined using Rhomberg integration with 32 panels covering the range from =0 to =90 (ASHRAE 1311-RP). The subscript "X" stands for either front or back side of the layer.

![](media/image1785.png)\


![](media/image1786.png)\


The above set of equations for drapery fabrics apply to the full range of A~o~, fabric transmittance and fabric reflectance including that falls within the bounds of Keyes' (1967) fabric chart plus sheer fabrics (ASHRAE 1311-RP). The longwave thermal emissivity and thermal transmittances of drapery fabric are calculated using the following correlations and fabric openness fraction (Kotey et al. 2008).

![](media/image1787.png)\


![](media/image1788.png)\


The optical and thermal properties determined using the above same sets of equations are equally valid for pleated drape shades (Kotey, et. al., 2009a). For pleated drape, the effective beam-beam and beam-diffuse solar properties are determined by tracking both radiation components, for a given incident angle, and interaction with a fabric pleated rectangular geometry shown in Figure 117. The solar optical properties of the two different pleat planes are evaluated on the basis of the local solar incidence angle. Therefore, the effective layer properties are influenced not just by horizontal solar profile angle, ~H~, but also by incidence angle (ASHRAE 1311-RP).

![Geometry used for Pleated Drape Analysis](media/geometry-used-for-pleated-drape-analysis.png)


The solar diffuse-diffuse and long-wave effective properties of the pleated drape are evaluated using a much simpler net-radiation analysis using conventional shape factors (Kotey, et. al., 2009a). Users can chose and apply the pleated drape model to any fabric and any degree of fullness (ASHRAE 1311-RP).

### Venetian Blinds

The effective shortwave optical and longwave optical properties of venetian blind layer is estimated using analytical models (Yahoda and Wright 2004, 2005; Kotey et al. 2008). The model requires properties of venetian blind slats and geometry of the slats shown in Figure 118. Likewise, the effective longwave properties are obtained for the layer knowing longwave properties of the slats.


![Geometry and properties used for venetian blind analysis](media/geometry-and-properties-used-for-venetian-blind-analysis.png)


The model assumes that venetian blind slats reflect and transmit solar radiation diffusely (ASHRAE 1311-RP). The same assumption is made regarding thermal radiation. The input data required to characterize a venetian blind are: front and back side reflectance and transmittance of the slat, geometry and infrared emissivity and transmittance of the slate. The effective optical properties of the venetian blind are determined by tracking beam and diffuse solar radiation through various interactions with slats positioned at a given slat angle. The model uses simple four-surface model if the slats are fully sunlit and a six-surface model if the slats are partially sunlit (ASHRAE 1311-RP). Slats are assumed to be thin and flat but a correction is applied to account for slat curvature effect (Kotey et al. 2008).

### Roller Blinds

The off-normal properties of roller-blind are determined from solar properties of roller blind fabric measured at normal incidence (=0) using correlations (Kotey, et. al., 2009b). The off-normal properties for roller blind shades are calculated using the set equations given below:

![](media/image1791.png)\


![](media/image1792.png)\


![](media/image1793.png)\


![](media/image1794.png)\


![](media/image1795.png)\


![](media/image1796.png)\


![](media/image1797.png)\


The off-normal solar property calculation of roller blind is based on a set of correlations developed from measurement data using samples of commonly used commercially produced roller blind material openness range of 0.0 – 0.14. Thus, these correlations are not applicable for shades with materials openness fraction exceeding 0.20. The mean solar reflectance of a roller blind material was found to be purely diffuse and unaffected by incidence angle and is given by:

![](media/image1798.png)\


The diffuse-diffuse transmittance and reflectance are obtained by Rhomberg numerical integration. The longwave properties of roller blind material determined using the material property and the openness fraction (Kotey et al. 2008) as shown below:

![](media/image1799.png)\


![](media/image1800.png)\


### Insect Screens

The empirical correlations formulated to obtain the effective off-normal solar and longwave properties of insect screens were based on measurements (Kotey et al. (2009a). Insect screen geometry is shown in Figure 119. The calculation of effective solar properties requires a set of properties measured at normal incidence: *~bb~* (=0), *~bd~* (=0) and *~bt~* (=0).

![Geometry used for insect screen analysis](media/geometry-used-for-insect-screen-analysis.png)


Openness can be determined by optical measurement at normal incidence, A~o~=*~bb~* (=0), but in the case of insect screens A~o~ can reliably be calculated knowing wire diameter (*d*), and wire spacing (*s*) as follows:

![](media/image1802.png)\


The incidence angle beyond which direct beam transmission is cut off, *~CutOff~*, can also be estimated purely from geometry and is given by:

![](media/image1803.png)\


The off-normal properties are calculated as follows.

![](media/image1804.png)\


![](media/image1805.png)\


![](media/image1806.png)\


![](media/image1807.png)\


![](media/image1808.png)\


![](media/image1809.png)\


![](media/image1810.png)\


![](media/image1811.png)\


![](media/image1812.png)\


The diffuse-diffuse material properties are obtained by Rhomberg numerical integration. The longwave properties of insect screen are given by expressions similar those formulated for drapery fabrics, and roller blinds (Kotey et al. 2008) is given by:

![](media/image1813.png)\


![](media/image1814.png)\


The apparent wire material emissivity can be taken as *^w^*=0.93 for common insect screens - screens with dark, rough, non-metallic wire surfaces. The corresponding infrared wire material transmittance is *^w^*=0.98. A lower infrared emissivity can be used for screens constructed with shiny metallic wire. For example, to model stainless-steel wire mesh use *^w^*=0.32 and *^w^*=0.81.

### Integration with Heat Balance Method

The solar model calculations are performed surface by surface following the existing structure of EnergyPlus's heat balance algorithm. The optical and thermal calculations are performed for each surface at each time step. The thermal model needs to be updated at each time step during the surface heat balance iteration. Thus, window thermal model is invoked during the surface heat balance but only once. This is consistent with the current EnergyPlus's window model inside surface heat balance iteration scheme. The equivalent layer window thermal model also has internal iteration scheme. The thermal model routine also requires the solar and shortwave radiation flux absorbed by each layer of a fenestration at each time step. The calculation of the absorbed radiation flux is performed using the existing scheme except that the optical properties are calculated by the equivalent layer optical model. The equivalent layer thermal model returns the temperatures and the fluxes at each layer.

### Equivalent Layer Window Solar Model

The equivalent layer window solar model calculates the transmittance of the window and absorptance of each layer. Separate optical properties calculations are performed for exterior beam and diffuse radiations, and interior diffuse radiation (e.g., lighting and inter-reflected solar gain). The fractions for beam radiations depend on solar incident angle and hence updated for each time step. Diffuse radiation fractions are the same for all time steps unless the shade characteristics are altered; for instance, when the venetian blind slat angle is controlled. Otherwise, the diffuse fractions are constant for a given configuration and time step. The hub of the optical properties calculation routine for Equivalent Layer window model is the routine "ASHWAT_Solar". For beam radiation, at each time step, first the normal incidence optical properties are modified for current incident angle. Using the off-normal properties, then the ASHWAT_Solar routine sets up the coefficient matrix based on the net radiation concept to determine the effective absorptance of each layer and the window transmittance. The total intensity of shortwave radiation absorbed at each layer is determined by multiplying the incident solar radiation, reflected internal solar radiations, and internal shortwave source components by the appropriate absorbed fractions and summing them.

### Equivalent Layer Window Thermal Model

The equivalent layer thermal model is calculated only once for each time step. But the thermal model has internal iterative solutions scheme. During each time step, the procedure initially assumes that room air and means radiant temperatures are known. The surface heat balance loops through all zone surfaces while invoking the Equivalent Layer thermal model only once for each window surface at the first iteration. The window surfaces temperatures from the first iteration are used to complete the heat balances for the indoor (and implicitly the outdoor) face of each surface iteratively. Once indoor surface temperatures are calculated using the surface heat balance, the zone air temperature can be updated and the loads are predicted. Shaded fenestration in general do not have single inside temperature by virture their long-wave radiation transmittance. The equivalent layer window model accounts for this using effective emissivity of the composite layers derived for each fenestration (ASHRAE 1311-RP) as shown below:

![](media/image1815.png)\


where

*^eff^*=composite indoor (room-side) longwave emissivity, (-)

*~j~=effective emissivity of layer j, (-)*

*~k~=effective infrared transmittance of layer k (~nl+1~* = 1)

*nl=number of layers in fenestration system (glazing and shade)*. Layers are numbered outside to inside (layer 1 is outermost, layer *nl* is innermost).

Each equivalent layer window surafce yields net longwave radiant flux exchanged with the zone surfaces. Net longwave radiation exchange from the window to the zone is recast for a composite surface temperature calculation as follows:

![](media/image1816.png)\


where

*T^eff^*=inside surface temperature of the composite layer, C (F)

*Q~lw~*=infrared radiant gain from zone, W/m2 (Btu/h-ft2)

*=*Stefan-Boltzmann constant, W/m2-K4 (Btu/h-ft2-R4)

*T~0~*=Temperature of absolute zero, -273.15 C (-459.67 F)

Recalculating effective inside face window temperature may result in extra convection flux. The  "extra" (Other) convective flux is computed; this is the gain in excess of that resulting from the standard surface heat balance model. The net other convection term *QX* calculated below is added to the zone air heat balance (ASHRAE 1311-RP).

![](media/image1817.png)\


Where,

*h~c~=inside* convective coefficient of the fenestration, W/m^2^-K (Btu/h-ft^2^-F)

*Q~conv~=total convective heat flux to zone from equivalent layer window thermal model, W/m*^2^ (Btu/h-ft^2^); includes open-channel gains and impact of inside surface convective coefficient

*T~a~=zone* air temperature, C (F)

### References:

Edwards, D.K. 1977. Solar absorption by each element in an absorber-coverglass array, Technical Note, Solar Energy, Vol. 19, pp. 401-402.

Parmelee, G. V., and W. W. Aubele. 1952. The shading of sunlit glass: an analysis of the effect of uniformly spaced flat opaque slats, ASHVE Transactions, Vol. 58, pp. 377-398.

Farber, Erich A.; William A. Smith, C.W. Pennington, John C. Reed. 1963. Theoretical analysis of solar heat gain through insulating glass with inside shading. ASHRAE Transactions, Vol. 69, pp.393-405.

Rheault, S., and E. Bilgen. 1989. Heat transfer analysis in an automated venetian blind system, Journal of Solar Energy, Vol. 111 (Feb.), pp. 89-95.

Pfrommer, P., K. J. Lomas, and C. Kupke. 1996. "Solar Radiation Transport through Slat-Type Blinds: a New Model and its Application for Thermal Simulation of Buildings," Solar Energy, Vol. 57, No. 2, pp. 77-91.

Rosenfeld, J.L.J., W. J. Platzer, H. Van Dijk, and A. Maccari. 2000. "Modelling the Optical and Thermal Properties of Complex Glazing: Overview of Recent Developments", Solar Energy, Vol. 69 Supplement, No. 1-6, pp.1-13.

Yahoda, D. S. and J. L. Wright. 2004. "Methods for Calculating the Effective Longwave Radiative Properties of a Venetian Blind Layer," ASHRAE Transactions, Vol. 110, Pt. 1., pp. 463-473.

Yahoda, D. S. and J. L. Wright. 2005. "Methods for Calculating the Effective Solar-Optical Properties of a Venetian Blind Layer," ASHRAE Transactions, Vol. 111, Pt. 1, pp. 572-586.

Yahoda, D. S. and J. L. Wright. 2004. "Heat Transfer Analysis of a Between-Panes Venetian Blind Using Effective Longwave Radiative Properties," ASHRAE Transactions, Vol. 110, Pt. 1., pp. 455-462.

Huang, N.Y.T., J. L. Wright, M. R. Collins. 2006. "Thermal Resistance of a Window with an Enclosed Venetian Blind: Guarded Heater Plate Measurements," ASHRAE Transactions, Vol. 112, Pt. 2. pp. 13-21.

Wright, J. L. 2008. "Calculating Centre-Glass Performance Indices of Glazing Systems with Shading Devices," ASHRAE Transactions, Vol. 114, Pt. 2.

Wright, J. L., N. Y. T. Huang, and M. R. Collins. 2008. "Thermal Resistance of a Window with an Enclosed Venetian Blind: A Simplified Model," ASHRAE Transactions, Vol. 114, Pt. 1.

Kotey, N. A., J. L. Wright, and M. R. Collins. 2008. "Determining Longwave Radiative Properties of Flat Shading Materials," 33rd Annual SESCI / 3rd CSBC Conference Proceedings, Fredericton, NB.

Kotey, N.A., Wright, J.L., M. R. Collins. 2009a. "Determination of Angle-Dependent Solar Optical Properties of Roller Blind Materials," drafted for submission to ASHRAE Transactions, Vol. 115, Pt. 1.

Kotey, N.A., Wright, J.L., M. R. Collins. 2009b. "Determination of Angle-Dependent Solar Optical Properties of Drapery Fabrics," in review, ASHRAE Transactions, Vol. 115, Pt. 2.

Wright, John L., Charles S. Barnaby, Michael R. Collins, and Nathan A. Kotey. Improving Cooling Load Calculations for Fenestration with Shading Devices . ASHRAE 1311-RP, Final Report, February 11, 2009.
