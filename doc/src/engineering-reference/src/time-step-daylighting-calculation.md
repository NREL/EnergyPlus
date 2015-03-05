# Time-Step Daylighting Calculation

## Overview

A daylighting calculation is performed each time step that the sun is up for each zone that has one or two daylighting reference points specified. The exterior horizontal illuminance from the sun and sky is determined from solar irradiance data from the weather file. The interior illuminance at each reference point is found for each window by interpolating the daylight illuminance factors for the current sun position, then, for sky-related interior illuminance, multiplying by the exterior horizontal illuminance from the appropriate sky types that time step, and, for sun-related interior illuminance, multiplying by the exterior horizontal solar illuminance that time step. By summation, the net illuminance and glare due to all of the windows in a zone are found. If glare control has been specified window shading (by movable shading devices or switchable glazing) is deployed to reduce glare. Finally the illuminance at each reference point for the final window and shade configuration is used by the lighting control system simulation to determine the electric lighting power required to meet the illuminance setpoint at each reference point.

Table: Variables in Time-Step Calculations

Mathematical variable|Description|Units||FORTRAN variable
---------------------|-----------|-----||----------------
S~norm,dir~|Direct normal solar irradiance|W/m^2^|BeamSolarRad
S~h,dif~|Exterior diffuse horizontal solar irradiance|W/m^2^|SDIFH, DifSolarRad
S~h,dir~|Exterior direct horizontal solar irradiance|W/m^2^|SDIRH
Z|Solar zenith angle|radians|Zeta
m|Relative optical air mass|-|AirMass
Δ|Sky brightness|-|SkyBrightness
ε|Sky clearness|-|SkyClearness
k, k'|Sky type index|-|ISky
s~k,k'~|Interpolation factor for skies k and k'|-|SkyWeight
ψ~k,k'~|Sky luminance distribution formed from linear interpolation of skies k and k'|cd/m^2^|-
f~k~|Fraction of sky that is type k|-|-
E~h,k~|Horizontal illuminance from sky type k|cd/m^2^|HorIllSky
E~h,sky~|Exterior horizontal illuminance from sky|lux|HISKF
E~h,sun~|Exterior horizontal illuminance from sun|lux|HISUNF
η~dif~, η~dir~|Luminous efficacy of diffuse and direct solar radiation|lm/W|DiffLumEff, DirLumEff
I~win~|Interior illuminance from a window|lux|DaylIllum
S~win~|Window luminance|cd/m^2^|SourceLumFromWinAtRefPt
B~win~|Window background luminance|cd/m^2^|BACLUM
d~sun~, d~sky,k~|Interior illuminance factor for sun, for sky of type k|-|DaylIllFacSun, DFSUHR, DaylIllFacSky, DFSUHR
w~sun~, w~sky,k~|Window luminance factor for sun, for sky of type k|-|DaylSourceFacSun, SFSUHR, DaylSourceFacSky, SFSKHR
b~sun~, b~sky,k~|Window background luminance factor for sun, for sky of type k|-|DaylBackFacSun, BFSUHR, DaylBackFacSky, BFSKHR
w~j~|Weighting factor for time step interpolation|-|WeightNow
|||
|||
i~L~|Reference point index|-|IL
i~S~|Window shade index|-|IS
I~tot~|Total daylight illuminance at reference point|lux|DaylIllum
B~tot~, B|Total window background luminance|cd/m^2^|BLUM
I~set~|Illuminance setpoint|lux|ZoneDaylight%IllumSetPoint
f~L~|Fractional electric lighting output|-|FL
f~P~|Fractional electric lighting input power|-|FP
N~L~|Number of steps in a stepped control system|-|LightControlSteps
M~P~|Lighting power multiplier|-|ZonePowerReductionFactor

## Time-Step Sky Luminance

The sky luminance distribution, *ψ*, for a particular time step is expressed as a linear interpolation of two of the four standard skies — *ψ~cs~*, *ψ~ts~* , *ψ~is~* and *ψ~os~* — described above under "Sky Luminance Distributions." The two sky types that are interpolated depend on the value of the sky clearness. The interpolation factors are a function of sky clearness and sky brightness (Perez et al., 1990). Sky clearness is given by

![](media/image775.png)\


where *S~h,dif~*~~is the diffuse horizontal solar irradiance, *S~norm,dir~* is the direct normal solar irradiance, *Z* is the solar zenith angle and *κ* is a constant equal to 1.041 for *Z* in radians.

Sky brightness is given by

![](media/image776.png)\


where *m* is the relative optical air mass and ![](media/image777.png) is the extraterrestrial direct normal solar irradiance.

If *ε* ≤ 1.2

![](media/image778.png)\


where *ψ~is~* is the intermediate sky luminance distribution, *ψ~os~* is the overcast sky luminance distribution, and

![](media/image779.png)\


If 1.2<*ε* ≤ 3

![](media/image780.png)\


where *ψ~ts~* is the clear turbid sky luminance distribution and

![](media/image781.png)\


If *ε* > 3

![](media/image782.png)\


where *ψ~cs~* is the clear sky luminance distribution and

![](media/image783.png)\


## Interior Illuminance

For each time step the interior illuminance, *I~win~*, from a window is calculated as follows by multiplying daylight factors and exterior illuminance.

First, the sun- and sky-related daylight illuminance factors for the time step are determined by interpolation of the hourly factors:

![](media/image784.png)\


![](media/image785.png)\


where *i~L~* is the reference point index (1 or 2), *i~S~* is the window shade index (1 for unshaded window, 2 for shaded window), *i~h~* is the hour number, and *k* is the sky type index. For the *j* th time step in an hour, the time-step interpolation weight is given by

![](media/image786.png)\


where *N~t~*~~is the number of  time steps per hour.

The interior illuminance from a window is calculated as

![](media/image787.png)\


where *E~h,sun~* and *E~h,sky~* are the exterior horizontal illuminance from the sun and sky, respectively, and *f~k~* and *f~k'~* are the fraction of the exterior horizontal illuminance from the sky that is due to sky type *k* and *k'*, respectively.

The horizontal illuminance from sun and sky are given by

![](media/image788.png)\


where *Z* is the solar zenith angle,  *η~dif~* is the luminous efficacy (in lumens/Watt) of diffuse solar radiation from the sky and *η~dir~* is the luminous efficacy of direct radiation from the sun. The efficacies are calculated from direct and global solar irradiance using a method described in (Perez et al, 1990).

The fractions *f~k~* and *f~k'~* are given by

![](media/image789.png)\


where *E~h,k~* and *E~h,k'~* are the horizontal illuminances from skies *k* and *k'*, respectively (see "Exterior Horizontal Luminance," above), and *s~k,k'~* is the interpolation factor for skies *k* and *k'* (see "Time-Step Sky Luminance," above).  For example, if  *ε* > 3, *k* = *cs* (clear sky), *k'* = *ts* (clear turbid sky) and

 ![](media/image790.png)

Similarly, the window source luminance, *S~win~*, and window background luminance, *B~win~*, for a window are calculated from

![](media/image791.png)\


![](media/image792.png)\


The total illuminance at a reference point from all of the exterior windows in a zone is

![](media/image793.png)\


where *i~S~* = 1 if the window is unshaded and *i~S~* = 2 if the window is shaded that time step. (Before the illuminance calculation is done the window shading control will have been simulated to determine whether or not the window is shaded.)

Similarly, the total background luminance is calculated:

![](media/image794.png)\


## Glare Index

The net glare index at each reference point is calculated as

![](media/image795.png)\


where

![](media/image796.png)\


In the last relationship, the background luminance is approximated as the larger of the background luminance from daylight and the average background luminance that would be produced by the electric lighting at full power if the illuminance on the room surfaces were equal to the setpoint illuminance. In a more detailed calculation, where the luminance of each room surface is separately determined, *B(i~L~)* would be better approximated as an area-weighted luminance of the surfaces surrounding a window, taking into account the luminance contribution from the electric lights.

### Glare Control Logic

If glare control has been specified and the glare index at either reference point exceeds a user-specified maximum value, *G~I,max~*, then the windows in the zone are shaded one by one in attempt to bring the glare at both points below *G~I,max~*. (Each time a window is shaded the glare and illuminance at each reference point is recalculated.) The following logic is used:

If there is only one reference point, shade a window if it is unshaded and shading it decreases the glare, even if it does not decrease the glare below *G~I,max~*. Note that if a window  has already been shaded, say to control solar gain, it will be left in the shaded state.

If there are two reference points, then:

If glare is too high at both points, shade the window if it decreases glare at both points.

If glare is too high only at the first point, shade the window if the glare at the first point decreases, and the glare at the second point stays below *G~I,max~*.

If glare is too high only at the second point, shade the window if the glare at the second point decreases, and the glare at the first point stays below *G~I,max~*.

Shades are closed in the order of window input until glare at both points is below *G~I,max~*, or until there are no more windows left to shade.

## Lighting Control System Simulation

Once the final daylight illuminance value at each reference point has been determined, the electric lighting control is simulated. The fractional electric lighting output, *f~L~*, required to meet the setpoint at reference point *i~L~*~~is given by

![](media/image797.png)\


Here, *I~set~* is the illuminance setpoint and *I~tot~*~~is the daylight illuminance at the reference point. This relationship assumes that the electric lights at full power produce an illuminance equal to *I~set~*~~at the reference point.

The fractional electric lighting input power, *f~P~*, corresponding to *f~L~* is then calculated. The relationship between *f~P~* and *f~L~* depends on the lighting control type.

### Continuous Dimming Control

For a continuously-dimmable control system, it is assumed that *f~P~* is constant and equal to *f~P,min~* for *f~L~<f~L,min~* and that f~P~ increases linearly from *f~P,min~* to 1.0 as *f~L~* increases from *f~L,min~* to 1.0 (Figure 60). This gives

![](media/image798.png)\


![Control action for a continuous dimming system.](media/control-action-for-a-continuous-dimming.png)


### Continuous/Off Dimming Control

A "continuous/off" dimming system has the same behavior as a continuous dimming system except that the lights switch off for *f~L~< f~L,min~* rather than staying at *f~P,min~*.

### Stepped Control

For a stepped control system, *f~P~* takes on discrete values depending on the range of *f~L~* and the number of steps, *N~L~*~~(Figure 61). This gives

![](media/image800.png)\


If a lighting control probability, *p~L~*, is specified, *f~P~* is set one level higher a fraction of the time equal to *1-p~L~*. Specifically, if *f~P~* < 1, *f~P~  f~P~* + 1/*N~L~* if a random number between 0 and 1 exceeds *p~L~*. This can be used to simulate the uncertainty associated with manual switching of lights.

![Stepped lighting control with three steps.](media/stepped-lighting-control-with-three-steps..png)


### Lighting Power Reduction

Using the value of *f~P~* at each reference point and the fraction *f~Z~* of the zone controlled by the reference point, the net lighting power multiplier, *M~P~*, for the entire zone is calculated; this value multiplies the lighting power output without daylighting.

![](media/image802.png)\


In this expression, the term to the right in the parentheses corresponds to the fraction of the zone not controlled by either reference point. For this fraction the electric lighting is unaffected and the power multiplier is 1.0.

## References

CIE Technical Committee 4.2. 1973. Standardization of the Luminance Distribution on Clear Skies. CIE Pub. No. 22, Commission Internationale d'Eclairage, Paris.

Hopkinson, R.G., J. Longmore and P. Petherbridge. 1954. An Empirical Formula for the Computation of the Indirect Component of Daylight Factors. Trans. Illum. Eng. Soc. (London) 19, 201.

Hopkinson, R.G., P. Petherbridge and J. Longmore. 1966. Daylighting. Heinnemann, London, p. 322.

Hopkinson, R.G. 1970. Glare from Windows. Construction Research and Development Journal 2, 98.

Hopkinson, R.G. 1972. Glare from Daylighting in Buildings. Applied Ergonomics 3, 206.

Kittler, R. 1965. Standardization of Outdoor Conditions for the Calculation of the Daylight Factor with Clear Skies. Proc. CIE Inter-Session Meeting on Sunlight, Newcastle-Upon-Tyne.

Lynes, J.A. 1968. Principles of Natural Lighting. Applied Science Publishers, Ltd., London, p. 129.

Matsuura, K. 1987. Luminance Distributions of Various Reference Skies. CIE Technical Report of TC 3-09.

Moon, P. and D. Spencer. 1942. Illumination from a Nonuniform Sky. Illuminating Engineering 37, 707-726.

Perez, R., P. Ineichen, R. Seals, J. Michalsky and R. Stewart. 1990. Modeling Daylight Availability and Irradiance Components from Direct and Global Irradiance. Solar Energy 44, 271-289.

Petherbridge, P. and J. Longmore. 1954. Solid Angles Applied to Visual Comfort Problems. Light and Lighting 47,173.

Winkelmann, F.C.  1983. Daylighting Calculation in DOE-2*.* Lawrence Berkeley Laboratory report no. LBL-11353, January 1983.

Winkelmann, F.C. and S. Selkowitz. 1985. Daylighting Simulation in the DOE-2 Building Energy Analysis Program*.* Energy and Buildings 8, 271-286.