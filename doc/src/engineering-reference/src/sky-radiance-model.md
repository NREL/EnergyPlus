# Sky Radiance Model

In EnergyPlus the calculation of diffuse solar radiation from the sky incident on an exterior surface takes into account the anisotropic radiance distribution of the sky. For this distribution, the diffuse sky irradiance on a surface is given by

![](media/image567.png)\


Where

Diffuse Solar Irradiance is the diffuse solar irradiance from the sky on the ground.

surface is the surface being analyzed.

AnisoSkyMultiplier is determined by surface orientation and sky radiance distribution, and accounts for the effects of shading of sky diffuse radiation by shadowing surfaces such as overhangs. It does not account for *reflection* of sky diffuse radiation from shadowing surfaces.

The sky radiance distribution is based on an empirical model based on radiance measurements of real skies, as described in Perez et al., 1990. In this model the radiance of the sky is determined by three distributions that are superimposed (see Figure 37)

(1) An isotropic distribution that covers the entire sky dome;

(2) A circumsolar brightening centered at the position of the sun;

(3) A horizon brightening.

![Schematic view of sky showing solar radiance distribution as a superposition of three components: dome with isotropic radiance, circumsolar brightening represented as a point source at the sun, and horizon brightening represented as a line source at the horizon.](media/schematic-view-of-sky-showing-solar-radiance.png)


The proportions of these distributions depend on the sky condition, which is characterized by two quantities, *clearness factor* and *brightness factor*, defined below, which are determined from sun position and solar quantities from the weather file.

The *circumsolar brightening* is assumed to be concentrated at a point source at the center of the sun although this region actually begins at the periphery of the solar disk and falls off in intensity with increasing angular distance from the periphery.

The *horizon brightening* is assumed to be a linear source at the horizon and to be independent of azimuth. In actuality, for clear skies, the horizon brightening is highest at the horizon and decreases in intensity away from the horizon. For overcast skies the horizon brightening has a negative value since for such skies the sky radiance increases rather than decreases away from the horizon.

Table: Variables in Anisotropic Sky Model and Shadowing of Sky Diffuse Radiation

Mathematical variable|Description|UnitsFORTRAN variable
---------------------|-----------|---------------------
I~sky~|Solar irradiance on surface from sky|W/m^2^|-
I~horizon~|Solar irradiance on surface from sky horizon|W/m^2^|-
I~dome~|Solar irradiance on surface from sky dome|W/m^2^|-
I~circumsolar~|Solar irradiance on surface from circumsolar region|W/m^2^|-
I~h~|Horizontal solar irradiance|W/m^2^|-
S|Surface tilt|radians|Surface(SurfNum)%Tilt\*DegToRadians
a, b|intermediate variables|-|-
F~1~, F~2~|Circumsolar and horizon brightening coefficients|-|F1, F2
 *α*|Incidence angle of sun on surface|radians|IncAng
Z|Solar zenith angle|radians|ZenithAng
Δ|Sky brightness factor|-|Delta
ε|Sky clearness factor|-|Epsilon
m|relative optical air mass|-|AirMass
I~O~|Extraterrestrial solar irradiance|W/m^2^|-
I|Direct normal solar irradiance|W/m^2^|Material%Thickness
κ|constant = 1.041 for Z in radians|radians^-3^|-
F~ij~|Brightening coefficient factors|-|F11R, F12R, etc.
R~circumsolar~|Shadowing factor for circumsolar radiation|-|SunLitFrac
R~dome~|Shadowing factor for sky dome radiation|-|DifShdgRatioIsoSky
R~horizon~|Shadowing factor for horizon radiation|-|DifShdgRatioHoriz
E|Sky radiance|W/m^2^|-
*θ*|Azimuth angle of point in sky|radians|Theta
φ|Altitude angle of point in sky|radians|Phi
I~i~|Irradiance on surface from a horizon element|W/m^2^|-
I~ij~|Irradiance on surface from a sky dome element|W/m^2^|-
SF|Sunlit fraction|-|FracIlluminated
I'|Sky solar irradiance on surface with shadowing|W/m^2^|-

## Sky Diffuse Solar Radiation on a Tilted Surface

The following calculations are done in subroutine AnisoSkyViewFactors in the SolarShading module.

In the absence of shadowing, the sky formulation described above gives the following expression for sky diffuse irradiance, *I~sky~*, on a tilted surface:

![](media/image569.png)\


where

![](media/image570.png)\


AnisoSkyMult is then *I~sky~*/DifSolarRad.

In the above equations:

I~h~ = horizontal solar irradiance (W/m^2^)

*S* = surface tilt (radians)

*a* = max(0,cos*α*)

*b* = max(0.087, cos*Z*)

*F~1~* = circumsolar brightening coefficient

*F~2~* = horizon brightening coefficient

where

*α* = incidence angle of sun on the surface (radians)

*Z* = solar zenith angle (radians).

The brightening coefficients are a function of sky conditions; they are given by

![](media/image571.png)\


Here the sky brightness factor is

![](media/image572.png)\


where

*m* = relative optical air mass

*I~o~* = extraterrestrial irradiance (taken to have an average annual value of 1353 W/m^2^);

and the sky *clearness factor* is

![](media/image573.png)\


where

I = direct normal solar irradiance

κ = 1.041 for Z in radians

The factors *F~ij~*  are shown in the following table. The F~ij~values in this table were provided by R. Perez, private communication, 5/21/99. These values have higher precision than those listed in Table 6 of Perez et al., 1990.

Table: F~ij~ Factors as a Function of Sky Clearness Range.

--------------------------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|----------------------------|------------------------
**ε  Range** |**1.000-1.065**|**1.065-1.230**|**1.230-1.500**|**1.500-1.950**|**1.950-2.800**|**2.800-4.500**|**4.500-6.200**|**> 6.200**
**F~11~**|-0.0083117|0.1299457|0.3296958|0.5682053|0.8730280|1.1326077|1.0601591|0.6777470
**F~12~**|0.5877285|0.6825954|0.4868735|0.1874525|-0.3920403|-1.2367284|-1.5999137|-0.3272588
**F~13~**|-0.0620636|-0.1513752|-0.2210958|-0.2951290|-0.3616149|-0.4118494|-0.3589221|-0.2504286
**F~21~**|-0.0596012|-0.0189325|0.0554140|0.1088631|0.2255647|0.2877813|0.2642124|0.1561313
**F~22~**|0.0721249|0.0659650|-0.0639588|-0.1519229|-0.4620442|-0.8230357|-1.1272340|-1.3765031
**F~23~**|-0.0220216|-0.0288748|-0.0260542|-0.0139754|0.0012448|0.0558651|0.1310694|0.2506212

## Shadowing of Sky Diffuse Solar Radiation

Sky diffuse solar shadowing on an exterior surface is calculated as follows in subroutine SkyDifSolarShading in the SolarShading module. The sky is assumed to be a superposition of the three Perez sky comp1onents described above.

For the horizon source the following ratio is calculated by dividing the horizon line into 24 intervals of equal length:

![](media/image574.png)\


where *I~i~* is the unobstructed irradiance on the surface from the *i*^th^ interval, *SF~i~* is the sunlit fraction from radiation coming from the *i*^th^ interval, and the sums are over intervals whose center lies in front of the surface. *SF~i~* is calculated using the beam solar shadowing method as though the sun were located at the *i*^th^ horizon point. Here

![](media/image575.png)\


where

*E* (*θ~i~*) = radiance of horizon band (independent of θ)

*dθ* = 2π/24 = azimuthal extent of horizon interval (radians) **

*θ~i~* = 0^O^, 15^O^, … , 345^O^

α~i~ = incidence angle on surface of radiation from *θ~i~*

The corresponding ratio for the isotropic sky dome is given by

![](media/image576.png)\


where (*i,j*) is a grid of 144 points (6 in altitude by 24 in azimuth) covering the sky dome, *I~ij~* is the unobstructed irradiance on the surface from the sky element at the *ij*^th^ point, *SF~ij~* is the sunlit fraction for radiation coming from the *ij*^th^ element, and the sum is over points lying in front of the surface. Here

![](media/image577.png)\


where

*E* (*θ~i~*,*φ~j~*) = sky radiance (independent of θ and φ for isotropic dome)

*dθ* = 2π/24 = azimuthal extent of sky element (radians)

*dφ* = (π/2)/6 = altitude extent of sky element (radians)

*θ~i~* = 0^O^, 15^O^, … , 345^O^

*φ~j~*= 7.5^O^, 22.5^O^, … , 82.5^O^

α~ij~ = incidence angle on surface of radiation from (*θ~i~*,*φ~j~*)

Because the circumsolar region is assumed to be concentrated at the solar disk, the circumsolar ratio is

![](media/image578.png)\


where *SF~sun~* is the beam sunlit fraction. The total sky diffuse irradiance on the surface with shadowing is then

![](media/image579.png)\


*R~horizon~* and *R~dome~* are calculated once for each surface since they are independent of sun position.

With shadowing we then have:

AnisoSkyMult = *I'~sky~*/DifSolarRad.

## Shadowing of Sky Long-Wave Radiation

EnergyPlus calculates the sky long-wave radiation incident on exterior surfaces assuming that the sky long-wave radiance distribution is isotropic. If obstructions such as overhangs are present the sky long-wave incident on a surface is multiplied by the isotropic shading factor, *R~dome~*, described above. The long-wave radiation from these obstructions is added to the long-wave radiation from the ground; in this calculation both obstructions and ground are assumed to be at the outside air temperature and to have an emissivity of 0.9.