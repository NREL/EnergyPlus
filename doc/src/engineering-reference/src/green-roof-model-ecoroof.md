# Green Roof Model (EcoRoof)

## Overview

The input object Material:RoofVegetation provides a model for green roofs (aka ecoroofs or vegetated roofs) that are becoming increasingly common for both new and retrofit buildings. There is widespread recognition and a growing literature of measured data that suggest green roofs can reduce building energy consumption. Currently, however, there are few design tools available to assist developers and architects in assessing the likely magnitude of energy savings associated with various implementation options (e.g., soil type/depth, irrigation options, plant type). As a result there is a significant need for a quantitative and physically-based building energy simulation tool that represents the effects of green roof constructions. Such a tool would facilitate more rapid spread of green roof technologies and make it possible to account for green roof benefits in state energy codes and related energy efficiency standards such as LEED.

In response to the need for green roof design tools a computational model of the heat transfer processes involved on a vegetated roof has been developed. This model accounts for:

long wave and short wave radiative exchange within the plant canopy,

plant canopy effects on convective heat transfer,

evapotranspiration from the soil and plants, and

heat conduction (and storage) in the soil layer

The ability to track moisture-dependent thermal properties is not implemented yet due to stability issues in the CTF scheme, but is under development for use with the finite difference solution scheme made available in EnergyPlus starting in version 2.

As implemented in EnergyPlus the green roof module allows the user to specify "ecoroof" as the outer layer of a rooftop construction using a "Material:RoofVegetation" object. The user can then specify various aspects of the green roof construction including growing media depth, thermal properties, plant canopy density, plant height, stomatal conductance (ability to transpire moisture), and soil moisture conditions (including irrigation).

The model formulation includes the following:

simplified moisture balance that allows precipitation, irrigation, and moisture transport between two soil layers (top and root zone).

soil and plant canopy energy balance based on the Army Corps of Engineers' FASST vegetation models (Frankenstein and Koenig), drawing heavily from BATS (Dickenson et al.) and SiB (Sellers et al.).

soil surface (T~g~) and foliage (T~f~) temperature equations are solved simultaneously each time step, inverting the CTF to extract heat flux information for the energy balance calculation.

The detailed energy balance analysis and resulting equations, being rather complicated, are summarized here. The interested reader is referred to the FASST documentation cited herein for the complete development. The end result is a set of two simultaneous equations for temperature—one for the soil surface and the other for the foliage.

## Green Roof Model Description

As with a traditional roof, the energy balance of an green roof is dominated by radiative forcing from the sun. This solar radiation is balanced by sensible (convection) and latent (evaporative) heat flux from soil and plant surfaces combined with conduction of heat into the soil substrate. This energy balance is illustrated in Figure 35. The variables introduced in this figure are defined in the equations that follow.

![The Energy Balance for a Green Roof.](media/the-energy-balance-for-a-green-roof..jpeg)


The energy budget analysis follows the Fast All Season Soil Strength (FASST) model developed by Frankenstein and Koenig for the US Army Corps of Engineers. FASST was developed, in part, to determine the ability of soils to support manned and unmanned vehicles and personnel movement. In order to accomplish this, however, FASST tracks the energy and moisture balance (including ice and snow) within a vegetated soil. It is a one-dimensional model that draws heavily from other plant canopy models including BATS (Dickinson et al.) and SiB (Sellers et al.). We have implemented FASST here with only a few modifications to adapt it for use with a relatively thin soil layer. The sign convention used assumes all heat fluxes are positive when energy is absorbed into the layer.

In the following discussion this energy budget is divided into a budget for the foliage layer (F~f~) and a budget for the ground surface (F~g~). The various parameterizations for latent and sensible heat flux are described in some detail and then the equation set is reduced to the simultaneous solution of two equations involving the temperatures of the foliage and ground surface.

### Energy budget in the foliage layer 

The foliage energy balance is given by:

![](media/image489.png)\


In addition to convective and sensible heat transfer this equation accounts for both the short and longwave radiation absorbed by the vegetation, including the effects of multiple reflections. The sensible and latent heat flux terms (H~f~ and L~f~) are somewhat complicated and therefore discussed in some detail below.

#### Sensible heat flux in the foliage layer

The sensible heat transfer between the leaf surface and near-canopy air (H~f~) is influenced by the temperature difference between them, wind speed, and Leaf Area Index (LAI). The Leaf Area Index is the dimensionless ratio of the projected leaf area for a unit ground area (Oke). In contrast fractional vegetative cover (~f~) is the ratio of shaded ground surface to total ground surface area. The sensible heat flux is given by:

![](media/image490.png)\


In this equation the constant 1.1 accounts for heat transfer from the stems, twigs and limbs^^(Deardorff).^^The properties of air near the foliage are modeled using the average from the foliage and instrument conditions:

![](media/image491.png)\


where ~a~ is the density of air at the instrument height  and ~f~ is the density of air at the leaf temperature. The air temperature within the foliage is estimated by:

![](media/image492.png)\


*where, T~a~* is the air temperature at the instrument height in Kelvin *T~f~*, is leaf temperature in Kelvin and *T~g~*, is the ground surface temperature in Kelvin. The foliage wind speed is estimated as:

![](media/image493.png)\


Here *W* is the larger of 2.0 m/s or the actual wind speed above the canopy (Hughes et al.) and C^f^~hn~ is the transfer coefficient at near-neutral atmospheric stability conditions:

![](media/image494.png)\


where *K~v~,* is von Karmen's constant (0.4), Z~a~ is the instrument height, Z~d~ is the zero displacement height in meters (height above soil within which the wind speed is effectively zero), and Z^f^~o~ is the foliage roughness length scale (m). The formulations for zero displacement height, roughness length are based on Balick et al.:

![](media/image495.png)\


![](media/image496.png)\


Finally, the bulk transfer coefficient as defined by Deardorff is given by:

![](media/image497.png)\


#### Latent heat flux in the foliage layer

The process of water loss through plant respiration is known as transpiration. It is controlled by the closing and opening of stomata - the intercellular openings between to epidermal (guard) cells (Gates). The resistance to the diffusion of water vapor from these spaces into the atmosphere is called stomatal resistance. It depends on factors such as light intensity, soil moisture content and vapor pressure difference between inside leaf and the outside atmosphere. It is measured in units of s/m and is formulated as:

![](media/image498.png)\


Here, r~s,min~ is the minimum stomatal resistance. The actual stomatal resistance at any time is proportional to this minimum resistance and inversely proportional to LAI. The stomatal resistance is further modified by fractional multiplying factors that relate to incoming solar radiation and atmospheric moisture. As found in Frankenstein and Koenig the inverses of the multiplying factors f~1~, f~2~, and f~3~ are given by:

![](media/image499.png)\


Here, ~r~, is the residual moisture content (defined as the amount of moisture in soil when plants begin to wilt), ~max~ is the maximum moisture content (defined as the maximum amount of moisture a particular type of soil can hold and above which run off occurs), and ![](media/image500.png)  is the average soil moisture in the root zone. The residual moisture content is typically around 0.01 m^3^/m^3^ (Frankenstein and Koenig). The maximum moisture content depends upon the soil, but generally varies from 0.3 to 0.6 m^3^/m^3^ (Guymon et al.). In the expression for f~3~, g~d~ is a plant specific characteristic that is only non-zero for trees, e~f,sat~ is the saturated vapor pressure at the leaf temperature, and e~a~is the air vapor pressure.

Resistance to moisture exchange offered by the boundary layer formed on the leaf surface is known as aerodynamic resistance. It is measured in units of (s/m) and is influenced by wind speed, surface roughness and stability of the atmosphere^^(Oke). It is formulated as:

![](media/image501.png)\


The combined effect of aerodynamic and stomatal resistances to vapor diffusion is integrated into a foliage surface wetness factor:

![](media/image502.png)\


This surface wetness factor is simply a ratio of the aerodynamic resistance to the total resistance. When the aerodynamic resistance is small the wetness factor approaches zero (leaf surfaces remain dry as surface moisture is readily evaporated). As the aerodynamic resistance increases in importance relative to stomatal resistance the wetness factor approaches 1.0 (moisture readily travels to the leaf surfaces, but is not easily evaporated).

The latent heat flux is then given by:

![](media/image503.png)\


Here *l~f~*, is the latent heat of vaporization (J/kg), q~f,sat~ is the saturation mixing ratio at the leaf surface temperature, and q~af~ is the mixing ratio of the air within the canopy. As developed in Frankenstein and Koenig the mixing ratio within the canopy can be determined from:

![](media/image504.png)\


**where the factor** *M~g~*  (ranging from 0 to 1) is the ratio of volumetric moisture content to the porosity of the soil (Koenig). ****The latent heat of vaporization ****(*l~f~*) ****is the amount of energy required to convert a unit mass of water to vapor. It is measured in units of J/kg and is inversely proportional to the temperature. From Henderson-Sellers it is estimated as:

![](media/image505.png)\


### Soil Energy budget

The energy budget at the soil surface is mainly influenced by the soil thermal properties, the amount of foliage coverage (~f~) and the amount of moisture in the soil. If the soil surface is densely covered the diurnal range of surface temperature is small. In the soil energy budget the heat released or gained due to phase changes of soil water, precipitation heat flux and heat flux due to vertical transport of water in the soil are ignored. Future refinements to this model will incorporate these phenomena. The sign convention followed here is the same as above (heat flux into the soil is positive). The overall energy balance at the soil surface (as given in Frankenstein and Koenig) is:

![](media/image506.png)\


**As with the energy equation for the foliage this equation represents sensible heat flux (H**~g~), latent heat flux (L~g~) and the multiple reflections associated with long and short wave radiation. The final term on the right side gives the conduction of heat into the soil substrate.

#### Sensible heat flux in the soil layer

Sensible heat flux between the soil surface and air in its vicinity is dependent on the temperature difference between them and the wind speed within the canopy. It is given as

![](media/image507.png)\


where ![](media/image508.png)  is the bulk transfer coefficient and ~ag~ is the density of air near the soil surface (kg/m^3^) given by:

![](media/image509.png)\


Here ~g~is the density of air at the ground surface temperature

The bulk transfer coefficient is given as the linear combination of bulk transfer coefficient near ground (C^f^~hn~) and near foliage-atmosphere interface (C^g^~hn~) multiplied by the stability factor (~h~) and is formulated as:

![](media/image510.png)\


The ground and foliage bulk transfer coefficients, in turn, are given by:

![](media/image511.png)\


And

![](media/image512.png)\


where![](media/image513.png)  and ![](media/image514.png)  are the ground and foliage roughness lengths,  r~ch~ is turbulent Schmidt number (0.63), and K~v~ is the von Karman constant (0.4).

The condition of the atmosphere (~h~) is determined as stable or unstable based on the sign of the bulk Richardson number:

![](media/image515.png)\


The atmospheric stability factor is then given by Businger and Lumley and Panofsky as:

![](media/image516.png)\


#### Latent heat flux in the soil layer

Removal of water vapor from the soil surface depends on the difference between the mixing ratio of the soil surface and air and the wind speed within the canopy. The resulting latent heat flux is then given by:

![](media/image517.png)\


Here![](media/image518.png) is the bulk transfer coefficient, l~g~is the latent heat of vaporization at the ground surface temperature, q~af~ is the mixing ratio at the foliage-atmosphere interface, and q~f~is the mixing ratio at the ground surface, given by:

![](media/image519.png)\


The bulk transfer coefficient for latent heat exchange is analogous to that for sensible heat exchange and is given by:

![](media/image520.png)\


where![](media/image521.png) is the near ground bulk transfer coefficient for Latent heat flux and ~e~ is the latent heat exchange stability correction factor (assumed to be the same as ~h~).

## Linearization

In order to solve the foliage and soil heat budget equations, the 4^th^ ^^order terms T~f~^4^ and T~g~^4^  and mixing ratio terms q~g,sat~ and q~f,sat~ ~~are linearized as given by Deardorff:

![](media/image522.png)\


![](media/image523.png)\


Here T~f~^n+1^ and T~g~^n+1^ are the current time step leaf and ground surface temperatures in Kelvin. T~f~^n^ and T~g~^n^ are the corresponding temperatures at the previous time step.

The saturation mixing ratio at the ground and leaf surface temperatures are given as:

![](media/image524.png)\


![](media/image525.png)\


where q~sat~(T~g~^n^) is the saturation mixing ratio at the previous time step and is formulated as given in Garratt:

![](media/image526.png)\


Here the saturation vapor pressure e\* (Pa) is evaluated at the ground temperature from the previous time step (T~g~^n^) as:

![](media/image527.png)\


The derivative of saturation mixing ratio at the previous time step is given by:

![](media/image528.png)\


Here, the derivative of the saturation vapor pressure can be calculated from the Clausius-Clapeyron equation:

![](media/image529.png)\


Where R~v~ is the gas constant for water vapor and l~g~ is the latent heat of vaporization at the soil surface temperature.

The corresponding saturation mixing ratio relations for the leaf surfaces can be obtained by replacing T~g~ with T~f~ in the above relations.

## Final Equations 

**After linearization t**he final equations are of the form:

![](media/image530.png)\


![](media/image531.png)\


The coefficients in these equations result from the direct combination of the equations from the above development. The interested reader is directed to the papers by Frankenstein and Koenig for the complete and somewhat complicated expressions.

This final set of equations is then solved simultaneously to obtain T*~g~* and T*~f~* . One key difference in our implementation of the FASST algorithm is that the conduction terms in the equations for ![](media/image532.png) and ![](media/image533.png)  are solved by inverting the Conduction Transfer Functions (CTF) within the EnergyPlus solution scheme.

## Green Roof Nomenclature

C~1~, C~2~, C~3~=coefficients in linearized temperature equations

*C~e~^g^=latent heat flux bulk transfer coefficient at ground layer*

*C~f~=*bulk heat transfer coefficient

C~h~^g^=sensible heat flux bulk transfer coefficient at ground layer

C~hn~^f^=near-neutral transfer coefficient at foliage layer

C~hn~^g^=near-neutral transfer coefficient at ground layer

C~p,a~=specific heat of air at constant pressure (1005.6 J/kg k)

e^\*^=saturation vapor pressure (Pa)

f~1~=multiplying factor for radiation effect on stomatal resistance

f~2~=multiplying factor for moisture effect on stomatal resistance

f~3~=additional multiplying factor for stomatal resistance

F~f~=net heat flux to foliage layer (W/m^2^)

F~g~=net heat flux to ground surface (W/m^2^)

g~d~=plant specific characteristic related to stomatal resistance

H *~f~* =foliage sensible heat flux (W/m^2^)

H~g~=ground sensible heat flux (W/m^2^)

![](media/image534.png) =total incoming short wave radiation (W/m^2^)

![](media/image535.png)  =total incoming longwave radiation (W/m^2^)

K~v~=von Karmen constant (0.4)

l~f~=latent heat of vaporization at foliage temperature (J/kg)

l~g~=latent heat of vaporization at ground temperature (J/kg)

L*~f~*  =foliage latent heat flux (W/m^2^)

L~g~=ground latent heat flux (W/m^2^)

*LAI*  =leaf area index (m^2^/m^2^)

M~g~=moisture saturation factor

q~a~=mixing ratio for air

q~af~=mixing ratio for air within foliage canopy

q~f,sat~=saturation mixing ratio at foliage temperature

q~g,sat~=saturation mixing ratio at ground temperature

r~a~=aerodynamic resistance to transpiration (s/m)

r~s~=foliage leaf stomatal resistance (s/m)

r~s,min~=minimal leaf stomatal resistance (s/m)

r"=surface wetness factor

R~ib~=bulk Richardson number

R~v~=gas constant for water vapor (461.53 J/kgK)

*T~a~=*the air temperature at the instrument height (Kelvin)

T~af~  =air temperature with in the canopy (Kelvin)

T*~f~*    =leaf temperature (Kelvin)

T~g~  =ground surface temperature (Kelvin)

W=wind speed above canopy (m/s)

W~af~=wind speed with in the canopy (m/s)

z=height or depth (m)

Z~a~=instrument height (m)

Z~d~=displacement height (m)

Z~o~^f^=foliage roughness length scale (m)

### Greek letters

~f~=albedo (short wave reflectivity) of the canopy

~g~=albedo (short wave reflectivity) of ground surface

![](media/image536.png) =![](media/image537.png)

![](media/image538.png) =emissivity of canopy

![](media/image539.png) =emissivity of the ground surface

~h~stability factor

**~a~  = density of air at instrument height (kg/m^3^)

**~f~  = density of air at foliage temperature (kg/m^3^)

~af~  = density of air at foliage temperature (kg/m^3^)

~a~~g~= density of air at ground surface temperature (kg/m^3^)

![](media/image540.png) =the Stefan-Boltzmann constant (5.699\*10^-8^ W/m^2^ ºK^4^)

![](media/image541.png) =fractional vegetation coverage

### Subscripts and superscripts

a=air

af=air within the foliage layer

e=latent heat flux term

f=foliage surface

g=ground surface

h=sensible heat flux term

n=current time step

n+1=future time step

ir=infrared (or long-wave)

sat=saturation value

S=short-wave

## References

ASHRAE. 2005. 2005 ASHRAE Handbook – Fundamentals. Chapter 16, Air flow Around Buildings, Atlanta: American Society of Heating Refrigerating and Air-Conditioning Engineers, Inc.

Balick, L. R., R. K. Scoggins, and L. E. Link. 1981. Inclusion of a simple vegetation layer in terrain temperature models for thermal IR signature prediction. IEEE Geoscience and Remote Sensing GE-19(3), pp.143-152.

Businger, J. A. 1966. In ‘Arctic Heat Budget and Atmospheric Circulation', Symposium Proceedings, pp. 305-332. The Rand Corporation.

Deardorff, J.W. 1978. "Efficient Prediction of ground surface temperature and moisture with inclusion of a layer of vegetation", Journal Geophysical Research, pp. 1889-1902.

Dickinson, R.E., A. Henderson-Sellers, P.J. Kennedy, and M.F. Wilson. 1986. Biosphere-Atmosphere Transfer Scheme (BATS) for the NCAR community climate model. NCAR Technical Note, TN-275+STR.

ECMWF. 2002. European Centre for Medium-Range Weather Forecasts, Integrated Forecast System. Documentation, CY25R1 (Operational implementation 9 April 2002). http://www.ecmwf.int/research/ifsdocs/CY25r1/Physics/Physics-08-03.html.

Frankenstein, S., and G. Koenig. 2004. FASST Vegetation Models. U. S. Army Engineer Research and Development Center, Cold regions Research and Engineering Laboratory, ERDC/CRREL Technical Report TR-04-25.

Frankenstein, S., and G. Koenig. 2004. Fast All-season Soil Strength (FASST). U.S. Army Engineer Research and Development Center, Cold regions Research and Engineering Laboratory, ERDC/CRREL Special Report SR-04-1.

Garratt, J.R. 1992. The Atmospheric Boundary Layer, Cambridge university press.

Gates, D.M. 1980. Biophysical Ecology. New York: Springer-Verlag

Guymon, G.L., R.L. Berg, and T.V. Hromadka. 1993. Mathematical Model of Frost Heave and Thaw Settlement in Pavements. U.S. Army Cold Regions Research and Engineering Laboratory, CRREL Report 93-2.

Henderson-Sellers, B. 1984. "A New Formula for Latent Heat of Vaporization of water as function of temperature", Quarterly Journal Royal Meteorological Society, 10 pp. 1186-1190.

Hughes, P.A., T.J.L. McComb, A.B. Rimmer, and K.E. Turver. 1993. "A mathematical model for the prediction of temperature of man-made and natural surfaces", International Journal of Remote Sensing 14 (7), pp. 1383-1412.

Koenig, G.G. 1994. Smart Weapons Operability Enhancement (SWOE) Joint Test and Evaluation (JT and E) Program: Final Report. Dr. James P. Welch, Joint Test Director, SWOE JT and E, SWOE Report 94-10, Annex D.

Lumley, J. L. and Panofsky, H. A. 1964. ‘The structure of Atmospheric Turbulence'. Interscience Monographs and Texts in Physics and Astronomy, Vol. XII. Wiley, New York.

Oke, T.R. 1987. Boundary Layer Climates, University Press, Cambridge

Sellers, P.J., Y. Mintz, Y.C. Sud, and A. Dalcher. 1986. A simple biosphere model (SiB) for use within general circulation models. Journal of Atmospheric Science, 43 (6), pp. 505-532.