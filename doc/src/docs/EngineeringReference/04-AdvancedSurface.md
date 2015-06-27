Advanced Surface Concepts
=========================

Exterior Naturally Vented  Cavity
---------------------------------

The input object “SurfaceProperty:ExteriorNaturalVentedCavity” allows modeling a special case for the outside boundary conditions of heat transfer surfaces with a multi-skin exterior that is opaque.  From the thermal envelope’s point of view, the presence of a vented cavity on the outside of the surface modifies the conditions experienced by the underlying heat transfer surfaces.  This exterior cavity acts as a radiation and convection baffle situated between the exterior environment and the outside face of the underlying heat transfer surface.  The actual outer surface is referred to as the “baffle”.  The modeling here assumes that the heat capacity in the outer baffle can be neglected since it is much lower than the underlying mass surface.  This object is used with the BuildingSurface:Detailed object where the heat transfer surfaces are referred to as the underlying surfaces.  The constructions and materials for the heat transfer surfaces should reflect the construction of just the underlying surface.  The SurfaceProperty:ExteriorNaturalVentedCavity object is used to describe the detached layer, or baffle, and the characteristics of the cavity and openings for natural ventilation.  This model uses the SurfaceProperty:OtherSideConditionsModel object to pass boundary conditions to the heat transfer modeling for the underlying surfaces.

### Baffle Heat Balance

The baffle is assumed to be sufficiently thin and high-conductivity so that it can be modeled using a single temperature (for both sides and along its area).  This temperature <span>\({T_{s,baff}}\)</span>is determined by formulating a heat balance on a control volume that just encapsulates the baffle surface.  The baffle is assumed to completely cover the underlying surface such that it is opaque to shortwave and longwave radiation.  This assumption means that even though the baffle will have some open area for ventilation, no solar energy passes through these openings. The heat balance is diagrammed in the following figure.

![](media/image438.png)

Figure 33.  Baffle Surface Heat Balance

The heat balance on the baffle surface’s control volume is:

<div>$${q''_{\alpha sol}} + {q''_{LWR,Env}} + {q''_{conv,Env}} + {q''_{LWR,cav}} + {q''_{conv,cav}} + {q''_{source}} = 0$$</div>

where:

<sub><span>\({q''_{\alpha sol}}\)</span> </sub>is absorbed direct and diffuse solar (short wavelength) radiation heat flux.

<span>\({q''_{LWR,Env}}\)</span> is net long wavelength (thermal) radiation flux exchange with the air and surroundings.

<sub><span>\({q''_{conv,Env}}\)</span> </sub>= surface convection flux exchange with outside air.

<sub><span>\({q''_{LWR,cav}}\)</span> </sub>is net long wavelength (thermal) radiation flux exchange with the outside face of the underlying surface(s).

<sub><span>\({q''_{conv,cav}}\)</span> </sub>= surface convection flux exchange with cavity air.

<span>\({q''_{source}}\)</span> is a source/sink term that accounts for energy exported out of the control volume when the baffle is a hybrid device such as a photovoltaic panel.

All terms are positive for net flux to the baffle.  Each of these heat balance components is introduced briefly below.

#### External SW Radiation

<sub><span>\({q''_{\alpha sol}}\)</span> </sub> is calculated using procedures presented elsewhere in this manual and includes both direct and diffuse incident solar radiation absorbed by the surface face.  This is influenced by location, surface facing angle and tilt, shading surfaces, surface face material properties, weather conditions, etc.  The baffle blocks all shortwave radiation from reaching the underlying surface.

#### External LW Radiation

<span>\({q''_{LWR,Env}}\)</span> is a standard radiation exchange formulation between the surface, the sky, the ground, and the atmosphere.  The radiation heat flux is calculated from the surface absorptivity, surface temperature, sky, air, and ground temperatures, and sky and ground view factors.  Radiation is modeled using linearized coefficients.  The baffle blocks all longwave radiation.

#### External Convection

<span>\(q''_{conv,env}\)</span> is modeled using the classical formulation: <span>\(q''_{conv} = h_{co}(T_{air} - T_{o})\)</span> where h<sub>co</sub>, is the convection coefficient.  The h<sub>co</sub> is treated in the same way as an outside face with ExteriorEnvironment conditions.  In addition, when it is raining outside, we assume the baffle gets wet and model the enhanced surface heat transfer using a large value for <span>\({h_{co}}\)</span>.

#### Cavity LW Radiation

<span>\({q''_{LWR,cav}}\)</span>is a standard radiation exchange formulation between the baffle surface and the underlying heat transfer surface located across the cavity.  Radiation is modeled using linearized coefficients.

#### Cavity Convection

<span>\(q''_{conv,cav}\)</span> is modeled using the classical formulation: <span>\(q''_{conv} = h_{cp}(T_{air} - T_{o))\)</span>  where h<sub>cp</sub>, is the convection coefficient.  The value for h<sub>cp</sub> is obtained from correlations used for window gaps from ISO (2003) standard 15099.

Substituting models into (113) and solving for <span>\({T_{s,baff}}\)</span> yields the following equation:

<div>$${T_{s,baff}} = \frac{{\left( {{I_s}\alpha  + {h_{co}}{T_{amb}} + {h_{r,atm}}{T_{amb}} + {h_{r,sky}}{T_{sky}} + {h_{r,gnd}}{T_{amb}} + {h_{r,cav}}{T_{so}} + {h_{c,cav}}{T_{a,cav}} + {{q''}_{source}}} \right)}}{{\left( {{h_{co}} + {h_{r,air}} + {h_{r,sky}} + {h_{r,gnd}} + {h_{r,cav}} + {h_{c,cav}}} \right)}}$$</div>

where,

<span>\({I_s}\)</span> is the incident solar radiation of all types [W/m<sup>2</sup>],

<span>\(\alpha \)</span> is the solar absorptivity of the baffle [dimensionless],

<span>\({h_{r,atm}}\)</span>is the linearized radiation coefficient for the surrounding atmosphere [W/m<sup>2</sup>·K],

<span>\({T_{amb}}\)</span> is the outdoor drybulb from the weather data, also assumed for ground surface [ºC],

<span>\({h_{r,sky}}\)</span> is the linearized radiation coefficient for the sky [W/m<sup>2</sup>·K],

<span>\({T_{sky}}\)</span> is the effective sky temperature [ºC],

<span>\({h_{r,gnd}}\)</span> is the linearized radiation coefficient for the ground [W/m<sup>2</sup>·K],

<span>\({h_{r,cav}}\)</span> is the linearized radiation coefficient for the underlying surface [W/m<sup>2</sup>·K],

<span>\({T_{so}}\)</span> is the temperature of the outside face of the underlying heat transfer surface [ºC],

<span>\({h_{co}}\)</span> is the convection coefficient for the outdoor environment [W/m<sup>2</sup>·K],

<span>\({h_{c,cav}}\)</span> is the convection coefficient for the surfaces facing the plenum [W/m<sup>2</sup>·K], and

<span>\({T_{a,cav}}\)</span> is the drybulb temperature for air in the cavity [ºC].

### Cavity Heat Balance

The *cavity* is the volume of air located between the baffle and the underlying heat transfer surface.  The cavity air is modeled as well-mixed.  The uniform temperature of the cavity air, <span>\({T_{a,cav}}\)</span>, is determined by formulating a heat balance on a control volume of air as diagrammed below.

![](media/image469.png)

Figure 34.  Cavity Air Heat Balance



The heat balance on the cavity air control volume is:

<div>$${\dot Q_{vent}} + {\dot Q_{co}} + {\dot Q_{c,baff}} = 0$$</div>

where,

<span>\({\dot Q_{vent}}\)</span> is the net rate of energy added from natural ventilation – where outdoor ambient air exchanges with the cavity air.

<sub><span>\({\dot Q_{co}}\)</span> </sub> is the net rate of energy added by surface convection heat transfer with the underlying surface.

<span>\({\dot Q_{c,baff}}\)</span>is the net rate of energy added by surface convection heat transfer with the collector.

And substituting into yields the following equation:

<div>$${T_{a,cav}} = \frac{{\left( {{h_{c,cav}}A\,{T_{so}} + {{\dot m}_{vent}}{c_p}{T_{amb}} + {h_{c,cav}}A\,{T_{s,baff}}} \right)}}{{\left( {{h_{c,cav}}A + {{\dot m}_{vent}}{c_p} + {h_{c,cav}}A} \right)}}$$</div>

where,

<span>\({\dot m_{vent}}\)</span> is the air mass flow from natural forces [kg/s]

Modeling natural ventilation air exchanges in a general way is challenging.  Simplistic engineering models are used to model <span>\({\dot m_{vent}}\)</span> resulting from natural buoyancy and wind forces.  Reasoning that the configuration is similar to single-side natural ventilation, we elect to use correlations for natural ventilation presented as equations (29) and (30) in Chapter 26. of ASHRAE HOF (2001).

<div>$${\dot m_{vent}} = \rho \,{{\rm{\rlap{--} \dot V}}_{{\rm{tot}}}}$$</div>

where,

<span>\(\rho \)</span> is the density of air [kg/m<sup>3</sup>], and

<span>\(\dot{V}_{tot} = \dot{V}_{wind} + \dot{V}_{thermal} \)</span> is the total volumetric flow rate of air ventilating in and out of the cavity.

<div>$$\dot{V}_{wind} = C_{v}A_{in}U_{\infty}$$</div>

<div>$$
  \dot{V}_{thermal} = \left\{
    \begin{array}{cl}
      C_{D}A_{in}\sqrt{2g\Delta H_{NPL}\left(T_{a,cav}-T_{amb}\right)/T_{a,cov}} & \; \left(T_{a,cav}>T_{amb}\right) \\
      C_{D}A_{in}\sqrt{2g\Delta H_{NPL}\left(T_{amb}-T_{a,cav}\right)/T_{amb}} & \; \left(T_{a,cav} \lt T_{amb}\,\text{and baffle is vertical}\right) 
    \end{array}
  \right.
$$</div>

<span>\({{\rm{\rlap{--} \dot V}}_{{\rm{thermal}}}} = {C_D}{A_{in}}\sqrt {2g\Delta {H_{NPL}}\left( {{T_{a,cav}} - {T_{amb}}} \right)/{T_{a,cav}}} \)</span>  (if <span>\({T_{a,cav}} > {T_{amb}}\)</span>)

<span>\({{\rm{\rlap{--} \dot V}}_{{\rm{thermal}}}} = {C_D}{A_{in}}\sqrt {2g\Delta {H_{NPL}}\left( {{T_{amb}} - {T_{a,cav}}} \right)/{T_{amb}}} \)</span>  (if <span>\({T_{amb}} > {T_{a,cav}}\)</span> and baffle is vertical)

<span>\({C_v}\)</span> is the effectiveness of the openings that depends on opening geometry and the orientation with respect to the wind.  ASHRAE HoF (2001) indicates values ranging from 0.25 to 0.6.  This value is available for user input.

<span>\({C_D}\)</span> is the discharge coefficient for the opening and depends on opening geometry.  This value is available for user input.

Mass continuity arguments lead to modeling the area of the openings as one half of the total area of the openings, so we have:

<div>$${A_{in}} = \frac{{A\,}}{2}$$</div>

<span>\(g\)</span> is the gravitational constant taken as 9.81 [m/s<sup>2</sup>].

<span>\(\Delta {H_{NPL}}\)</span> is the height from midpoint of lower opening to the Neutral Pressure Level.  This is value is available for user input.

If the cavity is horizontal and <span>\({T_{amb}} > {T_{a,cav}}\)</span> then <span>\({{\rm{\rlap{--} \dot V}}_{{\rm{thermal}}}}{\rm{  =  0}}\)</span> because this is a stable situation.

### Underlying Heat Transfer Surface

The exterior baffle and cavity are applied to the outside of a heat transfer surface.  This surface is modeled using the usual EnergyPlus methods for handling heat capacity and transients – typically the CTF method.  These native EnergyPlus heat balance routines are used to calculate <span>\({T_{so}}\)</span>.  The exterior baffle and cavity system is coupled to the underlying surface using the SurfaceProperty:OtherSideConditionsModel mechanism.  The exterior naturally vented cavity model provides values for <span>\({h_{r,cav}}\)</span>,<span>\({T_{s,baff}}\)</span>, <span>\({h_{c,cav}}\)</span>, and <span>\({T_{a,cav}}\)</span> for use with the heat balance model calculations for the outside face of the underlying surface (described elsewhere in this manual).

### Solar and Shading Calculations

The exterior vented cavity model uses standard EnergyPlus surfaces in order to take advantage of the detailed solar and shading calculations.  Solar radiation incident on the surface includes beam and diffuse radiation, as well as radiation reflected from the ground and adjacent surfaces.  Shading of the collector by other surfaces, such as nearby buildings or trees, is also taken into account.

### Local Wind Speed Calculations

The outdoor wind speed affects terms used in modeling.  The wind speed in the weather file is assumed to be measured at a meteorological station located in an open field at a height of 10 m.  To adjust for different terrain at the building site and differences in the height of building surfaces, the local wind speed is calculated for each surface.

The wind speed is modified from the measured meteorological wind speed by the equation (ASHRAE 2001):

<div>$${U_\infty } = {V_{met}}{\left( {\frac{{{\delta_{met}}}}{{{z_{met}}}}} \right)^{{a_{met}}}}{\left( {\frac{z}{\delta }} \right)^a}$$</div>

where z is the height of the centroid of the system, z<sub>met</sub> is the height of the standard meteorological wind speed measurement, and a and d are terrain-dependent coefficients.  d is the boundary layer thickness for the given terrain type.  The values of a and d are shown in the following tables:

Table 19. Terrain-Dependent Coefficients (ASHRAE 2001).

<table class="table table-striped">
<tr>
<th>Terrain</th>
<th>Description</th>
<th>Exponent, a</th>
<th>Layer Thickness,d (m)</th>
</tr>
<tr>
<td>1</td>
<td>Flat, open country</td>
<td>0.14</td>
<td>270</td>
</tr>
<tr>
<td>2</td>
<td>Rough, wooded country</td>
<td>0.22</td>
<td>370</td>
</tr>
<tr>
<td>3</td>
<td>Towns and cities</td>
<td>0.33</td>
<td>460</td>
</tr>
<tr>
<td>4</td>
<td>Ocean</td>
<td>0.10</td>
<td>210</td>
</tr>
<tr>
<td>5</td>
<td>Urban, industrial, forest</td>
<td>0.22</td>
<td>370</td>
</tr>

</table>

The exterior vented cavity can be defined such that it has multiple underlying heat transfer surfaces.  The centroid heights for each surface are area-weighted to determine the average height for use in the local wind calculation.

### Convection Coefficients

Exterior cavity modeling requires calculating up to three different coefficients for surface convection heat transfer.  These coefficients are defined in the classic way by:

<div>$${h_c} = \frac{{{T_{air}} - {T_{surf}}}}{{{{q''}_{conv}}}}$$</div>

First, <span>\({h_{co}}\)</span> is the convection coefficient for the baffle surface facing the outdoors.  It is modeled in exactly the same way as elsewhere in EnergyPlus and will depend on the user setting for Outside Convection Algorithm – Outside Surface Heat Balance entry elsewhere in this document.

Second, <span>\({h_{c,cav}}\)</span> is the convection coefficient for baffle surfaces facing the cavity.  This coefficient is applied to both the baffle and the underlying surface.  The convection coefficient is modeled in the same way used in EnergyPlus to model air gaps in windows.  These correlations vary by Rayleigh number and surface tilt and are based on the work of various research including Hollands et. al., Elsherbiny et. al., Wright, and Arnold.  The formulations are documented in ISO (2003) standard 15099.  The routines were adapted from Subroutine NusseltNumber in WindowManager.f90 (by F. Winkelmann), which itself was derived from Window5 subroutine “nusselt”.

### Radiation Coefficients

Exterior vented cavity modeling requires calculating up to four different linearized coefficients for radiation heat transfer.  Whereas radiation calculations usually use temperature raised to the fourth power, this greatly complicates solving heat balance equations for a single temperature.  Linearized radiation coefficients have the same units and are used in the same manner as surface convection coefficients and introduce very little error for the temperature levels involved.

The radiation coefficient, <span>\({h_{r,cav}}\)</span>, is used to model thermal radiation between the collector surface and the outside face of the underlying heat transfer surface.  We assume a view factor of unity.  It is calculated using:

<div>$${h_{r,cav}} = {\sigma_{SB}}{e_{baff}}{e_{so}}\frac{{\left( {T_{s,baff}^4 - T_{so}^4} \right)}}{{\left( {{T_{s,baff}} - {T_{so}}} \right)}}$$</div>

where,

all temperatures are converted to Kelvin,

<span>\({\sigma_{SB}}\)</span> is the Stefan-Boltzmann constant,

<span>\({e_{baff}}\)</span> is the longwave thermal emittance of the baffle, and

<span>\({e_{so}}\)</span> is the longwave thermal emittance of the underlying heat transfer surface.

The three other coefficients, <span>\({h_{r,atm}}\)</span>, <span>\({h_{r,sky}}\)</span>, and <span>\({h_{r,gnd}}\)</span> are used elsewhere in EnergyPlus for the outside face surface heat balance and are calculated in the same manner as equation .  [This is accomplished by calling subroutine InitExteriorConvectionCoeffs in the file HeatBalanceConvectionCoeffs.f90. ]

### References

ASHRAE HOF 2001.  2001 ASHRAE Fundamentals Handbook.  American Society of Heating Refrigeration and Air-Conditioning Engineers. Altanta GA.

ISO. 2003. ISO 15099:2003. Thermal performance of windows, doors, and shading devices – Detailed calculations. International Organization for Standardization.

Green Roof Model (EcoRoof)
----------------------------------------------------

### Overview

The input object Material:RoofVegetation provides a model for green roofs (aka ecoroofs or vegetated roofs) that are becoming increasingly common for both new and retrofit buildings. There is widespread recognition and a growing literature of measured data that suggest green roofs can reduce building energy consumption. Currently, however, there are few design tools available to assist developers and architects in assessing the likely magnitude of energy savings associated with various implementation options (e.g., soil type/depth, irrigation options, plant type). As a result there is a significant need for a quantitative and physically-based building energy simulation tool that represents the effects of green roof constructions. Such a tool would facilitate more rapid spread of green roof technologies and make it possible to account for green roof benefits in state energy codes and related energy efficiency standards such as LEED.

In response to the need for green roof design tools a computational model of the heat transfer processes involved on a vegetated roof has been developed. This model accounts for:

* long wave and short wave radiative exchange within the plant canopy,

* plant canopy effects on convective heat transfer,

* evapotranspiration from the soil and plants, and

* heat conduction (and storage) in the soil layer

The ability to track moisture-dependent thermal properties is not implemented yet due to stability issues in the CTF scheme, but is under development for use with the finite difference solution scheme made available in EnergyPlus starting in version 2.

As implemented in EnergyPlus the green roof module allows the user to specify “ecoroof” as the outer layer of a rooftop construction using a “Material:RoofVegetation” object. The user can then specify various aspects of the green roof construction including growing media depth, thermal properties, plant canopy density, plant height, stomatal conductance (ability to transpire moisture), and soil moisture conditions (including irrigation).

The model formulation includes the following:

* simplified moisture balance that allows precipitation, irrigation, and moisture transport between two soil layers (top and root zone).

* soil and plant canopy energy balance based on the Army Corps of Engineers’ FASST vegetation models (Frankenstein and Koenig), drawing heavily from BATS (Dickenson et al.) and SiB (Sellers et al.).

* soil surface (T<sub>g</sub>) and foliage (T<sub>f</sub>) temperature equations are solved simultaneously each time step, inverting the CTF to extract heat flux information for the energy balance calculation.

The detailed energy balance analysis and resulting equations, being rather complicated, are summarized here. The interested reader is referred to the FASST documentation cited herein for the complete development. The end result is a set of two simultaneous equations for temperature—one for the soil surface and the other for the foliage.

### Green Roof Model Description

As with a traditional roof, the energy balance of an green roof is dominated by radiative forcing from the sun. This solar radiation is balanced by sensible (convection) and latent (evaporative) heat flux from soil and plant surfaces combined with conduction of heat into the soil substrate. This energy balance is illustrated in Figure 35. The variables introduced in this figure are defined in the equations that follow.

![Fig1EcoRoof](media/image509.png)

Figure 35.  The Energy Balance for a Green Roof.

The energy budget analysis follows the Fast All Season Soil Strength (FASST) model developed by Frankenstein and Koenig for the US Army Corps of Engineers. FASST was developed, in part, to determine the ability of soils to support manned and unmanned vehicles and personnel movement. In order to accomplish this, however, FASST tracks the energy and moisture balance (including ice and snow) within a vegetated soil. It is a one-dimensional model that draws heavily from other plant canopy models including BATS (Dickinson et al.) and SiB (Sellers et al.). We have implemented FASST here with only a few modifications to adapt it for use with a relatively thin soil layer. The sign convention used assumes all heat fluxes are positive when energy is absorbed into the layer.

In the following discussion this energy budget is divided into a budget for the foliage layer (F<sub>f</sub>) and a budget for the ground surface (F<sub>g</sub>­). The various parameterizations for latent and sensible heat flux are described in some detail and then the equation set is reduced to the simultaneous solution of two equations involving the temperatures of the foliage and ground surface.

#### Energy budget in the foliage layer

The foliage energy balance is given by:

<div>$${F_f} = {\sigma_f}\left[ {{I_S}^ \downarrow (1 - {\alpha_f}) + {\varepsilon_f}{I_{ir}}^ \downarrow  - {\varepsilon_f}\sigma {T_f}^4} \right] + \frac{{{\sigma_f}{\varepsilon_g}{\varepsilon_f}\sigma }}{{{\varepsilon_1}}}\left( {{T_g}^4 - {T_f}^4} \right) + {H_f} + {L_f}$$</div>

In addition to convective and sensible heat transfer this equation accounts for both the short and longwave radiation absorbed by the vegetation, including the effects of multiple reflections. The sensible and latent heat flux terms (H<sub>f</sub> and L<sub>f­</sub>) are somewhat complicated and therefore discussed in some detail below.

##### Sensible heat flux in the foliage layer

The sensible heat transfer between the leaf surface and near-canopy air (H<sub>f</sub>) is influenced by the temperature difference between them, wind speed, and Leaf Area Index (LAI). The Leaf Area Index is the dimensionless ratio of the projected leaf area for a unit ground area (Oke). In contrast fractional vegetative cover (s<sub>f</sub>) is the ratio of shaded ground surface to total ground surface area. The sensible heat flux is given by:

<div>$${H_f} = (1.1*LAI{\rho_{af}}{C_{p,a}}{C_f}{W_{af}})*({T_{af}} - {T_f})$$</div>

In this equation the constant 1.1 accounts for heat transfer from the stems, twigs and limbs (Deardorff). The properties of air near the foliage are modeled using the average from the foliage and instrument conditions:

<div>$${\rho_{af}} = 0.5({\rho_a} + {\rho_f})$$</div>

where r<sub>a</sub> is the density of air at the instrument height  and r<sub>f</sub> is the density of air at the leaf temperature. The air temperature within the foliage is estimated by:

<div>$${T_{af}} = (1 - {\sigma_f})({T_a}) + {\sigma_f}\left( {0.3{T_a} + 0.6{T_f} + 0.1{T_g}} \right)$$</div>

*where, T<sub>a</sub>* is the air temperature at the instrument height in Kelvin *T<sub>f</sub>*, is leaf temperature in Kelvin and *T<sub>g</sub>*, is the ground surface temperature in Kelvin. The foliage wind speed is estimated as:

<div>$${W_{af}} = 0.83{\sigma_f}W\sqrt {C_{hn}^f}  + (1 - {\sigma_f})W$$</div>

Here *W* is the larger of 2.0 m/s or the actual wind speed above the canopy (Hughes et al.) and C<sup>f</sup><sub>hn</sub> is the transfer coefficient at near-neutral atmospheric stability conditions:

<div>$$C_{hn}^f = {K_v}^2 \cdot {\left( {\ln \left( {\frac{{{Z_a} - {Z_d}}}{{Z_o^f}}} \right)} \right)^{ - 2}}$$</div>

where *K<sub>v</sub>,* is von Karmen’s constant (0.4), Z<sub>a</sub> is the instrument height, Z<sub>d</sub> is the zero displacement height in meters (height above soil within which the wind speed is effectively zero), and Z<sup>f</sup><sub>o</sub> is the foliage roughness length scale (m). The formulations for zero displacement height, roughness length are based on Balick et al.:

<div>$${Z_d} = 0.701Z_{_f}^{0.979}$$</div>

<div>$${Z_o} = 0.131Z_{_f}^{0.997}$$</div>

Finally, the bulk transfer coefficient as defined by Deardorff is given by:

<div>$${C_f} = 0.01*\left( {1 + \frac{{0.3(m/s)}}{{{W_{af}}(m/s)}}} \right)$$</div>

##### Latent heat flux in the foliage layer

The process of water loss through plant respiration is known as transpiration. It is controlled by the closing and opening of stomata - the intercellular openings between to epidermal (guard) cells (Gates). The resistance to the diffusion of water vapor from these spaces into the atmosphere is called stomatal resistance. It depends on factors such as light intensity, soil moisture content and vapor pressure difference between inside leaf and the outside atmosphere. It is measured in units of s/m and is formulated as:

<div>$${r_s} = \frac{{{r_{s,\min }}}}{{LAI}} \cdot {f_1} \cdot {f_2} \cdot {f_3}$$</div>

Here, r<sub>s,min</sub> is the minimum stomatal resistance. The actual stomatal resistance at any time is proportional to this minimum resistance and inversely proportional to LAI. The stomatal resistance is further modified by fractional multiplying factors that relate to incoming solar radiation and atmospheric moisture. As found in Frankenstein and Koenig the inverses of the multiplying factors f<sub>1</sub>, f<sub>2</sub>, and f<sub>3</sub> are given by:

<div>
 $$
  \begin{array}{l}
   \frac{1}{f_1} = \min \left[ 1,\frac{{0.004*{I_s}^ \downarrow  + 0.005}}{{0.81*(0.004*{I_s}^ \downarrow  + 1)}} \right] \\
   \frac{1}{f_2} = \left\{ 
     \begin{array}{cl}
       0                                                                 & \text{when} \; \theta_r \gt \overline \theta \\
       \frac{ \overline \theta   - \theta_r }{ \theta_{max} - \theta_r } & \text{when} \; \theta_r \le \overline \theta \le \theta_{max}
     \end{array} \right. \\
   \frac{1}{f_3} = \exp \left[ - {g_d}({e_{f,sat}} - {e_a} \right]
  \end{array}
 $$
</div>

Here, Q<sub>r</sub>, is the residual moisture content (defined as the amount of moisture in soil when plants begin to wilt), Q<sub>max</sub> is the maximum moisture content (defined as the maximum amount of moisture a particular type of soil can hold and above which run off occurs), and <span>\(\overline \Theta  \)</span> is the average soil moisture in the root zone. The residual moisture content is typically around 0.01 m<sup>3</sup>/m<sup>3</sup> (Frankenstein and Koenig). The maximum moisture content depends upon the soil, but generally varies from 0.3 to 0.6 m<sup>3</sup>/m<sup>3</sup> (Guymon et al.). In the expression for f<sub>3</sub>, g<sub>d</sub> is a plant specific characteristic that is only non-zero for trees, e<sub>f,sat</sub> is the saturated vapor pressure at the leaf temperature, and e<sub>a</sub> is the air vapor pressure.

Resistance to moisture exchange offered by the boundary layer formed on the leaf surface is known as aerodynamic resistance. It is measured in units of (s/m) and is influenced by wind speed, surface roughness and stability of the atmosphere (Oke). It is formulated as:

<div>$${r_a} = \frac{1}{{{c_f}{W_{af}}}}$$</div>

The combined effect of aerodynamic and stomatal resistances to vapor diffusion is integrated into a foliage surface wetness factor:

<div>$$r'' = \frac{{{r_a}}}{{{r_a} + {r_s}}}$$</div>

This surface wetness factor is simply a ratio of the aerodynamic resistance to the total resistance. When the aerodynamic resistance is small the wetness factor approaches zero (leaf surfaces remain dry as surface moisture is readily evaporated). As the aerodynamic resistance increases in importance relative to stomatal resistance the wetness factor approaches 1.0 (moisture readily travels to the leaf surfaces, but is not easily evaporated).

The latent heat flux is then given by:

<div>$${L_f} = {l_f} * LAI{\rho_{af}}{C_f}{W_{af}}{r^``}\left( {{q_{af}} - {q_{f,sat}}} \right)$$</div>

Here *l<sub>f</sub>* , is the latent heat of vaporization (J/kg), q<sub>f,sat</sub> is the saturation mixing ratio at the leaf surface temperature, and q<sub>af</sub> is the mixing ratio of the air within the canopy. As developed in Frankenstein and Koenig the mixing ratio within the canopy can be determined from:

<div>$${q_{af}} = \left[ {\frac{{\left( {1 - {\sigma_f}} \right){q_a} + {\sigma_f}\left( {0.3{q_a} + 0.6{q_{f,sat}}{r^``} + 0.1{q_{f,sat}}{M_g}} \right)}}{{1 - {\sigma_f}\left[ {0.6\left( {1 - {r^``}} \right) + 0.1\left( {1 - {M_g}} \right)} \right]}}} \right]$$</div>

where the factor *M<sub>g</sub>*  (ranging from 0 to 1) is the ratio of volumetric moisture content to the porosity of the soil (Koenig). The latent heat of vaporization (*l<sub>f</sub>*) is the amount of energy required to convert a unit mass of water to vapor. It is measured in units of J/kg and is inversely proportional to the temperature. From Henderson-Sellers it is estimated as:

<div>$${l_f} = 1.91846*{10^6}{\left[ {\frac{{{T_f}}}{{{T_f} - 33.91}}} \right]^2}$$</div>

#### Soil Energy budget

The energy budget at the soil surface is mainly influenced by the soil thermal properties, the amount of foliage coverage (s<sub>f</sub>) and the amount of moisture in the soil. If the soil surface is densely covered the diurnal range of surface temperature is small. In the soil energy budget the heat released or gained due to phase changes of soil water, precipitation heat flux and heat flux due to vertical transport of water in the soil are ignored. Future refinements to this model will incorporate these phenomena. The sign convention followed here is the same as above (heat flux into the soil is positive). The overall energy balance at the soil surface (as given in Frankenstein and Koenig) is:

<div>$${F_g} = (1 - {\sigma_f})\left[ {I_s^ \downarrow (1 - {\alpha_g}) + {\varepsilon_g}I_{ir}^ \downarrow  - {\varepsilon_g}T_g^4} \right] - \frac{{{\sigma_f}{\varepsilon_g}{\varepsilon_f}\sigma }}{{{\varepsilon_1}}}\left( {T_g^4 - T_f^4} \right) + {H_g} + {L_g} + K*\frac{{\partial {T_g}}}{{\partial z}}$$</div>

As with the energy equation for the foliage this equation represents sensible heat flux (H<sub>g</sub>), latent heat flux (L<sub>g</sub>) and the multiple reflections associated with long and short wave radiation. The final term on the right side gives the conduction of heat into the soil substrate.

##### Sensible heat flux in the soil layer

Sensible heat flux between the soil surface and air in its vicinity is dependent on the temperature difference between them and the wind speed within the canopy. It is given as

<div>$${H_g} = {\rho_{ag}}{C_{p,a}}C_h^g{W_{af}}({T_{af}} - {T_g})$$</div>

where <span>\(C_h^g\)</span> is the bulk transfer coefficient and r<sub>ag</sub> is the density of air near the soil surface (kg/m<sup>3</sup>) given by:

<div>$${p_{ag}} = \frac{{{p_a} + {p_g}}}{2}$$</div>

Here r<sub>g</sub> is the density of air at the ground surface temperature

The bulk transfer coefficient is given as the linear combination of bulk transfer coefficient near ground (C<sup>f</sup><sub>hn</sub>) and near foliage-atmosphere interface (C<sup>g</sup><sub>hn</sub>) multiplied by the stability factor (G<sub>h</sub>) and is formulated as:

<div>$$C_h^g = {\Gamma_k}\left[ {\left( {1 - {\sigma_f}} \right)C_{hn}^g + {\sigma_f}C_{hn}^f} \right]$$</div>

The ground and foliage bulk transfer coefficients, in turn, are given by:

<div>$$C_{hn}^g = r_{ch}^{ - 1}{\left[ {\frac{{{K_v}}}{{\ln \left( {{\raise0.7ex\hbox{${{Z_a}}$} \!\mathord{\left/ {\vphantom {{{Z_a}} {Z_o^g}}}\right.}\!\lower0.7ex\hbox{${Z_o^g}$}}} \right)}}} \right]^2}$$</div>

And

<div>$$C_{hn}^f = {\left[ {\frac{{{K_v}}}{{\ln \left( {{\raise0.7ex\hbox{${{Z_a} - {Z_d}}$} \!\mathord{\left/ {\vphantom {{{Z_a} - {Z_d}} {Z_o^f}}}\right.}\!\lower0.7ex\hbox{${Z_o^f}$}}} \right)}}} \right]^2}$$</div>

where<span>\(Z_o^g\)</span> and <span>\(Z_o^f\)</span> are the ground and foliage roughness lengths,  r<sub>ch</sub> is turbulent Schmidt number (0.63), and K<sub>v</sub> is the von Karman constant (0.4).

The condition of the atmosphere (G<sub>h</sub>) is determined as stable or unstable based on the sign of the bulk Richardson number:

<div>$${R_{ib}} = \frac{{2g{Z_a}\left( {{T_{af}} - {T_g}} \right)}}{{\left( {{T_{af}} + {T_g}} \right)W_{af}^2}}$$</div>

The atmospheric stability factor is then given by Businger and Lumley and Panofsky as:

<div>$$
  \Gamma_h = \left\{ 
    \begin{array}{cl}
      \frac{1.0}{\left( 1.0 - 16.0 R_{ib} \right)^{0.5}} & \text{for} \; R_{ib} \lt 0 \\
      \frac{1.0}{\left( 1.0 - 5.0  R_{ib} \right)      } & \text{for} \; R_{ib} \gt 0
    \end{array}
  \right.
$$</div>

##### Latent heat flux in the soil layer

Removal of water vapor from the soil surface depends on the difference between the mixing ratio of the soil surface and air and the wind speed within the canopy. The resulting latent heat flux is then given by:

<div>$${L_R} = C_e^g{l_g}{W_{af}}{\rho_{ag}}\left( {{q_{af}} - {q_g}} \right)$$</div>

Here<span>\(C_e^g\)</span>is the bulk transfer coefficient, l<sub>g</sub> is the latent heat of vaporization at the ground surface temperature, q<sub>af</sub> is the mixing ratio at the foliage-atmosphere interface, and q<sub>f</sub> is the mixing ratio at the ground surface, given by:

<div>$${q_g} = {M_g}{q_{g,sat}} + \left( {1 - {M_g}} \right){q_{af}}$$</div>

The bulk transfer coefficient for latent heat exchange is analogous to that for sensible heat exchange and is given by:

<div>$$C_e^g = {\Gamma_e}\left[ {\left( {1 - {\sigma_f}} \right)C_{en}^g + {\sigma_f}C_{hn}^f} \right]$$</div>

where <span>\(C_{en}^g\)</span> is the near ground bulk transfer coefficient for Latent heat flux and G<sub>e</sub> is the latent heat exchange stability correction factor (assumed to be the same as G<sub>h</sub>).

### Linearization

In order to solve the foliage and soil heat budget equations, the 4<sup>th</sup> <sup> </sup>order terms T<sub>f</sub><sup>4</sup> and T<sub>g</sub><sup>4</sup>  and mixing ratio terms q<sub>g,sat</sub> and q<sub>f,sat</sub> <sub> </sub>are linearized as given by Deardorff:

<div>$${\left[ {T_f^{\left( {n + 1} \right)}} \right]^4} = {\left[ {T_f^n} \right]^4} + 4{\left[ {T_f^n} \right]^3}\left[ {T_f^{n + 1} - T_f^n} \right]$$</div>

<div>$${\left[ {T_g^{\left( {n + 1} \right)}} \right]^4} = {\left[ {T_g^n} \right]^4} + 4{\left[ {T_g^n} \right]^3}\left[ {T_g^{n + 1} - T_g^n} \right]$$</div>

Here T<sub>f</sub><sup>n+1</sup> and T<sub>g</sub><sup>n+1</sup> are the current time step leaf and ground surface temperatures in Kelvin. T<sub>f</sub><sup>n</sup> and T<sub>g</sub><sup>n</sup> are the corresponding temperatures at the previous time step.

The saturation mixing ratio at the ground and leaf surface temperatures are given as:

<div>$${q_{g,sat}}\left( {T_g^{n + 1}} \right) = {q_{sat}}\left( {T_g^n} \right) + {\left( {\frac{{\partial {q_{sat}}}}{{\partial T}}} \right)_{T_g^n}} * \left( {T_g^{n + 1} - T_g^n} \right)$$</div>

<div>$${q_{f,sat}}\left( {T_f^{n + 1}} \right) = {q_{sat}}\left( {T_f^n} \right) + {\left( {\frac{{\partial {q_{sat}}}}{{\partial T}}} \right)_{T_f^n}} * \left( {T_f^{n + 1} - T_f^n} \right)$$</div>

where q<sub>sat</sub>(T<sub>g</sub><sup>n</sup>) is the saturation mixing ratio at the previous time step and is formulated as given in Garratt:

<div>$${q_{sat}}\left( {T_g^n} \right) = \frac{{0.622{e^ * }\left( {T_g^n} \right)}}{{P - {e^ * }\left( {T_g^n} \right)}}$$</div>

Here the saturation vapor pressure e\* (Pa) is evaluated at the ground temperature from the previous time step (T<sub>g</sub><sup>n</sup>) as:

<div>$${e^*} = 611.2\exp \left[ {17.67\left( {\frac{{T_g^n - 273.15}}{{T_g^n - 29.65}}} \right)} \right]$$</div>

The derivative of saturation mixing ratio at the previous time step is given by:

<div>$$\frac{{d{q^ * }}}{{dT_g^n}} = \left[ {\frac{{0.622 * P}}{{{{\left( {P - 0.378 * {e^ * }} \right)}^2}}}} \right]\left( {\frac{{d{e^ * }}}{{dT_g^n}}} \right)$$</div>

Here, the derivative of the saturation vapor pressure can be calculated from the Clausius-Clapeyron equation:

<div>$$\frac{{d{e^ * }}}{{dT_g^n}} = \frac{{{l_g} * {e^ * }\left( {T_g^n} \right)}}{{\left( {{R_v} * {{\left( {T_g^n} \right)}^2}} \right)}}$$</div>

Where R<sub>v</sub> is the gas constant for water vapor and l<sub>g</sub> is the latent heat of vaporization at the soil surface temperature.

The corresponding saturation mixing ratio relations for the leaf surfaces can be obtained by replacing T<sub>g</sub> with T<sub>f</sub> in the above relations.

### Final Equations

After linearization the final equations are of the form:

<div>$$C_1^f + C_2^f{T_g} + C_3^f{T_f} = 0$$</div>

<div>$$C_1^g + C_2^g{T_g} + C_3^g{T_f} = 0$$</div>

The coefficients in these equations result from the direct combination of the equations from the above development. The interested reader is directed to the papers by Frankenstein and Koenig for the complete and somewhat complicated expressions.

This final set of equations is then solved simultaneously to obtain T*<sub>g</sub>* and T*<sub>f</sub>* . One key difference in our implementation of the FASST algorithm is that the conduction terms in the equations for <span>\(C_1^g\)</span>and <span>\(C_2^g\)</span> are solved by inverting the Conduction Transfer Functions (CTF) within the EnergyPlus solution scheme.

### Green Roof Nomenclature

C<sub>1</sub>, C<sub>2</sub>, C<sub>3</sub>    =          coefficients in linearized temperature equations

*C<sub>e</sub><sup>g</sup>              =         * latent heat flux bulk transfer coefficient at ground layer

*C<sub>f</sub>              * =          bulk heat transfer coefficient

C<sub>h</sub><sup>g</sup>              =          sensible heat flux bulk transfer coefficient at ground layer

C<sub>hn</sub><sup>f</sup>             =          near-neutral transfer coefficient at foliage layer

C<sub>hn</sub><sup>g</sup> =          near-neutral transfer coefficient at ground layer

C<sub>p,a</sub>             =          specific heat of air at constant pressure (1005.6 J/kg k)

e<sup>\*</sup>                =          saturation vapor pressure (Pa)

f<sub>1</sub>                =          multiplying factor for radiation effect on stomatal resistance

f<sub>2</sub>                =          multiplying factor for moisture effect on stomatal resistance

f<sub>3</sub>                =          additional multiplying factor for stomatal resistance

F<sub>f</sub>                =          net heat flux to foliage layer (W/m<sup>2</sup>)

F<sub>g</sub>               =          net heat flux to ground surface (W/m<sup>2</sup>)

g<sub>d</sub>               =          plant specific characteristic related to stomatal resistance

H *<sub>f</sub>*               =          foliage sensible heat flux (W/m<sup>2</sup>)

H<sub>g</sub>               =          ground sensible heat flux (W/m<sup>2</sup>)

<span>\(I_s^ \downarrow \)</span>             =          total incoming short wave radiation (W/m<sup>2</sup>)

<span>\(I_{i\gamma }^ \downarrow \)</span>             =          total incoming longwave radiation (W/m<sup>2</sup>)

K<sub>v</sub>               =          von Karmen constant (0.4)

l<sub>f</sub>                 =          latent heat of vaporization at foliage temperature (J/kg)

l<sub>g</sub>                =          latent heat of vaporization at ground temperature (J/kg)

L*<sub>f</sub>*                =          foliage latent heat flux (W/m<sup>2</sup>)

L<sub>g</sub>               =          ground latent heat flux (W/m<sup>2</sup>)

*LAI*             =          leaf area index (m<sup>2</sup>/m<sup>2</sup>)

M<sub>g</sub>              =          moisture saturation factor

q<sub>a</sub>               =          mixing ratio for air

q<sub>af</sub>               =          mixing ratio for air within foliage canopy

q<sub>f,sat</sub>             =          saturation mixing ratio at foliage temperature

q<sub>g,sat</sub>            =          saturation mixing ratio at ground temperature

r<sub>a</sub>                =          aerodynamic resistance to transpiration (s/m)

r<sub>s</sub>                =          foliage leaf stomatal resistance (s/m)

r<sub>s,min</sub>            =          minimal leaf stomatal resistance (s/m)

r”                =          surface wetness factor

R<sub>ib</sub>              =          bulk Richardson number

R<sub>v</sub>               =          gas constant for water vapor (461.53 J/kgK)

*T<sub>a     \\                        </sub>* =          the air temperature at the instrument height (Kelvin)

T<sub>af     </sub>             =          air temperature with in the canopy (Kelvin)

T*<sub>f</sub>*               =          leaf temperature (Kelvin)

T<sub>g</sub>                =          ground surface temperature (Kelvin)

W               =          wind speed above canopy (m/s)

W<sub>af</sub>              =          wind speed with in the canopy (m/s)

z                 =          height or depth (m)

Z<sub>a</sub>               =          instrument height (m)

Z<sub>d</sub>               =          displacement height (m)

Z<sub>o</sub><sup>f</sup>               =          foliage roughness length scale (m)

#### Greek letters

a<sub>f</sub>                =          albedo (short wave reflectivity) of the canopy

a<sub>g</sub>               =          albedo (short wave reflectivity) of ground surface

<sub><span>\({\varepsilon_1}\)</span></sub>              =          <sub><span>\({\varepsilon_g} + {\varepsilon_f} - {\varepsilon_f}{\varepsilon_g}\)</span></sub>

<sub><span>\({\varepsilon_f}\)</span>  </sub>             =          emissivity of canopy

<sub><span>\({\varepsilon_g}\)</span>  </sub>             =          emissivity of the ground surface

G<sub>h</sub>                    =             stability factor

r<sub>a</sub>               =          density of air at instrument height (kg/m<sup>3</sup>)

r<sub>f</sub>                =          density of air at foliage temperature (kg/m<sup>3</sup>)

r<sub>af</sub>               =          density of air at foliage temperature (kg/m<sup>3</sup>)

r<sub>ag</sub>              =          density of air at ground surface temperature (kg/m<sup>3</sup>)

<span>\(\sigma \)</span>              =          the Stefan-Boltzmann constant (5.699\*10<sup>-8</sup> W/m<sup>2</sup> ºK<sup>4</sup>)

<span>\({\sigma_f}\)</span>                        =          fractional vegetation coverage

#### Subscripts and superscripts

a                =          air

af               =          air within the foliage layer

e                =          latent heat flux term

f                 =          foliage surface

g                =          ground surface

h                =          sensible heat flux term

n                =          current time step

n+1             =          future time step

ir                =          infrared (or long-wave)

sat              =          saturation value

S                =          short-wave

### References

ASHRAE. 2005. 2005 ASHRAE Handbook – Fundamentals. Chapter 16, Air flow Around Buildings, Atlanta: American Society of Heating Refrigerating and Air-Conditioning Engineers, Inc.

Balick, L. R., R. K. Scoggins, and L. E. Link. 1981. Inclusion of a simple vegetation layer in terrain temperature models for thermal IR signature prediction. IEEE Geoscience and Remote Sensing GE-19(3), pp.143-152.

Businger, J. A. 1966. In ‘Arctic Heat Budget and Atmospheric Circulation’, Symposium Proceedings, pp. 305-332. The Rand Corporation.

Deardorff, J.W. 1978. “Efficient Prediction of ground surface temperature and moisture with inclusion of a layer of vegetation”, Journal Geophysical Research, pp. 1889-1902.

Dickinson, R.E., A. Henderson-Sellers, P.J. Kennedy, and M.F. Wilson. 1986. Biosphere-Atmosphere Transfer Scheme (BATS) for the NCAR community climate model. NCAR Technical Note, TN-275+STR.

ECMWF. 2002. European Centre for Medium-Range Weather Forecasts, Integrated Forecast System. Documentation, CY25R1 (Operational implementation 9 April 2002). .

Frankenstein, S., and G. Koenig. 2004.FASST Vegetation Models. U. S. Army Engineer Research and Development Center, Cold regions Research and Engineering Laboratory, ERDC/CRREL Technical Report TR-04-25.

Frankenstein, S., and G. Koenig. 2004.Fast All-season Soil Strength (FASST). U.S. Army Engineer Research and Development Center, Cold regions Research and Engineering Laboratory, ERDC/CRREL Special Report SR-04-1.

Garratt, J.R. 1992. The Atmospheric Boundary Layer, Cambridge university press.

Gates, D.M. 1980. Biophysical Ecology. New York: Springer-Verlag

Guymon, G.L., R.L. Berg, and T.V. Hromadka. 1993. Mathematical Model of Frost Heave and Thaw Settlement in Pavements. U.S. Army Cold Regions Research and Engineering Laboratory, CRREL Report 93-2.

Henderson-Sellers, B. 1984. “A New Formula for Latent Heat of Vaporization of water as function of temperature”, Quarterly Journal Royal Meteorological Society, 10 pp. 1186-1190.

Hughes, P.A., T.J.L. McComb, A.B. Rimmer, and K.E. Turver. 1993. “A mathematical model for the prediction of temperature of man-made and natural surfaces”, International Journal of Remote Sensing 14 (7), pp. 1383-1412.

Koenig, G.G. 1994. Smart Weapons Operability Enhancement (SWOE) Joint Test and Evaluation (JT and E) Program: Final Report. Dr. James P. Welch, Joint Test Director, SWOE JT and E, SWOE Report 94-10, Annex D.

Lumley, J. L. and Panofsky, H. A. 1964. ‘The structure of Atmospheric Turbulence’. Interscience Monographs and Texts in Physics and Astronomy, Vol. XII. Wiley, New York.

Oke, T.R. 1987. Boundary Layer Climates, University Press, Cambridge

Sellers, P.J., Y. Mintz, Y.C. Sud, and A. Dalcher. 1986. A simple biosphere model (SiB) for use within general circulation models. Journal of Atmospheric Science, 43 (6), pp. 505-532.
