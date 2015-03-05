# Solar Collectors

Solar collectors are devices that convert solar energy into thermal energy by raising the temperature of a circulating heat transfer fluid.  The fluid can then be used to heat water for domestic hot water usage or space heating.  Flat-plate solar collectors using water as the heat transfer fluid, Integral-Collector Storage solar collectors using water and unglazed transpired solar collectors using air are currently the only types of collector available in EnergyPlus.

## Flat-Plate Solar Collectors

The input object SolarCollector:FlatPlate:Water provides a model for flat-plate solar collectors that are the most common type of collector.  Standards have been established by ASHRAE for the performance testing of these collectors (ASHRAE 1989; 1991) and the Solar Rating and Certification Corporation (SRCC) publishes a directory of commercially available collectors in North America (SRCC 2003).

The EnergyPlus model is based on the equations found in the ASHRAE standards and Duffie and Beckman (1991).  This model applies to glazed and unglazed flat-plate collectors, as well as banks of tubular, i.e. evacuated tube, collectors.

### Solar and Shading Calculations

The solar collector object uses a standard EnergyPlus surface in order to take advantage of the detailed solar and shading calculations.  Solar radiation incident on the surface includes beam and diffuse radiation, as well as radiation reflected from the ground and adjacent surfaces.  Shading of the collector by other surfaces, such as nearby buildings or trees, is also taken into account.  Likewise, the collector surface can shade other surfaces, for example, reducing the incident radiation on the roof beneath it.

### Thermal Performance

The thermal efficiency of a collector is defined as the ratio of the useful heat gain of the collector fluid versus the total incident solar radiation on the gross surface area of the collector.

![](media/image6073.png)\


where

q = useful heat gain

A = gross area of the collector

I~solar~ = total incident solar radiation

Notice that the efficiency  is only defined for I~solar~ > 0.

An energy balance on a solar collector with double glazing shows relationships between the glazing properties, absorber plate properties, and environmental conditions.

![](media/image6074.png)\


where

~g1~ = transmittance of the first glazing layer

~g2~ = transmittance of the second glazing layer

~abs~ = absorptance of the absorber plate

R~rad~ = radiative resistance from absorber to inside glazing

R~conv~ = convective resistance from absorber to inside glazing

R~cond~ = conductive resistance from absorber to outdoor air through the insulation

T~abs~ = temperature of the absorber plate

T~g2~ = temperature of the inside glazing

T~air~ = temperature of the outdoor air

The equation above can be approximated with a simpler formulation as:

![](media/image6075.png)\


where

F~R~ = an empirically determined correction factor

() = the product of all transmittance and absorptance terms

U~L~ = overall heat loss coefficient combining radiation, convection, and conduction terms

T~in~ = inlet temperature of the working fluid

Substituting this into Equation ,

![](media/image6076.png)\


A linear correlation can be constructed by treating F~R~() and -F~R~U~L~ as characteristic constants of the solar collector:

![](media/image6077.png)\


Similarly, a quadratic correlation can be constructed using the form:

![](media/image6078.png)\


Both first- and second-order efficiency equation coefficients are listed in the *Directory of SRCC Certified Solar Collector Ratings*.

### Incident Angle Modifiers

As with regular windows the transmittance of the collector glazing varies with the incidence angle of radiation.  Usually the transmittance is highest when the incident radiation is normal to the glazing surface.  Test conditions determine the efficiency coefficients for normal incidence.  For off-normal angles, the transmittance of the glazing is modified by an *incident angle modifier* coefficient.

![](media/image6079.png)\


Additional testing determines the incident angle modifier as a function of incident angle .  This relationship can be fit to a first-order, linear correlation:

![](media/image6080.png)\


or a second-order, quadratic correlation:

![](media/image6081.png)\


The incident angle modifier coefficients *b~0~* and *b~1~* are usually negative, although some collectors have a positive value for *b~0~*.  Both first- and second-order incident angle modifier equation coefficients are listed in the *Directory of SRCC Certified Solar Collector Ratings*.

The SRCC incident angle modifier equation coefficients are only valid for incident angles of 60 degrees or less.  Because these curves can be valid yet behave poorly for angles greater than 60 degree, the EnergyPlus model cuts off collector gains for incident angles greater than 60 degrees.

For flat-plate collectors, the incident angle modifier is generally symmetrical.  However, for tubular collectors the incident angle modifier is different depending on whether the incident angle is parallel or perpendicular to the tubes.  These are called bi-axial modifiers.  Some special flat-plate collectors may also exhibit this asymmetry.  The current model cannot yet handle two sets of incident angle modifiers.  In the meantime it is recommended that tubular collectors be approximated with caution using either the parallel or perpendicular correlation.

Incident angle modifiers are calculated separately for sun, sky, and ground radiation.  The net incident angle modifier for all incident radiation is calculated by weighting each component by the corresponding modifier.

![](media/image6082.png)\


For sky and ground radiation the incident angle is approximated using Brandemuehl and Beckman's equations:

![](media/image6083.png)\


![](media/image6084.png)\


where  is the surface tilt in degrees.

The net incident angle modifier is then inserted into the useful heat gain equation :

![](media/image6085.png)\


Equation  is also modified accordingly.

![](media/image6086.png)\


### Outlet Temperature

Outlet temperature is calculated using the useful heat gain q as determined by Equation , the inlet fluid temperature T~in~, and the mass flow rate available from the plant simulation:

![](media/image6087.png)\


where

![](media/image6088.png)  = fluid mass flow rate through the collector

c~p~ = specific heat of the working fluid

Solving for T~out~,

![](media/image6089.png)\


If there is no flow through the collector, T~out~ is the stagnation temperature of the fluid.  This is calculated by setting the left side of Equation  to zero and solving for T~in~ (which also equals T~out~ for the no flow case).

### References

ASHRAE.  1989.  ASHRAE Standard 96-1980 (RA 89):  Methods of Testing to Determine the Thermal Performance of Unglazed Flat-Plate Liquid-Type Solar Collectors.  Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

ASHRAE.  1991.  ASHRAE Standard 93-1986 (RA 91):  Methods of Testing to Determine the Thermal Performance of Solar Collectors.  Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

Duffie, J. A., and Beckman, W. A.  1991.  Solar Engineering of Thermal Processes, Second Edition.  New York:  Wiley-Interscience.

Solar Rating and Certification Corporation.  2004.  Directory of SRCC Certified Solar Collector Ratings, OG 100.  Cocoa, Florida:  Solar Rating and Certification Corporation.

## Integral-collector-storage (ICS) Solar Collector

Solar collectors with integral storage unit models use SolarCollector:IntegralCollectorStorage object, and the characteristics parameter inputs of this collector are provided by the SolarCollectorPerformance:IntegralCollectorStorage object. This model is based on detailed Energy Balance equations of solar collectors that integrates storage in it. This model has two options to represent the collector bottom outside boundary conditions: AmbientAir, and OtherSideConditionsModel. AmbientAir simply applies outside air temperature using combined convection and radiation conductance, and the OtherSideConditionsModel applies combined radiation and convection models that exiats in a naturally ventilated cavity to represent the collector bottom outside boundary condition. The later boundary condition accounts for the shading of the collector on the underlying surface, hence, the ICS collector can be assumed as an integral part of the building envelope. Schamtic diagram of a rectangular ICS solar collector is shown in Figure 295 below:

![Schematic diagram of rectangular Integrated Collector Storage unit](media/schematic-diagram-of-rectangular-integrated.png)


### Solar and Shading Calculations

The solar collector object uses a standard EnergyPlus surface in order to take advantage of the detailed solar and shading calculations. Solar radiation incident on the surface includes beam and diffuse radiation, as well as radiation reflected from the ground and adjacent surfaces. Shading of the collector by other surfaces, such as nearby buildings or trees, is also taken into account. Likewise, the collector surface shades the roof surface beneath it, hence no direct solar radiation incident on the roof surface. The collector and the roof outside boundary conditions should be specified as OtherSideConditionModel to account for solar collector shading impact on the roof surface.

### Mathematical Model

The integral-collector-storage (ICS) solar collector is represented using two transient energy balance equations shown below.  These equations represent the energy balance equation for the absorber plate, and the water in the collector.

![](media/image6091.png)\


![](media/image6092.png)\


Where,

*m~p~C~p~*= thermal capacity of the absorber surface, J/°C

A= collector gross area, m^2^

*()*~e~= transmittance-absorptance product of the absorber plate and cover system

*I~t~* = total solar irradiation, (W/m^2^)

*h~pw~*= convective heat transfer coefficient from absorber plate to water, (W/m2°K)

*U~t~*= overall heat loss coefficient from absorber to the ambient air, (W/m2 °K)

*T~p~*= absorber plate average temperature, (°C)

*T~w~*= collector water average temperature, (°C)

*T~a~*= ambient air temperature, (°C)

*m~w~C~pw~* = thermal capacity of the water mass in the collector, (J/°C)

*U~s~*= area-weighted conductance of the collector side insulation, (W/m^2^°K)

*U~b~*= conductance of the collector bottom insulation, (W/m^2^°K)

*T~osc~*=outside temperature of bottom insulation determined from the other side condition model, (°C)

*T~wi~*= Entering makeup or mains water temperature, (°C)

![](media/image6093.png) = water capacity flow through the collector, (W/°C)

The other side condition model boundary condition represented by the *T~osc~*, allows us to apply a realistic outside boundary condition for a collector mounted on a building roof.  This also accounts for the shading impact of the collector on the under-laying surface (roof).  On the other hand if ambient air boundary condition is specified, then the collector does not shade the underlying surface it is mounted on.

The two energy balance equation can be written as non-homogeneous first order DE with constant coefficients. The initial conditions for these equations are the absorber plate average temperature and the collector water average temperature at previous time steps.

![](media/image6094.png)\


![](media/image6095.png)\


![](media/image6096.png)\


![](media/image6097.png)\


![](media/image6098.png)\


![](media/image6099.png)\


![](media/image6100.png)\


![](media/image6101.png)\


The two coupled first order differential equation are solved analytically.  Auxiliary equation of the the coupled homogeneous differential equation is given by:

![](media/image6102.png)\


This auxiliary quadratic equation has always two distinct real roots (~1~ and ~2~) hence the solution of the homogeneous equation is exponential, and the general solutions of the differential equations are given by:

![](media/image6103.png)\


![](media/image6104.png)\


The constant terms *A* and *B* are the particular solution of the non-homogeneous differential equations, the coefficients of the exponential terms (*c~1~*, *c~2~*, *r~1~*, and *r~2~*) are determined from the initial conditions of the absorber and collector water temperatures (*T~p0~*, *T~w0~*) and are given by:

![](media/image6105.png)\


![](media/image6106.png)\


![](media/image6107.png)\


### Thermal Network Model:

The thermal network model requires energy balance for each of the collector covers as well.  The heat balance equation of the collector covers is assumed to obey steady state formulation by ignoring their thermal mass. The thermal-network representation of the ICS collector is shown in Figure 296. Also, the heat balance at each cover surface requires knowledge of the amount of solar fraction absorbed, which is determined from the ray tracing analysis. For the thermal network model shown above the overall top heat loss coefficient is determined from combination of the resistances in series as follows:

![](media/image6108.png)\


Or

![](media/image6109.png)\


The convection and radiation heat transfer coefficients in equation above are calculated based on temperatures at the previous time step and determined as described in the *Heat Transfer Coefficients* section.

![Thermal network diagram for ICS Solar Collector](media/thermal-network-diagram-for-ics-solar.png)


**Collector Cover Heat Balance**

Ignoring the thermal mass of the collector cover, a steady state heat balance equations are formulated for each cover that allow us to determine cover temperatures.  The cover surface heat balance representation is shown in Figure 297 below.

![Collector Cover Surface Heat Balance](media/collector-cover-surface-heat-balance.png)


The steady state cover heat balance equation is given by:

**![](media/image6112.png)**

Linearizing the longwave radiation exchange and representing the convection terms using the classical equation for Newton's law of cooling, the equations for the temperatures of covers 1 and 2 are given by:

**![](media/image6113.png)**

**![](media/image6114.png)**

Where,

*~c~* = the weighted average solar absorptance of covers *1* and *2*, (-)

*h~r,c1-a~*= adjusted radiation heat transfer coefficient between cover *1* and the ambient air, (W/m^2^K)

*h~c,c1-a~*= convection heat transfer coefficient between cover *1* and the ambient, (W/m^2^K)

*h~r,c2-c1~*= radiation heat transfer coefficient between covers *1* and *2*, (W/m^2^K)

*h~c,c2-c1~*= convection heat transfer coefficient between covers *1* and *2*, (W/m^2^K)

*h~r,p-c2~*= radiation heat transfer coefficient between covers *2* and the absorber plate, (W/m^2^K)

*h~c,p-c2~*= convection heat transfer coefficient between covers *2* and the absorber plate, (W/m^2^K)

*q~LWR,1~*= longwave radiation exchange flux on side *1* of the collector cover, (W/m^2^)

*q~CONV,1~*= convection heat flux on side *1* of the collector cover, (W/m^2^)

*q~LWR,2~*= longwave radiation exchange flux on side *2* of the collector cover, (W/m^2^)

*q~CONV,2~*= convection heat flux on side *2* of the collector cover, (W/m^2^)

*q~solar,abs~*= net solar radiation absorbed by the collector cover, (W/m^2^)

*R*= thermal resistance for each section along the heat flow path, (m^2^K/W)

### Other Side Condition Model

ICS Solar Collectors are commonly mounted on building heat transfer surfaces hence the collectors shade the underlying heat transfer surface and require a unique boundary condition that reflects the air cavity environment created between the bottom of the collector surface and the underlying surface. The other side condition model that allows us to estimate the other side temperature, *T~osc~*, can be determined based on steady state heat balance using the known collector water temperature at the previous time step.

![Illustration for Other Side Condition Model](media/illustration-for-other-side-condition-model.png)


Ignoring thermal mass of the collector bottom insulation, steady state surface heat balance can be formulated on the outer plane of the collector bottom surface facing the cavity as shown in Figure 4.  The heat balance equation on the outer plane of the collector bottom surface is given by:

**![](media/image6116.png)**  ****

Substituting the equations for each term in the above equation yields:

**![](media/image6117.png)**  ****

Simplifying yields the bottom insulation other side condition temperature:

**![](media/image6118.png)**

The cavity air temperature is determined from cavity air heat balance as follows:

**![](media/image6119.png)**

Where

*h~r, cav~*= linearized radiation coefficient for underlying surface in the cavity, (W/m^2^K)

*h~c, cav~*= convection coefficient for underlying surface in the cavity, (W/m^2^K)

*T~so~*= the outside face temperature of the underlying heat transfer surface, (ºC)

![](media/image6120.png) = air mass flow rate due to natural ventilation, (kg/s)

*q~cond~*= conduction heat flux though the insulation bottom and, (W/m^2^)

*q~conv,cav~*= convection heat flux between the collector bottom outside surface and the cavity air, (W/m^2^)

*q~rad,cav~*=longwave radiation exchange flux between the collector bottom outside surface and the outside surface of the underlying surface, (W/m^2^)

The cavity air temperature is determined from the cavity air energy balance.  The air heat balance requires the ventilated cavity air natural ventilation rates.  The calculation of the ventilation rate is described else where in this document. The SurfaceProperty:ExteriorNaturalVentedCavity, object is required to describe the surface properties, the characteristics of the cavity and opening for natural ventilation.

### Heat Transfer Coefficients

The equations used to determine for the various heat transfer coefficients in the absorber and water heat balance equations are given below. The absorbed solar energy is transferred to the water by convection.  Assuming natural convection dominated heat transfer for a hot surface facing down and a clod surface facing down the following correlation for Nusselt number by Fujii and Imura (1972).  The Nusselt number for hot surface facing down ward is given by:

![](media/image6121.png)\


The Nusselt number for hot surface upward and cold surface facing down is given by:

![](media/image6122.png)\


![](media/image6123.png)\


**![](media/image6124.png)**  ****

**![](media/image6125.png)**  ****

**![](media/image6126.png)**  ****

![](media/image6127.png)\


Where,

![](media/image6128.png) = angle of inclination of the collector to the vertical, radians

*g* = gravitation force constant, 9.806 (m/s^2^)

*T~r~*= reference properties where the thermo-physical properties are calculated, (°C)

*L~c~*= characteristic length for the absorber plate, (m)

*k*= thermal conductivity of water at reference temperature, (W/mK)

*= kinematic viscosity of water at reference temperature, (m*^2^/s)

*= thermal diffusivity of water at reference temperature, (m*^2^/s)

*β~v~*= volumetric expansion coefficient evaluated at Tv, Tv =Tw+0.25(Tp-Tw), (K-1)

*Nu*= Nusselt number calculated for water properties at the reference temperature, (-)

*Gr*= Grashof number calculated for water properties at the reference temperature, (-)

*Pr*= Prandtle number calculated for water properties at the reference temperature, (-)

The various radiation and convection heat transfer coefficients are given by the following equations.  The convection heat transfer coefficients between the covers and the absorber plate are estimated from the empirical correlation for the Nusselt number for air gap between two parallel plates developed by Hollands et al. (1976) is:

**![](media/image6129.png)**

**![](media/image6130.png)**

**![](media/image6131.png)**

**![](media/image6132.png)**

The long wave radiation exchange coefficients between the outer collector cover and the sky and ground referencing the ambient air temperature for mathematical simplification are given.

**![](media/image6133.png)**

**![](media/image6134.png)**

**![](media/image6135.png)**

The convection heat transfer coefficient from the outer cover to the surrounding air is given by:

**![](media/image6136.png)**

When the bottom surface boundary condition is AmbientAir, the combined conductance from the outer cover to the surrounding is calculated from the equation below (Duffie and Beckman, 1991).

**![](media/image6137.png)**

The overall loss coefficient through the bottom and side of the collector-storage is estimated as follows:

![](media/image6138.png)\


![](media/image6139.png)\


Where,

*~c1~* = thermal emissivity of collector cover *1*, (-)

*~c2~* = thermal emissivity of collector cover *2*, (-)

*F~s~* = view factor from the collector to the sky, (-)

*F~g~* = view factor from the collector to the ground, (-)

*T~c1~* = temperature of collector cover *1*, (K)

*T~c2~* = temperature of collector cover *2*, (K)

*T~s~* = sky temperature, (K)

*T~g~*= ground temperature, (K)

*k* = thermal conductivity of air, (W/m K)

*L* = air gap between the covers, (m)

*β* = inclination of the plates or covers to the horizontal, (radian)

*V~w~* = wind speed, (m/s)

*U~Lb~*= user specified bottom heat loss conductance, W/m^2^K

*U~Ls~*= user specified side heat loss conductance, W/m^2^K

*A~b~*= collector bottom heat transfer area, m^2^

*A~s~*= collector side area, m^2^

*h~comb~*= combined conductance from the outer cover to the ambient air, W/m^2^K

**Transmittance-Absorptance Product**

The transmittance-absorptance product of solar collector is determined using ray tracing method for any incident angle (Duffie and Beckman, 1991).  This requires optical properties of the cover and absorber materials and the the transmittance-absorptance product for any incident angle is given by:

![](media/image6140.png)\


The transmittance of the cover system for single and two cover are given by:

![](media/image6141.png)\


![](media/image6142.png)\


The effective transmittance, reflectance and absorptance of a single cover are given by:

**![](media/image6143.png)**

![](media/image6144.png)\


![](media/image6145.png)\


The transmittance of the cover system with absorption only considered *~a~*, is defined as:

**![](media/image6146.png)**

![](media/image6147.png)\


The reflectance of un-polarized radiation on passing from medium 1 with reflective index *n*~1~ to medium 2 with reflective index *n*~2~ is given by:

 ![](media/image6148.png)

**![](media/image6149.png)**

The sky and ground reflected diffuse radiations average equivalent incident angles are approximated by Brandemuehl and Beckman correlation (Duffie and Beckman, 1991) as follows:

**![](media/image6150.png)**

**![](media/image6151.png)**

where,

 **= transmittance of the cover system, (-)

*~1~* = transmittance of the cover 1, (-)

*~2~* = transmittance of the cover 2, (-)

 **= absorptance of the absorber plate, (-)

*~d~*= diffuse reflectance of the inner cover, (-)

*L* = thickness of a cover material, (m)

*K* = extinction coefficient of a cover material, (m^-1^)

*~1~* = angle of incidence, degree

*~2~*= angle of refraction, degree

![](media/image6152.png)  = parallel component of reflected un-polarized radiation, (-)

![](media/image6153.png) = perpendicular component of reflected un-polarized radiation, (-)

 **= slope of the collector, degree

*~sd~* = equivalent incident angle for sky diffuse solar radiation, degree

*~gd~*= equivalent incident angle for ground diffuse solar radiation, degree

The integral collector storage unit thermal performance parameters are calculated as follows:

![](media/image6154.png)\


![](media/image6155.png)\


![](media/image6156.png)\


![](media/image6157.png)\


## References:

Duffie, J.A., and W.A. Beckman. 1991. *Solar Engineering of Thermal Processes,* 2d ed. New York: John Wiley & Sons.

Kumar, R. and M.A. Rosen. Thermal performance of integrated collector storage solar water heater with corrugated absorber surface. Applied Thermal Engineering:  30 (2010) 1764–1768.

Fujii, T., and H. Imura. Natural convection heat transfer from aplate with arbitrary inclination. International Journal of Heat and Mass Transfer: 15(4), (1972), 755-764.

## Photovoltaic Thermal Flat-Plate Solar Collectors

Photovoltaic-Thermal solar collectors (PVT) combine solar electric cells and thermal working fluid to collect both electricity and heat.  Athough there are currently comparatively few commercial products, PVT research has been conducted for the past 30 years and many different types of collectors have been studied.  Zondag (2008) and Charalambous et. al (2007) provide reviews of the PVT literature.  Because PVT is much less commercially-mature, there are no standards or rating systems such as for thermal-only, hot-water collectors.  EnergyPlus currently has one simple model based on user-defined efficiencies but a more detailed model based on first-principles and a detailed, layer-by-layer description is under development.

The PVT models reuse the PV models for electrical production. These are described elsewhere in this document in the section on Photovoltaic Arrays-Simple Model

### Simple PVT Thermal Model

The input object SolarCollector:FlatPlate:PhotovoltaicThermal provides a simple PVT model that is provided for quick use during design or policy studies.  The user simply provides values for a thermal efficiency and the incident solar heats the working fuild.  The model also includes a cooling mode for air-based systems where a user-provided surface emmittance is used to model cooling of the working fluid to the night sky (water-based cooling will be made available once a chilled water storage tank is available).  No other details of the PVT collector's construction are required as input data.

The simple model can heat either air or liquid.  If it heats air, then the PVT is part of HVAC air system loop with air nodes connected to an air system.  If it heats liquid, then the PVT is part of plant loop with nodes connected to a plant loop and the plant operating scheme determines flows.

Air-system-based PVT modeling include a modulating bypass damper arrangement.  Control logic decides if the air should bypass the collector to better meet setpoint.  The model requires a drybulb temperature setpoint be placed on the outlet node.  The model assume the collector is intended and available for heating when the incident solar is greater than 0.3 W/m^2^ and otherwise it is intended for cooling.  The inlet temperature is compare to the setpoint on the outlet node to determine if cooling or heating are beneficial.  If they are, then the PVT thermal models are applied to condition the air stream.  If they are not beneficial, then the PVT is completely bypassed and the inlet node is passed directly to the outlet node to model a completely bypassed damper arrangement.  A report variable is available for bypass damper status.

Plant-based PVT do not include a bypass (although one could be used in the plant loop).  The collector requests its design flow rate but it otherwise relies on the larger plant loop for control.

When the PVT themal collector is controlled to be "on," in heating mode, and working fluid is flowing, the model calculates the outlet temperature based on the inlet temperature and the collected heat using the following equations.

![](media/image6158.png)\


where,

![](media/image6159.png)  is the thermal energy collected [W]

![](media/image6160.png)  is the net area of the surface [m^2^]

![](media/image6161.png) is the fraction of surface aire with active PV/T collector, and

![](media/image6162.png) is the thermal conversion efficiency.

![](media/image6163.png)\


where,

![](media/image6164.png)  is the temperature of the working fluid leaving the PV/T

![](media/image6165.png)  is the temperature of the working fluid entering the PV/T

![](media/image6166.png)  is the entire mass flow of the working fluid through the PV/T

![](media/image6167.png)  is the specific heat of the working fluid.

For air-based systems, the value of ![](media/image6164.png) is then compared to the temperature setpoint on the outlet node.  If ![](media/image6164.png)  exceeds the desired outlet temperature, ![](media/image6168.png) ,then a bypass fraction is calculated to model a modulating bypass damper using:

![](media/image6169.png)\


When the PVT themal collector is controlled to be "on," in cooling mode, and working fluid is flowing, the model calculates the outlet temperature based on the inlet temperature and the heat radiated and convected to the ambient using a heat balance on the outside face of the collector:

![](media/image6170.png)\


Where,

![](media/image6171.png)  is the net rate of long wavelength (thermal) radiation exchange with the air, night sky, and ground.  See the section "External Longwave Radiation" in the Outside Surface Heat Balance, for full discussion of how this modeled in EnergyPlus using linearized radiation coefficients.

![](media/image6172.png)  is the net rate of convective flux exchange with outdoor air. See the section "Exterior/External Convection" in the Outside Surface Heat Balance, for full discussion of how this modeled in EnergyPlus.  The surface roughness is assumed to be "very smooth."

The simple model assumes that the effective collector temperature, ![](media/image6173.png) , is the average of the working fluid inlet and outlet temperatures so that we can make the following substitution:

![](media/image6174.png)\


Substituting and solving for ![](media/image6175.png)  we obtain the following model for collector temperatures during a (possible) cooling process :

![](media/image6176.png)\


Then the outlet temperature can be calculated and heat losses determined.  However, the model allows only sensible cooling of the air stream and limits the outlet temperature to not go below the dewpoint temperature of the inlet.

PVT collectors have a design volume flow rate for the working fluid that is autosizable.  For air-based systems used as pre-conditioners, the volume flow rate is sized to meet the maximum outdoor air flow rate.  For water-based systems on the supply side of a plant loop, each of the PVT collectors are sized to the overall loop flow rate.  For water-based systems on the demand side of a plant loop, the collectors are sized using a rule-of-thumb for typical flow rates per unit of collector area.  This rule-of-thumb is based on a constant factor of 1.905x10^-5^ m^3^/s-m^2^^^that was developed by analyzing SRCC data set for conventional solar collectors (see data set SolarCollectors.idf) and averaging the ratio for all 171 different collectors.

### References

Charalambous, P.G., Maidment, G.G., Kalagirou, S.A., and Yiakoumetti, K., Photovoltaic thermal (PV/T) collectors: A review. Applied Thermal Engineering 27 (2007) 275-286.

Zondag, H.A. 2008. Flat-plate PV-Thermal collectors and systems: A review.  Renewable and Sustainable Energy Reviews 12 (2008) 891-959.

## Unglazed Transpired Solar Collectors

The input object SolarCollector:UnglazedTranspired provides a model for transpired collectors that are perhaps one of the most efficient ways to collect solar energy with demonstrated instantaneous efficiencies of over 90% and average efficiencies of over 70%.  They are used for preheating outdoor air needed for ventilation and processes such as crop drying.

In EnergyPlus, an unglazed transpired solar collector (UTSC) is modeled as a special component attached to the outside face of a heat transfer surface that is also connected to the outdoor air path.  A UTSC affects both the thermal envelope and the HVAC air system.  From the air system's point of view, a UTSC is heat exchanger and the modeling needs to determine how much the device raises the temperature of the outdoor air.  From the thermal envelope's point of view, the presence of the collector on the outside of the surface modifies the conditions experienced by the underlying heat transfer surfaces.  EnergyPlus models building performance throughout the year and the UTSC will often be "off" in terms of forced airflow, but the collector is still present.  When the UTSC is "on" there is suction airflow that is assumed to be uniform across the face.  When the UTSC is "off" the collector acts as a radiation and convection baffle situated between the exterior environment and the outside face of the underlying heat transfer surface.  We distinguish these two modes of operation as *active* or *passive* and model the UTSC component differently depending on which of these modes it is in.

### Heat Exchanger Effectiveness

The perforated absorber plate is treated as a heat exchanger and modeled using a traditional effectiveness formulation.  The heat exchanger effectiveness, ![](media/image6177.png) , is determined from correlations derived from small-scale experiments.  Two correlations available in the literature are implemented in EnergyPlus.  The first is based on research by Kutscher at the National Renewable Energy Laboratory. The second is based on the research by Van Decker, Hollands, and Brunger at the University of Waterloo.  Because both correlations are considered valid, the choice of which correlation to use is left to the user.

### Kutscher Correlation

Kutscher's (1994) correlation encompasses surface convection between the collector and the incoming outdoor air stream that occurs on the front face, in the holes, and along the back face of the collector. The correlation uses a Reynolds number based on the hole diameter as a length scale and the mean velocity of air as it passes through the holes as the velocity scale:

![](media/image6178.png)\


where,

![](media/image6179.png)  is the velocity through the holes [m/s]

![](media/image6180.png)  is the hole diameter [m]

![](media/image6181.png)  is the kinematic viscosity of air  [m^2^/s]

The correlation is a function of Reynolds number, hole geometry, the free stream air velocity, and velocity through the holes:

![](media/image6182.png)\


where,

![](media/image6183.png)  is the pitch, or distance between holes, [m],

![](media/image6180.png)  is the diameter of the holes, [m],

![](media/image6184.png)  is the porosity, or area fraction of the holes, [dimensionless],

![](media/image6179.png)  is the mean velocity of air passing through the holes, [m/s],

![](media/image6185.png)  is the free stream velocity (local wind speed) [m/s].

The Nusselt number is formulated as:

![](media/image6186.png)\


where,

![](media/image6187.png) is the overall heat transfer coefficient based on log mean temperature difference, [W/m^2^·K], and

![](media/image6188.png)  is the thermal conductivity of air [W/m·K].

The heat exchanger effectiveness is:

![](media/image6189.png)\


Kutscher's relation was formulated for triangular hole layout, but based on Van Decker et al. (2001) we allow using the correlation for square hole layout and scale ![](media/image6183.png)  by a factor of 1.6.

### Van Decker, Hollands, and Brunger Correlation

Van Decker et. al. extended Kutscher's measurements to include a wider range of collector parameters including plate thickness, pitch, suction velocities, and square hole patterns.  Their model formulation differs from Kutscher's in that the model was built up from separate effectiveness models for the front, back, and holes of the collector.  Their published correlation is:

![](media/image6190.png)\


where,

![](media/image6191.png)\


![](media/image6192.png)\


![](media/image6193.png)\


![](media/image6194.png)  is the average suction velocity across the front face of the collector [m/s]

![](media/image6195.png)  is the collector plate thickness

### Heat Exchanger Leaving Temperature

Using either of the correlations above allows determining the heat exchanger effectiveness from known values.  By definition the heat exchanger effectiveness is also:

![](media/image6196.png)\


where,

![](media/image6197.png)  is the temperature of the air leaving the collector and entering the plenum [ºC]

![](media/image6198.png)  is the temperature of the collector's absorber plate, [ºC], and

![](media/image6199.png)  is the temperature of the ambient outdoor air [ºC].

By rewriting equation  to solve for ![](media/image6197.png)  we see that the temperature of the heated outdoor air entering the plenum can be determined once the collector surface temperature is known,

![](media/image6200.png)\


### Collector Heat Balance

The collector is assumed to be sufficiently thin and high-conductivity so that it can be modeled using a single temperature (for both sides and along its area).  This temperature ![](media/image6198.png) is determined by formulating a heat balance on a control volume that just encapsulates the collector surface.  The heat balances are formulated separately for active and passive modes and are diagrammed in the following figure.

Observe that for the passive case, we do not use the heat exchanger relations to directly model the interaction of ventilating air with the collector.  This is because these relations are considered to not apply when the UTSC is in passive mode.  They were developed for uni-directional flow (rather than the balanced-in-and-out flow expected from natural forces) and for specific ranges of suction face velocity.  Therefore, this heat transfer mechanism is handled using classical surface convection models (as if the collector was not perforated).  (Air exchanges are modeled as ventilation in the plenum air heat balance but do not interact with the hole edges in the collector surface.)

![Transpired Collector Heat Balance](media/transpired-collector-heat-balance.png)


When the UTSC is active, the heat balance on the collector surface control volume is:

![](media/image6202.png)\


where:

![](media/image6203.png) ~~is absorbed direct and diffuse solar (short wavelength) radiation heat flux.

![](media/image6204.png)  is net long wavelength (thermal) radiation flux exchange with the air and surroundings.

![](media/image6205.png)  is surface convection flux exchange with outdoor air under high wind and rain conditions.  Note that this term is usually assumed to be zero in UTSC model development but we add the term to allow for deteriorated performance of the UTSC under poor conditions.

![](media/image6206.png) ~~is heat exchanger flux from collector to incoming outdoor air.

![](media/image6207.png) ~~is net long wavelength (thermal) radiation flux exchange with the outside face of the underlying surface(s).

![](media/image6208.png)  is a source/sink term that accounts for energy exported out of the control volume when the collecter's absorber plate is a hybrid device such as a photovoltaic panel.

While the heat balance on the passive collector surface control volume is:

![](media/image6209.png)\


where:

![](media/image6210.png) ~~= surface convection flux exchange with outdoor air.

![](media/image6211.png) ~~= surface convection flux exchange with plenum air.

All terms are positive for net flux to the collector except the heat exchanger term, which is taken to be positive in the direction from collector to incoming air stream.  Each of these heat balance components is introduced briefly below.

### External SW Radiation

![](media/image6212.png) ~~ is calculated using procedures presented elsewhere in this manual and includes both direct and diffuse incident solar radiation absorbed by the surface face.  This is influenced by location, surface facing angle and tilt, shading surfaces, surface face material properties, weather conditions, etc.

### External LW Radiation

![](media/image6213.png)  is a standard radiation exchange formulation between the surface, the sky, the ground, and the atmosphere.  The radiation heat flux is calculated from the surface absorptivity, surface temperature, sky, air, and ground temperatures, and sky and ground view factors.  Radiation is modeled using linearized coefficients.

### External Convection

![](media/image6214.png) ~~is modeled using the classical formulation: ![](media/image6215.png)  ~~= h~co~(T~air~ - T~o~) where h~co~, is the convection coefficient.  This coefficient will differ depending on whether or not the UTSC is active or passive.  When the UTSC is passive, h~co~ is treated in the same way as an outside face with ExteriorEnvironment conditions.  When the UTSC is active, the special suction airflow situation of a transpired collector during operation means that h~co~is often zero because the suction situation can eliminate mass transport away from the collector.  However when the winds are high, the strong turbulence and highly varying pressures can cause the suction flow situation to breakdown.  Therefore, we include the ![](media/image6216.png)  term in the heat balance and use a special coefficient ![](media/image6217.png)  to model this lost heat transfer.  In addition, when it is raining outside, we assume the collector gets wet and model the enhanced surface heat transfer using a large value for ![](media/image6217.png) .

### Heat Exchanger

![](media/image6218.png) ~~is modeled using the classical formulation ![](media/image6219.png) ~~where ![](media/image6197.png)  is determined using correlations described above.  When the UTSC is active, the air mass flow is determined from the operation of the outdoor air mixer component.  When the UTSC is off, this term is zero.

### Plenum LW Radation

![](media/image6220.png) is a standard radiation exchange formulation between the collector surface and the underlying heat transfer surface located across the plenum.  Radiation is modeled using linearized coefficients.

### Plenum Convection

![](media/image6221.png) ~~is modeled using the classical formulation: ![](media/image6222.png)  ~~= h~cp~(T~air~ - T~o~) where h~cp~, is the convection coefficient.  This coefficient is taken as zero when the UTSC is operating because of the suction airflow situation.  When the UTSC is off, the value for h~cp~ is obtained from correlations used for window gaps from ISO (2003) standard 15099.

Substituting models into  and solving for ![](media/image6198.png)  yields the following equation when the UTSC is active ("on"):

![](media/image6223.png)\


and substituting into  yields the following equation when the UTSC is passive ("off"):

![](media/image6224.png)\


where,

![](media/image6225.png)  is the incident solar radiation of all types [W/m^2^],

![](media/image6226.png)  is the solar absorptivity of the collector [dimensionless],

![](media/image6227.png) is the linearized radiation coefficient for the surrounding atmosphere [W/m^2^·K],

![](media/image6228.png)  is the outdoor drybulb from the weather data, also assumed for ground surface [ºC],

![](media/image6229.png)  is the linearized radiation coefficient for the sky [W/m^2^·K],

![](media/image6230.png)  is the effective sky temperature [ºC],

![](media/image6231.png)  is the linearized radiation coefficient for the ground [W/m^2^·K],

![](media/image6232.png)  is the linearized radiation coefficient for the underlying surface [W/m^2^·K],

![](media/image6217.png)  is the convection coefficient for the outdoor environment when the UTSC is active and winds are high or it is raining [W/m^2^·K],

![](media/image6233.png)  is the temperature of the outside face of the underlying heat transfer surface [ºC],

![](media/image6234.png)  is the air mass flow rate when in active mode [kg/s],

![](media/image6235.png)  is the specific heat of air at constant pressure [J/kg·K],

![](media/image6236.png)  is the area of the collector [m^2^],

![](media/image6237.png)  is the convection coefficient for the outdoor environment [W/m^2^·K],

![](media/image6238.png)  is the convection coefficient for the surfaces facing the plenum [W/m^2^·K], and

![](media/image6239.png)  is the air drybulb for air in the plenum and entering the outdoor air system [ºC].

### Plenum Heat Balance

The *plenum* is the volume of air located between the collector and the underlying heat transfer surface.  The plenum air is modeled as well-mixed.  The uniform temperature of the plenum air, ![](media/image6239.png) , is determined by formulating a heat balance on a control volume of air as diagrammed below.

Note that we have formulated the control volumes with slight differences for the active and passive cases.  For the active case, the suction air situation and heat exchanger effectiveness formulations dictate that the collector surface control volume encompass part of the air adjacent to both the front and back surfaces of the collector.  However for the passive case, the collector surface control volume has no air in it and the plenum air control volume extends all the way to the surface of the collector.

![Transpired Collector Plenum Air Heat Balance](media/transpired-collector-plenum-air-heat-balance.png)


When the UTSC is active, the heat balance on the plenum air control volume is:

![](media/image6241.png)\


where,

![](media/image6242.png) is the net rate of energy added by suction air convecting through the control volume.

![](media/image6243.png) ~~ is the net rate of energy added by surface convection heat transfer with the underlying surface.

When the UTSC is passive, the heat balance on the plenum air control volume is:

![](media/image6244.png)\


where,

![](media/image6245.png)  is the net rate of energy added from infiltration – where outdoor ambient air exchanges with the plenum air.

![](media/image6246.png) is the net rate of energy added by surface convection heat transfer with the collector.

Substituting into  and solving for ![](media/image6239.png)  yields the following equation for when the UTSC is active:

![](media/image6247.png)\


And substituting into  yields the following equation when the UTSC is passive:

![](media/image6248.png)\


where,

![](media/image6249.png)  is the air mass flow from natural forces [kg/s]

The literature on UTSC does not appear to address the passive mode of operation and no models for ![](media/image6250.png)  have been identified.  Nevertheless, natural buoyancy and wind forces are expected to drive air exchange between the plenum and ambient and some method of modeling ![](media/image6251.png)  is needed.  Reasoning that the configuration is similar to single-side natural ventilation, we elect to use correlations for natural ventilation presented in Chapter 26. of ASHRAE HOF (2001).

![](media/image6252.png)\


where,

![](media/image6253.png)  is the density of air [kg/m^3^], and

![](media/image6254.png)  is the total volumetric flow rate of air ventilating in and out of the plenum.

![](media/image6255.png)\


![](media/image6256.png)   (if ![](media/image6257.png) )

![](media/image6258.png)   (if ![](media/image6259.png)  and UTSC vertical)

![](media/image6260.png)  is the effectiveness of the openings that depends on opening geometry and the orientation with respect to the wind.  ASHRAE HoF (2001) indicates values ranging from 0.25 to 0.6.  In the UTSC model, this value is available for user input and defaulted to 0.25.

![](media/image6261.png)  is the discharge coefficient for the opening and depends on opening geometry.  In the UTSC model, this value is available for user input and defaulted to 0.65.

Mass continuity arguments lead to modeling the area of the openings as one half of the total area of the holes, so we have:

![](media/image6262.png)\


![](media/image6263.png)  is the gravitational constant taken as 9.81 [m/s^2^].

![](media/image6264.png)  is the height from midpoint of lower opening to the Neutral Pressure Level.  This is taken as one-fourth the overall height of the UTSC if it is mounted vertically.  For tilted collectors, the nominal height is modified by the sine of the tilt.  If the UTSC is mounted horizontally (e.g. on the roof) then the ![](media/image6265.png)  is taken as the gap thickness of the plenum.

If the UTSC is horizontal and ![](media/image6266.png)  then ![](media/image6267.png)  because this is a stable situation.

### Underlying Heat Transfer Surface

The UTSC is applied to the outside of a heat transfer surface.  This surface is modeled using the usual EnergyPlus methods for handling heat capacity and transients – typically the CTF method.  These native EnergyPlus Heat Balance routines are used to calculate ![](media/image6233.png) .  The UTSC model is coupled to the underlying surface using the OtherSideConditionsModel mechanism.  The UTSC model provides values for ![](media/image6232.png) ,![](media/image6198.png) , ![](media/image6238.png) , and ![](media/image6239.png)  for use with the Heat Balance Model calculations for the outside face of the underlying surface (described elsewhere in this manual).

### Solar and Shading Calculations

The transpired collector object uses a standard EnergyPlus surface in order to take advantage of the detailed solar and shading calculations.  Solar radiation incident on the surface includes beam and diffuse radiation, as well as radiation reflected from the ground and adjacent surfaces.  Shading of the collector by other surfaces, such as nearby buildings or trees, is also taken into account.

### Local Wind Speed Calculations

The outdoor wind speed affects terms used in modeling UTSC components.  The wind speed in the weather file is assumed to be measured at a meteorological station located in an open field at a height of 10 m.  To adjust for different terrain at the building site and differences in the height of building surfaces, the local wind speed is calculated for each surface.

The wind speed is modified from the measured meteorological wind speed by the equation (ASHRAE 2001):

![](media/image6268.png)\


where z is the height of the centroid of the UTSC, z~met~ is the height of the standard metereological wind speed measurement, and a and  are terrain-dependent coefficients.   is the boundary layer thickness for the given terrain type.  The values of a and  are shown in the following tables:

Table: Terrain-Dependent Coefficients (ASHRAE 2001).

**Terrain**|**Description**|**Exponent, a**|**Layer Thickness,  (m)**
------------------------|----------------------------|----------------------------|---------------------------------------
1|Flat, open country|0.14|270
2|Rough, wooded country|0.22|370
3|Towns and cities|0.33|460
4|Ocean|0.10|210
*5*|*Urban, industrial, forest*|*0.22*|*370*

The UTSC can be defined such that it has multiple underlying heat transfer surfaces.  The centroid heights for each surface are area-weighted to determine the average height for use in the local wind calculation.

### Convection Coefficients

UTSC modeling requires calculating up to three different coefficients for surface convection heat transfer.  These coefficients are defined in the classic way by:

![](media/image6269.png)\


First, ![](media/image6237.png)  is the convection coefficient for the collector surface facing the outdoors when the UTSC is passive.  It is modeled in exactly the same way as elsewhere in EnergyPlus and will depend on the user setting for Outside Convection Algorithm – Outside Surface Heat Balance entry elsewhere in this document.

Second, ![](media/image6238.png)  is the convection coefficient for surfaces facing the plenum.  This coefficient is applied to just the underlying surface's convection when the UTSC is active and to both the collector and the underlying surface when the UTSC is passive.  When the UTSC is active, we use the convection correlation for forced air developed by McAdams (1954) as published by ASHRAE HoF (2001):

![](media/image6270.png)\


where,

![](media/image6271.png)  is the mean velocity in the plenum determined from ![](media/image6272.png)  where ![](media/image6273.png)  is the effective cross section area of the plenum perpendicular to the primary flow direction.  When the UTSC is passive, we model the convection in the same way used in EnergyPlus to model air gaps in windows.  These correlations vary by Rayleigh number and surface tilt and are based on the work of various research including Hollands et. al., Elsherbiny et. al., Wright, and Arnold.  The formulations are documented in ISO (2003) standard 15099.  For the UTSC implementation, the routines were adapted from Subroutine NusseltNumber in WindowManager.f90 (by F. Winkelmann), which itself was derived from Window5 subroutine "nusselt".

Third, ![](media/image6217.png)  is the convection coefficient used to degrade the UTSC performance under environmental conditions with high wind or rain.  If the weather file indicates it is raining, then we set ![](media/image6217.png)  = 1000.0 which has the effect of making the collector the same temperature as ambient air.  The heat exchanger effectiveness correlations described above account for a moderate amount of wind, but the correlations appear confined to the range 0 to 5.0 m/s.  Therefore we set ![](media/image6217.png)  equal to zero if ![](media/image6274.png)  is <= 5.0 m/s.  If ![](media/image6275.png)  is > 5.0 m/s then we use the McAdams correlation  but with a reduced velocity magnitude:

![](media/image6277.png)\


### Radiation Coefficients

UTSC modeling requires calculating up to four different linearized coefficients for radiation heat transfer.  Whereas radiation calculations usually use temperature raised to the fourth power, this greatly complicates solving heat balance equations for a single temperature.  Linearized radiation coefficients have the same units and are used in the same manner as surface convection coefficients and introduce very little error for the temperature levels involved.

The radiation coefficient, ![](media/image6232.png) , is used to model thermal radiation between the collector surface and the outside face of the underlying heat transfer surface.  We assume a view factor of unity.  It is calculated using:

![](media/image6278.png)\


where,

all temperatures are converted to Kelvin,

![](media/image6279.png)  is the Stefan-Boltzmann constant,

![](media/image6280.png)  is the longwave thermal emmittance of the collector, and

![](media/image6281.png)  is the longwave thermal emmittance of the underlying heat transfer surface.

The three other coefficients, ![](media/image6227.png) , ![](media/image6229.png) , and ![](media/image6231.png)  are used elsewhere in EnergyPlus for the outside face surface heat balance and are calculated in the same manner as equation  for UTSC collectors.  [This is accomplished by calling subroutine InitExteriorConvectionCoeffs in the file HeatBalanceConvectionCoeffs.f90. ]

### Bypass Control

The UTSC is assumed to be arranged so that a bypass damper controls whether or not air is drawn directly from the outdoors or through the UTSC.  The control decision is based on whether or not it will be beneficial to heat the outdoor air.  There are multiple levels of control including an availability schedule, whether or not the outdoor air is cooler than the mixed air setpoint, or whether or not the zone air temperature is lower than a so-called free heating setpoint.

### Sizing Warnings

Although the design of the transpired collector is left to the user, the program issues warnings when the suction airflow velocity falls outside the range 0.003 to 0.08 m/s.

### Overall Efficiency

The overall thermal efficiency of the UTSC is a useful output report and is defined as the ratio of the useful heat gain of the entire system versus the total incident solar radiation on the gross surface area of the collector.

![](media/image6282.png)\


where

![](media/image6283.png)  is useful heat gain

![](media/image6284.png)  is total incident solar radiation

Note that the efficiency ![](media/image6285.png)  is only defined for ![](media/image6286.png) .  This efficiency includes heat recovered from the underlying wall and can exceed 1.0.

### Collector Efficiency

The thermal efficiency of the collector is a useful output report and is defined as the ratio of the useful heat gain of the collector fluid versus the total incident solar radiation on the gross surface area of the collector.

![](media/image6287.png)\


Note that the efficiency ![](media/image6288.png)  is only defined for ![](media/image6289.png)

### References

Kutscher, C.F. 1994. Heat exchange effectiveness and pressure drop for air flow through perforated plates with and without crosswind. *Journal of Heat Transfer*. May 1994, Vol. 116, p. 391. American Society of Mechanical Engineers.

Van Decker, G.W.E., K.G.T. Hollands, and A.P. Brunger. 2001. Heat-exchange relations for unglazed transpired solar collectors with circular holes on a square of triangular pitch. *Solar Energy*. Vol. 71, No. 1. pp 33-45, 2001.

ISO. 2003. ISO 15099:2003. Thermal performance of windows, doors, and shading devices – Detailed calculations. International Organization for Standardization.
