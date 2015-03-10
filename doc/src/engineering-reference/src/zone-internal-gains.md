# Zone Internal Gains

## Sources and Types of Gains

Internal heat gains from lights, people, and equipment of various types are often significant elements in the zone thermal balance.  EnergyPlus allows the user to specify heat gains for several equipment types including people, lights, gas/electric equipment, and several other types.  The total heat gain is comprised of convective, radiant and latent gains in various proportions from these sources.  Convective gains are instantaneous additions of heat to the zone air.  Radiant gains are distributed on the surfaces of the zone, where they are first absorbed and then released back into the room (with some fraction conducted through the surface) according to the surface heat balances. {See Surface Heat Balance Manager / Processes in this document}.  Latent gains must be handled by ventilation or air conditioning equipment.  Recommended heat gains are given by ASHRAE [1].  These recommendations include the sensible (convective plus radiative) and latent proportions.  Sensible gains from equipment are primarily radiant.  The user can specify the heat gains and proportions for any type of equipment.  Determining the gains from lights, people and baseboard heat are slightly more complicated.

## Heat Gain from Lights

The input object Lights provides a model for internal gains from lights.  Radiant gains from lights must be handled differently from other radiant gains for reasons described here (long wavelength description).  The total radiant gains from lights must be divided into visible and thermal portions.  For example, the total electric input to typical incandescent lights is converted to 10% visible radiation, 80% thermal radiation, and 10% convective gain.  In contrast, the electric input to typical fluorescent lights is converted to 20% visible radiation, 20% thermal radiation, and 60% convective gain [2].  These percentage splits are under user control with the Lights input object.

## Heat Gain from People

The input object People provides a model for internal gains from occupants.  Heat is generated in the human body by oxidation at a rate called the metabolic rate (see Thermal Comfort discussion for more details).  This heat is dissipated from the body surface and respiratory tract by a combination of radiation, convection, and evaporation.  The relative proportions of sensible (radiation plus convection) and latent (evaporation) heat from people is a complex function of the metabolic rate and the environmental conditions.  EnergyPlus uses a polynomial function to divide the total metabolic heat gain into sensible and latent portions.  That function is based on a fit to data [3] at average adjusted metabolic rates of 350, 400, 450, 500, 750, 850, 1000 and 1450 Btu/h each at temperatures of 70, 75, 78, 80, 82 degrees Fahrenheit.  Sensible gains of 0 at 96 F and sensible gains equal to the metabolic rate at 30 F were assumed in order to give reasonable values beyond the reported temperature range.

> Average adjusted metabolic rate [3] is the metabolic rate to be applied to a mixed group of people with a typical percent composition based on the following factors:

> Metabolic rate, adult female=Metabolic rate, adult male X 0.85

> Metabolic rate, children = Metabolic rate, adult male X 0.75

The original data was in I-P (Inch-Pound) units, but the following correlation is in SI (Systems-International) units.

![](media/image5429.png)\


where

M=Metabolic Rate (W)

T=Air Temperature (C)

S=Sensible Gain (W)

Latent Gain is simply the total gain (metabolic rate) – sensible gain:

![](media/image5430.png)\


![Sensible Heat Gain from People Correlation](media/sensible-heat-gain-from-people-correlation.png)


The function for sensible gain calculation is compared to the original data points in the following figure.  The radiant fraction of the sensible gain is a user input on the People object.

## Heat Gain from Baseboard Heat

The input object ZoneBaseboard:OutdoorTemperatureControlled provides a model for an outdoor temperature controlled baseboard heater that adds energy to the zone according a control profile as shown in the following figure.  At TA = T2, the baseboard heat gain is Q2.  For TA > T2, there is no heat gain.  For TA < T1, a maximum amount of energy, Q1, is added to the zone.  There is proportional control between those two temperatures:

![Control of Outdoor Temperature Controlled Baseboard Heat](media/control-of-outdoor-temperature-controlled.png)


![](media/image5433.png)\


These temperature and capacity fields can be autosized based upon envelope, infiltration, and ventilation loads. To autosize these fields, users may set a design zone heating temperature that is assumed to be 20°C if blank.

The capacity at low temperature is the maximum capacity of the unit. It includes external envelope conduction load, infiltration load, and ventilation load in a space where the unit serves. The model first finds the lowest outdoor air temperature throughout design days included in the simulation, and determines the conduction load through external envelop as:

![](media/image5434.png)\


where

*q~Cond~~~*is conduction load through external envelope, W

*U* is heat transfer coefficient of external wall, W/m^2^K

*A* is area of external wall, m^2^

*T~Htg~* is baseboard zone heating setpoint temperature, °C

*T~L~* is low temperature, °C

The capacity at low temperature that is the maximum capacity of the unit is thus expressed as:

![](media/image5435.png)\


where

*CapT*L is capacity at low temperature, W

*q~I~* is design infiltration sensible load, W

*q~V~* = design ventilation sensible load, W

The capacity at high temperature is then prorated against the reference low and high temperatures as:

![](media/image5436.png)\


where

*CapT*H is capacity at high temperature, W

*T~H~* is high temperature, °C

## Distribution of Radiant Gains

It is useful to consider the distribution of short wavelength (including visible) radiant energy separate from long wavelength (thermal) radiant energy because many materials have different optical properties at different wavelengths.  An extreme example is glass that is opaque to the long wavelengths and transparent to the short.  Properties of materials vary across the entire spectrum of wavelengths.  In EnergyPlus, all radiant interactions are represented in terms of only two wavelengths: "short" and "long".  Short wavelength refers to the distribution given by a ~6000K black body source such as the sun.  Long wavelengths refer to radiation from ~300K sources such as walls or people.  There is negligible overlap between these two distributions.  Some sources, such as lights, must be considered as emitting both long and short wavelength radiation in proportions that approximate their actual effects on room surfaces.

Long wavelength radiation from all internal sources, such as people, lights and equipment, is combined and then distributed over surfaces. (see Internal Long-Wave Radiation Exchange).

Some fraction of the beam solar radiation transmitted into the zone is directly absorbed by the interior surfaces according to the solar distribution algorithm (see Solar Distribution) selected by the user.  The beam radiation not directly absorbed, plus the diffuse sky and ground-reflected radiation, plus the short wavelength radiation from lights are combined and distributed over the surfaces of the zone according to:

![](media/image5437.png)\


If all surfaces in the room are opaque, the radiation is distributed in proportion to the area\*absorptance product of each surface.  For surfaces which are transparent,

![](media/image5438.png)\


That fraction of radiation represented by ![](media/image5439.png)  is lost from the zone.

The transmittance and absorptance of transparent surfaces (windows or glass doors) are calculated as in section Window Calculation Module based on the optical properties of the window material layers.  The total absorptance of the window is computed for the interior shading device, the inside surface, and the outside surface for diffuse solar radiation incident from outside the zone.  Those absorptances are used for short wavelength radiation incident from inside the zone.  In most cases, this should not cause significant error.  When movable insulation covers the window, the radiation that would have been transmitted is absorbed at the outer surface of the window (thermally equal to the inside surface of the insulation).

## References

ASHRAE. 2001. Handbook of Fundamentals, pp 29.8-29.13, Atlanta: ASHRAE.

Carrier Air Conditioning Company. 1965a. Handbook of Air Conditioning System Design, pp 1-99 to 1-100. New York: McGraw Hill.

Carrier Air Conditioning Company. 1965b. Handbook of Air Conditioning System Design, pp 1-100, Table 48. New York: McGraw Hill.