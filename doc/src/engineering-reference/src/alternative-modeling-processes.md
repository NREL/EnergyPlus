# Alternative Modeling Processes

### RoomAir Models

The group of models described in this section is used to account for non-uniform room air temperatures that may occur within the interior air volume of a zone.  These models are accessed using the RoomAirModelType input object. RoomAir modeling was added to EnergyPlus starting with Version 1.2.  Although there are many types of analyses (comfort, indoor air quality, etc) that might benefit from localized modeling of how room air varies across space, only the *temperature* distribution of room air within the zone is currently addressed in EnergyPlus.  This allows surface heat transfer and air system heat balance calculations to be made taking into account natural thermal stratification of air and different types of intentional air distribution designs such as under-floor and side-wall displacement ventilation that purport to extract room air at higher-than-mean temperatures.  Note that EnergyPlus does **not** have completely general methods of modeling room air that are applicable to every conceivable type of airflow that might occur in a zone.  Such models (e.g. RANS-CFD) are too computationally expensive to use with EnergyPlus for the foreseeable future. The models that are available in EnergyPlus offer only limited modeling capabilities for select room airflow configurations.  Also note that because the complete mixing model for room air has long been the standard in building energy simulation, there is not currently a consensus on how to best model non-uniform air temperatures in buildings.  Therefore, it is up to the user to have a good understanding of when, where, and how to apply the room air models available in EnergyPlus.  The rest of this section provides some guidance in the way of examples and further discussion of the models available in EnergyPlus.

EnergyPlus offers the different types of air models listed in the table below along with the input objects associated with the use of that model.

Table: Summary of room air models available in EnergyPlus

**Air model name**|**Applicability**|**Input Objects Required**
-------------------------------|------------------------------|---------------------------------------
Well-Mixed |All zones|None, default
User Defined|Any zone where the user has prior knowledge of the temperature pattern|‘RoomAirModelType', ‘RoomAir:TemperaturePattern:UserDefined', ‘RoomAir:TemperaturePattern: xx'
One-Node Displacement Ventilation (Mundt)|displacement ventilation in typical office-type zones|‘RoomAirModelType', ‘RoomAirSettings:OneNodeDisplacementVentilation', ‘RoomAir:Node''
Three-Node Displacement Ventilation (UCSD)|displacement ventilation |‘RoomAirModelType', ‘RoomAirSettings:ThreeNodeDisplacementVentilation'
Under-Floor Air Distribution Interior Model (UCSD)|Interior zones served by a UFAD system|‘RoomAirModelType', ‘RoomAirSettings:UnderFloorAirDistributionInterior'
Under-Floor Air Distribution Exterior Model (UCSD)|Exterior zones served by a UFAD system|‘RoomAirModelType', ‘RoomAirSettings:UnderFloorAirDistributionExterior'
UCSD Cross Ventilation|cross ventilation|‘RoomAirModelType', ‘RoomAirSettings:CrossVentilation'

The room air models are coupled to the heat balance routines using the framework described by Griffith and Chen (2004).  Their framework was modified to include features needed for a comprehensive program for annual energy modeling rather than one for hourly load calculations.  The formulation is largely shifted from being based on the setpoint temperature to one based on the current mean air temperature.  This is necessary to allow for floating temperatures and dual setpoint control where there may be times that the mean zone temperatures are inside the dead band.  The coupling framework was also extended to allow for exhaust air flows (e.g. bathroom exhaust fans) in addition to air system return flows.

The inside face temperature calculation is modified by rewriting the zone air temperature, *T~a~*, with an additional subscript, *i*, for the surface index (![](media/image2218.png) or ![](media/image2219.png) ).  The inside face heat balance is solved for its surface temperature using,

![](media/image2220.png)\


where, *T~s~* is the inside face temperature

*i*subscript indicates individual surfaces

*j*subscript indicates current time step

*k*subscript indicates time history steps

*T~so~* is the outside face temperature

*Y~i~ are the cross CTF coefficients*

*Z~i~are the inside CTF coefficients*

*Φ~i~* are the flux CTF coefficients

![](media/image2221.png) is the conduction heat flux through the surface

![](media/image2222.png) is the surface convection heat transfer coefficient

*T~a~*is the near-surface air temperature

![](media/image2223.png) is the longwave radiation heat flux from equipment in zone

![](media/image2224.png) is the net long wavelength radiation flux exchange between zone surfaces

![](media/image2225.png) is the net short wavelength radiation flux to surface from lights

![](media/image2226.png) is the absorbed direct and diffuse solar (short wavelength) radiation

#### References:

Griffith, B. and Q. Chen. 2004. Framework for coupling room air models to heat balance load and energy calculations (RP-1222). International Journal of Heating, Ventilating, Air-conditioning and Refrigerating Research. ASHRAE, Atlanta GA.  Vol 10. No 2. April 2004.

### User Defined RoomAir Temperatures

The input object RoomAir:TemperaturePattern:UserDefined provides a capabity for users to define the sort of air temperature pattern he or she expects in the zone.  With these models, the pattern is generally set beforehand and does not respond to conditions that evolve during the simulation.  (Exception: the pattern available through the RoomAir:TemperaturePattern:TwoGradient object will switch between two different pre-defined vertical gradients depending on the current value of certain temperatures or thermal loads. )

The user-defined patterns obtain the mean air temperature, ![](media/image2227.png) , from the heat balance domain and then produce modified values for:

![](media/image2228.png) the adjacent air temperature which is then used in the calculation of inside face surface temperature during the heat balance calculations,

![](media/image2229.png) the temperature of air leaving the zone and entering the air system returns

![](media/image2230.png) the temperature of air leaving the zone and entering the exhaust.

![](media/image2231.png) the temperature of air "sensed" at the thermostat (not currently used in air system control because air system flows use load-based control).

The user defined room air models used indirect coupling so that the patterns provide values for, or ways to calculate, how specific temperatures differ from ![](media/image2232.png) .  The various ![](media/image2233.png)  values determined from the model are applied to ![](media/image2234.png)  as follows:

![](media/image2235.png)\


(where "*i's"* represents each surface in the zone that is affected by the model)

![](media/image2236.png)\


![](media/image2237.png)\


![](media/image2238.png)\


The patterns defined by the object ‘RoomAir:TemperaturePattern:SurfaceMapping' are fairly straightforward.  The user directly inputs values for ![](media/image2239.png)  for each surface.  The pattern "maps" specific surfaces, identified by name, to ![](media/image2240.png)  values.  This provides completely general control (but in practice may be cumbersome to use).  The other patterns focus on temperature changes in the vertical direction.  Surfaces do not need to be identified, but all the surfaces with the same height will be assigned the same ![](media/image2241.png)  values.

The patterns defined by the object ‘RoomAir:TemperaturePattern:NondimensonalHeight' apply a temperature profile based on a non-dimensionalized height, ![](media/image2242.png) .  The height of each surface is defined to be the z-coordinate of the surface's centroid relative to the average z-coordinate of the floor surfaces.  The zone ceiling height is used as the length scale to non-dimensionalize each surface's height so that,

![](media/image2243.png)\


(where "*i's"* represents each surface in the zone that is affected by the model)

The values for ![](media/image2244.png)  are constrained to be between 0.01 and 0.99 because the value is meant to describe the air layer near the surface (say approximate 0.1 m from the surface) rather than the surface itself.

The user-defined profile is treated as a look up table or piecewise linear model.  The values for ![](media/image2245.png)  are determined by searching the ![](media/image2246.png)  values in the user-defined profile and performing linear interpolation on the associated ![](media/image2247.png)  values.

The patterns defined by the object ‘RoomAir:TemperaturePattern:ConstantGradient' apply a constant temperature gradient in the vertical direction.  The model assumes that ![](media/image2248.png)  occurs at the mid-plane so that ![](media/image2249.png)  (by definition).  The surface ![](media/image2250.png)  values are compared to ![](media/image2251.png) and then scaled with zone ceiling height to obtain values for the change in height (in units of meters), ![](media/image2252.png) .  The user defined gradient, ![](media/image2253.png) , (units of ºC/m) is then used to determine ![](media/image2254.png)  values using

![](media/image2255.png)\


The patterns defined by the object ‘RoomAir:TemperaturePattern:TwoGradient' are very similar to the constant gradient pattern above but the value of ![](media/image2256.png)  used at any given time is selected by interpolating between two user-defined values for ![](media/image2257.png) .  Five options are available, three based on temperatures and two based on thermal loads – see the Input Output Reference.  The user provides upper and lower bounding values.  If the current value of the "sensing" variable lies between the upper and lower bounds, then ![](media/image2258.png)  is determined using linear interpolation.  If the designated value is above the upper bound then the upper value for ![](media/image2259.png)  is used (no extrapolation).  Similarly, if the designated value is below the lower bound, then the lower value for ![](media/image2260.png)  is used.  Note that "upper" and "lower" indicate the temperature and heat rate bounds and that the values for ![](media/image2261.png)  do not have to follow in the same way; the ![](media/image2262.png)  value for the lower bound could be higher than the ![](media/image2263.png)  value for the upper bound (providing a something of a reverse control scheme).  Rather than directly using ![](media/image2264.png)  values from the user, the temperatures for the return air, exhaust and thermostat are determined based on user-entered heights (in units of meters from the floor) and applying the current value for ![](media/image2265.png) .

### One-Node Displacement Ventilation RoomAir Model

The input object RoomAirSettings:OneNodeDisplacementVentilation provides a simple model for displacement ventilation.  Mundt (1996) points out that a floor air heat balance provides a simple and reasonably accurate method of modeling the temperature near the floor surface. The slope of a linear temperature gradient can then be obtained by adding a second upper air temperature value that comes from the usual overall air system cooling load heat balance.  The figure below diagrams the temperature distribution versus height being calculated by the model.  Mundt's floor air heat balance is extended to include convection heat gain from equipment and by ventilation or infiltration that may be introduced near the floor in order to maintain all the terms in the air heat balance of the Heat Balance Model.  This yields the following heat balance for a floor air node,

![](media/image2266.png)\


where

*ρ* is the air density

*c~p~*is the air specific heat at constant pressure

![](media/image2267.png) is the air system flow rate

*T~supply~* is the air system's supply air drybulb temperature

*h~cFloor~* is the convection heat transfer coefficient for the floor

*A~floor~* is the surface area of the floor

*T~floor~*~~is the surface temperature of the floor

*Q~convSourceFloor~*  is the convection from internal sources near the floor (< 0.2 m)

*Q~InfilFloor~*  is the heat gain (or loss) from infiltration or ventilation near the floor

"Floor splits" are the fraction of total convective or infiltration loads that are dispersed so as to add heat to the air located near the floor.  The user prescribes values for floor splits as input.  No guidance is known to be available to use in recommending floor splits, but the user could for example account for equipment known to be near the floor, such as tower computer cases, or supplementary ventilation designed to enter along the floor.  The equation above can be solved directly for *T~AirFloor~* and is used in the form of the equation below,

![](media/image2268.png)\


The upper air node temperature is obtained by solving the overall air heat balance for the entire thermal zone for the temperature of the air leaving the zone and going into the air system return, *T~leaving~*.

![](media/image2269.png)\


where ![](media/image2270.png)  is the air system heat load with negative values indicating a positive cooling load.  Values for ![](media/image2270.png)  are computed by the load calculation routines and passed to the air model.  The vertical temperature gradient or slope, *dT/dz*, is obtained from,

![](media/image2271.png)\


where *H~return~* is the distance between the air system return and the floor air node assumed to be 0.1 m from the floor and *z* is the vertical distance.

![Height versus temperature schematic for Mundt model](media/height-versus-temperature-schematic-for-mundt.png)


The constant slope allows obtaining temperatures at any vertical location using,

![](media/image2273.png)\


So for example the temperatures near the ceiling can easily be determined. Accounting for the location of the thermostat inside the zone (e.g. 1.1 m) is accomplished by returning the temperature for the appropriate height to the appropriate air node used for control. If the walls are subdivided in the vertical direction as shown in the figure above, then the air model can provide individual values for each surface based on the height and slope.  However, no additional heat balances are necessarily made (in the air domain) at these points as all the surface convection is passed to the model in the totaled value for ![](media/image2274.png) .

#### References

Mundt, E. 1996. The performance of displacement ventilation systems-experimental and theoretical studies, Ph. D. Thesis, Royal Institute of Technology, Stockholm.

### Three-Node Displacement Ventilation RoomAir Model

#### Overview

The input object RoomAirSettings:ThreeNodeDisplacementVentilation provides a simple model for heat transfer and vertical temperature profile prediction in displacement ventilation. The fully-mixed room air approximation that is currently used in most whole building analysis tools is extended to a three node approach, with the purpose of obtaining a first order precision model for vertical temperature profiles in displacement ventilation systems. The use of three nodes allows for greatly improved prediction of thermal comfort and overall building energy performance in low energy cooling strategies that make use of unmixed stratified ventilation flows.

The UCSD Displacement Ventilation Model is one of the non-uniform zone models provided through the Room Air Manager in EnergyPlus. The intent is to provide a selection of useful non-uniform zone air models to enable the evaluation of air-conditioning techniques that use stratified or partially stratified room air. Such techniques include displacement ventilation (DV) and underfloor air distribution (UFAD) systems. The methodology can also include, in principle, natural displacement ventilation and also wind-driven cross-ventilation (CV).

#### Displacement Ventilation

A DV system is a complete contrast to a conventional forced air system. In a conventional system conditioned air is delivered at ceiling level and the intent is to create a fully mixed space with uniform conditions. In a DV system conditioned air is delivered at floor level and low velocity in order to minimize mixing and to establish a vertical temperature gradient. The incoming air "displaces" the air above it which, in turn,  is exhausted through ceiling level vents. In DV a noticeable interface occurs between the occupied zone of the room and a mixed hot layer near the ceiling of the room (Dominique & Guitton, 1997). Maintaining the lower boundary of this warm layer above the occupied zone is one of the many unique challenges of displacement ventilation design. Often DV systems use 100% outside air. The vertical displacement air movement means that convective heat gains introduced near the ceiling will be removed without affecting the occupied region of the room. Also a fraction of the heat gains that occur in the occupied zones rise as plumes into the upper part of the space, thereby reducing the cooling load. Similarly the fresh air will be used more effectively than with a fully mixed system: the fresh air won't be "wasted" in the upper, unoccupied region of the room. Finally, the vertical temperature gradient means that the average room temperature can be higher for a DV conditioned room than with a conventionally conditioned room: the occupants feel the lower temperature in the lower region of the room and are unaffected by the higher temperature near the ceiling. However, whenever the outside air temperature is above ≈19C this advantage is mostly lost: the internal loads must be removed from the space independently of the airflow pattern (during the warmer hours buildings tend to be almost closed to the outside, operating in closed loop). The inflow temperature advantage is then only useful for the minimum outside air that must always be provided (in most cases this remaining advantage is negligible).

DV systems have limitations. In order to avoid chilling the occupants the supply air temperature used for DV is considerably higher than that used in conventional forced-air systems. This can lead to problems in removing both sensible and latent loads. Exterior spaces may have conditions that are not conducive to establishing a vertical temperature gradient. DV systems seem to be best suited to interior spaces with only moderate loads.

#### Non-uniform zone models

Several types of models have been proposed as suitable for inclusion in building energy simulation (BES) programs. These models must be simple enough not to impose an undue computational burden on a BES program, yet provide enough predictive capability to produce useful comparisons between conventional and stratified zone operation strategies. ASHRAE RP-1222 (Chen & Griffith 2002) divides the candidate models into two categories: *nodal* and *zonal*. Nodal models describe the zone air as a network of nodes connected by flow paths; each node couples convectively to one or more surfaces. Zonal models are coarse–grained finite volume models. ASHRAE RP-1222 provides a short history (and examples) of each type of model. In terms of nodal models for displacement ventilation we mention the Mundt model (Mundt 1996), since it is implemented in EnergyPlus, and the Rees-Haves model (Rees & Haves 2001) since it is a well developed nodal-type model and is implemented in the RP-1222 toolkit. The Rees-Haves model, while successful in predicting the flow and temperature field for geometries similar to those used in its development, can suffer from lack of flexibility and clarity in the modeling approximations. When dealing with diverse geometries it is not clear that the flow coefficients used in the model are applicable or why they can be used since plumes, the fundamental driving mechanisms of the displacement flow, are not explicitly modeled. This is the main difference between the DV models implemented in the RP-1222 toolkit and the model that is described here.

The UCSD DV model is closer to a nodal model than to a zonal model. However, it is best to classify it in a separate category: plume equation based multi-layer models (Linden *et al*. 1990, Morton *et al*. 1956). These models assume that the dominant mechanism is plume-driven flow from discrete internal sources and that other effects (such as buoyancy driven flow at walls or windows) may be neglected. Alternatively, these heat sources also produce plumes that can be included in the model. The result is a zone divided vertically into two or more well separated regions – each region characterized by a single temperature or temperature profile. This characterization allows the physics of the heat gains and the ventilation flow to be represented in a realistic manner, without the introduction of *ad hoc* assumptions.

#### Model Description

#### Single Plume Two Layer Model

The simplest form of the plume equation based models is the case of a single plume in an adiabatic box with constant supply air flow. For this configuration two layers form in the room: a lower layer with similar density and temperature as the inflow air and a mixed upper layer with the same density / temperature as the outflow air. The main assumption of this model, successfully validated against scaled model experiments (Linden *et al.* 1990), is that the interface between the two layers occurs at the height (h) where the vertical buoyancy driven plume flow rate is the same as the inflow rate. For a point source of buoyancy in a non-stratified environment (a plume) the airflow rate increases with vertical distance from the source according to:

![](media/image2275.png)\


where

![](media/image2276.png) = plume volume flux [m3/s]

![](media/image2277.png) = buoyancy flux  [m4/s3]

![](media/image2278.png) = vertical distance above source [m]

![](media/image2279.png)\


![](media/image2280.png) = plume entrainment constant; a value of  0.127 is used, suitable for top-hat profiles for density and velocity across the plumes.

For an ideal gas

![](media/image2281.png)\


resulting in the following relation between heat input rate and buoyancy flux:

![](media/image2282.png)\


where

![](media/image2283.png) = density of air [kg/m3]

![](media/image2284.png) = air temperature [K]

![](media/image2285.png) = acceleration of gravity [m/s2]

![](media/image2286.png) = heat input rate [W]

![](media/image2287.png) =specific heat capacity of air [J/kgK]

Since the plume volume flow rate increases with height with exponent 5/3, for any room inflow rate (F, (m^3^/s)) there will always be a height (h,(m)) where the plume driven flow rate matches the inflow rate. This height is obtained by setting (1.1) equal to F and solving for z=h:

![](media/image2288.png)\


Substituting  in  and introducing air properties at 20 C gives:

![](media/image2289.png)\


#### Multiple Plumes and Wall Heat Transfer

Of course, it would be rare for a real world case to consist of a single point-source plume originating on the floor, unaffected by heat gains from walls and windows. For multiple plumes of equal strength a straight-forward extension of the single is possible. N plumes of unequal strength result in the formation of n vertical layers. This case is much more complex but if we are satisfied with a first order precision model the equal strength model can be used by averaging the plume strengths (Carrilho da Graça, 2003). Even in a case where all plumes are of equal strength, nearby plumes may coalesce. Plumes that are less than 0.5 meters apart at their source will coalesce within 2 meters (Kaye & Linden,2004).

As the complexity of the physical systems modeled increases some limitations must be imposed. In particular, the biggest challenge remains the interaction between wall driven boundary layers (positively and negatively buoyant) and displacement flows. For this reason, the model that is developed below is not applicable when:

Downward moving buoyancy driven airflow rate is of the same order of magnitude as plume driven flow (these airflow currents are typically generated on lateral surfaces or in the ceiling whenever these surfaces are much cooler than the room air).

Upward moving wall or floor generated buoyancy flux in the lower layer is of the same order of magnitude as plume driven flow.

Although these limitations are significant it is important to note that even in the presence of dominant convection from the floor surface, a buoyancy, two layer flow can be established whenever the plume buoyancy flux is more than 1/7 of the horizontal flux (Hunt *et al.* 2002). A two layer structure can also originate when the only heat source is a heated portion of the room floor, as long as the heated area does not exceed 15% of the room floor (Holford *et al.* 2002).

For the case of multiple non-coalescing plumes (n), with equal strength, the total vertical airflow for a given height is:

![](media/image2290.png)\


resulting in a mixed layer height of:

![](media/image2291.png)\


#### Implementation

The model predicts three temperatures that characterize the three main levels in the stratification of the room:

a floor level temperature T~floor~ to account for the heat transfer from the floor into the supply air

an occupied subzone temperature T~oc~ representing the temperature of the occupied region;

an upper level temperature T~mx~representing the temperature of the upper, mixed region and the outflow temperature.

We assume that the model for multiple, equal strength plumes (equations  and  will be adequate for our calculations. The supply air flow rate ![](media/image2292.png)  is obtained by summing all the air flows entering the zone: supply air, infiltration, ventilation, and inter-zone flow. The heat gain ![](media/image2293.png)  is estimated by summing all the convective internal gains located in the occupied subzone – task lights, people, equipment – and dividing this power equally among the n plumes. With these assumptions we can describe the implementation.

The UCSD DV model is controlled by the subroutine *ManageUCSDDVModel* which is called from the *RoomAirModelManager*. The *RoomAirModelManager* selects which zone model will be used for each zone.

The calculation is done in subroutine *CalcUCSDDV*. First we calculate the convective heat gain going into the upper and lower regions.

![](media/image2294.png)\


![](media/image2295.png)\


![](media/image2296.png)\


Next we sum up the inlet air flows in the form of MCP (mass flow rate times the air specific heat capacity) and MCPT (mass flow rate times C~p~ times air temperature).

![](media/image2297.png)\


![](media/image2298.png)\


![](media/image2299.png)\


![](media/image2300.png)\


![](media/image2301.png)\


![](media/image2302.png)\


The number of plumes per occupant ![](media/image2303.png)  is a user input. The total number of plumes in the zone is:

![](media/image2304.png)\


The gains fraction ![](media/image2305.png) is a user input via a schedule. It is the fraction of the convective gains in the occupied subzone that remain in that subzone. Using this we calculate the total power in the plumes and the power per plume.

![](media/image2306.png)\


![](media/image2307.png)\


We now make an initial estimate of the height fraction *Fr~hb~* (height of the boundary layer divided by the total zone height).

![](media/image2308.png)\


where 0.000833  = ![](media/image2309.png) converts ![](media/image2310.png)  to a volumetric flow rate. Next we iterate over the following 3 steps.

#### Iterative procedure

Call subroutine *HcUCSDDV* to calculate a convective heat transfer coefficient for each surface in the zone, an effective air temperature for each surface, and HA~mx~, HAT~mx~, HA~oc~, HAT~oc~, HA~fl~, and HAT~fl~. Here HA is ![](media/image2311.png)  for a region and HAT is ![](media/image2312.png)  for a region. The sum is over all the surfaces bounding the region; ![](media/image2313.png)  is the convective heat transfer coefficient for surface i, ![](media/image2314.png)  is the area of surface i,  and ![](media/image2315.png)  is the surface temperature of surface i.

Recalculate ![](media/image2316.png)  using the equation .

Calculate the three subzone temperatures: *T~floor,~T~oc~* and *T~mx~*.

The h~c~'s calculated in step 1 depend on the subzone temperatures and the boundary layer height. In turn the subzone temperatures depend on the HA and HAT's calculated in step 1. Hence the need for iteration

Next we describe each steps 1 and 3 in more detail.

#### Step 1

Subroutine *HcUCSDDV* is quite straightforward. It loops through all the surfaces in each zone and decides whether the surface is located in the upper, mixed subzone or the lower, occupied subzone, or if the surface is in both subzones. If entirely in one subzone the subzone temperature is stored in the surface effective temperature variable *TempEffBulkAir(SurfNum)* and h~c~ for the surface is calculated by a call to subroutine *CalcDetailedHcInForDVModel*. This routine uses the "detailed" natural convection coefficient calculation that depends on surface tilt and ![](media/image2317.png) . This calculation is appropriate for situations with low air velocity.

For surfaces that bound 2 subzones, the subroutine calculates h~c~for each subzone and then averages them, weighting by the amount of surface in each subzone.

During the surface loop, once the h~c~ for a surface is calculated, the appropriate subzone HA and HAT sums are incremented. If a surface is in 2 subzones the HA and HAT for each subzone are incremented based on the area of the surface in each subzone.

#### Step 3

The calculation of  subzone temperatures follows the method used in the **ZoneTempPredictorCorrector** module and described in the section **Basis for the System and Zone Integration**. Namely a third order finite difference expansion of the temperature time derivative is used in updating the subzone temperatures. Otherwise the subzone temperatures are obtained straightforwardly by solving an energy balance equation for each subzone.

![](media/image2318.png)\



![](media/image2319.png) ![](media/image2320.png)

Here ![](media/image2321.png) , ![](media/image2322.png) , and ![](media/image2323.png)  are the heat capacities of the air volume in each subzone. ![](media/image2323.png)  is calculated by

![](media/image2324.png)\


![](media/image2325.png)\


The other subzone air heat capacities are calculated in the same manner.

#### Mixed calculation

The above iterative procedure assumed that displacement ventilation was taking place: i.e., conditions were favorable temperature stratification in the zone. Now that this calculation is complete and the subzone temperatures and depths calculated, we check to see if this assumption was justified. If not, zone conditions must be recalculated assuming a well-mixed zone.

If ![](media/image2326.png)  or ![](media/image2327.png)  or ![](media/image2328.png)  then the following mixed calculation will replace the displacement ventilation calculation.

> **Note:**  ![](media/image2329.png)  is the minimum thickness of occupied subzone. It is set to 0.2 meters. ![](media/image2330.png) is the height of the top of the floor subzone. It is defined to be 0.2 meters; that is, the floor subzone is always 0.2 meters thick and ![](media/image2331.png)  is the temperature at 0.1 meter above the floor surface.

The mixed calculation iteratively calculates surface convection coefficients and room temperature just like the displacement ventilation calculation described above. In the mixed case however, only one zone temperature *T~avg~* is calculated. The 3 subzone temperatures are then set equal to *T~avg~*.

First, *Fr~hb~*  is set equal to zero.

Then the code iterates over these steps.

Calculate *T~avg~* using

![](media/image2332.png)\


![](media/image2333.png)\


![](media/image2334.png)\


![](media/image2335.png)\


Call *HcUCSDDV* to calculate the *h~c~*'s.

Repeat step 1

#### Final calculations

The displacement ventilation calculation finishes by calculating some report variables. Using equation , setting the boundary height to 1.5 meters and solving for the flow, we calculate a minimum flow fraction:

![](media/image2336.png)\


![](media/image2337.png)\


We define heights:

![](media/image2338.png)\


![](media/image2339.png)\


![](media/image2340.png)\


![](media/image2341.png)\


Using the user defined comfort height we calculate the comfort  temperature.

If mixing:

![](media/image2342.png)\


If displacement ventilation:

If *H~comf~* < *H~flavg~*

![](media/image2343.png)\


Else if ![](media/image2344.png)  and ![](media/image2345.png)

![](media/image2346.png)\


Else if ![](media/image2347.png)  and ![](media/image2348.png)

![](media/image2349.png)\


Else if ![](media/image2350.png)  and ![](media/image2351.png)

![](media/image2352.png)\


Using the user defined thermostat height we calculate the temperature at the thermostat.

If mixing:

![](media/image2353.png)\


If displacement ventilation:

If *H~stat~* < *H~flavg~*

![](media/image2354.png)\


Else if ![](media/image2355.png)  and ![](media/image2356.png)

![](media/image2357.png)\


Else if ![](media/image2358.png)  and ![](media/image2359.png)

![](media/image2360.png)\


Else if ![](media/image2361.png)  and ![](media/image2362.png)

![](media/image2363.png)\


The average temperature gradient is:

If ![](media/image2364.png)

![](media/image2365.png)\


else ![](media/image2366.png)

The maximum temperature gradient is:

If  ![](media/image2367.png)

![](media/image2368.png)\


else ![](media/image2369.png)

If  ![](media/image2370.png)

![](media/image2371.png)\


else ![](media/image2372.png) and

![](media/image2373.png)\


For reporting purposes, if the zone is deemed to be mixed, the displacement ventilation report variables are set to flag values.

If ![](media/image2374.png)  or ![](media/image2375.png)  or ![](media/image2376.png)  or ![](media/image2377.png)

![](media/image2378.png)\


![](media/image2379.png)\


![](media/image2380.png)\


![](media/image2381.png)\


Finally, the zone node temperature is set to *T~mx~*.

#### References

Carrilho da Graca, G. 2003. Simplified models for heat transfer in rooms. Ph. D. Thesis, University of California, San Diego.

Chen, Q., and B. Griffith. 2002. Incorporating Nodal Room Air Models into Building Energy Calculation Procedures. ASHRAE RP-1222 Final Report.

Cooper, P. and P.F. Linden. 1996. Natural ventilation of an enclosure containing two buoyancy sources. Journal of Fluid Mechanics, Vol. 311, pp. 153-176.

Dominique, M. and P. Guitton. 1997.  Validation of displacement ventilation simplified models. Proc. of Building Simulation.

Holford, J.M., G.R. Hunt and P.F. Linden. 2002. Competition between heat sources in a ventilated space. Proceedings of RoomVent 2002, pp. 577-580.

Hunt, G.R., J.M. Holford and P.F. Linden. 2002. Characterization of the flow driven by a finite area heat source in a ventilated enclosure. Proceedings of RoomVent 2002, pp.  581-584.

Hunt, G.R. and P.F. Linden.  2001. Steady-state flows in an enclosure ventilated by buoyancy forces assisted by wind. . Journal of Fluid Mechanics, Vol. 426,  pp. 355-386.

Kaye, K.N. and P.F. Linden.  2004. Coalescing axisymmetric turbulent plumes*.* Journal of Fluid Mechanics, ** Vol. 502, pp. 41--63.

Linden, P.F., G.F. Lane-Serff and D.A. Smeed. 1990. Emptying filling boxes: the fluid mechanics of natural ventilation. Journal of Fluid Mechanics, Vol. 212, pp. 309-335.

Linden, P.F. and P. Cooper. 1996. Multiple sources of buoyancy in a naturally ventilated enclosure. Journal of Fluid Mechanics, Vol. 311, pp. 177-192.

Morton, B.R., G.I. Taylor andJ.S. Turner. 1956. Turbulent gravitational convection from maintained and instantaneous sources. Proceedings of the Royal Society of London, Vol A234, pp. 1-23.

Mundt, E. 1996. The Performance of Displacement Ventilation Systems – Experimental and Theoretical Studies, Ph. D. Thesis, Bulletin N38, Building Services Engineering KTH, Stockholm.

Rees, S.J., and P. Haves. 2001. A nodal model for displacement ventilation and chilled ceiling systems in office spaces. Building and Environment, Vol. 26, pp. 753-762.

### Under-Floor Air Distribution Interior Zone Model

#### Overview

The input object RoomAirSettings:UnderFloorAirDistributionInterior provides a simple model for heat transfer and nonuniform vertical temperature profile for interior zones of a UFAD system. These zones are expected to be dominated by internal loads, a portion of which (such as occupants and workstations) will act to generate plumes. The plumes act to potentially create room air stratification, depending on the type & number of diffusers, the amount and type of load, and the system flowrate. In order to better model this situation the fully-mixed room air approximation that is currently used in most whole building analysis tools is extended to a two node approach, with the purpose of obtaining a first order precision model for vertical temperature profiles for the interior zones of UFAD systems. The use of 2 nodes allows for greatly improved prediction of thermal comfort and overall building energy performance for the increasingly popular UFAD systems.

The UCSD UFAD Interior Zone Model is one of the non-uniform zone models provided through the Room Air Manager in EnergyPlus. The intent is to provide a selection of useful non-uniform zone air models to enable the evaluation of air-conditioning techniques that use stratified or partially stratified room air. Such techniques include displacement ventilation (DV) and underfloor air distribution (UFAD) systems. The methodology can also include natural displacement ventilation and also wind-driven cross-ventilation (CV).

#### Underfloor air distribution systems

UFAD systems represent, in terms of room air stratification, an intermediate condition between a well-mixed zone and displacement ventilation. Air is supplied through an underfloor plenum at low pressure through diffusers in the raised floor. The diffusers can be of various types: e.g., swirl, variable-area, displacement, and produce more or less mixing in the zone. UFAD systems are promoted as saving energy due to: higher supply air temperature; low static pressure; cooler conditions in the occupied subzone than in the upper subzone; and sweeping of some portion of the convective load (from ceiling lights, for instance) into the return air without interaction with the occupied region of the zone.

Modeling a UFAD system is quite complex and involves considerably more than just a non-uniform zone model. The zones' coupling to the supply and return plenums must be modeled accurately (particularly radiative transfer from a warm ceiling to a cool floor and on into the supply plenum by conduction). The supply plenum must be accurately modeled, giving a good estimate of the supply air temperature and conduction heat transfer between supply & return plenums through the slab. The HVAC system must be modeled including return air bypass and various types of fan powered terminal units.

The UCSD UFAD interior zone model is similar to the UCSD DV model. The most obvious difference is that the UFAD model has no separate near-floor subzone. Like the UCSD DV model it is a plume equation based multi-layer model (2 layers in this case). The zone is modeled as being divided into 2 well separated subzones which we denote as "occupied" and "upper". Each subzone is treated as having a single temperature. The boundary between the 2 subzones moves up & down each time step as a function of zone loads and supply air flow rate. Thus at each HVAC time step, the height of the boundary above the floor must be calculated, portions of surfaces assigned to each subzone, and a separate convective heat balance performed on each subzone.

#### Model Description

The UFAD interior zone model is based upon non-dimensional analysis of the system and using the non-dimensional description to make a comparison between full-scale UCB test chamber data & small-scale UCSD salt tank measurements.

In order to do the non-dimensional comparisons, we need to define two dimensionless parameters. One is ![](media/image2382.png) , and the other is ![](media/image2383.png) . Lin & Linden (Lin & Linden, 2005) showed that in a UFAD system, the buoyancy flux of the heat source ![](media/image2384.png) and the momentum flux of the cooling jets ![](media/image2385.png) are the controlling parameters on the stratification. Since ![](media/image2386.png) and![](media/image2387.png) , we can have a length scale as ![](media/image2388.png) .

*Definition of for the single-plume single-diffuser basic model*

We observed, in our small-scale experiments, that the total room height does not affect the interface position, or the height of the occupied zone. In other words, *H* might not be the critical length scale for the stratification. Therefore, we started to use ![](media/image2389.png)  as the reference length. Then ![](media/image2390.png) is defined as

![](media/image2391.png)\


*Definition for multi-diffuser and multi-source cases*

We only considered single-diffuser, single-source cases in above analysis. Suppose there are *n* equal diffusers and *m* equal heat sources in a UFAD room. We shall divide the number of diffusers up into a number of separate heat sources so that each subsection with *n'=n/m* diffusers per heat source will have the same stratification as other subsections. Further, the air flow and the heat load into the subsection *Q'* and *B'* will be![](media/image2392.png) ![](media/image2393.png) respectively, where *Q'* and *B'* are the total air flow and the total heat load for the entire UFAD space. Then the momentum flux each diffuser per heat source carries is![](media/image2394.png) .  will be modified as

![](media/image2395.png)\


*Full-scale cases*

Because *B* is the buoyancy flux of the heat sources and *M* is the momentum flux of the cooling jets, in a real full-scale room, we shall consider the total room net heat load (plume heat input, minus the room losses) and the total net flow rate coming from the diffusers (input room air flow, minus the room leakage). Further, if the diffuser is swirl type, the vertical momentum flux should be used.

![](media/image2396.png)\


where, *Q* is the net flow rate coming out from all diffusers (m^3^/s); *W* is the total net heat load (kW); *A* is the effective area of each diffuser (m^2^); *n'* is the number of diffusers per heat source; ** is the angle between the diffuser slots and the vertical direction and *m* is the number of heat sources

*Definition of *

In our theoretical model, two-layer stratification forms at steady state, provided that each diffuser carries the same momentum flux, and each heat source has the same heat load. We could define a dimensionless parameter **, which indicates the strength of stratification.

*Small-scale cases*

In our salt-water tank experiments, fluid density ** is measured. Define that

![](media/image2397.png)\


where, *~~* and *~l~* are the fluid density of the upper layer and lower layer, separately; and *~o~* is the source density at the diffusers.

Therefore, *~l~* =*~o~* gives **=*1*, which means the largest stratification (displacement ventilation case); *~l~* =*~u~* leads to **=*0*, in which case there is no stratification (mixed ventilation case).

*Full-scale cases*

Similarly, we can define ** for full-scale cases by using temperature.

![](media/image2398.png)\


where *T~r~*, *T~oz~*, and *T~s~* are the return air temperature, the occupied zone temperature and the supply temperature, respectively (K). Again *1* occurs in displacement ventilation; while ** happens in mixed ventilation.

*Comparisons between full-scale UCB data and small-scale UCSD data*

![Data comparisons in the non-dimensional (a) regular plot](media/data-comparisons-in-the-non-dimensional-a.png)


![(b) log-log plot.}](media/b-log-log-plot..png)


The figures (Figure 133. Data comparisons in the non-dimensional (a) regular plot and Figure 134. (b) log-log plot.} show the comparisons between UCB's data and the UCSD salt tank data in the ** plot. As seen in the figures, the full-scale and small-scale data are on the same trend curve. This provides the evidence that the salt-tank experiments have included most characteristics of a UFAD system. Note that big ** (>20) of UCB's experiments all have large DDR (from *1.19* to *2.18*). The largest DDR (*2.18*) even gives a negative![](media/image2401.png) , which is NOT shown in the figures.)

We could work out the occupied zone temperature by using the least-square fitting line suggested in figure 1(b). Hence the interface height is needed to determine a entire two-layer stratification. Figure 135 shows the dimensionless interface height![](media/image2402.png) of the UCSD small-scale experiments plotted against **. Note that for the experiments with elevated heat source, the interface heights have been modified by![](media/image2403.png)  where *h~s~* is the vertical position of the elevated heat source. All data then are located along a line in Figure 135. Since the salt-tank experiments are concluded to represent important characteristics of a full-scale UFAD room, this figure provides some guidelines for estimate the interface position in a real UFAD room.

![Non-dimensional interface height of small-scale experiments.](media/non-dimensional-interface-height-of-small.png)


*Formulas for EnergyPlus based on the dimensionless parameter *

If we have input including the supply temperature *T~s~* (K); the number of diffusers *n*; the number of heat sources *m*; the vertical position of the heat sources *h~s~*~~(m); the heat load *W* (kW); the effective area of a diffuser *A* (m^2^); and the total supply air flow rate *Q* (m^3^/s) then the output will be

![](media/image2405.png)\


![](media/image2406.png)\


![](media/image2407.png)\


where *T~r~* is the return temperature (K); *T~oz~* is the occupied subzone temperature (K); *h* is the interface height (m); and ** is defined above.

#### Implementation

The implementation closely follows the procedure described in the displacement ventilation zone model. The model predicts two temperatures that characterize the two main levels in the stratification of the room:

an occupied subzone temperature T~oc~ representing the temperature of the occupied region;

an upper level temperature T~mx~representing the temperature of the upper, mixed region and the outflow temperature.

We will use  to calculate the interface height and do a heat balance calculation on each subzone. ** is given by . The supply air flow rate ![](media/image2292.png)  is obtained by summing all the air flows entering the zone: supply air, infiltration, ventilation, and inter-zone flow. The heat gain ![](media/image2293.png)  is estimated by summing all the convective internal gains located in the occupied subzone – task lights, people, equipment – and dividing this power equally among the n plumes. With these assumptions we can describe the implementation.

The UCSD UFI model is controlled by the subroutine *ManageUCSDUFModels* which is called from the *RoomAirModelManager*. The *RoomAirModelManager* selects which zone model will be used for each zone.

The calculation is done in subroutine *CalcUCSDUI*. First we calculate the convective heat gain going into the upper and lower regions.

![](media/image2408.png)\


![](media/image2409.png)\


![](media/image2410.png)\


Next we sum up the inlet air flows in the form of MCP (mass flow rate times the air specific heat capacity) and MCPT (mass flow rate times C~p~ times air temperature).

![](media/image2411.png)\


![](media/image2412.png)\


![](media/image2413.png)\


![](media/image2414.png)\


![](media/image2415.png)\


![](media/image2416.png)\


![](media/image2417.png)\


The number of plumes per occupant ![](media/image2303.png)  is a user input. The total number of plumes in the zone is:

![](media/image2304.png)\


Using this we calculate the total power in the plumes and the power per plume.

![](media/image2418.png)\


![](media/image2307.png)\


The number of diffusers per plumes is also a user input. To obtain the number of diffusers in the zone:

![](media/image2419.png)\


The area *A~diff~* is also a user input. For swirl diffusers and for displacement diffusers this area is used as input. For the variable area diffusers, though, we calculate the area. We assume 400 ft/min velocity at the diffuser and a design flow rate per diffuser is 150 cfm (.0708 m^3^/s). The design area of the diffuser is 150 ft^3^/min /  400 ft/min = .575 ft^2^ = .035 m^2^. Then the variable area each time step is

![](media/image2420.png)\


We now calculate the height fraction *Fr~hb~* (height of boundary layer divided by the total zone height).

![](media/image2421.png)\


![](media/image2422.png)\


where *~throw~* is a user input: the angle between the diffuser slots and vertical; and *H~s~* is the source height above the floor (m).

Next we iterate over the following 2 steps.

#### Iterative procedure

Call subroutine *HcUCSDUF* to calculate a convective heat transfer coefficient for each surface in the zone, an effective air temperature for each surface, and HA~mx~, HAT~mx~, HA~oc~, HAT~oc~. Here HA is ![](media/image2311.png)  for a region and HAT is ![](media/image2312.png)  for a region. The sum is over all the surfaces bounding the region; ![](media/image2313.png)  is the convective heat transfer coefficient for surface i, ![](media/image2314.png)  is the area of surface i,  and ![](media/image2315.png)  is the surface temperature of surface i.

Calculate the two subzone temperatures: *T~oc~* and *T~mx~*.

The h~c~'s calculated in step 1 depend on the subzone temperatures. In turn the subzone temperatures depend on the HA and HAT's calculated in step 1. Hence the need for iteration

Next we describe each steps 1 and 2 in more detail.

#### Step 1

Subroutine *HcUCSDUF* is quite straightforward. It loops through all the surfaces in each zone and decides whether the surface is located in the upper, mixed subzone or the lower, occupied subzone, or if the surface is in both subzones. If entirely in one subzone the subzone temperature is stored in the surface effective temperature variable *TempEffBulkAir(SurfNum)* and h~c~ for the surface is calculated by a call to subroutine *CalcDetailedHcInForDVModel*. This routine uses the "detailed" natural convection coefficient calculation that depends on surface tilt and ![](media/image2317.png) . This calculation is appropriate for situations with low air velocity.

For surfaces that bound 2 subzones, the subroutine calculates h~c~for each subzone and then averages them, weighting by the amount of surface in each subzone.

During the surface loop, once the h~c~ for a surface is calculated, the appropriate subzone HA and HAT sums are incremented. If a surface is in 2 subzones the HA and HAT for each subzone are incremented based on the area of the surface in each subzone.

#### Step 2

The calculation of  subzone temperatures follows the method used in the **ZoneTempPredictorCorrector** module and described in the section **Basis for the System and Zone Integration**. Namely a third order finite difference expansion of the temperature time derivative is used in updating the subzone temperatures. Otherwise the subzone temperatures are obtained straightforwardly by solving an energy balance equation for each subzone.

![](media/image2423.png) ![](media/image2424.png)\


Here ![](media/image2322.png)  and ![](media/image2323.png)  are the heat capacities of the air volume in each subzone. ![](media/image2323.png)  is calculated by

![](media/image2324.png)\


![](media/image2325.png)\


The gains fraction ![](media/image2305.png) is a user input via a schedule. It is the fraction of the convective gains in the occupied subzone that remain in that subzone.

The other subzone air heat capacities are calculated in the same manner.

#### Mixed calculation

The above iterative procedure assumed that the UFAD nonuniform zone model was appropriate: i.e., conditions were favorable temperature stratification in the zone. Now that this calculation is complete and the subzone temperatures and depths calculated, we check to see if this assumption was justified. If not, zone conditions must be recalculated assuming a well-mixed zone.

If ![](media/image2326.png)  or ![](media/image2327.png)  or ![](media/image2425.png)  then the following mixed calculation will replace the UFAD interior zone calculation.

> **Note:**  ![](media/image2329.png)  is the minimum thickness of occupied subzone. It is set to 0.2 meters.

The mixed calculation iteratively calculates surface convection coefficients and room temperature just like the displacement ventilation calculation described above. In the mixed case however, only one zone temperature *T~avg~* is calculated. The 3 subzone temperatures are then set equal to *T~avg~*.

First, *Fr~hb~*  is set equal to zero.

Then the code iterates over these steps.

Calculate *T~avg~* using

![](media/image2426.png)\


![](media/image2333.png)\


![](media/image2334.png)\


Call *HcUCSDUF* to calculate the *h~c~*'s.

Repeat step 1

#### Final calculations

The UFAD interior zone calculation finishes by calculating some report variables.

We define heights:

![](media/image2338.png)\


![](media/image2339.png)\


![](media/image2427.png)\


Using the user defined comfort height we calculate the comfort  temperature.

If mixing:

![](media/image2342.png)\


If UFAD:

If  ![](media/image2345.png)

![](media/image2428.png)\


Else if ![](media/image2347.png)  and ![](media/image2348.png)

![](media/image2349.png)\


Else if ![](media/image2350.png)  and ![](media/image2351.png)

![](media/image2352.png)\


Using the user defined thermostat height we calculate the temperature at the thermostat.

If mixing:

![](media/image2353.png)\


If UFAD:

If![](media/image2356.png)

![](media/image2429.png)\


Else if ![](media/image2358.png)  and ![](media/image2359.png)

![](media/image2360.png)\


Else if ![](media/image2361.png)  and ![](media/image2362.png)

![](media/image2363.png)\


The average temperature gradient is:

If ![](media/image2430.png)

![](media/image2431.png)\


else ![](media/image2432.png)

Finally, the zone node temperature is set to *T~mx~*.

Other variables that are reported out are ![](media/image2433.png) and ![](media/image2434.png) .

![](media/image2435.png)\


where ![](media/image2436.png) is the zone supply air temperature.

#### References

Lin, Y.J. and P.F. Linden. 2005. A model for an under floor air distribution system. **Energy&Building, Vol. 37, pp. 399-409.

### Under-Floor Air Distribution Exterior Zone Model

#### Overview

The input object RoomAirSettings:UnderFloorAirDistributionExterior provides a simple model for heat transfer and a nonuniform vertical temperature profile for exterior zones of a UFAD system. These zones are expected to be dominated by internal loads, a portion of which (such as occupants and workstations) will act to generate plumes, and by window solar and conduction heat gains. The solar radiation penetrating the room is not expected to generate plumes. However, a window plume is likely to be generated in sunny conditions, particularly if an interior blind is deployed. Thus the exterior UFAD zone will have potentially have plumes from people and equipment and plumes arising from the windows. The plumes act to potentially create room air stratification, depending on the type & number of diffusers, the amount and type of load, and the system flowrate. In order to better model this situation the fully-mixed room air approximation that is currently used in most whole building analysis tools is extended to a two node approach, with the purpose of obtaining a first order precision model for vertical temperature profiles for the exterior zones of UFAD systems. The use of 2 nodes allows for greatly improved prediction of thermal comfort and overall building energy performance for the increasingly popular UFAD systems.

The UCSD UFAD Exterior Zone Model is one of the non-uniform zone models provided through the Room Air Manager in EnergyPlus. The intent is to provide a selection of useful non-uniform zone air models to enable the evaluation of air-conditioning techniques that use stratified or partially stratified room air. Such techniques include displacement ventilation (DV) and underfloor air distribution (UFAD) systems. The methodology can also include natural displacement ventilation and also wind-driven cross-ventilation (CV).

#### Underfloor air distribution systems

UFAD systems represent, in terms of room air stratification, an intermediate condition between a well-mixed zone and displacement ventilation. Air is supplied through an underfloor plenum at low pressure through diffusers in the raised floor. The diffusers can be of various types: e.g., swirl, variable-area, displacement, and produce more or less mixing in the zone. UFAD systems are promoted as saving energy due to: higher supply air temperature; low static pressure; cooler conditions in the occupied subzone than in the upper subzone; and sweeping of some portion of the convective load (from ceiling lights, for instance) into the return air without interaction with the occupied region of the zone.

Modeling a UFAD system is quite complex and involves considerably more than just a non-uniform zone model. The zones' coupling to the supply and return plenums must be modeled accurately (particularly radiative transfer from a warm ceiling to a cool floor and on into the supply plenum by conduction). The supply plenum must be accurately modeled, giving a good estimate of the supply air temperature and conduction heat transfer between supply & return plenums through the slab. The HVAC system must be modeled including return air bypass and various types of fan powered terminal units.

The UCSD UFAD exterior zone model is similar to the UCSD interior zone model. The most obvious difference is that the exterior UFAD has 2 different types of plume sources: people & equipment and windows. Like the UCSD UFAD interior model it is a plume equation based multi-layer model (2 layers in this case). The zone is modeled as being divided into 2 well separated subzones which we denote as "occupied" and "upper". Each subzone is treated as having a single temperature. The boundary between the 2 subzones moves up & down each time step as a function of zone loads and supply air flow rate. Thus at each HVAC time step, the height of the boundary above the floor must be calculated, portions of surfaces assigned to each subzone, and a separate convective heat balance performed on each subzone.

#### Model Description

As in the interior zone case, we define 2 dimensionless parameters: ![](media/image2437.png) and ![](media/image2438.png) . The definitions of the 2 parameters are the same as in the previous section (equations , , , , and ). As in the previous case, the experimental data can be plotted versus ![](media/image2439.png) and lines fitted to the data give the following formulas for occupied subzone temperature and interface height.

![](media/image2440.png)\


![](media/image2441.png)\


where *T~oz~* is the occupied subzone temperature (K); *T~r~* is the return temperature (K); ** is the dimensionless height parameter defined above; *T~s~* is the supply temperature (K); *h* is the interface height (m); *n* is the number of diffusers; *m* is the number of heat sources; *A* is the effective area of a diffuser (m^2^); and *h~s~* is the vertical position of the heat sources (m). The formula for *T~r~* is the same as in the previous section.

#### Implementation

The implementation closely follows the procedure described in the UFAD interior zone model. The model predicts two temperatures that characterize the two main levels in the stratification of the room:

an occupied subzone temperature T~oc~ representing the temperature of the occupied region;

an upper level temperature T~mx~representing the temperature of the upper, mixed region and the outflow temperature.

We will use  to calculate the interface height and do a heat balance calculation on each subzone. ** is given by . The supply air flow rate ![](media/image2292.png)  is obtained by summing all the air flows entering the zone: supply air, infiltration, ventilation, and inter-zone flow. The heat gain ![](media/image2293.png)  is estimated by summing all the convective internal gains located in the occupied subzone – task lights, people, equipment – and adding to this the convective gain coming from the window surface. With these assumptions we can describe the implementation.

The UCSD UFE model is controlled by the subroutine *ManageUCSDUFModels* which is called from the *RoomAirModelManager*. The *RoomAirModelManager* selects which zone model will be used for each zone.

The calculation is done in subroutine *CalcUCSDUEI*. First we calculate the convective heat gain going into the upper and lower regions.

![](media/image2442.png)\


![](media/image2443.png)\


![](media/image2444.png)\


Next we sum up the inlet air flows in the form of MCP (mass flow rate times the air specific heat capacity) and MCPT (mass flow rate times C~p~ times air temperature).

![](media/image2445.png)\


![](media/image2446.png)\


![](media/image2447.png)\


![](media/image2448.png)\


![](media/image2449.png)\


![](media/image2450.png)\


![](media/image2451.png)\


For exterior zone model, we assume one plume: ![](media/image2452.png) . The number of diffusers in the zone ![](media/image2453.png) is a user input.

The area *A~diff~* is also a user input. For swirl diffusers, linear bar grilles, and displacement diffusers this area is used as input. For the variable area diffusers, though, we calculate the area. We assume 400 ft/min velocity at the diffuser and a design flow rate per diffuser is 150 cfm (.0708 m^3^/s). The design area of the diffuser is 150 ft^3^/min /  400 ft/min = .575 ft^2^ = .035 m^2^. Then the variable area each time step is

![](media/image2454.png)\


We now make an initial estimate of the convective gain from the windows.

![](media/image2455.png)\


Then

![](media/image2456.png)\


We now calculate the height fraction *Fr~hb~* (height of boundary layer divided by the total zone height).

![](media/image2457.png)\


![](media/image2458.png)\


where *~throw~* is a user input: the angle between the diffuser slots and vertical; and *H~s~* is the source height above the floor (m).

Next we iterate over the following 2 steps.

#### Iterative procedure

Call subroutine *HcUCSDUF* to calculate a convective heat transfer coefficient for each surface in the zone, an effective air temperature for each surface, and HA~mx~, HAT~mx~, HA~oc~, HAT~oc~, HA~mx,win~,HAT~mx,win~,HA~oc,win~,and HAT~oc,win~. Here HA is ![](media/image2311.png)  for a region and HAT is ![](media/image2312.png)  for a region. The sum is over all the surfaces bounding the region; ![](media/image2313.png)  is the convective heat transfer coefficient for surface i, ![](media/image2314.png)  is the area of surface i,  and ![](media/image2315.png)  is the surface temperature of surface i. Variables with the *win* subscript are summed over window surfaces only. Then the convective gain from the window is recalculated:

![](media/image2459.png)\


and the power in the plume is recalculated: ![](media/image2460.png) .

Calculate the two subzone temperatures: *T~oc~* and *T~mx~*.

The h~c~'s calculated in step 1 depend on the subzone temperatures. In turn the subzone temperatures depend on the HA and HAT's calculated in step 1. Hence the need for iteration

Next we describe each steps 1 and 3 in more detail.

#### Step 1

Subroutine *HcUCSDUF* is quite straightforward. It loops through all the surfaces in each zone and decides whether the surface is located in the upper, mixed subzone or the lower, occupied subzone, or if the surface is in both subzones. If entirely in one subzone the subzone temperature is stored in the surface effective temperature variable *TempEffBulkAir(SurfNum)* and h~c~ for the surface is calculated by a call to subroutine *CalcDetailedHcInForDVModel*. This routine uses the "detailed" natural convection coefficient calculation that depends on surface tilt and ![](media/image2317.png) . This calculation is appropriate for situations with low air velocity.

For surfaces that bound 2 subzones, the subroutine calculates h~c~for each subzone and then averages them, weighting by the amount of surface in each subzone.

During the surface loop, once the h~c~ for a surface is calculated, the appropriate subzone HA and HAT sums are incremented. If a surface is in 2 subzones the HA and HAT for each subzone are incremented based on the area of the surface in each subzone. The subroutine calculates a separate HA and HAT for the windows for use in calculating the window convective gain.

#### Step 2

The calculation of  subzone temperatures follows the method used in the **ZoneTempPredictorCorrector** module and described in the section **Basis for the System and Zone Integration**. Namely a third order finite difference expansion of the temperature time derivative is used in updating the subzone temperatures. Otherwise the subzone temperatures are obtained straightforwardly by solving an energy balance equation for each subzone.

![](media/image2461.png) ![](media/image2462.png)

Here ![](media/image2322.png)  and ![](media/image2323.png)  are the heat capacities of the air volume in each subzone. ![](media/image2323.png)  is calculated by

![](media/image2324.png)\


![](media/image2325.png)\


The gains fraction ![](media/image2305.png) is a user input via a schedule. It is the fraction of the convective gains in the occupied subzone that remain in that subzone.

The other subzone air heat capacities are calculated in the same manner.

#### Mixed calculation

The above iterative procedure assumed that the UFAD zone model was applicable: i.e., conditions were favorable temperature stratification in the zone. Now that this calculation is complete and the subzone temperatures and depths calculated, we check to see if this assumption was justified. If not, zone conditions must be recalculated assuming a well-mixed zone.

If ![](media/image2326.png)  or ![](media/image2327.png)  or ![](media/image2463.png)  then the following mixed calculation will replace the UFAD exterior zone calculation.

> **Note:**  ![](media/image2329.png)  is the minimum thickness of occupied subzone. It is set to 0.2 meters.

The mixed calculation iteratively calculates surface convection coefficients and room temperature just like the displacement ventilation calculation described above. In the mixed case however, only one zone temperature *T~avg~* is calculated. The 2 subzone temperatures are then set equal to *T~avg~*.

First, *Fr~hb~*  is set equal to zero.

Then the code iterates over these steps.

Calculate *T~avg~* using

![](media/image2464.png)\


![](media/image2333.png)\


![](media/image2334.png)\


Call *HcUCSDUF* to calculate the *h~c~*'s.

Repeat step 1

#### Final calculations

The UFAD exterior zone calculation finishes by calculating some report variables.

We define heights:

![](media/image2338.png)\


![](media/image2339.png)\


![](media/image2465.png)\


Using the user defined comfort height we calculate the comfort  temperature.

If mixing:

![](media/image2342.png)\


If UFAD:

If  ![](media/image2345.png)

![](media/image2466.png)\


Else if ![](media/image2347.png)  and ![](media/image2348.png)

![](media/image2349.png)\


Else if ![](media/image2350.png)  and ![](media/image2351.png)

![](media/image2352.png)\


Using the user defined thermostat height we calculate the temperature at the thermostat.

If mixing:

![](media/image2353.png)\


If UFAD:

If![](media/image2356.png)

![](media/image2467.png)\


Else if ![](media/image2358.png)  and ![](media/image2359.png)

![](media/image2360.png)\


Else if ![](media/image2361.png)  and ![](media/image2362.png)

![](media/image2363.png)\


The average temperature gradient is:

If ![](media/image2468.png)

![](media/image2469.png)\


else ![](media/image2470.png)

Finally, the zone node temperature is set to *T~mx~*.

Other variables that are reported out are ![](media/image2471.png) and ![](media/image2472.png) .

![](media/image2473.png)\


where ![](media/image2474.png) is the zone supply air temperature.

#### References

Qing Liu. 2006. The Fluid Dynamics of an Underfloor Air Distribution System. A PhD dissertation in Engineering Sciences (Systems Science) at UC San Diego.

### Cross Ventilation Room Air Model

#### Overview

The input object RoomAirSettings:CrossVentilation provides a simple model, developed by UCSD, for heat transfer and vertical temperature profile prediction in cross ventilated rooms. This model was developed using scaling analysis, experimental correlations, computational fluid dynamics, and approximate solutions of the Navier-Stokes equations. The model distinguishes two regions in the room, the main jet region and the recirculations, and predicts characteristic airflow velocities and average air temperatures. The model consists of a set of analytical expressions that clearly display the first order effects of room geometry and inflow characteristics on Cross Ventilation airflow and heat transfer.

#### Cross-Ventilation

The CV model is developed from analysis of the flow structure inside a cross-ventilated room, which naturally divides into distinct jet and recirculation flow regions. The flows are calculated for different room and opening geometries using CFD. Correlations with room and aperture parameters are then deduced for a characteristic temperature and velocity in the jet and recirculation regions.

The SS 2-opening model is developed from analysis of wind tunnel data for pressure and ventilation rates, correlating the flow rate with the pressure difference between the two openings, with contributions from both the mean and unsteady components. The local velocity estimate is also based on wind tunnel data, and gives the velocity as a function of wind angle.

#### Approach

With the widespread availability of air conditioning, natural ventilation became a rare feature in commercial buildings in the US. The recent resurgence of the use of natural ventilation relies on systems whose performance can be predicted and controlled, often working in conjunction with mechanical ventilation and cooling systems in hybrid ventilation configurations. In this contemporary approach natural ventilation deals with thermal and air renewal requirements in the cold and mild season and often plays a limited role in the warm season (due to the need to control indoor velocities and noise levels). Whereas traditional natural ventilation systems often use large openings (1-4m^2^), both recent and expected future examples tend to use smaller openings (0.2-1m^2^), often in multiple opening configurations that allow for efficient heat removal with controlled indoor velocities.

The modeling approach used is based on simplified solutions of the Navier Stokes equations that are integrated into scaling formulae that predict relevant flow quantities (characteristic velocities and temperatures) in distinct regions of the flow. The scaling expressions that will be developed use the principle of flow similarity: for a given room geometry and flow regime (either laminar or turbulent), the flow pattern is constant. In the present case, where the goal is to predict internal velocities using as input the inflow velocity (that can be estimated by the airflow rate predicted by the AirflowNetwork model), the principle can be stated as:

(Velocity in a given position in the room) = (Inflow velocity) × (Constant)

The constant is non-dimensional and depends on the position or room zone where the prediction is being made. For a given geometry the constant can be measured experimentally or inferred from a detailed CFD simulation. It is expected that the constant may depend on non-dimensional room geometry parameters that can be inferred from a careful analysis of the flow. The proposed CV model uses a set of CFD simulations, in conjunction with simplified solutions of the Navier Stokes equations, to obtain the constants and their dependence on room geometry features.

The updated CV model proposed in this document was developed using inlet areas *A*~in~ and room cross-section areas *A*~RM~ in the following range:

0.5% < *A*~in~/*A*~RM~ < 5%

whereas the existing model used

3.2% < *A*~in~/*A*~RM~ < 21%

The room lengths for which these models are expected to be applied range between the smallest room size of 4m and the maximum achievable CV room flushing length of 15-18m.

![Development of an axisymmetric jet from airflow through a window.](media/development-of-an-axisymmetric-jet-from.png)


Air flows into the room through an inflow window in the form of an approximately axisymmetric jet, with characteristic diameter *L*=*A*~in~^1/2^, that is confined by the room surfaces (see Figure 137). The initial portion of the jet (up to six diameters) is called the potential core and is characterized by shear layer development along the perimeter of the jet. After this phase the jet transitions into a self similar profile whose centerline velocity decays as 1/*x*, where *x* = *x*/*D* (Awbi, 2003).

For the relevant room length range the geometries used in the two models result in:

New model:6 < x < 60

Existing model: 2 < x < 18

The smaller openings used in the new model result in significantly more space for jet development along the room length. In all but the shortest rooms the jet will enter the fully developed stage characterized by a 1/*x* centerline velocity decay rate. For this reason, the updated model uses the average jet velocity profile as opposed to shear layer momentum transfer.

#### Components of the flow

Figure 136 shows the schematic development of a free axisymmetric jet in an unconfined environment, and Figure 137 depicts the confinement effects. The air entrained by the jet is detrained at the outlet, creating two recirculation zones that do not exist in a free jet.

The recirculation flow is similar to the flow in a lid driven cavity, where here the driving force is the confined jet flow. Turbulent lid driven cavity flows have been shown to have self-similar velocity profiles, as shown in Figure 138.

The model is based on the hypothesis that the flow is composed of:

A – Lid driven cavity flows (in the recirculation regions).

B – A confined axisymmetric jet flow.

![Top view of a cross ventilated room. The air entrained by the jet is detrained at the outlet, typically creating two recirculation zones.](media/top-view-of-a-cross-ventilated-room.-the-air.png)


![Top view of one half of a cross ventilated room. The flow is approximately composed of a confined jet and two lid driven cavity flows (one on each side).](media/top-view-of-one-half-of-a-cross-ventilated.png)


The goal of the model is to obtain the value and functional form of the constant term shown in the scaling laws for the characteristic average velocities in the two flow regions: jet and recirculation regions.

To estimate the average jet velocity we integrate along the depth of the room (including both the core region and the 1/*x* velocity decay region) to derive an average maximum jet velocity, *V*~J,m~:∙

![](media/image2478.png)\


(To allow for wind angle effects the room depth *D* has been replaced by *D*\* - see a later section.) The lid-driven cavity flow profile that characterizes the recirculation region has a maximum velocity that is approximately one half of the driving velocity, which in this case is due to the jet. For this reason we define the separation between the two regions along the virtual surface where the jet reaches 50% of its maximum, centerline, velocity.

The analysis of the flow revealed that the room containment effect leads to higher velocities for cases that combine a large inflow opening with a small room cross-section area. In order to account for this effect we propose to use the following non-dimensional scaling parameter:

![](media/image2479.png)\


The expression used for the scaling of the average jet velocity is then:

![](media/image2480.png)\


or

![](media/image2481.png)\


where the constant *C*~1~ will be obtained from the results of a set of CFD simulations shown in the next section.

In addition to the jet velocity the characteristic recirculation flow velocity ![](media/image2482.png)  is also an interesting parameter that, according to the self-similar flow profile hypothesis shown in Figure 138, should scale with a similar expression to the one shown above for the jet (although with a different correlation constant, *C*~2~). The modeling hypothesis can also be tested by evaluating its capability to predict the maximum recirculation flow rate, which can be predicted by multiplying the average velocity by the room cross-section area:

![](media/image2483.png)\


Because of the self-similar velocity profile shape that occurs in the recirculation, the near wall velocity (used to predict forced convection) can be estimated simply by multiplying the characteristic recirculation velocity by two.

The expressions obtained apply in the turbulent regime (because the CFD simulations on which they are based are for that regime). Both velocity correlations presented below have the functional form:

![](media/image2484.png)\


where *a* is *C*~1~, *C*~2~ or *C*~3~, X is a velocity scale and *b* is a constant that results from the best fit. The model has a lower limit on the value of *X*: if *X* is too small the flow is no longer turbulent. This limit implies that the point *X* = 0 is never achieved, avoiding the unrealistic prediction of *V*(0)=*b*.

In addition to the velocity and airflow rate predictions described above it is also essential to predict the temperature variations in the two zones of the flow. In most cases these variations will be positive due to the internal sensible heat gains. However`, as a result of room surface heat transfer, in some cases the variations may be negative. Analysis of the CFD simulations that are presented in the next section revealed that approximately all the exhaust air comes from the jet regions. For this reason, the heat gains affect the two regions differently:

Heat gains in the jet region only affect this region (no temperature increase in the recirculation regions)

Heat gains in the recirculations affect both jet and recirculation regions (because they must be exhausted by the jets).

In order to predict the temperature increase, heat gains were inserted into the recirculation regions for different room geometries in the set of simulations presented in the next section. The volume containing the heat gains extended over the whole depth of the room, from floor to ceiling, and in the lateral direction from halfway between the window edge and the wall all the way to the wall. Applying energy conservation principles to predict the temperature increase in the jet and recirculation region as a function of the sensible heat gains in each region (the sum of internal gains and internal surface heat transfer) we can obtain the average value of temperature increase in each region:

![](media/image2485.png)\


![](media/image2486.png)\


where q''~RM~ is the total internal heat gain for the room,  and *c*~p~ are the density and specific heat capacity of air, respectively, and *C*~T,J~, *C*~T,R~ are constants.

#### Results

Table 44 lists the cases used to develop the updated CV model, which predicts the output variables defined in Table 45 (see also Figure 139). Figure 140, Figure 141, and Figure 142 show plots of the data derived from the CFD simulations of these test cases, and the lines giving the best fit to the data. Table 46 summarizes the correlation formulae and constants. Finally, Table 47 provides estimates for the error of the correlation in each test case.

![Jet and recirculation regions in typical vertical cross-section through room (y-z plane). Jet boundary occurs where jet velocity has fallen to 50% of centerline maximum. Remainder of cross-section is treated as recirculation. Volumetric heat sources ar](media/jet-and-recirculation-regions-in-typical.png)


Table: Test cases used to develop correlations

Case|Opening area, |*A*~in~ (m^2^)|Room width, |*W* (m)|Room depth, |*D* (m)|Room height, |*H* (m)
----|--------------|---------------------|------------|--------------|------------|--------------|-------------|--------------
1|0.25|6.0|9.0|2.3
2|0.25|11.0|9.0|2.3
3|0.5|9.0|4.5|2.3
4|0.5|9.0|6.0|2.3
5|0.5|6.0|9.0|2.3
6|0.5|9.0|9.0|2.3
7|0.5|11.0|9.0|2.3
8|0.5|13.5|9.0|2.3
9|0.5|9.0|13.5|2.3
10|0.5|9.0|18.0|2.3
11|0.5|13.5|18.0|2.3
12|1.0|9.0|9.0|2.3
13|1.0|9.0|13.5|2.3
14|1.0|9.0|18.0|2.3
|RANGE (max:min ratio)
|4.0|2.25|4.0|1.0

Table: Definitions of output variables

Output variable|Symbol|Units|Definition
---------------|------|-----|----------
Jet velocity|![](media/image2488.png) |m/s|Volume-averaged jet region velocity. The averaging volume is bounded at each *x* along the room depth by the line in the *y*-*z* plane where the jet velocity drops to 50% of its maximum centerline value.
Recirculation zone velocity|![](media/image2489.png) |m/s|Area-averaged velocity in the *y*-*z* plane with maximum flow. The averaging area is the recirculation part of the room cross-section. Typically the plane of maximum flow occurs at x ~ 2D/3 (D/3 before the outlet).
Recirculation zone flow rate|![](media/image2490.png) |m^3^/s|Total flow rate for the recirculation regions in the plane of maximum flow (see above).
Jet temperature rise|![](media/image2491.png) |°C|Volume-averaged temperature variations in the jet region, over the same volume used to define the jet velocity average.
Recirculation zone temperature rise|![](media/image2492.png) |°C|Volume-average temperature variations in the recirculation region. The average is calculated over the cuboidal volume placed in each recirculation containing the volumetric sensible heat gains (see main text).

See also Figure 139**.** Note the *x*-coordinate is along the room, the *y*-coordinate is across the room and the *z*-coordinate is vertical.

Table: Correlation formulae in the form Y = aX + b

|Y|X|a|b|R^2^
|-|-|-|-|----
Jet velocity|![](media/image2493.png) |![](media/image2494.png) |1.6173|0.1466|0.8343
Recirculation zone velocity|![](media/image2495.png) |![](media/image2496.png) |0.8911|0.0393|0.6464
Recirculation zone flow rate|![](media/image2497.png) |![](media/image2498.png) |0.4444|0.1751|0.6761
Jet temperature rise|![](media/image2499.png) |![](media/image2500.png) |0.8254|0|n/a
Recirculation zone temperature rise|![](media/image2501.png) |![](media/image2502.png) |1.2734|0|n/a

Table: Accuracy of correlation formulae in predicting test case results

Cases|Errors (%)
-----|----------
ID|*A*~in~|(m^2^)|*W*|(m)|*D*|(m)|*H*|(m)|Jet velocity|Recirculation velocity|Recirculation flow rate|Jet temperature rise|Recirculation temperature rise
--|--------------|------|----------|---|----------|---|----------|---|------------|----------------------|-----------------------|--------------------|------------------------------
1|0.25|6|9|2.3|16|45|60|1|10
2|0.25|11|9|2.3|5|43|32|5|7
3|0.5|9|4.5|2.3|2|18|27|15|2
4|0.5|9|6|2.3|6|32|28|7|1
5|0.5|6|9|2.3|7|6|21|7|0
6|0.5|9|9|2.3|10|4|6|15|17
7|0.5|11|9|2.3|6|7|0|14|20
8|0.5|13.5|9|2.3|14|9|5|21|2
9|0.5|9|13.5|2.3|10|11|17|7|8
10|0.5|9|18|2.3|10|25|29|8|3
11|0.5|13.5|18|2.3|8|2|23|9|6
12|1|9|9|2.3|2|5|3|3|4
13|1|9|13.5|2.3|8|10|10|3|6
14|1|9|18|2.3|0|18|6|13|2
Average|||||**8**|**17**|**19**|**9**|**6**

In each case the error is the percentage difference between the given correlation prediction and the test case result obtained from CFD, i.e. 100\*|(correlation prediction – CFD)/CFD|.

![](media/image2503.png)\


![](media/image2503.png)\


![](media/image2504.png)\


![](media/image2504.png)\

![Jet velocity correlation.](media/jet-velocity-correlation..png)


![](media/image2506.png)\


![](media/image2506.png)\


![](media/image2507.png)\


![](media/image2507.png)\

![Recirculation region velocity correlation.](media/recirculation-region-velocity-correlation..png)


![](media/image2509.png)  (m^2^)

![](media/image2509.png)  (m^2^)

![](media/image2510.png)  (m^2^)

![](media/image2510.png)  (m^2^)
![Recirculation region flow rate.](media/recirculation-region-flow-rate..png)


#### Wind angle effects

When the incoming wind is not normal to the façade the jet enters the room at an angle, as shown in Figure 143.

![Schematic of jet resulting from wind at angle φ to façade.](media/schematic-of-jet-resulting-from-wind-at-angle.png)


This oblique geometry has two main effects on the flow and consequent correlation predictions:

The inflow area is reduced, creating a larger inflow velocity for the same flow rate.

The room flow path length is increased, from *D* to *D*\*, resulting in a larger apparent depth.

The first effect increases indoor velocities while the second effect decreases the velocities.

#### Multiple apertures

CFD simulations for rooms with more than one inflow aperture show that the model can be applied to these configurations with minimal adjustments. As a result of increased dissipation in the central recirculation region, where two distinct recirculation flows collide (Figure 144), the airflow velocities are reduced by 5-10%, as shown in the charts in Figure 145.

![Flow regions for a room with 2 inlets and 2 outlets.](media/flow-regions-for-a-room-with-2-inlets-and-2.png)


![Jet velocity and recirculation velocity for the two halves of 3 different 2-inlet rooms, A-C, compared with the corresponding 1-inlet rooms.](media/jet-velocity-and-recirculation-velocity-for.png) ![Jet velocity and recirculation velocity for the two halves of 3 different 2-inlet rooms, A-C, compared with the corresponding 1-inlet rooms.](media/jet-velocity-and-recirculation-velocity-for-001.png)


**Heat transfer in recirculating flows**

In most cross ventilation flows with recirculations the mixing between recirculation and inflow jet is only partial. Figure 146 shows a schematic representation of the heat transfer process (showing a top view of one half of a symmetrical room). The flow is divided into three distinct streams with connected temperature variations:

The main jet (labeled J in the figure).

The part of the recirculation flow that exchanges heat with the jet (label R).

The wall boundary layer part of the recirculation flow (label W).

![Top view of the flow structure in case R.](media/top-view-of-the-flow-structure-in-case-r..png)


The light gray arrows show flow direction. The dark gray arrows show heat transfer in the shear layer. The recirculation region coordinate system is shown in the figure, with coordinate *r* varying between 0 and *L*.

#### Heat transfer in a shear layer

It is possible to use a formulation with a heat transfer area, a convection heat transfer coefficient and an average shear layer temperature difference to model the heat exchange between jet and recirculation flows:

![](media/image2517.png) , ![](media/image2518.png)

Locally, the heat transfer process is driven by the variable, local, temperature difference:

![](media/image2519.png)\


#### Predicting air temperature in the recirculation region of the flow

An accurate representation of system behavior must consider both the local heat transfer in the shear layer (varying along the *x* direction, driven by the temperature profiles *T~J~(x)* and *T~R~(x)*) and the heat transfer in the wall boundary layers (with temperature *T~W~(r)*). Both transfers can be modeled using differential equations, resulting in the following system of equations:

![](media/image2520.png)\


The relevant boundary conditions are:

![](media/image2521.png)\


The first equation in  models heat transfer in the wall layer. The second models the temperature variation of the portion of the recirculation flow that is in contact with the jet, it differs from the third equation only in a sign (the temperature variation in *T~R~* is symmetric to the variation in *T~J~*) and the flow rate (*F.R* for the recirculation and *F* for the main jet). The temperature in the recirculation region is given by:

![](media/image2522.png)\


#### Heat transfer in recirculating flows with internal gains

Whereas surface heat transfer is an important component of room heat transfer, internal gains often dominate the room temperature field. Referring to the previously used subdivision of the room into jet and recirculation regions we can see that gains can occur exclusively in the jet region, exclusively in the recirculation region or in both regions.

#### Heat gains in the jet region

The effect of gains in the jet region on room air temperature distribution is simple to model and, within the first order accuracy goal, can be characterized as follows:

For gains occurring in the jet region (between the inlet and the room outlet) a change in inflow temperature is an adequate, conservative approximation. From energy conservation we conclude that the altered inflow temperature is given by:

![](media/image2523.png)\


Gains occurring in the jet region, close to the outlet can be ignored in a first order accuracy model.

Clearly the two approaches described above are only exact when: the gains occur at the inlet and perfectly mix with the inflow jet (for the first case), or whenever the gains occur very close to the outlet (in the second case). In all other situations, the first approach provides a conservative approximation. In the model implementation in EnergyPlus the user will not define the exact location of the heat gains within each region of the flow therefore, heat gains in the jet region, are inserted in two equal parts in two separate points of the jet flow (in the room entrance and before the exit, see points r=0 and r=L~R~ in Figure 146).

#### Heat gains in the recirculation region

When heat gains occur in the recirculation region significant heat accumulation occurs due to the limited heat transfer ability of the shear layer. Due to this limited ability, the recirculation is partially isolated from the main jet flow and higher temperatures are generated in this region whenever heat gains are present. The concept of a global heat transfer coefficient is not applicable.

In the case of a flow in a room with adiabatic surfaces and heat gains in the recirculation region the indoor air temperatures can be predicted using the solution to the following system of equations:

![](media/image2524.png)\


This system of equations differs from system F16 in several ways. Because there is no wall heat transfer there is no need to split the recirculation flow in two parts (*R* and *W*). The temperature increase in the recirculation flow is simply defined by the internal gains (first equation in ). The second and third equations in  model shear layer heat transfer are similar in the two cases ( and ).

The recirculation flow temperature at *x*=0 is given by:

![](media/image2525.png)\


The temperature in the recirculation is inversely proportional to the recirculation flow ratio (*R*) and the shear layer area (measured by the product: *A~SL~ h~SL~*). As both of these parameters increase this expression becomes similar to . The recirculation flow temperature at *x=L* is given by:

![](media/image2526.png)\


The temperature variations in the recirculation flow is given by:

![](media/image2527.png)\


#### Combined effects of surface heat transfer and internal gains in the recirculation region

In this case, the following system of equations must be solved:

![](media/image2528.png)\


With the boundary conditions: ![](media/image2529.png)

For simplicity the heat gains are considered to be evenly distributed along the recirculation path. The temperature variation in the recirculation region is given by:

![](media/image2530.png)\


Table: List of variables for CV model

Symbol|Units|Description
------|-----|-----------
a|-|Slope of correlation line of best fit
A~in~|m^2^|Inlet area
A~RM~|m^2^|Room cross-sectional area
b|-|Intercept of correlation line of best fit
c~p~|J/kg/K|Specific heat capacity of air
C~RM~|-|Confinement scaling factor, (A~in~/A~RM~)^1/2^
C~1~|-|Correlation constant for jet velocity
C~2~|-|Correlation constant for recirculation velocity
C~3~|-|Correlation constant for recirculation flow rate
D|m|Room depth, distance between inlet and outlet facades
D\*|m|Effective room depth adjusted for wind angle
H|m|Room height
![](media/image2531.png) |W|Total heat input from internal heat sources
Q~in~|m^3^/s|Inlet flow rate, A~in~ U~in~
![](media/image2532.png) |m^3^/s|Maximum recirculation region flow rate
U~in~|m/s|Inlet velocity, average over inlet area
U~m~|m/s|Maximum centerline velocity of jet
U~0~|m/s|Uniform inlet velocity of jet
![](media/image2533.png) |m/s|Volume-averaged jet velocity
![](media/image2534.png) |m/s|Average recirculation region velocity over plane of maximum flow
V|m/s|Velocity scale
V~J,m~|m/s|Average of U~m~ over room depth
W|m|Room width
x|m|Coordinate along room depth, 0 ≤ x ≤ D
x|-|Non-dimensional x-coordinate, x/D
X|m/s|Velocity correlation parameter
Y|m|Lateral coordinate across room width
Z|m|Vertical coordinate
![](media/image2535.png) |°C|Average jet temperature variation
![](media/image2536.png) |°C|Average recirculation region temperature variation
|kg/m^3^|Density of air
φ|°|Wind direction relative to façade normal

#### References

Awbi, H.B. (2003) *Ventilation of Buildings* (2^nd^ edition). Routledge, 536pp.

Carrilho da Graca, G. 2003. Simplified models for heat transfer in rooms. Ph. D. Thesis, University of California, San Diego.

Kalema, T., Haapala, T. `995. Effect of interior heat transfer coefficients on thermal dynamics and energy consumption, Energy and Buildings 22 (2) pp. 101-113.

Aynsley, R.M., Melbourne, W., Vickery, B.J. 1977. Architectural Aerodynamics. Applied Science London.

Baturin, V. V. , Billington, N. S. 1972. Fundamentals of Industrial Ventilation, Franklin Book Company 1972, pp. 174-179.

Neiswanger, L., Johnson, G.A., Carey, V.P. 1987. An experimental study of high Raleigh number mixed convection in a rectangular enclosure with restricted inlet and outlet openings. Transactions of ASME, Vol. 109, 446-453.

Ohba, M., Irie, K., Kurabuchi, T. 2001. Study on airflow characteristics inside and outside a CVmodel, and ventilation flow rate using wind tunnel experiments. Journal of Wind Engineering and Industrial Aerodynamics, in press.

Chandra, S., Kerestecioglu, A.A. 1984. Heat transfer in naturally ventilated rooms data from full-scale measurements, ASHRAE Transactions, Volume 90, part 1b 211-224.

Altmayer, E.F., Gadgil, A.J., Bauman, F.S., Kammerud, R.C. Correlations for convective heat transfer from room surfaces. ASHRAE Transactions, No. 2764.

Spitler, J. D., Pedersen, C.O., Fisher, D.E. 1991. Interior Convective Heat Transfer in Buildings with Large Ventilative Flow Rates. ASHRAE Transactions, Vol. 97, Pt.1, pp. 505-515.

Awbi, H.B. & Hatton, A. 1999. Natural convection from heated room surfaces, Energy and Buildings, 30, 233-244.

Siebers, D. L., Schwind, R. G., Moffat, R. J. 1983. Experimental Mixed Convection Heat Transfer From a Large Vertical Surface in a Horizontal Flow. SAND 83-8225, Sandia National Laboratories, Livermore CA.

Awbi, H.B. & Hatton, A. 2000. Mixed convection from heated room surfaces, Energy and Buildings, 32, 153-166.

Bejan, A. 1994. Convection Heat Transfer 2nd ed, Wiley, USA.