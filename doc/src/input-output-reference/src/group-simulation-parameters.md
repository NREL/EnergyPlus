# Group -- Simulation Parameters

This group of objects influences the simulation in various ways.

## Version

### Inputs

#### Field: Version Identifier

The [Version](#version) object allows you to enter the proper version that your IDF was created for. This is checked against the current version of EnergyPlus and a Severe error issued (non-terminating) if it does not match the current version string. Note that versions are often significant and there is no guarantee that the older file will run in the newer versions of the program. See IDF [Version](#version) Updater (Auxiliary Programs Document) for methods of changing the older files to newer versions.

## Timestep

### Inputs

#### Field: Number of Timesteps per Hour

The [Timestep](#timestep) object specifies the "basic" timestep for the simulation. The value entered here is usually known as the [Zone](#zone) [Timestep](#timestep).  This is used in the [Zone](#zone) Heat Balance Model calculation as the driving timestep for heat transfer and load calculations.  The value entered here is the number of timesteps to use within an hour.  Longer length timesteps have lower values for Number of Timesteps per Hour.  For example a value of 6 entered here directs the program to use a zone timestep of 10 minutes and a value of 60 means a 1 minute timestep.  The user's choice for Number of Timesteps per Hour must be evenly divisible into 60; the allowable choices are 1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, and 60.

The choice made for this field has important implications for modeling accuracy and the overall time it takes to run a simulation.  Here are some considerations when choosing a value:

The solution technique used in EnergyPlus has been designed to be stable with zone timesteps of up to sixty minutes (Number Timesteps in Hour = 1).  However, 60 minutes is considered a "long" timestep and it should only be used in rare occasions where there is no HVAC system, accuracy is not a concern, and short run times are critical.  Such long timesteps are not recommended to use because simulation results are more accurate for shorter timesteps, of say 10 minutes or less (Number of Timesteps per Hour of 6 or more).  Shorter zone timesteps improve the numerical solution of the [Zone](#zone) Heat Balance Model because they improve how models for surface temperature and zone air temperature are coupled together.  Longer timesteps introduce more lag and lead to more a dampened dynamic response.

Simulation run time increases with shorter timesteps or larger values for Number of Timesteps per Hour.  The effect varies with the nature of the model.  The user can test out different values on their particular model to understand the implications for his or her particular case.  Sometimes large models with multizone HVAC and Plant systems execute nearly as fast with 15 minute timesteps as with 60 minute timesteps because fewer iterations are required in the system modeling since the prior timestep's results are close to the final outcome of next timestep.

The weather data files usually have 60-minute (or hourly) data.  However, it does not follow that this should be used as the basis for choosing the zone timestep because:

EnergyPlus carefully interpolates the weather data between data points for use at shorter timesteps.  This is discussed in a later section: Weather Data Hourly Interpolation

Many aspects of a model have time scales that differ from the that of the weather data.  A goal of the modeling is to predict how the building will respond to the weather.  However, the building's response is not *governed* by the time scale that the weather data are available at, but rather the time scales of the dynamic performance of the thermal envelope as well as things like schedules for internal gains, thermostats, and equipment availability.

If the model will include calculating the cost of electricity, then the user should be aware that many electric utility tariffs base charges on demand windows of a specified length of time.  If the choice of Number of Timesteps per Hour is not consistent with the demand window, then unexpected results may be obtained.  For reasonable prediction of the maximum rates for electricity use for in calculating demand charges, the length of the zone timestep needs to be consistent with the tariff's demand window.  The following table lists what values are consistent with various demand windows.

Demand Window|Applicable Number of Timesteps per Hour
-------------|----------------------------------------
QuarterHour|4, 12, 20, or 60
HalfHour|2, 4, 6, 10, 12, 20, 30, or 60
FullHour, Day, Week|Any

There is also second type of timestep inside EnergyPlus that is known as the System [Timestep](#timestep). This is a variable-length timestep that governs the driving timestep for HVAC and Plant system modeling.  The user cannot directly control the system timestep (except by use of the [ConvergenceLimits](#convergencelimits) object). When the HVAC portion of the simulation begins its solution for the current zone timestep, it uses the zone timestep as its maximum length but then can reduce the timestep, as necessary, to improve the solution.  The technical details of the approach are explained in the Engineering Documentation under "Integrated Solution Manager".

Users can see the system timestep used if they select the "detailed" frequency option on an HVAC output variable (e.g. [Zone](#zone) Air Temperature). To contrast, the "[Zone](#zone)" variables will only be reported on the zone timestep (e.g. [Zone](#zone) Mean Air Temperature).

And, the IDF example:

~~~~~~~~~~~~~~~~~~~~

    Timestep, 6;  !Suggested default for most system simulations
~~~~~~~~~~~~~~~~~~~~

Suggested defaults are 4 for non-HVAC simulations, 6 for simulations with HVAC, 20 is the minimum for ConductionFiniteDifference and HeatAndMoistureFiniteElement simulations. Green roof (ref: [Material:RoofVegetation](#materialroofvegetation)) also may require more timesteps.

> Note that hourly data (such as outdoor conditions expressed by Design Days or Weather data) are interpolated to the [Zone](#zone) [Timestep](#timestep). This is discussed in a later section: Weather Data Hourly Interpolation

## ConvergenceLimits

This item is an "advanced" feature that should be used only with caution. It is specifically included to assist some users "speed up" calculations while not overly compromising accuracy. The user must judge for him/herself whether the reduced run time is useful.

### Inputs

#### Field: Minimum System Timestep

Usually the minimum system timestep is allowed to vary from the zone timestep (as maximum) to a minimum timestep of 1 minute during certain system calculations. This might be when the system turns on or off, for example. Entering 0 in this field sets the minimum system timestep to be the same as the zone timestep. Otherwise the units of the field are minutes. It's probably a good idea to have any minimum entered be a divisor of the zone timestep.

#### Field: Maximum HVAC Iterations

The HVAC Manager will iterate to a solution or up to a set number of iterations. If not "converged", then a warning error appears:

~~~~~~~~~~~~~~~~~~~~

    SimHVAC: Maximum iterations (20) exceeded for all HVAC loops, at CHICAGO IL USA TMY2-94846 WMO#=725300, 10/07 14:06 - 14:08
~~~~~~~~~~~~~~~~~~~~

In order to reduce time used in simulating your building, you may choose to enter a lesser number than the default of 20 for the maximum number of iterations to be used. Or, you may wish to enter a bigger number for certain buildings. To get more information printed with a "max iteration" message, you need to enter a "[Output:Diagnostics](#outputdiagnostics), DisplayExtraWarnings;" command (which may also generate other warnings than just this one).

#### Field: Minimum Plant Iterations

The plant system modeling includes a solver that iterates within a single HVAC manager iteration.  This input field and the next one provide some control over how the plant solver iterates.  This field sets a minimum threshold for plant interations. The default for this field is the value "2" which indicates that a minimum of two full plant model interations will be performed every time plant is called by the HVAC manager.  For faster performance with simple plant systems, this input field could be set to the value "1".  For complicated plant systems that present difficulties to solve, this value may need to be set higher to ensure accuracy but at the expense of speed.  Complicated plant systems include those with several interconnected loops, sizing miss-matches such that plant components are starved of flow compared to their desired flow, heat recovery systems, and thermal load following onsite generators.

#### Field: Maximum Plant Iterations

The plant system solver iterates within a single HVAC manager iteration.  This input field and the previous one provide some control over how the plant model iterates.  This field sets a maximum limit for plant interations.  The default for this field is the value "8" which indicates that the plant solver will exit after having completed eight full iterations.  This value can be raised for better accuracy with complex plants or lowered for faster speed with simple plants. The output variable called "Plant Solver Sub Iteration Count" (typically reported at the "detailed" frequency) is useful for understanding how many plant solver iterations are actually being used during a particular simulation.  The lower limit of the value for this field is "2."

Use in an IDF:

~~~~~~~~~~~~~~~~~~~~

    ConvergenceLimits,
      0,        !- Minimum System Timestep (0=same as zone timestep)
      25,       !- Maximum HVAC Iterations
      3,        !- Minimum Plant Iterations
      9;        !- Maximum Plant Iterations
~~~~~~~~~~~~~~~~~~~~

## Building

The [Building](#building) object describes parameters that are used during the simulation of the building. There are necessary correlations between the entries for this object and some entries in the [Site:WeatherStation](#siteweatherstation) and [Site:HeightVariation](#siteheightvariation) objects, specifically the Terrain field.

### Inputs

#### Field: Building Name

[Building](#building) name is specified for output convenience.

#### Field: North Axis

The [Building](#building) North Axis is specified **relative to true North**. Buildings frequently do not line up with true north. For convenience, one may enter surfaces in a "regular" coordinate system and then shift them via the use of the North Axis. The value is specified in degrees from "true north" (clockwise is positive).

The figure below shows how the building north axis can be rotated to correspond with one of the major axes of an actual building. The relevance of this field is described more completely under "[GlobalGeometryRules](#globalgeometryrules)"; in particular, the value of "North Axis" is *ignored* if a coordinate system other than "relative" is used.

![Illustration of Building North Axis](media/illustration-of-building-north-axis.png)


#### Field: Terrain

The site's terrain affects how the wind hits the building – as does the building height. In addition, the external conduction method usually has its own parameters for the calculation. Please see the Engineering Documentation, External Conduction section for particulars. The legal values for this field are shown in the following table.

Table: Values for "Terrain"

Terrain Type Value|Terrain Description
------------------|-------------------
Country|Flat, Open Country
Suburbs|Rough, Wooded Country, Suburbs
City|Towns, city outskirts, center of large cities
Ocean|Ocean, Bayou flat country
Urban|Urban, Industrial, Forest

#### Warmup Convergence

The following two fields along with the minimum and maximum number of warmup days (also in this object) define the user specified criteria for when EnergyPlus will "converge" at each environment (each sizing period or run period set as Yes in the [SimulationControl](#simulationcontrol) object). EnergyPlus "runs" the first day of the environment (starting with a set of hard-coded initial conditions) until the loads/temperature convergence tolerance values are satisfied (next two fields) or until it reaches "maximum number of warmup days". Note that setting the convergence tolerance values too loose will cause the program to be satisifed too early and you may not get the results you expect from the actual simulation.

#### Field: Loads Convergence Tolerance Value

This value represents the number at which the loads values must agree before "convergence" is reached. Loads tolerance value is a fraction of the load.

#### Field: Temperature Convergence Tolerance Value

This value represents the number at which the zone temperatures must agree (from previous iteration) before "convergence" is reached. (Units for this field is delta C).

Convergence of the simultaneous heat balance/HVAC solution is reached when either the loads or temperature criterion is satisfied.

All tolerances have units so the temperature tolerance is in degrees C (or degrees K) and the loads tolerance is in Watts. Both tolerances work the same way, just one looks at temperatures and one looks at heating and cooling loads. After the second warm-up day, the program compares the maximum temperature experienced in a space with the maximum temperature from the previous day. If those two temperatures are within the tolerance, then it has passed the first warm-up check.

It does a similar comparison with lowest temperatures experience within all the zones. If the current simulation day and the previous day values are within the tolerance, then it has passed the second warm-up check. Similar things are done with the loads tolerance and the maximum heating and cooling loads that are experienced within the spaces. Those are compared individually to the values for the previous day. If they are both in tolerance, then the simulation has passed the third and fourth warm-up check. The simulation stays in the warm-up period until ALL FOUR checks have been passed. See Engineering Reference and Output Details document for further explanation and outputs.

Please note--other "convergence tolerance" inputs are required for certain HVAC equipment (unit ventilator, unit heater, window AC, etc.). The purpose and units of these parameters are different from "load convergence tolerance" and "temperature convergence tolerance" in the BUILDING object.

#### Field: Solar Distribution

Setting this value determines how EnergyPlus treats beam solar radiation and reflectances from exterior surfaces that strike the building and, ultimately, enter the zone. There are five choices: **MinimalShadowing**, **FullExterior** and **FullInteriorAndExterior, FullExteriorWithReflections, FullInteriorAndExteriorWithReflections**.

**MinimalShadowing**

In this case, there is no exterior shadowing except from window and door reveals. All beam solar radiation entering the zone is assumed to fall on the floor, where it is absorbed according to the floor's solar absorptance. Any reflected by the floor is added to the transmitted diffuse radiation, which is assumed to be uniformly distributed on all interior surfaces. If no floor is present in the zone, the incident beam solar radiation is absorbed on all interior surfaces according to their absorptances. The zone heat balance is then applied at each surface and on the zone's air with the absorbed radiation being treated as a flux on the surface.

**FullExterior, FullExteriorWithReflections**

In this case, shadow patterns on exterior surfaces caused by detached shading, wings, overhangs, and exterior surfaces of all zones are computed. As for MinimalShadowing, shadowing by window and door reveals is also calculated. Beam solar radiation entering the zone is treated as for MinimalShadowing -- All beam solar radiation entering the zone is assumed to fall on the floor, where it is absorbed according to the floor's solar absorptance. Any reflected by the floor is added to the transmitted diffuse radiation, which is assumed to be uniformly distributed on all interior surfaces. If no floor is present in the zone, the incident beam solar radiation is absorbed on all interior surfaces according to their absorptances. The zone heat balance is then applied at each surface and on the zone's air with the absorbed radiation being treated as a flux on the surface.

**FullInteriorAndExterior, FullInteriorAndExteriorWithReflections**

This is the same as FullExterior except that instead of assuming all transmitted beam solar falls on the floor the program calculates the amount of beam radiation falling on each surface in the zone, including floor, walls and windows, by projecting the sun's rays through the exterior windows, taking into account the effect of exterior shadowing surfaces and window shading devices.

If this option is used, you should be sure that the surfaces of the zone totally enclose a space. This can be determined by viewing the **eplusout.dxf** file with a program like AutoDesk's Volo View Express. You should also be sure that the zone is **convex**. Examples of convex and non-convex zones are shown in Figure 2. The most common non-convex zone is an L-shaped zone. (A formal definition of convex is that any straight line passing through the zone intercepts at most two surfaces.)  If the zone's surfaces do not enclose a space or if the zone is not convex you should use Solar Distribution = **FullExterior** instead of **FullInteriorAndExterior**.

If you use **FullInteriorAndExterior** the program will also calculate how much beam radiation falling on the inside of an exterior window (from other windows in the zone) is absorbed by the window, how much is reflected back into the zone, and how much is transmitted to the outside. In this calculation the effect of a shading device, if present, is accounted for.

![Illustration of Convex and Non-convex Zones](media/illustration-of-convex-and-non-convex-zones.png)


**Reflection calculations**

> Note: Using the reflection calculations can be very time-consuming. Even error-prone. As a possible alleviation, you can use the Output:Diagnostics,DoNotMirrorDetachedShading; in many cases to get past a fatal error.

If using reflections, the program calculates beam and sky solar radiation that is reflected from exterior surfaces and then strikes the building. These reflecting surfaces fall into three categories:

#. **Shadowing surfaces**. These are surfaces like overhangs or neighboring buildings entered with Shading:Site, [Shading:Building](#shadingsite-shadingbuilding), Shading:Site:Detailed, [Shading:Building:Detailed](#shadingsitedetailed-shadingbuildingdetailed), [Shading:Overhang](#shadingoverhang), [Shading:Overhang:Projection](#shadingoverhangprojection), [Shading:Fin](#shadingfin), [Shading:Fin:Projection](#shadingfinprojection) or [Shading:Zone:Detailed](#shadingzonedetailed) objects. See Figure 3.
#. These surfaces can have diffuse and/or specular (beam-to-beam)  reflectance values that are specified with the [ShadingProperty:Reflectance](#shadingpropertyreflectance) object which specifies those parameters. They have a default value of .2 for both visible and diffuse reflection.
#. **Exterior building surfaces**. In this case one section of the building reflects solar radiation onto another section (and vice-versa). See Figure 4.
#. The building surfaces are assumed to be diffusely reflecting if they are opaque (walls, for example) and specularly reflecting if they are windows or glass doors. The reflectance values for opaque surfaces are calculated by the program from the Solar Absorptance and Visible Absorptance values of the outer material layer of the surface's construction (ref: [Material](#material-and-material-properties) object properties). The reflectance values for windows and glass doors are calculated by the program from the reflectance properties of the individual glass layers that make up surface's construction assuming no shading device is present and taking into account inter-reflections among the layers (ref: [Window](#window) Properties).
#. **The ground surface**. Reflection from the ground is calculated even if reflections option is not used;l but then the ground plane is considered unobstructed, i.e., the shadowing of the ground by the building itself or by obstructions such as neighboring buildings is ignored. Shadowing by the building itself or neighboring buildings is taken into account  when the "with reflections" option is used but then the "view factor to ground" is NOT used. This is shown in Figure 5.

![Solar reflection from shadowing surfaces. Solid arrows are beam solar radiation; dashed arrows are diffuse solar radiation. (a) Diffuse reflection of beam solar radiation from the top of an overhang. (b) Diffuse reflection of sky solar radiation from the top of an overhang. (c) Beam-to-beam (specular) reflection from the façade of an adjacent highly-glazed building represented by a vertical shadowing surface.](media/solar-reflection-from-shadowing-surfaces..png)


![Solar reflection from building surfaces onto other building surfaces. In this example beam solar reflects from a vertical section of the building onto a roof section. The reflection from the window is specular. The reflection from the wall is diffuse.](media/solar-reflection-from-building-surfaces-onto.png)


![Shadowing from building affects beam solar reflection from the ground. Beam-to-diffuse reflection from the ground onto the building occurs only for sunlit areas, A and C, not from shaded area, B.](media/shadowing-from-building-affects-beam-solar.png)


#### Field: Maximum Number of Warmup Days

This field specifies the number of "warmup" days that might be used in the simulation before "convergence" is achieved. The default number, 25, is usually more than sufficient for this task; however, some complex buildings (with complex constructions) may require more days. If you enter less than 25 as a maximum, that is the number of maximum warmup days that will be used. An error message will occur when the simulation "runs" out of days and has not converged:

~~~~~~~~~~~~~~~~~~~~

    CheckWarmupConvergence: Loads Initialization, Zone="MAIN ZONE" did not converge after 30 warmup days.
    See Warmup Convergence Information in .eio file for details
    ..Environment(SizingPeriod)="DENVER CENTENNIAL  GOLDEN   N ANN CLG 1% CONDNS DB=>MWB"
    ..Max Temp Comparison = 2.06E-002 vs Temperature Convergence Tolerance=0.50 – Pass Convergence
    ..Min Temp Comparison = 5.95E-003 vs Temperature Convergence Tolerance=0.50 – Pass Convergence
    ..Max Cool Load Comparison = 9.5082E-002 vs Loads Convergence Tolerance=5.00E-002 – Fail Convergence
~~~~~~~~~~~~~~~~~~~~

As noted in the message, there will be more information in the .eio file. (Refer to Output Details document as well for examples.)

You may be able to increase the Maximum Number of Warmup Days and get convergence, but some anomalous buildings may still not converge. Simulation proceeds for x warmup days until "convergence" is reached (see the discussion under the Temperature Convergence Tolerance Value field in this object, just above).

#### Field: Minimum Number of Warmup Days

This field specifies the minimum number of "warmup" days before EnergyPlus will check if it has achieved convergence and can thus start simulating the particular environment (design day, annual run) in question. Research into the minimum number of warmup days indicates that 6 warmup days is generally enough on the minimum end of the spectrum to avoid false predictions of convergence and thus to produce enough temperature and flux history to start EnergyPlus simulation. This was based on a study that used the benchmark reference buildings. It also was observed that convergence performance improved when the number of warmup days increased. As a result, the default value for the minimum warmup days has been set to 6. Users should decrease this number only if they have knowledge that a specific file converges more quickly than 6 days. Users may wish to increase the value in certain situations when, based on the output variables described in the Output Details document, it is determined that EnergyPlus has not converged. While this parameter should be less than the previous parameter, a value greater than the value entered in the field "Maximum Number of Warmup Days" above may be used when users wish to increase warmup days more than the previous field. In this particular case, the previous field will be automatically reset to the value entered in this field and EnergyPlus will run exactly the number of warmup days specified in this field.

An example from an IDF:

~~~~~~~~~~~~~~~~~~~~

    Building,
      PSI HOUSE DORM AND OFFICES,  !- Name
      36.87000,               !- North Axis {deg}
      Suburbs,                !- Terrain
      0.04,                   !- Loads Convergence Tolerance Value
      0.4000000,              !- Temperature Convergence Tolerance Value {deltaC}
      FullInteriorAndExterior, !- Solar Distribution
      40,                     !- Maximum Number of Warmup Days
      6;                      !- Minimum Number of Warmup Days
~~~~~~~~~~~~~~~~~~~~

## SurfaceConvectionAlgorithm:Inside

This input object is used control the choice of models used for surface convection at the inside face of all the heat transfer surfaces in the model.  This object sets the selection for convection correlations in a global way. The [Zone](#zone) Inside Convection Algorithm input field in the [Zone](#zone) object may be used to selectively override this value on a zone-by-zone basis. Further, individual surfaces can refine the choice by each surface or surface lists – see  object [SurfaceProperty:ConvectionCoefficients](#surfacepropertyconvectioncoefficients) and object [SurfaceProperty:ConvectionCoefficients:MultipleSurface](#surfacepropertyconvectioncoefficientsmultiplesurface).

### Inputs

#### Field: Algorithm

The model specified in this field is the default algorithm for the inside face all the surfaces.. The key choices are **Simple**, **TARP**, **CeilingDiffuser**, and **AdaptiveConvectionAlgorithm**.

The **Simple** model applies constant heat transfer coefficients depending on the surface orientation.

The **TARP** model correlates the heat transfer coefficient to the temperature difference for various orientations. This model is based on flat plate experiments.

The **CeilingDiffuser** model is a mixed and forced convection model for ceiling diffuser configurations. The model correlates the heat transfer coefficient to the air change rate for ceilings, walls and floors. These correlations are based on experiments performed in an isothermal room with a cold ceiling jet. To avoid discontinuities in surface heat transfer rate calculations, all of correlations have been extrapolated beyond the lower limit of the data set (3 ACH) to a natural convection limit that is applied during the hours when the system is off.

The **AdaptiveConvectionAlgorithm** model is an dynamic algorithm that organizes a large number of different convection models and automatically selects the one that best applies.  The adaptive convection algorithm can also be customized using the [SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections](#surfaceconvectionalgorithminsideadaptivemodelselections) input object.  These models are explained in detail in the EnergyPlus Engineering Reference Document.

The default is **TARP**.

IDF Example:

~~~~~~~~~~~~~~~~~~~~

    SurfaceConvectionAlgorithm:Inside,TARP;
~~~~~~~~~~~~~~~~~~~~

## SurfaceConvectionAlgorithm:Outside

Various exterior convection models may be selected for global use.  The optional [Zone](#zone) Outside Convection Algorithm input field in the [Zone](#zone) object may be used to selectively override this value on a zone-by-zone basis. Further, individual surfaces can refine the choice by each surface or surface lists – see  object [SurfaceProperty:ConvectionCoefficients](#surfacepropertyconvectioncoefficients) and object [SurfaceProperty:ConvectionCoefficients:MultipleSurface](#surfacepropertyconvectioncoefficientsmultiplesurface).

### Inputs

#### Field: Algorithm

The available key choices are **SimpleCombined**, **TARP**, **MoWiTT**, **DOE-2**, and **AdaptiveConvectionAlgorithm**.

The **Simple** convection model applies heat transfer coefficients depending on the roughness and windspeed. This is a combined heat transfer coefficient that includes radiation to sky, ground, and air. The correlation is based on Figure 1, Page 25.1 (Thermal and Water Vapor Transmission Data), 2001 ASHRAE Handbook of Fundamentals. Note that if **Simple** is chosen here or in the [Zone](#zone) field and a [SurfaceProperty:ConvectionCoefficients](#surfacepropertyconvectioncoefficients) object attempts to override the caulcation with a different choice, the action will still be one of combined calculation. To change this, you must select one of the other methods for the global default.

All other convection models apply heat transfer coefficients depending on the roughness, windspeed, and terrain of the building's location. These are *convection only* heat transfer coefficients; radiation heat transfer coefficients are calculated automatically by the program.

The **TARP** algorithm was developed for the TARP software and combines natural and wind-driven convection correlations from laboratory measurements on flat plates.

The **DOE-2** and **MoWiTT** were derived from field measurements. DOE-2 uses a correlation from measurements by Klems and Yazdanian for rough surfaces. MoWitt uses a correlation from measurements by Klems and Yazdanian for smooth surfaces and, therefore, is most appropriate for windows (see [SurfaceProperty:ConvectionCoefficients:MultipleSurface](#surfacepropertyconvectioncoefficientsmultiplesurface) for how to apply to only windows).

The **AdaptiveConvectionAlgorithm** model is an dynamic algorithm that organizes a large number of different convection models and automatically selects the one that best applies.  The adaptive convection algorithm can also be customized using the [SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections](#surfaceconvectionalgorithmoutsideadaptivemodelselections) input object.  All algorithms are described more fully in the Engineering Reference.

The default is **DOE-2**.

> Note that when the surface is wet (i.e. it is raining and the surface is exposed to wind) then the convection coefficient appears as a very large number (1000) and the surface is exposed to the Outdoor Wet-bulb Temperature rather than the Outdoor Dry-bulb Temperature.

IDF Example:

~~~~~~~~~~~~~~~~~~~~

    SurfaceConvectionAlgorithm:Outside, AdaptiveConvectionAlgorithm;
~~~~~~~~~~~~~~~~~~~~

## HeatBalanceAlgorithm

The [HeatBalanceAlgorithm](#heatbalancealgorithm) object provides a way to select what type of heat and moisture transfer algorithm will be used for calculating the performance of the building's surface assemblies.  This input controls the overall algorithm used for all the surfaces unless one or more of the SurfaceProperty:HeatTransferAlgorithm:\* objects are used to alter the selection for particular surfaces.

### Inputs

#### Field: Algorithm

Four values are allowed to select which solution will be used.

- The **ConductionTransferFunction** selection is a sensible heat only solution and does not take into account moisture storage or diffusion in the construction elements.
- The **MoisturePenetrationDepthConductionTransferFunction** selection is a sensible heat diffusion and an inside surface moisture storage algorithm that also needs additional moisture material property information. Sometimes, this is referred to as the Effective Moisture Penetration Depth or EMPD. See the moisture material property object for additional information and description of outputs:

- MaterialProperty:MoisturePenetrationDepth:Settings

- **Advanced/Research usage:** The **ConductionFiniteDifference** selection is a sensible heat only solution and does not take into account moisture storage or diffusion in the construction elements. This solution technique uses a 1-D finite difference solution in the construction elements. Outputs for the surfaces are described with the material property objects. The Conduction Finite Difference (aka CondFD) property objects are:

- MaterialProperty:PhaseChange
- MaterialProperty:VariableThermalConductivity

- **Advanced/Research usage:** The **CombinedHeatAndMoistureFiniteElement** is a coupled heat and moisture transfer and storage solution. The solution technique uses a one dimensional finite difference solution in the construction elements and requires further material properties described in the Heat and Moisture Transfer material properties objects. Outputs from the algorithm are described with these objects. The Heat and Moisture Transfer property objects are:

- MaterialProperty:HeatAndMoistureTransfer:Settings
- MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm
- MaterialProperty:HeatAndMoistureTransfer:Suction
- MaterialProperty:HeatAndMoistureTransfer:Redistribution
- MaterialProperty:HeatAndMoistureTransfer:Diffusion
- MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity

#### Field: Surface Temperature Upper Limit

This field is a bit "advanced".  It should only be used when the simulation fails AND you cannot determine a cause for the failure.  That is, you receive an error similar to:

~~~~~~~~~~~~~~~~~~~~

       ** Severe  ** Temperature out of bounds (202.91) for surface=Wall1
       **   ~~~   ** in Zone=Zone01
       **   ~~~   **  Occurrence info=NEW YORK CITY NY SUMMER, 07/21 16:00 - 16:01
       **   ~~~   ** A temperature out of bounds problem can be caused by several things.  The user
       **   ~~~   ** should check the weather environment, the level of internal gains with respect
       **   ~~~   ** to the zone, and the thermal properties of their materials among other things.
       **   ~~~   ** A common cause is a building with no thermal mass -- all materials with
       **   ~~~   ** Regular-R definitions.
~~~~~~~~~~~~~~~~~~~~

And, after careful perusal, you cannot find a solution as suggested in the error description. You may then want to enter a higher number than the default for this field.

#### Field: Minimum Surface Convection Heat Transfer Coefficient Value

This optional field is used to set an overall minimum for the value of the coefficient for surface convection heat transfer (Hc) in W/m2-K.  A minimum is necessary for numerical robustness because some correlations for Hc can result in zero values and create numerical problems.   This field can be used to support specialized validation testing to suppress convection heat transfer and to investigate the implications of different minimum Hc values. The default is 0.1.

#### Field: Maximum Surface Convection Heat Transfer Coefficient Value

This optional field is used to set an overall maximum for the value of the coefficient for surface convection heat transfer (Hc) in W/m2-K.   High Hc values are used in EnergyPlus to approximate fixed surface temperature boundary conditions. This field can be used to alter the accepted range of user-defined Hc values.

And, a default IDF example

~~~~~~~~~~~~~~~~~~~~

    HeatBalanceAlgorithm,ConductionTransferFunction;   ! Solution Algorithm
~~~~~~~~~~~~~~~~~~~~

## HeatBalanceSettings:ConductionFiniteDifference

This object is used to control the behavior of the Conduction Finite Difference algorithm for surface heat transfer.  The settings are global and affect how the model behaves for all the surfaces.

### Inputs

#### Field: Difference Scheme

This field determines the solution scheme used by the Conduction Finite Difference model.  There are two options CrankNicholsonSecondOrder and FullyImplicitFirstOrder.  The CrankNicholsonSecondOrder scheme is second order in time and may be faster.  But it can be unstable over time when boundary conditions change abruptly and severely.  The FullyImplicitFirstOrder scheme is first order in time and is more stable over time. But it may be slower. The default is FullyImplicitFirstOrder when ConductionFiniteDifference is selected as the Heat Balance Algorithm.

#### Field: Space Discretization Constant

This field controls the how the model determines spatial discretization, or the count of nodes across each material layer in the construction.  The model calculates the nominal distance associated with a node, ![](media/image6.png) , using

![](media/image7.png)\


Where

![](media/image8.png) is the thermal diffusivity of the material layer, in m^2^/s

![](media/image9.png) is the length of the timestep in seconds.

*C* is a constant set by this field.

The default is 3.  Typical values are from 1 to 3.  Lower values for this constant lead to more nodes and finer-grained space discretization.

#### Field: Relaxation Factor

The finite difference solver includes under-relaxation for improved stability for interactions with the other surfaces. This input field can optionally be used to modify the starting value for the relaxation factor.  Larger numbers may solve faster, while smaller numbers may be more stable.  The default is 1.0.  If the program detects numerical instability, it may reduce the value entered here to something lower and more stable.

#### Field: Inside Face Surface Temperature Convergence Criteria

The surface heat balance model at the inside face has a numerical solver that uses a convergence parameter for a maximum allowable differences in surface temperature.  This field can optionally be used to modify this convergence criteria.  The default value is 0.002 and was selected for stability.  Lower values may further increase stability at the expense of longer runtimes, while higher values may decrease runtimes but lead to possible instabilities.  The units are in degrees Celsius.

An example IDF object follows.

~~~~~~~~~~~~~~~~~~~~

      HeatBalanceSettings:ConductionFiniteDifference,
        FullyImplicitFirstOrder , !- Difference Scheme
        3.0 ,  !- Space Discretization Constant
        1.0,   !- Relaxation Factor
        0.002 ;!- Inside Face Surface Temperature Convergence Criteria
~~~~~~~~~~~~~~~~~~~~

## ZoneAirHeatBalanceAlgorithm 

The [ZoneAirHeatBalanceAlgorithm](#zoneairheatbalancealgorithm) object provides a way to select what type of solution algorithm will be used to calculate zone air temperatures and humidity ratios. This object is an optional object. If the default algorithm is used, this object is not required in an input file.

### Inputs

#### Field: Algorithm

Three choices are allowed to select which solution algorithm will be used. The **ThirdOrderBackwardDifference** selection is the default selection and uses the third order finite difference approximation to solve the zone air energy and moisture balance equations. The **AnalyticalSolution** selection uses the integration approach to solve the zone air energy and moisture balance equations. The **EulerMethod** selection uses the first order finite backward difference approximation to solve the zone air energy and moisture balance equations.

And, a default IDF example is shown below:

~~~~~~~~~~~~~~~~~~~~

    ZoneAirHeatBalanceAlgorithm, ThirdOrderBackwardDifference;   !- Algorithm
~~~~~~~~~~~~~~~~~~~~

## ZoneAirContaminantBalance

The [ZoneAirContaminantBalance](#zoneaircontaminantbalance) object provides a way to select which contaminant type will be simulated. Although carbon dioxide is not considered as an indoor contaminant but it is used as an indicator of indoor air quality in buildings. From modeling point of view EnergyPlus treats carbon dioxide as a type of contaminant. In addition to carbon dioxide, a generic contaminant type model was also added. This object is optional, only required in the input data file if the user wishes to model contaminant concentration levels as part of their simulation.

### Inputs

#### Field: Carbon Dioxide Concentration

Input is Yes or No. The default is No. If Yes, simulation of carbon dioxide concentration levels will be performed. If No, simulation of carbon dioxide concentration levels will not be performed.

#### Field: Outdoor Carbon Dioxide Schedule Name

This field specifies the name of a schedule that contains outdoor air carbon dioxide level values in units of ppm. One source of monthly average CO~2~ levels in the atmosphere is available at a NOAA ftpsite as ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_mm_mlo.txt.

#### Field: Generic Contaminant Concentration

Input is Yes or No. The default is No. If Yes, simulation of generic contaminant concentration levels will be performed. If No, simulation of generic contaminant concentration levels will not be performed.

#### Field: Outdoor Generic Contaminant Schedule Name

This field specifies the name of a schedule that contains outdoor air generic contaminant level values in units of ppm.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

    ZoneAirContaminantBalance,
      Yes,                       !- Carbon Dioxide Concentration
      Outdoor CO2 Schedule,      !- Outdoor Carbon Dioxide Schedule Name
      Yes,                       !- Generic Contaminant Concentration
      Generic Contaminant Schedule; !- Outdoor Generic Contaminant Schedule Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

The following output variables are available when Carbon Dioxide Concentration = Yes.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Zone Air CO2 Internal Gain Volume Flow Rate
    HVAC,Average,Zone Air CO2 Concentration [ppm]
~~~~~~~~~~~~~~~~~~~~

#### Zone Air CO2 Concentration [ppm]

This output variable represents the carbon dioxide concentration level in parts per million (ppm) for each zone. This is calculated and reported from the Correct step in the [Zone](#zone) Air Contaminant Predictor-Corrector module.

#### Zone Air CO2 Internal Gain Volume Flow Rate [m3/s]

This is the rate of carbon dioxide added to a zone from all types of sources or sinks, in m^3^/s.

### Outputs

The following output variable is available when Generic Contaminant Concentration = Yes.

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Zone Generic Air Contaminant Generation Volume Flow Rate [m3/s]
    HVAC,Average,Zone Air Generic Air Contaminant Concentration [ppm]
~~~~~~~~~~~~~~~~~~~~

#### Zone Air Generic Air Contaminant Concentration [ppm]

This output variable represents the generic contaminant concentration level in parts per million (ppm) for each zone. This is calculated and reported from the Correct step in the [Zone](#zone) Air Contaminant Predictor-Corrector module.

#### Zone Generic Air Contaminant Generation Volume Flow Rate [m3/s]

This is the rate of generic air contaminant added (or subtracted) to a zone from all types of sources or sinks.

## ShadowCalculation

This object is used to control some details of EnergyPlus's solar, shadowing and daylighting models. There are two basic methods available for the calculations. In order to speed up the calculations, shadowing calculations (sun position, etc.) for the default method are performed over a period of days. Note that this value may be very important for determining the amount of sun entering your building and by inference the amount of cooling or heating load needed for maintaining the building. Though termed "shadowing" calculations, it in affect determines the sun position for a particular day in a weather file period simulation. (Each design day will use the date of the design day object). Even though weather file data contains the amount of solar radiation, the internal calculation of sun position will govern how that affects various parts of the building. By default, the calculations are done for every 20 days throughout a weather run period; an average solar position is chosen and the solar factors (such as sunlit areas of surfaces) remain the same for that number of days. When more integrated calculations are needed for controlling dynamic windows or shades, a secod method is available where solar calculations are performed at each zone timestep.

### Inputs

#### Field: Calculation Method

This field is used to control how the solar, shading, and daylighting models are calculated with respect to the time of calculations during the simulation.  The default and fastest method is selected using the keyword AverageOverDaysInFrequency.  A more detailed and slower method can be selected using the keyword DetailedTimestepIntegration.  The later method is useful modeling dynamic fenestration and shading surfaces.

#### Field: Calculation Frequency

This numeric field will cause the shadowing calculations to be done periodically using the number in the field as the number of days in each period. This field is only used if the default method AverageOverDaysInFrequency is used in the previous field. Using this field will allow you to synchronize the shadowing calculations with changes in shading devices. Using the default of 20 days in each period is the average number of days between significant changes in solar position angles. For these shadowing calculations, an "average" (over the time period) of solar angles, position, equation of time are also used.

#### Field: Maximum Figures in Shadow Overlap Calculations

This numeric field will allow you to increase the number of figures in shadow overlaps. Due to the shadowing algorithm, the number of shadows in a figure may grow quite large even with fairly reasonable looking structures. Of course, the inclusion of more allowed figures will increase calculation time.  Likewise, too few figures may not result in as accurate calculations as you desire.

#### Field: Polygon Clipping Algorithm

This is an advanced feature. Prior to V7, the internal polygon clipping method was a special case of the Weiler-Atherton method. Now, two options are available: **SutherlandHodgman** (default) and **ConvexWeilerAtherton**. Theoretically, Sutherland-Hodgman is a simpler algorithm but it works well in cases where receiving surfaces (of shadows) are non-convex. The Weiler-Atherton implementation is only accurate where both casting and receiving surfaces are convex. Warnings/severe errors are displayed when necessary. More details on polygon clipping are contained in the Engineering Reference.

#### Field: Sky Diffuse Modeling Algorithm

Two choices are available here: SimpleSkyDiffuseModeling and DetailedSkyDiffuseModeling. **SimpleSkyDiffuseModeling** (default) performs a one-time calculation for sky diffuse properties. This has implications if you have shadowing surfaces with changing transmittance (i.e. not all opaque or not all transparent) during the year. The program checks to see if this might be the case and automatically selects **DetailedSkyDiffuseModeling** if the shading transmittance varies. Even if the transmittance doesn't vary and the option for detailed modeling is used, that option is retained (though it will increase execution time) because you may be using EMS to vary the transmittance. When the detailed modeling is done, there will be a warning posted if the Calculation Frequency (above) is > 1.

In general (and you should also read the previous field description), if shadowing surfaces are used with the transmittance property, the user should be careful to synchronize this calculation with the scheduled occurrence of the transmittance (if any) (or use 1, which will be the most accurate but will cause more time in the calculations).

This field applies to the method called "AverageOverDaysInFrequency."  When the method called "DetailedTimestepIntegration" is used the diffuse sky modeling always uses DetailedSkyDiffuseModeling.

Examples of this object in IDF: (note this object must be unique in an IDF)

~~~~~~~~~~~~~~~~~~~~

    ShadowCalculation,AverageOverDaysInFrequency,1;
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    ShadowCalculation, AverageOverDaysInFrequency, 1, , SutherlandHodgman, DetailedSkyDiffuseModeling;
~~~~~~~~~~~~~~~~~~~~

Note that the use of "1" in the examples is NOT the same as using DetailedTimestepIntegration – "1" causes daily calculation of the sun position variables but does not change the shadowing calculations more frequently than daily.

## Output:Diagnostics

Sometimes, messages only confuse users – especially new users. Likewise, sometimes certain output variables exist for only a certain condition but some take them at face value/name. Some features may be very important but under certain instances cause problems. Thus, we have added the **diagnostic output** object to be able to turn on or off certain messages, variables, and features depending on conditions.

Both fields of the [Output:Diagnostics](#outputdiagnostics) command can accept all the applicable keys. More than one object may be entered.

### Inputs

#### Field: key1, key2

Allowable choices are:

**DisplayAllWarnings** – use this to get all warnings (except the developer warnings "DisplayZoneAirHeatBalanceOffBalance").  This key sets all other display warning values to on.

**DisplayExtraWarnings** – use this to get all extra warnings. An example of an extra warning is when a user enters a ceiling height or volume with the [Zone](#zone) object and EnergyPlus calculates something significantly different based on the entered zone geometry.

**DisplayUnusedSchedules** – use this to have the unused schedules (by name) listed at the end of the simulation.

**DisplayUnusedObjects** – use this to have unused (orphan) objects (by name) listed at the end of the simulation.

**DisplayAdvancedReportVariables** – use this to be able to use certain advanced output variables where the name may be misleading and you need to understand the concepts or reasons for use. If you put in this field, then you will be able to report on these features. They are noted in the descriptions of objects or output variables.

**DisplayZoneAirHeatBalanceOffBalance** – this is a developer diagnostic which you can turn on, if you desire.

**DoNotMirrorDetachedShading** – use this to turn off the automatic mirroring of detached shading surfaces. These surfaces are automatically mirrored so that the user does not need to worry about facing direction of the surface and the shading surface will shade the building as appropriate. Note that [Shading:Zone:Detailed](#shadingzonedetailed) surfaces are also mirrored and there is no way to turn that "off".

**DisplayWeatherMissingDataWarnings** – use this to turn on the missing data warnings from the read of the weather file.

**ReportDuringWarmup** – use this to allow reporting during warmup days. This can show you exactly how your facility is converging (or not) during the initial "warmup" days of the simulation.  Generally, only developers or expert simulation users would need this kind of detail.

**ReportDetailedWarmupConvergence** – use this to produce detailed reporting (essentially each warmup day for each zone) for warmup convergence.

In IDF use:

~~~~~~~~~~~~~~~~~~~~

    Output:Diagnostics,
      DisplayExtraWarnings;
~~~~~~~~~~~~~~~~~~~~

## Output:DebuggingData

There may be times when a particular input file requires additional debugging. The [Output:DebuggingData](#outputdebuggingdata) object may be used to report all available node data (e.g., temperature, mass flow rate, set point, pressure, etc.). The debug data is reported to the DBG text file. The debug file first reports the node number and name, and then all available node information for each zone time step (Ref. [Timestep](#timestep)).

The 2 fields of the [Output:DebuggingData](#outputdebuggingdata) object can accept either a 1 (turn on) or any other value (turn off). Only one object may be entered.

### Inputs

#### Field: Report Debugging Data

This field turns on debug reporting when a value of 1 is entered. Any other value (usually 0) disables debug reporting.

#### Field: Report During Warmup

This field allows the debug data to be reported during the warmup period. When a value of 1 is entered the data is reported at all times, even during warmup. Any other value (usually 0) disables "reporting at all time" and debug data is only reported for each environment ([RunPeriod](#runperiod) or [SizingPeriod:DesignDay](#sizingperioddesignday)).

In IDF use:

~~~~~~~~~~~~~~~~~~~~

    Output:DebuggingData,
    1,1;
~~~~~~~~~~~~~~~~~~~~

## Output:PreprocessorMessage

The [Output:PreprocessorMessage](#outputpreprocessormessage) object can be used by preprocessor programs to EnergyPlus for passing certain conditions/errors that might not be detected by scripts executing the EnergyPlus system of programs. This allows EnergyPlus to intercept problems and terminate gracefully rather than the user having to track down the exact conditions.

There is no reason for a user to enter an [Output:PreprocessorMessage](#outputpreprocessormessage) object but you should encourage interface developers to use this feature. More than one [Output:PreprocessorMessage](#outputpreprocessormessage) objects may be entered. Of course, no preprocessor message objects are necessary if there is no error information to be passed.

### Inputs

#### Field: Preprocessor Name

The preprocessor name (e.g. EPMacro, ExpandObjects) is entered here.  Case is retained so that messages from EnergyPlus look very similar to what a preprocessor would produce.

#### Field: Error Severity

This is the error severity. If Fatal, EnergyPlus will terminate after showing all preprocessor messages.

#### Fields: Message Line 1 through Message Line 10

Each line is limited to 100 characters and an appropriate message can be composed.

An IDF Example:

~~~~~~~~~~~~~~~~~~~~

    Output:PreprocessorMessage,
       No Preprocessor Used,     !- preprocessor name
       Information,              !- error severity
       Illustrative Message,     !- message line 1
       No problems for processing;  !- message line 2
~~~~~~~~~~~~~~~~~~~~

And would appear in output:

~~~~~~~~~~~~~~~~~~~~

    Preprocessor="No Preprocessor Used" has the following Information messages:
    Illustrative Message
    No problems for processing
~~~~~~~~~~~~~~~~~~~~

## ZoneCapacitanceMultiplier:ResearchSpecial

This object is an advanced feature that can be used to control the effective storage capacity of the zone.  Capacitance multipliers of 1.0 indicate the capacitance is that of the (moist) air in the volume of the specified zone. This multiplier can be increased if the zone air capacitance needs to be increased for stability of the simulation or to allow modeling higher or lower levels of damping of behavior over time. The multipliers are applied to the base value corresponding to the total capacitance for the zone's volume of air at current zone (moist) conditions.

### Inputs

#### Field: Sensible Heat Capacity Multiplier

This field is used to alter the effective heat capacitance of the zone air volume. This affects the transient calculations of zone air temperature. Values greater than 1.0 have the effect of smoothing or damping the rate of change in the temperature of zone air from timestep to timestep.  Note that sensible heat capacity can also be modeled using internal mass surfaces.

#### Field: Humidity Capacity Multiplier

This field is used to alter the effective moisture capacitance of the zone air volume.  This affects the transient calculations of zone air humidity ratio.  Values greater than 1.0 have the effect of smoothing, or damping, the rate of change in the water content of zone air from timestep to timestep.

#### Field: Carbon Dioxide Capacity Multiplier 

This field is used to alter the effective carbon dioxide capacitance of the zone air volume. This affects the transient calculations of zone air carbon dioxide concentration.  Values greater than 1.0 have the effect of smoothing or damping the rate of change in the carbon dioxide level of zone air from timestep to timestep.

#### Field: Generic Contaminant Capacity Multiplier 

This field is used to alter the effective generic contaminant capacitance of the zone air volume. This affects the transient calculations of zone air generic contaminant concentration.  Values greater than 1.0 have the effect of smoothing or damping the rate of change in the generic contaminant level of zone air from timestep to timestep.

## SimulationControl

The input for [SimulationControl](#simulationcontrol) allows the user to specify what kind of calculations a given EnergyPlus simulation will perform. For instance the user may want to perform one or more of the sizing calculations but not proceed to an annual weather file simulation. Or the user might have all flow rates and equipment sizes already specified and  desire an annual weather without any preceding sizing calculations. Sizing runs, even for large projects, are quickly run – they do not add much to the overall simulation time. The [SimulationControl](#simulationcontrol) input allows all permutations of run selection by means of 5 yes/no inputs.

> Only one [SimulationControl](#simulationcontrol) object is permitted for each EnergyPlus input file. While a [SimulationControl](#simulationcontrol) is needed to trigger sizing calculations, it is optional for other runs (design days, run periods). The actions will still be shown in the eplusout.eio file (see Output Details and Examples Document).

### Inputs

#### Field: Do Zone Sizing Calculation

Input is Yes or No. The default is No. [Zone](#zone) Sizing (see Sizing:[Zone](#zone) object) performs a special calculation, using a theoretical ideal zonal system, and determines the zone design heating and cooling flow rates and loads, saving the results in the zone sizing arrays.

#### Field: Do System Sizing Calculation

Input is Yes or No. The default is No. System Sizing (see [Sizing:System](#sizingsystem) object) also performs a special calculation that, to oversimplify, sums up the results of the zone sizing calculation and saves the results in the system sizing arrays for reporting on component size requirements. Thus, in order to perform the system sizing calculations, the zone sizing arrays need to be filled and hence the zone sizing calculations must be performed in the same run. (This requirement is enforced by the program).

#### Field: Do Plant Sizing Calculation

Input is Yes or No. The default is No. Unlike [Zone](#zone) and System Sizing, Plant Sizing does not use the [Zone](#zone) or System sizing arrays. Plant Sizing uses the [Sizing:Plant](#sizingplant) object fields and data on the maximum component flow rates. The data on component (such as coil) flow rates is saved and made available to the Plant code whether or not component autosizing is performed and whether or not zone sizing and/or system sizing is performed. Therefore, you can specify Plant Sizing without also specifying to do [Zone](#zone) Sizing or System Sizing calculations.

#### Field: Run Simulation for Sizing Periods

Input is Yes or No. The default is Yes. Yes implies that the simulation will be run on all the included SizingPeriod objects (i.e., [SizingPeriod:DesignDay](#sizingperioddesignday), [SizingPeriod:WeatherFileDays](#sizingperiodweatherfiledays), and [SizingPeriod:WeatherFileConditionType](#sizingperiodweatherfileconditiontype)). Note that each SizingPeriod object constitutes an "environment" and warmup convergence (see earlier topic under the [Building](#building) object) will occur for each.

#### Field: Run Simulation for Weather File Run Periods

Input is Yes or No. The default is Yes. Yes implies the simulation will be run on all the included [RunPeriod](#runperiod) objects. Note that each [RunPeriod](#runperiod) object constitutes an "environment" and warmup convergence (see earlier topic under the [Building](#building) object) will occur for each.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

      SimulationControl,
        No,                      !- Do Zone Sizing Calculation
        No,                      !- Do System Sizing Calculation
        No,                      !- Do Plant Sizing Calculation
        Yes,                     !- Run Simulation for Sizing Periods
        Yes;                     !- Run Simulation for Weather File Run Periods
~~~~~~~~~~~~~~~~~~~~

## ProgramControl

The [ProgramControl](#programcontrol) object can be used to control how the EnergyPlus program executes on the computer.  Portions of EnergyPlus have been programmed to use more than one processor, or CPU core, at the same time during a single simulation.  This multithreading may not be desireable when running more than one simulation at a time on the same computer (because it can actually run more slowly).  This input object is optional and allows controlling the number of threads (or cores, or processors) that EnergyPlus will use so that conflicts can be managed.  When this object is used, its input for the number of threads will take precedent over the value of the environment variable **"EP_OMP_NUM_THREADS"** which is an alternate method of controlling the program's threading. Since the primary method for OpenMP simulations currently (V7.1 and V7.2) is the interior radiant exchange to solve for inside surface temperatures, EnergyPlus sets the threads to 1 if the nominal number of surfaces is <= 30.

### Inputs

#### Field: Number of Threads Allowed

This field is used to specify a limit on the number of threads EnergyPlus will use.  If a zero is entered then the program will set the number of threads to the maximum available on the computer.  If the number entered is larger than the number of processors or cores, then the hardware limits will take precedent over this limit.

An example IDF object that sets the program to use only one thread follows.

~~~~~~~~~~~~~~~~~~~~

      ProgramControl,
        1 ; !- Number of Threads Allowed
~~~~~~~~~~~~~~~~~~~~

## Meter:Custom

A custom meter allows the user to group variables or meters onto a virtual meter that can be used just like a normal meter created by EnergyPlus. For consistency, the items being grouped must all be similar.

### Inputs

#### Field: Name

This is a user defined name for the custom meter. Names for custom meters cannot duplicate internal meter names.

#### Field: Fuel Type

A fuel type should be specified for the meter. All assignments to this meter will be checked to assure that the same fuel type is used. Additionally, this may be used in other objects (such as the Demand Limiting). Valid choices for this field are:

- Electricity
- NaturalGas
- PropaneGas
- FuelOil#1
- FuelOil#2
- Coal
- Diesel
- Gasoline
- Water
- Generic
- OtherFuel1
- OtherFuel2

Fuel types are generally self-explanatory. Generic is included for convenience when a custom meter is defined that doesn't quite fit the "fuel" categories.  See the examples below.

#### Field: group(s) Key Name-Output Variable/Meter Name

The rest of the object is filled with parameters of the key name/output variable or meter names. When a meter name is used, the key name field is left blank.

#### Field: Key Name #

A key name field is used when the following field specifies an output variable. If the field is left blank, then all the output variables in the following field are assigned to the meter.

#### Field: Output Variable or Meter Name #

This field must be a valid output variable name or a valid meter name.

## Meter:CustomDecrement

The decrement custom meter is very similar to the custom meter specification but additionally allows a predefined meter to be used as the "source" meter and the remaining items subtract from that predefined meter.

### Inputs

#### Field: Name

This is a user defined name for the custom meter. Names for custom meters cannot duplicate internal meter names.

#### Field: Fuel Type

A fuel type should be specified for the meter. All assignments to this meter will be checked to assure that the same fuel type is used. Additionally, this may be used in other objects (such as the Demand Limiting). Valid choices for this field are:

- Electricity
- NaturalGas
- PropaneGas
- FuelOil#1
- FuelOil#2
- Coal
- Diesel
- Gasoline
- Water
- Generic
- OtherFuel1
- OtherFuel2

#### Field: Source Meter Name

This name specifies the meter that will be used as the main source for the decrement custom meter. The remainder of the fields are subtracted from the value of this meter to create the meter value named above. The Source Meter is not changed in any way by including this custom meter.

#### Field: group(s) Key Name-Output Variable/Meter Name

The rest of the object is filled with parameters of the key name/output variable or meter names. When a meter name is used, the key name field is left blank.

#### Field: Key Name #

A key name field is used when the following field specifies an output variable. If the field is left blank, then all the output variables in the following field are assigned to the meter.

#### Field: Output Variable or Meter Name #

This field must be a valid output variable name or a valid meter name. Additionally, it must be contained on the Source Meter. Note that, if an error occurs, only the Variable in error will show – confusing things if what was entered was a meter name.

## Custom Meter Examples

Details of the Meter:Custom/Meter:CustomDecrement are shown on the Meter Details file.

In the following examples, the custom meters are set up to illustrate the capabilities of custom meters. Custom meter "**MyGeneralLights**" duplicates the InteriorLights:Electricity meter. Custom meter "**MyBuildingElectric**" duplicates the Electricity:Building meter (by specifying that meter). Custom Meter (Decrement) "**MyBuildingOther**" uses the Electricity:Building meter as the source meter and subtracts out the values for MyGeneralLights (aka InteriorLights:Electricity). The resultant value for the MyBuildingOther meter should be equal to the value for the meters Electricity:Building – InteriorLights:Electricity.

~~~~~~~~~~~~~~~~~~~~

    Meter:Custom,
        MyGeneralLights,         !- Name
        Electricity,             !- Fuel Type
        SPACE1-1,                !- Key Name 1
        Lights Electric Energy,  !- Output Variable or Meter Name 1
        SPACE2-1,                !- Key Name 2
        Lights Electric Energy,  !- Output Variable or Meter Name 2
        SPACE3-1,                !- Key Name 3
        Lights Electric Energy,  !- Output Variable or Meter Name 3
        SPACE4-1,                !- Key Name 4
        Lights Electric Energy,  !- Output Variable or Meter Name 4
        SPACE5-1,                !- Key Name 5
        Lights Electric Energy;  !- Output Variable or Meter Name 5
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    Meter:Custom,
        MyBuildingElectric,      !- Name
        Electricity,             !- Fuel Type
        ,                        !- Key Name #1
        Electricity:Building;    !- Output Variable or Meter Name #1

    Meter:CustomDecrement,
        MyBuildingOther,         !- Name
        Electricity,             !- Fuel Type
        Electricity:Building,    !- Source Meter Name
        ,                        !- Key Name #1
        MyGeneralLights;         !- Output Variable or Meter Name #1
~~~~~~~~~~~~~~~~~~~~

For an example of "generic" fuel type, one might put the [Building](#building) Infiltration Heat Loss & Heat Gain on a set of custom meters:

~~~~~~~~~~~~~~~~~~~~

      Meter:Custom,
        Building Infiltration Heat Loss,  !- Name
        Generic,             !- Fuel Type
        *,                       !- Key Name 1
        Zone Infiltration Total Heat Loss Energy;  !- Output Variable Name 1

      Meter:Custom,
        Building Infiltration Heat Gain,  !- Name
        Generic,             !- Fuel Type
        *,                       !- Key Name 1
        Zone Infiltration Total Heat Gain Energy;  !- Output Variable Name 1
~~~~~~~~~~~~~~~~~~~~

One can then report these values the same way one reports other standard meters.

## Simulation Parameter Outputs

These appear in the **eplusout.eio** file. For details of the reporting, please see the Output Details and Examples document.