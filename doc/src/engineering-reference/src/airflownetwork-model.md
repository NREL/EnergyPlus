# AirflowNetwork Model

## Overview

The AirflowNetwork model provides the ability to simulate the performance of an air distribution system, including supply and return leaks, and calculate multizone airflows driven by outdoor wind and forced air during HVAC system operation. The pressure and airflow model described here was developed based on AIRNET (Walton 1989). This detailed model is used to simulate thermal conduction and air leakage losses for constant volume air distribution systems (e.g., in residential or light commercial buildings). The multizone airflow calculations are performed at the HVAC system time step which, among other benefits,.allows for modeling hybrid ventilation systems.

## Model Description

The input object AirflowNetwork:SimulationControl provides access to the airflow network method, which consists of a set of nodes connected by airflow components through linkages. The objects AirflowNetwork:Multizone:Zone,  AirflowNetwork:Multizone:ExternalNode, and AirflowNetwork:Distribution:Node represent airflow nodes. The objects AirflowNetwork:Multizone:Surface and AirflowNetwork:Distribution:Linkage represent airflow linkages. The other objects with a relationship between pressure and airflow represent airflow components.

The AirflowNetwork model consists of three sequential steps:

- Pressure and airflow calculations
- Node temperature and humidity calculations
- Sensible and latent load calculations

The pressure and airflow calculations determine pressure at each node and airflow through each linkage given wind pressures and forced airflows. Based on the airflow calculated for each linkage, the model then calculates node temperatures and humidity ratios given zone air temperatures and zone humidity ratios.  Using these node temperatures and humidity ratios, the sensible and latent loads from duct system conduction and leakage are summed for each zone. The sensible and latent loads obtained in this step are then used in the zone energy balance equations to predict HVAC system loads and to calculate the final zone air temperatures, humidity ratios, and pressures.

The present AirflowNetwork model may only be applied to a single heating and cooling system that uses a single air distribution system (a single AirLoopHVAC object). The model excludes the impact of the air and duct system thermal capacitance. The impact of thermal capacity will be addressed in future upgrades to this model.

## Pressure and Airflow Calculations

The EnergyPlus airflow network consists of a set of nodes linked by airflow components. Therefore, it is a simplified airflow model, compared to detailed models such as those used in computational fluid dynamics (CFD) models. The node variable is pressure and the linkage variable is airflow rate. A brief description is presented below. A detailed description of the airflow network model may be found in the work of Walton (1989), Dols and Walton (2002), and Walton and Dols (2003).

### Initialization

Newton's method is used to solve for node air pressures and it requires an initial set of values for the node pressures. There are two initialization methods available. The first is linear initialization and equivalent to Initialization flag = 0. These initial values may be obtained by including in each airflow component a linear approximation relating airflow to pressure drop:

![](media/image2537.png)\


where

![](media/image2538.png) = Air mass flow rate at i-th linkage [kg/s]

*C~i~* = Air mass flow coefficient [m^3^]

![](media/image2539.png) ~i~= Pressure difference across the i-th linkage [Pa]

*µ= Air viscosity [Pa-*s]

This initialization handles stack effects very well and tends to establish the proper direction for the airflows. The linear approximation is provided by the laminar regime.

The second initialization method assumes the initial pressures are zero and uses Newton's method directly.

### Convergence criteria

Conservation of air mass flow rate at each linkage provides the convergence criterion. When the sum of mass flow rates in all the linkages approaches zero within the convergence tolerance, the solution has converged. The solution is assumed to have converged when the sum is less than the convergence value, in order to reduce the number of iterations and obtain sufficient accuracy. There are two convergence criteria used in the AirflowNetwork model: Relative airflow convergence tolerance and Absolute airflow convergence tolerance.

Relative airflow tolerance = ![](media/image2540.png)

Absolute airflow tolerance = ![](media/image2541.png)

The relative airflow tolerance is equivalent to the ratio of the absolute value of the sum of all network airflows to the sum of the network airflow magnitudes.  The absolute airflow tolerance is the summation of the absolute value of all network airflows. The solution has converged when both of these convergence criteria have been met.

### Linkage models

A linkage used in the AirflowNetwork model has two nodes, inlet and outlet, and is linked by a component which has a relationship between airflow and pressure.  The pressure difference across each component in a linkage is assumed to be governed by Bernoulli's equation:

![](media/image2542.png)\


 where

*ΔP= Total pressure difference between nodes n and m [Pa]*

*P~n~, P~m~*= Entry and exit static pressures [Pa]

*V~n~, V~m~*= Entry and exit airflow velocities [m/s]

*ρ= Air density [kg/m*^3^]

*g*= Acceleration due to gravity [9.81 m/s^2^]

*z~n~, z~m~*= Entry and exit elevations [m]

By rearranging terms and adding wind pressure impacts, the above equation may be rewritten in the format used by the airflow network model:

![](media/image2543.png)\


where

*P~n~, P~m~*~~= Total pressures at nodes n and m [Pa]

*P~S~*= Pressure difference due to density and height differences [Pa]

*P~W~= Pressure difference due to wind [Pa]*

The Input Output Reference provides the relationship between airflow and pressure for the most of the components (Ref. AirflowNetwork Model). The relationship between airflow and pressure for the AirflowNetwork:Multizone:Component:DetailedOpening, AirflowNetwork:Multizone:Component:SimpleOpening, and AirflowNetwork:Multizone:Component:HorizontalOpening objects are provided in detail in this reference.

![The general problem of gravitational flow through a vertical opening](media/the-general-problem-of-gravitational-flow.png)


The schematic drawing of a possible air flow pattern through a detailed vertical opening (AirflowNetwork:Multizone:Component:DetailedOpening) is shown in Figure 147. The equations used below are extracted from the COMIS Fundamentals manual (1990).

The air density is assumed to be a linear function of height:

![](media/image2545.png)\


The pressure difference is assumed to be linear and simulate the effect of turbulence:

 ![](media/image2546.png)

The reference pressures on each side are given at the bottom of the opening. By assuming the Bernoulli hypothesis on both sides, the pressure difference can be defined at any level of z as:

![](media/image2547.png)\


The velocity at any level z is given by

![](media/image2548.png)\


The locations of the two possible neutral planes are given by an equilibrium in pressure which leads to a zero velocity point. By assuming the left terms in the equation above to be zero, one may have:

![](media/image2549.png)\


This equation above can have two, one, or zero real solutions.  The zero solution represents a one-way flow through the opening and may be expressed in the following equation:

![](media/image2550.png)\


The one real solution represents two-way (bi-directional) flow, which may be written in the following equations.

![](media/image2551.png)\


![](media/image2552.png)\


The two real solutions represent three-way flow, which may be written in the following equations.

![](media/image2553.png)\


![](media/image2554.png)\


![](media/image2555.png)\


where

*C~d~*= discharge coefficient [dimensionless]

*θ*= Area reduction factor [dimensionless]

*W*= Opening width [m]

The discharge coefficient, opening width, opening height, and start height factor are modulated based on opening factors. A detailed description of opening factor calculations may be found in the Input Output Reference (Ref. AirflowNetwork:Multizone:Zone, AirflowNetwork:Multizone:Surface, and

AirflowNetwork:Multizone:Component:DetailedOpening).

The above calculation procedure is used for a normal rectangular window. For a horizontally pivoted rectangular window, the calculation procedure is slightly different. A schematic drawing of a horizontally-pivoted window is shown in Figure 148.

![Schematic drawing of a horizontally-pivoted window](media/schematic-drawing-of-a-horizontally-pivoted.jpeg)


The opening angle α (0-90°) is linearly proportional to the window opening factor (0-1.0). An opening factor of 1.0 is equal to an opening angle of 90°. The heights in the pivoted area are expressed as:

![](media/image2557.png)\


![](media/image2558.png)\


When z < h2 or z > h4, where z is the distance from the bottom of the window, the integration procedure is the same as the procedure for a normal rectangular window. When h2<z<h4, the window width W in the above equations is modified as:

 ![](media/image2559.png)

The mass flow rate in the pivoted area becomes:

![](media/image2560.png)\


It should be pointed out that the discharge coefficient is modulated based on opening factors, while opening width, opening height, and start height factor do not apply for a horizontally-pivoted window. The actual window width and height are used to calculate airflows for a horizontally-pivoted window.

The schematic drawing of air flow patterns through a simple vertical opening (AirflowNetwork:Multizone:Component:SimpleOpening) is shown in Figure 149. The equations used below are available from Walton (1989).

![Schematic of large opening and associated three flow patterns](media/schematic-of-large-opening-and-associated.png)


The air density for each node is assumed to be constant. The hydrostatic equation is used to relate pressures at various heights for each node:

![](media/image2562.png)\


![](media/image2563.png)\


where

*P~0~~n~, P~0~~m~ = ~~*pressure at nodes (zones) n and m at y = 0, the reference elevation of the opening [Pa]

*ρ~n~, ρ~m~~~ =* air densities of zones n and m [kg/m^3^]

*P~n~, P~m~ =* reference pressures of zones n and m [Pa]

It is assumed that the velocity of the airflow as a function of height is given by the orifice equation (Brown and Solvason 1962):

![](media/image2564.png)\


 where

*C~d~*~~~~= discharge coefficient [dimensionless]

*ρ*   = density of the air going through the opening [kg/m^3^]

The neutral height, Y, where the velocity of the air is zero, may be calculated in the following equation:

![](media/image2565.png)\


When the neutral plane is within the opening (first pattern in Figure 149), two-way (bi-directional) flows occur. The total flow through a large opening is the sum of both flows.

![](media/image2566.png)\


![](media/image2567.png)\


When the neutral plane is below or above the large opening (second and third pattern in Figure 149), one-way flow occurs.

![](media/image2568.png)\


The opening width is modulated based on opening factors. A detailed description of opening factor calculations may be found in the Input Output Reference (Ref. AirflowNetwork:Multizone:Zone, AirflowNetwork:Multizone:Surface, and AirflowNetwork:Multizone:Component:DetailedOpening).

The above two objects are used to simulate airflows across large vertical openings. The simple opening component (AirflowNetwork:Multizone:Component:SimpleOpening) assumes the pressure difference across the opening is a function of height varied from opening bottom to top, so that two-way flow may be obtained if appropriate (Walton 1989). The Detailed Opening component (AirflowNetwork:Multizone:Component:DetailedOpening) assumes both the pressure difference across the opening and air density are a function of height, so that three-way flow may be obtained (COMIS 1990). If these opening models would be used for horizontal openings, the pressure difference across the opening and air density remain constant, so that only one-way flow is possible using the detailed and simple opening components which are meant for vertical or near-vertical openings. In reality, there are two-way flows (air recirculation) across a large horizontal opening caused by buoyancy due to temperature and pressure difference and forced flow driven by air pressure difference only. Therefore, a horizontal opening component (AirflowNetwork:Multizone:Component:HorizontalOpening) is available to simulate airflows across large horizontal openings with the possibility of two-way flow by combining forced and buoyancy airflows together.

The model for horizontal openings consists of forced airflow, buoyancy airflow, purge pressure and sloping plane. The model is mainly from a NIST report presented by Cooper (1989). The sloping plane (Bolmqvist and Sandberg 2004) portion of the model was added to allow for staircase simulations.

For simplicity, a two zone building (upper and lower zones) connected by a large horizontal opening is used to describe the model, as shown in Figure 150. Forced and buoyancy airflows are described separately below.

![Air movements across a horizontal opening](media/air-movements-across-a-horizontal-opening.jpeg)


Forced airflows

The air mass flow rate is determined by the pressure difference across the opening. The relationship between pressure and airflow is the same as AIRNET for a component (see AirflowNetwork:Multizone:Component:SimpleOpening description above). Since the height of the opening is constant, the forced airflow is unidirectional. A positive value for pressure difference indicates flow direction is from the lower zone to the upper zone across the opening, while a negative value represents flow in the opposite direction. The following description addresses forced air mass flow rates and partial derivatives for three possible scenarios of pressure difference:

P~L~ = P~U~

![](media/image2570.png) = ![](media/image2571.png) = 0

where:

P~L~= Air pressure in the lower zone [Pa]

P~U~= Air pressure in the upper zone [Pa]

![](media/image2570.png) = Air mass flow rate from the lower zone to the upper zone driven by forced airflow pressure difference [kg/s]

![](media/image2571.png) = Air mass flow rate from the upper zone to the lower zone driven by forced airflow pressure difference [kg/s]

P~L~ > P~U~

![](media/image2570.png) = 0

![](media/image2572.png)\


![](media/image2573.png)\


where:

ρ~L~= Air density in the lower zone [kg/m^3^]

A= Opening area [m^2^]

C~d~= Discharge coefficient [Dimensionless]

ρ~ave~ = Average air density between the lower and upper zones [Pa]

ΔP= Pressure difference P~L~ -~~P~U~ [Pa]

P~L~ < P~U~

![](media/image2574.png) = 0

![](media/image2575.png)\


![](media/image2576.png)\


**where:**

ρ~U~= Air density in the upper zone [kg/m^3^]

A= Opening area [m^2^]

C~d~= Discharge coefficient [Dimensionless]

ρ~ave~ = Average air density between the lower and upper zones [Pa]

ΔP= Pressure difference P~L~ -~~P~U~ [Pa]

#### Buoyancy airflows

Buoyancy flow only occurs when the air density in the upper zone is greater than the air density in the lower zone. The flow is bi-directional and the amount of upper flow is equal to the lower flow across the opening. The following discussion assumes the air density in the upper zone is greater than the air density in the lower zone. Otherwise, the buoyancy flow rate is equal to zero. It is also assumed that the maximum buoyancy flow occurs when the pressure difference across the opening due to forced airflows is zero. The maximum buoyancy flow may be expressed as a part of Cooper's model:

![](media/image2577.png)\


where:

![](media/image2578.png)  = Buoyancy mass flow rate at zero forced airflow pressure difference [kg/s]

g= Gravity acceleration [m/s^2^]

D~H~= Hydraulic diameter [m]

ρ~ave~ = Average air density between the lower and upper zones [kg/m^3^]

Δρ= Density difference between the lower and upper zones [kg/m^3^]

#### Combined airflows

When forced and buoyancy flows co-exist, it is possible to have either unidirectional or bi-directional flows. For example, when the upward force due to pressure difference is greater than the buoyancy force (downward), unidirectional flow occurs. Bi-directional flow only occurs when the upward imposed force is less than the buoyancy force. The critical pressure between unidirectional and bi-directional flows is called the purge pressure (Tan and Jaluria 1992). The purge pressure is a function of opening geometry and the buoyancy force (ΔP/(gΔρD~H~)) and may be expressed as (Cooper 1998):

![](media/image2579.png)\


where:

ΔP~Flood~ = Purging pressure [Pa]

g= Gravity acceleration [m/s^2^]

D~H~= Hydraulic diameter [m]

A= Opening area [m^2^]

Δρ= Density difference between the lower and upper zones [kg/m^3^]

C~Shape~ = Shape factor [dimensionless]

![](media/image2580.png)\


where:

w= Opening width [m]

D= Opening depth [m]

As mentioned above, when the air pressure difference between two zones is zero there is the maximum bi-directional flow due to the buoyancy force. When the pressure difference increases from 0 and is less than |ΔP~Flood~|, there is some bi-directional flow across the opening, but less than the maximum flow. If the pressure difference keeps increasing and exceeds |ΔP~Flood~|, there is no bi-directional flow.  Cooper's model assumes the buoyancy flow varies linearly with pressure difference.

![](media/image2581.png)\


The total air flow across the opening is based on superposition of the forced and buoyancy flows, and may be expressed for three different pressure difference scenarios as follows:

P~L~ = P~U~

![](media/image2582.png)\


![](media/image2583.png)\


 ![](media/image2584.png)

P~L~ > P~U~

![](media/image2585.png)\


![](media/image2586.png)\


![](media/image2587.png)\


where:

ρ~L~= Air density in the lower zone [kg/m^3^]

A= Opening area [m^2^]

C~d~= Discharge coefficient [dimensionless]

Ρ~ave~ = Average air density between the lower and upper zones [Pa]

ΔP= Pressure difference P~L~ -~~P~U~ [Pa]~~

P~L~ < P~U~

![](media/image2588.png)\


![](media/image2589.png)\


![](media/image2590.png)\


*Sloping plane*

![A Staircase is attached to the horizontal opening.](media/a-staircase-is-attached-to-the-horizontal.png)


When a staircase is introduced as shown in Figure 151, the effective opening area will be used to replace A (opening area) in the above equations. The effective area may be estimated as (Bolmqvist and Sandberg 2004):

![](media/image2592.png)\


where:

A~eff~= Effective area of horizontal opening [m^2^]

A= Area of horizontal opening [m^2^]

α= Angle between the stair plane and horizontal opening plane [degrees]

Note: the hydraulic diameter calculation is based on the effective opening area, while the opening depth remains the same.

Figure 152 demonstrates possible forced and buoyancy flow rates at different ratios of pressure difference to purging pressure across a horizontal opening when the upper zone air density is greater than the lower zone air density. The pressure difference is the lower zone pressure minus the upper zone pressure. Otherwise, the buoyancy flow is zero. In addition, when the absolute ratio is above 1, the buoyancy flow is also zero. The following table provides a brief description for the legend listed in Figure 152.

Table: Legend Description

Legend|Description
------|-----------
Forced downward|Forced flow rate from upper to lower at P~L~-P~U~ < 0
Forced upward|Forced flow rate from lower to upper at P~L~-P~U~ > 0
Buoyancy upward|Total upward flow rate due to buoyancy only at P~L~-P~U~ < 0
Buoyancy downward|Total downward flow rate due to buoyancy only at P~L~-P~U~ > 0
Combined downward|Total downward flow at P~L~-P~U~ < 0 (Forced downward + buoyancy upward)
Combined upward|Total upward flow at P~L~-P~U~ > 0 (Forced upward + buoyancy downward)

![Flow rates at different pressure differences](media/flow-rates-at-different-pressure-differences.png)


### Wind pressure calculations

The wind pressure is determined by Bernoulli's equation, assuming no height change or pressure losses:

![](media/image2594.png)\


where:

*p~w~= Wind surface pressure relative to static pressure in undisturbed flow [Pa]*

*ρ*= Air density [kg/m^3^]

*V~ref~*= Reference wind speed at local height [m/s]

*C~p~*= Wind surface pressure coefficient [dimensionless]

*V~ref~* may be expressed as (Ref, Local Wind Speed Calculations):

![](media/image2595.png)\


*C~p~* is a function of location on the building envelope and wind direction. When Wind Pressure Coefficient Type = "INPUT", the *C~p~* values are explicitly defined in the input for AirflowNetwork:Multizone:Wind Pressure Coefficient Values. When Wind Pressure Coefficient Type = "AVERAGE-SURFACE CALCULATION" and the building shape is rectangular, the program uses the following equations to calculate wind pressure coefficient (*C~p~*) values for different wind directions. For a low rise building, the normalized surface pressure coefficient may be written as (Swami and Chandra 1988):

![](media/image2596.png)\


where

*C~p,n~*= *C~p~* value at a given angle between wind direction and the outward normal of the surface under consideration [dimensionless]

α= Angle between wind direction and outward normal of wall under consideration [deg]

*G*= Natural log of the ratio of the width of the wall under consideration to the width of the adjacent wall [dimensionless]

*n* = Index of incident angle at 30-degree increments

For walls of a high rise building, a two-dimensional array of surface-averaged wind pressure coefficients is generated based on wind incident angle and side ratio. The wind pressure coefficients are provided in 2001 ASHRAE Fundamentals Handbook, p. 16.5, Fig. 7, "Surface Averaged Wall Pressure Coefficients for Tall Buildings". The original work was performed by Atkins et al. (1979). The incident angle has an increment of 30 degrees. The side ratio values are 0.25, 1.0, and 4.0. For a given incident angle and building aspect ratio, the program uses linear interpolation to calculate the corresponding wind pressure coefficient *C~p,n~*.

For the roof of a high rise building, a two-dimensional array of surface-averaged wind pressure coefficients is also generated based on wind incident angle and side ratio. The wind pressure coefficients are provided in 2001 ASHRAE Fundamentals Handbook, p. 16.6, Fig. 9, "Surface Averaged Roof Pressure Coefficients for Tall Buildings". The original work was performed by Holmes (1986). The incident angle has an increment of 30 degrees. The side ratio values are 0.25, 1.0, and 4.0. At a given wind incident angle and building aspect ratio, the program uses linear interpolation to calculate the corresponding wind pressure coefficient *C~p,n~*.

The wind surface pressure at the given incident angle can be calculated by combining the above two equations:

![](media/image2597.png)\


### Solution method

Based on the relationship between airflow rate and pressure drop for each component, a system of equations for all components can be assembled together in an n x n square matrix, where n is the number of nodes. Newton's method is used to iteratively solve for the air pressure at each node. A new estimated vector for all node pressures, {P}\*, is computed from the current estimated vector of node pressures, {P}, by:

![](media/image2598.png)\


where the correction vector, {C}, is computed by the matrix relationship:

![](media/image2599.png)\


{B} is a column vector with each component given by:

![](media/image2600.png)\


where n is the node number and i indicates all flow paths connecting node n to other nodes, and [J] is the square Jacobian matrix whose elements are given by:

![](media/image2601.png)\


### Convergence acceleration

The convergence tolerance is used to check the sum of the mass flow rates by applying mass conservation. The convergence acceleration equation shown below is used to correct the node pressures to more rapidly obtain a solution. By assuming a constant ratio of correction values from one iteration to the next, the following method is applied:

![](media/image2602.png)\


where

*r*= the ratio of C~n~ for the current iteration to its value for the previous iteration [dimensionless]

*C~n~*= Correction value at the n~th~ node [Pa]

*P~n~*~~= Estimated pressure at the n~th~ node [Pa]

*P~n~^\*^~~*= Corrected pressure at the n~th~ node used in the next iteration [Pa]

This method is similar to a Steffensen iteration (Conte and de Boor 1972) which is used as a fixed-point iteration method for individual nonlinear equations.

The iteration correction method presented in the above equation gives a variable factor. When the solution is close to convergence, the solution method converges quadratically. By limiting cases where the value of r is less than some value, such as -0.5, the solution will not interfere with the rapid convergence. It has not been proven that the convergence acceleration equation will always lead to convergence, but it can be shown that it will not prevent convergence. Newton's method converges when the estimated solution values are within some distance, called the radius of convergence, or the correct solution. Applying the convergence acceleration equation when -1 < r <0, will cause a smaller correction than Newton's method, which therefore, can not force the iterations outside the radius of convergence. When r<-1, the solution diverges in an oscillatory fashion. When r>1, the solution also diverges, but in a nonoscillatory manner. For 0<r<1, the solution is approached from one direction. In all three cases, the convergence acceleration equation applies as long as r is truly constant over several iterations. However, for the last case, this involves a true extrapolation of correction factor which is very sensitive to the accuracy of r. This is most extreme for the case of r=1, which would cause an infinite correction.

## Node Temperature Calculations

A brief description of the air node temperature calculation is given below. A detailed description can be found in the work of Swami et al. (1992). The following equation is used to calculate temperature distribution across a duct element at the given airflow rate and inlet air temperature:

![](media/image2603.png)\


where

*C~p~*= Specific heat of airflow [J/kg•K]

![](media/image2604.png) = Airflow rate [kg/s]

*P*= Perimeter of a duct element [m]

*T*= Temperature as a field variable [°C]

![](media/image2605.png) = Temperature of air surrounding the duct element [°C]

*U*= Overall heat transfer coefficient [W/m^2^•K]

![](media/image2606.png)\


*h~i~*= Inside heat transfer coefficient [W/m^2^•K]

*h~o~*= Outside heat transfer coefficient [W/m^2^•K]

*t~j~*= Thickness at j-th layer [m]

*k~j~*= Thermal conductivity at j-th layer [W/m•K]

The outlet air temperature at the end of the duct (x=L) is:

![](media/image2607.png)\


where

*T~i~*= Inlet air temperature [°C]

*T~o~*= Outlet air temperature [°C]

*T~∞~*= Temperature of air surrounding the duct element [°C]

*A*= Surface area (Perimeter \* Length) [m^2^]

The heat transfer by convection to ambient, *Q*, is:

![](media/image2608.png)\


The outlet air temperature can be calculated using the above equation at the given inlet air temperature. Since the inlet temperature at one linkage is the outlet temperature for the connected linkage, the outlet air temperatures at all nodes are solved simultaneously. A square linear system assembled by the AirflowNetwork model is expressed below:

![](media/image2609.png)\


where

{M}= Airflow matrix

[T]= Temperature vector

[B]= Given boundary conditions

The zone air temperatures and primary air loop component (fan and coils) outlet conditions are used as prescribed conditions in the AirflowNetwork model. In addition, the temperature difference across zone loop components (terminal units) is held constant during the calculations. For example, thermal zone temperatures calculated during the previous system time step are used as prescribed temperatures when calculating all other node temperatures. The zone air temperature is assumed constant (prescribed) throughout the AirflowNetwork iterative solution. The fan and coil outlet air temperatures, and terminal unit temperature differences are assumed constant within an AirflowNetwork iteration. The sensible heat gains calculated during the AirflowNetwork solution are then used to predict a new zone air temperature.

## Node Humidity Ratio Calculations

A brief description of the air node humidity ratio calculation is given below. A detailed description can found in the work of Swami et al. (1992). The following equation is used to calculate humidity ratio distribution across a duct element at the given airflow rate and inlet air humidity ratio:

![](media/image2610.png)\


where

![](media/image2611.png) = Airflow rate [kg/s]

*P= Perimeter of a duct element [m]*

*W= Humidity ratio [kg/kg]*

![](media/image2612.png) = Humidity ratio of air surrounding the duct element [kg/kg]

*U~m~*= Overall moisture transfer coefficient [kg/m^2^•s]

![](media/image2613.png)\


*h~m,i~*~~= Inside moisture transfer coefficient [kg/m^2^•s]

*h~m,o~= Outside moisture transfer coefficient [kg/m*^2^•s]

*t~j~*= Thickness at j-th layer [m]

D~j~= Moisture diffusivity at j-th layer [kg/m•s]

The outlet air humidity ratio at the end of the duct (x=L) is:

![](media/image2614.png)\


where

*W~i~*= Inlet air humidity ratio [kg/kg]

*W~o~= Outlet air humidity ratio [kg/kg]*

*A*= Surface area (Perimeter \* Length) [m^2^]

The moisture transfer by convection to ambient, *Q~m~*, is

![](media/image2615.png)\


The outlet air humidity ratio can be calculated using the above equation at the given inlet air humidity ratio. Since the inlet humidity ratio at one linkage is the outlet humidity ratio for the connected linkage, the outlet air humidity ratio at all nodes are solved simultaneously. A square linear system assembled by the AirflowNetwork model is expressed below:

![](media/image2616.png)\


where

{M~m~}= Airflow matrix

[W]= Humidity ratio vector

[B~m~]= Given boundary conditions

The zone air humidity ratios and primary air loop component (fan and coils) outlet conditions are used as prescribed conditions in the AirflowNetwork model. For example, thermal zone humidity ratios calculated during the previous system time step are used as prescribed humidity ratios when calculating all other node humidity ratios. The zone air humidity ratio is assumed constant (prescribed) throughout the AirflowNetwork iterative solution. The coil outlet air humidity ratio is assumed constant within an AirflowNetwork iteration. The latent heat gains calculated during the AirflowNetwork solution are then used to predict a new zone air humidity ratio.

## Sensible and Latent Load Calculations

The zone sensible and latent loads calculated in the AirflowNetwork model consist of multizone, duct conduction and leakage. In addition, the impact of infiltration and mixing is accounted for in this calculation. The multizone load only includes incoming airflows from outside (infiltration) and other adjacent zones (mixing) with and without forced-air fan operation. It is divided into two terms: variable and constant. The constant term is the sum of the mass flow rate multiplied by the specific heat for both infiltration and mixing. The variable term includes the impact of zone and outdoor air temperature. Each of these terms is used in the zone energy balance equation. The sensible load items from the multizone load calculations may be written as follows:

![](media/image2617.png)\


![](media/image2618.png)\


where

*MCP~airflow~* = Sum of air mass flow rate multiplied by specific heat for infiltration and mixing [W/K]

*MCPT~airflow~* = Sum of air mass flow rate multiplied by specific heat and temperature for infiltration and mixing [W]

![](media/image2619.png) = Incoming air mass flow rate from outdoors [kg/s]

![](media/image2620.png) = Incoming air mass flow rate from adjacent zones [kg/s]

*T~amb~*= Outdoor air dry-bulb temperature [°C]

*T~zone~*= Adjacent zone air temperature [°C]

The latent load items from multizone load calculations may be written as follows:

![](media/image2621.png)\


![](media/image2622.png)\


where

*M~airflow~* = Sum of air mass flow rates for infiltration and mixing [kg/s]

*MW~airflow~* = Sum of air mass flow rate multiplied by humidity ratio for infiltration and mixing [kg/s]

![](media/image2623.png) = Incoming air mass flow rate from outdoors [kg/s]

![](media/image2624.png) = Incoming air mass flow rate from adjacent zones [kg/s]

*W~amb~*= Outdoor air humidity ratio [kg/kg]

*W~zone~*= Adjacent zone air humidity ratio [kg/kg]

The air distribution system (ADS) loads due to duct conduction and leakage depend on the air distribution system component (e.g., duct) location. The air distribution system sensible and latent loads are calculated for each zone as follows:

![](media/image2625.png)\


 ![](media/image2626.png)

where

*Q~ADS,~~i~*= Total sensible load in the i-th zone due to ADS losses [W]

*Q~cond(ij)~*= Duct wall conduction loss at the j-th duct located in the i-th zone [W]

*Q~leak(ij)~*= Sensible supply leak loss at the j-th linkage located in the i-th zone [W]

*Q~ADS,~~m,i~*= Total latent load in the i-th zone due to ADS losses [kg/s]

*Q~cond,m(ij)~~~*= Duct wall vapor diffusion loss at the j-th duct located in the i-th zone [kg/s]

*Q~leak,m(ij)~* = Latent supply leak loss at the j-th linkage located in the i-th zone [kg/s]

## Impacts of Supply Air Constant Volume Fan Control on Load: Cycling vs. Continuous

The AirflowNetwork model currently allows two types of constant volume fans: Fan:ConstantVolume and Fan:OnOff. The Fan:ConstantVolume object has only one type of supply air fan operation mode: continuous fan, cycling compressor (ContinuousFanWithCyclingCompressor). However, the Fan:OnOff has two types of supply air fan operation modes: cycling fan, cycling compressor (CyclingFanAndCompressor) or continuous fan, cycling compressor (ContinuousFanWithCyclingCompressor). The CyclingFanAndCompressor operation mode is frequently referred to as "AUTO fan", where the compressor(s) and supply air fan operate in unison to meet the zone heating or cooling load, and cycle off together when the heating or cooling load has been met. The ContinuousFanWithCyclingCompressor operation mode is often referred to as "fan ON", where the compressor(s) cycle on and off to meet the zone heating or cooling load but the supply air fan operates continuously regardless of compressor operation. The supply air fan operation mode is specified in an HVAC system object based on a given fan operation mode schedule (e.g., AirLoopHVAC:UnitaryHeatCool object).

The determination of the zone sensible and latent loads caused by multizone airflows and forced air distribution system operation is dependent on the supply air fan operation mode (see Sensible and Latent Load Calculations section above). The zone loads calculated by the AirflowNetwork model are added to zone sensible and latent balances in the ZonePredictorCorrector module to calculate zone air temperatures and humidity ratios (see Integration of the AirflowNetwork Model section below). For the case of continuous fan/cycling compressor, the zone loads during forced air distribution system operation are calculated with the system design air mass flow rate without modification, since the system air node conditions (temperature and humidity) reflect the average values for the time step considering the coil/fan on and off periods during the time step.

For the case of cycling fan/cycling compressor, where the forced air distribution system can operate for a portion of the simulation time step, the airflows are determined based on the air distribution system part-load ratio (ratio of the average air mass flow rate for the time step divided by the design air mass flow rate). The **airflows** for the AirflowNetwork:Distribution:Linkage objects are reported during the air distribution system on cycle, since no airflow is assumed during the system off cycle. The **airflows** for the AirflowNetwork:Multizone:Surface objects are weighted by the air distribution system part-load ratio. The **zone loads** are the sum of energy losses during both the air distribution system on and off cycle at each system time step. The energy losses when the air distribution system is on are calculated using the system "on" air flow rate multiplied by the air distribution system run time fraction. The energy losses when the air distribution system is off are obtained from the multizone airflow calculations (without forced air through the air distribution system) and these losses are multiplied by (1.0 - system run time fraction), assuming no airflows through the air distribution system when the fan is off. The formulas used to calculate airflows and zone loads are given below:

Airflow

Airflow = Airflow during ADS on cycle \* ADS Part-load ratio + Airflow during ADS off cycle \* (1.0 – ADS Part-load ratio)

where ADS = Air Distribution System

Zone load

System run time fraction = Maximum run time fraction of coils and fans in the air distribution system

Zone energy losses = Zone energy loss during ADS on cycle \* System run time fraction + Zone energy loss during ADS off cycle \* (1.0 - System run time fraction)

The calculation of loads due to multizone airflow, without forced air distribution system operation, is performed when the HVAC system is first simulated during a simulation time step (FirstHVACIteration = True). The calculation of loads due to air distribution system operation is performed on subsequent iterations within the same simulation time step when the mass flow rate at the supply air fan inlet is greater than 0.0 (and FirstHVACIteration = False).

## Airflow Calculation Procedure using A Supply Variable Air Volume Fan

The AirflowNetwork model currently also allows a variable air volume fan type as Fan:VariableVolume. The allowed terminal type is AirTerminal:SingleDuct:VAV:Reheat only. Other types of terminals will be added later.

In general, the supply fan air flow rate in a VAV central system is determined by a sum of terminal flow rates when the AirflowNetwork model is not applied. When the AirflowNetwork model is applied and the supply air fan flow rate is given, each terminal flow is determined by pressure resistance of each supply air pathway. It is possible that the delivered air flow rate from the pressure resistance at each terminal may be totally different from the desired flow rate determined by terminal units. Therefore, it is not easy to meet both requirements. The following compromised approach, including possible supply and return leaks in an air distribution system, is implemented.

Set up terminal airflows in the AirflowNetwork module based on the SimVAV subroutine in the HVACSingleDuctSystem module.

Require AirflowNetwork:Distribution:Component:LeakageRatio objects to define supply leaks, so that the values of the Effective Leakage Ratio field are used to decide the supply fan flow rates. The base of the ratio will be actual supply fan flow rate, instead of the maximum fan flow rate used in the constant volume fan systems.

Assign the supply fan airflow rate based on the sum of all terminal flow rates and all supply leak ratios until it reaches the maximum fan flow rate

![](media/image2627.png)\


where

![](media/image2628.png) = The supply fan flow rate

![](media/image2629.png) = The flow rate at the ith terminal, which is determined in the subroutine SimVAV in the HVACSingleDuctSystem module

n= Number of terminals

![](media/image2630.png) = The fraction of the supply fan flow rate at the jth supply leak, given in the AirflowNetwork:Distribution:Component:LeakageRatio objects.

k= Number of supply leaks

If the calculated supply fan flow rate is above the maximum limit of the supply fan flow rate, and the supply leak ratios remain the same, the supply fan flow rate is set to the maximum limit, and the terminal flow rates are reduced proportionally weighted by a ratio of the maximum supply fan flow rate by input to the calculated supply fan flow rate. Therefore, a sum of all terminal rates and all supply leak rates is equal to the maximum supply fan rate.  ****

![](media/image2631.png)\


![](media/image2632.png)\


where

R= The ratio of the maximum fan flow rate given in the inputs to the requested fan flow rate based on the above equation

![](media/image2633.png) = The maximum supply fan flow rate by input

![](media/image2634.png) = The calculated supply fan flow rate

![](media/image2635.png) = The final flow rate at each terminal adjusted by the ratio

## Integration of the AirflowNetwork Model

The loads calculated by the AirflowNetwork model are integrated into the EnergyPlus heat balance equation in a similar manner as described elsewhere in this document in the section "Basis for the Zone and System Integration". The mass flow rate summations and sensible and latent loads described in the previous section are included in the calculation of zone temperature and humidity ratio.

The revised zone temperature update equation becomes:

![](media/image2636.png)\


Where MCPT~airflow~ is the sum of mass flow rate multiplied by specific heat and temperature for infiltration and mixing, Q~ADS,z~ is the added total sensible load in the zone due to Air Distribution System losses, and MCP~airflow~ is the sum of mass flow rate multiplied by specific heat for infiltration and mixing as calculated in the AirflowNetwork model described above.

The revised coefficient (B) used in the zone humidity ratio calculation is shown below:

![](media/image2637.png)\


Where MW~airflow~ is the sum of mass flow rate multiplied by humidity ratio for infiltration and mixing and Q~ADS~~,m,z~~~~~is the~~added total latent (moisture) load in the zone due to Air Distribution System losses from the AirflowNetwork model described above. This coefficient is used in the prediction of moisture as described in the section "Moisture Predictor-Corrector" found elsewhere in this document.

## Model Output

The available outputs from the AirflowNetwork model are described in the EnergyPlus Input Output Reference manual.

## References

Atkins, R. E., J. A. Peterka, and J. E. Cermak. 1979. "Averaged pressure coefficients for rectangular buildings," Wind Engineering, Proceedings of the Fifth International Conference 7:369-80, Fort Collins, CO. Pergamon Press, NY.

Bolmqvist, C. and M. Sandberg, 2004, "Air Movements through Horizontal Openings in Buildings – A Model Study," International Journal of Ventilation, Vol. 3, No. 1, pp. 1-9

COMIS Fundamentals. 1990. Edited by Helmut E. Feustel and Alison Rayner-Hooson, LBL-28560, Lawrence Berkeley Laboratory, Berkeley, CA

Conte, S. D. and C de Boor. 1972. Elementary Numerical Analysis: an Algorithmic Approach, McGraw-Hill.

Cooper, L., 1989, "Calculation of the Flow Through a Horizontal Ceiling/Floor Vent," NISTIR 89-4052, National Institute of Standards and Technology, Gaithersburg, MD

Dols, W. S. & G. N. Walton. 2002. "CONTAMW 2.0 User Manual," NISTIR 6921, National Institute of Standards and Technology, Gaithersburg, Maryland

Holmes, J. D. 1986. Wind Loads on low-rise buildings: The structural and environmental effects of wind on buildings and structures, Chapter 12, Faculty of Engineering, Monash University, Melbourne, Australia

Swami, M. V. and S. Chandra. 1988. Correlations for pressure distribution on buildings and calculation of natural-ventilation airflow, ASHRAE Transactions 94(1988) (Pt 1), pp. 243-266.

Swami, M. V., L. Gu, & V. Vasanth. 1992. "Integration of Radon and Energy Models for Building," FSEC-CR-553-92, Florida Solar Energy Center, Cocoa, Florida

Tan, Q. and Y. Jaluria, 1992, "Flow through Horizontal Vents as Related to Compartment Fire Environments," NIST-GCR-92-607, National Institute of Standards and Technology, Gaithersburg, Maryland

Walton, G. N. 1989. "AIRNET – A Computer Program for Building Airflow Network Modeling," NISTIR 89-4072, National Institute of Standards and Technology, Gaithersburg, Maryland

Walton, G. N. & W. S. Dols. 2003. "CONTAM 2.1 Supplemental User Guide and Program Documentation," NISTIR 7049, National Institute of Standards and Technology, Gaithersburg, Maryland