# Generic Contaminant Predictor-Corrector

The transient air mass balance equation for the change in zone air generic contaminant concentration may be expressed as follows:

![](media/image99.png)\


where:

![](media/image100.png) =  Sum of internal generic contaminant loads from sources in a zone or interior surfaces.

The zone air density is used to convert the volumetric rate of generic contaminant generation from user input into mass generation rate [kg/s].The coefficient of 10^6^ is used to make the units of generic contaminant as ppm.

![](media/image101.png) = Sum of removal rate from sinks in a zone or interior surfaces [ppm-kg/s]

![](media/image102.png)  = Generic contaminant transfer due to interzone air mixing [ppm-kg/s]

![](media/image103.png)  = Generic contaminant concentration in the zone air being transferred into this zone [ppm]

![](media/image104.png)  = Generic contaminant transfer due to infiltration and ventilation of outdoor air [ppm-kg/s]

![](media/image105.png)  = Generic contaminant concentration in outdoor air [ppm]

![](media/image106.png)  = Generic contaminant transfer due to system supply [ppm-kg/s]

![](media/image107.png)  = Generic contaminant concentration in the system supply airstream [ppm]

![](media/image108.png)  = Air system supply mass flow rate [kg/s]

![](media/image109.png) = Generic contaminant storage term in zone air [ppm-kg/s]

![](media/image110.png)  = Zone air generic contaminant concentration at the current time step [ppm]

![](media/image111.png) = Zone air density [kg/m^3^]

![](media/image112.png) = Zone volume [m^3^]

![](media/image113.png) = Generic contaminant transport through diffusion between interior surfaces and zone air

![](media/image114.png) = Generic contaminant generation or removal rate as a function of zone air generic contaminant level at the previous time step

Mfor = Generic contaminant capacity multiplier [dimensionless] (See the InputOutput Reference for additional information on the object ZoneCapacitanceMultiplier:ResearchSpecial)

In the same manner as described above for zone air temperature (ref. Basis for the Zone and Air System Integration), the solution algorithms provided in the ZoneAirHeatBalanceAlgorithm object are also applied to the zone air carbon dioxide calculations.

In order to calculate the derivative term with respect to time, the first order backward finite difference method, defined as the EulerMethod in the ZoneAirHeatBalanceAlgorithm object, may be used:

![](media/image115.png)\


The zone air generic contaminant concentration update at the current time step using the EulerMethod may be expressed as follows:

![](media/image116.png)\


To preserve the stability of the calculation of the zone generic contaminant concentration, the third order differential approximation, derived by a Taylor Series and used in the calculation of the next time step's zone air temperature, is also applied to the zone air carbon dioxide calculations. This algorithm is the default choice and is defined as ThirdOrderBackwardDifference in the ZoneAirHeatBalanceAlgorithm object.

The third order derivative resulting from a Taylor Series expansion is defined as:

![](media/image117.png)\


The coefficients of the approximated derivative are very close to the coefficients of the analogous Adams-Bashforth algorithm. Then the approximated derivative is substituted into the mass balance, and the terms with the carbon dioxide concentration at past time steps are all put on the right-hand side of the equation. This third order derivative zone carbon dioxide update increases the number of previous time steps that are used in calculating the new zone generic contaminant concentration and decreases the dependence on the most recent. The higher order derivative approximations have the potential to allow the use of larger time steps by smoothing transitions through sudden changes in zone operating conditions.

![](media/image118.png)\


This gives us the basic air mass balance equation that will be solved in two different ways, one way for the predict step and one way for the correct step.

Since the third choice of solution algorithms uses an integration approach, defined as AnalyticalSolution in the ZoneAirHeatBalanceAlgorithm object, it does not require any approximations and has no truncation errors. The solutions in both prediction and correction are provided below in detail.

## Generic Contaminant Prediction

For the generic contaminant concentration prediction case, the equation is solved for the anticipated system response as shown below.

![](media/image119.png)\


Since the program provides three solution algorithms, the generic contaminant prediction from each solution algorithm is given below.

### EulerMethod

For this solution algorithm, the air mass balance for the predicted air system load or response is:

![](media/image120.png)\


### ThirdOrderBackwardDifference

For this solution algorithm, the air mass balance for the predicted system load or response is given below:

![](media/image121.png)\


### AnalyticalSolution

For this solution algorithm, the air mass balance for the predicted air system load or response is given below:

![](media/image122.png)\


At the prediction point in the simulation, the system air mass flows are not known; therefore, the system response is approximated. The predicted air system generic contaminant load is then used in the system simulation to achieve the best results possible. If a central HVAC system provides the outdoor flow rate from a Controller:MechanicalVentilation object, the outdoor airflow rate may be approximated as:

![](media/image123.png)\


where:

![](media/image124.png) = Supply outdoor airflow rate into the controlled zone [kg/s]

The above approximation is based on the assumption that the generic contaminant concentration at the outdoor air (OA) mixer inlet is equal to the zone air outlet concentration level, and the generic contaminant level at the zone supply air inlet is equal to the level at the outlet node of the OA mixer.

After the system simulation is completed, the actual response from the air system is used in the generic contaminant correction step, which is shown next.

## Generic Contaminant Correction

For the correct step, the expanded air mass balance equation is solved for the final zone generic contaminant concentration at the current time step. In the same manner as described above for predicting the carbon dioxide load to be met by the air system, the zone air carbon dioxide correction calculation will be described individually for the three solution algorithms.

### EulerMethod

![](media/image125.png)\


### ThirdOrderBackwardDifference

![](media/image126.png)\


### AnalyticalSolution

![](media/image127.png)\


The above solutions are implemented in the Correct Zone Air Generic Contaminant step in the Zone Contaminant Predictor Corrector module of EnergyPlus.