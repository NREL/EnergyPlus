# Carbon Dioxide Predictor-Corrector

The transient air mass balance equation for the change in zone air carbon dioxide concentration may be expressed as follows:

![](media/image73.png)\


where:

![](media/image74.png)  = sum of scheduled internal carbon dioxide loads. The zone air density is used to convert the volumetric rate of carbon dioxide generation from user input into mass generation rate [kg/s].The coefficient of 10^6^ is used to make the units of carbon dioxide as ppm.

![](media/image75.png)  = carbon dioxide transfer due to interzone air mixing [ppm-kg/s]

![](media/image76.png)  = carbon dioxide concentration in the zone air being transferred into this zone [ppm]

![](media/image77.png)  = carbon dioxide transfer due to infiltration and ventilation of outdoor air [ppm-kg/s]

![](media/image78.png)  = carbon dioxide concentration in outdoor air [ppm]

![](media/image79.png)  = carbon dioxide transfer due to system supply [ppm-kg/s]

![](media/image80.png)  = carbon dioxide concentration in the system supply airstream [ppm]

![](media/image81.png)  = air system supply mass flow rate [kg/s]

![](media/image82.png) = carbon dioxide storage term in zone air [kg/s]

![](media/image83.png)  = zone air carbon dioxide concentration at the current time step [ppm]

![](media/image84.png) = zone air density [kg/m^3^]

![](media/image85.png) = zone volume [m^3^]

C~CO2~ = carbon dioxide capacity multiplier [dimensionless] (See the InputOutput Reference for additional information on the object ZoneCapacitanceMultiplier:ResearchSpecial)

In the same manner as described above for zone air temperature (ref. Basis for the Zone and Air System Integration), the solution algorithms provided in the ZoneAirHeatBalanceAlgorithm object are also applied to the zone air carbon dioxide calculations.

In order to calculate the derivative term with respect to time, the first order backward finite difference method, defined as the EulerMethod in the ZoneAirHeatBalanceAlgorithm object, may be used:

![](media/image86.png)\


The zone air carbon dioxide concentration update at the current time step using the EulerMethod may be expressed as follows:

![](media/image87.png)\


To preserve the stability of the calculation of the zone carbon dioxide concentration, the third order differential approximation, derived by a Taylor Series and used in the calculation of the next time step's zone air temperature, is also applied to the zone air carbon dioxide calculations. This algorithm is the default choice and is defined as ThirdOrderBackwardDifference in the ZoneAirHeatBalanceAlgorithm object.

The third order derivative derived from a Taylor Series expansion is defined as:

![](media/image88.png)\


The coefficients of the approximated derivative are very close to the coefficients of the analogous Adams-Bashforth algorithm. Then the approximated derivative is substituted into the mass balance and the terms with the carbon dioxide concentration at past time steps are all put on the right-hand side of the equation. This third order derivative zone carbon dioxide update increases the number of previous time steps that are used in calculating the new zone carbon dioxide concentration, and decreases the dependence on the most recent. The higher order derivative approximations have the potential to allow the use of larger time steps by smoothing transitions through sudden changes in zone operating conditions.

![](media/image89.png)\


This gives us the basic air mass balance equation that will be solved two different ways, one way for the predict step and one way for the correct step.

Since the third choice of solution algorithms uses an integration approach, defined as AnalyticalSolution in the ZoneAirHeatBalanceAlgorithm object, it does not require any approximations and has no truncation errors. The solutions in both prediction and correction are provided below in detail.

## Carbon Dioxide Prediction

For the carbon dioxide concentration prediction case, the equation is solved for the anticipated system response as shown below.

![](media/image90.png)\


Since the program provides three solution algorithms, the carbon dioxide prediction from each solution algorithm is given below.

### EulerMethod

For this solution algorithm, the air mass balance for the predicted air system load or response is:

![](media/image91.png)\


### ThirdOrderBackwardDifference

For this solution algorithm, the air mass balance for the predicted system load or response is given below:

![](media/image92.png)\


### AnalyticalSolution

For this solution algorithm, the air mass balance for the predicted air system load or response is given below:

![](media/image93.png)\


At the prediction point in the simulation, the system air mass flows are not known; therefore, the system response is approximated. The predicted air system carbon dioxide load is then used in the system simulation to achieve the best results possible. If a central HVAC system provides the outdoor flow rate from a Controller:MechanicalVentilation object, the outdoor airflow rate may be approximated as:

![](media/image94.png)\


where:

![](media/image95.png) = supply outdoor airflow rate into the controlled zone [kg/s]

The above approximation is based on the assumption that the carbon dioxide concentration at the outdoor air (OA) mixer inlet is equal to the zone air outlet concentration level, and the carbon dioxide level at the zone supply air inlet is equal to the level at the outlet node of the OA mixer.

After the system simulation is completed the actual response from the air system is used in the carbon dioxide correction step, which is shown next.

## Carbon Dioxide Correction

For the correct step the expanded air mass balance equation is solved for the final zone carbon dioxide concentration at the current time step. In the same manner as described above for predicting the carbon dioxide load to be met by the air system, the zone air carbon dioxide correction calculation will be described individually for the three solution algorithms.

### EulerMethod

![](media/image96.png)\


### ThirdOrderBackwardDifference

![](media/image97.png)\


### AnalyticalSolution

![](media/image98.png)\


The above solutions are implemented in the Correct Zone Air Carbon Dioxide step in the Zone Contaminant Predictor Corrector module of EnergyPlus.
