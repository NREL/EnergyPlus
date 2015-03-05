# Moisture Predictor-Corrector

The transient air mass balance equation for the change in the zone humidity ratio = sum of internal scheduled latent loads + infiltration + system + multizone airflows + convection to the zone surfaces may be expressed as follows:

![](media/image50.png)\


where

C~W~= humidity capacity multiplier (See the InputOutput Reference for additional information on the object ZoneCapacitanceMultiplier:ResearchSpecial)

In the same manner as described above for zone air temperature (ref. Basis for the Zone and Air System Integration), the solution algorithms provided in the ZoneAirHeatBalanceAlgorithm object are also applied to zone air moisture calculations.

In order to calculate the derivative term with respect to time, the first order backward finite difference method, defined as the EulerMethod in the ZoneAirHeatBalanceAlgorithm object, may be used:

 ![](media/image51.png)

The zone air humidity ratio update at the current time step using the EulerMethod may be expressed as follows:

![](media/image52.png)\


To preserve the stability of the calculation of the zone humidity ratio, the third order differential approximation, derived by a Taylor Series and used in the calculation of the next time step's zone air temperature, is also applied to the zone air humidity ratio calculations. This algorithm is the default choice and is defined as 3rdOrderBackwardDifference in the ZoneAirHeatBalanceAlgorithm object.

The third order derivative derived from a Taylor Series expansion is defined as:

![](media/image53.png) .

The coefficients of the approximated derivative are very close to the coefficients of the analogous Adams-Bashforth algorithm. Then the approximated derivative is substituted into the mass balance and the terms with the humidity ratio at past time steps are all put on the right hand side of the equation. This third order derivative zone humidity ratio update increases the number of previous time steps that are used in calculating the new zone humidity ratio, and decreases the dependence on the most recent. The higher order derivative approximations have the potential to allow the use of larger time steps by smoothing transitions through sudden changes in zone operating conditions.

![](media/image54.png)\


This gives us the basic air mass balance equation that will be solved two different ways, one way for the predict step and one way for the correct step.

Since the third choice of solution algorithms uses an integration approach, defined as AnalyticalSolution in the ZoneAirHeatBalanceAlgorithm object, it does not require any approximations and has no truncation errors. The solutions in both prediction and correction are provided below in detail.

## Moisture Prediction

For the moisture prediction case the equation is solved for the anticipated system response as shown below.

![](media/image55.png)\


Since the program provides three solution algorithms, the moisture prediction from each solution algorithm is given below.

### EulerMethod

For this solution algorithm, the air mass balance for the predicted air system load or response is:

![](media/image56.png)\


### ThirdOrderBackwardDifference

For this solution algorithm, the air mass balance for the predicted system load or response is given below:

![](media/image57.png)\


Then, using the following substitutions, the air mass balance equation becomes:

![](media/image58.png)\


![](media/image59.png)\


![](media/image60.png)\


![](media/image61.png)\


### AnalyticalSolution

For this solution algorithm, the air mass balance for the predicted air system load or response is given below:

![](media/image62.png)\


At the prediction point in the simulation, the system air mass flows are not known; therefore, the system response is approximated. The predicted air system moisture load is then used in the system simulation to achieve the best results possible. The system simulation components that have moisture control will try to meet this predicted moisture load. For example, humidifiers will look for positive moisture loads and add moisture at the specified rate to achieve the relative humidity setpoint. Likewise, dehumidification processes will try to remove moisture at the specified negative predicted moisture load to meet the relative humidity setpoint.

After the system simulation is completed the actual response from the air system is used in the moisture correction of step, which is shown next.

## Moisture Correction

For the correct step the expanded air mass balance equation is solved for the final zone humidity ratio at the current time step. When the air system is operating, the mass flow for the system outlet includes the infiltration mass flow rate, therefore the infiltration mass flow rate is not included as a separate term in the air mass balance equation. But when the air system is off, the infiltration mass flow in is then exhausted out of the zone directly.

In the same manner as described above for predicting the moisture load to be met by the air system, the zone air moisture correction calculation will be described individually for the three solution algorithms.

### EulerMethod

![](media/image63.png)\


### ThirdOrderBackwardDifference

![](media/image64.png)\


Using the same A, B, and C parameters from the prediction step modified with actual zone mass flows with the air system ON and OFF result in:

**If** (ZoneSupplyAirMassFlowRate > 0.0) **Then**

![](media/image65.png)\


![](media/image66.png)\


![](media/image67.png)\


**Else If** (ZoneSupplyAirMassFlowRate <= 0.0) **Then**

![](media/image68.png)\


![](media/image69.png)\


![](media/image70.png)\


**End If**

Inserting in the parameters A, B and C above in the air mass balance equation, it simplifies to:

![](media/image71.png)\


### AnalyticalSolution

![](media/image72.png)\


The above solutions are implemented in the Correct Zone Air Humidity Ratio step in EnergyPlus. This moisture update equation is used for the Conduction Transfer Function (CTF) heat balance algorithm, in addition to the effective moisture penetration depth (EMPD) with conduction transfer function heat balance algorithm. The equations are identical except that the convection to the zone surfaces is non-zero for the moisture penetration depth case. This moisture update allows both methods to be updated in the same way, with the only difference being the additional moisture capacitance of the zone surfaces for the Effective Moisture Penetration Depth (EMPD) solution approach.

When the HAMT (Combined Heat And Moisture Finite Element) defined in the HeatBalanceAlgorithm object is applied, the moisture update equations are also the same as the equations used in the effective moisture penetration depth (EMPD) with conduction transfer function solution algorithm.