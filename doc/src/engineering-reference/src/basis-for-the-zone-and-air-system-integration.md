# Basis for the Zone and Air System Integration

The basis for the zone and air system integration is to formulate energy and moisture balances for the zone air and solve the resulting ordinary differential equations using a predictor-corrector approach. The formulation of the solution scheme starts with a heat balance on the zone air.

![](media/image9.png)\


where:

![](media/image10.png)  = sum of the convective internal loads

![](media/image11.png)  = convective heat transfer from the zone surfaces

![](media/image12.png)  = heat transfer due to infiltration of outside air

![](media/image13.png)  = heat transfer due to interzone air mixing

$dot{Q}_{sys}$ = air systems output

![](media/image15.png) energy stored in zone air

C~z~~~= ρ~air~C~p~C~T~

ρ~air~~~= zone air density

C~p~~~= zone air specific heat

C~T~~~= sensible heat capacity multiplier (Detailed description is provided below)

If the air capacitance is neglected, the steady-state system output must be:

![](media/image16.png)\


Air systems provide hot or cold air to the zones to meet heating or cooling loads. The system energy provided to the zone, ![](media/image17.png) , can thus be formulated from the difference between the supply air enthalpy and the enthalpy of the air leaving the zone as in Equation :

![](media/image18.png)\


This equation assumes that the zone supply air mass flow rate is exactly equal to the sum of the air flow rates leaving the zone through the system return air plenum and being exhausted directly from the zone. Both air streams exit the zone at the zone mean air temperature. The result of substituting Equation  for ![](media/image19.png)  in the heat balance Equation  is shown in Equation :

![](media/image20.png)\


The sum of zone loads and air system output now equals the change in energy stored in the zone. Typically, the capacitance Cz would be that of the zone air only. However, thermal masses assumed to be in equilibrium with the zone air could be included in this term.

EnergyPlus provides three different solution algorithms to solve the zone air energy and moisture balance equations. These are defined in the Algorithm field in the ZoneAirHeatBalanceAlgorithm object: 3rdOrderBackwardDifference, EulerMethod and AnalyticalSolution. The first two methods to solve Equation  use the finite difference approximation while the third uses an analytical solution. A short description is given below.

In order to calculate the derivative term with respect to time, a finite difference approximation may be used, such as:

![](media/image21.png)\


The use of numerical integration in a long time simulation is a cause for some concern due to the potential build-up of truncation error over many time steps. In this case, the finite difference approximation is of low order that further aggravates the problem. However, the cyclic nature of building energy simulations should cause truncation errors to cancel over each daily cycle so that no net accumulation of error occurs, even over many days of simulation (Walton, 1990). The Euler formula, Equation , was employed in Equation  to replace the derivative term. All the terms containing the zone mean air temperature were then grouped on the left hand side of the equation.  Since the remaining terms are not known at the current time, they were lagged by one time step and collected on the right hand side. This manipulation resulted in Equation , the formula for updating the zone mean air temperature:

![](media/image22.png)\


One final rearrangement was to move the lagged temperature in the derivative approximation to the right side of the equation. The explicit appearance of the zone air temperature was thus eliminated from one side of the equation. An energy balance equation that includes the effects of zone capacitance was then obtained by dividing both sides by the coefficient of Tz:

![](media/image23.png)\


Equation  could be used to estimate zone air temperatures, and is defined as the EulerMethod, one of the three solution algorithms provided in the ZoneAirHeatBalanceAlgorithm object. However, it can severely limit the time step size under some conditions. To improve on this, higher order expressions for the first derivative, with corresponding higher-order truncation errors, were developed. The goal of this approach was to allow for the use of larger time steps in the simulation than would be possible using the first order Euler form, without experiencing instabilities. Approximations from second through fifth order were tried as reported by Taylor, et al. (1990) with the conclusion that the third order finite difference approximation, shown below, gave the best results:

![](media/image24.png)\


When this form for the derivative is used, equation  changes to:

![](media/image25.png)\


and the zone temperature update equation becomes:

![](media/image26.png)\


This is the form historically used in EnergyPlus and is the current default referred to as 3rdOrderBackwardDifference in the ZoneAirHeatBalanceAlgorithm object. This algorithm requires zone air temperatures at three previous time steps and uses constant temperature coefficients. The assumption is that three previous time steps lengths are the same.

The AnalyticalSolution algorithm is an integration approach. While the 3^rd^ order finite difference approximation provides stability without requiring a prohibitively small time step, the method still has truncation errors and requires a fixed time step length for the previous three simulation time steps. Therefore, different time step lengths for the previous three simulation time steps may make the temperature coefficients invalid.

The AnalyticalSolution algorithm provides a possible way to obtain solutions without truncation errors and independent of time step length. In addition, the algorithm only requires the zone air temperature for one previous time step, instead of three previous time steps as required by the 3rdOrderBackwardDifference algorithm. The integrated (analytical) solution for Eq. (4) may be expressed as follows:

![](media/image27.png)\


Since the load on the zone drives the entire process, that load is used as a starting point to give a demand to the air system. Then a simulation of the air system provides the actual supply capability and the zone temperature is adjusted if necessary. This process in EnergyPlus is referred to as a Predictor/Corrector process. It is summarized below.

~~~~~~~~~~~~~~~~~~~~

    Code Reference: the ZoneTempPredictorCorrector module performs the calculations.
~~~~~~~~~~~~~~~~~~~~

## Zone Sensible Heat Capacity Multiplier

If the Zone Sensible Heat Capacity Multiplier = 1.0, this represents just the sensible heat capacitance of the air volume in the specified zone. If the value is not defined, it is set to 1.0. This multiplier can be greater than 1.0 if the zone air sensible heat capacity needs to be increased for stability of the simulation. This multiplier increases the capacitance of the air volume by increasing the zone volume that is used in the zone predictor-corrector algorithm in the simulation. This can be done for numerical reasons, such as to increase the stability by decreasing the zone air temperature deviations at the time step level. Or it can be increased to try and account for the additional capacitance in the air loop not specified in the zone, i.e. dampers, diffusers, duct work, etc., to see the effect on the dynamics of the simulation. See the Input/Output Reference for additional information (Object: ZoneCapacitanceMultiplier:ResearchSpecial).

In the source code below we see how the ZoneVolCapMultpSens increases the zone volume used for the air ratio at the time step in the air system. This multiplier is constant throughout the simulation.

~~~~~~~~~~~~~~~~~~~~

        AIRRAT(ZoneNum) = Zone(ZoneNum)%Volume*ZoneVolCapMultpSens* &
                   PsyRhoAirFnPbTdbW(OutBaroPress,MAT(ZoneNum),ZoneAirHumRat(ZoneNum))* &
                   PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MAT(ZoneNum))/(TimeStepSys*SecInHour)
~~~~~~~~~~~~~~~~~~~~
