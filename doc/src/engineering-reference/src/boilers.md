# Boilers

## Simple Hot Water Boiler

The input object Boiler:HotWater provides a simple model for boilers that only requires the user to supply the nominal boiler capacity and thermal efficiency. An efficiency curve can also be used to more accurately represent the performance of non-electric boilers but is not considered a required input. The fuel type is input by the user for energy accounting purposes.

The model is based the following three equations

![](media/image2765.png)\


![](media/image2766.png)\


![](media/image2767.png)\


-or-

![](media/image2768.png)\


The final equation above includes the impact of the optional boiler efficiency performance curve. To highlight the use of the normalized boiler efficiency curve, the fuel use equation is also shown in an expanded format. The normalized boiler efficiency curve represents the changes in the boiler's nominal thermal efficiency due to loading and changes in operating temperature. If the optional boiler efficiency curve is not used, the boiler's nominal thermal efficiency remains constant throughout the simulation (i.e., BoilerEfficiencyCurveOutput = 1).

When a boiler efficiency performance curve is used, any valid curve object with 1 or 2 independent variables may be used. The performance curves are accessed through EnergyPlus' built-in performance curve equation manager (curve objects). The linear, quadratic, and cubic curve types may be used when boiler efficiency is soley a function of boiler loading, or part-load ratio (PLR). These curve types are used when the boiler operates at the specified setpoint temperature throughout the simulation. Other curve types may be used when the boiler efficiency can be represented by both PLR and boiler operating temperature. Examples of valid single and dual independent variable equations are shown below. For all curve types, PLR is always the x independent variable. When using curve types with 2 independent variables, the boiler water temperature (Twater) is always the y independent variable and can represent either the inlet or outlet temperature depending on user input.

### Single independent variable:

![](media/image2769.png) ![](media/image2770.png) Linear

![](media/image2771.png) Quadratic

![](media/image2772.png) Cubic

### Dual independent variables:

**![](media/image2773.png)** **QuadraticLinear**

![](media/image2774.png) Biquadratic

![](media/image2775.png) Bicubic

When a boiler efficiency curve is used, a constant efficiency boiler may be specified by setting C1 = 1 and all other coefficients to 0. A boiler with an efficiency proportional to part-load ratio or which has a non-linear relationship of efficiency with part-load ratio will typically set the coefficients of a linear, quadratic, or cubic curve to non-zero values. Using other curve types allows a more accurate simulation when boiler efficiency varies as a function of part-load ratio and as the boiler outlet water temperature changes over time due to loading or as changes occur in the water temperature setpoint.

The parasitic electric power is calculated based on the user-defined parasitic electric load and the operating part load ratio calculated above. The model assumes that this parasitic power does not contribute to heating the water.

![](media/image2776.png)\


where:

![](media/image2777.png) = parasitic electric power (W), average for the simulation time step

![](media/image2778.png)     = parasitic electric load specified by the user (W)

## Steam Boiler

### Description of Model

A steam boiler is the essential part of a building steam heating system and can be described as primary driver of the steam loop.  It is the component that maintains the desired loop temperature.

The emphasis in EnergyPlus was laid on developing a building simulation model for steam boiler with ability to model detailed boiler performance without the cost of exhaustive user inputs to the boiler model.  The Boiler:Steam input object is used on the plant loop supply side of EnergyPlus with the primary purpose of supplying steam to the heating coils, which constitute the demand side of the loop.

The steam boiler is a variable mass flow rate device.  The mass flow rate of steam through the boiler is determined by the heating demand on the loop which in turn is determined by the equipment that is hooked to the demand side of the loop, namely the steam coils and hot water heater.  In short, the steam coil determines the mass flow rate of steam required for heating the zone to its required setpoint, the mixer sums up the total steam demanded by each of the individual coils and reports it to the boiler via the pump.

![Schematic of Steam Boiler in the Steam loop](media/schematic-of-steam-boiler-in-the-steam-loop.jpeg)


Figure 158 describes the rudimentary loop structure with steam flowing from coils to boiler.  It is essential to mention that it is the coils that determine the mass of steam required and the boiler simply delivers the required mass flow at desired temperature provided it is adequately sized.  The algorithm for determining the mass flow rate is structured on the demand side and the variable flow boiler has no role to play in determining the steam mass flow.

Figure 159 outlines the simple steam boiler model.  Sub cooled water enters the variable flow boiler through the pump, the boiler inputs energy to water stream consuming fuel, boiler losses are accounted via boiler efficiency.  The boiler delivers steam at a quality equal to 1.0 at saturated condition.

The advantage of steam heating systems over hot water is the high latent heat carrying capacity of steam, which reduces the mass flow rate of the fluid required.  The amount of superheated and sub cooled heat transfer in Steam heating systems is negligible, latent heat transfer accounts for almost all of the heat exchange into the zones via steam to air heat exchangers.

![Schematic of Steam Boiler Operation](media/schematic-of-steam-boiler-operation.jpeg)


Boiler Load is a summation of sensible and latent heat addition to the water stream as described with the following equation.  The mass flow rate through the boiler is known, while delta temp is the temperature difference between the boiler inlet and boiler outlet.  Latent heat of steam is calculated at loop operating temperature.

![](media/image2781.png)\


Theoretical fuel used is calculated with the following equation.  Boiler efficiency is a user input and accounts for all the losses in the steam boiler.

![](media/image2782.png)\


The operation part load ratio is calculated with the following equation.  This is later used to calculate the actual fuel consumption, its ratio of boiler load to boiler nominal capacity.

![](media/image2783.png)\


The actual fuel consumption by the boiler is calculated as using the following equation, where C1, C2, and C3 are the Part Load Ratio coefficients.

![](media/image2784.png)\


Essentially the boiler model provides a first order approximation of performance for fuel oil, gas, and electric boilers.  Boiler performance is based on theoretical boiler efficiency and a single quadratic fuel use-part load ratio curve represented in the equation above.  This single curve accounts for all combustion inefficiencies and stack losses.

The control algorithm for a steam boiler is an important issue.  The user may want the boiler to be undersized and in such a case it will not be able to meet the demand side steam flow request.  Subsequently the boiler load exceeds the boiler nominal capacity.  The boiler operates at its nominal capacity but is unable to meet the plant heating demand.  Pseudo code from EnergyPlus has been used to describe the control logic used in the steam boiler simulation.

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*PSEUDO CODE SECTION STARTS\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

At start of simulation an initial value of steam mass flow rate is calculated.  This is required to start the flow of steam around the loop.

- ![](media/image2785.png) 

- Calculate the boiler supply steam mass flow rate at start of simulation.
- ![](media/image2786.png) 

- ![](media/image2787.png)    ! Not first time through

- Steam boiler calculations rely heavily on the variable ![](media/image2788.png) ~b~, boiler mass flow rate.  This variable ![](media/image2789.png) ~b~ is the assigned equal to mass flow at boiler inlet node for preliminary calculations. 
- ![](media/image2790.png) 
- Calculating the boiler delta temperature difference between the inlet and outlet nodes.  This calculation is used to determine various boiler control situation.
- ![](media/image2791.png) 
- In case the temperature difference calculated with the previous equation equation  is zero then the boiler just needs to supply latent heat to steam, else the boiler performs its normal load calculations by providing both sensible and latent heat to the inlet stream.
- ![](media/image2792.png) 

- ![](media/image2793.png) 

- ![](media/image2794.png) 

- ![](media/image2795.png) 

- ![](media/image2796.png) 
- Sometimes the boiler load Q~B~ is greater than the demand side requested load at the current time step, which may occur because the boiler inlet conditions is from previous time step.  There is sudden fall in request of steam mass flow from the demand side.  The boiler now recalculates its new mass flow and adjusts to these new conditions.
- ![](media/image2797.png) 

- Boiler load is set equal to the new boiler heating demand and steam mass flow rate is recalculated.
- ![](media/image2798.png) 
- ![](media/image2799.png) 

- ![](media/image2800.png) 
- In case the requested load exceeds the boiler nominal capacity, which is its maximum heating capacity.  In this case the requested steam mass flow is not met and the zone is not heated adequately.  This happens if the boiler is undersized.  The steam mass flow rate is recalculated at nominal capacity.
- ![](media/image2801.png) 

- Boiler load is set equal to boiler nominal capacity and steam mass flow rate recalculated.
- ![](media/image2802.png) 
- ![](media/image2803.png) 

- ![](media/image2804.png) 

- ![](media/image2805.png) 

End If statement for the boiler load control algorithm.  This algorithm determines all possible control conditions that might while simulating a system in EnergyPlus.

\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*PSEUDO CODE SECTION ENDS\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

If the boiler operating pressure exceeds the maximum allowable boiler pressure, the simulation trips and outputs a warning regarding the same.  This notifies the user about potential system pressure sizing problems.

Integration of the steam boiler simulation model in EnergyPlus required developing number of subroutines, which operate in sequence.  These subroutines are designed to read inputs from the input file, initialize the variables used in the boiler simulation model, simulate the boiler performance, update the node connections, and report the required variables.  In case the user has difficulty with boiler inputs, provisions have been made to auto size the boiler nominal capacity and maximum steam flow rate.  These two values play an important role in sizing the boiler.

### Model Assumptions

The EnergyPlus boiler model is "simple" in the sense that it requires the user to supply the theoretical boiler efficiency.  The combustion process is not considered in the model.  The model is independent of the fuel type, which is input by the user for energy accounting purposes only.  This is an ideal model for Building Simulation Program such that it utilizes the desired amount of resources in terms of simulation run time, but successfully provides fairly good sizing parameters for an actual boiler.

It is assumed that the steam boiler operates to maintain a desired temperature, the temperature being saturation temperature of steam and corresponding to this saturation temperature there exist a single value of saturation pressure at which the loop operates.  Hence the boiler could either be saturation pressure controlled or temperature controlled.  Since users would have better idea of steam temperatures rather than pressure the boiler inputs are designed for temperature control.

### Nomenclature for Steam Loop

Table: Steam Loop Nomenclature

-------------------------|--------------------------
![](media/image2806.png) |Boiler Heat Transfer.  W.
![](media/image2807.png) |Boiler Nominal Capacity.  W.
![](media/image2808.png) |Boiler Operating Part Load Ratio.
![](media/image2809.png) |Degree of subcooling in coil.
![](media/image2810.png) |Temperature difference across the steam boiler.  ºC.
![](media/image2811.png) |Density of condensate entering the pump.  Kg/m3.
![](media/image2812.png) |Design Load on the steam coil.  W.
![](media/image2813.png) |Enthalpy of fluid at point n on the Ts diagram.  J/kg.
![](media/image2814.png) |Fraction of Pump Full Load Power.  W.
![](media/image2815.png) |Fractional Motor Power Lost to Fluid.  W.
![](media/image2816.png) |Heating load on the Air Loop Steam Coil.  W.
![](media/image2817.png) |Heating load on the Zone Steam Coil.  W.
![](media/image2818.png)  |Latent heat of steam at Loop operating Temperature.  J/kg.
![](media/image2819.png) |Latent Heat of Steam.  J/kg.
![](media/image2820.png) |Latent Heat Part of the Heating Coil Load.  W.
![](media/image2821.png) |Loop losses in steam coil.  W.
![](media/image2822.png) |Loop Temperature Difference.
![](media/image2823.png) |Mass flow rate for steam coil Kg/s.
![](media/image2824.png) |Mass flow rate of steam entering the steam coil .Kg/s.
![](media/image2825.png) |Mass flow rate of steam for Air loop steam coil Kg/s
![](media/image2826.png) |Mass flow rate of steam for zone steam coil Kg/s.
![](media/image2827.png) |Mass flow rate of steam.  Kg/s.
![](media/image2828.png) |Mass flow rate of steam for the steam loop.  Kg/s.
![](media/image2829.png) |Mass of condensate entering the pump.  Kg/s.
![](media/image2830.png)  |Maximum allowed mass flow rate of air.  Kg/s
![](media/image2831.png) |Maximum Mass flow rate of steam Kg/s
![](media/image2832.png) |Maximum steam mass flow rate supplied by boiler.  Kg/s.
![](media/image2833.png) |Maximum Volume flow rate of condensate in pump.  m^3^/s.
![](media/image2834.png) |Maximum Volume flow rate of condensate in steam loop.  m^3^/s.
![](media/image2835.png) |Minimum inlet air temperature possible.  ºC.
![](media/image2836.png) |Nominal Power Capacity for condensate pump.  W.
![](media/image2837.png) |Nominal power of the pump.  W.
![](media/image2838.png) |Nominal Pump Head.  M.
![](media/image2839.png) |Nominal volume flow rate through the condensate pump.  m^3^/s.
![](media/image2840.png) |Part Load Ratio for condensate pump.
![](media/image2841.png) |Pump efficiency.
![](media/image2842.png) |Pump Motor Efficiency.
![](media/image2843.png) |Pump Power.  W.
![](media/image2844.png) |Sensible Heat Part of the Heating Coil Load.  W.
![](media/image2845.png) |Setpoint Temperature of the zone.  ºC.
![](media/image2846.png) |Setpoint air outlet temperature for the steam coil.  ºC.
![](media/image2847.png) |Shaft power of the pump.  W.
![](media/image2848.png) |Specific Heat Capacity for Air.  J/Kg K.
![](media/image2849.png) |Specific Heat Capacity for Water.  J/Kg K.
![](media/image2850.png) |Steam Boiler Efficiency.
![](media/image2851.png) |Temperature of air entering the coil.  ºC.
![](media/image2852.png) |Temperature of air entering the steam coil.  ºC.
![](media/image2853.png) |Temperature of air leaving the coil.  ºC.
![](media/image2854.png) |Temperature of steam entering the coil.  ºC.
![](media/image2855.png) |Theoretical Fuel Consumption by the Steam Boiler.  W.
![](media/image2856.png) |Total Mass flow rate requested by all the steam coils.  Kg/s.
![](media/image2857.png) |Volume of condensate entering the pump.  m^3^/s.
![](media/image2858.png) |Water outlet temperature from pump.  ºC.

### References

ASHRAE Handbook. 1996. HVAC Systems and Equipment, Air Conditioning and Heating Systems.  Chapter 10, Steam Systems.  pp.  **10.1-10.16. 1996.

*BLAST 3.0 Users Manual*. 1999. Building Systems Laboratory.  Urbana-Champaign: Building Systems Laboratory, Department of Mechanical and Industrial Engineering, University of Illinois.

Chillar, R.J. 2005. "Development and Implementation of a Steam Loop In The Building Energy Simulation Program EnergyPlus," M.S. Thesis, Department of Mechanical and Industrial Engineering, University of Illinois at Urbana-Champaign.

*TRNSYS 16 User Manual*. 2004. A Transient System Simulation Program. Solar Energy Laboratory, Madison. University of Wisconsin-Madison.

El-Wakil, M. M. 1984. Power Plant Technology, McGraw Hill, New York, pp.   30-72.

Babcock & Wilcox. 1978. Steam-Its Generation and Use, The Babcock & Wilcox Company, New York ,Section I, II, IV, and VII.

S.A. Klein. 2004. Engineering Equation Solver EES. University of Wisconsin Madison.