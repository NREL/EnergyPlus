# Air System Humidifiers

## Overview

Air system humidifiers are components that add moisture to the supply air stream. They fall into 2 broad categories: spray type humidifiers which act like direct evaporative coolers, cooling the supply air as well as humidifying it; and dry steam humidifiers, which humidify the supply air stream while causing almost no change to the supply air stream temperature. The EnergyPlus electric steam humidifier uses electrical energy to convert ordinary tap water to steam which it then injects into the supply air stream by means of a blower fan. The actual unit might be an electrode-type humidifier or a resistance-type humidifier.

## Electric Steam Humidifier

The electric steam humidifier model (object name: Humidifier:Steam:Electric) is based on moisture and enthalpy balance equations plus standard psychrometric relationships. The approach is similar to that taken in the ASHRAE HVAC 2 Toolkit, page 4-112 (ASHRAE 1993). EnergyPlus contains its own module of psychrometric routines; the psychrometric theory and relations are given in the 2001 edition of ASHRAE Fundamentals, Chapter 6 (ASHRAE 2001). The model contains both an ideal controller and the component. The control model assumes that there is a minimum humidity setpoint on the component air outlet node. This setpoint is established by a setpoint manager described elsewhere.

### Model

The component model is a forward model: its inputs are its inlet conditions; its outputs are its outlet conditions and its energy consumption. The inputs are the temperature, humidity ratio, and mass flow rate of the inlet air stream, which are known; and the water addition rate (kg/s) which is determined by the controller.

### Controller

The controller first decides whether the humidifier is on or off. For the humidifier to be on:

- the humidifier schedule value must be nonzero;
- the inlet air mass flow must be greater than zero;
- the inlet air humidity ratio must be less than the minimum humidity ratio setpoint.

If the humidifier is off, the water addition rate is set to zero. If the humidifier is on, the water addition rate needed to meet the humidity setpoint is calculated.

![](media/image5379.png)\


where

![](media/image5380.png)  = the air mass flow rate [kg/s]

![](media/image5381.png) = the inlet air humidity ratio [kg/kg]

![](media/image5382.png)  = water addition rate needed to meet the setpoint [kg/s]

![](media/image5383.png) = the humidity ratio setpoint [kg/kg]

Equation  is the moisture balance equation for the component. It is solved for ![](media/image5384.png)  (the other variables are known) which is passed to the humidifier component model as its desired inlet water addition rate.

### Component

The inputs to the component model are the air inlet conditions and mass flow rate and the water addition rate set by the controller. The outputs are the air outlet conditions. First the desired water addition rate is checked against component capacity.

![](media/image5385.png)\


where

![](media/image5386.png)  = the humidifier nominal capacity [kg/s], a user input.

If ![](media/image5387.png)  is zero, the outlet conditions are set to the inlet conditions and the water addition rate is set to zero. If the humidifier is scheduled on the component power consumption is set to the standby power consumption: ![](media/image5388.png) . Otherwise ![](media/image5389.png)  = 0.

If ![](media/image5390.png)  > 0, then the moisture and enthalpy balance equations

![](media/image5391.png)\


![](media/image5392.png)\


with ![](media/image5393.png)  set equal to ![](media/image5394.png)  are solved for ![](media/image5395.png)  and ![](media/image3284.png) . Here

![](media/image5396.png)  = the air mass flow rate [kg/s]

![](media/image5381.png) = the inlet air humidity ratio [kg/kg]

![](media/image5397.png)  = the inlet water addition rate [kg/s]

![](media/image5395.png)  = the outlet air humidity ratio [kg/kg]

![](media/image3276.png)  = the inlet air specific enthalpy [J/kg]

![](media/image5398.png)  = the steam specific enthalpy = 2676125.  [J/kg] at 100 ^o^C

![](media/image3284.png)  = the outlet air specific enthalpy [J/kg]

The outlet temperature is obtained from

![](media/image5399.png)\


where

![](media/image3815.png)  = outlet air temperature [^o^C],

![](media/image5400.png)  is an EnergyPlus psychrometric function.

The humidity ratio at saturation at the outlet temperature is

![](media/image5401.png)\


where

![](media/image5402.png)  = the barometric pressure [Pa],

1.0 is the relative humidity at saturation,

![](media/image5403.png)  is an EnergyPlus psychrometric function.

IF ![](media/image5404.png)  then the outlet condition is below the saturation curve and the desired moisture addition rate can be met. ![](media/image5405.png)  is set to ![](media/image5406.png)  and the calculation of outlet conditions is done. But if ![](media/image5407.png)  then it is assumed that this condition will be detected and the steam addition rate throttled back to bring the outlet conditions back to the saturation condition. We need to find the point where the line drawn between state 1 (inlet) and state 2 (our desired outlet) crosses the saturation curve. This will be the new outlet condition. Rather than iterate to obtain this point, we find it approximately by solving for the point where 2 lines cross: the first drawn from state 1 to state 2, the second from ![](media/image5408.png) , ![](media/image5409.png)  to ![](media/image5410.png) , ![](media/image5411.png) ; where

![](media/image5408.png)  is the inlet temperature [^o^C],

![](media/image5412.png)  is the humidity ratio at saturation at temperature ![](media/image5408.png)  [kg/kg],

![](media/image5410.png)  is the desired outlet temperature [^o^C],

![](media/image5413.png)  is the humidity ratio at saturation at temperature ![](media/image5410.png)  [kg/kg].

The 2 lines are given by the equations:

![](media/image5414.png)\


![](media/image5415.png)\


Solving for the point (state 3) where the lines cross:

![](media/image5416.png)\


![](media/image5417.png)\


This point isn't quite on the saturation curve since we made a linear approximation of the curve, but the temperature should be very close to the correct outlet temperature. We will use this temperature as the outlet temperature and move to the saturation curve for the outlet humidity and enthalpy. Thus we set ![](media/image3815.png)  = ![](media/image5418.png)  and

![](media/image5419.png)\


![](media/image5420.png)\


where ![](media/image5421.png)  is an EnergyPlus psychrometric function. The water addition rate is set to

![](media/image5422.png)\


We now have the outlet conditions and the adjusted steam addition rate for the case where the desired outlet humidity results in an outlet state above the saturation curve.

Finally, the electricity consumption is given by

![](media/image5423.png)\


where

![](media/image5424.png)  = nominal fan power [W], a user input,

![](media/image5425.png)  = standby power [W], a user input.

and the water consumption rate is

![](media/image5426.png)\


where

![](media/image5427.png)  = the water consumption rate [m^3^],

![](media/image5428.png)  = water density (998.2 kg/m^3^).

### References

ASHRAE. 1993. HVAC 2 Toolkit: A Toolkit for Secondary HVAC System Energy Calculations. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

ASHRAE. 2001. 2001 ASHRAE Handbook Fundamentals. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.