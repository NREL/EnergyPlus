# Standard EnergyPlus Conditions

## Standard Temperature and Pressure

Several objects specify inputs at "standard temperature and pressure". Standard Temperature  is dry air at 20^o^C drybulb. The program uses local barometric pressure to account for altitude using the equation for "standard atmospheric" pressure on p 6.1 of the ASHRAE 1997 HOF (SI edition) to initialize the air systems being simulated.

![](media/image596.png)\


where

StdPressure = pressure {Pa}

Z = altitude/elevation {m}