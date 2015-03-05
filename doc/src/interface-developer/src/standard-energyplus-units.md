# Standard EnergyPlus Units

EnergyPlus expects information in a single unit system (SI).  This requires interface developers to convert user inputs from those preferred by architects and engineers into the standard metric units of EnergyPlus.  EnergyPlus will not perform any units conversions and will not have any unit conversion routines.

ASCII with no spaces is used for abbreviations.  Note that exponents appear without any indication of exponentiation: i.e., kg/m3 not kg/m^3 or kg/m\*\*3.  Also note the use of dashes.  We have W/m2-K not W/m2\*K or W/(m2\*K).

At the end we note the "problem" variables – the inputs that have non-standard units. Inputs using these units will have to be changed and the code checked to see how the quantities are used internally.

Table: Standard EnergyPlus Units

Quantity|unit|abbreviation
--------|----|------------
angular degrees|degree|deg
Length|meter|m
Area|square meter|m2
Volume|cubic meter|m3
Time|seconds|s
Frequency|Hertz|Hz
Temperature|Celsius|C
absolute temperature|Kelvin|K
temperature difference|Kelvin|delK
speed|meters per second|m/s
energy (or work)|Joules|J
power|Watts|W
mass|kilograms|kg
force|Newton|N
mass flow|kilograms per second|kg/s
volume flow|cubic meters per second|m3/s
pressure|Pascals|Pa
pressure difference|Pascals|delPa
specific enthalpy|Joules per kilogram|J/kg
density|kilograms per cubic meter|kg/m3
heat flux|watts per square meter|W/m2
specific heat|-------|J/kg-K
conductivity|-------|W/m-K
diffusivity|-------|m2/s
heat transfer coefficient|-------|W/m2-K
R-value|-------|m2-K/W
heating or cooling capacity|Watts|W
electric potential|volts|V
electric current|Amperes|A
illuminace|lux|lx
luminous flux|lumen|lm
luminous intensity|candelas|cd
luminance|candelas per square meter|cd/m2
vapor diffusivity|m2/s|
viscosity|-------|kg/m-s
porosity|-------|m3/m3
thermal gradient coeff for moisture capacity|-------|kg/kg-K
isothermal moisture capacity|-------|m3/kg