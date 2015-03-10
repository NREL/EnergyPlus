# Standard EnergyPlus Units

EnergyPlus has adopted the standard SI units for input and output.

ASCII with no spaces is used for abbreviations. Note that exponents appear without any indication of exponentiation: i.e., kg/m3 not kg/m^3 or kg/m\*\*3. Also note the use of dashes. We have W/m2-K not W/m2\*K or W/(m2\*K).

At the end we note the "problem" variables – the inputs that have non-standard units. Inputs using these units will have to be changed and the code checked to see how the quantities are used internally.

Table: Standard EnergyPlus Units

Quantity|unit|abbreviation
--------|----|------------
angular degrees|degree|deg
Length|meter|m
Area|square meter|m2
Volume|cubic meter|m3
Time|seconds|s
frequency|Hertz|Hz
temperature|Celsius|C
absolute temperature|Kelvin|K
temperature difference|Celsius|deltaC
Speed|meters per second|m/s
energy (or work)|Joules|J
Power|Watts|W
Mass|kilograms|kg
Force|Newton|N
mass flow|kilograms per second|kg/s
volume flow|cubic meters per second|m3/s
Pressure|Pascals|Pa
pressure difference|Pascals|Pa
specific enthalpy|Joules per kilogram|J/kg
Density|kilograms per cubic meter|kg/m3
heat flux|watts per square meter|W/m2
specific heat|-------|J/kg-K
conductivity|-------|W/m-K
Diffusivity|-------|m2/s
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
Viscosity|-------|kg/m-s
Dynamic Viscosity|-------|N-s/m2
Porosity|-------|m3/m3
thermal gradient coeff for moisture capacity|-------|kg/kg-K
isothermal moisture capacity|-------|m3/kg