Appendix A. Units and Abbreviations
===================================

Standard EnergyPlus Conditions
------------------------------

### Standard Temperature and Pressure

Several objects specify inputs at “standard temperature and pressure”. Standard Temperature  is dry air at 20<sup>o</sup> C drybulb. The program uses local barometric pressure to account for altitude using the equation for "standard atmospheric" pressure on p 6.1 of the ASHRAE 1997 HOF (SI edition) to initialize the air systems being simulated.

<div>\[Stdpressure = 101325\cdot {\left( {1.0 - Z\cdot {{2.25577}^{ - 5}}} \right)^{5.2559}}\]</div>

where

StdPressure = pressure {Pa}

Z = altitude/elevation {m}



Standard EnergyPlus Units
-------------------------

EnergyPlus expects information in a single unit system (SI). This requires interface developers to convert user inputs from those preferred by architects and engineers into the standard metric units of EnergyPlus. EnergyPlus will not perform any units conversions and will not have any unit conversion routines.

ASCII with no spaces is used for abbreviations. Note that exponents appear without any indication of exponentiation: i.e., kg/m3 not kg/m^3 or kg/m\*\*3. Also note the use of dashes. We have W/m2-K not W/m2\*K or W/(m2\*K).

At the end we note the “problem” variables – the inputs that have non-standard units. Inputs using these units will have to be changed and the code checked to see how the quantities are used internally.

<table class="table table-striped">
  <caption>Standard EnergyPlus Units</caption>
  <tr>
    <th>Quantity</th>
    <th>unit</th>
    <th>abbreviation</th>
  </tr>
  <tr>
    <td>angular degrees</td>
    <td>degree</td>
    <td>deg</td>
  </tr>
  <tr>
    <td>Length</td>
    <td>meter</td>
    <td>m</td>
  </tr>
  <tr>
    <td>Area</td>
    <td>square meter</td>
    <td>m2</td>
  </tr>
  <tr>
    <td>Volume</td>
    <td>cubic meter</td>
    <td>m3</td>
  </tr>
  <tr>
    <td>Time</td>
    <td>seconds</td>
    <td>s</td>
  </tr>
  <tr>
    <td>Frequency</td>
    <td>Hertz</td>
    <td>Hz</td>
  </tr>
  <tr>
    <td>Temperature</td>
    <td>Celsius</td>
    <td>C</td>
  </tr>
  <tr>
    <td>absolute temperature</td>
    <td>Kelvin</td>
    <td>K</td>
  </tr>
  <tr>
    <td>temperature difference</td>
    <td>Kelvin</td>
    <td>deltaC</td>
  </tr>
  <tr>
    <td>speed</td>
    <td>meters per second</td>
    <td>m/s</td>
  </tr>
  <tr>
    <td>energy (or work)</td>
    <td>Joules</td>
    <td>J</td>
  </tr>
  <tr>
    <td>power</td>
    <td>Watts</td>
    <td>W</td>
  </tr>
  <tr>
    <td>mass</td>
    <td>kilograms</td>
    <td>kg</td>
  </tr>
  <tr>
    <td>force</td>
    <td>Newton</td>
    <td>N</td>
  </tr>
  <tr>
    <td>mass flow</td>
    <td>kilograms per second</td>
    <td>kg/s</td>
  </tr>
  <tr>
    <td>volume flow</td>
    <td>cubic meters per second</td>
    <td>m3/s</td>
  </tr>
  <tr>
    <td>pressure</td>
    <td>Pascals</td>
    <td>Pa</td>
  </tr>
  <tr>
    <td>pressure difference</td>
    <td>Pascals</td>
    <td>Pa</td>
  </tr>
  <tr>
    <td>specific enthalpy</td>
    <td>Joules per kilogram</td>
    <td>J/kg</td>
  </tr>
  <tr>
    <td>density</td>
    <td>kilograms per cubic meter</td>
    <td>kg/m3</td>
  </tr>
  <tr>
    <td>heat flux</td>
    <td>watts per square meter</td>
    <td>W/m2</td>
  </tr>
  <tr>
    <td>specific heat</td>
    <td>-------</td>
    <td>J/kg-K</td>
  </tr>
  <tr>
    <td>conductivity</td>
    <td>-------</td>
    <td>W/m-K</td>
  </tr>
  <tr>
    <td>diffusivity</td>
    <td>-------</td>
    <td>m2/s</td>
  </tr>
  <tr>
    <td>heat transfer coefficient</td>
    <td>-------</td>
    <td>W/m2-K</td>
  </tr>
  <tr>
    <td>R-value</td>
    <td>-------</td>
    <td>m2-K/W</td>
  </tr>
  <tr>
    <td>heating or cooling capacity</td>
    <td>Watts</td>
    <td>W</td>
  </tr>
  <tr>
    <td>electric potential</td>
    <td>volts</td>
    <td>V</td>
  </tr>
  <tr>
    <td>electric current</td>
    <td>Amperes</td>
    <td>A</td>
  </tr>
  <tr>
    <td>illuminace</td>
    <td>lux</td>
    <td>lx</td>
  </tr>
  <tr>
    <td>luminous flux</td>
    <td>lumen</td>
    <td>lm</td>
  </tr>
  <tr>
    <td>luminous intensity</td>
    <td>candelas</td>
    <td>cd</td>
  </tr>
  <tr>
    <td>luminance</td>
    <td>candelas per square meter</td>
    <td>cd/m2</td>
  </tr>
  <tr>
    <td>vapor diffusivity</td>
    <td>meters squared per second</td>
    <td>m2/s</td>
  </tr>
  <tr>
    <td>viscosity</td>
    <td>-------</td>
    <td>kg/m-s</td>
  </tr>
  <tr>
    <td>dynamic Viscosity</td>
    <td>-------</td>
    <td>N-s/m2</td>
  </tr>
  <tr>
    <td>thermal gradient coeff for moisture capacity</td>
    <td>-------</td>
    <td>kg/kg-K</td>
  </tr>
  <tr>
    <td>isothermal moisture capacity</td>
    <td>-------</td>
    <td>m3/kg</td>
  </tr>
</table>


