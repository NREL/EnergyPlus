Input Changes
=============

This file documents the structural changes on the input of EnergyPlus that could affect interfaces, etc.
This was previously an Excel workbook that made for very difficult version control, especially during busy times around code freezes.

# Object Change: `ThermalStorage:Ice:Detailed`
Fields A5 and A7 change description to Discharging Curve Variable Specifications and Charging Curve Variable Specifications, respectively.  The old options for these fields were either QuadraticLinear or CubicLinear.  For A5, if the field previously had QuadraticLinear, it should be changed to PercentDischargedLMTD.  If A5 was defined as CubicLinear, it should be changed to LMTDMassFlow.  For A7, if the field had previously been QuadraticLinear, it should not say PercentChargedLMTD.  If A7 was CubicLinear, it should be changed to LMTDMassFlow rate as shown above for A5.

# Object Change: `ObjectNameB`

# Object Change: `ObjectNameC`

# Object Change: `ObjectNameD`

# Object Change: `ObjectNameE`

# Object Change: `ObjectNameF`

