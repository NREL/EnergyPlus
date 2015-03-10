# Loop, Equipment Sizing and other Design Data

The importance of correct equipment sizing is often ignored in discussions of building simulation methods. The lack of reliable, efficient and flexible sizing calculations can present a serious barrier to the adoption and acceptance of building simulation programs. This section describes the sizing methodology implemented in EnergyPlus. This method includes:

A zone by zone heat balance load and air-flow calculation for multiple design days;

Significant user control with modest input requirements;

Zone, system and plant level calculations of design heating and cooling capacities and fluid flow rates;

Modular, component-specific sizing algorithms for each HVAC component.