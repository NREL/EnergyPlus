# ASHRAE 62.1-2019 Simplified Procedure
Jeremy Lerond, Wooyoung Jung, Jian Zhang, PNNL

June 2021

# Justification for New Feature
The latest version of ASHRAE Standard 62.1 introduces a new approach to calculate a multi-zone system's design outdoor air intake and corresponding minimum primary zone airflows. This new approach is a simplification of the previous method, the Ventilation Rate Procedure (VRP). Language from the standard is provided below.

![ASHRAE 62.1 Simplified Procedure](NFP-SimplifiedVRP.png)

The Simplified Procedure (SP) is required by ASHRAE Standard 90.1 to calculate the zone air flow rate in dead band when zones with thermostatic DDC controls are served by terminal boxes that re-heat and/or re-cool the air delivered to the zone (see section 6.5.2.1, exception 2.a.(1)).

EnergyPlus is currently able to calculate the design outdoor air (OA) intake of multi-zone systems based the VRP but not based the SP. EnergyPlus currently handles both multi- and single-zone VRP calculations for single- and dual-duct systems.

This new feature would allow users to let EnergyPlus size/calculate a system's outdoor air intake based on the SP and size/calculate the corresponding zone terminal minimum primary air flows.

# Approach
In order for the design system OA to be calculated following the SP, a new `System Outdoor Air Method` will be added to the `Sizing:System` object (proposed name: `SimplifiedVentilationRateProcedure`).

As shown in the previous section, minimum terminal primary air flow can (be required to) be calculated using the SP. We propose to add one of the two following items:
- Add a new `Cooling Design Air Flow Method` to the `Sizing:Zone` object (proposed name: `SimplifiedVentilationRateProcedure`)
- Add a new `Zone Minimum Air Flow Input Method` for all applicable `AirTerminal:*` objects (proposed name: `SimplifiedVentilationRateProcedure`)

The occupant diversity, `D`, is currently calculated by EnergyPlus using user-input schedules. While this might generally be convenient for users, extra care should be put into developing schedules so `D`, as calculated by EnergyPlus, match a potentially expected value. Additionally, performance might be somewhat impacted by the current implementation of that routine (see [issue 6487](https://github.com/NREL/EnergyPlus/issues/6487)). We propose to make `D` a user-input in the `Sizing:System` object. If not specified by the user the current routine would be called, the user-defined `D` would otherwise be used to calculate the system OA intake.

The system ventilation optimization control requirement in ASHRAE Standard 90.1 requires that the system OA intake be adjusted, based on changes in System Ventilation Efficiency, below the design rate when applicable. EnergyPlus is able to make this adjustment through the `Controller:MechanicalVentilation` object but does not currently checks if the calculated adjusted system OA is less than the design value. It is possible for the System Ventilation Efficiency at non-design conditions to be lower than at design conditions, this would result in a system OA intake increase which violates the building energy code requirement. We proposed to "cap" the system OA calculated by `VentilationMechanicalProps::CalcMechVentController` to the system design OA value when the VRP is selected (Note: the SP is only used for system OA sizing calculations).

## Testing/Validation/Data Sources
Unit tests covering the calculation of the SP as well as the ventilation optimization control "cap" will be added. An example file (see below) will be developed and its output checked against "manual" calculations.

## Input Output Reference Documentation
To be developed.

## Input Description
A new `System Outdoor Air Method` will be added to the `Sizing:System` object. One of the two following option will be added as well:
- Add a new `Cooling Design Air Flow Method` to the `Sizing:Zone` object (proposed name: `SimplifiedVentilationRateProcedure`)
- Add a new `Zone Minimum Air Flow Input Method` for all applicable `AirTerminal:*` objects

## Outputs Description
The existing output summary report will be used. The proposal does not include any new outputs.

## Engineering Reference
To be developed.

## Example File and Transition Changes
An example file using the SP will be developed. No transition changes are expected at this point.

# References
* ASHRAE. 2019. ANSI/ASHRAE/IES 90.1-2019, Energy Standard for Buildings Except Low-Rise
Residential Buildings. ASHRAE, Atlanta, GA

* ASHRAE. 2019. ANSI/ASHRAE 62.1-2019, Ventilation for Acceptable Indoor Air Quality. ASHRAE, Atlanta, GA