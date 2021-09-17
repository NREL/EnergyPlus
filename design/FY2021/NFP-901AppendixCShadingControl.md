# NFP ASHRAE 90.1 2019 Appendix C Shading Control

WIP

**Xuechen (Jerry) Lei, Jeremy Lerond, and Jian Zhang. PNNL**

- Original Date: 06/09/2021
- Revision Date: 09/17/2021

## Justification for New Feature

In the most recent ASHRAE Standard 90.1-2019, Appendix C - Methodology for Building Envelope Trade-Off Option in Section 5.6, there is a shading control specification:

> **C3.5.5.1 Shading**
>
> Manually operated interior shades shall be modeled on all vertical fenestration. **Shades shall be modeled to be in the lowered position when either the transmitted luminance is greater than 200 cd/ft2 or the direct solar transmitted energy exceeds 30 Btu/h·ft2 and then remain lowered for rest of the day.** Shades shall be modeled with visible light transmittance of 0.10, visible light reflectance of 0.40, solar transmittance of 0.21, and solar reflectance of 0.23. Permanent shading devices such as fins and overhangs shall be modeled.

This shading control (bolded in quote) can be summarizied / generalized as: if (transmitted luminance > threshold_x) or (direct solar transmitted energy > threshold_y), then shades should turn on and stay on till the end of the day.

Everyone who tries to adopt Appendix C in their model should be able to implement this control in a straightfoward manner without explicitly specifying the control logic through EMS related objects. Currently, EnergyPlus has various `Shading Control Type` options in the `WindowShadingControl` object (EP 9.5 I/O Reference 1.10.58) but this is not one of them.

## E-mail and Conference Call Conclusions

The current ASHRAE 90.1 narrative does not descibe sufficiently on the detail about 1) whether the luminance requirement for shading control in Appendix C is regarding a photosensor, and if it is, where should that photosensor location be; 2) what exactly should be considered as "rest of the day" in this context. The PNNL team reached out to ASHRAE 90.1 ESC (Envelope Subcommittee) and confirmed that 1) the luminance value for this control is to be based on the photosensor location defined in the daylighting reference point; 2) once the shade is lowered, it will remain down until the end of the day, i.e. midnight.

Further feedback and comments will be collected during development of this feature.

## Overview and approach

A new `Shading Control Type` option in the `WindowShadingControl` object needs to be implemented. Based on the naming convention of other options, and existing setpoints arrangements for shading control, this option is tentatively named as `OnIfHighLuminOrHighSolarTillMidnight`.

Two setpoints need to be specificd for this shading control option in the idf file. There are two existing setpoints fields in `WindowShadingControl` object:

- Setpoint: units depend on the type of trigger:
  - W/m2 for solar-based controls
  - W for cooling- or heating-based controls
  - Degrees C for temperature-based controls
  - This setpoint is not used for DaylightIlluminance control
- Setpoint2: only used for certain two-setpoint control types, in all existing cases, the second setpoint is for Solar:
  - `OnIfHighOutdoorAirTempAndHighSolarOnWindow`
  - `OnIfHighOutdoorAirTempAndHighHorizontalSolar`
  - `OnIfHighZoneAirTempAndHighSolarOnWindow`
  - `OnIfHighZoneAirTempAndHighHorizontalSolar`

Based on the current setpoints setup, for the new control option, direct solar transmitted energy can be directly specified in `Setpoint2`, while the specification of luminance setpoint may be implemented thorugh one the the following the similar approach as another existing window shading control option: `MeetDaylightIlluminanceSetpoint`. In this existing control option, it is only for `ShadingType = SwitchableGlazing`, and the illuminance setpoint used by this control is specified in the `Daylighting:Control` object for the Zone (daylight illuminance set point at the first daylighting reference point), not `WindowShadingControl` `Setpoint`.

To add a setpoint for luminance based control in the option to be implemented, we will be using `Daylighting:Control` object for the Zone luminance setpoint. This requires adding one more field for luminance setpoint in the `Daylighting:Control` object and expanding the application of `Daylighting:Control` object as it was dedicated for illuminance based control in its current version.

## Design document - Adding a new window shading control method "OnIfHighLuminOrHighSolarTillMidnight"

For **Initializing Simulation**

1. Around `EnergyPlus\src\EnergyPlus\DaylightingManager.cc(4326)`, check if the zone using this new control method has daylighting reference points.
2. Around `EnergyPlus\src\EnergyPlus\SolarShading.cc(9704)`, add a case for the new control method.
3. Around `\EnergyPlus\src\EnergyPlus\HeatBalanceSurfaceManager.cc(1031)`, add the new method in `WindowShadingControlTypeStr` vector.

For **Daylighting Factors calculation**

1. Add luminance based shading control logic code in `energyplusapi.dll!EnergyPlus::DaylightingManager::DayltgInteriorIllum(EnergyPlus::EnergyPlusData & state, int & ZoneNum)` (or add a new method dedicated for luminance based control and call it from `\EnergyPlus\src\EnergyPlus\HeatBalanceSurfaceManager.cc` ) at `EnergyPlus\src\EnergyPlus\DaylightingManager.cc`, following the same coding pattern as the `WindowShadingControlType::MeetDaylIlumSetp`
2. Luminance value to be used in checking the requirement will be `LumWinFromRefPtRep`. (This value is reported as an output variable and its illuminance counterpart is used in the illuminance based shading control (Reference in `DaylightingManager.cc`))
3. Solar based control for this logic will be added following code patterns for "HiSolar" related window shading options in `EnergyPlus\src\EnergyPlus\SolarShading.cc`.
4. The logic of keeping the shade done until end of day (midnight) is to be implemented either in Item 1 or 2 above, together with related logic.

## Testing/Validation/Data Sources

A test case model to use this new shading control option and the result shading status time series and related solar gain and illuminance value time series are to be compared to verify the shading is controlled as designed.

## Input Output Reference Documentation

To be added according to overview.

## Input Description

- Add a new `WindowShadingControl` option: `OnIfHighLuminOrHighSolarTillMidnight`
- Add a field for `Daylighting:Control` object to specify the luminance set point for shading control

## Outputs Description

N/A

## Engineering Reference

To be added according to overview.

## Example File and Transition Changes

Instance of this new shading control method will be added to the existing window testing idf `WindowTests.idf`. No transition change is expected.

## References

- ASHRAE. 2019. ANSI/ASHRAE/IES 90.1-2019, Energy Standard for Buildings Except Low-Rise
  Residential Buildings. ASHRAE, Atlanta, GA
- [Add additional option for shading control for WindowProperty:ShadingControl object #7081](https://github.com/NREL/EnergyPlus/issues/7081)
