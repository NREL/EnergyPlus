# NFP ASHRAE 90.1 2019 Appendix C Shading Control

**Xuechen (Jerry) Lei, Jeremy Lerond, and Jian Zhang. PNNL**

- Original Date: 06/09/2021
- Revision Date: 11/29/2021

## Justification for New Feature

In "ASHRAE Standard 90.1-2019, Appendix C - Methodology for Building Envelope Trade-Off Option in Section 5.6", there is a shading control requirement:

> **C3.5.5.1 Shading**
>
> Manually operated interior shades shall be modeled on all vertical fenestration. **Shades shall be modeled to be in the lowered position when either the transmitted luminance is greater than 200 cd/ft2 or the direct solar transmitted energy exceeds 30 Btu/h·ft2 and then remain lowered for rest of the day.** Shades shall be modeled with visible light transmittance of 0.10, visible light reflectance of 0.40, solar transmittance of 0.21, and solar reflectance of 0.23. Permanent shading devices such as fins and overhangs shall be modeled.

This shading control (bolded in quote) can be summarizied / generalized as: if (transmitted luminance > threshold_x) or (direct solar transmitted energy > threshold_y), then shades should be on and stay on till the end of the day.

Everyone who tries to adopt Appendix C in their model should be able to implement this control in a straightfoward manner without explicitly specifying the control logic through EMS related objects. Currently, EnergyPlus has various `Shading Control Type` options in the `WindowShadingControl` object (EP 9.5 I/O Reference 1.10.58) but this is not one of them. We would like to add this control feature to allow users to capture 90.1-2019 Section C3.5.5.1 easily.

## E-mail and Conference Call Conclusions

The current ASHRAE 90.1 narrative does not descibe sufficiently on the detail about 1) whether the luminance requirement for shading control in Appendix C is regarding a photosensor, and if it is, where should that photosensor location be; 2) what exactly should be considered as "rest of the day" in this context. The PNNL team reached out to ASHRAE 90.1 ESC (Envelope Subcommittee) and confirmed that 1) the luminance value for this control is to be based on the photosensor location defined in the daylighting reference point; 2) once the shade is lowered, it will remain down until the end of the day, i.e. midnight.

When interpreting "rest of the day" being "midnight", feedback collected during technicalities call suggests expanding the implementation to cover three additional shading off options: 1) sunset; 2) sunrise next day; and 3) when luminance value drops below a setpoint, which requires adding one more field 'Setpoint3' under WindowShadingControl. The rest of this document describes the case of adding one new windows shading control option which accounts for the control option of shading off by midnight, other control options will be implemented in similar ways.

Feedback collected during technicalities call also suggests not adding luminance from different windows. We further confirm with lightning experts that the luminance control described in the code implies control for the maximum luminance through any window and adding luminance values from different windows view at the same reference point does not make sense.

## Overview

A new `Shading Control Type` option in the `WindowShadingControl` object needs to be implemented. Based on the naming convention of other options, and existing setpoints arrangements for shading control, this option is tentatively named as `OnIfHighLuminanceOrHighSolarTillMidnight`.

Two setpoints need to be specificd for this shading control option in the idf file. There are two existing setpoints fields in `WindowShadingControl` object:

- Setpoint: units depend on the type of trigger:
  - W/m2 for solar-based controls
  - W for cooling- or heating-based controls
  - Degrees C for temperature-based controls
- Setpoint2: only used for certain two-setpoint control types, in all existing cases listed below, the second setpoint is for solar:
  - `OnIfHighOutdoorAirTempAndHighSolarOnWindow`
  - `OnIfHighOutdoorAirTempAndHighHorizontalSolar`
  - `OnIfHighZoneAirTempAndHighSolarOnWindow`
  - `OnIfHighZoneAirTempAndHighHorizontalSolar`

Based on the current setpoints setup, for the new control option, direct solar transmitted energy can be directly specified in `Setpoint2`.

<!-- while the specification of luminance setpoint may be implemented following the similar approach as another existing window shading control option: `MeetDaylightIlluminanceSetpoint`. In this existing control option, it is only for `ShadingType = SwitchableGlazing`, and the illuminance setpoint used by this control is specified in the `Daylighting:Control` object for the Zone (daylight illuminance set point at the first daylighting reference point), not `WindowShadingControl` `Setpoint`. -->

<!-- To add a setpoint for luminance based control in the option to be implemented, we will be using `Daylighting:Control` object for the Zone luminance setpoint. This requires adding one more field for luminance setpoint in the `Daylighting:Control` object and expanding the application of `Daylighting:Control` object as it was dedicated for illuminance based control in its current version. -->

To add a setpoint for luminance based control in the option to be implemented, we will be using the Setpoint field of `WindowShadingControl` object for the Zone luminance setpoint and expand the application of this field with "cd/m2 for luminance-based controls". We choose not to use `Daylighting:Control` object as this object is dedicated for illuminance based control and adding luminance based control would change its scope and cause confusion (for instance, we would not support luminance based electric lighting control).

<!-- ## Design document - Adding a new window shading control method "OnIfHighLuminanceOrHighSolarTillMidnight" -->

## Approach

1. In `GetDaylightingParametersInput` around `EnergyPlus\src\EnergyPlus\DaylightingManager.cc(4300)`, check if the zone using this new control method has daylighting reference points.
2. In `WindowShadingManager` around `EnergyPlus\src\EnergyPlus\SolarShading.cc(9704)`, add a case for the new control with name `WindowShadingControlType::HiLumin_HiSolar_OffMidNight`. Check solar setpoint in this case block.
3. Around `\EnergyPlus\src\EnergyPlus\HeatBalanceSurfaceManager.cc(1040)`, add the new control name string in `WindowShadingControlTypeStr` vector.
<!-- 4. Calculate and report a new output variable of `Daylighting Reference Point {} Luminance` in `EnergyPlus::DaylightingManager::DayltgInteriorIllum`, which is the sum of all window view luminance for a reference point. -->
4. Add a new function with name `EnergyPlus:DaylightingManager::DayltgInteriorLumTillMidnight` that implements the luminance based control and logic of shades-off by midnight (more detail in the next bullet). Call this method in `EnergyPlus:HeatBalanceSurfaceManager:InitSurfaceHeatBalance` right after the call of `DayltgInteriorIllum` so the updated luminance value is available for control in the same timestep.
5. The shading control is based on three conditions
   1. Compare solar gain of the current timestep with setpoint. This is implemented in `WindowShadingManager` and shading will be turned on (by flags `shadingOn` and `shadingOffButGlareControlOn`) if solar is above setpoint. If luminance is below setpoint, do nothing because shades will only be turned off when it is the end of the day.
   2. Compare total window view luminance to reference point of the current timestep with setpoint. This is implemented in the new function of `DayltgInteriorLumTillMidnight` and shading will be turned on if luminance is above setpoint (by directly modifying value of `state.dataSurface->SurfWinShadingFlag`). If luminance is below setpoint, do nothing because shades will only be turned off when it is the end of the day.
   3. If window shading control is `HiLumin_HiSolar_OffMidNight`, check timestep of the day, if it is the first timestep of the day, reset window shading to off by `state.dataSurface->SurfWinShadingFlag(ISurf) = WinShadingType::ShadeOff`. This is also implemented in the same new function of `DayltgInteriorLumTillMidnight`.

<!-- 7. Add luminance based shading control logic code in `energyplusapi.dll!EnergyPlus::DaylightingManager::DayltgInteriorIllum(EnergyPlus::EnergyPlusData & state, int & ZoneNum)` (or add a new method dedicated for luminance based control and call it from `\EnergyPlus\src\EnergyPlus\HeatBalanceSurfaceManager.cc` ) at `EnergyPlus\src\EnergyPlus\DaylightingManager.cc`, following the same coding pattern as the `WindowShadingControlType::MeetDaylIlumSetp`
8. Luminance value to be used in checking the requirement will be `LumWinFromRefPtRep`. (This value is reported as an output variable and its illuminance counterpart is used in the illuminance based shading control (Reference in `DaylightingManager.cc`))
9. Solar based control for this logic will be added following code patterns for "HiSolar" related window shading options in `EnergyPlus\src\EnergyPlus\SolarShading.cc`.
10. The logic of keeping the shade done until end of day (midnight) is to be implemented either in Item 1 or 2 above, together with related logic. -->

## Testing/Validation/Data Sources

A test case model to use this new shading control option and the result shading status time series and related solar gain and luminance value time series are to be compared to verify the shading is controlled as designed.

## Input Output Reference Documentation

Under 1.10.58 WindowShadingControl

- A paragraph describing this new window control method will be added under 1.10.58.1.6 Field: Shading Control Type. We need to emphasize that this control uses luminance measured at the first daylighting reference point.
- One more bullet of "cd/m2 for luminance-based controls" is to be added under 1.10.58.1.8 Field: Setpoint
- The new control method name is to be added under 1.10.58.1.14 Field: setpoint 2.

## Input Description

- Add a new `WindowShadingControl` option: `OnIfHighLuminOrHighSolarTillMidnight`

## Outputs Description

- Add a new output variable option: `Daylighting Reference Point {} Luminance`
<!-- - We may need to add a report variable for luminance based control, check illuminance based one to see what's in there. -->

<!-- ## Engineering Reference

To be added according to overview. -->

## Example File and Transition Changes

Instance of this new shading control method will be added to the existing window testing idf `WindowTests.idf`. No transition change is expected.

## References

- ASHRAE. 2019. ANSI/ASHRAE/IES 90.1-2019, Energy Standard for Buildings Except Low-Rise
  Residential Buildings. ASHRAE, Atlanta, GA
- [Add additional option for shading control for WindowProperty:ShadingControl object #7081](https://github.com/NREL/EnergyPlus/issues/7081)
