# NFP ASHRAE 90.1 2019 Appendix C Shading Control

**Xuechen (Jerry) Lei, Jeremy Lerond, and Jian Zhang. PNNL**

- Original Date: 06/09/2021
- Revision Date: 01/06/2022

## Justification for New Feature

In "ASHRAE Standard 90.1-2019, Appendix C - Methodology for Building Envelope Trade-Off Option in Section 5.6", there is a shading control requirement:

> **C3.5.5.1 Shading**
>
> Manually operated interior shades shall be modeled on all vertical fenestration. **Shades shall be modeled to be in the lowered position when either the transmitted luminance is greater than 200 cd/ft2 or the direct solar transmitted energy exceeds 30 Btu/h·ft2 and then remain lowered for rest of the day.** Shades shall be modeled with visible light transmittance of 0.10, visible light reflectance of 0.40, solar transmittance of 0.21, and solar reflectance of 0.23. Permanent shading devices such as fins and overhangs shall be modeled.

This shading control (bolded in quote) can be summarizied / generalized as: if (transmitted luminance > threshold_x) or (direct solar transmitted energy > threshold_y), then shades should be on and stay on till the end of the day.

Everyone who tries to adopt Appendix C in their model should be able to implement this control in a straightfoward manner without explicitly specifying the control logic through EMS related objects. Currently, EnergyPlus has various `Shading Control Type` options in the `WindowShadingControl` object (EP 9.5 I/O Reference 1.10.58) but this is not one of them. We would like to add this control feature to allow users to capture 90.1-2019 Section C3.5.5.1 easily.

## E-mail and Conference Call Conclusions

The current ASHRAE 90.1 narrative does not descibe sufficiently on the detail about 1) whether the luminance requirement for shading control in Appendix C is regarding a photosensor, and if it is, where should that photosensor location be; 2) what exactly should be considered as "rest of the day" in this context. The PNNL team reached out to ASHRAE 90.1 ESC (Envelope Subcommittee) and confirmed that 1) the luminance value for this control is to be based on the photosensor location defined in the daylighting reference point; 2) once the shade is lowered, it will remain down until the end of the day, i.e. midnight.

When interpreting "rest of the day" being "midnight", feedback collected during technicalities call suggests expanding the implementation to cover three additional shading off options: 1) sunset; 2) sunrise next day; and 3) when luminance value drops below a setpoint, which requires adding one more field 'Setpoint3' under WindowShadingControl. **The rest of this document describes the case of adding one new windows shading control option which accounts for the control option of shading off by midnight, other control options will be implemented in similar ways.**

At this moment, we decided not to implement window shading control strategy that would turn off shade when luminance value drops below a setpoint in this NFP because 1) it requires adding more setpoints (e.g. a solar gain lower bound to turn off shades and a luminance lower bound to turn off shades to make them equal) to the WindowShadingControl object, which already has two setpoints fields; 2) turning on and off shades based on setpoints may cause shades to turn on and off frequently (oscillate) during a short period of time when luminance / solar gain value variates up and down.

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

To add a setpoint for luminance based control in the option to be implemented, we will be using the Setpoint field of `WindowShadingControl` object for the Zone luminance setpoint and expand the application of this field with "cd/m2 for luminance-based controls". We choose not to use `Daylighting:Control` object as this object is dedicated for illuminance based control and adding luminance based control would change its scope and cause confusion (for instance, we would not support luminance based electric lighting control).

In the meantime, we decide to continue using the daylighting objects (control and reference points) to specify daylighting reference points for measuring luminance values and to calculate luminance values with existing routines associated with daylighting control object.

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

N/A

## Example File and Transition Changes

Instance of this new shading control method will be added to the existing window testing idf `WindowTests.idf`. No transition change is expected.

## References

- ASHRAE. 2019. ANSI/ASHRAE/IES 90.1-2019, Energy Standard for Buildings Except Low-Rise
  Residential Buildings. ASHRAE, Atlanta, GA
- [Add additional option for shading control for WindowProperty:ShadingControl object #7081](https://github.com/NREL/EnergyPlus/issues/7081)
