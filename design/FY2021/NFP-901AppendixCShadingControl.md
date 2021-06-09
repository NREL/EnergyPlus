# NFP ASHRAE 90.1 2019 Appendix C Shading Control

WIP

**Xuechen (Jerry) Lei, Jian Zhang, PNNL**

- Original Date: 06/09/2021
- Revision Date

## Justification for New Feature

In the most recent ASHRAE Standard 90.1-2019, Appendix C - Methodology for Building Envelope Trade-Off Option in Section 5.6, there is a shading control specification:

> **C3.5.5.1 Shading**
>
> Manually operated interior shades shall be modeled on all vertical fenestration. **Shades shall be modeled to be in the lowered position when either the transmitted luminance is greater than 200 cd/ft2 or the direct solar transmitted energy exceeds 30 Btu/h·ft2 and then remain lowered for rest of the day.** Shades shall be modeled with visible light transmittance of 0.10, visible light reflectance of 0.40, solar transmittance of 0.21, and solar reflectance of 0.23. Permanent shading devices such as fins and overhangs shall be modeled.

This shading control (bolded in quote) can be summarizied / generalized as: if (transmitted luminance > threshold_x) or (direct solar transmitted energy > threshold_y), then shades should turn on and stay on till the end of the day.

Everyone who tries to adopt Appendix C in their model should be able to implement this control in a straightfoward manner without explicitly specifying the control logic through EMS related objects. Currently, EnergyPlus has various `Shading Control Type` options in the `WindowShadingControl` object (EP 9.5 I/O Reference 1.10.58) but this is not one of them.

## E-mail and Conference Call Conclusions

None

## Overview

A new `Shading Control Type` option in the `WindowShadingControl` object needs to be implemented. Based on the naming convention of other options, and existing setpoints arrangements for shading control, this option is tentatively named as `OnIfHighLuminOrHighSolarTillMidnight`.

There are two existing setpoints fields in `WindowShadingControl` object:

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

---

**Potential errors / misunderstanding in current code/doc**

- the last two zone air temperature options are not listed in hsading control type field section
- Setpoint 2 annotation comment in idf object is 'Setpoint 2 {W/m2 or deg C}', not sure when deg C will be effective

---

Based on the current setpoints setup, for the new control option, the second setpoint (for direct solar transmitted energy can be directly specified in `Setpoint2`), while we need to make a decision on two options of specifying the first setpoint (luminance threshold) for this control.

1. Use `Setpoint`
2. Use `Daylighting:Control` object for the Zone

There are a couple of considerations for making this decision.

1. In its current implementation, `Setpoint` in `WindowShadingControl` does not deal with illuminance level related setpoint
2. One of the existing shading control options is `MeetDaylightIlluminanceSetpoint`. Note this is only for 'ShadingType = SwitchableGlazing'. The illuminance setpoint used by this control is specified in the `Daylighting:Control` object for the Zone (daylight illuminance set point at the first daylighting reference point), not `WindowShadingControl` `Setpoint`. From the code in the next section. It looks like the shade is actually off in this option. **Also may need to dig more in the code to figure out which option is better**

## Approach (Needs to be verified by running test cases)

### input (IDD)

Add a new `WindowShadingControl` option: `OnIfHighLuminOrHighSolarTillMidnight`

### shading control logic code

The shading control logic of different control options are implemented in `src/EnergyPlus/SolarShading.cc` LOC 9589 onwards. Some existing options' logic are implemented as below. The new option will be implemented in similar fashion within this code block referencing the coding patterns of other options.

```cpp
// ...
switch (state.dataSurface->WindowShadingControl(IShadingCtrl).ShadingControlType) {
case WindowShadingControlType::AlwaysOn: // 'ALWAYSON'
    shadingOn = true;
    break;
// ...
case WindowShadingControlType::OnHiOutTemp_HiSolarWindow: // 'OnIfHighOutdoorAirTempAndHighSolarOnWindow'  ! Outside air temp and solar on
                                                          // window
    if (state.dataEnvrn->SunIsUp) {
        if (state.dataSurface->SurfOutDryBulbTemp(ISurf) > SetPoint && SolarOnWindow > SetPoint2 && SchedAllowsControl) {
            shadingOn = true;
        } else if (GlareControlIsActive) {
            shadingOffButGlareControlOn = true;
        }
    }
    break;

case WindowShadingControlType::OnHiOutTemp_HiHorzSolar: // 'OnIfHighOutdoorAirTempAndHighHorizontalSolar'  ! Outside air temp and
                                                        // horizontal solar
    if (state.dataEnvrn->SunIsUp) {
        if (state.dataSurface->SurfOutDryBulbTemp(ISurf) > SetPoint && HorizSolar > SetPoint2 && SchedAllowsControl) {
            shadingOn = true;
        } else if (GlareControlIsActive) {
            shadingOffButGlareControlOn = true;
        }
    }
    break;
// ...
case WindowShadingControlType::MeetDaylIlumSetp:
    // 'MEETDAYLIGHTILLUMINANCESETPOINT')  !  Daylight illuminance test is done in DayltgInteriorIllum
    // Only switchable glazing does daylight illuminance control
    if (state.dataEnvrn->SunIsUp && SchedAllowsControl) {
        shadingOffButGlareControlOn = true;
    }
    break;
// ...
if (IS_SHADED(ShType)) {
    if (shadingOn) {
        state.dataSurface->SurfWinShadingFlag(ISurf) = ShType;
    } else if (shadingOffButGlareControlOn) {
        if (ShType == WinShadingType::SwitchableGlazing)
            state.dataSurface->SurfWinShadingFlag(ISurf) = WinShadingType::GlassConditionallyLightened;
        else if (ShType == WinShadingType::IntShade)
            state.dataSurface->SurfWinShadingFlag(ISurf) = WinShadingType::IntShadeConditionallyOff;
        else if (ShType == WinShadingType::ExtShade)
            state.dataSurface->SurfWinShadingFlag(ISurf) = WinShadingType::ExtShadeConditionallyOff;
        else if (ShType == WinShadingType::IntBlind)
            state.dataSurface->SurfWinShadingFlag(ISurf) = WinShadingType::IntBlindConditionallyOff;
        else if (ShType == WinShadingType::ExtBlind)
            state.dataSurface->SurfWinShadingFlag(ISurf) = WinShadingType::ExtBlindConditionallyOff;
        else if (ShType == WinShadingType::BGShade)
            state.dataSurface->SurfWinShadingFlag(ISurf) = WinShadingType::BGShadeConditionallyOff;
        else if (ShType == WinShadingType::BGBlind)
            state.dataSurface->SurfWinShadingFlag(ISurf) = WinShadingType::BGBlindConditionallyOff;
    }
}
// ...
```

**May also need to change the Python API? need more digging**

## Testing/Validation/Data Sources

A test case model to use this new shading control option and the result shading status time series and related solar gain and illuminance value time series are to be compared to verify the shading is controlled as designed.

## Input Output Reference Documentation

To be added according to overview.

## Input Description

To be added according to overview.

## Outputs Description

N/A

## Engineering Reference

To be added according to overview.

## Example File and Transition Changes

An example file using this shading control option shall be added, or this shading control option can be implemented in an existing example file. **Need to make a decision**

## References

- ASHRAE. 2019. ANSI/ASHRAE/IES 90.1-2019, Energy Standard for Buildings Except Low-Rise
  Residential Buildings. ASHRAE, Atlanta, GA
- [Add additional option for shading control for WindowProperty:ShadingControl object #7081](https://github.com/NREL/EnergyPlus/issues/7081)
