Reference Temperature Inputs for Water To Air Coils
================

**Jeremy Lerond, Wooyoung Jung, and Jian Zhang, PNNL**

 - Original Date: 10/21/2021
 - Revision Date: N/A


## Justification for New Feature ##

When __auto-sizing__ water to air coils (`Coil:Cooling:WaterToAirHeatPump:EquationFit` and `Coil:Heating:WaterToAirHeatPump:EquationFit`), EnergyPlus currently determines their total rated capacity by normalizing the total capacity at peak design conditions by the total capacity curve modifier at 85&deg;F Entering Water Temperature (EWT) and at the Entering Air Wet-bulb Temperature (EAWT) at peak design conditions. The total rated capacity is then used to determine the rated power which is in turn used in the simulation to calculate the coil's power by multiplying it by the power curve modifier.

Water-source heat pumps are rated following the test procedures in ISO 13256-1. The rating conditions vary depending on the application of the water-source heat pump. For instance, for cooling operation, groundwater water-to-air heat pumps are rated at 59&deg;F EWT whereas water loop water-to-air heat pumps are rated at 86&deg;F EWT. In fact, none of the water-to-air applications currently listed in ASHRAE Standard 90.1 are shown to be rated at 85&deg;F EWT.

Since the rated power is calculated from the rated capacity, when __auto-sizing__ water to air coils using these objects in EnergyPlus users currently have to input a de-rated Coefficient Of Performance (COP) that is calculated at 85&deg;F using the performance curves used in a model. Additionally, when both a cooling and heating coil are modeled, the rated capacity of the heating coil is set to be the same as the cooling coil, however, the rating conditions between cooling and heating operation are very different, this also requires users to input a de-rated heating COP at 85&deg;F.

This document proposes to add new reference or rated temperature inputs (in the code, EnergyPlus uses the "rated" terminology but "reference" might be more general) for both the `Coil:Cooling:WaterToAirHeatPump:EquationFit` and `Coil:Heating:WaterToAirHeatPump:EquationFit` objects. These new inputs would be used to in conjunction with the capacity and COP inputs to determine the correct reference or rated power when auto-sizing coils. This is mostly relevant, but not limited, to code compliance modeling where users rely on the simulation software the calculate the correct autosized rated capacity and power.

In addition to the `Coil"*:WaterToAirHeatPump:EquationFit`, we propose to make the same changes to the `Coil:*:WaterToAirHeatPump:VariableSpeedEquationFit` because sizing is pretty similar (While the calculation for these objects is done differently).

## E-mail and  Conference Call Conclusions ##

## Overview ##

New reference or rated temperature inputs (EnergyPlus (I/O and code) uses the "rated" terminology but "reference" might be more general and is currently used by the chiller objects) for both the `Coil:Cooling:WaterToAirHeatPump:EquationFit` and `Coil:Heating:WaterToAirHeatPump:EquationFit` objects. These new inputs would be used in conjunction with the capacity and COP inputs to determine the correct reference or rated power when auto-sizing coils.

## Approach ##

The reference or rated capacity is currently calculated as follows (in the `SizeHVACWaterToAir` function).

```
ratioTWB = (MixWetBulb + 273.15) / 283.15;
[...]
ratioTS = (((85.0 - 32.0) / 1.8) + 273.15) / 283.15;
[...]
TotCapTempModFac = CurveManager::CurveValue(state, simpleWatertoAirHP.TotalCoolCapCurveIndex, ratioTWB, ratioTS, 1.0, 1.0);
[...]
RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac
```

This approach assumes that the reference or rated capacity is calculated using the peak conditions mixed-air wet-bulb temperature and an entering water temperature of 85&deg;F. The proposed approach would calculate the __reference__ capacity (we proposed to drop the _rated_ term) using the user specified reference conditions, the input for gross rated COP would be changed to gross reference COP and would correspond to the coil's efficiency at these reference conditions. Below is a pseudo-code that illustrates how the code would be modified:

```
RefCapCoolTotalDes = CoolCapAtPeak * RefTotCapTempModFac / PeakTotCapTempModFac
```

- `PeakTotCapTempModFac` would be the capacity modifier at peak design conditions. Currently, the peak design conditions correspond to the mixed air wet-bulb temperature and an entering water temperature of 85&deg;F. We propose to keep the former but to change the latter to be based on the user-specified reference entering water temperature (default to 30&deg;C, 86&deg;F).
- `RefTotCapTempModFac` would be the capacity modifier at the reference conditions. The reference conditions would correspond to the user-specified mixed air wet-bulb (default to 19&deg;C, 66.2&deg;F) and entering water temperature, these could follow the rating conditions for a specific application the coil is being used for (see Table 2 and 3 in ISO 13256-1) the user intends to model a _rated_ COP.

When a heating coil is modeled along with a cooling coil, and both are setup to have their capacity auto-sized, EnergyPlus currently sets the rated heating capacity to be the same as the cooling coil. However, rating conditions are different for heating coils, hence, the proposed approach would consider that the heating capacity is the same at peak conditions and the same "peak to reference" adjustment will be applied to determine the correct reference heating capacity.

Also, a review of the code helped identified another issue with the calculation of the rated capacity: [#7996](https://github.com/NREL/EnergyPlus/pull/7996) introduced an if statement (see code block below) checking if a coil has a valid condenser inlet node (probably aimed to identify if the coil is of `Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed` type as it has a field for `Rated Condenser Inlet Water Temperature`), if it does not, the outdoor air temperature at peak sizing conditions is used. A `Coil:*:WaterToAirHeatPump:VariableSpeedEquationFit` coil doesn't ever have a valid condenser node so the latter applies which is inconsistent with the type of variable that's expected by the model: a water temperature is expected, not an outdoor air temperature. See code snippet below for additional details.

```
if (state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).CondenserInletNodeNum != 0) {
     RatedSourceTempCool = state.dataVariableSpeedCoils->RatedInletWaterTemp;
} else {
     RatedSourceTempCool = OutTemp;
}
TotCapTempModFac = CurveValue(state,
                              state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).MSCCapFTemp(
                                   state.dataVariableSpeedCoils->VarSpeedCoil(DXCoilNum).NormSpedLevel),
                              MixWetBulb,
                              RatedSourceTempCool);
[...]
RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac;
```

## Testing/Validation/Data Sources ##

Unit tests will be added to test that the reference heating and cooling capacity of the `Coil:Cooling:WaterToAirHeatPump:EquationFit` and `Coil:Heating:WaterToAirHeatPump:EquationFit` objects (and variable speed versions) is correctly calculated using the user-input reference temperatures, and/or with the default values.

## Input Output Reference Documentation ##

Documentation for the new inputs will be added for both coil objects. The proposed documentation could be as follows:

> This numeric field contains the reference entering air wet-bulb temperature. This field along with the next one is only used when the capacity is set to be autosized and is used to determine the coil's reference capacity. The COP should be entered at these conditions. If left blank, 19&deg;C is used (rating temperature for water loop water-to-air heat pumps in ISO-13256-1-2021).

> This numeric field contains the entering water temperature. This field along with the previous one is only used when the capacity is set to be autosized and is used to determine the coil's reference capacity. The COP should be entered at these conditions. If left blank, 30&deg;C is used (rating temperature for water loop water-to-air heat pumps in ISO-13256-1-2021).

## Input Description ##

The following inputs will be added to the `Coil:Cooling:WaterToAirHeatPump:EquationFit` and `Coil:Heating:WaterToAirHeatPump:EquationFit` objects (and variable speed versions of them):

```
Coil:Cooling:WaterToAirHeatPump:EquationFit,
   N*,  \field Reference Entering Water Temperature
        \note Reference or rated entering water temperature corresponding to the water-to
        \note -air application for which this coil is used. For example: for water loop
        \note applications, the rated temperature is 30 degree Celsius.
        \units C
        \type real
        \minimum> 0
        \default 30
   N*,  \field Reference Entering Air Wet-Bulb Temperature
        \note Reference or rated entering air wet-bulb temperature corresponding to the
        \note water-to-air application for which this coil is used. For example: for
        \note water loop applications, the rated temperature is 19 degree Celsius.
        \units C
        \type real
        \minimum> 0
        \default 19
```

```
Coil:Heating:WaterToAirHeatPump:EquationFit,
   N*,  \field Reference or Rated Entering Water Temperature
        \note Reference or rated entering water temperature corresponding to the water-to
        \note -air application for which this coil is used. For example: for water loop
        \note applications, the rated temperature is 20 degree Celsius.
        \units C
        \type real
        \minimum> 0
        \default 20
   N*,  \field Reference or Rated Entering Air Wet-Bulb Temperature
        \note Reference or rated entering air wet-bulb temperature corresponding to the
        \note water-to-air application for which this coil is used. For example: for
        \note water loop applications, the rated temperature is 15 degree Celsius.
        \units C
        \type real
        \minimum> 0
        \default 15
```

It is also proposed to remove `Rated` from all four coil objects inputs and replace it by `Reference`.

## Outputs Description ##

This new feature proposal does not include any new outputs.

## Engineering Reference ##

The "Rated Total Cooling Capacity" subsection of the "Coil:Cooling:WaterToAirHeatPump:EquationFit Sizing" section will be modified as follows:

> The calculation for coil operating temperatures (inlet and outlet) are identical to that done for \emph{Coil:Cooling:Water}. The following calculations are then performed to determine the rated total cooling capacity.
>
> \begin{equation}
>   T_{WB,ratio,ref} = \frac{T_{WB,air,in,ref}+273.15C}{283.15C}
> \end{equation}
>
> \begin{equation}
>   T_{S,ratio,ref} = \frac{T_{water,in,ref}+273.15C}{283.15C}
> \end{equation}
>
> \begin{equation}
>   T_{WB,ratio,peak} = \frac{T_{WB,air,in,peak}+273.15C}{283.15C}
> \end{equation}
>
> \begin{equation}
>   T_{S,ratio,peak} = \frac{T_{water,in,peak}+273.15C}{283.15C}
> \end{equation}
>
> where:
>
> $T_{WB,ratio,ref} = $ ratio of reference load-side inlet air wet-bulb temperature in Kelvin to a reference temperature
>
> $T_{S,ratio,ref} = $ ratio of reference source-side inlet water temperature in Kelvin to a reference temperature
>
> $T_{WB,ratio,peak) = $ ratio of peak load-side inlet air wet-bulb temperature in Kelvin to a reference temperature
>
> $T_{S,ratio,peak} = $ ratio of peak source-side inlet water temperature in Kelvin to a reference temperature
>
> $T_{WB,air,in,ref} = $ the reference load-side inlet air wet-bulb temperature, if not specified by the user, 19&deg;C (66.2&deg;F; the peak value) is used which corresponds to the rating conditions of a water loop water-source heat pump according to ISO-13256-1
>
> $T_{S,air,in,ref} = $ the reference source-side inlet water temperature, if not specified by the user, 30&deg;C (86&deg;F; the peak value) is used which corresponds to the rating conditions of a water loop water-source heat pump according to ISO-13256-1
>
> $T_{WB,air,in,peak} = $ the peak load-side inlet air wet-bulb temperature, if not specified by the user, the peak design mixed air entering the coil will be used
>
> $T_{S,air,in,peak} = $ the peak source-side inlet water temperature, if not specified by the user, 30&deg;C (86&deg;F; the peak value) is used which corresponds to the rating conditions of a water loop water-source heat pump according to ISO-13256-1
>
> \begin{equation}
> TotCapTempModFacRef = \,TCC1 + TCC2\left( {{T_{WB,ratio,ref}}} \right) + TCC3\left( {{T_{S,ratio,ref}}} \right) + TCC4 + TCC5
> TotCapTempModFacPeak = \,TCC1 + TCC2\left( {{T_{WB,ratio,peak}}} \right) + TCC3\left( {{T_{S,ratio,peak}}} \right) + TCC4 + TCC5
> \end{equation}
>
> [...]
>
>\begin{equation}
>   \dot{Q}_{coil,des,total}   = \frac{\dot{m}_{air,des}\PB{H_{in}-H_{out}}TotCapTempModFacRef + \dot{Q}_{fan,heat,des}}{TotCapTempModFacPeak}
>\end{equation}

## Example File and Transition Changes ##

Two existing example files will be modified to illustrate the proposed changes: (1) the Ground Source Heat Pump (GSHP) application and (2) the Water Source Heat Pump (WSHP) application.

The proposed approach plans on adding two new inputs in the middle of the `Coil:*:WaterToAirHeatPump:EquationFit` objects (and variable speed versions), so transition rules will be needed.

## References ##
* ASHRAE. 2019. ANSI/ASHRAE/IES 90.1-2019, Energy Standard for Buildings Except Low-Rise
Residential Buildings. ASHRAE, Atlanta, GA
* ISO, 2021. ISO 13256-1, Water-source heat pumps — Testing and rating for performance — Part 1: Water-to-air and brine-to-air heat pumps
