Reference Temperature Inputs for Water To Air Coils
================

**Jeremy Lerond, Wooyoung Jung, and Jian Zhang, PNNL**

 - Original Date: 10/21/2021
 - Revision Date: 11/01/2021


## Justification for New Feature ##

When auto-sizing the capacity and power of the `Coil:Cooling:WaterToAirHeatPump:EquationFit` and `Coil:Heating:WaterToAirHeatPump:EquationFit` objects, EnergyPlus determines their total rated capacity by normalizing the total capacity at peak design conditions by the total capacity curve modifier at 85&deg;F Entering Water Temperature (EWT) and at the Entering Air Wet-bulb Temperature (EAWT) at peak design conditions. The total rated capacity is then used to determine the rated power which is in turn used in the simulation to calculate the coil's power by multiplying it by the power curve modifier.

Water-source heat pumps are rated following the test procedures in ISO 13256-1. The rating conditions vary depending on the application of the water-source heat pump. For instance, for cooling operation, groundwater water-to-air heat pumps are rated at 59&deg;F EWT whereas water loop water-to-air heat pumps are rated at 86&deg;F EWT (see Figure 1). In fact, none of the water-to-air applications currently listed in ASHRAE Standard 90.1 are shown to be rated at 85&deg;F EWT.

![ASHRAE 90.1 WSHP requirements](NFP-RefCondsWtrToAirCoils_901reqs.PNG)

_Figure 1 - Excerpt of ASHRAE Standard 90.1 Minimum Efficiency requirements for water-to-air heat pumps_

![Rating conditions for water-loop heat pumps](NFP-RefCondsWtrToAirCoils_ISOrtgs.PNG)

_Figure 2 - Excerpt of the ISO 13256-1 rating conditions_

Since the rated power is calculated from the rated capacity, when auto-sizing water-to-air coils using these objects, users currently have to input a de-rated Coefficient Of Performance (COP) that is calculated at 85&deg;F EWT and peak design EAWT. Additionally, when both a cooling and heating coil are modeled, the rated capacity of the heating coil is set to be the same as the cooling coil, however, the rating conditions between cooling and heating operation are very different, this also requires users to input a de-rated heating COP.

This document proposes to add new reference or rated temperature inputs (in the code, EnergyPlus uses the "rated" terminology but "reference" might be more general) for both the `Coil:Cooling:WaterToAirHeatPump:EquationFit` and `Coil:Heating:WaterToAirHeatPump:EquationFit` objects. These new inputs would be used to, in conjunction with the capacity and COP inputs, determine the correct reference or rated power when auto-sizing these coil objects. This is mostly relevant, but not limited, to code compliance modeling where users rely on the simulation software the calculate the correct autosized rated capacity and power.

In addition to the `Coil:*:WaterToAirHeatPump:EquationFit`, we propose to make the same changes to the `Coil:*:WaterToAirHeatPump:VariableSpeedEquationFit` because sizing is pretty similar even though the simulation for these objects is done differently.

## E-mail and  Conference Call Conclusions ##

## Overview ##

New reference or rated temperature inputs (EnergyPlus (I/O and code) uses the "rated" terminology but "reference" might be more general and is currently used by the chiller objects) for both the `Coil:Cooling:WaterToAirHeatPump:EquationFit` and `Coil:Heating:WaterToAirHeatPump:EquationFit` objects. These new inputs would be used in conjunction with the capacity and COP inputs to determine the correct reference or rated power when auto-sizing coils.

## Approach ##

### Core Changes ###

The reference or rated capacity is currently calculated as follows (in the `SizeHVACWaterToAir` function).

```
ratioTWB = (MixWetBulb + 273.15) / 283.15;
[...]
ratioTS = (((85.0 - 32.0) / 1.8) + 273.15) / 283.15;
[...]
TotCapTempModFac = CurveManager::CurveValue(state, simpleWatertoAirHP.TotalCoolCapCurveIndex, ratioTWB, ratioTS, 1.0, 1.0);
[...]
RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac
[...]
simpleWatertoAirHP.RatedPowerCool = simpleWatertoAirHP.RatedCapCoolTotal / simpleWatertoAirHP.RatedCOPCool;
```

This approach assumes that the reference or rated capacity is calculated using the coil's peak mixed-air wet-bulb temperature and an entering water temperature of 85&deg;F. The proposed approach would calculate the reference (we propose to drop the _rated_ term) capacity and power using the user specified reference conditions, the input for gross rated COP would be changed to gross reference COP and would correspond to the coil's efficiency at these reference conditions. Below is a pseudo-code that illustrates how the code would be modified:

```
RefCapCoolTotalDes = CoolCapAtPeak * RefTotCapTempModFac / PeakTotCapTempModFac
CapCoolTotalDesAtRefCdts = RefCapCoolTotalDes * RefTotCapTempModFac
RefPowerCool = CapCoolTotalDesAtRefCdts / (RefCOPCool * RefPowerTempModFac)
PowerCoolAtRefCdts = RefPowerCool * RefPowerTempModFac
```

- `PeakTotCapTempModFac` is the capacity modifier at peak design conditions. Currently, the peak design conditions correspond to the mixed air wet-bulb temperature and an entering water temperature of 85&deg;F. We propose to keep the former but to change the latter to be based on the user-specified reference entering water temperature (default to 30&deg;C, 86&deg;F).
- `RefTotCapTempModFac` is the capacity modifier at the reference conditions. The reference conditions would correspond to the user-specified mixed air wet-bulb (default to 19&deg;C, 66.2&deg;F) and entering water temperature (default to 30&deg;C, 86&deg;F). The reference conditions could be the rating conditions for a specific water-to-air application (see Table 2 and 3 in ISO 13256-1) if the user provides a rated COP as input.
- `RefPowerTempModFac` is the power modifier at the reference conditions. The reference conditions would correspond to the user-specified mixed air wet-bulb (default to 19&deg;C, 66.2&deg;F) and entering water temperature (default to 30&deg;C, 86&deg;F). The reference conditions could be the rating conditions for a specific water-to-air application (see Table 2 and 3 in ISO 13256-1) if the user provides a rated COP as input.

The coil's sensible capacity is calculated similarly to the total capacity. The main difference being that it uses the sensible capacity/load at peak as the basis for the calculation. The same adjustments will be made to accurately calculate the reference sensible capacity.

When a heating coil is modeled along with a cooling coil and both are setup to have their capacity auto-sized, EnergyPlus currently sets the rated heating capacity to be the same as the cooling coil. However, rating conditions are different for heating coils, hence, the proposed approach would consider that the heating capacity is the same as the cooling capacity at peak conditions and the same "peak to reference" adjustments will be applied to determine the correct reference heating capacity and power.

### Example ###

The following example illustrates the issue and proposed approach. Let's consider a model using a `Coil:Cooling:WaterToAirHeatPump:EquationFit` using the same performance curves as in the `ASHRAE901_ApartmentHighRise_STD2019_Denver.idf` file.

Let's assume that:
- The coil load at peak design conditions is `CoolCapAtPeak` = 1000W, 
- The entering air wet-bulb temperature at peak design conditions is `MixWetBulb` = 15.6&deg;C, 
- The entering air dry-bulb temperature at peak design conditions is `MixTemp` = 21.1&deg;C, 
- The user-specified cooling COP at rated conditions is `RatedCOPCool` = 4.2

#### Current Approach ###
_Note: The current variable names are used below._

- `PeakTotCapTempModFac` is calculated at 29.44&deg;C (85&deg;F) EWT and 15.6&deg;C (60&deg;F) EAWT and is about 0.873
- `RatedCapCoolTotalDes` = 1000 / 0.873 = 1145 W
- `RatedPowerCool` = 1145 / 4.2 = 273 W

The rating conditions for water-to-air water loop heat pumps are 30&deg;C (86&deg;F) EWT and 19&deg;C (66.2&deg;F), at these conditions the `TotCapTempModFac` is 0.956 and `PowerTempModFac` is 1.006. When using these modifiers to calculate the operating capacity and power at these conditions we get a capacity of 1145 * 0.956 ~= 1095 W and a power of 274 * 1.006 ~= 274 W which correspond to a COP of 1095 / 274 ~= 4 which doesn't align with the user-specified COP of 4.2.

#### Proposed Approach ####
_Note: The proposed variable names are used below._

- `PeakTotCapTempModFac` is calculated at 30&deg;C (86&deg;F) EWT and 15.6&deg;C (60&deg;F) EAWT and is about 0.869
- `RefTotCapTempModFac` is calculated at 30&deg;C (86&deg;F) EWT and 19&deg;C (66.2&deg;F) EAWT and is about 0.956 which corresponds to the total capacity modifier at the reference conditions which are the actual rating condition in this example.
- `RefPowerTempModFac` is calculated at 30&deg;C (86&deg;F) EWT and 19&deg;C (66.2&deg;F) EAWT and is about 1.006 which corresponds to the power modifier at the reference conditions which are the actual rating condition in this example.
- `RefCapCoolTotalDes` = 1000 * 0.956 / 0.869 = 1101 W
- `RatedPowerCool` = 1101 * 0.956 / (4.2 * 1.006) = 249 W

The rating conditions for water-to-air water loop heat pumps are 30&deg;C (86&deg;F) EWT and 19&deg;C (66.2&deg;F), at these conditions the `TotCapTempModFac` is 0.956 and `PowerTempModFac` is 1.006. When using these modifiers to calculate the operating capacity and power at these conditions we get a capacity (`CapCoolTotalDesAtRefCdts`) of 1101 * 0.956 = 1052 W and a power (`PowerCoolAtRefCdts`) of 249 * 1.006 ~= 251 W which correspond to a COP of 1052 / 251 ~= 4.2 which aligns with the user-specified COP of 4.2.

### Additional Proposed Changes ###
#### Coil:*:WaterToAirHeatPump:VariableSpeedEquationFit ####
As previously mentioned the `Coil:*:WaterToAirHeatPump:VariableSpeedEquationFit` coil objects are sized similarly to the `Coil:*:WaterToAirHeatPump:EquationFit` coil objects so the same changes will be applied.

Additionally, a review of the code helped identified another potential issue with the calculation of the rated capacity. PR [#7996](https://github.com/NREL/EnergyPlus/pull/7996) introduced an if statement (see code block below) checking if a coil has a valid condenser inlet node (probably aimed to identify if the coil is of `Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed` type as it has a field for `Rated Condenser Inlet Water Temperature`), if it does not, the outdoor air temperature at peak sizing conditions is used. A `Coil:*:WaterToAirHeatPump:VariableSpeedEquationFit` coil doesn't ever have a valid condenser node so the latter applies which is inconsistent with the type of variable that's expected by the model: a water temperature is expected, not an outdoor air temperature. See code snippet below for additional details.

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
> $T_{WB,ratio,peak} = $ ratio of peak load-side inlet air wet-bulb temperature in Kelvin to a reference temperature
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
>   \dot{Q}_{coil,des,total}   = \frac{\dot{m}_{air,des}\PB{H_{in}-H_{out}}TotCapTempModFacRef}{TotCapTempModFacPeak} + \dot{Q}_{fan,heat,des}
>\end{equation}

## Example File and Transition Changes ##

Two existing example files will be modified to illustrate the proposed changes: a Ground Source Heat Pump (GSHP) application and a Water Source Heat Pump (WSHP) application.

The proposed approach plans on adding two new inputs in the middle of the `Coil:*:WaterToAirHeatPump:EquationFit` objects (and variable speed versions), so transition rules will be needed.

## References ##
* ASHRAE. 2019. ANSI/ASHRAE/IES 90.1-2019, Energy Standard for Buildings Except Low-Rise
Residential Buildings. ASHRAE, Atlanta, GA
* ISO, 2021. ISO 13256-1, Water-source heat pumps — Testing and rating for performance — Part 1: Water-to-air and brine-to-air heat pumps
