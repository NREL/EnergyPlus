# Group – Heat Recovery

## HeatExchanger:AirToAir:FlatPlate

The air-to-air flat plate heat exchanger is an HVAC component typically used for exhaust or relief air heat recovery. The user can choose the flow arrangement type: counter flow, parallel flow, or cross flow with both streams unmixed. The input requires no geometric data. Performance is defined by specifying primary outlet air temperature at nominal (user specified) inlet conditions. In addition, the ratio (h^.^A)~p~ / (h^.^A)~s~ at nominal flow needs to be input, where h is the convective heat transfer  coefficient, A is the surface area, p stands for primary side, s for secondary side.

If the heat exchanger is operated in conjunction with an outdoor air economizer (economizer lockout set to *Yes*), the nominal supply air flow rate should be set equal to the minimum outdoor air flow rate specified in the [Controller:OutdoorAir](#controlleroutdoorair). The heat exchanger detects that the economizer is operating by the fact that its inlet air flow rate is greater than its nominal supply air flow rate.

### Inputs

#### Field: Name

A unique user-assigned name for a particular air-to-air flat plate heat exchanger unit. Any reference to this unit by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the unit can run during a given time period. A schedule value of 0 indicates that the unit is off for that time period. A schedule value greater than 0 indicates that the unit can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Flow Arrangement Type

The user specified flow arrangement of the heat exchanger. The possible inputs are *CounterFlow*, *ParallelFlow*, or *CrossFlowBothUnmixed*.

#### Field: Economizer Lockout

This input denotes whether the heat exchanger unit is locked out (bypassed) when the air-side economizer is operating. Both the economizer and high humidity control (Ref. [Controller:OutdoorAir](#controlleroutdoorair)) activate the heat exchanger lockout as specified by this input. The input choices are *Yes* (meaning locked out) or *No*. The default input for this field is Yes.

#### Field: Ratio of Supply to Secondary hA Values

The ratio (h^.^A)~p~ / (h^.^A)~s~ at nominal flow. h is the surface convective heat transfer coefficient, A is the heat transfer area, and p and s stand for primary side and secondary side respectively. A typical value for this ratio is 1.0.

#### Field: Nominal Supply Air Flow Rate

The nominal primary side air flow rate in cubic meters per second. If the unit is operated in conjunction with an outdoor air economizer this should be equal to the minimum outdoor air flow rate. This field is autosizable.

#### Field: Nominal Supply Air Inlet Temperature

The nominal primary side air inlet temperature in Celsius.

#### Field: Nominal Supply Air Outlet Temperature

The nominal primary side air outlet temperature in Celsius.

#### Field: Nominal Secondary Air Flow Rate

The nominal secondary side air flow rate in cubic meters per second. This field is autosizable. It is equal to the primary side air flow rate defined above, if autosized.

#### Field: Nominal Secondary Air Inlet Temperature

The nominal secondary side air inlet temperature in Celsius.

#### Field: Nominal Electric Power

The electric consumption rate of the unit in watts. Electric power is considered constant whenever the unit operates. This input can be used to model electric power consumption by controls (transformers, relays, etc.) and/or a motor for a rotary heat exchanger. None of this electric power contributes thermal load to the supply or exhaust air streams. The default value for this field is 0.

#### Field: Supply Air Inlet Node Name

The name of the HVAC system node from which the unit draws its primary inlet air.

#### Field: Supply Air Outlet Node Name

The name of the HVAC system node to which the unit sends its primary outlet air.

#### Field: Secondary Air Inlet Node Name

The name of the HVAC system node from which the unit draws its secondary inlet air.

#### Field: Secondary Air Outlet Node Name

The name of the HVAC system node to which the unit sends its secondary outlet air.

An IDF example:

~~~~~~~~~~~~~~~~~~~~

      HeatExchanger:AirToAir:FlatPlate,
        OA Heat Recovery 1,      !- Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        CounterFlow,             !- Flow Arrangement Type
        Yes,                     !- Economizer Lockout
        1.0,                     !- Ratio of Supply to Secondary hA Values
        0.4333,                  !- Nominal Supply Air Flow Rate {m3/s}
        5.0,                     !- Nominal Supply Air Inlet Temperature {C}
        15.0,                    !- Nominal Supply Air Outlet Temperature {C}
        0.4333,                  !- Nominal Secondary Air Flow Rate {m3/s}
        20.0,                    !- Nominal Secondary Air Inlet Temperature {C}
        0.0,                     !- Nominal Electric Power {W}
        Desiccant Process Outlet Node,  !- Supply Air Inlet Node Name
        Heat Recovery Outlet Node,  !- Supply Air Outlet Node Name
        Relief Air Outlet Node,  !- Secondary Air Inlet Node Name
        Heat Recovery Secondary Outlet Node;  !- Secondary Air Outlet Node Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Heat Exchanger Sensible Heating Rate [W]
    HVAC,Sum,Heat Exchanger Sensible Heating Energy [J]
    HVAC,Average,Heat Exchanger Latent Gain Rate [W]
    HVAC,Sum,Heat Exchanger Latent Gain Energy [J]
    HVAC,Average,Heat Exchanger Total Heating Rate [W]
    HVAC,Sum,Heat Exchanger Total Heating Energy [J]
    HVAC,Average,Heat Exchanger Sensible Cooling Rate [W]
    HVAC,Sum,Heat Exchanger Sensible Cooling Energy [J]
    HVAC,Average,Heat Exchanger Latent Cooling Rate [W]
    HVAC,Sum,Heat Exchanger Latent Cooling Energy [J]
    HVAC,Average,Heat Exchanger Total Cooling Rate [W]
    HVAC,Sum,Heat Exchanger Total Cooling Energy [J]
    HVAC,Average,Heat Exchanger Electric Power[W]
    HVAC,Sum,Heat Exchanger Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Heat Exchanger Sensible Heating Rate [W]

This output is the sensible heating rate of the supply air by the heat exchanger in Watts. This rate is determined using the supply air mass flow rate through the heat exchanger unit, the supply air inlet and outlet conditions, and the specific heat of the inlet supply air. A positive value is reported if the supply air is heated by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Sensible Heating Energy [J]

This output is the sensible heating energy added to the supply air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Latent Gain Rate [W]

This output is the latent heating rate (humidification) of the supply air by the heat exchanger in Watts. This rate is determined by taking the difference between the Heat Exchanger Total Heating Rate and the Heat Exchanger Sensible Heating Rate. A positive value is reported if the supply air is humidified by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Latent  Heating Energy [J]

This output is the latent heating energy added to the supply air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Total Heating Rate [W]

This output is the total heating rate of the supply air by the heat exchanger in Watts. This rate is determined using the supply air mass flow rate through the heat exchanger unit, and the enthalpy of the supply air entering and leaving the unit. A positive value is reported if the enthalpy of the supply air is increased by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Total Heating Energy [J]

This output is the total heating energy added to the supply air by the heat exchanger in Joules over the timestep being reported. This output is also added to a meter with ResouceType = EnergyTransfer, EndUseKey = HeatRecoveryforHeating, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heat Exchanger Sensible Cooling Rate [W]

This output is the sensible cooling rate of the supply air by the heat exchanger in Watts. This rate is determined using the supply air mass flow rate through the heat exchanger unit, the supply air inlet and outlet conditions, and the specific heat of the inlet supply air. A positive value is reported if the supply air is cooled by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Sensible Cooling Energy [J]

This output is the sensible cooling energy added to the supply air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Latent Cooling Rate [W]

This output is the latent cooling rate (dehumidification) of the supply air by the heat exchanger in Watts. This rate is determined by taking the difference between the Heat Exchanger Total Cooling Rate and the Heat Exchanger Sensible Cooling Rate. A positive value is reported if the supply air is dehumidified by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Latent  Cooling Energy [J]

This output is the latent cooling energy added to the supply air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Total Cooling Rate [W]

This output is the total cooling rate of the supply air by the heat exchanger in Watts. This rate is determined using the supply air mass flow rate through the heat exchanger unit, and the enthalpy of the supply air entering and leaving the unit. A positive value is reported if the enthalpy of the supply air is decreased by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Total Cooling Energy [J]

This output is the total cooling energy added to the supply air by the heat exchanger in Joules over the timestep being reported. This output is also added to a meter with ResouceType = EnergyTransfer, EndUseKey = HeatRecoveryforCooling, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heat Exchanger Electric Power [W]

This output is the electric consumption rate of the unit in Watts. This rate is applicable whenever the unit operates (i.e., whenever the unit is scheduled to be available and supply and exhaust air flows exist).

#### Heat Exchanger Electric Energy [J]

This output is the electric consumption of the unit in Joules for the timestep being reported. This ouput is also added to a meter with ResourceType = Electricity, EndUseKey = HeatRecovery, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

## HeatExchanger:AirToAir:SensibleAndLatent

![Schematic of the Sensible and Latent Air-to-Air Heat Exchanger](media/schematic-of-the-sensible-and-latent-air-to.png) The sensible and latent air-to-air heat exchanger is an HVAC component typically used for exhaust or relief air heat recovery (Figure 150). Heat exchanger performance can be specified to transfer sensible energy, latent energy or both between the supply and exhaust air streams. The input requires no geometric data. Performance is defined by specifying sensible and/or latent effectiveness at 75% and 100% of the nominal (rated) supply air flow rate at two operating conditions as shown in Table 30.


Table: Operating Conditions for Defining Heat Exchanger Performance

Parameter|Conditions
---------|----------
|Heating|Cooling
Entering supply air temperature:|     Dry-bulb|     Wet-bulb||1.7ºC (35ºF)|0.6ºC (33ºF)||35ºC (95ºF)|26ºC (78ºF)
Entering exhaust air temperature:|     Dry-bulb|     Wet-bulb||21ºC (70ºF)|14ºC (58ºF)||24ºC (75ºF)|17ºC (63ºF)

Note: Conditions consistent with the Air-Conditioning and Refrigeration Institute's (ARI) Standard 1060-2001.

This object models the basic operation of an air-to-air heat exchanger. Heat exchange between the supply and exhaust air streams occurs whenever the unit is scheduled to be available (Availability schedule) and supply/exhaust air flow is present. This heat exchanger object can be used in conjunction with a conventional air-side economizer (i.e., specify ModulateFlow in the [Controller:OutdoorAir](#controlleroutdoorair) object), whereby heat exchange is suspended whenever the air-side economizer (or high humidity control) is active (i.e., air flow is fully bypassed around a fixed-plate heat exchanger or the rotation of a rotary heat exchanger is stopped). This object is also able to suspend heat exchange for the purpose of providing free cooling operation in the absence of a conventional air-side economizer (i.e., specify MinimumFlowWithBypass in the [Controller:OutdoorAir](#controlleroutdoorair) object).

During winter weather, humid exhaust air entering the heat exchanger can form frost on the cold heat exchanger surfaces, which can reduce air flow and the amount of energy recovery. Several methods are used to control or eliminate frost formation, and the following types can be modeled for this heat exchanger object: supply air preheat, minimum exhaust air temperature, exhaust air recirculation and exhaust only. For preheat frost control, a separate heating coil object must be placed in the supply inlet air stream to keep the air temperature above the frost threshold temperature. The other frost control types are modeled within this object itself (i.e., do not require a separate object to be defined) based on alpha and numeric inputs to this heat exchanger object.

Air-to-air heat exchangers are sometimes controlled to maintain a fixed supply air outlet temperature to avoid overheating when the heat exchanger is heating the supply (primary) air. To model this control in EnergyPlus, a set point manager object is used to establish a temperature set point at the supply air outlet node of the heat exchanger. Wheel speed modulation or plate supply air bypass is used to control the supply air exiting conditions to this set point. The set point for supply air temperature control should be set at the minimum economizer temperature set point if an air-side economizer is also being used by the air system. If frost control and supply air outlet temperature control are used, frost control takes precedence over supply air temperature control (e.g., frost control defrost time fraction is determined as if wheel speed modulation or plate supply air bypass is not used).

To model a sensible and latent air-to-air heat exchanger located in an air loop, the input data file should include the following objects:

AirLoopHVAC:OutdoorAirSystem

Controller:OutdoorAir

OutdoorAir:Mixer

HeatExchanger:AirToAir:SensibleAndLatent

[Coil:Heating:Water](#coilheatingwater), [Coil:Heating:Electric](#coilheatingelectric) or [Coil:Heating:Gas](#coilheatinggas) (if preheat frost control is to be modeled)

[SetpointManager:Scheduled](#setpointmanagerscheduled) (if supply air outlet temperature control is used)

The sensible and latent air-to-air heat exchanger can also be used in a number of other applications, including conditioning outdoor ventilation air and supplying it directly to a zone without an air loop. See object [ZoneHVAC:EnergyRecoveryVentilator](#zonehvacenergyrecoveryventilator) for further details on this specific application.

A description of each input field for this object is provided below.

### Inputs

#### Field: Name

A unique user-assigned name for a particular sensible/latent air-to-air heat exchanger. Any reference to this heat exchanger by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the unit can operate during a given time period. A schedule value of less than or equal to 0 indicates that the unit is off for that time period. A schedule value greater than 0 indicates that the unit can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Nominal Supply Air Flow Rate

The nominal primary side (supply) air flow rate in cubic meters per second. The actual supply and exhaust air flow rates must be between 50% and 130% of this value or a warning will be issued.

#### Field: Sensible Effectiveness at 100% Heating Air Flow

The sensible heat exchange effectiveness at the *heating* condition defined in Table 30 above with both the supply and exhaust air volume flow rates equal to 100% of the nominal supply air flow rate specified in the previous input field. The default value for this field is 0.

#### Field: Latent Effectiveness at 100% Heating Air Flow

The latent heat exchange effectiveness at the *heating* condition defined in Table 30 with both the supply and exhaust air volume flow rates equal to 100% of the nominal supply air flow rate. Specify this value as 0.0 if the heat exchanger does not transfer latent energy. The default value for this field is 0.

#### Field: Sensible Effectiveness at 75% Heating Air Flow

The sensible heat exchange effectiveness at the *heating* condition defined in Table 30 with both the supply and exhaust air volume flow rates equal to 75% of the nominal supply air flow rate. The default value for this field is 0.

#### Field: Latent Effectiveness at 75% Heating Air Flow

The latent heat exchange effectiveness at the *heating* condition defined in Table 30 with both the supply and exhaust air volume flow rates equal to 75% of the nominal supply air flow rate. Specify this value as 0.0 if the heat exchanger does not transfer latent energy. The default value for this field is 0.

#### Field: Sensible Effectiveness at 100% Cooling Air Flow

The sensible heat exchange effectiveness at the *cooling* condition defined in Table 30 with both the supply and exhaust air volume flow rates equal to 100% of the nominal supply air flow rate. The default value for this field is 0.

#### Field: Latent Effectiveness at 100% Cooling Air Flow

The latent heat exchange effectiveness at the *cooling* condition defined in Table 30 with both the supply and exhaust air volume flow rates equal to 100% of the nominal supply air flow rate. Specify this value as 0.0 if the heat exchanger does not transfer latent energy. The default value for this field is 0.

#### Field: Sensible Effectiveness at 75% Cooling Air Flow

The sensible heat exchange effectiveness at the *cooling* condition defined in Table 30 with both the supply and exhaust air volume flow rates equal to 75% of the nominal supply air flow rate. The default value for this field is 0.

#### Field: Latent Effectiveness at 75% Cooling Air Flow

The latent heat exchange effectiveness at the *cooling* condition defined in Table 30 with both the supply and exhaust air volume flow rates equal to 75% of the nominal supply air flow rate. Specify this value as 0.0 if the heat exchanger does not transfer latent energy. The default value for this field is 0.

#### Field: Supply Air Inlet Node Name

The name of the HVAC system node from which the unit draws its supply (primary) inlet air.

#### Field: Supply Air Outlet Node Name

The name of the HVAC system node to which the unit sends its supply (primary) outlet air.

#### Field: Exhaust Air Inlet Node Name

The name of the HVAC system node from which the unit draws its exhaust (secondary) inlet air.

#### Field: Exhaust Air Outlet Node Name

The name of the HVAC system node to which the unit sends its exhaust (secondary) outlet air.

#### Field: Nominal Electric Power

The electric consumption rate of the unit in watts. Electric power is considered constant whenever the unit operates. This numeric input can be used to model electric power consumption by controls (transformers, relays, etc.) and/or a motor for a rotary heat exchanger. None of this electric power contributes thermal load to the supply or exhaust air streams. The default value for this field is 0.

#### Field: Supply Air Outlet Temperature Control

This alpha field determines if the heat exchanger's supply air outlet is controlled to a temperature set point when the heat exchanger is heating the supply (primary) air. The choices for this input field are "Yes" or "No", with the default being "No". When supply air outlet temperature control is used, the wheel rotational speed modulates or supply air is bypassed around the plate heat exchanger to maintain the desired setpoint temperature. A setpoint manager object is required to establish the desired set point at the supply air outlet node (reference: [SetpointManager:Scheduled](#setpointmanagerscheduled)). When an air-side economizer is also being modeled for this air system, the set point for the supply air outlet temperature control should be equal to the economizer outdoor air temperature lower limit (reference: [Controller:OutdoorAir](#controlleroutdoorair), field Economizer Minimum Limit Dry-Bulb Temperature).

#### Field: Heat Exchanger Type

This alpha field denotes the type of heat exchanger being modeled: Plate (e.g., fixed plate) or Rotary (e.g., rotating cylinder or wheel). The default choice for this field is "Plate". The heat exchanger type affects the modeling of frost control options and supply air outlet temperature control. For rotary heat exchangers, rotational speed is varied to control frost formation or the supply air outlet temperature. For plate exchangers, air bypass around the heat exchanger is used to obtain the desired effect.

#### Field: Frost Control Type

This alpha field has four choices: None, ExhaustAirRecirculation, ExhaustOnly and MinimumExhaustTemperature. If this field is left blank, the default frost control type is "None". For modeling preheat frost control, specify "None" for this input field and insert a separate heating coil object in the supply inlet air stream to keep the air temperature above the desired frost threshold temperature.

*ExhaustAirRecirculation*: dampers are used to direct exhaust air back into the zone through the supply side of the heat exchanger when the supply (outdoor) air inlet temperature falls below a threshold temperature (defined in the next input field). The fraction of time that exhaust air is circulated through the supply side of the heat exchanger is dependent on the supply (outdoor) air inlet temperature with respect to the threshold temperature, the initial defrost time fraction, and the rate of change of defrost time fraction (see *Field: Rate of Defrost Time Fraction Increase*). When exhaust air is being recirculated, no supply (outdoor ventilation) air is being provided through the heat exchanger unit (which may or may not be acceptable regarding ventilation for occupants).

*ExhaustOnly (supply air bypass)*: this control cycles off the supply air flow through the heat exchanger for a certain period of time while the exhaust air continues to flow through the exhaust side of the heat exchanger. The fraction of time that the supply flow through the heat exchanger is cycled off is dependent on the supply (outdoor) air inlet temperature with respect to the threshold temperature, the initial defrost time fraction, and the rate of change of defrost time fraction (see *Field: Rate of Defrost Time Fraction Increase*). When implemented in real applications, provisions are usually made to avoid building depressurization when this frost control is operating (automatic or pressure-operated dampers, or a bypass air damper around the supply side of the heat exchanger). For this frost control type, it is assumed that the supply air is bypassed around the heat exchanger during frost control operation (i.e., the total supply flow is not reduced during defrost, but merely bypassed around the heat exchanger).

*MinimumExhaustTemperature*: the temperature of the exhaust air leaving the heat exchanger is monitored and the heat exchanger effectiveness is decreased (by slowing heat exchanger rotation or bypassing supply air around the plate exchanger) to keep the exhaust air from falling below the threshold temperature.

#### Field: Threshold Temperature

This numeric field defines the dry-bulb temperature of air which is used to initiate frost control. The default value is 1.7ºC. For ExhaustAirRecirculation and ExhaustOnly frost control, the threshold temperature defines the supply (outdoor) air inlet temperature below which frost control is active. For MinimumExhaustTemperature frost control, heat exchanger effectiveness is controlled to keep the exhaust air outlet temperature from falling below this threshold temperature value.

The appropriate threshold temperature varies with exhaust (inlet) air temperature and humidity, frost control type, heat exchanger type, and whether the heat exchanger transfers sensible energy alone or both sensible and latent energy (enthalpy). Typical threshold temperatures are provided in Table 31 below. However, it is recommended that the user consult manufacturer's information for the specific air-to-air heat exchanger being modeled.

Table: Typical threshold temperatures

Frost control type|Heat exchanger type|Energy exchange|Threshold temperature
------------------|-------------------|---------------|---------------------
Exhaust air recirculation|Plate|Sensible-only|-1.1ºC (30ºF)
||Sensible + latent|-12.2ºC (10ºF)
|Rotary|Sensible-only|-12.2ºC (10ºF)
||Sensible + latent|-23.3ºC (-10ºF)
Exhaust only|Plate|Sensible-only|-1.1ºC (30ºF)
||Sensible + latent|-12.2ºC (10ºF)
|Rotary|Sensible-only|-12.2ºC (10ºF)
||Sensible + latent|-23.3ºC (-10ºF)
Minimum exhaust temperature|Plate|Sensible-only|1.7ºC (35ºF)
||Sensible + latent|1.7ºC (35ºF)
|Rotary|Sensible-only|1.7ºC (35ºF)
||Sensible + latent|1.7ºC (35ºF)
Preheat\*\*|Plate|Sensible-only|-1.1ºC (30ºF)
||Sensible + latent|-12.2ºC (10ºF)
|Rotary|Sensible-only|-12.2ºC (10ºF)
||Sensible + latent|-23.3ºC (-10ºF)

Source: Indoor Humidity Assessment Tool, U.S. Environmental Protection Agency, http://www.epa.gov/iaq/schooldesign/saves.html

\*\* To model preheat frost control, specify frost control type as "None" and place a heating coil in the supply inlet air stream controlled to the keep the air temperature above the frost threshold temperature.

#### Field: Initial Defrost Time Fraction

This numeric field defines the fraction of the simulation timestep when frost control will be invoked when the threshold temperature is reached. This field is only used for the ExhaustAirRecirculation and ExhaustOnly frost control types. The value for this field must be ≥ 0 and ≤ 1. The default time fraction is 0.083 (e.g., 5 min / 60 min) which is typical for ExhaustAirRecirculation frost control. Higher initial defrost time fractions (e.g., 0.167 = 10 min / 60 min) are typically required for ExhaustOnly frost control. For best results, the user should obtain this information from the manufacturer.

#### Field: Rate of Defrost Time Fraction Increase

This numeric field defines the rate of increase in the defrost time fraction as the supply (outdoor) air inlet temperature falls below the threshold temperature. This field is only used for the ExhaustAirRecirculation and ExhaustOnly frost control types. The value for this field must be ≥ 0. The default value is 0.012 (e.g., 0.72 min / 60 min per degree C temperature difference) which is typical for ExhaustAirRecirculation frost control. Higher values (e.g., 0.024 = 1.44 min / 60 min per degree C temperature difference) are typically required for ExhaustOnly frost control. For best results, the user should obtain this information from the manufacturer.

This value is used to determine the total defrost time fraction as follows:

Total defrost time fraction = Initial Defrost Time Fraction +

      Rate of Defrost Time Fraction Increase \* (T~threshold~ – T~supply air inlet~)

The model does not allow the total defrost time fraction to exceed 1.0 or be less than 0.

#### Field: Economizer Lockout

This input denotes whether the heat exchanger unit is locked out (bypassed for plate type heat exchangers or the rotation is suspended for rotary type heat exchangers) when the air-side economizer is operating. Both the economizer and high humidity control (Ref. [Controller:OutdoorAir](#controlleroutdoorair)) activate the heat exchanger lockout as specified by this input. The input choices are *Yes* (meaning locked out) or *No*. The default input for this field is Yes.

Following is an example input for this heat exchanger object:

~~~~~~~~~~~~~~~~~~~~

      HeatExchanger:AirToAir:SensibleAndLatent,
        OA Heat Recovery 1,      !- Name
        FanAndCoilAvailSched,    !- Availability Schedule Name
        0.4333,                  !- Nominal Supply Air Flow Rate {m3/s}
        .76,                     !- Sensible Effectiveness at 100% Heating Air Flow {dimensionless}
        .68,                     !- Latent Effectiveness at 100% Heating Air Flow {dimensionless}
        .81,                     !- Sensible Effectiveness at 75% Heating Air Flow {dimensionless}
        .73,                     !- Latent Effectiveness at 75% Heating Air Flow {dimensionless}
        .76,                     !- Sensible Effectiveness at 100% Cooling Air Flow {dimensionless}
        .68,                     !- Latent Effectiveness at 100% Cooling Air Flow {dimensionless}
        .81,                     !- Sensible Effectiveness at 75% Cooling Air Flow {dimensionless}
        .73,                     !- Latent Effectiveness at 75% Cooling Air Flow {dimensionless}
        Outside Air Inlet Node Preheated,  !- Supply Air Inlet Node Name
        Heat Recovery Outlet Node,  !- Supply Air Outlet Node Name
        Relief Air Outlet Node,  !- Exhaust Air Inlet Node Name
        Heat Recovery Secondary Outlet Node,  !- Exhaust Air Outlet Node Name
        200.0,                   !- Nominal Electric Power {W}
        No,                      !- Supply Air Outlet Temperature Control
        Plate,                   !- Heat Exchanger Type
        None;                    !- Frost Control Type
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Heat Exchanger Sensible Heating Rate [W]
    HVAC,Sum,Heat Exchanger Sensible Heating Energy [J]
    HVAC,Average,Heat Exchanger Latent Gain Rate [W]
    HVAC,Sum,Heat Exchanger Latent Gain Energy [J]
    HVAC,Average,Heat Exchanger Total Heating Rate [W]
    HVAC,Sum,Heat Exchanger Total Heating Energy [J]
    HVAC,Average,Heat Exchanger Sensible Cooling Rate [W]
    HVAC,Sum,Heat Exchanger Sensible Cooling Energy [J]
    HVAC,Average,Heat Exchanger Latent Cooling Rate [W]
    HVAC,Sum,Heat Exchanger Latent Cooling Energy [J]
    HVAC,Average,Heat Exchanger Total Cooling Rate [W]
    HVAC,Sum,Heat Exchanger Total Cooling Energy [J]
    HVAC,Average,Heat Exchanger Electric Power[W]
    HVAC,Sum,Heat Exchanger Electric Energy [J]
    HVAC,Average,Heat Exchanger Sensible Effectiveness[]
    HVAC,Average,Heat Exchanger Latent Effectiveness[]
    HVAC,Average,Heat Exchanger Supply Air Bypass Mass Flow Rate [kg/s]
    HVAC,Average,Heat Exchanger Exhaust Air Bypass Mass Flow Rate [kg/s] HVAC,Average,Heat Exchanger Defrost Time Fraction[]
~~~~~~~~~~~~~~~~~~~~

#### Heat Exchanger Sensible Heating Rate [W]

This output is the sensible heating rate of the supply air by the heat exchanger in Watts. This rate is determined using the supply air mass flow rate through the heat exchanger unit, the supply air inlet and outlet conditions, and the specific heat of the inlet supply air. A positive value is reported if the supply air is heated by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Sensible Heating Energy [J]

This output is the sensible heating energy added to the supply air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Latent Gain Rate [W]

This output is the latent heating rate (humidification) of the supply air by the heat exchanger in Watts. This rate is determined by taking the difference between the Heat Exchanger Total Heating Rate and the Heat Exchanger Sensible Heating Rate. A positive value is reported if the supply air is humidified by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Latent Gain Energy [J]

This output is the latent heating energy added to the supply air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Total Heating Rate [W]

This output is the total heating rate of the supply air by the heat exchanger in Watts. This rate is determined using the supply air mass flow rate through the heat exchanger unit, and the enthalpy of the supply air entering and leaving the unit. A positive value is reported if the enthalpy of the supply air is increased by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Total Heating Energy [J]

This output is the total heating energy added to the supply air by the heat exchanger in Joules over the timestep being reported. This output is also added to a meter with ResouceType = EnergyTransfer, EndUseKey = HeatRecoveryforHeating, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heat Exchanger Sensible Cooling Rate [W]

This output is the sensible cooling rate of the supply air by the heat exchanger in Watts. This rate is determined using the supply air mass flow rate through the heat exchanger unit, the supply air inlet and outlet conditions, and the specific heat of the inlet supply air. A positive value is reported if the supply air is cooled by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Sensible Cooling Energy [J]

This output is the sensible cooling energy added to the supply air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Latent Cooling Rate [W]

This output is the latent cooling rate (dehumidification) of the supply air by the heat exchanger in Watts. This rate is determined by taking the difference between the Heat Exchanger Total Cooling Rate and the Heat Exchanger Sensible Cooling Rate. A positive value is reported if the supply air is dehumidified by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Latent  Cooling Energy [J]

This output is the latent cooling energy added to the supply air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Total Cooling Rate [W]

This output is the total cooling rate of the supply air by the heat exchanger in Watts. This rate is determined using the supply air mass flow rate through the heat exchanger unit, and the enthalpy of the supply air entering and leaving the unit. A positive value is reported if the enthalpy of the supply air is decreased by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Total Cooling Energy [J]

This output is the total cooling energy added to the supply air by the heat exchanger in Joules over the timestep being reported. This output is also added to a meter with ResouceType = EnergyTransfer, EndUseKey = HeatRecoveryforCooling, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heat Exchanger Electric Power [W]

This output is the electric consumption rate of the unit in Watts. This rate is applicable whenever the unit operates (i.e., whenever the unit is scheduled to be available and supply and exhaust air flows exist).

#### Heat Exchanger Electric Energy [J]

This output is the electric consumption of the unit in Joules for the timestep being reported. This ouput is also added to a meter with ResourceType = Electricity, EndUseKey = HeatRecovery, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heat Exchanger Sensible Effectiveness []

This output is the average sensible effectiveness of the heat exchanger (excluding bypass air, if any) over the timestep being reported.

#### Heat Exchanger Latent Effectiveness []

This output is the average latent effectiveness of the heat exchanger (excluding bypass air, if any) over the timestep being reported.

#### Heat Exchanger Supply Air Bypass Mass Flow Rate [kg/s]

This output is the average mass flow rate in kg/second of the supply (primary) air stream that is bypassing the heat exchanger over the timestep being reported. This flow rate is equal to the total supply mass flow rate through the heat exchanger **unit** minus the amount passing through the supply side of the heat exchanger **core**.

#### Heat Exchanger Exhaust Air Bypass Mass Flow Rate [kg/s]

This output is the average mass flow rate in kg/second of the exhaust (secondary) air stream that is bypassing the heat exchanger over the timestep being reported. This flow rate is equal to the total exhaust mass flow rate through the heat exchanger **unit** minus the amount passing through the exhaust side of the heat exchanger **core**.

#### Heat Exchanger Defrost Time Fraction []

This output is the average fraction of the reporting timestep when frost control is being implemented.

## HeatExchanger:Desiccant:BalancedFlow

This desiccant heat exchanger object is an HVAC component used to model both temperature (sensible) and moisture (latent) heat exchange between two air streams (Figure 151). The model assumes balanced air flow through the regeneration and process sides of the heat exchanger (i.e., regeneration and process air volume flow rates and face velocities are the same). Heat exchanger performance is specified through a performance data type object (e.g., [HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1](#heatexchangerdesiccantbalancedflowperformancedatatype1)).

![Schematic of the Balanced Flow Desiccant Heat Exchanger](media/schematic-of-the-balanced-flow-desiccant-heat.jpeg)


Currently, this desiccant heat exchanger model can be referenced by two compound objects: [CoilSystem:Cooling:DX:HeatExchangerAssisted](#coilsystemcoolingdxheatexchangerassisted) and [Dehumidifier:Desiccant:System](#dehumidifierdesiccantsystem), both of which are used to provide enhanced dehumidification over conventional systems. If this heat exchanger is referenced by a compound object, the compound object will control heat exchanger operation (i.e., tell the heat exchanger if heat exchange is needed or not for each simulation timestep).

This desiccant heat exchanger object may also be specified directly in an [AirLoopHVAC](#airloophvac) (air loop [BranchList](#branchlist)) or in an [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem) ([AirLoopHVAC:OutdoorAirSystem:EquipmentList](#airloophvacoutdoorairsystemequipmentlist)) without being referenced by a compound object. If specified directly in an [AirLoopHVAC](#airloophvac) or [AirLoopHVAC:OutdoorAirSystem](#airloophvacoutdoorairsystem), then the heat exchanger can be controlled to provide heat exchange based on a maximum and/or minimum humidity set point placed on the process air outlet node (ref. Setpoint Managers). If no humidity set points are provided on this node, then heat exchange will be provided whenever the heat exchanger is available to operate (via its availability schedule) and there is a temperature and/or humidity ratio difference between the two air streams.

Inputs to this model include an availability schedule name, inlet and outlet air node names, and the type and name of the heat exchanger performance data object. A description of each input field for this object is provided below.

### Inputs

#### Field: Name

A unique, user-assigned name for a particular balanced flow desiccant heat exchanger. Any reference to this heat exchanger by another object will use this name.

#### Field: Availability Schedule Name

The name of the schedule (ref: Schedule) that denotes whether the heat exchanger can operate during a given time period. A schedule value of less than or equal to 0 indicates that the unit is off for that time period. A schedule value greater than 0 indicates that the unit can operate during the time period. If this field is blank, the schedule has values of 1 for all time periods.

#### Field: Regeneration Air Inlet Node Name

The name of the HVAC system node from which the unit draws its regeneration inlet air.

#### Field: Regeneration Air Outlet Node Name

The name of the HVAC system node to which the unit sends its regeneration outlet air.

#### Field: Process Air Inlet Node Name

The name of the HVAC system node from which the unit draws its process inlet air.

#### Field: Process Air Outlet Node Name

The name of the HVAC system node to which the unit sends its process outlet air.

#### Field: Heat Exchanger Performance Object Type

This alpha field contains the type of model used to simulate the desiccant heat exchanger's thermal performance and electrical energy use. Currently, the only valid choice is [HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1](#heatexchangerdesiccantbalancedflowperformancedatatype1).

#### Field: Heat Exchanger Performance Name

This alpha field contains the identifying name of the specific heat exchanger performance type object (e.g., [HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1](#heatexchangerdesiccantbalancedflowperformancedatatype1)) that defines the performance for this heat exchanger. A single heat exchanger performance type object may be used to define performance for many [HeatExchanger:Desiccant:BalancedFlow](#heatexchangerdesiccantbalancedflow) objects (i.e., the same name may be used in this input field for more than one balanced flow desiccant heat exchanger).

#### Field: Economizer Lockout

This input denotes whether the heat exchanger unit is locked out (heat exchanger rotation is suspended) when the air-side economizer is operating. Both the economizer and high humidity control (Ref. [Controller:OutdoorAir](#controlleroutdoorair)) activate the heat exchanger lockout as specified by this input. The input choices are *Yes* (meaning locked out) or *No*. Economizer lockout is typically *not* used for this type of heat exchanger. For this reason, the default input for this field is No.

Following is an example input for this heat exchanger object:

~~~~~~~~~~~~~~~~~~~~

    HeatExchanger:Desiccant:BalancedFlow,
     Desiccant Heat Exchanger 1,         !- Name
     OfficeHeatCoolAvail,                !- Availability Schedule Name
     DXSystem 1 Mixed Air Node,          !- Regeneration Air Inlet Node Name
     DX Cooling Coil Air Inlet Node,     !- Regeneration Air Outlet Node Name
     DX Cooling Coil Air Outlet Node,    !- Process Air Inlet Node Name
     DXSystem 1 Fan Air Inlet Node,      !- Process Air Outlet Node Name
     HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1, !- Heat Exchanger Performance Object Type
     HXDesPerf1;                         !- Heat Exchanger Performance Name
~~~~~~~~~~~~~~~~~~~~

### Outputs

~~~~~~~~~~~~~~~~~~~~

    HVAC,Average,Heat Exchanger Sensible Heating Rate [W]
    HVAC,Sum,Heat Exchanger Sensible Heating Energy [J]
    HVAC,Average,Heat Exchanger Latent Gain Rate [W]
    HVAC,Sum,Heat Exchanger Latent Gain Energy [J]
    HVAC,Average,Heat Exchanger Total Heating Rate [W]
    HVAC,Sum,Heat Exchanger Total Heating Energy [J]
    HVAC,Average,Heat Exchanger Sensible Cooling Rate [W]
    HVAC,Sum,Heat Exchanger Sensible Cooling Energy [J]
    HVAC,Average,Heat Exchanger Latent Cooling Rate [W]
    HVAC,Sum,Heat Exchanger Latent Cooling Energy [J]
    HVAC,Average,Heat Exchanger Total Cooling Rate [W]
    HVAC,Sum,Heat Exchanger Total Cooling Energy [J]
    HVAC,Average,Heat Exchanger Electric Power[W]
    HVAC,Sum,Heat Exchanger Electric Energy [J]
~~~~~~~~~~~~~~~~~~~~

#### Heat Exchanger Sensible Heating Rate [W]

This output is the sensible heating rate of the process air by the heat exchanger in Watts. This rate is determined using the process air mass flow rate through the heat exchanger unit, the process air inlet and outlet temperatures, and the specific heat of the inlet process air. A positive value is reported if the process air is heated by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Sensible Heating Energy [J]

This output is the sensible heating energy added to the process air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Latent Gain Rate [W]

This output is the latent heating rate (humidification) of the process air by the heat exchanger in Watts. This rate is determined by taking the difference between the heat exchanger's total heat recovery rate and the sensible heat recovery rate. A positive value is reported if the process air is humidified by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Latent Gain Energy [J]

This output is the latent heating energy added to the process air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Total Heating Rate [W]

This output is the total heating rate of the process air by the heat exchanger in Watts. This rate is determined using the process air mass flow rate through the heat exchanger unit and the enthalpy of the air entering and leaving the process side of the unit. A positive value is reported if the enthalpy of the process air is increased by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Total Heating Energy [J]

This output is the total heating energy added to the process air by the heat exchanger in Joules over the timestep being reported. This output is also added to a meter with ResouceType = EnergyTransfer, EndUseKey = HeatRecoveryforHeating, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heat Exchanger Sensible Cooling Rate [W]

This output is the sensible cooling rate of the process air by the heat exchanger in Watts. This rate is determined using the process air mass flow rate through the heat exchanger unit, the process air inlet and outlet temperatures, and the specific heat of the inlet process air. A positive value is reported if the process air is cooled by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Sensible Cooling Energy [J]

This output is the sensible cooling energy added to the process air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Latent Cooling Rate [W]

This output is the latent cooling rate (dehumidification) of the process air by the heat exchanger in Watts. This rate is determined by taking the difference between the heat exchanger's total heat recovery rate and the sensible heat recovery rate. A positive value is reported if the process air is dehumidified by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Latent Cooling Energy [J]

This output is the latent cooling energy added to the process air by the heat exchanger in Joules over the timestep being reported.

#### Heat Exchanger Total Cooling Rate [W]

This output is the total cooling rate of the process air by the heat exchanger in Watts. This rate is determined using the process air mass flow rate through the heat exchanger unit and the enthalpy of the air entering and leaving the process side of the unit. A positive value is reported if the enthalpy of the process air is decreased by the heat exchanger, else the rate is set to zero.

#### Heat Exchanger Total Cooling Energy [J]

This output is the total cooling energy added to the process air by the heat exchanger in Joules over the timestep being reported. This output is also added to a meter with ResouceType = EnergyTransfer, EndUseKey = HeatRecoveryforCooling, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

#### Heat Exchanger Electric Power [W]

This output is the average electric consumption rate of the unit in Watts. This average rate includes portions of the simulation timestep when the heat exchanger may be cycled off (i.e., average of on and off periods, as appropriate).

#### Heat Exchanger Electric Energy [J]

This output is the electric consumption of the unit in Joules for the timestep being reported. This ouput is also added to a meter with ResourceType = Electricity, EndUseKey = HeatRecovery, GroupKey = System (ref. [Output:Meter](#outputmeter-and-outputmetermeterfileonly) objects).

## HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1

This object specifies a performance model and model coefficients for a balanced flow desiccant heat exchanger. A [HeatExchanger:Desiccant:BalancedFlow](#heatexchangerdesiccantbalancedflow) object will reference a [HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1](#heatexchangerdesiccantbalancedflowperformancedatatype1) object. This performance data object is used to specify the thermal performance and electric consumption of the heat exchanger. Some representative inputs for this object are provided in the EnergyPlus Reference DataSets (PerfCurves.idf and AllDataSets.idf).

This model predicts the regeneration air stream outlet temperature and humidity ratio values based on the entering regeneration and process air stream temperature, humidity ratio and face velocity. The process air stream outlet humidity ratio and temperatures are calculated based on a simple heat and moisture balance. The model requires that the user enter the nominal volumetric flow rate and a nominal face velocity, electric power consumption, empirical model coefficients for the regeneration outlet air temperature and humidity ratio equations, and the applicable minimum and maximum values for both the independent and dependent variables for the empirical model coefficients provided. This model is based on the following equations:

- The dry-bulb temperature of the regeneration outlet air is defined using the following equation:

![](media/image410.png)\


where,

![](media/image411.png)  = Regeneration outlet air dry-bulb temperature (C)

![](media/image412.png)  = Regeneration inlet air humidity ratio (kgWater/kgDryAir)

![](media/image413.png)   = Regeneration inlet air dry-bulb temperature (C)

![](media/image414.png)  = Process inlet air humidity ratio (kgWater/kgDryAir)

![](media/image415.png)   = Process inlet air dry-bulb temperature (C)

![](media/image416.png)  = Regeneration (and process) face velocity (m/s)

- The humidity ratio of the regeneration outlet air is defined using the same empirical equation form; however, different coefficients are used as follows:

![](media/image417.png)\


where,

![](media/image418.png)  = Regeneration outlet air humidity ratio (kgWater/kgDryAir)

- The process outlet air conditions are then calculated based on the regeneration outlet air conditions (temperature or humidity ratio) calculated above, the actual regeneration inlet air conditions entering the heat exchanger, and assuming the same amount of sensible and total energy transfer across both the process and regeneration sides of the heat exchanger. The difference between the actual inlet air conditions and RTI/RWI/PTI/PWI is made here because user-defined limits for RTI/RWI/PTI/PWI may result in a difference between the actual conditions and those used in the equations shown above (see the EnergyPlus Engineering Reference for further discussion).

A description of each input field for this object is provided below.

### Inputs

#### Field: Name

This alpha field contains the identifying name for this set of desiccant heat exchanger performance data. Any reference to this performance data set by another object (e.g., [HeatExchanger:Desiccant:BalancedFlow](#heatexchangerdesiccantbalancedflow)) will use this name.

#### Field: Nominal Air Flow Rate

The nominal air volume flow rate in cubic meters per second. This model assumes balanced air flow (i.e., the same air volume flow rate across the process and regeneration sides of the heat exchanger). The minimum value should be greater than 0. This field is autosizable.

#### Field: Nominal Air Face Velocity

This numeric field contains the nominal air velocity across the heat exchanger face area in meters per second. It is assumed that this air velocity is the same for both sides of the heat exchanger. This value, along with the Nominal Air Flow Rate sets the heat exchanger face area. With this fixed face area, the air face velocity is calculated every simulation timestep based on the actual air volume flow rate for the timestep. The minimum value should be greater than 0 and less than or equal to 6. The default value is 3.0.

#### Field: Nominal Electric Power

This numeric field contains the nominal electric consumption rate of the heat exchanger in watts. This electric power is considered constant and is consumed only when the heat exchanger operates (e.g., for all or a portion of the simulation timestep, as appropriate). This numeric input can be used to model electric power consumption by controls (transformers, relays, etc.) and/or a motor for a rotary heat exchanger. None of this electric power contributes thermal load to the process or regeneration air streams. The minimum value should be greater than or equal to 0. The default value for this field is also 0.

The coefficients for the regeneration air outlet temperature equation described above are defined in the following eight fields:

#### Field: Temperature Equation Coefficient 1

The constant coefficient (B~1~) in the temperature equation shown above (RTO).

#### **Field: Temperature Equation Coefficient 2**

The coefficient (B~2~) in the temperature equation shown above (RTO).

#### Field: Temperature Equation Coefficient 3

The coefficient (B~3~) in the temperature equation shown above (RTO).

#### Field: Temperature Equation Coefficient 4

The coefficient (B~4~) in the temperature equation shown above (RTO).

#### Field: Temperature Equation Coefficient 5

The coefficient (B~5~) in the temperature equation shown above (RTO).

#### **Field: Temperature Equation Coefficient 6**

The coefficient (B~6~) in the temperature equation shown above (RTO).

#### Field: Temperature Equation Coefficient 7

The coefficient (B~7~) in the temperature equation shown above (RTO).

#### Field: Temperature Equation Coefficient 8

The coefficient (B~8~) in the temperature equation shown above (RTO).

The following 16 fields are used to establish the valid range for the dependent and independent variables associated with the coefficients defined above (B~1~ through B~8~) for the regeneration outlet air temperature equation.

#### Field: Minimum Regeneration Inlet Air Humidity Ratio for Temperature Equation

The minimum allowable value of RWI in the temperature equation shown above (kgWater/kgDryAir). Values of RWI less than the minimum will be replaced by this minimum when calculating the regeneration outlet air temperature and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Maximum Regeneration Inlet Air Humidity Ratio for Temperature Equation

The maximum allowable value of RWI in the temperature equation shown above (kgWater/kgDryAir). Values of RWI greater than the maximum will be replaced by this maximum when calculating the regeneration outlet air temperature and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Minimum Regeneration Inlet Air Temperature for Temperature Equation

The minimum allowable value of RTI in the temperature equation shown above (C). Values of RTI less than the minimum will be replaced by this minimum when calculating the regeneration outlet air temperature and a warning message will be issued.

#### Field: Maximum Regeneration Inlet Air Temperature for Temperature Equation

The maximum allowable value of RTI in the temperature equation shown above (C). Values of RTI greater than the maximum will be replaced by this maximum when calculating the regeneration outlet air temperature and a warning message will be issued.

#### Field: Minimum Process Inlet Air Humidity Ratio for Temperature Equation

The minimum allowable value of PWI in the temperature equation shown above (kg/kg). Values of PWI less than the minimum will be replaced by this minimum when calculating the regeneration outlet air temperature and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Maximum Process Inlet Air Humidity Ratio for Temperature Equation

The maximum allowable value of PWI in the temperature equation shown above (kg/kg). Values of PWI greater than the maximum will be replaced by this maximum when calculating the regeneration outlet air temperature and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Minimum Process Inlet Air Temperature for Temperature Equation

The minimum allowable value of PTI in the temperature equation shown above (C). Values of PTI less than the minimum will be replaced by this minimum when calculating the regeneration outlet air temperature and a warning message will be issued.

#### Field: Maximum Process Inlet Air Temperature for Temperature Equation

The maximum allowable value of PTI in the temperature equation shown above (C). Values of PTI greater than the maximum will be replaced by this maximum when calculating the regeneration outlet air temperature and a warning message will be issued.

#### Field: Minimum Regeneration Air Velocity for Temperature Equation

The minimum allowable value of RFV in the temperature equation shown above (m/s). Values of RFV less than the minimum will be replaced by this minimum when calculating the regeneration outlet air temperature and a warning message will be issued. The minimum value for this input field should be greater than 0.

#### Field: Maximum Regeneration Air Velocity for Temperature Equation

The maximum allowable value of RFV in the temperature equation shown above (m/s). Values of RFV greater than the maximum will be replaced by this maximum value when calculating the regeneration outlet air temperature and a warning message will be issued. The minimum value for this input field should be greater than 0.

#### Field: Minimum Regeneration Outlet Air Temperature for Temperature Equation

The minimum value of RTO resulting from the temperature equation shown above (C). If RTO is less than this value, RTO will be replaced by this minimum value and a warning message will be issued.

#### Field: Maximum Regeneration Outlet Air Temperature for Temperature Equation

The maximum value of RTO resulting from the temperature equation shown above (C). If RTO is greater than this value, RTO will be replaced by this maximum value and a warning message will be issued.

#### Field: Minimum Regeneration Inlet Air Relative Humidity for Temperature Equation

The minimum relative humidity of the regeneration inlet air for the temperature equation shown above (percent). If the relative humidity of the regeneration inlet air is below this value, a warning message will be issued. The valid range for this input field is 0.0 to 100.0.

#### Field: Maximum Regeneration Inlet Air Relative Humidity for Temperature Equation

The maximum relative humidity of the regeneration inlet air for the temperature equation shown above (percent). If the relative humidity of the regeneration inlet air is above this value, a warning message will be issued. The valid range for this input field is 0.0 to 100.0.

#### Field: Minimum Process Inlet Air Relative Humidity for Temperature Equation

The minimum relative humidity of the process inlet air for the temperature equation shown above (percent). If the relative humidity of the process inlet air is below this value, a warning message will be issued. The valid range for this input field is 0.0 to 100.0.

#### Field: Maximum Process Inlet Air Relative Humidity for Temperature Equation

The maximum relative humidity of the process inlet air for the temperature equation shown above (percent). If the relative humidity of the process inlet air is above this value, a warning message will be issued. The valid range for this input field is 0.0 to 100.0.

The coefficients for the regeneration outlet air humidity ratio equation are defined in the following eight fields:

#### Field: Humidity Ratio Equation Coefficient 1

The constant coefficient (C~1~) in the humidity ratio equation shown above (RWO).

#### **Field:** Humidity Ratio Equation Coefficient 2

The coefficient (C~2~) in the humidity ratio equation shown above (RWO).

#### Field: Humidity Ratio Equation Coefficient 3

The coefficient (C~3~) in the humidity ratio equation shown above (RWO).

#### Field: Humidity Ratio Equation Coefficient 4

The coefficient (C~4~) in the humidity ratio equation shown above (RWO).

#### Field: Humidity Ratio Equation Coefficient 5

The coefficient (C~5~) in the humidity ratio equation shown above (RWO).

#### **Field:** Humidity Ratio Equation Coefficient 6

The coefficient (C~6~) in the humidity ratio equation shown above (RWO).

#### Field: Humidity Ratio Equation Coefficient 7

The coefficient (C~7~) in the humidity ratio equation shown above (RWO).

#### Field: Humidity Ratio Equation Coefficient 8

The coefficient (C~8~) in the humidity ratio equation shown above (RWO).

The following 16 fields are used to establish the valid range for the dependent and independent variables associated with the coefficients defined above (C~1~ through C~8~) for the regeneration outlet air humidity ratio equation.

#### Field: Minimum Regeneration Inlet Air Humidity Ratio for Humidity Ratio Equation

The minimum allowable value of RWI in the humidity ratio equation shown above (kgWater/kgDryAir). Values of RWI less than the minimum will be replaced by this minimum when calculating the regeneration outlet air humidity ratio and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Maximum Regeneration Inlet Air Humidity Ratio for Humidity Ratio Equation

The maximum allowable value of RWI in the humidity ratio equation shown above (kgWater/kgDryAir). Values of RWI greater than the maximum will be replaced by this maximum when calculating the regeneration outlet air humidity ratio and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Minimum Regeneration Inlet Air Temperature for Humidity Ratio Equation

The minimum allowable value of RTI in the humidity ratio equation shown above (C). Values of RTI less than the minimum will be replaced by this minimum when calculating the regeneration outlet air humidity ratio and a warning message will be issued.

#### Field: Maximum Regeneration Inlet Air Temperature for Humidity Ratio Equation

The maximum allowable value of RTI in the humidity ratio equation shown above (C). Values of RTI greater than the maximum will be replaced by this maximum when calculating the regeneration outlet air humidity ratio and a warning message will be issued.

#### Field: Minimum Process Inlet Air Humidity Ratio for Humidity Ratio Equation

The minimum allowable value of PWI in the humidity ratio equation shown above (kgWater/kgDryAir). Values of PWI less than the minimum will be replaced by this minimum when calculating the regeneration outlet air humidity ratio and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Maximum Process Inlet Air Humidity Ratio for Humidity Ratio Equation

The maximum allowable value of PWI in the humidity ratio equation shown above (kgWater/kgDryAir). Values of PWI greater than the maximum will be replaced by this maximum when calculating the regeneration outlet air humidity ratio and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Minimum Process Inlet Air Temperature for Humidity Ratio Equation

The minimum allowable value of PTI in the humidity ratio equation shown above (C). Values of PTI less than the minimum will be replaced by this minimum when calculating the regeneration outlet air humidity ratio and a warning message will be issued.

#### Field: Maximum Process Inlet Air Temperature for Humidity Ratio Equation

The maximum allowable value of PTI in the humidity ratio equation shown above (C). Values of PTI greater than the maximum will be replaced by this maximum when calculating the regeneration outlet air humidity ratio and a warning message will be issued.

#### Field: Minimum Regeneration Air Velocity for Humidity Ratio Equation

The minimum allowable value of RFV in the humidity ratio equation shown above (m/s). Values of RFV less than the minimum will be replaced by this minimum when calculating the regeneration outlet air humidity ratio and a warning message will be issued. The minimum value for this input field should be greater than 0.

#### Field: Maximum Regeneration Air Velocity for Humidity Ratio Equation

The maximum allowable value of RFV in the humidity ratio equation shown above (m/s). Values of RFV greater than the maximum will be replaced by this maximum when calculating the regeneration outlet air humidity ratio and a warning message will be issued. The minimum value for this input field should be greater than 0.

#### Field: Minimum Regeneration Outlet Air Humidity Ratio for Humidity Ratio Equation

The minimum value of RWO resulting from the humidity ratio equation shown above (kgWater/kgDryAir). If RWO is less than this value, RWO will be replaced by this minimum value and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Maximum Regeneration Outlet Air Humidity Ratio for Humidity Ratio Equation

The maximum value of RWO resulting from the humidity ratio equation shown above (kgWater/kgDryAir). If RWO is greater than this value, RWO will be replaced by this maximum value and a warning message will be issued. The valid range for this input field is 0.0 to 1.0.

#### Field: Minimum Regeneration Inlet Air Relative Humidity for Humidity Ratio Equation

The minimum relative humidity of the regeneration inlet air for the humidity ratio equation shown above (percent). If the relative humidity of the regeneration inlet air is below this value, a warning message will be issued. The valid range for this input field is 0.0 to 100.0.

#### Field: Maximum Regeneration Inlet Air Relative Humidity for Humidity Ratio Equation

The maximum relative humidity of the regeneration inlet air for the humidity ratio equation shown above (percent). If the relative humidity of the regeneration inlet air is above this value, a warning message will be issued. The valid range for this input field is 0.0 to 100.0.

#### Field: Minimum Process Inlet Air Relative Humidity for Humidity Ratio Equation

The minimum relative humidity of the process inlet air for the humidity ratio equation shown above (percent). If the relative humidity of the process inlet air is below this value, a warning message will be issued. The valid range for this input field is 0.0 to 100.0.

#### Field: Maximum Process Inlet Air Relative Humidity for Humidity Ratio Equation

The maximum relative humidity of the process inlet air for the humidity ratio equation shown above (percent). If the relative humidity of the process inlet air is above this value, a warning message will be issued. The valid range for this input field is 0.0 to 100.0.

Following is an example input for this heat exchanger performance data type object:

~~~~~~~~~~~~~~~~~~~~

      HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1,
        HXDesPerf1,     !- Name
        1.05,           !- Nominal Air Flow Rate {m3/s}
        3.25,           !- Nominal Air Face Velocity {m/s}
        50.0,           !- Nominal Electric Power {W}
        -2.53636E+00,   !- Temperature Equation Coefficient 1
        2.13247E+01,    !- Temperature Equation Coefficient 2
        9.23308E-01,    !- Temperature Equation Coefficient 3
        9.43276E+02,    !- Temperature Equation Coefficient 4
        -5.92367E+01,   !- Temperature Equation Coefficient 5
        -4.27465E-02,   !- Temperature Equation Coefficient 6
        1.12204E+02,    !- Temperature Equation Coefficient 7
        7.78252E-01,    !- Temperature Equation Coefficient 8
~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

        0.007143,       !- Minimum Regeneration Inlet Air Humidity Ratio for Temperature Equation
        0.024286,       !- Maximum Regeneration Inlet Air Humidity Ratio for Temperature Equation
        46.111110,      !- Minimum Regeneration Inlet Air Temperature for Temperature Equation {C}
        46.111112,      !- Maximum Regeneration Inlet Air Temperature for Temperature Equation {C}
        0.005000,       !- Minimum Process Inlet Air Humidity Ratio for Temperature Equation
        0.015714,       !- Maximum Process Inlet Air Humidity Ratio for Temperature Equation
        4.583333,       !- Minimum Process Inlet Air Temperature for Temperature Equation {C}
        21.83333,       !- Maximum Process Inlet Air Temperature for Temperature Equation {C}
        2.286,          !- Minimum Regeneration Air Velocity for Temperature Equation {m/s}
        4.826,          !- Maximum Regeneration Air Velocity for Temperature Equation {m/s}
        35.0,           !- Minimum Regeneration Outlet Air Temperature for Temperature Equation {C}
        50.0,           !- Maximum Regeneration Outlet Air Temperature for Temperature Equation {C}
        5.0,            !- Minimum Regeneration Inlet Air Relative Humidity for Temperature Equation {percent}
        45.0,           !- Maximum Regeneration Inlet Air Relative Humidity for Temperature Equation {percent}
        80.0,           !- Minimum Process Inlet Air Relative Humidity for Temperature Equation {percent}
        100.0,          !- Maximum Process Inlet Air Relative Humidity for Temperature Equation {percent}
        -2.25547E+01,   !- Humidity Ratio Equation Coefficient 1
        9.76839E-01,    !- Humidity Ratio Equation Coefficient 2
        4.89176E-01,    !- Humidity Ratio Equation Coefficient 3
        -6.30019E-02,   !- Humidity Ratio Equation Coefficient 4
        1.20773E-02,    !- Humidity Ratio Equation Coefficient 5
        5.17134E-05,    !- Humidity Ratio Equation Coefficient 6
        4.94917E-02,    !- Humidity Ratio Equation Coefficient 7
        -2.59417E-04,   !- Humidity Ratio Equation Coefficient 8
        0.007143,       !- Minimum Regeneration Inlet Air Humidity Ratio for Humidity Ratio Equation
        0.024286,       !- Maximum Regeneration Inlet Air Humidity Ratio for Humidity Ratio Equation
        46.111110,      !- Minimum Regeneration Inlet Air Temperature for Humidity Ratio Equation {C}
        46.111112,      !- Maximum Regeneration Inlet Air Temperature for Humidity Ratio Equation {C}
        0.005000,       !- Minimum Process Inlet Air Humidity Ratio for Humidity Ratio Equation
        0.015714,       !- Maximum Process Inlet Air Humidity Ratio for Humidity Ratio Equation
        4.583333,       !- Minimum Process Inlet Air Temperature for Humidity Ratio Equation {C}
        21.83333,       !- Maximum Process Inlet Air Temperature for Humidity Ratio Equation {C}
        2.286,          !- Minimum Regeneration Air Velocity for Humidity Ratio Equation {m/s}
        4.826,          !- Maximum Regeneration Air Velocity for Humidity Ratio Equation {m/s}
        0.007914,       !- Minimum Regeneration Outlet Air Humidity Ratio for Humidity Ratio Equation
        0.026279,       !- Maximum Regeneration Outlet Air Humidity Ratio for Humidity Ratio Equation
        5.0,         !- Minimum Regeneration Inlet Air Relative Humidity for Humidity Ratio Equation {%}
        45.0,        !- Maximum Regeneration Inlet Air Relative Humidity for Humidity Ratio Equation {percent}
        80.0,          !- Minimum Process Inlet Air Relative Humidity for Humidity Ratio Equation {percent}
        100.0;         !- Maximum Process Inlet Air Relative Humidity for Humidity Ratio Equation {percent}
~~~~~~~~~~~~~~~~~~~~

### Outputs

No variables are reported for this object. However, outputs are provided by the [HeatExchanger:Desiccant:BalancedFlow](#heatexchangerdesiccantbalancedflow) object that references this PerformanceDataType1 object.