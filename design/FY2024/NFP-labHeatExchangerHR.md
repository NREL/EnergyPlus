
Enhancement for Variable-Speed Heat Recovery Ventilation in Laboratories
================

**Yujie Xu, Tianzhen Hong**

**Lawrence Berkeley National Laboratory***

 - Original Date: Oct 24, 2023

## Justification for Feature Update

For lab buildings, the heating-cooling air flow rate could vary substantially
depending on the occupancy, the type of experiment, and the operation protocol.
Correctly capturing its HVAC energy consumption and heat recovery performance
requires accurate information on heat exchangers' sensible and latent
effectiveness at various flow conditions. However, in EnergyPlus, the heat
exchanger object, HeatExchanger:AirToAir:SensibleAndLatent, describes the
sensible and latent effectiveness at only two reference points (100% and 75%
airflow) for either heating or cooling. This feature will add four new optional
fields at the end of the HeatExchanger:AirToAir:SensibleAndLatent object. These
new fields will specify four performance curves to more flexibly express the
relationship between sensible and latent effectiveness at different relative
airflow (the percentage of the actual airflow relative to the nominal supply
airflow). Being able to characterize the efficiency at low-flow-rate conditions
can allow more accurate modeling of the heat recovery system and justify the
adoption of high-potential energy-saving strategies like using variable speed
fans in the exhaust air heat recovery systems in laboratories.

A team led by Jon McHugh, who is doing EnergyPlus modeling to support California
Title 24 development, requested the feature. This feature supports ANSI/ASSP
Z9.5-2022 Laboratory Ventilation standard, which requires variable-airflow
exhaust system operations.

## Overview ##

### Existing approach ###

Currently, the HeatExchanger:AirToAir:SensibleAndLatent in EnergyPlus only has
two reference points to describe the relationship between relative airflow and
heat exchange effectiveness in transferring sensible and latent heat (the fields
between the two arrows in the following example).

Example:

    HeatExchanger:AirToAir:SensibleAndLatent,
        OA Heat Recovery 1,            !- Heat exchanger name
        FanAndCoilAvailSched,          !- Availability schedule name
        0.05,                          !- Nominal supply air flow rate {m3/s}
        .76,                           !- Sensible effectiveness at 100% airflow heating condition     <--
        .68,                           !- Latent effectiveness at 100% airflow heating condition
        .81,                           !- Sensible effectiveness at 75% airflow heating condition
        .73,                           !- Latent effectiveness at 75% airflow heating condition
        .76,                           !- Sensible effectiveness at 100% airflow cooling condition
        .68,                           !- Latent effectiveness at 100% airflow cooling condition
        .81,                           !- Sensible effectiveness at 75% airflow cooling condition
        .73,                           !- Latent effectiveness at 75% airflow cooling condition        <--
        ERV Outdoor air Inlet Node,    !- Supply air inlet node name
        Heat Recovery Outlet Node,     !- Supply air outlet node name
        Zone 1 Exhaust Node,           !- Exhaust air inlet node name
        Heat Recovery Secondary Outlet Node,  !- Exhaust air outlet node name
        50.0,                          !- Nominal electric power {W}
        Yes,                           !- Supply air outlet temperature control
        Rotary,                        !- Heat exchanger type
        MinimumExhaustTemperature,   !- Frost control type
        1.7;                           !- Threshold temperature

### The gap in the modeling capability ###

In current EnergyPlus, a linear relationship is specified for the heat exchanger
effectiveness at different relative airflow, using the performance at 75% and
100% of the supply air flow rate. The linear performance curve is valid for
relative airflow from 50% to 130%. However, some heat exchangers could have
non-linear performance curves as shown in Figure 1.

![Figure 1](9-Comparison-of-effect-of-airflow-rate-on-heat-exchanger-effectiveness-for-cooling-and.png)

Figure 1. An example of a heat exchanger with non-linear performance curves at different airflow conditions [1]

Furthermore, when the relative airflow is below 50% or above 130%, the program
will throw a warning, "Average air volume flow rate is <50% or >130% of the
nominal HX supply air volume flow rate.", indicating that there could be an
issue of linearly extrapolating the curve. This also suggests the need for more
points to better capture the heat exchanger performance at very low or very high
airflow conditions.

## Approach

This feature proposes to add 4 optional fields holding 4 performance curves at
the end of the HeatExchanger:AirToAir:SensibleAndLatent. These fields will
specify the sensible and latent effectiveness of heating or cooling as a
function of relative airflow. When any of the curves is specified, the
effectiveness will be calculated using this curve instead of the effectiveness
specified in N2 through N9 will not be used. The warning message of "Average air
volume flow rate is <50% or >130% of the nominal HX supply air volume flow
rate." will also be silenced if four new curve fields all have inputs and the
specified curves all cover the range of relative airflow below 50% and above
130%.

    HeatExchanger:AirToAir:SensibleAndLatent,
          \memo This object models an air-to-air heat exchanger using effectiveness relationships.
          \memo The heat exchanger can transfer sensible energy, latent energy, or both between the
          \memo supply (primary) and exhaust (secondary) air streams.
          \min-fields 19
      A1,  \field Name
          \required-field
          \type alpha
          \reference HXAirToAirNames
          \reference HXAirToAirSensibleAndLatentNames
          \reference AFNHeatExchangerNames
          \reference-class-name validBranchEquipmentTypes
          \reference validBranchEquipmentNames
          \reference-class-name validOASysEquipmentTypes
          \reference validOASysEquipmentNames
      A2, \field Availability Schedule Name
          \note Availability schedule name for this system. Schedule value > 0 means the system is available.
          \note If this field is blank, the system is always available.
          \type object-list
          \object-list ScheduleNames
      N1, \field Nominal Supply Air Flow Rate
          \required-field
          \type real
          \autosizable
          \minimum> 0.0
          \units m3/s
      N2, \field Sensible Effectiveness at 100% Heating Air Flow
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
          \default 0.0
      N3, \field Latent Effectiveness at 100% Heating Air Flow
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
          \default 0.0
      N4, \field Sensible Effectiveness at 75% Heating Air Flow
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
          \default 0.0
      N5, \field Latent Effectiveness at 75% Heating Air Flow
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
          \default 0.0
      N6, \field Sensible Effectiveness at 100% Cooling Air Flow
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
          \default 0.0
      N7, \field Latent Effectiveness at 100% Cooling Air Flow
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
          \default 0.0
      N8, \field Sensible Effectiveness at 75% Cooling Air Flow
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
          \default 0.0
      N9, \field Latent Effectiveness at 75% Cooling Air Flow
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
          \default 0.0
      A7, \field Supply Air Inlet Node Name
          \required-field
          \type node
      A4, \field Supply Air Outlet Node Name
          \required-field
          \type node
      A5, \field Exhaust Air Inlet Node Name
          \required-field
          \type node
      A6, \field Exhaust Air Outlet Node Name
          \required-field
          \type node
      N10,\field Nominal Electric Power
          \type real
          \units W
          \ip-units W
          \minimum 0.0
          \default 0.0
      A7, \field Supply Air Outlet Temperature Control
          \type choice
          \key No
          \key Yes
          \default No
      A8, \field Heat Exchanger Type
          \type choice
          \key Plate
          \key Rotary
          \default Plate
      A9, \field Frost Control Type
          \type choice
          \key None
          \key ExhaustAirRecirculation
          \key ExhaustOnly
          \key MinimumExhaustTemperature
          \default None
      N11,\field Threshold Temperature
          \type real
          \units C
          \default 1.7
          \note Supply (outdoor) air inlet temp threshold for exhaust air recirculation and
          \note exhaust only frost control types. Exhaust air outlet threshold Temperature for
          \note minimum exhaust temperature frost control type.
      N12,\field Initial Defrost Time Fraction
          \type real
          \units dimensionless
          \minimum 0.0
          \maximum 1.0
          \default 0.083
          \note Fraction of the time when frost control will be invoked at the threshold temperature.
          \note This field only used for exhaust air recirc and exhaust-only frost control types.
      N13,\field Rate of Defrost Time Fraction Increase
          \type real
          \units 1/K
          \minimum 0.0
          \default 0.012
          \note Rate of increase in defrost time fraction as actual temp falls below threshold temperature.
          \note This field only used for exhaust air recirc and exhaust-only frost control types.
      A10,\field Economizer Lockout
          \type choice
          \key Yes
          \key No
          \default Yes
          \note Yes means that the heat exchanger will be locked out (off)
          \note when the economizer is operating or high humidity control is active
      A11, \field Sensible Effectiveness of Heating Air Flow Curve Name
          \note optional
          \note if this field has value, then field N2 and N4 will be ignored
          \type object-list
          \object-list UnivariateFunctions
      A12, \field Latent Effectiveness of Heating Air Flow Curve Name
          \note optional
          \note if this field has value, then field N3 and N5 will be ignored
          \type object-list
          \object-list UnivariateFunctions
      A13, \field Sensible Effectiveness of Cooling Air Flow Curve Name
          \note optional
          \note if this field has value, then field N6 and N8 will be ignored
          \type object-list
          \object-list UnivariateFunctions
      A14; \field Latent Effectiveness of Cooling Air Flow Curve Name
          \note optional
          \note if this field has value, then field N7 and N9 will be ignored
          \type object-list
          \object-list UnivariateFunctions


## IDD Object changes

A field (N4) will be added to the ZoneHVAC:EvaporativeCoolerUnit

    ZoneHVAC:EvaporativeCoolerUnit,
        \memo Zone evaporative cooler. Forced-convection cooling-only unit with supply fan,
        \memo 100% outdoor air supply. Optional relief exhaust node
        \min-fields 15
    A1 , \field Name
        \required-field
        \reference ZoneEquipmentNames
    A2 , \field Availability Schedule Name
        \note Availability schedule name for this system. Schedule value > 0 means the system is available.
        \note If this field is blank, the system is always available.
        \type object-list
        \object-list ScheduleNames
    A3,  \field Availability Manager List Name
        \note Enter the name of an AvailabilityManagerAssignmentList object.
        \type object-list
        \object-list SystemAvailabilityManagerLists
    A4 , \field Outdoor Air Inlet Node Name
        \required-field
        \type node
        \note this is an outdoor air node
    A5 , \field Cooler Outlet Node Name
        \required-field
        \type node
        \note this is a zone inlet node
    A6 , \field Zone Relief Air Node Name
        \type node
        \note this is a zone exhaust node, optional if flow is being balanced elsewhere
    A7 , \field Supply Air Fan Object Type
        \required-field
        \type choice
        \key Fan:SystemModel
        \key Fan:ComponentModel
        \key Fan:ConstantVolume
        \key Fan:OnOff
        \key Fan:VariableVolume
    A8 , \field Supply Air Fan Name
        \required-field
        \type object-list
        \object-list Fans
    N1 , \field Design Supply Air Flow Rate
        \required-field
        \units m3/s
        \minimum> 0
        \autosizable
    A9 , \field Fan Placement
        \required-field
        \type choice
        \key BlowThrough
        \key DrawThrough
    A10, \field Cooler Unit Control Method
        \required-field
        \type choice
        \key ZoneTemperatureDeadbandOnOffCycling
        \key ZoneCoolingLoadOnOffCycling
        \key ZoneCoolingLoadVariableSpeedFan
    N2 , \field Throttling Range Temperature Difference
        \note used for ZoneTemperatureDeadbandOnOffCycling hystersis range for thermostatic control
        \type real
        \units deltaC
        \default 1.0
        \minimum> 0.0
    N3 , \field Cooling Load Control Threshold Heat Transfer Rate
        \type real
        \units W
        \default 100.0
        \note Sign convention is that positive values indicate a cooling load
        \minimum> 0.0
    A11, \field First Evaporative Cooler Object Type
        \required-field
        \type choice
        \key EvaporativeCooler:Direct:CelDekPad
        \key EvaporativeCooler:Direct:ResearchSpecial
        \key EvaporativeCooler:Indirect:CelDekPad
        \key EvaporativeCooler:Indirect:WetCoil
        \key EvaporativeCooler:Indirect:ResearchSpecial
    A12, \field First Evaporative Cooler Object Name
        \required-field
        \type object-list
        \object-list EvapCoolerNames
    A13, \field Second Evaporative Cooler Object Type                           <- added new fields starts here
        \note optional, used for direct/indirect configurations
        \note second cooler must be immediately downstream of first cooler, if present
        \type choice
        \key EvaporativeCooler:Direct:CelDekPad
        \key EvaporativeCooler:Direct:ResearchSpecial
        \key EvaporativeCooler:Indirect:CelDekPad
        \key EvaporativeCooler:Indirect:WetCoil
        \key EvaporativeCooler:Indirect:ResearchSpecial
    A14, \field Second Evaporative Cooler Name
        \note optional, used for direct/indirect configurations
        \type object-list
        \object-list EvapCoolerNames
    A15, \field Design Specification ZoneHVAC Sizing Object Name
        \note Enter the name of a DesignSpecificationZoneHVACSizing object.
        \type object-list
        \object-list DesignSpecificationZoneHVACSizingName
    N4; \field Shut Off Relative Humidity
        \note Zone relative humidity above which the evap cooler is shut off.
        \type real
        \minimum 0.00
        \maximum 100.00
        \units percent                                                          <- added new fields ends here

## Testing/Validation/Data Source(s)

The feature will be tested and demonstrated with a test file derived from 5Zone_Unitary_HXAssistedCoil.idf.

## Acknowledgments

LBNL had a few meetings with Jon McHugh and his team who made the original request of the new feature.

## Proposed additions to Meters:

N/A

## Proposed Report Variables:

N/A
 
## References

[1]	W. J. Turner, "Investigation and Development of Hybrid Ventilation Wall Convector," The University of Reading, 2009. [Online]. Available: https://www.researchgate.net/publication/263209666_Investigation_and_Development_of_Hybrid_Ventilation_Wall_Convector/figures?lo=1
