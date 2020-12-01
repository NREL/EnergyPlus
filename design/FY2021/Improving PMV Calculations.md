Improving PMV Calculations
================

**Xuan Luo, Tianzhen Hong**

**Lawrence Berkeley National Laboratory**

 - Original Date: Nove 28, 2020

## Justification for New Feature ##

ASHRAE Thermal Comfort Standard 55 has been evolving in recent years to encourage more sustainable building designs and operational practices. Current thermal comfort models in EnergyPlus do not reflect latest ASHRAE Standard 55-2017 which provides cooling credit for elevated air velocity. Updating the calculations of existing thermal comfort parameters is crucial to support passive cooling technologies such as natural ventilation, ceiling fan and portable fan. It is noted that such PMV/PPD calculations are already implemented in the CBE Thermal Comfort tool for ASHRAE Standard 55, https://comfort.cbe.berkeley.edu/ and pythermalcomfort, https://pythermalcomfort.readthedocs.io/.

In this new feature, we propose to implement the following adjustments according to ASHRAE Standard 55-2017 in EnergyPlus to facilitate more accurate comfort modeling:

1) Add the cooling effect (CE) calculations, according to ASHRAE Standard 55-2017 NORMATIVE APPENDIX D. 

2) Add a modified set of PMV/PPD calculations that consider the cooling effect of elevated air speed. Keep the original PMV/PPD calculations.

This proposal does not cover the calculations of effective radiant field which measures the comfort impact of solar heat gain on occupants, according to ASHRAE Standard 55-2017 NORMATIVE APPENDIX C. One of the considerations is such calculations require detailed time-varying occupant locations and explicit definition of individual windows (location, size) of a space, which involve lots of inputs from users that are hard to determine or highly uncertain. This type of detailed calculations is better done through Python EMS or co-simulation with EnergyPlus.


## Overview ##

1. Cooling effect (CE) calculations

ASHRAE Standard 55-2017 Section 5.3 requires that the Elevated Air Speed Comfort Zone Method be used when average air speed V<sub>a</sub> is greater than 0.20 m/s.

The Standard Effective Temperature (SET) model shall be used to account for the cooling effect of air speeds greater than the 0.20 m/s. Specifically, for a given set of environmental and personal variables, including an elevated average air speed, an average air temperature t<sub>a</sub>, and a mean radiant temperature t<sub>r</sub>, the SET is first calculated. Then the average air speed V<sub>a</sub> is replaced by still air (0.1 m/s), and the average air temperature and radiant temperature are adjusted according to the cooling effect (CE). The CE of the elevated air speed is the value that, when subtracted equally from both the average air temperature and the mean radiant temperature, yields the same SET under still air as in the first SET calculation under elevated air speed. 

The following is a formal description of this process. To define the CE, we assert that it satisfies the following:

SET(t<sub>a</sub>, t<sub>r</sub>, v<sub>elev</sub>, \*) = SET(t<sub>a</sub> - CE, t<sub>r</sub> - CE, v<sub>still</sub>, \*) &nbsp;&nbsp; Eq. (1)

where t<sub>a</sub> is the average air temperature, t<sub>r</sub> is the mean radiant temperature, v<sub>elev</sub> is the elevated average air speed, such that v<sub>elev</sub> > 0.1 m/s, still is the elevated average air speed (v<sub>still</sub> = 0.1 m/s). The other parameters (indoor air humidity, clothing, metabolic rate) that stay the same are denoted by “\*”. 

The current PierceSET method to calculate SET implemented in EnergyPlus is consistent with the ASHRAE 55 proposed method. We’ll calculate the CE using the PierceSET model.

2. Adjusted ASHRAE 55 PMV/PPD calculations

The current PMV/PPD calculations implemented in EnergyPlus are consistent with the algorithms described in ASHRAE 55 NORMATIVE APPENDIX B for still air speed. The Cooling Effect adjusted PMV/PPD for an environment with elevated average air speed is calculated using the adjusted average air temperature, the adjusted radiant temperature, and still air (0.1 m/s). The following are equations to calculate the adjusted PMV and PPD.

PMV<sub>adj</sub> = PMV(t<sub>a</sub> - CE, t<sub>r</sub> - CE, v<sub>still</sub>, \*) &nbsp;&nbsp; Eq. (2)

PPD<sub>adj</sub> = 100 - 95 \* exp(-0.03353 \* PMV<sub>adj</sub><sup>4</sup> - 0.2179 \* PMV<sub>adj</sub><sup>2</sup>) &nbsp;&nbsp; Eq. (3)

## Approach ##

We will add a new key, `ASHRAE55`, to the current `People` object, `Thermal Comfort Model N Type` field to reflect the adjusted thermal comfort calculations with considerations of the elevated air velocity cooling effect. When choosing the ASHRAE55 as the thermal comfort model, the still-air PMV/PPD and the original Pierce SET will be calculated, along with the adjusted cooling effect adjusted PMV/PPD. Both the original still-air and the adjusted thermal comfort metrics will be reported.

```
People,
   \memo Sets internal gains and contaminant rates for occupants in the zone.
   \memo If you use a ZoneList in the Zone or ZoneList name field then this definition applies
   \memo to all the zones in the ZoneList.
   \min-fields 10
   A14, \field Thermal Comfort Model 1 Type
       \type choice
       \key Fanger
       \key Pierce
       \key KSU
       \key AdaptiveASH55
       \key AdaptiveCEN15251
       \key ASHRAE55
```

The following new variables will be added in the thermal comfort data structures and be set as reportable variables to be included in the output.

- ASHRAE55CoolingEffect
- CoolingEffectAdjustedPMV
- CoolingEffectAdjustedPPD


## Testing/Validation/Data Source(s) ##

The PMV/PPD and SET results will be validated with the validation table in the ASHRAE Standard. Two example files (the DOE reference small office and the one zone uncontrolled model) will be modified to demonstrate the use of the new feature. Simulation results will be manually checked/benchmarked using excel spreadsheet with input and output from EnergyPlus runs.

## Input Output Reference Documentation ##

To be developed.

## Input Description ##

The `Thermal Comfort Model N Type` field in the `People` object will be modified to take another `ASHRAE55` key as one of the choices.

## Outputs Description ##
The following new report variables will be added:

- Zone, Average, Zone Thermal Comfort ASHRAE 55 Elevated Air Speed Cooling Effect [°C]
- Zone, Average, Zone Thermal Comfort ASHRAE 55 Elevated Air Speed Cooling Effect Adjusted PMV []
- Zone, Average, Zone Thermal Comfort ASHRAE 55 Elevated Air Speed Cooling Effect Adjusted PPD []

## Engineering Reference ##

To be developed.

## Example Files and Transition Changes ##

The existing DOE reference small office model will be modified to report the adjusted thermal comfort calculations.

No transition change is required.

## E-mail and  Conference Call Conclusions ##

N/A

## Acknowledgments ##

N/A

