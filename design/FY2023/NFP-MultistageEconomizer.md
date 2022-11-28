Multi-stage Economizer
================

**Jeremy Lerond, Pacific Northwest National Laboratory**

 - Original Date: Nov 28, 2022
 - Revision Date: Nov 28, 2022

## Justification for New Feature ##

Currently, unitary system economizer operation is dependent on the operating cooling speed being simulated. For example, if a unitary system with a two-speed fan is modeled along with an economizer, the economizer will be used at the low-speed fan whenever the system is operating at the low cooling operating speed, and at the high speed whenever the system is operating at the high operating speed, see Figure 0.

![Current](NFP-MultistageEconomizer_Figure0_Current.png)

*Figure 0 - Simulation results excerpt for an `AirLoopHVAC:UnitarySystem` with a 2-speed `Fan:SystemModel` and a `Coil:Cooling:DX:MultiSpeed`*

This is not how actual single-zone systems and their controller operate. Figures 1 and 2 show actual sequence of operation of two widely used controllers in such systems. Both figures show that the economizer, if conditions allow its operation, runs first at low and high speed before mechanical cooling is turned on.

![Honeywell JADE](NFP-MultistageEconomizer_Figure1_Honeywell_JADE.png)

*Figure 1 - Honeywell JADE controller*

![Belino ZIP Economizer](NFP-MultistageEconomizer_Figure1_Belimo_ZIPEconomizer.png)

*Figure 2 - Belino ZIP Economizer controller*

Moreover, this control approach is a building energy code requirement. For instance, ASHRAE Standard 90.1-2019 states in Section 6.5.3.2.1 that "*units that include an air economizer to meet the requirements of Section 6.5.1 shall have a minimum of two speeds of fan control during economizer operation*".

The proposed feature will enable EnergyPlus to simulate more closely the performance of multi-speed single-zone equipment and enable building energy code requirements to be modeled.

## E-mail and  Conference Call Conclusions ##

## Overview ##

This proposal is about adding to EnergyPlus the capability to simulation multi-stage economizer for single-zone systems modeled through an `AirLoopHVAC:UnitarySystem` object. Here, a multi-stage economizer is an economizer that is simulated independently of the operating cooling fan speed for a `AirLoopHVAC:UnitarySystem` object.

## Approach ##

A new input will be added to the `Controller:OutdoorAir` object to turn on/off the multi-stage economizer proposed feature. The default behavior will be set to the current approach.

The required "cooling" speed (among the cooling speeds specified in the `UnitarySystemPerformance:Multispeed` object attached to the `AirLoopHVAC:UnitarySystem` object) to meet the mixed air temperature setpoint will be determined for economizer operation. Mechanical cooling will only be simulated if the cooling speed for economizer operation has reached the highest cooling speed air flow rate and the outdoor air fraction is 1.0.

## Testing/Validation/Data Sources ##

Appropriate unit tests will be added to test the new feature and make sure that the current implementation still works as it does now.

## Input Output Reference Documentation ##

Documentation for the new input will be added to the I/O reference guide.

## Input Description ##

The following new input will be added to the `Controller:OutdoorAir` object:

```
Controller:OutdoorAir,
       \memo Controller to set the outdoor air flow rate for an air loop. Control options include
       \memo fixed, proportional, scheduled, economizer, and demand-controlled ventilation.

[...]

  A20; \field Economizer Staging
       \type choice
       \key EconomizerFirst
       \key InterlockedWithMechanicalCooling
       \default InterlockedWithMechanicalCooling
       \note When modeled with an AirLoopHVAC:UnitarySystem with multiple cooling speeds
       \note (specified in a UnitarySystemPerformance:Multispeed), EconomizerFirst runs
       \note the economizer at all speeds, all the way to the highest cooling speed before
       \note mechanical cooling is needed. InterlockedWithMechanicalCooling runs the
       \note economizer at  the cooling speed chosen by the AirLoopHVAC:UnitarySystem.
       \note Use EconomizerFirst to model typical economizer staging for in multi-speed
       \note packaged single-zone equipment.
```

## Outputs Description ##

No new output is currently planned for this new feature.

## Engineering Reference ##

No updates to the engineering reference is currently planned for this new feature.

## Example File and Transition Changes ##

No transition files will be needed since the default will be wired to the current approach.

A new example file based on `UnitarySystem_MultiSpeedDX.idf` will be created to showcase the new feature.

## References ##

- GitHub issue, [2 speed operation with Air Economizer in UnitarySystem](https://github.com/NREL/EnergyPlus/issues/6109)
- ASHRAE. 2019. ANSI/ASHRAE/IES 90.1-2019, Energy Standard for Buildings Except Low-Rise
Residential Buildings. ASHRAE, Atlanta, GA
- [Honeywell JADE controller](https://customer.honeywell.com/resources/techlit/TechLitDocuments/62-0000s/62-0331.pdf)
- [Belino ZIP Economizer controller](http://www.kele.com/Catalog/22%20Thermostats_Controllers/PDFs/ZIP_Economizer_%20Complete%20Installation%20and%20Operation%20Manual.pdf)
