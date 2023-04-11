
Enhancement of Evaporative Cooler in EnergyPlus
================

**Yujie Xu, Tianzhen Hong**

**Lawrence Berkeley National Laboratory***

 - Original Date: Apr 7, 2023
 - Modified Date: Apr 11, 2023

## Justification for Feature Update

As global climate change continues, the frequency, duration and intensity of heatwaves could increase. As an affordable and energy efficient cooling option [1], evaporative cooling could become more prevalent in the future, especially in hot and dry climates. As a result, it is crucial to provide accurate and more user-friendly simulation support for prototyping new evaporative coolers and their applications. With this motivation, this EnergyPlus feature is proposed to provide an additional relative humidity-driven control option.

![zoneEvapCoolerDiagram](zoneEvapCoolerDiagram.png)
<p style="text-align: center;"> Figure 1. Conceptual diagram of a direct evaporative cooler[^1]  (left), an example of a zone-level direct evaporative cooler[^2] (right).</p>

[^1]: Source: https://basc.pnnl.gov/resource-guides/evaporative-cooling-systems#edit-group-description
[^2]: Source: https://www.nytimes.com/wirecutter/blog/do-swamp-coolers-work/

The enhancement was motivated by discussions with the CBE research group at UC Berkeley: Hui Zhang, Roberto Rugani, and Maria Andre.

## Overview ##

We plan to enhance this direct evaporative cooler object, *EvaporativeCooler:Direct:ResearchSpecial*. This object is similar to the *EvaporativeCooler:Direct:CelDekPad* object but with additional input fields allowing for easier control of the air flow (Primary Air Design Flow Rate), cooler efficiency (cooler design effectiveness, Effectiveness Flow Ratio Modifier Curve Name, etc), pump power (Water Pump Power Modifier Curve Name).  It allows control of the evaporative cooler based on the environmental condition (Evaporative Operation Minimum Drybulb Temperature, Evaporative Operation Maximum Limit Wetbulb Temperature).

The introduction of excessive moisture is one of the potential issues of direct evaporative coolers. A humidity control could become useful in preventing the evaporative cooler from raising indoor humidity to an uncomfortable level. Currently, the direct evaporative cooler can be controlled with the sensor node  temperature using AvailabilityManagers (*AvailabilityManager:LowTemperatureTurnOff* or *AvailabilityManager:HighTemperatureTurnOn*). This feature proposes to add a relative humidity (RH) control to shut down the evaporative cooler when the indoor RH is too high.

## Approach

To enable a RH control, two *AvailabilityManager:\* * objects will be added *AvailabilityManager:HighRHTurnOff* and *AvailabilityManager:HighTemperatureLowRHTurnOn*.

## Testing/Validation/Data Source(s)

This feature will be tested and demonstrated with a test file derived from 1ZoneEvapCooler.idf. Manual check of the time-step EnergyPlus simulation results will be conducted to confirm the added feature is working correctly. 

## IDD Object changes

Two *AvailabilityManager:\* * objects, *AvailabilityManager:HighRHTurnOff* and *AvailabilityManager:HighTemperatureLowRHTurnOn* will be added to enable RH control of the direct evaporative cooling.

    AvailabilityManager:HighRHTurnOff,
          \memo Overrides fan/pump schedules depending on relative humidity at sensor node.
      A1 , \field Name
          \required-field
          \type alpha
          \reference SystemAvailabilityManagers
      A2 , \field Sensor Node Name
          \note The air node where the relative humidity is monitored
          \required-field
          \type node
      N1 ; \field Relative Humidity
          \note The relative humidity at or exceeding which the system is turned off
          \required-field
          \type real
          \units dimensionless

    AvailabilityManager:HighTemperatureLowRHTurnOn
          \memo Overrides fan/pump schedules depending on relative humidity and temperature at sensor node.
      A1 , \field Name
          \required-field
          \type alpha
          \reference SystemAvailabilityManagers
      A2 , \field Sensor Node Name
          \note The air node where the temperature and the relative humidity are monitored
          \required-field
          \type node
      N1 , \field Relative Humidity
          \note When the sensor node relative humidity is lower than this RH and the sensor node temperature is higher than the temperature specified in N2, then the system is turned on.
          \required-field
          \type real
          \units dimensionless
      N2 ; \field Temperature
          \note When the sensor node temperature is higher than this temperature and the sensor node RH is lower than the RH specified in N1, then the system is turned on.
          \required-field
          \type real
          \units C

These two corresponds two the following control methods
* When the evaporative cooling system is always on, it will be shut down when temperature is too low or humidity is too high, using the following two availability managers 

    AvailabilityManager:LowTemperatureTurnOff
    AvailabilityManager:HighRHTurnOff (to be added in this feature)
 
* When the evaporative cooling system is always off, it will be turned on when temperature is too high and humidity is low enough. This will be realized using the following availability manager

    AvailabilityManager:HighTemperatureLowRHTurnOn (to be added in this feature)

## Proposed additions to Meters:

N/A

## Proposed Report Variables:

N/A
 
## References

[1] https://www.energy.gov/energysaver/evaporative-coolers <br>
