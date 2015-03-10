# Ideal Loads Air System

## Overview

The input object ZoneHVAC:IdealLoadsAirSystem provides a model for an ideal HVAC system. It occupies a place in the program hierarchy corresponding to a zone HVAC unit. It is not connected to a central air system – instead each ZoneHVAC:IdealLoadsAirSystem object supplies cooling or heating air to a zone in sufficient quantity to meet the zone load or up to its limits, if specified. The supply air conditions are controlled based on specifications in the ZoneHVAC:IdealLoadsAirSystem input. The system has options for humidity control, outdoor air, economizer, demand controlled ventilation, and heat recovery.

## Model

The ZoneHVAC:IdealLoadsAirSystem object is modeled as an ideal VAV terminal unit with variable supply temperature and humidity. The supply air flow rate is varied between zero and the maximum in order to satisfy the zone heating or cooling load, zone humidity controls, outdoor air requirements, and other constraints, if specified.

### Inputs and Data

The user specifies some or all of the following data for each ZoneHVAC:IdealLoadsAirSystem object:

name of unit availability schedule

name of the zone inlet node;

name of the zone exhaust node;

maximum supply air temperature when in heating mode *T~max,heating~* [C];

minimum supply air temperature when in cooling mode *T~min,cooling~* [C];

maximum supply air humidity ratio when in heating mode *W~max,humid~* [kg water/kg dry air];

minimum supply air humidity ratio when in cooling mode *W~min,dehum~* [kg water/kg dry air];

heating limit type flag (*LimitFlowRate, LimitCapacity, LimitFlowRateAndCapacity* or *NoLimit*) *HeatingLimit*;

maximum heating air flow rate  [m^3^/s]

maximum sensible heating capacity  [W]

cooling limit type flag (*LimitFlowRate, LimitCapacity, LimitFlowRateAndCapacity* or *NoLimit*) *CoolingLimit*

maximum cooling air flow rate  [m^3^/s]

maximum total cooling capacity  [W]

name of heating availability schedule

name of cooling availability schedule

*dehumidification control type flag (ConstantSensibleHeatRatio, Humidistat, None,* or *ConstantSupplyHumidityRatio*) *DehumidCtrlType*

cooling sensible heat ratio

humidification control type flag (*Humidistat, None,* or *ConstantSupplyHumidityRatio*) *HumidCtrlType*

name of a DesignSpecification:OutdoorAir object

outdoor air inlet node name

demand controlled ventilation control type flag (*None, OccupancySchedule or CO2Setpoint*)

outdoor air economizer type flag (*NoEconomizer, DifferentialDryBulb, or DifferentialEnthalpy*)

heat recovery type flag (*None, Sensible, or Enthalpy*)

sensible heat recovery effectiveness

latent heat recovery effectiveness

### All input data for the ZoneHVAC:IdealLoadsAirSystem is stored in the array PurchAir. The model and data are encapsulated in the module PurchasedAirManager.Calculation

Set the unit on/off flag *UnitOn*.

The unit is off (*UnitOn* = *False*) if the unit availability schedule value is <=0; otherwise the unit is on (*UnitOn* = *True*). If the unit is on, the calculation proceeds through the remaining steps. If the unit is off, the zone inlet node conditions are set to the zone node condition, the inlet node mass flow rate is set to zero, and the unit outputs are set to zero.

Calculate the minimum outdoor air mass flow rate based on the specifications in the DesignSpecification:OutdoorAir object, if specified.

Calculate the sensible and latent impact of the outdoor air flow relative to the zone conditions

Determine if the unit needs to heat or cool

If outdoor air sensible impact is >= load to zone cooling setpoint and the current thermostat type is not SingleHeatingSetPoint, then unit is in cooling mode

If outdoor air sensible impact is < load to zone heating setpoint then unit is in heating mode

Else if neither condition is true, then unit is in deadband mode (provides outdoor air but shuts off economizer and heat recovery and all humidity control options except *Humidistat* option)

If in cooling mode, simulate outdoor air economizer and adjust outdoor air mass flow rate

Calculate supply air mass flow rate

If outdoor air flow rate exceeds applicable maximum flow rate (heating or cooling) then reduce outdoor air mass flow rate, issue warning, and set supply air mass flow rate equal to outdoor air mass flow rate

Else

Calculate supply air mass flow rate required to meet zone sensible load at the applicable (heating or cooling) supply temperature limit (*T~max,heating~*or *T~min,cooling~*)

![](media/image5473.png)\


If *DehumidCtrlType*= Humidistat (and other conditions are met, see below), then calculate the supply air mass flow rate required to meet the humidistat dehumidification setpoint at *W~min,dehum~*

If *HumidCtrlType*= Humidistat (and other conditions are met, see below), then calculate the supply air mass flow rate required to meet the humidistat humidification setpoint at *W~max,humid~*

Set the supply air mass flow rate to the greatest of these, but limit to the applicable (heating or cooling) maximum flow rate

Calculate the mixed air conditions, modeling heat recovery, if applicable

The recirculation air conditions are set equal to the zone return air node conditions; if there is no return air node the recirculation air conditions are set equal to the conditions at the zone node.

The unit entering conditions are then:

If ![](media/image5474.png) > ![](media/image5475.png)  then

![](media/image5476.png)\


![](media/image5477.png)\


![](media/image5478.png)\


Otherwise the entering air conditions are set equal to the outside air conditions.

Calculate the supply air temperature required to meet the zone sensible load at the supply air mass flow rate, but limit to the applicable (heating or cooling) supply temperature limit (*T~max,heating~*or *T~min,cooling~*)

![](media/image5479.png)\


Calculate the supply humidity ratio based on the specified humidity control types, but limit to the applicable (heating or cooling) supply humidity ratio limit

*DehumidCtrlType = None* sets the supply air humidity ratio equal to the mixed air humidity ratio.

*DehumidCtrlType = Humidistat,* this will actively dehumidify to the humidistat dehumidification setpoint during cooling and deadband operation, and during heating operation if *HumidCtrlType = Humidistat*

*DehumidCtrlType = ConstantSensibleHeatRatio* sets the supply air humidity ratio using the cooling sensible heat ratio.

*DehumidCtrlType = ConstantSupplyHumidityRatio* sets the supply air humidity ratio = *W~min,dehum~*.

*HumidCtrlType = None* sets the supply air humidity ratio equal to the mixed air humidity ratio.

*HumidCtrlType = Humidistat,* this will actively humidify to the humidistat humidifying setpoint during heating and deadband operation, and during cooling operation if *DehumidCtrlType = Humidistat*

*HumidCtrlType = ConstantSupplyHumidityRatio* sets the supply air humidity ratio = *W~max,humid~*.

Limit supply humidity ratio to saturation at the supply temperature

Check the applicable capacity limits (sensible heating and total cooling) and adjust supply air temperature and humidity if needed.

Set the zone inlet node conditions to the supply air mass flow rate, temperature, and humidity ratio.

Calculate the unit output and load components.

## References

No specific references.