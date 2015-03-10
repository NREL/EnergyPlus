# Heat Exchangers

## Air System Air-To-Air Sensible and Latent Effectiveness Heat Exchanger

### Overview

The input object HeatExchanger:AirToAir:SensibleAndLatent provides a model for a generic, sensible and latent air-to-air heat exchanger that is an HVAC air system component that consists of a heat exchanger and primary/secondary airflow bypass dampers. The specific configuration of the component is shown in the following figure.

![Schematic of the Sensible and Latent Air-to-Air Heat Exchanger](media/schematic-of-the-sensible-and-latent-air-to.jpeg)


The sensible and latent air-to-air heat exchanger is typically used for exhaust or relief air heat recovery. Heat exchanger performance can be specified to transfer sensible energy, latent energy or both between the supply and exhaust air streams. The input requires no geometric data. Performance is defined by specifying sensible and/or latent effectiveness at 75% and 100% of the nominal (rated) supply air flow rate in both heating and cooling conditions (Table 74).

Table: Operating Conditions for Defining Heat Exchanger Performance

Parameter|Conditions
---------|----------
Heating|Cooling
-------|-------
Entering supply air temperature:|     Dry-bulb|     Wet-bulb||1.7°C (35°F)|0.6°C (33°F)||35°C (95°F)|26°C (78°F)
Entering exhaust air temperature:|     Dry-bulb|     Wet-bulb||21°C (70°F)|14°C (58°F)||24°C (75°F)|17°C (63°F)

Note: Conditions consistent with the Air-Conditioning and Refrigeration Institute's Standard 1060 (ARI 2001).

Heat exchange between the supply and exhaust air streams occurs whenever the unit is scheduled to be available (availability schedule) and supply/exhaust air flows are present. This heat exchanger object can be used in conjunction with a conventional air-side economizer (i.e., specify an appropriate economizer control type in the Controller:OutdoorAir object), whereby heat exchange is suspended whenever the air-side economizer is active (i.e., air flow is fully bypassed around a fixed-plate heat exchanger or the rotation of a rotary heat exchanger is stopped). This object is also able to suspend heat exchange for the purpose of providing free cooling operation in the absence of a conventional air-side economizer (i.e., specify MinimumFlowWithBypass in the Controller:Outside Air object). Suspension of heat exchange during economizer mode may be customized as necessary using the economizer lockout field. Note that the Economizer Type must be set to something other than NoEconomizer for this control interaction to function.

Several methods of frost control are available to warm the heat exchanger core to prevent frost formation. Available methods are preheat, exhaust only, exhaust air recirculation, and minimum exhaust temperature. Preheat frost control uses a separate heater object placed in the supply inlet air stream to keep the air temperature above the frost threshold temperature. All other frost control methods are modeled within this heat exchanger object.

This heat exchanger object can also control the supply air outlet temperature to a setpoint when a setpoint manager and temperature schedule are used. This temperature control is accomplished through wheel speed modulation or bypassing supply air around the heat exchanger to maintain the desired setpoint and avoid overheating the supply air.

### Model Description

The heat exchanger object models energy transfer between the supply air stream and the exhaust air stream according to the effectiveness values that are specified by the user in the input data file (see IO Reference Document, HeatExchanger:AirToAir:SensibleAndLatent). The operating volumetric air flow rate through the heat exchanger (i.e., the average of the actual supply and exhaust air flow rates for the simulation time step) should be between 50% and 130% of the nominal supply air flow rate specified for the heat exchanger. Operating air flow rates outside this range result in a warning message and a recommendation to adjust air flow rates to within the appropriate range.

The user must enter the sensible and latent effectiveness of the heat exchanger for heating and cooling conditions (Table 74) with balanced air flow (supply flow equal to the exhaust flow) at two flow rates: 75% and 100% of the nominal supply air flow rate. Heat exchanger manufacturers can typically provide this performance information, and it is also available for equipment contained in ARI's Certified Product Directory for Air-to-Air Energy Recovery Ventilation Equipment (ARI 2003). Values may be entered for sensible effectiveness, latent effectiveness, or both. The model assumes default values of 0.0 for sensible and latent effectiveness, thus requiring the user to input representative values for the heat exchanger being modeled.

To obtain the "operating" effectiveness of the heat exchanger at different air flow rates, the model first calculates the average volumetric air flow rate through the heat exchanger (average of the supply and exhaust air flow rates) for each simulation time step. Air flows through the heat exchanger may be unbalanced (supply greater than exhaust, or vice versa), but an unbalanced air flow ratio greater than 2:1 is not recommended  (beyond this range a warning message is issued). The model determines the operating effectiveness of the heat exchanger by linear interpolation or extrapolation of the 100% flow and 75% flow effectiveness values specified in the input data file, using the average volumetric air flow rate through the heat exchanger. Extrapolation is allowed down to 50% and up to 130% of the nominal supply air flow rate (beyond this range a warning message is issued).

![](media/image5111.png)\


![](media/image5112.png)\


*where*:

![](media/image5113.png)  = operating sensible effectiveness of the heat exchanger

![](media/image5114.png)     = operating latent effectiveness of the heat exchanger

![](media/image5115.png)   =  sensible effectiveness at 75% airflow condition

![](media/image5116.png)  =  sensible effectiveness at 100% airflow condition

![](media/image5117.png)   =  latent effectiveness at 75% airflow condition

![](media/image5118.png)  =  latent effectiveness at 100% airflow condition

![](media/image5119.png)  = the ratio of the average operating volumetric air flow rate [(supply flow plus exhaust flow) / 2.0] to the nominal supply air flow rate

If the heat exchanger's supply air inlet temperature is less than the exhaust air inlet temperature, the operating sensible and latent effectivenesses are calculated using the 75% and 100% heating condition values; otherwise, the 75% and 100% cooling effectiveness values are used in Equations  and .

The supply air conditions leaving the heat exchanger are determined using the heat exchanger operating effectiveness calculated above, the ratio of the air stream with the minimum heat capacity rate to the supply air stream heat capacity rate, and the difference in temperature or humidity ratio between the supply and exhaust inlet air:

![](media/image5120.png)\


![](media/image5121.png)\


![](media/image5122.png)\


*where*:

![](media/image5123.png) = minimum heat capacity rate (W/K)

![](media/image5124.png) = heat capacity rate of the supply air stream (W/K)

![](media/image5125.png) = heat capacity rate of the exhaust air stream (W/K)

*T~SupAirOut~*= supply air temperature leaving the heat exchanger (°C)

*T~SupAirIn~*= supply air inlet temperature (°C)

*T~ExhAirIn~*= exhaust air inlet temperature (°C)

![](media/image5126.png) = supply air humidity ratio leaving the heat exchanger (kg/kg)

![](media/image5127.png) = supply air inlet humidity ratio (kg/kg)

![](media/image5128.png) = exhaust air inlet humidity ratio (kg/kg)

Using the supply air outlet temperature and humidity ratio, the enthalpy of the supply air leaving the heat exchanger is calculated.

![](media/image5129.png)\


where:

*h~SupAirOut~*= enthalpy of the supply air leaving the heat exchanger (J/kg)

*PsyHFnTdbW*= psychrometric routine calculating air enthalpy as a function of

    temperature and humidity ratio

*If the predicted conditions of the supply air leaving the heat exchanger exceed the saturation curve (>100% RH), then the temperature and humidity ratio of the air are reset to saturated conditions (= 100% RH) at the enthalpy condition calculated above (h~SupAirOut~).*

Next, the sensible and total heat recovery rates of the heat exchanger are calculated:

![](media/image5130.png)\


![](media/image5131.png)\


where:

![](media/image5132.png) = sensible heat recovery rate (W)

![](media/image5133.png) = total heat recovery rate (W)

*h~SupAirIn~= supply air inlet enthalpy (J/kg)*

![](media/image5134.png) = mass flow rate of the supply air stream (kg/s)

The conditions of the exhaust (secondary) air leaving the heat exchanger are then calculated:

![](media/image5135.png)\


![](media/image5136.png)\


![](media/image5137.png)\


*where*:

*T~ExhAirOut~*= exhaust air temperature leaving the heat exchanger (°C)

*h~ExhAirOut~*= exhaust air enthalpy leaving the heat exchanger (J/kg)

![](media/image5138.png) = mass flow rate of the exhaust air stream (kg/s)

![](media/image5139.png) = exhaust air humidity ratio leaving the heat exchanger (kg/kg)

*PsyWFnTdbH*= psychrometric routine calculating air humidity ratio as a function of

    temperature and enthalpy

As was done for the supply air, calculated exhaust air conditions beyond the saturation curve are reset to saturation conditions at the calculated air enthalpy value.

Once the air conditions leaving each side of the heat exchanger (supply and exhaust) are calculated, this air is blended with any bypass air that was directed around the heat exchanger core to determine the final air conditions leaving the heat exchanger unit. These outlet air conditions are used in Equations  and  to determine the sensible and total heat recovery rate for the overall heat exchanger unit. The latent heat recovery rate for the overall unit is then calculated as the difference between the total and sensible heat recovery rates:

![](media/image5140.png)\


Heat recovery electric power is the electric consumption rate of the unit in watts. The nominal electric power rate for the heat exchanger is specified in the input data file, and can be used to model controls (transformers, relays, etc.) and/or a motor for a rotary heat exchanger. The model assumes that this electric power is consumed whenever the heat exchanger is scheduled to operate and supply/exhaust air flow rates exist. The electric power is assumed to be zero for all other times or if heat exchange is suspended to provide free cooling (economizer operation). None of this electric power is assumed to contribute thermal load to either of the heat exchanger air streams.

At the end of each HVAC simulation time step, this object reports the sensible, latent and total heat recovery rates for the overall unit as calculated above. The heat recovery rates are reported separately for times when the supply air is heated and when it is cooled (Ref: HeatExchanger:AirToAir:SensibleAndLatent in the EnergyPlus Input Output Reference). The heat recovery electric power is also reported for each simulation time step. In addition to the heat recovery rates and electric power, heating/cooling energy transferred to the supply air and the electric energy consumption by the heat exchanger unit are calculated for the time step being reported as follows:

![](media/image5141.png)\


![](media/image5142.png)\


![](media/image5143.png)\


![](media/image5144.png)\


![](media/image5145.png)\


![](media/image5146.png)\


![](media/image5147.png)\


where:

![](media/image5148.png) = output variable ‘Heat Exchanger Sensible Cooling Energy, J'

![](media/image5149.png) = output variable ‘Heat Exchanger Sensible Cooling Rate, W' = ![](media/image5150.png)

                                   during times when the supply air is cooled

*TimeStepSys* = HVAC system simulation time step, hr

![](media/image5151.png) = output variable ‘Heat Exchanger Latent Cooling Energy, J'

![](media/image5152.png) = output variable ‘Heat Exchanger Latent Cooling Rate, W' = ![](media/image5153.png)

   during times when the supply air is dehumidified

![](media/image5154.png) = output variable ‘Heat Exchanger Total Cooling Energy, J'

![](media/image5155.png) = output variable ‘Heat Exchanger Total Cooling Rate, W' = ![](media/image5156.png) during

    times when the supply air enthalpy is reduced

![](media/image5157.png) = output variable ‘Heat Exchanger Sensible Heating Energy, J'

![](media/image5158.png) = output variable ‘Heat Exchanger Sensible Heating Rate, W' = ![](media/image5159.png)

   during times when the supply air is heated

![](media/image5160.png) = output variable ‘Heat Exchanger Latent Heating Energy, J'

![](media/image5160.png) = output variable ‘Heat Exchanger Latent Gain Energy, J'

![](media/image5161.png) = output variable ‘Heat Exchanger Latent Gain Rate, W' = ![](media/image5162.png)  during times when the supply air is humidified

![](media/image5163.png) = output variable ‘Heat Exchanger Total Heating Energy, J'

![](media/image5164.png) = output variable ‘Heat Exchanger Total Heating Rate, W' = ![](media/image5165.png) during times when the supply air enthalpy is increased

![](media/image5166.png) = output variable ‘Heat Exchanger Electric Energy, J'

![](media/image5167.png) = output variable ‘Heat Exchanger Electric Power, W'

### Frost Control Methods

In cold weather, frost can form on the heat exchanger causing a reduction in air flow and heat recovery performance. Various strategies can be employed to limit frost formation. Heat exchangers that transfer total energy (sensible plus latent) usually have a lower frost threshold temperature than sensible-only heat exchangers. Frost threshold temperatures for sensible-only heat exchangers may be -1°C to -12°C for plate and rotary heat exchangers respectively, while those for comparable total (sensible plus latent) heat exchangers may be 10°C lower. The frost threshold temperature for a specific application is dependent on the exhaust air dry-bulb temperature and relative humidity, heat exchanger type (e.g., sensible-only or total heat exchange, flat plate or rotary), and the heat exchanger effectiveness. Consult manufacturer's literature to obtain specific frost threshold temperatures for the heat exchanger being modeled.

Four frost control strategies can be modeled for this air-to-air heat exchanger unit. Each of these four strategies is discussed in detail below.

### Preheat

One method to control frost formation is to preheat the cold outdoor (supply) air entering the heat exchanger. When a preheat coil is used for frost control, a separate heating coil object must be placed in the supply air stream at the inlet to the heat exchanger (Coil:Heating:Water, Coil:Heating:Electric or Coil:Heating:Gas). The preheat coil should be controlled to maintain a minimum supply air inlet temperature thereby eliminating frost buildup on the heat exchanger core. When modeling preheat frost control, specify "None" as the frost control method in the heat exchanger object. When modeling this heat exchanger as part of an air loop, refer to the objects AirLoopHVAC:OutdoorAirSystem and SetpointManager:Scheduled the EnergyPlus Input Output Reference for additional information on specifying a preheat coil and controlling its supply air temperature.This frost control method is not currently available when this heat exchanger is being used as part of the compound object ZoneHVAC:EnergyRecoveryVentilator.

### Exhaust Only

This method of frost control bypasses the incoming supply air around the heat exchanger core thereby warming the core using the exiting exhaust air. This method is similar to ‘supply air off' frost control where the supply air fan is turned off for a predetermined period of time while the exhaust air fan continues to operate. For the ‘supply air off' method, the supply air flow is stopped for a period of time thereby reducing the ventilation air supplied to the zone(s). In addition, the building may be negatively pressurized for the period of time that the supply air flow is stopped since the exhaust air fan continues to operate. On the other hand, the ‘exhaust only' method of frost control modeled by EnergyPlus continues to provide outdoor ventilation air to the zone(s), but this air is simply bypassed around the heat exchanger core for a portion of the time and the potential problem with negatively pressurizing the building is avoided. Since the supply airflow rate through the heat exchanger core is purposely reduced to control frost formation, average volumetric airflow rates below 50% of nominal are allowed when this frost control is active and no warning message is issued.

The user enters a threshold temperature, an initial defrost time fraction, and a rate of defrost time fraction increase. When the temperature of the supply air (e.g., outdoor air) entering the heat exchanger is equal to or below the specified threshold temperature, the fractional amount of time that the supply air is bypassed around the heat exchanger core is determined from the following equation:

![](media/image5168.png)\


where:

![](media/image5169.png)  = Fractional time period for frost control ![](media/image5170.png)

![](media/image5171.png) = Initial defrost time fraction

![](media/image5172.png) = Rate of defrost time fraction increase (K^-1^)

![](media/image5173.png) = Threshold temperature (°C)

![](media/image5174.png) = Supply air inlet temperature (°C)

During the defrost time, supply air flow is fully bypassed around the heat exchanger core and no heat transfer takes place. For the remainder of the time period, no air is bypassed and full heat exchange is achieved. The average supply air flow bypassed around the heat exchanger core is calculated as follows:

![](media/image5175.png)\


To determine the average heat transfer rates for the simulation time step, the supply air outlet conditions are first calculated as if the heat exchanger were not in defrost mode (see previous section, Model Description). The sensible and total heat transfer rates are then calculated and multiplied by the fractional time period that the heat exchanger is not in defrost mode (1-*X~DefrostTime~*).

![](media/image5176.png)\


![](media/image5177.png)\


Once the average heat transfer rates are determined, the average conditions of the supply air exiting the overall heat exchanger unit are calculated as follows:

![](media/image5178.png)\


![](media/image5179.png)\


![](media/image5180.png)\


As described previously, if the predicted conditions of the exiting supply air exceed the saturation curve (>100% RH), then the temperature and humidity ratio of the air are reset to saturated conditions (= 100% RH) at the enthalpy condition calculated above (*h~SupAirOut~).* If the supply air temperature is reset, the average sensible heat transfer rate is recalculated before the exhaust air outlet conditions are determined:

![](media/image5181.png)\


![](media/image5182.png)\


![](media/image5183.png)\


### Exhaust Air Recirculation

This method of frost control routes exhaust (outlet) air back through the supply side of the heat exchanger to warm the core. Since this method routes exhaust air back into the building, the building is typically not depressurized when this frost control is active. However, the incoming supply (outdoor ventilation) air flow is stopped for the fractional period of time that frost control is active. If significant periods of time exist when outdoor temperatures are below the selected threshold temperature and outdoor ventilation air is continuously required, an alternative method of frost control should be considered.

The user enters a threshold temperature, an initial defrost time fraction, and a rate of defrost time fraction increase. When the temperature of the inlet supply air (e.g., outdoor air) is equal to or below the specified threshold temperature, the fractional amount of time that this heat exchanger frost control strategy is active is determined from the following equation:

![](media/image5184.png)\


The air mass flow rate of the supply air leaving the heat exchanger unit is then calculated using the defrost time fraction calculated above the mass flow rates of supply and exhaust air entering the unit.

![](media/image5185.png)\


The model assumes that no heat exchange occurs during defrost, and the average supply supply air conditions are simply a blend of the conditions when the unit is not in defrost and the exhaust air inlet conditions during defrost operation:

![](media/image5186.png)\


![](media/image5187.png)\


![](media/image5188.png)\


The operating effectivenesses of the heat exchanger are initially calculated according to Equations  and  assuming no defrost operation. Since the supply air flow across the heat exchanger core is not reduced during defrost operation, the sensible and latent effectiveness are therefore derated (for reporting purposes) in direct proportion to the fraction of time that frost control is not active.

![](media/image5189.png)\


![](media/image5190.png)\


Since the exhaust outlet air is recirculated through the supply side of the heat exchanger core, the incoming supply air and exiting exhaust air flows are stopped for the fraction of the time when frost control is active. The average air mass flow rate at the supply air inlet and the exhaust air outlet nodes are therefore reduced accordingly.

![](media/image5191.png)\


![](media/image5192.png)\


The conditions of the exiting (outlet) exhaust air (temperature, humidity ratio and enthalpy) are reported as the values when frost control is not active (i.e., the conditions when exhaust air is actually leaving the unit).

### Minimum Exhaust Temperature

With this frost control method, frost formation is avoided by continuously maintaining the temperature of the exhaust air leaving the heat exchanger core above a specified setpoint. The minimum exhaust air temperature is maintained by modulating heat exchanger rotational speed or by bypassing supply air around a plate heat exchanger. For this frost control method, the user must only enter the threshold (minimum) temperature.

For the case of modulating heat exchanger rotation, the operating effectivenesses and outlet air conditions are first calculated as if the heat exchanger is not in defrost mode (see Model Description). If the resulting temperature of the exhaust air leaving the heat exchanger core is below the specified threshold temperature, then the operating effectivenesses are reduced as follows:

![](media/image5193.png)\


![](media/image5194.png)\


![](media/image5195.png)\


The supply air and exhaust air outlet conditions are then recalculated using these reduced effectiveness values. Finally the sensible, latent and total heat recovery rates are calculated along with the unit's electric power and electric consumption.

The calculation procedure is slightly different for the case of a plate heat exchanger where the supply air is bypassed around the heat exchanger core. Since the volumetric air flow rate through the heat exchanger core is reduced when frost control is active, an iterative process is used to determine the operating effectiveness of the heat exchanger. The operating effectivenesses and outlet air conditions are first calculated as if the heat exchanger is not in defrost mode (see Model Description). If the resulting temperature of the exhaust air leaving the heat exchanger core is below the specified threshold temperature, then the fractional defrost time is calculated as follows:

![](media/image5196.png)\


The iteration process then begins to determine the heat exchanger effectiveness and the exhaust air outlet temperature as if frost control were active. The operating mass flow rate through the supply side of the heat exchanger core is calculated.

*Beginning of iteration process:*

![](media/image5197.png)\


![](media/image5198.png)\


The ratio of average volumetric flow rate through the heat exchanger core to heat exchanger's nominal volumetric flow rate (*HX~flowratio~*) is then determined and used to calculate the operating effectiveness of the heat exchanger using Equations  and . Since the supply airflow rate through the heat exchanger core is purposely reduced to control frost formation, average volumetric airflow rates below 50% of nominal are allowed and no warning message is issued. Supply air outlet temperature (leaving the heat exchanger core), sensible heat transfer, and exhaust air outlet temperature are then calculated using the revised heat exchanger effectiveness.

![](media/image5199.png)\


![](media/image5200.png)\


![](media/image5201.png)\


The error between the exhaust outlet temperature and the threshold temperature for frost control and a new defrost time fraction are subsequently calculated.

![](media/image5202.png)\


![](media/image5203.png)\


*End of iteration process:*

The iteration process ends when the calculated error is within an error tolerance of 0.001.  The air streams passing through the heat exchanger core and bypassing the core through the bypass damper are then blended together to provide the air conditions leaving the heat exchanger unit. Finally the sensible, latent and total heat recovery rates are calculated along with the unit's electric power and electric consumption.

### Economizer Operation

A conventional air-side economizer may be used in conjunction with this heat exchanger object. The air-side economizer is specified through the use of an outside air controller (see object: Controller:OutdoorAir). Specify the appropriate economizer control type, and provide the required control points and air flow rates as defined in the outside air controller object. Energy transfer provided by the heat exchanger will be suspended whenever free cooling is available (i.e., when the air-side economizer is activated) and *the user specified economizer lockout input is specified as Yes*. For plate heat exchangers, heat transfer is suspended by fully bypassing the supply and exhaust air around the heat exchanger core. For rotary heat exchangers, air flows continue through the core but it is assumed that heat exchanger rotation is stopped.

Heat exchange can also be suspended for the purposes of providing free cooling operation in the absence of a conventional air-side economizer. In this case specify "MinimumFlowWithBypass" as the economizer choice and again provide the required control points as defined in the outside air controller object. Energy transfer provided by the heat exchanger will be suspended whenever free cooling is available and *the user specified economizer lockout input is specified as Yes*, however the supply air flow rate will remain at the minimum value specified in the outside air controller object. Note that the Economizer Type must be set to something other than NoEconomizer for this control interaction to function.

If economizer operation is not required, specify "NoEconomizer" as the economizer control type in the outside air controller object. The heat exchanger will operate according to its availability schedule and free cooling will not be provided. . If economizer operation *is* required and the heat exchanger *should not* provide free cooling, specify the input for the heat exchanger's economizer lockout as No and heat recovery will remain active during economizer mode.

Heat recovery for this heat exchanger may also be suspended during a high humidity control event (see object Controller:OutdoorAir) in a similar manner. Specifying Yes for economizer lockout will also suspend heat recovery when high humidity control is activated. The default value for economizer lockout is Yes and must be specifically entered as No to disable the economizer or high humidity control lockout feature.

### Supply Air Outlet Temperature Control

This heat exchanger object can also control the supply air outlet temperature to a setpoint to avoid overheating. This temperature control is accomplished through wheel speed modulation or bypassing supply air around the heat exchanger. To model this temperature control, the user must specify ‘Yes' for the Supply Air Outlet Temperature Control field in this heat exchanger object, and a separate setpoint manager (see object: SetpointManager:Scheduled) and temperature schedule  must be specified for the heat exchanger unit's supply air outlet node.

This control strategy is typically used in conjunction with economizer operation (see object Controller:OutdoorAir), and an example control profile is shown in the figure below. When the outdoor air temperature falls to the specified maximum limit for economizer operation, heat exchange is suspended (air is fully bypassed around the heat exchanger core or heat exchanger rotation is stopped). The figure below shows economizer operation being initiated based on outdoor temperature but other triggers can be used (e.g. differential temperature [outdoor temperature with respect to exhaust air temperature], single point enthalpy or differential enthalpy). Heat exchange remains suspended until the outdoor temperature falls to the minimum temperature (temperature lower limit) for economizer control. The setpoint for the supply air outlet temperature control should match the economizer temperature lower limit.

As the outdoor air temperature falls further below the setpoint for the supply air outlet temperature (same as the economizer lower temperature limit), the heat exchanger bypass dampers will modulate closed to maintain the desired supply air temperature for a plate heat exchanger. For a rotary heat exchanger the rotary heat exchanger speed will gradually increase to maintain the desired supply air temperature. Modulation of heat exchanger performance will continue until the supply air temperature setpoint can no longer be maintained. This control is only active if the entering supply (primary) air temperature is less thatn the heat exchanger setpoint temperature.

![Air to Air Heat Exchanger with Supply Air Temperature Control](media/air-to-air-heat-exchanger-with-supply-air.jpeg)


0

100

0

0

100

0
References

**ARI. 2001. Rating Air-to-Air Heat Exchangers for Energy Recovery Ventilation Equipment.** Arlington, Virginia: Air-Conditioning & Refrigeration Institute.

ARI. 2003. Certified Product Directory for Air-to-Air Energy Recovery Ventilation Equipment. Arlington, Virginia: Air-Conditioning & Refrigeration Institute.

## Air System Air-To-Air Flat Plate Heat Exchanger

### Overview

The input object HeatExchanger:AirToAir:FlatPlate provides an NTU – effectiveness model of a static flat plate air-to-air heat exchanger. Humidity issues are ignored in this model. A full, detailed description of the model can be found in the reference.

The inputs for the model are the design values for the primary air flow rate, primary air inlet and outlet temperature,  secondary air flow rate, and secondary air inlet temperature. No design UAs are required: instead, the ratio (at design conditions) of the primary *hA* to the secondary *hA* (*r~hA~*) is input. The flow configuration is also an input (counter, parallel, or crossflow).

- An important feature of this model is that the heat exchanger total *UA* is a time varying function of the primary and secondary mass flow rates and inlet temperatures.

### Model Description

The design inlet / outlet conditions determine a design effectiveness *eff*~des~. From the design capacity flow ratio, the flow arrangement, and *eff*~des,~ the NTU – effectiveness formulas give the *NTU~des~* and *UA~des~*.

The time varying calculations proceed as follows. First the *UA* is determined:

![](media/image5205.png)\


where *des* means *design*, *p* means *primary*, *s* means *secondary*, *T* is air stream temperature, and ![](media/image5206.png) is air stream mass flow rate. From the *UA* and the capacity flow ratio the *NTU* is determined: ![](media/image5207.png) . Then the NTU – effectiveness formulas are used to calculate the effectiveness. From the effectiveness and the inlet conditions, outlet condtions are determined.

### Economizer Operation

A conventional air-side economizer may be used in conjunction with this heat exchanger object. The air-side economizer is specified through the use of an outside air controller (see object: Controller:OutdoorAir). Specify the appropriate economizer control type, and provide the required control points and air flow rates as defined in the outside air controller object. Energy transfer provided by the heat exchanger will be suspended whenever free cooling is available (i.e., when the air-side economizer is activated) or high humidity control is active and *the user specified economizer lockout input is specified as Yes*. For this flat plate heat exchanger, heat transfer is suspended by fully bypassing the supply and exhaust air around the heat exchanger core. If the economizer lockout is specified as No, the flat plate heat exchanger is operational even when economizer or high humidity mode is active. The default value for economizer lockout is Yes and must be specifically entered as No to disable the economizer or high humidity control lockout feature.

### References

M. Wetter. 1999. *Simulation Model: Air-To-Air Plate Heat Exchanger*, LBNL-42354. This document can be downloaded from *http://simulationresearch.lbl.gov*.

## Air System Air-To-Air Balanced Flow Desiccant Heat Exchanger

### Overview

The input object HeatExchanger:Desiccant:BalancedFlow provides a model for a desiccant heat exchanger that is an HVAC component used to model both temperature (sensible) and moisture (latent) heat exchange between two air streams (Figure 252). The model assumes balanced air flow through the regeneration and process sides of the heat exchanger (i.e., regeneration and process air volume flow rates and face velocities are the same). Heat exchanger performance is specified through a performance data type object (e.g., Heat Exchanger:Desiccant:BalancedFlow:Performance Data Type 1). Refer to the EnergyPlus Input Output Reference for the specific input syntax for this desiccant heat exchanger object.

![Schematic of the Balanced Flow Desiccant Heat Exchanger](media/schematic-of-the-balanced-flow-desiccant-heat.jpeg)


EnergyPlus has another air-to-air heat exchanger object for modeling sensible and latent heat transfer between two air streams that uses the input object HeatExchanger:AirToAir:SensibleAndLatent. That heat exchanger model uses effectiveness values specified by the user to determine exiting air conditions from each side of the heat exchanger. In contrast, the balanced flow desiccant heat exchanger references a performance data type object (e.g., HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1) which defines the model equations, user-specified model coefficients, and min/max limits for the model's independent and dependent variables.

### Model Description

This heat exchanger is a simple object that performs three basic functions:

Informs the associated performance data type object (e.g., HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1) if it should calculate heat exchange for a simulation time step, or if heat exchange is suspended for the time step (i.e., bypass air flow around a fixed-plate heat exchanger or stop the rotation of a rotary heat exchanger),

Passes the process and regeneration air inlet conditions (e.g., dry-bulb temperature, humidity ratio, air mass flow rate) to the associated performance data type object, and

Reports the total, sensible and latent cooling/heating rates and electric power based on the results from the performance data type model that is specified.

While the desiccant heat exchanger object's availability to provide heat exchange between the two air streams is determined by the user-specified availability schedule, other objects that call this heat exchanger object can also control its heat exchange during a simulation time step. Currently, this desiccant heat exchanger model can be referenced by two compound objects: CoilSystem:Cooling:DX:HeatExchangerAssisted and Dehumidifier:Desiccant:System, both of which are used to provide enhanced dehumidification over conventional systems. If this heat exchanger is referenced by a compound object, the compound object will control heat exchanger operation (i.e., tell the heat exchanger if heat exchange is needed or not for each simulation time step). Details for how the compound objects control exchanger operation are described elsewhere in this document (ref. CoilSystem:Cooling:DX:HeatExchangerAssisted and Dehumidifier:Desiccant:System).

This desiccant heat exchanger object may also be specified directly in a AirLoopVHAC (air loop BranchList) or in an AirLoopHVAC:OutdoorAirSystem:EquipmentList without being referenced by a compound object. If specified directly in a AirLoopHVAC loop or AirLoopHVAC:OutdoorAirSystem:EquipmentList, then the heat exchanger can be controlled to provide heat exchange based on a maximum and/or minimum humidity setpoint placed on the process air outlet node (ref. SetpointManagers). If no humidity setpoints are provided on this node, then heat exchange will be provided whenever the heat exchanger is available to operate (via its availability schedule) and there is a temperature and/or humidity ratio difference between the two air streams. Further details regarding heat exchanger control via humidity setpoints on the process air outlet node are described in the section for the associated data type object (e.g., HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1).

The balanced flow desiccant heat exchanger model first checks for three conditions to be true before calling the specified performance data type model to calculate heat exchanger performance:

Non-zero air mass flow rates on the process and regeneration inlet air nodes,

Desiccant heat exchanger is available to operate based on its availability schedule,

If a compound object is calling this desiccant heat exchanger, it is requesting that heat exchange be provided.

If any of these conditions is false, then heat exchange is suspended and the model simply passes the air conditions on the process and regeneration air inlet nodes to the respective outlet air nodes. In addition, the heat exchanger electric power is set to zero.

If all of the above conditions are true, then heat exchange is active and the specified performance data type model is called to calculate the process and regeneration outlet air conditions and heat exchanger electric power. Immediately before this call to the performance data type model, a check is made to determine if the operating air flow rates through the heat exchanger (i.e., the actual air flow rates for the simulation time step) are equal (balanced flow). If a difference of more than 2% exists between the process and regeneration air flow rates at any time during the simulation, a warning is issued.

After the specified performance data type model calculates the process/regeneration air outlet conditions and heat exchanger electric power, the balanced flow desiccant heat exchanger model uses that information to report overall performance. Specifically, the heat exchanger's sensible, latent and total heating and cooling rates are calculated for the process air side of the heat exchanger. Since energy must be conserved, the same heating or cooling rates apply to the regeneration air stream although they are opposite in heat transfer direction (e.g., a sensible cooling rate on the process air side of the heat exchanger would indicate an equivalent sensible heating rate on the regeneration air side).

![](media/image5209.png)\


![](media/image5210.png)\


![](media/image5211.png)\


where:

![](media/image5212.png) = sensible heat transfer rate to the process air stream (W)

![](media/image5213.png) = total heat transfer rate to the process air stream (W)

![](media/image5214.png) = latent heat transfer rate to the process air stream (W)

![](media/image5215.png) = process air mass flow rate (kg/s)

![](media/image5216.png) = specific heat of inlet process air (J/kg-K)

![](media/image5217.png) = process air outlet temperature (°C)

![](media/image5218.png) = process air inlet temperature (°C)

![](media/image5219.png) = process air outlet enthalpy (J/kg)

![](media/image5220.png) = process air inlet enthalpy (J/kg)

To simplify the accounting of heat exchanger performance, the sensible, latent, and total heat transfer rates for the process side of the heat exchanger are stored in cooling and heating report variables. For example, if the sensible heat transfer rate is negative, the absolute value is stored in a "cooling" rate report variable. Conversely, if the sensible heat transfer rate is positive, the value is stored in a "heating" rate report variable. Similar accounting is performed for the latent and total heat transfer rate variables as follows:

![](media/image5221.png)\


![](media/image5222.png)\


![](media/image5223.png)\


![](media/image5224.png)\


![](media/image5225.png)\


![](media/image5226.png)\


At the end of each HVAC simulation time step, this object reports the sensible, latent and total cooling/heating energy and electric consumption for the heat exchanger as follows:

![](media/image5141.png)\


![](media/image5142.png)\


![](media/image5143.png)\


![](media/image5144.png)\


![](media/image5145.png)\


![](media/image5146.png)\


![](media/image5147.png)\


where:

![](media/image5148.png) = output variable ‘Heat Exchanger Sensible Cooling Energy, J'

![](media/image5149.png) = output variable ‘Heat Exchanger Sensible Cooling Rate, W' = ![](media/image5150.png)

                                   during times when the process air is cooled

*TimeStepSys* = HVAC system simulation time step, hr

![](media/image5151.png) = output variable ‘Heat Exchanger Latent Cooling Energy, J'

![](media/image5152.png) = output variable ‘Heat Exchanger Latent Cooling Rate, W' = ![](media/image5153.png)

   during times when the process air is dehumidified

![](media/image5154.png) = output variable ‘Heat Exchanger Total Cooling Energy, J'

![](media/image5155.png) = output variable ‘Heat Exchanger Total Cooling Rate, W' = ![](media/image5156.png) during

    times when the process air enthalpy is reduced

![](media/image5157.png) = output variable ‘Heat Exchanger Sensible Heating Energy, J'

![](media/image5158.png) = output variable ‘Heat Exchanger Sensible Heating Rate, W' = ![](media/image5159.png)

   during times when the process air is heated

![](media/image5160.png) = output variable ‘Heat Exchanger Latent Gain Energy, J'

![](media/image5161.png) = output variable ‘Heat Exchanger Latent Gain Rate, W' = ![](media/image5162.png)  during times when the process air is humidified

![](media/image5163.png) = output variable ‘Heat Exchanger Total Heating Energy, J'

![](media/image5164.png) = output variable ‘Heat Exchanger Total Heating Rate, W' = ![](media/image5165.png) during times when the process air enthalpy is increased

![](media/image5166.png) = output variable ‘Heat Exchanger Electric Energy, J'

![](media/image5167.png) = output variable ‘Heat Exchanger Electric Power, W'

### Economizer Operation

A conventional air-side economizer may be used in conjunction with this heat exchanger object. The air-side economizer is specified through the use of an outside air controller (see object: Controller:OutdoorAir). Specify the appropriate economizer control type, and provide the required control points and air flow rates as defined in the outside air controller object. Energy transfer provided by the heat exchanger will be suspended whenever free cooling is available (i.e., when the air-side economizer is activated) or high humidity control is active and *the user specified economizer lockout input is specified as Yes*. For the desiccant heat exchanger, heat transfer is assumed to be provided by a rotating heat exchanger core. For this reason, air continues to flow through the core when the outdoor air economizer is active but it is assumed that the rotation of the heat exchanger core is stopped. If the economizer lockout is specified as No, the desiccant heat exchanger is operational even when economizer or high humidity mode is active. This specific type of heat exchanger is typically *not* controlled by an outdoor air economizer. For this reason, the default value for economizer lockout is No and must be specifically entered as Yes to enable the economizer or high humidity control lockout feature.

### Desiccant Heat Exchanger Performance Data

#### Overview

The input object HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1 specifies a performance model and model coefficients for a balanced flow desiccant heat exchanger. The HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1 object is referenced by a HeatExchanger:Desiccant:BalancedFlow object. This performance data object is used to specify the thermal performance and electric consumption of the heat exchanger. Some representative inputs for this object are provided in the EnergyPlus Reference DataSets (PerfCurves.idf).

This model predicts the regeneration air stream outlet temperature and humidity ratio values based on the entering regeneration and process air stream temperature, humidity ratio and face velocity. The process air stream outlet humidity ratio and temperatures are calculated based on a simple heat and moisture balance. The model requires that the user enter the nominal volumetric flow rate and a nominal face velocity, electric power consumption, empirical model coefficients for the regeneration outlet air temperature and humidity ratio equations, and the applicable minimum and maximum values for both the independent and dependent variables for the empirical model coefficients provided. Refer to the EnergyPlus Input Output Reference for details regarding the input syntax for this object.

#### Heat Exchanger Face Area and Air Velocity

The user is required to enter a nominal volumetric air flow rate and a nominal face velocity. From these inputs, a heat exchanger face area (applicable for both the regeneration and process sides of the heat exchanger) is calculated and used to determine the operating face velocity during the simulation.

![](media/image5227.png)\


![](media/image5228.png)\


where:

![](media/image5229.png) = heat exchanger face area (m^2^)

![](media/image5230.png) = nominal air volume flow rate specified for the heat exchanger (m^3^/s)

![](media/image5231.png) = nominal air face velocity specified for the heat exchanger (m/s)

![](media/image5232.png) = face velocity of the regeneration (and process) air stream (m/s)

![](media/image5233.png) = mass flow rate of the regeneration air stream (kg/s)

![](media/image4885.png) = density of air at standard temperature and pressure [dry air at 20°C] (m^3^/kg)

The face velocity calculated each simulation time step is used in the empirical equations (see ‘Model Calculations' below) and should be within the minimum and maximum velocity boundaries specified for the model coefficients (see ‘Empirical Model Boundaries (Minimum and Maximum)‘ below). When the calculated air velocity exceeds one of the boundaries, a warning is issued and the velocity is reset to the appropriate boundary value before being passed to the empirical equations for calculating regeneration air outlet temperature and humidity ratio. If the user is confident in their empirical model coefficients, the minimum and maximum velocity boundaries may be expanded slightly (caution should be used here) to allow extrapolation of the empirical equations during the simulation.

#### Model Calculations

The model coefficients may be obtained by curve fitting field measurements or the results from other computer models that accurately reflect the performance of a balanced flow desiccant heat exchanger. A wide range of data is necessary to properly define the performance of the desiccant heat exchanger such that all operating conditions expected during a simulation are included in the range of data used to obtain the model coefficients. The minimum and maximum boundaries for the independent variables used to generate the empirical model coefficients are used by this model to ensure that model extrapolation does not occur at any point during the simulation. For this reason, it is recommended that the widest possible range of data be used to determine the model coefficients.

The dry-bulb temperature of the regeneration outlet air is determined using the equation shown below.

![](media/image5234.png)\


where:

![](media/image5235.png)  = regeneration outlet air dry-bulb temperature (°C)

![](media/image5236.png)  = regeneration inlet air humidity ratio (kg/kg)

![](media/image5237.png)   = regeneration inlet air dry-bulb temperature (°C)

![](media/image5238.png)  = process inlet air humidity ratio (kg/kg)

![](media/image5239.png)   = process inlet air dry-bulb temperature (°C)

![](media/image5240.png)  = regeneration (and process) face velocity (m/s)

Similarly, the humidity ratio of the regeneration outlet air is defined using the same empirical equation form; however, different coefficients are used as follows:

![](media/image5241.png)\


where:

![](media/image5242.png)  = regeneration outlet air humidity ratio (kg/kg)

If the regeneration outlet air conditions exceed the saturation curve, RTO and RWO are reset to saturated conditions (100% RH) at the enthalpy calculated based on the original RTO and RWO values.

Once the regeneration outlet air conditions are determined as described above, the dry-bulb temperature and humidity ratio differences across the regeneration side of the heat exchanger are calculated.

![](media/image5243.png)\


![](media/image5244.png)\


where:

![](media/image5245.png) = actual regeneration inlet air dry-bulb temperature (°C)

![](media/image5246.png) = actual regeneration inlet air humidity ratio (kg/kg)

![](media/image5247.png) = regeneration air temperature difference based on empirical model (°C)

![](media/image5248.png) = regeneration air humidity ratio difference based on empirical model (kg/kg)

The regeneration outlet air conditions described above represent the full load outlet conditions under steady-state operation. However, there are times when the heat exchanger may not operate for the entire simulation time step (i.e., bypass air flow around a fixed-plate heat exchanger or stop the rotation of a rotary heat exchanger for a portion of the time step). For example, the parent object calling this heat exchanger model may request that it only provide heat exchange for a portion of the time step (*HXPartLoadRatio*). Another example would be if this heat exchanger is not called by a parent object but is instead placed directly in a AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem and a setpoint manager is used to place a minimum and/or maximum humidity ratio setpoint on the process air outlet node. For this case the humidity setpoints, if present, are used to calculate a part-load ratio for the heat exchanger assuming the full-load process air humidity ratio difference is equivalent (but opposite in sign) to the regeneration air humidity ratio difference (*w~diff~*):

![](media/image5249.png)\


If this heat exchanger is not being called by a parent object and no humidity setpoints are placed on the process air outlet node, then the model assumes that the heat exchanger operates for the entire simulation time step when it is available to operate (based on its availability schedule) and there is a temperature and/or humidity ratio difference between the two air streams.

EnergyPlus has an established convention for placing information on outlet air nodes. If the air flow rate is continuous but the device only operates for a portion of the simulation time step, then the average outlet conditions (temperature, humidity and enthalpy) are placed on the outlet air node. If the air flow rate cycles on and off during the simulation time step, then the full load outlet conditions are placed on the outlet air node along with the average air mass flow rate for the simulation time step. To account for these cases, this model uses the following logic:

![](media/image5250.png)\


where:

![](media/image5251.png) = regeneration outlet air dry-bulb temperature (°C)

![](media/image5252.png) = regeneration outlet air humidity ratio (kg/kg)

![](media/image5253.png)  = heat exchanger part-load ratio (determined by parent object calling this model, or calculated based on a minimum/maximum humidity ratio setpoints as shown above).

In the logic shown above, *RegAirInletIsOANode* is true if the regeneration air inlet node is an outside air node. If so, the model assumes that the air flow through the regeneration side of the heat exchanger cycles on and off as required during the simulation time step. Also, the regeneration outlet air humidity ratio is limited to be between 1E-5 and 1.0.

The regeneration outlet air enthalpy is then calculated using the regeneration outlet air temperature and humidity ratio.

![](media/image5254.png)\


Using the regeneration outlet air conditions, the heat transfer on the regeneration side of the heat exchanger is then calculated:

![](media/image5255.png)\


![](media/image5256.png)\


where:

![](media/image5257.png) = sensible heat transfer rate to the regeneration air stream (W)

![](media/image5258.png) = total heat transfer rate to the regeneration air stream (W)

![](media/image5259.png) = regeneration air mass flow rate (kg/s)

![](media/image5260.png) = specific heat of inlet regeneration air (J/kg-K)

Since the model assumes that total and sensible heat transfer is conserved, these heat transfer rates are then used to calculate the process air outlet conditions:

![](media/image5261.png)\


![](media/image5262.png)\


![](media/image5263.png)\


where:

![](media/image5264.png) = process outlet air enthalpy (J/kg)

![](media/image5265.png) = process outlet air dry-bulb temperature (°C)

![](media/image5266.png) = process outlet air humidity ratio (kg/kg)

![](media/image5215.png) = process air mass flow rate (kg/s)

![](media/image5216.png) = specific heat of inlet process air (J/kg-K)

Like the regeneration outlet air conditions, the process outlet air conditions are also checked for exceeding saturated conditions and, if detected, the temperature and humidity ratio are reset assuming constant enthalpy.

![](media/image5267.png)\


Heat recovery electric power is the electric consumption rate of the unit in watts. The nominal electric power for the heat exchanger is specified in the input data file, and can be used to model controls (transformers, relays, etc.) and/or a motor for a rotary heat exchanger. The model assumes that this electric power is consumed whenever the heat exchanger operates. The electric power is assumed to be zero for all other times. None of this electric power is assumed to contribute thermal load to either of the heat exchanger air streams. As with the thermal performance of the heat exchanger, the power used by the heat exchanger is also proportional to the heat exchanger part load ratio.

![](media/image5268.png)\


where:

![](media/image5269.png) = output variable ‘Heat Exchanger Electric Power, W' reported by the HeatExchanger:Desiccant:BalancedFlow object.

![](media/image5270.png) = user specified ‘Nominal Electric Power, W'

#### Empirical Model Boundaries (Minimum and Maximum)

Since this model uses empirical equations and extrapolation can cause unexpected results, the entering air conditions to both the regeneration and process sides of the heat exchanger are checked during each simulation time step to make sure they are within the valid range defined by the user. If any of the independent variables used in the equations for RTO and RWO above are outside the minimum and maximum limits specified for each independent variable, the values for these specific independent variables are limited to the appropriate boundary (min/max) value for use by the empirical equations and a warning is issued.

Additional model checks occur by testing the regeneration and process inlet air relative humidities. In this case, the process and regeneration inlet air relative humidities are calculated and compared to the minimum/maximum values specified by the user. If the calculated relative humidity exceeds the specified range at any point during the simulation when the heat exchanger is operating, a warning message is issued but the simulation proceeds.

> If the user is confident in their empirical model coefficients, the minimum and maximum independent variable boundaries may be expanded (caution should be used here) to allow extrapolation of the empirical equations during the simulation.

In addition to checking the inlet air conditions, the model allows the user to specify limits for the regeneration air outlet conditions (RTO and RWO). If the calculated value for RTO or RWO exceeds the minimum/maximum limits specified by the user, then the calculated value is reset to the appropriate boundary (min/max) value and a warning message is issued.

This model also assumes that the performance of the desiccant heat exchanger (i.e., the output of the empirical equations) conforms to the following two guidelines.

The regeneration outlet air temperature (RTO) is always less than or equal to the regeneration inlet air temperature (*T~Reg~~In~*)

The regeneration outlet air humidity ratio (RWO) is always greater than or equal to the regeneration inlet air humidity ratio (*w~Reg~~In~*)

When the calculated regeneration outlet air conditions do not follow these guidelines, a warning is issued and the simulation proceeds.

## Plant Loop Deep-Ground-To-Water Vertical U-Tube Field Heat Exchanger 

This model (Object: GroundHeatExchanger:Vertical) documentation is derived from the M.S. thesis of Arunachalam Murugappan, which is available on the Oklahoma State University web site http://www.hvac.okstate.edu/.  Eskilson (1987) Yavuzturk and Spitler (1999) developed the long and short time response factors respectively, which are used in determining the borehole temperature responses. Response factors are infinite series of numbers, which relate the current value of a variable to past values of other variables at discrete time intervals.  These response factors are referred as *g*-functions. The variable time-step model presented here uses both long time-step *g*-functions and short time-step *g*-functions to predict the boreholes response to short term fluctuations in the load.

### Long Time-Step Response Factors

Eskilson developed *g*-functions for various borehole configurations. He plotted the g function curves for different sets of borehole spacing to borehole length ratio ![](media/image5271.png)  typically for .05, 0.1, 0.15, 0.2, 0.3 and ∞ (∞ represents the single borehole configuration. All the plots were for the ratio of 0.0005 between the borehole radius and the borehole length ![](media/image5272.png) . For any other radius a simple relation between the two radii as given by Eskilson (1987) can be used. Eskilson gives the *g*-function curves for 38 different configurations.

Figure 253 shows the *g*-function plotted against the non-dimensional time defined as ![](media/image5273.png) , ![](media/image5274.png)  for various configurations of vertical boreholes with *B/H* ratio of .1 along with single borehole. It is seen from this figure that the thermal interaction between boreholes increases with time and with the number of boreholes in field.

![Short Time Step g Function Curve as an Extension of Long Time Step g Function Curves for Different Configuration of Boreholes (Eskilson 1987, Yavuzturk 1999).](media/short-time-step-g-function-curve-as-an.png)


The *g*-functions developed by Eskilson are valid only after time estimated by Eskilson as ![](media/image5276.png) . This time varies from 3-6 hours for a typical borehole field. This is because the analytical line source model, based on which the Eskilson's model was developed, does not give a prompt increase in borehole wall temperature at ![](media/image5277.png) . It gives acceptable results only after the non-dimensional times of ![](media/image5278.png) . But to model short time responses of a borehole we need response factors, which can give accurate results down to minutes.

### Short Time-Step Response Factors

Yavuzturk and Spitler (1999) developed short time step response factors using a transient, two-dimensional, implicit finite volume model on a polar grid.

The circular u-tube pipe in the ground loop heat exchanger was approximated as a pie sector of equivalent perimeter.  A constant heat flux for the heat transfer form/to U-tube, a zero heat flux in the angular direction and a constant far field temperature in the radial axis makes up the three boundary condition and undisturbed far field temperature as the initial condition. The numerical model accounts for the thermal resistance due to individual borehole elements; such as resistance of the pipe and grout material and the convection resistance due to the heat transfer fluid in the pipes. The long time step *g*-functions discussed in the previous section do not account for such effects.

The short time-step *g*-functions are the same for different borehole configurations. This is because there is no thermal interaction between the boreholes for times less than 200 hrs during which the short time-step *g*-functions apply. So it is suggested to use the short time-step *g* function for time steps in the range of 2.5 min and 200 hours and the long time-step *g*-functions for time steps longer than 200 hours. The *g* function for any time can be found by linear interpolation between the bounding known values.

### Development of the Variable Short Time Step Model

The variable time step model was developed as an extension of the model presented by Yavuzturk and Spitler (1999). The variable, short time step model uses a similar algorithm and extends it to accommodate sub-hourly responses, variable time steps and explicit equations to calculate the outlet fluid temperature of the ground loop heat exchanger.

The uniform time-steps model developed by Yavuzturk and Spitler (1999) is able to pre-calculate all the g-functions at the beginning of the simulation. The variable time-step model on the other hand must calculate the *g*-functions when the borehole response calculation for each time step is carried out. For every time step a different set of *g*-functions is needed in the variable time step model as the time at which the *g*-function is to be applied for the past loads changes for each time-steps.

This is made clear from the illustration in Figure 254, which shows a simulation in progress. The boxes with numbers represent the sub-hourly loads. The time (in hrs) at which these loads occurred are shown by solid arrows above the respective load boxes. The right-most solid arrow gives the current simulation time, which is 3.31 hrs. The times given below the boxes, pointed by dashed arrows, are the time at which the *g*-functions are to be estimated and applied to the respective sub hourly loads (boxes) for the current time.

For example, let us take the sub hourly loads 1,2 & 3. These loads occurred at 0 hrs, 0.16 hrs & 0.28 hrs. The response of the borehole temperature for the current time step is calculated by applying the *g-*functions at 3.15 hrs, 3.03 hrs & 2.5 hrs respectively. Thus to calculate the present borehole temperature, the sub hourly loads 1-12 are superposed using the corresponding *g* functions at times given by the dashed lines.  This gives the borehole temperature at hr 3.31. However, for the previous time step, which occurred at 3.15 hrs, the g-functions for the loads 1, 2 & 3 are at 2.99 hrs 2.87 hrs and 2.42 hrs, and the over all response is obtained by superposing the loads 1-11.

Thus for each time step since the time step increments are not uniform we need to store the simulation times at which these time-steps occurred, and calculate corresponding *g*-functions at each time-step.

![Variable Timestep Ground Loop Heat Exchanger Model Schematic Explaining the g Function Estimation.](media/variable-timestep-ground-loop-heat-exchanger.png)


Yavuzturk model calculates the outlet fluid temperature by iteration beginning with the undisturbed mean temperature of the surrounding ground as an initial guess. This aggravates the time taken by an already computationally intensive algorithm. To circumvent this a set of explicit equations were formulated to estimate the outlet fluid temperature.

### Description of the Load Aggregation Scheme

A load aggregation scheme was developed for energy simulation software with variable short time steps down to one minute. A major issue in the development was the calculation of the *g*-functions.  As discussed previously in the variable time step environments, the time step increments are not uniform. As a result, g-functions cannot e pre-calculated. Figure 255 shows a schematic of the variable time step ground loop heat exchanger model. The figure shows the larger monthly block loads, hourly loads and sub hourly loads and along with the time of occurrence of those loads. The figure also shows the time at which the *g*-functions are applied to different load blocks.

![Schematic of Variable Time Step Model g Function Calculation.](media/schematic-of-variable-time-step-model-g.png)


To calculate the response of a past load on the borehole temperature we apply the *g*-function corresponding to the time elapsed since the load was applied. This is easily understood form the schematic. For example, to calculate the response of the aggregated load 1" (at the end of 730hrs.) for the current time step (2193.733hrs) we apply a *g*-function at 1463.73hrs. The *g*-function for the same block 1" at the previous time step, which occurred at 2193.25 hrs, would be at 1463.25hrs. From the schematic it is also seen that for the other two aggregated monthly loads 2", 3" the *g*-function are applied at 733.73 hrs and 3.73 hrs for the current time-step and at 733.25 hrs and 3.25 hrs respectively for the previous time-step. The same scheme applies to hourly and sub-hourly. Thus to estimate the time at which the past monthly, hourly or sub-hourly loads occur, we might be tempted to store the simulation times at each time step for the entire simulation, storing load times for the whole length of simulation for a multi year simulation with a variable short time step would require a large amount of memory. But little consideration shows that it is not necessary.  Since the monthly and hourly loads occur at equal intervals of time 730hrs and 1hr respectively, the *g*-functions can be estimated with the current simulation time and the time at which the load block ends, which is a multiple of the monthly duration of the block size. Only the sub-hourly loads require storage of simulation times.

For example from the schematic (Figure 256), for the sub hourly load 1, which occurred at the end of 2193.25 a *g*-function at .48 hrs has to be applied and for the next load 2 a *g*-function at 0.34 hrs has to be applied. Since the time intervals are not even for the sub hourly loads, we need to store the time steps at which those loads occurred. These times are required to estimate the time elapsed between the current simulation time and the time at which the sub hourly loads occurred.

Thus, the algorithm keeps track of the sub hourly loads along with their time of occurrence for a user-defined length of time during which the sub hourly calculations are made. The algorithm also estimates the time weighted hourly load from their corresponding sub hourly loads as each hour passes. The sub-hourly loads are time weighted because of the irregular intervals at which the time-step occurs. This is also illustrated in Figure 256. The sub hourly loads 1,2 &3 occur for varying length of time. The load 3 occurs for a longer duration than 1 and 2 in that order. This implies that the load 3 has to be given more weight than 1 and 2. So the sub hourly loads for a particular hour are multiplied by the length of their respective period of occurrence and averaged over the hour. This is further explained by the schematic in Figure 5.

![Schematic Showing the Calculation of Hourly Load from the Sub Houly Loads.](media/schematic-showing-the-calculation-of-hourly.png)


The bottom text in the boxes represents the magnitude of the sub hourly loads in W/m for each time step. The duration of the occurrence of each time-step for the each block is shown below the respective block. The first hourly load is given by the expression

![](media/image5282.png)\


Where ![](media/image5283.png)  = the first hourly load in W/m

The algorithm keeps track of enough of these past hourly loads to calculate the monthly load. As each month or user defined time passes, hourly loads over the entire month or user defined time "blocks" are averaged and stored in arrays for the respective monthly user defined block of time.

The borehole temperature for any time step is computed by superposing the monthly (larger time block loads) hourly and sub-hourly loads for each time step. To understand more clearly consider the schematic in Figure 256 where the borehole temperature at 2193.733 hour is to be estimated. Here the monthly block time is 730 hrs. We have three monthly aggregated load blocks for 730 hrs, 1460 hrs and 2190 hrs and hourly loads from 2191^st^ hr to 2193^rd^ hour. For the remaining 0.733 hours a sub hourly calculation is done. The three monthly aggregated load blocks when superposed using long time *g-*functions, yields the borehole temperature at the end of 2190^th^ hour. Then the hourly loads from 2191^st^ to 2193^rd^ hrs are superposed using the corresponding short time step *g* functions values yielding the borehole temperature at the end of 2193^rd^ hour. The sub-hourly variations for the current hour are obtained, by superposing the sub-hourly loads. From the schematic, we see there are two sub-hourly loads, 1 and 2. Thus the borehole temperature at the end of 2193.733 is expressed as:

![](media/image5284.png)\


![](media/image5285.png)\


![](media/image5286.png)\


Where

![](media/image5287.png)  = The average monthly loads

![](media/image5288.png)   = The average hourly loads

*Q* = the sub-hourly loads

*m* = index for monthly aggregated blocks

*p* = array index for sub hourly loads

*t* = time

t~p~ =  the sub hourly time steps over the history period. (here increment is not always unity)

Superposing the temperature responses of monthly (larger) blocks over the shorter, namely the hourly and sub hourly, introduces some error in the borehole temperature calculation at the beginning of every month. Yavuzturk and Spitler suggest a method to reduce the error in borehole temperature prediction by using a minimum hourly history period during which only the short time step superposition is carried out. In our model this idea is extended to sub hourly loads as well. Thus a user specified minimum sub-hourly history period is included along with the minimum hourly history period to model the sub-hourly variations. During this period only sub-hourly and hourly superpositions are made. This guarantees that at any given time step the superposition of temperature responses involves a minimum period of short time responses, which ensures a better estimation of borehole temperature. For example, a minimum hourly history period of 96 hrs and a minimum sub hourly history period of 5 hours would result in only 2 monthly aggregation blocks (1" and 2"). The last monthly aggregation does not occur because neither of the minimum hourly history period of 96 hours or sub-hourly history period of five hrs is met. So an hourly superposition of the load is carried out for the third month until the minimum sub-hourly history period after which sub hourly superposition is carried out. The equation becomes

![](media/image5289.png)\


![](media/image5290.png)\


![](media/image5291.png)\


Yavuzturk and Spitler have done a detailed analysis on the effect of minimum hourly history period. They found that a minimum hourly history period of 192 hrs for an annual simulation would reduce the running time by 90%.  They also found that for a 20year simulation, the computation time of the aggregated load scheme is just 1% of the non-aggregated load scheme.

### Summary of Variable Short Time Step Response Factor Model

The load aggregation scheme developed in line with the above example is summarized eight steps as follows:

Define monthly load blocks duration (mb) in hrs (generally 730 hrs) and the minimum hourly history period and minimum sub hourly history period.

Read Borehole Geometry Parameters: number of boreholes, borehole length radius thickness of the pipe etc. Read Ground and Fluid thermal properties: Ground conductivity, volumetric specific heat capacity of the ground and heat carrier fluid. Read the short and long time-step *g-*functions into arrays with their respective non-dimensionalized times.

Start Simulation from p=1 to nts. Here "nts" is the number of time steps that have occurred since the start of simulation. (Note that P in not a count of number of hour elapsed in the simulation)

Compute the hourly loads as each hour passes. This is done by averaging the sub hourly loads during the past hour. The monthly loads are calculated by averaging the hourly loads during that month. This is done by summing the hourly loads during that monthly period and dividing the sum by 730 hours NumMonths(the number of months used in aggregation calculations) is set to the number of months of simulation (current number of aggregated load blocks)

If the simulation time is less than the minimum sub hourly history period the borehole temperature is estimated with no aggregation. Only sub hourly loads are superposed as given by the following equation.

![](media/image5292.png)\


. If the simulation time is less than sum of minimum hourly history and sub hourly history periods, then decomposed hourly aggregated loads are superposed using their corresponding g function until the sub hourly history required. Then the sub hourly temperature differences are found by superposing the decomposed sub hourly loads with their short time step *g* functions. Average borehole temperature is found by superposing the hourly and sub hourly temperature differences with the following equation

![](media/image5293.png)\


If the simulation time is greater then the sum of a monthly period, sub hourly history and the hourly history period. Then monthly load aggregation is performed. Here if the difference between the simulation time and product of a monthly block period and the current number of monthly blocks is greater than the sum of the minimum hourly history and sub hourly history periods then the average borehole temperature is found by the following equation.

![](media/image5294.png)\


![](media/image5295.png)\


**![](media/image5296.png)**

If the difference between the simulation time and product of a monthly block period and the current number of monthly blocks is less than the sum of the minimum hourly history and sub hourly history periods, then NumMonths is set to one month less than the actual number of months of simulation completed. The average borehole temperature is calculated by superposing the long and time step temperature differences using the following equation.

![](media/image5297.png)\


![](media/image5298.png)\


**![](media/image5299.png)**

~~~~~~~~~~~~~~~~~~~~

    Define Monthly block duration, minimum hourly history period and minimum sub hourly history period (step 1)

    Read Borehole Geometry parameters, Ground and carrier fluid properties and read short and long time-step g functions. (Step 2)

    Do until p = 1 to number of time-steps (nts) (Step 3)
    Compute the Ground Load for the current time step using Entering Fluid temperature (For the first time-step T~FarField~may be used)
    Compute the hourly loads as each hour and monthly loads as each month passes. Calculate the number of monthly blocks (NumMonths)(Step 4)
    If (Current time less than minimum sub hourly history) (Step 5)
    use Equation
    Else If (Current Simulation Time less than sum of minimum hourly history and minimum sub hourly history) (step 6)
    use Equation
    Else
    If (Difference between current simulationtime and duration of the total number months is Greater than sum of minimum hourly and sub hourly histories) (step 7)
    use Equation
    Else (step 8)
    use Equation
    End if
    End if
    End do

~~~~~~~~~~~~~~~~~~~~

### References

Eskilson, P. 1987. Thermal Analysis of Heat Extraction Boreholes. Ph.D. Thesis, Department of Mathematical Physics, University of Lund, Lund, Sweden.

Yavuzturk, C. 1999. Modeling of Vertical Ground Loop Heat Exchangers for Ground Source Heat Pump Systems. Ph.D. Thesis, Department of Mechanical and Aerospace Engineering, Oklahoma State University, Stillwater, Oklahoma.

Yavuzturk, C., J.D. Spitler. 1999. A Short Time Step Response Factor Model for Vertical Ground Loop Heat Exchangers. *ASHRAE Transactions*. 105(2):475-485.

## Plant Loop Pond-To-Water Heat Exchanger

The pond model (Object: GroundHeatExchanger:Pond) is a ‘lumped parameter' model where the pond is represented by a single node with thermal mass. The pond surface temperature is the same as the temperature at this node, i.e. the surface temperature is the same as the bulk temperature. A first order differential equation is solved in the model to calculate the pond temperature at each time step. This type of heat rejecter is modeled as several circuits connected in parallel.

![Heat Transfer Mechanisms in a Pond (Chiasson 1999)](media/heat-transfer-mechanisms-in-a-pond-chiasson.png)


Rees(2002) implemented the model developed by Chiasson(1999) for the shallow ponds. The model is based on the assumption that thermal gradients in shallow ponds are negligible, especially during times of heat rejection. Using the lumped parameter approach, an overall energy balance for the pond may be devised as

![](media/image5301.png)\


Where

![](media/image5302.png)    =   Heat transfer to the pond

![](media/image5303.png)   =   Heat transfer from the pond

![](media/image5304.png)      =   Pond Volume

![](media/image5305.png)      =   Density of pond water

![](media/image5302.png)  = Heat transfer to the pond

![](media/image5306.png)  = Specific heat capacity of pond water

![](media/image5307.png)  = rate of change of temperature of the pond water

Rate of change in average pond temperature is given as

![](media/image5308.png)\


Where

q~solar~~~= Solar radiation heat gain to the pond

q~thermal~= Thermal radiation heat transfer at the pond surface.

q~convection~= Convection heat transfer at the pond surface

q~ground~~~= Heat transfer to/from ground to the pond

q~groundwater~~~= Heat transfer due to ground water inflow/outflow

q~evaporation~= Heat transfer due to evaporation at the pond surface

q~fluid~= Total heat transfer to/from the heat exchanging fluid flowing in all spools or  coils in the pond

A heat balance is calculated at a single node that represents the pond. Heat transfer takes place by surface convection, long-wave radiation to the sky, absorption of solar energy, ground heat transfer and heat exchange with the fluid. A heat exchanger analogy is used to calculate the heat transfer between the heat transfer fluid and the pond. The differential equation defined by the heat balance is solved using a fourth order Runge-Kutta numerical integration method. The implementation along with the model equations are summarized in the figure below.

![Pond Model Component Configuration (Chiasson 1999)](media/pond-model-component-configuration-chiasson.png)


The model overall pond model consists in a number of sub-models which are enumerated below.

### Solar radiation heat gain to the pond

![](media/image5310.png)\


where

![](media/image5311.png) is the reflectance

![](media/image5312.png)  is the transmittance of solar radiation by the pond surface and the subscript ‘a' refers to the absorbed component.

![](media/image5313.png)\


and

![](media/image5314.png)\


where

![](media/image5315.png) is the extinction coefficient for water

![](media/image5316.png)  is the pond depth

![](media/image5317.png) represents the parallel component of unpolarized radiation and

![](media/image5318.png) represents the perpendicular component of unpolarized radiation which are computed by Duffie and Beckman (1991) as:

![](media/image5319.png)\


![](media/image5320.png)\


Finally, the amount of solar radiation absorbed by the pond (![](media/image5321.png) ) is expressed as:

![](media/image5322.png)\


where

![](media/image5323.png) is the solar radiation flux incident on the pond surface (here, the total reflectance is approximated by the beam reflectance)

![](media/image5324.png) is the area of the pond surface.

The model also accepts solar radiation in the form of beam ![](media/image5325.png) and diffuse ![](media/image5326.png)  components, in which case ![](media/image5323.png) is computed from:

![](media/image5327.png)\


### Thermal radiation heat transfer at the pond surface

![](media/image5328.png)\


Where

![](media/image5329.png) = Thermal absorptivity

![](media/image5330.png) = Stefan Boltzman constant

### Convection heat transfer at the pond surface

![](media/image5331.png)\


ASHRAE simple convection coefficient model is used to obtain the convection coefficient for external surfaces. Initializing of pond temps to mean of dry-bulb temperature T~db~ and ground temperatures is useful because repeated warm up days tend to drive the initial pond temperature toward the dry-bulb temperature Hence, for each environment the simulation starts the pond temperature T~pond~midway between the dry-bulb and ground temperature.

### Heat transfer to/from ground to the pond

Hull et al (1984) expressed the following equation for ground heat losses from any pond. The equations are also based on the assumption that, for all practical purposes, the constant temperature sink is the ground water table (Kishore and Joshi 1984)

![](media/image5332.png)\


![](media/image5333.png)\


Where

![](media/image5334.png)         = Thermal conductivity of the ground

 ![](media/image5335.png)  = depth of water table

![](media/image5336.png)           = Pond depth

![](media/image5337.png)          = Pond perimeter

### Heat transfer due to evaporation at the pond surface

Evaporation is calculated assuming a fixed Lewis number unlike the Chaisson model.

![](media/image5338.png)\


Where

Pr =Prandtl number for air

Sc = Schmidt number for air

Hc = convection coefficient

HRf= humidity ratio at pond surface/film temperature

HRa= humidity ratio of air

ql  = Latent heat of air

Air properties are obtained by applying the existing the psychometric functions of EnergyPlus.

### Total heat transfer to/from the heat exchanging fluid flowing in all spools or coils in the pond

![](media/image5339.png)\


Where

![](media/image5340.png)  = overall heat transfer coefficient expressed in terms of inside pipe area

![](media/image5341.png) = number of spools installed in the pond

The fluid temperature Tfluid is computed as the average fluid temperatures of the inlet and the outlet node at a given temperature.

![](media/image5342.png)\


where

![](media/image5343.png) = inner pipe radius

![](media/image5344.png) l= length of one spool

![](media/image5345.png) = composite thermal resistance defined as

![](media/image5346.png) =Resistance due to fluid flow in the pipe +external pipe thermal resistance +fouling factor

### References

*Chiasson, A.D. 1999. Advances in modeling of groundsourceheat pump systems*. Master's thesis,Oklahoma State University, Stillwater, OK

*Duffie, J.A., and W.A. Beckman. 1991. Solar engineering of thermal processes,* 2d ed. New York: John Wiley & **Sons.

Hull, J.R., K.V. Liu, W.T. Sha, J. Kamal, and C.E. Nielsen. 1984. Dependence of ground heat losses upon solar pond size and perimeter insulation—Calculated and experimental results. *Solar Energy* 33(1): 25-33

Kishore, V.V.N., and V. Joshi. 1984. A practical collector efficiency equation for nonconvecting solar ponds. *Solar Energy* 33(5): 391-395.

Rees, S.J., J.D. Spitler and X. Xiao, X 2002. Transient Analysis of Snow-melting System Performance. *ASHRAE Transactions.* 108(2):406-423.

## Plant Loop Surface-Ground-To-Water Heat Exchanger

This model (Object: GroundHeatExchanger:Surface) is based on the QTF formulation of heat transfer through building elements with embedded heat sources/sinks. The model uses a heat exchanger analogy to relate the inlet fluid temperature to the net heat transfer rate and consequently outlet temperature. The model is entirely passive, i.e. it does not set any flow rates or incorporate any controls. In order to deal with the non-linear boundary conditions at the top surface due to the presence of ice/snow fluxes have to be calculated by the QTF model and temperature calculated from the surface heat balance. This requires some iteration. Note, top surface variables correspond to ‘outside' variables in standard CTF/QTF definition. Bottom surface variables correspond to ‘inside' variables.

For given current surface temperatures the terms of the QTF equations can be grouped into constant terms, and those depending on the current source flux. The surface heat balance may be given by the following equation (Strand,1997)

![](media/image5347.png)\


![](media/image5348.png)\


Where

T~s~    = temperature of the node where the heat source or sink is present

![](media/image5349.png) = Surface heat balance

![](media/image5350.png) = Heat flux

T   = Temperature

i    =  inside of the building element

o   = outside of the building element

t    = current time step

X Y F = Conduction transfer functions

The surface balance equation includes terms for incident solar energy, radiation heat transfer from internal sources, linearized radiation between surfaces using the mean radiation temperature concept and convection to the surrounding air.

The heat balance on the water loop is given by

![](media/image5351.png)\


Where

q = heat transferred between the water loop and the building elements.

![](media/image5352.png) = mass flow rate of water

![](media/image5353.png) = Inlet water temperature

![](media/image5354.png) = Outlet water temperature

From the second law of thermodynamics the maximum amount of heat transfer is

![](media/image5355.png)\


Where

T~s~ = temperature at the source location

The effectiveness of the heat exchanger is given by

![](media/image5356.png)\


Where NTU is defined by

![](media/image5357.png)\


![](media/image5358.png)\


h is the convection coefficient, D is the interior tube diameter and L is the total length of the tube.

The Colburn equation is used to define the Nusselt number Nu

![](media/image5359.png)\


Pr is the Prandtl number, Re is the Reynolds number and k is the thermal conductivity of the fluid

![](media/image5360.png)\


with ![](media/image5361.png)  being the absolute viscosity of water

## Plant Loop Fluid-to-Fluid Heat Exchanger

This component (Object: HeatExchanger:FluidToFluid) is a simple hydronic heat exchanger that can be used to couple two (hydronic) plant or condenser loops.  Sizing and nominal capacity calculations are discussed elsewhere in this document, see the section called Plant Heat Exchanger Sizing. This section first discusses the heat transfer modeling and the control issues.

Heat exchanger performance modeling uses classic effectiveness-NTU correlations. The heat exchanger model can be specified as one of seven types:  cross flow both fluid streams unmixed, cross flow both fluid streams mixed, cross flow maximum capacity flow mixed min), counter flow, parallel flow, or ideal. The model correlations determine a heat transfer effectiveness value, ![](media/image5362.png) , which is a function of heat exchanger UA, the mass flow rates through boths sides, and the specific heat of the fluids in the streams.  The effectiveness of an ideal heat exchanger is set to 1.0 and no correlation is needed.

Because the heat exchanger is intended to be generic, its two sides are distinguished by the nature of loop side being connected.  One side is called "Loop Supply Side" to indicate the heat exchanger is situated on the supply side of a loop. The other side is called "Loop Demand Side" to indicate it is on the demand side of a loop.  The heat exchanger is intended to act as a supply component for the loop connected to it as the "Loop Supply Side" and as a demand component for the loop connected to it as the "Loop Demand Side."  From the point of view of the heat exchanger component itself, the Loop Demand Side acts like a supply source/sink for the Loop Supply Side which acts like a demand to the component.

The mass flow rates and specific heat capacity are multiplied together to evaluate flow thermal capacity for each side and the minimum, maximum, and capacity ratio, ![](media/image5363.png) , are determined.

![](media/image5364.png)\


![](media/image5365.png)\


![](media/image5366.png)\


![](media/image5367.png)\


![](media/image5368.png)\


NTU, or Number of Transfer Units, is calculated using:

![](media/image5369.png)\


For a counter flow heat exchanger, effectiveness is calculated using:

![](media/image5370.png)\


For a parallel flow heat exchanger, effectiveness is calculated using:

![](media/image5371.png)\


For a cross flow heat exchanger with both streams unmixed, effectiveness is calculated using:

![](media/image5372.png)\


For a cross flow heat exchanger with both streams mixed, the effectiveness is calculated using:

![](media/image5373.png)\


For a cross flow heat exchanger with the stream with the higher capacity mixed and the stream with the lower capacity unmixed, the effectiveness is calculated using:

![](media/image5374.png)\


For a cross flow heat exchanger with the stream with higher capacity unmixed and the stream with lower capacity mixed, the effectiveness is calculated using:

![](media/image5375.png)\


Effectiveness values are always limited to be no higher than 1.0.  The program code protects from evaluating the exponential with operands that are too large or too small. Once the effectiveness value is determined, the heat transfer rate is calculated using:

![](media/image5376.png)\


Lastly, the fluid temperatures leaving the heat exchanger are calculated using:

![](media/image5377.png)\


![](media/image5378.png)\


The plant heat exchanger offers a number of control options for a wide variety of applications.  This section documents the control decision logic. The general goal of the control is to provide appropriate conditioning of the fluid at the Loop Supply Side connection.  The basic control action is to request the appropriate flow rates for each side of the heat exchanger.  For all of the control modes, there is also an availability schedule that provides a level of supervisory control to determine if the heat exchange is available to run at all.  If the device is scheduled off, then zero flow is requested for both connections. All the control modes can also use minimum and maximum temperature limits.  If the limits are used and either of the two inlet temperatures exceeds the limit, the zero flow is requested for both connections.

**UncontrolledOn.**  This control type is very simple.  If scheduled on it requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then the full design flow rate is requested for the Loop Demand Side connection.

**OperationSchemeModulated.**  This control type uses the plant operation schemes. Any of the various plant (or condenser) operation schemes (e.g. PlantEquipmentOperation:CoolingLoad or PlantEquipmentOperation:HeatingLoad ). When the heat exchanger is called, the operation scheme will pass a value for the load to be met.  If the absolute value of the load is less than a small tolerance (1 W), then zero flow is requested for both connections.  If load is significant and negative, then the heat exchanger will attempt to provide cooling.  If the load is significant and positive, then it will attempt to provide heating.  The two inlet fluid temperatures are compared and if their difference exceeds the minimum temperature difference and has the correct sign for heating or cooling as needed, then the heat exchanger is turned "on."  The model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then a flow rate for the Loop Demand Side is found to attempt to meet the load.  The load value is converted to a target temperature for the fluid leaving the Loop Supply Side connection.  The target temperature is then used with the numerical method Regula Falsi to solve for the flow rate.

**OperationSchemeOnOff**.  This control type uses the plant operation schemes. Any of the various plant (or condenser) operation schemes (e.g. PlantEquipmentOperation:CoolingLoad or PlantEquipmentOperation:HeatingLoad). When the heat exchanger is called, the operation scheme will pass a value for the load to be met.  If the absolute value of the load is less than a small tolerance (1 W), then zero flow is requested for both connections.  If load is significant and negative, then the heat exchanger will attempt to provide cooling.  If the load is significant and positive, then it will attempt to provide heating.  The two inlet fluid temperatures are compared and if their difference exceeds the minimum temperature difference and has the correct sign for heating or cooling as is needed, then the heat exchanger is turned "on."  The model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then the full design flow rate is requested for the Loop Demand Side connection.

**HeatingSetpointModulated**.  This control scheme uses a node setpoint. The two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for heating to be possible.  The temperature setpoint on the reference node is compared to the inlet temperature for the Loop Supply Side to see if heating is desired.  If heating is both desired and possible, then the heat exchanger is turned "on."  The model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then a flow rate for the Loop Demand Side is found to attempt to meet the setpoint.  The setpoint temperature is then used with the numerical method Regula Falsi to solve for the flow rate.

**HeatingSetpointOnOff**.  This control scheme uses a node setpoint. The two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for heating to be possible.  The temperature setpoint on the reference node is compared to the inlet temperature for the Loop Supply Side to see if heating is desired.  If heating is both desired and possible, then the heat exchanger is turned "on."  The model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then the full design flow rate is requested for the Loop Demand Side connection.

**CoolingSetpointModulated**.  This control scheme uses a node setpoint. The two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible.  The temperature setpoint on the reference node is compared to the inlet temperature for the Loop Supply Side to see if cooling is desired.  If cooling is both desired and possible, then the heat exchanger is turned "on."  The model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then a flow rate for the Loop Demand Side is found to attempt to meet the setpoint.  The setpoint temperature is then used with the numerical method Regula Falsi to solve for the flow rate.

**CoolingSetpointOnOff**.  This control scheme uses a node setpoint. The two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible.  The temperature setpoint on the reference node is compared to the inlet temperature for the Loop Supply Side to see if cooling is desired.  If cooling is both desired and possible, then the heat exchanger is turned "on."  The model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then the full design flow rate is requested for the Loop Demand Side connection.

**CoolingSetpointOnOffWithComponentOverride**.  This control scheme uses one of three different control signals, a node setpoint, outdoor air drybulb temperature, or outdoor air wetbulb temperature. The two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible.  The control signal is compared to the inlet temperature for the Loop Supply Side to see if cooling is desired.  If cooling is both desired and possible, then the heat exchanger is turned "on."  When the heat exchanger is on, the remote chiller component is turned off.  The model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then the full design flow rate is requested for the Loop Demand Side connection.

**CoolingDifferentialOnOff**.  This control scheme is based on a simple temperature difference. The two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible.  If cooling is possible then it is assumed to be desired and the heat exchanger is turned "on."  The model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then the full design flow rate is requested for the Loop Demand Side connection.

**DualDeadbandSetpointModulated**.  This control scheme uses dual setpoints on a node. The basic idea is that the heat exchanger will condition the Loop Supply Side connection to drive its temperature into the deadband whenever its inlet temperature is outside the deadband.  The model expects dual temperature setpoints to be placed on the setpoint, for example using the object SetpointManager:Scheduled:DualSetpoint.

The heat exchanger will operate to cool the Loop Supply Side fluid when the following three conditions are all met:

1)  the two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible,

2) the higher temperature setpoint on the reference node is compared to the inlet temperature for the Loop Supply Side to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible, and

3)  the higher temperature setpoint on the reference node is compared to the inlet temperature for the Loop Demand Side to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible.

The heat exchanger will operate to heat the Loop Supply Side fluid when the following three conditions are all met:

1)  the two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for heating to be possible,

2)  the lower temperature setpoint on the reference node is compared to the inlet temperature for the Loop Supply Side to see if their difference exceeds the minimum temperature difference and has the correct sign for heating to be possible, and

3) the lower temperature setpoint on the reference node is compared to the inlet temperature for the Loop Demand Side to see if their difference exceeds the minimum temperature difference and has the correct sign for heating to be possible.

When the heat exchanger is turned "on."  The model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then a flow rate for the Loop Demand Side is found to attempt to meet the setpoint.  The setpoint temperature is then used with the numerical method Regula Falsi to solve for the flow rate.

**DualDeadbandSetpointOnOff**. This control scheme uses dual setpoints on a node. The basic idea is that the heat exchanger will condition the Loop Supply Side connection to drive its temperature into the deadband whenever its inlet temperature is outside the deadband.  The model expects dual temperature setpoints to be placed on the setpoint, for example using the object SetpointManager:Scheduled:DualSetpoint.

The heat exchanger will operate to cool the Loop Supply Side fluid when the following three conditions are all met:

1)  the two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible,

2) the higher temperature setpoint on the reference node is compared to the inlet temperature for the Loop Supply Side to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible, and

3)  the higher temperature setpoint on the reference node is compared to the inlet temperature for the Loop Demand Side to see if their difference exceeds the minimum temperature difference and has the correct sign for cooling to be possible.

The heat exchanger will operate to heat the Loop Supply Side fluid when the following three conditions are all met:

1)  the two inlet fluid temperatures are compared to see if their difference exceeds the minimum temperature difference and has the correct sign for heating to be possible,

2)  the lower temperature setpoint on the reference node is compared to the inlet temperature for the Loop Supply Side to see if their difference exceeds the minimum temperature difference and has the correct sign for heating to be possible, and

3) the lower temperature setpoint on the reference node is compared to the inlet temperature for the Loop Demand Side to see if their difference exceeds the minimum temperature difference and has the correct sign for heating to be possible.

**When the heat exchanger is turned "on," the model requests the full design flow rate on the Loop Supply Side connection.  If the Loop Supply Side flow rate exceeds a small tolerance, then the full design flow rate is requested for the Loop Demand Side connection.**

## References

Strand, R.K. and C.O. Pedersen, 1997.  Implementation of a Radiant Heating and Cooling Model Into an Integrated Building Energy Analysis Program, ASHRAE Transactions v. 103, n. 1, pp 949-958, 1997.

Incropera, F.P. and D.P. DeWitt. 1981. Fundamentals of Heat Transfer. New York: John Wiley & Sons.

Mills, A.F.1999. Heat Transfer, Second Edition. Prentice Hall. New Jersey.

Presentation by Taylor Engineering.