# Air System Compound Component Groups

## Unitary Systems

### Overview

The input object AirLoopHVAC:UnitarySystem provide a "virtual" component that collect and control a set of components: fan, heating coil, cooling coil, and/or reheat coil. Reheat coil is modeled for controlling high zone humidity levels. The unit may be configured to have either a blow through or draw through fan. In a blow through configuration, fan is generally the fist component upstream of heating ro cooling coil. In a draw through fan configuration, fan is placed directly after the heating coil and before reheat coil.

![Schematic of the EnergyPlus Unitary System (Blow Through Configuration)](media/schematic-of-the-energyplus-unitary-system.jpg)


### Model Description

As described previously, the unitary system is a "virtual" component consisting of a fan, heating coil, cooling coil and reheat coil. The sole purpose of the unitary system model is to properly coordinate the operation of the various system components. The following sections describe the flow of information within the model, as well as the differences between cycling and continuous supply air fan operation.

### Controls

There are two types of control types allowed to be specified in the unitary system which are setpoint based and load based. Each control type is described in detail below.

Setpoint based control:

The unitary system calculates the current sensible load using the temperature of the inlet node and the System Node Setpoint Temp on the control node. If the control node is not the outlet node, the desired outlet node temperature is adjusted for the current temperature difference between the outlet node and the control node. Likewise, the current latent load is calculated using the humidity ratio of the inlet node and the System Node Humidity Ratio Max on the control node. The controls determine the required coil run-time fraction and dehumidification mode (if applicable) using the steps outlined below.

#### Step 1 – Meet Sensible Load Requirement

The controls first attempt to meet the sensible requirement. The specified coil model is called with a part-load ratio (PLR) of 1.0 to determine the full-load output of the coil. This is compared with the desired outlet node temperature and a sensible PLR is calculated. If the PLR is <1.0, a Regula-Falsi iteration routine is called to determine the coil run-time fraction which results in the desired outlet node temperature. For a variable-speed DX cooling coil, if the load is smaller than the sensible capacity at the lowest speed, the coil run-time fraction is determined in the same way as a single-speed DX cooling coil. Otherwise, its speed number and speed ratio between two neighboring speeds are selected to match the load.

#### Step 2 – Meet Latent Load Requirement (if activated)

If dehumidification controls are active, the leaving humidity ratio resulting from operation to meet the sensible load (Step 1 above) is compared with the desired outlet node humidity ratio. If the humidity requirement is already met, then no further control action is taken. If the humidity requirement has not been met, then the coil is re-simulated depending on the type of humidity control.

#### Step 2a – Humidity Control = MultiMode

If the humidity control type is MultiMode, then the coil's enhanced dehumidification mode is activated when the coil type is Coil:Cooling:DX:TwoStageWithHumidityControlMode and Step 1 above is repeated to meet the sensible load using the coil performance resulting from the enhanced dehumidificaiton mode. This is a semi-passive approach to dehumidification which may fall short or may exceed the dehumidification requirement.

#### Step 2b – Humidity Control = CoolReheat

If the humidity control type is CoolReheat, the coil is re-simulated to achieve the desired outlet node humidity ratio. This option is valid for all cooling coil types. When the coil type is Coil:Cooling:DX:TwoStageWithHumidityControlMode, only the cooling performance mode is used for this step and enhanced dehumidification mode is not activated.

Load based control:

While the unitary system may be configured to serve multiple zones, system operation is controlled by a thermostat located in a single "control" zone. One of the key parameters for the unitary system component is the fraction of the total system air flow that goes through the control zone. This fraction is calculated as the ratio of the maximum air mass flow rate for the air loop's supply inlet node for the control zone (e.g., AirTerminal:SingleDuct:Uncontrolled, field = Maximum Air Flow Rate, converted to mass flow) to the sum of the maximum air mass flow rates for the air loop's supply inlet nodes for all zones served by this air loop. The unitary system module scales the calculated load for the control zone upward based on this fraction to determine the total load to be met by the unitary system. The module then proceeds to calculate the required part-load ratio for the system coil and the supply air fan to meet this total load. The heating or cooling capacity delivered by the unitary system is distributed to all of the zones served by this system via the terminal units that supply air to each zone. The supply air fraction that goes though the control zone is calculated as follows:

![](media/image4660.png)\


where:

![](media/image4661.png) =maximum air mass flow rate for the air loop's supply inlet node (terminal unit) for the control zone (kg/s)

![](media/image4662.png) =maximum air mass flow rate for the air loop's supply inlet node for the jth zone (kg/s)

![](media/image4663.png) =number of zones, or number of air loop supply air inlet nodes for all zones served by the air loop (-)

The unitary system component is able to model supply air fan operation in two modes: cycling fan – cycling coil (i.e., AUTO fan) and continuous fan – cycling coil (i.e., fan ON). Fan:OnOff must be used to model AUTO fan, while Fan:OnOff or Fan:ConstantVolume can be used to model fan ON. The fan operation mode is specified using a supply air fan operating mode schedule where schedule values of 0 denote cycling fan operation and schedule values other than 0 (a 1 is usually used) denote continuous fan operation. Using this schedule, the unitary system fan may be cycled with cooling or heating coil operation or operated continuously based on time of day (e.g., cycling fan operation at night and continuous fan operation during the daytime). If the fan operating mode schedule name field is left blank in the unitary system object, the unitary system assumes cycling or AUTO fan mode operation throughout the simulation.

The unitary system operates based on the user-specified (or autosized) design supply air flow rate(s). The ‘design' supply air mass flow rate may be different for cooling, heating, and when no cooling or heating is required and the fan operates continuously based on user-specified inputs.

**Cooling Operation**

If EnergyPlus determines that the unitary system must supply cooling to the control zone to meet the zone air temperature setpoint, then the model computes the total sensible cooling load to be met by the unitary system based on the control zone sensible cooling load and the fraction of the unitary system air flow that goes through the control zone.

![](media/image4664.png)\


If the supply air fan operating mode schedule requests cycling fan operation, the model first checks for the presence of an ecomomizer in the outside air system serving the unitary system's air loop (Ref. AirLoopHVAC:OutdoorAirSystem). If an outside air system is not present or if an air-side economizer is not used, the unitary system's compressor is used to meet the unitary system cooling load. If an air-side economizer is used and is active (i.e., economizer controls indicate that conditions are favorable to increase the outside air flow rate), the unitary system will try to meet the cooling load by operating only the supply air fan. If the fan is able to satisfy the unitary system cooling load, the compressor remains off for the entire simulation time step. If the operation of the fan alone is unable to meet the entire cooling load, then the compressor is enabled and additional calculations are performed to determine the compressor's part-load ratio.

The model then calculates the unitary system's sensible cooling energy rate delivered to the zones being served when the system runs at full-load conditions and when the cooling coil is OFF. If the supply air fan cycles with the compressor, then the sensible cooling energy rate is zero when the cooling coil is OFF. However if the fan is configured to run continuously regardless of coil operation, then the sensible cooling energy rate will probably not be zero when the cooling coil is OFF. Calculating the sensible cooling energy rate involves modeling the supply air fan (and associated fan heat), the cooling coil, and the heating and reheat coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node). For each of these cases (full load and cooling coil OFF), the sensible cooling energy rate delivered by the unitary system is calculated as follows:

![](media/image4665.png)\


![](media/image4666.png)\


where:

*Mass Flow Rate~full load~* = air mass flow rate through unitary system at full-load conditions, kg/s

*h~out, full load~* = enthalpy of air exiting the unitary system at full-load conditions, J/kg

*h~control zone~*~~= enthalpy of air in the control zone (where thermostat is located), J/kg

*HR~min~       =* enthalpies evaluated at a constant humidity ratio, the minimum humidity ratio

of the unitary system exiting air or the air in the control zone

*Mass Flow Rate~coil off~* = air mass flow rate through the unitary system with the cooling coil OFF, kg/s

*h~out, coil off~*  = enthalpy of air exiting the unitary system with the cooling coil OFF, J/kg

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4667.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the heating coil OFF conditions

![](media/image4668.png)\


With the calculated sensible cooling energy rates and the total sensible cooling load to be met by the system, the part-load ratio for the unitary system is estimated.

![](media/image4669.png)\


Since the part-load performance of the cooling coil is frequently non-linear, and the supply air fan heat varies based on cooling coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the cooling coil compressor and fan are determined through iterative calculations (successive modeling of the cooling coil and fan) until the unitary system's cooling output matches the cooling load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the unitary system's cooling output divided by the load to be met.

![](media/image4670.png)\


where:

![](media/image4671.png) = Unitary system delivered sensible capacity (W)

If the unitary system has been specified with cycling fan/cycling coil (AUTO fan), then the unitary system's operating supply air mass flow rate is multiplied by PartLoadRatio to determine the average air mass flow rate for the system simulation time step. In this case, the air conditions at nodes downstream of the cooling coil represent the full-load (steady-state) values when the coil is operating.

If the fan operates continuously (i.e., when the supply air fan operating mode schedule values are NOT equal to 0), the operating air mass flow rate through the unitary system is calculated as the average of the user-specified air flow rate when the cooling coil is ON and the user-specified air flow rate when the cooling coil is OFF (user-specified supply air volumetric flow rates converted to dry air mass flow rates).

![](media/image4672.png)\


where:

![](media/image4673.png)  = air mass flow rate through unitary system when the cooling coil is ON (kg/s)

![](media/image4674.png)  = air mass flow rate through unitary system when no cooling or heating is needed (kg/s)

In this case, the air conditions at nodes downstream of the cooling coil are calculated as the average conditions over the simulation time step (i.e., the weighted average of full-load conditions when the coil is operating and inlet air conditions when the coil is OFF).

**Cooling Operation (multi or variable speed coils)**

After the unitary system cooling load is determined as described in Eq.  above, the multi or variable speed cooling coil models calculations are described in this section.

The model calculates the unitary system's sensible cooling energy rate delivered to the zones being served when the system runs at full-load conditions at the highest speed and when the DX cooling coil is OFF. If the supply air fan cycles with the compressor, then the sensible cooling energy rate is zero when the cooling coil is OFF. However if the fan is scheduled to run continuously regardless of coil operation, then the sensible cooling energy rate will not be zero when the cooling coil is OFF. Calculating the sensible cooling energy rate involves modeling the supply air fan (and associated fan heat) and the multi/variable speed DX cooling coil. The multi/variable speed DX heating coil and the supplemental heating coil are also modeled, but only to pass the air properties and mass flow rate from their inlet nodes to their outlet nodes. For each of these cases (full load at highest cooling speed and DX cooling coil OFF), the sensible cooling energy rate delivered by the unitary system is calculated as follows:

![](media/image4675.png) ![](media/image4676.png)

where:

*![](media/image4677.png)*  *= air mass flow rate through unitary system at the highest cooling speed [kg/s]*

*h~out, full load~*   = enthalpy of air exiting the unitary system at full-load conditions [J/kg]

*h~control  zone~*   = enthalpy of air leaving the control zone (where thermostat is located) [J/kg]

*HR~min~ =* the minimum humidity ratio of the unitary system exiting air or the air leaving the control zone [kg/kg]

![](media/image4678.png)  = air mass flow rate through the unitary system with the cooling coil OFF [kg/s]

*h~out,coil  off~*    = enthalpy of air exiting the unitary system with the cooling coil OFF [J/kg]

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4679.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the cooling coil OFF conditions

![](media/image4680.png)\


If the unitary system's sensible cooling rate at the highest speed (full load, no cycling) is insufficient to meet the entire cooling load, the controlled zone conditions will not be met. The reported cycling rate and speed ratio are 1, and the speed number is set to the highest index number. If the total sensible cooling load to be met by the system is less than the sensible cooling rate at the highest speed, then the following steps are performed.

Calculate the sensible cooling energy rate at Speed 1

![](media/image4681.png)\


where

*![](media/image4682.png)*  *= air mass flow rate through unitary system at Speed 1 [kg/s]*

Δ~sen,~ ~Speed1~ **= Sensible load difference between the system output node and the zone inlet node at full-load conditions at Speed 1

![](media/image4683.png)\


If the sensible cooling energy rate delivered by the unitary system at Speed 1 is greater or equal to the sensible load, the cycling ratio (part-load ratio) for the unitary system is estimated.

![](media/image4684.png)\


where

AddedFanHeat= generated supply air fan heat, which is a function of part load ratio and as internal component cooling load [W].

AddedFanHeat~Speed1~= generated supply air fan heat at Speed 1 (part load ratio=1) [W].

Since the part-load performance of the DX cooling coil is frequently non-linear,and the supply air fan heat varies based on cooling coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the cooling coil compressor and fan are determined through iterative calculations (successive modeling of the cooling coil and fan) until the unitary system's cooling output matches the cooling load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the unitary system's cooling output divided by the load to be met.

![](media/image4685.png)\


where:

Unitary systemOutput~Cycling~ = unitary system delivered sensible capacity for Speed 1 operating at a specific cycling ratio (W)

![](media/image4686.png)\


where

![](media/image4687.png) = average air mass flow rate defined in the next section [kg/s]

h~out,~    = enthalpy of air exiting the unitary system at part load conditions [J/kg]

Δ~cycling~ = average sensible load difference between the system output node and the zone inlet node

![](media/image4688.png)\


![](media/image4689.png) = Air mass flow rate in the supply inlet node in the controlled zone [kg/s]

For this case where speed 1 operation was able to meet the required cooling load, the speed ratio is set to zero and speed number is equal to 1.

If the unitary system's cooling output at full load for Speed 1 is insufficient to meet the entire cooling load, the Cycling ratio is set equal to 1.0 (compressor and fan are not cycling). Then the cooling speed is increased and the delivered sensible capacity is calculated. If the full load sensible capacity at Speed n is greater than or equal to the sensible load, the speed ratio for the unitary system is estimated:

![](media/image4690.png)\


Although a linear relationship is assumed by applying the speed ratio to obtain the effective capacity and mass flow rate between speed n and n-1, the outlet air node conditions are dependent on the combined outputs and may not be linear. In addition, the supply air fan heat varies with the speed ratio due to different supply mass flow rates between speed n and n-1 . Therefore, the final speed ratio for the cooling coil compressor and fan are determined through iterative calculations (successive modeling of the cooling coil and fan) until the unitary system's cooling output matches the cooling load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the unitary system's cooling output divided by the load to be met.

![](media/image4691.png)\


where:

Unitary systemOutput~Speed,n~= unitary system delivered sensible capacity between two consecutive speeds at a specific speed ratio (W)

![](media/image4692.png)\


Where

AddedFanHeat~SpeedRatio~= generated supply air fan heat at a specific speed ratio [W]

In this case, the reported cycling ratio is 1 and speed number is equal to n.

#### Air Mass Flow Rate Calculation

Speed 1 operation

If the unitary system has been specified with cycling fan/cycling coil (AUTO fan), then the unitary system's operating supply air mass flow rate is determined by the cycling ratio (PartLoadRatio) for Speed 1. The supply air mass flow rate is multiplied by the cycling ratio to determine the average air mass flow rate for the system simulation time step. The air conditions at nodes downstream of the cooling coils represent the full-load (steady-state) values when the coil is operating.

![](media/image4693.png)\


If the fan operates continuously (i.e., when the supply air fan operating mode schedule values are NOT equal to 0), the operating air mass flow rate through the unitary system is calculated as the average of the user-specified air flow rate when the unitary system cooling coil is ON at Speed 1 and the user-specified air flow rate when the unitary system cooling coil is OFF (user-specified supply air volumetric flow rates converted to dry air mass flow rates).

![](media/image4694.png)\


where:

![](media/image4695.png)  = average air mass flow rate through unitary system [kg/s]

![](media/image4696.png) = air mass flow rate through unitary system when cooling coil is ON at Speed 1 [kg/s]

![](media/image4697.png)  = air mass flow rate through unitary system when no heating or cooling is needed [kg/s]

In this case, the air conditions at nodes downstream of the cooling coils are calculated as the average conditions over the simulation time step (i.e., the weighted average of full-load conditions when the coil is operating and inlet air conditions when the coil is OFF).

#### Higher Speed Operation

When the unitary system operates at higher speeds to meet the required cooling load, the supply air mass flow rate is linearly interpolated between two consecutive speeds:

![](media/image4698.png)\


where:

![](media/image4699.png) = average air mass flow rate through the unitary system for the time step [kg/s]

![](media/image4700.png) = air mass flow rate through unitary system when cooling coil is ON at Speed n [kg/s]

![](media/image4701.png) = air mass flow rate through unitary system when cooling coil is ON at Speed n-1 [kg/s]

For this case of higher speed operation, the air conditions at nodes downstream of the cooling coils are determined by the delivered cooling capacity and supply air mass flow rates between two consecutive speeds.

Although the above sections present the capacity and air mass flow rate calculation separately, they are dependent and change every iteration until convergence is reached for the time step being simulated.

**Heating Operation**

Calculations for heating operation are similar to those for cooling operation in most respects. However, due to the inclusion of a supplemental heating coil, additional calculations are necessary to properly meet the total heating load for the zones being served.

If EnergyPlus determines that the unitary system must supply heating to the control zone to meet the zone air temperature setpoint, then the unitary system model computes the total sensible heating load to be delivered to the zones being served based on the control zone sensible heating load and the control zone airflow fraction.

![](media/image4702.png)\


The model then calculates the unitary system's sensible heating energy rate delivered to the zones being served when the system runs at full-load conditions and when the heating coil is OFF (without supplemental heater operation in either case). If the supply air fan cycles with the compressor, then the sensible heating energy rate is zero when the compressor is OFF. However if the fan is scheduled to run continuously regardless of coil operation, then the sensible heating energy rate will not be zero when the compressor is OFF. Calculating the sensible heating energy rate involves modeling the supply air fan (and associated fan heat), the  cooling coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node), the  heating coil, and the supplemental heating coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node). For each of these cases (full load and  heating coil OFF, without supplemental heater operation in either case), the sensible heating energy rate delivered by the unitary system is calculated as follows:

![](media/image4703.png)\


![](media/image4704.png)\


where:

*Mass Flow Rate ~full load~*  = air mass flow rate through unitary system at full-load conditions, kg/s

*h~out, full load~*  = enthalpy of air exiting the unitary system at full-load conditions, J/kg

*h~control  zone~*  = enthalpy of air leaving the control zone (where thermostat is located), J/kg

*HR~min~ =* enthalpies evaluated at a constant humidity ratio, the minimum humidity ratio of the unitary system exiting air or the air leaving the control zone

*Mass Flow Rate ~coil  off~*  = air mass flow rate through the unitary system with the heating coil OFF, kg/s

*h~out, coil  off~*  = enthalpy of air exiting the unitary system with the heating coil OFF, J/kg

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4705.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the heating coil OFF conditions

![](media/image4706.png)\


With the calculated sensible heating energy rates and the total sensible heating load to be met by the system, the part-load ratio for the unitary system is estimated.

![](media/image4707.png)\


Since the part-load performance of the  heating coil is frequently non-linear, and the supply air fan heat varies based on heating coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the heating coil compressor and fan are determined through iterative calculations (successive modeling of the heating coil and fan) until the unitary system's heating output matches the heating load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the unitary system's heating output divided by the load to be met.

![](media/image4708.png)\


where:

![](media/image4709.png) = Unitary system delivered sensible capacity (W)

If the unitary system's  heating coil output at full load is insufficient to meet the entire heating load, PartLoadRatio is set equal to 1.0 (compressor and fan are not cycling) and the remaining heating load is passed to the supplemental heating coil. If the unitary system model determines that the outdoor air temperature is below the minimum outdoor air temperature for compressor operation, the compressor is turned off and the entire heating load is passed to the supplemental gas or electric heating coil. The unitary system exiting air conditions and energy consumption are calculated and reported by the individual component models (fan,  heating coil, and supplemental gas or electric heating coil).

If the unitary system has been specified with cycling fan/cycling coil (AUTO fan), then the unitary system's operating supply air mass flow rate is multiplied by PartLoadRatio to determine the average air mass flow rate for the system simulation time step. The air conditions at nodes downstream of the heating coils represent the full-load (steady-state) values when the coils are operating. If the fan operates continuously (i.e., when the supply air fan operating mode schedule values are NOT equal to 0), the operating air mass flow rate through the unitary system is calculated as the average of the user-specified air flow rate when the unitary system heating coil is ON and the user-specified air flow rate when the unitary system heating coil is OFF (user-specified supply air volumetric flow rates converted to dry air mass flow rates).

![](media/image4710.png)\


where:

![](media/image4711.png)  = air mass flow rate through unitary system when the heating coil is ON (kg/s)

![](media/image4712.png)  = air mass flow rate through unitary system when no heating or cooling is needed (kg/s)

In this case, the air conditions at nodes downstream of the heating coils are calculated as the average conditions over the simulation time step (i.e., the weighted average of full-load conditions when the coils are operating and inlet air conditions when the coils are OFF).

### Heating Operation (multi or variable speed coils )

After the unitary system heating load is determined as described in Eq.  above, the multi or variable speed heating coil models calculation are described in this section.

The model calculates the unitary system's sensible heating energy rate delivered to the zones being served when the system runs at full-load conditions at the highest speed and when the DX heating coil is OFF (without supplemental heater operation in either case). If the supply air fan cycles with the compressor, then the sensible heating energy rate is zero when the compressor is OFF. However if the fan is scheduled to run continuously regardless of coil operation, then the sensible heating energy rate will not be zero when the compressor is OFF. Calculating the sensible heating energy rate involves modeling the supply air fan (and associated fan heat), the DX cooling coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node), the DX heating coil, and the supplemental heating coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node). For each of these cases (full load and DX heating coil OFF, without supplemental heater operation in either case), the sensible heating energy rate delivered by the unitary system is calculated as follows:

![](media/image4713.png)\


![](media/image4714.png)\


where:

*![](media/image4715.png)*  *= air mass flow rate through unitary system at the highest heating speed [kg/s]*

*h~out, full load~*   = enthalpy of air exiting the unitary system at full-load conditions [J/kg]

*h~control  zone~*   = enthalpy of air leaving the control zone (where thermostat is located) [J/kg]

*HR~min~ =* enthalpies evaluated at a constant humidity ratio, the minimum humidity ratio of the unitary system exiting air or the air leaving the control zone

![](media/image4716.png) = air mass flow rate through the unitary system with the heating coil OFF [kg/s]

*h~out,coil  off~*    = enthalpy of air exiting the unitary system with the heating coil OFF [J/kg]

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4717.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the heating coil OFF conditions

![](media/image4718.png)\


If the unitary system's DX heating coil output full load at the highest speed is insufficient to meet the entire heating load, the remaining heating load is passed to the supplemental heating coil. If the unitary system model determines that the outdoor air temperature is below the minimum outdoor air temperature for compressor operation (specified by the user), the compressor is turned off and the entire heating load is passed to the supplemental gas or electric heating coil. The unitary system exiting air conditions and energy consumption are calculated and reported by the individual component models (fan, DX heating coil, and supplemental gas or electric heating coil).

If the total heating load to be met by the system is less than the sensible heating rate at the highest speed, then the following steps are performed.

Calculate the sensible heating energy rate at Speed 1

![](media/image4719.png)\


where:

*![](media/image4720.png)*  *= air mass flow rate through unitary system at Speed 1 [kg/s]*

Δ~sen,~ ~Speed1~ **= Sensible load difference between the system output node and the zone inlet node at full-load conditions at Speed 1

![](media/image4721.png)\


If the sensible heating energy rate delivered by the unitary system at Speed 1 is greater or equal to the sensible load, the cycling ratio (part-load ratio) for the unitary system is estimated.

![](media/image4722.png)\


where

AddedFanHeat= generated supply air fan heat, which is a function of part load ratio and as internal component heating load [W].

AddedFanHeat~Speed1~= generated supply air fan heat at Speed 1 (part load ratio=1) [W].

Since the part-load performance of the DX heating coil is frequently non-linear, and the supply air fan heat varies based on heating coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the heating coil compressor and fan are determined through iterative calculations (successive modeling of the heating coil and fan) until the unitary system's heating output matches the heating load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the unitary system's heating output divided by the load to be met.

![](media/image4723.png)\


where:

Unitary systemOutput~Cycling~= unitary system delivered sensible capacity for Speed 1 operating at a specific cycling ratio (W)

![](media/image4724.png)\


where

![](media/image4725.png)  = average air mass flow rate defined in the next section [kg/s]

h~out,~    = enthalpy of air exiting the unitary system at part load conditions [J/kg]

Δ~cycling~ = average sensible load difference between the system output node and the zone inlet node

![](media/image4726.png)\


![](media/image4727.png) = Air mass flow rate in the supply inlet node in the controlled zone [kg/s]

For this case where speed 1 operation was able to meet the required heating load, the speed ratio is set to zero and speed number is equal to 1.

If the unitary system's heating output at full load for Speed 1 is insufficient to meet the entire heatling load, the Cycling ratio (PartLoadRatio) is set equal to 1.0 (compressor and fan are not cycling). Then the heating speed is increased and the delivered sensible capacity is calculated. If the full load sensible capacity at Speed n is greater than or equal to the sensible load, the speed ratio for the unitary system is estimated:

![](media/image4728.png)\


Although a linear relationship is assumed by applying the speed ratio to obtain the effective capacity and air mass flow rate between speed n and n-1, the outlet node conditions are dependent on the combined outputs and may not be linear. In addition, the supply air fan heat varies based on heating coil operation for the case of cycling fan/cycling coil (AUTO fan). Therefore, the final speed ratio for the heating coil compressor and fan are determined through iterative calculations (successive modeling of the heating coil and fan) until the unitary system's heating output matches the heating load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the unitary system's heating output divided by the load to be met.

![](media/image4729.png)\


where:

UnitarySystemOutput~Speed~~Ratio~= unitary system delivered sensible capacity between two consecutive speeds at a specific ratio [W]

![](media/image4730.png)\


Where

AddedFanHeat~SpeedRatio~= generated supply air fan heat at a specific speed ratio [W]

In this case, the reported cycling ratio is 1 and speed number is equal to n.

#### Air Mass Flow Rate Calculation

The air mass flow rate calculations during heating operation are the same as those described above for cooling operation for multi/variable speed.

### High Humidity Control

The specific configuration of the unitary system with supplemental heating coil is shown above (see Figure 227). This figure shows the fan placement when a blow through fan is specified. If a draw through fan is specified, the fan is located between the heating coil and the supplemental heating coil. The system is controlled to keep the high relative humidity in the control zone from exceeding the setpoint specified in the object ZoneControl:Humidistat. This option is available when the supply air fan operates continuously (i.e., the supply air fan operating mode schedule values are never equal to 0) or the supply air fan cycles with the compressor. In addition, when high humidity control is specified and the compressor operates, the unitary system operates at the cooling air flow rate when a zone heating load is present as determined by the zone thermostat. High humidity control is specified as either None, MultiMode, or CoolReheat in the Dehumidification Control Type input field. MultiMode is specified when a heat exchanger is used to improve the dehumidification performance of the cooling coil. The heat exchanger will be activated when the sensible part-load ratio is insufficient to meet the zone latent load. CoolReheat is specified when a cooling coil is used to over-cool the supply air stream in order to meet the zone latent load. In this case, a supplemental heating coil will ensure the zone temperature does not fall below the zone heating temperature set point. When a heat exchanger is used in conjunction with a cooling coil and CoolReheat is specified as the Dehumidification Control Type, the heat exchanger is "locked on" to meet either the sensible or latent cooling load. If the dehumidification control type is selected as None and a heat exchanger assisted cooling coil is used, the heat exchanger is "locked on" and the air conditioner runs only to meet the sensible cooling load. A supplemental heating coil is required for all dehumidification control types.

The model first calculates the *PartLoadRatio* required to meet the sensible cooling load.  The unitary system's sensible cooling load is determined from the control zone sensible cooling load to the cooling setpoint and the control zone air flow fraction to maintain the dry-bulb temperature setpoint in the control zone.:

![](media/image4731.png)\


The unitary system's sensible cooling load to be met and the full load cooling output are used to calculate the sensible the part-load ratio iteratively based on user specified convergence criterion.

![](media/image4732.png)\


hen the unitary system's sensible cooling capacity meets the system sensible cooling load at a given sensible part load ratio, then the Unitary system meets the controlled zone cooling setpoint temperature. If a moisture (latent) load exists because the control zone humidity has exceeded the setpoint, the total moisture load to be met by the unitary systems (Unitary systemMoistureLoad) is calculated based on the control zone moisture load and the control zone air flow fraction.

![](media/image4733.png)\


Then the *LatentPartLoadRatio* required to meet the high humidity setpoint is calculated as follows:

![](media/image4734.png) The model uses the greater of the two part-load ratios, *PartLoadRatio* or *LatentPartLoadRatio*, to determine the operating part-load ratio of the Unitary system's DX cooling coil.

![](media/image4735.png)\


As previously described, iterations are performed to converge on the solution within the convergence tolerance.

Where,

![](media/image4736.png) = the control zone sensible cooling load to the cooling setpoint, (W).

![](media/image4737.png) = the control zone moisture load to the dehumidifying relative humidity setpoint, (W).

![](media/image4738.png)  = the supply air fraction that goes though the control zone, (-).

![](media/image4739.png) = the Unitary system's latent cooling energy rate at full-load conditions, W

![](media/image4740.png)  = the Unitary system's latent cooling energy rate with cooling coil OFF, W

![](media/image4741.png) = the unitary system's part-load-ratio required to meet system sensible load, (-).

![](media/image4742.png) = the unitary system's part-load-ratio required to meet system moisture load, (-).

*![](media/image4743.png)* *=* the minimum part-load ratio, which is usually 0.0. For the case when the latent capacity degradation model is used (Ref: DX Cooling Coil Model), this value is the minimum part-load ratio at which the cooling coil will dehumidify the air. **

When the predicted zone air temperature is above the heating setpoint and if there is a dehumidification load, the supplemental heating coil load is required to offset the excess cooling as shown in Figure 228. If the model determines that the LatentPartLoadRatio is to be used as the operating part-load ratio of the unitary system's cooling coil, the supplemental heating coil is used to offset the excess sensible capacity provided by the unitary system cooling coil. The model first checks the sensible load that exists for the current simulation time step (predicted zone temperature with no HVAC operation compared to the thermostat setpoint temperatures). If a sensible cooling load or no sensible cooling or heating load exists, the model calculates the difference between the sensible heating load required to reach or maintain the heating dry-bulb temperature setpoint and the actual sensible cooling energy rate delivered by the unit (with LatentPartLoadRatio). In this case, the supplemental heating coil is used to offset the excess sensible cooling energy provided by the cooling coil (if any) that could have caused an overshoot of the heating dry-bulb temperature setpoint. Note that when a humidistat is used and high humidity control is required, the zone dry-bulb temperature will typically move toward the heating temperature setpoint when a high moisture (latent) load exists.

![Supplemental heating coil load when predicted zone air temperature is above the heating Setpoint](media/supplemental-heating-coil-load-when-predicted.jpeg)


If a heating load exists (Figure 229), the supplemental heating coil is used to meet the heating coil load and at the same time offset the entire sensible cooling energy rate of the cooling coil (to meet the humidistat setpoint). Note that when a heating load exists and high humidity control is required, the unitary system operates at the user-specified cooling air flow rate for the entire simulation time step. As with the fan, and cooling coil, report variables associated with supplemental heating coil performance (e.g., heating coil energy, heating coil rate, heating coil gas or electric energy, heating coil runtime fraction, etc.) are managed in the supplemental (heating) coil object.

![Supplemental heating coil load when predicted zone air temperature is below the heating setpoint](media/supplemental-heating-coil-load-when-predicted-001.jpeg)


### Waste Heat Calculation

Waste heat calculations are done when the multi speed cooling and heating coils are specified in the unitary system and the heat recovery is active (the value of the Design Heat Recovery Water Flow Rate field is greater than 0), the outlet node temperature of heat recovery is calculated based on the recoverable waste heat generated by the child objects Coil:Cooling:DX:MultiSpeed and Coil:Heating:DX:MultiSpeed:

![](media/image4746.png)\


where

T~outlet~= outlet node temperature of heat recovery, C

T~in~~let~= inlet node temperature of heat recovery, C

Q~WasteHeat~= recoverable waste heat generated by its child objects, W

C~p~= inlet node temperature of heat recovery, C

![](media/image4747.png) = mass flow rate of heat recovery, kg/s

If the outlet node temperature is above the value of the Maximum Temp for Heat Recovery field, the outlet node temperature is reset to the value of Maximum Temp for Heat Recovery.

## Forced-Air Furnace and Central Air Conditioning

### Overview

The input objects AirLoopHVAC:Unitary:Furnace:HeatOnly and AirLoopHVAC:Unitary:Furnace:HeatCool provide a "virtual" component that collect and control a set of components:  an on/off or constant volume fan component and a gas or electric heating coil component. If the HeatCool version is selected, then a DX cooling coil is also modeled as part of the system as shown in Figure 221 below. For the HeatCool version, an optional reheat coil may also be modeled for controlling high zone humidity levels and the furnace's configuration when specifying this option is shown in Figure 222 below. The unit may be configured to have either a blow through or draw through fan. If a blow through fan configuration is specified, the furnace fan is placed before the heating coil for the HeatOnly version, or before the cooling coil for the HeatCool version as shown in the figure below. If a draw through fan configuration is specified, the fan is placed directly after the heating coil.

> Note: the coil order shown here has been revised from previous versions of Energyplus to configure the cooling coil upstream of the heating coil. This configuration provides uniformity with all unitary equipment. However, for unitary HeatCool systems that do not use a reheat coil, the heating coil can also be placed upstream of the cooling coil. This optional coil placement is retained to allow compatibility with previous versions of Energyplus. For input files developed using previous versions of Energyplus, it is recommended that the coil order be revised according to the figure below.

![Schematic of the EnergyPlus Furnace (Blow Through Configuration)](media/schematic-of-the-energyplus-furnace-blow.jpeg)


While the furnace may be configured to serve multiple zones, system operation is controlled by a thermostat located in a single "control" zone. One of the key parameters for the furnace component is the fraction of the total system air flow that goes through the control zone. This fraction is calculated as the ratio of the maximum air mass flow rate for the air loop's supply inlet node for the control zone (e.g., AirTerminal:SingleDuct:Uncontrolled, field = Maximum Air Flow Rate, converted to mass flow) to the sum of the maximum air mass flow rates for the air loop's supply inlet nodes for all zones served by this air loop. The furnace module scales the calculated load for the control zone upward based on this fraction to determine the total load to be met by the furnace. The module then proceeds to calculate the required part-load ratio for the system coil and the supply air fan to meet this total load. The heating or cooling capacity delivered by the furnace is distributed to all of the zones served by this system via the terminal units that supply air to each zone. The supply air fraction that goes though the control zone is calculated as follows:

![](media/image4660.png)\


where:

![](media/image4661.png) =maximum air mass flow rate for the air loop's supply inlet node (terminal unit) for the control zone (kg/s)

![](media/image4662.png) =maximum air mass flow rate for the air loop's supply inlet node for the jth zone (kg/s)

![](media/image4663.png) =number of zones, or number of air loop supply air inlet nodes for all zones served by the air loop (-)

The furnace component is able to model supply air fan operation in two modes: cycling fan – cycling coil (i.e., AUTO fan) and continuous fan – cycling coil (i.e., fan ON). Fan:OnOff must be used to model AUTO fan, while Fan:OnOff or Fan:ConstantVolume can be used to model fan ON. The fan operation mode is specified using a supply air fan operating mode schedule where schedule values of 0 denote cycling fan operation and schedule values other than 0 (a 1 is usually used) denote continuous fan operation. Using this schedule, the furnace fan may be cycled with cooling or heating coil operation or operated continuously based on time of day (e.g., cycling fan operation at night and continuous fan operation during the daytime). If the fan operating mode schedule name field is left blank in the furnace object, the furnace assumes cycling or AUTO fan mode operation throughout the simulation.

The only output variables reported by the furnace object are the fan part-load ratio and the compressor part-load ratio (HeatCool only). The fan part-load ratio is defined as the actual air mass flow rate through the system for the time step divided by the design supply air mass flow rate specified for the furnace (![](media/image4749.png) )..The furnace operates based on the user-specified (or autosized) design supply air flow rate(s). The ‘design' supply air mass flow rate may be different for cooling, heating, and when no cooling or heating is required and the fan operates continuously based on user-specified inputs (HeatCool only). For the HeatCool version, If alternate air flow rates are specified for cooling, heating, and when no cooling or heating is required, the design supply air mass flow rate is the maximum of these specified values. Also for the HeatCool version, the compressor part-load ratio is reported as the ratio of the actual cooling load to the full-load sensible capacity (see Eqn. ). Reporting of other variables of interest for the furnace (heating rate, cooling rate, energy consumption, etc.) is done by the individual system components (fan, heating coil and DX cooling coil).

### Model Description

As described previously, the furnace is a "virtual" component consisting of a fan, heating coil and, for the HeatCool version, a cooling coil with an optional reheat coil. The sole purpose of the furnace model is to properly coordinate the operation of the various system components. The following sections describe the flow of information within the model for both the HeatOnly and HeatCool configurations, as well as the differences between cycling and continuous supply air fan operation. The last section describes the optional control of high zone humidity with a reheat coil for the HeatCool configuration.

### HeatOnly Configuration

The HeatOnly configuration consists of an on/off or constant volume fan and an electric or gas heating coil. When the model is first called during an EnergyPlus simulation, all of the input data specified for each furnace in the input data file are read into data structures for use throughout the remainder of the simulation.

For each simulation time step when the performance of a heat-only furnace is being modeled, the first step is to retrieve the heating load required to meet the thermostat setpoint for the "control" zone (see Figure 221. Schematic of the EnergyPlus Furnace). See the section "Summary of Predictor-Corrector Procedure" elsewhere in this document for more details regarding load calculations. Since the furnace may be specified to serve several zones but controlled based on the load calculated for the "control" zone, the total heating load to be met by the furnace is determined from the following equation:

![](media/image4750.png)\


The model then calculates the furnace's sensible heating energy rate delivered to the zones being served when the system runs at full-load conditions and when the heating coil is OFF. If the supply air fan cycles on and off with the heater, then the sensible heating energy rate is zero when the heating coil is OFF. However if the fan is scheduled to run continuously regardless of coil operation, then the sensible heating energy rate will not be zero when the heating coil is OFF. Calculating the sensible heating energy rate involves modeling the supply air fan (and associated fan heat) and the heating coil. For each of these cases (full load and heating coil OFF), the sensible heating energy rate delivered by the furnace is calculated as follows:

![](media/image4751.png)\


![](media/image4752.png)\


where:

*Mass Flow Rate~full load~* = air mass flow rate through furnace at full-load conditions, kg/s

*h~out, full load~* = enthalpy of air exiting the furnace at full-load conditions, J/kg

*h~control zone~*~~= enthalpy of air in the control zone (where thermostat is located), J/kg

*HR~min~       =* enthalpies evaluated at a constant humidity ratio, the minimum humidity ratio

of the furnace exiting air or the air in the control zone

*Mass Flow Rate~coil off~* = air mass flow rate through the furnace with the heating coil OFF, kg/s

*h~out, coil off~*  = enthalpy of air exiting the furnace with the heating coil OFF, J/kg

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4753.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the heating coil OFF conditions

![](media/image4754.png)\


With the calculated sensible heating energy rates and the total sensible heating load to be met by the system, the part-load ratio for the furnace is estimated.

![](media/image4755.png)\


The part-load ratio calculated above is used to determine the required heating coil capacity as ![](media/image4756.png)  where *Q~design~* is the nominal heating coil capacity as specified in the heating coil object. If the fan cycles on and off with the heating coil (i.e.,  when the supply air fan operating mode schedule values are equal to 0), then this part-load ratio is also used to determine the operating mass flow rate of the furnace as

![](media/image4757.png)\


 If the fan operates continuously (i.e. fan ON), the operating mass flow rate is specified as ![](media/image4758.png) . The furnace's fan and heating coil are then re-simulated to determine the furnace's delivered sensible heating capacity at the above calculated part-load ratio.

![](media/image4759.png)\


where:

![](media/image4760.png)    = sensible heating capacity delivered by the furnace (W)

![](media/image4761.png)     = air mass flow rate through the furnace (kg/s)

*h~out, actual~*~~= enthalpy of air exiting the furnace (J/kg)

*h~out, control zone~*~~ = enthalpy of air in the control zone (J/kg)

*HR~min~*              = enthalpies evaluated at a constant humidity ratio, the minimum humidity ratio

of the furnace exiting air or the air in the control zone

Δ~sen,~~actual~= Sensible load difference between the system output node and the zone inlet node at the above calculated part-load ratio.

![](media/image4762.png)\


Since the part-load performance of the heating coil can be non-linear, and the supply air fan heat varies based on heating coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the heating coil and fan are determined through iterative calculations (successive modeling of the heating coil and fan) until the furnace's heating output matches the heating load to be met within the heating convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the furnace's heating output divided by the load to be met.

![](media/image4763.png)\


If the furnace has been specified with cycling fan/cycling coil (AUTO fan), then the furnace's design air mass flow rate is multiplied by PartLoadRatio to determine the average air mass flow rate for the system simulation time step. The air conditions at nodes downstream of the heating coil represent the full-load (steady-state) values when the coil is operating. If the supply air fan is specified to run continuously (fan ON), then the air mass flow rate remains at the furnace's design air mass flow rate. In this case, the air conditions at nodes downstream of the heating coil are calculated as the average conditions over the simulation time step (i.e., the weighted average of full-load conditions when the coil is operating and inlet air conditions when the coil is OFF).

For the case where the furnace is scheduled to operate with continuous supply air fan operation, but no heating load is required to meet the setpoint temperature in the control zone, the supply air fan model is still called to determine the fan exiting air conditions. The heating coil model is also called, but for the case with no heating load the heating coil model simply passes the inlet air conditions and mass flow rate from its inlet node to its outlet node. The air exiting the heating coil is then sent to the direct air units for distribution to each zone served by the furnace, where the zone heat balance is performed to determine the resulting zone air conditions. The furnace exiting air conditions and energy consumption are calculated and reported by the individual component models (fan and heating coil).

### HeatCool Configuration

The HeatCool configuration consists of an on/off or constant volume fan, a DX cooling coil, and an electric or gas heating coil. For the cases where a heating load is calculated for the control zone or no heating/cooling load is calculated for the control zone, the model follows nearly identical computational steps as described in the HeatOnly Configuration section above. The only difference is the air mass flow rate during no cooling/heating operation with continuous supply air fan operation can be different from the air mass flow rate during heater operation for the HeatCool configuration. If a cooling load is calculated by EnergyPlus for the control zone, the solution methodology is also virtually identical and is described here for completeness.

If EnergyPlus determines that the furnace must supply cooling to the control zone to meet the zone air temperature setpoint, then the model computes the total sensible cooling load to be met by the furnace based on the control zone sensible cooling load and the fraction of the furnace air flow that goes through the control zone.

![](media/image4764.png)\


If the supply air fan operating mode schedule requests cycling fan operation, the model first checks for the presence of an ecomomizer in the outside air system serving the furnace's air loop (Ref. AirLoopHVAC:OutdoorAirSystem). If an outside air system is not present or if an air-side economizer is not used, the furnace's compressor is used to meet the furnace cooling load. If an air-side economizer is used and is active (i.e., economizer controls indicate that conditions are favorable to increase the outside air flow rate), the furnace will try to meet the cooling load by operating only the supply air fan. If the fan is able to satisfy the furnace cooling load, the compressor remains off for the entire simulation time step. If the operation of the fan alone is unable to meet the entire cooling load, then the compressor is enabled and additional calculations are performed to determine the compressor's part-load ratio.

The model then calculates the furnace's sensible cooling energy rate delivered to the zones being served when the system runs at full-load conditions and when the DX cooling coil is OFF. If the supply air fan cycles with the compressor, then the sensible cooling energy rate is zero when the cooling coil is OFF. However if the fan is configured to run continuously regardless of coil operation, then the sensible cooling energy rate will probably not be zero when the cooling coil is OFF. Calculating the sensible cooling energy rate involves modeling the supply air fan (and associated fan heat), the DX cooling coil, and the heating coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node). For each of these cases (full load and DX cooling coil OFF), the sensible cooling energy rate delivered by the furnace is calculated as follows:

![](media/image4665.png)\


![](media/image4666.png)\


where:

*Mass Flow Rate~full load~* = air mass flow rate through furnace at full-load conditions, kg/s

*h~out, full load~* = enthalpy of air exiting the furnace at full-load conditions, J/kg

*h~control zone~*~~= enthalpy of air in the control zone (where thermostat is located), J/kg

*HR~min~       =* enthalpies evaluated at a constant humidity ratio, the minimum humidity ratio

of the furnace exiting air or the air in the control zone

*Mass Flow Rate~coil off~* = air mass flow rate through the furnace with the cooling coil OFF, kg/s

*h~out, coil off~*  = enthalpy of air exiting the furnace with the cooling coil OFF, J/kg

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4667.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the heating coil OFF conditions

![](media/image4668.png)\


With the calculated sensible cooling energy rates and the total sensible cooling load to be met by the system, the part-load ratio for the furnace is estimated.

![](media/image4765.png)\


Since the part-load performance of the DX cooling coil is frequently non-linear (Ref: DX Cooling Coil Model), and the supply air fan heat varies based on cooling coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the cooling coil compressor and fan are determined through iterative calculations (successive modeling of the cooling coil and fan) until the furnace's cooling output matches the cooling load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the furnace's cooling output divided by the load to be met.

![](media/image4766.png)\


where:

![](media/image4767.png) = Furnace delivered sensible capacity (W)

If the furnace has been specified with cycling fan/cycling coil (AUTO fan) and high humidity control *has not* been specified, then the furnace's operating supply air mass flow rate is multiplied by PartLoadRatio to determine the average air mass flow rate for the system simulation time step. The air conditions at nodes downstream of the cooling coil represent the full-load (steady-state) values when the coil is operating. If a cycling fan is used and high humidity control *has* been specified, the calculation of average air mass flow rate is based on the greater of the heating or cooling part-load ratio (see following section on high humidity control). When the heating part-load ratio is greater than the cooling part-load ratio, the air conditions at nodes downstream of the cooling coil represent the weighted average of full-load conditions when the cooling coil is operating and inlet air conditions when the cooling coil is off (i.e., the fan continues to operate due to a heating requirement where the heating PLR is greater than the cooling PLR). If the supply air fan is specified to run continuously (fan ON), then the air mass flow rate continues to operate at the user-specified supply air mass flow rate when no cooling or heating is required. In this case, the air conditions at nodes downstream of the cooling coil are calculated as the average conditions over the simulation time step (i.e., the weighted average of full-load conditions when the coil is operating and inlet air conditions when the coil is OFF). The furnace exiting air conditions and energy consumption are calculated and reported by the individual component models (fan and DX cooling coil).

### High Humidity Control with HeatCool Configuration

An optional reheat coil can be specified with the HeatCool configuration to allow the furnace to control high zone humidity levels. The specific configuration of the HeatCool Furnace with high humidity control option is shown in Figure 222. The figure below shows the fan placement when a blow through fan is specified. If a draw through fan is specified, the fan is located between the heating coil and the reheat coil. The system is controlled to keep the relative humidity in the control zone from exceeding the setpoint specified in the object ZoneControl:Humidistat. This option is available when the supply air fan operates continuously (i.e., the supply air fan operating mode schedule values are never equal to 0) or the supply air fan cycles with the compressor. For the case of cycling fan mode when the high humidity control is specified as CoolReheat *and* the heating part-load ratio is greater than the compressor part-load ratio, the air conditions at nodes downstream of the cooling coil are calculated as the average conditions during the duration of fan operation (i.e., the weighted average of full load conditions when the coil is operating and inlet air conditions when the coil is off and the fan continues to operate to meet the heating load). For the case where cycling fan mode is used *and* the heating part-load ratio is less than the compressor part-load ratio, the air conditions at the nodes downstream of the cooling coil are simply the full load (steady-state) values when the coil is operating. In addition, when high humidity control is specified and the compressor operates, the furnace operates at the cooling air flow rate when a zone heating load is present as determined by the zone thermostat.

High humidity control is specified as either None, MultiMode, or CoolReheat in the Dehumidification Control Type input field. MultiMode is specified when a heat exchanger is used to improve the dehumidification performance of the cooling coil. The heat exchanger will be activated when the sensible part-load ratio is insufficient to meet the zone latent load. CoolReheat is specified when a DX cooling coil is used to over-cool the supply air stream in order to meet the zone latent load. In this case, a reheat coil will ensure the zone temperature does not fall below the zone heating temperature set point. When a heat exchanger is used in conjunction with a DX cooling coil and CoolReheat is specified as the Dehumidification Control Type, the heat exchanger is "locked on" to meet either the sensible or latent cooling load. If the dehumidification control type is selected as None and a heat exchanger assisted cooling coil is used, the heat exchanger is "locked on" and the air conditioner runs only to meet the sensible cooling load. Although a reheat coil is required when CoolReheat is specified in the Dehumidification Control Type input field, this reheat coil may optionally be present for the other Dehumidification Control Types (e.g., None and Multimode). If the reheat coil is present and the dehumidification control type input is not specified as CoolReheat, the reheat coil will not be active. This allows changing the dehumidification control option without requiring a change in the unit's coil configuration.

The model first calculates the PartLoadRatio required to meet the sensible cooling load as described above (see Eqn. ) to maintain the dry-bulb temperature setpoint in the control zone. If a moisture (latent) load exists because the control zone humidity has exceeded the setpoint, the total moisture load to be met by the HeatCool furnace (SystemMoistureLoad) is calculated based on the control zone moisture load and the control zone air flow fraction. The model then calculates the LatentPartLoadRatio required to meet the humidistat setpoint.

![](media/image4768.png)\


![](media/image4769.png)\


where:

*FullLatentOutput*   = the furnace's latent cooling energy rate at full-load conditions, W

*NoLatentOutput*     = the furnace's latent cooling energy rate with the cooling coil OFF, W

*MinPLR*= the minimum part-load ratio, which is usually 0.0. For the case when the latent capacity degradation model is used (Ref: DX Cooling Coil Model), this value is the minimum part-load ratio at which the cooling coil will dehumidify the air.

The model uses the greater of the two part-load ratios, PartLoadRatio or LatentPartLoadRatio, to determine the operating part-load ratio of the furnace's DX cooling coil. As previously described, iterations are performed to converge on the solution within the convergence tolerance.

![Schematic for Blow Through Furnace with High Humidity Control](media/schematic-for-blow-through-furnace-with-high.jpeg)


If the model determines that the LatentPartLoadRatio is to be used as the operating part-load ratio of the furnace's cooling coil, the reheat coil is used to offset the excess sensible capacity provided by the unit. The model first checks the sensible load that exists for the current simulation time step (predicted zone temperature with no HVAC operation compared to the thermostat setpoint temperatures). If a sensible cooling load or no sensible cooling or heating load exists (Figure 223), the model calculates the difference between the sensible heating load required to reach or maintain the heating dry-bulb temperature setpoint and the actual sensible cooling energy rate delivered by the unit (with LatentPartLoadRatio). In this case, the reheat coil is used to offset the excess sensible cooling energy provided by the DX cooling coil (if any) that could have caused an overshoot of the heating dry-bulb temperature setpoint. Note that when a humidistat is used and high humidity control is required, the zone dry-bulb temperature will typically move toward the heating temperature setpoint when a high moisture (latent) load exists. If a heating load exists (Figure 224), the reheat coil is used to offset the entire sensible cooling energy rate of the DX cooling coil (to meet the humidistat setpoint) and the heating coil is used to meet the entire heating load as described in the HeatOnly configuration section above. Note that when a heating load exists and high humidity control is required, the furnace operates at the user-specified cooling air flow rate for the entire simulation time step. As with the fan, DX cooling coil, and heating coil, report variables associated with reheat coil performance (e.g., heating coil energy, heating coil rate, heating coil gas or electric consumption, heating coil runtime fraction, etc.) are managed in the reheat (heating) coil object.

![Reheat Coil Load when Predicted Zone Temperature is Above Heating Setpoint](media/reheat-coil-load-when-predicted-zone.jpeg)


![Reheat Coil Load when Predicted Zone Temperature is Below Heating Setpoint](media/reheat-coil-load-when-predicted-zone-001.jpeg)


## Unitary Systems 

The input objects AirLoopHVAC:UnitaryHeatCool and AirLoopHVAC:UnitaryHeatOnly provides models that are identical to the Furnace and Central Air models described above.  Please reference the previous section for details.

## Unitary System with Changeover-Bypass-Variable Air Volume

### Overview

The input object AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass provides a model for a changeover-bypass variable air volume (CBVAV) unitary system that is a compound object made up of other components. Each CBVAV system consists of an outside air mixer, direct expansion (DX) cooling coil, heating coil, and a supply air fan as shown in the figures below. Zone thermostats and terminal units are required in each zone served by this system. The terminal units are specific to this system type and are either AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat or AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat. A zone humidistat and single zone max humidity setpoint manager may also be specified to help control high humidity levels. These individual components are described elsewhere in this document. The CBVAV unitary system object coordinates the operation of these components and is modeled as a type of air loop equipment (Ref. AirLoopHVAC).

![Schematic of a Changeover Bypass VAV Unitary System with Draw Through Fan](media/schematic-of-a-changeover-bypass-vav-unitary.jpeg)


![Schematic of Changeover Bypass VAV Unitary System with Blow Through Fan](media/schematic-of-changeover-bypass-vav-unitary.jpeg)


The CBVAV unitary system conditions one or more zones and is controlled by thermostats located in each zone (the use of a single humidistat is also allowed when using multi-mode DX cooling coils). The CBVAV system operates to meet the zone sensible cooling or sensible heating requirements as dictated by the thermostat schedule(s). The priority control input determines the mode of operation and is specified as Cooling Priority, Heating Priority, or Zone Priority. If Cooling Priority is specified, the system operates to meet the cooling load when any zone served by this system (air loop) requires cooling. If Heating Priority is specified, the system operates to meet the heating load when any zone requires heating. If Zone Priority is specified, the system operates based on the maximum number of zones requiring either heating or cooling.

Once the operating mode is determined, the CBVAV model calculates a target supply air temperature required to operate a single terminal unit at its maximum air flow rate. The remaining terminal units will modulate as required to maintain the dry-bulb temperature in the zone they are serving according to the thermostat schedule for their respective zone. The system air flow rate (through the supply air fan, cooling coil and heat coil) remains constant during cooling operation, heating operation, and no cooling/heating mode as specified by the user. Therefore, as the zone terminal units modulate to reduce zone air flow rates, the excess system air flow is "bypassed" from the bypass duct splitter node back to the bypass duct mixer node of the CBVAV system (see figures above).

The CBVAV system is able to model supply air fan operation in two modes: cycling fan – cycling coil (i.e., AUTO fan) and continuous fan – cycling coil (i.e., fan ON). A Fan:OnOff or Fan:ConstantVolume is used in either case. Since the excess system air flow is bypassed while the zone terminal units modulate, the supply air fan operates the entire simulation time step when heating or cooling is required. For this reason, AUTO fan only allows the supply air fan to turn off when no cooling or heating is required. If fan ON is specified, the supply air fan runs the entire time the system is scheduled to operate (via its availability schedule). The mode of operation for the supply air fan is specified through a fan operating mode schedule where a value of 0 in the schedule indicates cycling fan mode and a value greater than 0 indicates continuous fan mode. If the schedule is not provided, the supply air fan operating mode is considered to be continuous (fan ON).

Output variables reported for the CBVAV system include the supply air fan part-load ratio, the compressor part-load ratio, and the electric consumption of the CBVAV system. Additional output variables report the total heating rate and total cooling rate provided by the CBVAV system. The sensible and latent components for total system cooling and heating are also available as output variables. Reporting of other variables of interest for the CBVAV system (DX coil cooling rate, heating rate, crankcase electric power and energy, supply air fan electric power, etc.) is done by the individual system components (fan, DX cooling coil, and heating coil).

### Model Description

As described previously, the CBVAV system conditions one or more zones and is controlled by zone thermostats (Ref. ZoneControl:Thermostat). For each simulation time step, EnergyPlus performs a zone air heat balance to determine if cooling or heating is required to meet the zone thermostat setpoints, excluding any impacts from CBVAV system operation. CBVAV system performance is then modeled with all heating/cooling coils off but with the supply air fan operating according to the user specified supply air fan operating mode schedule. If the zone air heat balance plus the impact of CBVAV system operation with coils off results in no requirement for heating or cooling by the CBVAV coils, or if the CBVAV system is scheduled off (via its availability schedule), then the CBVAV coils do not operate and the compressor part-load ratio output variable is set to 0. If the model determines that cooling or heating is required and the CBVAV system is scheduled to operate, the model calculates a target supply air temperature required to maintain a single terminal unit at it's maximum air flow rate, the amount of bypass air returning to the bypass duct mixer node, and the part-load ratio of the cooling or heating coils in order to meet the target supply air temperature.

When high humidity control is desired, a ZoneControl:Humidistat input object and a SetpointManager:SingleZone:Humidity:Maximum, SetpointManager:MultiZone:MaximumHumidity:Average    or SetpointManager:MultiZone:Humidity:Maximum object are required. The air outlet node of the CBVAV system is used as the control node for the setpoint manager. For this reason, only a single humidistat should be specified for one of the zones being served by this system. If humidistat/setpoint manager objects are specified for more than one zone served by this system, only the last of the SetpointManager:SingleZone:Humidity:Maximum objects is used (the setpoint manager objects are read in order of occurrence in the input file). Since the outlet node of the CBVAV system is always used as the control node, the setpoints calculated by the previous setpoint managers are overwritten by the last setpoint manager in the input. When a heat exchanger is used to improve the dehumidification performance of the cooling coil (i.e. CoilSystem:Cooling:DX:HeatExchangerAssisted), the heat exchanger is always active and can not be turned on and off based on zone humidity levels.

The remainder of this section describes the calculations performed when cooling or heating coil operation is required. For any HVAC simulation time step, the CBVAV system can only cool or heat the air, not both. Because the CBVAV system bypasses system air flow back to the inlet of the CBVAV unit, the system operates for the entire simulation time step. If the user specifies continuous fan operation, then the supply air fan continues to operate at a user-specified flow rate even during periods when the coils cycle off. If the user specifies AUTO fan operation, then the supply air fan cycles off for the entire simulation time step only when no cooling or heating is required.

#### Operating Mode

The first step in modeling a CBVAV system is to obtain the cooling or heating load for each zone as calculated by EnergyPlus based on the zone thermostat setpoint temperature(s). The calculated loads for each zone are used to determine the total cooling and heating requirements for all zones served by this system. In addition to summing the zone cooling and heating loads, the number of zones in cooling and the number of zones in heating are totalized. The priority control mode specified by the user is then used to determine the operating mode for this simulation time step.

If Cooling Priority is specified and the total cooling requirement in all zones is not equal to zero, then cooling is selected as the operating mode. If the total cooling requirement is equal to zero and the total heating requirement is not equal to zero, then heating is selected as the operating mode. If the total cooling requirement and total heating requirement are equal to zero then the zones are allowed to float (no heating or cooling provided).

If Heating Priority is specified and the total heating requirement in all zones is not equal to zero, then heating is selected as the operating mode. If the total heating requirement is equal to zero and the total cooling requirement is not equal to zero, then cooling is selected as the operating mode. If the total cooling requirement and total heating requirement are equal to zero then the zones are allowed to float (no heating or cooling provided).

If Zone Priority is specified and the total number of zones requiring cooling is greater than the total number of zones requiring heating, then cooling is selected as the operating mode. If the total number of zones requiring heating is greater than the total number of zones requiring cooling, then heating is selected as the operating mode. If the total number of zones requiring cooling is equal to the total number of zones requiring heating, then the magnitude of the total cooling and heating requirements for all zones sets the operating mode. In this case, if the magnitudes of the cooling and heating requirements are zero, then the zones are allowed to float (no heating or cooing provided). If the magnitudes of the cooling and heating requirements are non-zero and identical, then cooling is selected as the operating mode.

#### Calculation of Bypass Duct Mixer Node Conditions

The operation of this system is unique in that it uses constant-air-volume equipment to provide variable air volume to the conditioned zones. This is accomplished with the use of a bypass duct (shown in the figures above) which shunts excess system air flow from the bypass duct splitter node at the system's outlet back to the bypass duct mixer node at the system's inlet. The air conditions at the bypass duct mixer node are based on the inlet air to the CBVAV system, the system outlet air conditions required to meet the zone loads, and the fraction of bypassed air. The following calculations are performed each simulation time step.

![](media/image4775.png)\


![](media/image4776.png)\


![](media/image4777.png)\


![](media/image4778.png)\


where:

![](media/image4779.png) = fraction of system air directed to the bypass duct mixer node

![](media/image4780.png) = Air mass flow rate at the system's air outlet node, kg/s

![](media/image4781.png) = Air mass flow rate through CBVAV system (coils and supply air fan), kg/s

![](media/image4782.png) = Air temperature at the bypass duct mixer node, °C

![](media/image4783.png) = Air inlet node temperature, °C

![](media/image4784.png) = Air outlet node temperature, °C

![](media/image4785.png) = Air humidity ratio at the bypass duct mixer node, kg/kg

![](media/image4786.png) = Air inlet node humidity ratio, kg/kg

![](media/image4787.png) = Air outlet node humidity ratio, kg/kg

![](media/image4788.png) = Air enthalpy at the bypass duct mixer node, J/kg

![](media/image4789.png) = Psychrometric function calculating enthalpy given dry-bulb temperature and humidity ratio

#### Cooling Operation

If the model determines that the CBVAV system must supply cooling to the zones, the model first calculates a target outlet air dry-bulb temperature required to operate a single terminal unit at its maximum air flow rate. The minimum target temperature for all zones is used to control the CBVAV system in cooling mode.

![](media/image4790.png)\


![](media/image4791.png)\


where:

![](media/image4792.png) = Target supply air temperature for zone *i*, °C

![](media/image4793.png) = Air temperature in zone *i*, °C

![](media/image4794.png) = Cooling or heating load for zone *i* (cooling loads are negative values, heating loads are positive values), W

![](media/image4795.png) = Specific heat of supply air with coils off, J/kg-K

![](media/image4796.png) = Maximum terminal unit mass flow rate in zone *i*, kg/s

![](media/image4797.png) = Target supply (outlet) air temperature for the CBVAV system, °C

The model then calculates the part-load ratio of the DX compressor required to meet the target supply (outlet) air temperature. Since the part-load performance of the DX cooling coil is frequently non-linear (Ref: Coil:Cooling:DX:SingleSpeed model), the actual part-load ratio for the cooling coil compressor is determined through iterative calculations (successive modeling of the DX cooling coil model) until the CBVAV system's outlet air temperature (including on/off cycling effects of the DX coil) matches the target supply (outlet) air temperature within a small temperature convergence tolerance (1E-5 ˚C).

Since the supply air fan operates continuously for each simulation time step (or is OFF for the entire HVAC system time step), the air conditions at nodes downstream of the cooling coil are calculated as the average conditions over the simulation time step (i.e., the weighted average of coil's outlet node conditions when the coil is operating and the coil's inlet node conditions when the coil is OFF).

#### Heating Operation

Modeling the CBVAV system's DX heating coil is identical to the calculations described above for cooling operation except that the maximum target supply air temperature for all zones is used to control the CBVAV system in heating mode.

![](media/image4798.png)\


Iterative calculations (successive modeling of the DX heating coil model) are used to determine the final heating coil part-load ratio to account for the non-linear performance of the DX heating coil at part-load conditions.

When a gas or electric heating coil is used instead of a DX heating coil, the amount of heat required by the coil is calculated based on the target supply (outlet) air temperature and the coil inlet air temperature as follows:

![](media/image4799.png)\


where:

![](media/image4800.png) = Heating coil load, W

![](media/image4801.png) = Specific heat of heating coil inlet air, J/kg-K

![](media/image4802.png) = Heating coil inlet air temperature, °C

#### Floating Operation (no cooling or heating)

When the zone thermostats determine that there is no cooling or heating requirement in any zone, the CBVAV coils are OFF and the zone temperatures are allowed to float. However, a special case exists where the user specifies continuous supply air fan operation and no cooling or heating requirements exist in any zone. In this case, the outlet air temperature of the CBVAV system (with fan ON and coils OFF) is compared to the target outlet air temperatures required to maintain the zone's cooling and heating setpoint temperatures. If the outlet air temperature with the supply fan ON and coils OFF is below the target outlet air temperature required to maintain the heating setpoint, then heating mode is enabled to avoid overshooting the zone heating setpoint temperature. Conversely, if the outlet air temperature with the supply fan ON and coils OFF is above the target outlet air temperature required to maintain the cooling setpoint, then cooling mode is enabled to avoid overshooting the zone cooling setpoint temperature. This special case is handled at any time a no load condition is reported by the thermostats and overshooting the zone air temperature setpoint is possible regardless of the priority control mode selected by the user.

#### Minimum and Maximum Outlet Air Temperature in Cooling/Heating Operation

The user also specifies a minimum outlet air temperature during cooling operation and a maximum outlet air temperature during heating operation. The target outlet air temperature, as calculated above, is compared to each of these limits during each simulation time step. The resulting target outlet air temperature is used to control the heating and cooling coils.

![](media/image4803.png)\


![](media/image4804.png)\


where:

![](media/image4805.png) = Minimum outlet air temperature during cooling operation, °C

![](media/image4806.png) = Maximum outlet air temperature during heating operation, °C

#### Air Flow Calculations

The changeover-bypass VAV system operates based on user-specified (or autosized) air flow rates. The CBVAV system air flow rate (i.e., air flow through the supply air fan and heating/cooling coils) during cooling operation may be different than the system air flow rate during heating operation. In addition, the system air flow rate when no cooling or heating is required but the supply air fan remains ON can be different than the air flow rates when cooling or heating is required. The outside air flow rates can likewise be different in these various operating modes. The model takes these different air flow rates into account when simulating the performance of the CBVAV system. The system air flow rate for each simulation time step is based on the current operating mode (cooling, heating, or no cooling/heating) and is reported on the inlet/outlet air nodes of the various CBVAV components (e.g. fan, cooling coil, and heating coil). The supply air flow rate delivered to the individual zones is calculated based on the sum of the air flow rates through each terminal unit and is reported at the air inlet and outlet nodes of the CBVAV system each simulation time step. The difference between the system air mass flow rate and the supply air mass flow rate delivered to all zones is equal to the bypass air mass flow rate which is also reported each simulation time step.

The system and outside air flow rates when a system coil is ON (e.g., system air volumetric flow rate during cooling operation, system air volumetric flow rate during heating operation, outside air volumetric air flow rate during cooling operation, and outside air volumetric air flow rate during heating operation) or the system coils are OFF and the fan operates continuously (e.g., system air volumetric flow rate when no cooling or heating is needed and outside air volumetric air flow rate when no cooling or heating is needed) are specified by the user, or can be autosized, and are converted from volumetric flow rate to mass flow rate at standard conditions. If the user has specified cycling fan operation, then the system air and outside air mass flow rates when the system coils are OFF the entire time step are zero. If the user has specified constant fan operation, then the air flow rates when no cooling or heating is needed are used when the system coils are OFF the entire time step.

There is one special case. If the user has specified constant fan operation and they specify that the system air volumetric flow rate when no cooling or heating is needed is zero (or if the field is left blank), then the model assumes that the system air mass flow rate when the system coils are OFF the entire time step is equal to the corresponding air mass flow rate when any system coil was last operating (ON). This model handles the zero (or blank) entry for outside air volumetric flow rate when no cooling or heating is needed in an analogous fashion.

![](media/image4807.png)\


![](media/image4808.png)\


![](media/image4809.png)\


![](media/image4810.png)\


where:

![](media/image4811.png) = standard air density (1.204 kg/m^3^) adjusted for the local barometric pressure (standard barometric pressure corrected for altitude, ASHRAE 1997 HOF pg. 6.1), kg/m^3^

![](media/image4812.png) = User-specified system volumetric flow rate in cooling, heating, or no cooling or heating mode, m^3^/s

![](media/image4813.png) = Outdoor air mass flow rate introduced through the CBVAV system, kg/s

![](media/image4814.png) = User-specified outdoor air volumetric flow rate in cooling, heating, or no cooling or heating mode, m^3^/s

#### Calculation of System Heating and Cooling Rates

At the end of each HVAC simulation time step, this object reports the heating or cooling rate and energy delivered by the system, as well as the electric power and consumption by the system. In terms of thermal energy delivered by the system, the sensible, latent and total energy transfer rates for the system are calculated as follows:

![](media/image4815.png)\


![](media/image4816.png)\


![](media/image4817.png)\


where:

![](media/image4818.png) = total energy transfer rate by the system, W

![](media/image4819.png) = sensible energy transfer rate by the system, W

![](media/image4820.png) = latent energy transfer rate by the system, W

*h~in~~let~*= enthalpy of the air entering the unit at its inlet node, J/kg

*h~outlet~* = enthalpy of the air leaving the unit at its outlet node, J/kg

![](media/image4821.png) = minimum of the inlet air and outlet air humidity ratio, kg/kg

Since each of these energy transfer rates can be calculated as positive or negative values, individual reporting variables are established for cooling and heating and only positive values are reported. The following calculations are representative of what is done for each of the energy transfer rates:

![](media/image4822.png)\


where:

![](media/image4823.png) = output variable ‘Unitary System Total Cooling Rate, W'

![](media/image4824.png) = output variable ‘Unitary System Total Heating Rate, W'

In addition to heating and cooling rates, the heating and cooling energy supplied by the system are also calculated for the time step being reported. The following example for total zone cooling energy is representative of what is done for the sensible and latent energy as well as the heating counterparts.

![](media/image4825.png)\


where:

![](media/image4826.png) = output variable ‘Unitary System Total Cooling Energy, J'

*TimeStepSys*= HVAC system simulation time step, hr

3600= conversion factor, sec/hr

### Changeover-bypass VAV Unitary System Sizing

The changeover-bypass VAV system is modeled as an air loop component and is sized according to the methodology described elsewhere in this document (Ref. System Design Loads and Air Flow Rates). The following volumetric air flow rates may be autosized:

system air volumetric flow rate during cooling operation

system air volumetric flow rate during heating operation

system air volumetric flow rate when no cooling or heating is needed

outside air volumetric air flow rate during cooling operation

outside air volumetric air flow rate during heating operation

outside air volumetric air flow rate when no cooling or heating is needed

## Unitary Air-To-Air Heat Pump

### Overview

The input object AirLoopHVAC:UnitaryHeatPump:AirToAir provides a model for an air-to-air heat pump that is a "virtual" component that consists of an on/off or constant volume fan component, a DX cooling coil, a DX heating coil, and a gas or electric supplemental heating coil. The specific configuration of the blow through heat pump is shown in the following figure. For a draw through heat pump, the fan is located between the DX heating coil and the supplemental heating coil.

![Schematic of a Blow Through Air-to-Air Heat Pump](media/schematic-of-a-blow-through-air-to-air-heat.jpeg)


While the heat pump may be configured to serve multiple zones, system operation is controlled by a thermostat located in a single "control" zone. One of the key parameters for the heat pump component is the fraction of the total system airflow that goes through the control zone. This fraction is calculated as the ratio of the maximum air mass flow rate for the air loop's supply inlet node for the control zone (e.g., AirTerminal:SingleDuct:Uncontrolled, field = Maximum Air Flow Rate, converted to mass flow) to the sum of the maximum air mass flow rates for the air loop's supply inlet nodes for all zones served by this air loop. The heat pump module scales the calculated load for the control zone upward based on this fraction to determine the total load to be met by the heat pump. The module then proceeds to calculate the required part-load ratio for the system coil and the supply air fan to meet this total load. The heating or cooling capacity delivered by the heat pump is distributed to all of the zones served by this system via the direct air units that supply air to each zone. The supply air fraction that goes though the control zone is calculated as follows:

![](media/image4828.png)\


where:

![](media/image4829.png) =maximum air mass flow rate for the air loop's supply inlet node (terminal unit) for the control zone (kg/s)

![](media/image4830.png) =maximum air mass flow rate for the air loop's supply inlet node for the jth zone (kg/s)

![](media/image4831.png) =number of zones, or number of air loop supply air inlet nodes for all zones served by the air loop (-)

The heat pump component is able to model supply air fan operation in two modes: cycling fan – cycling coil (i.e., AUTO fan) and continuous fan – cycling coil (i.e., fan ON). Fan:OnOff must be used to model AUTO fan, while Fan:OnOff or Fan:ConstantVolume can be used to model fan ON. The fan operation mode is specified using a supply air fan operating mode schedule where schedule values of 0 denote cycling fan operation and schedule values other than 0 (a 1 is usually used) denote continuous fan operation. Using this schedule, the furnace fan may be cycled with cooling or heating coil operation or operated continuously based on time of day (e.g. cycling fan operation at night and continuous fan operation during the day). If the fan operating mode schedule name field is left blank in the heat pump object, the heat pump assumes cycling or AUTO fan mode operation throughout the simulation.

The output variables reported by the heat pump object are fan part-load ratio and compressor part-load ratio. Fan part-load ratio is defined as the actual air mass flow rate through the system for the time step divided by the operating supply air mass flow rate specified for the heat pump (![](media/image4832.png) ). The operating supply air mass flow rate may be different for cooling, heating, and when no cooling or heating is required and the fan operates continuously. Compressor part-load ratio is the actual load for the time step divided by the full-load sensible capacity (see Eqn.  or Eqn.). Reporting of other variables of interest for the heat pump (heating rate, cooling rate, energy consumption, etc.) is done by the individual system components (fan, DX cooling coil, DX heating coil, and supplemental heating coil).

### Model Description

As described previously, the heat pump is a "virtual" component consisting of a fan, DX cooling coil, DX heating coil and a supplemental heating coil. The sole purpose of the heat pump model is to properly coordinate the operation of the various system components. The following sections describe the flow of information within the model, as well as the differences between cycling and continuous supply air fan operation.

### Cooling Operation 

If EnergyPlus determines that the heat pump must supply cooling to the control zone to meet the zone air temperature setpoint, then the heat pump model computes the total sensible cooling load to be delivered to the zones being served based on the control zone sensible cooling load and the fraction of the heat pump air flow that goes through the control zone.

![](media/image4833.png)\


If the supply air fan operating mode schedule requests cycling fan operation, the model first checks for the presence of an ecomomizer in the outside air system serving the heat pump's air loop (Ref. AirLoopHVAC:OutdoorAirSystem). If an outside air system is not present or if an air-side economizer is not used, the heat pump's compressor is used to meet the heat pump cooling load. If an air-side economizer is used and is active (i.e., economizer controls indicate that conditions are favorable to increase the outside air flow rate), the heat pump will try to meet the cooling load by operating only the supply air fan. If the fan is able to satisfy the heat pump cooling load, the compressor remains off for the entire simulation time step. If the operation of the fan alone is unable to meet the entire cooling load, then the compressor is enabled and additional calculations are performed to determine the compressor's part-load ratio.

The model then calculates the heat pump's sensible cooling energy rate delivered to the zones being served when the system runs at full-load conditions and when the DX cooling coil is OFF. If the supply air fan cycles with the compressor, then the sensible cooling energy rate is zero when the cooling coil is OFF. However if the fan is scheduled to run continuously regardless of coil operation, then the sensible cooling energy rate will not be zero when the cooling coil is OFF. Calculating the sensible cooling energy rate involves modeling the supply air fan (and associated fan heat) and the DX cooling coil. The DX heating coil and the supplemental heating coil are also modeled, but only to pass the air properties and mass flow rate from their inlet nodes to their outlet nodes. For each of these cases (full load and DX cooling coil OFF), the sensible cooling energy rate delivered by the heat pump is calculated as follows:

![](media/image4834.png)\


![](media/image4835.png)\


where:

*Mass Flow Rate~full  load~*  = air mass flow rate through heat pump at full-load conditions, kg/s

*h~out, full load~*   = enthalpy of air exiting the heat pump at full-load conditions, J/kg

*h~control  zone~*   = enthalpy of air leaving the control zone (where thermostat is located), J/kg

*HR~min~ =* enthalpies evaluated at a constant humidity ratio, the minimum humidity ratio of the heat pump exiting air or the air leaving the control zone

*Mass Flow Rate~coil off~*  = air mass flow rate through the heat pump with the cooling coil OFF, kg/s

*h~out,coil  off~*    = enthalpy of air exiting the heat pump with the cooling coil OFF, J/kg

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4836.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the heating coil OFF conditions

![](media/image4837.png)\


With the calculated sensible cooling energy rates and the total sensible cooling load to be met by the system, the part-load ratio for the heat pump is estimated.

![](media/image4838.png)\


Since the part-load performance of the DX cooling coil is frequently non-linear, and the supply air fan heat varies based on cooling coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the cooling coil compressor and fan are determined through iterative calculations (successive modeling of the cooling coil and fan) until the heat pump's cooling output matches the cooling load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the heat pump's cooling output divided by the load to be met.

![](media/image4839.png)\


where:

![](media/image4840.png) = Heat pump delivered sensible capacity (W)

If the heat pump has been specified with cycling fan/cycling coil (AUTO fan), then the heat pump's operating supply air mass flow rate is multiplied by PartLoadRatio to determine the average air mass flow rate for the system simulation time step. In this case, the air conditions at nodes downstream of the cooling coil represent the full-load (steady-state) values when the coil is operating.

If the fan operates continuously (i.e., when the supply air fan operating mode schedule values are NOT equal to 0), the operating air mass flow rate through the heat pump is calculated as the average of the user-specified air flow rate when the cooling coil is ON and the user-specified air flow rate when the cooling coil is OFF (user-specified supply air volumetric flow rates converted to dry air mass flow rates).

![](media/image4841.png)\


where:

![](media/image4673.png)  = air mass flow rate through heat pump when the cooling coil is ON (kg/s)

![](media/image4674.png)  = air mass flow rate through heat pump when no cooling or heating is needed (kg/s)

In this case, the air conditions at nodes downstream of the cooling coil are calculated as the average conditions over the simulation time step (i.e., the weighted average of full-load conditions when the coil is operating and inlet air conditions when the coil is OFF).

Heating Operation

Calculations for heating operation are similar to those for cooling operation in most respects. However, due to the inclusion of a supplemental heating coil, additional calculations are necessary to properly meet the total heating load for the zones being served.

If EnergyPlus determines that the heat pump must supply heating to the control zone to meet the zone air temperature setpoint, then the heat pump model computes the total sensible heating load to be delivered to the zones being served based on the control zone sensible heating load and the control zone airflow fraction.

![](media/image4842.png)\


The model then calculates the heat pump's sensible heating energy rate delivered to the zones being served when the system runs at full-load conditions and when the DX heating coil is OFF (without supplemental heater operation in either case). If the supply air fan cycles with the compressor, then the sensible heating energy rate is zero when the compressor is OFF. However if the fan is scheduled to run continuously regardless of coil operation, then the sensible heating energy rate will not be zero when the compressor is OFF. Calculating the sensible heating energy rate involves modeling the supply air fan (and associated fan heat), the DX cooling coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node), the DX heating coil, and the supplemental heating coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node). For each of these cases (full load and DX heating coil OFF, without supplemental heater operation in either case), the sensible heating energy rate delivered by the heat pump is calculated as follows:

![](media/image4703.png)\


![](media/image4704.png)\


where:

*Mass Flow Rate ~full load~*  = air mass flow rate through heat pump at full-load conditions, kg/s

*h~out, full load~*  = enthalpy of air exiting the heat pump at full-load conditions, J/kg

*h~control  zone~*  = enthalpy of air leaving the control zone (where thermostat is located), J/kg

*HR~min~ =* enthalpies evaluated at a constant humidity ratio, the minimum humidity ratio of the heat pump exiting air or the air leaving the control zone

*Mass Flow Rate ~coil  off~*  = air mass flow rate through the heat pump with the heating coil OFF, kg/s

*h~out, coil  off~*  = enthalpy of air exiting the heat pump with the heating coil OFF, J/kg

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4705.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the heating coil OFF conditions

![](media/image4706.png)\


With the calculated sensible heating energy rates and the total sensible heating load to be met by the system, the part-load ratio for the heat pump is estimated.

![](media/image4843.png)\


Since the part-load performance of the DX heating coil is frequently non-linear (Ref: Single-Speed Electric Heat Pump DX Air Heating Coil), and the supply air fan heat varies based on heating coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the heating coil compressor and fan are determined through iterative calculations (successive modeling of the heating coil and fan) until the heat pump's heating output matches the heating load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the heat pump's heating output divided by the load to be met.

![](media/image4844.png)\


where:

![](media/image4845.png) = Heat pump delivered sensible capacity (W)

If the heat pump's DX heating coil output at full load is insufficient to meet the entire heating load, PartLoadRatio is set equal to 1.0 (compressor and fan are not cycling) and the remaining heating load is passed to the supplemental heating coil. If the heat pump model determines that the outdoor air temperature is below the minimum outdoor air temperature for compressor operation, the compressor is turned off and the entire heating load is passed to the supplemental gas or electric heating coil. The heat pump exiting air conditions and energy consumption are calculated and reported by the individual component models (fan, DX heating coil, and supplemental gas or electric heating coil).

If the heat pump has been specified with cycling fan/cycling coil (AUTO fan), then the heat pump's operating supply air mass flow rate is multiplied by PartLoadRatio to determine the average air mass flow rate for the system simulation time step. The air conditions at nodes downstream of the heating coils represent the full-load (steady-state) values when the coils are operating. If the fan operates continuously (i.e., when the supply air fan operating mode schedule values are NOT equal to 0), the operating air mass flow rate through the heat pump is calculated as the average of the user-specified air flow rate when the heat pump heating coil is ON and the user-specified air flow rate when the heat pump heating coil is OFF (user-specified supply air volumetric flow rates converted to dry air mass flow rates).

![](media/image4846.png)\


where:

![](media/image4711.png)  = air mass flow rate through heat pump when the heating coil is ON (kg/s)

![](media/image4712.png)  = air mass flow rate through heat pump when no heating or cooling is needed (kg/s)

In this case, the air conditions at nodes downstream of the heating coils are calculated as the average conditions over the simulation time step (i.e., the weighted average of full-load conditions when the coils are operating and inlet air conditions when the coils are OFF).

### High Humidity Control with AirToAir HeatPump Model

The specific configuration of the AirToAir HeatPump with supplemental heating coil is shown above (see Figure 227). This figure shows the fan placement when a blow through fan is specified. If a draw through fan is specified, the fan is located between the heating coil and the supplemental heating coil. The system is controlled to keep the high relative humidity in the control zone from exceeding the setpoint specified in the object ZoneControl:Humidistat. This option is available when the supply air fan operates continuously (i.e., the supply air fan operating mode schedule values are never equal to 0) or the supply air fan cycles with the compressor. In addition, when high humidity control is specified and the compressor operates, the heatpump operates at the cooling air flow rate when a zone heating load is present as determined by the zone thermostat. High humidity control is specified as either None, MultiMode, or CoolReheat in the Dehumidification Control Type input field. MultiMode is specified when a heat exchanger is used to improve the dehumidification performance of the cooling coil. The heat exchanger will be activated when the sensible part-load ratio is insufficient to meet the zone latent load. CoolReheat is specified when a DX cooling coil is used to over-cool the supply air stream in order to meet the zone latent load. In this case, a supplemental heating coil will ensure the zone temperature does not fall below the zone heating temperature set point. When a heat exchanger is used in conjunction with a DX cooling coil and CoolReheat is specified as the Dehumidification Control Type, the heat exchanger is "locked on" to meet either the sensible or latent cooling load. If the dehumidification control type is selected as None and a heat exchanger assisted cooling coil is used, the heat exchanger is "locked on" and the air conditioner runs only to meet the sensible cooling load. A supplemental heating coil is required for all dehumidification control types.

The model first calculates the *PartLoadRatio* required meeting the sensible cooling load.  The heatpump's sensible cooling load is determined from the control zone sensible cooling load to the cooling setpoint and the control zone air flow fraction to maintain the dry-bulb temperature setpoint in the control zone.:

![](media/image4847.png)\


The heatpump's sensible cooling load to be met and the full load cooling output are used to calculate the sensible the part-load ratio iteratively based on user specified convergence criterion.

![](media/image4848.png)\


When the heat pump's sensible cooling capacity meets the system sensible cooling load at a given sensible part load ratio, then the Heat pump meets the controlled zone cooling setpoint temperature. If a moisture (latent) load exists because the control zone humidity has exceeded the setpoint, the total moisture load to be met by the heat pumps (HeatPumpMoistureLoad) is calculated based on the control zone moisture load and the control zone air flow fraction.

![](media/image4849.png)\


Then the *LatentPartLoadRatio* required to meet the high humidity setpoint is calculated as follows:

![](media/image4850.png)\


The model uses the greater of the two part-load ratios, *PartLoadRatio* or *LatentPartLoadRatio*, to determine the operating part-load ratio of the Heat Pump's DX cooling coil.

![](media/image4851.png)\


As previously described, iterations are performed to converge on the solution within the convergence tolerance.

Where,

![](media/image4736.png) = the control zone sensible cooling load to the cooling setpoint, (W).

![](media/image4852.png) = the control zone moisture load to the dehumidifying relative humidity setpoint, (W).

![](media/image4853.png)  = the supply air fraction that goes though the control zone, (-).

*FullLatentOutput* =the Heat Pump's latent cooling energy rate at full-load conditions, W

*NoLatentOutput* =the Heat Pump's latent cooling energy rate with cooling coil OFF, W

![](media/image4854.png) =the heat pump's part-load-ratio required to meet system sensible load, (-).

![](media/image4855.png) =the heat pump's part-load-ratio required to meet system moisture load, (-).

*![](media/image4856.png)* *=the minimum part-load ratio, which is usually 0.0. For the case when the latent capacity degradation model is used (Ref: DX Cooling Coil Model), this value is the minimum part-load ratio at which the cooling coil will dehumidify the air.*

When the predicted zone air temperature is above the heating setpoint and if there is a dehumidification load, the supplemental heating coil load is required to offset the excess cooling as shown in Figure 228. If the model determines that the LatentPartLoadRatio is to be used as the operating part-load ratio of the heatpump's cooling coil, the supplemental heating coil is used to offset the excess sensible capacity provided by the heat pump DX cooling coil. The model first checks the sensible load that exists for the current simulation time step (predicted zone temperature with no HVAC operation compared to the thermostat setpoint temperatures). If a sensible cooling load or no sensible cooling or heating load exists (see Figure 2),  the model calculates the difference between the sensible heating load required to reach or maintain the heating dry-bulb temperature setpoint and the actual sensible cooling energy rate delivered by the unit (with LatentPartLoadRatio). In this case, the supplemental heating coil is used to offset the excess sensible cooling energy provided by the DX cooling coil (if any) that could have caused an overshoot of the heating dry-bulb temperature setpoint. Note that when a humidistat is used and high humidity control is required, the zone dry-bulb temperature will typically move toward the heating temperature setpoint when a high moisture (latent) load exists.

![Supplemental heating coil load when predicted zone air temperature is above the heating Setpoint](media/supplemental-heating-coil-load-when-predicted.jpeg)


If a heating load exists (Figure 229), the supplemental heating coil is used to meet the heating coil load and at the same time offset the entire sensible cooling energy rate of the DX cooling coil (to meet the humidistat setpoint). Note that when a heating load exists and high humidity control is required, the heat pump operates at the user-specified cooling air flow rate for the entire simulation time step. As with the fan, and DX cooling coil, report variables associated with supplemental heating coil performance (e.g., heating coil energy, heating coil rate, heating coil gas or electric energy, heating coil runtime fraction, etc.) are managed in the supplemental (heating) coil object.

![Supplemental heating coil load when predicted zone air temperature is below the heating setpoint](media/supplemental-heating-coil-load-when-predicted-001.jpeg)


## Unitary Multi-Speed Air-To-Air Heat Pump 

### Overview

The input object AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed provides a model for a multispeed air-to-air heat pump that is a "virtual" component that consists of an on/off or constant volume fan component, a multispeed DX cooling coil, a multispeed DX heating coil, and a gas or electric supplemental heating coil. The main difference between this heat pump object and other EnergyPlus heat pump objects is that this object allows from two to four discrete compressor speeds for heating and cooling operation (instead of a single speed for each mode). The specific configuration of the blow through heat pump is shown in the following figure. For a draw through heat pump, the fan is located between the DX heating coil and the supplemental heating coil.

![Schematic of a Multispeed Air-to-Air Heat Pump (Blow-through Configuration)](media/schematic-of-a-multispeed-air-to-air-heat.jpeg)


While the heat pump may be configured to serve multiple zones, system operation is controlled by a thermostat located in a single "control" zone. One of the key parameters for the heat pump component is the fraction of the total system airflow that goes through the control zone. This fraction is calculated as the ratio of the maximum air mass flow rate for the air loop's supply inlet node for the control zone (e.g., AirTerminal:SingleDuct:Uncontrolled, field = Maximum Air Flow Rate, converted to mass flow) to the sum of the maximum air mass flow rates for the air loop's supply inlet nodes for all zones served by this air loop. The heat pump module scales the calculated load for the control zone upward based on this fraction to determine the total load to be met by the heat pump. The module then proceeds to calculate the required cycling ratio, speed ratio and speed number for the system coil and determines the supply air mass flow rate to meet this total load based on the speed number. The cycling ratio is the ratio of the sensible load (heating or cooling) to the steady-state capacity of the multispeed heat pump's DX heating or cooling coil at Speed 1 for the entire system time step. It is equivalent to the part load ratio for a single speed DX coil. The value is between 0.0 and 1.0 when the system operates at its lowest speed (Speed 1) and 1.0 when the multispeed heat pump operates at speeds above 1. The speed ratio is the ratio of time in a system time step that the compressor is at rated speed between two consecutive speed numbers ([Compressor Speed - Compressor speed at Speed i-1] / [Compressor speed at Speed i - Compressor speed at Speed i-1]). The compressor speed ratio is between 0.0 and 1.0 when the speed number is above 1 and is 0.0 during Speed 1 operation. The speed number is the lowest index number whose corresponding full-load sensible capacity at the given air mass flow rate is greater than or equal to the sensible load (heating or cooling) in a system time step. The heating or cooling capacity delivered by the heat pump is distributed to all of the zones served by this system via the direct air units that supply air to each zone.

The heat pump component is able to model supply air fan operation in two modes: cycling fan – cycling coil (i.e., AUTO fan) and continuous fan – cycling coil (i.e., fan ON). Fan:OnOff must be used to model AUTO fan, while Fan:OnOff or Fan:ConstantVolume can be used to model fan ON. The fan operation mode is specified using a supply air fan operating mode schedule where schedule values of 0 denote cycling fan operation and schedule values other than 0 (a 1 is usually used) denote continuous fan operation. Using this schedule, the supply air fan may be cycled with cooling or heating coil operation or operated continuously based on time of day (e.g. cycling fan operation at night and continuous fan operation during the day).

Several output variables are reported by the heat pump object including fan part-load ratio, compressor part-load ratio, cycling ratio, speed ratio and speed number. Fan part-load ratio is defined as the actual air mass flow rate through the system for the time step divided by the operating supply air mass flow rate specified for the heat pump (![](media/image4832.png) ) at speed 1. Fan part-load ratio is set to 1.0 when the heat pump operates at speeds above 1. The operating supply air mass flow rate may be different for cooling, heating, and when no cooling or heating is required. Compressor part-load ratio is the actual load for the time step divided by the full-load sensible capacity (see Eqn.  or Eqn.). If the defrost strategy is reverse cycle for a DX heating coil, the compressor part-load ratio is the sum of the actual load and the defrost load divided by the full-load sensible capacity. Therefore, the compressor part load ratio for the DX heating coil may be greater than the cycling ratio. This heat pump object also reports the sensible, latent and total cooling and heating rate, as well as the electricity consumption for the unit with separate accounting of auxiliary electric consumption. Furthermore, five report variables related to waste heat recovery are available if the user chooses to model this option.

### Model Description

As described previously, the heat pump is a "virtual" component consisting of a fan, multispeed DX cooling coil, multispeed DX heating coil and supplemental heating coil. The sole purpose of the heat pump model is to properly coordinate the operation of the various system components. The following sections describe the flow of information within the model, as well as the differences between cycling and continuous supply air fan operation.

### Cooling Operation

The description of heat pump cooling operation is divided in two sections: sensible capacity and average supply air flow rate. Actually, the determinations of capacity and supply air flow rate are related, so these calculations are performed in unison.

#### Capacity calculation

If EnergyPlus determines that the heat pump must supply cooling to the control zone to meet the zone air temperature setpoint, then the heat pump model computes the total sensible cooling load (negative) to be delivered to the zones being served based on the control zone sensible cooling load and the fraction of the heat pump air flow that goes through the control zone.

![](media/image4833.png)\


The model then calculates the heat pump's sensible cooling energy rate delivered to the zones being served when the system runs at full-load conditions at the highest speed and when the DX cooling coil is OFF. If the supply air fan cycles with the compressor, then the sensible cooling energy rate is zero when the cooling coil is OFF. However if the fan is scheduled to run continuously regardless of coil operation, then the sensible cooling energy rate will not be zero when the cooling coil is OFF. Calculating the sensible cooling energy rate involves modeling the supply air fan (and associated fan heat) and the multispeed DX cooling coil. The multispeed DX heating coil and the supplemental heating coil are also modeled, but only to pass the air properties and mass flow rate from their inlet nodes to their outlet nodes. For each of these cases (full load at highest cooling speed and DX cooling coil OFF), the sensible cooling energy rate delivered by the heat pump is calculated as follows:

![](media/image4675.png) ![](media/image4676.png)\


where:

*![](media/image4677.png)*  *= air mass flow rate through heat pump at the highest cooling speed [kg/s]*

*h~out, full load~*   = enthalpy of air exiting the heat pump at full-load conditions [J/kg]

*h~control  zone~*   = enthalpy of air leaving the control zone (where thermostat is located) [J/kg]

*HR~min~ =* the minimum humidity ratio of the heat pump exiting air or the air leaving the control zone [kg/kg]

![](media/image4678.png)  = air mass flow rate through the heat pump with the cooling coil OFF [kg/s]

*h~out,coil  off~*    = enthalpy of air exiting the heat pump with the cooling coil OFF [J/kg]

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4679.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the cooling coil OFF conditions

![](media/image4680.png)\


If the heat pump's sensible cooling rate at the highest speed (full load, no cycling) is insufficient to meet the entire cooling load, the controlled zone conditions will not be met. The reported cycling rate and speed ratio are 1, and the speed number is set to the highest index number. If the total sensible cooling load to be met by the system is less than the sensible cooling rate at the highest speed, then the following steps are performed.

Calculate the sensible cooling energy rate at Speed 1

![](media/image4681.png)\


where

*![](media/image4682.png)*  *= air mass flow rate through heat pump at Speed 1 [kg/s]*

Δ~sen,~ ~Speed1~ **= Sensible load difference between the system output node and the zone inlet node at full-load conditions at Speed 1

![](media/image4683.png)\


If the sensible cooling energy rate delivered by the heat pump at Speed 1 is greater or equal to the sensible load, the cycling ratio (part-load ratio) for the heat pump is estimated.

![](media/image4858.png)\


where

AddedFanHeat= generated supply air fan heat, which is a function of part load ratio and as internal component cooling load [W].

AddedFanHeat~Speed1~= generated supply air fan heat at Speed 1 (part load ratio=1) [W].

Since the part-load performance of the DX cooling coil is frequently non-linear,and the supply air fan heat varies based on cooling coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the cooling coil compressor and fan are determined through iterative calculations (successive modeling of the cooling coil and fan) until the heat pump's cooling output matches the cooling load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the heat pump's cooling output divided by the load to be met.

![](media/image4859.png)\


where:

HeatPumpOutput~Cycling~ = heat pump delivered sensible capacity for Speed 1 operating at a specific cycling ratio (W)

![](media/image4860.png)\


where

![](media/image4861.png) = average air mass flow rate defined in the next section [kg/s]

h~out,~    = enthalpy of air exiting the heat pump at part load conditions [J/kg]

Δ~cycling~ = average sensible load difference between the system output node and the zone inlet node

![](media/image4862.png)\


![](media/image4689.png) = Air mass flow rate in the supply inlet node in the controlled zone [kg/s]

For this case where speed 1 operation was able to meet the required cooling load, the speed ratio is set to zero and speed number is equal to 1.

If the heat pump's cooling output at full load for Speed 1 is insufficient to meet the entire cooling load, the Cycling ratio is set equal to 1.0 (compressor and fan are not cycling). Then the cooling speed is increased and the delivered sensible capacity is calculated. If the full load sensible capacity at Speed n is greater than or equal to the sensible load, the speed ratio for the heat pump is estimated:

![](media/image4863.png) Although a linear relationship is assumed by applying the speed ratio to obtain the effective capacity and mass flow rate between speed n and n-1, the outlet air node conditions are dependent on the combined outputs and may not be linear. In addition, the supply air fan heat varies with the speed ratio due to different supply mass flow rates between speed n and n-1 . Therefore, the final speed ratio for the cooling coil compressor and fan are determined through iterative calculations (successive modeling of the cooling coil and fan) until the heat pump's cooling output matches the cooling load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the heat pump's cooling output divided by the load to be met.

![](media/image4864.png)\


where:

HeatPumpOutput~Speed,n~= heat pump delivered sensible capacity between two consecutive speeds at a specific speed ratio (W)

![](media/image4865.png)\


Where

AddedFanHeat~SpeedRatio~= generated supply air fan heat at a specific speed ratio [W]

In this case, the reported cycling ratio is 1 and speed number is equal to n.

#### Air Mass Flow Rate Calculation

Speed 1 operation

If the heat pump has been specified with cycling fan/cycling coil (AUTO fan), then the heat pump's operating supply air mass flow rate is determined by the cycling ratio (PartLoadRatio) for Speed 1. The supply air mass flow rate is multiplied by the cycling ratio to determine the average air mass flow rate for the system simulation time step. The air conditions at nodes downstream of the cooling coils represent the full-load (steady-state) values when the coil is operating.

![](media/image4866.png)\


If the fan operates continuously (i.e., when the supply air fan operating mode schedule values are NOT equal to 0), the operating air mass flow rate through the heat pump is calculated as the average of the user-specified air flow rate when the heat pump cooling coil is ON at Speed 1 and the user-specified air flow rate when the heat pump cooling coil is OFF (user-specified supply air volumetric flow rates converted to dry air mass flow rates).

![](media/image4867.png)\


where:

![](media/image4868.png)  = average air mass flow rate through heat pump [kg/s]

![](media/image4696.png) = air mass flow rate through heat pump when cooling coil is ON at Speed 1 [kg/s]

![](media/image4697.png)  = air mass flow rate through heat pump when no heating or cooling is needed [kg/s]

In this case, the air conditions at nodes downstream of the cooling coils are calculated as the average conditions over the simulation time step (i.e., the weighted average of full-load conditions when the coil is operating and inlet air conditions when the coil is OFF).

#### Higher Speed Operation

When the heat pump operates at higher speeds to meet the required cooling load, the supply air mass flow rate is linearly interpolated between two consecutive speeds:

![](media/image4869.png)\


where:

![](media/image4870.png) = average air mass flow rate through the heat pump for the time step [kg/s]

![](media/image4700.png) = air mass flow rate through heat pump when cooling coil is ON at Speed n [kg/s]

![](media/image4701.png) = air mass flow rate through heat pump when cooling coil is ON at Speed n-1 [kg/s]

For this case of higher speed operation, the air conditions at nodes downstream of the cooling coils are determined by the delivered cooling capacity and supply air mass flow rates between two consecutive speeds.

Although the above sections present the capacity and air mass flow rate calculation separately, they are dependent and change every iteration until convergence is reached for the time step being simulated.

### Heating Operation

The description of heat pump heating operation is divided in two sections: total (sensible) capacity and average supply air flow rate. Actually, the determinations of capacity and supply air flow rate are related, so these calculation are performed in unison.

#### Capacity calculation

If EnergyPlus determines that the heat pump must supply heating to the control zone to meet the zone air temperature setpoint, then the heat pump model computes the total sensible heating load (positive) to be delivered to the zones being served based on the control zone sensible heating load and the fraction of the heat pump air flow that goes through the control zone.

![](media/image4842.png)\


The model then calculates the heat pump's sensible heating energy rate delivered to the zones being served when the system runs at full-load conditions at the highest speed and when the DX heating coil is OFF (without supplemental heater operation in either case). If the supply air fan cycles with the compressor, then the sensible heating energy rate is zero when the compressor is OFF. However if the fan is scheduled to run continuously regardless of coil operation, then the sensible heating energy rate will not be zero when the compressor is OFF. Calculating the sensible heating energy rate involves modeling the supply air fan (and associated fan heat), the DX cooling coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node), the DX heating coil, and the supplemental heating coil (simply to pass the air properties and mass flow rate from its inlet node to its outlet node). For each of these cases (full load and DX heating coil OFF, without supplemental heater operation in either case), the sensible heating energy rate delivered by the heat pump is calculated as follows:

![](media/image4713.png)\


![](media/image4714.png)\


where:

*![](media/image4715.png)*  *= air mass flow rate through heat pump at the highest heating speed [kg/s]*

*h~out, full load~*   = enthalpy of air exiting the heat pump at full-load conditions [J/kg]

*h~control  zone~*   = enthalpy of air leaving the control zone (where thermostat is located) [J/kg]

*HR~min~ =* enthalpies evaluated at a constant humidity ratio, the minimum humidity ratio of the heat pump exiting air or the air leaving the control zone

![](media/image4716.png) = air mass flow rate through the heat pump with the heating coil OFF [kg/s]

*h~out,coil  off~*    = enthalpy of air exiting the heat pump with the heating coil OFF [J/kg]

Δ~sen,~ *~full load~* = Sensible load difference between the system output node and the zone inlet node at full-load conditions

![](media/image4717.png)\


where:

Frac = Control zone air fraction with respect to the system mass flow rate

Δ~sen,~~coil off~ **= Sensible load difference between the system output node and the zone inlet node with the heating coil OFF conditions

![](media/image4718.png)\


If the heat pump's DX heating coil output full load at the highest speed is insufficient to meet the entire heating load, the remaining heating load is passed to the supplemental heating coil. If the heat pump model determines that the outdoor air temperature is below the minimum outdoor air temperature for compressor operation (specified by the user), the compressor is turned off and the entire heating load is passed to the supplemental gas or electric heating coil. The heat pump exiting air conditions and energy consumption are calculated and reported by the individual component models (fan, DX heating coil, and supplemental gas or electric heating coil).

If the total heating load to be met by the system is less than the sensible heating rate at the highest speed, then the following steps are performed.

Calculate the sensible heating energy rate at Speed 1

![](media/image4719.png)\


where:

*![](media/image4720.png)*  *= air mass flow rate through heat pump at Speed 1 [kg/s]*

Δ~sen,~ ~Speed1~ **= Sensible load difference between the system output node and the zone inlet node at full-load conditions at Speed 1

![](media/image4721.png)\


If the sensible heating energy rate delivered by the heat pump at Speed 1 is greater or equal to the sensible load, the cycling ratio (part-load ratio) for the heat pump is estimated.

![](media/image4871.png)\


where

AddedFanHeat= generated supply air fan heat, which is a function of part load ratio and as internal component heating load [W].

AddedFanHeat~Speed1~= generated supply air fan heat at Speed 1 (part load ratio=1) [W].

Since the part-load performance of the DX heating coil is frequently non-linear (Ref: Single-Speed Electric Heat Pump DX Air Heating Coil), and the supply air fan heat varies based on heating coil operation for the case of cycling fan/cycling coil (AUTO fan), the final part-load ratio for the heating coil compressor and fan are determined through iterative calculations (successive modeling of the heating coil and fan) until the heat pump's heating output matches the heating load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the heat pump's heating output divided by the load to be met.

![](media/image4872.png)\


where:

HeatPumpOutput~Cycling~= heat pump delivered sensible capacity for Speed 1 operating at a specific cycling ratio (W)

![](media/image4873.png)\


where

![](media/image4874.png) = average air mass flow rate defined in the next section [kg/s]

h~out,~    = enthalpy of air exiting the heat pump at part load conditions [J/kg]

Δ~cycling~ = average sensible load difference between the system output node and the zone inlet node

![](media/image4875.png)\


![](media/image4727.png) = Air mass flow rate in the supply inlet node in the controlled zone [kg/s]

For this case where speed 1 operation was able to meet the required heating load, the speed ratio is set to zero and speed number is equal to 1.

If the heat pump's heating output at full load for Speed 1 is insufficient to meet the entire heatling load, the Cycling ratio (PartLoadRatio) is set equal to 1.0 (compressor and fan are not cycling). Then the heating speed is increased and the delivered sensible capacity is calculated. If the full load sensible capacity at Speed n is greater than or equal to the sensible load, the speed ratio for the heat pump is estimated:

![](media/image4876.png)\


Although a linear relationship is assumed by applying the speed ratio to obtain the effective capacity and air mass flow rate between speed n and n-1, the outlet node conditions are dependent on the combined outputs and may not be linear. In addition, the supply air fan heat varies based on heating coil operation for the case of cycling fan/cycling coil (AUTO fan). Therefore, the final speed ratio for the heating coil compressor and fan are determined through iterative calculations (successive modeling of the heating coil and fan) until the heat pump's heating output matches the heating load to be met within the convergence tolerance. The convergence tolerance is fixed at 0.001 and is calculated based on the difference between the load to be met and the heat pump's heating output divided by the load to be met.

![](media/image4877.png)\


where:

HeatPumpOutput~Speed~~Ratio~= heat pump delivered sensible capacity between two consecutive speeds at a specific ratio [W]

![](media/image4878.png)\


Where

AddedFanHeat~SpeedRatio~= generated supply air fan heat at a specific speed ratio [W]

In this case, the reported cycling ratio is 1 and speed number is equal to n.

#### Air Mass Flow Rate Calculation

The air mass flow rate calculations during heating operation are the same as those described above for cooling operation.

### Fan Placement

Supply air fan placement impacts the iteration strategy. When the fan placement type is blow through, the air mass flow rate and coil part load factor (PLF) affect the fan outlet conditions. Since the fan is upstream of the coil components with this fan placement, the fan outlet conditions are calculated without knowing the next component's performance at the beginning of each iteration. DX coil performance is strongly dependent on the inlet conditions, so without correct inlet conditions the DX coil components may not be simulated correctly. Therefore, the heat pump components are called twice for each iteration when fan placement is ‘blow through'. The correct part load factor for the fan component is obtained after the first call, so that the more realistic fan outlet conditions are used to simulate the coil performance in the second call. This extra call to the heat pump components is not required for the draw through fan since the supply air fan is located downstream of the DX coils with this fan placement.

### Waste Heat Calculation

When the heat recovery is active (the value of the Design Heat Recovery Water Flow Rate field is greater than 0), the outlet node temperature of heat recovery is calculated based on the recoverable waste heat generated by its child objects (Coil:Cooling:DX:MultiSpeed and Coil:Heating:DX:MultiSpeed):

![](media/image4746.png)\


where

T~outlet~= outlet node temperature of heat recovery, C

T~in~~let~= inlet node temperature of heat recovery, C

Q~WasteHeat~= recoverable waste heat generated by its child objects, W

C~p~= inlet node temperature of heat recovery, C

![](media/image4747.png) = mass flow rate of heat recovery, kg/s

If the outlet node temperature is above the value of the Maximum Temp for Heat Recovery field, the outlet node temperature is reset to the value of Maximum Temp for Heat Recovery.

## DX Cooling Package

### Overview

The DX cooling package subsystem model provides a "virtual" component that consists of a DX air cooling coil component as shown in the figure below. The CoilSystem:Cooling:DX input object provides a container component that controls the associated DX cooling coil which is specified in a separate object. This coil can be specified anywhere in the air loop simulation and is controlled by the setpoint(s) on the control node. Optional dehumidification controls may also be specified. CoilSystem:Cooling:DX assumes continuous fan operation for any given time step, because it is not able to control cycling fan operation in the way that other AirLoopHVAC:Unitary\* systems can.

This system does not need any plant specification and will provide the electric consumption necessary to operate the compressor and the condenser fan.

![Schematic of Packaged DX Cooling Subsystem in Air Loop for a Blow-Thru Application](media/schematic-of-packaged-dx-cooling-subsystem-in.png)


### Controls

The system calculates the current sensible load using the temperature of the inlet node and the System Node Setpoint Temp on the control node. If the control node is not the outlet node, the desired outlet node temperature is adjusted for the current temperature difference between the outlet node and the control node. Likewise, the current latent load is calculated using the humidity ratio of the inlet node and the System Node Humidity Ratio Max on the control node. The controls determine the required coil run-time fraction and dehumidification mode (if applicable) using the steps outlined below.

#### Step 1 – Meet Sensible Load Requirement

The controls first attempt to meet the sensible requirement. The specified DX coil model is called with a part-load ratio (PLR) of 1.0 to determine the full-load output of the coil. This is compared with the desired outlet node temperature and a sensible PLR is calculated. If the PLR is <1.0, a Regula-Falsi iteration routine is called to determine the coil run-time fraction which results in the desired outlet node temperature. For a variable-speed DX cooling coil, if the load is smaller than the sensible capacity at the lowest speed, the coil run-time fraction is determined in the same way as a single-speed DX cooling coil. Otherwise, its speed level and speed ratio between two neighboring speeds are selected to match the load.

> If the cooling coil type is specified as CoilSystem:Cooling:DX:CoolingHeatExchanganerAssisted and the dehumidification control type is specified as CoolReheat, the heat exchanger is active during this attempt to meet the sensible requirement.

#### Step 2 – Meet Latent Load Requirement (if activated)

If dehumidification controls are active, the leaving humidity ratio resulting from operation to meet the sensible load (Step 1 above) is compared with the desired outlet node humidity ratio. If the humidity requirement is already met, then no further control action is taken. If the humidity requirement has not been met, then the coil is re-simulated depending on the type of humidity control.

#### Step 2a – Humidity Control = MultiMode

If the humidity control type is MultiMode, then the coil's enhanced dehumidification mode is activated when the coil type is Coil:Cooling:DX:TwoStageWithHumidityControlMode or the heat exchanger is activated when the coil type is CoilSystem:Cooling:DX:CoolingHeatExchangerAssisted and Step 1 above is repeated to meet the sensible load using the coil performance resulting from the enhanced dehumidificaiton mode. This is a semi-passive approach to dehumidification which may fall short or may exceed the dehumidification requirement. If the user has specified Run on Latent Load = Yes in the CoilSystem:Cooling:DX object, and there is no sensible load to be met, then the system will try to meet the entire dehumidification load. If dehumidification mode should not be active when there is no sensible load, then choose Run on Latent Load = No.

#### Step 2b – Humidity Control = CoolReheat

If the humidity control type is CoolReheat, the coil is re-simulated to achieve the desired outlet node humidity ratio. This option is valid for all cooling coil types. When the coil type is Coil:Cooling:DX:TwoStageWithHumidityControlMode, only the cooling performance mode is used for this step and enhanced dehumidification mode is not activated.

## DX Heating Package

### Overview

The DX heating package subsystem model provides a "virtual" component that consists of a DX air-to-air heating coil component and associated controls. The CoilSystem:Heating:DX input object provides a container component that controls the associated DX heating coil  which is specified in a separate object. See the entry called Single-Speed and Variable-Speed Electric Heat Pump DX Air Heating Coil for details of the coil model itself.  This coil can be specified anywhere in the air loop simulation and is controlled by the setpoint on the heating coil's outlet node. This model is analogous to the model for CoilSystem:Cooling:DX which is similar but for cooling.  The model assumes continuous fan operation for any given time step, because it is not able to control cycling fan operation in the way that other AirLoopHVAC:Unitary\* systems can.

This system does not need any plant specification and will provide the electric consumption necessary to operate the compressor and the condenser fan.

### Controls

The DX heating package system calculates how the coil should operate to meet current sensible load using the temperature of the inlet node and the System Node Setpoint Temp on the outlet node. The controls determine the required coil run-time fraction to meet the sensible requirement. The Single-Speed Electric Heat Pump DX Air Heating Coil model is called with a part-load ratio (PLR) of 1.0 to determine the full-load output of the coil. If the PLR is <1.0, a Regula-Falsi iteration routine is called to determine the coil run-time fraction which results in the desired outlet node temperature. For a variable-speed DX heating coil, if the load is smaller than the heating capacity at the lowest speed, the coil run-time fraction is determined in the same way as a single-speed DX heating coil. Otherwise, its speed level and speed ratio between two neighboring speeds are selected to match the load.

## Desiccant Dehumidifier Package

### Overview

The input object Dehumidifier:Desiccant:System provides a model that packages components for a desiccant-based subsystem that dehumidifies an air stream, normally called the process air stream. A second heated air stream, called the regeneration air stream, is used to remove the collected moisture from the desiccant heat exchanger and this moisture-laden air is then usually exhausted from the building. This Dehumidifier:Desiccant:System object is similar to the Dehumidifier:Desiccant:NoFans object but has some additional modeling capabilities.

The Dehumidifier:Desiccant:System desiccant dehumidifier object in EnergyPlus is a compound object that can be placed anywhere in an air loop. Common locations for this object are in an AirLoopHVAC:OutdoorAirSystem or in the main air loop downstream of a cooling coil (postcooling desiccant dehumidifier). This compound object coordinates the operation of several ‘children' objects: a desiccant heat exchanger, a regeneration air fan, and an optional regeneration air heater. If this dehumidifier is placed in the main air loop immediately downstream of a direct expansion (DX) cooling coil, then the dehumidifier's operation can be coordinated with the operation of the companion DX coil and it is also possible to specify that the DX system's condenser waste heat can be used to help regenerate the desiccant heat exchanger. For the case of condenser waste heat regeneration, an optional exhaust fan can also be modeled by this desiccant dehumidifier compound object to help maintain a setpoint temperature for air entering the regeneration side of the desiccant heat exchanger. Refer to the EnergyPlus Input Output Reference for the specific input syntax for this desiccant dehumidifier object.

![Schematic of a Desiccant Dehumidifier with Draw Through Regeneration Fan Placement](media/schematic-of-a-desiccant-dehumidifier-with.jpeg)


![Schematic of a Desiccant Dehumidifier in Blow Through Regeneration Fan Placement](media/schematic-of-a-desiccant-dehumidifier-in-blow.jpeg)


### Control Logic

The model first decides if the dehumidifier can operate for the simulation time step, based on its availability schedule and if there is air flow detected on the process air inlet node. If available to operate, then the target humidity ratio for the process outlet air is determined based on the maximum humidity ratio setpoint on the control node (System Node Setpoint Humidity Ratio Max). If the control node is not the process air outlet node, the target humidity ratio is adjusted for the current humidity ratio difference between the process air outlet node and the control node. If the humidity ratio of the process inlet air is greater than the target humidity ratio for the process outlet air, then the dehumidifier operates to meet the target to the extent possible.

Once it is determined that the dehumidifier should operate, the components upstream of the desiccant heat exchanger's regeneration inlet are modeled. The actual components that are modeled depend on the configuration specified by the user.

If the waste heat from the companion cooling coil is being used to heat the regeneration air, then this model determines the temperature of the air leaving the cooling coil condenser. The user may also specify a setpoint temperature for the regeneration air entering the desiccant heat exchanger and an optional exhaust fan, which can impact the condenser leaving air temperature. For this case, the exhaust fan is used to control the condenser leaving air temperature by boosting the air flow rate through the condenser to meet the setpoint temperature (minus regeneration fan heat if blow through fan placement).

> Note: If the desiccant dehumidifier is OFF for a simulation time step but its companion cooling coil is operating and is specified to provide regeneration air heating, then the exhaust fan operates at the maximum air flow rate (i.e., this fan serves at the condenser fan for the companion cooling coil system when regeneration air heating is specified, so the inputs for the companion cooling coil object should not include the condenser fan energy since the condenser fan energy is modeled by the Dehumidifier:Desiccant:SystemDesiccant Dehumidifier object).

The exhaust fan power is determined as follows:

![](media/image4882.png)\


where:

![](media/image4883.png) = part load ratio of the exhaust fan

![](media/image4884.png) = exhaust fan maximum volumetric flow rate, user input (m^3^/s)

![](media/image4885.png) = density of air at standard temperature and pressure [dry air at 20°C] (m^3^/kg)

![](media/image4886.png) = exhaust fan maximum power, user input (W)

![](media/image4887.png) = exhaust fan power modifier curve evaluated at PLR~exhaust~. If modifier                    curve not provided by the user, then this factor is assumed to be 1.0.

![](media/image4888.png)      = part load ratio of the companion cooling coil

![](media/image4889.png) = output variable ‘Dehumidifier Exhaust Fan Electric Power, W'

The exhaust fan electric consumption is then calculated as:

![](media/image4890.png)\


where:

![](media/image4891.png)    = output variable ‘Dehumidifier Exhaust Fan Electric Energy, J'

![](media/image4892.png) = HVAC system simulation time step, hr

Once the outlet conditions from the companion coil condenser are determined (if present), then the regeneration air fan (if blow through configuration) and regeneration air heater (if present) are simulated. To the extent possible (e.g., if the heater is available to operate based on its availability schedule and it has sufficient heating capacity), the regeneration air heater operates to raise its outlet air temperature to the specified regeneration inlet air setpoint temperature.

With the inlet air conditions to the regeneration and process sides of the desiccant heat exchanger now known, the performance of the desiccant heat exchanger is modeled. If the desiccant dehumidifier is specified with a companion cooling coil upstream of the heat exchanger's process inlet, then the model assumes that the dehumidifier operates at the same time as the companion coil (same part-load ratio). If not, then the desiccant dehumidifier model calculates the fraction of time it must operate to meet the process outlet air maximum humidity target (setpoint) using the following equation:

![](media/image4893.png)\


where:

![](media/image4894.png)  = output variable ‘Dehumidifier Part Load Ratio'

![](media/image4895.png) = process inlet air humidity ratio (kg/kg)

![](media/image4896.png) = target humidity ratio (setpoint) for the process outlet air (kg/kg)

![](media/image4897.png) = process outlet air humidity ratio when the desiccant heat exchanger operates     (kg/kg)

After all of the desiccant dehumidifier components are modeled at the appropriate part load ratio, the water removal rate and water removed are calculated.

![](media/image4898.png)\


where:

![](media/image4899.png)  = output variable ‘Dehumidifier Removed Water Mass Flow Rate, kg/s'

![](media/image4900.png) = air mass flow rate at the process air inlet node (kg/s)

![](media/image4901.png) = process outlet air humidity ratio (kg/kg)

![](media/image4902.png) = output variable ‘Dehumidifier Removed Water Mass, kg'

### References

ASHRAE. 2004. Chapter 22: Desiccant Dehumidification and Pressure-Drying Equipment. 2004 ASHRAE HVAC Systems and Equipment Handbook. Atlanta, GA: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc. http://www.ashrae.org

Kosar, D. 2006. Dehumidification Enhancements, ASHRAE Journal, Vol. 48, No. 2, February 2006. http://www.ashrae.org

Kosar, D. et al. 2006. Dehumidification Enhancement of Direct Expansion Systems Through Component Augmentation of the Cooling Coil. 15^th^ Symposium on Improving Building Systems in Hot and Humid Climates, July 24-26, 2006. http://www.hothumidsymposium.org/

## Unitary Water-To-Air Heat Pump

### Overview

The input object AirLoopHVAC:UnitaryHeatPump:WaterToAir provides a model for a water-to-Air heat pump that is a "virtual" component that consists of an on/off fan component, a water-to-air heat pump cooling coil, a water-to-air heat pump heating coil, and a gas or electric supplemental heating coil. The specific configuration of the blowthru heat pump is shown in the following figure. For a drawthru heat pump, the fan is located between the water-to-air heat pump heating coil and the supplemental heating coil. The configuration of the water-to-air heat pump in the air loop is similar to an air-to-air heat pump. In addition, a water-to-air heat pump has a water loop connection on its source side. The water loop can be served by a condenser loop (like GHE for Ground source systems), or by a cooling tower/ boiler plant loop (for water loop systems).

![Source Side and Load Side Configuration of a BlowThru WateroToAir Heat Pump](media/source-side-and-load-side-configuration-of-a.jpeg)


There are two different models for water-to-air heat pump cooling and heating coils. Cooling and heating coils are modeled using a Single Speed or Variable Speed Equation Fit model or a Parameter Estimation model. The parameter estimation procedure is quite detailed and the equation fit model is designed to simplify the user inputs. Each model is discussed in short in the following sections. The Variable-Speed Equation Fit model is described in a separate section, as Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit and Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit.

### Single Speed Equation-Fit Model:

This section describes the equation-fit model for Water-to-Air heat pump  (Object names: **Coil:Cooling:WaterToAirHeatPump:EquationFit** and **Coil:Heating:WaterToAirHeatPump:EquationFit**). This documentation is derived from the M.S. dissertation of Tang (2005) which is available on the Oklahoma State University web site http://www.hvac.okstate.edu/. The model uses five non-dimensional equations or curves to predict the heat pump performance in cooling and heating mode. The methodology involved using the generalized least square method to generate a set of performance coefficients from the catalog data at indicated reference conditions. Then the respective coefficients and indicated reference conditions are used in the model to simulate the heat pump performance. The variables or inlet conditions that influenced the water-to-air heat pump performance are load side inlet water temperature, source side inlet temperature, source side water flow rate and load side water flow rate. The governing equations for the cooling and heating mode are as following:

Cooling Mode:

![](media/image4904.png)\


![](media/image4905.png)\


![](media/image4906.png)\


Heating Mode:

![](media/image4907.png)\


![](media/image4908.png)\


Assuming no losses, the source side heat transfer rate for cooling and heating mode is calculated as following;

![](media/image4909.png)\


![](media/image4910.png)\


where:

![](media/image4911.png)   = Equation fit coefficients for the cooling and heating mode

![](media/image4912.png)  = 283K

![](media/image4913.png) = Entering water temperature, K

![](media/image4914.png) = Entering air dry-bulb temperature, K

![](media/image4915.png) = Entering air wet-bulb temperature, K

![](media/image4916.png)  = Load side air volumetric flow rate, m^3^/s

![](media/image4917.png)  = Source side water volumetric flow rate, m^3^/s

![](media/image4918.png)  = Total cooling capacity, W

![](media/image4919.png)  = Sensible cooling capacity, W

![](media/image4920.png)  = Power consumption (cooling mode), W

![](media/image4921.png)  = Source side heat transfer rate (cooling mode), W

![](media/image4922.png)  = Total heating capacity, W

![](media/image4923.png)  = Power consumption (heating mode), W

![](media/image4924.png)  = Source side heat transfer rate (heating mode), W

The inlet conditions or variables are divided by the reference conditions. This formulation allows the coefficients to fall into smaller range of values. Moreover, the value of the coefficient indirectly represents the sensitivity of the output to that particular inlet variable. The reference conditions used when generating the performance coefficients must be the same as the reference conditions used later in the model. The reference temperature ![](media/image4925.png) is fixed at 283K. Temperature unit of Kelvin is used instead of Celsius to keep the ratio of the water inlet temperature and reference temperature positive value should the water inlet temperature drop below the freezing point.

For cooling mode, the reference conditions; reference load side air volumetric flow rate ![](media/image4926.png) ,reference source side water volumetric flow rate![](media/image4927.png) ,reference sensible capacity ![](media/image4928.png)  and reference power input ![](media/image4929.png)  are the conditions when the heat pump is operating at the highest cooling capacity or reference cooling capacity![](media/image4930.png)  indicated in the manufacturer's catalog. Note that the reference conditions for heating mode might differ from the reference conditions specified for the cooling mode.

### Coefficient estimation procedure:

The generalized least square method is used to generate the coefficients. This method utilizes an optimization method which calculates the coefficients that will give the least amount of differences between the model outputs and the catalog data. A set of coefficients for the cooling mode is generated which includes A1-A5 for total cooling capacity, B1-B6 for sensible cooling capacity, and C1-C5 for power consumption. The same procedure is repeated for the heating mode to generate the coefficients E1-E5 and F1-F5. An information flow chart showing the inputs, reference conditions, performance coefficients and outputs are shown in the figure below:

![Information Flow Chart for Water-to-Air Heat Pump Equation Fit Model (Tang 2005)](media/information-flow-chart-for-water-to-air-heat.png)


### High Humidity Control with WaterToAir HeatPump Equation Fit model

The specific configuration of the WaterToAir HeatPump with supplemental heating coil is shown above (see Figure 234). This figure shows the fan placement when a blow through fan is specified. If a draw through fan is specified, the fan is located between the heating coil and the reheat coil. The system is controlled to keep the high relative humidity in the control zone from exceeding the setpoint specified in the object ZoneControl:Humidistat. When high humidity control is specified and the compressor operates, the heatpump always operates at the cooling air flow rate when a zone heating load is present as determined by the zone thermostat. High humidity control is specified as either None, or CoolReheat in the Dehumidification Control Type input field. CoolReheat is specified when a DX cooling coil is used to over-cool the supply air stream in order to meet the zone latent load. In this case, a supplemental heating coil will ensure the zone temperature does not fall below the zone heating temperature set point. If the dehumidification control type is selected as None, the WaterToAir HeatPump uns only to meet the sensible cooling load. A supplemental heating coil is required for all dehumidification control types.

The model first calculates the *PartLoadRatio* required meeting the sensible cooling load.  The heatpump's sensible cooling load is determined from the control zone sensible cooling load to the cooling setpoint and the control zone air flow fraction to maintain the dry-bulb temperature setpoint in the control zone:

![](media/image4932.png)\


The heatpump's sensible cooling load to be met and the full load cooling output are used to calculate the sensible the part-load ratio iteratively based on user specified convergence criterion.

![](media/image4933.png)\


When the heat pumps sensible cooling capacity meets the system sensible cooling load at a given sensible part load ratio, then the Heat pump meets the controlled zone cooling setpoint temperature.  If a moisture (latent) load exists because the control zone humidity has exceeded the setpoint, the total moisture load to be met by the heat pumps (HeatPumpMoistureLoad) is calculated based on the control zone moisture load and the control zone air flow fraction.

![](media/image4934.png)\


Then the *LatentPartLoadRatio* required to meet the high humidity setpoint is calculated as follows:

![](media/image4935.png)\


The model uses the greater of the two part-load ratios, *PartLoadRatio* or *LatentPartLoadRatio*, to determine the operating part-load ratio of the Heat Pump's DX cooling coil.

![](media/image4936.png)\


As previously described, iterations are performed to converge on the solution within the convergence tolerance.

Where,

![](media/image4937.png) = the control zone sensible cooling load to the cooling setpoint, (W).

![](media/image4938.png) = the control zone moisture load to the dehumidifying relative humidity setpoint, (W).

![](media/image4939.png)  = the supply air fraction that goes though the control zone, (-).

*FullLatentOutput* =the Heat Pump's latent cooling energy rate at full-load conditions, W

*NoLatentOutput* =the Heat Pump's latent cooling energy rate with cooling coil OFF, W

![](media/image4940.png) =the heat pump's part-load-ratio required to meet system sensible load, (-).

![](media/image4941.png) =the heat pump's part-load-ratio required to meet system moisture load, (-).

![](media/image4942.png) *=the minimum part-load ratio, which is usually 0.0. For the case when the latent capacity degradation model is used (Ref: DX Cooling Coil Model), this value is the minimum part-load ratio at which the cooling coil will dehumidify the air.*

When the predicted zone air temperature is above the heating setpoint and if there is a dehumidification load, the supplemental heating coil load is required to offset the excess cooling as shown in Figure 236. If the model determines that the LatentPartLoadRatio is to be used as the operating part-load ratio of the heatpump's cooling coil, the supplemental coil is used to offset the excess sensible capacity provided by the unit. The model first checks the sensible load that exists for the current simulation time step (predicted zone temperature with no HVAC operation compared to the thermostat setpoint temperatures). If a sensible cooling load or no sensible cooling or heating load exists (see Figure 236),  the model calculates the difference between the sensible heating load required to reach or maintain the heating dry-bulb temperature setpoint and the actual sensible cooling energy rate delivered by the heat pump (with LatentPartLoadRatio). In this case, the supplemental heating coil is used to offset the excess sensible cooling energy provided by the DX cooling coil (if any) that could have caused an overshoot of the heating dry-bulb temperature setpoint. Note that when a humidistat is used and high humidity control is required, the zone dry-bulb temperature will typically move toward the heating temperature setpoint when a high moisture (latent) load exists.

![Supplemental heating coil load when predicted zone air temperature is above the heating Setpoint](media/supplemental-heating-coil-load-when-predicted-002.jpeg)


If a heating load exists (Figure 237), the supplemental heating coil is used to meet the heating coil load and at the same time offset the entire sensible cooling energy rate of the DX cooling coil (to meet the humidistat setpoint). Note that when a heating load exists and high humidity control is required, the heat pump operates at the user-specified cooling air flow rate for the entire simulation time step. As with the fan, and DX cooling coil, report variables associated with supplemental heating coil performance (e.g., heating coil energy, heating coil rate, heating coil gas or electric energy, heating coil runtime fraction, etc.) are managed in the supplemental (heating) coil object.

![Supplemental heating coil load when predicted zone air temperature is below the heating setpoint](media/supplemental-heating-coil-load-when-predicted-003.jpeg)


### Parameter Estimation Model

The steady state simulation model (Object: AirLoopHVAC:UnitaryHeatPump:WaterToAir) for a water-to-air vapor compression heat pump is described in this section. The model is implemented under the air-loop manager similar to the algorithm discussed under AirLoopHVAC:UnitaryHeatPump. The heat pump 'coil' objects (Coil:Cooling:WaterToAirHeatPump:ParameterEstimation andCoil:Heating:WaterToAirHeatPump:ParameterEstimation) actually consist of a steady state simulation of the unitary heat pump in cooling or heating mode respectively.  This documentation is derived from the Ph.D. dissertation of Hui Jin which is available on the Oklahoma State University web site http://www.hvac.okstate.edu/.  The model parameters, solution technique and solution algorithm are all identical to that of the water to water heat pump.  The only difference between the two models is in the modeling of the water to air heat exchanger.

The effectiveness of this heat exchanger is given by

![](media/image4945.png)\


Where NTU is defined by

![](media/image4946.png)\


UA for the source side and the load side are two of the parameters estimated in the parameter estimation procedure.

Additional model details and a discussion of the parameters and parameter estimation technique are described in the following section on Water to Water Heat Pumps.

## Water To Water Heat Pumps

There are two water-water heat pump models available in EnergyPlus which are **parameter estimation based model** and **equation-fit model**. Detailed descriptions of the model are available in the references Tang and Jin

## Equation Fit Water To Water Heat Pump Model

This section describes the equation-fit model for water-to-water heat pump.   (Object names: HeatPump:WaterToWater:EquationFit:Cooling & HeatPump:WaterToWater:EquationFit:Heating). This documentation is derived from the M.S. dissertation of Tang (2005) which is available on the Oklahoma State University web site http://www.hvac.okstate.edu/. The model uses four non-dimensional equations or curves to predict the heat pump performance in cooling and heating mode. The methodology involved using the generalized least square method to generate a set of performance coefficients from the catalog data at indicated reference conditions. Then the respective coefficients and indicated reference conditions are used in the model to simulate the heat pump performance. The variables that influenced the water-to-water heat pump performance are load side inlet water temperature, source side inlet temperature, source side water flow rate and load side water flow rate. The governing equations for the cooling and heating mode are as following:

Cooling Mode:

![](media/image4947.png)\


![](media/image4948.png)\


Heating Mode:

![](media/image4949.png)\


![](media/image4950.png)\


Assuming no losses, the source side heat transfer rate for cooling and heating mode is calculated as following;

![](media/image4951.png)\


![](media/image4952.png)\


Where:

![](media/image4953.png)   = Equation fit coefficients for the cooling and heating mode

![](media/image4954.png)  = 283.15K

![](media/image4955.png) = Entering load side water temperature, K

![](media/image4956.png) = Entering source side water temperature, K

![](media/image4957.png)  = Load side volumetric flow rate, m^3^/s

![](media/image4958.png)  = Source side volumetric flow rate, m^3^/s

![](media/image4959.png)  = Load side heat transfer rate (cooling mode), W

![](media/image4960.png)  = Power consumption (cooling mode), W

![](media/image4961.png)  = Source side heat transfer rate (cooling mode), W

![](media/image4962.png)  = Load side heat transfer rate (heating mode), W

![](media/image4963.png)  = Power consumption (heating mode), W

![](media/image4964.png)  = Source side heat transfer rate (heating mode), W

*COP~c~*= Cooling coefficient of performance, W/W

*COP~h~*= Heating coefficient of performance, W/W

If the load side heat transfer rate (*Q~c~* or *Q~h~*) or power consumption (*Power~c~* or *Power~h~*) are less than or equal to zero, then the heat pump is turned off for that simulation time step, a warning is issued, and the simulation continues.

The inlet conditions or variables are divided by the reference conditions. This formulation allows the coefficients to fall into smaller range of values. Moreover, the value of the coefficient indirectly represents the sensitivity of the output to that particular inlet variable. The reference conditions used when generating the performance coefficients must be the same as the reference conditions used later in the model. The reference temperature ![](media/image4965.png) is fixed at 283K. Temperature unit of Kelvin is used instead of Celsius to keep the ratio of the water inlet temperature and reference temperature positive value should the water inlet temperature drop below the freezing point.

For cooling mode, the reference conditions; reference load side volumetric flow rate, ![](media/image4966.png) , reference source side volumetric flow rate, ![](media/image4967.png) , ![](media/image4968.png)   and reference source side heat transfer rate, ![](media/image4969.png)   are the conditions when the heat pump is operating at the highest cooling capacity or reference cooling capacity, ![](media/image4970.png)  indicated in the manufacturer's catalog. Note that the reference conditions for heating mode might differ from the reference conditions specified for the cooling mode.

### Coefficient estimation procedure:

The generalized least square method is used to generate the coefficients. This method utilizes an optimization method which calculates for the coefficients that will give the least amount of differences between the model outputs and the catalog data. A set of coefficients for the cooling mode is generated which includes A1-A5 for load side heat transfer. The same procedure is repeated for the heating mode to generate the coefficients B1-B5. An information flow chart showing the inputs, reference conditions, performance coefficients and outputs are shown in the figure below:

![Information Flow Chart for Water-To-Water Heat Pump Equation Fit (Tang 2005)](media/information-flow-chart-for-water-to-water.png)


## Parameter Estimation Water-To-Water Heat Pump Model

A steady state simulation model for a water-to-water reciprocating vapor compression heat pump (Object names: HeatPump:WaterToWater:ParameterEstimation:Cooling & HeatPump:WaterToWater:ParameterEstimation:Heating) is described in this section. This documentation is derived from the Ph.D. dissertation of Hui Jin which is available on the Oklahoma State University web site http://www.hvac.okstate.edu/. The model incorporates a multivariable unconstrained optimization algorithm to estimate several unspecific parameters. The aim of the model is to describe the detailed physical geometry and operation of each component and replicate the performance of the actual unit in operation. Assuming the thermodynamic process in the expansion device and the pressure drop at the suction and discharge valves to be isenthalpic the heat balance equation is given by

![](media/image4972.png)\


Where:

![](media/image4973.png)  = Source side heat transfer rate

![](media/image4974.png)  = Load side heat transfer rate

![](media/image4975.png) = Compressor power input

The compressor model is based on an isentropic process shown in Figure 239 governed by

![](media/image4976.png)  = Constant

Where:

P = Pressure

v = Specific volume

![Schematic indicator diagram for a reciprocating Compressor(Jin 2002)](media/schematic-indicator-diagram-for-a.png)


Since the refrigerant vapor in the clearance volume as shown in the previous figure goes through a re-expansion procedure, the mass flow rate of the compressor refrigerant is a decreasing function of the pressure ratio.

![](media/image4978.png)\


Where:

![](media/image4979.png)   = refrigerant mass flow rate

PD = Piston displacement

C   = Clearance factor

![](media/image4980.png)  = discharge pressure

![](media/image4981.png)  = Suction pressure

![](media/image4982.png)      = Isentropic exponent

### Parameter estimation procedure:

A set of parameters for the cooling mode is defined on the basis of the equations used in the model. An information flowchart indicating the parameters, inputs to the model and the resulting outputs acquired are shown in Figure 240. The estimation of parameters is conducted using the catalog data.

The parameter definition include:

- Piston displacement, PD
- Clearance factor, C
- Pressure drop across the suction and discharge valves, ![](media/image4983.png) 
- Loss factor used to define the electromechanical losses supposed to be proportional to the theoretical power, ![](media/image4984.png) 
- Superheat in ^ο^C or F, ![](media/image4985.png) 
- Constant part of the electromechanical losses,![](media/image4986.png)  
- Source side heat transfer coefficient, (UA)~S~
- Load side heat transfer coefficient, (UA)~L~

![Information Flowchart for Water-To-Water Heat Pump Parameter Estimation Mmodel implementation (Jin 2002)](media/information-flowchart-for-water-to-water-heat.png)


Where:

TWiL   = Entering water Load side temperature

TWiS   = Entering water Source side temperature

![](media/image4988.png) = Entering water Load side mass flow rate

![](media/image4989.png) = Entering water Source side mass flow rate

S      =  Thermostatic Signal

The parameter estimation procedure incorporates an objective function that computes the difference between the model outputs and the catalog outputs.  The objective function is then minimized by using a multi variable unconstrained multi modal Nelder Mead optimization algorithm. As the objective function value lowers after each iteration, the model outputs approach the catalog outputs consequently leading to convergence and the correct parameters are estimated for the respective model. The inputs to the model include the entering water temperatures and mass flow rates on the load side and the source side. The calculation of the objective function is shown in the form of a formula flowchart in Figure 241. The square of the sum of the errors (SSQE) for a given set of parameter values that will be minimized is given by

![](media/image4990.png)\


Where

![](media/image4991.png) = Catalog power consumption

![](media/image4992.png)  = Model power consumption

![](media/image4993.png) = Catalog load side heat transfer

![](media/image4994.png)  = Model load side heat transfer

Extrapolation beyond the catalog data grants the parameter estimation model an upper hand in comparison with the equation fit and deterministic models. However, the detailed model is computationally more intensive. Moreover, when the model is implemented within a transient system simulation program, it may come across figures that are random and unplanned by the manufacturer such as low water flow rates or extreme temperatures. This oddity may result in unrealistic set of results.

![Flow diagram of EnergyPlus Water to Water HeatPump implementation](media/flow-diagram-of-energyplus-water-to-water.jpeg)


### Control Strategy:

Both water-to-water heat pump models; parameter estimation based model and curve-fit model use the "cycle time control logic" developed by Murugappan (2002).

This strategy keeps the heat pump from short-cycling whereby the heat pump will stay on or off for the specified cycle time after switching states. The control logic is identical to the operation of a physical heat pump whereby the heat pump does not switch between on and off instantly. Refer to Muraggapan (2002) for the further details on the control strategy.

### References

Jin, Hui. 2002. Parameter Estimation Based Models of Water Source Heat Pumps. Phd. Thesis, Department of Mechanical and Aerospace Engineering, Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

Tang,C. C. 2005. Modeling Packaged Heat Pumps in Quasi-Steady State Energy Simulation Program. M.S. Thesis. Department of Mechanical and Aerospace Engineering, Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

Murugappan, Arun. 2002.  Implementing Ground Source Heat Pump and Ground Loop Heat Exchanger Models in the EnergyPlus Simulation Environment, M.S. Thesis, Department of Mechanical and Aerospace Engineering, Oklahoma State University (downloadable from http://www.hvac.okstate.edu/)