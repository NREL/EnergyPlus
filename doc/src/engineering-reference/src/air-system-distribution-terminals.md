# Air System Distribution Terminals

## Constant Volume Single Duct Uncontrolled Air Terminal

The input object AirTerminal:SingleDuct:Uncontrolled provides a method for directly connecting a central air system to a thermal zone.  Central system air is usually supplied to a zone through a terminal unit such as a single duct VAV reheat box.  Sometimes, however, it is desirable to supply central system air directly to a zone without any zone level control or tempering.  An example would be Furnace or Central DX equipment.  AirTerminal:SingleDuct:Uncontrolled is the input object for a component used to pass supply air directly into a zone without any thermostatic control.  This unit allows the program to know what zone this branch of the air system is attached to and a place to input the maximum air flow rate.  It is typically used with an AirLoopHVAC running as a constant-volume, variable-temperature system.

The AirTerminal:SingleDuct:Uncontrolled object creates the capability of supplying central system air directly to a zone and only contains the zone inlet node.  This node is both the zone inlet node and the outlet node of the AirLoopHVAC:ZoneSplitter.  It can be thought of as a balancing damper in the duct branch going to the zone.  This inlet flow can be controlled by an availability schedule.  This can be thought of as a seasonal shut off of the balancing damper.

For the AirTerminal:SingleDuct:Uncontrolled objects to work correctly, it is important in any systems including them for the sum of the maximum zone air flow rates to be equal to the maximum central system flow rate.  The zone maximum flow rates are specified in the direct air inputs.  The central air system flow rate is specified in the AirLoopHVAC input and also in the air loop branch and central fan inputs.

## Constant Volume Single Duct Reheat Air Terminal

The input object AirTerminal:SingleDuct:ConstantVolume:Reheat provides a model for single duct constant volume systems with reheat that satisfy the cooling load in a zone by changing the inlet air temperature with a reheat coil. The supply air temperature must be low enough to meet the cooling load in the zone having the greatest load. For zones with a smaller cooling load, a reheat coil is used to raise the temperature of the zone inlet air.

This object can be configured with a water, steam, electric or gas reheat coil. Operation is basically the same with all coil types. The coil is controlled to raise the zone supply air temperature (i.e., the Unit Air Outlet Node temperature) to match the zone load.  If the coil is undersized, the zone setpoint temperature will not be maintained.

![Schematic of AirTerminal:SingleDuct:ConstantVolume:Reheat Unit](media/schematic-of-airterminal-singleduct.jpeg)


## Variable Air Volume Single Duct Reheat and No Reheat Air Terminals

The VAV Single Duct Reheat and No Reheat terminal units (objects AirTerminal:SingleDuct:VAV:Reheat and AirTerminal:SingleDuct:VAV:NoReheat) provide models for single duct variable-air-volume (VAV) systems that control zone temperature primarily by varying the quantity of supply air rather than by varying the supply air temperature. The supply air temperature must be low enough to meet the cooling load in the zone having the greatest load when the zone terminal device is wide open. For zones with a smaller cooling load, the terminal device damper reduces the flow to match the zone setpoint.. If the lower flow limit on the terminal device is reached and the load is not matched, the inlet air temperature can be moderated if the terminal device has a reheat coil. In that case both the quantity of air and its temperature entering the zone are varied to meet the load. For air terminals using reheat coils, the maximum flow during reheat may be limited. Limiting the maximum flow during reheat occurs only when cooling is required (when any valid air loop cooling coil is active) and the terminal unit must reheat the air. Optional user inputs may also be used to control the amount of outdoor air entering the zone.

![Schematic of AirTerminal:SingleDuct:VAV:NoReheat Unit](media/schematic-of-airterminal-singleduct-vav.jpeg)


![Schematic of AirTerminal:SingleDuct:VAV:Reheat Unit](media/schematic-of-airterminal-singleduct-vav-001.jpeg)


The operation of the dampers and the control are described in the section AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat and AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat**,** which follows. The exception is that the section below describes how the air flow rate is varied for both cooling and heating. For the case of AirTerminal:SingleDuct:VAV:NoReheat and AirTerminal:SingleDuct:VAV:Reheat, air flow only varies during cooling operation and the air flow rate is set at the minimum value (minimum air flow fraction) when zone heating is required.

### Minimum Outdoor Air Control

The single duct air terminals may also be used to provide a minimum outdoor air quantity. When the air flow rate required to meet the zone load does not provide sufficient outdoor air, the terminal device damper will open to allow sufficient outdoor air to enter the zone. In this case, the terminal damper is controlled based on the air loop's outdoor air fraction. The outdoor air may be specified as a fixed value per person, per floor area, or per zone. The minimum outdoor air may also be specified as air changes per hour. In addition, these values may be added together to provide a combined minimum outdoor air flow rate or the maximum of each of these values may be used. An outdoor air fraction schedule may also be used to modify the calculation for the minimum amount of outdoor air throughout the simulation (Ref. DesignSpecification:OutdoorAir).

## Variable Air Volume Heating and Cooling Single Duct Reheat and NoReheat Air Terminal

### Overview

The VAV Heating and Cooling Single Duct Reheat and No Reheat terminal units (objects AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat and AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat provide models for variable-air-volume (VAV) terminal units are widely used in commercial and industrial applications. The VAV terminal units contain actuated dampers that vary the amount of central system air supplied to a zone. These terminal units may also contain a heating coil to trim the supply air temperature when overcooling is possible. The heating coil may also serve as the primary air heating source when the central system contains cooling-only equipment.

The VAV terminal units described here are used primarily with central air handling equipment with cooling and heating capability. The terminal unit dampers modulate in **both** cooling and heating mode to maintain the zone setpoint temperature(s). The central air handling equipment may be either variable air volume or constant volume where a bypass duct is used to shunt excess system air flow back to the inlet of the central air handler as terminal unit dampers modulate to satisfy the zone thermostat (i.e., AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass).

### Model Description

The no reheat version of the single duct VAV heat and cool terminal unit contains a single virtual damper assembly and requires minimal inputs. The reheat version contains both a virtual damper assembly and an air reheat coil. Multiple reheat coil types are available:

Coil:Heating:Water

Coil:Heating:Electric

Coil:Heating:Gas

Coil:Heating:Steam

Both units are simulated to provide an air flow rate sufficient to satisfy the thermostat request. The air flow rate is a function of the terminal unit's inlet air temperature and the load sensed by the thermostat. The output of the models are simply the damper position required to satisfy the zone's thermal load. Other information regarding terminal unit performance may be viewed using node report variables and heating coil report variables.

![Schematic of AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat Unit](media/schematic-of-airterminal-singleduct-vav.jpeg)


![Schematic of AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat Unit](media/schematic-of-airterminal-singleduct-vav-001.jpeg)


### Terminal Unit Inputs

Both terminal unit types share several common input fields. A unique terminal unit name must be entered. A system availability schedule is also defined to allow operational control of the terminal unit. The user must then connect the unit to the air distribution system by defining the unit inlet and outlet node names. Design air flow rates are then specified: maximum total air flow rate (autosizable) and minimum air flow fraction.

The reheat version of this terminal unit requires additional information. The name and type of reheat coil and the damper air outlet node name (same as reheat coil inlet node name). Maximum and minimum water flow rates are entered when a water or steam heating coil is used, as well as a control node name for actuating water-side flow rates and a convergence tolerance for iteration control.

### Simulation and Control

The simulation begins by determining the air mass flow rate required to satisfy the heating/cooling demand.

![](media/image2641.png)\


![](media/image2642.png)\


![](media/image2643.png)\


![](media/image2644.png)\


where

![](media/image2645.png) = Specific heat of zone air, J/kg-K

![](media/image2646.png) = Specific heat of terminal unit inlet air, J/kg-K

![](media/image2647.png) = Zone air humidity ratio, kg/kg

![](media/image2648.png) = Zone air dry-bulb temperature, °C

![](media/image2649.png) = Terminal unit inlet air humidity ratio, kg/kg

![](media/image2650.png) = Terminal unit inlet air dry-bulb temperature, °C

![](media/image2651.png) = Zone load, W (positive values denote heating, negative values denote cooling)

![](media/image2652.png) = Terminal unit air mass flow rate, kg/s

![](media/image2653.png) = Psychrometric function calculating air specific heat given air humidity ratio and dry-bulb temperature

![](media/image2654.png) = User-specified zone minimum air flow fraction

![](media/image2655.png) = Terminal unit maximum air mass flow rate, kg/s

The outdoor air input fields, if entered, are then used to adjust the terminal unit air mass flow rate to ensure the correct amount of outdoor air enters the zone (within the constraints of the terminal unit maximum and minimum flow rate inputs). The amount of outdoor air is calculated per the outdoor air requirements and is adjusted by the fraction of outdoor air entering the air loop outdoor air system.

![](media/image2656.png) ![](media/image2657.png)

where:

![](media/image2658.png)  = zone outdoor air flow rate, kg/s

![](media/image2659.png)  = fraction of outdoor air entering the air loop outside air system

If the terminal unit is in reheat mode (i.e., the central air loop cooling coil is active, the supply air was overcooled, and the zone thermostat is requesting heating) the maximum air flow rate allowed during reheat mode is adjusted as necessary.

![](media/image2660.png)\


where:

![](media/image2661.png)  = maximum air mass flow rate during reheat, kg/s

The damper position is then calculated as:

![](media/image2662.png)\


And the amount of outdoor air entering the zone is:

![](media/image2663.png)\


where

![](media/image2664.png) = Output variable ‘Zone Air Terminal VAV Damper Position', fraction of maximum flow

![](media/image2665.png)  = Output variable "Zone Air Terminal Outdoor Air Volume Flow Rate" entering the zone, m3/s

Simulation of the reheat coil occurs next when applicable. The heating demand required to maintain the thermostat heating setpoint temperature and the heating capacity of air flowing through the terminal unit are used to determine the amount of reheat required.

![](media/image2666.png)\


 where

![](media/image2667.png) = Reheat coil load, W (positive values denote heating)

![](media/image2668.png) = Load to heating setpoint temperature, W (positive values denote heating)

### References

No specific references.

## Constant Volume Single Duct Four Pipe Induction Air Terminal

The four pipe induction terminal unit (object name: AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction) is a hybrid air-hydronic unit that supplies both centrally conditioned air and local hydronic heating/cooling to a zone. Centrally conditioned air is supplied to the terminal unit at high pressure and constant flow. The central (primary) air is discharged into the terminal unit through a nozzle, inducing a fixed flow of zone (secondary) through a hydronic heating/cooling coil. The primary and secondary air streams mix and are discharged to the zone. Hot or cold water flow through the coil is varied to meet the zone heating or cooling requirement.

### Model

The four pipe induction terminal unit is modeled as a compound component consisting of three sub-components: a hot water coil, a chilled water coil and an air mixer. In terms of EnergyPlus objects these are *Coil:Heating:Water*, *Coil:Cooling:Water*, and *AirLoopHVAC:ZoneSplitter*.  The terminal unit is a forward model: its inputs are defined by the state of its inlets: namely its 2 air streams – primary and secondary; and its two water inlets – hot and cold. The outputs of the model are the conditions of the outlet air stream: flow rate, temperature and humidity ratio. The terminal unit data and simulation are encapsulated in the module *HVACSingleDuctInduc*.

### Inputs and Data

The user describes the terminal unit by inputting the name and type of the heating and cooling coils and the name of the zone mixer. The user must also specify the connectivity of the component by naming the inlet air and water nodes and the air outlet node. Finally maximum and fixed flow rates need to be specified (although these can be autosized): maximum and minimum hot and cold water volumetric flow rates and the total air volumetric flow rate (sum of primary and secondary flow rates). The relative convergence tolerances for the hot and cold water flow rates also need to be input (or allowed to default). Finally the induction ratio needs to be specified: this  is defined as the ratio of the secondary air flow rate to the primary air flow rate. The relationship between the flow rates is:

![](media/image2669.png)\


![](media/image2670.png)\


so

![](media/image2671.png)\


![](media/image2672.png)\


where *R~induc~* is the user-input induction ratio.

All input data for the four pipe induction terminal units is stored in the array *IndUnit*.

### Calculation

Given the needed inputs, the output is calculated in subroutine *CalcFourPipeIndUnit*. The temperature, humidity ratio and flow rate of the primary and secondary air streams are taken from the inlet air nodes. The inlet hot and chilled water flow rates are passed in as parameters – temperatures are taken from the inlet water nodes. Then

The hot water coil is simulated (Call *SimulateWaterCoilComponents);*

The chilled water coil is simulated (Call *SimulateWaterCoilComponents*);

The two air streams are mixed (Call *SimAirMixer*).

Finally the load met by the terminal unit is calculated and passed back to the calling routine:

![](media/image2673.png)\


Note that data is never explicitly passed between the sub-components. This is all handled automatically by the node connections and the data stored on the nodes.

### Simulation and Control

From the result of the zone simulation we have the heating/cooling demand on the terminal unit ![](media/image2674.png) . For a given hot and cold water flow *CalcFourPipeIndUnit* will give us the **terminal unit heating/cooling output. We need to vary the hot or cold water flow to make the unit output match the demand. To do this we need to numerically invert *CalcFourPipeIndUnit*: given the output, we want one of the inputs – the hot or cold water flow. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function (the *residual* function) of a single independent variable. In this case the residual function calculates ![](media/image2675.png) . *SolveRegulaFalsi* varies either the hot water or cold water mass flow rate to zero the residual

Decide whether the unit is on or off. The unit is off if: a) it is scheduled off; b) the inlet air mass flow rate is zero; c) the zone thermostat is in the deadband; d) or the zone heating/cooling demand is very small.

If the unit is off, call *CalcFourPipeIndUnit* with the hot and cold water flow rates set to their minimum flows and return.

If the unit is on, check whether active heating or cooling by the hydronic coils is needed. Call *CalcFourPipeIndUnit* with minimum water flows to see what how much cooling (or possibly heating) the unit is doing with primary air only. The output for this case is ![](media/image2676.png) .

If ![](media/image2677.png)  we need active heating. Set the cold water flow rate to the minimum. Check that the terminal unit can meet the load by setting the hot water flow rate to the maximum and calling *CalcFourPipeIndUnit*. If the output is less than the zone demand we are done – all the outputs have been calculated. Otherwise call *SolveRegulaFalsi* to obtain the hot water flow rate that will make the unit output match the zone demand. This ends the unit simulation.

If ![](media/image2678.png) we need active cooling. We set the hot water flow rate to the minimum. We check whether the terminal unit can supply the needed output by setting the cold water flow rate to the maximum and calling *CalcFourPipeIndUnit.* If this maximum cooling output is not able to meet the zone cooling demand we are done. Otherwise call *SolveRegulaFalsi* to obtain the cold water flow rate that will make the unit output match the zone demand. This ends the unit simulation.

Note that the terminal unit output is never explicitly passed to another routine. Instead the output is saved as the outlet conditions on the terminal unit outlet air node. The node data is accessed when the terminal unit output is needed elsewhere in the program (in *SimZoneAirLoopEquipment* for instance).

### References

No specific references.

## Fan Powered Induction Series and Parallel Single Duct  Reheat Air Terminal

### Overview

The input objects AirTerminal:SingleDuct:SeriesPIU:Reheat and AirTerminal:SingleDuct:ParallelPIU:Reheat provide models for fan powered induction terminal units that occur in a variety of configurations. EnergyPlus models 2 types: *series* (sometimes called *constant*) and *parallel* (sometimes called *intermittent*). The series unit provides a constant flow of air to the zone (the fan is always on at a constant flow) with a variable proportion of primary and secondary air. The parallel unit has an intermittent fan: the fan is off at maximum cooling and does not switch on until primary air flow is significantly reduced from the maximum. Once on it provides a constant flow of secondary air. Both units induce air from the zone or plenum (secondary air) and mix it with centrally conditioned supply air (primary air). Both units are variable volume: the supply air flow rate is varied to match zone conditioning requirement.

### Model

Both types of PIU are modeled as compound components. The series unit, in sequence from its inlet, consists of an air mixer, a constant volume fan, and a heating coil. In terms of EnergyPlus objects this is:

AirLoopHVAC:ZoneMixer

Fan:ConstantVolume

Coil:Heating:Water, Coil:Heating:Electric, Coil:Heating:Gas or Coil:Heating:Steam

The parallel unit contains a fan (in the secondary air stream, an air mixer, and a heating coil. In terms of EnergyPlus objects this is:

Fan:ConstantVolume

AirLoopHVAC:ZoneMixer

Coil:Heating:Water, Coil:Heating:Electric, Coil:Heating:Gas or Coil:Heating:Steam

Both units are forward models: their inputs are defined by the state of their inlets: namely the air inlet and the hot water  inlet. The outputs of the models are the conditions of the outlet air stream: flow rate, temperature and humidity ratio. The PIUs data and simulation are encapsulated in the module *PoweredInductionUnits*. The main simulation routine for the units within the module are *CalcSeriesPIU* and *CalcParallelPIU*.

### Inputs and Data

The user describes the PIU by inputting the names of the mixer, fan and heating coil sub-components plus the type of the heating coil sub-component. The user must connect the unit to the overall HVAC systems by naming various inlet and outlet nodes: the primary air inlet node, the secondary air inlet node, the unit air outlet node, and the hot water inlet node. One internal node name is needed: the coil air inlet node (same as fan outlet mode for series, mixer outlet node for parallel). Design flow rates need to be specified (although these can be autosized): maximum total air flow rate (series), maximum primary air flow rate, minimum primary air flow fraction, maximum secondary air flow rate (parallel), fan-on flow fraction (parallel), and maximum and minimum hot water flow rates. A convergence tolerance needs to be specified for units with hot water coils. There is an on/off availability schedule that need to be set.

All input data for the powered induction terminal units is stored in the array *PIU*.

### Calculation

The calculation is performed by simulating the sub-components in the order given above. Most of the code is involved with figuring out what the various flow rates should be before simulating the subcomponents. These calculations are described below.

### Simulation and Control

*Series*

From the result of the zone simulation we have the heating/cooling demand on the terminal unit ![](media/image2679.png) . The subroutine *CalcSeriesPIU* needs to determine the flow rates that will allow the unit to meet this load. The first step is to determine the on/off state of the unit and determine the air flow rates.

If the unit is scheduled off, the primary and secondary flow rates are set to zero.

If there is no primary air flow (or less than .001 kg/s), the primary air flow is set to zero and the secondary air flow is set to the constant total air flow input by the user.

If the zone temperature is in the deadband or the zone load is less than 1 watt or the zone needs heating, the primary air flow rate is set to the minimum flow rate specified by the input and the secondary air flow rate is set to the difference between the fixed total air flow rate and the primary air flow rate.

Otherwise, the zone needs cooling and the unit is active.

We determine the fan temperature rise: the secondary air flow is set to the max total air flow, primary air flow to zero, and the mixer and fan are simulated. The fan delta T is the difference between the temperature at the fan's outlet node and inlet node.

We calculate the unit air outlet temperature needed to meet the zone cooling load: ![](media/image2680.png)

The temperature needed at the outlet of the mixer is then: ![](media/image2681.png)

We can then set the primary air flow rate.

If ![](media/image2682.png) then ![](media/image2683.png)

else if ![](media/image2684.png) and ![](media/image2685.png) then ![](media/image2686.png)

otherwise ![](media/image2687.png) , subject to the constraints that the flow rate can't be bigger than the max and min allowed.

The air flow rates are now determined and we can fire the air mixer (Call *SimAir-Mixer*) and fan (Call *SimulateFanComponents*) component simulations. Finally we simulate the heating coil:

for a hot water coil, if the coil is off (no flow, deadband, no load) just fire the coil simulation once (Call *SimulateWaterCoilComponents*). Otherwise call *ControlCompOutput*; *ControlCompOutput* is a general component control routine. In this case it calls *SimulateWaterCoilComponents* repeatedly while varying the hot water flow rate and minimizing

![](media/image2688.png)\


to within the heating convergence tolerance.

For gas, electric or steam coils, the required coil output is set to ![](media/image2689.png) . Then the coil simulation is fired (Call *SimulateHeatingCoilComponent* or *SimulateSteamCoilCompo-nents*).

Finally the unit sensible output is calculated:

![](media/image2690.png)\


where *PsyHFnTdb* is the EnergyPlus function giving enthalpy as a function of temperature and humidity ratio.

*Parallel*

From the result of the zone simulation we have the heating/cooling demand on the terminal unit ![](media/image2691.png) . The subroutine *CalcParallelPIU* needs to determine the flow rates that will allow the unit to meet this load. The first step is to determine the on/off state of the unit and determine the air flow rates.

If the unit is scheduled off, the primary and secondary flow rates are set to zero.

If there is no primary air flow (or less than .001 kg/s), the primary air flow is set to zero and the secondary air flow is set to the max secondary air flow input by the user.

If the zone temperature is in the deadband or the zone load is less than 1 watt or the zone needs heating, the primary air flow rate is set to the minimum flow rate specified by the input and the secondary air flow rate is set to max secondary air flow input by the user.

Otherwise, the zone needs cooling and the unit is active.

We determine the fan temperature rise: the secondary air flow is set to the max secondary air flow, primary air flow to zero, and the fan and mixer are simulated. The fan delta T is defined as the difference between the temperature at the mixer outlet node and the fan inlet node.

Assuming that the fan is off, we calculate the primary air flow needed to meet the cooling demand.

![](media/image2692.png)\


The flow rate is constrained to be between the min and max primary air flow rates. If this calculated primary flow rate is greater than the fan-on flow rate, the secondary flow rate is set to zero and we are done. Otherwise, the fan is on and we need to recalculate the primary air flow rate.

![](media/image2693.png)\


The secondary flow rate is set to the user input fixed flow rate. The primary air flow rate is constrained to be between the min and max primary flow rated.

The air flow rates are now determined and we can fire the fan (Call *SimulateFanComponents*) and air mixer (Call *SimAirMixer*) component simulations. Finally we simulate the heating coil:

for a hot water coil, if the coil is off (no flow, deadband, no load) just fire the coil simulation once (Call *SimulateWaterCoilComponents*). Otherwise call *ControlCompOutput*; *ControlCompOutput* is a general component control routine. In this case it calls *SimulateWaterCoilComponents* repeatedly while varying the hot water flow rate and minimizing

![](media/image2694.png)\


to within the heating convergence tolerance.

For gas, electric or steam coils, the required coil output is set to ![](media/image2695.png) . Then the coil simulation is fired (Call *SimulateHeatingCoilComponent* or *SimulateSteamCoilCompo-nents*).

Finally the unit sensible output is calculated:

![](media/image2696.png)\


where *PsyHFnTdb* is the EnergyPlus function giving enthalpy as a function of temperature and humidity ratio.

### References

No specific references.

## Variable Air Volume Fan Powered Single Duct Air Terminal

### Overview

The input object AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan provides a model for variable speed (VS) fan VAV terminal unit that is a typical VAV reheat unit with the addition of a variable-speed blower fan to assist in moving supply air from the plenum to the conditioned zone. It is typically used with under-floor air distribution systems (UFAD) where the supply air is sent at low static pressure through an under-floor supply plenum. The fan has two maximum flow rate settings: one for cooling and one for heating. The cooling maximum is usually the actual fan maximum while the heating maximum is a lesser flow rate. The fan is upstream of the heating coil (this is a blow-through unit). The heating coil can be hot-water, electric or gas. Cooling control is obtained by varying the supply air flow rate from the cooling maximum to the minimum flow rate. Heating control is established by varying both the heating coil output (or hot water flow rate for hot water coils) and the supply air flow rate. Note that for this unit the minimum supply air flow rate is the flow rate when the fan is off.

### Model

The VS fan VAV terminal unit is modeled as a compound component consisting of two sub-components: a fan and a heating coil. In terms of EnergyPlus objects the fan is a *Fan:VariableVolume* object and the heating coil is a *Coil:Heating:Water*, *Coil:Heating:Electric* or a *Coil:Heating:Gas*. The terminal unit is a forward model: its inputs are defined by the state of its inlets: namely its air inlet and its hot water  inlet, if it has a hot water coil. The outputs of the model are the conditions of the outlet air stream: flow rate, temperature and humidity ratio. The terminal unit data and simulation are encapsulated in the module *SingleDuct*. The main simulation routine for the unit within the module is *SimVAVVS*.

### Inputs and Data

The user describes the terminal unit by inputting the name and type of the heating coil and the name and type of the fan. The user must also specify the connectivity of the component by naming the inlet air node; the air node connecting the fan and heating coil (fan outlet, coil inlet); the unit air outlet node (same as the zone inlet node); and hot water inlet node (if any). Maximum flow rates need to be specified (although these can be autosized): maximum cooling and heating air flow rates and the maximum hot water flow rate (if there is a hot water coil). Minimum flow rates are specified by giving by giving a minimum flow fraction for the air flow and a volumetric flow rate minimum for the hot water. For the units with hot water coils the relative convergence tolerance for the hot water flow rate also needs to be input (or allowed to default).

All input data for the VS fan VAV terminal units is stored in the array *Sys*.

### Calculation

Given the needed inputs, the output is calculated in subroutine *CalcVAVVS*. The temperature and humidity of the supply air stream are taken from the inlet air node. The inlet air flow rate and the hot water flow rate are passed in as parameters. If the coil is electric or gas the coil heating power is passed instead of the hot water flow rate. Then

The fan is simulated (call *SimulateFanComponents*). If the fan is off the fan outlet conditions are set to the inlet conditions.

The heating coil is simulated (call *SimulateWaterCoilComponents* if the coil is a hot water coil; call *SimulateHeatingCoilComponents* if the coil is gas or electric).

Finally the sensible load met by the terminal unit is calculated and passed back to the calling routine:

![](media/image2697.png)\


Note that data is never explicitly passed between the sub-components. This is all handled automatically by the node connections and the data stored on the nodes.

### Simulation and Control

From the result of the zone simulation we have the heating/cooling demand on the terminal unit ![](media/image2698.png) . For the given inlet conditions *CalcVAVVS* will give us the **terminal unit heating/cooling output. We need to vary the air or hot water flow rate or the heating coil power (for gas or electric coils) to make the unit output match the demand. To do this we need to numerically invert *CalcVAVVS*: given the output, we want one of the inputs – the air or hot water flow rate or the heating coil power. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function (the *residual* function) of a single independent variable. In this case the residual function calculates ![](media/image2699.png) . *SolveRegulaFalsi* varies either the air mass flow rate, the hot water mass flow rate or the heating coil power to zero the residual.

The unit is simulated in the following sequence.

Decide whether the unit is on or off. The unit is off if: a) it is scheduled off; b) the inlet air mass flow rate is zero; or c) the zone thermostat is in the deadband

If the unit is off, call *CalcVAVVS* with flow rates set to their minimum flows and return.

If the unit is on, we need to establish the boundaries of 4 conditioning regions: a) active cooling with fan on; b) active heating with fan on; c) active heating with fan off; d) passive cooling with fan off. The heating/cooling demand will fall into one of these regions. Once the correct region is determined, we will know which model input to vary for control and thus how to invert the calculation.

To establish the boundaries of region a) we call *CalcVAVVS* twice: once with the supply air flow rate set to the cooling maximum, once with the cooling air flow rate set to the minimum. In both cases the heating coil output is at the minimum and the fan is on. Call the 2 cooling outputs ![](media/image2700.png) and ![](media/image2701.png) . Remembering that EnergyPlus convention is that cooling loads are negative, then if ![](media/image2702.png) the terminal unit can not meet the demand. Set the air mass flow rate to the cooling maximum and call *CalcVAVV* again. This concludes the simulation. If  ![](media/image2703.png) the cooling demand is in the active cooling region. We hold the heating at the minimum, allow the supply air flow to vary between the cooling maximum and the minimum with the fan on, and call *SolveRegulaFalsi* to obtain the supply air flow rate that will produce the unit sensible cooling output that matches the demand. This concludes the simulation.

To establish the boundaries of region b) call *CalcVAVVS* twice: once with the supply air flow rate set to the heating maximum, once with the supply air flow rate set to the minimum. In both calls, if the heating coil is a hot water coil, the hot water flow rate is at the maximum. For electric and gas coils, the heating power is set to the maximum at maximum supply air flow and to zero at the minimum supply air flow. In both calls the fan is set to be on. Call the 2 heating outputs returned from the two calls to *CalcVAVVS* ![](media/image2704.png) and ![](media/image2705.png) . If ![](media/image2706.png)  the terminal unit can not meet the load. Set the air flow rate to the heating maximum and the hot water flow rate or heating coil power to the maximum and call *CalcVAVVS* again. This concludes the simulation for this case. If ![](media/image2707.png)  the heating demand is in the active heating, fan on region. For a hot water coil we call *SolveRegulaFalsi* with the supply air flow rate as the input that is varied and the hot water flow rate set to the maximum. For electric and gas coils the coil power and the supply air flow rate are both varied together from their minimum to maximum in a call to *SolveRegulaFalsi*.  The call to *SolveRegulaFalsi* concludes the simulation for this case.

This region only applies to terminal units with a hot water coil. To establish the boundaries of region c) the fan is set to off, the supply air flow rate is set to minimum flow and *CalcVAVVS* is called twice: once with the hot water flow at maximum and once with the hot water flow at minimum. Call the two heating outputs ![](media/image2708.png) and ![](media/image2709.png) . If ![](media/image2710.png)  is between these values, the supply air flow rate is set to its minimum, the fan is set to off, and in the call to *SolveRegulaFalsi* the hot water flow rate is varied to meet the load. This concludes the simulation for this case.

If the cooling demand does not fall into cases a) – c), the unit is assumed to be in the passive cooling state: heating is off or at the minimum, the fan is off, and the minimum supply air flow is delivered to the zone.

Note that the terminal unit output is never explicitly passed to another routine. Instead the output is saved as the outlet conditions on the terminal unit outlet air node. The node data is accessed when the terminal unit output is needed elsewhere in the program (in *SimZoneAirLoopEquipment* for instance).

### References

No specific references.

## Cooled Beam Unit (AirTerminal:SingleDuct:ConstantVolume:CooledBeam)

Cooled beam (frequently called chilled beams) systems are usually hybrid water – air systems. Commonly there is a constant flow, fixed temperature central forced air system for meeting ventilation and latent load requirements.  Sometimes this forced air system's flow rate is varied according to ventilation demand; and of course its supply air temperature could be reset in various ways. Sensible cooling load is met by the cooled beam units; these are ceiling suspended units with cool water circulating through them. Some types of units are "passive" – they cool by radiation and natural convection. Other types of units are "active" and act as supply air terminal units with the supply air inducing room air over the beam cooling elements. These units cool almost entirely by convection. The DOE-2 model (upon which this model is based) is a convection only model – even the "passive" units are assumed to operate 100% convectively. The cooled beam elements act as an alternative to normal ceiling radiant cooling: they are not coupled to the building mass and they operate more in a convective mode, but they can, like radiant cooling, use fairly warm cooling water.

Heating is accomplished separately from the cooled beam system – usually baseboards are used on the building perimeter to meet heating loads.

### Model

The chilled beam system is modeled as an EnergyPlus terminal unit. In terms of configuration within the overall HVAC system it will resemble a 4 pipe induction terminal unit. The user describes the system as a typical single duct constant volume system (with outside air mixer, fan, heating and cooling coils) on the air loop side, and with cooled beam terminal units on the zone equipment side.

The model is an empirical model developed at the equipment manufacturer Halton Oy. It consists of the following relationships.

*P~beam~* = *AKTbeam cooling output per unit length W/m*

*KT^n1^v^n2^^n3^*coil heat transfer coefficient W/(m^2^K)

*vq~in~~0~~air~*room air mass flow rate across coil kg/(m^2^s)

*q~in~K~1~T^n^K~in~q~pr~*room air volumetric flow rate across coil per

unit length m^3^/(s-m)

T is the room air –water temperature difference (average water temperature is used) in degrees C.

 is the water velocity in m/s.

q~pr~ is the supply air flow rate per unit length m^3^/(s-m)

The other symbols are the model parameters input by the user (see the IO Ref for descriptions).

### Inputs and Data

The user describes the unit by inputting the name, referencing an availability schedule, and choosing a type (*active* or *passive*). The user must also specify the connectivity of the component by naming the inlet and outlet air and water nodes. The maximum water and fixed air flow rates need to be specified (although these can be autosized). The design inlet and outlet water temperatures are inputs. Generally the inlet water temperature is quite warm (15C is the default) and the temperature rise is small (design outlet water temperature defaults to 17C). Two key inputs are the number of beams (in the zone) and the beam length. It is generally wise to let these inputs autosize.

The remaining inputs are parameters specific to the product model. Good defaults are supplied and they should not be changed without information from the manufacturer.

### Sizing

The Cooled Beam sizing calculations generally follow the procedures used for other terminal units (see *Loop Equipment Sizing*). One difference is that the Cooled Beams use the Cooled Beam inputs *Design Inlet Water Temperature* and *Design Outlet Water Temperature* for the chilled water T rather than the T from Plant Sizing. There are also two inputs unique to the Cooled Beam units that are autosized and will be described here.

The input *Number of individual beam units in the zone* is autosized by dividing the beam system zone design chilled water flow rate (either input by the user or autosized) by a nominal chilled water beam flow rate: 0.07 kg/s.

The input *Length of an individual beam unit* is autosized by using the model equations to calculate the length. The inputs to the equations are:

the design load per beam. The design load is calculated from the design water mass flow rate and the design water inlet and outlet temperatures. The design load is divided by the number of beams to obtain the design load per beam.

The design air supply air flow per beam – obtained by dividing the design supply air flow by the number of beams.

The design water flow per beam (m^3^/s) – obtained by dividing the design water flow by the number of beams.

The design water velocity – obtained by dividing the design water flow per beam by the cross sectional inside area of a water tube (D^2^/4, where D is the input *Pipe inside diameter*.

Average air to water *T = T~z~~, cool peak~*  *0.5(T~w,des inlet~  T~w,des outlet~)*; where        *T~z~~, cool peak~* is the zone air temperature at the cooling peak and the *T~w,des~* ‘s are the water design inlet and outlet temperatures.

With these inputs the model equations can be solve directly for beam length for passive cooled beams, and iteratively for active cooled beams.

### Calculation

The subroutine *CalcCoolBeam*  uses the model equations to calculate the cooling power *P~beams,out~* delivered to the room air and the outlet water temperature given the water flow rate (and the room air temperature and water inlet temperature). Since the model equations are nonlinear they must be solved iteratively. The subroutine does this by varying the outlet water temperature *T~w,out~* and calculating the water-side cooling power

P~w~  q~w,beam~c~p,w~T~w,out~T~w,in~

and comparing it to the air-side cooling power

P~air~KATL~beam~

where *q~w,beam~* is the water mass flow rate (kg/s) per beam and *L~beam~* is the length of a beam (m). When *P*~w~ and *P~air~* match to within 0.1 W the subroutine terminates the iteration.

### Simulation and Control

From the result of the zone simulation we have the heating/cooling demand for the zone equipment. For the cooling demand, we use the load to cooling set point *P~c~*. Part of the demand may be satisfied by the zone supply air:

P~sup~q~air~c~p,air,sys~T~sys~  c~p,air,z~T~z~

The demand on the actual beams is then

P~beams,dem~P~c~  P~sup~

We want to know the chilled water flow rate that will give a beam cooling output of *P~beams~*. To obtain this we need to numerically invert *CalcCoolBeam*: given its desired output, we want to know the chilled water flow rate. This numerical inversion is carried out by calling the subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function (the *residual* function) of a single independent variable. In this case the residual function is basically

P~beams,out~P~beams,dem~P~beams,out,max~

*SolveRegulaFalsi* varies the cold water mass flow rate to zero the residual. The water inlet and outlet node flow rates are set to the flow rate found by *SolveRegulaFalsi* and the water outlet node temperature is set to the outlet water temperature from *SolveRegulaFalsi*.

### References

Documentation Package Update #2 for DOE-2.1E, Version 107, page 3.152 describes the input and the model for the DOE-2 cooled beam model.

## Constant Volume Dual Duct Air Terminal

### Overview

The input object AirTerminal:DualDuct:ConstantVolume provides a model for dual duct constant-air-volume (DDCAV) systems that are typically used in special applications where precise temperature and humidity control are required and energy efficiency is not of primary concern. Thermal control for each zone is achieved by mixing air from the hot deck with air from the cold deck to achieve a supply air temperature that will exactly meet the zone load and satisfy the zone thermostat demand. Each zone has its own mixing box which is connected directly to the hot and cold decks. The mixing box dampers change the relative amount of hot and cold air that will be delivered (at a constant volumetric flow rate) to the zone.

### Model Description

The DDCAV model will attempt to meet all of the thermostatic loads of a particular zone by explicitly calculating the hot and cold deck mass flow rates. For the energy and mass balance equations shown below, the zone load, temperatures, specific heats and the design mass flow rate are all known. These equations can then be solved directly for the hot deck and cold deck mass flow rates.

![](media/image2711.png)\


![](media/image2712.png)\


Where:

![](media/image2713.png) = Zone load, W (positive=heating, negative=cooling)

![](media/image2714.png) = Specific heat of zone air, J/kg-K

![](media/image2715.png) = Specific heat of cold deck air, J/kg-K

![](media/image2716.png) = Specific heat of hot deck air, J/kg-K

![](media/image2717.png) = Zone air dry-bulb temperature, °C

![](media/image2718.png) = Cold deck air dry-bulb temperature, °C

![](media/image2719.png) = Hot deck air dry-bulb temperature, °C

![](media/image2720.png) = System design air mass flow rate through both heating or cooling duct, kg/s

![](media/image2721.png) = Cold deck air mass flow rate, kg/s

![](media/image2722.png) = Hot deck air mass flow rate, kg/s

### Simulation and Control

The simulation first calculates the hot deck and cold deck air mass flow rates required to satisfy the heating/cooling demand on the zone. Once the individual flow rates have been calculated based on temperature control, the zone mixed air conditions are calculated assuming adiabatic mixing of the two air streams.

## Variable Air Volume Dual Duct Air Terminal

### Overview

The input object AirTerminal:DualDuct:VAV provides a model for dual duct variable-air-volume (DDVAV) systems that are typically used in special applications where both temperature and humidity control as well as energy efficiency are of primary concern.  This system combines the advantages of the standard dual duct system for better thermal control with the possibility to reduce fan energy using a variable speed fan. The DDVAV terminal units contain actuated dampers that vary the amount of central system air supplied to a zone from both the hot and cold deck. Optional user inputs may also be used to control the amount of outdoor air entering the zone.

The DDVAV terminal units described here are used primarily with central air handling equipment with cooling and heating capability. The terminal unit dampers modulate the amount of cold air and hot air as well as the overall flow rate to maintain the zone setpoint temperature(s).

### Model Description

The DDVAV model will attempt to meet all of the thermostatic loads of a particular zone by first sending air through either the heating duct or the cooling duct depending on whether there is a heating or cooling load (respectively).  Flow rate through the opposite duct is kept at zero and flow through the active duct is varied between the minimum air flow rate (minimum zone air fraction multiplied by the maximum flow rate) and the maximum air flow rate.  If the flow rate to meet the load through either the heating or cooling duct results in a flow outside these ranges, then air must be passed through the other duct as well to avoid over- or under-heating or –cooling. This is done using a conservation of energy and mass analysis of the terminal unit as well as the known inlet and necessary outlet condition to meet the thermal needs of the zone.

When there is no load on the zone, the system could either be scheduled off or be in a "no load" condition.  If the system is scheduled off, the model keeps the flow rate at zero for both the heating and cooling duct.  If in a no load condition, the system attempts to throttle back to the minimum possible flow and then find a balance between flow through the heating and cooling duct that will provide no net conditioning to the space.  This means that the enthalpy of air delivered to the space must be equal to the enthalpy of the (average) air in the zone.

### DDVAV Terminal Unit Inputs

Like other terminal units, the DDVAV terminal unit requires an availability schedule and inlet and outlet node designations.  The DDVAV terminal unit, like the DD terminal unit, has two inlet nodes (one for the heating duct and one for the cooling duct) and one outlet node.

In addition, the DDVAV terminal unit also has a maximum flow rate and a minimum flow fraction like the VAV terminal unit.  This allows the flow to be throttled back when it is possible to provide the proper amount of conditioning with less flow.  The maximum flow rate can be auto-sized, if desired.

### Minimum Outdoor Air Control

This dual duct air terminal may also be used to provide a minimum outdoor air quantity. When the air flow rate required to meet the zone load does not provide sufficient outdoor air, the terminal device damper will open to allow sufficient outdoor air to enter the zone. In this case, the terminal damper is controlled based on the air loop's outdoor air fraction. The outdoor air may be specified as a fixed value per person, per floor area, or per zone or as the required minimum air changes per hour. In addition, these values may be added together to provide a combined minimum outdoor air flow rate or the maximum of each of these values may be used. An outdoor air fraction schedule may also be used to modify the calculation for the minimum amount of outdoor air throughout the simulation (Ref. DesignSpecification:OutdoorAir).

### Simulation and Control

The simulation begins by determining the air mass flow rate required to satisfy the heating/cooling demand using either the heating duct or cooling duct.

![](media/image2723.png)\


![](media/image2724.png)\


![](media/image2725.png)\


![](media/image2726.png)\


where

![](media/image2727.png) = Specific heat of zone air, J/kg-K

![](media/image2728.png) = Specific heat of terminal unit inlet air, J/kg-K

![](media/image2729.png) = Zone air humidity ratio, kg/kg

![](media/image2730.png) = Zone air dry-bulb temperature, °C

![](media/image2731.png) = Terminal unit inlet air humidity ratio, kg/kg

![](media/image2732.png) = Terminal unit inlet air dry-bulb temperature, °C

![](media/image2733.png) = Zone load, W (positive values denote heating, negative values denote cooling)

![](media/image2734.png) = Terminal unit air mass flow rate through either heating or cooling duct, kg/s

![](media/image2735.png) = Psychrometric function calculating air specific heat given air humidity ratio and dry-bulb temperature

![](media/image2736.png) = User-specified zone minimum air flow fraction

![](media/image2737.png) = Terminal unit maximum air mass flow rate, kg/s

The outdoor air input requirements, if entered, are then used to adjust the terminal unit air mass flow rate to ensure the correct amount of outdoor air enters the zone (within the constraints of the terminal unit maximum and minimum flow rate inputs). The amount of outdoor air is calculated per the outdoor air requirements and is adjusted by the fraction of outdoor air entering the air loop outdoor air system.

![](media/image2656.png) ![](media/image2657.png)\


where:

![](media/image2738.png)  = zone outdoor air flow rate, kg/s

![](media/image2739.png)  = fraction of outdoor air entering the air loop outside air system

The damper position is then calculated as:

![](media/image2740.png)\


where

![](media/image2741.png) = Output variable ‘Zone Air Terminal VAV Damper Position', fraction of maximum flow

If the flow rate was between the maximum flow rate and the minimum flow rate for the terminal unit, then no other calculations are needed.  However, if the flow was reset to either the maximum or minimum flow rate, then flow through the active duct must be balanced by flow through the other duct to achieve the proper conditioning.

### References

No specific references.  Refer to the ASHRAE Handbook series for general information on different system types as needed.

## Dual Duct Dedicated Outside Air Terminal with VAV Cooling

### Overview

The input object AirTerminal:DualDuct:VAV:OutdoorAir provides a model for dedicated outside air combined with recirculated air for cooling.  This air terminal has two inlets and one outlet. The outdoor air inlet has one damper that is controlled to meet the air flow requirements for ventilation.  The second inlet is for cool recirculated air and has a second damper that is controlled to meet the zone's cooling loads.  The two streams are then mixed and inlet to the zone.  This unit is for central air systems (using AirLoopHVAC object).  Because of the limitation in EnergyPlus of allowing only one air terminal per zone, the dual duct approach offers advantages in that it allows modeling dedicated outdoor air systems (DOAS) and central VAV cooling at the same time.  The original motivation for adding this terminal was to model twin-fan, twin-coil systems.

The recirculated cool air duct is actually optional. If no node name is input for the recirculated air inlet node, then only the outdoor air duct is operational and the air terminal behaves as a single duct.  This offers additional capabilities for single duct DOAS in that this terminal can request outdoor air flows that change over time but are not controlled to meet zone loads.

### Model Description

The model attempts to meet the ventilation requirements and the cooling loads of a particular zone.  If the zone requires heating, ancilliary heating equipment is needed as this terminal cannot do any heating.  The model first determines the current required outdoor air flow rate for ventilation and then calculates the flow of cool air needed to reach the cooling setpoint.

The outdoor air rate is controlled by the schedule and specifications contained in a DesignSpecification:OutdoorAir object and can be based on flows per person, per zone, per area, or air changes per hour.  Using the key CurrentOccupancy, the per person rate can be set to operate based on the current occupancy level to model demand controlled ventilation. Using the key DesignOccupancy it can be set to operate based on the design, or maximum, level of occupancy.   The outdoor air inlet side of the terminal is assigned a design maximum flow rate based on the largest flow rates specified by the associated DesignSpecification:OutdoorAir object. This maximum for the outdoor air is used to calculate the damper position and contributes to the overall maximum if that is autosized.

The recirculated cool air flow rate is controlled to meet the zone cooling loads.  The first step is to calculate the impact that the outdoor air flow has on the loads starting with the specific heats.

![](media/image2742.png)\


![](media/image2743.png)\


![](media/image2744.png)\


where,

![](media/image2745.png)  specific heat of zone air being served by the terminal unit, J/kg-K

![](media/image2746.png)  specific heat of outdoor air entering the terminal unit, J/kg-K

![](media/image2747.png)  specific heat of the recirculated (cool) air entering the terminal unit (if present), J/kg-K

![](media/image2748.png) humidity ratio of the zone air, kg/kg

![](media/image2749.png) humidity ratio of the outdoor air entering the terminal unit, kg/kg

![](media/image2750.png) humidity ratio of the recirculated air entering the terminal unit, kg/kg

![](media/image2751.png) air drybulb temperature of the zone, ºC

![](media/image2752.png) air drybulb temperature of the outdoor air entering the terminal unit, ºC

![](media/image2753.png) air drybulb temperature of the recirculated cool air entering the terminal unit, ºC

![](media/image2754.png)  is a psychrometric function for calculating the specific heat of moist air as a function of humidity ratio and drybulb temperature.

The contribution to zone load provided by the outdoor air toward meeting the cooling setpoint, ![](media/image2755.png)  (W), is then calculated using:

![](media/image2756.png)\


where,

![](media/image2757.png) is the mass flow rate of outdoor air determined by the outdoor air requirement, kg/s

![](media/image2758.png) is the zone cooling setpoint drybulb temperature, ºC

This is then used to calculate the load that the recirculated cool air should deliver, ![](media/image2759.png)  (W):

![](media/image2760.png)\


where,

![](media/image2761.png)  is the remaining load to cooling setpoint as determined by Predictor and including the impacts of any other zone equipment sequenced before this terminal. Then the recirculated cool air mass flow rate, ![](media/image2762.png)  (kg/s), is calculated using:

![](media/image2763.png)\


The model also includes a form of damping where the last three values for ![](media/image2764.png)  are stored and used to detect if the solution is oscillating from one iteration to the next and if it is then the new value is not used but rather the value from the previous iteration is used.  Once the two mass flows are known, the moist air properties of the outlet node are calculated using mass flow weighting.

### References

Sekhar, S. C., K. W. Tham, et al. (2004). Development of energy-efficient single-coil twin-fan air-conditioning system with zonal ventilation control, Nashville, TX, United states, Amer. Soc. Heating, Ref. Air-Conditoning Eng. Inc.