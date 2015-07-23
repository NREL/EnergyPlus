
Simulation Models – Encyclopedic Reference
==========================================

The following descriptions are grouped alphabetically (as much as possible) with some also having additional tags of “Heat Balance”, “HVAC”, and “Plant”.  With the integrated solution, these designations signify where the effects of these models have their first impacts.

<!--RemoveStart-->
Main Sections:

* [Air Terminals](#Terminals)
* [Boilers](#Boilers)
* [Chillers](#Chillers)
<!--RemoveEnd-->

Air System Distribution Terminals <a name="Terminals"></a>
---------------------------------

### Constant Volume Single Duct Uncontrolled Air Terminal

The input object AirTerminal:SingleDuct:Uncontrolled provides a method for directly connecting a central air system to a thermal zone.  Central system air is usually supplied to a zone through a terminal unit such as a single duct VAV reheat box.  Sometimes, however, it is desirable to supply central system air directly to a zone without any zone level control or tempering.  An example would be Furnace or Central DX equipment.  AirTerminal:SingleDuct:Uncontrolled is the input object for a component used to pass supply air directly into a zone without any thermostatic control.  This unit allows the program to know what zone this branch of the air system is attached to and a place to input the maximum air flow rate.  It is typically used with an AirLoopHVAC running as a constant-volume, variable-temperature system.

The AirTerminal:SingleDuct:Uncontrolled object creates the capability of supplying central system air directly to a zone and only contains the zone inlet node.  This node is both the zone inlet node and the outlet node of the AirLoopHVAC:ZoneSplitter.  It can be thought of as a balancing damper in the duct branch going to the zone.  This inlet flow can be controlled by an availability schedule.  This can be thought of as a seasonal shut off of the balancing damper.

For the AirTerminal:SingleDuct:Uncontrolled objects to work correctly, it is important in any systems including them for the sum of the maximum zone air flow rates to be equal to the maximum central system flow rate.  The zone maximum flow rates are specified in the direct air inputs.  The central air system flow rate is specified in the AirLoopHVAC input and also in the air loop branch and central fan inputs.

### Constant Volume Single Duct Reheat Air Terminal

The input object AirTerminal:SingleDuct:ConstantVolume:Reheat provides a model for single duct constant volume systems with reheat that satisfy the cooling load in a zone by changing the inlet air temperature with a reheat coil. The supply air temperature must be low enough to meet the cooling load in the zone having the greatest load. For zones with a smaller cooling load, a reheat coil is used to raise the temperature of the zone inlet air.

This object can be configured with a water, steam, electric or gas reheat coil. Operation is basically the same with all coil types. The coil is controlled to raise the zone supply air temperature (i.e., the Unit Air Outlet Node temperature) to match the zone load.  If the coil is undersized, the zone setpoint temperature will not be maintained.

![ConstVolumeReheat](media/image2788.png)

Figure 153. Schematic of AirTerminal:SingleDuct:ConstantVolume:Reheat Unit

### Variable Air Volume Single Duct Reheat and No Reheat Air Terminals

The VAV Single Duct Reheat and No Reheat terminal units (objects AirTerminal:SingleDuct:VAV:Reheat and AirTerminal:SingleDuct:VAV:NoReheat) provide models for single duct variable-air-volume (VAV) systems that control zone temperature primarily by varying the quantity of supply air rather than by varying the supply air temperature. The supply air temperature must be low enough to meet the cooling load in the zone having the greatest load when the zone terminal device is wide open. For zones with a smaller cooling load, the terminal device damper reduces the flow to match the zone setpoint.. If the lower flow limit on the terminal device is reached and the load is not matched, the inlet air temperature can be moderated if the terminal device has a reheat coil. In that case both the quantity of air and its temperature entering the zone are varied to meet the load. For air terminals using reheat coils, the maximum flow during reheat may be limited. Limiting the maximum flow during reheat occurs only when cooling is required (when any valid air loop cooling coil is active) and the terminal unit must reheat the air. Optional user inputs may also be used to control the amount of outdoor air entering the zone.



![Damper\_NoHeat](media/image2789.png)

Figure 154. Schematic of AirTerminal:SingleDuct:VAV:NoReheat Unit



![Damper](media/image2790.png)

Figure 155. Schematic of AirTerminal:SingleDuct:VAV:Reheat Unit

The operation of the dampers and the control are described in the section AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat and AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat**,** which follows. The exception is that the section below describes how the air flow rate is varied for both cooling and heating. For the case of AirTerminal:SingleDuct:VAV:NoReheat and AirTerminal:SingleDuct:VAV:Reheat, air flow only varies during cooling operation and the air flow rate is set at the minimum value (minimum air flow fraction) when zone heating is required.

#### Minimum Outdoor Air Control

The single duct air terminals may also be used to provide a minimum outdoor air quantity. When the air flow rate required to meet the zone load does not provide sufficient outdoor air, the terminal device damper will open to allow sufficient outdoor air to enter the zone. In this case, the terminal damper is controlled based on the air loop’s outdoor air fraction. The outdoor air may be specified as a fixed value per person, per floor area, or per zone. The minimum outdoor air may also be specified as air changes per hour. In addition, these values may be added together to provide a combined minimum outdoor air flow rate or the maximum of each of these values may be used. An outdoor air fraction schedule may also be used to modify the calculation for the minimum amount of outdoor air throughout the simulation (Ref. DesignSpecification:OutdoorAir).

### Variable Air Volume Heating and Cooling Single Duct Reheat and NoReheat Air Terminal

#### Overview

The VAV Heating and Cooling Single Duct Reheat and No Reheat terminal units (objects AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat and AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat provide models for variable-air-volume (VAV) terminal units are widely used in commercial and industrial applications. The VAV terminal units contain actuated dampers that vary the amount of central system air supplied to a zone. These terminal units may also contain a heating coil to trim the supply air temperature when overcooling is possible. The heating coil may also serve as the primary air heating source when the central system contains cooling-only equipment.

The VAV terminal units described here are used primarily with central air handling equipment with cooling and heating capability. The terminal unit dampers modulate in **both** cooling and heating mode to maintain the zone setpoint temperature(s). The central air handling equipment may be either variable air volume or constant volume where a bypass duct is used to shunt excess system air flow back to the inlet of the central air handler as terminal unit dampers modulate to satisfy the zone thermostat (i.e., AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass).

#### Model Description

The no reheat version of the single duct VAV heat and cool terminal unit contains a single virtual damper assembly and requires minimal inputs. The reheat version contains both a virtual damper assembly and an air reheat coil. Multiple reheat coil types are available:

1)   Coil:Heating:Water

2)   Coil:Heating:Electric

3)   Coil:Heating:Gas

4)   Coil:Heating:Steam

Both units are simulated to provide an air flow rate sufficient to satisfy the thermostat request. The air flow rate is a function of the terminal unit’s inlet air temperature and the load sensed by the thermostat. The output of the models are simply the damper position required to satisfy the zone’s thermal load. Other information regarding terminal unit performance may be viewed using node report variables and heating coil report variables.

![Damper\_NoHeat](media/image2791.png)

Figure 156. Schematic of AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat Unit



![Damper](media/image2792.png)

Figure 157. Schematic of AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat Unit

#### Terminal Unit Inputs

Both terminal unit types share several common input fields. A unique terminal unit name must be entered. A system availability schedule is also defined to allow operational control of the terminal unit. The user must then connect the unit to the air distribution system by defining the unit inlet and outlet node names. Design air flow rates are then specified: maximum total air flow rate (autosizable) and minimum air flow fraction.

The reheat version of this terminal unit requires additional information. The name and type of reheat coil and the damper air outlet node name (same as reheat coil inlet node name). Maximum and minimum water flow rates are entered when a water or steam heating coil is used, as well as a control node name for actuating water-side flow rates and a convergence tolerance for iteration control.

#### Simulation and Control

The simulation begins by determining the air mass flow rate required to satisfy the heating/cooling demand.

<div>$$C{p_{zone}} = PsyCpAirFnWTdb\left( {{\omega_{zone}},{T_{zone}}} \right)$$</div>

<div>$$C{p_{inlet}} = PsyCpAirFnWTdb\left( {{\omega_{inlet}},{T_{inlet}}} \right)$$</div>

<div>$$DeltaCpT = \left( {C{p_{inlet}}} \right)\left( {{T_{inlet}}} \right) - \left( {C{p_{zone}}} \right)\left( {{T_{zone}}} \right)$$</div>

<div>$$\dot m = MIN\left( {{{\dot m}_{max}},\,\,\,MAX\left( {{{\dot m}_{max}}*MinAirFlowFrac\,,\,\,\,{\raise0.7ex\hbox{${{{\dot Q}_{zone}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{zone}}} {DeltaCpT}}}\right.}\!\lower0.7ex\hbox{${DeltaCpT}$}}} \right)\,\,} \right)$$</div>

where

<span>\(C{p_{zone}}\)</span>  = Specific heat of zone air, J/kg-K

<span>\(C{p_{inlet}}\)</span>  = Specific heat of terminal unit inlet air, J/kg-K

<span>\({\omega_{zone}}\)</span>    = Zone air humidity ratio, kg/kg

<span>\({T_{zone}}\)</span>     = Zone air dry-bulb temperature, °C

<span>\({\omega_{inlet}}\)</span>    = Terminal unit inlet air humidity ratio, kg/kg

<span>\({T_{inlet}}\)</span>     = Terminal unit inlet air dry-bulb temperature, °C

<span>\({\dot Q_{zone}}\)</span>    = Zone load, W (positive values denote heating, negative values denote cooling)

<span>\(\dot m\)</span>        = Terminal unit air mass flow rate, kg/s

<span>\(PsyCpAirFnWTdb\)</span>  = Psychrometric function calculating air specific heat given air humidity ratio and dry-bulb temperature

<span>\(MinAirFlowFrac\)</span>    = User-specified zone minimum air flow fraction

<span>\({\dot m_{max}}\)</span>              = Terminal unit maximum air mass flow rate, kg/s

The outdoor air input fields, if entered, are then used to adjust the terminal unit air mass flow rate to ensure the correct amount of outdoor air enters the zone (within the constraints of the terminal unit maximum and minimum flow rate inputs). The amount of outdoor air is calculated per the outdoor air requirements and is adjusted by the fraction of outdoor air entering the air loop outdoor air system.

<div>$$
  \dot m = \max \left( \dot m, \frac{\dot m_{OA}}{OAFrac} \right)
$$</div>

where:

<span>\({\mathop m\limits^\cdot_{OA}}\)</span> = zone outdoor air flow rate, kg/s

<span>\(OAFrac\)</span> = fraction of outdoor air entering the air loop outside air system

If the terminal unit is in reheat mode (i.e., the central air loop cooling coil is active, the supply air was overcooled, and the zone thermostat is requesting heating) the maximum air flow rate allowed during reheat mode is adjusted as necessary.

<div>$$\mathop m\limits^\cdot  \, = \,MIN\left( {\mathop m\limits^\cdot  ,\,{{\mathop m\limits^\cdot  }_{reheat}}} \right)$$</div>

where:

<span>\({\mathop m\limits^\cdot_{reheat}}\)</span> = maximum air mass flow rate during reheat, kg/s

The damper position is then calculated as:

<div>$$FRAC_{damper} = \frac{\dot m}{\dot m_{max}} $$</div>

And the amount of outdoor air entering the zone is:

<div>$$\dot V_{OA} = \dot m (OAFrac)$$</div>

where

<span>\(FRAC_{damper}\)</span>  = Output variable ‘Zone Air Terminal VAV Damper Position’, fraction of maximum flow

<span>\({\mathop V\limits^\cdot_{OA}}\)</span> = Output variable “Zone Air Terminal Outdoor Air Volume Flow Rate” entering the zone, m3/s

Simulation of the reheat coil occurs next when applicable. The heating demand required to maintain the thermostat heating setpoint temperature and the heating capacity of air flowing through the terminal unit are used to determine the amount of reheat required.

<div>$${\dot Q_{reheat}} = {\dot Q_{heatSP}} + \dot m C_{p,zone}(T_{inlet}-T_{zone})$$</div>

 where

<span>\({\dot Q_{reheat}}\)</span>  = Reheat coil load, W (positive values denote heating)

<span>\({\dot Q_{heatSP}}\)</span> = Load to heating setpoint temperature, W (positive values denote heating)

#### References

No specific references.

### Constant Volume Single Duct Four Pipe Induction Air Terminal

The four pipe induction terminal unit (object name: AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction) is a hybrid air-hydronic unit that supplies both centrally conditioned air and local hydronic heating/cooling to a zone. Centrally conditioned air is supplied to the terminal unit at high pressure and constant flow. The central (primary) air is discharged into the terminal unit through a nozzle, inducing a fixed flow of zone (secondary) through a hydronic heating/cooling coil. The primary and secondary air streams mix and are discharged to the zone. Hot or cold water flow through the coil is varied to meet the zone heating or cooling requirement.

#### Model

The four pipe induction terminal unit is modeled as a compound component consisting of three sub-components: a hot water coil, a chilled water coil and an air mixer. In terms of EnergyPlus objects these are *Coil:Heating:Water*, *Coil:Cooling:Water*, and *AirLoopHVAC:ZoneSplitter*.  The terminal unit is a forward model: its inputs are defined by the state of its inlets: namely its 2 air streams – primary and secondary; and its two water inlets – hot and cold. The outputs of the model are the conditions of the outlet air stream: flow rate, temperature and humidity ratio. The terminal unit data and simulation are encapsulated in the module *HVACSingleDuctInduc*.

#### Inputs and Data

The user describes the terminal unit by inputting the name and type of the heating and cooling coils and the name of the zone mixer. The user must also specify the connectivity of the component by naming the inlet air and water nodes and the air outlet node. Finally maximum and fixed flow rates need to be specified (although these can be autosized): maximum and minimum hot and cold water volumetric flow rates and the total air volumetric flow rate (sum of primary and secondary flow rates). The relative convergence tolerances for the hot and cold water flow rates also need to be input (or allowed to default). Finally the induction ratio needs to be specified: this  is defined as the ratio of the secondary air flow rate to the primary air flow rate. The relationship between the flow rates is:

<div>$${\dot m_{air,tot}} = {\dot m_{air,pri}} + {\dot m_{air,\sec }}$$</div>

<div>$${\dot m_{air,sec}} = {R_{induc}}\cdot {\dot m_{air,pri}}$$</div>

so

<div>$${\dot m_{air,pri}} = {\dot m_{air,tot}}/(1 + {R_{induc}})$$</div>

<div>$${\dot m_{air,sec}} = {\dot m_{air,tot}}\cdot {R_{induc}}/(1 + {R_{induc}})$$</div>

where *R<sub>induc</sub>* is the user-input induction ratio.

All input data for the four pipe induction terminal units is stored in the array *IndUnit*.

#### Calculation

Given the needed inputs, the output is calculated in subroutine *CalcFourPipeIndUnit*. The temperature, humidity ratio and flow rate of the primary and secondary air streams are taken from the inlet air nodes. The inlet hot and chilled water flow rates are passed in as parameters – temperatures are taken from the inlet water nodes. Then

The hot water coil is simulated (Call *SimulateWaterCoilComponents);*

The chilled water coil is simulated (Call *SimulateWaterCoilComponents*);

The two air streams are mixed (Call *SimAirMixer*).

Finally the load met by the terminal unit is calculated and passed back to the calling routine:

<div>$${\dot Q_{out}} = {\dot m_{tot}}\cdot {c_{p,air}}\cdot ({T_{air,out}} - {T_{air,zone}})$$</div>

Note that data is never explicitly passed between the sub-components. This is all handled automatically by the node connections and the data stored on the nodes.

#### Simulation and Control

From the result of the zone simulation we have the heating/cooling demand on the terminal unit 
<span>\({\dot Q_{z,req}}\)</span>
. For a given hot and cold water flow *CalcFourPipeIndUnit* will give us the terminal unit heating/cooling output. We need to vary the hot or cold water flow to make the unit output match the demand. To do this we need to numerically invert *CalcFourPipeIndUnit*: given the output, we want one of the inputs – the hot or cold water flow. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function (the *residual* function) of a single independent variable. In this case the residual function calculates 
<span>\(({\dot Q_{z,req}} - {\dot Q_{out}})/{\dot Q_{z,req}}\)</span>
. *SolveRegulaFalsi* varies either the hot water or cold water mass flow rate to zero the residual.

Decide whether the unit is on or off. The unit is off if: a) it is scheduled off; b) the inlet air mass flow rate is zero; c) the zone thermostat is in the deadband; d) or the zone heating/cooling demand is very small.

If the unit is off, call *CalcFourPipeIndUnit* with the hot and cold water flow rates set to their minimum flows and return.

If the unit is on, check whether active heating or cooling by the hydronic coils is needed. Call *CalcFourPipeIndUnit* with minimum water flows to see what how much cooling (or possibly heating) the unit is doing with primary air only. The output for this case is <span>\({\dot Q_{pri}}\)</span>.

If <span>\({\dot Q_{z,req}} > {\dot Q_{pri}}\)</span> we need active heating. Set the cold water flow rate to the minimum. Check that the terminal unit can meet the load by setting the hot water flow rate to the maximum and calling *CalcFourPipeIndUnit*. If the output is less than the zone demand we are done – all the outputs have been calculated. Otherwise call *SolveRegulaFalsi* to obtain the hot water flow rate that will make the unit output match the zone demand. This ends the unit simulation.

If <span>\({\dot Q_{z,req}} &lt; {\dot Q_{pri}}\)</span>we need active cooling. We set the hot water flow rate to the minimum. We check whether the terminal unit can supply the needed output by setting the cold water flow rate to the maximum and calling *CalcFourPipeIndUnit.* If this maximum cooling output is not able to meet the zone cooling demand we are done. Otherwise call *SolveRegulaFalsi* to obtain the cold water flow rate that will make the unit output match the zone demand. This ends the unit simulation.

Note that the terminal unit output is never explicitly passed to another routine. Instead the output is saved as the outlet conditions on the terminal unit outlet air node. The node data is accessed when the terminal unit output is needed elsewhere in the program (in *SimZoneAirLoopEquipment* for instance).

#### References

No specific references.

### Fan Powered Induction Series and Parallel Single Duct  Reheat Air Terminal

#### Overview

The input objects AirTerminal:SingleDuct:SeriesPIU:Reheat and AirTerminal:SingleDuct:ParallelPIU:Reheat provide models for fan powered induction terminal units that occur in a variety of configurations. EnergyPlus models 2 types: *series* (sometimes called *constant*) and *parallel* (sometimes called *intermittent*). The series unit provides a constant flow of air to the zone (the fan is always on at a constant flow) with a variable proportion of primary and secondary air. The parallel unit has an intermittent fan: the fan is off at maximum cooling and does not switch on until primary air flow is significantly reduced from the maximum. Once on it provides a constant flow of secondary air. Both units induce air from the zone or plenum (secondary air) and mix it with centrally conditioned supply air (primary air). Both units are variable volume: the supply air flow rate is varied to match zone conditioning requirement.

#### Model

Both types of PIU are modeled as compound components. The series unit, in sequence from its inlet, consists of an air mixer, a constant volume fan, and a heating coil. In terms of EnergyPlus objects this is:

1.    AirLoopHVAC:ZoneMixer

2.    Fan:ConstantVolume

3.    Coil:Heating:Water, Coil:Heating:Electric, Coil:Heating:Gas or Coil:Heating:Steam

The parallel unit contains a fan (in the secondary air stream, an air mixer, and a heating coil. In terms of EnergyPlus objects this is:

1.    Fan:ConstantVolume

2.    AirLoopHVAC:ZoneMixer

3.    Coil:Heating:Water, Coil:Heating:Electric, Coil:Heating:Gas or Coil:Heating:Steam

Both units are forward models: their inputs are defined by the state of their inlets: namely the air inlet and the hot water  inlet. The outputs of the models are the conditions of the outlet air stream: flow rate, temperature and humidity ratio. The PIUs data and simulation are encapsulated in the module *PoweredInductionUnits*. The main simulation routine for the units within the module are *CalcSeriesPIU* and *CalcParallelPIU*.

#### Inputs and Data

The user describes the PIU by inputting the names of the mixer, fan and heating coil sub-components plus the type of the heating coil sub-component. The user must connect the unit to the overall HVAC systems by naming various inlet and outlet nodes: the primary air inlet node, the secondary air inlet node, the unit air outlet node, and the hot water inlet node. One internal node name is needed: the coil air inlet node (same as fan outlet mode for series, mixer outlet node for parallel). Design flow rates need to be specified (although these can be autosized): maximum total air flow rate (series), maximum primary air flow rate, minimum primary air flow fraction, maximum secondary air flow rate (parallel), fan-on flow fraction (parallel), and maximum and minimum hot water flow rates. A convergence tolerance needs to be specified for units with hot water coils. There is an on/off availability schedule that need to be set.

All input data for the powered induction terminal units is stored in the array *PIU*.

#### Calculation

The calculation is performed by simulating the sub-components in the order given above. Most of the code is involved with figuring out what the various flow rates should be before simulating the subcomponents. These calculations are described below.

#### Simulation and Control

*Series*

From the result of the zone simulation we have the heating/cooling demand on the terminal unit <span>\({\dot Q_{tot}}\)</span>. The subroutine *CalcSeriesPIU* needs to determine the flow rates that will allow the unit to meet this load. The first step is to determine the on/off state of the unit and determine the air flow rates.

·        If the unit is scheduled off, the primary and secondary flow rates are set to zero.

·        If there is no primary air flow (or less than .001 kg/s), the primary air flow is set to zero and the secondary air flow is set to the constant total air flow input by the user.

·        If the zone temperature is in the deadband or the zone load is less than 1 watt or the zone needs heating, the primary air flow rate is set to the minimum flow rate specified by the input and the secondary air flow rate is set to the difference between the fixed total air flow rate and the primary air flow rate.

·        Otherwise, the zone needs cooling and the unit is active.

§ We determine the fan temperature rise: the secondary air flow is set to the max total air flow, primary air flow to zero, and the mixer and fan are simulated. The fan delta T is the difference between the temperature at the fan’s outlet node and inlet node.

§ We calculate the unit air outlet temperature needed to meet the zone cooling load: <span>\({T_{out}} = C{p_h} + {\dot Q_{z,req}}/({\dot m_{air,tot}}\cdot {c_{p,air,z}})\)</span>

§ The temperature needed at the outlet of the mixer is then: <span>\({T_{mix}} = {T_{out}} - \Delta {T_{fan}}\)</span>

§ We can then set the primary air flow rate.

·        If 
         <span>\({T_{mix}} \le {T_{in,pri}}\)</span>
         then 
         <span>\({\dot m_{pri}} = {\dot m_{pri,max}}\)</span>

·        else if 
         <span>\({T_{mix}} \ge {T_{in,pri}}\)</span>
         and 
         <span>\({T_{mix}} \ge {T_{in,sec}}\)</span>
         then 
         <span>\({\dot m_{pri}} = {\dot m_{pri,min}}\)</span>

·        otherwise 
         <span>\({\dot m_{pri}} = {\dot m_{air,tot}}\cdot ({T_{in,sec}} - {T_{mix}})/({T_{in,sec}} - {T_{in,pri}})\)</span>
         , subject to the constraints that the flow rate can’t be bigger than the max and min allowed.

The air flow rates are now determined and we can fire the air mixer (Call *SimAir-Mixer*) and fan (Call *SimulateFanComponents*) component simulations. Finally we simulate the heating coil:

·        for a hot water coil, if the coil is off (no flow, deadband, no load) just fire the coil simulation once (Call *SimulateWaterCoilComponents*). Otherwise call *ControlCompOutput*; *ControlCompOutput* is a general component control routine. In this case it calls *SimulateWaterCoilComponents* repeatedly while varying the hot water flow rate and minimizing

<div>$$({c_{p,air}}\cdot {\dot m_{air,tot}}\cdot ({T_{out}} - {T_z}) - {\dot Q_{z,req}})/{\dot Q_{z,req}}$$</div>

to within the heating convergence tolerance.

·        For gas, electric or steam coils, the required coil output is set to <span>\({\dot Q_{coil,req}} = {\dot Q_{z,req}} - {c_{p,air}}\cdot {\dot m_{air,coil}}({T_{air,coil,in}} - {T_z})\)</span>. Then the coil simulation is fired (Call *SimulateHeatingCoilComponent* or *SimulateSteamCoilCompo-nents*).

Finally the unit sensible output is calculated:

<div>$${\dot Q_{sens,out}} = {\rm{PsyHFnTdbW}}({T_{air,out}},{W_z}) - {\rm{PsyHFnTdbW(}}{T_z},{W_z})$$</div>

where *PsyHFnTdb* is the EnergyPlus function giving enthalpy as a function of temperature and humidity ratio.

*Parallel*

From the result of the zone simulation we have the heating/cooling demand on the terminal unit <span>\({\dot Q_{tot}}\)</span>. The subroutine *CalcParallelPIU* needs to determine the flow rates that will allow the unit to meet this load. The first step is to determine the on/off state of the unit and determine the air flow rates.

·        If the unit is scheduled off, the primary and secondary flow rates are set to zero.

·        If there is no primary air flow (or less than .001 kg/s), the primary air flow is set to zero and the secondary air flow is set to the max secondary air flow input by the user.

·        If the zone temperature is in the deadband or the zone load is less than 1 watt or the zone needs heating, the primary air flow rate is set to the minimum flow rate specified by the input and the secondary air flow rate is set to max secondary air flow input by the user.

·        Otherwise, the zone needs cooling and the unit is active.

§ We determine the fan temperature rise: the secondary air flow is set to the max secondary air flow, primary air flow to zero, and the fan and mixer are simulated. The fan delta T is defined as the difference between the temperature at the mixer outlet node and the fan inlet node.

§ Assuming that the fan is off, we calculate the primary air flow needed to meet the cooling demand.

<div>$${\dot m_{pri}} = {\dot Q_{z,req}}/({c_{p,air}}\cdot ({T_{in,pri}} - {T_z}))$$</div>

The flow rate is constrained to be between the min and max primary air flow rates. If this calculated primary flow rate is greater than the fan-on flow rate, the secondary flow rate is set to zero and we are done. Otherwise, the fan is on and we need to recalculate the primary air flow rate.

<div>$${\dot m_{pri}} = ({\dot Q_{z,req}} - {c_{p,air}}\cdot {\dot m_{sec}}\cdot ({T_{in,sec}} + \Delta {T_{fan}} - {T_z}))/({c_{p,air}}\cdot ({T_{in,pri}} - {T_z}))$$</div>

The secondary flow rate is set to the user input fixed flow rate. The primary air flow rate is constrained to be between the min and max primary flow rated.

The air flow rates are now determined and we can fire the fan (Call *SimulateFanComponents*) and air mixer (Call *SimAirMixer*) component simulations. Finally we simulate the heating coil:

·        for a hot water coil, if the coil is off (no flow, deadband, no load) just fire the coil simulation once (Call *SimulateWaterCoilComponents*). Otherwise call *ControlCompOutput*; *ControlCompOutput* is a general component control routine. In this case it calls *SimulateWaterCoilComponents* repeatedly while varying the hot water flow rate and minimizing

<div>$$({c_{p,air}}\cdot {\dot m_{air,tot}}\cdot ({T_{out}} - {T_z}) - {\dot Q_{z,req}})/{\dot Q_{z,req}}$$</div>

to within the heating convergence tolerance.

·        For gas, electric or steam coils, the required coil output is set to <span>\({\dot Q_{coil,req}} = {\dot Q_{z,req}} - {c_{p,air}}\cdot {\dot m_{air,coil}}({T_{air,coil,in}} - {T_z})\)</span>. Then the coil simulation is fired (Call *SimulateHeatingCoilComponent* or *SimulateSteamCoilCompo-nents*).

Finally the unit sensible output is calculated:

<div>$${\dot Q_{sens,out}} = {\rm{PsyHFnTdbW}}({T_{air,out}},{W_z}) - {\rm{PsyHFnTdbW(}}{T_z},{W_z})$$</div>

where *PsyHFnTdb* is the EnergyPlus function giving enthalpy as a function of temperature and humidity ratio.

#### References

No specific references.

### Variable Air Volume Fan Powered Single Duct Air Terminal

#### Overview

The input object AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan provides a model for variable speed (VS) fan VAV terminal unit that is a typical VAV reheat unit with the addition of a variable-speed blower fan to assist in moving supply air from the plenum to the conditioned zone. It is typically used with under-floor air distribution systems (UFAD) where the supply air is sent at low static pressure through an under-floor supply plenum. The fan has two maximum flow rate settings: one for cooling and one for heating. The cooling maximum is usually the actual fan maximum while the heating maximum is a lesser flow rate. The fan is upstream of the heating coil (this is a blow-through unit). The heating coil can be hot-water, electric or gas. Cooling control is obtained by varying the supply air flow rate from the cooling maximum to the minimum flow rate. Heating control is established by varying both the heating coil output (or hot water flow rate for hot water coils) and the supply air flow rate. Note that for this unit the minimum supply air flow rate is the flow rate when the fan is off.

#### Model

The VS fan VAV terminal unit is modeled as a compound component consisting of two sub-components: a fan and a heating coil. In terms of EnergyPlus objects the fan is a *Fan:VariableVolume* object and the heating coil is a *Coil:Heating:Water*, *Coil:Heating:Electric* or a *Coil:Heating:Gas*. The terminal unit is a forward model: its inputs are defined by the state of its inlets: namely its air inlet and its hot water  inlet, if it has a hot water coil. The outputs of the model are the conditions of the outlet air stream: flow rate, temperature and humidity ratio. The terminal unit data and simulation are encapsulated in the module *SingleDuct*. The main simulation routine for the unit within the module is *SimVAVVS*.

#### Inputs and Data

The user describes the terminal unit by inputting the name and type of the heating coil and the name and type of the fan. The user must also specify the connectivity of the component by naming the inlet air node; the air node connecting the fan and heating coil (fan outlet, coil inlet); the unit air outlet node (same as the zone inlet node); and hot water inlet node (if any). Maximum flow rates need to be specified (although these can be autosized): maximum cooling and heating air flow rates and the maximum hot water flow rate (if there is a hot water coil). Minimum flow rates are specified by giving by giving a minimum flow fraction for the air flow and a volumetric flow rate minimum for the hot water. For the units with hot water coils the relative convergence tolerance for the hot water flow rate also needs to be input (or allowed to default).

All input data for the VS fan VAV terminal units is stored in the array *Sys*.

#### Calculation

Given the needed inputs, the output is calculated in subroutine *CalcVAVVS*. The temperature and humidity of the supply air stream are taken from the inlet air node. The inlet air flow rate and the hot water flow rate are passed in as parameters. If the coil is electric or gas the coil heating power is passed instead of the hot water flow rate. Then

The fan is simulated (call *SimulateFanComponents*). If the fan is off the fan outlet conditions are set to the inlet conditions.

The heating coil is simulated (call *SimulateWaterCoilComponents* if the coil is a hot water coil; call *SimulateHeatingCoilComponents* if the coil is gas or electric).

Finally the sensible load met by the terminal unit is calculated and passed back to the calling routine:

<div>$${\dot Q_{out}} = {\dot m_{air}}\cdot {c_{p,air}}\cdot ({T_{air,out}} - {T_{air,zone}})$$</div>

Note that data is never explicitly passed between the sub-components. This is all handled automatically by the node connections and the data stored on the nodes.

#### Simulation and Control

From the result of the zone simulation we have the heating/cooling demand on the terminal unit 
<span>\({\dot Q_{tot}}\)</span>
. For the given inlet conditions *CalcVAVVS* will give us the terminal unit heating/cooling output. We need to vary the air or hot water flow rate or the heating coil power (for gas or electric coils) to make the unit output match the demand. To do this we need to numerically invert *CalcVAVVS*: given the output, we want one of the inputs – the air or hot water flow rate or the heating coil power. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function (the *residual* function) of a single independent variable. In this case the residual function calculates 
<span>\(({\dot Q_{tot}} - {\dot Q_{out}})/{\dot Q_{tot}}\)</span>
. *SolveRegulaFalsi* varies either the air mass flow rate, the hot water mass flow rate or the heating coil power to zero the residual.

The unit is simulated in the following sequence.

Decide whether the unit is on or off. The unit is off if: a) it is scheduled off; b) the inlet air mass flow rate is zero; or c) the zone thermostat is in the deadband

If the unit is off, call *CalcVAVVS* with flow rates set to their minimum flows and return.

If the unit is on, we need to establish the boundaries of 4 conditioning regions: a) active cooling with fan on; b) active heating with fan on; c) active heating with fan off; d) passive cooling with fan off. The heating/cooling demand will fall into one of these regions. Once the correct region is determined, we will know which model input to vary for control and thus how to invert the calculation.

To establish the boundaries of region a) we call *CalcVAVVS* twice: once with the supply air flow rate set to the cooling maximum, once with the cooling air flow rate set to the minimum. In both cases the heating coil output is at the minimum and the fan is on. Call the 2 cooling outputs 
<span>\({\dot Q_{cool,max,fanon}}\)</span>
and 
<span>\({\dot Q_{cool,min,fanon}}\)</span>
. Remembering that EnergyPlus convention is that cooling loads are negative, then if 
<span>\({\dot Q_{tot}} &lt; {\dot Q_{cool,max,fanon}}\)</span>
the terminal unit can not meet the demand. Set the air mass flow rate to the cooling maximum and call *CalcVAVV* again. This concludes the simulation. If  
<span>\({\dot Q_{cool,max,fanon}} &lt; {\dot Q_{tot}} &lt; {\dot Q_{cool,min,fanon}}\)</span>
the cooling demand is in the active cooling region. We hold the heating at the minimum, allow the supply air flow to vary between the cooling maximum and the minimum with the fan on, and call *SolveRegulaFalsi* to obtain the supply air flow rate that will produce the unit sensible cooling output that matches the demand. This concludes the simulation.

To establish the boundaries of region b) call *CalcVAVVS* twice: once with the supply air flow rate set to the heating maximum, once with the supply air flow rate set to the minimum. In both calls, if the heating coil is a hot water coil, the hot water flow rate is at the maximum. For electric and gas coils, the heating power is set to the maximum at maximum supply air flow and to zero at the minimum supply air flow. In both calls the fan is set to be on. Call the 2 heating outputs returned from the two calls to *CalcVAVVS* 
<span>\({\dot Q_{heat,max,fanon}}\)</span>
and 
<span>\({\dot Q_{heat,min,fanon}}\)</span>
. If 
<span>\({\dot Q_{heat,max,fanon}} &lt; {\dot Q_{tot}}\)</span>
the terminal unit can not meet the load. Set the air flow rate to the heating maximum and the hot water flow rate or heating coil power to the maximum and call *CalcVAVVS* again. This concludes the simulation for this case. If 
<span>\({\dot Q_{heat,min,fanon}} &lt; {\dot Q_{tot}} &lt; {\dot Q_{heat,max,fanon}}\)</span>
the heating demand is in the active heating, fan on region. For a hot water coil we call *SolveRegulaFalsi* with the supply air flow rate as the input that is varied and the hot water flow rate set to the maximum. For electric and gas coils the coil power and the supply air flow rate are both varied together from their minimum to maximum in a call to *SolveRegulaFalsi*.  The call to *SolveRegulaFalsi* concludes the simulation for this case.

This region only applies to terminal units with a hot water coil. To establish the boundaries of region c) the fan is set to off, the supply air flow rate is set to minimum flow and *CalcVAVVS* is called twice: once with the hot water flow at maximum and once with the hot water flow at minimum. Call the two heating outputs 
<span>\({\dot Q_{heat,max,fanoff}}\)</span> 
and <span>\({\dot Q_{tot}}\)</span>
. If 
<span>\({\dot Q_{tot}}\)</span>
 is between these values, the supply air flow rate is set to its minimum, the fan is set to off, and in the call to *SolveRegulaFalsi* the hot water flow rate is varied to meet the load. This concludes the simulation for this case.

If the cooling demand does not fall into cases a) – c), the unit is assumed to be in the passive cooling state: heating is off or at the minimum, the fan is off, and the minimum supply air flow is delivered to the zone.

Note that the terminal unit output is never explicitly passed to another routine. Instead the output is saved as the outlet conditions on the terminal unit outlet air node. The node data is accessed when the terminal unit output is needed elsewhere in the program (in *SimZoneAirLoopEquipment* for instance).

#### References

No specific references.

### Cooled Beam Unit (AirTerminal:SingleDuct:ConstantVolume:CooledBeam)

Cooled beam (frequently called chilled beams) systems are usually hybrid water – air systems. Commonly there is a constant flow, fixed temperature central forced air system for meeting ventilation and latent load requirements.  Sometimes this forced air system’s flow rate is varied according to ventilation demand; and of course its supply air temperature could be reset in various ways. Sensible cooling load is met by the cooled beam units; these are ceiling suspended units with cool water circulating through them. Some types of units are “passive” – they cool by radiation and natural convection. Other types of units are “active” and act as supply air terminal units with the supply air inducing room air over the beam cooling elements. These units cool almost entirely by convection. The DOE-2 model (upon which this model is based) is a convection only model – even the “passive” units are assumed to operate 100% convectively. The cooled beam elements act as an alternative to normal ceiling radiant cooling: they are not coupled to the building mass and they operate more in a convective mode, but they can, like radiant cooling, use fairly warm cooling water.

Heating is accomplished separately from the cooled beam system – usually baseboards are used on the building perimeter to meet heating loads.

#### Model

The chilled beam system is modeled as an EnergyPlus terminal unit. In terms of configuration within the overall HVAC system it will resemble a 4 pipe induction terminal unit. The user describes the system as a typical single duct constant volume system (with outside air mixer, fan, heating and cooling coils) on the air loop side, and with cooled beam terminal units on the zone equipment side.

The model is an empirical model developed at the equipment manufacturer Halton Oy. It consists of the following relationships.

*P<sub>beam</sub>* = *A·K·DT       *                         beam cooling output per unit length W/m

*K=a·DT<sup>n1</sup>·vr<sup>n2</sup>·w<sup>n3</sup>*                            coil heat transfer coefficient W/(m<sup>2</sup>K)

*vr=(q<sub>in</sub>*/a*<sub>0</sub>)·r<sub>air</sub>*                                  room air mass flow rate across coil kg/(m<sup>2</sup>s)

*q<sub>in</sub>=K<sub>1</sub>·DT<sup>n</sup>+K<sub>in</sub>·q<sub>pr</sub>*                             room air volumetric flow rate across coil per

                                                      unit length m<sup>3</sup>/(s-m)

DT is the room air –water temperature difference (average water temperature is used) in degrees C.

w is the water velocity in m/s.

q<sub>pr</sub> is the supply air flow rate per unit length m<sup>3</sup>/(s-m)

The other symbols are the model parameters input by the user (see the IO Ref for descriptions).

#### Inputs and Data

The user describes the unit by inputting the name, referencing an availability schedule, and choosing a type (*active* or *passive*). The user must also specify the connectivity of the component by naming the inlet and outlet air and water nodes. The maximum water and fixed air flow rates need to be specified (although these can be autosized). The design inlet and outlet water temperatures are inputs. Generally the inlet water temperature is quite warm (15C is the default) and the temperature rise is small (design outlet water temperature defaults to 17C). Two key inputs are the number of beams (in the zone) and the beam length. It is generally wise to let these inputs autosize.

The remaining inputs are parameters specific to the product model. Good defaults are supplied and they should not be changed without information from the manufacturer.

#### Sizing

The Cooled Beam sizing calculations generally follow the procedures used for other terminal units (see *Loop Equipment Sizing*). One difference is that the Cooled Beams use the Cooled Beam inputs *Design Inlet Water Temperature* and *Design Outlet Water Temperature* for the chilled water DT rather than the DT from Plant Sizing. There are also two inputs unique to the Cooled Beam units that are autosized and will be described here.

The input *Number of individual beam units in the zone* is autosized by dividing the beam system zone design chilled water flow rate (either input by the user or autosized) by a nominal chilled water beam flow rate: 0.07 kg/s.

The input *Length of an individual beam unit* is autosized by using the model equations to calculate the length. The inputs to the equations are:

1)    the design load per beam. The design load is calculated from the design water mass flow rate and the design water inlet and outlet temperatures. The design load is divided by the number of beams to obtain the design load per beam.

2)    The design air supply air flow per beam – obtained by dividing the design supply air flow by the number of beams.

3)    The design water flow per beam (m<sup>3</sup>/s) – obtained by dividing the design water flow by the number of beams.

4)    The design water velocity – obtained by dividing the design water flow per beam by the cross sectional inside area of a water tube (pD<sup>2</sup>/4, where D is the input *Pipe inside diameter*.

5)    Average air to water *DT = T<sub>z,\\ cool\\ peak</sub>* - *0.5(T<sub>w,des\\ inlet</sub>* *+ T<sub>w,des\\ outlet</sub>)*; where        *T<sub>z,\\ cool\\ peak</sub>* is the zone air temperature at the cooling peak and the *T<sub>w,des</sub>* ‘s are the water design inlet and outlet temperatures.

With these inputs the model equations can be solve directly for beam length for passive cooled beams, and iteratively for active cooled beams.

#### Calculation

The subroutine *CalcCoolBeam*  uses the model equations to calculate the cooling power *P<sub>beams,out</sub>* delivered to the room air and the outlet water temperature given the water flow rate (and the room air temperature and water inlet temperature). Since the model equations are nonlinear they must be solved iteratively. The subroutine does this by varying the outlet water temperature *T<sub>w,out</sub>* and calculating the water-side cooling power

P<sub>w</sub> = q<sub>w,beam</sub>·c<sub>p,w</sub>·(T<sub>w,out</sub>-T<sub>w,in</sub>)

and comparing it to the air-side cooling power

P<sub>air</sub>=K·A·DT·L<sub>beam</sub>

where *q<sub>w,beam</sub>* is the water mass flow rate (kg/s) per beam and *L<sub>beam</sub>* is the length of a beam (m). When *P*<sub>w</sub>  and *P<sub>air</sub>* match to within 0.1 W the subroutine terminates the iteration.

#### Simulation and Control

From the result of the zone simulation we have the heating/cooling demand for the zone equipment. For the cooling demand, we use the load to cooling set point *P<sub>c</sub>*. Part of the demand may be satisfied by the zone supply air:

P<sub>sup</sub>=q<sub>air</sub>·(c<sub>p,air,sys</sub>·T<sub>sys</sub> - c<sub>p,air,z</sub>·T<sub>z</sub>)

The demand on the actual beams is then

P<sub>beams,dem</sub>=P<sub>c</sub> - P<sub>sup</sub>

We want to know the chilled water flow rate that will give a beam cooling output of *P<sub>beams</sub>*. To obtain this we need to numerically invert *CalcCoolBeam*: given its desired output, we want to know the chilled water flow rate. This numerical inversion is carried out by calling the subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function (the *residual* function) of a single independent variable. In this case the residual function is basically

(P<sub>beams,out</sub>-P<sub>beams,dem</sub>)/P<sub>beams,out,max</sub>

*SolveRegulaFalsi* varies the cold water mass flow rate to zero the residual. The water inlet and outlet node flow rates are set to the flow rate found by *SolveRegulaFalsi* and the water outlet node temperature is set to the outlet water temperature from *SolveRegulaFalsi*.

#### References

Documentation Package Update \#2 for DOE-2.1E, Version 107, page 3.152 describes the input and the model for the DOE-2 cooled beam model.

### Constant Volume Dual Duct Air Terminal

#### Overview

The input object AirTerminal:DualDuct:ConstantVolume provides a model for dual duct constant-air-volume (DDCAV) systems that are typically used in special applications where precise temperature and humidity control are required and energy efficiency is not of primary concern. Thermal control for each zone is achieved by mixing air from the hot deck with air from the cold deck to achieve a supply air temperature that will exactly meet the zone load and satisfy the zone thermostat demand. Each zone has its own mixing box which is connected directly to the hot and cold decks. The mixing box dampers change the relative amount of hot and cold air that will be delivered (at a constant volumetric flow rate) to the zone.

#### Model Description

The DDCAV model will attempt to meet all of the thermostatic loads of a particular zone by explicitly calculating the hot and cold deck mass flow rates. For the energy and mass balance equations shown below, the zone load, temperatures, specific heats and the design mass flow rate are all known. These equations can then be solved directly for the hot deck and cold deck mass flow rates.

<div>$$\dot Q_{zone} = \dot m_c C_{p,c} T_c + \dot m_h C_{p,h}T_h - \dot m_d C_{p,z} T_z$$</div>

<div>$$\dot m_d = \dot m_c + \dot m_h$$</div>

Where:

<span>\(\dot Q_{zone}\)</span>= Zone load, W (positive=heating, negative=cooling)

<span>\(C{p_z}\)</span>= Specific heat of zone air, J/kg-K

<span>\(C{p_c}\)</span>= Specific heat of cold deck air, J/kg-K

<span>\(C{p_h}\)</span>= Specific heat of hot deck air, J/kg-K

<span>\({T_z}\)</span>= Zone air dry-bulb temperature, °C

<span>\({T_c}\)</span>= Cold deck air dry-bulb temperature, °C

<span>\({T_h}\)</span>= Hot deck air dry-bulb temperature, °C

<span>\({\dot m_d}\)</span>= System design air mass flow rate through both heating or cooling duct, kg/s

<span>\({\dot m_c}\)</span>= Cold deck air mass flow rate, kg/s

<span>\({\dot m_h}\)</span>= Hot deck air mass flow rate, kg/s

#### Simulation and Control

The simulation first calculates the hot deck and cold deck air mass flow rates required to satisfy the heating/cooling demand on the zone. Once the individual flow rates have been calculated based on temperature control, the zone mixed air conditions are calculated assuming adiabatic mixing of the two air streams.

### Variable Air Volume Dual Duct Air Terminal

#### Overview

The input object AirTerminal:DualDuct:VAV provides a model for dual duct variable-air-volume (DDVAV) systems that are typically used in special applications where both temperature and humidity control as well as energy efficiency are of primary concern.  This system combines the advantages of the standard dual duct system for better thermal control with the possibility to reduce fan energy using a variable speed fan. The DDVAV terminal units contain actuated dampers that vary the amount of central system air supplied to a zone from both the hot and cold deck. Optional user inputs may also be used to control the amount of outdoor air entering the zone.

The DDVAV terminal units described here are used primarily with central air handling equipment with cooling and heating capability. The terminal unit dampers modulate the amount of cold air and hot air as well as the overall flow rate to maintain the zone setpoint temperature(s).

#### Model Description

The DDVAV model will attempt to meet all of the thermostatic loads of a particular zone by first sending air through either the heating duct or the cooling duct depending on whether there is a heating or cooling load (respectively).  Flow rate through the opposite duct is kept at zero and flow through the active duct is varied between the minimum air flow rate (minimum zone air fraction multiplied by the maximum flow rate) and the maximum air flow rate.  If the flow rate to meet the load through either the heating or cooling duct results in a flow outside these ranges, then air must be passed through the other duct as well to avoid over- or under-heating or –cooling. This is done using a conservation of energy and mass analysis of the terminal unit as well as the known inlet and necessary outlet condition to meet the thermal needs of the zone.

When there is no load on the zone, the system could either be scheduled off or be in a “no load” condition.  If the system is scheduled off, the model keeps the flow rate at zero for both the heating and cooling duct.  If in a no load condition, the system attempts to throttle back to the minimum possible flow and then find a balance between flow through the heating and cooling duct that will provide no net conditioning to the space.  This means that the enthalpy of air delivered to the space must be equal to the enthalpy of the (average) air in the zone.

#### DDVAV Terminal Unit Inputs

Like other terminal units, the DDVAV terminal unit requires an availability schedule and inlet and outlet node designations.  The DDVAV terminal unit, like the DD terminal unit, has two inlet nodes (one for the heating duct and one for the cooling duct) and one outlet node.

In addition, the DDVAV terminal unit also has a maximum flow rate and a minimum flow fraction like the VAV terminal unit.  This allows the flow to be throttled back when it is possible to provide the proper amount of conditioning with less flow.  The maximum flow rate can be auto-sized, if desired.

#### Minimum Outdoor Air Control

This dual duct air terminal may also be used to provide a minimum outdoor air quantity. When the air flow rate required to meet the zone load does not provide sufficient outdoor air, the terminal device damper will open to allow sufficient outdoor air to enter the zone. In this case, the terminal damper is controlled based on the air loop’s outdoor air fraction. The outdoor air may be specified as a fixed value per person, per floor area, or per zone or as the required minimum air changes per hour. In addition, these values may be added together to provide a combined minimum outdoor air flow rate or the maximum of each of these values may be used. An outdoor air fraction schedule may also be used to modify the calculation for the minimum amount of outdoor air throughout the simulation (Ref. DesignSpecification:OutdoorAir).

#### Simulation and Control

The simulation begins by determining the air mass flow rate required to satisfy the heating/cooling demand using either the heating duct or cooling duct.

<div>$$C{p_{zone}} = \dot m\left( {C{p_{inlet}},{T_{zone}}} \right)$$</div>

<div>$$C{p_{zone}} = PsyCpAirFnWTdb\left( {{T_{zone}},{T_{inlet}}} \right)$$</div>

<div>$$DeltaCpT = \left( {C{p_{inlet}}} \right)\left( {{T_{inlet}}} \right) - \left( {C{p_{zone}}} \right)\left( {{T_{zone}}} \right)$$</div>

<div>$$ \dot m = \min \left( \dot m_{max}, \max \left( \dot m_{max}\cdot MinAirFlowFrac,\frac{\dot Q_{zone}}{DeltaCpT} \right) \right) $$</div>

where

<span>\(C{p_{zone}}\)</span>  = Specific heat of zone air, J/kg-K

<span>\(C{p_{inlet}}\)</span>  = Specific heat of terminal unit inlet air, J/kg-K

<span>\({\omega_{zone}}\)</span>    = Zone air humidity ratio, kg/kg

<span>\({T_{zone}}\)</span>     = Zone air dry-bulb temperature, °C

<span>\({\omega_{inlet}}\)</span>    = Terminal unit inlet air humidity ratio, kg/kg

<span>\({T_{inlet}}\)</span>     = Terminal unit inlet air dry-bulb temperature, °C

<span>\(\dot m\)</span>    = Zone load, W (positive values denote heating, negative values denote cooling)

<span>\(PsyCpAirFnWTdb\)</span>        = Terminal unit air mass flow rate through either heating or cooling duct, kg/s

<span>\(MinAirFlowFrac\)</span>           = Psychrometric function calculating air specific heat given air humidity ratio and dry-bulb temperature

<span>\(MinAirFlowFrac\)</span>    = User-specified zone minimum air flow fraction

<span>\({\dot m_{max}}\)</span>              = Terminal unit maximum air mass flow rate, kg/s

The outdoor air input requirements, if entered, are then used to adjust the terminal unit air mass flow rate to ensure the correct amount of outdoor air enters the zone (within the constraints of the terminal unit maximum and minimum flow rate inputs). The amount of outdoor air is calculated per the outdoor air requirements and is adjusted by the fraction of outdoor air entering the air loop outdoor air system.

<div>$$\mathop m\limits^\cdot   = MAX\left( {\mathop m\limits^\cdot  ,\,{\raise0.7ex\hbox{${\mathop {{m_{OA}}}\limits^\cdot  }$} \!\mathord{\left/ {\vphantom {{\mathop {{m_{OA}}}\limits^\cdot  } {OAFrac}}}\right.}\!\lower0.7ex\hbox{${OAFrac}$}}} \right)$$</div>

where:

<span>\({\mathop m\limits^\cdot_{OA}}\)</span> = zone outdoor air flow rate, kg/s

<span>\(OAFrac\)</span> = fraction of outdoor air entering the air loop outside air system

The damper position is then calculated as:

<div>$$FRA{C_{damper}} = {\raise0.7ex\hbox{${\dot m}$} \!\mathord{\left/ {\vphantom {{\dot m} {{{\dot m}_{max}}}}}\right.}\!\lower0.7ex\hbox{${{{\dot m}_{max}}}$}}$$</div>

where

<span>\(FRA{C_{damper}}\)</span>  = Output variable ‘Zone Air Terminal VAV Damper Position’, fraction of maximum flow

If the flow rate was between the maximum flow rate and the minimum flow rate for the terminal unit, then no other calculations are needed.  However, if the flow was reset to either the maximum or minimum flow rate, then flow through the active duct must be balanced by flow through the other duct to achieve the proper conditioning.

#### References

No specific references.  Refer to the ASHRAE Handbook series for general information on different system types as needed.

### Dual Duct Dedicated Outside Air Terminal with VAV Cooling

#### Overview

The input object AirTerminal:DualDuct:VAV:OutdoorAir provides a model for dedicated outside air combined with recirculated air for cooling.  This air terminal has two inlets and one outlet. The outdoor air inlet has one damper that is controlled to meet the air flow requirements for ventilation.  The second inlet is for cool recirculated air and has a second damper that is controlled to meet the zone’s cooling loads.  The two streams are then mixed and inlet to the zone.  This unit is for central air systems (using AirLoopHVAC object).  Because of the limitation in EnergyPlus of allowing only one air terminal per zone, the dual duct approach offers advantages in that it allows modeling dedicated outdoor air systems (DOAS) and central VAV cooling at the same time.  The original motivation for adding this terminal was to model twin-fan, twin-coil systems.

The recirculated cool air duct is actually optional. If no node name is input for the recirculated air inlet node, then only the outdoor air duct is operational and the air terminal behaves as a single duct.  This offers additional capabilities for single duct DOAS in that this terminal can request outdoor air flows that change over time but are not controlled to meet zone loads.

#### Model Description

The model attempts to meet the ventilation requirements and the cooling loads of a particular zone.  If the zone requires heating, ancillary heating equipment is needed as this terminal cannot do any heating.  The model first determines the current required outdoor air flow rate for ventilation and then calculates the flow of cool air needed to reach the cooling setpoint.

The outdoor air rate is controlled by the schedule and specifications contained in a DesignSpecification:OutdoorAir object and can be based on flows per person, per zone, per area, or air changes per hour.  Using the key CurrentOccupancy, the per person rate can be set to operate based on the current occupancy level to model demand controlled ventilation. Using the key DesignOccupancy it can be set to operate based on the design, or maximum, level of occupancy.   The outdoor air inlet side of the terminal is assigned a design maximum flow rate based on the largest flow rates specified by the associated DesignSpecification:OutdoorAir object. This maximum for the outdoor air is used to calculate the damper position and contributes to the overall maximum if that is autosized.

The recirculated cool air flow rate is controlled to meet the zone cooling loads.  The first step is to calculate the impact that the outdoor air flow has on the loads starting with the specific heats.

<div>$${c_{p,zone}} = PsyCpAirFnWTdb\left( {{\omega_{zone}},{T_{zone}}} \right)$$</div>

<div>$${c_{p,OA}} = PsyCpAirFnWTdb\left( {{\omega_{OA}},{T_{OA}}} \right)$$</div>

<div>$${c_{p,RC}} = PsyCpAirFnWTdb\left( {{\omega_{RC}},{T_{RC}}} \right)$$</div>

where,

<span>\({c_{p,zone}} = \)</span> specific heat of zone air being served by the terminal unit, J/kg-K

<span>\({c_{p,OA}} = \)</span> specific heat of outdoor air entering the terminal unit, J/kg-K

<span>\({c_{p,RC}} = \)</span> specific heat of the recirculated (cool) air entering the terminal unit (if present), J/kg-K

<span>\({\omega_{zone}} = \)</span>humidity ratio of the zone air, kg/kg

<span>\({\omega_{OA}} = \)</span>humidity ratio of the outdoor air entering the terminal unit, kg/kg

<span>\({\omega_{RC}} = \)</span>humidity ratio of the recirculated air entering the terminal unit, kg/kg

<span>\({T_{zone}} = \)</span>air drybulb temperature of the zone, ºC

<span>\({T_{OA}} = \)</span>air drybulb temperature of the outdoor air entering the terminal unit, ºC

<span>\({T_{RC}} = \)</span>air drybulb temperature of the recirculated cool air entering the terminal unit, ºC

<span>\(PsyCpAirFnWTdb\)</span> is a psychrometric function for calculating the specific heat of moist air as a function of humidity ratio and drybulb temperature.

The contribution to zone load provided by the outdoor air toward meeting the cooling setpoint, <span>\({\dot Q_{OA}}\)</span> (W), is then calculated using:

<div>$${\dot Q_{OA}} = {\dot m_{OA}}\left( {{c_{p,OA}}{T_{OA}} - {c_{p,zone}}{T_{zonesetpoint}}} \right)$$</div>

where,

<span>\({\dot m_{OA}} = \)</span>is the mass flow rate of outdoor air determined by the outdoor air requirement, kg/s

<span>\({T_{zonesetpoint}} = \)</span>is the zone cooling setpoint drybulb temperature, ºC

This is then used to calculate the load that the recirculated cool air should deliver, <span>\({Q_{RC}}\)</span> (W):

<div>$${\dot Q_{RC}} = {\dot Q_{ToCoolSetpointRemain - }}{\dot Q_{OA}}$$</div>

where,

<span>\({\dot Q_{ToCoolSetpointRemain = }}\)</span> is the remaining load to cooling setpoint as determined by Predictor and including the impacts of any other zone equipment sequenced before this terminal. Then the recirculated cool air mass flow rate, <span>\({\dot m_{RC}}\)</span> (kg/s), is calculated using:

<div>$${\dot m_{RC}} = \frac{{{{\dot Q}_{RC}}}}{{\left( {{c_{p,RC}}{T_{RC}} - {c_{p,zone}}{T_{zone}}} \right)}}$$</div>

The model also includes a form of damping where the last three values for <span>\({\dot m_{RC}}\)</span> are stored and used to detect if the solution is oscillating from one iteration to the next and if it is then the new value is not used but rather the value from the previous iteration is used.  Once the two mass flows are known, the moist air properties of the outlet node are calculated using mass flow weighting.

#### References

Sekhar, S. C., K. W. Tham, et al. (2004). Development of energy-efficient single-coil twin-fan air-conditioning system with zonal ventilation control, Nashville, TX, United states, Amer. Soc. Heating, Ref. Air-Conditoning Eng. Inc.

Boilers <a name="Boilers"></a>
-------

### Simple Hot Water Boiler

The input object Boiler:HotWater provides a simple model for boilers that only requires the user to supply the nominal boiler capacity and thermal efficiency. An efficiency curve can also be used to more accurately represent the performance of non-electric boilers but is not considered a required input. The fuel type is input by the user for energy accounting purposes.

The model is based the following three equations

<div>$$OperatingPartLoadRatio = \frac{{BoilerLoad}}{{BoilerNomCapacity}}$$</div>

<div>$$TheoreticalFuelUse = \frac{{BoilerLoad}}{{NominalThermalEfficiency}}$$</div>

<div>$$FuelUsed = \frac{{TheoreticalFuelUse}}{{BoilerEfficiencyCurveOuput}}$$</div>

-or-

<div>$$FuelUsed = \frac{{BoilerLoad}}{{\left( {NominalThermalEfficiency} \right)\left( {BoilerEfficiencyCurveOutput} \right)}}$$</div>

The final equation above includes the impact of the optional boiler efficiency performance curve. To highlight the use of the normalized boiler efficiency curve, the fuel use equation is also shown in an expanded format. The normalized boiler efficiency curve represents the changes in the boiler’s nominal thermal efficiency due to loading and changes in operating temperature. If the optional boiler efficiency curve is not used, the boiler’s nominal thermal efficiency remains constant throughout the simulation (i.e., BoilerEfficiencyCurveOutput = 1).

When a boiler efficiency performance curve is used, any valid curve object with 1 or 2 independent variables may be used. The performance curves are accessed through EnergyPlus’ built-in performance curve equation manager (curve objects). The linear, quadratic, and cubic curve types may be used when boiler efficiency is solely a function of boiler loading, or part-load ratio (PLR). These curve types are used when the boiler operates at the specified setpoint temperature throughout the simulation. Other curve types may be used when the boiler efficiency can be represented by both PLR and boiler operating temperature. Examples of valid single and dual independent variable equations are shown below. For all curve types, PLR is always the x independent variable. When using curve types with 2 independent variables, the boiler water temperature (Twater) is always the y independent variable and can represent either the inlet or outlet temperature depending on user input.

#### Single independent variable:

* <span>\(BoilerEfficiencyCurve = C1 + C2\left( {PLR} \right)\)</span> (Linear)

* <span>\(BoilerEfficiencyCurve = C1 + C2\left( {PLR} \right) + C3{\left( {PLR} \right)^2}\)</span> (Quadratic)

* <span>\(BoilerEfficiencyCurve = C1 + C2\left( {PLR} \right) + C3{\left( {PLR} \right)^2} + C4{(PLR)^3}\)</span> (Cubic)

#### Dual independent variables:

* <span>\(BoilerEfficiencyCurve = C1 + C2\left( {PLR} \right) + C3{\left( {PLR} \right)^2} + \left( {C4 + C5\left( {PLR} \right) + C6{{\left( {PLR} \right)}^2}} \right)\left( {Twater} \right)\)</span> (QuadraticLinear)

* <span>\(BoilerEfficiencyCurve = C1 + C2\left( {PLR} \right) + C3{\left( {PLR} \right)^2} + C4\left( {Twater} \right) + C5{(Twater)^2} + C6(PLR)(Twater)\)</span> (Biquadratic)

* <span>\(BoilerEfficiencyCurve = C1 + C2\left( {PLR} \right) + C3{\left( {PLR} \right)^2} + C4\left( {Twater} \right) + C5{(Twater)^2} + C6\left( {PLR} \right)\left( {Twater} \right) + C7{(PLR)^3} + C8{(Twater)^3} + C9{\left( {PLR} \right)^2}\left( {Twater} \right) + C10\left( {PLR} \right){(Twater)^2}\)</span> (Bicubic)

When a boiler efficiency curve is used, a constant efficiency boiler may be specified by setting C1 = 1 and all other coefficients to 0. A boiler with an efficiency proportional to part-load ratio or which has a non-linear relationship of efficiency with part-load ratio will typically set the coefficients of a linear, quadratic, or cubic curve to non-zero values. Using other curve types allows a more accurate simulation when boiler efficiency varies as a function of part-load ratio and as the boiler outlet water temperature changes over time due to loading or as changes occur in the water temperature setpoint.

The parasitic electric power is calculated based on the user-defined parasitic electric load and the operating part load ratio calculated above. The model assumes that this parasitic power does not contribute to heating the water.

<div>$${P_{parasitic}} = {P_{load}}\left( {PLR} \right)$$</div>

where:

<span>\({P_{parasitic}}\)</span>= parasitic electric power (W), average for the simulation time step

<span>\({P_{load}}\)</span>    = parasitic electric load specified by the user (W)

### Steam Boiler

#### Description of Model

A steam boiler is the essential part of a building steam heating system and can be described as primary driver of the steam loop.  It is the component that maintains the desired loop temperature.

The emphasis in EnergyPlus was laid on developing a building simulation model for steam boiler with ability to model detailed boiler performance without the cost of exhaustive user inputs to the boiler model.  The Boiler:Steam input object is used on the plant loop supply side of EnergyPlus with the primary purpose of supplying steam to the heating coils, which constitute the demand side of the loop.

The steam boiler is a variable mass flow rate device.  The mass flow rate of steam through the boiler is determined by the heating demand on the loop which in turn is determined by the equipment that is hooked to the demand side of the loop, namely the steam coils and hot water heater.  In short, the steam coil determines the mass flow rate of steam required for heating the zone to its required setpoint, the mixer sums up the total steam demanded by each of the individual coils and reports it to the boiler via the pump.

![SteamBoilerInSteamLoop](media/image2932.png)

Figure 158.  Schematic of Steam Boiler in the Steam loop



Figure 158 describes the rudimentary loop structure with steam flowing from coils to boiler.  It is essential to mention that it is the coils that determine the mass of steam required and the boiler simply delivers the required mass flow at desired temperature provided it is adequately sized.  The algorithm for determining the mass flow rate is structured on the demand side and the variable flow boiler has no role to play in determining the steam mass flow.

Figure 159 outlines the simple steam boiler model.  Sub cooled water enters the variable flow boiler through the pump, the boiler inputs energy to water stream consuming fuel, boiler losses are accounted via boiler efficiency.  The boiler delivers steam at a quality equal to 1.0 at saturated condition.

The advantage of steam heating systems over hot water is the high latent heat carrying capacity of steam, which reduces the mass flow rate of the fluid required.  The amount of superheated and sub cooled heat transfer in Steam heating systems is negligible, latent heat transfer accounts for almost all of the heat exchange into the zones via steam to air heat exchangers.

![SteamBoilerOperation](media/image2933.png)

Figure 159.  Schematic of Steam Boiler Operation

Boiler Load is a summation of sensible and latent heat addition to the water stream as described with the following equation.  The mass flow rate through the boiler is known, while delta temp is the temperature difference between the boiler inlet and boiler outlet.  Latent heat of steam is calculated at loop operating temperature.

<div>$${Q_B} = \,\dot m\, \times \left[ {({c_{p,w}} \times \Delta T)\,\,\, + \,\,\,{h_{fg}}} \right]$$</div>

Theoretical fuel used is calculated with the following equation.  Boiler efficiency is a user input and accounts for all the losses in the steam boiler.

<div>$${F_t} = \frac{{{Q_B}}}{{{\eta_B}}}$$</div>

The operation part load ratio is calculated with the following equation.  This is later used to calculate the actual fuel consumption, its ratio of boiler load to boiler nominal capacity.

<div>$${O_{PLR}} = \frac{{{Q_B}}}{{{Q_{B,N}}}}$$</div>

The actual fuel consumption by the boiler is calculated as using the following equation, where C1, C2, and C3 are the Part Load Ratio coefficients.

<div>$${F_{}} = \frac{{{F_t}}}{{{C_1}\,\, + \,\,{C_2}\,\, \times \,\,{O_{PLR}}\,\, + \,\,{C_3} \times \,{O_{PLR}}^2\,}}$$</div>

Essentially the boiler model provides a first order approximation of performance for fuel oil, gas, and electric boilers.  Boiler performance is based on theoretical boiler efficiency and a single quadratic fuel use-part load ratio curve represented in the equation above.  This single curve accounts for all combustion inefficiencies and stack losses.

The control algorithm for a steam boiler is an important issue.  The user may want the boiler to be undersized and in such a case it will not be able to meet the demand side steam flow request.  Subsequently the boiler load exceeds the boiler nominal capacity.  The boiler operates at its nominal capacity but is unable to meet the plant heating demand.  Pseudo code from EnergyPlus has been used to describe the control logic used in the steam boiler simulation.



\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*PSEUDO CODE SECTION STARTS\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

At start of simulation an initial value of steam mass flow rate is calculated.  This is required to start the flow of steam around the loop.

<div>$$If\,\,\,\,(FirstTimeThrough)\,\,\,THEN$$</div>

Calculate the boiler supply steam mass flow rate at start of simulation.

<div>$$\,{\dot m_s}\,\, = \,\,\frac{{{Q_B}}}{{{h_{fg\,\,\, + }}\,\,{c_{p,w}}\,\, \times \Delta {T_{loop}}\,\,}}$$</div>

<div>$$ELSE$$</div>   ! Not first time through

Steam boiler calculations rely heavily on the variable <span>\(\dot m\)</span><sub>b</sub>, boiler mass flow rate.  This variable <span>\(\dot m\)</span><sub>b</sub> is the assigned equal to mass flow at boiler inlet node for preliminary calculations.

<div>$$\,{\dot m_b}\,\,\, = \,\,\,\mathop {\,{{\dot m}_{Inlet\_Node}}}\limits^{} $$</div>

Calculating the boiler delta temperature difference between the inlet and outlet nodes.  This calculation is used to determine various boiler control situation.

<div>$$\Delta {T_{in\_out}}\,\, = \,\,{T_{SP}}\,\, - \,\,{T_{in}}$$</div>

In case the temperature difference calculated with the previous equation equation  is zero then the boiler just needs to supply latent heat to steam, else the boiler performs its normal load calculations by providing both sensible and latent heat to the inlet stream.

<div>$$If\,\,(\Delta {T_{in\_out}}\,\, &lt; \,\,\,\,0\,\,\,)\,\,\,THEN$$</div>

<div>$${Q_B}\,\, = \,\,\,{\dot m_b}\,\,\, \times \,\,{h_{fg}}$$</div>

<div>$$ELSE$$</div>

<div>$${Q_B}\,\, = \,\,\,{\dot m_b}\,\,\, \times \,\,(\,\,\,{h_{fg}} + \,\,{c_{p,w}} \times \Delta {T_{in\_out}}\,\,)$$</div>

<div>$$End\,\,If$$</div>

Sometimes the boiler load Q<sub>B</sub> is greater than the demand side requested load at the current time step, which may occur because the boiler inlet conditions is from previous time step.  There is sudden fall in request of steam mass flow from the demand side.  The boiler now recalculates its new mass flow and adjusts to these new conditions.

<div>$$If\,\,({Q_B}\,\, > \,\,{Q_{HeatingDemand}}\,\,\,)\,\,\,THEN$$</div>

Boiler load is set equal to the new boiler heating demand and steam mass flow rate is recalculated.

<div>$${Q_B}\,\, = \,\,{Q_{HeatingDemand}}$$</div>

<div>$$\,{\dot m_s}\,\,\, = \,\,\frac{{{Q_B}}}{{{h_{fg\,\,\, + }}\,\,{c_{p,w}}\,\, \times \Delta {T_{loop}}\,\,}}$$</div>

<div>$$End\,\,If$$</div>

In case the requested load exceeds the boiler nominal capacity, which is its maximum heating capacity.  In this case the requested steam mass flow is not met and the zone is not heated adequately.  This happens if the boiler is undersized.  The steam mass flow rate is recalculated at nominal capacity.

<div>$$If\,\,({Q_B}\,\, > \,\,{Q_{No\min al\_Capacity}}\,\,\,)\,\,\,THEN$$</div>

Boiler load is set equal to boiler nominal capacity and steam mass flow rate recalculated.

<div>$${Q_B}\,\, = \,\,{Q_{No\min al\_Capacity}}$$</div>

<div>$$\,{\dot m_s}\,\,\, = \,\,\frac{{{Q_B}}}{{{h_{fg\,\,\, + }}\,\,{c_{p,w}}\,\, \times \Delta {T_{loop}}\,\,}}$$</div>

<div>$$End\,\,If$$</div>

<div>$$End\,\,If$$</div>

End If statement for the boiler load control algorithm.  This algorithm determines all possible control conditions that might while simulating a system in EnergyPlus.



\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*PSEUDO CODE SECTION ENDS\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

If the boiler operating pressure exceeds the maximum allowable boiler pressure, the simulation trips and outputs a warning regarding the same.  This notifies the user about potential system pressure sizing problems.

Integration of the steam boiler simulation model in EnergyPlus required developing number of subroutines, which operate in sequence.  These subroutines are designed to read inputs from the input file, initialize the variables used in the boiler simulation model, simulate the boiler performance, update the node connections, and report the required variables.  In case the user has difficulty with boiler inputs, provisions have been made to auto size the boiler nominal capacity and maximum steam flow rate.  These two values play an important role in sizing the boiler.

#### Model Assumptions

The EnergyPlus boiler model is “simple” in the sense that it requires the user to supply the theoretical boiler efficiency.  The combustion process is not considered in the model.  The model is independent of the fuel type, which is input by the user for energy accounting purposes only.  This is an ideal model for Building Simulation Program such that it utilizes the desired amount of resources in terms of simulation run time, but successfully provides fairly good sizing parameters for an actual boiler.

It is assumed that the steam boiler operates to maintain a desired temperature, the temperature being saturation temperature of steam and corresponding to this saturation temperature there exist a single value of saturation pressure at which the loop operates.  Hence the boiler could either be saturation pressure controlled or temperature controlled.  Since users would have better idea of steam temperatures rather than pressure the boiler inputs are designed for temperature control.

#### Nomenclature for Steam Loop

Table 50.  Steam Loop Nomenclature

<table class="table table-striped">
<tr>
<th><span>\({Q_B}\)</span></th>
<th>Boiler Heat Transfer.  W.</th>
</tr>
<tr>
<td><span>\({Q_{B,N}}\)</span></td>
<td>Boiler Nominal Capacity.  W.</td>
</tr>
<tr>
<td><span>\(\,{O_{PLR}}\)</span></td>
<td>Boiler Operating Part Load Ratio. </td>
</tr>
<tr>
<td><span>\(\Delta {T_{sc}}\)</span></td>
<td>Degree of subcooling in coil. </td>
</tr>
<tr>
<td><span>\(\Delta {T_{in\_out}}\)</span></td>
<td>Temperature difference across the steam boiler.  ºC.</td>
</tr>
<tr>
<td><span>\({\rho_w}\)</span></td>
<td>Density of condensate entering the pump.  Kg/m3. </td>
</tr>
<tr>
<td><span>\({Q_{Des}}\)</span></td>
<td>Design Load on the steam coil.  W.</td>
</tr>
<tr>
<td><span>\({h_{f,\,n}}\)</span></td>
<td>Enthalpy of fluid at point n on the Ts diagram.  J/kg.</td>
</tr>
<tr>
<td><span>\({P_{Frac}}\)</span></td>
<td>Fraction of Pump Full Load Power.  W.</td>
</tr>
<tr>
<td><span>\({F_{m,f}}\)</span></td>
<td>Fractional Motor Power Lost to Fluid.  W.</td>
</tr>
<tr>
<td><span>\({Q_{a,l}}\)</span></td>
<td>Heating load on the Air Loop Steam Coil.  W.</td>
</tr>
<tr>
<td><span>\({Q_{z,c}}\)</span></td>
<td>Heating load on the Zone Steam Coil.  W.</td>
</tr>
<tr>
<td><span>\({h_{fg,{T_{Loop}}}}\)</span> </td>
<td>Latent heat of steam at Loop operating Temperature.  J/kg.</td>
</tr>
<tr>
<td><span>\({h_{fg}}\)</span></td>
<td>Latent Heat of Steam.  J/kg.</td>
</tr>
<tr>
<td><span>\({Q_{L,H}}\)</span></td>
<td>Latent Heat Part of the Heating Coil Load.  W.</td>
</tr>
<tr>
<td><span>\(\Delta {Q_{loss}}\)</span></td>
<td>Loop losses in steam coil.  W.</td>
</tr>
<tr>
<td><span>\(\Delta {T_{loop\,\,}}\)</span></td>
<td>Loop Temperature Difference.</td>
</tr>
<tr>
<td><span>\({\dot m_a}\)</span></td>
<td>Mass flow rate for steam coil Kg/s.</td>
</tr>
<tr>
<td><span>\({\dot m_{in}}\)</span></td>
<td>Mass flow rate of steam entering the steam coil .Kg/s.</td>
</tr>
<tr>
<td><span>\({\dot m_{a,l}}\)</span></td>
<td>Mass flow rate of steam for Air loop steam coil Kg/s</td>
</tr>
<tr>
<td><span>\({\dot m_{z,c}}\)</span></td>
<td>Mass flow rate of steam for zone steam coil Kg/s.</td>
</tr>
<tr>
<td><span>\({\dot m_s}\)</span></td>
<td>Mass flow rate of steam.  Kg/s.</td>
</tr>
<tr>
<td><span>\({\dot m_{loop}}\)</span></td>
<td>Mass flow rate of steam for the steam loop.  Kg/s.</td>
</tr>
<tr>
<td><span>\(\dot m\)</span></td>
<td>Mass of condensate entering the pump.  Kg/s.</td>
</tr>
<tr>
<td><span>\({\dot m_{a,\max }}\)</span> </td>
<td>Maximum allowed mass flow rate of air.  Kg/s</td>
</tr>
<tr>
<td><span>\({\dot m_{S,\max }}\)</span></td>
<td>Maximum Mass flow rate of steam Kg/s</td>
</tr>
<tr>
<td><span>\({\dot m_{B,Supply}}\)</span></td>
<td>Maximum steam mass flow rate supplied by boiler.  Kg/s.</td>
</tr>
<tr>
<td><span>\({\dot V_{w,\max }}\,\,\)</span></td>
<td>Maximum Volume flow rate of condensate in pump.  m<sup>3</sup> /s.</td>
</tr>
<tr>
<td><span>\({\dot V_{w,loop}}\)</span></td>
<td>Maximum Volume flow rate of condensate in steam loop.  m<sup>3</sup> /s.</td>
</tr>
<tr>
<td><span>\({T_{a,\,\,in,\,\,\min }}\)</span></td>
<td>Minimum inlet air temperature possible.  ºC.</td>
</tr>
<tr>
<td><span>\({P_n}\,\,\)</span></td>
<td>Nominal Power Capacity for condensate pump.  W.</td>
</tr>
<tr>
<td><span>\({P_{nom}}\)</span></td>
<td>Nominal power of the pump.  W.</td>
</tr>
<tr>
<td><span>\({H_n}\)</span></td>
<td>Nominal Pump Head.  M.</td>
</tr>
<tr>
<td><span>\({\dot V_{nom}}\)</span></td>
<td>Nominal volume flow rate through the condensate pump.  m<sup>3</sup> /s.</td>
</tr>
<tr>
<td><span>\(PLR\)</span></td>
<td>Part Load Ratio for condensate pump.</td>
</tr>
<tr>
<td><span>\({\eta_{\rm{p}}}\)</span></td>
<td>Pump efficiency.</td>
</tr>
<tr>
<td><span>\({\eta_m}\)</span></td>
<td>Pump Motor Efficiency.</td>
</tr>
<tr>
<td><span>\({P_{}}\)</span></td>
<td>Pump Power.  W.</td>
</tr>
<tr>
<td><span>\({Q_{S,H}}\)</span></td>
<td>Sensible Heat Part of the Heating Coil Load.  W.</td>
</tr>
<tr>
<td><span>\({T_{sp}}\)</span></td>
<td>Setpoint Temperature of the zone.  ºC.</td>
</tr>
<tr>
<td><span>\({T_{a,out,SP}}\)</span></td>
<td>Setpoint air outlet temperature for the steam coil.  ºC.</td>
</tr>
<tr>
<td><span>\({P_S}\)</span></td>
<td>Shaft power of the pump.  W.</td>
</tr>
<tr>
<td><span>\(\,\,{c_{p,\,a}}\)</span></td>
<td>Specific Heat Capacity for Air.  J/Kg K.</td>
</tr>
<tr>
<td><span>\(\,\,{c_{p,\,w}}\)</span></td>
<td>Specific Heat Capacity for Water.  J/Kg K.</td>
</tr>
<tr>
<td><span>\({\eta_B}\)</span></td>
<td>Steam Boiler Efficiency.</td>
</tr>
<tr>
<td><span>\(\,Ta,\,i{n_{}}\,\)</span></td>
<td>Temperature of air entering the coil.  ºC.</td>
</tr>
<tr>
<td><span>\({T_a}\)</span></td>
<td>Temperature of air entering the steam coil.  ºC.</td>
</tr>
<tr>
<td><span>\(Ta,out\,\,\,\,\,\)</span></td>
<td>Temperature of air leaving the coil.  ºC.</td>
</tr>
<tr>
<td><span>\(Ts,in\)</span></td>
<td>Temperature of steam entering the coil.  ºC.</td>
</tr>
<tr>
<td><span>\({F_t}\)</span></td>
<td>Theoretical Fuel Consumption by the Steam Boiler.  W.</td>
</tr>
<tr>
<td><span>\({\dot m_{coils,R}}\)</span></td>
<td>Total Mass flow rate requested by all the steam coils.  Kg/s.</td>
</tr>
<tr>
<td><span>\(\dot V\)</span></td>
<td>Volume of condensate entering the pump.  m<sup>3</sup> /s.</td>
</tr>
<tr>
<td><span>\(Tw,\,out\)</span></td>
<td>Water outlet temperature from pump.  ºC.</td>
</tr>

</table>

#### References

ASHRAE Handbook. 1996. HVAC Systems and Equipment, Air Conditioning and Heating Systems.  Chapter 10, Steam Systems.  pp.* * 10.1-10.16. 1996.

*BLAST 3.0 Users Manual*. 1999. Building Systems Laboratory.  Urbana-Champaign: Building Systems Laboratory, Department of Mechanical and Industrial Engineering, University of Illinois.

Chillar, R.J. 2005. “Development and Implementation of a Steam Loop In The Building Energy Simulation Program EnergyPlus,” M.S. Thesis, Department of Mechanical and Industrial Engineering, University of Illinois at Urbana-Champaign.

*TRNSYS 16 User Manual*. 2004. A Transient System Simulation Program. Solar Energy Laboratory, Madison. University of Wisconsin-Madison.

El-Wakil, M. M. 1984. Power Plant Technology, McGraw Hill, New York, pp.   30-72.

Babcock & Wilcox. 1978. Steam-Its Generation and Use, The Babcock & Wilcox Company, New York ,Section I, II, IV, and VII.

S.A. Klein. 2004. Engineering Equation Solver EES. University of Wisconsin Madison.

Chillers <a name="Chillers"></a>
--------

### Absorption Chiller

The input object Chiller:Absorption provides a model for absorption chillers that is an empirical model of a standard absorption refrigeration cycle.  The condenser and evaporator are similar to that of a standard chiller, which are both water-to-water heat exchangers.  The assembly of a generator and absorber provides the compression operation.  Low-pressure vapor from the evaporator is absorbed by the liquid solution in the absorber.  A pump receives low-pressure liquid from the absorber, elevates the pressure of the liquid, and delivers the liquid to the generator.  In the generator, heat from a high temperature source (hot water or steam) drives off the vapor that has been absorbed by the solution.  The liquid solution returns to the absorber through a throttling valve whose purpose is to provide a pressure drop to maintain the pressure difference between the generator and absorber.  The heat supplied to the absorber can be waste heat from a diesel jacket, or the exhaust heat from diesel, gas, and steam turbines.  For more information on absorption chillers, see the Input/Output Reference Document (Object: Chiller:Absorption).

The part-load ratio of the absoprtion chiller’s evaporator is simply the actual cooling effect produced by the chiller divided by the maximum cooling effect available.

<div>$$PLR = {\raise0.7ex\hbox{${{{\dot Q}_{evap}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{evap}}} {{{\dot Q}_{evap,\,\,rated}}}}}\right.}\!\lower0.7ex\hbox{${{{\dot Q}_{evap,\,\,rated}}}$}}$$</div>

where

<span>\(PLR\)</span> = part-load ratio of chiller evaporator

<span>\({\dot Q_{evap}}\)</span> = chiller evaporator load [W]

<span>\({\dot Q_{evap,\,rated}}\)</span> = rated chiller evaporator capacity [W]

This absorption chiller model is based on a polynomial fit of absorber performance data.  The Generator Heat Input Part Load Ratio Curve is a quadratic equation that determines the ratio of the generator heat input to the *demand* on the chiller’s evaporator (Q<sub>evap</sub>).

<div>$$GeneratorHeatInputRatio = \frac{{C1}}{{PLR}} + C2 + C3\left( {PLR} \right)$$</div>

The Pump Electric Use Part Load Ratio Curve is a quadratic equation that determines the ratio of the actual absorber pumping power to the nominal pumping power.

<div>$$ElectricInputRatio = C1 + C2 * PLR + C3 * PL{R^2}$$</div>

Thus, the coefficient sets establish the ratio of heat power in-to-cooling effect produced as a function of part load ratio.  The ratio of heat-power-in to cooling-effect-produced is the inverse of the coefficient of performance.

If the operating part-load ratio is greater than the minimum part-load ratio, the chiller will run the entire time step and cycling will not occur (i.e. *CyclingFrac* = 1). If the operating part-load ratio is less than the minimum part-load ratio, the chiller will be on for a fraction of the time step equal to *CyclingFrac*. Steam (or hot water) and pump electrical energy use are also calculated using the chiller part-load cycling fraction.

<div>$$CyclingFrac = MIN\left( {1,\frac{{PLR}}{{PL{R_{min}}}}} \right)$$</div>

<div>$${\dot Q_{generator}} = GeneratorHeatInputRatio\left( {{{\dot Q}_{evap}}} \right)\left( {CyclingFrac} \right)$$</div>

<div>$${\dot Q_{pump}} = ElectricInputRatio\left( {{P_{pump}}} \right)\left( {CyclingFrac} \right)$$</div>

where

<span>\(CyclingFrac\)</span> = chiller part-load cycling fraction

<span>\(PL{R_{min}}\)</span> = chiller minimum part-load ratio

<span>\({\dot Q_{generator}}\)</span> = generator input power [W]

<span>\({\dot Q_{pump}}\)</span> = absorbtion chiller pumping power [W]

The evaporator water mass flow rate is calculated based on the Chiller Flow Mode as follows.

**Constant Flow Chillers:**

**<span>\({\dot m_{evap}} = {\dot m_{evap,max}}\)</span>**

**Variable Flow Chillers:**

<div>$$\Delta {T_{evap}} = {T_{evap,\,in}} - {T_{evap,\,SP}}$$</div>

<div>$${\dot m_{evap}} = {\raise0.7ex\hbox{${{{\dot Q}_{evap}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{evap}}} {{C_{p,\,evap}}\left( {\Delta {T_{evap}}} \right)}}}\right.}\!\lower0.7ex\hbox{${{C_{p,\,evap}}\left( {\Delta {T_{evap}}} \right)}$}}$$</div>

where

<span>\({\dot m_{evap}}\)</span>          = chiller evaporator water mass flow rate (kg/s)

<span>\({\dot m_{evap,\,max}}\)</span>     = chiller design evaporator water mass flow rate (kg/s)

<span>\(\Delta {T_{evap}}\)</span>        = chiller evaporator water temperature difference (ºC)

<span>\({T_{evap,\,in}}\)</span>        = chiller evaporator inlet water temperature (ºC)

<span>\({T_{evap,\,SP}}\)</span>       = chiller evaporator outlet water setpoint temperature (ºC)

<span>\({C_p}\)</span>              = specific heat of water entering evaporator (J/kg•ºC)

The evaporator outlet water temperature is then calculated based on the cooling effect produced and the evaporator entering water temperature.

<div>$${T_{evap,out}} = {T_{evap,in}} + {\raise0.7ex\hbox{${{{\dot Q}_{evap}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{evap}}} {{C_{p,\,evap}}\left( {{{\dot m}_{evap}}} \right)}}}\right.}\!\lower0.7ex\hbox{${{C_{p,\,evap}}\left( {{{\dot m}_{evap}}} \right)}$}}$$</div>

where

<span>\({T_{evap,out}}\)</span> = chiller evaporator outlet water temperature [ºC]

<span>\({T_{evap,in}}\)</span> = chiller evaporator inlet water temperature [ºC]

<span>\({C_{p,\,evap}}\)</span> = specific heat of chiller evaporator inlet water [J/kg/ºC]

<span>\({\dot m_{evap}}\)</span> = chiller evaporator water mass flow rate [kg/s]

The condenser heat transfer and condenser leaving water temperature are also calculated.

<div>$${\dot Q_{cond}} = {\dot Q_{evap}} + {\dot Q_{generator}} + {\dot Q_{pump}}$$</div>

<div>$${T_{cond,out}} = {T_{cond,in}} + {\raise0.7ex\hbox{${{{\dot Q}_{cond}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{cond}}} {{C_{p,\,cond}}\left( {{{\dot m}_{cond}}} \right)}}}\right.}\!\lower0.7ex\hbox{${{C_{p,\,cond}}\left( {{{\dot m}_{cond}}} \right)}$}}$$</div>

where

<span>\({\dot Q_{cond}}\)</span> = chiller condenser heat transfer rate [W]

<span>\({T_{cond,out}}\)</span> = chiller condenser outlet water temperature [ºC]

<span>\({T_{cond,in}}\)</span> = chiller condenser inlet water temperature [ºC]

<span>\({C_{p,\,cond}}\)</span> = specific heat of chiller condenser inlet water [J/kg/ºC]

<span>\({\dot m_{cond}}\)</span> = chiller condenser water mass flow rate [kg/s]

The absorption chiller can model the impact of steam or hot water entering the generator, although the connection of the steam (hot water) nodes to a plant is not actually required. The calculations specific to the generator depend on the type of fluid used and are described here in further detail.

#### Steam Loop Calculations

When a steam loop is used and the inlet and outlet node names are specified (i.e. the nodes are connected to a steam loop), the generator outlet node steam mass flow rate and temperature are calculated based on the generator input power, latent heat of steam, the specific heat of water, and the amount of subcooling in the steam generator. The model assumes dry saturated steam enters the absorption chiller’s generator and exits the generator as a subcooled liquid. The temperature leaving the generator is calculated based on the user entered amount of liquid subcooling in the generator. The effect of subcooling of the liquid (condensate) in the pipe returning to the boiler is not modeled.

<div>$${\dot m_{steam}}\,\,\,\, = \,\,\,\,\,\frac{{{{\dot Q}_{generator}}}}{{{h_{fg}} + {c_{p,\,water}} \times \Delta {T_{sc}}}}$$</div>

<div>$${T_{generator,out}} = {T_{generator,in}} - \Delta {T_{sc}}$$</div>

where

<span>\({\dot m_{steam}}\)</span> = chiller steam mass flow rate [kg/s]

<span>\({h_{fg}}\)</span> = latent heat of steam [J/kg]

<span>\({c_{p,\,water}}\)</span> = specific heat of saturated water in the generator [J/Kg ºK]

<span>\(\Delta {T_{sc}}\)</span> = amount of subcooling in steam generator [ºC]

<span>\({T_{generator,out}}\)</span> = generator steam outlet node temperature [ºC]

<span>\({T_{generator,in}}\)</span> = generator steam inlet node temperature [ºC]

#### Hot Water Loop Calculations

When a hot water loop is used and the inlet and outlet node names are specified (i.e. the nodes are connected to a hot water loop), the generator outlet node temperature is calculated based on the generator input power, mass flow rate of water, and the specific heat of water entering the hot water generator. The calculations are based on the Chiller Flow Mode as follows.

**Constant Flow Chillers:**

<div>$${\dot m_{generator}} = {\dot m_{generator,max}}$$</div>

**Variable Flow Chillers:**

<div>$${\dot m_{generator}} = {\raise0.7ex\hbox{${{{\dot Q}_{generator}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{generator}}} {{C_{p,\,water}}\left( {\Delta {T_{generator}}} \right)}}}\right.}\!\lower0.7ex\hbox{${{C_{p,\,water}}\left( {\Delta {T_{generator}}} \right)}$}}$$</div>

<div>$${T_{generator,out}} = {T_{generator,in}} - \frac{{{{\dot Q}_{generator}}}}{{{{\dot m}_{generator}}\left( {{C_{p\,,\,water}}} \right)}}$$</div>

where

<span>\({\dot m_{generator}}\)</span> = generator hot water mass flow rate [kg/s]

<span>\({\dot m_{generator,\,max}}\)</span>= generator design hot water mass flow rate (kg/s)

<span>\(\Delta {T_{generator}}\)</span>   = generator design hot water temperature difference (ºC)

### Indirect Absorption Chiller

The Chiller:Absorption:Indirect object is an enhanced version of the absorption chiller model found in the Building Loads and System Thermodynamics (BLAST) program. This enhanced model is nearly identical to the existing absorption chiller model (Ref. Chiller:Absorption) with the exceptions that: 1) the enhanced indirect absorption chiller model provides more flexible performance curves and 2) chiller performance now includes the impact of varying evaporator, condenser, and generator temperatures. Since these absorption chiller models are nearly identical (i.e., the performance curves of the enhanced model can be manipulated to produce similar results to the previous model), it is quite probable that the Chiller:Absorption model will be deprecated in a future release of EnergyPlus.

The indirect absorption chiller’s condenser and evaporator are similar to that of a standard chiller, which are both water-to-water heat exchangers. The assembly of a generator and absorber provides the compression operation. A schematic of a single-stage absorption chiller is shown in the figure below. Low-pressure vapor from the evaporator is absorbed by the liquid solution in the absorber. A pump receives low-pressure liquid from the absorber, elevates the pressure of the liquid, and delivers the liquid to the generator. In the generator, heat from a high temperature source (hot water or steam) drives off the vapor that has been absorbed by the solution. The liquid solution returns to the absorber through a throttling valve whose purpose is to provide a pressure drop to maintain the pressure difference between the generator and absorber. The heat supplied to the generator can be either hot water or steam, however, connection to an actual plant loop is not required. For more information on indirect absorption chillers, see the Input/Output Reference Document (Object: Chiller:Absorption:Indirect).

![Schematic\_AbsorptionChiller](media/image3060.png)

Figure 160. Schematic Diagram of a Single-Stage Absorption Chiller

The chiller cooling effect (capacity) will change with a change in condenser water temperature. Similarly, the chiller cooling effect will change as the temperature of the evaporator water changes. The chiller cooling effect will also change with a change in or generator inlet water temperature and only applies when Hot Water is used as the generator heat source. A quadratic or cubic equation is used to modify the rated chiller capacity as a function of both the condenser and generator inlet water temperatures and the evaporator outlet water temperature. If any or all of the capacity correction factor curves are not used, the correction factors are assumed to be 1.

<div>$$CAPF{T_{evaporator}} = a + b\left( {{T_{evaporator}}} \right) + c{\left( {{T_{evaporator}}} \right)^2} + d{\left( {{T_{evaporator}}} \right)^3}$$</div>

<div>$$CAPF{T_{condenser}} = e + f\left( {{T_{condenser}}} \right) + g{\left( {{T_{condenser}}} \right)^2} + h{\left( {{T_{condenser}}} \right)^3}$$</div>

<span>\(CAPF{T_{generator}} = i + j\left( {{T_{generator}}} \right) + k{\left( {{T_{generator}}} \right)^2} + l{\left( {{T_{generator}}} \right)^3}\)</span> (*Hot Water only*)

<div>$${\mathop Q\limits^\cdot_{evap,max}} = {\mathop Q\limits^\cdot_{evap,rated}}\left( {CAPF{T_{evaporator}}} \right)\left( {CAPF{T_{condenser}}} \right)\left( {CAPF{T_{generator}}} \right)$$</div>

where

<span>\(CAPF{T_{evaporator}}\)</span>= Capacity correction (function of evaporator temperature) factor

<span>\(CAPF{T_{condenser}} = \)</span>= Capacity correction (function of condenser temperature) factor

<span>\(CAPF{T_{generator}}\)</span>= Capacity correction (function of generator temperature) factor

<span>\({T_{evaporator}}\)</span> = evaporator outet water temperature [C]

<span>\({T_{condenser}}\)</span> = condenser inlet water temperature [C]

<span>\({T_{generator}}\)</span> = generator inlet water temperature [C]

<span>\({\dot Q_{evap,\,\,max}}\)</span> = maximum chiller capacity [W]

<span>\({\dot Q_{evap,\,\,rated}}\)</span> = rated chiller capacity [W]

The part-load ratio of the indirect absoprtion chiller’s evaporator is simply the actual cooling effect required (load) divided by the maximum cooling effect available.

<div>$$PLR = {\raise0.7ex\hbox{${{{\dot Q}_{evap}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{evap}}} {{{\dot Q}_{evap,\,\,max}}}}}\right.}\!\lower0.7ex\hbox{${{{\dot Q}_{evap,\,\,max}}}$}}$$</div>

where

<span>\(PLR\)</span> = part-load ratio of chiller evaporator

<span>\({\dot Q_{evap}}\)</span> = chiller evaporator operating capacity [W]

The generator’s heat input is also a function of several parameters. The primary input for determining the heat input requirements is the Generator Heat Input function of Part-Load Ratio Curve. The curve is a quadratic or cubic equation that determines the ratio of the generator heat input to the chiller’s maximum capacity (Q<sub>evap,\\ max</sub>) and is solely a function of part-load ratio. Typical generator heat input ratios at full load (i.e., PLR=1) are between 1 and 2. Two additional curves are available to modifiy the heat input requirement based on the generator inlet water temperature and the evaporator outlet water temperature.

<div>$$GeneratorHIR = a + b\left( {PLR} \right) + c{\left( {PLR} \right)^2} + d{\left( {PLR} \right)^3}$$</div>

<div>$$GenfCondT = e + f\left( {{T_{generator}}} \right) + g{\left( {{T_{generator}}} \right)^2} + h{\left( {{T_{generator}}} \right)^3}$$</div>

<div>$$GenfEvapT = i + j\left( {{T_{evaporator}}} \right) + k{\left( {{T_{evaporator}}} \right)^2} + l{\left( {{T_{evaporator}}} \right)^3}$$</div>

where

*GeneratorHIR* = ratio of generator heat input to chiller operating capacity

*GenfCondT* = heat input modifier based on generator inlet water temperature

*GenfEvapT* = heat input modifier based on evaporator outlet water temperature

The Pump Electric Use function of Part-Load Ratio Curve is a quadratic or cubic equation that determines the ratio of the actual absorber pumping power to the nominal pumping power.

<div>$$ElectricInputRatio = a + b\left( {PLR} \right) + c{\left( {PLR} \right)^2} + d{\left( {PLR} \right)^3}$$</div>

If the chiller operating part-load ratio is greater than the minimum part-load ratio, the chiller will run the entire time step and cycling will not occur (i.e. *CyclingFrac* = 1). If the operating part-load ratio is less than the minimum part-load ratio, the chiller will be on for a fraction of the time step equal to *CyclingFrac*. Generator heat input and pump electrical energy use are also calculated using the chiller part-load cycling fraction.

<div>$$CyclingFrac = MIN\left( {1.\frac{{PLR}}{{PL{R_{min}}}}} \right)$$</div>

<div>$${\dot Q_{generator}} = GeneratorHIR\left( {{{\dot Q}_{evap,\,max}}} \right)\left( {GenfCondT} \right)\left( {GenfEvapT} \right)\left( {CyclingFrac} \right)$$</div>

<div>$${\dot Q_{generator}} = ElectricInputRatio({P_{pump}})(CyclingFrac)$$</div>

where

<span>\(CyclingFrac\)</span> = chiller part-load cycling fraction

<span>\(PL{R_{min}}\)</span> = chiller minimum part-load ratio

<span>\({\dot Q_{generator}}\)</span> = generator heat input [W]

<span>\({\dot Q_{pump}}\)</span> = chiller pumping power [W]

The evaporator water mass flow rate is calculated based on the Chiller Flow Mode as follows.

**Constant Flow Chillers:**

<div>$${\dot m_{evap}} = {\dot m_{evap,max}}$$</div>

**Variable Flow Chillers:**

<div>$$\Delta {T_{evap}} = {T_{evap,\,in}} - {T_{evap,\,SP}}$$</div>

<div>$${\dot m_{evap}} = {\raise0.7ex\hbox{${{{\dot Q}_{evap}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{evap}}} {{C_{p,\,evap}}\left( {\Delta {T_{evap}}} \right)}}}\right.}\!\lower0.7ex\hbox{${{C_{p,\,evap}}\left( {\Delta {T_{evap}}} \right)}$}}$$</div>

where

<span>\({\dot m_{evap}}\)</span>          = chiller evaporator water mass flow rate (kg/s)

<span>\({\dot m_{evap,\,max}}\)</span>     = chiller design evaporator water mass flow rate (kg/s)

<span>\(\Delta {T_{evap}}\)</span>        = chiller evaporator water temperature difference (ºC)

<span>\({T_{evap,\,in}}\)</span>        = chiller evaporator inlet water temperature (ºC)

<span>\({T_{evap,\,SP}}\)</span>       = chiller evaporator outlet water setpoint temperature (ºC)

<span>\({C_{p,\,evap}}\)</span>         = specific heat of water entering evaporator (J/kg ºC)

The evaporator outlet water temperature is then calculated based on the cooling effect produced and the evaporator entering water temperature.

<div>$${T_{evap,out}} = {T_{evap,in}} + {\raise0.7ex\hbox{${{{\dot Q}_{evap}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{evap}}} {{C_{p,\,evap}}\left( {{{\dot m}_{evap}}} \right)}}}\right.}\!\lower0.7ex\hbox{${{C_{p,\,evap}}\left( {{{\dot m}_{evap}}} \right)}$}}$$</div>

where

<span>\({T_{evap,out}}\)</span> = chiller evaporator outlet water temperature [ºC]

<span>\({T_{evap,in}}\)</span> = chiller evaporator inlet water temperature [ºC]

<span>\({C_{p,\,evap}}\)</span> = specific heat of chiller evaporator inlet water [J/kg/ºC]

<span>\({\dot m_{evap}}\)</span> = chiller evaporator water mass flow rate [kg/s]

The condenser heat transfer and condenser leaving water temperature are also calculated.

<div>$${\dot Q_{cond}} = {\dot Q_{evap}} + {\dot Q_{generator}} + {\dot Q_{pump}}$$</div>

<div>$${T_{cond,out}} = {T_{cond,in}} + {\raise0.7ex\hbox{${{{\dot Q}_{cond}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{cond}}} {{C_{p,\,cond}}\left( {{{\dot m}_{cond}}} \right)}}}\right.}\!\lower0.7ex\hbox{${{C_{p,\,cond}}\left( {{{\dot m}_{cond}}} \right)}$}}$$</div>

where

<span>\({\dot Q_{cond}}\)</span> = chiller condenser heat transfer rate [W]

<span>\({T_{cond,out}}\)</span> = chiller condenser outlet water temperature [ºC]

<span>\({T_{cond,in}}\)</span> = chiller condenser inlet water temperature [ºC]

<span>\({C_{p,\,cond}}\)</span> = specific heat of chiller condenser inlet water [J/kg/ºC]

<span>\({\dot m_{cond}}\)</span> = chiller condenser water mass flow rate [kg/s]

The absorption chiller can model the impact of steam or hot water entering the generator, although the connection of the steam (hot water) nodes to a plant is not actually required. The calculations specific to the generator depend on the type of fluid used and are described here in further detail.

#### Steam Loop Calculations

When a steam loop is used and the inlet and outlet node names are specified (i.e. the nodes are connected to a steam loop), the generator outlet node steam mass flow rate and temperature are calculated based on the generator heat input, latent heat of steam, the specific heat of water, and the amount of subcooling in the steam generator. The model assumes dry saturated steam enters the generator and exits the generator as a subcooled liquid. The temperature leaving the generator is calculated based on the user entered amount of liquid subcooling in the generator. The effect of subcooling of the liquid (condensate) in the pipe returning to the boiler is also modeled using the user entered abount of steam condensate loop subcooling.

<div>$${\dot m_{steam}}\,\,\,\, = \,\,\,\,\,\frac{{{{\dot Q}_{generator}}}}{{{h_{fg}} + {c_{p,water}} \times \Delta {T_{sc}}}}$$</div>

<div>$${T_{generator,out}} = {T_{generator,in}} - \Delta {T_{sc}}$$</div>

<div>$${T_{loop,out}} = {T_{generator,out}} - \Delta {T_{sc,\,\,loop}}$$</div>

where

<span>\({\dot m_{steam}}\)</span> = chiller steam mass flow rate [kg/s]

<span>\({h_{fg}}\)</span> = latent heat of steam [J/kg]

<span>\({c_{p,water}}\)</span> = specific heat of water [J/Kg ºC]

<span>\(\Delta {T_{sc}}\)</span> = amount of subcooling in steam generator [ºC]

<span>\(\Delta {T_{sc,\,loop}}\)</span> = amount of condensate subcooling in steam loop [ºC]

<span>\({T_{generator,out}}\)</span> = generator steam outlet node temperature [ºC]

<span>\({T_{generator,in}}\)</span> = generator steam inlet node temperature [ºC]

#### Hot Water Loop Calculations

When a hot water loop is used and the inlet and outlet node names are specified (i.e. the nodes are connected to a hot water loop), the generator outlet node temperature is calculated based on the generator heat input, mass flow rate of water, and the specific heat of water entering the hot water generator. The calculations are based on the Chiller Flow Mode as follows.

**Constant Flow Chillers:**

<div>$${\dot m_{generator}} = {\dot m_{generator,\,max}}$$</div>

**Variable Flow Chillers:**

<div>$${\dot m_{generator}} = {\raise0.7ex\hbox{${{{\dot Q}_{generator}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{generator}}} {{C_{p,\,water}}\left( {\Delta {T_{generator}}} \right)}}}\right.}\!\lower0.7ex\hbox{${{C_{p,\,water}}\left( {\Delta {T_{generator}}} \right)}$}}$$</div>

<div>$${T_{generator,out}} = {T_{generator,in}} - \frac{{{{\dot Q}_{generator}}}}{{{{\dot m}_{generator}}\left( {{C_{p\,,\,water}}} \right)}}$$</div>

where

<span>\({\dot m_{generator}}\)</span> = generator hot water mass flow rate [kg/s]

<span>\({\dot m_{generator,\,max}}\)</span>= generator design hot water mass flow rate (kg/s)

<span>\(\Delta {T_{generator}}\)</span>   = generator design hot water temperature difference (ºC)

### Combustion Turbine Chiller

The input object Chiller:CombustionTurbine provides a chiller model that is the empirical model from the Building Loads and System Thermodynamics (BLAST) program. Fitting catalog data to a third order polynomial equations generates the chiller performance curves.  Three sets of coefficients are required to model the open centrifugal chiller as discussed in the section, titled, ‘Electric Chiller Based on BLAST Centrifugal Chiller Model’.

The gas turbine-driven chiller is an open centrifugal chiller driven directly by a gas turbine.  The BLAST model of an open centrifugal chiller is modeled as standard vapor compression refrigeration cycle with a centrifugal compressor driven by a shaft power from an engine.  The centrifugal compressor has the incoming fluid entering at the eye of a spinning impeller that throws the fluid by centrifugal force to the periphery of the impeller.  After leaving the compressor, the refrigerant is condensed to liquid in a refrigerant to water condenser.  The heat from the condenser is rejected to a cooling tower, evaporative condenser, or well water condenser depending on which one is selected by the user based on the physical parameters of the plant.  The refrigerant pressure is then dropped through a throttling valve so that fluid can evaporate at a low pressure that provides cooling to the evaporator.  The evaporator can chill water that is pumped to chilled water coils in the building.  For more information, see the Input/Output Reference Document.

This chiller is modeled like the electric chiller with the same numerical curve fits and then some additional curve fits to model the turbine drive.  Shown below are the definitions of the curves that describe this model.

The chiller’s temperature rise coefficient which is defined as the ratio of the required change in condenser water temperature to a given change in chilled water temperature, which maintains the capacity at the nominal value.  This is calculated as the following ratio:

<div>$$\frac{{TCEn{t_{required}} - TCEn{t_{rated}}}}{{TEL{v_{required}} - TEL{v_{rated}}}}$$</div>

Where:

TCEnt<sub>required</sub> = Required entering condenser air or water temperature to maintain rated capacity.

TCEnt<sub>rated</sub> = Rated entering condenser air or water temperature at rated capacity.

TELv<sub>required</sub> = Required leaving evaporator water outlet temperature to maintain rated capacity.

TELv<sub>rated</sub> = Rated leaving evaporator water outlet temperature at rated capacity.

The Capacity Ratio Curve is a quadratic equation that determines the Ratio of Available Capacity to Nominal Capacity.  The defining equation is:

<div>$$AvailToNominalCapacityRatio = {C_1} + {C_2}{\Delta_{temp}} + {C_3}\Delta_{temp}^2$$</div>

Where the Delta Temperature is defined as:

<div>$${\Delta_{{\rm{Temp}}}} = \frac{{{\rm{TempCondIn  -  TempCondInDesign }}}}{{{\rm{TempRiseCoefficient}}}} - ({\rm{TempEvapOut  -  TempEvapOutDesign)}}$$</div>

TempCondIn = Temperature entering the condenser (water or air temperature depending on condenser type).

TempCondInDesign = Temp Design Condenser Inlet from User input above.

TempEvapOut = Temperature leaving the evaporator.

TempEvapOutDesign = Temp Design Evaporator Outlet from User input above.

TempRiseCoefficient = User Input from above.

The following three fields contain the coefficients for the quadratic equation.

The Power Ratio Curve is a quadratic equation that determines the Ratio of Full Load to Power.  The defining equation is:

<div>$$FullLoadtoPowerRatio = {C_1} + {C_2}AvailToNominalCapRatio + {C_3}AvailToNominalCapRati{o^2}$$</div>

The Full Load Ratio Curve is a quadratic equation that determines the fraction of full load power.  The defining equation is:

<div>$$FracFullLoadPower = {C_1} + {C_2}PartLoadRatio + {C_3}PartLoadRati{o^2}$$</div>

The Fuel Input Curve is a polynomial equation that determines the Ratio of Fuel Input to Energy Output.  The equation combines both the Fuel Input Curve Coefficients and the Temperature Based Fuel Input Curve Coefficients.  The defining equation is:

<div>$$FuelEnergyInput = PLoad * (FI{C_1} + FI{C_2}RLoad + FI{C_3}RLoa{d^2}) * (TBFI{C_1} + TBFI{C_2}A{T_{air}} + TBFI{C_3}AT_{air}^2)$$</div>

Where FIC represents the Fuel Input Curve Coefficients, TBFIC represents the Temperature Based Fuel Input Curve Coefficients, Rload is the Ratio of Load to Combustion Turbine Engine Capacity, and AT<sub>air</sub> is the difference between the current ambient and design ambient temperatures.

The Exhaust Flow Curve is a quadratic equation that determines the Ratio of Exhaust Gas Flow Rate to Engine Capacity.  The defining equation is:

*<span>\(ExhaustFlowRate = GTCapacity * ({C_1} + {C_2}A{T_{air}} + {C_3}AT_{air}^2)\)</span> *

Where GTCapacity is the Combustion Turbine Engine Capacity, and AT<sub>air</sub> is the difference between the current ambient and design ambient temperatures.

The Exhaust Gas Temperature Curve is a polynomial equation that determines the Exhaust Gas Temperature.  The equation combines both the Exhaust Gas Temperature Curve Coefficients (Based on the Part Load Ratio) and the (Ambient) Temperature Based Exhaust Gas Temperature Curve Coefficients.  The defining equation is:

 <span>\(ExhaustTemperature = ({C_1} + {C_2}RLoad + {C_3}RLoa{d^2}) * (TB{C_1} + TB{C_2}A{T_{air}} + TB{C_3}AT_{air}^2) - 273.15\)</span>

Where C represents the Exhaust Gas Temperature Curve Coefficients, TBC are the Temperature Based Exhaust Gas Temperature Curve Coefficients, RLoad is the Ratio of Load to Combustion Turbine Engine Capacity, and AT<sub>air</sub> is the difference between the actual ambient and design ambient temperatures.

The Recovery Lubricant Heat Curve is a quadratic equation that determines the recovery lube energy.  The defining equation is:

<div>$$RecoveryLubeEnergy = PLoad * ({C_1} + {C_2}RL + {C_3}R{L^2})$$</div>

Where Pload is the engine load and RL is the Ratio of Load to Combustion Turbine Engine Capacity

The UA is an equation that determines the overall heat transfer coefficient for the exhaust gasses with the stack.  The heat transfer coefficient ultimately helps determine the exhaust stack temperature.  The defining equation is:

<div>$$UAToCapacityRatio = {C_1}GasTurbineEngineCapacit{y^{{C_2}}}$$</div>

#### Chiller Basin Heater

This chiller’s basin heater (for evaporatively-cooled condenser type) operates in the same manner as the Engine driven chiller’s basin heater. The calculations for the chiller basin heater are described in detail at the end of the engine driven chiller description (Ref. Engine Driven Chiller).

### ChillerHeater:Absorption:DirectFired

#### Overview

This model (object name ChillerHeater:Absorption:DirectFired) simulates the performance of a direct fired two-stage absorption chiller with optional heating capability. The model is based on the direct fired absorption chiller model (ABSORG-CHLR) in the DOE-2.1 building energy simulation program. The EnergyPlus model contains all of the features of the DOE-2.1 chiller model, plus some additional capabilities.

This model simulates the thermal performance of the chiller and the fuel consumption of the burner(s). This model does not simulate the thermal performance or the power consumption of associated pumps or cooling towers. This auxiliary equipment must be modeled using other EnergyPlus models (e.g. Cooling Tower:Single Speed).

#### Model Description

The chiller model uses user-supplied performance information at design conditions along with five performance curves (curve objects) for cooling capacity and efficiency to determine chiller operation at off-design conditions. Two additional performance curves for heating capacity and efficiency are used when the chiller is operating in a heating only mode or simultaneous cooling and heating mode.

#### Cooling

The following nomenclature is used in the cooling equations:

*AvailCoolCap*         =   available full-load cooling capacity at current conditions [W]

*CEIR*                       =   user input “Electric Input to Cooling Output Ratio”

*CEIRfPLR*               =   electric input to cooling output factor, equal to 1 at full load, user input “Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name”

*CEIRfT*                    =   electric input to cooling output factor, equal to 1 at design conditions, user input “Electric Input to Cooling Output Ratio Function of Temperature Curve Name”

*CFIR*                       =   user input “Fuel Input to Cooling Output Ratio”

*CFIRfPLR*               =   fuel input to cooling output factor, equal to 1 at full load, user input “Fuel Input to Cooling Output Ratio Function of Part Load Ratio Curve Name”

*CFIRfT*                    =   fuel input to cooling output factor, equal to 1 at design conditions, user input “Fuel Input to Cooling Output Ratio Function of Temperature Curve Name”

*CondenserLoad       *=   condenser heat rejection load [W]

*CoolCapfT*              =   cooling capacity factor, equal to 1 at design conditions, user input “Cooling Capacity Function of Temperature Curve Name”

*CoolElectricPower*  =   cooling electricity input [W]

*CoolFuelInput*         =   cooling fuel input [W]

*CoolingLoad*           =   current cooling load on the chiller [W]

*CPLR*                      =   cooling part-load ratio = *CoolingLoad* / *AvailCoolCap*

*HeatingLoad*           =   current heating load on the chiller heater [W]

*HFIR*                       =   user input “Fuel Input to Heating Output Ratio”

*HPLR*                      =   heating part-load ratio = *HeatingLoad* / *AvailHeatCap*

*MinPLR                   *=   user input “Minimum Part Load Ratio”

*NomCoolCap*          =   user input “Nominal Cooling Capacity” [W]

*RunFrac*                  =   fraction of time step which the chiller is running

*T<sub>cond</sub>*                         =   entering or leaving condenser fluid temperature [C]. For a water-cooled condenser this will be the water temperature returning from the condenser loop (e.g., leaving the cooling tower) if the entering condenser fluid temperature option is used. For air- or evap-cooled condensers this will be the entering outdoor air dry-bulb or wet-bulb temperature, respectively, if the entering condenser fluid temperature option is used.

*T<sub>cw,l</sub>*                          =   leaving chilled water temperature [C]

Five performance curves are used in the calculation of cooling capacity and efficiency:

1)    Cooling Capacity Function of Temperature Curve

2)    Fuel Input to Cooling Output Ratio Function of Temperature Curve

3)    Fuel Input to Cooling Output Ratio Function of Part Load Ratio Curve

4)    Electric Input to Cooling Output Ratio Function of Temperature Curve

5)    Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve

The cooling capacity function of temperature (*CoolCapfT*) curve represents the fraction of the cooling capacity of the chiller as it varies by temperature. This a biquadratic curve with the input variables being the leaving chilled water temperature and either the entering or leaving condenser fluid temperature. The output of this curve is multiplied by the nominal cooling capacity to give the full-load cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the design temperatures). The curve should have a value of 1.0 at the design temperatures and flow rates specified in the input data file by the user. The biquadratic curve should be valid for the range of water temperatures anticipated for the simulation.

<div>$$CoolCapfT = a + b{T_{cw,l}} + cT_{cw,l}^2 + d{T_{cond}} + eT_{cond}^2 + f{T_{cw,l}}{T_{cond}}$$</div>

The available cooling capacity of the chiller is then computed as follows:

<div>$$AvailCoolCap = NomCoolCap \cdot CoolCapfT({T_{cw,l}},{T_{cond}})$$</div>

The fuel input to cooling output ratio function of temperature (*CFIRfT*) curve represents the fraction of the fuel input to the chiller at full load as it varies by temperature. This a biquadratic curve with the input variables being the leaving chilled water temperature and either the entering or leaving condenser fluid temperature. The output of this curve is multiplied by the nominal fuel input to cooling output ratio (*CFIR*) to give the full-load fuel input to cooling capacity ratio at specific temperature operating conditions (i.e., at temperatures different from the design temperatures). The curve should have a value of 1.0 at the design temperatures and flow rates specified in the input data file by the user. The biquadratic curve should be valid for the range of water temperatures anticipated for the simulation.

<div>$$CFIRfT = a + b{T_{cw,l}} + cT_{cw,l}^2 + d{T_{cond}} + eT_{cond}^2 + f{T_{cw,l}}{T_{cond}}$$</div>

The fuel input to cooling output ratio function of part load ratio (*CFIRfPLR*) curve represents the fraction of the fuel input to the chiller as the load on the chiller varies at a given set of  operating temperatures. The curve is normalized so that at full load the value of the curve should be 1.0. The curve is usually linear or quadratic.

<div>$$CFIRfPLR = a + b \cdot CPLR + c \cdot CPL{R^2}$$</div>

The fraction of the time step during which the chiller heater is operating is computed as a function of the cooling and heating part-load ratios and the user-input minimum part-load ratio:

<div>$$RunFrac = {\mathop{\rm MIN}\nolimits} \left( {1.0,{\mathop{\rm MAX}\nolimits} \left( {HPLR,CPLR} \right)/MinPLR} \right)$$</div>

The cooling fuel input to the chiller is then computed as follows:

<div>$$\begin{array}{l}CoolFuelInput = \\AvailCoolCap \cdot RunFrac \cdot CFIR \cdot CFIRfT({T_{cw,l}},{T_{cond}}) \cdot CFIRfPLR(CPLR)\end{array}$$</div>

The electric input to cooling output ratio as function of temperature (*CEIRfT*) curve represents the fraction of electricity to the chiller at full load as it varies by temperature. This a biquadratic curve with the input variables being the leaving chilled water temperature and either the entering or leaving condenser fluid temperature.

<div>$$CEIRfT = a + b{T_{cw,l}} + cT_{cw,l}^2 + d{T_{cond}} + eT_{cond}^2 + f{T_{cw,l}}{T_{cond}}$$</div>

The electric input to cooling output ratio function of part load ratio (*CEIRfPLR*) curve represents the fraction of electricity to the chiller as the load on the chiller varies at a given set of operating temperatures. The curve is normalized so that at full load the value of the curve should be 1.0. The curve is usually linear or quadratic.

<div>$$CEIRfPLR = a + b \cdot CPLR + c \cdot CPL{R^2}$$</div>

The cooling electric input to the chiller is computed as follows:

<div>$$CoolElectricPower = NomCoolCap \cdot RunFrac \cdot CEIR \cdot CEIRfT({T_{cw,l}},{T_{cond}}) \cdot CEIRfPLR(CPLR)$$</div>

All five of these cooling performance curves are accessed through EnergyPlus’ built-in performance curve equation manager (objects Curve:Linear, Curve:Quadratic and Curve:Biquadratic). It is not imperative that the user utilize all coefficients in the performance curve equations if their performance equation has fewer terms (e.g., if the user’s *CFIRfPLR* performance curve is linear instead of quadratic, simply enter the values for a and b, and set coefficient c equal to zero).

The condenser load is computed as follows:

<div>$$CondenserLoad = CoolingLoad + {\raise0.7ex\hbox{${CoolFuelInput}$} \!\mathord{\left/ {\vphantom {{CoolFuelInput} {HFIR}}}\right.}\!\lower0.7ex\hbox{${HFIR}$}} + CoolElectricPower$$</div>

#### Heating

The following nomenclature is used in the heating equations:

*AvailHeatCap*         =   available full-load heating capacity at current conditions [W]

*CPLRh*                    =   cooling part-load ratio for heating curve =
 *CoolingLoad* / *NomCoolCap*

*HeatCapfCPLR*       =   heating capacity factor as a function of cooling part load ratio, equal to 1 at zero cooling load, user input “Heating Capacity Function of Cooling Capacity Curve Name”

*HeatCoolCapRatio *=   user input “Heating to Cooling Capacity Ratio”

*HeatElectricPower*  =   heating electricity input [W]

*HeatFuelInput*         =   heating fuel input [W]

*HeatingLoad*           =   current heating load on the chiller [W]

*HEIR*                       =   user input “Electric Input to Heating Output Ratio”

*HFIR*                       =   user input “Fuel Input to Heating Output Ratio”

*HFIRfHPLR*            =   fuel input to heating output factor, equal to 1 at full load, user input “Fuel Input to Heat Output Ratio During Heating Only Operation Curve Name”

*HPLR*                      =   heating part-load ratio = *HeatingLoad* / *AvailHeatCap*

*MinPLR                   *=   user input “Minimum Part Load Ratio”

*NomCoolCap*          =   user input “Nominal Cooling Capacity” [W]

*RunFrac*                  =   fraction of time step which the chiller is running

*TotalElectricPower *=   total electricity input [W]

*TotalFuelInput        *=   total fuel input [W]

Cooling is the primary purpose of the Direct Fired Absorption Chiller so that function is satisfied first and if energy is available for providing heating that is provided next.

The two performance curves for heating capacity and efficiency are:

1)    Heating Capacity Function of Cooling Capacity Curve

2)    Fuel-Input-to Heat Output Ratio Function

The heating capacity function of cooling capacity curve (*HeatCapfCool*) determines how the heating capacity of the chiller varies with cooling capacity when the chiller is simultaneously heating and cooling. The curve is normalized so an input of 1.0 represents the nominal cooling capacity and an output of 1.0 represents the full heating capacity.  An output of 1.0 should occur when the input is 0.0.

<div>$$HeatCapfCPLR = a + b \cdot CPLRh + c \cdot CPLR{h^2}$$</div>

The available heating capacity is then computed as follows:

<div>$$AvailHeatCap = NomCoolCap \cdot HeatCoolCapRatio \cdot HeatCapfCPLR(CPLRh)$$</div>

The fuel input to heat output ratio curve (*HFIRfHPLR*) function is used to represent the fraction of fuel used as the heating load varies as a function of heating part load ratio. It is normalized so that a value of 1.0 is the full available heating capacity. The curve is usually linear or quadratic and will probably be similar to a boiler curve for most chillers.

<div>$$HFIRfHPLR = a + b \cdot HPLR + c \cdot HPL{R^2}$$</div>

The fuel use rate when heating is computed as follows:

<div>$$HeatFuelInput = AvailHeatCap \cdot HFIR \cdot HFIRfHPLR(HPLR)$$</div>

The fraction of the time step during which the chiller is operating is computed as a function of the cooling and heating part-load ratios and the user-input minimum part-load ratio:

<div>$$RunFrac = {\mathop{\rm MIN}\nolimits} \left( {1.0,{\mathop{\rm MAX}\nolimits} \left( {HPLR,CPLRh} \right)/MinPLR} \right)$$</div>

The heating electric input to the chiller is computed as follows:

<div>$$HeatElectricPower = NomCoolCap \cdot HeatCoolCapRatio \cdot HEIR \cdot RunFrac$$</div>

If the chiller is delivering heating and cooling simultaneously, the parasitic electric load will be double-counted, so the following logic is applied:

```
IF ( HeatElectricPower <= CoolElectricPower ) THEN
  HeatElectricPower = 0.0
ELSE
  HeatElectricPower = HeatElectricPower - CoolElectricPower
ENDIF
```

The total fuel and electric power input to the chiller is computed as shown below:

<div>$$\begin{array}{l}TotalElectricPower = HeatElectricPower + CoolElectricPower\\TotalFuelInput = HeatFuelInput + CoolFuelInput\end{array}$$</div>

### ChillerHeater:Absorption:DoubleEffect

#### Overview

This model (object name ChillerHeater:Absorption:DoubleEffect) simulates the performance of an exhaust  fired two-stage (double effect) absorption chiller with optional heating capability. The model is based on the direct fired absorption chiller model (ABSORG-CHLR) in the DOE-2.1 building energy simulation program. The EnergyPlus model contains all of the features of the DOE-2.1 chiller model, plus some additional capabilities. The model uses the exhaust gas output from Microturbine.

This model simulates the thermal performance of the chiller and the thermal energy input to the chiller. This model does not simulate the thermal performance or the power consumption of associated pumps or cooling towers. This auxiliary equipment must be modeled using other EnergyPlus models (e.g. Cooling Tower:Single Speed).

#### Model Description

The chiller model uses user-supplied performance information at design conditions along with five performance curves (curve objects) for cooling capacity and efficiency to determine chiller operation at off-design conditions. Two additional performance curves for heating capacity and efficiency are used when the chiller is operating in a heating only mode or simultaneous cooling and heating mode.

#### Cooling

The following nomenclature is used in the cooling equations:

*AvailCoolCap*         =   available full-load cooling capacity at current conditions [W]

*CEIR*                       =   user input “Electric Input to Cooling Output Ratio”

*CEIRfPLR*               =   electric input to cooling output factor, equal to 1 at full load, user input “Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name”

*CEIRfT*                    =   electric input to cooling output factor, equal to 1 at design conditions, user input “Electric Input to Cooling Output Ratio Function of Temperature Curve Name”

*TeFIR*                      =   user input “Thermal Energy Input to Cooling Output Ratio”

*TeFIRfPLR*              =   thermal energy input to cooling output factor, equal to 1 at full load, user input “Thermal Energy Input to Cooling Output Ratio Function of Part Load Ratio Curve Name”

*TeFIRfT*                   =   thermal energy input to cooling output factor, equal to 1 at design conditions, user input “Thermal Energy Input to Cooling Output Ratio Function of Temperature Curve Name”

*CondenserLoad       *=   condenser heat rejection load [W]

*CoolCapfT*              =   cooling capacity factor, equal to 1 at design conditions, user input “Cooling Capacity Function of Temperature Curve Name”

*CoolElectricPower*  =   cooling electricity input [W]

*CoolThermalEnergyInput*=          cooling thermal energy input [W]

*CoolingLoad*           =   current cooling load on the chiller [W]

*CPLR*                      =   cooling part-load ratio = *CoolingLoad* / *AvailCoolCap*

*HeatingLoad*           =   current heating load on the chiller heater [W]

*HFIR*                       =   user input “Thermal Energy Input to Heating Output Ratio”

*HPLR*                      =   heating part-load ratio = *HeatingLoad* / *AvailHeatCap*

<span>\({\dot m_{ExhAir}}\)</span>                     =  exhaust air mass flow rate from microturbine (kg/s)

*MinPLR                   *=   user input “Minimum Part Load Ratio”

*NomCoolCap*          =   user input “Nominal Cooling Capacity” [W]

*RunFrac*                  =   fraction of time step which the chiller is running

<span>\({T_{a,o}}\)</span>                            =  exhaust air outlet temperature from microturbine entering the chiller

(<sup>o</sup>C)

<span>\({T_{abs,gen,o}}\)</span>                 =  Temperature of exhaust leaving the chiller (the generator                    component  of the absorption chiller)

*T<sub>cond</sub>*                         =   entering condenser fluid temperature [°C]. For a water-cooled condenser this will be the water temperature returning from the condenser loop (e.g., leaving the cooling tower). For air- or evap-cooled condensers this will be the entering outdoor air dry-bulb or wet-bulb temperature, respectively.

*T<sub>cw,l</sub>*                          =   leaving chilled water temperature [°C]

The selection of entering or leaving condense fluid temperature can be made through the optional field-Temperature Curve Input Variable.

Five performance curves are used in the calculation of cooling capacity and efficiency:

6)    Cooling Capacity Function of Temperature Curve

7)    Thermal Energy Input to Cooling Output Ratio Function of Temperature Curve

8)    Thermal Energy Input to Cooling Output Ratio Function of Part Load Ratio Curve

9)    Electric Input to Cooling Output Ratio Function of Temperature Curve

10) Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve

The cooling capacity function of temperature (*CoolCapfT*) curve represents the fraction of the cooling capacity of the chiller as it varies with temperature. This a biquadratic curve with the input variables being the leaving chilled water temperature and the entering condenser fluid temperature. The output of this curve is multiplied by the nominal cooling capacity to give the full-load cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the design temperatures). The curve should have a value of 1.0 at the design temperatures and flow rates specified in the input data file by the user. The biquadratic curve should be valid for the range of water temperatures anticipated for the simulation.

<div>$$CoolCapfT = a + b{T_{cw,l}} + cT_{cw,l}^2 + d{T_{cond}} + eT_{cond}^2 + f{T_{cw,l}}{T_{cond}}$$</div>

The available cooling capacity of the chiller is then computed as follows:

<div>$$AvailCoolCap = NomCoolCap \cdot CoolCapfT({T_{cw,l}} - {T_{cond}})$$</div>

The thermal energy input to cooling output ratio function of temperature (*TeFIRfT*) curve represents the fraction of the thermal energy input to the chiller at full load as it varies with temperature. This a biquadratic curve with the input variables being the leaving chilled water temperature and the entering condenser fluid temperature. The output of this curve is multiplied by the nominal thermal energy input to cooling output ratio (*TeFIR*) to give the full-load thermal energy input to cooling capacity ratio at specific temperature operating conditions (i.e., at temperatures different from the design temperatures). The curve should have a value of 1.0 at the design temperatures and flow rates specified in the input data file by the user. The biquadratic curve should be valid for the range of water temperatures anticipated for the simulation.

<div>$$TeFIRfT = a + b{T_{cw,l}} + cT_{cw,l}^2 + d{T_{cond}} + eT_{cond}^2 + f{T_{cw,l}}{T_{cond}}$$</div>

The thermal energy input to cooling output ratio function of part load ratio (*TeFIRfPLR*) curve represents the fraction of the thermal energy input to the chiller as the load on the chiller varies at a given set of  operating temperatures. The curve is normalized so that at full load the value of the curve should be 1.0. The curve is usually linear or quadratic.

<div>$$TeFIRfPLR = a + b \cdot CPLR + c \cdot CPL{R^2}$$</div>

The fraction of the time step during which the chiller heater is operating is computed as a function of the cooling and heating part-load ratios and the user-input minimum part-load ratio:

<div>$$RunFrac = MIN(1.0,MAX\left( {HPLR,CPLR} \right)/MinPLR)$$</div>

The cooling thermal energy input to the chiller is then computed as follows:

<div>$$CoolThermalEnergyInput = AvailCoolCap \cdot RunFrac \cdot TeFIR \cdot TeFIRfT\left( {{T_{cw,l}},{T_{cond}}} \right) \cdot TeFIRfPLR(CPLR)$$</div>

To make sure that the exhaust mass flow rate and temperature from microturbine are sufficient to drive the chiller, the heat recovery potential is compared with the cooling thermal energy input to the chiller (CoolThermalEergyInput). The heat recovery potential should be greater than the CoolThermalEnergyInput. Heat recovery potential is calculated as:

<div>$${Q_{Recovery}} = {\dot m_{ExhAir}} \cdot {\rm{}}C{p_{Air}} \cdot {\rm{}}({T_{a,o}} - {T_{Abs,gen,o}})$$</div>

T<sub>abs,gen,o </sub> is the minimum temperature required for the proper operation of the double-effect chiller. It will be defaulted to 176°C.

The electric input to cooling output ratio as function of temperature (*CEIRfT*) curve represents the fraction of electricity to the chiller at full load as it varies with temperature. This a biquadratic curve with the input variables being the leaving chilled water temperature and either the entering or leaving condenser fluid temperature.

<div>$$CEIRfT = a + b{T_{cw,l}} + cT_{cw,l}^2 + d{T_{cond}} + eT_{cond}^2 + f{T_{cw,l}}{T_{cond}}$$</div>

The electric input to cooling output ratio function of part load ratio (*CEIRfPLR*) curve represents the fraction of electricity to the chiller as the load on the chiller varies at a given set of operating temperatures. The curve is normalized so that at full load the value of the curve should be 1.0. The curve is usually linear or quadratic.

<div>$$CEIRfPLR = a + b \cdot CPLR + c \cdot CPL{R^2}$$</div>

The cooling electric input to the chiller is computed as follows:

<div>$$CoolElectricPower = NomCoolCap \cdot RunFrac \cdot CEIR \cdot CEIRfT\left( {{T_{cw,l}},{T_{cond}}} \right) \cdot CEIRfPLR(CPLR)$$</div>

All five of these cooling performance curves are accessed through EnergyPlus’ built-in performance curve equation manager (objects Curve:Linear, Curve:Quadratic and Curve:Biquadratic). It is not imperative that the user utilize all coefficients in the performance curve equations if their performance equation has fewer terms (e.g., if the user’s *TeFIRfPLR* performance curve is linear instead of quadratic, simply enter the values for a and b, and set coefficient c equal to zero). A set of curves derived from manufacturer’s data are also provided in the dataset (ExhaustFiredChiller.idf) is provided with E+ installation.

The condenser load is computed as follows:

<div>$$CondenserLoad = CoolingLoad + CoolThermalEnergyInput/HFIR + CoolElectricPower$$</div>

#### Heating

The following nomenclature is used in the heating equations:



*AvailHeatCap*         =   available full-load heating capacity at current conditions [W]

*CPLRh*                    =   cooling part-load ratio for heating curve =
 *CoolingLoad* / *NomCoolCap*

*HeatCapfCPLR*       =   heating capacity factor as a function of cooling part load ratio, equal to 1 at zero cooling load, user input “Heating Capacity Function of Cooling Capacity Curve Name”

*HeatCoolCapRatio *=   user input “Heating to Cooling Capacity Ratio”

*HeatElectricPower*  =   heating electricity input [W]

*HeatThermalEnergyInput*= heating thermal energy input [W]

*HeatingLoad*           =   current heating load on the chiller [W]

*HEIR*                       =   user input “Electric Input to Heating Output Ratio”

*HFIR*                       =   user input “Thermal Energy Input to Heating Output Ratio”

*HFIRfHPLR*            =   thermal energy input to heating output factor, equal to 1 at full load, user input “Thermal Energy Input to Heat Output Ratio During Heating Only Operation Curve Name”

*HPLR*                      =   heating part-load ratio = *HeatingLoad* / *AvailHeatCap*

*MinPLR                   *=   user input “Minimum Part Load Ratio”

*NomCoolCap*          =   user input “Nominal Cooling Capacity” [W]

*RunFrac*                  =   fraction of time step which the chiller is running

*TotalElectricPower *=   total electricity input [W]

*TotalThermalEnergyInput*=          total thermal energy input [W]

Cooling is the primary purpose of the Exhaust Fired Absorption Chiller so that function is satisfied first and if energy is available for providing heating that is provided next.

The two performance curves for heating capacity and efficiency are:

1)    Heating Capacity Function of Cooling Capacity Curve

2)    Thermal-Energy-Input-to Heat Output Ratio Function

The heating capacity function of cooling capacity curve (*HeatCapfCPLR*) determines how the heating capacity of the chiller varies with cooling capacity when the chiller is simultaneously heating and cooling. The curve is normalized so an input of 1.0 represents the nominal cooling capacity and an output of 1.0 represents the full heating capacity.  An output of 1.0 should occur when the input is 0.0.

<div>$$HeatCapfCPLR = a + b \cdot CPLRh + c \cdot CPLR{h^2}$$</div>

The available heating capacity is then computed as follows:

<div>$$AvailHeatCap = NomCoolCap \cdot HeatCoolCapRatio \cdot HeatCapfCPLR(CPLRh)$$</div>

The thermal energy input to heat output ratio curve (*HFIRfHPLR*) function is used to represent the fraction of thermal energy used as the heating load varies as a function of heating part load ratio. It is normalized so that a value of 1.0 is the full available heating capacity. The curve is usually linear or quadratic and will probably be similar to a boiler curve for most chillers.

<div>$$HFIRfHPLR = a + b \cdot HPLR + c \cdot HPL{R^2}$$</div>

The thermal energy use rate when heating is computed as follows:

<div>$$HeatThermalEnergyInput = AvailHeatCap.HFIR.HFIRfHPLR(HPLR)$$</div>

The fraction of the time step during which the chiller is operating is computed as a function of the cooling and heating part-load ratios and the user-input minimum part-load ratio:

<div>$$RunFrac = MIN(1.0,MAX\left( {HPLR,CPLRh} \right)/MinPLR)$$</div>

The heating electric input to the chiller is computed as follows:

<div>$$HeatElectricPower = NomCoolCap \cdot HeatCoolCapRatio \cdot HEIR.RunFrac$$</div>

If the chiller is delivering heating and cooling simultaneously, the parasitic electric load would be double-counted, so the following logic is applied:

<div>$$\begin{array}{l}IF\left( {HeatElectricPower \le CoolElectricPower} \right)THEN\\\quad HeatElectricPower = 0.0\\ELSE\\\quad HeatElectricPower = HeatElectricPower - CoolElectricPower\\ENDIF\end{array}$$</div>

The total thermal energy and electric power input to the chiller is computed as shown below:

<div>$$TotalElectricPower = HeatElectricPower + CoolElectricPower$$</div>

<div>$$TotalThermalEnergyInput = HeatThermalEnergyInput + CoolThermalEnergyInput$$</div>

#### References

Personal communications with various absorption chiller manufacturers, March 2011.

Absorption Chillers and Heat Pumps, Keith Herold, Reinhard Radermacher and Sanford A. Klein (Mar 18, 1996).

Absorption systems for combined heat and power: The problem of part-load operation, ASHRAE Transactions, 2003, Vol 109, Part1.

### Constant COP Chiller

The input object Chiller:ConstantCOP provides a chiller model that is based on a simple, constant COP simulation of the chiller.  In this case, performance does not vary with chilled water temperature or condenser conditions.  The nominal capacity of the chiller and the COP are user specified along with the connections to the plant and condenser loop and mass flow rates.  *Such a model is useful when the user does not have access to detailed performance data.*

The chiller power is calculated from the load divided by the COP.  This chiller will meet the load as long as it does not exceed the nominal capacity specified by the user.

        QEvaporator = Load

        Power = Load / ConstCOPChiller(ChillNum)%COP

Then the evaporator temperatures are calculated from the load

        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/CPwater

        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp

The condenser load and temperatures are calculated from the evaporator load and the power to the chiller.

     QCondenser = Power + QEvaporator



     IF (ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) THEN

       IF (CondMassFlowRate &gt; WaterMassFlowTol) THEN

         CondOutletTemp = QCondenser/CondMassFlowRate/CPCW(CondInletTemp) + CondInletTemp

       ELSE

         CALL ShowSevereError('CalcConstCOPChillerModel: Condenser flow = 0, for CONST COP Chiller='//  &

                              TRIM(ConstCOPChiller(ChillNum)%Name))

         CALL ShowContinueErrorTimeStamp(' ')

         CALL ShowFatalError('Program Terminates due to previous error condition.')

       END IF

    ELSE ! Air Cooled or Evap Cooled

         !  Set condenser outlet temp to condenser inlet temp for Air Cooled or Evap Cooled

         !  since there is no CondMassFlowRate and would divide by zero

      CondOutletTemp = CondInletTemp

    END IF

See the InputOutput Reference for additional information.

#### Chiller Basin Heater

This chiller’s basin heater (for evaporatively-cooled condenser type) operates in the same manner as the Engine driven chiller’s basin heater. The calculations for the chiller basin heater are described in detail at the end of the engine driven chiller description (Ref. Engine Driven Chiller).

### Hot Water Heat Recovery from Chillers

The electric chillers (e.g., Chiller:Electric, Chiller:EngineDriven, Chiller:CombustionTurbine, Chiller:Electric:EIR, and Chiller:Electric:ReformulatedEIR) all have the option of connecting a third plant loop for heating hot water at the same time the chiller cools the chilled water.  The engine and combustion turbine chillers models include curves for heat recovery from oil and or jacket coolers.  The other three chillers can model heat recovery where part of its condenser section is connected to a heat recovery loop for what is commonly known as a double bundled chiller, or single condenser with split bundles.  The heat recovery chiller is simulated as a standard vapor compression refrigeration cycle with a double bundled condenser.  A double bundle condenser involves two separate flow paths through a split condenser.  One of these paths is condenser water typically connected to a standard cooling tower; the other path is hot water connected to a heat recovery loop.  After leaving the compressor, the refrigerant is condensed to liquid in a refrigerant to water condenser.  In a split bundle, the chiller’s internal controls will direct a part of the refrigerant to heat recovery condenser bundle and/or to the tower water condenser bundle depending on the chilled water load, the condenser inlet temperatures and internal chiller controls (and possibly a leaving hot water temperature setpoint).  The refrigerant pressure is then dropped through a throttling valve so that fluid can evaporate at a low pressure that provides cooling to the evaporator.

![](media/image3175.png)

Figure 161.  Diagram of Chiller:Electric with Heat Recovery

The algorithm for the heat recovery portion of the chiller needs to be determined from relatively simple inputs to estimate the amount of the heat that is recovered and then send the rest of the heat to the cooling tower. For the chiller models associated with the object Chiller:Electric,  air- or evaporatively-cooled condensers are allowed to be used with heat recovery and, when used, the condenser specific heat, mass flow rate, and temperatures shown below refer to outdoor air. A condenser air volume flow rate must be specified when using heat recovery with air- or evaporatively-cooled chillers.

The basic energy balance for the condenser section of a heat recovery chiller is

<div>$${\dot Q_{tot}} = {\dot Q_{Evap}} + {\dot Q_{Elec}} = {\dot Q_{Cond}} + {\dot Q_{HR}}$$</div>

In practice, if the entering temperature of the heat recovery hot fluid is too high, the chiller’s internal controls will redirect refrigerant away from the heat recovery bundle.  A user input is available for declaring the inlet high temperature limit, and if it is exceeded, the chiller will shut down heat recovery and request no flow and will not reject any condenser heat to that fluid.

The heat recovery condenser bundle is often physically smaller than the tower water condenser bundle and therefore may have limited heat transfer capacity.  User input for the relative capacity of the heat recovery bundle, <span>\({F_{HR,Cap}}\)</span>, is used to define a maximum rate of heat recovery heat transfer using

<div>$${\dot Q_{HR,Max}} = {F_{HR,Cap}}\left( {{{\dot Q}_{Evap,Ref}} + \frac{{{{\dot Q}_{Evap,Ref}}}}{{CO{P_{Ref}}}}} \right)$$</div>

This capacity factor is also used to autosize the heat recovery design fluid flow rate when it is set to autosize.  The design heat recover flow rate is calculated by multiplying <span>\({F_{HR,Cap}}\)</span> by the condenser tower water design flow rate.  If no capacity factor is input, it is assumed to be 1.0.

A heat recovery chiller may control the temperature of heat recovery fluid leaving the device by modulating the flow of refrigerant to the heat recovery condenser bundle.  There are two different algorithms used depending on if the input has declared a leaving setpoint node.

If no control setpoint node was named, then the model developed by Liesen and Chillar (2004) is used to approximate the relative distribution of refrigerant flow and condenser heat transfer between the bundles.  This model approximates the heat transfer situation by using average temperatures in and out of the condenser section.

<div>$${Q_{Tot}} = ({\dot m_{Heat{\mathop{\rm Re}\nolimits} c}}*C{p_{Heat{\mathop{\rm Re}\nolimits} c}} + {\dot m_{Cond}}*C{p_{Cond}})*({T_{AvgOut}} - {T_{AvgIn}})$$</div>

Then the inlet temperature is flow-weighted to determine lumped inlet and outlet conditions.

<div>$${T_{AvgIn}} = \frac{{({{\dot m}_{Heat{\mathop{\rm Re}\nolimits} c}}*C{p_{Heat{\mathop{\rm Re}\nolimits} c}}*{T_{Heat{\mathop{\rm Re}\nolimits} cIn}} + {{\dot m}_{Cond}}*C{p_{Cond}}*{T_{CondIn}})}}{{({{\dot m}_{Heat{\mathop{\rm Re}\nolimits} c}}*C{p_{Heat{\mathop{\rm Re}\nolimits} c}} + {{\dot m}_{Cond}}*C{p_{Cond}})}}$$</div>

<div>$${T_{AvgOut}} = \frac{{{Q_{Tot}}}}{{({{\dot m}_{Heat{\mathop{\rm Re}\nolimits} c}}*C{p_{Heat{\mathop{\rm Re}\nolimits} c}} + {{\dot m}_{Cond}}*C{p_{Cond}})}} + {T_{AvgIn}}$$</div>

The lumped outlet temperature is then used for an approximate method of determining the heat recovery rate

<div>$${\dot Q_{HR}} = {\dot m_{HR}}{c_p}_{HR}\left( {{T_{Avg,out}} - {T_{HR,in}}} \right)$$</div>

This rate is then limited by the physical size of the heat recovery bundle.

<div>$${\dot Q_{HR}} = Min\left( {{{\dot Q}_{HR}},{{\dot Q}_{HR,max}}} \right)$$</div>

If user input for the leaving temperature setpoint is available, then a second model is used to distribute refrigerant flow and condenser heat transfer between the bundles that attempts to meet the heat recovery load implied by the leaving setpoint.  When setpoint control is used, the desired rate of heat recovery heat transfer is:

<div>$${\dot Q_{HR,Setpoint}} = {\dot m_{HR}}{c_p}_{HR}\left( {{T_{HR,set}} - {T_{HR,in}}} \right)$$</div>

<div>$${\dot Q_{HR,Setpoint}} = Max\left( {{{\dot Q}_{HR,Setpoint}},0.0} \right)$$</div>

Then the heat recovery rate is simply modeled as the lower of the three different heat flow rates:  the desired capacity, the maximum capacity, and the current total heat rejection rate.

<div>$${\dot Q_{HR}} = Min\left( {{{\dot Q}_{HR,Setpoint}},{{\dot Q}_{HR,max}},{{\dot Q}_{Tot}}} \right)$$</div>

For both models, the condenser heat transfer rate is then

<div>$${\dot Q_{Cond}} = {\dot Q_{Tot}} - {\dot Q_{HR}}$$</div>

The outlet temperatures are then calculated using

<div>$${T_{HR,out}} = {T_{HR,in}} + {\raise0.7ex\hbox{${{{\dot Q}_{HR}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{HR}}} {{{\dot m}_{HR}}{c_p}_{HR}}}}\right.}\!\lower0.7ex\hbox{${{{\dot m}_{HR}}{c_p}_{HR}}$}}$$</div>

<div>$${T_{Cond,out}} = {T_{Cond,in}} + {\raise0.7ex\hbox{${{{\dot Q}_{Cond}}}$} \!\mathord{\left/ {\vphantom {{{{\dot Q}_{Cond}}} {{{\dot m}_{Cond}}{c_p}_{cond}}}}\right.}\!\lower0.7ex\hbox{${{{\dot m}_{Cond}}{c_p}_{cond}}$}}$$</div>

A heat recovery chiller may need to work harder because the refrigeration system faces a higher lift owing to an elevated effective temperature for heat rejection.  With heat recovery, the condenser temperature used with the chiller’s performance curves is determined using one of the following heat-flow-weighted methods. The first is used for the chiller model for the objects Chiller:Electric, and Chiller:Electric:EIR which use the condensing entering temperature for performance.

<div>$${T_{Cond,in,Avg}} = \frac{{\left( {{{\dot Q}_{HR}}{T_{HR,in}} + {{\dot Q}_{Cond}}{T_{Cond,in}}} \right)}}{{\left( {{{\dot Q}_{HR}} + {{\dot Q}_{Cond}}} \right)}}$$</div>

The second is used for the chiller model for the object Chiller:Electric:ReformulatedEIR which uses the leaving condenser fluid temperature.

<div>$${T_{Cond,out,Avg}} = \frac{{\left( {{{\dot Q}_{HR}}{T_{HR,out}} + {{\dot Q}_{Cond}}{T_{Cond,out}}} \right)}}{{\left( {{{\dot Q}_{HR}} + {{\dot Q}_{Cond}}} \right)}}$$</div>

Both of these are available as an output variable called Chiller Effective Heat Rejection Tempeature, in C.

#### Chiller Basin Heater

This chiller’s basin heater (for evaporatively-cooled condenser type) operates in the same manner as the Engine driven chiller’s basin heater. The calculations for the chiller basin heater are described in detail at the end of the engine driven chiller description (Ref. Engine Driven Chiller).

**Reference**

Leisen and Chillar. 2004. Variable Heat Recovery In Double Bundle Electric Chillers. SimBuild 2004, IBPSA-USA National Conference Boulder, CO, August 4-6, 2004.

### Electric Chiller Model Based on Fluid Temperature Differences

The centrifugal chiller model (object name Chiller:Electric) was originally developed for the BLAST program.  The model is based on a ‘capacity ratio’ curve, which is a quadratic equation that determines the Ratio of Available Capacity to Nominal Capacity. The defining equation is:

<div>$$CapRatio = \frac{{Available Chiller Capacity}}{{Nominal Chiller Capacity}} = {A_1} + {A_2}\Delta {T_{chiller}} + {A_3}\Delta {T_{chiller}}^2$$</div>

Where the Delta Temperature is defined as:

<div>$$\Delta {T_{chiller}} = \frac{{{T_{cond, in}} - {T_{cond,in,design}}}}{{TempRiseRatio}} - \left( {{T_{evap,out}} - {T_{evap,out,design}}} \right)$$</div>

where the temperature rise coefficient is defined as the ratio of the required change in condenser water temperature to a given change in chilled water temperature, which maintains the capacity at the nominal value.  If the chiller is a heat recovery chiller,then the condenser inlet temperature is adjusted to account for both fluid streams as described in the section above on heat recovery chillers. This is calculated as the following ratio:

<div>$$TempRiseRatio = \frac{{{T_{cond, in,required}} - {T_{cond,in,rated}}}}{{{T_{evap, out,required}} - {T_{evap,out,rated}}}} - \left( {{T_{evap,out}} - {T_{evap,out,design}}} \right)$$</div>

Where:

*T<sub>cond,in,required</sub>* = Required entering condenser air or water temperature to maintain rated capacity (C)

*T<sub>cond,in,rated</sub>* = Rated entering condenser air or water temperature at rated capacity (C)

*T<sub>evap,out,required</sub>* = Required leaving evaporator water outlet temperature to maintain rated capacity (C)

*T<sub>evap,out,rated</sub>* = Rated leaving evaporator water outlet temperature at rated capacity (C)

The Power Ratio Curve is a quadratic equation that determines the Ratio of Full Load to Power.  The defining equation is:

<div>$$PowerRatio = {B_1} + {B_2}PLR + {B_3}PL{R^2}$$</div>

where the part load ratio, PLR is defined as:

<div>$$PLR = \frac{{chiller cooling load}}{{nominal chiller capacity}}$$</div>

The Load Ratio Curve is a quadratic equation that determines the Ratio of Actual Cooling Load to Full Cooling Load.  The defining equation is:

<div>$$LoadRatio = {C_1} + {C_2}CapRatio + {C_3}CapRati{o^2}$$</div>

The evaporator heat transfer rate and the power required by the chiller are then calculated as:

<div>$$Qevap = AvailableChillerCap*PLR$$</div>

<div>$$Power = PowerRatio*LoadRatio*\frac{{AvailableChillerCap}}{{RatedCOP}}$$</div>

### Electric Chiller Model Based on Condenser Entering Temperature

#### Overview

This model (object name Chiller:Electric:EIR) simulates the performance of an electric liquid chiller. The model is based on the compression chiller model (COMREF) in the DOE-2.1 building energy simulation program. The EnergyPlus model contains all of the features of the DOE-2.1 chiller model, plus additional abilities for modeling evaporatively-cooled condensers and heat recovery for water heating.

This model simulates the thermal performance of the chiller and the power consumption of the compressor(s). It also models the power consumption of condenser fans if modeling an air-cooled or evaporatively-cooled condenser. This model does not simulate the thermal performance or the power consumption of associated pumps or cooling towers. This auxiliary equipment must be modeled using other EnergyPlus models (e.g. CoolingTower:SingleSpeed).

#### Model Description

The chiller model uses user-supplied performance information at reference conditions along with three performance curves (curve objects) for cooling capacity and efficiency to determine chiller operation at off-reference conditions. The three performance curves are:

1)    Cooling Capacity Function of Temperature Curve

2)    Energy Input to Cooling Output Ratio Function of Temperature Curve

3)    Energy Input to Cooling Output Ratio Function of Part Load Ratio Curve

·        The cooling capacity function of temperature curve is a biquadratic performance curve with two independent variables: the leaving chilled water temperature and the entering condenser fluid temperature. The output of this curve is multiplied by the reference capacity to give the full-load cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the reference temperatures). The curve should have a value of 1.0 at the reference temperatures and flow rates specified in the input data file by the user. The biquadratic curve should be valid for the range of water temperatures anticipated for the simulation. If the chiller is a heat recovery chiller,then the condenser inlet temperature is adjusted to account for both fluid streams as described in the section above on heat recovery chillers.

<span>\(ChillerCapFTemp = a + b\left( {{T_{cw,l}}} \right) + c{\left( {{T_{cw,l}}} \right)^2} + d\left( {{T_{cond,e}}} \right) + e{\left( {{T_{cond,e}}} \right)^2} + f\left( {{T_{cw,l}}} \right)\left( {{T_{cond,e}}} \right)\)</span>where

*ChillerCapFTemp* = cooling capacity factor, equal to 1 at reference conditions

*T<sub>cw,l</sub>*            = leaving chilled water temperature, ˚C

*T<sub>cond,e</sub>*         = entering condenser fluid temperature, ˚C. For a water-cooled condenser this will be the water temperature returning from the condenser loop (e.g., leaving the cooling tower). For air- or evap-cooled condensers this will be the entering outdoor air dry-bulb or wet-bulb temperature, respectively.

·        The energy input to cooling output ratio function of temperature curve is a biquadratic performance curve that parameterizes the variation of the energy input to cooling output ratio (EIR) as a function of the leaving chilled water temperature and the entering condenser fluid temperature. The EIR is the inverse of the COP. The output of this curve is multiplied by the reference EIR (inverse of the reference COP) to give the full-load EIR at specific temperature operating conditions (i.e., at temperatures different from the reference temperatures). The curve should have a value of 1.0 at the reference temperatures and flow rates specified in the input data file by the user. The biquadratic curve should be valid for the range of water temperatures anticipated for the simulation.

<div>$$ChillerEIRFTemp = a + b\left( {{T_{cw,l}}} \right) + c{\left( {{T_{cw,l}}} \right)^2} + d\left( {{T_{cond,e}}} \right) + e{\left( {{T_{cond,e}}} \right)^2} + f\left( {{T_{cw,l}}} \right)\left( {{T_{cond,e}}} \right)$$</div>

where

*ChillerEIRFTemp* = energy input to cooling output factor, equal to 1 at reference conditions

*T<sub>cw,l</sub>*      = leaving chilled water temperature, ˚C

*T<sub>cond,e</sub>*<sub>     </sub>= entering condenser fluid temperature, ˚C. For a water-cooled condenser this will be the water temperature returning from the condenser loop (e.g., leaving the cooling tower). For air- or evap-cooled condensers this will be the entering outdoor air dry-bulb or wet-bulb temperature, respectively. If the chiller is a heat recovery chiller,then the condenser inlet temperature is adjusted to account for both fluid streams as described in the section above on heat recovery chillers.

·        The energy input to cooling output ratio function of part-load ratio curve is a quadratic performance curve that parameterizes the variation of the chiller input power ratio as a function of the part-load ratio. The part-load ratio is the actual cooling load divided by the chiller’s available cooling capacity. The output of this curve is multiplied by the reference EIR (inverse of the reference COP) and the Energy Input to Cooling Output Ratio Function of Temperature Curve to give the EIR at the specific temperatures and part-load ratio at which the chiller is operating. This curve should have a value of 1.0 when the part-load ratio equals 1.0. The quadratic curve should be valid for the range of part-load ratios anticipated for the simulation.

<div>$$ChillerEIRFPLR = a + b\left( {PLR} \right) + c{\left( {PLR} \right)^2}\, = \,{\raise0.7ex\hbox{${{P_{chiller}}}$} \!\mathord{\left/ {\vphantom {{{P_{chiller}}} {{P_{ref}}\left( {ChillerCapFTemp} \right)\left( {ChillerEIRFTemp} \right)}}}\right.}\!\lower0.7ex\hbox{${{P_{ref}}\left( {ChillerCapFTemp} \right)\left( {ChillerEIRFTemp} \right)}$}}$$</div>

where

*ChillerEIRFPLR* = energy input to cooling output factor, equal to 1 at reference conditions

*PLR* =part-load ratio = (cooling load) / (chiller’s available cooling capacity)

*P<sub>chiller</sub> = chiller power at specific PLR*

*P<sub>ref</sub> = <span>\({\mathop Q\limits^ \bullet_{ref}}\)</span>/COP<sub>ref</sub>*

All three of the performance curves are accessed through EnergyPlus’ built-in performance curve equation manager (curve:quadratic and curve:biquadratic). It is not imperative that the user utilize all coefficients in the performance curve equations if their performance equation has fewer terms (e.g., if the user’s ChillerEIRFPLR performance curve is linear instead of quadratic, simply enter the values for a and b, and set coefficient c equal to zero). Performance curves for more than 160 chillers, including the default DOE-2.1E reciprocating and centrifugal chillers, are provided in the EnergyPlus Reference DataSets (Chillers.idf and AllDataSets.idf).

Note: Chiller:Electric:EIR objects and their associated performance curve objects are developed using performance information for a specific chiller and should normally be used together for an EnergyPlus simulation. Changing the object input values, or swapping performance curves between chillers, should be done with caution.

For any simulation time step, the chiller’s available cooling capacity is calculated as follows:

<div>$${\mathop Q\limits^ \bullet_{avail}}\,\, = \,\,{\mathop Q\limits^ \bullet_{ref}}\left( {ChillerCapFTemp} \right)$$</div>

where

<span>\({\mathop Q\limits^ \bullet_{ref}}\)</span>     = chiller capacity at reference conditions (reference temperatures and flow rates defined by the user), W

<span>\(\mathop Q\limits^ \bullet  avail\)</span> = available chiller capacity adjusted for current fluid temperatures, W

The model then calculates the evaporator heat transfer rate required to bring the entering chilled water temperature down to the leaving chilled water setpoint temperature (established using a SetpointManager object and referenced in the PlantLoop object). If this calculated heat transfer rate is greater than the heat transfer rate being requested by the plant equipment operation scheme, then the evaporator heat transfer rate is reset to the requested cooling rate.

The evaporator heat transfer rate is then compared to the available capacity. If the available chiller capacity is sufficient to meet the evaporator heat transfer rate, the leaving chilled water temperature is set equal to the chilled water setpoint temperature. If the requested evaporator heat transfer rate is larger than the available capacity the chilled water leaving the evaporator is allowed to float upward. For this case, the exiting chilled water temperature is calculated based on the water temperature entering the evaporator, the available cooling capacity, and the evaporator mass flow rate as follows:

<div>$${T_{cw,l}} = {T_{cw,e}} - \left( {{\raise0.7ex\hbox{${{{\mathop Q\limits^ \bullet  }_{avail}}}$} \!\mathord{\left/ {\vphantom {{{{\mathop Q\limits^ \bullet  }_{avail}}} {{{\mathop m\limits^ \bullet  }_{evap}}{C_{p,evap}}}}}\right.}\!\lower0.7ex\hbox{${{{\mathop m\limits^ \bullet  }_{evap}}{C_{p,evap}}}$}}} \right)$$</div>

where

*T<sub>cw,l</sub>*            = water temperature leaving the evaporator, ˚C

*T<sub>cw,e</sub>*           = water temperature entering the evaporator, ˚C

<span>\({\mathop m\limits^ \bullet_{evap}}\)</span>         = evaporator mass flow rate, kg/s

*C<sub>p,evap</sub>*         = specific heat of water entering evaporator at *T<sub>cw,e</sub>*, J/kg-˚C

The part-load ratio is then calculated as the ratio of the evaporator heat transfer rate to the available chiller capacity. The part-load ratio is not allowed to be greater than the maximum part-load ratio specified by the user or less than zero as follows:

<div>$$PLR = MAX\left( {0.0,MIN\left( {{\raise0.7ex\hbox{${{{\mathop Q\limits^ \bullet  }_{evap}}}$} \!\mathord{\left/ {\vphantom {{{{\mathop Q\limits^ \bullet  }_{evap}}} {{{\mathop Q\limits^ \bullet  }_{avail}}}}}\right.}\!\lower0.7ex\hbox{${{{\mathop Q\limits^ \bullet  }_{avail}}}$}},\,\,PL{R_{\max }}} \right)} \right)$$</div>

where

*PLR*           = part-load ratio

<span>\({\mathop Q\limits^ \bullet_{evap}}\)</span>         = load to be met by the chiller, W

*PLR<sub>max\\        </sub>*= maximum part-load ratio (specified by the user in the input data file)



The model assumes that the cooling load is met through chiller unloading down to the minimum unloading ratio. False loading (e.g. hot-gas bypass) is assumed to occur between the minimum unloading ratio and the minimum part load ratio yielding constant electrical power consumption under these conditions. Below the minimum part load ratio, the chiller cycles on and off to meet very small loads and the power consumption during the on cycle is the same as when the chiller is operating at the minimum part load ratio. When the chiller part load ratio is less than the minimum part load ratio, the on-off cycling ratio of the chiller is calculated as follows and is available as an output variable.

<div>$$ChillerCyclingRatio = MIN\left( {{\raise0.7ex\hbox{${PLR}$} \!\mathord{\left/ {\vphantom {{PLR} {PL{R_{\min }}}}}\right.}\!\lower0.7ex\hbox{${PL{R_{\min }}}$}},\,\,1.0} \right)$$</div>

To properly account for chiller electric power consumption when PLR is less than the minimum unloading ratio, the PLR is reset to the greater of the PLR calculated above and the PLR at the minimum unloading ratio. The result is available as the output variable Chiller Part Load Ratio.

<div>$$PLR = MAX(PLR,PL{R_{MinUnloadRatio}})$$</div>

This revised PLR accounts for the “false loading” (e.g., hot gas bypass) that is assumed to occur whenever the PLR (based on cooling load divided by available capacity) is less than the minimum unloading ratio specified. The amount of false loading on the chiller is calculated using this revised PLR and is reported as an output variable as follows:

<div>$${\mathop Q\limits^ \bullet_{falseloading}} = \left( {{{\mathop Q\limits^ \bullet  }_{avail}}} \right)\left( {PLR} \right)\left( {ChillerCyclingRatio} \right) - {\mathop Q\limits^ \bullet_{evap}}$$</div>

The electrical power consumption for the chiller compressor(s) for any simulation time step is then calculated using the following equation:

<span>\({P_{chiller}}\, = \,\,\left( {{{\mathop Q\limits^ \bullet  }_{avail}}\,} \right)\,\left( {\frac{1}{{CO{P_{ref}}}}} \right)\,\left( {ChillerEIRFTemp} \right)\left( {ChillerEIRFPLR} \right)\left( {ChillerCyclingRatio} \right)\)</span>where

*P<sub>chiller</sub>*         = chiller compressor power, W

*COP<sub>ref</sub>*       = reference coefficient of performance, W/W



Heat rejected by the chiller condenser includes the heat transferred in the evaporator plus a portion or all of the compressor electrical energy consumption. For electric chillers with hermetic compressors, all compressor energy consumption is rejected by the condenser (compressor motor efficiency = *eff<sub>motor</sub>* = 1.0). For chillers with semi-hermetic or open compressors, only a portion of the compressor energy use is rejected by the condenser. The heat transfer rate for the chiller condenser is calculated as follows:

<div>$${\mathop Q\limits^ \bullet_{cond}} = \left( {{P_{chiller}} * ef{f_{motor}}} \right) + {\mathop Q\limits^ \bullet_{evap}} + {\mathop Q\limits^ \bullet_{falseloading}}$$</div>

where

<span>\({\mathop Q\limits^ \bullet_{cond}}\)</span>         = condenser heat transfer rate, W

<span>\(ef{f_{motor}}\)</span>       = compressor motor efficiency = fraction of compressor electrical energy consumption rejected as condenser heat

For water-cooled chillers, the water temperature leaving the condenser is then calculated as shown below.

<div>$${T_{cond,l}} = {T_{cond,e}} + {\raise0.7ex\hbox{${{{\mathop Q\limits^ \bullet  }_{cond}}}$} \!\mathord{\left/ {\vphantom {{{{\mathop Q\limits^ \bullet  }_{cond}}} {\left( {{{\mathop m\limits^ \bullet  }_{cond}} * {C_{p,cond}}} \right)}}}\right.}\!\lower0.7ex\hbox{${\left( {{{\mathop m\limits^ \bullet  }_{cond}} * {C_{p,cond}}} \right)}$}}$$</div>

where:

*T<sub>cond,l</sub>*          = water temperature leaving the condenser, ˚C

*T<sub>cond,e</sub>*         = water temperature entering the condenser, ˚C

<span>\({\mathop m\limits^ \bullet_{cond}}\)</span>         = mass flow rate through the condenser, kg/s

<span>\({C_{p,cond}}\)</span>       = specific heat of water entering the condenser at *T<sub>cond,e</sub>*, J/kg-˚C

For air- and evaporatively-cooled condensers, the exiting air temperature is not calculated and is set equal to the entering air or wet-bulb temperature, respectively.

The model then calculates the condenser fan energy for air- and evaporatively-cooled condensers. The amount of condenser fan energy is assumed to be proportional to the chiller cycling ratio and is calculated as follows:

<div>$${P_{cond}}\, = \,{\mathop Q\limits^ \bullet_{ref}}\,\left( {{P_{condfanratio}}} \right)\,\left( {ChillerCyclingRatio} \right)$$</div>

where

*P<sub>cond</sub>*           = chiller condenser fan electric power, W

*P<sub>condfanratio</sub>*   = condenser fan power ratio, W/W



The final calculations determine the total heat transfer energy for the condenser and evaporator, as well as the total electric energy consumed by the chiller compressor motor(s) and condenser fan(s). The results are available as output variables.

<div>$${Q_{cond}} = {\mathop Q\limits^ \bullet_{cond}}*TimeStepSys*3600$$</div>

<div>$${Q_{evap}} = {\mathop Q\limits^ \bullet_{evap}}\,\,*\,\,TimeStepSys\,\,*\,\,3600$$</div>

<div>$${E_{chiller}} = {P_{chiller}}*\,\,TimeStepSys\,\,*\,\,3600$$</div>

<div>$${E_{cond}} = {P_{cond}}\,\, * \,\,TimeStepSys\,\,*\,\,3600$$</div>

where

*Q<sub>cond</sub>*          = chiller condenser heat transfer energy, J

*Q<sub>evap</sub>*           = chiller evaporator cooling energy, J

*E<sub>chiller</sub>*         = chiller (compressor) electric energy, J

*E<sub>cond</sub>*           = chiller condenser fan electric energy, J

*TimeStepSys* = HVAC system simulation time step, hr

<span>\(3600\)</span>         = conversion factor, sec/hr

#### Electric EIR Chiller with Heat Recovery Option

Heat from the electric EIR chiller condenser may be recovered when a water-cooled condenser is selected for simulation. The heat recovery water flow rate is specified by the user along with the input and output nodes connected to the heat recovery loop. The algorithms are identical to those used for Chiller:Electric. Refer to the section entitled Chillers with Plant Heat Recovery for details.

#### Standard Rating (Integrated Part Load Value)

For the Chiller:Electric:EIR and Chiller:Electric:ReformulatedEIR objects in EnergyPlus, the industry standard rating of Integrated Part Load Value (IPLV) is calculated according to ANSI/AHRI Standard 550/590 (2011). This standard rating is not direct input to the model and is calculated using user-entered information for these objects. These standard rating values are provided in the eplusout.eio output file (Ref. Output Details document) and also in the predefined tabular output reports (Output:Table:SummaryReports object, Equipment Summary).

Note: The standard ratings described in this section require that the EIR/Reformulated EIR chiller models be evaluated at specific operating conditions (e.g., specific evaporator outlet temperature (6.67 C) and dry-bulb temperatures for air entering the air-cooled [outdoor] condenser). If the chiller  performance curves can not be evaluated at the required test conditions, then the standard rating value will be determined at user specified curve limit and warning error message is provided. For example, if the curve object (Curve:Biquadratic) for Cooling Capacity Function of Temperature Curve has a minimum value of 21C for dry-bulb temperature entering the air-cooled condenser coil, the IPLV calculation requires that EER<sub>D</sub> be calculated at 13 C – so, this would result in IPLV value calculated at 21C and reported in the output and a warning message in the eplusout.err file.

The IPLV is a single number part-load performance figure of merit for Water-Chilling Packages. The IPLV equations and procedures described below are taken from Appendix D of ANSI/AHRI Std. 550/590, 2011 and provide a consistent method for calculating IPLV. These equations provide representative average part-load efficiency for a single chiller.  For equipment covered by this standard, the *IPLV* is calculated using the following formula:

<div>$$IPLV = (0.01A) + (0.42B) + (0.45C) + (0.12D)$$</div>

where,

<span>\(A\)</span>  =   *EER* or *COP* at 100% capacity at AHRI standard rating conditions

<span>\(B\)</span>  =   *EER* or *COP* at 75% capacity and reduced ambient (see Table 51)

<span>\(C\)</span>  =   *EER* or *COP* at 50% capacity and reduced ambient (see Table 51)

<span>\(D\)</span>  =   *EER* or *COP* at 25% capacity and reduced ambient (see Table 51)

The Coefficient of Performance (*COP*) at the various load capacity points (100%, 75%, 50%, and 25% part-load ratios) are calculated using the procedure outlined below and applicable test conditions specified in Table 51.

EER at desired reduced capacity (75%, 50%, and 25%) is calculated as follows

<div>$$COP = \frac{{\rm{1}}}{{EIR}}$$</div>

<div>$$EIR = EnergyInputRatio = \left( {\frac{{{\rm{Power }}}}{{{\rm{PartLoadRatio * AvailChillerCap}}}}} \right)$$</div>

<div>$$Power = \left( {\frac{{{\rm{AvailChillerCap}}}}{{CO{P_{reference}}}}} \right)(EIRTempModFa{c_{ReducedTemp}})(EIRPLRModFa{c_{ReducedPLR}})$$</div>

<div>$$AvailChillerCap = Capacit{y_{reference}}(CAPTempModFa{c_{ReducedTemp}})$$</div>

<span>\(Capacit{y_{reference}}\)</span>= Reference chiller capacity specified by the user, (W).

<span>\(CO{P_{reference}}\)</span>= Reference coefficient of performance specified by the user, (W/W).

<span>\(EIRTempModFa{c_{ReducedTemp}}\)</span>= User-specified bi-quadratic curve for modifying EIR as a function of leaving chilled water temperature (6.7°C) and entering condenser temperature obtained from Table 51 for reduced capacities, (dimensionless).

<span>\(EIRPLRModFa{c_{ReducedPLR}}\)</span>= User-specified quadratic curve for modifying EIR as a function of part load ratio.

<span>\(CapTempModFa{c_{ReducedTemp}}\)</span>= Capacity (W) of the chiller determined per the ANSI/AHRI Standard 550/590 reduced ambient test conditions as shown in Table 51.

If the equipment cannot be unloaded to the desired reduced capacity (75%, 50%, and 25%) i.e. if the minimum unloading ratio of the chiller is greater than desired reduced capacity, then the model is run at the minimum unloading PLR of the equipment at the condenser entering conditions defined in Table 51 and the efficiency is adjusted for cyclic performance.

<div>$$COP = \frac{{\rm{1}}}{{{C_D} \cdot EIRatMinPLR}}$$</div>

 where,

<span>\(EIRatMinPLR\)</span>= EIR of chiller at minimum unloading ratio

<span>\({C_D}\)</span>= degradation coefficient to account for cycling of the compressor for capacity less than the minimum capacity.

Thedegradation coefficient *C<sub>D</sub>* for the desired reduced load points (75%, 50%, or 25%) is determined using the following equation:

<div>$${C_D} = 1.13 - 0.13LF$$</div>

The load factor (*LF*) for the desired reduced load points (75%, 50%, or 25%) calculated from the following equation:

<div>$$LF = \frac{{\left( {\frac{{\% Load}}{{100}}} \right) \cdot Full{\rm{ }}load{\rm{ }}unit{\rm{ }}capacity}}{{Part{\rm{ }}load{\rm{ }}unit{\rm{ }}capacity}}$$</div>

Where,

*<span>\(\left( {\frac{{\% Load}}{{100}}} \right)\)</span>*            =          standard rating part load ratio (*PLR*) points, 75%, 50%, 25%.

<span>\(Full{\rm{ }}load{\rm{ }}unit{\rm{ }}capacity\)</span> =            Full load heating capacity (W) of the air-source heat pump equipment determined from ANSI/AHRI Standard 550/590 and test conditions shown in Table 51 for 100% load.

<span>\(Part{\rm{ }}load{\rm{ }}unit{\rm{ }}capacity\)</span> =           Part load heating capacity (W) of the air-source heat pump units determined from ANSI/AHRI Standard 550/590 at the standard desired reduced ambient test conditions as shown in Table 51  and the minimum part load capacity of the unit.

<div>$$Full{\rm{ }}load{\rm{ }}unit{\rm{ }}capacity = {\dot Q_{total,Reference}}\left( {CapTempModFa{c_{Rated}}} \right)$$</div>

<div>$$Part{\rm{ }}load{\rm{ }}unit{\rm{ }}capacity = MinPLR \cdot {\dot Q_{total,Reference}}\left( {CapTempModFa{c_{ReducedTemp}}} \right)$$</div>

where,

<span>\({\dot Q_{total,Reference}}\)</span> = Reference capacity specified by the user, (W).

<span>\(CapTempModFa{c_{Rated}}\)</span>= User-specified bi-quadratic curve evaluated at full load (100%) test conditions shown in Table 51, (dimensionless).

<span>\(CapTempModFa{c_{ReducedTemp}}\)</span>= Part load capacity (W) of the chiller determined from ANSI/AHRI Standard 550/590 at the standard desired reduced ambient test conditions as shown in Table 51.

<span>\(MinPLR\)</span>= Minimum PLR up to which chiller can be unloaded

Table 51. Standard Rating (Integrated Part Load Value)

<table class="table table-striped">
  <tr>
    <th colspan="5">Standard Rating (Integrated Part Load Value)</th>
  </tr>
  <tr>
    <td colspan="5">Source: Table 3, Page 10, ANSI/AHRI Standard 550/590-2011</th>
  </tr>
  <tr>
    <th></th>
    <td colspan="2">IPLV</th>
    <td colspan="2">NPLV</th>
  </tr>
  <tr>
    <td>Evaporator (All types)<BR>100 % Load LWT<BR>0% Load LWT<BR>Flow Rate (gpm)<BR>F.F.A</td>
    <td>2 44.0&deg;F<BR>44.0&deg;F<BR>3 2.4 gpm/ton<BR>0.0001<BR>h- ft 2 -&deg;F/Btu</td>
    <td>6.7&deg;C<BR>6.7&deg;C<BR>0.043 L/s per<BR>kW<BR>0.000018<BR>m 2 -&deg;C/W</td>
    <td>2 Selected LWT<BR>Same as 100%<BR>load<BR>3 Selected<BR>gpm/ton<BR>As Specified</td>
    <td>2 Selected<BR>LWT<BR>Same as 100%<BR>load<BR>3 [L/s per kW]<BR>As Specified</td>
  </tr>
  <tr>
    <td>1 Condenser (Water<BR>Cooled)<BR>100% load EWT<BR>75% load EWT<BR>50% load EWT<BR>25% load EWT<BR>0% load EWT<BR>Flow rate (gpm) [L/s]<BR>F.F.A.</td>
    <td>2 85.0&deg;F<BR>75.0&deg;F<BR>65.0&deg;F<BR>65.0&deg;F<BR>65.0&deg;F<BR>3 3.0 gpm/ton<BR>0.00025<BR>h- ft 2 -&deg;F/Btu</td>
    <td>29.4&deg;C<BR>23.9&deg;C<BR>18.3&deg;C<BR>18.3&deg;C<BR>18.3&deg;C<BR>0.054 L/s per<BR>kW<BR>0.000044<BR>m 2 -&deg;C/W</td>
    <td>2 Selected LWT<BR>4 65.0&deg;F<BR>3 Selected<BR>gpm/ton<BR>As Specified</td>
    <td>2 Selected LWT<BR>4 18.3&deg;C<BR>3 Selected<BR>L/s per kW<BR>As Specified</td>
  </tr>
  <tr>
    <td>1 Condenser (Air Cooled)<BR>100% load EDB<BR>75% load EDB<BR>50% load EDB<BR>25% load EDB<BR>0% load EDB<BR>F.F.A.</td>
    <td>95.0&deg;F<BR>80.0&deg;F<BR>65.0&deg;F<BR>55.0&deg;F<BR>55.0&deg;F<BR>0.0 h- ft 2<BR>-&deg;F/Btu</td>
    <td>35&deg;C<BR>26.7&deg;C<BR>18.3&deg;C<BR>12.8&deg;C<BR>12.8&deg;C<BR>0.0 m 2 -&deg;C/W</td>
    <td colspan="2">No Rating Requirements</td>
  </tr>
  <tr>
    <td>1 Condenser<BR>(Evaporatively Cooled)<BR>100% load EWB<BR>0% load EWB<BR>F.F.A.</td>
    <td>75.0&deg;F<BR>50.0&deg;F<BR>0.0 h- ft 2<BR>-&deg;F/Btu</td>
    <td>23.9&deg;C<BR>10.0&deg;C<BR>0.0 m 2 -&deg;C/W</td>
    <td colspan="2">No Rating Requirements</td>
  </tr>
  <tr>
    <td>Air-Cooled Without<BR>Condenser<BR>100% load SDT<BR>0% load SDT</td>
    <td>125.0&deg;F<BR>55.0&deg;F</td>
    <td>51.7&deg;C<BR>12.8&deg;C</td>
    <td colspan="2">No Rating Requirements</td>
  </tr>
  <tr>
    <td>Water and Evaporatively-<BR>Cooled Without<BR>Condenser<BR>100% load SDT<BR>0% load SDT</td>
    <td>105.0&deg;F<BR>65.0&deg;F</td>
    <td>40.6&deg;C<BR>18.3&deg;C</td>
    <td colspan="2">No Rating Requirements</td>
  </tr>
  <tr>
    <td colspan="5">1 If the unit Manufacturer’s recommended minimum temperatures are greater than those specified in<BR>Table 3, then those may be used in lieu of the specified temperatures.<BR>2 Correction for Fouling Factor Allowance<BR>3 The flow rates are to be held constant at full load values for all part-load conditions.<BR>4 For part-load entering condenser water temperatures, the temperature should vary linearly from<BR>the selected<BR>EWT at 100% load to 65.0 oF at 50% loads, and fixed at 65.0&deg;F for 50% to 0% loads.<BR>SDT - saturated discharge temperature<BR>LWT - leaving water (liquid) temperature<BR>EWT - entering water (liquid) temperature<BR>EDB - entering air dry-bulb temperature<BR>EWB - entering air wet-bulb temperature<BR>F.F.A. - Fouling Factor Allowance</td>
  </tr>
</table>

### Electric Chiller Model Based on Condenser Leaving Temperature

#### Overview

This model (object name Chiller:Electric:ReformulatedEIR) simulates the thermal performance of an electric liquid chiller and the power consumption of its compressor(s). The model, developed by Hydeman et al. (2002) as part of the CoolTools™ project sponsored by Pacific Gas and Electric Company (PG&E), is an empirical model similar to EnergyPlus’ Chiller:Electric:EIR model. The model uses performance information at reference conditions along with three curve fits for cooling capacity and efficiency to determine chiller operation at off-reference conditions. The model has the same capabilities as the Chiller:Electric:EIR model, but can potentially provide significant accuracy improvement over the Chiller:Electric:EIR model for chillers with variable-speed compressor motor drives and/or variable condenser water flow applications.

Chiller performance curves can be generated by fitting manufacturer’s catalog data or measured data. Performance curves developed primarily from manufacturer’s performance data are provided in the EnergyPlus Reference DataSets (Chillers.idf and AllDataSets.idf). This chiller model can be used to predict the performance of various chiller types (e.g., reciprocating, screw, scroll, and centrifugal) with water-cooled condensers. The model does not simulate the thermal performance or the power consumption of associated pumps or cooling towers. This auxiliary equipment must be modeled using other EnergyPlus models (e.g. CoolingTower:SingleSpeed).

The main difference between this model and the Chiller:Electric:EIR model is the condenser fluid temperature used in the associated performance curves: the Chiller:Electric:ReformulatedEIR model uses the LEAVING condenser water temperature while the Chiller:Electric:EIR model uses the ENTERING condenser water temperature. In addition, the Energy Input to Cooling Output Function of Part Load Ratio curve for this reformulated EIR chiller model includes the condenser leaving water temperature as an independent variable in addition to part-load ratio. Since the leaving condenser water temperature is a function of load, chiller performance, and condenser entering water temperature, EnergyPlus must iterate to converge on a solution for each simulation time step.

#### Model Description

The chiller model uses user-supplied performance information at reference conditions along with three performance curves (curve objects) for cooling capacity and efficiency to determine chiller operation at off-reference conditions. The three performance curves are:

1)    Cooling Capacity Function of Temperature Curve

2)    Energy Input to Cooling Output Ratio Function of Temperature Curve

3)    Energy Input to Cooling Output Ratio Function of Part Load Ratio Curve

·        The cooling capacity function of temperature curve is a biquadratic performance curve with two independent variables: the leaving chilled water temperature and the leaving condenser water temperature. The output of this curve is multiplied by the reference capacity to give the full-load cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the reference temperatures). The curve should have a value of 1.0 at the reference temperatures and flow rates specified in the input data file by the user. The biquadratic curve should be valid for the range of water temperatures anticipated for the simulation (otherwise the program issues warning messages).

<div>$$ChillerCapFTemp = a + b({T_{cw,l}}) + c{({T_{cw,l}})^2} + d({T_{cond,l}}) + e{({T_{cond,l}})^2} + f({T_{cw,l}})({T_{cond,l}})$$</div>

where

*ChillerCapFTemp* = Cooling capacity factor, equal to 1 at reference conditions

*T<sub>cw,l</sub>*            = leaving chilled water temperature, ˚C

*T<sub>cond,l</sub>*<sub>            </sub>= leaving condenser water temperature, ˚C. This will be the water temperature entering the condenser loop (e.g., entering the cooling tower). If the chiller is a heat recovery chiller,then the condenser leaving temperature is adjusted to account for both fluid streams as described in the section above on heat recovery chillers.

·        The energy input to cooling output ratio function of temperature curve is a biquadratic performance curve that parameterizes the variation of the energy input to cooling output ratio (EIR) as a function of the leaving chilled water temperature and the leaving condenser water temperature. The EIR is the inverse of the COP. The output of this curve is multiplied by the reference EIR (inverse of the reference COP) to give the full-load EIR at specific temperature operating conditions (i.e., at temperatures different from the reference temperatures). The curve should have a value of 1.0 at the reference temperatures and flow rates specified in the input data file by the user. The biquadratic curve should be valid for the range of water temperatures anticipated for the simulation (otherwise the program issues warning messages).

<span>\(ChillerEIRFTemp = a + b({T_{cw,l}}) + c{({T_{cw,l}})^2} + d({T_{cond,l}}) + e{({T_{cond,l}})^2} + f({T_{cw,l}})({T_{cond,l}})\)</span>where

*ChillerEIRFTemp* = Energy input to cooling output factor, equal to 1 at reference conditions

*T<sub>cw,l</sub>*      = leaving chilled water temperature, ˚C

*T<sub>cond,l</sub>*<sub>     </sub>= leaving condenser water temperature, ˚C. This will be the water temperature entering the condenser loop (e.g., entering the cooling tower). If the chiller is a heat recovery chiller,then the condenser leaving temperature is adjusted to account for both fluid streams as described in the section above on heat recovery chillers.

·        The energy input to cooling output ratio function of part-load ratio curve parameterizes the variation of the energy input ratio (EIR). The output of this curve is multiplied by the reference EIR (inverse of the reference COP) and the Energy Input to Cooling Output Ratio Function of Temperature Curve to give the EIR at the specific temperatures and part-load ratio at which the chiller is operating. This curve should have a value of 1.0 at the reference leaving condenser water temperature with part-load ratio equal to 1.0. It is recommended that this performance curve be developed using both full- and part-load performance data. The bicubic curve should be valid for the range of condenser water temperatures and part-load ratios anticipated for the simulation (otherwise the program issues warning messages). Either of the following two types of curves can be used.

The first type is a bicubic performance curve that parameterizes the variation of the chiller input power ratio as a function of the leaving condenser water temperature and the part-load ratio. The part-load ratio is the actual cooling load divided by the chiller’s available cooling capacity.

<div>$$ChillerEIRFPLR = a + b({T_{cond,l}}) + c{({T_{cond,l}})^2} + d(PLR) + e{(PLR)^2} + f({T_{cond,l}})(PLR) + g{(PLR)^3}$$</div>

<div>$$ChillerEIRFPLR = \,{\raise0.7ex\hbox{${{P_{chiller}}}$} \!\mathord{\left/ {\vphantom {{{P_{chiller}}} {{P_{ref}}\left( {ChillerCapFTemp} \right)\left( {ChillerEIRFTemp} \right)}}}\right.}\!\lower0.7ex\hbox{${{P_{ref}}\left( {ChillerCapFTemp} \right)\left( {ChillerEIRFTemp} \right)}$}}$$</div>

where

*ChillerEIRFPLR* = Energy input to cooling output factor, equal to 1 at the reference leaving condenser water temperature and PLR = 1.0

*T<sub>cond,l</sub>*<sub>     </sub>= leaving condenser water temperature, ˚C. This will be the water temperature entering the condenser loop (e.g., entering the cooling tower). If the chiller is a heat recovery chiller,then the condenser leaving temperature is adjusted to account for both fluid streams as described in the section above on heat recovery chillers.

*PLR*      =Part load ratio = (cooling load) / (chiller’s available cooling capacity)

*P<sub>chiller</sub> = chiller power at specific PLR*

*P<sub>ref</sub> = <span>\({\mathop Q\limits^ \bullet_{ref}}\)</span>/COP<sub>ref</sub>*

Note: Although a bicubic curve requires 10 coefficients (ref. Curve:Bicubic), coefficients 7, 9 and 10 are typically not used in the performance curve described here and should be entered as 0 unless sufficient performance data and regression accuracy exist to justify the use of these terms of the bicubic curve.

The second type is a Chiller Part Load Custom Curve that parameterizes the variation of EIR as a function of the normalized dT, normalized Tdev and the PLR.

<div>$$ChillerEIRFPLR = a + b(dT^*) + c(dT^*)^2 + d \cdot PLR + e \cdot PLR^2 + f \cdot (dT^*) \cdot PLR + g \cdot (dT^*)^3  + h \cdot PLR^3 + i \cdot (dT^*)^2 \cdot PLR + j \cdot (dT^*) \cdot PLR^2 + k \cdot (dT^*)^2 \cdot PLR^2 + l \cdot (T_{dev}^*) \cdot PLR^3$$</div>

<div>$$dT^* = dT / dT_{ref}$$</div>

<div>$$T_{dev}^* = T_{dev} / dT_{ref}$$</div>

where

$dT$ = the delta of temperature across the leaving condenser water temperature and leaving evaporator water temperature of a chiller (lift)

$dT^*$ = the normalized fractional lift

$dT_{ref}$ = the lift under the reference condition

PLR = the part load ratio

$T_{dev}$ = the deviation of leaving chilled water temperature from the reference condition

$T_{dev}^*$ = the normalized Tdev term

All three of the performance curves are accessed through EnergyPlus’ built-in performance curve equation manager (curve:biquadratic, curve:bicubic, and Curve:ChillerPartLoadWithLift). Note that the above three performance curves use the leaving condenser water temperature as an independent variable, instead of the entering condenser water temperature used in the performance curves for the Chiller:Electric:EIR model. Since the leaving condenser water temperature is calculated based on the condenser heat transfer rate, which is a function of the load to be met by the chiller, chiller compressor power, and the false loading (detailed calculations are given below), iterative calculations are required to determine the actual (converged) leaving condenser water temperature. The program uses the leaving condenser water temperature from the previous iteration to calculate values for each of the three performance curves described above. After obtaining the condenser heat transfer rate, the leaving condenser water temperature is recalculated. When the difference between the leaving condenser water temperature calculated on successive iterations is less than 0.0001<sup>°</sup>C, the solution is assumed to have converged. Warning messages are issued if the calculated solution for leaving condenser water temperature and/or part-load ratio falls outside the valid range specified for the chiller’s performance curves. If these warnings are issued, the user may choose to extend the range for the performance curves (only if a small extension is required since model extrapolation may produce significant errors) or a different chiller and associated performance curves with extended performance range can be located and used for the simulation.

Note: Chiller:Electric:ReformulatedEIR objects and their associated performance curve objects are developed using performance information for a specific chiller and should almost always be used together for an EnergyPlus simulation. Changing the object input values, or swapping performance curves between chillers, should be done with extreme caution. For example, if the user wishes to model a chiller size that is different from the reference capacity, it is highly recommended that the reference flow rates be scaled proportionately to the change in reference capacity. Although this model can provide more accurate prediction than the Chiller:Electric:EIR model, it requires more performance data to develop the associated performance curves (at least 12 points from full-load performance and 7 points from part-load performance).

Although performance curve data sets for 160 chillers are provided in the EnergyPlus Reference DataSets (Chillers.idf and AllDataSets.idf), they may not meet the requirements for specific applications. One can develop performance curves from performance data using two available techniques (Hydeman and Gillespie 2002). The first technique is called the Least-squares Linear Regression method and is used when sufficient performance data exist to employ standard least-square linear regression techniques. The second technique is called Reference Curve Method and is used when insufficient performance data exist to apply linear regression techniques. A detailed description of both techniques can be found in the reference mentioned above.

For any simulation time step, the chiller’s available cooling capacity is calculated as follows:

<div>$${\mathop Q\limits^ \bullet_{avail}}\,\, = \,\,{\mathop Q\limits^ \bullet_{ref}}\left( {ChillerCapFTemp} \right)$$</div>

where

<span>\({\mathop Q\limits^ \bullet_{ref}}\)</span>     = chiller capacity at reference conditions (reference temperatures and flow rates defined by the user), W

<span>\({\mathop Q\limits^ \bullet_{avail}}\)</span> = available chiller capacity adjusted for current water temperatures, W

The model then calculates the evaporator heat transfer rate required to bring the entering chilled water temperature down to the leaving chilled water setpoint temperature (established using a SetpointManager object and referenced in the PlantLoop object). If this calculated heat transfer rate is greater than the heat transfer rate being requested by the plant equipment operation scheme, then the evaporator heat transfer rate is reset to the requested cooling rate.

The evaporator heat transfer rate is then compared to the available capacity. If the available chiller capacity is sufficient to meet the evaporator heat transfer rate, the leaving chilled water temperature is set equal to the chilled water setpoint temperature. If the requested evaporator heat transfer rate is larger than the available capacity the chilled water leaving the evaporator is allowed to float upward. For this case, the exiting chilled water temperature is calculated based on the water temperature entering the evaporator, the available cooling capacity, and the evaporator mass flow rate as follows:

<div>$${T_{cw,l}} = {T_{cw,e}} - \left( {{\raise0.7ex\hbox{${{{\mathop Q\limits^ \bullet  }_{avail}}}$} \!\mathord{\left/ {\vphantom {{{{\mathop Q\limits^ \bullet  }_{avail}}} {{{\mathop m\limits^ \bullet  }_{evap}}{C_{p,evap}}}}}\right.}\!\lower0.7ex\hbox{${{{\mathop m\limits^ \bullet  }_{evap}}{C_{p,evap}}}$}}} \right)$$</div>

where

*T<sub>cw,l</sub>*            = water temperature leaving the evaporator, ˚C

*T<sub>cw,e</sub>*           = water temperature entering the evaporator, ˚C

<span>\({\mathop m\limits^ \bullet_{evap}}\)</span>         = evaporator mass flow rate, kg/s

*C<sub>p,evap</sub>*         = specific heat of water entering evaporator at *T<sub>cw,e</sub>*, J/kg-˚C

The part-load ratio is then calculated as the ratio of the evaporator heat transfer rate to the available chiller capacity. The part-load ratio is not allowed to be greater than the maximum part-load ratio specified by the user or less than zero as follows:

<div>$$PLR = MAX\left( {0.0,MIN\left( {{\raise0.7ex\hbox{${{{\mathop Q\limits^ \bullet  }_{evap}}}$} \!\mathord{\left/ {\vphantom {{{{\mathop Q\limits^ \bullet  }_{evap}}} {{{\mathop Q\limits^ \bullet  }_{avail}}}}}\right.}\!\lower0.7ex\hbox{${{{\mathop Q\limits^ \bullet  }_{avail}}}$}},\,\,PL{R_{\max }}} \right)} \right)$$</div>

where

*PLR*           = part-load ratio

<span>\({\mathop Q\limits^ \bullet_{evap}}\)</span>         = load to be met by the chiller, W

*PLR<sub>max\\        </sub>*= maximum part-load ratio (specified by the user in the input data file)

Note that the maximum part-load ratio (PLR<sub>max</sub>, specified in the Chiller:Electric:ReformulatedEIR object) used in the equation should be less than or equal to the maximum part-load ratio specified in the “Energy Input to Cooling Output Ratio Function of Part-Load Ratio” performance curve object.

The model assumes that the cooling load is met through chiller unloading down to the minimum unloading ratio. False loading (e.g. hot-gas bypass) is assumed to occur between the minimum unloading ratio and the minimum part-load ratio yielding constant electrical power consumption under these conditions. Below the minimum part-load ratio, the chiller cycles on/off to meet very small loads and the power consumption during the on cycle is the same as when the chiller is operating at the minimum part load ratio. When the chiller part-load ratio is less than the minimum part-load ratio, the on-off cycling ratio of the chiller is calculated as follows and is available as an output variable.

<div>$$ChillerCyclingRatio = MIN\left( {{\raise0.7ex\hbox{${PLR}$} \!\mathord{\left/ {\vphantom {{PLR} {PL{R_{\min }}}}}\right.}\!\lower0.7ex\hbox{${PL{R_{\min }}}$}},\,\,1.0} \right)$$</div>

To properly account for chiller electric power consumption when PLR is less than the minimum unloading ratio, the PLR is reset to the greater of the PLR calculated above and the PLR at the minimum unloading ratio. The result is available as the output variable Chiller Part Load Ratio.

<div>$$PLR = MAX(PLR,PL{R_{MinUnloadRatio}})$$</div>

This revised PLR accounts for the “false loading” (e.g., hot-gas bypass) that is assumed to occur whenever the PLR (based on cooling load divided by available capacity) is less than the minimum unloading ratio specified. The amount of false loading on the chiller is calculated using this revised PLR and is reported as an output variable as follows:

<div>$${\mathop Q\limits^ \bullet_{falseloading}} = \left( {{{\mathop Q\limits^ \bullet  }_{avail}}} \right)\left( {PLR} \right)\left( {ChillerCyclingRatio} \right) - {\mathop Q\limits^ \bullet_{evap}}$$</div>

The electrical power consumption for the chiller compressor(s) for any simulation time step is then calculated using the following equation:

<span>\({P_{chiller}}\, = \,\,\left( {{{\mathop Q\limits^ \bullet  }_{avail}}\,} \right)\,\left( {\frac{1}{{CO{P_{ref}}}}} \right)\,\left( {ChillerEIRFTemp} \right)\left( {ChillerEIRFPLR} \right)\left( {ChillerCyclingRatio} \right)\)</span>where

*P<sub>chiller</sub>*         = Chiller compressor power, W

*COP<sub>ref</sub>*       = Reference coefficient of performance, W/W

Heat rejected by the chiller condenser includes the heat transferred in the evaporator plus a portion or all of the compressor electrical energy consumption. For electric chillers with hermetic compressors, all compressor energy consumption is rejected by the condenser (compressor motor efficiency = *eff<sub>motor</sub>* = 1.0). For chillers with semi-hermetic or open compressors, only a portion of the compressor energy use is rejected by the condenser. The heat transfer rate for the chiller condenser is calculated as follows:

<div>$${\mathop Q\limits^ \bullet_{cond}} = \left( {{P_{chiller}} * ef{f_{motor}}} \right) + {\mathop Q\limits^ \bullet_{evap}} + {\mathop Q\limits^ \bullet_{falseloading}}$$</div>

where

<span>\({\mathop Q\limits^ \bullet_{cond}}\)</span>         = condenser heat transfer rate, W

<span>\(ef{f_{motor}}\)</span>       = compressor motor efficiency = fraction of compressor electrical energy consumption rejected as condenser heat

The above curve values are calculated based on the leaving condenser water temperature found through iteration. After obtaining the condenser heat transfer rate, the final leaving condenser water temperature is then calculated as:

<div>$${T_{cond,l}} = {T_{cond,e}} + {\raise0.7ex\hbox{${{{\mathop Q\limits^ \bullet  }_{cond}}}$} \!\mathord{\left/ {\vphantom {{{{\mathop Q\limits^ \bullet  }_{cond}}} {\mathop {\left( {{{\mathop m\limits^ \bullet  }_{cond}}*{C_{p,cond}}} \right)}\limits^{} }}}\right.}\!\lower0.7ex\hbox{${\mathop {\left( {{{\mathop m\limits^ \bullet  }_{cond}}*{C_{p,cond}}} \right)}\limits^{} }$}}$$</div>

where:

*T<sub>cond,l</sub>*          = water temperature leaving the condenser, ˚C

*T<sub>cond,e</sub>*         = water temperature entering the condenser, ˚C

<span>\({\mathop m\limits^ \bullet_{cond}}\)</span>         = mass flow rate through the condenser, kg/s

<span>\({C_{p,cond}}\)</span>       = specific heat of water entering the condenser at *T<sub>cond,e</sub>*, J/kg-˚C

The final calculations determine the total heat transfer energy for the condenser and evaporator, as well as the total electric energy consumed by the chiller compressor motor(s) and condenser fan(s). The results are available as output variables.

<div>$${Q_{cond}} = {\mathop Q\limits^ \bullet_{cond}}*TimeStepSys*3600$$</div>

<div>$${Q_{evap}} = {\mathop Q\limits^ \bullet_{evap}}\,\,*\,\,TimeStepSys\,\,*\,\,3600$$</div>

<div>$${E_{chiller}} = {P_{chiller}}*\,\,TimeStepSys\,\,*\,\,3600$$</div>

<div>$${E_{cond}} = {P_{cond}}\,\, * \,\,TimeStepSys\,\,*\,\,3600$$</div>

where

*Q<sub>cond</sub>*          = chiller condenser heat transfer energy, J

*Q<sub>evap</sub>*           = chiller evaporator cooling energy, J

*E<sub>chiller</sub>*         = chiller (compressor) electric energy, J

*E<sub>cond</sub>*           = chiller condenser fan electric energy, J

*TimeStepSys* = HVAC system simulation time step, hr

<span>\(3600\)</span>         = conversion factor, sec/hr

#### Electric Reformulated EIR Chiller with Heat Recovery Option

Heat from the electric reformulated EIR chiller condenser may be recovered. The heat recovery water flow rate is specified by the user along with the input and output nodes connected to the heat recovery loop. The algorithms are identical to those used for Chiller:Electric and Chiller:Electric:EIR. Refer to the section entitled Chillers with Plant Heat Recovery for details.

#### Standard Rating (Integrated Part Load Value)

Integrated Part Laod Value (IPLV) calculations for Reformulated EIR chiller are similar to what are described above for EIR chillers. The only difference with Reformulated EIR chiller is that it calls an iterative subroutine (SolveRegulaFalsi) to obtain a condenser water outlet temperature which corresponds to condenser inlet temperature at reduced capacity conditions as outlined in Table 51 above. SolveRegulaFalsi is a general utility routine for finding the zero of a function. In this case it finds the condenser inlet temperature that will zero the residual function – the difference between calculated condenser inlet temperature and desired condenser inlet temperature per ANSI/AHRE 550/590, 2011 (table 42 above) divided by desired condenser inlet temperature.

#### References

Hydeman, M., N. Webb, P. Sreedharan, and S. Blanc. 2002. Development and Testing of a Reformulated Regression-Based Electric Chiller Model. ASHRAE Transactions HI-02-18-2.

Hydeman, M. and K.L. Gillespie. 2002. Tools and Techniques to Calibrate Electric Chiller Component Models. ASHRAE Transactions AC-02-9-1.

Hydeman, M., K. Gillespie, and R. Kammerud. 1997. PG&E’s CoolTools project: A toolkit to improve evaluation and operation of chilled water plants. Presented at the Cool$ense National Forum on Integrated Chilled Water Retrofits, Sep. 1997. Berkeley California: Lawrence Berkeley National Laboratory.

### Engine Driven Chiller

The engine driven chiller (Object name: Chiller:EngineDriven) is the empirical model from the Building Loads and System Thermodynamics (BLAST) program. Fitting catalog data to a third order polynomial equations generates the chiller performance curves.  Three sets of coefficients are required to model the open centrifugal chiller as discussed in the section, titled, ‘Electric Chiller Based on BLAST Centrifugal Chiller Model’. Additional curve fits are required to model the engine.  Because the model inherently involves the lower heating value of the fuel, a reference temperature is also involved in the calculations, which manufacturers present at 25°C.

The engine model was also developed for the BLAST program. It was adapted for use in EnergyPlus. This model is used for both the engine driven generator and the engine driven chiller.  It uses the following set of equations all of which are quadratic fits to the PLR (Part Load Ratio) of the generator.  The coefficients must be derived from manufacturers data.

<div>$$\frac{{chiller\,\,cooling\,\,load}}{{fuel\,\,energy\,\,input}} = \frac{{chiller\,\,cooling\,\,load}}{{\left( {{{\dot m}_{fuel}}\left\{ {kg/s} \right\} \cdot LHV\left\{ {J/kg} \right\}} \right)}} = {a_1} + {a_2}PLR + {a_3}PL{R^2}$$</div>

The exhaust gas temp and flow rate are used if a stack heat exchanger is used to recover waste heat from the exhaust.  This temperature is the inlet temperature to the heat exchanger which is modeled in a UA-effectiveness form:

<div>$$\frac{{Total\,Exhaust\,heat}}{{fuel\,energy\,input}} = \frac{{Total\,\,Exhaust\,\,heat}}{{\left( {{{\dot m}_{fuel}}\left\{ {kg/s} \right\} \cdot LHV\left\{ {J/kg} \right\}} \right)}} = {d_1} + {d_2}PLR + {d_3}PL{R^2}$$</div>

<div>$$\frac{{Exhaust\,\,Gas\,\,Temp\,\,\left\{ K \right\}}}{{fuel\,\,energy\,\,input}} = \frac{{Exhaust\,\,Gas\,\,Temp\,\,\left\{ K \right\}}}{{\left( {{{\dot m}_{fuel}}\left\{ {kg/s} \right\} \cdot LHV\left\{ {J/kg} \right\}} \right)}} = {e_1} + {e_2}PLR + {e_3}PL{R^2}$$</div>

The exhaust flow rate is then calculated as:

<div>$${\dot m_{exhaust}} = \frac{{Total\,\,Exhaust\,\,heat}}{{C{p_{exhaust}} \cdot \left( {{T_{exhaust}} - {T_{reference}}} \right)}}$$</div>

where T<sub>reference</sub> is the reference temperature for the fuel lower heating value, and is given as 25°C in manufacturer’s data, and

<div>$${T_{stack}} = {T_{DesignMinExhaust}} + {\frac{{\left( {{T_{exhaust}} - {T_{DesignMinExhaust}}} \right)}}{{{\rm{exp}}\left( {\frac{{{\rm{UA}}}}{{{{{\rm{\dot m}}}_{{\rm{exhaust}}}}C{p_{exhaust}}}}} \right)}}^2}$$</div>

Finally, heat recovered from the lube oil and the water jacket are accounted for as follows:

<div>$$\frac{{Recoverable\,\,jacket\,\,heat}}{{fuel\,\,energy\,\,input}} = \frac{{Recoverable\,\,jacket\,\,heat}}{{\left( {{{\dot m}_{fuel}}\left\{ {kg/s} \right\} \cdot LHV\left\{ {J/kg} \right\}} \right)}} = {b_1} + {b_2}PLR + {b_3}PL{R^2}$$</div>

<div>$$\frac{{Recoverable\,\,lube\,\,oil\,\,heat}}{{fuel\,\,energy\,\,input}} = \frac{{Recoverable\,\,lube\,\,oil\,\,heat}}{{\left( {{{\dot m}_{fuel}}\left\{ {kg/s} \right\} \cdot LHV\left\{ {J/kg} \right\}} \right)}} = {c_1} + {c_2}PLR + {c_3}PL{R^2}$$</div>

#### Chiller Basin Heater

Calculations are also made to estimate the electric power input to the basin heater for chillers with evaporatively-cooled condensers. The chillers which calculate basin heater power are Chiller:Electric:EIR, Chiller:Electric, Chiller:ConstantCOP, Chiller:EngineDriven and Chiller:CombustionTurbine.A schedule may be used to disable the basin heater during regular maintenance periods or other time periods (e.g., during summer). If a schedule is not provided, the basin heater is assumed to be available the entire simulation time period. The basin heater operates when it is scheduled on, the outdoor air dry-bulb temperature is below the basin heater setpoint temperature, and the chiller is not active (i.e., chiller is not operating for the simulation time step --- for example, when there is no cooling load to be met by the chiller, *or if there is no water flow through the chiller due to a chiller or pump availability schedule, etc.*). The user is required to enter a basin heater capacity (watts per degree Kelvin) and a heater setpoint temperature (<sup>o</sup>C) if they want to model basin heater electric power.

<div>$$\begin{array}{l}{P_{heater,basin}} = 0.0\\IF\left( {ChillerIsOFF} \right)THEN\\\,\,\,\,\,\,IF\left( {Schedul{e_{heater,basin}}\,is\,Defined} \right)THEN\\\,\,\,\,\,\,\,\,\,\,\,\,IF\left( {CA{P_{heater,basin}}\, > \,0\,.AND.\,Schedul{e_{heater,basin}}\, = \,ON} \right)THEN\\\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,{P_{heater,basin}} = \,MAX\left( {0.0,CA{P_{heater,basin}}\left( {{T_{setpoint,basin}} - {T_{db,outdoor}}} \right)} \right)\,\,\,\,\,\,\\\,\,\,\,\,\,\,\,\,\,\,ENDIF\\\,\,\,\,\,ELSE\\\,\,\,\,\,\,\,\,\,\,\,IF\left( {CA{P_{heater,basin}}\, > \,0\,} \right)THEN\\\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,{P_{heater,basin}} = \,MAX\left( {0.0,CA{P_{heater,basin}}\left( {{T_{setpoint,basin}} - {T_{db,outdoor}}} \right)} \right)\,\,\,\,\,\,\\\,\,\,\,\,\,\,\,\,\,\,ENDIF\,\\\,\,\,\,\,ENDIF\\ENDIF\end{array}$$</div>

<div>$${E_{heater,basin}} = \left( {{P_{heater,basin}}} \right)\left( {TimeStepSys} \right)3600$$</div>

where:

<span>\({P_{heater,basin}}\)</span> = Chiller basin heater electric power (W)

<span>\({E_{heater,basin}}\)</span>= Chiller basin heater electric consumption (J)

*T<sub>setpoint,basin</sub>* = Basin heater setpoint temperature, user input (<sup>o</sup>C)

*T<sub>db,outdoor</sub>*    = Outdoor air dry-bulb temperature (<sup>o</sup>C)

*CAP<sub>heater,basin</sub>* = Basin heater capacity, user input (W/K)

*Schedule<sub>heater,basin</sub>* = Basin heater schedule, user input (schedule value &gt; 0 means ON)

*ChillerIsOFF =*Logical variable denoting that the chiller is not operating for the current simulation time step (e.g.,  there is no cooling load to be met by the chiller, or if there is no water flow through the chiller due to a chiller or pump availability schedule, etc.)

