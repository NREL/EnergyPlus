# Air Path Components

## Overview

The air path from the outlet of an AirLoopHVAC (the supply side of a full air loop / central air system) to the zone air distribution units (terminal units) is specified in a AirLoopHVAC:SupplyPath. The AirLoopHVAC:SupplyPath consists of AirLoopHVAC:ZoneSplitters and AirLoopHVAC:SupplyPlenums hooked together outlets to inlet.

The air path from the zone return nodes to the AirLoopHVAC inlet node is described in a AirLoopHVAC:ReturnPath object. The  AirLoopHVAC:ReturnPath consists of AirLoopHVAC:ZoneMixers and AirLoopHVAC:ReturnPlenums hooked together outlet to inlets.

## Zone Supply Air Path

The AirLoopHVAC:SupplyPaths are simulated just before the zone equipment is simulated and just after the zone equipment is simulated (in module ZoneEquipmentManager, subroutine SimZoneEquipment). In both cases the simulation consists of looping over all the AirLoopHVAC:SupplyPaths and simulating each component (AirLoopHVAC:ZoneSplitter or AirLoopHVAC:SupplyPlenum) in the path. For the "just before" simulation a flag *FirstCall* is set to *True* which tells the zone splitter and zone supply plenum models to do a downstream simulation (pass component inlet conditions to component outlets, except for air mass flow rate). For the "just after" simulation, *FirstCall* = *False*, which tells the components to do an upstream simulation (set the component inlet air mass flow rate to the sum of the component outlet air mass flow rates, do nothing with other properties).

## Zone Splitter

The AirLoopHVAC:ZoneSplitter divides an inlet air stream into multiple outlet streams. Its data and simulation model are encapsulated in the module *SplitterComponent*. There are two simulation modes. If *SimAirLoopSplitter* is called with input parameter *FirstCall* = *True* then the splitter is simulated in downstream mode. The outlet air streams' humidity ratio, pressure, enthalpy, and temperature are set to the conditions on the inlet air stream.  If *SimAirLoopSplitter* is called with input parameter *FirstCall* = *False*, the splitter is simulated in the upstream mode. In this case the inlet air stream air mass flow rate is set to the sum of the outlet air mass flow rates. The same calculation is done for the maximum available and minimum available air flow rates.

## Zone Supply Plenum

The AirLoopHVAC:SupplyPlenum acts analogously to the AirLoopHVAC:ZoneSplitter. The only difference is that the AirLoopHVAC:SupplyPlenum is associated with a Zone for which it sets the supply air flow rate and from which it gets its outlet air conditions. The AirLoopHVAC:SupplyPlenum divides an inlet air stream into multiple outlet streams. Its data and simulation model are encapsulated in the module *ZonePlenum*. There are two simulation modes. If *SimAirZonePlenum* is called with input parameter *FirstCall* = *True* then the plenum is simulated in downstream mode. The outlet air streams' humidity ratio, pressure, enthalpy, and temperature are set to the zone conditions.  If *SimAirZonePlenum* is called with input parameter *FirstCall* = *False*, the plenum is simulated in the upstream mode. In this case the inlet air stream air mass flow rate is set to the sum of the outlet air mass flow rates. The same calculation is done for the maximum available and minimum available air flow rates.

## Zone Return Air Path

The AirLoopHVAC:ReturnPaths are simulated after all the zone equipment is simulated (in module ZoneEquipmentManager, subroutine SimZoneEquipment). The simulation is accomplished by a call to *SimReturnAirPath* (in module *ReturnAirPathManager*). The simulation just consists of looping over all the AirLoopHVAC:ReturnPaths and simulating the components (AirLoopHVAC:ZoneMixer or AirLoopHVAC:ReturnPlenum) in each path.

## Zone Mixer

The AirLoopHVAC:ZoneMixer combines multiple inlet air streams into a single outlet air stream. Its data and simulation model are encapsulated in the module *MixerComponent*. The following mass and energy balance equations are used.

![](media/image1882.png)\


![](media/image1883.png)\


![](media/image1884.png)\


![](media/image1885.png)\


![](media/image1886.png)\


Where ![](media/image1887.png) is air mass flow rate, *W* is humidity ratio, *h* is specific enthalpy, *P* is pressure, and *T* is temperature. *PsyTdbFnHW* is the EnergyPlus psychrometric function for drybulb temperature as a function of enthalpy and humidity ratio. The air mass flow rate calculation is also done for the maximum and minimum available mass flow rates.

## Zone Return Plenum

The AirLoopHVAC:ReturnPlenum acts analogously to the AirLoopHVAC:ZoneMixer. The only difference is that the AirLoopHVAC:ReturnPlenum is associated with a Zone for which it sets the supply air flow rate and from which it gets its outlet air conditions. The AirLoopHVAC:ReturnPlenum combines multiple inlet air streams into a single outlet air stream. Its data and simulation model are encapsulated in the module *ZonePlenum*. The outlet air mass flow rate is obtained from

![](media/image1888.png)\


The outlet air temperature, enthalpy, humidity ratio and pressure are set to the zone conditions.

In addition to its normal function of acting as an air stream mixer, the return plenum can have 2 types of special connection to upstream air terminal units.

(1) The user can model the effects of duct leakage in VAV single duct systems using the Simplified Duct Leakage Model (see ZoneHVAC:AirDistributionUnit for how to set up this model). After the normal outlet air flow calculation is completed as above, the calculation loops over the air distribution units connected to the zones that feed air to the plenum and adds in to the outlet air mass flow rate the leakage air flow rates from the upstream leaks defined in the ZoneHVAC:AirDistributionUnit inputs. This connection between the leaks and the plenum is not explicit: no extra nodes are defined in the return plenum or in the terminal unit.

(2) The user can model secondary (recirculated) air drawn from the plenum into a fan powered induction unit (AirTerminal:SingleDuct:SeriesPIU:Reheat or AirTerminal:SingleDuct:ParallelPIU:Reheat). In this case the connection is explicit: extra outlet nodes are defined in the return plenum which act as the secondary air inlet nodes for the terminal units. The recirculated air flow rates are set by the terminal units. The outlet return air is then:

![](media/image1889.png)\
