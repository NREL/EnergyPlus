# Water Systems

Water systems include a variety of components that simulate water consumption, production, and storage, including:

Water Use Equipment and Connections

Rainwater Collection

Groundwater Well

Water Storage Tanks

## Water Mains Temperatures

The input object Site:WaterMainsTemperature provides a model for the temperature of water mains that deliver water to a building via underground pipes.  The water mains temperatures are an important input for plant equipment that require make-up water from the water mains, such as for domestic hot water systems.

Water mains temperatures are a function of outdoor climate conditions and vary with time of year.  A correlation has been formulated to predict water mains temperatures based on two weather inputs:

average annual outdoor air temperature (dry-bulb)

maximum difference in monthly average outdoor air temperatures

These values can be easily calculated from annual weather data using a spreadsheet.

The correlation was developed by Craig Christensen and Jay Burch and is described in Hendron et al. (2004).  The correlation is:

T~mains~ = (T~out,avg~ + 6) + ratio \* (ΔT~out,maxdiff~ / 2) \* SIN(0.986 \* (day - 15 - lag) - 90)

where

T~mains~ = water mains temperature (°F)

T~out,avg~ = average annual outdoor air temperature (°F)

ΔT~out,maxdiff~ = maximum difference in monthly average outdoor air temperatures (°F)

day = Julian day of the year (1-365)

ratio = 0.4 + 0.01 \* (Tamb,avg - 44)

lag = 35 - 1.0 \* (Tamb,avg - 44) (°F)

For the Chicago-O'Hare TMY2 weather file, T~out,avg~ = 9.69 C and ΔT~out,maxdiff~ = 28.1 C.  The resulting water mains temperature profile is graphed below.

![](media/image6487.png)\


### References

Hendron, R., Anderson, R., Christensen, C., Eastment, M., and Reeves, P.  2004.  "Development of an Energy Savings Benchmark for All Residential End-Uses", Proceedings of SimBuild 2004, IBPSA-USA National Conference, Boulder, CO, August 4 - 6, 2004.

## Water Use Equipment and Connections

Essential parts of most water systems are the end uses where the water is utilized for a purpose.  These end uses are characterized by familiar pieces of equipment such as sinks, showers, dishwashers, clothes washers, and toilets.  End uses can also include outdoor uses such as landscape irrigation and agricultural irrigation.

Water end-use modeling in EnergyPlus is accomplished via two input objects:

WaterUse:Equipment

WaterUse:Connections

The WaterUse:Equipment object simulates all different types of water end uses.  The WaterUse:Connections object manages multiple pieces of water equipment with a common supply and return, internally providing the services of a splitter and a mixer.  The WaterUse:Connections object also allows water consuming equipment to be simulated in a closed plant loop by supplying makeup water from the water mains.

## Unconnected Water Use Equipment

Common water equipment, such as sinks and showers, requires mixing of hot and cold water to provide a desired temperature at the tap.  Water use equipment that is simulated in "unconnected" mode, i.e., without being referenced by a WaterUse:Connections object, presents a relatively simple modeling problem illustrated by the diagram below:

![Hot and Cold Water Mixing](media/hot-and-cold-water-mixing.png)


The variables are defined as:

![](media/image6489.png) = hot water supply mass flow rate

![](media/image6490.png) = hot water supply temperature

![](media/image6491.png) = cold water supply mass flow rate

![](media/image6492.png) = cold water supply temperature

*![](media/image6493.png)* *= target mass flow rate at the tap*

![](media/image6494.png) = target temperature at the tap

The basic physics of the problem are described by two equations representing conservation of mass and conservation of energy respectively:

![](media/image6495.png)\


![](media/image6496.png)\


The hot and cold water supply temperatures, ![](media/image6497.png)  and ![](media/image6498.png) , are specified by the user with schedules (or indirectly from the water mains temperatures).  The target flow rate and target temperature, ![](media/image6499.png)  and ![](media/image6500.png) , at the tap are also specified with schedules.  The target conditions can be thought of as a way to simulate occupant behavior; a person at a sink will adjust the hot and cold flow rates to get the overall water flow and temperature that is desired.

The two equations above are easily solved to calculate the necessary hot and cold flow rates needed to attain the target conditions.

![](media/image6501.png)\


![](media/image6502.png)\


Even though hot and cold flow rates are unlimited in "unconnected" mode, it is still possible to fail to meet the target conditions if ![](media/image6503.png)  > ![](media/image6504.png) .  In this case, the actual mixed water temperature at the tap, ![](media/image6505.png) , is set equal to ![](media/image6506.png) .  The target flow rate is always met.

Water equipment that omits schedules for the target temperature and/or hot water suppy temperature implies that no hot water is needed.  The result is all cold water at the target flow rate.

For "unconnected" water equipment, the heating rate and energy that is required to supply the hot water is calculated by the following equations.

![](media/image6507.png)\


![](media/image6508.png)\


where

![](media/image6509.png) = heating rate

![](media/image6510.png) = specific heat of water

![](media/image6511.png) = heating energy

![](media/image6512.png) = time step interval

All heating is assigned to "Purchased Heating".

## Zone Heat Gain from Water Use Equipment

Some water use equipment can be a significant source of sensible and latent heat gain when located in a zone.  Showers, for example, add a noticeable latent gain as some of the hot water evaporates into the zone air (and at the same time adding some sensible gain).  Other types of equipment may contribute greater sensible gain but less latent gain.

Calculating the amount of sensible and latent heat gain can be extremely complicated and depends on many variables.  One set of variables is related to the initial conditions of the entering water and the undisturbed zone air, namely:

Entering water temperature

Entering water flow rate

Ambient zone air temperature

Ambient zone humidity

Ambient zone barometric pressure

A second set of variables is tied to the specifics of the water equipment which describe the heat and mass transfer processes that occur as the water passes through the zone.  These variables might include the velocity of the water in the zone air, the surface area of water exposed to the zone air, the dwell time of water in the zone, and possibly others.  The complexity of these variables cannot be easily cast into a generalized EnergyPlus input object.  Consider that in a shower, the velocity of water droplets is increasing as they fall, and the surface area of the spray is increasing.  How to even determine the surface area of the spray at any point?

The approach taken here is to utilize the first set of initial condition variables and forego the overly-complex second set.  The initial conditions can be used to calculate a maximum possible change in sensible and latent energy between the entering water conditions and the leaving water conditions at an infinite time later.  Sensible and latent energy are calculated separately and represent the maximum heat gain, respectively, that could be added to the zone air.  The user simply specifies a fraction of the maximum heat gain for sensible and latent that is actually added to the zone air.  The fraction is specified with a schedule to account for different modes of operation.  The split between sensible and latent will vary depending on the type of equipment that is to be modeled.  Typically, both fractions should be small numbers.

![Zone Heat Gain from Water Use Equipment](media/zone-heat-gain-from-water-use-equipment.png)


The maximum sensible heat gain rate is calculated by assuming that all of the water flow is allowed to cool to the zone dry-bulb temperature.  The actual sensible zone heat gain rate is found by multiplying the maximum by the user fraction.

![](media/image6514.png)\


![](media/image6515.png)\


where

![](media/image6516.png)  = sensible heat gain rate

![](media/image6517.png)  = user fraction of maximum sensible heat

![](media/image6518.png) = target mass flow rate at the tap

![](media/image6519.png) = specific heat of water

![](media/image6520.png) = target temperature at the tap

![](media/image6521.png)  = zone dry-bulb air temperature

![](media/image6522.png) = sensible heat gain

![](media/image6523.png) = time step interval

The maximum latent heat gain rate is calculated by assuming that the maximum amount of water is evaporated into the zone air during the time step.  The actual latent zone heat gain rate is found by multiplying the maximum by the user fraction.

![](media/image6524.png)\


![](media/image6525.png)\


![](media/image6526.png)\


![](media/image6527.png)\


![](media/image6528.png)\


![](media/image6529.png)\


![](media/image6530.png)\


where

![](media/image6531.png) = mass of water that can be absorbed by the zone air in one timestep

![](media/image6532.png) = fully-saturated zone humidity ratio (function of air temperature and pressure)

![](media/image6533.png)  = zone humidity ratio

![](media/image6534.png)  = zone dry-air density (function of air temperature and pressure)

![](media/image6535.png)  = zone air volume

![](media/image6536.png) = mass of water available in the flow in one timestep

![](media/image6537.png) = target mass flow rate at the tap

![](media/image6538.png) = time step interval

![](media/image6539.png) = maximum mass of water that can be evaporated

![](media/image6540.png) = actual mass of water evaporated into the zone air

![](media/image6541.png)  = user fraction of maximum latent heat

![](media/image6542.png) = mass rate of water evaporation into the zone air

![](media/image6543.png)  = latent heat gain rate

![](media/image6544.png) = heat of vaporization for water

![](media/image6545.png) = latent heat gain

Sensible and latent heat gains to the zone are lagged by one zone time step.

Note that this method allows the heat gain to vary dynamically according to changing water and zone conditions instead of the fixed or scheduled value that is traditionally used.  However, sensible and latent fractions may be difficult to estimate.  Experimental measurement is probably the best way to determine the fractions.

A related result of any zone heat and moisture gain is that the water undergoes an equal heat and moisture loss.  For a shower the water will generally leave the zone at a cooler temperature and slightly reduced flow rate when compared to the entering water conditions.

![](media/image6546.png)\


![](media/image6547.png)\


where

![](media/image6548.png) = drainwater mass flow rate at the drain

![](media/image6549.png) = target mass flow rate at the tap

![](media/image6550.png) = mass rate of water evaporation into the zone air

![](media/image6551.png) = water temperature at the drain

![](media/image6552.png) = specific heat of water

![](media/image6553.png) = target temperature at the tap

![](media/image6554.png)  = sensible heat gain rate

![](media/image6555.png)  = latent heat gain rate

The change in leaving water conditions has implications for drainwater heat recovery and storage tank conditions.

## Connected Water Use Equipment

Water use equipment referenced by a WaterUse:Connections object is considered to be in "connected" mode.  The WaterUse:Connections object manages multiple pieces of water equipment with a common supply and return, internally providing the services of a splitter and a mixer.  The connections object also offers three internal configurations for drainwater heat recovery.  External connections are available for coupling to plant loop nodes or water storage tanks for graywater reuse or reclamation.

Keep in mind that water use equipment in "connected" mode does not imply that it is connected to a plant loop.  WaterUse:Equipment objects are never directly connected to a plant loop; they are connected indirectly through the WaterUse:Connections object.  WaterUse:Connections can operate either within a plant loop or in stand-alone mode, i.e., without plant loop connections.

The WaterUse:Connections object overrides the hot and cold water supply temperatures of its subordinate WaterUse:Equipment objects based on the specified schedules, plant loop conditions, or water storage tank conditions.  But the WaterUse:Equipment objects still maintain their individual target flow rates and target temperatures.

Solving the water subsystem described by the WaterUse:Connections object is appreciably more difficult than solving a single, unconnected piece of water use equipment.  The subsystem is illustrated below.

![Water Use Connections Subsystem](media/water-use-connections-subsystem.png)


The nomenclature here uses uppercase letters to indicate variables related to the connections object and lowercase letters to indicate variables related to the equipment objects.  The variables are defined as:

![](media/image6557.png) = hot water supply mass flow rate

![](media/image6558.png) = hot water supply temperature

![](media/image6559.png) = cold water supply mass flow rate

![](media/image6560.png) = cold water supply temperature

![](media/image6561.png) = drainwater mass flow rate at the drain

![](media/image6562.png) = water temperature at the drain

![](media/image6563.png) = wastewater temperature after the drain

![](media/image6564.png) = return water mass flow rate back to the plant loop

![](media/image6565.png) = return temperature of makeup water back to the plant loop

## Water Use Equipment Calculations

The solution method proceeds in steps:

*InitConnectionsTemps*

*CalcConnectionsFlowRates*

*CalcConnectionsDrainTemp*

*UpdateConnectionsNodes*

### Initialize Connections Temperatures

Initializes the hot and cold supply water temperatures, ![](media/image6566.png)  and ![](media/image6567.png) , for the common supply to all the equipment.  If plant-coupled, ![](media/image6568.png)  is taken from the plant loop inlet node.  If stand-alone, ![](media/image6569.png)  is taken from the schedule.

![](media/image6570.png)  is taken from the supply water storage tank, if specified.  Otherwise ![](media/image6571.png)  is taken from the schedule.  If a schedule is not specified, ![](media/image6572.png)  is taken from the water mains.

### Calculate Connections Flow Rates

First calls on each piece of equipment to simulate itself with the given supply water temperatures ![](media/image6573.png)  and ![](media/image6574.png) .  The desired hot and cold flow rates for ![](media/image6575.png)  and ![](media/image6576.png)  are calculated (as described earlier) and passed back to the connections object.  The total flow rates for ![](media/image6577.png)  and ![](media/image6578.png)  are then calculated:

![](media/image6579.png)            ![](media/image6580.png)

If plant-coupled, ![](media/image6581.png)  is compared to the maximum flow rate available in the plant loop (after the first HVAC iteration).  If ![](media/image6582.png)  > ![](media/image6583.png) , the actual flow rate is reset to equal the maximum:  ![](media/image6584.png)  = ![](media/image6585.png) .  New hot and cold flow rates, ![](media/image6586.png)  and ![](media/image6587.png) , at the equipment level are then recalculated, decreasing the hot flow rate and increasing the cold flow rate in order to still meet the target flow rate.

![](media/image6588.png)\


![](media/image6589.png)\


![](media/image6590.png)\


A new mixed water temperature is also recalculated at the equipment level.

Although water storage tanks can also have a maximum flow rate on the cold side, the cold flow rate is not limited in this implementation.

### Calculate Connections Drain Temperature

At this point zone heat gains are calculated for all equipment in zones.  The final drainwater temperatures and flow rates are determined for each piece of equipment by subtracting the lost heat and moisture (see above).  The total drainwater temperature and flow rate for the subsystem are calculated:

![](media/image6591.png)            ![](media/image6592.png)

In the case of no drainwater heat recovery, the subsystem wastewater temperature, ![](media/image6593.png) , is equal to the drainwater temperature, ![](media/image6594.png) .  (For drainwater heat recovery, see below.)  The wastewater temperature and flow rate are propogated to the reclamation water storage tank, if specified.

### Update Connections Nodes

Finally, if plant-coupled, the return water temperature, ![](media/image6595.png) , is passed on to the plant outlet node.  ![](media/image6596.png)  is taken from the cold water supply schedule or the water mains.  The return flow rate is equal to the hot water supply flow rate in order to preserve a closed plant loop:  ![](media/image6597.png)  = ![](media/image6598.png) .

For "connected" water equipment, the heating rate and energy that is required to supply the hot water for individual water equipment is calculated by the following equations.

![](media/image6599.png)\


![](media/image6600.png)\


where

![](media/image6601.png) = heating rate

![](media/image6602.png) = specific heat of water

![](media/image6603.png) = heating energy

![](media/image6604.png) = time interval

The heating rate and energy for the WaterUse:Connections is the sum of the values for its constituent water equipment objects.  If the WaterUse:Connections object is stand-alone, all heating is assigned to "Purchased Heating" by the individual water equipment objects.  If the WaterUse:Connections object is coupled to a plant loop, all heating is supplied by the plant loop equipment.

## Drainwater Heat Recovery

Drainwater heat recovery adds one more complication to the water connections subsystem.  A heat exchanger is added between the drain and the waste outlet to recover some heat from the water before it leaves the system.  The heat is usually recovered by preheating the incoming makeup water from the mains.

The heat exchanger can be modeled as "ideal", "counterflow", or "crossflow".  One common type of drainwater heat exchanger is the gravity-film exchanger (GFX).  The GFX can be approximated with a counterflow heat exchanger.

The destination of the preheated makeup water can be plumbed in three possible configurations:  "plant", "equipment", "plant and equipment".  In the "plant" configuration, all preheated water flow is returned to the plant loop to match the makeup water for the hot water supply flow.  In the "equipment" configuration, all preheated water flow is directed internally within the WaterUse:Connections object to provide the makeup water for the cold water supply flow for the local water use equipment.  In the "plant and equipment" configuration, the preheated water is split between both of the previous configurations.  This is the only configuration where the flow rate is equal on both sides of the heat exchanger.

![Water Use Connections Subsystem with Drainwater Heat Recovery](media/water-use-connections-subsystem-with.png)


The new variables added here are defined as:

![](media/image6606.png) = makeup water mass flow rate in the heat exchanger

![](media/image6607.png) = water temperature leaving the heat exchanger

![](media/image6608.png) = cold makeup water temperature supplied to various flows

Another subroutine is inserted into the solution method:

*InitConnectionsTemps*

*CalcConnectionsFlowRates*

*CalcConnectionsDrainTemp*

**CalcConnectionsHeatRecovery**

*UpdateConnectionsNodes*

For the "equipment" and "plant and equipment" configurations, the solution requires iteration to solve because the preheated water leaving the heat exchanger is connected to the cold water supply water, creating a feedback loop.

### Calculate Connections Heat Recovery

Based on the selected configuration, the flow rate through the heat exchanger is first determined.  For the "plant" configuration, ![](media/image6609.png) .  For the "equipment" configuration, ![](media/image6610.png) .  For the "plant and equipment" configuration, ![](media/image6611.png) .

The heat capacity rates on both sides of the heat exchanger are calculated and the minimum and maximum heat capacity rates, ![](media/image6612.png)  and ![](media/image6613.png) , are determined.

![](media/image6614.png)\


![](media/image6615.png)\


![](media/image6616.png)\


![](media/image6617.png)\


![](media/image6618.png)\


where ![](media/image6619.png)  = the heat capacity ratio.

The effectiveness ![](media/image6620.png)  is then calculated for the given type of heat exchanger.  For the "ideal" heat exchanger, ![](media/image6621.png) .

For the "counterflow" and "crossflow" heat exchangers, the effectiveness is calculated using the effectiveness-NTU method, where:

![](media/image6622.png)\


For the "counterflow" heat exchanger:

![](media/image6623.png) , if ![](media/image6624.png)  = 1

![](media/image6625.png) , if ![](media/image6626.png)  < 1

For the "crossflow" heat exchanger:

![](media/image6627.png)\


The heat recovery rate is then calculated.

![](media/image6628.png)\


The outlet water temperatures can then be calculated for both sides of the heat exchanger.

![](media/image6629.png)\


![](media/image6630.png)\


At each iteration the difference between ![](media/image6631.png)  and ![](media/image6632.png) is compared and checked for convergence.

![](media/image6633.png)\


Once the temperatures have converged, the solution is complete.  ![](media/image6634.png)  is then passed on to a storage tank, if specified.