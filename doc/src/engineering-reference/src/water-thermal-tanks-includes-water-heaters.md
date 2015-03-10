# Water Thermal Tanks (includes Water Heaters)

Water thermal tanks are devices for storing thermal energy in water.  The most common types are water heaters. devices for storing and heating water.  Typical water heater applications are for domestic hot water heating, low-temperature radiant space heating, and energy storage for solar hot water systems or waste heat recovery.  In EnergyPlus, water heater objects can be coupled to a plant loop simulation or used stand-alone.  There are also chilled water storage tanks that can be used to hold cold water

## Mixed Water Thermal Tank

The input object WaterHeater:Mixed provides a model that simulates a well-mixed water tank, i.e. non-stratified, and is appropriate for simulating many types of water heaters and storage tanks, including gas and electric residential water heaters, a variety of large commercial water heaters, and also instantaneous, tankless water heaters.  This model is used for both the mixed water heater and the mixed chilled water storage tanks.

### Energy Balance

The well-mixed assumption implies that all water in the tank is at the same temperature.  To calculate the water temperature, the model analytically solves the differential equation governing the energy balance of the water tank:

![](media/image6391.png)\


where

** = density of water

*V* = volume of the tank

*c~p~* = specific heat of water

*T* = temperature of the tank water

*t* = time

*q~net~* = net heat transfer rate to the tank water

The density and volume can be replaced with the total mass *m* of water in the tank to get:

![](media/image6392.png)\


The net heat transfer rate *q~net~* is the sum of gains and losses due to multiple heat transfer pathways.

![](media/image6393.png)\


where

*q~heater~* = heat added by the heating element or burner

*q~oncycpara~* = heat added due to on-cycle parasitic loads (zero when off)

*q~offcycpara~* = heat added due to off-cycle parasitic loads (zero when on)

*q~oncycloss~* = heat transfer to/from the ambient environment (zero when off)

*q~offcycloss~* = heat transfer to/from the ambient environment (zero when on)

*q~use~* = heat transfer to/from the use side plant connections

*q~source~* = heat transfer to/from the source side plant connections

*q~oncycloss~* and *q~offcycloss~* are defined as:

![](media/image6394.png)\


![](media/image6395.png)\


where

*UA~oncyc~* = on-cycle loss coefficient to ambient environment (zero when off)

*UA~offcyc~* = off-cycle loss coefficient to ambient environment (zero when on)

*T~amb~* = temperature of ambient environment

*q~use~*, and *q~source~* are defined as:

![](media/image6396.png)\


![](media/image6397.png)\


where

*~use~* = heat exchanger effectiveness for the use side plant connections

![](media/image6398.png) = mass flow rate for the use side plant connections

*T~use~* = inlet fluid temperature of the use side plant connections

*~source~* = heat exchanger effectiveness for the source side plant connections

![](media/image6399.png) = mass flow rate for the source side plant connections

*T~source~* = inlet fluid temperature of the use side plant connections

Incorporating all of these equations into the original differential equation,

![](media/image6400.png)\


Associating terms not dependent on temperature *T* and terms dependent on temperature *T* yields:

![](media/image6401.png)\


The differential equation now has the form

![](media/image6402.png)\


where

![](media/image6403.png)\


![](media/image6404.png)\


The solution to the differential equation can be written in terms of *a* and *b* as:

![](media/image6405.png)\


where

*T(t)* = temperature of the tank water at time *t*

*T~i~* = initial temperature of the tank water at time *t* = 0

However, if *b* = 0, the solution instead is:

![](media/image6406.png)\


Since the control algorithm must sometimes calculate the time needed to reach a specified temperature, the equations above can also be rearranged to solve for *t*.

![](media/image6407.png)\


or, if *b* = 0,

![](media/image6408.png)\


where

*T~f~* = final temperature of the tank water at time t.

In the special case where *b* = 0 and *a* = 0, and *T~f~* <> *T~i~*, the time *t* is infinity.

### Water Heater Control Algorithm

For water heaters, control options allow the heater to cycle or modulate to meet the load.  When cycling, the heater element or burner is either on or off.  The heater remains fully on while heating the tank up to the setpoint temperature.  When the setpoint is reached, the heater turns off.  The heater remains off until the tank temperature falls below the "cut-in" temperature, i.e., the setpoint temperature minus the deadband temperature difference.  The heater continuously cycles on and off to maintain the tank temperature within the deadband.  Most storage-tank water heaters cycle.

When modulating, the heater power varies between the maximum and minimum heater capacities.  The heater stays on as long as the required total demand is above the minimum capacity.  Below the minimum capacity, the heater will begin to cycle on and off based on the deadband temperature difference.  Most tankless/instantaneous water heaters modulate.

Within a time step, the differential equation is solved separately for when the heater element or burner is "on" (on-cycle) and when it is "off" (off-cycle).  This approach allows ambient losses and parasitic loads to be divided into on-cycle and off-cycle effects and accounted for in detail.

An illustration of how the control algorithm cycles on and off is shown below.  Ambient losses cool the tank temperature until the bottom of the deadband is reached (50 C) at which point the heater cycles on and reheats the tank back to the setpoint (60 C).  A water draw causes hot water to be replaced with cold water from the water mains.  The incoming cold water rapidly cools the tank.  In this example the heater cannot keep up with the water draw and the tank temperature continues to drop until the water draw ends.

Although the instantaneous tank water temperature may vary considerably within a timestep (due to cycling, etc.), only the average temperature over the timestep is reported.  The model calculates the average by piece-wise integration of the area under the instantaneous temperature curve for each unique set of conditions.  The instantaneous temperature is preserved internally by the program and is propogated from the end of one timestep to the beginning of the next.

![Water Heater Cycle Control Algorithm](media/water-heater-cycle-control-algorithm.png)


### Chilled Water Tank Control Algorithm

The input objects ThermalStorage:ChilledWater:Mixed and ThermalStorage:ChilledWater:Stratified provide chilled water tank models that do not include active cooling elements, there is only indirect cooling by remote devices such as a chiller.  The tank's setpoint controls are used to determine if flow is to be requested through the source side of the tank.  The setpont and deadband control scheme is similar to the water heater but the logic is flipped around for cooling instead of heating.  The setpoint temperatue is the "cut-out" temperature and the setpoint plus deadband is the "cut-in" temperature.  If the tank temperature ( or tank sensing node for stratified tanks) is above the "cut-in" temperature, then flow is requested.  If temperatures are below the "cut-out" temperature, then flow is not requested.  The chilled water tanks also have separate availability schedules for the use side and source side for additional control options.

### Standard Ratings

For water heaters, the industry standard ratings of Recovery Efficiency and Energy Factor are calculated according to the 10CFR430 test procedure.  To emulate the test procedure, a 24-hour simulation of the water heater is performed internally using the specified test conditions:

- Setpoint Temperature = 57.2 C (135 F)
- Ambient Temperature = 19.7 C (67.5 F)
- Ambient Relative Humidity = 50% (used for heat pump water heaters)
- Inlet Temperature (Water Mains) = 14.4 C (58 F)

For heat pump water heaters, the water heater tank's heating element and water heater tank parasitic loads are disabled and the user-defined water heating capacity, energy use, and parasitic loads for the heat pump are used to calculate Recovery Efficiency and Energy Factor.

The simulated test procedure performs six equal draws of approximately 0.041 m^3^ (10.7 gal) in each of the first six hours of the simulation.  Each draw occurs over the first timestep of the hour.

The Recovery Efficiency is calculated when the water heater recovers to the setpoint after the first draw.

![](media/image6410.png)\


where

*m~1~* = water mass of the first draw

*c~p~* = specific heat of water

*E~1~* = fuel energy consumed until the setpoint is recovered (including parasitics)

> Note: When the standards rating for a heat pump water heater is calculated, the fuel energy consumed refers to the total energy consumed by the heat pump compressor, evaporator fan, condenser pump, and parasitic loads. It is assumed that the parasitic loads for a heat pump water heater do not contribute to heating the water (ref. Heat Pump Water Heater).

The Energy Factor is calculated at the end of the 24-hour simulation period.

![](media/image6411.png)\


where

*m~total~* = total water mass of all six draws

*c~p~* = specific heat of water

*E~total~* = total fuel energy consumed over 24 hours (including parasitics)

Under certain input parameters, the rating method will not succeed and a warning message will be generated.  Problems occur when inputs do not allow the tank to recover to the setpoint temperature within the test period.  This can occur if the maximum heater capacity is undersized, or if the deadband temperature difference is large enough that the first draw of the test does not trigger the heater to come on.  In either case, the Recovery Efficiency test will not compute properly because recovery to the setpoint was not achieved.

### References

10CFR430.  *Title 10, Code of Federal Regulations, Part 430 - Energy Conservation Program for Consumer Products, Appendix E to Subpart B - Uniform Test Procedure for Measuring the Energy Consumption of Water Heaters*.

## Heat Pump Water Heater

### Overview

The input object WaterHeater:HeatPump provides a model for a heat pump water heater (HPWH) that is a compound object consisting of a water heater tank (e.g., WaterHeater:Mixed or WaterHeater:Stratified), a direct expansion (DX) "coil" (i.e., an air-to-water DX compression system which includes a water heating coil, air coil, compressor, and water pump), and a fan to provide air flow across the air coil associated with the DX compression system. These objects work together to model a system which heats water using zone air, outdoor air, or a combination of zone and outdoor air as the primary heat source.

Numerous configurations of tank location, inlet air source, and DX coil compressor location can be modeled. The DX coil compressor may be located in a zone, outdoors, or the ambient temperature surrounding the compressor may be scheduled. The location of the compressor controls the operation of its crankcase heater. The water heater tank location is specified in the water heater tank object and is independent of the compressor location. In addition, the inlet air configuration may be specified in one of several ways. The heat pump water heater air coil and fan assembly may draw its inlet air from the zone and outdoor air using an optional mixer and splitter assembly as shown in the first figure below. When used, the mixer and splitter air streams are controlled by a single inlet air mixer schedule. When the HPWH draws its inlet air solely from a zone, the mixer/splitter assembly is not required as shown in the second figure below. In this case, the inlet air to the evaporator and fan assembly is made up entirely of zone air and the heat pump outlet air is directed back to the zone. The final figure illustrates a HPWH that draws its inlet air solely from outdoors and exhausts its outlet air outdoors as well. Each of these configurations may also be connected to a plant hot water loop (via the water heater tank use nodes).

![Schematic of a heat pump water heater using optional mixer/splitter nodes](media/schematic-of-a-heat-pump-water-heater-using.jpeg)


![Schematic of a Heat Pump Water Heater with Inlet Air from a Zone](media/schematic-of-a-heat-pump-water-heater-with.jpeg)


![Schematic of a Heat Pump Water Heater with Inlet Air from Outdoors](media/schematic-of-a-heat-pump-water-heater-with-001.jpeg)


> Note: The water heater tank location shown in the figures above is completely independent of the heat pump water heater's inlet air configuration and its compressor location. The water heater tank may be located outdoors, in a zone, or the ambient temperature surrounding the tank can be scheduled as described in the mixed water heater section below.

### Model Description

The heat pump water heater input requires a compressor setpoint temperature schedule and dead band temperature difference, which are independent from the setpoint temperature schedule and dead band temperature difference for the heater (element or burner) associated with the water heater tank. The cut-in temperature for the heat pump compressor is defined as the heat pump compressor's setpoint temperature minus its dead band temperature difference.

![](media/image6414.png)\


where:

![](media/image6415.png) = Cut-in temperature for the heat pump compressor (°C)

![](media/image6416.png) = Heat pump compressor setpoint temperature (°C)

![](media/image6417.png) = Heat pump compressor dead band temperature difference (°C)

In this model, the heat pump water heater's DX compression system is considered the primary heat source and the water tank's heater (element or burner) provides supplemental heat as necessary. Therefore, the cut-in temperature for the heat pump compressor (setpoint minus dead band temperature difference) is usually higher than the setpoint temperature for the heater (element or burner) in the associated water heater tank object. At times when the water heater tank setpoint temperature is greater than the cut-in temperature of the heat pump compressor, the heat pump compressor is disabled and the tank's heater is used to heat the water.

The simulation starts by first calculating the air conditions entering the air coil (evaporator)/fan assembly based on the inlet air configuration of the heat pump water heater and the presence of the optional mixer/splitter nodes. When the HPWH draws its inlet air from the zone and outdoors using the optional mixer/splitter nodes (i.e., Inlet Air Configuration = Zone and Outdoor Air), the inlet air conditions are calculated as follows:

![](media/image6418.png)\


![](media/image6419.png)\


![](media/image6420.png)\


where:

![](media/image6421.png)  = current value of the inlet air mixer schedule (fraction of outdoor air, 0-1)

![](media/image6422.png)  = inlet air dry-bulb temperature to the HPWH evaporator/fan assembly (°C)

![](media/image6423.png)  = outdoor air dry-bulb temperature (°C)

![](media/image6424.png)   = zone (exhaust) air dry-bulb temperature (°C)

![](media/image6425.png)  = inlet air humidity ratio to the HPWH evaporator/fan assembly (kg/kg)

![](media/image6426.png) = outdoor air humidity ratio (kg/kg)

![](media/image6427.png)   = zone (exhaust) air humidity ratio (kg/kg)

When the heat pump water heater draws its inlet air solely from the zone (i.e., Inlet Air Configuration = Zone Air Only), the inlet air conditions to the evaporator/fan assembly are simply set equal to the zone (exhaust) air conditions. If the heat pump water heater draws its inlet air solely from outdoors (i.e., Inlet Air Configuration = Outdoor Air Only), the inlet air conditions to the evaporator/fan assembly are simply set equal to the outdoor air conditions. When the inlet air to the heat pump water heater evaporator and fan assembly is scheduled (i.e., Inlet Air Configuration = Schedule), the inlet air conditions are determined directly from the user-supplied schedules as follows.

![](media/image6428.png)\


![](media/image6429.png)\


![](media/image6430.png)\


where:

![](media/image6431.png) = inlet air relative humidity to heat pump water heater evaporator/fan assembly (0-1)

![](media/image6432.png) = psychrometric function returning air humidity ratio given dry-bulb temperature, relative humidity, and barometric pressure

![](media/image6433.png) = outdoor barometric pressure (Pa)

For each simulation time step, the heat pump water heating capacity, energy use, and air-side/water-side mass flow rates are set to zero and the water heater tank is simulated with the heat pump compressor disabled when any of the following conditions apply:

the HPWH is scheduled off by its availability schedule,

the water heater tank setpoint temperature is greater than or equal to the heat pump compressor cut-in temperature,

the inlet air dry-bulb temperature to the evaporator/fan assembly is less than the Minimum Inlet Air Temperature for Heat Pump Compressor Operation (as specified by the user in the HPWH input object), or

the HPWH setpoint temperature is greater than or equal to the Maximum Temperature Limit (specified in the Water Heater:Mixed object).

Otherwise, simulation of the heat pump water heater is based on its current mode of operation. This mode of operation is either floating (heat pump compressor is off and tank water temperature has not fallen below the heat pump compressor cut-in temperature) or heating (tank water temperature dropped below the compressor cut-in temperature on a previous time step but was unable to reach the compressor setpoint temperature). Each mode is handled differently and they will be discussed separately.

If the heat pump water heater is using the stratified tank model, then there is more than one value for the tank temperature.  The model includes input for where the heat pump controls detect the temperature in the form of six options for keyword choices:  Heater1, Heater2, SourceInlet, SourceOutlet, UseInlet, and UseOutlet.  The input data in the associated WaterHeater:Stratified includes the heights of these locations and the nearest stratified tank node is identified based on these heights.  When the heat pump model needs to evaluate the tank temperature of a stratified tank, it evaluates the temperature at the tank node associated with these locations.

### Float Mode

When the heat pump water heater tank temperature is floating between the heat pump compressor's cut-in and cut-out temperatures at the end of the previous simulation time step, both the heat pump compressor and the water heater tank's heating element are disabled and a resulting tank temperature is calculated. If the resulting tank temperature is below the heat pump compressor's cut-in temperature, the heat pump compressor part-load ratio is estimated using the ratio of the temperature differences shown below. The part-load ratio can not be less than zero or greater than one.

![](media/image6434.png)\


where:

![](media/image6435.png) = part-load ratio of the heat pump water heater compressor

![](media/image6436.png) = tank temperature in float mode when heating capacity is set to zero(°C)

![](media/image6437.png) = tank temperature at the beginning of the simulation time step (°C)

Since the pump and fan are assumed to cycle on and off with the heat pump compressor, the average condenser water and evaporator air mass flow rates for the simulation time step are calculated based on the PLR calculated above:

![](media/image6438.png)\


![](media/image6439.png)\


where:

![](media/image6440.png) = average condenser water mass flow rate for the time step (kg/s)

![](media/image6441.png) = condenser water volumetric flow rate, user input (m^3^/s)

![](media/image6442.png) = density of condenser inlet water (kg/m^3^)

![](media/image6443.png) = average evaporator/fan air mass flow rate for the time step (kg/s)

![](media/image6444.png) = evaporator/fan air volumetric flow rate, user input (m^3^/s)

![](media/image6445.png) = density of evaporator/fan inlet air (kg/m^3^)

The water tank temperature is then calculated based on heat pump operation at the part-load ratio estimated above and with the water tank's heating element enabled. If the resulting water tank temperature is above the heat pump compressor's setpoint (cut-out) temperature, then the part-load ratio is reduced and the water heater tank is simulated again. The process is performed iteratively until the part-load ratio of the heat pump compressor achieves the desired setpoint temperature (to the extent possible).

### Heating Mode

When the HPWH is in heating mode at the end of the previous simulation time step (i.e., the heat pump compressor operated during the previous simulation time step but was unable to achieve the setpoint temperature), both the heat pump compressor and the water heater tank's heating element are enabled. The part-load ratio of the heat pump compressor is set to 1, and the condenser water and evaporator air mass flow rates are set to their maximum flow rates.

![](media/image6446.png)\


![](media/image6447.png)\


If the resulting tank temperature is above the heat pump compressor's setpoint (cut-out) temperature, the part-load ratio of the heat pump compressor is reduced and the water heater tank is simulated again. The process is performed iteratively until the part-load ratio of the heat pump compressor achieves the desired setpoint temperature (to the extent possible).

The air-side outlet conditions of the HPWH are calculated through simulation of the fan and DX coil with either a blow through or draw through fan placement (user selectable). If mixer/splitter nodes are used, the HPWH model splits the heat pump outlet air mass flow rate with exhaust air flow equaling the outdoor air flow, and the balance of the outlet air being sent to the zone supply air node (i.e., ensures that the heat pump water heater does not contribute to zone pressurization or depressurization). Calculations of heat pump water heating capacity, energy use, air-side performance, and water-side temperature difference are performed in the associated DX Coil object. See the engineering reference section for the Coil:WaterHeating:AirToWaterHeatPump object for details.

### Model Outputs

After completing the float mode or heating mode calculations and the final part-load ratio has been determined, the output (report) variables are calculated as follows:

![](media/image6448.png)\


![](media/image6449.png)\


![](media/image6450.png) ![](media/image6451.png) ![](media/image6452.png)

where:

![](media/image6453.png) = on-cycle parasitic electric load, user input (W)

![](media/image6454.png) = off-cycle parasitic electric load, user input (W)

![](media/image6455.png) = HVAC system simulation time step (hours)

> Note: All heat pump water heater output variables, including off-cycle ancillary electric power and consumption, equal 0 when the heat pump water heater availability schedule equals 0 (i.e., the heat pump water heater is scheduled OFF).

## Stratified Water Thermal Tank

The input objects WaterHeater:Stratified and ThermalStorage:ChilledWater:Stratified provide models for a stratified water thermal tank that divides the water tank into multiple nodes of equal volume.  This model is used for both the stratified water heater and the stratified chilled water storage tank. The nodes are coupled by vertical conduction effects, internode fluid flow, and temperature inversion mixing.  The object simultaneously solves the differential equations governing the energy balances on the nodes using the Forward-Euler numerical method.  The system time step is divided into one second substeps that allow the simulation to capture events that occur on a very short time scale.

### Energy Balance

Similar to the well-mixed model, the stratified model solves the same fundamental differential equation governing the energy balance on a mass of water:

![](media/image6456.png)\


where

*m* = mass of water

*c~p~* = specific heat of water

*T* = temperature of water

*t* = time

*q~net~* = net heat transfer rate

The difference for the stratified model is that it must solve the energy balance on *n* number of nodes simultaneously.  Node 1 is at the top of the water tank and node *n* is at the bottom of the water tank.

![](media/image6457.png)\


where

*m~n~* = mass of water for node *n*

*c~p~* = specific heat of water

*T~n~* = temperature of water for node *n*

*t* = time

*q~net,n~* = net heat transfer rate for node *n*

The net heat transfer rate *q~net~* is the sum of gains and losses due to multiple heat transfer pathways.

![](media/image6458.png)\


where

*q~heater,n~* = heat added by Heater 1 or Heater 2

*q~oncycpara,n~* = heat added due to on-cycle parasitic loads (zero when off)

*q~offcycpara,n~* = heat added due to off-cycle parasitic loads (zero when on)

*q~oncycloss,n~* = heat transfer to/from the ambient environment (zero when off)

*q~offcycloss,n~* = heat transfer to/from the ambient environment (zero when on)

*q~cond,n~* = heat transfer due to conduction between the node above and below

*q~use,n~* = heat transfer to/from the use side plant connections

*q~source,n~* = heat transfer to/from the source side plant connections

*q~flow,n~* = heat transfer due to fluid flow from the node above and below

*q~invmix,n~* = heat transfer due to inversion mixing from the node above and below

*q~oncycloss,n~* and *q~offcycloss,n~* are defined as:

![](media/image6459.png)\


![](media/image6460.png)\


where

*UA~oncyc,n~* = on-cycle loss coefficient to ambient environment (zero when off)

*UA~offcyc,n~* = off-cycle loss coefficient to ambient environment (zero when on)

*T~amb~* = temperature of ambient environment

*q~cond,n~* is defined as:

![](media/image6461.png)\


where

*k* = fluid thermal conductivity of water, 0.6 W/m-K

*A~n+1~* = shared surface area between node *n* and node *n+1*

*L~n+1~* = distance between the center of mass of node *n* and n*+1*

*T~n+1~* = temperature of node *n+1*

*A~n-1~* = shared surface area between node *n* and node *n-1*

*L~n-1~* = distance between the center of mass of node *n* and n*-1*

*T~n-1~* = temperature of node *n-1*

*q~use,n~*, and *q~source,n~* are defined as:

![](media/image6396.png)\


![](media/image6397.png)\


where

*~use~* = heat exchanger effectiveness for the use side plant connections

![](media/image6398.png) = mass flow rate for the use side plant connections

*T~use~* = inlet fluid temperature of the use side plant connections

*~source~* = heat exchanger effectiveness for the source side plant connections

![](media/image6399.png) = mass flow rate for the source side plant connections

*T~source~* = inlet fluid temperature of the source side plant connections

*q~flow,n~* is defined as:

![](media/image6462.png)\


where

![](media/image6463.png) = mass flow rate from node *n+1*

*![](media/image6464.png)* *= mass flow rate from node n-1*

*q~invmix,n~* is defined as:

![](media/image6465.png)\


where

![](media/image6466.png) = mass flow rate from node *n+1* due to temperature inversion mixing

![](media/image6467.png) = mass flow rate from node *n-1* due to temperature inversion mixing

Inversion mixing occurs when the node below is warmer than the node above.  The difference in temperatures drives a difference in density that causes the nodes to mix.  Usually inversion mixing occurs very rapidly.  In this algorithm, the inversion mixing rate is selected to be the maximum value that will provide a stable solution given the node mass and the substep interval:

![](media/image6468.png)\


where

*t* = the substep time interval.

The use and source fluid steam outlet temperatures calculation procedure depends on the values of the effectiveness.  If the effectiveness is 1.0, then complete mixing of these fluid steam and the tank water is assumed.  In this case the outlet temperatures for the use and source streams will be simply the tank water temperatures at point of the outlet nodes. When the effectiveness is less than 1.0, an indirect heat exchange is assumed between the use or source stream and the water in the stratified thermal storage tank.  When the effectiveness is less than 1.0, the use and source outlet temperatures are calculated using *Q~use~* and *Q~source~*, and energy balance equations as follows:

![](media/image6469.png)\


![](media/image6470.png)\


 where,

*T~use out~*~~= outlet fluid temperature of the use side plant connections

*T~source out~*~~= outlet fluid temperature of the source side plant connections

### Numerical Solution

The system of simultaneous differential equations is solved using the Forward-Euler numerical method.  The system time step is divided into one-second substeps.  The new temperature for a given node is calculated using the following equation:

![](media/image6471.png)\


All node temperatures for *q~net,n~* are the old temperatures from the previous substep.

Before each system time step is calculated the following evaluations are made:

Use and source inlet flow rates are applied to the inlet nodes

Internode flow is determined and net flow rates are determined

Before each substep is calculated, the following evaluations are made:

Thermostatic controls for heater 1 and heater 2 are evaluated to determine if the heater elements should turn on or off

Node 1 temperature is compared against the maximum limit to determine if venting is necessary

Adjoining node temperatures are compared to determine if there are any temperature inversions for which the inversion mixing rate should be used.

The solution continues looping through all substeps until the system time step is completed.

### References

Duffie, J., and W. Beckman. 1980. *Solar Engineering of Thermal Processes*.  John Wiley & Sons.

Newton, B. 1995. *Modeling of Solar Storage Tanks*.  Master's Dissertation, University of Wisconsin-Madison.

## Water Heating Sizing

Some inputs for water heaters can be autosized using the input object WaterHeater:Sizing.  This section describes the sizing calculations for water heaters.  There are six general methods for sizing tank volume and heating capacity.

### Autosizing Tank Volume

The volume a water heater can be sized in the following ways depending on the design method chosen by the user.

Peak Draw.  The volume is determined from the loop design flow rate.  The water heater is positioned on the supply side of a plant loop.  After the plant sizing routines have run, the model  obtains the design flow rate for all components on the demand side.  The tank volume is then: ![](media/image6472.png) ,

Residential HUD-FHA Minimum.  The volume is determined from a set of rules defined by the table below.  This is from Chapter 48 of 1999 ASHRAE Handbook HVAC Applications, Americal Society of Heating Refrigeration and Air-conditioning Engineeers, Atlanta GA.  (also used in the Building America Benchmark).

Table: Residential HUD-FHA Minimum

---------------------------------------------------------------------------
Residential HUD-FHA Minimum DHW Storage and Burner Capacity (ASHRAE 1999) 
**# Bedrooms**|**1**|**2**|**3**|**4**|**5**|**6**
**# Bathrooms**|**All**|**≤1.5**|**2-2.5**|**≥3**|**≤1.5**|**2-2.5**|**≥3**|**≤1.5**|**2-2.5**|**≥3**|**All**|**All**
Gas||||||||||||
   Storage (gal)|20|30|30|40|30|40|40|40|40|50|50|50
   Burner (kBtu/hr)|27|36|36|36|36|36|38|36|38|38|47|50
Electric||||||||||||
   Storage (gal)|20|30|40|50|40|50|50|50|50|66|66|80
   Burner (kW)|2.5|3.5|4.5|5.5|4.5|5.5|5.5|5.5|5.5|5.5|5.5|5.5

Per Person.  The tank volume is determined by summing the design level of people in the model and multiplying by a user-entered volume per person factor.

Per Floor Area.  The tank volume is determined by summing the floor area in all the zones in the model and multiplying by a user-entered volume per floor area factor.

Per Unit.  The tank volume is determined by multiplying a user-entered volume per unit and a user-entered number of units.

Per Solar Collector Area.  The tank volume is determined by summing the collector area in all the hot water solar collectors in the model and multiplying by a user-entered volume per collector area factor.

### Autosizing Heater Capacity

The heater capacity can be sized in the following ways depending on the design method chosen by the user.

Peak Draw.  The heater capacity is determined from the tank volume, assumed start and finish temperatures and a user defined time for recovery.  The heater capacity is then

![](media/image6474.png)\


where,

![](media/image6475.png)\


![](media/image6476.png)\


Residential HUD-FHA Minimum.  The heater capacity is determined from a set of rules defined by the table above.  This is from 1999 ASHRAE Handbook HVAC Applications, Americal Society of Heating Refrigeration and Air-conditioning Engineeers, Atlanta GA.  (also used the Building America Benchmark).

Per Person.  The heater capacity is determined by summing the design level of people in the model and using a user-entered factor for recovery capacity per person.  The heater capacity is then:

![](media/image6477.png)\


Per Floor Area.  The heater capacity is determined by summing the floor area in all the zones in the model and using a user-entered factor for recovery capacity per floor area.  The heater capacity is then:

![](media/image6478.png)\


Per Unit.  The heater capacity is determined from a user-entered Recovery Capacity per unit and a user-entered number of units.  The heater capacity is then:

![](media/image6479.png)\


Per Solar Collector Area. The water heater is assumed to be used for solar hot water storage and the heater capacity is set to zero.

### Autosizing Tank Height

If the water heater is Stratified, then the geometry is important and the height of the tank can be scaled with changes in volume.  For tank shapes of Vertical Cylinder, the user defined height aspect ratio, *AR*,  is used to calculate a tank height, *H*, using

![](media/image6480.png)\


### Autosizing Plant Connection Flow Rates

When the water thermal tank is connected to a plant loop, it is convient to autosize the design volume flow rates through the plant connections.  When the water thermal tank is connected to the supply side of plant loop and flow rates are autosized, the flow rate is the sum of the flow requests of all the various components on the demand side of that plant loop.  When the water thermal tank is connected on the demand side of a plant loop (e.g. as for indirect water heating with a boiler) and flow rates are autosized, the design flow rates are calculated with the following equation,

![](media/image6481.png)\


where

*V* = volume of the tank

![](media/image6482.png)  = User parameter for the time it takes for the tank to recover from assumed starting temperature to an assumed setpoint temperature.  For water heaters, the starting temperature is 14.4ºC and the final assumed setpoint temperature is 57.2ºC.  For chilled water tanks, the starting temperature is 14.4 ºC and the final temperature is 9.0ºC.

![](media/image6483.png)  = *~use~*  or *~source~*

![](media/image6484.png) = the exit temperature specified in the Plant Sizing object

![](media/image6485.png)  =  the final tank temperature of 57.2ºC for heaters and 9.0ºC for chilled water tanks.

![](media/image6486.png)  = the initial tank temperature of 14.4ºC

If the demand side connections are autosized and a water heater's tank volume is autosized, then the problem cannot be readily solved in EnergyPlus because the demand side connection flows need to be reported earlier in the simulation and the tank volume is not yet available.  This situation is resolved by using an interim, nominal tank volume for sizing the connections and the actual volume is calculated later in the simulation.
