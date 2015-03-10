# Electric Load Center Distribution Manager

## Overview

The electric load center distribution manager (object name: ElectricLoadCenter:Distribution) operates on-site generators specified in the simulation and reports the amount of generated and purchased electricity. Electric load centers really serve as a "load" to the generators and "supply" power to the rest of the building. The internal meters used by EnergyPlus for reporting do all of the demand tracking. For more details on the individual inputs required see the Input Output Reference document.

The electric load center manager sums all of the building and system electric loads and provides operating schemes for the generators. The electric load center objects are operated in the order they are defined in the input data file (IDF), and generators are dispatched sequentially in the order they are specified within each load center object. The electricity produced from photovoltaic arrays is handled somewhat separately and is always "used" to reduced the demand that the generators will try to meet for that time step. What is not provided by the on-site generation equipment, and electric storage units if specified, is met by (purchasing) off-site electricity.

The available operating schemes are "Baseload", "DemandLimit", "TrackElectrical," "TrackSchedule," "TrackMeter," "FollowThermal" and "FollowThermalLimitElectrical."  These operating schemes affect how loads are dispatched to the generators, in effect telling the generators whether or not to run and requesting power levels.

The BASELOAD scheme operates the generators at their rated (requested) electric power output when the generator is scheduled ON (ref. ElectricLoadCenter:Generators in the Input Output Reference). The Baseload scheme requests all generators scheduled ON (available) to operate, even if the amount of electric power generated exceeds the total facility electric power demand.

The DEMAND LIMIT scheme limits the amount of purchased electricity from the utility to the amount specified in the input. The Demand Limit scheme tries to have the generators meet all of the demand above the purchased electric limit defined by the user.

The TRACK ELECTRICAL scheme tries to have the generators meet all of the electrical demand for the building.

The TRACK METER scheme tries to have the generators meet all the electrical demand from a meter chosen by the user rather than the usual meter for the entire facility. The meter can be a custom meter so that generators are tied to only certain loads in the building.

The TRACK SCHEDULE scheme tries to have the generators meet all of the electrical demand determined by a user-defined schedule.

The FOLLOW THERMAL and FOLLOW THERMAL LIMIT ELECTRICAL schemes run the generators to meet thermal demand. The thermal demand is determined from the plant modeling and depends on the flow requested by other components on the demand side of the plant loop, the loop temperatures, and the loop temperature setpoint. The electric load center distribution manager converts the thermal load to an electrical load using a nominal ratio of the thermal to electrical power production for each generator. For these schemes, the generator needs to be connected to the supply side of a plant loop and serve components that use hot water on the demand side of the plant loop. The thermal load request is obtained from the plant data structure (structure location in code is PlantLoop%LoopSide%Branch%Comp%MyLoad). The distribution manager converts the thermal load, ![](media/image6965.png) , to an electrical load using:

![](media/image6966.png)\


where,

![](media/image6967.png)  is a nominal, constant, user-defined value for the ratio of thermal production to electrical production for a cogenerator. This ratio is used for supervisory control and dispatch of the electric power request to the generator; however, the cogenerator model may determine that actual performance varies from this nominal value at different times in the simulation when operating conditions differ from those used for the nominal ratio.

For all operating schemes except BASELOAD, a total electric load reduction target (or thermal load converted to electrical equivalent) is established for the load center based on the specific operating scheme. The load center then requests that its generators operate, one-by-one in the order specified, until the target is met or exceeded. Generators that are not scheduled as ‘available' for the simulation time step are not called to operate. The requested power demand to be met by each generator is the **smaller of** the nominal ‘rated' electric power output (as specified in the ElectricLoadCenter:Generators object) or the remaining total electric load reduction target for the load center. After each electric generator is requested to operate, the actual electric power delivered by the generator, which may be greater than or less than the requested amount due to inputs specified in the generator performance model (e.g., Generator:CombustionTurbine, Generator:MicroTurbine, etc.), is used to update the remaining total electric power target for the other generators associated with this load center.

Most of the operating schemes will sequentially load the available electric load centers and generators. EnergyPlus can accept multiple "ElectricLoadCenter:Distribution" objects with different operating schemes. Because of this, there are two levels of reporting, one for the whole building and a second for each load center. The whole-building results are managed with the internal meters for the entire model. The individual load-center results are summed for those generators connected to a particular load center. The total electricity purchased is reported both in power and energy units. This value is positive when the amount of energy is purchased from the utility. This value can be negative when the total electricity produced is greater than the facility electrical needs. The excess will either be available for storage or to sell back to the electric utility company.

The order of input objects (ElectricLoadCenter:Distribution) in the input file is significant and used to structure how generators are dispatched with the first load centers and generators managed before the later ones. Therefore, load centers listed earlier in the file effectively have a higher priority.

Electric load centers can have one of five different configurations. Load centers can get fairly complicated and include power conditioning and storage. Separate inverter models are used to condition DC power from photovoltaics into AC power for the building and utility. Load centers serving PV need to be specified with a direct current buss. The other generators may have inverters inside the devices but these are already embedded in the generator models. The load center can also manage electrical storage (ref. Electrical Storage below).

The most basic configuration is selected with the keyword "Alternating Current" for the Electrical Buss Type, shown in the following diagram.

![Basic Alternating Current Schematic](media/basic-alternating-current-schematic.png)


The *AlternatingCurrent* load centers have AC generators with no storage and behave in the following way. All electric demand not met by the sum of the electrical power produced by the available generators will be met by purchased electricity. If a generator is needed in the simulation for a small load and the load is less than the generator's minimum part load ratio, the generator will operate at the minimum part load ratio and the excess will either reduce demand or the excess energy will be exported back to the electric utility company. The purchased electrical demand limit is the user input for the demand limit above which the generators will try and meet the entire electrical load on the building. It is possible to prescribe a set of ElectricLoadCenter:Distribution objects with inconsistent or conflicting operating schemes, so users need to be careful.

A configuration with AC generators with on-site electrical storage is selected with the keyword "AlternatingCurrentWithStorage" and is shown in the following diagram.

![AC Generators with On-site Electrical Storage Schematic](media/ac-generators-with-on-site-electrical-storage.png)


The *AlternatingCurrentWithStorage* load centers attempt to augment the generator electricity production so that the power requests are met. Storage control logic is discussed below under Electrical Storage.

The basic configuration for photovoltaic generators is selected using the "DirectCurrentWithInverter" keyword and is shown in the following diagram.

![Direct Current With Inverter Photovoltaic Generators Schematic](media/direct-current-with-inverter-photovoltaic.png)


The *DirectCurrentWithInverter* load centers collect DC power from various PV arrays, run the DC power through an inverter and produce AC power. The PV arrays produce DC power based on the availability of sunshine and do not respond to load requests made by the electric load center. The AC output from the inverter is what is recorded as electricity production.

If the PV-based load center is equipped with DC electrical storage that is connected before the inverter, then the buss type should be "DirectCurrentWithInverterDCStorage" and is shown in the following diagram.

![PV based Load Center with DC Electrical Storage Schematic](media/pv-based-load-center-with-dc-electrical.png)


The *DirectCurrentWithInverterDCStorage* load centers charge or draw DC power to meet the requested electrical load.

If the PV-based load center is equipped with AC electrical storage that is connected after the inverter, then the buss type should be "DirectCurrentWithInverterACStorage" and is shown in the following diagram.

![PV Based Load Center with AC Electrical Storage Schematic](media/pv-based-load-center-with-ac-electrical.png)


The *DirectCurrentWithInverterACStorage* load centers charge or draw AC power to meet the requested electrical load.

## Electric Load Center Generators

The electric load center generators (object name: ElectricLoadCenter:Generators) provide a set of scheduled electric generators for electric power generation. Here is where the user lists what generators and PVs are available at any given time. For more details on the individual inputs required see the EnergyPlus Input Output Reference.

## Inverters

EnergyPlus includes three models for converting Direct Current (DC) electrical power into Alternating Current (AC) electrical power. The DC power into the inverter, ![](media/image6973.png) , is converted to AC power out, ![](media/image6974.png) , of the inverter using:

![](media/image6975.png)\


The inverter efficiency is determined using one of the three models. For the"Simple" inveter model, efficiency is constant and input by the user. For the "Look Up Table" model, the efficiency is calculated using linear interpolation. For the "Function of Power" model, the efficiency is calculating using a single-variable curve object. For both the Look Up Table and Function of Power models, the power production is normalized by ![](media/image6976.png) .

The thermal losses are calculated from the difference between ![](media/image6977.png)  and ![](media/image6978.png) .

## Electrical Storage

EnergyPlus includes two models for storing electrical energy: a simple model that is not intended to represent any specific type of storage technology and a battery model that represents the kinetic battery model originally developed by Manwell and McGowan.

The simple model might be called "constrained bucket with energy losses."  The "bucket" holds a quantity of Joules of electrical energy, refered to as the state of charge. There are losses and limits to storing and drawing power but otherwise the bucket just holds electricity. The user sets constraints on the rates of charging,![](media/image6979.png) , and drawing, ![](media/image6980.png) . The user defines efficiencies for charging,![](media/image6981.png) , and drawing, ![](media/image6982.png) . The user defines an initial state of charge and a maximum state of charge.

The storage control algorithms determine a value for the charging power, ![](media/image6983.png) , or the drawing power, ![](media/image6984.png) . The basic storage control algorithm is to compare the requested generator electricity loads to the current available supply and make up the difference with storage. If extra power is generated, then store it. If there is a shortage, then attempt to draw from storage to meet the remaining electricity request. The load center dispatchs a requested electric load for each generator, runs each generator, and then stores the actual power. This power dispatch can be a function of many different things depending on the operating scheme. The sum of the generator load requests,![](media/image6985.png) , is then compared to the sum of the generator production, ![](media/image6986.png)

![](media/image6987.png)\


![](media/image6988.png)\


The limits ![](media/image6989.png)  and ![](media/image6990.png)  are applied.

If charging, the new state of charge, ![](media/image6991.png) , is determined using:

![](media/image6992.png)\


If drawing, the new state of charge is:

![](media/image6993.png)\


Where ![](media/image6994.png)  is the length of the system time step in seconds.

The storage device has an availability schedule. If it is not available then no power can be drawn or stored.

The gross electric power drawn and stored includes losses in the form of heat. These thermal losses are calculated from the user-specified efficiencies for charging and drawing and gross electric power stored and drawn. The thermal (heat) losses are included in a zone heat balance if the user specifies a thermal zone. A user-defined radiative split is used to divide thermal losses into radiation and convection portions. If no zone is specified, then the thermal losses are simply disregarded (e.g., rejected to outdoors and do not impact the zone air heat balance).

## Electrical Storage – Kinetic Battery Model

The Kinetic Battery Model (KiBaM) (object: ElectricLoadCenter:Storage:Battery) was originally developed by Manwell and McGowan (1993) for use in time series performance models of hybrid energy systems. The model is called kinetic because it is based on a chemical kinetics process to simulate the battery charging and discharging behavior. The model, with different improvements and modifications, has been incorporated into the software Hybrid2 and HOMER as the electrical storage module of hybrid and distributed power systems. In 2005, KiBaM was implemented as a stand-alone application in support of the European Union Benchmarking research project (Bindner et al. 2005).

The Kinetic Battery Model assumes that the battery charge is distributed over two tanks: an available-charge tank and a bound-charge tank. The tank for available charges can supply electrons directly to the load, whereas the tank for chemically bound charges can only supply electrons to the available-charge tank. At any time, the total charge ![](media/image6995.png)  in the battery is the sum of the available charge (![](media/image6996.png) ) and bound charge (![](media/image6997.png) ). That is:

![](media/image6998.png)\


Based on the governing equations on the change of charge in both tanks (Manwell and McGowan 1993), the battery capacity can be related to a constant charge/discharge current (![](media/image6999.png) ) as the following equation:

![](media/image7000.png)\


where,

![](media/image7001.png)   : Maximum capacity (Ah) at charge or discharge current I

![](media/image7002.png)        : Maximum capacity (Ah) at infinitesimal current

![](media/image7003.png)             : Charge or discharge time (hr), defined by ![](media/image7004.png)

![](media/image7005.png)            : Constant coefficient (hr^-1^)

![](media/image7006.png)            : Parameter indicating the ratio of available charge capacity to total capacity

Assuming that a constant current is used in any time step for charging and discharging, the available charge (![](media/image7007.png) ) and bound charge (![](media/image7008.png) ) at any time step are given by:

![](media/image7009.png)\


![](media/image7010.png)\


where,

![](media/image7011.png)   :  Available charge at the beginning of time step (Ah)

![](media/image7012.png)  :  Bound charge at the beginning of time step (Ah)

![](media/image7013.png)    :  Total charge at the beginning of time step (Ah), ![](media/image7014.png)

![](media/image7015.png)    :  Length of time step (hr)

KiBaM views the battery as a voltage source in series with an electric resistance (Figure 338). The internal resistance is assumed to be constant and the open circuit voltage varies with current and state of charge.

![Electrical equivalent model for KiBaM](media/electrical-equivalent-model-for-kibam.png)


The battery's open circuit voltage is modeled in the same form for charging and discharging, but with different coefficients. The open circuit voltage in charging (![](media/image7017.png) ) and in discharging (![](media/image7018.png) ) can be respectively expressed as:

![](media/image7019.png)\


![](media/image7020.png)\


where,

![](media/image7021.png)                : Open circuit voltage for a fully charged battery

![](media/image7022.png)                : Open circuit voltage for a fully discharged battery

![](media/image7023.png) , ![](media/image7024.png) , ![](media/image7025.png)    : Constant parameters for charging

![](media/image7026.png) , ![](media/image7027.png) , ![](media/image7028.png)   : Constant parameters for discharging

![](media/image7029.png) , ![](media/image7030.png)         : Normalized maximum capacity at a given charging or discharging current, calculated as:

![](media/image7031.png)\


It needs to be noted that the performance curve (Curve:RectangularHyperbola2) used in the model input covers the 2^nd^ and the 3^rd^ item of the open circuit voltage equation. Due to the reformatting of performance curve, the voltage function regression coefficients can map to the curve coefficients as follows:

![](media/image7032.png)  ;![](media/image7033.png) ;![](media/image7034.png)

With open circuit voltage, the battery terminal voltage (V) can be calculated as:

![](media/image7035.png)\


where, R is the battery internal resistance in Ohms; the current is positive for discharging and negative for charging.

Given desired power in/out of the battery, the desired charge or discharge current can be calculated from the basic power equation: ![](media/image7036.png) . In this calculation, iteration is needed to ensure the electric current has converged and the battery operation satisfies all specified technical constraints such as maximum discharge current and charge rate limit.

KiBaM assumes that battery life is a primary function of charge/discharge cycles. One cycle is defined as the process of starting from a certain state of charge (SOC), the battery is discharged to a lower SOC and then recharged back to the starting SOC. It is regarded that the magnitude of cycle plays more important than the average of SOC during the cycle. This means that in terms of the impact on battery life, the cycle from 90% to 70% and then recharge back to 90% of SOC is equivalent to another cycle from 50% to 30% and then recharge back to 50% of SOC.  Battery life in terms of the number of cycles is predicted as a function of the cycle range measured by the fractional depth of discharge. A double exponential equation is used to capture the damage to batteries due to cycling. The equation takes the following form where the coefficients need to be derived from battery test data via curve fitting.

![](media/image7037.png)\


where,

![](media/image7038.png)     :    Cycles to failure

![](media/image7039.png) -![](media/image7040.png) :    Regression coefficients

R       :    Cycle range in terms of fractional SOC

Following Hybrid2, the rainflow counting method (Downing and Socie 1982) is used to count battery cycles within a state of charge time series. Based on the number of cycles for each fractional SOC range, the battery damage is estimated as:

![](media/image7041.png)\


where,

D    : Fractional battery damage. For example, a value of 0.5 at the end of simulation means that half of the battery life is used up after the length of the simulation period.

![](media/image7042.png) : Number of cycles to failure for the i-th cycle range

![](media/image7043.png)   : Total number of cycles over the simulation with the i-th cycle range

It needs to be noted that the temperature effects on battery performance and battery self-discharge are not supported in the current model.

### References

Bindner H., Cronin T., Lundsager P., Manwell J.F., Abdulwahid U., and Baring-Gould I. 2005. Lifetime Modeling of Lead Acid Batteries. Riso National Laboratory, Roskilde, Denmark.

Downing S. D. and Socie D. F. 1982.  Simple rainflow counting algorithms, International Journal of Fatigue, 1982.

Manwell J. F. and McGowan J. G. 1993. A lead acid battery storage model for hybrid energy systems, Solar Energy 50(5): 399- 405.

## Electric Load Center Transformers

Transformers (object name: ElectricLoadCenter:Transformer) are an integral part of the electric distribution system. They have two broad applications closely related to building energy simulation. First, transformers are used to lower the voltage of electricity from utility primary circuits to customer secondary circuits, and in this case they are called distribution transformers. Second, transformers are used to output the surplus power from onsite generators to the electricity grid.

Distribution transformers reduce the voltage on utility distribution lines (34.5 kV or less) to a lower secondary voltage (600 V or less) suitable for customer equipment. Distribution transformers are usually categorized according to the medium used for cooling and insulation (liquid or air), the voltage class that they serve (low or medium), and the number of phases (single phase or three phase).

Liquid-immersed transformers rely on oil or other fire resistant liquid around the coils for cooling. In contrast, dry type transformers rely only on the natural convection of air for insulation and cooling. Medium-voltage transformers step from utility line voltage down to a lower secondary voltage, depending on the application. The secondary voltage from a medium-voltage transformer is usually at 277 V for single phase and 480 V for three phase. This secondary voltage can be directly used as 480 V three-phase power for large motors or as 277 V single-phase power for some fluorescent lighting. However, for most industrial and commercial facilities, low-voltage transformers are needed to reduce the above voltages further to 208/120 V. Common 120 V loads are wall plugs and incandescent lighting.

Most liquid-immersed transformers are owned by utilities and they are of the medium-voltage type. Virtually all dry type transformers are owned by commercial and industrial customers (Barnes et al. 1996). Of the dry type transformers, those of the medium-voltage type are generally special-order items while those of the low-voltage type are commodity items. The efficiency requirement of distribution transformers is covered by the NEMA (National Electrical Manufactures Association) Standard TP 1. ASHRAE 90.1-2010 will cite the NEMA Standard TP 1 to stipulate the efficiency requirement for the low-voltage dry type distribution transformers.

There are two main types of energy losses in transformers: no load loss and load loss. The no load loss comes primarily from the switching of the magnetic fields in the core material. Hence, it is also called the core loss. The no load (core) loss is roughly constant and exists continuously in the core material as long as the transformer is energized. The load loss comes from the electrical resistance in the windings when there is a load on the transformer. Hence, the load loss is also called the winding loss. The load (winding) loss is proportional to the load squared with a small temperature correction.

Given the no load loss (NL) and the load loss (LL) at rated load and conditions, the total energy losses in a transformer at time t is calculated as:

![](media/image7044.png)\


where,

![](media/image7045.png) Total energy loss at time t (W)

![](media/image7046.png) Load loss at time t (W)

![](media/image7047.png) Per unit load at time t

![](media/image7048.png) Temperature correction factor for the load loss at time t

The per unit load at time t is calculated as:

![](media/image7049.png)\


where,

![](media/image7050.png) Transformer load at time t (W)

![](media/image7051.png) Transformer nameplate rating (VA)

The temperature correction factor at time t is calculated as (NEMA 2002):

![](media/image7052.png)\


where,

![](media/image7053.png) Per unit load loss due to electrical resistance

![](media/image7054.png) Per unit load loss due to eddy currents

![](media/image7055.png) Winding electrical resistance at time t

![](media/image7056.png) Winding electrical resistance at the full load reference conditions

The ratio of winding electrical resistance is calculated as:

![](media/image7057.png)\


where,

![](media/image7058.png) Thermal coefficient of resistance for the winding material (=225 for aluminum and 234.5 for copper)

![](media/image7059.png) Winding temperature rise at the full load reference conditions (°C)

![](media/image7060.png) Winding temperature rise at time t (°C)

![](media/image7061.png) Ambient temperature at the reference condition (=20 °C)

![](media/image7062.png) Ambient temperature at time t (°C)

The Ambient temperature ![](media/image7063.png) is equal to the zone temperature if a thermal zone is specified in the input; otherwise, it is assumed equal to 20 °C. The winding temperature rise at time t is calculated as (Barnes et al. 1997):

![](media/image7064.png)\


Based on the derived total energy losses in a transformer, the transformer efficiency at time t can be calculated according to the following equation:

![](media/image7065.png)\


The above procedure describes how to calculate the total transformer energy losses based on the no load loss and load loss at rated conditions. The transformer model also supports the case when the nominal transformer efficiency is given. In this case, the user needs to provide the nameplate efficiency and the corresponding per unit load, the maximum efficiency and the corresponding per unit load, and the reference conductor temperature at which the nameplate efficiency is measured. Given these information, both no load loss and load loss at rated conditions can be derived as below.

The nameplate efficiency can be expressed as:

![](media/image7066.png)\


where,

![](media/image7067.png) Nameplate efficiency

![](media/image7068.png) Nameplate rating (VA)

![](media/image7069.png) Per unit load at which the nameplate efficiency is measured

![](media/image7070.png) Applied temperature correction factor for the nameplate efficiency

Maximum efficiency generally occurs when the load loss is equal to the no-load loss. Because the no-load loss does not vary with the load on the transformer, the following relationship can be established:

![](media/image7071.png)\


where,

![](media/image7072.png) Per unit load at which the maximum efficiency is obtained

![](media/image7073.png) Applied temperature correction factor for the maximum efficiency

Transformers typically have close per unit loads for the nameplate efficiency and the maximum efficiency. Therefore, it is reasonable to assume that the applied temperature correction factors are equal at those two efficiencies. This implies that:

![](media/image7074.png)\


Rearranging Equation  and combining it with Equation  leads to:

![](media/image7075.png)\


Combining Equations  and , we can obtain the no load loss as:

![](media/image7076.png)\


Substitute NL into Equation , we can calculate the load loss at rated conditions as:

![](media/image7077.png)\


Since both no load and load losses at rated conditions are known, the total energy losses in a transformer at time t can then be calculated according to Equation

### References:

Barnes, PR., JW. Van Dyke, BW. McConnell, and S. Das. 1996. Determination Analysis of Energy Conservation Standards for Distribution Transformer, ORNL-6847. Oak Ridge National Laboratory, Oak Ridge, TN.

Barnes, PR., S. Das, BW. McConnell, and JW. Van Dyke. 1997. Supplement to the "Determination Analysis" (ORNL-6847) and Analysis of the NEMA Efficiency Standard for Distribution Transformer, ORNL-6925. Oak Ridge National Laboratory, Oak Ridge, TN.

NEMA. 2002. NEMA Standards Publication TP 1-2002: Guide for Determining Energy Efficiency for Distribution Transformers. National Electrical Manufactures Association, Rosslyn, VA.