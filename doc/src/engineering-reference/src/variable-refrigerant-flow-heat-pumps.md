# Variable Refrigerant Flow Heat Pumps

There are two common types of variable refrigerant flow heat pump systems:

- cooling only or heating only air-conditioning systems (a.k.a. heat pump), or
- heat recovery systems that allow simultaneous cooling and heating

Energyplus models heat pump and heat recovery operating modes as described in the section entitled Variable Refrigerant Flow Heat Pump Model. The variable refrigerant flow model currently supports air-, evaporatively-, or water-cooled condenser equipment. Throughout this section, the term "condenser" refers to the outdoor unit where the compressor is located.

## Variable Refrigerant Flow Heat Pump Model

### Overview

The figure below schematically depicts the AirConditioner:VariableRefrigerantFlow (VRF AC) system. The outdoor unit is connected directly to the zone terminal units using a zone terminal unit list (ref: ZoneTerminalUnitList). The VRF AC system conditions multiple zones and is controlled by thermostats located in each zone. Zone terminal units operate to meet the zone sensible cooling or sensible heating requirements as determined by the zone thermostat schedule.

When the heat pump does not operate to reclaim waste heat, the VRF AC system can only operate in either cooling *or* heating mode. Based on the master thermostat priority control selection, the operating mode is determined by polling the appropriate zone(s) served by the VRF HP system. When the system is operating in cooling mode, the cooling coils will be enabled only in the terminal units where zone cooling is required. When the system is operating in heating mode, the heating coils will be enabled only in the terminal units where zone heating is required. Supply air fans will continue to operate if the zone terminal unit's fan operating mode is set to continuous fan.

When the heat pump does operate to reclaim waste heat, the VRF AC system can simultaneously cool and heat multiple zones. The heat pump will select an operating mode according to the dominant load as reported by the zone thermostat(s). The calculation of the dominant load is based on the master thermostat priority control selection and may either be based on individual zone loads, the number of zones requiring cooling or heating, the master thermostat zone load, or an operating mode schedule. The heat pump will operate in cooling mode, and provide waste heat to zones with a heating load, when the dominant load among all zone terminal units is cooling. The heat pump will operate in heating mode, and absorb heat from zones with a cooling load, when the dominant load among all zone terminal units is heating.

The figure below shows the VRF AC terminal units with draw through fan placement. Blow through fan placement can also be modeled by connecting the supply air fan inlet node to the outside air mixer's mixed air node if an outdoor air mixer is used or to the zone terminal unit inlet node if an outdoor air mixer is not used. The variable refrigerant flow heat pump coordinates the operation of these components and is modeled as a type of zone equipment where the *zone terminal units* are specified in a zone equipment list (Ref. ZoneHVAC: EquipmentList and ZoneHVAC:EquipmentConnections). The AirConditioner: VariableRefrigerantFlow object *is not* specified in an air primary loop or a zone equipment list object.

![Variable Refrigerant Flow Heat Pump (draw through fan placement)](media/variable-refrigerant-flow-heat-pump-draw.jpeg)


![Variable Refrigerant Flow object connections in Energyplus](media/variable-refrigerant-flow-object-connections.jpg)


The terminal unit is able to model supply air fan operation in two modes: cycling fan – cycling coil (i.e., AUTO fan) and continuous fan – cycling coil (i.e., fan ON). Fan:Simple:OnOff must be used to model AUTO fan, while Fan:Simple:OnOff or Fan:Simple:ConstVolume can be used to model fan ON.

Output variables reported by the VRF AC object include the heat pump's operating capacity (cooling or heating), electric consumption, operating COP, defrost electric consumption, part-load ratio (ratio of actual capacity to available capacity), runtime fraction (when cycling), cycling ratio (actual compressor cycling rate), crankcase heater power, and other report variables associated with an evaporative condenser. Report variables are also available to indicate the maximum available terminal unit cooling or heating capacity (i.e., when the condensers available capacity is insufficient to supply the capacity requested by all terminal units). Reporting of other variables of interest for the VRF AC (fan electric power, DX coil cooling rate, DX coil heating rate, terminal unit total cooling rate, etc.) is done by the individual system components (e.g., fan, DX cooling coil, DX heating coil, and zone terminal unit). For convenience, the total cooling and heating coil capacities (i.e., the coil loads) is also reported.

### Model Description

As described previously, the VRF AC system conditions multiple zones and is controlled by one or more zone thermostats (ZoneControl:Thermostatic). Each simulation time step, EnergyPlus performs a zone air heat balance to determine if cooling or heating is required to meet each zone's thermostat set point. When heat pump mode is selected (i.e., heat recovery is not selected), an operating mode is chosen based on the master thermostat priority control and all terminal units are operated in that specific mode. When heat recovery mode is selected, the dominant load is determined based on the master thermostat priority control and the outdoor unit operates in this mode. The indoor terminal units operate in either cooling or heating mode based on the individual coil loads The heat pump condenser is then modeled to determine any impact the condensing unit might have on the zone terminal units (i.e., capacity limitations due to oversized terminal units, operating limitations due to allowable operating temperature range, impacts of defrost operation, changes in performance when heat recovery mode is active, etc.). The following sections describe the performance calculations for cooling and heating.

### Cooling Operation

The operating capacity of the heat pump is calculated based on the user defined input for the heat pump's rated cooling capacity, the ratio of indoor terminal unit capacity to outdoor unit capacity (combination ratio), the actual operating conditions, and whether or not the system's heat recovery mode is active. Performance correction factors are used to correct for off-design performance as follows:

### Cooling Combination Ratio Correction Factor

The cooling combination ratio correction factor is defined as the total terminal unit rated cooling capacity divided by the heat pump's rated cooling capacity. The combination ratio is important when multiple terminal units (or DX coils) are attached to a single variable-speed condensing unit. If the combination ratio is less than 1, the condenser's rated capacity is assumed to be able to meet the indoor terminal unit's demand request. However, if the combination ratio is determined to be greater than 1, the maximum cooling capacity available from the heat pump's condenser may be higher than the user specified rated cooling capacity. The combination ratio capacity correction factor is based on a linear, quadratic or cubic curve and provides a multiplier > 1 to correct for combination ratio's greater than 1. For example, a combination ratio of 1.3 (130%) may result in a combination ratio capacity correction factor of 1.06 (outdoor condenser can actually provide 106% of rated capacity) . The cooling combination ratio correction factor is applied to cooling performance calculations. If the cooling combination ratio correction curve name is not specified by the user, the cooling combination ratio correction factor (CR ~cool~~ing,~~correction~) in the following equation is assumed to be equal to 1.

![](media/image4998.png)\


![](media/image4999.png)\


![](media/image5000.png)\


where

![](media/image5001.png)  = rated total (sensible + latent) cooling capacity in zone *i* (W)

![](media/image5002.png)    = rated total cooling capacity of heat pump (W)

![](media/image5003.png)  = Cooling Combination Ratio capacity correction factor at rated conditions (this value is reported in the eio file)

![](media/image5004.png)          = equation coefficients for cooling combination ratio correction factor

![](media/image5005.png)       = the cooling combination ratio defined as the total indoor terminal unit's rated total cooling capacity divided by the rated total cooling capacity of the heat pump condenser.

The sum of the individual zone total cooling requirements is used to calculate the performance of the outdoor condensing unit. The operating capacities of the indoor cooling coils are calculated based on the indoor cooling coil's rated cooling capacity and the actual operating conditions. The operating capacity of the heat pump condenser is calculated in a similar fashion using a load-weighted average indoor wet-bulb temperature of all operating cooling coils.

### Cooling Capacity Ratio Modifier Function of Low Temperature

The heat pump's cooling capacity correction factor (function of temperature) is determined by averaging the zone cooling coil inlet air wet-bulb temperature for all operating cooling coils. A load-weighted average inlet air wet-bulb temperature is used in the calculations. The weighted average cooling coil inlet air wet-bulb temperature and the outdoor condenser entering air dry-bulb temperature are then used to calculate the temperature correction factor in cooling mode for the heat pump condenser. The actual zone air wet-bulb temperature is used to calculate the operating capacity of the individual zone cooling coils.

![](media/image5006.png)\


![](media/image5007.png)\


The operating capacity of the heat pump is calculated using a bi-quadratic equation using a load-weighted average indoor wet-bulb temperature and outdoor dry-bulb temperature as the independent variables.

![](media/image5008.png)\


The operating capacity of the terminal unit's DX cooling coil is calculated using either a linear, quadratic, or cubic curve. If more information is available to more accurately model the DX cooling coil, a bi-quadratic curve may be used (i.e., if the performance of the DX cooling coil is also a function of outdoor dry-bulb temperature).

![](media/image5009.png)\


 - or -

![](media/image5010.png)\


where

![](media/image3247.png)  = wet-bulb temperature of the air entering the cooling coil in zone *i* (°C)

![](media/image5011.png) = load-weighted average wet-bulb temperature of the air entering all operating cooling coils (°C)

![](media/image5012.png)  = total (sensible + latent) cooling load in zone *i* (W)

![](media/image5013.png)  =total (sensible + latent) cooling load in all zones (W)

![](media/image5014.png)  = heat pump Cooling Capacity Ratio Modifier (function of temperature)

![](media/image5015.png)  = zone coil Cooling Capacity Ratio Modifier (function of temperature)

![](media/image5016.png)   = equation coefficients for Cooling Capacity Ratio Modifier

![](media/image5017.png)     = temperature of the air entering an air-cooled or evaporatively-cooled condenser (°C)

### Using multiple curves to define Cooling Capacity Ratio Modifier

The cooling capacity ratio modifier determines the change in total (sensible + latent) capacity with respect to the heat pump rated cooling capacity. This modifier corrects for off-design performance and provides the operating total (sensible + latent) cooling capacity for the heat pump condenser. The performance data for VRF AC systems may be specified using a single curve object, however, if the performance data does not reflect a smooth change in performance as outdoor conditions vary, the overall performance of the system may be described using two cooling capacity ratio modifier curves (i.e., a separate curve at low and high outdoor temperatures) and a boundary curve (i.e., the curve defining the separation at specific outdoor temperatures).

### Application of Dual Performance Curves

Two additional inputs are available to more accurately model the cooling performance of a VRF AC system. The first additional input is a boundary curve which is used to distinguish differences in performance at "low" and "high" outdoor temperature regions and the second input is a cooling capacity performance curve for the "high" outdoor temperature region. The boundary curve is a linear, quadratic or cubic curve defining the outdoor temperature as a function of average indoor wet-bulb temperature. The cooling capacity ratio function of high temperature curve is a biquadratic curve using average indoor wet-bulb temperature and outdoor dry-bulb temperature as the two independent variables. These additional curves are used to define the performance over two separate performance regions as shown in Figure 244. The red circles in the figure identify the points at which the performance changes abruptly (i.e., slopes or shapes are significantly different) and are used to create the boundary curve. The performance on either side of the boundary curve can be a smooth curve changing with outdoor condition or a flat plateau where system controls maintain a constant operating condition. This type of performance cannot be accurately simulated using a single performance curve object. For this reason, dual capacity performance curves may be used.

> Note: If the cooling performance can be described using a single performance curve object, the boundary and high temperature performance curve objects are not required.

Manufacturers may also provide this information in a tabular format. In this case the data should first be graphically displayed to identify any non-linearities and to also identify where an abrupt change in performance occurs so that a boundary curve can be created.

The cooling capacity ratio boundary curve object is used to differentiate between dual cooling capacity ratio performance curves. This curve defines the outdoor temperature below which the cooling capacity ratio function of low temperature curve object is used, otherwise, the cooling capacity ratio function of high temperature curve object is used.

![](media/image5018.png)\


where

T~OA,DB~ = Outside air dry-bulb temperature (C)

a-d = coefficients for cooling capacity ratio boundary curve object

T~I,WB~ = Weighted average indoor wet-bulb temperature (C)

For the performance data shown below, the boundary curve would be created by identifying the outdoor dry-bulb temperature at which the performance changed abruptly for each of the indoor wet-bulb temperature curves shown in the figure. The following example shows the data interpreted from the figure used to regress the cooling capacity ratio boundary curve coefficients. For this example, the regression coefficients were found to be: A1 = 29.87396, B1 = -0.6928, C1= 0.01928, D1 = -0.000532

Table: Performance Data for Variable Refrigerant Flow Air Conditioner Model

![](media/performance-data-for-variable-refrigerant.png)\


Although the capacity and energy performance curves each have an independent boundary curve input, in this example the same boundary curve may be used for both the capacity and energy input ratio curves. When the "low" and "high" predicted performance data do not line up at the boundary curve points, the boundary curve may have to be modified slightly to allow a smooth transition in performance from the "low" to "high" temperature region. In this case, the boundary curves for capacity and energy may be different.

![Non-Linear Performance of VRF Heat Pump in Cooling Mode](media/non-linear-performance-of-vrf-heat-pump-in.jpeg)


A regression is then performed on the data to the left of the boundary curve to calculate the low temperature cooling performance curve coefficients. A regression is also performed on the data to the right of the boundary curve to create the high temperature cooling performance curve coefficients. The model then uses the boundary curve to determine which performance curve (low or high) to use during the simulation. For example, given a particular average indoor wet-bulb temperature, if the boundary curve object calculates an outdoor dry-bulb temperature that is above the actual outdoor dry-bulb temperature then the cooling capacity ratio function of low temperature performance curve is used to determine AC system performance for that specific simulation time step. When creating the boundary curve, be careful to make sure the low and high performance curves meet, as closely as possible, at the boundary curve points (i.e., that discontinuities do not exist or are minimized to the extent possible). Tabular data (ref: Table:TwoIndependentVariables) may also be used to specify performance and will usually eliminate the need for dual performance curves.

### Determining Cooling Coil Sensible Capacity

The zone terminal unit's cooling coil is controlled by a zone thermostat and must meet a zone "sensible" load. The cooling coil's sensible capacity is defined as the cooling coil's total capacity multiplied by the coil's sensible heat ratio (SHR) at the current operation conditions. Since the SHR of the variable refrigerant flow cooling coil changes as the inlet air wet-bulb temperature and the operating part-load ratio change, an iterative solution technique must be used to determine the coil's sensible capacity. The target solution is found when the sensible capacity of the zone cooling coil equals the zone sensible cooling load (if sufficient capacity is available). The iterative solution converges when the difference (error) between the zone sensible cooling load and the terminal unit sensible cooling capacity is within tolerance (0.001) or the terminal unit has insufficient capacity to meet the zone sensible cooling load. Refer to the description of the VRF cooling coil model for further details (ref: Variable Refrigerant Flow Cooling Coil).

![](media/image5021.png)\


![](media/image5022.png)\


![](media/image5023.png)\


where

![](media/image5024.png) = zone terminal unit total (sensible + latent) cooling capacity, [W], report variable "Zone VRF Air Terminal Total Cooling Rate"

![](media/image5025.png) = zone terminal unit sensible cooling capacity [W], report variable "Zone VRF Air Terminal Sensible Cooling Rate"

![](media/image5026.png) = cooling coil sensible part-load ratio in zone *i*

![](media/image5027.png)  = cooling coil sensible heat ratio (function of PLR, inlet air wet-bulb temperature, and cooling coil inlet air mass flow rate)

![](media/image5028.png)  = cooling coil inlet air mass flow rate [m^3^/s]

The terminal unit total cooling (sensible + latent) capacity is then summed to provide the total terminal unit cooling requirement. The cooling coils capacity includes the impacts of fan heat and any outdoor air provided to the zone.

![](media/image5029.png)\


where

![](media/image5030.png) = total terminal unit cooling requirement (sensible + latent) in all zones, [W]

The piping correction factor is then used to adjust the total zone cooling requirement to account for piping losses in the air conditioner's refrigeration piping system.

### Cooling Piping Correction Factor in cooling mode

The cooling piping correction factor is calculated using either one or two independent variables. Both the equivalent piping length and the combination ratio are used together to determine piping losses, or the equivalent piping length itself is used. The vertical height specified by the user is also added to the result calculated by either. If a single independent variable is used, a linear, quadratic, or cubic equation is used to determine the equivalent length correction factor. If two independent variables are used, a biquadratic equation is used to determine the equivalent length correction factor. The program will automatically use the correct equation based on the performance curve type. In either case, a single coefficient adjusts the piping correction factor based on the difference in height from the highest to lowest terminal unit. Coefficients a-d (e) in the following equation(s) are inputs in the piping correction factor for length in cooling mode performance curve object. Coefficient f is a direct input to the VRF model as the piping correction factor for height in cooling mode coefficient. The equivalent piping length in cooling mode (P~EQ,cool~~ing~) is also a direct input in the VRF model. The cooling combination ratio (CR~cooling~) is automatically calculated by the program. The vertical height, the difference between the highest and lowest terminal unit (e.g., 12 m higher than condenser – 3 m lower than condenser = 9 m height), is a common input for both cooling and heating. When all terminal units are above or below the outdoor unit, an average positive or negative value is used. The limits on the calculated cooling piping correction factor are 0.5 < P~correction,cooling~ < 1.

![](media/image5031.png)\


![](media/image5032.png)\


- or  -

![](media/image5033.png)\


where

![](media/image5034.png)  = Piping Correction Factor in Cooling Mode

*a - f*       = equation coefficients for piping correction factor in cooling mode

*g*              = user specified piping correction factor for height in cooling mode coefficient

![](media/image5035.png)  = user specified equivalent piping length in cooling mode [m]

![](media/image5036.png)  = combination ratio in cooling mode (total rated indoor terminal unit capacity divided by the rated condenser cooling capacity) (reported to eio file)

![](media/image5037.png)            = user specified vertical height used for piping correction factor calculation [m]

An example piping correction factor chart is shown in the following figure. The height selected for use in the equation above is selected to minimize the piping correction factor and serves to identify the worst case piping losses.

![Typical Piping Factor Correction Chart](media/typical-piping-factor-correction-chart.jpeg)


The total demand on the heat pump condenser is then calculated as the quotient of the total terminal unit cooling capacity and the cooling piping correction factor. The piping losses are fixed throughout the simulation (i.e., these losses are not based on which terminal units are requesting refrigerant).

![](media/image5039.png)\


The heat pump's total available cooling capacity is then calculated as:

![](media/image5040.png)\


where

![](media/image5041.png)  = total heat pump condenser cooling load (W)

![](media/image5042.png)  = heat pump total available cooling capacity (W)

### Heat Recovery Cooling Capacity Modifier

When operating in heat recovery mode, the heat pump's available cooling capacity is typically different than the available capacity when operating in cooling only mode. This modifier is used to adjust the available cooling capacity using a fraction when heat recovery is active. This fraction is based on a bi-quadratic equation with indoor and outdoor temperatures used as the independent terms.

*HRCapMod ~HP,cooling~* = ![](media/image5043.png)

This equation can be used to provide a constant fractional difference for available cooling capacity in heat recovery mode (i.e., only *a* is non-zero) or a fractional term that varies with indoor and outdoor conditions. With very limited performance data available at this time, it is recommended that only the constant (a) term be used at this time. When the VRF system is not operating in heat recovery mode, this fraction is set to 1.The available cooling capacity in heat recovery mode is then:

![](media/image5044.png)\


where

![](media/image5045.png)  = heat recovery total available cooling capacity (W)

The figure below shows VRF system laboratory data for cooling only mode (solid characters) and heat recovery mode (dotted characters). Using the limited laboratory data, the available cooling capacity fraction used to model heat recovery mode is approximately 0.91 and the cooling energy fraction is approximately 1.14. This is the only data available at this time to estimate the impact of heat recovery mode on performance. In the bi-quadratic equation, only coefficient a should be used until more complete data sets exist. Laboratory testing will eventually provide more data and better estimates of performance in heat recovery mode.

![Comparison of cooling only and heat recovery mode operation](media/comparison-of-cooling-only-and-heat-recovery.jpg)


### Transition from Cooling Only mode to Heat Recovery mode

When the VRF system transitions from cooling only operation to heat recovery operation, this transition takes some finite amount of time. During the transition period the available cooling capacity can change significantly, The following figure illustrates the transition between cooling only mode and heat recovery mode. For this test, the VRF system was turned on and allowed to reach steady-state operation. Three of the four indoor terminal units were operating in cooling mode, When the fourth terminal unit was enabled in heating mode, the transition from cooling only mode to heat recovery mode took approximately 45 minutes. During this time, the available cooling is significantly reduced and recovers over time. When the system again reaches steady-state operation, the available cooling capacity and power consumption are markedly different. Although computer models do not typically simulate this type of transitional performance, efforts to model this aspect of performance were included in the VRF AC heat recovery model. The initial heat recovery cooling capacity fraction and heat recovery cooling capacity time constant are used to model this transition period. The initial heat recovery cooling capacity fraction identifies the fraction of available heat recovery mode cooling capacity at the start of the transition period, the heat recovery cooling capacity time constant identifies the time needed to recover to 99% of the steady-state value. This exponential model used for simulating the transition period can be turned off by setting the initial heat recovery cooling capacity fraction to 1.

![Laboratory test identifies performance changes during transition period](media/laboratory-test-identifies-performance.jpg)


### Heat Recovery Cooling Capacity Fraction and Time Constant

When the heat pump changes operating modes (i.e., from cooling only to heat recovery mode), the transition does not happen immediately. There is a time delay, and a period of time where the available cooling capacity is well below the steady-state capacity available after the system has had time to adjust. When this type of transition is modeled, an exponential decay curve is used. At the start of the transition period, only a fraction (*k~cool~*) of the steady-state capacity in heat recovery mode is available. The remaining capacity is recovered over a period of 5 time constants (t~c~) and is modeled with the following equation. This equation was used to replicate both the cooling capacity and condenser power curves in the previous figure.

![](media/image5048.png)\


where:

*k~cool~* = fraction of steady-state capacity at beginning of heat recovery mode

*t~c~* = time constant to reach steady-state operation, 5 time constants equals 99% of steady-state capacity (hr)

*Q~HR,avail,cooling~* = available cooling capacity in heat recovery mode (W)

*![](media/image5049.png)* *= The multiplier used during the transition period from cooling mode to heat recovery mode.* This multiplier can be viewed using the report variable "VRF Heat Pump Heat Recovery Status Change Multiplier" (ref: object Output:Variable).

The operating part-load ratio of the VRF system can then be calculated:

![](media/image5050.png)\


![](media/image5051.png)          = heat pump part-load ratio, report variable "VRF Heat Pump Part Load Ratio"

> Note: for calculation purposes ![](media/image5052.png)  is equivalent to ![](media/image5042.png)  when heat recovery mode is not active.

The heat pump total available cooling capacity must be greater than or equal to the total cooling capacity requested by the zone terminal units. When the total operating capacity of all terminal unit's will be greater than the available operating capacity of the heat pump condenser, one or more of the terminal unit's operating capacity must be reduced to the point where the sum of the indoor terminal unit demand request plus piping losses is equal to the total available cooling capacity of the outdoor condenser. At this point, the part-load ratio of the heat pump condenser will be equal to 1.

A maximum terminal unit cooling capacity limit is used to restrict the cooling capacity of each indoor terminal unit. The capacity limit is equivalent to a maximum allowed operating capacity for all zone terminal units. This limit is used to conserve energy between multiple indoor terminal units and a single outdoor condensing unit. Assuming no terminal unit can provide more capacity than can be delivered by the outdoor condenser, the terminal unit total cooling capacity calculation previously discussed is revised as follows:

![](media/image5053.png)\


where:

![](media/image5054.png) = Report variable (ref: Output:Variable) describing the "VRF Heat Pump Maximum Capacity Cooling Rate", W

When multiple terminal units are operating, the terminal units near their maximum capacity are more likely to be capacity limited than those terminal units operating well below their available capacity. The assumption here is that terminal units that are not capacity limited can provide more refrigerant to meet the same load. When the model finds that there is no terminal unit capacity limit, this variable will report 1E+20 indicating that no limit exists.  The figure below demonstrates the application of the capacity limit factor for the case where all indoor terminal units are operating at different capacities. A solution is reached when the sum of the indoor terminal unit's cooling capacities (accounting for piping losses) is equal to the heat pump condenser's available cooling capacity.

![Example of Cooling Capacity Limit](media/example-of-cooling-capacity-limit.jpg)


When the heat pump's part-load ratio is less than 1 (i.e., the total capacity of all terminal unit's is less than the actual operating capacity of the heat pump condenser), the heat pump's part-load ratio is compared to the minimum heat pump part-load ratio. If the heat pump's part-load ratio is less than the minimum heat pump part-load ratio, the heat pump will cycle on and off to meet the cooling load. A correction factor is used to account for startup losses of the compression system.

**Cooling Part-Load Fraction Correlation (function of cycling ratio)**

The cooling part-load fraction correlation (function of heat pump cycling ratio) is a linear, quadratic or cubic curve with the independent variable being cycling ratio (part-load ratio / minimum part-load ratio). The output of this curve is used in combination with the rated EIR and EIR modifier curves to give the "effective" EIR for a given simulation time step.

![](media/image5056.png)\


where

![](media/image5057.png)\


a-c (d) = coefficients for the quadratic (cubic) cycling ratio correlation curve equation

The cycling ratio can be viewed using the report variable "VRF Heat Pump Cycling Ratio" (ref: Output:Variable).

The cooling part-load ratio correlation should be normalized to a value of 1.0 when the cycling ratio equals 1.0 (i.e., no efficiency losses when the heat pump runs continuously [PLR ≥ PLR~min~] for the simulation time step). For cycling ratio values between 0 and 1 (0 <= CyclingRatio < 1), the following rules apply:

CyclingRatioFrac >= 0.7   and   CyclingRatioFrac >= CyclingRatio

If CyclingRatioFrac < 0.7 a warning message is issued, the program resets the CyclingRatioFrac value to 0.7, and the simulation proceeds. The runtime fraction of the heat pump is defined as CyclingRatio/CyclingRatioFrac. If CyclingRatioFrac < CyclingRatio, the runtime fraction will exceed 1. In this case a warning message is issued and the runtime fraction of the heat pump is limited to 1.0.

![](media/image5058.png)\


The heat pump runtime fraction can be viewed using the report variable "VRF Heat Pump Runtime Fraction" (ref: Output:Variable).

Since manufacturers data do not typically provide information defining the cycling losses of VRF AC systems, a typical part-load fraction correlation for a conventional DX refrigeration system (e.g., residential or small commercial unit) may be substituted here as:

CyclingRatioFrac = 0.85 + 0.15(CyclingRatio)

The electrical power consumed by the heat pump condenser is then calculated based on the heat pump's rated electric power consumption (including condenser fan power), the actual operating conditions, and the cycling ratio of the refrigeration system. Performance correction factors are used here to correct for off-design performance as follows:

### Cooling Energy Input Ratio Modifier Function of Low Temperature

As described previously (Ref. Application of Dual Performance Curves), the cooling energy input ratio modifier curve can either be applied as a single curve object as shown below, or applied using dual performance curve objects. The equation form used for a single or dual performance curve application is shown here.

![](media/image5059.png)\


where

![](media/image5060.png)  = cooling energy input ratio modifier (function of temperature)

### Cooling Energy Input Ratio Modifier Function of Part-Load Ratio

The cooling energy input ratio calculation is based on 2 EIR curves. One is used when the heat pump part-load ratio is less than or equal to 1, and a second curve is used to describe the variation of cooling energy input ratio when the part-load ratio is greater than 1. The part-load ratio curve when PLR>1 is not a required input and when not used, the energy use is assumed to be constant when PLR > 1.

![](media/image5061.png)\


where

![](media/image5062.png)  = cooling energy input ratio correction factor (function of part-load ratio)

a-d = coefficient for cooling energy input ratio correlation when part-load ratio ≤ 1

e-h = coefficient for cooling energy input ratio correlation when part-load ratio > 1

The total power consumed by the heat pump condenser in cooling mode is based on the user specified coefficient of performance (COP) and calculated as:

![](media/image5063.png)\


The cooling COP is then calculated as the ratio of the outdoor unit cooling capacity divided by the total electric consumption rate of all components associated with the outdoor unit. Although included in the equiation, defrost power is typically 0 during cooling mode operation.

![](media/image5064.png)\


COP~cooling~        = operating coefficient of performance, report variable "VRF Heat Pump Cooling COP"

CoolingPower = operating electric consumption rate, [W], report variable "VRF Heat Pump Cooling Electric Power"

P~CrankcaseHeater~ = report variable for electric consumption rate of crankcase heater (W)

P~EvapCoolerPump~ = report variable for electric consumption rate of evaporatively-cooled condenser water pump (W)

![](media/image3642.png)  = report variable for average defrost power for the simulation time step (W)

When operating in heat recovery mode, equations similar to those used for available cooling capacity are used to model heating electric consumption rate.

*HREIRMod ~HP,cooling~* = ![](media/image5043.png)

This equation can be used to provide a constant fractional difference for cooling electric consumption rate in heat recovery mode (i.e., only *a* is non-zero) or a fractional term that varies with indoor and outdoor conditions. With very limited performance data available at this time, it is recommended that only the constant (a) term be used at this time. When the VRF system is not operating in heat recovery mode, this fraction is set to 1. The cooling electric consumption rate in heat recovery mode is then:

![](media/image5065.png)\


![](media/image5066.png) where:

*k~EIR, copl~* = fraction of steady-state cooling electric consumption rate at beginning of heat recovery mode

*t~c~~,EIR, cool~* = time constant to reach steady-state operation, 5 time constants equals 99% of steady-state capacity (hr)

*CoolingPower~HR~* = cooling electric consumption rate in heat recovery mode (W)

*![](media/image5049.png)* *= The multiplier used during the transition period from cooling mode to heat recovery mode.* This multiplier can be viewed using the report variable "VRF Heat Pump Heat Recovery Status Change Multiplier" (ref: Output:Variable).

### Heating Operation

Calculations of the heat pump's heating performance is nearly identical to the calculations described above for cooling operation.

The sum of the individual zone total heating requirements are used to calculate the performance of the outdoor condensing unit. The operating capacity of the indoor heating coils are calculated based on the indoor heating coil's nominal heating capacity and the actual operating conditions. A capacity correction factor is used to correct for off-design performance as follows:

**Heating Combination Ratio Correction Factor (function of capacity ratio)**

The heating combination ratio correction factor is defined as the total terminal unit rated heating capacity divided by the heat pump's rated heating capacity. In some instances, the cooling combination ratio may be used to define the correction factor used to modify the heat pump's rated heating capacity and is manufacturer specific. The combination ratio is important when multiple terminal units (or DX coils) are attached to a single variable-speed condensing unit. If the combination ratio is less than 1, the condenser has sufficient capacity to meet the indoor terminal unit's capacity request. However, if the combination ratio is determined to be greater than 1, the maximum heating capacity available from the heat pump's condenser may be higher than the user specified rated heating capacity. The combination ratio capacity correction factor is based on a linear, quadratic or cubic curve and provides a multiplier > 1 to correct for combination ratio's greater than 1. For example, a combination ratio of 1.3 (130%) may result in a combination ratio capacity correction factor of 1.06 (outdoor condenser can actually provide 106% of rated capacity) . The heating combination ratio correction factor is applied to heating performance calculations. If the heating combination ratio correction curve name is not specified by the user, the heating combination ratio correction factor (CR ~heating,~~correction~) in the following equation is assumed to be equal to 1.

![](media/image5067.png)\


![](media/image5068.png)\


![](media/image5069.png)\


where

![](media/image5070.png)  = rated total heating capacity in zone *i* (W)

![](media/image5071.png)  = rated total heating capacity of heat pump (W)

![](media/image5072.png)  = Heating Combination Ratio capacity correction factor at rated conditions

![](media/image5004.png)  = cubic equation coefficients for heating combination ratio correction factor

![](media/image5073.png)  = combination ratio in heating mode (total rated indoor terminal unit capacity divided by the rated condenser heating capacity) (reported to eio file)

### Heating Capacity Ratio Modifier (function of temperature)

As described previously (Ref. Application of Dual Performance Curves), the heating capacity ratio modifier curve can either be applied as a single curve object as shown below, or applied using dual performance curve objects. The single curve object application is described here.

The heat pump's heating capacity ratio modifier (function of temperature) is determined by averaging the zone heating coil inlet air dry-bulb temperature for all operating heating coils. A zone load-weighted average is used when calculating the performance of the heat pump condenser. This weighted-average heating coil inlet air dry-bulb temperature and the outdoor condenser entering air wet-bulb temperature are then used to calculate the heat pump's temperature correction factor in heating mode.

> Note that some manufacturers do not provide performance data as a function of outdoor wet-bulb temperature. In this case, substituting outdoor dry-bulb temperature is permitted and the Heating Performance Curve Outdoor Temperature Type input should be specified as DryBulbTemperature. This also means that performance curve coefficients for both capacity (CAPFT) and energy (EIRFT) should be calculated using indoor and outdoor dry-bulb temperature.

![](media/image5074.png)\


![](media/image5075.png)\


![](media/image5076.png)\


where

![](media/image5077.png) = dry-bulb temperature of the air entering the heating coil in zone *i*, °C

![](media/image5078.png) = weighted-average dry-bulb temperature of the air entering all operating heating coils, °C

![](media/image5079.png)  = Heating Capacity Correction Factor (function of temperature)

![](media/image5016.png)   = bi-quadratic equation coefficients

![](media/image5017.png)     = wet-bulb temperature of the air entering an air-cooled condenser, °C

The total terminal unit heating capacity required is the simple sum of the terminal unit capacity. If the heat pump is off or there is no zone heating requirement, the terminal unit total heating requirement will be zero. The heating coils capacity includes the impacts of fan heat and any outdoor air provided to the zone.

![](media/image5080.png)\


where

![](media/image5081.png) = heat pump's outdoor condenser total zone heating requirement (W)

![](media/image5082.png) = zone terminal unit total heating capacity [W], report variable "Zone VRF Air Terminal Total Heating Rate"

The piping correction factor is then used to adjust the zone heating requirement to account for piping losses in the AC refrigeration system.

### Piping Correction Factor in heating mode

The piping correction factor in heating mode is calculated based on the length of the farthest terminal unit and the difference in height from the highest to lowest terminal unit. The piping losses are fixed throughout the simulation (i.e., these losses are not based on which terminal units are requesting refrigerant). Coefficients a-f in the following equations are inputs in the piping correction factor for length in heating mode curve object. The curve may use either one or two independent variables. Coefficient g is a direct input to the VRF model as piping correction factor for height in heating mode coefficient. The equivalent piping length in heating mode (P~EQ,heating~) is also a direct input in the VRF model. The vertical height (P~H~), the difference between the highest and lowest terminal unit (e.g., 12 m higher than condenser – 3 m lower than condenser = 9 m height), is a common input for both cooling and heating.

![](media/image5083.png)\


- or -

![](media/image5084.png)\


where

![](media/image5085.png)  = Piping Correction Factor in Heating Mode

*a-f* = equation coefficients for piping correction factor in heating mode

*g*   = user specified piping correction factor for height in heating mode coefficient

![](media/image5086.png) = the equivalent piping length for heating specified by the user [m]

![](media/image5037.png)           = user specified vertical height used for piping correction factor [m]

The heat pump's total available heating capacity is then determined using the previously described modifiers.

![](media/image5040.png)\


### Heat Recovery Heating Capacity Modifier

When operating in heat recovery mode, the heat pump's available heating capacity is typically different than the available capacity when operating in heating only mode. This modifier is used to adjust the available heating capacity using a fraction when heat recovery is active. This fraction is based on a bi-quadratic equation with indoor and outdoor temperatures used as the independent terms.

*HRCapMod ~HP,~~heat~~ing~* = ![](media/image5043.png)

This equation can be used to provide a constant fractional difference for available heating capacity in heat recovery mode (i.e., only *a* is non-zero) or a fractional term that varies with indoor and outdoor conditions. With very limited performance data available at this time, it is recommended that only the constant (a) term be used at this time. The available heating capacity in heat recovery mode is then:

![](media/image5087.png)\


### Transition from Heating Only mode to Heat Recovery mode

When the VRF system transitions from heating only operation to heat recovery operation, this transition takes some finite amount of time. During the transition period the available heating capacity can change significantly, As described for the transition from cooling only operating to heat recovery mode, the initial heat recovery heating capacity fraction and heat recovery heating capacity time constant are used to model the transition from heating only mode to heat recovery mode. The initial heat recovery heating capacity fraction identifies the fraction of available heat recovery mode heating capacity at the start of the transition period, the heat recovery heating capacity time constant identifies the time needed to recover to 99% of the steady-state value. This exponential model used for modeling the transition period can be turned off by setting the initial heat recovery heating capacity fraction to 1.0.

### Heat Recovery Heating Capacity Fraction and Time Constant

When the heat pump changes operating modes (i.e., from heating only to heat recovery mode), the transition does not happen immediately. There is a time delay, and a period of time where the available heating capacity is different from the steady-state capacity available after the system has had time to adjust. When this type of transition is modeled, an exponential decay curve is used. At the start of the transition period, only a fraction (*k~heat~*) of the steady-state capacity in heat recovery mode is available. The remaining capacity is recovered over a period of 5 time constants (t~c~~, heat~) and is modeled with the following equation. This equation was used to replicate both the cooling capacity and condenser power curves in the previous figure.

![](media/image5088.png)\


where:

*k~heat~*= fraction of steady-state heating capacity at beginning of heat recovery mode

*t~c~~, heat~* = time constant to reach steady-state operation, 5 time constants equals 99% of steady-state capacity (hr)

*Q~HR,avail,heat~~ing~* = available heating capacity in heat recovery mode (W)

*![](media/image5089.png)* *= The multiplier used during the transition period from heating mode to heat recovery mode.* This multiplier can be viewed using the report variable "VRF Heat Pump Heat Recovery Status Change Multiplier" (ref: Output:Variable).

### Defrost Energy Input Ratio Modifier (function of temperature)

The defrost energy input ratio (EIR) modifier curve (function of temperature) is a bi-quadratic curve with two independent variables: heating coil average entering air wet-bulb temperature and outdoor air dry-bulb temperature. The output of this curve is multiplied by the heating coil capacity, the fractional defrost time period and the runtime fraction of the heating coil to give the defrost power at the specific temperatures at which the coil is operating. The use of outdoor wet-bulb temperature is explicit in this curve object and the defrost equation below and is independent of the selection for Heating Performance Curve Outdoor Temperature Type.

![](media/image5090.png)\


The heat pump condenser's total heating load is then calculated as the quotient of the total terminal unit capacity and the piping correction factor. Additional load due to defrost is also included (see following section).

![](media/image5091.png)\


The heat pump's total (gross) heating capacity is then calculated based on the capacity correction factor as a function of temperatures. The impact of defrost on total heat pump heating capacity is also accounted for (see following section). The part-load ratio of the heat pump condenser can then be calculated.

![](media/image5092.png)\


![](media/image5093.png)\


where

![](media/image5094.png)  = heat pump total available heating capacity (W)

![](media/image5051.png)      = heat pump part-load ratio

![](media/image5095.png) = heating capacity correction factor for defrost mode

As described for cooling operation, the available heating capacity of the heat pump is compared to the requested heating capacity of all indoor terminal units. If the requested heating capacity of all indoor terminal units is greater than the available heating capacity of the heat pump, one or more of the indoor terminal unit's capacity is limited such that the sum of the zone terminal unit heating demand plus piping losses is equal to the available heat pump heating capacity (including the impact due to defrost).

The electrical power consumed by the heat pump condenser is calculated based on the heat pump's nominal electric power consumption in heating mode and the actual operating conditions. A performance correction factor is used to correct for off-design performance as follows:

Heating Part-Load Fraction Correlation (function of heat pump cycling ratio)

The part-load fraction correlation (function of heat pump cycling ratio) is a linear, quadratic or a cubic curve with the independent variable being cycling ratio (part-load ratio / minimum part-load ratio). The output of this curve is used in combination with the rated EIR and EIR modifier curves to give the "effective" EIR for a given simulation time step. The cycling ratio correlation accounts for startup losses of the heat pump's compression system.

![](media/image5096.png)\


or

![](media/image5056.png)\


where

![](media/image5057.png)\


The cycling ratio can be viewed using the report variable "VRF Heat Pump Cycling Ratio" (ref: Output:Variable).

The cycling ratio correlation should be normalized to a value of 1.0 when the cycling ratio equals 1.0 (i.e., no efficiency losses when the compressor(s) run continuously for the simulation time step). For cycling ratio values between 0 and 1 (0 <= CyclingRatio < 1), the following rules apply:

CyclingRatioFrac >= 0.7   and   CyclingRatioFrac >= CyclingRatio

If CyclingRatioFrac < 0.7 a warning message is issued, the program resets the CyclingRatioFrac value to 0.7, and the simulation proceeds. The runtime fraction of the heat pump is defined as CyclingRatio/CyclingRatioFrac. If CyclingRatioFrac < CyclingRatio, then a warning message is issued and the runtime fraction of the heat pump is limited to 1.0.

![](media/image5058.png)\


The heat pump runtime fraction can be viewed using the report variable "VRF Heat Pump Runtime Fraction" (ref: Output:Variable).

Since manufacturers data do not typically provide information defining the cycling losses of VRF AC systems, a typical part-load fraction correlation for a conventional DX refrigeration system (e.g., residential or small commercial unit) may be substituted here as:

CyclingRatioFrac = 0.85 + 0.15(CyclingRatio)

Heating Energy Input Ratio Modifier (function of temperature)

As described previously (Ref. Application of Dual Performance Curves), the heating energy input ratio modifier curve can either be applied as a single curve object as shown below, or applied using dual performance curve objects. The single curve object application is discussed here.

![](media/image5097.png)\


where

![](media/image5098.png)  = heating energy input ratio correction factor (function of temperature) (0-1)

Heating Energy Input Ratio Modifier (function of part-load ratio)

![](media/image5099.png)\


where

![](media/image5100.png)  = heating energy input ratio modifier (function of part-load ratio)

a-d = coefficient for heating energy input ratio modifier when part-load ratio ≤ 1

e-h = coefficient for heating energy input ratio modifier when part-load ratio > 1

The total power consumed by the heat pump condenser in heating mode is then calculated. A correction for power consumed due to defrost is also included (see following section).

The total power consumed by the heat pump condenser in heating mode is based on the user specified coefficient of performance (COP). A correction for power consumed due to defrost is also included (see following section).

![](media/image5101.png)\


The operating COP is then calculated as:

The heating COP is then calculated as the ratio of the outdoor unit heating capacity divided by the total electric consumption rate of all components associated with the outdoor unit. Evaporatively-cooled condenser pump power is typically 0 during heating mode operation.![](media/image5102.png)

COP~heating~        = operating coefficient of performance, report variable "VRF Heat Pump Heating COP"

HeatingPower = operating electric consumption rate, [W], report variable "VRF Heat Pump Heating Electric Power"

When operating in heat recovery mode, equations similar to those used for available heating capacity are used to model operating power.

*HREIRMod ~HP,~~heat~~ing~* = ![](media/image5043.png)

This equation can be used to provide a constant fractional difference for heating electric consumption rate in heat recovery mode (i.e., only *a* is non-zero) or a fractional term that varies with indoor and outdoor conditions. With very limited performance data available at this time, it is recommended that only the constant (a) term be used at this time. When the VRF system is not operating in heat recovery mode, this fraction is set to 1.The available heating electric consumption rate in heat recovery mode is then calculated as:

![](media/image5103.png)\


![](media/image5104.png)\


where:

*k~EIR, heat~* = fraction of steady-state heating electric consumption rate at beginning of heat recovery mode

*t~c~~,EIR, heat~* = time constant to reach steady-state operation, 5 time constants equals 99% of steady-state capacity (hr)

*HeatingPower~HR~* = heating electric consumption rate in heat recovery mode (W)

*![](media/image5049.png)* *= The multiplier used during the transition period from heating mode to heat recovery mode.* This multiplier can be viewed using the report variable "VRF Heat Pump Heat Recovery Status Change Multiplier" (ref: Output:Variable).

### Operating Coefficient of Performance

Similar to the cooling and heating COP report variables, a report variable is included to identify the overall COP of the system. The numerator represents the total cooling and heating coil capacities (or loads) where piping losses have been accounted for. If heat recovery is not used only one of these terms is non-zero. When heat recovery is used, one or both of these terms can be non-zero, therefore, the operating COP includes recovered energy. The denominator includes the electric consumption rates of all system components. For water-cooled VRF AC systems, the plant pump power is not included.

![](media/image5105.png)\


where:

OutdoorUnitPower = Cooling or heating electric consumption rate of outdoor unit (W)

P~TU,fan~ = electric consumption rate of all terminal unit fans (W)

P~TU,parasitic~ = electric consumption rate of all terminal unit parasitic electric (W)

### Defrost Adjustment Factors

Frost formation on the outdoor coil, and the need to periodically defrost this coil, has a significant impact on heating capacity and energy use by the DX heating system. This model uses a timed reverse-cycle defrost model. If the outdoor air dry-bulb temperature is below the specified maximum temperature for defrost operation, then the model calculates adjustment factors for heating capacity and input power due to frost formation This method of accounting for the impacts of frosting/defrost was taken from the model used in DOE-2.1E (ESTSC 2001, Miller and Jaster 1985).

The model first estimates the outdoor coil temperature according to a linear empirical relationship with outdoor air dry-bulb temperature as the independent variable.

![](media/image3626.png)\


The difference between the outdoor air humidity ratio (from the weather file) and the saturated air humidity ratio at the estimated outdoor coil temperature is then calculated, and this value is used as an indication of frost formation on the outdoor coil.

![](media/image3627.png)\


Frost formation on the outdoor coil must be periodically removed. The fraction of compressor runtime when the coil is being defrosted is entered by the user. Adjustment factors to total heating coil capacity and input power due to frost formation on the outdoor coil are calculated by an empirical model with ![](media/image3629.png)  as the independent variable as shown below.

![](media/image5106.png)\


![](media/image5107.png)\


If the outdoor air dry-bulb temperature is above the specified maximum temperature for defrost operation, the fractional defrost time period is set to zero and the heating capacity/input power multipliers are set to unity (1).

### Defrost Operation

If the fractional defrost time period is greater than zero for the simulation time step, then the model calculates the electrical power used during defrost. The additional heating load due to defrost (indoor cooling during defrost) is also calculated so that it may be added to the existing heating load when calculating input power for the compressor(s) and outdoor coil fan(s).

![](media/image3636.png)\


![](media/image3637.png)\


where:

![](media/image3640.png)  = additional indoor heating load due to reverse-cycle defrost (*W*)

![](media/image3641.png) = total full-load heating capacity of the coil at rated conditions (W)

![](media/image3642.png)  = report variable for average defrost power for the simulation time step (W)

*DefrostEIRTempModFac* = energy input ratio modifier curve applicable during defrost

![](media/image5108.png) = defrost time period fraction specified by user

![](media/image3644.png)\


## Zone Terminal Unit List

The zone terminal unit list identifies the terminal units that are connected to a single variable refrigerant flow heat pump. The zone terminal unit list is used exclusively in the variable refrigerant flow (VRF) heat pump object (ref: AirConditioner:VariableRefrigerantFlow) and VRF zone terminal units (ref: ZoneHVAC: TerminalUnit:VariableRefrigerantFlow). Up to 20 terminal units may be connected to a single VRF outdoor condensing unit. This list is extensible if additional indoor terminal units are required. The following figure shows the connection scheme between the zone terminal units, the zone terminal unit list, and finally the VRF AC system. The zone terminal units are connected to the zone through zone inlet and outlet zone nodes. Each zone terminal unit is entered in a list which represents all terminal units connected to a single VRF AC system. And finally, the zone terminal unit list name is entered in the corresponding VRF AC object.

![Zone Terminal List connections in EnergyPlus objects](media/zone-terminal-list-connections-in-energyplus.jpg)
