# **Cooling Towers** and Evaporative Fluid Coolers

## One, Two, and Variable Speed Cooling Towers and Evaporative Fluid Coolers

### Overview

The input objects CoolingTower:SingleSpeed, CoolingTower:TwoSpeed, and CoolingTower:VariableSpeed:Merkel provide models for single-speed, two-speed, and variable-speed cooling towers that are based on Merkel's theory (Merkel 1925), which is also the basis for the tower model included in ASHRAE's HVAC1 Toolkit for primary HVAC system energy calculations (ASHRAE 1999, Bourdouxhe et al. 1994). Cooling tower performance is modeled using effectiveness-NTU relationships for counterflow heat exchangers. The model can be used to simulate the performance of both single speed, two speed, and variable speed mechanical-draft cooling towers. The model will also account for tower performance in the "free convection" regime, when the tower fan is off but the water pump remains on. For part-load operation, the model assumes a simple linear interpolation between two steady-state regimes without accounting for any cycling losses.

For single speed cooling towers, the capacity control can be fan cycling or fluid bypass. In fluid bypass mode, portion of the water goes through the tower media and gets cooled while the remaining water flow gets bypassed, two water flows then mix together trying to meet the tower exiting water setpoint temperature. In both the free convection cooling when fan is off and normal cooling when fan is on for the entire time step, if the tower exiting water temperature is lower than the setpoint, the tower operates in fluid bypass mode. The model determines the fluid bypass fraction by iterations until the mixed water meets the tower exiting water temperature setpoint. In the fluid bypass mode, except the free convection, the tower fan runs at full speed for the entire time step. The maximum amount of tower water that can be bypassed is bounded by the freezing point of the tower water – the tower exiting water temperature cannot be lower than the freezing setpoint.

Evaporative fluid coolers are modeled very similar to cooling towers. The main difference between the two is in the "Performance input method" input field. Cooling tower has two choices for this field namely "UFactorTimesAreaAndDesignWaterFlowRate" and "Nominal capacity". The nominal capacity is specified for the standard conditions i.e. entering water at 35C (95F), leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb temperature and 35C (95F) dry-bulb temperature. On the other hand evaporative fluid cooler has three choices for "Performance input method" which are "UFactorTimesAreaAndDesignWaterFlowRate", "StandardDesignCapacity" and "UserSpecifiedDesignCapacity". First method is same for both tower and fluid cooler. Standard design capacity is specified for the same conditions which are used to specify nominal capacity for tower as described above. If the capacity of fluid cooler for conditions other than the standard ones is known then UserSpecifiedDesignCapacity method should be used. In this case, the conditions for which the fluid cooler capacity is known i.e. entering water temperature, entering air temperature and entering air wet bulb temperature must be specified in the input. To calculate evaporation loss for fluid cooler, spray water flow rate which is different than the process fluid flow rate must be specified for all the performance input methods. This is not required for cooling tower because cooled fluid i.e. water is in direct contact with the air so the water loss is calculated by using cooled fluid flow rate only. Unlike cooling tower, evaporative fluid cooler model does not account for free convection.

Cooling tower model is described below which holds equally good for evaporative fluid cooler. The differences are mentioned whenever required.

### Model Description

Based on Merkel's theory, the steady-state total heat transfer between the air and water entering the tower can be defined by the following equation:

![](media/image4279.png)\


where

*h~s~* = enthalpy of saturated air at the wetted-surface temperature, J/kg

*h~a~* = enthalpy of air in the free stream, J/kg

*c~p~* = specific heat of moist air, J/kg-^o^C

*U* = cooling tower overall heat transfer coefficient, W/m^2^-^o^C

*A* = heat transfer surface area, m^2^

Equation  is based on several assumptions:

- air and water vapor behave as ideal gases
- the effect of water evaporation is neglected
- fan heat is neglected
- the interfacial air film is assumed to be saturated
- the Lewis number is equal to 1

In this model, it is also assumed that the moist air enthalpy is solely a function of the wet-bulb temperature and that the moist air can be treated as an equivalent ideal gas with its mean specific heat defined by the following equation:

![](media/image4280.png)\


where

Δ*h* = enthalpy difference between the air entering and leaving the tower, J/kg

Δ*T~wb~* = wet-bulb temperature difference between the air entering and leaving the tower, ^o^C

Since the liquid side conductance is much greater than the gas side conductance, the wetted-surface temperature is assumed to be equal to the water temperature. Based on this assumption and equations  and , the expression for total heat transfer becomes:

![](media/image4281.png)\


where

![](media/image4282.png)\


*T~wb~* = wet-bulb temperature of the air, ^o^C

*T~w~* = temperature of the water, ^o^C

An energy balance on the water and air sides of the air/water interface yields the following equations:

![](media/image4283.png)\


![](media/image4284.png)\


where

![](media/image4285.png)  = mass flow rate of water, kg/s

![](media/image4286.png)  = mass flow rate of air, kg/s

Assuming that the heat capacity rate (![](media/image4287.png) ) for the cooling tower water is less than that for the air, the effectiveness of the cooling tower can be defined by analogy to the effectiveness of a simple heat exchanger:

![](media/image4288.png)\


where

*ε* = heat exchanger effectiveness

*T~win~* = inlet water temperature, ^o^C

*T~wout~* = outlet water temperature, ^o^C

*T~wbin~* = wet-bulb temperature of the inlet air, ^o^C

Combining equations , , and  and integrating over the entire heat transfer surface area, and combining the result with equation  provides the following expression for cooling tower effectiveness:

![](media/image4289.png)\


where

![](media/image4290.png)  and ![](media/image4291.png)

![](media/image4292.png)\


This equation is identical to the expression for effectiveness of an indirect contact (i.e., fluids separated by a solid wall) counterflow heat exchanger (Incropera and DeWitt 1981). Therefore, the cooling tower can be modeled, in the steady-state regime, by an equivalent counterflow heat exchanger as shown in the following figure.

![Cooling Tower Schematic](media/cooling-tower-schematic.png)


The first fluid is water and the second fluid is an equivalent fluid entering the heat exchanger at temperature T~wbin~ and specific heat ![](media/image4294.png) . The heat exchanger is characterized by a single parameter, its overall heat transfer coefficient-area product UA~e~. The actual cooling tower heat transfer coefficient-area product is related to UA~e~ by the following expression:

![](media/image4295.png)\


This heat transfer coefficient-area product is assumed to be a function of the air mass flow rate only and can be estimated from laboratory test results or manufacturers' catalog data.

The model for the variable speed Merkel tower also includes Scheier's modifications.  Scheier has extended the Merkel model to also include terms that adjust UA with three factors that model how UA values change when the tower is operating away from its rated conditions.  The first factor, ![](media/image4296.png) , adjusts UA for the current outdoor wetbulb temperature.  The user enters a performance curve or lookup table that is a function of one independent variable.  The independent variable is the difference between the design wetbulb temperature and the current wetbulb temperature, in degrees Celsius.

![](media/image4297.png)\


The second factor, ![](media/image4298.png) , adjusts UA for the current air flow rate.  The user enters a performance curve or lookup table that is a function of one independent variable.  The independent variable is the ratio of the current air flow rate to the design air flow rate at full speed.

![](media/image4299.png)\


The third factor, ![](media/image4300.png) , adjusts UA for the current water flow rate.  The user enters a performance curve or lookup table that is a function of one independent variable.  The independent variable is the ratio of the current water flow rate to the design water flow rate.

![](media/image4301.png)\


Then the UA value at any given time is calculated using

![](media/image4302.png)\


### Method for Calculating Steady-State Exiting Water Temperature

The objective of the cooling tower model is to predict the exiting water temperature and the fan power required to meet the exiting water setpoint temperature. Since only the inlet air and inlet water temperatures are known at any simulation time step, an iterative procedure is required to determine the exiting fluid temperatures using the equations defined in the previous section. In the case of the EnergyPlus model, the iterations are performed to determine the exiting wet-bulb temperature of the air. The exiting water temperature is then calculated based on an energy balance that assumes that the energy absorbed by the air is equivalent to the energy removed from the water. The procedure for calculating the steady-state, exiting air wet-bulb temperature is outlined below.

As explained previously, it is assumed that the moist air enthalpy can be defined by the wet-bulb temperature alone. Therefore, the first step in the procedure is to calculate the enthalpy of moist air entering the cooling tower based on the ambient wet-bulb temperature from the weather file. Since an iterative solution is required, a first guess of the outlet air wet-bulb temperature is then made and the enthalpy of this estimated outlet air wet-bulb temperature is calculated. Based on these inlet and outlet air conditions, the mean specific heat of the air is calculated based on equation , repeated here:

![](media/image4303.png)\


With the overall heat transfer coefficient-area product for the cooling tower entered by the user, the effective heat transfer coefficient-area product is calculated by rearranging equation :

![](media/image4304.png)\


With ![](media/image4305.png)  and ![](media/image4306.png)  known, the effectiveness of the heat exchanger is then calculated:

![](media/image4307.png)\


where

![](media/image4308.png)  and ![](media/image4309.png)

![](media/image4310.png)  and ![](media/image4311.png)

![](media/image4312.png)\


The heat transfer rate is then calculated as follows:

![](media/image4313.png)\


The outlet air wet-bulb temperature is then recalculated:

![](media/image4314.png)\


The iterative process of calculating ![](media/image4315.png)  continues until convergence is reached.

Finally, the outlet water temperature is calculated as follows:

![](media/image4316.png)\


### Calculating the Actual Exiting Water Temperature and Fan Power

The previous section describes the methodology used for calculating the steady-state temperature of the water leaving the cooling tower. This methodology is used to calculate the exiting water temperature in the free convection regime (water pump on, tower fan off) and with the tower fan operating (including low and high fan speed for the two-speed tower). The exiting water temperature calculations use the fluid flow rates (water and air) and the UA-values entered by the user for each regime.

The cooling tower model seeks to maintain the temperature of the water exiting the cooling tower at (or below) a setpoint. The model obtains the target temperature setpoint from the setpoints placed on either the tower outlet node or the loop's overall setpoint node (typically set to the supply side outlet node). The model checks to see if the outlet node has a setpoint placed on it and uses that if it does.  If the outlet node does not have a temperature setpoint then the model uses the loop-level outlet node specified in the input field called Loop Temperature Setpoint Node Name in the PlantLoop or CondenserLoop object. The model first checks to determine the impact of "free convection", if specified by the user, on the tower exiting water temperature. If free convection is not specified by the user, then the exiting water temperature is initially set equal to the entering tower water temperature. If the user specifies "free convection" and the steady-state exiting water temperature based on "free convection" is at or below the setpoint, then the tower fan is not turned on.

If the exiting water temperature remains above the setpoint after "free convection" is modeled, then the tower fan is turned on to reduce the exiting water temperature to the setpoint. The model assumes that part-load operation is represented by a simple linear interpolation between two steady-state regimes (e.g., tower fan on for the entire simulation time step and tower fan off for the entire simulation time step). Cyclic losses are not taken into account.

The fraction of time that the tower fan must operate is calculated based on the following equation:

![](media/image4317.png)\


where

![](media/image4318.png)  = exiting water setpoint temperature, ^o^C

![](media/image4319.png)  = exiting water temperature with tower fan off, ^o^C

![](media/image4320.png)  = exiting water temperature with tower fan on, ^o^C

The average fan power for the simulation time step is calculated by multiplying ![](media/image4321.png) by the steady-state fan power specified by the user.

The calculation method for the two-speed tower is similar to that for the single-speed tower example described above. The model first checks to see if "free convection" is specified and if the resulting exiting water temperature is below the setpoint temperature. If not, then the model calculates the steady-state exiting water temperature with the tower fan at low speed. If the exiting water temperature at low fan speed is below the setpoint temperature, then the average fan power is calculated based on the result of equation  and the steady-state, low speed fan power specified by the user. If low-speed fan operation is unable to reduce the exiting water temperature below the setpoint, then the tower fan is increased to its high speed and the steady-state exiting water temperature is calculated. If this temperature is below the setpoint, then a modified version of equation  is used to calculate runtime at high fan speed:

![](media/image4322.png)\


where

![](media/image4323.png)  = exiting water setpoint temperature, ^o^C

![](media/image4324.png)  = exiting water temperature with tower fan at low speed, ^o^C

![](media/image4325.png)  = exiting water temperature with tower fan at high speed, ^o^C

The average fan power for the simulation time step is calculated for the two-speed cooling tower as follows:

![](media/image4326.png)\


The calculation method for the variable-speed Merkel/Scheier model is different from the one- and two-speed models.  Whereas the other towers are setpoint driven, the variable speed Merkel/Scheier model is driven by the load dispatched from the plant operation scheme, such as PlantEquipmentOperation:CoolingLoad.  The plant operation scheme provides the cooling tower model with a target load to meet and the tower is controlled to meet that load (as opposed to meeting a leaving setpoint).  This allows complex configurations with more than one cooling tower to be properly dispatched using all the features available in the various plant operation schemes.  The model first evaluates the load provided with the fans off using only free convection.  If the free convection mode meets or exceeds the target load then the tower runs in free convection mode.  The model then evaluates the load provided by running the fans at full speed.  If the full fan speed load provided is less than or equal to the target load then the tower runs at full speed.  The model then evaluates the load provided with the variable speed fan running at the minimum air speed ratio, which is a user input (default is 0.2).  If the minimum fan speed load provided meets or exceeds the target load, then tower runs at the minimum fan speed.  If the target load is between the load provided by minimum and maximum fans speeds, then the model solves for an airflow rate ratio that just meets the target load using Regula Falsi numerical method.  The variable speed Merkel/Scheier model does not model bypass and may provided excess cooling at times when running in free convection or at the minimum fan speed.  Fan power for the variable speed Merkel/Scheier model is then calculated from the airflow rate ratio using a design fan power and a performance curve or lookup table with one independent variable for the airflow ratio.

![](media/image4327.png)\


### Tower Basin Heater

The tower basin heater operates in the same manner as the variable speed cooling tower basin heater. Refer to the variable speed cooling tower basin heater description in the following section.

### Cooling Tower Makeup Water Usage

The cooling tower makeup water usage is the same as the variable speed cooling tower makeup water usage. Refer to the variable speed cooling tower makeup water usage description in the following section.

### References

Rosaler, Robert C. 1995. Standard Handbook of Plant Engineering, 2^nd^ Ed. New York, NY: McGraw-Hill, pp. 6-36-37.

## Variable Speed Cooling Towers Empirical Models

### Overview

The input object CoolingTower:VariableSpeed provides models for variable speed towers that are based on empirical curve fits of manufacturer's performance data or field measurements. The user specifies tower performance at design conditions, and empirical curves are used to determine the approach temperature and fan power at off-design conditions. The user defines tower performance by entering the inlet air wet-bulb temperature, tower range, and tower approach temperature at the design conditions. The corresponding water flow rate, air flow rate, and fan power must also be specified. The model will account for tower performance in the "free convection" regime, when the tower fan is off but the water pump remains on and heat transfer still occurs (albeit at a low level). Basin heater operation and makeup water usage (due to evaporation, drift, and blowdown) are also modeled.

The cooling tower seeks to maintain the temperature of the water exiting the cooling tower at (or below) a setpoint. The setpoint temperature is defined by the setpoints placed on either the tower outlet node or the loop's overall setpoint node (typically set to the supply side outlet node). The model checks to see if the outlet node has a setpoint placed on it and uses that if it does.  If the outlet node does not have a temperature setpoint then the model uses the loop-level outlet node specified in the input field called Loop Temperature Setpoint Node Name in the PlantLoop or CondenserLoop object. The model simulates the outlet water temperature in four successive steps:

The model first determines the tower outlet water temperature with the tower fan operating at maximum speed. If the outlet water temperature is above the setpoint temperature, the fan runs at maximum speed.

If the outlet water temperature with maximum fan speed is below the setpoint temperature, then the model next determines the impact of "free convection" (water flowing through tower with fan off). If the exiting water temperature based on "free convection" is at or below the setpoint, then the tower fan is not turned on.

If the outlet water temperature remains above the setpoint after "free convection" is modeled, then the tower fan is turned on at the minimum fan speed (minimum air flow rate ratio) to reduce the leaving water temperature. If the outlet water temperature is below the setpoint at minimum fan speed, the tower fan is cycled on and off to maintain the outlet water setpoint temperature.

If the outlet water temperature remains above the setpoint after minimum fan speed is modeled, then the tower fan is turned on and the model determines the required air flow rate and corresponding fan speed to meet the desired setpoint temperature.

### Model Description

The variable speed tower model utilizes user-defined tower performance at design conditions along with empirical curves to determine tower heat rejection and fan power at off-design conditions. Basin heater operation and makeup water usage are also modeled based on user inputs, tower entering air conditions, and tower operation. The following sections describe how each of these tower performance areas is modeled.

### Tower Heat Rejection

Heat rejection by the variable speed cooling tower is modeled based on the CoolTools correlation, YorkCalc correlation, or user-defined coefficients for either the CoolTools or YorkCalc correlations. These purely-empirical correlations model the tower approach temperature using a polynomial curve fit with a large number of terms and either three or four independent variables.

The CoolTools correlation has 35 terms with four independent variables:

    Approach =   Coeff(1)  +  Coeff(2)•FRair  +  Coeff(3)•(FRair)^2^  +

Coeff(4)•(FRair)^3^  +  Coeff(5)•FRwater  +

Coeff(6)•FRair•FRwater  +  Coeff(7)•(FRair)^2^•FRwater  +

Coeff(8)•(FRwater)^2^  +  Coeff(9)•FRair•(FRwater)^2^  +

Coeff(10)•(FRwater)^3^  +  Coeff(11)•Twb  +  Coeff(12)•FRair•Twb  +

Coeff(13)•(FRair)^2^•Twb  +  Coeff(14)•FRwater•Twb  +

Coeff(15)•FRair•FRwater•Twb  +  Coeff(16)•(FRwater)^2^•Twb  +

Coeff(17)•(Twb)^2^  +  Coeff(18)•FRair•(Twb)^2^  +

Coeff(19)•FRwater•(Twb)^2^  +  Coeff(20)•(Twb)^3^  +  Coeff(21)•Tr  +

Coeff(22)•FRair•Tr  +  Coeff(23)•(FRair)^2^•Tr  +

Coeff(24)•FRwater•Tr  +  Coeff(25)•FRair•FRwater•Tr  +

Coeff(26)•(FRwater)^2^•Tr  +  Coeff(27)•Twb•Tr  +

Coeff(28)•FRair•Twb•Tr  +  Coeff(29)•FRwater•Twb•Tr  +

Coeff(30)•(Twb)^2^•Tr  +  Coeff(31)•(Tr)^2^+ Coeff(32)•FRair•(Tr)^2^+

Coeff(33)•FRwater•(Tr)^2^+  Coeff(34)•Twb•(Tr)^2^+  Coeff(35)•(Tr)^3^

where:

Approach = approach temperature (^o^C) = outlet water temperature minus inlet air wet-bulb temperature

FRair = air flow rate ratio (actual air flow rate divided by design air flow rate)

FRwater = water flow rate ratio (actual water flow rate divided by design water flow rate)

Tr = range temperature (^o^C) = inlet water temperature minus outlet water temperature

Twb = inlet air wet-bulb temperature (^o^C)

Coeff(#) = correlation coefficients

If the user selects Tower Model Type = CoolToolsCrossFlow, then the 35 coefficients derived for the CoolTools simulation model (Benton et al. 2002) are used and these coefficients are already defined within EnergyPlus as shown in Table 63. If the user specifies Tower Model Type = CoolToolsUserDefined, then the user must enter a CoolingTowerPerformance:CoolTools object to define the 35 coefficients that will be used by the CoolTools approach temperature correlation.

Table: Approach Temperature Correlation Coefficients

**Coefficient Number**|**Coefficient Value**
-----------------------------------|----------------------------------
|**CoolTools**|**YorkCalc**
Coeff(1)|0.52049709836241|-0.359741205
Coeff(2)|-10.617046395344|-0.055053608
Coeff(3)|10.7292974722538|0.0023850432
Coeff(4)|-2.74988377158227|0.173926877
Coeff(5)|4.73629943913743|-0.0248473764
Coeff(6)|-8.25759700874711|0.00048430224
Coeff(7)|1.57640938114136|-0.005589849456
Coeff(8)|6.51119643791324|0.0005770079712
Coeff(9)|1.50433525206692|-0.00001342427256
Coeff(10)|-3.2888529287801|2.84765801111111
Coeff(11)|0.0257786145353773|-0.121765149
Coeff(12)|0.182464289315254|0.0014599242
Coeff(13)|-0.0818947291400898|1.680428651
Coeff(14)|-0.215010003996285|-0.0166920786
Coeff(15)|0.0186741309635284|-0.0007190532
Coeff(16)|0.0536824177590012|-0.025485194448
Coeff(17)|-0.00270968955115031|0.0000487491696
Coeff(18)|0.00112277498589279|0.00002719234152
Coeff(19)|-0.00127758497497718|-0.0653766255555556
Coeff(20)|0.0000760420796601607|-0.002278167
Coeff(21)|1.43600088336017|0.0002500254
Coeff(22)|-0.5198695909109|-0.0910565458
Coeff(23)|0.117339576910507|0.00318176316
Coeff(24)|1.50492810819924|0.000038621772
Coeff(25)|-0.135898905926974|-0.0034285382352
Coeff(26)|-0.152577581866506|0.00000856589904
Coeff(27)|-0.0533843828114562|-0.000001516821552
Coeff(28)|0.00493294869565511|N/A
Coeff(29)|-0.00796260394174197|N/A
Coeff(30)|0.000222619828621544|N/A
Coeff(31)|-0.0543952001568055|N/A
Coeff(32)|0.00474266879161693|N/A
Coeff(33)|-0.0185854671815598|N/A
Coeff(34)|0.00115667701293848|N/A
Coeff(35)|0.000807370664460284|N/A

Similarly, the YorkCalc correlation has 27 terms with three independent variables:

    Approach =   Coeff(1)  +  Coeff(2)•Twb +  Coeff(3)•Twb^2^  +  Coeff(4)•Tr  +

Coeff(5)•Twb•Tr  +  Coeff(6)•Twb^2^•Tr  +  Coeff(7)•Tr^2^  +

Coeff(8)•Twb•Tr^2^+  Coeff(9)•Twb^2^•Tr^2^+  Coeff(10)•LGRatio  +

Coeff(11)•Twb•LGRatio  + Coeff(12)•Twb^2^•LGRatio  +

Coeff(13)•Tr•LGRatio  +  Coeff(14)•Twb•Tr•LGRatio  +

Coeff(15)•Twb^2^•Tr•LGRatio  +  Coeff(16)•Tr^2^•LGRatio  +

Coeff(17)•Twb•Tr^2^•LGRatio  + Coeff(18)•Twb^2^•Tr^2^•LGRatio +

Coeff(19)•LGRatio^2^+  Coeff(20)•Twb•LGRatio^2^  +

Coeff(21)• Twb^2^•LGRatio^2^+  Coeff(22)•Tr•LGRatio^2^+

Coeff(23)•Twb•Tr•LGRatio^2^+  Coeff(24)•Twb^2^•Tr•LGRatio^2^+

Coeff(25)•Tr^2^•LGRatio^2^+  Coeff(26)•Twb•Tr^2^•LGRatio^2^+

Coeff(27)•Twb^2^•Tr^2^•LGRatio^2^

where:

Approach = approach temperature (^o^C) = outlet water temperature minus inlet air wet-bulb temperature

Tr = range temperature (^o^C) = inlet water temperature minus outlet water temperature

Twb = inlet air wet-bulb temperature (^o^C)

LGratio = liquid-to-gas ratio = ratio of water flow rate ratio (FRwater) to air flow rate ratio (FRair)

Coeff(#) = correlation coefficients

If the user selects Tower Model Type = YorkCalc, then the 27 coefficients derived for the YorkCalc simulation model (York International Corp. 2002) are used and these coefficients are already defined within EnergyPlus as shown in Table 63. If the user specifies Tower Model Type = YorkCalcUserDefined, then the user must enter a CoolingTowerPerformance:YorkCalc object to define the 27 coefficients that will be used by the YorkCalc approach temperature correlation.

The approach temperature correlations for the CoolTools and YorkCalc simulation models are valid for a range of conditions defined in Table 64. If the user defines their own model coefficients (CoolingTowerPerformance:CoolTools or CoolingTowerPerformance:YorkCalc), then they must also define in that same object the range of conditions for which the model is valid. For all of these correlation variables, the program issues warnings if the actual values are beyond the minimum/maximum values specified for the correlation being used. For inlet air wet-bulb temperature and water mass flow rate ratio, the values of these variables used in the calculation of approach temperature are limited to be within the valid minimum/maximum range. For approach, range, and liquid-to-gas ratio the warnings are issued if the values are beyond the specified minimum/maximum range but the actual values are still used. The warnings issued do not necessarily indicate a poor estimate of tower performance at the condition(s) which caused the warning, but are provided to identify conditions outside the defined correlation limits. Exceeding the defined limits by a small amount may not introduce significant errors, but large deviations may be problematic. It is for this reason that we recommend using a very broad range of cooling tower performance data (i.e., data covering the entire range expected during the simulation) when generating user-defined coefficients for the variable speed tower model.

Table: Minimum and Maximum Limits for Approach Temperature Correlation Variables

Independent Variable Limit|CoolTools|YorkCalc
--------------------------|---------|--------
Minimum Inlet Air Wet-Bulb Temperature|-1.0°C|-34.4°C
Maximum Inlet Air Wet-Bulb Temperature|26.7°C|26.7°C
Minimum Tower Range Temperature|1.1°C|1.1°C
Maximum Tower Range Temperature|11.1°C|22.2°C
Minimum Tower Approach Temperature|1.1°C|1.1°C
Maximum Tower Approach Temperature|11.1°C|40°C
Minimum Water Flow Rate Ratio|0.75|0.75
Maximum Water Flow Rate Ratio|1.25|1.25
Maximum Liquid-to-Gas Ratio|N/A|8.0

The approach temperature correlation(s) used to simulate cooling tower heat rejection are based on water and air flow rate "ratios" and are not directly dependent on the size of the tower or the actual air and water flow rates through the tower. However, the model correlations are developed based on a reference condition. For Model Types "CoolToolsCrossFlow" and "YorkCalc", the reference condition is a water flow rate of 0.000043 m^3^/s per kW of heat rejected (2.4 gal/min per ton of heat rejected) with 25.6^o^C (78^o^F) enter air wet-bulb temperature, 35^o^C (95^o^F) hot water inlet temperature, and 29.4^o^C (85^o^F) cold water outlet temperature. The reference condition may be different if the user defines tower model coefficients using CoolingTowerPerformance:CoolTools or CoolingTowerPerformance:YorkCalc.

Due to the inherent reference condition used to generate the tower performance curves, the water flow rate at the reference condition must be determined using the design performance information specified by the user and the tower model's approach temperature correlation. This is done by using the model's approach temperature correlation (described earlier in this section) to calculate the water flow rate ratio which yields the user-defined design approach temperature based on an air flow rate ratio of 1.0 (*FR~air~* = 1.0), the design inlet air wet-bulb temperature, and the design range temperature. The calculated approach temperature (using the model correlation) must satisfy the following two equations:

![](media/image4328.png) ![](media/image4329.png)

where:

![](media/image4330.png) = design outlet water temperature (^o^C)

![](media/image4331.png) = design inlet water temperature (^o^C)

![](media/image4332.png) = design inlet air wet-bulb temperature (^o^C)

![](media/image4333.png) = design approach temperature (^o^C)

![](media/image4334.png) = design range temperature (^o^C)

![](media/image4335.png) = air flow rate ratio (actual air flow rate divided by design air flow rate)

The water flow rate ratio used in the approach temperature correlation which satisfies these two equations is the ratio of the design water flow rate (specified by the user) to the water flow rate at the reference condition. This ratio is used to calculate the reference water volumetric flow rate, which is then used throughout the simulation to determine the actual water flow rate ratio used in the approach temperature correlation for each simulation time step.

![](media/image4336.png)\


where:

![](media/image4337.png) = water volumetric flow rate at the reference condition (m^3^/s)

![](media/image4338.png) = design water volumetric flow rate specified by the user (m^3^/s)

![](media/image4339.png) = design water flow rate divided by the reference water flow rate

The cooling tower seeks to maintain the temperature of the water exiting the cooling tower at (or below) a setpoint. The setpoint temperature is defined by the field "Condenser Loop Temperature Setpoint schedule or reference" for the CondenserLoop object. The model simulates the outlet water temperature in four successive steps:

The model first determines the tower outlet water temperature with the tower fan operating at maximum speed. If the outlet water temperature is above the setpoint temperature, the fan runs at maximum speed.

If the outlet water temperature with maximum fan speed is below the setpoint temperature, then the model next determines the impact of "free convection" (water flowing through tower with fan off). If the exiting water temperature based on "free convection" is at or below the setpoint, then the tower fan is not turned on.

If the outlet water temperature remains above the setpoint after "free convection" is modeled, then the tower fan is turned on at the minimum fan speed (minimum air flow rate ratio) to reduce the leaving water temperature. If the outlet water temperature is below the setpoint at minimum fan speed, the tower fan is cycled on and off to maintain the outlet water setpoint temperature.

If the outlet water temperature remains above the setpoint after minimum fan speed is modeled, then the tower fan is turned on and the model determines the required air flow rate and corresponding fan speed to meet the desired setpoint temperature.

For each simulation time step, the model first calculates the outlet water temperature with the tower fan operating at maximum speed (FRair = 1.0). The calculated approach temperature (using the correlations described above), inlet air wet-bulb temperature (weather data), and range temperature are used to determine the tower outlet water temperature as follows:

![](media/image4340.png)\


where:

![](media/image4341.png)  = tower outlet water temperature at maximum fan speed (^o^C)

![](media/image4342.png)  = tower inlet air wet-bulb temperature (^o^C)

![](media/image4343.png) = approach temperature at current operating conditions (^o^C)

![](media/image4344.png) = range temperature at current operating conditions (^o^C)

Note that the approach temperature correlation as described previously is a function of range temperature, so the equations above must be solved iteratively to converge on a solution. If the resulting outlet water temperature is above the desired setpoint temperature, then the fan runs at maximum speed and does not cycle on/off (fan part-load ratio = FanPLR = 1.0 and *FR~air~* = 1.0).

If the outlet water temperature with maximum fan speed is below the setpoint temperature, then the model next determines the impact of "free convection" (water flowing through tower with fan off). In the free convection regime, the outlet water temperature is calculated using a fraction of the water temperature difference through the tower when the fan is at its maximum speed. This fraction is defined by the user (Fraction of Tower Capacity in Free Convection Regime).

![](media/image4345.png)\


where:

![](media/image4346.png)  = tower outlet water temperature in free convection regime (^o^C)

![](media/image4347.png)  = tower inlet water temperature (^o^C)

![](media/image4348.png) = fraction of tower capacity in free convection regime (user specified)

If the outlet water temperature in the free convection regime is below the setpoint temperature, the tower fan is not turned on and the fan part-load ratio is set equal to 0. In addition, the air flow rate ratio through the tower is assumed to be equal to the fraction of tower capacity in the free convection regime.

![](media/image4349.png)\


![](media/image4350.png)\


where:

![](media/image4351.png) = fan part-load ratio

![](media/image4352.png) = fan part-load ratio in free convection regime

![](media/image4353.png)  = air flow rate ratio in free convection regime

If the outlet water temperature in the free convection regime is above the setpoint temperature, then the fan is turned on at the minimum fan speed (minimum air flow rate ratio, FR~air,min~, entered by the user) and the outlet water temperature is calculated as the inlet air wet-bulb temperature plus the calculated approach temperature:

![](media/image4354.png)\


![](media/image4355.png)\


where:

![](media/image4356.png)  = outlet water temperature at minimum fan speed (^o^C)

![](media/image4357.png)  = air flow rate ratio at the minimum fan speed

If the outlet water temperature at minimum fan speed is below the setpoint temperature, the cooling tower fan cycles on and off at the minimum air flow rate ratio in order to meet the setpoint temperature.

![](media/image4358.png)\


where:

![](media/image4359.png)  = outlet water setpoint temperature (^o^C)

If the outlet water temperature at minimum fan speed is above the outlet water temperature setpoint, then the cooling tower fan speed (*FR~air~*) is increased until the calculated approach temperature produces the required outlet water temperature to meet the setpoint.

![](media/image4360.png)\


![](media/image4361.png)\


![](media/image4362.png)  *(i.e., fan does not cycle on/off)*

### Fan Power

When the cooling tower fan is operating, fan electric power is calculated based on the air flow rate ratio required to meet the above conditions. If the user has entered a fan power curve object (cubic curve), the output of that curve is multiplied by the design fan power. Otherwise, tower fan power is assumed to be directly proportional to the cube of the air flow rate ratio. In either case, the fan part-load ratio is applied to account for times when the tower fan cycles on/off to meet the setpoint temperature. Fan energy consumption is calculated each simulation time step.

![](media/image4363.png)\


![](media/image4364.png)\


where:

![](media/image4365.png)  = name of fan power ratio as a function of air flow rate ratio curve

![](media/image4366.png)  = tower fan electric power (W)

![](media/image4367.png)  = tower fan electric consumption (J)

![](media/image4368.png)  = output of FanPowerCurveObject evaluated at the operating air flow rate ratio (*FR~air~*)

![](media/image4369.png)  = design fan power at design (maximum) air flow through the tower (W)

*TimeStepSys* = HVAC system simulation time step (hr)

### Tower Basin Heater

Calculations are also made to estimate the electric power input to the tower basin heater. A schedule may be used to disable the basin heater during regular maintenance periods or other time periods (e.g., during summer). If a schedule is not provided, the basin heater is assumed to be available the entire simulation time period. The basin heater operates when it is scheduled on, the outdoor air dry-bulb temperature is below the basin heater setpoint temperature, and the cooling tower is not active (i.e., water is not flowing through the tower). The user is required to enter a basin heater capacity (watts per degree Kelvin) and a heater setpoint temperature (^o^C) if they want to model basin heater electric power.

![](media/image4370.png)\


![](media/image4371.png)\


where:

![](media/image4372.png)  = tower basin heater electric power (W)

![](media/image4373.png)  = tower basin heater electric consumption (J)

*T~setpoint,basin~* = basin heater setpoint temperature (^o^C)

*T~db,outdoor~*    = outdoor air dry-bulb temperature (^o^C)

CAP*~heater,basin~* = basin heater capacity (W/K)

*Schedule~heater,basin~* = basin heater schedule (schedule value > 0 means ON)

### References

ASHRAE 1999. HVAC1 Toolkit: A Toolkit for Primary HVAC System Energy Calculations. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

Benton, D.J., Bowman, C.F., Hydeman, M., Miller, P. 2002. An Improved Cooling Tower Algorithm for the CoolTools^TM^ Simulation Model. *ASHRAE Transactions,* Vol. 108, Part 1, pp.760-768.

Bourdouxhe, J.P., M. Grodent, J. Lebrun and C. Silva. 1994. Cooling tower model developed in a toolkit for primary HVAC system energy calculation: part 1. Proceedings of the fourth international conference on system simulation in buildings, Liege (Belgium), December 5-7, 1994.

Incropera, F.P. and D.P. DeWitt. 1981. Fundamentals of Heat Transfer. New York: John Wiley & Sons.

Merkel, F.  1925. Verduftungskuhlung. VDI Forschungarbeiten, No 275, Berlin.

Rosaler, Robert C. 1995. Standard Handbook of Plant Engineering, 2^nd^ Ed. New York, NY: McGraw-Hill, pp. 6-36-37.

Scheier, L. 2013. Personal communication.

York International Corporation, 2002. "YORKcalc^TM^ Software, Chiller-Plant Energy-Estimating Program", Form 160.00-SG2 (0502).

## Cooling Towers with Multiple Cells

Many towers are constructed to be capable of being grouped together to achieve the desired capacity. Thus, many cooling towers are assemblies of two or more individual cooling towers or "cells." The number of cells they have, e.g., an eight-cell tower, often refers to such towers.

For the operation of multi-cell towers, the first step is to determine the number of cells *n,* which will be operating during the timestep using the calculation logic from DOE-2.1E.

The maximum and minimum flow rates per cell are determined according to the input fractions (*Minimum  Water Flow Rate Fraction:* *k*min ** and *Maximum  Water Flow Rate Fraction: k*max) as follows:

!!!BEGIN XML MATH!!!

mw,min=mw,desntot×kmin mw,max=mw,desntot×kmax

!!!END XML MATH!!!

where *m*w,des is the design water flow rate through the entire cooling tower.

Then, we determine the minimum and maximum number of cells that can operate with this water flow rate:

*n*min=MIN(*m*w,tot *m*w,max,*n*tot)*n*max=MIN(*m*w,tot *m*w,min,*n*tot)

where *n~tot~* is the total number of cells of the tower, and *m*w,tot is the water flow rate to the tower.

The number of cells operating *n* is set accordingly:

*If the Cell Control* method is *MinimalCell,*

*n = n*min

*If the Cell Control* method is *MaximalCell,*

*n = n*max

Finally, the water mass flow rate per cell (*m*w) will be:

!!!BEGIN XML MATH!!!

mw=mw,totn

!!!END XML MATH!!!

Then we simulate the performance of one cell with this flow rate per cell (calling the SimSimpleTower subroutine for single and two speed cooling tower objects). As we assume that each cell is identical, the UA of one cell is calculated dividing the UA of the whole tower (obtained from the input or from the auto sizing calculations). The air flow rate per cell is also equal to the one of the whole tower divided by the number of cells operating:

!!!BEGIN XML MATH!!!

UAcell=UAcellntot

!!!END XML MATH!!!

!!!BEGIN XML MATH!!!

mair,cell=mair,totn

!!!END XML MATH!!!

At the end, the total fan power of the tower operating with a certain number of cells is given by:

!!!BEGIN XML MATH!!!

FanPower=FanPowertot×nntot

!!!END XML MATH!!!

If the cells operating do not meet the loads, we increase the number of cells if spare cells are available and the water flow through each cell is within the user specified minimum and maximum water flow rate fractions range. This is an iteration process.

## Cooling Tower Makeup Water Usage

Makeup water use for all types of cooling towers is made up of three components: evaporation, drift, and blowdown.  The first is the amount of water evaporated to reduce the water's temperature as it passes through the cooling tower. There are two methods that evaporation makeup water can be modeled in EnergyPlus.  The first method assumes that the tower outlet air conditions are saturated (which may not always be the case for certain operating conditions).  For this "Saturated Exit" mode, the enthalpy of the tower's outlet air is calculated as the inlet air enthalpy plus the water side heat transfer divided by the air mass flow rate through the tower.

![](media/image4374.png)\


![](media/image4375.png)\


where:

![](media/image4376.png)  = water-side heat transfer (W)

![](media/image4377.png)  = mass flow rate of water through the tower (kg/s)

![](media/image4378.png)  = specific heat of water (W/kg-K)

![](media/image4379.png)  = saturated outlet air enthalpy (J/kg)

![](media/image4380.png)  = inlet air enthalpy (J/kg)

![](media/image4381.png)  = mass flow rate of air through the tower (kg/s)

The saturation temperature and humidity ratio are then calculated for the tower's outlet air.

![](media/image4382.png)\


![](media/image4383.png)\


where:

![](media/image4384.png)  = saturated outlet air temperature (^o^C)

![](media/image4385.png)  = EnergyPlus psychrometric function, returns saturation temperature given enthalpy and barometric pressure

![](media/image4386.png)  = outdoor barometric pressure (Pa)

![](media/image4387.png)  = saturated outlet air humidity ratio (kg~water~/kg~dry air~)

![](media/image4388.png)  = EnergyPlus psychrometric function, returns humidity ratio given dry-bulb temperature and enthalpy

The makeup water quantity required to replenish the water lost due to evaporation is then calculated as the product of the air mass flow rate and the difference between the entering and leaving air humidity ratio divided by the density of water.

![](media/image4389.png)\


where:

![](media/image4390.png)  = makeup water usage due to evaporation (m^3^/s)

![](media/image4391.png)  = mass flow rate of air through tower (kg/s)

![](media/image4392.png)  = humidity ratio of tower inlet air (kg~water~/kg~dry air~)

![](media/image4393.png)  = density of water evaluated at the tower inlet air temperature (kg/m^3^)

The second method available for calculating water makeup for evaporation is for the user to provide a value for a loss factor.  The evaporation loss is then calculated as a fraction of the circulating condenser water flow and varies with the temperature change in the condenser water.  The value provided by the user is in units of percent-per-degree Kelvin.  The evaporation rate will equal this value times each degree Kelvin of temperature drop in the condenser water.  Typical values are from 0.15 to 0.27 [percent/K]. The default is 0.2.  The rate of water makeup for evaporation is then calculated by multiplying this factor times the condenser water flow rate and the temperature decrease in the condenser water flow rate. For evaporative fluid coolers, a numerical value of loss factor can be entered in the same manner as for cooling towers. If this field is blank, an empirical correlation will be used to calculate the value based on current outdoor dry bulb temperature and relative humidity. The following correlation from Qureshi and Zubair (2007) is used to calculate the loss factor:

![](media/image4394.png)\


where:

ϕ = relative humidity of inlet air

*t*db = Dry-bulb temperature of inlet air

Additional makeup water usage is modeled as a percentage of design water flow rate through the tower to account for drift, and as a scheduled flow rate to model blowdown. Drift is water loss due to the entrainment of small water droplets in the air stream passing through the tower. Drift is defined by the model user as a percentage of the tower's design water flow rate, and is assumed to vary with tower air flow rate ratio as follows:

![](media/image4395.png)\


where:

![](media/image4396.png)  = makeup water usage due to drift (m^3^/s)

![](media/image4397.png)  = design (volumetric) water flow rate (m^3^/s)

![](media/image4398.png)  = percent of design water flow rate lost to drift at the tower design air flow rate

![](media/image4399.png) = ratio of actual air flow rate to tower design air flow rate

Blowdown is water flushed from the basin on a periodic basis to purge the concentration of mineral scale or other contaminants. There are two ways that blowdown is calculated in EnergyPlus.  Blowdown water rates can be scheduled so that we have:.

![](media/image4400.png)\


where:

![](media/image4401.png)  = makeup water usage due to blowdown (m^3^/s)

*ScheduleValue~blowdown~* = blowdown schedule value for the time step being simulated (m^3^/s)

The second (and default) way that blowdown can be calculated is to assume that blowdown water is continually introduced at a rate that will provide a constant concentration ratio.  As water evaporates it leaves behind minerals and the like causing the concentration of water impurities to be higher in the tower than in the makeup water.  Acceptable concentration ratios are in the range of 3 to 5 depending on the purity of the make up water.  Water lost as drift does not evaporate and decrease the water needed for blowdown.  Using the "Concentration Ratio" method, the rate of blowdown can be calculated using:

![](media/image4402.png)\


where,

![](media/image4403.png)  is the concentration ratio or the ratio of solids in the blowdown water to solids in the makeup water.

The tower makeup water consumption (m^3^) for each simulation time step is calculated as the sum of the individual components of makeup water usage multiplied by the simulation time step in hours and the conversion for hours to seconds (3600 sec/hr). Makeup water usage is only calculated when the cooling tower is active and water is flowing through the cooling tower.

![](media/image4404.png)\


where:

![](media/image4405.png)  = tower makeup water consumption (m^3^)

### References

Robert C. Rosaler. 1995. Standard Handbook of Plant Engineering, 2^nd^ Ed. McGraw-Hill, New York, NY, pp 6-36-37.

Quereshi, B.A. and S.M.Zubair. 2007. Prediction of evaporation losses in evaporative fluid coolers, Applied Thermal Engineering 27 pp. 520-527

## One and Two Speed Fluid Coolers

### Overview

The input objects FluidCooler:SingleSpeed and FluidCooler:TwoSpeed provide models for dry fluid coolers. Fluid cooler's performance is modeled using effectiveness-NTU relationships for cross flow heat exchanger with both streams unmixed. The model can be used to simulate the performance of both single speed and two speed mechanical-draft fluid coolers. For part-load operation, the model assumes a simple linear interpolation between two steady-state regimes without accounting for any cycling losses.

### Model Description

The expression for fluid cooler effectiveness is as follows:

![](media/image4406.png)\


Where

![](media/image4407.png) ℇℇ **= heat exchanger effectiveness

![](media/image4408.png)\


![](media/image4409.png) Ca=macpaCa=macpa and ![](media/image4410.png)

![](media/image4411.png) Cmax=Max (Ca,Cw)Cmax=Max(Ca,Cw);![](media/image4412.png)

![](media/image4413.png)\


![](media/image4414.png) η=NTU-0.22η=NTU-0.22

The first fluid is water and the second fluid is air entering the heat exchanger at temperature ![](media/image4415.png) TdbinTdbin and specific heat ![](media/image4416.png) cpacpa. The heat exchanger is characterized by a single parameter, its overall heat transfer coefficient-area product UA.

When the user selects the nominal capacity method, the UA is calculated as follows:

The model inputs (other than the UA) and the fluid cooler load that it must meet are specified at design conditions. Then the fluid cooler model converges to a UA value, using the regulafalsi method that will enable it to meet the design fluid cooler load given at the specified inputs.

### Method for Calculating Steady-State Exiting Water Temperature

The objective of the fluid cooler model is to predict the exiting water temperature and the fan power required to meet the exiting water setpoint temperature. The exiting water temperature is calculated based on an energy balance that assumes that the energy absorbed by the air is equivalent to the energy removed from the water. The procedure for calculating the steady-state, exiting air dry-bulb temperature is outlined below.

With the overall heat transfer coefficient-area product for the fluid cooler calculated by the nominal capacity information entered by the user, the effectiveness of the heat exchanger is then calculated as:

![](media/image4417.png)\


The heat transfer rate is then calculated as follows:

![](media/image4418.png) Q=ℇCmin(Tw,in-Tdbin)Q=ℇCmin(Tw,in-Tdbin)

Then the outlet air dry-bulb and outlet water temperature are calculated:

![](media/image4419.png)\


![](media/image4420.png)\


![](media/image4421.png) = inlet water temperature, ^o^C

![](media/image4422.png) = outlet water temperature, ^o^C

![](media/image4423.png)  = dry-bulb temperature of the inlet air, ^o^C

![](media/image4424.png) = dry-bulb temperature of the outlet air, ^o^C

### Calculating the Actual Exiting Water Temperature and Fan Power

The previous section describes the methodology used for calculating the steady-state temperature of the water leaving the fluid cooler. This methodology is used to calculate the exiting water temperature with the fluid cooler fans operating (including low and high fan speed for the two-speed fluid cooler). The exiting water temperature calculations use the fluid flow rates (water and air) and the Nominal capacity information entered by the user for each regime.

The fluid cooler model seeks to maintain the temperature of the water exiting the fluid cooler at (or below) a setpoint. The setpoint schedule is defined by the field "Loop Temperature Setpoint  Node or reference" for the CondenserLoop object.

The fluid cooler fans are turned on to reduce the exiting water temperature to the setpoint. The model assumes that part-load operation is represented by a simple linear interpolation between two steady-state regimes (e.g., Fluid cooler fans on for the entire simulation time step and fluid cooler fans off for the entire simulation time step). Cyclic losses are not taken into account. If the outlet water temperature is less than the set-point then the fraction of time for which the fluid cooler must operate to meet the set-point is calculated by using the following equation:

![](media/image4425.png)\


Where

![](media/image4426.png)  = exiting water setpoint temperature, ^o^C

![](media/image4427.png)  = exiting water temperature with all fluid cooler fans off, ^o^C

![](media/image4428.png)  = exiting water temperature with all fluid cooler fans on, ^o^C

The average fan power for the simulation time step is calculated by multiplying ![](media/image4429.png) by the steady-state fan power specified by the user.

The calculation method for the two-speed fluid cooler is similar to that for the single-speed fluid cooler example described above. The model first calculates the steady-state exiting water temperature with the fluid cooler fans at low speed. If the exiting water temperature at low fan speed is below the setpoint temperature, then the average fan power is calculated based on the result of previous equation and the steady-state, low speed fan power specified by the user.  If low-speed fan operation is unable to reduce the exiting water temperature below the setpoint, then the fluid cooler fans' speed is increased to high speed and the steady-state exiting water temperature is calculated. If this temperature is below the setpoint, then a modified version of previous equation is used to calculate runtime at high fan speed:

![](media/image4430.png)\


where

![](media/image4431.png)  = exiting water setpoint temperature, ^o^C

![](media/image4432.png)  = exiting water temperature with fluid cooler fans at low speed, ^o^C

![](media/image4433.png)  = exiting water temperature with fluid cooler fans at high speed, ^o^C

The average fan power for the simulation time step is calculated for the two-speed fluid cooler as follows

![](media/image4434.png)\
