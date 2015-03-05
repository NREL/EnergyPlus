# ChillerHeaterPerformance:Electric:EIR 

## Overview

The object simulates the performance of a chiller-heater which can receive pre-cooled or pre-heated water from the source loop, and provide cooling, heating, or simultaneous cooling-heating. The object needs to work with the Central Heat Pump System object to be controlled properly. This model does not simulate the thermal performance or the power consumption of associated pumps or cooling towers. The Central Heat Pump System object holds the input/output nodes connection of the chiller-heater and its control scheme, once the chiller-heater is properly referred.

## Model Description

The model uses user-input performance information at design conditions along with three performance curves (curve objects) for cooling capacity and efficiency to determine chiller operation at off-design conditions. Three additional performance curves for heating capacity and efficiency are used when the chiller is operating in a heating-only mode or simultaneous cooling-heating mode.

### Cooling-only mode

The following nomenclature is used in the cooling equations:

*CompMotorEffic* = compressor motor efficiency

*CompPower~clg~* = compressor power [W]

*CompPower~@PLRmin~*  = compressor power at the minimum part-load ratio [W]

![](media/image5519.png)  = chilled water specific heat  [J/kgK]

*CyclingRatio*~~ = compressor cycling ratio =*PLR~actual~* / *PLR~min~*

*EvapCapAvail~clg~* = available full-load cooling capacity at current conditions [W]

*EvapCapFT~clg~* = cooling capacity function of temperature curve

*EIRFT~clg~* = electric input to cooling output factor for temperature function curve

*EIRFPLR~clg~* = electric input to cooling output factor for part-load function curve

![](media/image5520.png)  = chilled water mass flow rate [kg/s]

![](media/image5521.png)  = chilled water maximum available mass flow rate [kg/s]

*PLR~clg~* = cooling part-load ratio = *CoolingLoad* / *EvapCapAvail~clg~*

*PLR~actual~* = actual part-load ratio at current conditions

*PLR~min~* = minimum part-load ratio

![](media/image5522.png)  = total condenser heat transfer energy [J]

![](media/image5523.png)  = condenser heat transfer rate [W]

![](media/image5524.png)  = total evaporator heat transfer energy [J]

![](media/image5525.png)  = evaporator heat transfer rate [W]

![](media/image5526.png)  = false loading rate [W]

*RefCOP~clg~*  = reference coefficient of performance [W/W]

*RefEvapCap~clg~*  = reference evaporator capacity [W]

*FullLoadPwr~clg~*  = reference full load power = *EvapCapAvail~clg~* / *RefCOP~clg~~~*[W]

*T~cond~* = either entering or leaving condenser water temperature depending on user input for condenser water independent variable. *T~c~~ond,l~*, if "LeavingCondenser" is chosen, or *T~c~~ond,~~e~*, if "EnteringCondenser" is chosen.

*T~cond~~,e~* = entering condenser water temperature [C]

*T~cond~~,l~* = leaving condenser water temperature [C]

*T~cw,e~* = entering chilled water  temperature [W]

*T~cw,l~* = leaving chilled water  temperature [W]

![](media/image5527.png)  = chilled water inlet and outlet temperature difference [C]

![](media/image5528.png)  = maximum chilled water inlet and outlet temperature difference [C]

The model sequentially calls each chiller-heater module in the order defined in the Central Heat Pump System object. It then determines cooling load that each chiller-heater needs to meet and water flow rates delivered to each chiller-heater. Once each chiller-heater is assumed to operate, it determines cooling capacity and efficiency using user-supplied performance information.

Three performance curves are used in the calculation of cooling capacity and efficiency as follows:

#. Cooling mode cooling capacity function of temperature curve (*EvapCapFT~clg~*)
#. Cooling mode electric input to cooling output ratio function of temperature curve (*EIRFT~clg~*)
#. Cooling mode electric input to cooling output ratio function of part load ratio curve (*EIRFPLR~clg~*)

The Cooling Capacity Function of Temperature Curve (*EvapCapFT~clg~*) represents the fraction of the cooling capacity of the chiller-heater as it varies by temperature. The curve should have a value of 1.0 at the reference conditions. The output of a bi-quadratic curve with the input variables being the leaving chilled water temperature and either the entering or leaving condenser water temperature is given by:

**![](media/image5529.png)** **.**

**The** Cooling Mode Electric Input to Cooling Output Ratio Function of Temperature (EIRFT~clg~) curve represents the fraction of electricity to the chiller-heater at full load as it varies by temperature. The output of a bi-quadratic curve with the input variables being the leaving chilled water temperature and either the entering or leaving condenser water temperature is given by:

**![](media/image5530.png)** **.**

**The** Cooling Mode Electric Input to Cooling Output Ratio Function of Part Load Ratio (EIRFPLR~clg~) curve represents the fraction of electricity to the chiller-heater as the load on the chiller varies at a given set of operating temperatures. The curve is normalized so that at full load the value of the curve should be 1.0. Note that the bi-cubic formulation below is generally only valid when LeavingCondenser variable is chosen for the field of Cooling Mode Condenser Water Temperature Curve Input Variable whereas the quadratic curve can be used for both choices, i.e., LeavingCondenser and EnteringCondenser. Bi-cubic may also be used when the chiller-heater uses a variable-speed compressor motor drive. The output of this curve can be determined by one of the following three performance curves:

![](media/image5531.png)\


![](media/image5532.png)\


![](media/image5533.png)\


The full-load cooling capacity at specific temperature operating conditions (i.e., at temperatures different from the design temperatures) is then computed as follows:

**![](media/image5534.png)** **.**

The model then determines current chiller-heater's evaporator heat transfer rate based on the total cooling load required a central heat pump system to meet and the maximum available chiller-heater cooling capacity. The maximum evaporator temperature difference between the entering chilled water temperature (*T~cw,e~*) and the leaving chilled water temperature (*T~cw,l~*) obtained from the plant loop setpoint temperature can also be determined. It then calculates mass flow rate for variable flow control chiller-heaters and the temperature difference for constant flow control chiller-heaters, setting the cooling load each chiller-heater needs to meet equal to the evaporator heat transfer rate.

As for variable flow control chiller-heaters, the chilled water mass flow rate is computed as follows:

**![](media/image5535.png)** **.**

The chilled water mass flow rate calculated is then compared to the maximum available mass flow rate for individual chiller-heaters. If the calculated one is bigger than the maximum, the model sets the chilled water mass flow rate equal to the maximum. It then adjusts the temperature difference based on the evaporator heat transfer rate and the maximum mass flow rate. If the adjusted temperature difference also exceeds the maximum, the model finally adjusts the evaporator heat transfer rate at the maximum temperature difference and mass flow rate as follows:

**![](media/image5536.png)** **.**

As for constant flow control chiller-heaters, the model calculates chilled water temperature difference as follows:

**![](media/image5537.png)** **.**

The temperature difference calculated is then compared to the maximum temperature difference allowed. If the calculated one is bigger than the maximum, the model sets the chilled water temperature difference equal the maximum, and then adjusts the evaporator heat transfer rate at the given conditions as follows:

**![](media/image5538.png)** **.**

The model then calculates the part-load ratio as the ratio of the evaporator heat transfer rate to the available chiller-heater capacity as follows:

**![](media/image5539.png)** **.**

The part-load ratio calculated is set to be between the maximum of 1.0 and the minimum of 0.0 when it is out of the range. Once the part-load ratio is calculated the cycling ratio and false loading rate can be obtained as follows:

![](media/image5540.png)\


![](media/image5541.png) .

**The compressor power demand is then computed by:**

![](media/image5542.png) .

**The heat transfer rate for the chiller-heater condenser can then be computed as follows:**

![](media/image5543.png)  .

The total heat transfer energy by the evaporator and condenser can be calculated as follows:

![](media/image5544.png)\


![](media/image5545.png)  .

### Heating-only mode and Simultaneous cooling-heating mode

The following nomenclature is used in the heating equations:

*CompMotorEffic* = compressor motor efficiency

*CompPower~ht~~g~* = compressor power demand [W]

*CompPower~@PLRmin~*  = compressor power at the minimum part-load ratio [W]

![](media/image5546.png)  = evaporator water specific heat  [J/kgK]

![](media/image5547.png)  = hot water specific heat  [J/kgK]

*CyclingRatio*~~ = compressor cycling ratio =*PLR~actual~* / *PLR~min~*

*EvapCapAvail~ht~~g~* = available full-load cooling capacity at current conditions [W]

*EvapCapFT~ht~~g~* = heating mode cooling capacity function of temperature curve

*EIRFT~ht~~g~* = electric input to cooling output factor for temperature function curve

*EIRFPLR~ht~~g~* = electric input to cooling output factor for part-load function curve

![](media/image5548.png)  = evaporator water maximum available mass flow rate [kg/s]

![](media/image5549.png)  = condenser water maximum available mass flow rate [kg/s]

![](media/image5550.png)  = hot water mass flow rate [kg/s]

*PLR~ht~~g~* = cooling part-load ratio = *RefCap* / *EvapCapAvail~htg~*

*PLR~max~* = maximum part-load ratio at current conditions

*PLR~min~* = minimum part-load ratio

![](media/image5551.png)  = total condenser heat transfer energy [J]

![](media/image5552.png)  = available full-load heating capacity at current conditions [W]

![](media/image5553.png)  = condenser heat transfer rate [W]

![](media/image5554.png)  = total evaporator heat transfer energy [J]

![](media/image5555.png)  = evaporator heat transfer rate [W]

![](media/image5556.png)  = false loading rate [W]

*RefCOP~htg~*  = reference coefficient of performance [W/W]

*RefEvapCap~htg~*  = reference evaporator capacity [W]

*FullLoadPwr~htg~*  = reference full load power = *EvapCapAvail~ht~~g~* / *RefCOP~ht~~g~~~*[W]

*T~cond~* = either entering or leaving condenser water temperature depending on user input for condenser water independent variable. *T~c~~ond,l~*, if "LeavingCondenser" is chosen, or *T~c~~ond,~~e~*, if "EnteringCondenser" is chosen.

*T~cond,e~* = entering condenser water temperature [C]

*T~cond~~,l~* = leaving condenser water temperature [C]

*T~cw,l~* = leaving chilled water  temperature [C]

*T~hw,e~* = entering hot water temperature [C]

*T~hw,l~* = leaving hot water temperature [C]

![](media/image5557.png)  = evaporator inlet and outlet water temperature difference [C]

![](media/image5558.png)  = hot water inlet and outlet temperature difference [C]

![](media/image5559.png)  = maximum hot water inlet and outlet temperature difference [C]

The calculations for the evaporator side are similar to the cooling-only mode calculations. The evaporator capacity and efficiency is determined by a different set of three performance curves read in the cooling-only mode, and the performance curve set is used for both heating-only mode and simultaneous cooling-heating mode. During these modes, the evaporator side is not connected to the chilled water loop, but source water loop. The model thus assumes that each chiller-heater does not meet the plant loop chilled water setpoint temperature while the evaporator operates at the full load capacity to produce heating at a constant water flow rate.

The model sequentially calls each chiller-heater module in the order of the definition in the central heat pump system. It then determines heating load that each chiller-heater needs to meet and water flow rates delivered to each chiller-heater. Once each chiller-heater is assumed to operate, it determines heating capacity and efficiency using the following performance curves:

#. Heating mode cooling capacity function of temperature curve (*EvapCapFT~htg~*)
#. Heating mode electric input to cooling output ratio function of temperature curve (*EIRFT~htg~*)
#. Heating mode electric input to cooling output ratio function of part load ratio curve (*EIRFPLR~htg~*)

The output of a Heating Mode Cooling Capacity Function of Temperature curve with the input variables being the leaving chilled water temperature and either the entering or leaving condenser water temperature is given by:

**![](media/image5560.png)**

**The output of** a Heating Mode Cooling Output Ratio Function of Temperature curve with the input variables being the leaving chilled water temperature and either the entering or leaving condenser water temperature is given by:

**![](media/image5561.png)** **.**

**The** output of Heating Mode Cooling Output Ratio Function of Part Load Ratio curve can be determined by one of the following three performance curves as follows:

![](media/image5562.png)\


![](media/image5563.png)\


![](media/image5564.png)\


The full-load evaporator capacity at specific temperature operating conditions is then given by:

**![](media/image5565.png)** **.**

The part-load ratio is set to be between zero and the maximum, and the evaporator heat transfer rate is computed by:

**![](media/image5566.png)** **The evaporator inlet and outlet temperature difference is then given by:**

**![](media/image5567.png)**

Once the part-load ratio is calculated the cycling ratio and false loading rate are computed by:

![](media/image5568.png)\


![](media/image5569.png) .

The compressor power demand is then computed by:

**![](media/image5570.png)**

The heat transfer rate of the chiller-heater condenser is then computed as follows:

![](media/image5571.png)\


Once condenser available heating capacity is determined, the model calculates current chiller-heater's condenser heat transfer rate based on the total heating load required a central heat pump system to meet as well as available heating capacity of the chiller-heater. The maximum condenser temperature difference between the entering hot water temperature (*T~h~~w,e~*) and the leaving hot water temperature (*T~h~~w,l~*) obtained from the plant loop setpoint temperature can also be obtained. It then calculates condenser water mass flow rate for variable flow control chiller-heaters and the hot water temperature difference for constant flow control chiller-heaters, setting the cooling load that each chiller-heater needs to meet equal the evaporator heat transfer rate.

As for variable flow control chiller-heaters, the condenser water mass flow rate is computed as follows:

**![](media/image5572.png)** **.**

The condenser water mass flow rate calculated is then compared to the maximum available mass flow rate for individual chiller-heaters. If the calculated one is bigger than the maximum, the model sets the condenser water mass flow rate equal the maximum. It then adjusts the hot water temperature difference at the maximum mass flow rate. If the adjusted temperature difference also exceeds the maximum, the model finally adjusts the condenser heat transfer rate at the maximum allowable conditions as follows:

**![](media/image5573.png)** **.**

As for constant flow control chiller-heaters, the model calculates condenser temperature difference as follows:

**![](media/image5574.png)** **.**

The temperature difference calculated is then compared to maximum hot water temperature difference. If the calculated one is bigger than the maximum, the model sets the hot water temperature difference equal the maximum, and then adjusts the condenser heat transfer rate at the given conditions as follows:

**![](media/image5575.png)** **.**

Finally, the total heat transfer energy by the evaporator and condenser can then be calculated as follows:

**![](media/image5576.png)**

**![](media/image5577.png)** **.**

## References

Central Geothermal Systems, Applications Engineering Manual, Trane Company, April 2010, SYS-APM009-EN.