# Component Sizing

## Introduction

In EnergyPlus each HVAC component sizes itself. Each component module contains a sizing subroutine. When a component is called for the first time in a simulation, it reads in its user specified input data and then calls the sizing subroutine. This routine checks the autosizable input fields for missing data and calculates the data when needed.

A number of high-level variables are used in the sizing subroutines.

*CurDuctType* (in *DataSizing*) contains the information about the current duct type. The types can be *main*, *cooling*, *heating* or *other*.

*CurZoneEqNum* (in *DataSizing*) is the current zone equipment set index and indicates that the component is a piece of zone equipment and should size itself using the zone sizing data arrays.

*CurSysNum* (in *DataSizing*) is the current air loop index and indicates that the component is part of the primary air system and should size itself using the system sizing data arrays.

## Fan Sizing

Fan sizing is done in subroutine *SizeFan*.

### Max Flow Rate

If the fan is part of the central air system then check the duct type.

For duct type = *main, other* or default

*![](media/image1989.png)*

*F*or duct type=*cooling*

*![](media/image1990.png)*

*F*or duct type=*heating*

![](media/image1991.png)\


If the fan is zone equipment then check whether it is part of a component that only does heating.

For heating only ![](media/image1992.png) ;

Otherwise ![](media/image1993.png)

If the max fan flow rate is less than *SmallAirVolFlow* the max flow rate is set to zero.

## Coil:Cooling:Water

*The sizing is done in subroutine SizeWaterCoil* of module *WaterCoils*

### Design Water Flow Rate (m^3^/s)

#### System Coils

The design water volumetric flow rate is calculated using:

![](media/image1994.png)\


*T~w,des~* is just the *Loop Design Temperature Difference* user input from *Sizing:Plant* (if the coil is in the outside air stream, ½ the *Loop Design Temperature Difference* is used). The design coil load *Load~coil,des~* is calculated from:

![](media/image1995.png)\


The design air mass flow rate depends on the location of the coil. If the coil is in the outside air stream the flow rate is set to *~air~DesOutAirVolFlow~sys~* (the design outside air volumetric flow for the system). If the coil is in a cooling duct the flow rate is set to *~air~DesCoolVolFlow~sys~*. If the coil is in a heating duct the flow rate is set to *~air~DesHeatVolFlow~sys~*. If the coil is in the main duct (or any other kind of duct) the flow rate is set to *~air~DesMainVolFlow~sys~*.

To obtain the inlet and outlet enthalpies, we need the inlet and outlet temperatures and humidity ratios. The inlet and outlet conditions depend on whether the coil is in the outside air stream and if it is not, whether or not there is outside air preconditioning.

Coil in outside air stream

*T~air,in,des~* = *CoolOutTemp~sys~* (the outside air temperature at the design cooling peak)

*T~air,out,des~* = *PrecoolTemp~sys~*  (the specified Precool Design Temperature from the *Sizing:System* object).

*W~air,in,des~* = *CoolOutHumRat~sys~* (the outside humidity ratio at the design cooling peak)

*W~air,out,des~* = *PrecoolHumRat~sys~* (the specified Precool Design Humidity Ratio from the *Sizing:System* object)

Coil in main air stream, no preconditioning of outside air

*T~air,in,des~* = *CoolMixTemp~sys~* (the mixed air temperature at the design cooling peak)

*W~air,in,des~* = *CoolMixHumRat~sys~* (the mixed air humidity ratio at the design cooling peak)

*T~air,out,des~* = *CoolSupTemp~sys~*  (the specified Central Cooling Design Supply Air Temperature from the *Sizing:System* object)

*W~air,out,des~* = *CoolSupHumRat~sys~* (the specified Central Cooling Design Supply Air Humidity Ratio from the *Sizing:System* object)

Coil in main air stream, outside air preconditioned. The outside air fraction is calculated as *Frac*~oa~ **= *DesOutAirVolFlow~sys~* / *DesVolFlow*. *DesVolFlow* is just *AirMassFlowRate~coil,des~* / *~air~*.

*T~air,in,des~*=*Frac*~oa~*PrecoolTemp~sys~* + (1. *Frac~oa~*)*CoolRetTemp~sys~*(see Table 41.  System Sizing Data)

*W~air,in,des~*=*Frac~oa~PrecoolHumRat~sys~* + (1. *Frac~oa~*)*CoolRetHumRat~sys~*

*T~air,out,des~* = *CoolSupTemp~sys~* (the specified Central Cooling Design Supply Air Temperature from the *Sizing:System* object)

*W~air,out,des~* = *CoolSupHumRat~sys~* (the specified Central Cooling Design Supply Air Humidity Ratio from the *Sizing:System* object)

With the inlet and outlet conditions established, we can obtain the inlet and outlet enthalpies:

*h~air,coil,des,in~* = *PsyHFnTdbW*(*T~air,in~~,des~*, *W~air,in~~,des~*)

*h~air,coil,des,out~*~~= *PsyHFnTdbW*(*T~air,out~~,des~*, *W~air,out~~,des~*)

where *PsyHFnTdbW* is the EnergyPlus function for calculating air specific enthalpy given the air temperature and humidity ratio. We now have all we need to calculate *Load~coil,des~* and *WaterVolFlowRate~coil,des~*.

#### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, the water flow rate is set equal to the terminal unit's chilled water flow rate. Otherwise (e.g., the zone-level coil is part of *ZoneHVAC:FourPipeFanCoil, ZoneHVAC:UnitVentilator or ZoneHVAC:VentilatedSlab*) the calculation is similar to that at the system level. A design load is calculated:

![](media/image1996.png)\


Where:

AirMassFlowRate~coil,des~~~= DesCoolMassFlow~zone~ (see Table 40.  Zone Sizing Data)

h~air,coil,des,in~ = PsyHFnTdbW(T~air,in~~,des~, W~air,in~~,des~)

h~air,coil,des,out~= PsyHFnTdbW(T~air,out~~,des~, W~air,out~~,des~)

T~air,in~~,des~ = DesCoolCoilInTemp~zone~ (see Table 40)

W~air,in~~,des~ = DesCoolCoilInHumRat~zone~ (see Table 40)

T~air,out,des~ = CoolDesTemp~zone~ (user input from Zone:Sizing object)

W~air,out,des~ = CoolDesHumRat~zone~ (user input from Zone:Sizing object)

![](media/image1997.png)\


where *T~w,des~* is the *Loop Design Temperature Difference* user input from the *Sizing:Plant* object*.*

### Design Air Flow Rate

#### System Coils

The design air volumetric flow rate depends on the location of the coil. If the coil is in the outside air stream the flow rate is set to *DesOutAirVolFlow~sys~*. If the coil is in a cooling duct the flow rate is set to *DesCoolVolFlow~sys~*. If the coil is in a heating duct the flow rate is set to *DesHeatVolFlow~sys~*. If the coil is in the main duct (or any other kind of duct) the flow rate is set to *DesMainVolFlow~sys~*.

#### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, the design air volumetric flow rate is set equal to the flow rate of the terminal unit. For all other zone coils it is set equal to:

Max(DesCoolMassFlow~zone~,DesHeatMassFlow~zone~)  ~air~

### Design Inlet Air Temperature

#### System Coils

The inlet air temperature depends on whether the coil is in the outside air stream and if it is not, whether or not there is outside air preconditioning.

Coil in outside air stream: *T~air,in~* = *CoolOutTemp~sys~* (the outside air temperature at the design cooling peak).

Coil in main air stream, no preconditioning of outside air: *T~air,in~* = *CoolMixTemp~sys~* (the mixed air temperature at the design cooling peak).

Coil in main air stream, outside air preconditioned. The outside air fraction is calculated as *Frac*~oa~ **= *DesOutAirVolFlow~sys~* / *DesVolFlow*. *DesVolFlow* is just *AirMassFlowRate~coil,des~* / *~air~*. Then

*T~air,in~*=*Frac*~oa~*PrecoolTemp~sys~* + (1. *Frac~oa~*)*CoolRetTemp~sys~*

#### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, the Design Inlet Air Temperature is set equal to *ZoneTempAtCoolPeak~zone~* (see Table 40.  Zone Sizing Data). For all other zone coils, it is set equal to *DesCoolCoilInTemp~zone~* (see Table 40).

### Design Outlet Air Temperature

#### System Coils

The outlet air temperature depends on whether the coil is in the outside air stream.

Coil in outside air stream: *T~air,out,des~* = *PrecoolTemp~sys~*  (the specified Precool Design Temperature from the *Sizing:System* object).

Coil in main air stream: *T~air,out,des~* = *CoolSupTemp~sys~*  (the specified Central Cooling Design Supply Air Temperature from the *Sizing:System* object)

#### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, then:

![](media/image1998.png)\


![](media/image1999.png)\


where *CoolVolFlow~coil,air,des~* is the user input or previously autosized coil Design Air Flow Rate. For all other zone coils the Design Outlet Air Temperature is set to *CoolDesTemp~zone~* (see Table 40.  Zone Sizing Data).

### Design Inlet Air Humidity Ratio

#### System Coils

The inlet air humidity ratio depends on whether the coil is in the outside air stream and if it is not, whether or not there is outside air preconditioning.

Coil in outside air stream: *W~air,in,des~* = *CoolOutHumRat~sys~* (the outside humidity ratio at the design cooling peak).

Coil in main air stream, no preconditioning of outside air: *W~air,in,des~* = *CoolMixHumRat~sys~* (the mixed air humidity ratio at the design cooling peak).

Coil in main air stream, outside air preconditioned. The outside air fraction is calculated as *Frac*~oa~ **= *DesOutAirVolFlow~sys~* / *DesVolFlow*. *DesVolFlow* is just *AirMassFlowRate~coil,des~* / *~air~*. Then

*W~air,in,des~*=*Frac~oa~PrecoolHumRat~sys~* + (1. *Frac~oa~*)*CoolRetHumRat~sys~*

#### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, the Design Inlet Air Humidity Ratio is set equal to *ZoneHumRatAtCoolPeak~zone~* (see Table 40.  Zone Sizing Data). For all other zone coils, it is set equal to *DesCoolCoilInHumRat~zone~* (see Table 40).

### Design Outlet Air Humidity Ratio

#### System Coils

The outlet air humidity ratio depends on whether the coil is in the outside air stream.

Coil in outside air stream: *W~air,out,des~* = *PrecoolHumRat~sys~* (the specified Precool Design Humidity Ratio from the *Sizing:System* object)

Coil in main air stream: *W~air,out,des~* = *CoolSupHumRat~sys~* (the specified Central Cooling Design Supply Air Humidity Ratio from the *Sizing:System* object)

#### Zone Coils

The Design Outlet Air Humidity Ratio is set equal to *CoolDesHumRat~zone~* (user input from *Zone:Sizing*).

### Design Inlet Water Temperature

#### System Coils

The Design Inlet Water Temperature is set to the *Design Loop Exit Temperature* specified in the *Sizing*:*Plant* object for the water loop serving this coil.

#### Zone Coils

The Design Inlet Water Temperature is set to the *Design Loop Exit Temperature* specified in the *Sizing*:*Plant* object for the water loop serving this coil.

## Coil:Cooling:Water:DetailedGeometry Sizing

The sizing is done in subroutine *SizeWaterCoil*

### Max Water Flow Rate of Coil

The calculation is identical to that done for *Coil:Cooling:Water*.

### Number of Tubes per Row

![](media/image2000.png)\


*N~tube/row~*=**Max**(*N~tube/row~*,3)

### Fin Diameter

Depending on the duct type, get the coil design air flow rate.

For duct type = *main, other* or default

![](media/image2001.png)\


*for duct type=cooling*

![](media/image2002.png)\


*for duct type=heating*

![](media/image2003.png)\


![](media/image2004.png)\


### Minimum Air Flow Area

Depending on the duct type, get the coil design air flow rate.

For duct type = *main, other* or default

![](media/image2005.png)\


*for duct type=cooling*

![](media/image2006.png)\


*for duct type=heating*

![](media/image2007.png)\


![](media/image2008.png)\


### Fin Surface Area

Depending on the duct type, get the coil design air flow rate.

For duct type = *main, other* or default

![](media/image2009.png)\


*for duct type=cooling*

![](media/image2010.png)\


*for duct type=heating*

![](media/image2011.png)\


![](media/image2012.png)\


### Total Tube Inside Area

*A~tube,total inside~*=4.4*D~tube,inside~N~tube rows~N~tubes/row~*

Where *D~tube,inside~* is the tube inside diameter.

### Tube Outside Surf Area

*A~tube,outside~*=4.1*D~tube,outside~N~tube rows~N~tubes/row~*

Where *D~tube,outside~* is the tube outside diameter.

### Coil Depth

*Depth~coil~*=*Depth~tube spacing~ N~tube rows~*

## Coil:Cooling:WaterToAirHeatPump:EquationFit Sizing

The sizing is done in subroutine *SizeHVACWaterToAir*

### Rated Air Flow Rate

The calculation is identical to that done for *Coil:Cooling:Water*.

### Rated Water Flow Rate

The calculation is identical to that done for *Coil:Cooling:Water*, which is the coil design load divided by the *Loop Design Temperature Difference* user input from *Sizing:Plant.* If there is a companion heating coil, the heating coil design load is used so that both modes will have the same rated water flow rate. For sizing the plant loop serving this coil, only one half of this flow rate is used since both the cooling and heating coil will save a flow rate but only one of these coils will operate at a time.

### Rated Total Cooling Capacity

The calculation for coil operating temperatures (inlet and outlet) are identical to that done for *Coil:Cooling:Water*. The following calculations are then performed to determine the rated total cooling capacity.

![](media/image2013.png)\


![](media/image2014.png)\


where:

![](media/image2015.png) ratio of load-side inlet air wet-bulb temperature in Kelvin to a reference temperature

![](media/image2016.png)  ratio of source-side inlet water temperature in Kelvin to a reference temperature

TCC1 = user input for Total Cooling Capacity Coefficient 1

TCC2 = user input for Total Cooling Capacity Coefficient 2

TCC3 = user input for Total Cooling Capacity Coefficient 3

TCC4 = user input for Total Cooling Capacity Coefficient 4

TCC5 = user input for Total Cooling Capacity Coefficient 5

![](media/image2017.png)\


The 4^th^ and 5^th^ coefficient (TCC4 and TCC5) used in the above equation are multipliers for the load-side and source-side flow ratios, respectively. For sizing, these ratios are assumed to be 1.

The enthalpy of the entering air is then compared with the enthalpy of the exiting air. The calculations for air enthalpy are identical to that done for *Coil:Cooling:Water*. If the entering air enthalpy is less than the exiting air enthalpy, a reference value of 48,000 J/kg is used as the entering air enthalpy. If the TotCapTempModFac calculation above yields 0 as the result, a value of 1 is used in the following calculation. If the design air mass flow rate is determined to be less than a very small flow value (0.001 kg/s) or the capacity calculated here is less than 0, the coil total cooling capacity is set equal to 0.

![](media/image2018.png)\


### Rated Sensible Cooling Capacity

The calculation for coil operating temperatures (inlet and outlet) are identical to that done for *Coil:Cooling:Water*. The following calculations are then performed to determine the rated sensible cooling capacity.

![](media/image2019.png)\


![](media/image2020.png)\


where:

![](media/image2021.png) ratio of load-side inlet air dry-bulb temperature in Kelvin to a reference temperature

SCC1 = user input for Sensible Cooling Capacity Coefficient 1

SCC2 = user input for Sensible Cooling Capacity Coefficient 2

SCC3 = user input for Sensible Cooling Capacity Coefficient 3

SCC4 = user input for Sensible Cooling Capacity Coefficient 4

SCC5 = user input for Sensible Cooling Capacity Coefficient 5

SCC6 = user input for Sensible Cooling Capacity Coefficient 6

![](media/image2022.png)\


The 5^th^ and 6^th^ coefficient (SCC5 and SCC6) used in the above equation are multipliers for the load-side and source-side flow ratios, respectively. For sizing, these ratios are assumed to be 1.

The dry-bulb temperature of the entering air is then compared with the dry-bulb temperature of the exiting air. The calculations for air dry-bulb temperature are identical to that done for *Coil:Cooling:Water*. If the entering air dry-bulb temperature is less than the exiting air dry-bulb temperature, a reference value of 24 C is used as the entering air dry-bulb temperature. If the SensCapTempModFac calculation above yields 0 as the result, a value of 1 is used in the following calculation. If the design air mass flow rate is determined to be less than a very small flow value (0.001 kg/s) or the capacity calculated here is less than 0, the coil sensible cooling capacity is set equal to 0.

![](media/image2023.png)\


## Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit Sizing

For the cooling coil of VS WSHP, we specify a nominal speed level. During the sizing calculation, the Rated Air Volume Flow Rate, the Rated Water Volume Flow Rate and the Rated Total Cooling Capacity at the Selected Nominal Speed Level are determined in the same way as the *Coil:Cooling:WaterToAirHeatPump:EquationFit* object. The sensible heat transfer rate is not allowed for auto-sizing, instead, it is a function of the rated air and water flow rates, rated total cooling capacity and the Reference Unit SHR at the nominal speed level. The default nominal speed level is the highest speed. However, the model allows the user to select a nominal speed level rather than the highest.

### Rated Air Flow Rate

The calculation is identical to that done for *Coil:Cooling:WaterToAirHeatPump:EquationFit*.

### Rated Water Flow Rate

The calculation is identical to that done for *Coil:Cooling:WaterToAirHeatPump:EquationFit* , which is the coil design load divided by the *Loop Design Temperature Difference* user input from *Sizing:Plant.* If there is a companion heating coil, the heating coil design load is used so that both modes will have the same rated water flow rate. For sizing the plant loop serving this coil, only one half of this flow rate is used since both the cooling and heating coil will save a flow rate but only one of these coils will operate at a time.

### Rated Total Cooling Capacity

The calculation for coil operating temperatures (inlet and outlet) are identical to that done for *Coil:Cooling:WaterToAirHeatPump:EquationFit*. The calculations for air enthalpy are similar to that done for *Coil:Cooling:WaterToAirHeatPump:EquationFit.* The difference is in calculating the total cooling capacity temperature modifier function at the selected nominal speed level, as below:

![](media/image2024.png)\


where

WB~i~ = wet-bulb temperature of the air entering the heating coil, °C

EWT = entering water temperature, °C

a-f = regression curve-fit coefficients.

If the entering air enthalpy is less than the exiting air enthalpy, a reference value of 48,000 J/kg is used as the entering air enthalpy. If the *TotCapTempModFac* calculation above yields 0 as the result, a value of 1 is used in the following calculation. If the rated air mass flow rate is determined to be less than a very small flow value (0.001 kg/s) or the capacity calculated here is less than 0, the coil total cooling capacity is set equal to 0.

*If H~in~ > H~out~ Then*

![](media/image2025.png)\


*Else*

![](media/image2026.png)\


*End If*

## Coil:Heating:WaterToAirHeatPump:EquationFit Sizing

The sizing is done in subroutine *SizeHVACWaterToAir.*

### Rated Air Flow Rate

The calculation is identical to that done for *Coil:Cooling:Water*.

### Rated Water Flow Rate

The calculation is identical to that done for *Coil:Cooling:Water* , which is the coil design load divided by the *Loop Design Temperature Difference* user input from *Sizing:Plant.* For sizing the plant loop serving this coil, only one half of this flow rate is used since both the cooling and heating coil will save a flow rate but only one of these coils will operate at a time.

### Rated Total Heating Capacity

The rated total heating capacity is set equal to the rated total cooling capacity.

## Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit Sizing

**For the heating coil of VS WSHP, we specify a nominal speed level. During the sizing calculation, the Rated Air Volume Flow Rate and the Rated Water Volume Flow Rate are determined in the same way as the** *Coil:Heating:WaterToAirHeatPump:EquationFit* object. On the other hand, the Rated Heating Capacity at the Selected Nominal Speed Level should be the same as the total cooling capacity of its corresponding cooling coil, which has to be sized first. The default nominal speed level will be the highest speed. However, the model allows the user to select a nominal speed level rather than the highest.

### Rated Air Flow Rate

The calculation is identical to that done for Coil:Cooling:WaterToAirHeatPump:EquationFit.

### Rated Water Flow Rate

The calculation is identical to that done for Coil:Cooling:WaterToAirHeatPump:EquationFit, which is the coil design load divided by the *Loop Design Temperature Difference* user input from *Sizing:Plant.* For sizing the plant loop serving this coil, only one half of this flow rate is used since both the cooling and heating coil will save a flow rate but only one of these coils will operate at a time.

### Rated Total Heating Capacity

The rated total heating capacity is set equal to the rated total cooling capacity.

## Coil:Heating:Water Sizing

The sizing is done in subroutine *SizeWaterCoil*.

### Max Water Flow Rate of Coil

#### System Coils

With the coil load from the system design data array and the user specified (in a Sizing:Plant object) design hot water temperature fall, calculate the max water flow rate:

![](media/image2027.png)\


#### Zone Coils

Using the zone design coil inlet and supply air conditions calculate the design coil load.

If the coil is not part of an induction unit then obtain the coil inlet temperature from the zone design data array;

*T~in,air~= DesHeatCoilInTemp~zone~*

If the coil is part of an induction unit take into account the induced air:

*Frac~minflow~*=*MinFlowFrac~zone~*

*T~in,air~*= *DesHeatCoilInTemp~zone~ Frac~minflow~* +

*ZoneTempAtHeatPeak~zone~*(1 *Frac~minflow~*)

*T~out,air~=HeatDesTemp~zone~*

*W~out,air~= HeatDesHumRat~zone~*

If the coil is part of a terminal unit the mass flow rate is determined by the volumetric flow rate of the terminal unit:

![](media/image2028.png)\


Otherwise the design flow is obtained from the zone design data array:

![](media/image2029.png)\


![](media/image2030.png)\


Here *c~p,air~* is calculated at the outlet humidity and the average of the inlet and outlet temperatures.

With the coil load and the user specified (in a Sizing:Plant object) design hot water temperature decrease, calculate the max water flow rate:

![](media/image2031.png)\


### UA of the Coil

To obtain the UA of the coil, we specify the model inputs (other than the UA) at design conditions and the design coil load that the coil must meet. Then we numerically invert the coil model to solve for the UA that will enable the coil to meet the design coil load given the specified inputs.

#### System Coils

The design coil load is the system design sensible cooling capacity;

*Q~coil,des~*= *HeatCap~sys~*

The required inputs for the simple coil model are:

*T~in,air~*= *HeatMixTemp~sys~*

*W~in,air~*= *HeatMixHumRat~sys~*

*T~in,water~*= *ExitTemp~plt,hw,des~*

![](media/image2032.png)\


Depending on the duct type, get the coil design air flow rate.

For duct type = *main, other* or default

*![](media/image2033.png)*

*for duct type=cooling*

*![](media/image2034.png)*

*for duct type=heating*

![](media/image2035.png)\


We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design coil load and the coil output divided by the design coil load. The residual is calculated in the function *SimpleHeatingCoilUAResidual*.

#### Zone Coils

If the coil is not part of an induction unit then obtain the coil inlet temperature from the zone design data array;

*T~in,air~= DesHeatCoilInTemp~zone~*

If the coil is part of an induction unit take into account the induced air:

*Frac~minflow~*=*MinFlowFrac~zone~*

*T~in,air~*= *DesHeatCoilInTemp~zone~ Frac~minflow~* +

*ZoneTempAtHeatPeak~zone~*(1 *Frac~minflow~*)

*W~in,air~*= *DesHeatCoilInHumRat~zone~*

*T~in,water~*= *ExitTemp~plt,hw,des~*

![](media/image2036.png)\


*T~out,air~=HeatDesTemp~zone~*

*W~out,air~= HeatDesHumRat~zone~*

If the coil is part of a terminal unit the mass flow rate is determined by the volumetric flow rate of the terminal unit:

![](media/image2037.png)\


Otherwise the design flow is obtained from the zone design data array:

![](media/image2038.png)\


![](media/image2039.png)\


Here *c~p,air~* is calculated at the outlet humidity and the average of the inlet and outlet temperatures.

We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design coil load and the coil output divided by the design coil load. The residual is calculated in the function *SimpleHeatingCoilUAResidual*.

## Coil:Heating:Steam Sizing

The sizing is done in subroutine *SizeSteamCoil*.

### Maximum Steam Flow Rate

#### System Coils

The maximum steam volumetric flow rate is calculated using:

![](media/image2040.png)\


The steam density (![](media/image2041.png) ) is for saturated steam at 100°C (101325.0 Pa) and *h~fg~* is the latent heat of vaporization of water at 100°C (101325.0 Pa). *C~p,w~* is the heat capacity of saturated water (condensate) at 100°C (101325.0 Pa)  and ![](media/image2042.png)  is the Degree of  Subcooling defined in the Coil:Heating:Steam object input. The design coil load *Load~coil,des~* is calculated from:

![](media/image2043.png)\


The design air mass flow rate depends on the location of the coil (duct type). For duct type =  *main,* the flow rate is set to *~air~DesMainVolFlow~sys~MinSysAirFlowRatio*. If the coil is in a cooling duct the flow rate is set to *~air~DesCoolVolFlow~sys~MinSysAirFlowRatio*. If the coil is in a heating duct the flow rate is set to *~air~DesHeatVolFlow~sys~*. If the coil is in any other kind of duct, the flow rate is set to *~air~DesMainVolFlow~sys~*.

For sizing, the design outlet air temperature (*T~air,coil,des,out~*) is the Central Heating Design Supply Air Temperature specified in the Sizing:System object.

The design inlet air temperature depends on whether the coil is being sized for 100% outdoor air or minimum outdoor air flow (per 100% Outdoor Air in Heating input field in the Sizing:System object).

#. Sizing based on 100% Outdoor Air in Heating

*T~air,coil,des,in~* = *HeatOutTemp~sys~* (the outdoor air temperature at the design heating peak)

#. Sizing based on minimum outdoor air flow. The outdoor air fraction is calculated as *Frac*~oa~ **= *DesOutAirVolFlow~sys~* / *DesVolFlow*. *DesVolFlow* is ![](media/image2044.png) *.*

*T~air,coil,des,in~* =*Frac*~oa~ *HeatOutTemp~sys~* + (1. *Frac~oa~*)*HeatRetTemp~sys~*(see Table 41.  System Sizing Data)

#### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:\** unit (e.g., *AirTerminal:SingleDuct:ConstantVolume:Reheat, AirTerminal:SingleDuct:VAV:Reheat, AirTerminal:SingleDuct:SeriesPIU:Reheat, etc.)*, the maximum steam flow rate is set equal to the terminal unit's maximum steam flow rate. Otherwise (e.g., the zone-level coil is part of *ZoneHVAC:PackagedTerminalAirConditioner, ZoneHVAC:UnitVentilator, ZoneHVAC:UnitHeater or ZoneHVAC:VentilatedSlab*) the calculation is similar to that at the system level. A design load is calculated:

![](media/image2045.png)\


where:

![](media/image2046.png) = *DesHeatMassFlow~zone~* (see Table 40.  Zone Sizing Data)

*T~air,coil,des,in~* = *DesHeatCoilInTemp~zone~* (see Table 40)

*T~air,coil,des,out~* = *HeatDesTemp~zone~* (user input from Sizing:Zone object)

![](media/image2047.png)  = Specific heat of air (evaluated at the average of inlet and outlet air temperatures, and at the zone heating design supply air humidity ratio *HeatDesHumRat~zone~* [user input from Sizing:Zone object])

![](media/image2048.png)\


The terms in the denominator of this equation (*ρ~steam~*, *h~fg~*, etc.) are evaluated in the same way as described above for steam System Coils.

## Sizing of Gas and Electric Heating Coils

The sizing calculation is done in subroutine *SizeHeatingCoil* in module *HeatingCoils*.

### Nominal Capacity of the Coil

#### System Coils

The value is obtained from the system design array.

*Cap~nom~*= *HeatCap~sys~*

#### Zone Coils

The capacity is calculated from the design coil inlet and outlet conditions.

If the coil is not part of an induction unit then obtain the coil inlet temperature from the zone design data array;

*T~in,air~= DesHeatCoilInTemp~zone~*

If the coil is part of an induction unit take into account the induced air:

*Frac~minflow~*=*MinFlowFrac~zone~*

*T~in,air~*= *DesHeatCoilInTemp~zone~ Frac~minflow~* +

*ZoneTempAtHeatPeak~zone~*(1 *Frac~minflow~*)

*T~out,air~=HeatDesTemp~zone~*

*W~out,air~= HeatDesHumRat~zone~*

*Q~coil,des~*=*C~p,air~ DesHeatMassFlow~zone~*(*T~out,air~T~in,air~*)

Here *c~p,air~* is calculated at the outlet humidity and the average of the inlet and outlet temperatures.

## DX Coil Sizing

The sizing calculations are done in subroutine *SizeDXCoil* in module *DXCoils*. This section covers the sizing of the objects

Coil:Cooling:DX:SingleSpeed

Coil:Heating:DX:SingleSpeed

Coil:Cooling:DX:TwoSpeed

### Rated Air Volume Flow Rate

### System Coils

The rated air flow rate is obtained from the system design array.

![](media/image2049.png)\


### Zone Coils

The rated air flow rate is the maximum of the heating and cooling design flow rates from the zone design array.

![](media/image2050.png)\


### Rated Total Cooling Capacity

### System Coils

The rated cooling capacity is obtained by dividing the peak cooling capacity by the *Cooling Capacity Modifier Curve* evaluated at peak mixed wetbulb and outdoor drybulb temperatures.

*T~mix~*= *CoolMixTemp~sys~*

*W~mix~*=*CoolMixHumRat~sys~*

*T~sup~*=*CoolSupTemp~sys~*

*W~sup~*=*CoolSupHumRat~sys~*

*T~outside~*=*CoolOutTemp~sys~*

*~air~*=*PsyRhoAirFnPbTdbW*(*p~air,std~*, *T~mix~*,*W~mix~*)

*h~mix~*= *PsyHFnTdbW*(*T~mix~*,*W~mix~*)

*h~sup~*= *PsyHFnTdbW*(*T~sup~*,*W~sup~*)

*T~mix,wb~*= *PsyTwbFnTdbWPb*(*T~mix~*,*W~mix~*, *p~air,std~*)

*CapModFac*=*CurveValue*(CCapFTemp,*T~mix,wb~*,*T~outside~*)

![](media/image2051.png)\


*CCap~rated~*=*CCap~peak~ CapModFac*

We check that the design volume flow per total capacity is within the prescribed range:

![](media/image2052.png)\


If *FlowCapRatio* < *FlowCapRatio~min~*  then

![](media/image2053.png)\


If *FlowCapRatio* > *FlowCapRatio~max~*  then

![](media/image2054.png)\


where

*FlowCapRatio~min~* = 0.00004027 m^3^/s per watt (300 cfm/ton)

And

*FlowCapRatio~max~*= 0.00006041 m^3^/s per watt (450 cfm/ton)

The sizing calculation for DX cooling coils for 100% dedicated outdor air system (DOAS) are identical to regular DX cooling coils.  However, they operate operate at different flow to capacity ratio ranges and are within the prescribed range below:

*FlowCapRatio~min~* = 0.00001677 m^3^/s per Watt (125 cfm/ton)

And

*FlowCapRatio~max~*= 0.00003355 m^3^/s per Watt (250 cfm/ton)

### Zone Coils

The rated cooling capacity for zone coils is calculated in the same manner as for system coils.

*T~mix~*= *DesCoolCoilInTemp~zone~*

*W~mix~*= *DesCoolCoilInHumRat~zone~*

*T~sup~*= *CoolDesTemp~zone~*

*W~sup~*= *CoolDesHumRat~zone~*

*T~outside~*=*T~outside~,~desday,peak~*

*~air~*=*PsyRhoAirFnPbTdbW*(*p~air,std~*, *T~mix~*,*W~mix~*)

*h~mix~*= *PsyHFnTdbW*(*T~mix~*,*W~mix~*)

*h~sup~*= *PsyHFnTdbW*(*T~sup~*,*W~sup~*)

*T~mix,wb~*= *PsyTwbFnTdbWPb*(*T~mix~*,*W~mix~*, *p~air,std~*)

*CapModFac*=*CurveValue*(CCapFTemp,*T~mix,wb~*,*T~outside~*)

![](media/image2055.png)\


*CCap~rated~*=*CCap~peak~ CapModFac*

We check that the design volume flow per total capacity is within the prescribed range:

![](media/image2056.png)\


If *FlowCapRatio* < *FlowCapRatio~min~*  then

![](media/image2057.png)\


If *FlowCapRatio* > *FlowCapRatio~max~*  then

![](media/image2058.png)\


where

*FlowCapRatio~min~* = 0.00004027 m^3^/s per watt (300 cfm/ton)

And

*FlowCapRatio~max~*= 0.00006041 m^3^/s per watt (450 cfm/ton)

We check the design flow to the total cooling capacity rato for dedicated zone outdoor unit DX cooling coils to be within the limits prescribed below:

*FlowCapRatio~min~* = 0.00001677 m^3^/s per Watt (125 cfm/ton)

And

*FlowCapRatio~max~*= 0.00003355 m^3^/s per Watt (250 cfm/ton)

### Rated Total Heating Capacity

For Coil:Heating:DX:SingleSpeed the rated heating capacity is set equal to the cooling capacity.

### Rated SHR

The rated sensible heat ratio is calculated to be the sensible cooling (from rated inlet conditions to user specified supply conditions) divided by the total cooling (from rated inlet to specified supply).

*T~in,rated~*= 26.6667 ^o^C (80 ^o^F)

*W~in,rated~*= 0.01125 (corresponds to 80 ^o^F drybulb, 67 ^o^F wetbulb)

*C~p,air~*= *PsyCpAirFnWTdb*(*W~in,rated~*, *T~in,rated~*)

For system coils

*T~sup~*=*CoolSupTemp~sys~*

*W~sup~*=*CoolSupHumRat~sys~*

For zone coils

*T~sup~*= *CoolDesTemp~zone~*

*W~sup~*= *CoolDesHumRat~zone~*

Then

*h~rated~*= *PsyHFnTdbW*(*T~in,rated~*, *W~in,rated~*)

*h~sup~*= *PsyHFnTdbW*(*T~sup~*, *W~sup~*)

*h*~rated,sup~=*h~rated~h~sup~*

*Qs~rated,sup~*=*C~p,air~*(*T~in,rated~T~sup~*)

*SHR~rated~*=*Qs~rated,sup~h*~rated,sup~

### Evaporative Condenser Air Volume Flow Rate

The evaporative condenser air volume flow rate (m^3^/s) is set to 0.000114 m^3^/s per watt (850 cfm/ton) times the total rated cooling capacity.

### Evaporative Condenser Air Volume Flow Rate, Low Speed

The evaporative condenser air volume flow rate, low speed (m^3^/s) is set to 1/3 times 0.000114 m^3^/s per watt (850 cfm/ton) times the total rated cooling capacity.

### Evaporative Condenser Pump Rated Power Consumption

The evaporative condenser pump rated power consumption is set equal to the total cooling capacity times 0.004266 watts pump power per watt capacity (15 W/ton).

### Evaporative Condenser Pump Rated Power Consumption, Low Speed

The evaporative condenser pump rated power consumption, low speed, is set equal to 1/3 times the total cooling capacity times 0.004266 watts pump power per watt capacity (15 W/ton).

### Rated Air Volume Flow Rate, low speed

The rated air volume flow rate, low speed, is set equal to 1/3 times the full rated air volume flow rate.

### Rated Total Cooling Capacity, Low Speed

The rated total cooling capacity, low speed, is set equal to 1/3 times the full rated total cooling capacity.

### Rated SHR, low speed

The rated sensible heat ratio, low speed, is set equal to the full speed SHR.

### Resistive Defrost Heater Capacity

For the heat pump the resistive defrost heat capacity is set equal to the cooling capacity.

## DX MultiSpeed Coil Sizing

The sizing calculations are done in subroutine *SizeDXCoil* in module *DXCoils*. This section covers the sizing of the objects

- Coil:Heating:DX:MultiSpeed
- Coil:Cooling:DX: MultiSpeed

The rated air volume flow rate, rated total cooling capacity, rated heating capacity, rated SHR, evaporative condenser air volume flow rate, evaporative condenser pump rated power consumption at the highest speed are sized in the same ways as DX Coil Sizing.

After the sizes are determined at the highest speed, the sizes in the rest of speeds are assumed to

![](media/image2059.png)\


where

Value~n~= Any autosizable variable at Speed n, except SHR

SHR~n~ = SHR~NumberOfSpeed~

n= Speed Index number from 1 to NumberOfSpeed-1

NumberOfSpeed= The highest speed number

## Coil:Cooling:DX:VariableSpeed Sizing

For the variable-speed DX cooling coil, we specify a nominal speed level. During the sizing calculation, the Rated Total Cooling Capacity at the Selected Nominal Speed Level is determined in the same way as the Coil:Cooling:DX:SingleSpeed object. If the user chooses to autosize the Rated Air Volume Flow Rate, the flow rate, as compared to the Rated Total Cooling Capacity, is sized to have the same ratio as the air volume flow rate to the total cooling capacity at the nominal speed, of the Reference Unit. The sensible heat transfer rate is not allowed for auto-sizing, instead, it is a function of the rated air flow, rated total cooling capacity and the Reference Unit SHR at the nominal speed level. The default nominal speed level is the highest speed. However, the model allows the user to select a nominal speed level rather than the highest.

**Rated Total Cooling Capacity**

The calculation for coil operating temperatures (inlet and outlet) are identical to that done for Coil:Cooling:DX:SingleSpeed. The calculations for air enthalpy are similar to that done for Coil:Cooling:DX:SingleSpeed*.* The difference is in calculating the total cooling capacity temperature modifier function at the selected nominal speed level, as below:

![](media/image2060.png)\


where

WB~i~ = wet-bulb temperature of the air entering the cooling coil, °C

DB~o~ = condenser entering air temperature, °C

a-f = regression curve-fit coefficients.

If the entering air enthalpy is less than the exiting air enthalpy, a reference value of 48,000 J/kg is used as the entering air enthalpy. If the *TotCapTempModFac* calculation above yields 0 as the result, a value of 1 is used in the following calculation. If the rated air mass flow rate is determined to be less than a very small flow value (0.001 kg/s) or the capacity calculated here is less than 0, the coil total cooling capacity is set equal to 0.

*If H~in~ > H~out~ Then*

![](media/image2061.png)\


*Else*

![](media/image2062.png)\


*End If*

The other sizing procedures, e.g. evaporative condenser pump, etc., are the same as Coil:Cooling:DX:SingleSpeed.

## Coil:Heating:DX:VariableSpeed Sizing

For the variable-speed DX heating coil, we specify a nominal speed level. During the sizing calculation, the Rated Heating Capacity at the Selected Nominal Speed Level should be the same as the total cooling capacity of its corresponding cooling coil, which has to be sized first. The default nominal speed level will be the highest speed. However, the model allows the user to select a nominal speed level rather than the highest. If the user chooses to autosize the Rated Air Volume Flow Rate, the flow rate, as compared to the Rated Heating Capacity, is sized to have the same ratio as the air volume flow rate to the heating capacity at the nominal speed, of the Reference Unit. The other sizing procedures are the same as Coil:Heating:DX:SingleSpeed.

## Pump Sizing

The loop pumps' autosizable inputs are nominal volumetric flow rate and nominal power consumption. We have

*Eff~tot~*=*Eff~mot~Eff~impeller~*

The motor efficiency is an input. Since we need the total efficiency to calculate the nominal power consumption we assume an impeller efficiency of 0,78 for purposes of sizing.

### Rated Volumetric Flow Rate

This is just set equal to the design loop demand obtained from summing the needs of the components on the demand side of the loop.

### Rated Power Consumption

![](media/image2063.png)\


*H~nom~*, the nominal head, is an input.

## Electric Chiller Sizing

Generally chillers will need nominal cooling capacity, evaporator flow rate and condenser flow rate. All 3 quantities can be straightforwardly obtained using the user specified loop sizing data and the loop design flow rates.

All chillers on a loop are sized to meet the full loop load. If there are multiple chillers on a loop that call for autosizing, they will all be assigned the same cooling capacity and evaporator flow rate.

### Nominal Cooling Capacity

![](media/image2064.png)\


where

*C~p,w~* is the specific heat of water at 5 ^o^C;

*~w~* is the density of water at standard conditions (5.05 ^o^C);

*T~loop,des~* is the chilled water loop design temperature rise;

![](media/image2065.png) is the loop design volumetric flow rate.

### Design Evaporator Volumetric Water Flow Rate

![](media/image2066.png)\


### Design Condenser Volumetric Water Flow Rate

![](media/image2067.png)\


where

*C~p,w~* is the specific heat of water at design condenser inlet temperature;

*~w~* is the density of water at standard conditions (5.05 ^o^C);

*T~loop,des~* is the chilled water loop design temperature rise;

*COP~chiller,nom~* is the chiller nominal COP.

Boiler Sizing

Generally boilers will need nominal heating capacity and rate. Both quantities can be straightforwardly obtained using the user specified loop sizing data and the loop design flow rates.

All boilers on a loop are sized to meet the full loop load. If there are multiple boilers on a loop that call for autosizing, they will all be assigned the same heating capacity and flow rate.

### Nominal Capacity

![](media/image2068.png)\


where

*C~p,w~* is the specific heat of water at the boiler design outlet temperature;

*~w~* is the density of water at standard conditions (5.05 ^o^C);

*T~loop,des~* is the hot water loop design temperature decrease;

![](media/image2069.png) is the loop design volumetric flow rate.

### Design Evaporator Volumetric Water Flow Rate

![](media/image2070.png)\


## Plant Heat Exchanger Sizing

The sizing of plant heat exchanger component (object: HeatExchanger:FluidToFluid) involves determining design flow rates for both sides, a UA value, and a nominal capacity for reporting.  The component has a sizing factor for fine control and uses the design temperatures defined in the Sizing:Plant object.

The Loop Supply Side design flow rate, ![](media/image2071.png) , is set equal to the design flow rate for that loop, multiplied by the component sizing factor, ![](media/image2072.png) .

![](media/image2073.png)\


The Loop Demand Side design flow rate,![](media/image2074.png)  , is set equal to the Loop Supply Side design flow rate.

![](media/image2075.png)\


The design heat transfer capacity and UA for the heat exchanger are calculated using the design temperatures for the two plant loops.  The loop design temperature difference for the Loop Supply Side, ![](media/image2076.png) , is used to determine a nominal capacity.

![](media/image2077.png)\


A loop-to-loop design temperature difference, ![](media/image2078.png) , is determined depending on the nature of the plant loop connected to the Loop Supply Side.  The Sizing:Plant object includes  classifications for the type of loop that include Heating, Steam, Cooling, or Condenser. For Cooling and Condenser loop types, the loop design temperature difference is added to the design exit temperature for the Loop Supply Side, ![](media/image2079.png) .  For Heating and Stem loop types, the loop design temperature difference is subtracted from the design exit temperature.  This adjusted supply side temperature is then compared to the design exit temperature for the Loop Demand Side,![](media/image2080.png)  .

![](media/image2081.png)     (Cooling, Condenser)

![](media/image2082.png)     (Heating, Steam)

![](media/image2083.png)\


The UA (U-Factor Time Area Value) is determined by assuming that the target capacity can be delivered for the loop-to-loop temperature difference which after substituting and rearranging becomes:

![](media/image2084.png)\


A nominal capacity for the heat exchanger is determined from the design flow rates and UA (regardless of if they were automatically sized or input by the user) and the expected operating temperatures of the two loops.  The loop operating temperatures are obtained from the input in Sizing:Plant object if it is present for that loop.  If no Sizing:Plant is present then the loop's overall setpoint is used (if the loop's load scheme is DualSetpointDeadband then the average of the high and low setpoints is used).  The full heat exchanger model is then calculated for the maximum loop flow rates and expected loop temperatures as inlets to the heat exchanger.  The absolute value for the model result for heat transfer rate is then used as the capacity of the heat exchanger.  This capacity is reported and may be used for controls based on operation scheme.

## Humidifier Sizing

The rated power, or nominal electric power input of an Electric Steam Humidifier (Humidifier:Steam:Electric) is calculated from user specified rated capacity (m^3^/s) and the enthalpy change of the water from a reference temperature (20.0°C) to saturated steam at 100.0°C. Autosizing procedure assumes that electrical heating element in the humidifier heat the water from the reference temperature and generate saturated steam at 100°C, and electric to thermal energy conversion efficiency of 100.0%.

### Rated Power

![](media/image2085.png)\


where

*C~p,w~ is the specific heat of water at average temperature ((100+20)/2 = 60.0 °C), (J/kgK);*

*~w~ is the density of water at standard conditions (5.05 °C);*

*T~w~  is the sensible temperature rise of water (100.0 – 20.0=80.0 °C);*

*![](media/image2086.png)*  *is the rated capacity of the humidifier in volumetric flow rate.*

*h~fg~ is the latent heat of vaporization of water at 100.0°C, (J/kg);*

### Rated Capacity

![](media/image2087.png)\


where

*m*w *is water mass flow rate, kg/s;*

*m*a *is design air mass flow rate, kg/s;*

*ω~o~ is design outlet humidity ratio, kg-water/kg-air;*

*ω~i~ is design inlet humidity ratio, kg-water/kg-air.*

The air mass flow rate and humidity ratios are determined based upon zone design conditions. If the unit is part of zone equipment, then:

![](media/image2088.png)\


![](media/image2089.png)\


 ![](media/image2090.png)

where

*~a~ is the density of air at design conditions, kg/s.*

If the unit is part of the central air system, then check if outdoor air system is present. If outdoor air system is part of the air loop and design outdoor air flow rate is greater than zero, then:

![](media/image2091.png)\


![](media/image2092.png)\


 ![](media/image2093.png)

Otherwise, air mass flow rate is determined as follows:

*for duct type = main*

*![](media/image2094.png)*

*for duct type = cooling*

![](media/image2095.png)\


*for duct type = heating*

![](media/image2096.png)\


*for duct type = other*

![](media/image2097.png) ,

and the humidity ratios are:

![](media/image2098.png)\


![](media/image2099.png)\


## Cooling Tower Sizing

The quantities needed to autosize a cooling tower include the design water flow rate, the nominal fan power and air flow rate, and the tower UA. This data may be need to be given at more than one operating point:, for instance – high speed fan, low speed fan and free convection.

EnergyPlus provides two input choices: the user can input the design water flow rate and tower UA at each operating point or the tower nominal capacity (and let the program calculate the water flow rate and UA). Choice of input method will affect the sizing calculations in ways noted below.

### Design Water Flow Rate

If *Tower Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* then

![](media/image2100.png)\


If *Tower Performance Input Method* = *NominalCapacity* then

![](media/image2101.png)\


where 5.38210^-08^ is m^3^/s per watt corresponds to the rule-of-thumb of sizing the tower flow rate at 3 gallons per minute per ton. For the CoolingTower:VariableSpeed:Merkel model with NominalCapacity input method, the user can input the value used to scale design water flow rate from nominal capacity and the default is 5.38210^-08^ m^3^/s/W.

### Fan Power at Design Air Flow Rate

The nominal fan power is sized to be 0.0105 times the design load.

If *Tower Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* then

![](media/image2102.png)\


where

*C~p,w~* is the specific heat of water at the condenser loop design exit temperature;

*~w~* is the density of water at standard conditions (5.05 ^o^C);

*T~loop,des~* is the condenser water loop design temperature rise;

Finally

![](media/image2103.png)\


For the CoolingTower:VariableSpeed:Merkel model, the design fan power is determined using a scaling factor, in units of Watts per Watt, that can be input by the user.  The default value is 0.0105 which is the same as above.

### Design Air Flow Rate

We assume a fan efficiency of 0.5 and a fan pressure rise of 190 Pascals. Then

![](media/image2104.png)\


where

~air~ is the density of air at standard conditions.

For the CoolingTower:VariableSpeed:Merkel model, the design air flow rate is determined from the nominal capacity using a scaling factor, ![](media/image2105.png) ,in units of m^3^/s/W.  The default value is 2.76316\*10^-5^.  When the input field is left blank, the default is used as follows

![](media/image2106.png)\


where, ![](media/image2107.png)  is the standard barometric pressure for the location's elevation.

When the input field is filled with a hard value, the pressure scaling is not used

![](media/image2108.png)\


### Tower UA Value at Design Air Flow Rate

To obtain the UA of the tower, we specify the model inputs (other than the UA) at design conditions and the design tower load that the tower must meet. Then we numerically invert the tower model to solve for the UA that will enable the tower to meet the design tower load given the specified inputs.

The design tower load is:

*for Tower Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate*

![](media/image2109.png)\


*for Tower Performance Input Method* = *NominalCapacity*

![](media/image2110.png) (to allow for compressor heat)

Where, ![](media/image2111.png)  is the ratio of actual heat rejection capacity to nominal capacity.  This ratio is available as a user input with a default value of 1.25 (to allow for compressor heat).

Then we assign the inputs needed for the model.

*T~in,air~*=35 ^o^C (95 ^o^F design air inlet temperature)

*T~in,air,wb~*=25.6 ^o^C (78 ^o^F design air inlet wetbulb temperature)

*W~in~* is calculated from the entering air drybulb and wetbulb.

The  inlet water mass flow rate is just the design volumetric flow rate times the density of water.

The inlet water temperature is set slightly differently for the 2 input methods. For

*UFactorTimesAreaAndDesignWaterFlowRate*

*T~in,water~*=*T~loop,exit,des~T~loop,des~*

*NominalCapacity*

*T~in,water~*=35 ^o^C (95 ^o^F design inlet water temperature).

*We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design tower load and the tower output divided by the design tower load. The residual is calculated in the function *SimpleTowerUAResidual.*

### Air Flow Rate at Low Fan Speed

The nominal air flow rate at low fan speed is set to a fraction of the full speed air flow rate. The fraction is available for user input in the field called Low Fan Speed Air Flow Rate Sizing Factor. The default is 0.5.

### Fan Power at Low Fan Speed

The fan power at low fan speed is set to a fraction of the fan power at full speed.  The fraction is available for user input in the field called Low Fan Speed Fan Power Sizing Factor. The default is 0.16.

### Tower UA Value at Low Fan Speed

For *Tower Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* the low speed UA is set to a fraction of the full speed UA.  The fraction is available for user input in the field called Low Fan Speed U-Factor Times Area Sizing Factor. The default is 0.6. For *Tower Performance Input Method* = *NominalCapacity* the low speed UA is calculated in the same manner as the full speed UA using ![](media/image2112.png)  instead of ![](media/image2113.png) .

### Air Flow Rate in Free Convection Regime

The free convection air flow rate is set to a fraction of the full air flow rate. The fraction is available for user input in the field called Free Convection Regime Air Flow Rate Sizing Factor. The default is 0.1.

### Tower UA Value in Free Convection Regime

For *Tower Performance Input Method* = *UA and Design Water Flow Rate* the low speed UA is set to a fraction of the full speed UA. The fraction is available for user input in the field called Free Convection U-Factor Times Area Value Sizing Factor. The default is 0.1. For *Tower Performance Input Method* = *NominalCapacity* the low speed UA is calculated in the same manner as the full speed UA using ![](media/image2114.png)  instead of ![](media/image2115.png) .

## Fluid Cooler Sizing

The quantities needed to autosize a fluid cooler include the design water flow rate, the nominal fan power, air flow rate, and the fluid cooler UA. This data may need to be given at more than one operating point:, for instance – high speed fan and low speed fan.

EnergyPlus provides two input choices: the user can input the design water flow rate and fluid cooler UA at each operating point or the fluid cooler nominal capacity and the water flow rate (and let the program calculate UA). Choice of input method will affect the sizing calculations in ways noted below.

### Design Water Flow Rate

The design water flow rate is sized as follows

![](media/image2116.png)\


### Fan Power at Design Air Flow Rate

The nominal fan power is sized to be 0.0105 times the design load.

If *Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* then

![](media/image2117.png)\


  where

  *C~p,w~* is the specific heat of water at the condenser loop design exit temperature;

  *~w~* is the density of water at standard conditions (5.05 ^o^C);

  *T~loop,des~* is the condenser water loop design temperature rise;

  Finally

![](media/image2118.png)\


### **Elseif** ****Performance Input Method = NominalCapacity then**

      ![](media/image2119.png)

  Where

      ![](media/image2120.png)  is provided by the user.

### Design Air Flow Rate

- For Performance Input Method = UFactorTimesAreaAndDesignWaterFlowRate

![](media/image2121.png)\


- For Performance Input Method = NominalCapacity

![](media/image2122.png)  is provided by the user.

![](media/image2123.png)\


Where,

*T~in,water~*= Design entering water temperature provided by the user

*T~in,air~*= Design air inlet temperature provided by the user

### Fluid cooler UA Value at Design Air Flow Rate

To obtain the UA of the fluid cooler, we specify the model inputs (other than the UA) at design conditions and the design fluid cooler load that the fluid cooler must meet. Then we numerically invert the fluid cooler model to solve for the UA that will enable the fluid cooler to meet the design fluid cooler load given the specified inputs.

The design fluid cooler load is:

- For Performance Input Method = UFactorTimesAreaAndDesignWaterFlowRate

![](media/image2124.png)\


- For Performance Input Method = NominalCapacity

![](media/image2125.png)  is provided by the user.

Then we assign the inputs needed for the model.

*T~in,air~*= Design air inlet temperature provided by the user

*T~in,air,wb~*= Design air inlet wetbulb temperature provided by the user

*W~in~* is calculated from the entering air drybulb and wetbulb.

The  inlet water mass flow rate is just the design entering volumetric flow rate times the density of water.

The inlet water temperature is set slightly differently for the 2 input methods. For

- UFactorTimesAreaAndDesignWaterFlowRate

![](media/image2126.png)\


- NominalCapacity

![](media/image2127.png)\


*We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design fluid cooler load and the fluid cooler output divided by the design fluid cooler load. The residual is calculated in the function *SimpleFluidCoolerUAResidual.*

### Air Flow Rate at Low Fan Speed

The nominal air flow rate at low fan speed is set to a fraction of the full speed air flow rate. The fraction is available for user input in the field called Low Fan Speed Air Flow Rate Sizing Factor. The default is 0.5.

### Fan Power at Low Fan Speed

The fan power at low fan speed is set to a fraction of the fan power at full speed. The fraction is available for user input in the field called Low Fan Speed Fan Power Sizing Factor. The default is 0.16.

### Fluid cooler UA Value at Low Fan Speed

For *Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* the low speed UA is set to a fraction of the full speed UA. .  The fraction is available for user input in the field called Low Fan Speed U-Factor Times Area Sizing Factor. The default is 0.6. For *Performance Input Method* = *NominalCapacity* the low speed UA is calculated in the same manner as the full speed UA using ![](media/image2128.png)  instead of ![](media/image2129.png) .

## Evaporative Fluid cooler Sizing

The quantities needed to autosize an evaporative fluid cooler include the design water flow rate, the nominal fan power, air flow rate, and the fluid cooler UA. This data may need to be given at more than one operating point:, for instance – high speed fan and low speed fan.

EnergyPlus provides three input choices: the user can input the design water flow rate and fluid cooler UA at each operating point (*UFactorTimesAreaAndDesignWaterFlowRate*) or the fluid cooler design capacity and the water flow rate and let the program calculate UA (*UserSpecifiedDesignCapacity*) or only the fluid cooler design capacity and let the program calculate UA and the water flow rate (*StandardDesignCapacity*). Choice of input method will affect the sizing calculations in ways noted below.

### Design Water Flow Rate

If *Performance Input Method* = *StandardDesignCapacity* then

![](media/image2130.png)\


Else

![](media/image2131.png)\


where 5.38210^-08^ is m^3^/s per watt corresponds to the rule-of-thumb of sizing the fluid cooler flow rate at 3 gallons per minute per ton.

### Fan Power at Design Air Flow Rate

The design fan power is sized to be 0.0105 times the design load.

If *Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* then

![](media/image2132.png)\


where

*C~p,w~* is the specific heat of water at the condenser loop design exit temperature;

*~w~* is the density of water at standard conditions (5.05 ^o^C);

*T~loop,des~* is the condenser water loop design temperature rise;

Finally

![](media/image2133.png)\


### **Else**

![](media/image2134.png)\


Where

![](media/image2135.png) is the design capacity provided by the user for the other two performance input methods

### Design Air Flow Rate

We assume a fan efficiency of 0.5 and a fan pressure rise of 190 Pascals. Then

![](media/image2136.png)\


where

~air~ is the density of air at standard conditions.

### Fluid cooler UA Value at Design Air Flow Rate

To obtain the UA of the evaporative fluid cooler, we specify the model inputs (other than the UA) at design conditions and the design fluid cooler load that the fluid cooler must meet. Then we numerically invert the fluid cooler model to solve for the UA that will enable the fluid cooler to meet the design fluid cooler load given the specified inputs.

The design fluid cooler load is:

- For Performance Input Method = UFactorTimesAreaAndDesignWaterFlowRate

![](media/image2137.png)\


- For Performance Input Method = StandardDesignCapacity

![](media/image2138.png)  (to allow for compressor heat)

Then we assign the inputs needed for the model.

*T~in,air~*= 35 ^o^C (95 ^o^F design air inlet temperature)

*T~in,air,wb~*= 25.6 ^o^C (78 ^o^F design air inlet wetbulb temperature)

*W~in~* is calculated from the entering air drybulb and wetbulb.

- For Performance Input Method = UserSpecifiedDesignCapacity

![](media/image2139.png)\


Where, ![](media/image2140.png)  is the ratio of actual heat rejection capacity to nominal capacity.  This ratio is available as a user input with a default value of 1.25 (to allow for compressor heat)

Then we assign the inputs needed for the model.

*T~in,air~*= Design air inlet temperature provided by the user

*T~in,air,wb~*= Design air inlet wetbulb temperature provided by the user

*W~in~* is calculated from the entering air drybulb and wetbulb.

The inlet water mass flow rate is just the design entering volumetric flow rate times the density of water.

The inlet water temperature is set slightly differently for the 3 input methods. For

- UFactorTimesAreaAndDesignWaterFlowRate

![](media/image2141.png)\


- StandardDesignCapacity

![](media/image2142.png)\


- UserSpecifiedDesignCapacity

![](media/image2143.png)\


*We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design fluid cooler load and the fluid cooler output divided by the design fluid cooler load. The residual is calculated in the function *SimpleEvapFluidCoolerUAResidual.*

### Air Flow Rate at Low Fan Speed

The design air flow rate at low fan speed is set to a fraction of the full speed air flow rate. The fraction is available for user input in the field called Low Fan Speed Air Flow Rate Sizing Factor. The default is 0.5.

### Fan Power at Low Fan Speed

The fan power at low fan speed is set to a fraction of the fan power at full speed. The fraction is available for user input in the field called Low Fan Speed Fan Power Sizing Factor. The default is 0.16.

### Fluid cooler UA Value at Low Fan Speed

For *Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* the low speed UA is set to a fraction of the full speed UA. The fraction is available for user input in the field called Low Fan Speed U-Factor Times Area Sizing Factor. The default is 0.6. For *Performance Input Method* = *StandardDesignCapacity* (and similarly for *UserSpecifiedDesignCapacity method*) the low speed UA is calculated in the same manner as the full speed UA using ![](media/image2144.png)  instead of ![](media/image2145.png) .

## Fan Coil Unit Sizing

Fan Coil units are compound components: each unit contains a fan, hot water coil, chilled water coil and outside air mixer. The inputs that may need to be autosized are the nominal unit air flow rate, the maximum hot and chilled water flow rates, and the design outside air flow rate. The data needed for sizing the units is obtained from the zone design arrays and the user specified plant sizing input.

### Maximum Air Flow Rate

![](media/image2146.png)\


### Maximum Outside Air Flow Rate

![](media/image2147.png)\


### Maximum Hot Water Flow

*T~coil,in~*=*DesHeatCoilInTemp~zone~*

*T~coil,out~*=*HeatDesTemp~zone~*

![](media/image2148.png)\


![](media/image2149.png)\


where

*c~p,air~* is evaluated at the average of the inlet & outlet temperatures and the coil outlet humidity ratio.

### Maximum Cold Water Flow

*T~coil,in~*=*DesColdCoilInTemp~zone~*

*T~coil,out~*=*ColdDesTemp~zone~*

*W~coil,in~*= *DesCoolCoilInHumRat~zone~*

*W~coil,out~*= *CoolDesHumRat~zone~*

*H~coil,in~*= *PsyHFnTdbW*(*T~coil,in~*, *W~coil,in~*)

*H~coil,out~*= *PsyHFnTdbW*(*T~coil,out~*, *W~coil,out~*)

![](media/image2150.png)\


![](media/image2151.png)\


where

*c~p,air~* is evaluated at the average of the inlet & outlet temperatures and the coil outlet humidity ratio.

## Window Air Conditioner Sizing

Window air conditioners are compound components: each unit contains a fan, a DX coil and an outside air mixer. The inputs that may need to be autosized are the nominal unit air flow rate and the design outside air flow rate. The data needed for sizing the units is obtained from the zone design arrays.

### Maximum Air Flow Rate

![](media/image2152.png)\


### Maximum Outside Air Flow Rate

![](media/image2153.png)\


## Unit Ventilator Sizing

Unit ventilators are compound components: each unit contains an outdoor air mixer, a fan, a heating coil, and a cooling coil. The inputs that may need to be autosized are the maximum supply air flow rate and the maximum/minimum outside air flow rates. The data needed for sizing the units is obtained from the zone design arrays.

### Maximum Air Flow Rate

If both the cooling and heating coil are present, then:

![](media/image2154.png)\


If only the heating coil is present, then:

![](media/image2155.png)\


If only cooling coil is present, then:

![](media/image2156.png)\


If neither heating nor cooling coil is present, then:

![](media/image2157.png)\


### Maximum Outside Air Flow Rate

![](media/image2158.png)\


### Minimum Outside Air Flow Rate

![](media/image2159.png)\


## Packaged Terminal Heat Pump Sizing

Packaged terminal heat pumps are compound components: each unit contains a supply air fan, a DX cooling coil, a DX heating coil, a GAS or ELECTRIC supplemental heating coil, and an outside air mixer. The inputs that may need to be autosized are the supply air and outside air volumetric air flow rates during cooling operation, heating operation, and when no cooling or heating is needed. In addition, the maximum supply air temperature from the supplemental heater can also be automatically selected. The data needed for sizing the units are obtained from the zone design arrays.

### Supply air volumetric flow rate during cooling operation

![](media/image2160.png)\


### Supply air volumetric flow rate during heating operation

![](media/image2161.png)\


### Supply air volumetric flow rate when no cooling or heating is needed

![](media/image2162.png)\


### Outside air volumetric flow rate during cooling operation

![](media/image2163.png)\


### Outside air volumetric flow rate during heating operation

![](media/image2164.png)\


### Outside air volumetric flow rate when no cooling or heating is needed

![](media/image2165.png)\


### Maximum supply air temperature from supplemental heater

![](media/image2166.png)\


## MultiSpeed Heat Pump Sizing

MultiSpeed heat pumps are compound components: each unit contains a supply air fan, a multispeed DX cooling coil, a multispeed DX heating coil, and a GAS or ELECTRIC supplemental heating coil. The inputs that may need to be autosized are the supply air volumetric air flow rates during cooling operation, heating operation, and when no cooling or heating is needed. The data needed for sizing the units are obtained from the controlled zone design arrays.

### Supply air volumetric flow rate during cooling operation at the highest speed

![](media/image2167.png)\


### Supply air volumetric flow rate during heating operation at the highest speed

![](media/image2168.png)\


### Supply air volumetric flow rate when no cooling or heating is needed

![](media/image2169.png)\


where

ZoneFraction = Fraction of the total volume flow that goes through the controlling zone

### Supply air volumetric flow rate during cooling operation at Speed n (1 to NumberOfSpeed-1)

![](media/image2170.png)\


### Supply air volumetric flow rate during heating operation at Speed n (1 to NumberOfSpeed-1)

![](media/image2171.png)\


where

n= Speed Index number from 1 to NumberOfSpeed-1

NumberOfSpeed= The highest speed number

## Single Duct Terminal Units

These are all the EnergyPlus components whose names begin with "*AirTerminal:SingleDuct:*" (except for Cooled Beam units). This includes *Uncontrolled, ConstantVolume:Reheat, VAV:NoReheat, VAV:Reheat, VAV:Reheat:VariableSpeedFan, VAV:HeatAndCool:NoReheat, VAV:HeatAndCool:Reheat, SeriesPIU:Reheat, ParallelPIU:Reheat,* and *ConstantVolume:FourPipeInduction*. The inputs that may need to be autosized are the various maximum air flow rates through the unit, minimum air flow rates, and maximum hot water and/or chilled water flow rates if heating or cooling coils are present.

*Note:* all zone design flow rates and loads referenced below may have been altered by system sizing inputs. For instance, if the user specifies a *Cooling Design Air Flow Method = Flow/System*  and specifies a *Cooling Design Air Flow Rate* the zone cooling design values will be altered to match the specified system flow rate.

### Maximum Air Flow Rate

![](media/image2172.png)\


### Maximum Heating Air Flow Rate

![](media/image2173.png)\


### Maximum Primary and Secondary Air Flow Rates

For the PIU terminal units, the maximum primary and secondary air flow rates are sized to the same value as the maximum total air flow rate.

### Minimum Air Flow Rate

Basically minimum air flow rates are sized to the ventilation air requirement. This may be more or less complicated.

For the PIU's, the minimum primary air flow fraction is set to

![](media/image2174.png) .

For other VAV terminal units

![](media/image2175.png)\


where, *Fracair,min* corresponds to the minimum flow fraction of the teminal unit. This value is provided as user input, typically as the field "Zone Minimum Air Flow Fraction." For the VAV terminals that allow scheduling minimum flow fraction (e.g., AirTerminal:SingleDuct:VAV:Reheat), there are two ways that *Fracair,min* can be determined. If a value is entered in the input field Constant Minimum Air Flow Fraction, then it is always used for *Fracair,min*. If the mimimum air flow fraction method is "Schedule" and the Constant Minimum Air Flow Fraction is left blank, then the program uses the average of the minimum and maximum values in the schedule for *Fracair,min*.

### Fan On Flow Fraction

For the parallel PIU, this is set to the minimum primary air flow fraction.

### Max Hot  Water Flow

![](media/image2176.png)\


![](media/image2177.png)\


The coil load and max hot water flow rate are then:

![](media/image2178.png)\


![](media/image2179.png)\


where

*c~p,air~* is evaluated at the average of the inlet & outlet temperatures and the coil outlet humidity ratio.

The four-pipe induction unit sizes the heating coil differently: to meet the zone load rather than match the design zone supply temperature. The load on the hot water coil is then the zone load minus whatever the central system does.

![](media/image2180.png)\


where

![](media/image2181.png)\


![](media/image2182.png)\


![](media/image2183.png)\


### Max Chilled Water Flow

The chilled water flow for the four-pipe induction unit is sized analogously to the hot water flow.

## Indirect Evaporative Cooler Sizing

The model for the object called EvaporativeCooler:Indirect:ResearchSpecial has a field for the secondary fan flow rate that can be autosized.

### Secondary Fan Flow Rate

The secondary fan is not part of an airstream that is directly modeled in EnergyPlus.  Because the primary side air flows can be autosized as part of the air system, it is convenent to also scale the size of the secondary flow.   If the cooler is part of the main loop of a central air system, then the secondary fan flow rate is sized to equal to the main design flow rate.

![](media/image1989.png)\


If the cooler is part of the outdoor air path of a central air system, then the secondary fan flow rate is sized to be the maximum of either the design minimum outdoor air flow rate or one-half of the main design flow rate.

![](media/image2184.png)\


## Desiccant Dehumidifier Sizing

The sizing of dehumidifier with no fans is done in subroutine SizeDesiccantDehumidifier.

### Nominal Process Air Flow Rate

If the unit is part of zone equipment, then:

![](media/image2185.png)\


If the unit is part of the central air system, then check if outdoor air system is present. If outdoor air system is part of the air loop and design outdoor air flow rate is greater than zero, then:

![](media/image2186.png)\


Otherwise, nominal process air flow rate is determined as follows:

*for duct type = main*

*![](media/image2187.png)*

*for duct type = cooling*

![](media/image2188.png)\


*for duct type = heating*

![](media/image2189.png)\


*for duct type = other*

![](media/image2190.png)\


## Evaporative Cooler Sizing

The sizing of evaporative cooler objects is done in subroutine SizeEvapCooler.

### Secondary Fan Flow Rate

![](media/image2191.png)\


Note that it is only applicable to indirect CELdek pad object.

### Direct Pad Area

As from the continuity equation, the area of direct pad is directly determined by assuming face air velocity of 3m/s as:

![](media/image2192.png)\


where

*A~pad~* is area of direct pad, m^2^

*V* is face air velocity, m/s

### Direct Pad Depth

The solution of the following regression equation is used, assuming saturation effectiveness of 70% and face air velocity of 3m/s:

ɛ = 0.792714 + 0.958569D - 0.25193V - 1.03215D2 + 0.0262659V2 + 0.914869DV

- 1.48241VD^2^ - 0.018992 D V^3^ + 1.13137D^3^V + 0.0327622 D^2^V^3^ - 0.145384D^3^V^2^

where

D is depth of pad, m

ɛ is saturation effectiveness

## Heat Recovery Sizing

The sizing of heat exchanger objects is done in subroutine SizeHeatRecovery.

### Nominal Supply Air Flow Rate

If the unit is part of zone equipment, then:

![](media/image2185.png)\


If the unit is part of the central air system, then check if outdoor air system is present. If outdoor air system is part of the air loop and design outdoor air flow rate is greater than zero, then:

![](media/image2186.png)\


Otherwise, nominal supply air flow rate is determined as follows:

*for duct type = main*

*![](media/image2187.png)*

*for duct type = cooling*

![](media/image2188.png)\


*for duct type = heating*

![](media/image2189.png)\


*for duct type = other*

![](media/image2190.png)\


### Nominal Supply Air Flow Rate

It is assumed to be equal to the nominal supply air flow rate.

## Low Temperature Radiant System Sizing

The sizing is done in subroutine *SizeLowTempRadiantSystem*.

### ZoneHVAC:LowTemperatureRadiant:Electric:

#### Nominal Capacity

![](media/image2193.png)\


### ZoneHVAC:LowTemperatureRadiant:VariableFlow:

#### Maximum Hot Water Flow

![](media/image2194.png)\


where

*V*h *is maximum hot water flow rate, m^3^/s*

*c~p~~,h~ is specific heat of hot water at reference condition (60°C), J/kgK*

*~h~ is the density of water at reference condition (60°C), kg/m^3^*

#### Maximum Cool Water Flow

![](media/image2195.png)\


*V*c *is maximum chilled water flow rate, m^3^/s*

*c~p~~,~~c~ is specific heat of hot water at reference condition (5°C), J/kgK*

*~c~ is the density of chilled water at reference condition (5°C), kg/m^3^*

#### Hydronic Tubing Length

![](media/image2196.png)\


Note that tube spacing is assumed to be 0.15m.

### ZoneHVAC:LowTemperatureRadiant:ConstantFlow:

#### Rated Flow Rate

The object provides both cooling and heating, and also operates in a single operating mode such as cooling-only or heating-only mode. Thus, the rated flow rate is determined, depending upon the operating mode of the unit. If the unit operates in a single operating mode, either design chilled water or hot water flow rate is chosen. The larger of the two is chosen if the unit provides both cooling and heating. The flow rates are determined in the same fashion to the variable flow system above.

#### Hydronic Tubing Length

The length of hydronic tube is determined as described in the variable flow radiant system above.
