# System Design Loads and Air Flow Rates

## Overview

The purpose of the system design calculation is to estimate design heating and cooling loads and air flow rates for each air loop in the simulation problem. The calculation sequence for system level design loads and air flow rates resembles the calculation sequence for zone loads and air flow rates. There is an update subroutine *UpdateSysSizing* called at the beginning, during, and end  of a loop in the Sizing Manager over all the design days. The major difference is that this calculation is done at the zone time-step only. There is no idealized component calculation triggered at the system time-step as in the zone calculation. The system design calculation operates at the zone time step using the design environment weather data and the data stored in the zone sizing arrays. The results of the system design calculation are stored in the system sizing arrays described below.

## System Design Data Arrays

The system design data arrays are:

*SysSizInput(i)* stores the input data from the Sizing:System objects.

*SysSizing(i,j)* stores the results of the system design calculations for all systems and all design days. The index i is for air loops, j for design days.

*CalcSysSizing(i*) stores the results of the system design calculations for the peak heating and cooling cases for each air loop. **The index i is for the air loops.

*FinalSysSizing(i*) corresponds to *CalcSysSizing* but includes the effect of the user specified sizing factor or user specified system design flow rate.

The data stored in *SysSizing*, *CalcSysSizing* and *FinalSysSizing* includes the following data items.

Table: System Sizing Data

Name|Description
----|-----------
All the data from *SysSizInput*|
*CoinCoolMassFlow*|coincident peak cooling mass flow rate [kg/s]
*CoinHeatMassFlow*|coincident peak heating mass flow rate [kg/s]
*NonCoinCoolMassFlow*|noncoincident peak cooling mass flow rate [kg/s]
*NonCoinHeatMassFlow*|noncoincident peak heating mass flow rate [kg/s]
*DesMainVolFlow*|design main supply duct volume flow [m^3^/s]
*DesHeatVolFlow*|design heat supply duct volume flow [m^3^/s]
*DesCoolVolFlow*|design cool supply duct volume flow [m^3^/s]
*SensCoolCap*|design sensible cooling capacity [W]
*HeatCap*|design heating capacity [W]
*PreheatCap*|design preheat capacity [W]
*CoolMixTemp*|design mixed air temperature for cooling [C]
*CoolMixHumRat*|design mixed air humidity ratio for cooling [kg water/kg dry air]
*CoolRetTemp*|design return air temperature for cooling [C]
*CoolRetHumRat*|design return air humidity ratio for cooling [kg water/kg dry air]
*CoolOutTemp*|design outside air temperature for cooling [C]
*CoolOutHumRat*|design outside air humidity ratio for cooling [kg water/kg dry air]
*HeatMixTemp*|design mixed air temperature for heating [C]
*HeatMixHumRat*|design mixed air humidity ratio for heating [kg water/kg dry air]
*HeatRetTemp*|design return air temperature for heating [C]
*HeatRetHumRat*|design return air humidity ratio for heating [kg water/kg dry air]
*HeatOutTemp*|design outside air temperature for heating [C]
*HeatOutHumRat*|design outside air humidity ratio for heating [kg water/kg dry air]
*HeatFlowSeq(i)*|daily sequence of system heating air mass flow rate (zone time step) [kg/s]
*CoolFlowSeq(i)*|daily sequence of system cooling air mass flow rate (zone time step) [kg/s]
*SensCoolCapSeq(I)*|daily sequence of system sensible cooling capacity (zone time step) [W]
*HeatCapSeq(i)*|daily sequence of system heating capacity (zone time step) [W]
*PreHeatCapSeq(i)*|daily sequence of system preheat capacity (zone time step) [W]
*SysCoolRetTempSeq(i)*|daily sequence of system cooling return temperatures  (zone time step) [C]
*SysCoolRetHumRatSeq(I)*|daily sequence of system cooling return humidity ratios (zone time step) [kg water/kg dry air]
*SysHeatRetTempSeq(i)*|daily sequence of system heating return temperatures  (zone time step) [C]
*SysHeatRetHumRatSeq(I)*|daily sequence of system heating return humidity ratios (zone time step) [kg water/kg dry air]
*SysCoolOutTempSeq*|daily sequence of system cooling outside temperatures (zone time step) [C]
*SysCoolOutHumRatSeq*|daily sequence of system cooling outside humidity ratios (zone time step) [kg water/kg dry air]
*SysHeatOutTempSeq*|daily sequence of system heating outside temperatures (zone time step) [C]
*SysHeatOutHumRatSeq*|daily sequence of system heating outside humidity ratios (zone time step) [kg water/kg dry air]

## System Design Flow Rate and Load Summation and Adjustment

There is no system level subroutine corresponding to *SizeZoneEquipment.* Instead the system design loads and flow rates are calculated using the zone level results. The zone design flow rates for the zones served by an air loop are summed to obtain the system level design flow rates. These air flows are mixed with the system level design minimum outside air flow rate to obtain system design coil loads. These activities are all performed within the *UpdateSysSizing* subroutine in the *SimAirServingZones* module. It is called at the start of each design day (*CallIndicator = BeginDay*), at the zone time-step (*CallIndicator = DuringDay*), at the end of the design day (*CallIndicator = EndDay*) and at the end of the zone design calculation (*CallIndicator = EndSysSizingCalc*).

There is a logical flag *SysSizingCalc* corresponding to *ZoneSizingCalc*.  It is used to allow the component routines to distinguish a normal simulation call from a being called during a system sizing calculation.

### BeginDay

The environment (in this case, a design day) name is stored in the system sizing data structures.

Loop over the zones cooled by this air loop:

*NonCoinCoolMassFlow~sys~*=**DesCoolMassFlow***~zone~*

Loop over the zones heated by this air loop:

*NonCoinHeatMassFlow~sys~*=**DesHeatMassFlow***~zone~*

### DuringDay

Loop over the zones cooled by this air loop:

*CoolFlowSeq~sys~(i)* =**CoolFlowSeq***~zone~*(i)

*SysCoolRetTemp(i)*= **(***CoolZoneRetTempSeq(i) CoolFlowSeq~zone~(i)*) *CoolFlowSeq~sys~(i)*

*SysCoolRetHumRat(i)*= **(***CoolZoneHumRatSeq(i) CoolFlowSeq~zone~(i)*) *CoolFlowSeq~sys~(i)*

*FracOA=~air~ DesOutAirVolFlow~sys~*/ *CoolFlowSeq~sys~(i)*

*T~mix~*=*T~outside~ FracOA* + *SysCoolRetTemp(i)*(1 – *FracOA*)

*W~mix~*=*W~outside~ FracOA* + *SysCoolRetHumRat (i)*(1 – *FracOA*)

*SysCoolOutTempSeq(i)*= *T~outside~*

*SysCoolOutHumRatSeq(i)*= *W~outside~*

Get the current (zone time-step) system cooling capacity:

*SysSensCoolCap~cur~*=*C~p,air~ CoolFlowSeq~sys~(i)* ( *T~mix~*-*T~sup~*)

*SensCoolCapSeq(I)*= *SysSensCoolCap~cur~*

If *SysSensCoolCap~cur~* is the maximum for the day so far then save *SysSensCoolCap~cur~* as the design value:

*SensCoolCap(i~~)~sys~*= *SysSensCoolCap~cur~*

And save the corresponding mixed, return and outside conditions:

*CoolMixTemp~sys~*=~~*T~mix~*

*CoolMixHumRat~sys~*=*W~mix~*

*CoolRetTemp~sys~*= *SysCoolRetTemp(i)*

*CoolRetHumRat~sys~*= *SysCoolRetHumRat(I)*

*CoolOutTemp~sys~*= *T~outside~*

*CoolOutHumRat~sys~*= *W~outside~*

Here*~air~*is the density of dry air at 20C and standard elevation corrected pressure, [kg/m^3]^;

*FracOA* is the outside air fraction;

*C~p,air~* is the specific heat of dry air at 20C, [J/kg-K];

*T~sup~* is the user specified design cooling supply temperature [C];

*T~mix~* is the current mixed air temperature [C];

*W~mix~* is the current mixed air humidity ratio [kg water / kg dry air];

*T~outside~* is the current outside air temperature [C];

*W~outside~* is the current outside air humidity ratio [kg water / kg dry air].

Loop over the zones heated by this air loop.

*HeatFlowSeq~sys~(i)* =**HeatFlowSeq***~zone~*(i)

*SysHeatRetTemp(i)*= **(***HeatZoneRetTempSeq(i) HeatFlowSeq~zone~(i)*)

*HeatFlowSeq~sys~(i)*

*SysHeatRetHumRat(i)*= **(***HeatZoneHumRatSeq(i) HeatFlowSeq~zone~(i)*)

 *HeatFlowSeq~sys~(i)*

*FracOA=~air~ DesOutAirVolFlow~sys~*/ *HeatFlowSeq~sys~(i)*

*T~mix~*=*T~outside~ FracOA* + *SysHeatRetTemp(i)*(1 – *FracOA*)

*W~mix~*=*W~outside~ FracOA* + *SysHeatRetHumRat (i)*(1 – *FracOA*)

*SysHeatOutTempSeq(i)*= *T~outside~*

*SysHeatOutHumRatSeq(i)*= *W~outside~*

Get the current (zone time-step) system heating capacity:

*SysHeatCap~cur~*=*C~p,air~ MinFlowRat~sys~HeatFlowSeq~sys~(i)* ( *T~sup~*-*T~mix~*)

*HeatCapSeq(I)*= *SysHeatCap~cur~*

If *SysHeatCap~cur~* is the maximum for the day so far then save *SysHeatCap~cur~* as the design value:

*HeatCap(i~~)~sys~*= *SysHeatCap~cur~*

And save the corresponding mixed, return and outside conditions:

*HeatMixTemp~sys~*=~~*T~mix~*

*HeatMixHumRat~sys~*=*W~mix~*

*HeatRetTemp~sys~*= *SysHeatRetTemp(i)*

*HeatRetHumRat~sys~*= *SysHeatRetHumRat(I)*

*HeatOutTemp~sys~*= *T~outside~*

*HeatOutHumRat~sys~*= *W~outside~*

Here *MinFlowRat~sys~* is the user specified minimum supply flow ratio.

### EndDay

If the user has specified *coincident* system sizing then:

*DesCoolVolFlow~sys~*=*~air~CoinCoolMassFlow~sys~*

*DesHeatVolFlow~sys~*=*~air~CoinHeatMassFlow~sys~*

*DesMainVolFlow~sys~*=**Max**(*DesCoolVolFlow~sys~*, *DesHeatVolFlow~sys~*)

If the user has specified *noncoincident*system sizing then:

*DesCoolVolFlow~sys~*=*~air~NonCoinCoolMassFlow~sys~*

*DesHeatVolFlow~sys~*=*~air~NonCoinHeatMassFlow~sys~*

*DesMainVolFlow~sys~*=**Max**(*DesCoolVolFlow~sys~*, *DesHeatVolFlow~sys~*)

Based on the outdoor air method selected, the *DesCoolVolFlow~sys~* and *DesHeatVolFlow~sys~* are modified based on the system ventilation effciency calculated based on the maximum outdoor air fraction.

### EndSysSizingCalc

At this point all the calculations have been done in *SysSizing(i,j)*: we have results for each design day. Now these results need to be processed to find the heating and cooling design quantities for each system over all the design days.

For coincident sizing the task is quite easy.

Loop over all of the air loops.

Loop over all of the design days.

If the value of *DesCoolVolFlow* in *SysSizing* for the current design day is greater than the value stored in *CalcSysSizing*, then move *DesCoolVolFlow* from *SysSizing* into *CalcSysSizing* along with *CoolDesDay*, *CoinCoolMassFlow*, *SensCoolCap*, *CoolFlowSeq(i)*, *SensCoolCapSeq(i),* *CoolMixTemp*, *CoolRetTemp*, *CoolMixHumRat*, *CoolRetHumRat*, *CoolOutTemp*, *CoolOutHumRat*, *SysCoolRetTempSeq(i)*, *SysCoolRetHumRatSeq(i)*, *SysCoolOutTempSeq(i)* and *SysCoolOutHumRatSeq(i)*.

If the value of *DesHeatVolFlow* in *SysSizing* for the current design day is greater than the value stored in *CalcSysSizing*, then move *DesHeatVolFlow* from *SysSizing* into *CalcSysSizing* along with *HeatDesDay*, *CoinHeatMassFlow*, *HeatCap*, *PreHeatCap*, *HeatFlowSeq(i)*, *HeatCapSeq(i),* *PreHeatCapSeq(i), HeatMixTemp*, *HeatRetTemp*, *HeatMixHumRat*, *HeatRetHumRat*, *HeatOutTemp*, *HeatOutHumRat*, *SysHeatRetTempSeq(i)*, *SysHeatRetHumRatSeq(i)*, *SysHeatOutTempSeq(i)* and *SysHeatOutHumRatSeq(i)*.

At the end of each design day loop the peak cooling and the peak heating data will be stored in *CalcSysSizing*. At this point we set *DesMainVolFlow* in *CalcSysSizing* equal to the maximum of *DesCoolVolFlow* and *DesHeatVolFlow.*

For noncoincident sizing the task is harder since we don't have a single time-step during which all the zone peaks occur. So there is no obvious value for outside air temperature at the peak, return air temperature at the peak and so forth. We must return to the zone sizing data and calculate average values for return and outside conditions.

Loop over all of the zones cooled by this air loop

In *FinalZoneSizing* replace the value in *DesCoolCoilInTemp* with the user specified *CoolSupTemp~sys~*. Do the same for *DesCoolCoilInHumRat* and *CoolSupHumRat*. This ensures that zone equipment connected to an air loop will use the system design supply air conditions as coil entering conditions.

*NonCoinCoolMassFlow~sys~*=**DesCoolMassFlow***~zone~*

*SysCoolRetTemp*=**(***ZoneRetTempAtCoolPeakDesCoolMassFlow~zone~*)

*/ NonCoinCoolMassFlow~sys~*

*SysCoolRetHumRat*=**(***ZoneHumRatAtCoolPeak*

*DesCoolMassFlow~zone~*)/ *NonCoinCoolMassFlow~sys~*

*SysCoolOutTemp*=**(***T~OA,zone peak~DesCoolMassFlow~zone~*)/

 *NonCoinCoolMassFlow~sys~*

*SysCoolOutHumRat*=**(***W~OA,zone peak~DesCoolMassFlow~zone~*)/

 *NonCoinCoolMassFlow~sys~*

At the end of the zone loop calculate mixed air conditions and the system sensible cooling capacity.

*FracOA=~air~ DesOutAirVolFlow~sys~*/ *NonCoinCoolMassFlow~sys~*

*T~mix~* =*SysCoolOutTemp FracOA* + *SysCoolRetTemp* (1 – *FracOA*)

*W~mix~* = *SysCoolOutHumRat*  *FracOA* + *SysCoolRetHumRat*

(1 – *FracOA*)

*SysSensCoolCap*=*C~p,air~ NonCoinCoolMassFlow* ( *T~mix~*-*T~sup~*)

Then (for noncoincident sizing) the variables calculated in section (ii) are moved into the *CalcSysSizing* Array.

Loop over all of the zones heated by this air loop.

In *FinalZoneSizing* replace the value in *DesHeatCoilInTemp* with the user specified *HeatSupTemp~sys~*. Do the same for *DesHeatCoilInHumRat* and *HeatSupHumRat*. This ensures that zone equipment connected to an air loop will use the system design supply air conditions as coil entering conditions.

*NonCoinHeatMassFlow~sys~*=**DesHeatMassFlow***~zone~*

*SysHeatRetTemp*=**(***ZoneRetTempAtHeatPeakDesHeatMassFlow~zone~*)

*/ NonCoinHeatMassFlow~sys~*

*SysHeatRetHumRat*=**(***ZoneHumRatAtHeatPeak*

*DesHeatMassFlow~zone~*)/ *NonCoinHeatMassFlow~sys~*

*SysHeatOutTemp*=**(***T~OA,zone peak~DesHeatMassFlow~zone~*)/

 *NonCoinHeatMassFlow~sys~*

*SysHeatOutHumRat*=**(***W~OA,zone peak~DesHeatMassFlow~zone~*)/

 *NonCoinHeatMassFlow~sys~*

At the end of the zone loop calculate mixed air conditions and the system sensible cooling capacity.

*FracOA=~air~ DesOutAirVolFlow~sys~*/ *NonCoinHeatMassFlow~sys~*

*T~mix~* =*SysHeatOutTemp FracOA* + *SysHeatRetTemp* (1 – *FracOA*)

*W~mix~* = *SysHeatOutHumRat*  *FracOA* + *SysHeatRetHumRat*

(1 – *FracOA*)

*SysHeatlCap*=*C~p,air~ NonCoinHeatlMassFlow* ( *T~sup~*-*T~mix~*)

Then (for noncoincident sizing) the variables calculated in section (ii) are moved into the *CalcSysSizing* Array.

We now have the calculated system sizing data. This data needs to be altered to take into account the user input system design flow rates (if any), or the fact that the user may have requested that the system flow rate be sized on the ventilation requirement. Note that user specified sizing ratios have already been applied to the zone sizing data which have been used in out preceding system sizing calculation. Thus the user specified sizing ratios do not have to be explicitly taken into account at the system level.

First we move the calculated system sizing data from *CalcSysSizing* array into the *FinalSysSizing* array. *FinalSysSizing* will contain the user modified system design data when we are all done.

Loop over the air loops.

As in the zone case, the user specified system design flow rates are turned into sizing ratios by dividing the user input value by the calculated value. The same strategy is employed for sizing on the ventilation requirement: the design ventilation flow rate is divided by the calculated design flow rate value. For each air loop this gives us a *SizRat~cool~* and *SizRat~heat~*.

*CoinCoolMassFlow*= *SizRat~cool~ CoinCoolMassFlow~calc~*

*NonCoinCoolMassFlow*= *SizRat~cool~ NonCoinCoolMassFlow~calc~*

*DesCoolVolFlow*= *SizRat~cool~ DesCoolVolFlow~calc~*

Since the flow rates have been altered the outside air fraction will change. This will alter the design mixed air conditions and lead to an altered value for the cooling capacity. This must be done for the time-step sequence and for the peak value.

Loop over the zone timesteps (index=*i*).

*CoolFlowSeq~sys~(i)*= *SizRat~cool~ CoolFlowSeq~sys,calc~(i)*

*FracOA=~air~ DesOutAirVolFlow~sys~*/ *CoolFlowSeq~sys~(i)*

*T~mix~*= *SysCoolOutTempSeq(i)FracOA +*

*SysCoolRetTempSeq(i)(*1-FracOA)

*SensCoolCapSeq(i)*= *C~p,air~ CoolFlowSeq~sys~(i)* ( *T~mix~*-*T~sup~*)

Do the same calculation for peak cooling.

*FracOA=~air~ DesOutAirVolFlow~sys~*/ *DesCoolVolFlow*

*T~mix~*= *CoolOutTemp~sys~FracOA + CoolRetTemp~sys~(*1-FracOA)

*W~mix~*= *CoolOutHumRat~sys~FracOA + CoolRetHumRat~sys~*

*(*1-FracOA)

*SensCoolCap~sys~*= *C~p,air~ DesCoolVolFlow~sys~* ( *T~mix~*-*T~sup~*)

*T~mix~* and *W~mix~* are saved in *FinalSysSizing* *.*

Do the same calculation for the heating case.

*CoinHeatMassFlow*= *SizRat~heat~ CoinHeatMassFlow~calc~*

*NonCoinHeatMassFlow*= *SizRat~heat~ NonCoinHeatMassFlow~calc~*

*DesHeatVolFlow*= *SizRat~heat~ DesHeatVolFlow~calc~*

Loop over the zone timesteps (index=*i*).

*HeatFlowSeq~sys~(i)*= *SizRat~Heat~ HeatFlowSeq~sys,calc~(i)*

*FracOA=~air~ DesOutAirVolFlow~sys~*/ *HeatFlowSeq~sys~(i)*

*T~mix~*= *SysHeatOutTempSeq(i) FracOA +*

 *SysHeatRetTempSeq(i) (*1-FracOA)

*HeatCapSeq(i)*= *C~p,air~ HeatFlowSeq~sys~(i)* (*T~sup~*-*T~mix~*)

Do the same calculation for peak heating.

*FracOA=~air~ DesOutAirVolFlow~sys~*/ *DesHeatVolFlow*

*T~mix~*= *HeatOutTemp~sys~FracOA + HeatRetTemp~sys~ (*1-FracOA)

*W~mix~*= *HeatOutHumRat~sys~FracOA + HeatRetHumRat~sys~*

*(*1-FracOA)

*HeatCap~sys~*= *C~p,air~ DesHeatVolFlow~sys~* ( *T~sup~*-*T~mix~*)

*T~mix~* and *W~mix~* are saved in *FinalSysSizing* *.*

*DesMainVolFlow~sys~*=**MAX**(*DesCoolVolFlow~sys~*,*DesHeatVolFlow~sys~*)

**This concludes the system design calculation.**

**Scalable System HVAC Sizing**

The scalable system sizing applies to system supply air flow rates and sysyem capacity in coolin and heating modes.

**Scalable System Air Flow Sizing**

The scalable sizing methods for supply air flow rate allowed are  either, *FlowPerFloorArea*, *FractionOfAutosizedCoolingAirflow*, or *FlowPerCoolingCapacity*. The scalable system air flow sizing methods are defined as follows:

*FlowPerFloorArea* means the program calculates the cooling supply air volume flow rate from zone floor area served by the airloop and user specified *Flow Per Floor Area* value.

*FractionOfAutosizedCoolingAirflow* means the program calculates the cooling supply air volume flow rate from user specified fraction and the autosized design cooling supply air volume flow rate value determined by the simulation.

*FractionOfAutosizedHeatingAirflow* means the program calculates the heating supply air volume flow rate from user specified fraction and the autosized design heating supply air volume flow rate value determined by the simulation.

*FlowPerCoolingCapacity* means the supply air volume is calculated from user specified flow per cooling capacity and design cooling capacity determined by the simulation.

*FlowPerHeatingCapacity* means the supply air volume is calculated from user specified flow per heating capacity and design heating capacity determined by the simulation.

**Scalable System Capacity Sizing**

The scalable sizing methods for system capacity available are: *CapacityPerFloorArea*, *FractionOfAutosizedCoolingCapacity* and *FractionOfAutosizedHeatingCapacity*. The scalable system capacity sizing methods are defined as follows:

*CapacityPerFloorArea* means the program calculates the design capacity from user specified capacity per floor area and floor area of the zones served by the airloop.

*FractionOfAutosizedCoolingCapacity* means the program calculates the design cooling capacity from user specified fraction and the auto-sized design cooling capacity.

*FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity.