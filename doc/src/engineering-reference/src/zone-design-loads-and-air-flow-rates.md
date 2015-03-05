# Zone Design Loads and Air Flow Rates

## Overview

There is no single best way to establish design HVAC flow rates and size HVAC equipment. Different building designs, climates, and HVAC systems will impose varying constraints on the designer. The method used to size an HVAC system in a hot, moist climate such as Miami will be different than the method used for a building in Albuquerque. The type of building is also relevant – a simple watts per square foot loads estimate could be adequate for a building containing a network server farm while a detailed, dynamic loads simulation would be necessary for a passive solar building. In the end the designer's experience and engineering judgement will play an important role in any sizing calculation.

HVAC equipment sizing begins with the calculation of space heating and cooling loads. A space cooling  (heating) load is defined as the rate at which heat must be removed (added) to a space to maintain a constant temperature. The current industry standard method for calculating space loads is the *heat balance method*  [ASHRAE Fundamentals (2001), page 29.1; Pedersen et al., (1997); Pedersen (2001). Since EnergyPlus is a heat balance based simulation program it is straightforward for the program to use this method for calculating zone loads.

## Zone Design Data Arrays

The zone design data arrays are:

*ZoneSizingInput(i)* stores the input data from the Sizing:Zone **objects.

*CalcZoneSizing(i,j)* stores the results of the zone design calculations for all zones and all design days. The index i is for the controlled zones, j for design days.

*CalcFinalZoneSizing(i)* stores the results of the zone design calculations for the peak heating and cooling cases for each zone. The index i is for the controlled zones.

*ZoneSizing(i,j)* corresponds to *CalcZoneSizing* but includes the effect of the user specified sizing factor or user specified zone design flow rate.

*FinalZoneSizing(i)* corresponds to *CalcFinalZoneSizing* but includes the effect of the user specified sizing factor or user specified zone design flow rate.

The data stored in *CalcZoneSizing*, *CalcFinalZoneSizing*, *ZoneSizing* and *FinalZoneSizing* includes the following data items.

Table: Zone Sizing Data

Name|Description
----|-----------
All the data from *ZoneSizingInput*|
*DesHeatMassFlow*|the zone design heating air mass flow rate in [kg/s]
*DesCoolMassFlow*|the zone design cooling air mass flow rate in [kg/s]
*DesHeatLoad*|the zone design heating load in [W]
*DesCoolLoad*|the zone design cooling load in [W]
*DesHeatDens*|the zone design heating air density [kg/m^3^]
*DesCoolDens*|the zone design cooling air density [kg/m^3^]
*DesHeatVolFlow*|the zone design heating air volume flow rate [m^3^/s]
*DesCoolVolFlow*|the zone design cooling air volume flow rate [m^3^/s]
*DesHeatCoilInTemp*|zone heating coil design air inlet temperature [C]
*DesCoolCoilInTemp*|zone cooling coil design air inlet temperature [C]
*DesHeatCoilInHumRat*|the zone heating coil design air inlet humidity ratio [kg/kg]
*DesCoolCoilInHumRat*|the zone cooling coil design air inlet humidity ratio [kg/kg]
*HeatMassFlow*|current zone heating air mass flow rate at the HVAC time step [kg/s]
*CoolMassFlow*|current zone cooling air mass flow rate at the HVAC time step [kg/s]
*HeatLoad*|Current zone heating load [W]
*CoolLoad*|Current zone cooling load [W]
*HeatZoneTemp*|Current zone temperature during heating [C]
*HeatZoneRetTemp*|current zone return temperature during heating [C]
*CoolZoneTemp*|Current zone temperature during cooling [C]
*CoolZoneRetTemp*|current zone return temperature during cooling [C]
*HeatZoneHumRat*|Current zone humidity ratio during heating [C]
*CoolZoneHumRat*|Current zone humidity ratio during cooling [C]
*ZoneTempAtHeatPeak*|zone temperature at maximum heating [C]
*ZoneRetTempAtHeatPeak*|zone return temperature at maximum heating [C]
*ZoneTempAtCoolPeak*|zone temperature at maximum cooling [C]
*ZoneRetTempAtCoolPeak*|zone return temperature at maximum cooling [C]
*ZoneHumRatAtHeatPeak*|zone humidity ratio at maximum heating [kg/kg]
*ZoneHumRatAtCoolPeak*|zone humidity ratio at maximum cooling [kg/kg]
*TimeStepNumAtHeatMax*|zone time step number (in the day) at the heating peak
*TimeStepNumAtCoolMax*|zone time step number (in the day) at the cooling peak
*HeatDDNum*|design day index of design day causing heating peak
*CoolDDNum*|design day index of design day causing cooling peak
*MinOA*|design minimum outside air [m3/s]
*HeatFlowSeq(i)*|daily sequence of zone heating air mass flow rates (zone time step) [kg/s]
*CoolFlowSeq(i)*|daily sequence of zone cooling air mass flow rates (zone time step) [kg/s]
*HeatLoadSeq(i)*|daily sequence of zone heating loads (zone time step) [W]
*CoolLoadSeq(i)*|daily sequence of zone cooling loads (zone time step) [W]
*HeatZoneTempSeq(i)*|daily sequence of zone temperatures (heating, zone time step) [C]
*HeatZoneRetTempSeq(i)*|daily sequence of zone return temperatures (heating, zone time step) [C]
*CooltZoneTempSeq(i)*|daily sequence of zone temperatures (cooling, zone time step) [C]
*CoolZoneRetTempSeq(i)*|daily sequence of zone return temperatures (cooling, zone time step) [C]
*HeatZoneHumRatSeq(i)*|daily sequence of zone humidity ratios (heating, zone time step) [kg/kg]
*CoolZoneHumRatSeq(i)*|daily sequence of zone humidity ratios (cooling, zone time step) [kg/kg]

## Zone Design Load Calculation

As described in the preceding section, the Sizing Manager initiates the zone design calculation by looping over all of the design days and calling the Heat Balance Manager for each zone time-step in each design day. The Heat Balance manager then causes the HVAC Manager to be called in a manner identical to a normal simulation. The *ZoneSizingCalc* set to *true* signals the HVAC Manager to ignore the actual HVAC system and instead calculate the design zone loads and air flow rates using an ideal zonal system.

In module *HVACManager*, subroutine *ManageHVAC* calls *SimHVAC*. *SimHVAC* checks *ZoneSizingCalc*. If it is *true*, *SimHVAC* calls *ManageZoneEquipment* and returns, rather than simulating the actual system. In turn *ManageZoneEquipment* checks if *ZoneSizingCalc* is *true*; if it is it calls *SizeZoneEquipment* rather than *SimZoneEquipment*.

*SizeZoneEquipment* assumes that each controlled zone is served by an ideal air conditioning unit. This unit supplies heating or cooling air at a fixed, user input temperature and humidity (specified in the Sizing:Zone objects). The units have infinite capacity – the flow rate can be any amount. The calculation steps are as follows.

#. Loop over all the controlled zones.
#. If the system is active (zone temperature not in the deadband and zone load greater than 1 watt) the sign of the zone load is used to determine whether heating or cooling is required and *T~in~* and W*~in~* are set to the appropriate values from the Sizing:Zone input. When the SupplyTemperature method is specified in the Sizing:Zone object, *T~in~* is fixed at the cooling or heating supply temperature. When the TemperatureDifference method is selected, *T~in~* is calculated at each time step using the current zone air temperature. The system output *Q~sys~* is simply set equal to the zone demand – it is assumed that the ideal system can always meet the zone load. The air flow rate corresponding to the load is just

![](media/image1984.png)\


If the system is not active, the mass flow rate is set to zero and the system output is left at zero.

#. The results for each zone are stored in the zone sizing data arrays.

## Updating and Adjusting the Zone Results

The results from *SizeZoneEquipment* are at the system time-step and are for all design days. These results then need to be summed or averaged over the zone time-step, peak values calculated for each design day, a heating & a cooling load sequence chosen for each zone from all the design day results, possible further smoothing of results done, zone coil loads calculated, and user sizing multipliers or user specified design flows taken into account. These tasks are accomplished by the subroutine *UpdateZoneSizing*. It is called at the start of each design day (*CallIndicator = BeginDay*), at the zone time-step (*CallIndicator = DuringDay*), at the end of the design day (*CallIndicator = EndDay*) and at the end of the zone design calculation (*CallIndicator = EndZoneSizingCalc*).

### BeginDay

The environment (in this case, a design day) name and number are stored in the zone sizing data structures

### DuringDay

The calculated and stored sequences are summed or averaged over the zone time-step.

### EndDay

Smooth the design sequences by applying a moving, fixed-width averaging window to the sequences. The width of the window is user specified in the *Sizing:Parameters* input object. The sequences that are smoothed are:

*CoolFlowSeq*

*CoolLoadSeq*

*HeatFlowSeq*

*HeatLoadSeq*

*CoolZoneRetTempSeq*

*HeatZoneRetTempSeq*

The peak heating and cooling loads and mass & volume flow rates are extracted from each set of design sequences.

Using the time of the peak and the design outside air fraction the design zone heating and cooling coil inlet temperatures and humidity ratios are calculated.

For each zone, looking at the results for all of the design days, the design days that cause the peak heating and peak cooling for that zone are chosen and the corresponding design sequences and peak loads and flow rates are saved in the CalcFinalZoneSizing array. This finishes the calculated – unmodified by the user – portion of the zone design calculation.

### EndZoneSizingCalc

Write out onto a comma-separated file the calculated design sequences for each zone: *HeatLoadSeq*, *CoolLoadSeq*, *HeatFlowSeq*, *CoolFlowSeq* and the corresponding peaks and volumetric flow peaks.

The data in *CalcZoneSizing* and *CalcFinalZoneSizing* is moved to *ZoneSizing* and *FinalZoneSizing*. The user modifications to the calculated sizing will be applied to and stored in *ZoneSizing* and *FinalZoneSizing*.

The user can modify the calculated zone design results by specifying heating and cooling sizing factors at the global or zone level or by specifying and actual design heating or cooling zone design volumetric flow rate. All of this input is treated as a sizing factor. If the user inputs a cooling design volumetric flow rate for a zone it is divided by the calculated cooling design volumetric flow rate for the zone to give a zone cooling sizing factor. Note that the user can input a zone sizing factor or a zone design flow rate – not both – so there is never a conflict.

Once the zone heating and cooling sizing factors are established, the design flow and load sequences as well as peak loads and flows are multiplied by the appropriate sizing factor and stored in *ZoneSizing* and *FinalZoneSizing*. This is the data that will be used for sizing zone HVAC equipment and in the system sizing calculation.

The outside air fractions are recalculated using the new user-modified design flow rates and new design zone coil inlet conditions calculated and stored. At this point the condition that the design flow rates are never allowed to be less than the minimum outside air flow rate is imposed.

If *outside air method* is *flow/zone*, the input *outside air flow per zone* value will be used, even if it is zero or blank. If *outside air method* is *sum*, the sum of the *outside air flow per person* \* *DesignNumberOfPeople* + *outside air flow per area* \* *ZoneArea* will be used. If *outside air method* is *maximum*, the maximum of the *outside air flow per person* \* *DesignNumberOfPeople* and *outside air flow per area* \* *ZoneArea* will be used. If *outside air method* is *flow/person*, *outside air flow per person* will be used to calculate the design minimum outside airflow rate.

If *cooling design air flow method* is *flow/zone*, then *cooling design air flow rate* will be used for the design max cooling air flow rate.  If *cooling design air flow method* is *design day*, then the design day calculation will set the design max cooling air flow rate. If  *cooling design air flow method* is *design day with limit*, then the maximum from *cooling min flow per area* and *cooling min flow* will set a lower limit on the design max cooling air flow rate. In all cases, the maximum from *cooling min flow per area*, *cooling min flow*, and *cooling min flow fraction* will set a minimum zone cooling air flow rate. In all cases the maximum design cooling air flow rate must be >= to the ventilation requirement.

If *heating design air flow method* is *flow/zone*, then *heating design air flow rate* will be used for the design max heating air flow rate.  If *heating design air flow method* is *design day*, then the design day calculation will set the design max heating air flow rate. If *heating design air flow method* is *design day with limit*, then the maximum from *heating max flow per area*, *heating max flow* and *heating max flow fraction* will set an upper limit on the design max heating air flow rate. The design max heating air flow rate must always be >= the ventilation requirement. In each case, the outside airflow will be modified based on zone ventilation effectiveness specified in the zone sizing object.

**This concludes the calculation of the zone design flow rates and loads.**

## Zone HVAC Scalable Sizing

For zone HVAC equipments scalable sizing applies to supply air flow rate and capacity for both cooling and heating.  The scalable sizing method allowed for supply air flow rates include: *FractionOfAutosizedCoolingAirflow*, *FractionOfAutosizedHeatingAirflow*, *FlowPerFloorArea, FlowPerCoolingCapacity*, and *FlowPerHeatingCapacity*.  The supply air flow rate scalable sizing methods are defined as follows:

**FlowPerFloorArea**: the simulation engine determine the supply air flow rates from the user specified *supply air flow rates per unit floor area* and the zone floor area of the zone served by the zone HVAC equipment.

**FractionOfAutosizedCoolingAirflow**: the simulation engine determines the supply air flow rates from the user specified *flow fraction* and *autosized cooling design supply air flow rate*.

**FractionOfAutosizedHeatingAirflow**: the simulation engine determines the supply air flow rates from the user specified *flow fraction* and *autosized heating design supply air flow rate*.

**FlowPerCoolingCapacity**: he simulation engine determines the supply air flow rates from the user specified *supply air flow per cooling capacity value* and *autosized cooling design capacity*.

**FlowPerHeatingCapacity**: the simulation engine determines the supply air flow rates from the user specified *supply air flow per heating capacity value* and *autosized heating design capacity*.

The scalable capacity sizing may be indirectly impacted by the scalable supply air flow rates sizing values. Moreover, the autosized cold water, hot water and steam flow rates in the parent zone HVAC objects and capacity in child components are determined using the scalable sizing method.  Scalable capacity sizing methods allowed for cooling and heating include: *CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity*, *FractionOfAutosizedHeatingCapacity*. The scalable sizing capacity methods are defined as follows:

**CapacityPerFloorArea**: the simulation engine determines the cooling or heating capacity from user specified capacity per floor area value and the floor area of the zone served by the zone HVAC equipment.

**FractionOfAutosizedCoolingCapacity**: the simulation engine sizes the cooling capacity from the user specified *capacity fraction* and *autosized cooling design capacity* value.

**FractionOfAutosizedHeatingCapacity**: the simulation engine sizes the heating capacity from the user specified *capacity fraction* and *autosized heating design capacity* value.