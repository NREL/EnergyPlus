
Loop, Equipment Sizing and other Design Data
============================================

The importance of correct equipment sizing is often ignored in discussions of building simulation methods. The lack of reliable, efficient and flexible sizing calculations can present a serious barrier to the adoption and acceptance of building simulation programs. This section describes the sizing methodology implemented in EnergyPlus. This method includes:

1.    A zone by zone heat balance load and air-flow calculation for multiple design days;

2.    Significant user control with modest input requirements;

3.    Zone, system and plant level calculations of design heating and cooling capacities and fluid flow rates;

4.    Modular, component-specific sizing algorithms for each HVAC component.

5.    Options for monitoring how the initial sizes operate over multiple design days and then making adjustments and repeating plant level calculations

Sizing Manager
--------------

The sizing calculations in EnergyPlus are managed by a sizing manager contained in the software module *SizingManager*. The main sizing manager routine *ManageSizing* is called from *ManageSimulation* before the annual simulation sequence is invoked. *ManageSizing* performs the following tasks.

*  By calling *GetSizingParams*, *GetZoneSizingInput*, *GetSystemSizingInput* and *GetPlantSizingInput* reads in all the user sizing input contained in objects *Sizing:Parameters*, *Sizing:Zone*, *Sizing:System* and *Sizing:Plant*. These objects and their data are described in the EnergyPlus Input Output Reference, Group – Design Objects.

*  Set the *ZoneSizingCalc* flag equal to *true*.

*  Loop over all the sizing periods by each day. **This starts the zone design calculations.**

    * Call *UpdateZoneSizing(BeginDay)* to initialize zone design load and flow rate  sequences.

    * Loop over hours in the day

        * Loop over zone time steps in each hour

            * Call *ManageWeather* to obtain outside conditions for this time-step.

            * Call *ManageHeatBalance* to do a full heat balance calculation for each zone. The call to *ManageHeatBalance* also brings about an HVAC simulation. *ZoneSizingCalc = true* signals the *HVACManager* to ignore the real HVAC system and instead run the ideal zonal system (described below) used to calculate design loads and flow rates. HVACManager also calls *UpdateZoneSizing(DuringDay)* to save the results of the ideal zonal system calculation in the design load and flow rate sequences.

        * Call *UpdateZoneSizing(EndDay)* to calculate peaks and moving averages from the zone design sequences for each design day.

* Call *UpdateZoneSizing(EndZoneSizingCalc)* to calculate for each zone the peak heating & cooling loads and flow rates over all the sizing periods (design days and sizing periods from the weather file, if specified). The corresponding design load and flow rate sequences are saved for use in the system design calculations. **This ends the zone design calculations.**

*  Set the *SysSizingCalc* flag equal to *true*.

* Call *ManageZoneEquipment* and *ManageAirLoops* to read in the zone and central system inputs needed for the system design calculations. The program needs enough information to be able to figure out the overall air loop connectivity.

*  Loop over all the sizing periods by each day. **This starts the system design calculations.**

    * Call *UpdateSysSizing(BeginDay)* to initialize system design load and flow rate  sequences.

    * Loop over hours in the day

        * Loop over zone time steps in each hour

            * Call *ManageWeather* to obtain outside conditions for this time-step.

            * Call *UpdateSysSizing(DuringDay)* to save the results of the system design calculations in the system design load and flow rate sequences.

    * Call *UpdateSysSizing(EndDay)* to calculate peaks and moving averages from the system design sequences for each sizing period.

* Call *UpdateSysSizing(EndSysSizingCalc))* to calculate for each system the peak heating & cooling loads and flow rates over all the sizing periods (design days and sizing periods from the weather file, if specified). The corresponding design load and flow rate sequences are saved for use in the component sizing calculations. **This ends the system design calculations.**

* And this ends the tasks of the Sizing Manager.

HVAC Sizing Simulation Manager
------------------------------

After the Sizing Manager has completed its initial pass, all the data needed to complete a running model should be available and the program is ready to run the main simulation(s).  However, as of Version 8.3 there is now an option of applying some advanced sizing calculations using what are called HVAC Sizing Simulations.  

With this new sizing method we distinguish between different kinds of simulations and introduce some new terminology.  The Primary Simulations are the main simulations that are the final version of the model to be run.  Prior to Version 8.3, these are just the usual simulations with the final results.  When the program is running the zone heat balance model over the sizing periods for zone sizing (for step 3 in the Sizing Manager description above), we call those Ideal Loads Sizing Simulations.  When the program is running the zone heat balance model over the sizing periods for component loads calculations we call those Ideal Component Loads Simulations. HVAC Sizing Simulations are a kind of simulation, where the program creates copies of sizing periods and runs them as complete EnergyPlus simulations with the most current equipment sizes and full HVAC systems.  The advanced sizing algorithms monitor what occurred during those sizing periods and determines if new size results are needed and signals systems and components to repeat their sizing calculations.  The process can repeat in an iterative manner and a Sizing Pass refers to a set of the HVAC Sizing Simulations for each of the sizing periods (e.g. two design days).  

If the user has selected a sizing option that requires HVAC Sizing Simulations the main ManageSimulation will call ManageHVACSizingSimulation before going on to the main simulations.  

* Instantiate a new HVACSizingSimulationManager object.  

* Call DetermineSizingAnalysesNeeded(). This checks what user input and decides what, if anything, needs to be done for advanced sizing algorithms.   This involves, for example, checking the input in Sizing:Plant object to see if coincident sizing option has been selected.

* Call SetupSizingAnalyses().  This method creates the data logging apparatus needed to monitor operation during HVAC Sizing Simulations.  Individual sizing algorithms include selecting specific variables, such as system node states or load output variables, that will be recorded. 

* Loop over some number of Sizing Passes.  The set of sizing periods, run as HVAC Sizing Simulations, can iterate up to a maximum limit on the number of passes

    * Loop over all the sizing periods by each day.  This runs the HVAC Sizing Simulations which have basically the same set of calls as are used for marching through time and calling of EnergyPlus modeling for the Primary Simulations (in ManageSimulation).

        * Call PostProcessLogs().  This method applies running averages (if desired) and averages system timestep data to fill zone timestep data in the records.

        * Call ProcessCoincidentPlantSizeAdjustments(). This method retrieves data from the logs and calls for the coincident plant sizing algorithm to execute. Set flag if the sizing analyses request another Sizing Pass.  (See the section below on Coincident Plant Sizing.)

        * Call RedoKickOffAndResize(). The methods calls SetupSimulation() and sets flag to signal that system and component level sizing methods need to be called again.  These are fake timesteps used to initialize and are not part of a Simulation.

    * Break out of Sizing Pass loop if size results did not change or the limit on Sizing Passes has been reached.

* Empty HVACSizingSimulationManager object to free memory

Currently the only application for HVAC Sizing Simulations is to improve the sizing of plant loops using the “Coincident” sizing option.  However this approach may be expanded in the future to extend advanced sizing methods to air-side equipment. 


Zone Design Loads and Air Flow Rates
------------------------------------

### Overview

There is no single best way to establish design HVAC flow rates and size HVAC equipment. Different building designs, climates, and HVAC systems will impose varying constraints on the designer. The method used to size an HVAC system in a hot, moist climate such as Miami will be different than the method used for a building in Albuquerque. The type of building is also relevant – a simple watts per square foot loads estimate could be adequate for a building containing a network server farm while a detailed, dynamic loads simulation would be necessary for a passive solar building. In the end the designer’s experience and engineering judgement will play an important role in any sizing calculation.

HVAC equipment sizing begins with the calculation of space heating and cooling loads. A space cooling  (heating) load is defined as the rate at which heat must be removed (added) to a space to maintain a constant temperature. The current industry standard method for calculating space loads is the *heat balance method*  [ASHRAE Fundamentals (2001), page 29.1; Pedersen et al., (1997); Pedersen (2001). Since EnergyPlus is a heat balance based simulation program it is straightforward for the program to use this method for calculating zone loads.

### Zone Design Data Arrays

The zone design data arrays are:

*ZoneSizingInput(i)* stores the input data from the Sizing:Zone objects.

*CalcZoneSizing(i,j)* stores the results of the zone design calculations for all zones and all design days. The index i is for the controlled zones, j for design days.

*CalcFinalZoneSizing(i)* stores the results of the zone design calculations for the peak heating and cooling cases for each zone. The index i is for the controlled zones.

*ZoneSizing(i,j)* corresponds to *CalcZoneSizing* but includes the effect of the user specified sizing factor or user specified zone design flow rate.

*FinalZoneSizing(i)* corresponds to *CalcFinalZoneSizing* but includes the effect of the user specified sizing factor or user specified zone design flow rate.

The data stored in *CalcZoneSizing*, *CalcFinalZoneSizing*, *ZoneSizing* and *FinalZoneSizing* includes the following data items.



Table 40.  Zone Sizing Data

<table class="table table-striped">





<tr>
<td>Name</td>
<td>Description</td>
</tr>
<tr>
<td>All the data from ZoneSizingInput</td>
<td> </td>
</tr>
<tr>
<td>DesHeatMassFlow</td>
<td>the zone design heating air mass flow rate in [kg/s]</td>
</tr>
<tr>
<td>DesCoolMassFlow</td>
<td>the zone design cooling air mass flow rate in [kg/s]</td>
</tr>
<tr>
<td>DesHeatLoad</td>
<td>the zone design heating load in [W]</td>
</tr>
<tr>
<td>DesCoolLoad</td>
<td>the zone design cooling load in [W]</td>
</tr>
<tr>
<td>DesHeatDens</td>
<td>the zone design heating air density [kg/m<sup>3</sup>]</td>
</tr>
<tr>
<td>DesCoolDens</td>
<td>the zone design cooling air density [kg/m<sup>3</sup>]</td>
</tr>
<tr>
<td>DesHeatVolFlow</td>
<td>the zone design heating air volume flow rate [m<sup>3</sup>/s]</td>
</tr>
<tr>
<td>DesCoolVolFlow</td>
<td>the zone design cooling air volume flow rate [m<sup>3</sup>/s]</td>
</tr>
<tr>
<td>DesHeatCoilInTemp</td>
<td>zone heating coil design air inlet temperature [C]</td>
</tr>
<tr>
<td>DesCoolCoilInTemp</td>
<td>zone cooling coil design air inlet temperature [C]</td>
</tr>
<tr>
<td>DesHeatCoilInHumRat</td>
<td>the zone heating coil design air inlet humidity ratio [kg/kg]</td>
</tr>
<tr>
<td>DesCoolCoilInHumRat</td>
<td>the zone cooling coil design air inlet humidity ratio [kg/kg]</td>
</tr>
<tr>
<td>HeatMassFlow</td>
<td>current zone heating air mass flow rate at the HVAC time step [kg/s]</td>
</tr>
<tr>
<td>CoolMassFlow</td>
<td>current zone cooling air mass flow rate at the HVAC time step [kg/s]</td>
</tr>
<tr>
<td>HeatLoad</td>
<td>Current zone heating load [W]</td>
</tr>
<tr>
<td>CoolLoad</td>
<td>Current zone cooling load [W]</td>
</tr>
<tr>
<td>HeatZoneTemp</td>
<td>Current zone temperature during heating [C]</td>
</tr>
<tr>
<td>HeatZoneRetTemp</td>
<td>current zone return temperature during heating [C]</td>
</tr>
<tr>
<td>CoolZoneTemp</td>
<td>Current zone temperature during cooling [C]</td>
</tr>
<tr>
<td>CoolZoneRetTemp</td>
<td>current zone return temperature during cooling [C]</td>
</tr>
<tr>
<td>HeatZoneHumRat</td>
<td>Current zone humidity ratio during heating [C]</td>
</tr>
<tr>
<td>CoolZoneHumRat</td>
<td>Current zone humidity ratio during cooling [C]</td>
</tr>
<tr>
<td>ZoneTempAtHeatPeak</td>
<td>zone temperature at maximum heating [C]</td>
</tr>
<tr>
<td>ZoneRetTempAtHeatPeak</td>
<td>zone return temperature at maximum heating [C]</td>
</tr>
<tr>
<td>ZoneTempAtCoolPeak</td>
<td>zone temperature at maximum cooling [C]</td>
</tr>
<tr>
<td>ZoneRetTempAtCoolPeak</td>
<td>zone return temperature at maximum cooling [C]</td>
</tr>
<tr>
<td>ZoneHumRatAtHeatPeak</td>
<td>zone humidity ratio at maximum heating [kg/kg]</td>
</tr>
<tr>
<td>ZoneHumRatAtCoolPeak</td>
<td>zone humidity ratio at maximum cooling [kg/kg]</td>
</tr>
<tr>
<td>TimeStepNumAtHeatMax</td>
<td>zone time step number (in the day) at the heating peak</td>
</tr>
<tr>
<td>TimeStepNumAtCoolMax</td>
<td>zone time step number (in the day) at the cooling peak</td>
</tr>
<tr>
<td>HeatDDNum</td>
<td>design day index of design day causing heating peak</td>
</tr>
<tr>
<td>CoolDDNum</td>
<td>design day index of design day causing cooling peak</td>
</tr>
<tr>
<td>MinOA</td>
<td>design minimum outside air [m3/s]</td>
</tr>
<tr>
<td>HeatFlowSeq(i)</td>
<td>daily sequence of zone heating air mass flow rates (zone time step) [kg/s]</td>
</tr>
<tr>
<td>CoolFlowSeq(i)</td>
<td>daily sequence of zone cooling air mass flow rates (zone time step) [kg/s]</td>
</tr>
<tr>
<td>HeatLoadSeq(i)</td>
<td>daily sequence of zone heating loads (zone time step) [W]</td>
</tr>
<tr>
<td>CoolLoadSeq(i)</td>
<td>daily sequence of zone cooling loads (zone time step) [W]</td>
</tr>
<tr>
<td>HeatZoneTempSeq(i)</td>
<td>daily sequence of zone temperatures (heating, zone time step) [C]</td>
</tr>
<tr>
<td>HeatZoneRetTempSeq(i)</td>
<td>daily sequence of zone return temperatures (heating, zone time step) [C]</td>
</tr>
<tr>
<td>CooltZoneTempSeq(i)</td>
<td>daily sequence of zone temperatures (cooling, zone time step) [C]</td>
</tr>
<tr>
<td>CoolZoneRetTempSeq(i)</td>
<td>daily sequence of zone return temperatures (cooling, zone time step) [C]</td>
</tr>
<tr>
<td>HeatZoneHumRatSeq(i)</td>
<td>daily sequence of zone humidity ratios (heating, zone time step) [kg/kg]</td>
</tr>
<tr>
<td>CoolZoneHumRatSeq(i)</td>
<td>daily sequence of zone humidity ratios (cooling, zone time step) [kg/kg]</td>
</tr>

</table>



### Zone Design Load Calculation

As described in the preceding section, the Sizing Manager initiates the zone design calculation by looping over all of the design days and calling the Heat Balance Manager for each zone time-step in each design day. The Heat Balance manager then causes the HVAC Manager to be called in a manner identical to a normal simulation. The *ZoneSizingCalc* set to *true* signals the HVAC Manager to ignore the actual HVAC system and instead calculate the design zone loads and air flow rates using an ideal zonal system.

In module *HVACManager*, subroutine *ManageHVAC* calls *SimHVAC*. *SimHVAC* checks *ZoneSizingCalc*. If it is *true*, *SimHVAC* calls *ManageZoneEquipment* and returns, rather than simulating the actual system. In turn *ManageZoneEquipment* checks if *ZoneSizingCalc* is *true*; if it is it calls *SizeZoneEquipment* rather than *SimZoneEquipment*.

*SizeZoneEquipment* assumes that each controlled zone is served by an ideal air conditioning unit. This unit supplies heating or cooling air at a fixed, user input temperature and humidity (specified in the Sizing:Zone objects). The units have infinite capacity – the flow rate can be any amount. The calculation steps are as follows.

1)    Loop over all the controlled zones.

2)    If the system is active (zone temperature not in the deadband and zone load greater than 1 watt) the sign of the zone load is used to determine whether heating or cooling is required and *T<sub>in</sub>* and W*<sub>in</sub>* are set to the appropriate values from the Sizing:Zone input. When the SupplyTemperature method is specified in the Sizing:Zone object, *T<sub>in</sub>* is fixed at the cooling or heating supply temperature. When the TemperatureDifference method is selected, *T<sub>in</sub>* is calculated at each time step using the current zone air temperature. The system output *Q<sub>sys</sub>* is simply set equal to the zone demand – it is assumed that the ideal system can always meet the zone load. The air flow rate corresponding to the load is just

<div>\[{\dot m_{sys}} = {Q_{sys}}/({C_{p,air}} \cdot ({T_{in}} - {T_z}))\]</div>

If the system is not active, the mass flow rate is set to zero and the system output is left at zero.

3)    The results for each zone are stored in the zone sizing data arrays.

### Updating and Adjusting the Zone Results

The results from *SizeZoneEquipment* are at the system time-step and are for all design days. These results then need to be summed or averaged over the zone time-step, peak values calculated for each design day, a heating & a cooling load sequence chosen for each zone from all the design day results, possible further smoothing of results done, zone coil loads calculated, and user sizing multipliers or user specified design flows taken into account. These tasks are accomplished by the subroutine *UpdateZoneSizing*. It is called at the start of each design day (*CallIndicator = BeginDay*), at the zone time-step (*CallIndicator = DuringDay*), at the end of the design day (*CallIndicator = EndDay*) and at the end of the zone design calculation (*CallIndicator = EndZoneSizingCalc*).

#### BeginDay

The environment (in this case, a design day) name and number are stored in the zone sizing data structures

#### DuringDay

The calculated and stored sequences are summed or averaged over the zone time-step.

#### EndDay

(1)  Smooth the design sequences by applying a moving, fixed-width averaging window to the sequences. The width of the window is user specified in the *Sizing:Parameters* input object. The sequences that are smoothed are:

(a)  *CoolFlowSeq*

(b)  *CoolLoadSeq*

(c)  *HeatFlowSeq*

(d)  *HeatLoadSeq*

(e)  *CoolZoneRetTempSeq*

(f)   *HeatZoneRetTempSeq*

(2)  The peak heating and cooling loads and mass & volume flow rates are extracted from each set of design sequences.

(3)  Using the time of the peak and the design outside air fraction the design zone heating and cooling coil inlet temperatures and humidity ratios are calculated.

(4)  For each zone, looking at the results for all of the design days, the design days that cause the peak heating and peak cooling for that zone are chosen and the corresponding design sequences and peak loads and flow rates are saved in the CalcFinalZoneSizing array. This finishes the calculated – unmodified by the user – portion of the zone design calculation.

#### EndZoneSizingCalc

(1)  Write out onto a comma-separated file the calculated design sequences for each zone: *HeatLoadSeq*, *CoolLoadSeq*, *HeatFlowSeq*, *CoolFlowSeq* and the corresponding peaks and volumetric flow peaks.

(2)  The data in *CalcZoneSizing* and *CalcFinalZoneSizing* is moved to *ZoneSizing* and *FinalZoneSizing*. The user modifications to the calculated sizing will be applied to and stored in *ZoneSizing* and *FinalZoneSizing*.

(3)  The user can modify the calculated zone design results by specifying heating and cooling sizing factors at the global or zone level or by specifying and actual design heating or cooling zone design volumetric flow rate. All of this input is treated as a sizing factor. If the user inputs a cooling design volumetric flow rate for a zone it is divided by the calculated cooling design volumetric flow rate for the zone to give a zone cooling sizing factor. Note that the user can input a zone sizing factor or a zone design flow rate – not both – so there is never a conflict.

(4)  Once the zone heating and cooling sizing factors are established, the design flow and load sequences as well as peak loads and flows are multiplied by the appropriate sizing factor and stored in *ZoneSizing* and *FinalZoneSizing*. This is the data that will be used for sizing zone HVAC equipment and in the system sizing calculation.

(5)  The outside air fractions are recalculated using the new user-modified design flow rates and new design zone coil inlet conditions calculated and stored. At this point the condition that the design flow rates are never allowed to be less than the minimum outside air flow rate is imposed.

If *outside air method* is *flow/zone*, the input *outside air flow per zone* value will be used, even if it is zero or blank. If *outside air method* is *sum*, the sum of the *outside air flow per person* \* *DesignNumberOfPeople* + *outside air flow per area* \* *ZoneArea* will be used. If *outside air method* is *maximum*, the maximum of the *outside air flow per person* \* *DesignNumberOfPeople* and *outside air flow per area* \* *ZoneArea* will be used. If *outside air method* is *flow/person*, *outside air flow per person* will be used to calculate the design minimum outside airflow rate.

If *cooling design air flow method* is *flow/zone*, then *cooling design air flow rate* will be used for the design max cooling air flow rate.  If *cooling design air flow method* is *design day*, then the design day calculation will set the design max cooling air flow rate. If  *cooling design air flow method* is *design day with limit*, then the maximum from *cooling min flow per area* and *cooling min flow* will set a lower limit on the design max cooling air flow rate. In all cases, the maximum from *cooling min flow per area*, *cooling min flow*, and *cooling min flow fraction* will set a minimum zone cooling air flow rate. In all cases the maximum design cooling air flow rate must be &gt;= to the ventilation requirement.

If *heating design air flow method* is *flow/zone*, then *heating design air flow rate* will be used for the design max heating air flow rate.  If *heating design air flow method* is *design day*, then the design day calculation will set the design max heating air flow rate. If *heating design air flow method* is *design day with limit*, then the maximum from *heating max flow per area*, *heating max flow* and *heating max flow fraction* will set an upper limit on the design max heating air flow rate. The design max heating air flow rate must always be &gt;= the ventilation requirement. In each case, the outside airflow will be modified based on zone ventilation effectiveness specified in the zone sizing object.

**This concludes the calculation of the zone design flow rates and loads.**

### Zone HVAC Scalable Sizing

For zone HVAC equipments scalable sizing applies to supply air flow rate and capacity for both cooling and heating.  The scalable sizing method allowed for supply air flow rates include: *FractionOfAutosizedCoolingAirflow*, *FractionOfAutosizedHeatingAirflow*, *FlowPerFloorArea, FlowPerCoolingCapacity*, and *FlowPerHeatingCapacity*.  The supply air flow rate scalable sizing methods are defined as follows:



***FlowPerFloorArea***: the simulation engine determine the supply air flow rates from the user specified *supply air flow rates per unit floor area* and the zone floor area of the zone served by the zone HVAC equipment.



***FractionOfAutosizedCoolingAirflow***: the simulation engine determines the supply air flow rates from the user specified *flow fraction* and *autosized cooling design supply air flow rate*.



***FractionOfAutosizedHeatingAirflow***: the simulation engine determines the supply air flow rates from the user specified *flow fraction* and *autosized heating design supply air flow rate*.



***FlowPerCoolingCapacity***: he simulation engine determines the supply air flow rates from the user specified *supply air flow per cooling capacity value* and *autosized cooling design capacity*.



***FlowPerHeatingCapacity***: the simulation engine determines the supply air flow rates from the user specified *supply air flow per heating capacity value* and *autosized heating design capacity*.



The scalable capacity sizing may be indirectly impacted by the scalable supply air flow rates sizing values. Moreover, the autosized cold water, hot water and steam flow rates in the parent zone HVAC objects and capacity in child components are determined using the scalable sizing method.  Scalable capacity sizing methods allowed for cooling and heating include: *CapacityPerFloorArea, FractionOfAutosizedCoolingCapacity*, *FractionOfAutosizedHeatingCapacity*. The scalable sizing capacity methods are defined as follows:



**CapacityPerFloorArea**: the simulation engine determines the cooling or heating capacity from user specified capacity per floor area value and the floor area of the zone served by the zone HVAC equipment.



**FractionOfAutosizedCoolingCapacity**: the simulation engine sizes the cooling capacity from the user specified *capacity fraction* and *autosized cooling design capacity* value.



**FractionOfAutosizedHeatingCapacity**: the simulation engine sizes the heating capacity from the user specified *capacity fraction* and *autosized heating design capacity* value.

System Design Loads and Air Flow Rates
--------------------------------------

### Overview

The purpose of the system design calculation is to estimate design heating and cooling loads and air flow rates for each air loop in the simulation problem. The calculation sequence for system level design loads and air flow rates resembles the calculation sequence for zone loads and air flow rates. There is an update subroutine *UpdateSysSizing* called at the beginning, during, and end  of a loop in the Sizing Manager over all the design days. The major difference is that this calculation is done at the zone time-step only. There is no idealized component calculation triggered at the system time-step as in the zone calculation. The system design calculation operates at the zone time step using the design environment weather data and the data stored in the zone sizing arrays. The results of the system design calculation are stored in the system sizing arrays described below.

### System Design Data Arrays

The system design data arrays are:

*SysSizInput(i)* stores the input data from the Sizing:System objects.

*SysSizing(i,j)* stores the results of the system design calculations for all systems and all design days. The index i is for air loops, j for design days.

*CalcSysSizing(i*) stores the results of the system design calculations for the peak heating and cooling cases for each air loop. The index i is for the air loops.

*FinalSysSizing(i*) corresponds to *CalcSysSizing* but includes the effect of the user specified sizing factor or user specified system design flow rate.

The data stored in *SysSizing*, *CalcSysSizing* and *FinalSysSizing* includes the following data items.



Table 41.  System Sizing Data

<table class="table table-striped">





<tr>
<td>Name</td>
<td>Description</td>
</tr>
<tr>
<td>All the data from SysSizInput</td>
<td> </td>
</tr>
<tr>
<td>CoinCoolMassFlow</td>
<td>coincident peak cooling mass flow rate [kg/s]</td>
</tr>
<tr>
<td>CoinHeatMassFlow</td>
<td>coincident peak heating mass flow rate [kg/s]</td>
</tr>
<tr>
<td>NonCoinCoolMassFlow</td>
<td>noncoincident peak cooling mass flow rate [kg/s]</td>
</tr>
<tr>
<td>NonCoinHeatMassFlow</td>
<td>noncoincident peak heating mass flow rate [kg/s]</td>
</tr>
<tr>
<td>DesMainVolFlow</td>
<td>design main supply duct volume flow [m<sup>3</sup>/s]</td>
</tr>
<tr>
<td>DesHeatVolFlow</td>
<td>design heat supply duct volume flow [m<sup>3</sup>/s]</td>
</tr>
<tr>
<td>DesCoolVolFlow</td>
<td>design cool supply duct volume flow [m<sup>3</sup>/s]</td>
</tr>
<tr>
<td>SensCoolCap</td>
<td>design sensible cooling capacity [W]</td>
</tr>
<tr>
<td>HeatCap</td>
<td>design heating capacity [W]</td>
</tr>
<tr>
<td>PreheatCap</td>
<td>design preheat capacity [W]</td>
</tr>
<tr>
<td>CoolMixTemp</td>
<td>design mixed air temperature for cooling [C]</td>
</tr>
<tr>
<td>CoolMixHumRat</td>
<td>design mixed air humidity ratio for cooling [kg water/kg dry air]</td>
</tr>
<tr>
<td>CoolRetTemp</td>
<td>design return air temperature for cooling [C]</td>
</tr>
<tr>
<td>CoolRetHumRat</td>
<td>design return air humidity ratio for cooling [kg water/kg dry air]</td>
</tr>
<tr>
<td>CoolOutTemp</td>
<td>design outside air temperature for cooling [C]</td>
</tr>
<tr>
<td>CoolOutHumRat</td>
<td>design outside air humidity ratio for cooling [kg water/kg dry air]</td>
</tr>
<tr>
<td>HeatMixTemp</td>
<td>design mixed air temperature for heating [C]</td>
</tr>
<tr>
<td>HeatMixHumRat</td>
<td>design mixed air humidity ratio for heating [kg water/kg dry air]</td>
</tr>
<tr>
<td>HeatRetTemp</td>
<td>design return air temperature for heating [C]</td>
</tr>
<tr>
<td>HeatRetHumRat</td>
<td>design return air humidity ratio for heating [kg water/kg dry air]</td>
</tr>
<tr>
<td>HeatOutTemp</td>
<td>design outside air temperature for heating [C]</td>
</tr>
<tr>
<td>HeatOutHumRat</td>
<td>design outside air humidity ratio for heating [kg water/kg dry air]</td>
</tr>
<tr>
<td>HeatFlowSeq(i)</td>
<td>daily sequence of system heating air mass flow rate (zone time step) [kg/s]</td>
</tr>
<tr>
<td>CoolFlowSeq(i)</td>
<td>daily sequence of system cooling air mass flow rate (zone time step) [kg/s]</td>
</tr>
<tr>
<td>SensCoolCapSeq(I)</td>
<td>daily sequence of system sensible cooling capacity (zone time step) [W]</td>
</tr>
<tr>
<td>HeatCapSeq(i)</td>
<td>daily sequence of system heating capacity (zone time step) [W]</td>
</tr>
<tr>
<td>PreHeatCapSeq(i)</td>
<td>daily sequence of system preheat capacity (zone time step) [W]</td>
</tr>
<tr>
<td>SysCoolRetTempSeq(i)</td>
<td>daily sequence of system cooling return temperatures  (zone time step) [C]</td>
</tr>
<tr>
<td>SysCoolRetHumRatSeq(I)</td>
<td>daily sequence of system cooling return humidity ratios (zone time step) [kg water/kg dry air]</td>
</tr>
<tr>
<td>SysHeatRetTempSeq(i)</td>
<td>daily sequence of system heating return temperatures  (zone time step) [C]</td>
</tr>
<tr>
<td>SysHeatRetHumRatSeq(I)</td>
<td>daily sequence of system heating return humidity ratios (zone time step) [kg water/kg dry air]</td>
</tr>
<tr>
<td>SysCoolOutTempSeq</td>
<td>daily sequence of system cooling outside temperatures (zone time step) [C]</td>
</tr>
<tr>
<td>SysCoolOutHumRatSeq</td>
<td>daily sequence of system cooling outside humidity ratios (zone time step) [kg water/kg dry air]</td>
</tr>
<tr>
<td>SysHeatOutTempSeq</td>
<td>daily sequence of system heating outside temperatures (zone time step) [C]</td>
</tr>
<tr>
<td>SysHeatOutHumRatSeq</td>
<td>daily sequence of system heating outside humidity ratios (zone time step) [kg water/kg dry air]</td>
</tr>

</table>

### System Design Flow Rate and Load Summation and Adjustment

There is no system level subroutine corresponding to *SizeZoneEquipment.* Instead the system design loads and flow rates are calculated using the zone level results. The zone design flow rates for the zones served by an air loop are summed to obtain the system level design flow rates. These air flows are mixed with the system level design minimum outside air flow rate to obtain system design coil loads. These activities are all performed within the *UpdateSysSizing* subroutine in the *SimAirServingZones* module. It is called at the start of each design day (*CallIndicator = BeginDay*), at the zone time-step (*CallIndicator = DuringDay*), at the end of the design day (*CallIndicator = EndDay*) and at the end of the zone design calculation (*CallIndicator = EndSysSizingCalc*).

There is a logical flag *SysSizingCalc* corresponding to *ZoneSizingCalc*.  It is used to allow the component routines to distinguish a normal simulation call from a being called during a system sizing calculation.

#### BeginDay

(1)  The environment (in this case, a design day) name is stored in the system sizing data structures.

(2)  Loop over the zones cooled by this air loop:

(a)  *NonCoinCoolMassFlow<sub>sys</sub>*=**å***DesCoolMassFlow<sub>zone</sub>*

(3)  Loop over the zones heated by this air loop:

(a)  *NonCoinHeatMassFlow<sub>sys</sub>*=**å***DesHeatMassFlow<sub>zone</sub>*

#### DuringDay

(1)  Loop over the zones cooled by this air loop:

*CoolFlowSeq<sub>sys</sub>(i)* =**å***CoolFlowSeq <sub>zone</sub>*(i)

*SysCoolRetTemp(i)*=**å**(*CoolZoneRetTempSeq(i)· CoolFlowSeq<sub>zone</sub>(i)*)*/ CoolFlowSeq<sub>sys</sub>(i)*

*SysCoolRetHumRat(i)*=**å**(*CoolZoneHumRatSeq(i)· CoolFlowSeq<sub>zone</sub>(i)*)*/ CoolFlowSeq<sub>sys</sub>(i)*

*FracOA=r<sub>air</sub>*· *DesOutAirVolFlow<sub>sys</sub>*/ *CoolFlowSeq<sub>sys</sub>(i)*

*T<sub>mix</sub>*=*T<sub>outside</sub>*· *FracOA* + *SysCoolRetTemp(i)*(1 – *FracOA*)

*W<sub>mix</sub>*=*W<sub>outside</sub>*· *FracOA* + *SysCoolRetHumRat (i)*(1 – *FracOA*)

*SysCoolOutTempSeq(i)*= *T<sub>outside</sub>*

*SysCoolOutHumRatSeq(i)*= *W<sub>outside</sub>*

Get the current (zone time-step) system cooling capacity:

*SysSensCoolCap<sub>cur</sub>*=*C<sub>p,air</sub>*· *CoolFlowSeq<sub>sys</sub>(i)* ·( *T<sub>mix</sub>*-*T<sub>sup</sub>*)

*SensCoolCapSeq(I)*= *SysSensCoolCap<sub>cur</sub>*

If *SysSensCoolCap<sub>cur</sub>* is the maximum for the day so far then save *SysSensCoolCap<sub>cur</sub>*  as the design value:

*SensCoolCap(i )<sub>sys</sub>*= *SysSensCoolCap<sub>cur</sub>*

And save the corresponding mixed, return and outside conditions:

*CoolMixTemp<sub>sys</sub>*= *T<sub>mix</sub>*

*CoolMixHumRat<sub>sys</sub>*=*W<sub>mix</sub>*

*CoolRetTemp<sub>sys</sub>*= *SysCoolRetTemp(i)*

*CoolRetHumRat<sub>sys</sub>*= *SysCoolRetHumRat(I)*

*CoolOutTemp<sub>sys</sub>*= *T<sub>outside</sub>*

*CoolOutHumRat<sub>sys</sub>*= *W<sub>outside</sub>*

Here     *r<sub>air</sub>*is the density of dry air at 20C and standard elevation corrected pressure, [kg/m<sup>3]</sup>;

            *FracOA* is the outside air fraction;

*            C<sub>p,air</sub>* is the specific heat of dry air at 20C, [J/kg-K];

*            T<sub>sup</sub>* is the user specified design cooling supply temperature [C];

*            T<sub>mix</sub>* is the current mixed air temperature [C];

*            W<sub>mix</sub>* is the current mixed air humidity ratio [kg water / kg dry air];

            *T<sub>outside</sub>* is the current outside air temperature [C];

*            W<sub>outside</sub>* is the current outside air humidity ratio [kg water / kg dry air].

(2)  Loop over the zones heated by this air loop.

*HeatFlowSeq<sub>sys</sub>(i)* =**å***HeatFlowSeq <sub>zone</sub>*(i)

*SysHeatRetTemp(i)*=**å**(*HeatZoneRetTempSeq(i)· HeatFlowSeq<sub>zone</sub>(i)*)*/*

*HeatFlowSeq<sub>sys</sub>(i)*

*SysHeatRetHumRat(i)*=**å**(*HeatZoneHumRatSeq(i)· HeatFlowSeq<sub>zone</sub>(i)*)*/*

* HeatFlowSeq<sub>sys</sub>(i)*

*FracOA=r<sub>air</sub>*· *DesOutAirVolFlow<sub>sys</sub>*/ *HeatFlowSeq<sub>sys</sub>(i)*

*T<sub>mix</sub>*=*T<sub>outside</sub>*· *FracOA* + *SysHeatRetTemp(i)*(1 – *FracOA*)

*W<sub>mix</sub>*=*W<sub>outside</sub>*· *FracOA* + *SysHeatRetHumRat (i)*(1 – *FracOA*)

*SysHeatOutTempSeq(i)*= *T<sub>outside</sub>*

*SysHeatOutHumRatSeq(i)*= *W<sub>outside</sub>*

Get the current (zone time-step) system heating capacity:

*SysHeatCap<sub>cur</sub>*=*C<sub>p,air</sub>*· *MinFlowRat<sub>sys</sub>*·*HeatFlowSeq<sub>sys</sub>(i)* ·( *T<sub>sup</sub>*-*T<sub>mix</sub>*)

*HeatCapSeq(I)*= *SysHeatCap<sub>cur</sub>*

If *SysHeatCap<sub>cur</sub>* is the maximum for the day so far then save *SysHeatCap<sub>cur</sub>*  as the design value:

*HeatCap(i )<sub>sys</sub>*= *SysHeatCap<sub>cur</sub>*

And save the corresponding mixed, return and outside conditions:

*HeatMixTemp<sub>sys</sub>*= *T<sub>mix</sub>*

*HeatMixHumRat<sub>sys</sub>*=*W<sub>mix</sub>*

*HeatRetTemp<sub>sys</sub>*= *SysHeatRetTemp(i)*

*HeatRetHumRat<sub>sys</sub>*= *SysHeatRetHumRat(I)*

*HeatOutTemp<sub>sys</sub>*= *T<sub>outside</sub>*

*HeatOutHumRat<sub>sys</sub>*= *W<sub>outside</sub>*

Here *MinFlowRat<sub>sys</sub>* is the user specified minimum supply flow ratio.

#### EndDay

If the user has specified *coincident* system sizing then:

*DesCoolVolFlow<sub>sys</sub>*=*r<sub>air</sub>*·*CoinCoolMassFlow<sub>sys</sub>*

*DesHeatVolFlow<sub>sys</sub>*=*r<sub>air</sub>*·*CoinHeatMassFlow<sub>sys</sub>*

*DesMainVolFlow<sub>sys</sub>*=**Max**(*DesCoolVolFlow<sub>sys</sub>*, *DesHeatVolFlow<sub>sys</sub>*)

If the user has specified *noncoincident*system sizing then:

*DesCoolVolFlow<sub>sys</sub>*=*r<sub>air</sub>*·*NonCoinCoolMassFlow<sub>sys</sub>*

*DesHeatVolFlow<sub>sys</sub>*=*r<sub>air</sub>*·*NonCoinHeatMassFlow<sub>sys</sub>*

*DesMainVolFlow<sub>sys</sub>*=**Max**(*DesCoolVolFlow<sub>sys</sub>*, *DesHeatVolFlow<sub>sys</sub>*)

Based on the outdoor air method selected, the *DesCoolVolFlow<sub>sys</sub>* and *DesHeatVolFlow<sub>sys</sub>* are modified based on the system ventilation effciency calculated based on the maximum outdoor air fraction.

#### EndSysSizingCalc

At this point all the calculations have been done in *SysSizing(i,j)*: we have results for each design day. Now these results need to be processed to find the heating and cooling design quantities for each system over all the design days.

For coincident sizing the task is quite easy.

(1)  Loop over all of the air loops.

(a)  Loop over all of the design days.

(i)    If the value of *DesCoolVolFlow* in *SysSizing* for the current design day is greater than the value stored in *CalcSysSizing*, then move *DesCoolVolFlow* from *SysSizing* into *CalcSysSizing* along with *CoolDesDay*, *CoinCoolMassFlow*, *SensCoolCap*, *CoolFlowSeq(i)*, *SensCoolCapSeq(i),* *CoolMixTemp*, *CoolRetTemp*, *CoolMixHumRat*, *CoolRetHumRat*, *CoolOutTemp*, *CoolOutHumRat*, *SysCoolRetTempSeq(i)*, *SysCoolRetHumRatSeq(i)*, *SysCoolOutTempSeq(i)* and *SysCoolOutHumRatSeq(i)*.

(ii)   If the value of *DesHeatVolFlow* in *SysSizing* for the current design day is greater than the value stored in *CalcSysSizing*, then move *DesHeatVolFlow* from *SysSizing* into *CalcSysSizing* along with *HeatDesDay*, *CoinHeatMassFlow*, *HeatCap*, *PreHeatCap*, *HeatFlowSeq(i)*, *HeatCapSeq(i),* *PreHeatCapSeq(i), HeatMixTemp*, *HeatRetTemp*, *HeatMixHumRat*, *HeatRetHumRat*, *HeatOutTemp*, *HeatOutHumRat*, *SysHeatRetTempSeq(i)*, *SysHeatRetHumRatSeq(i)*, *SysHeatOutTempSeq(i)* and *SysHeatOutHumRatSeq(i)*.

At the end of each design day loop the peak cooling and the peak heating data will be stored in *CalcSysSizing*. At this point we set *DesMainVolFlow* in *CalcSysSizing* equal to the maximum of *DesCoolVolFlow* and *DesHeatVolFlow.*

For noncoincident sizing the task is harder since we don’t have a single time-step during which all the zone peaks occur. So there is no obvious value for outside air temperature at the peak, return air temperature at the peak and so forth. We must return to the zone sizing data and calculate average values for return and outside conditions.

(b)  Loop over all of the zones cooled by this air loop

(i)    In *FinalZoneSizing* replace the value in *DesCoolCoilInTemp* with the user specified *CoolSupTemp<sub>sys</sub>*. Do the same for *DesCoolCoilInHumRat* and *CoolSupHumRat*. This ensures that zone equipment connected to an air loop will use the system design supply air conditions as coil entering conditions.

(ii)   *NonCoinCoolMassFlow<sub>sys</sub>*=**å***DesCoolMassFlow<sub>zone</sub>*

*SysCoolRetTemp*=**å**(*ZoneRetTempAtCoolPeak·DesCoolMassFlow<sub>zone</sub>*)

/*NonCoinCoolMassFlow<sub>sys</sub>*

*SysCoolRetHumRat*=**å**(*ZoneHumRatAtCoolPeak·*

*DesCoolMassFlow<sub>zone</sub>*)/*NonCoinCoolMassFlow<sub>sys</sub>*

*SysCoolOutTemp*=**å**(*T<sub>OA,zone\\ peak</sub>·DesCoolMassFlow<sub>zone</sub>*)/

* NonCoinCoolMassFlow<sub>sys</sub>*

*SysCoolOutHumRat*=**å**(*W<sub>OA,zone\\ peak</sub>·DesCoolMassFlow<sub>zone</sub>*)/

* NonCoinCoolMassFlow<sub>sys</sub>*

At the end of the zone loop calculate mixed air conditions and the system sensible cooling capacity.

*FracOA=r<sub>air</sub>*· *DesOutAirVolFlow<sub>sys</sub>*/ *NonCoinCoolMassFlow<sub>sys</sub>*

*T<sub>mix</sub>* =*SysCoolOutTemp*· *FracOA* + *SysCoolRetTemp*· (1 – *FracOA*)

*W<sub>mix</sub>* = *SysCoolOutHumRat* · *FracOA* + *SysCoolRetHumRat* ·

(1 – *FracOA*)

*SysSensCoolCap*=*C<sub>p,air</sub>*· *NonCoinCoolMassFlow* ·( *T<sub>mix</sub>*-*T<sub>sup</sub>*)

Then (for noncoincident sizing) the variables calculated in section (ii) are moved into the *CalcSysSizing* Array.

(c)  Loop over all of the zones heated by this air loop.

(i)    In *FinalZoneSizing* replace the value in *DesHeatCoilInTemp* with the user specified *HeatSupTemp<sub>sys</sub>*. Do the same for *DesHeatCoilInHumRat* and *HeatSupHumRat*. This ensures that zone equipment connected to an air loop will use the system design supply air conditions as coil entering conditions.

(ii)   *NonCoinHeatMassFlow<sub>sys</sub>*=**å***DesHeatMassFlow<sub>zone</sub>*

*SysHeatRetTemp*=**å**(*ZoneRetTempAtHeatPeak·DesHeatMassFlow<sub>zone</sub>*)

/*NonCoinHeatMassFlow<sub>sys</sub>*

*SysHeatRetHumRat*=**å**(*ZoneHumRatAtHeatPeak·*

*DesHeatMassFlow<sub>zone</sub>*)/*NonCoinHeatMassFlow<sub>sys</sub>*

*SysHeatOutTemp*=**å**(*T<sub>OA,zone\\ peak</sub>·DesHeatMassFlow<sub>zone</sub>*)/

* NonCoinHeatMassFlow<sub>sys</sub>*

*SysHeatOutHumRat*=**å**(*W<sub>OA,zone\\ peak</sub>·DesHeatMassFlow<sub>zone</sub>*)/

* NonCoinHeatMassFlow<sub>sys</sub>*

At the end of the zone loop calculate mixed air conditions and the system sensible cooling capacity.

*FracOA=r<sub>air</sub>*· *DesOutAirVolFlow<sub>sys</sub>*/ *NonCoinHeatMassFlow<sub>sys</sub>*

*T<sub>mix</sub>* =*SysHeatOutTemp*· *FracOA* + *SysHeatRetTemp*· (1 – *FracOA*)

*W<sub>mix</sub>* = *SysHeatOutHumRat* · *FracOA* + *SysHeatRetHumRat* ·

(1 – *FracOA*)

*SysHeatlCap*=*C<sub>p,air</sub>*· *NonCoinHeatlMassFlow* ·( *T<sub>sup</sub>*-*T<sub>mix</sub>*)

Then (for noncoincident sizing) the variables calculated in section (ii) are moved into the *CalcSysSizing* Array.

(2)  We now have the calculated system sizing data. This data needs to be altered to take into account the user input system design flow rates (if any), or the fact that the user may have requested that the system flow rate be sized on the ventilation requirement. Note that user specified sizing ratios have already been applied to the zone sizing data which have been used in out preceding system sizing calculation. Thus the user specified sizing ratios do not have to be explicitly taken into account at the system level.

First we move the calculated system sizing data from *CalcSysSizing* array into the *FinalSysSizing* array. *FinalSysSizing* will contain the user modified system design data when we are all done.

Loop over the air loops.

(i)    As in the zone case, the user specified system design flow rates are turned into sizing ratios by dividing the user input value by the calculated value. The same strategy is employed for sizing on the ventilation requirement: the design ventilation flow rate is divided by the calculated design flow rate value. For each air loop this gives us a *SizRat<sub>cool</sub>* and *SizRat<sub>heat</sub>*.

*CoinCoolMassFlow*= *SizRat<sub>cool</sub>*· *CoinCoolMassFlow<sub>calc</sub>*

*NonCoinCoolMassFlow*= *SizRat<sub>cool</sub>*· *NonCoinCoolMassFlow<sub>calc</sub>*

*DesCoolVolFlow*= *SizRat<sub>cool</sub>*· *DesCoolVolFlow<sub>calc</sub>*

Since the flow rates have been altered the outside air fraction will change. This will alter the design mixed air conditions and lead to an altered value for the cooling capacity. This must be done for the time-step sequence and for the peak value.

(ii)   Loop over the zone timesteps (index=*i*).

*CoolFlowSeq<sub>sys</sub>(i)*= *SizRat<sub>cool</sub>*· *CoolFlowSeq<sub>sys,calc</sub>(i)*

*FracOA=r<sub>air</sub>*· *DesOutAirVolFlow<sub>sys</sub>*/ *CoolFlowSeq<sub>sys</sub>(i)*

*T<sub>mix</sub>*= *SysCoolOutTempSeq(i)·FracOA +*

*SysCoolRetTempSeq(i)·(*1-FracOA)

*SensCoolCapSeq(i)*= *C<sub>p,air</sub>*· *CoolFlowSeq<sub>sys</sub>(i)* ·( *T<sub>mix</sub>*-*T<sub>sup</sub>*)

(iii)  Do the same calculation for peak cooling.

*FracOA=r<sub>air</sub>*· *DesOutAirVolFlow<sub>sys</sub>*/ *DesCoolVolFlow*

*T<sub>mix</sub>*= *CoolOutTemp<sub>sys</sub>·FracOA + CoolRetTemp<sub>sys</sub>·(*1-FracOA)

*W<sub>mix</sub>*= *CoolOutHumRat<sub>sys</sub>·FracOA + CoolRetHumRat<sub>sys</sub>·*

*(*1-FracOA)

*SensCoolCap<sub>sys</sub>*= *C<sub>p,air</sub>*· *DesCoolVolFlow<sub>sys</sub>* ·( *T<sub>mix</sub>*-*T<sub>sup</sub>*)

*T<sub>mix</sub>* and *W<sub>mix</sub>* are saved in *FinalSysSizing* *.*

(iv) Do the same calculation for the heating case.

*CoinHeatMassFlow*= *SizRat<sub>heat</sub>*· *CoinHeatMassFlow<sub>calc</sub>*

*NonCoinHeatMassFlow*= *SizRat<sub>heat</sub>*· *NonCoinHeatMassFlow<sub>calc</sub>*

*DesHeatVolFlow*= *SizRat<sub>heat</sub>*· *DesHeatVolFlow<sub>calc</sub>*

(v)  Loop over the zone timesteps (index=*i*).

*HeatFlowSeq<sub>sys</sub>(i)*= *SizRat<sub>Heat</sub>* · *HeatFlowSeq<sub>sys,calc</sub>(i)*

*FracOA=r<sub>air</sub>*· *DesOutAirVolFlow<sub>sys</sub>*/ *HeatFlowSeq<sub>sys</sub>(i)*

*T<sub>mix</sub>*= *SysHeatOutTempSeq(i)· FracOA +*

* SysHeatRetTempSeq(i)·* *(*1-FracOA)

*HeatCapSeq(i)*= *C<sub>p,air</sub>*· *HeatFlowSeq<sub>sys</sub>(i)* ·(*T<sub>sup</sub>*-*T<sub>mix</sub>*)

(vi) Do the same calculation for peak heating.

*FracOA=r<sub>air</sub>*· *DesOutAirVolFlow<sub>sys</sub>*/ *DesHeatVolFlow*

*T<sub>mix</sub>*= *HeatOutTemp<sub>sys</sub>·FracOA + HeatRetTemp<sub>sys</sub>·* *(*1-FracOA)

*W<sub>mix</sub>*= *HeatOutHumRat<sub>sys</sub>·FracOA + HeatRetHumRat<sub>sys</sub>·*

*(*1-FracOA)

*HeatCap<sub>sys</sub>*= *C<sub>p,air</sub>*· *DesHeatVolFlow<sub>sys</sub>* ·( *T<sub>sup</sub>*-*T<sub>mix</sub>*)

*T<sub>mix</sub>* and *W<sub>mix</sub>* are saved in *FinalSysSizing* *.*

(vii)*DesMainVolFlow<sub>sys</sub>*=**MAX**(*DesCoolVolFlow<sub>sys</sub>*,*DesHeatVolFlow<sub>sys</sub>*)

**This concludes the system design calculation.**

** **

**Scalable System HVAC Sizing**

The scalable system sizing applies to system supply air flow rates and sysyem capacity in coolin and heating modes.



**Scalable System Air Flow Sizing**

The scalable sizing methods for supply air flow rate allowed are  either, *FlowPerFloorArea*, *FractionOfAutosizedCoolingAirflow*, or *FlowPerCoolingCapacity*. The scalable system air flow sizing methods are defined as follows:





*FlowPerFloorArea* means the program calculates the cooling supply air volume flow rate from zone floor area served by the airloop and user specified *Flow Per Floor Area* value.



*FractionOfAutosizedCoolingAirflow* means the program calculates the cooling supply air volume flow rate from user specified fraction and the autosized design cooling supply air volume flow rate value determined by the simulation.



*FractionOfAutosizedHeatingAirflow* means the program calculates the heating supply air volume flow rate from user specified fraction and the autosized design heating supply air volume flow rate value determined by the simulation.



*FlowPerCoolingCapacity* means the supply air volume is calculated from user specified flow per cooling capacity and design cooling capacity determined by the simulation.



*FlowPerHeatingCapacity* means the supply air volume is calculated from user specified flow per heating capacity and design heating capacity determined by the simulation.



**Scalable System Capacity Sizing**

The scalable sizing methods for system capacity available are: *CapacityPerFloorArea*, *FractionOfAutosizedCoolingCapacity* and *FractionOfAutosizedHeatingCapacity*. The scalable system capacity sizing methods are defined as follows:



*CapacityPerFloorArea* means the program calculates the design capacity from user specified capacity per floor area and floor area of the zones served by the airloop.



*FractionOfAutosizedCoolingCapacity* means the program calculates the design cooling capacity from user specified fraction and the auto-sized design cooling capacity.

** **

*FractionOfAutosizedHeatingCapacity* means the program calculates the design heating capacity from user specified fraction and the auto-sized design heating capacity.

Plant Loop Sizing
-----------------

### Introduction

The program needs to be able to autosize the fluid flow rate in each plant fluid loop. The design plant loop flow rates are set by the sum of the needs of the demanding components on each loop. For chilled water loops these components will be cooling coils. For hot water loops – hot water coils. And for condenser loops – various types of chiller that use condenser water for cooling. Each component that uses water for heating or cooling stores its design water flow rate (in its sizing routine) in the array *CompDesWaterFlow*, labeled by its inlet water supply node number. These individual component design water flow rates are then accessed, summed for each plant loop, and stored in the *PlantSizingData* array. This array also contains the user specified design values for each plant loop.

### Hot and Chilled Water Loop Sizing

#### Maximum Loop Volumetric Flow Rate

The loop maximum volumetric flow rate (m<sup>3</sup>) is just set equal to the value stored in the *PlantSizData* array for this loop.

#### Volume of the plant loop

Since the loop capacitance has a stability requirement of <div img="image2042.txt">\((\dot V\cdot \Delta tstep/V) \le 1\)</div> the volume is set so that the stability requirement will be 0.8 at the zone time step, which is the largest time step encountered at the max flow rate the loop can reach.

<div>\[Vloop = (\dot Vloop,max\cdot \Delta tstep,zone\cdot 3600)/0.8\]</div>

### Condenser Loop Sizing

#### Maximum Loop Volumetric Flow Rate

The loop maximum volumetric flow rate (m<sup>3</sup>) is just set equal to the value stored in the *PlantSizData* array for this loop.

#### Volume of the plant loop

Since the loop capacitance has a stability requirement of <div img="image2044.txt">\((\dot V\cdot \Delta tstep/V) \le 1\)</div> the volume is set so that the stability requirement will be 0.8 at the zone time step, which is the largest time step encountered at the max flow rate the loop can reach.

<div>\[Vloop = (\dot Vloop,max\cdot \Delta tstep,zone\cdot 3600)/0.8\]</div>

Coincident Plant Sizing using HVAC Sizing Simulation
----------------------------------------------------

Coincident plant sizing is an advanced sizing method that uses HVAC Sizing Simulations to determine coincident flows.  This section describes the algorithm used for sizing plant loop flow rate based on the coincidence of flow requests that actually occur when operating the system.  The purpose is to provide a more accurate value for the plant loop design flow rate.  This value is held in PlantSizData( PltSizIndex ).DesVolFlowRate.  For plant, this flow rate is the main independent variable used by component models in their sizing routines (along with the design temperature difference in Sizing:Plant). The code is contained in a PlantCoinicidentAnalysis object, one for each plant loop that is to be sized using the coincident method using HVAC Sizing Simulation.

The analysis will proceed as follows:

1. Find the maximum mass flow rate over all Sizing Periods, along with the coinciding return temperature and load.  Record which sizing period and timestep. This system node used for logging here is the plant loop supply side inlet node. 

2. Find the maximum load, and the coinciding mass flow and return temperature. Record which sizing period and timestep. For a heating or steam plant loop, the load that is logged is associated with the output variable called Plant Supply Side Heating Demand Rate.  For a cooling or condenser plant loop, the load log is as for the output variable called Plant Supply Side Cooling Demand Rate.

3. Calculate a maximum design flow rate from the maximum load, from step 2, and the temperature difference entered in the Plant:Sizing object and the specific heat (at 5°C) of the plant fluid.  

4. Compare the flow rate from step 1 to the flow rate from step 3 and take the higher.

5. Apply a sizing factor to the flow rate from Step 4, if desired.  The user can select among different options for which sizing factor use.  

6. Compare the flow rate from step 5 to the current value for plant loop flow rate and calculate a normalized change using 

<div>\[\rm{Normalized_Change} = \abs{\frac{\rm{NewFlowRate}-\rm{PreviousFlowRate}}{\rm{PreviousFlowRate}}\]</div>

7. Normalized_Change =ABS(((New flow rate-previous flow rate))/(previous flow rate))
    * Compare magnitude of Normalized_Change to a threshold, currently set at 0.005, to determine if it was significant or not.
    * If change is significant, then alter the size result for that plant loop. Set flags that sizes have changed and sizing calculations need to be called again.  Trigger special setup timesteps with flags set so that all plant system and component level sizes will be recomputed.  Not this will call and resize all of plant so that if one loop has coincident sizing and it places a load on a loop that has noncoincident sizing, the noncoincident loop might still change size because the loop it depends on changed.  Call for another Sizing Pass. 
    * If change is not significant, then leave the sizes alone and do not trigger resizing. Do not call for another Sizing Pass. 

See OutputDetailsAndExamples documentation for a description of a fairly comprehensive report sent the EIO file called “Plant Coincident Sizing Algorithm” which provides the user details for each execution of  the algorithm. There is also a predefined summary table 

The algorithm described above can have powerful impacts on the sizes of plant loops.  It is not uncommon for a hot water plant to size out at around half of what would be determined from the noncoincident sum of the sizes of all the components connected to the loop.  The maximum load aspect of the algorithm is able to increase plant flow rates above the size of the pumps, whereas the flow rate aspect of the algorithm is only able to reduce the flow rates.  It can happen that load spikes cause sizes to increase after the first Sizing Pass, and then the coincident flow rate bring the sizes back down some during subsequent Sizing Passes.  It is worthwhile to explore multiple Sizing Passes, or iterations, because sometimes the algorithm will switch between coincident flow and coincident demand from one Sizing Pass and gradually find a size that just meets conditions.  Be aware that all the controls and and EMS are also      


Component Sizing
----------------

### Introduction

In EnergyPlus each HVAC component sizes itself. Each component module contains a sizing subroutine. When a component is called for the first time in a simulation, it reads in its user specified input data and then calls the sizing subroutine. This routine checks the autosizable input fields for missing data and calculates the data when needed.

A number of high-level variables are used in the sizing subroutines.

*CurDuctType* (in *DataSizing*) contains the information about the current duct type. The types can be *main*, *cooling*, *heating* or *other*.

*CurZoneEqNum* (in *DataSizing*) is the current zone equipment set index and indicates that the component is a piece of zone equipment and should size itself using the zone sizing data arrays.

*CurSysNum* (in *DataSizing*) is the current air loop index and indicates that the component is part of the primary air system and should size itself using the system sizing data arrays.

### Fan Sizing

Fan sizing is done in subroutine *SizeFan*.

#### Max Flow Rate

If the fan is part of the central air system then check the duct type.

For duct type = *main, other* or default

*<div>\[\dot Vfan,max = DesMainVolFlowsys\]</div>*

For duct type=*cooling*

*<div img="image2047.txt">\(\dot Vfan,max = DesCoolVolFlowsys\)</div>*

For duct type=*heating*

<div>\[\dot Vfan,max = DesHeatVolFlowsys\]</div>

If the fan is zone equipment then check whether it is part of a component that only does heating.

For heating only <div img="image2049.txt">\(\dot Vfan,max = DesHeatVolFlowzone\)</div>;

Otherwise <div img="image2050.txt">\(\dot Vfan,max = Max(DesHeatVolFlowzone,DesCoolVolFlowzone)\)</div>

If the max fan flow rate is less than *SmallAirVolFlow* the max flow rate is set to zero.

### Coil:Cooling:Water

The sizing is done in subroutine *SizeWaterCoil* of module *WaterCoils*

#### Design Water Flow Rate (m<sup>3</sup>/s)

##### System Coils

The design water volumetric flow rate is calculated using:

<div>\[WaterVolFlowRat{e_{coil,des}} = \frac{{Loa{d_{coil,des}}}}{{{\rho_w} \cdot {c_{p,w}} \cdot \Delta {T_{w,des}}}}\]</div>

*DT<sub>w,des</sub>* is just the *Loop Design Temperature Difference* user input from *Sizing:Plant* (if the coil is in the outside air stream, ½ the *Loop Design Temperature Difference* is used). The design coil load *Load<sub>coil,des</sub>* is calculated from:

<div>\[Loa{d_{coil,des}} = AirMassFlowRat{e_{coil,des}} \cdot ({h_{air,coil,des,in}} - {h_{air,coil,des,out}})\]</div>

The design air mass flow rate depends on the location of the coil. If the coil is in the outside air stream the flow rate is set to *r<sub>air</sub>*·*DesOutAirVolFlow<sub>sys</sub>* (the design outside air volumetric flow for the system). If the coil is in a cooling duct the flow rate is set to *r<sub>air</sub>*·*DesCoolVolFlow<sub>sys</sub>*. If the coil is in a heating duct the flow rate is set to *r<sub>air</sub>*·*DesHeatVolFlow<sub>sys</sub>*. If the coil is in the main duct (or any other kind of duct) the flow rate is set to *r<sub>air</sub>*·*DesMainVolFlow<sub>sys</sub>*.

To obtain the inlet and outlet enthalpies, we need the inlet and outlet temperatures and humidity ratios. The inlet and outlet conditions depend on whether the coil is in the outside air stream and if it is not, whether or not there is outside air preconditioning.

1)    Coil in outside air stream

a.    *T<sub>air,in,des</sub>* = *CoolOutTemp<sub>sys</sub>* (the outside air temperature at the design cooling peak)

b.   *T<sub>air,out,des</sub>* = *PrecoolTemp<sub>sys</sub>*  (the specified Precool Design Temperature from the *Sizing:System* object).

c.    *W<sub>air,in,des</sub>* = *CoolOutHumRat<sub>sys</sub>* (the outside humidity ratio at the design cooling peak)

d.   *W<sub>air,out,des</sub>* = *PrecoolHumRat<sub>sys</sub>* (the specified Precool Design Humidity Ratio from the *Sizing:System* object)

2)    Coil in main air stream, no preconditioning of outside air

a.    *T<sub>air,in,des</sub>* = *CoolMixTemp<sub>sys</sub>* (the mixed air temperature at the design cooling peak)

b.   *W<sub>air,in,des</sub>* = *CoolMixHumRat<sub>sys</sub>* (the mixed air humidity ratio at the design cooling peak)

c.    *T<sub>air,out,des</sub>* = *CoolSupTemp<sub>sys</sub>*  (the specified Central Cooling Design Supply Air Temperature from the *Sizing:System* object)

d.   *W<sub>air,out,des</sub>* = *CoolSupHumRat<sub>sys</sub>* (the specified Central Cooling Design Supply Air Humidity Ratio from the *Sizing:System* object)

3)    Coil in main air stream, outside air preconditioned. The outside air fraction is calculated as *Frac*<sub>oa</sub> = *DesOutAirVolFlow<sub>sys</sub>* / *DesVolFlow*. *DesVolFlow* is just *AirMassFlowRate<sub>coil,des</sub>* / *r<sub>air</sub>*.

a.    *T<sub>air,in,des</sub>*=*Frac*<sub>oa</sub>*·PrecoolTemp<sub>sys</sub>* + (1.- *Frac<sub>oa</sub>*)·*CoolRetTemp<sub>sys</sub>* (see Table 41.  System Sizing Data)

b.   *W<sub>air,in,des</sub>*=*Frac<sub>oa</sub>·PrecoolHumRat<sub>sys</sub>* + (1.- *Frac<sub>oa</sub>*)·*CoolRetHumRat<sub>sys</sub>*

c.    *T<sub>air,out,des</sub>* = *CoolSupTemp<sub>sys</sub>* (the specified Central Cooling Design Supply Air Temperature from the *Sizing:System* object)

d.   *W<sub>air,out,des</sub>* = *CoolSupHumRat<sub>sys</sub>* (the specified Central Cooling Design Supply Air Humidity Ratio from the *Sizing:System* object)

With the inlet and outlet conditions established, we can obtain the inlet and outlet enthalpies:

*h<sub>air,coil,des,in</sub>* = *PsyHFnTdbW*(*T<sub>air,in,des</sub>*, *W<sub>air,in,des</sub>*)

*h<sub>air,coil,des,out</sub>* = *PsyHFnTdbW*(*T<sub>air,out,des</sub>*, *W<sub>air,out,des</sub>*)

where *PsyHFnTdbW* is the EnergyPlus function for calculating air specific enthalpy given the air temperature and humidity ratio. We now have all we need to calculate *Load<sub>coil,des</sub>* and *WaterVolFlowRate<sub>coil,des</sub>*.

##### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, the water flow rate is set equal to the terminal unit’s chilled water flow rate. Otherwise (e.g., the zone-level coil is part of *ZoneHVAC:FourPipeFanCoil, ZoneHVAC:UnitVentilator or ZoneHVAC:VentilatedSlab*) the calculation is similar to that at the system level. A design load is calculated:

<div>\[Loa{d_{coil,des}} = AirMassFlowRat{e_{coil,des}} \cdot ({h_{air,coil,des,in}} - {h_{air,coil,des,out}})\]</div>

Where:

AirMassFlowRate<sub>coil,des</sub> = DesCoolMassFlow<sub>zone</sub> (see Table 40.  Zone Sizing Data)

h<sub>air,coil,des,in</sub> = PsyHFnTdbW(T<sub>air,in,des</sub>, W<sub>air,in,des</sub>)

h<sub>air,coil,des,out</sub> = PsyHFnTdbW(T<sub>air,out,des</sub>, W<sub>air,out,des</sub>)

T<sub>air,in,des</sub> = DesCoolCoilInTemp<sub>zone</sub> (see Table 40)

W<sub>air,in,des</sub> = DesCoolCoilInHumRat<sub>zone</sub> (see Table 40)

T<sub>air,out,des</sub> =CoolDesTemp<sub>zone</sub> (user input from Zone:Sizing object)

W<sub>air,out,des</sub> = CoolDesHumRat<sub>zone</sub> (user input from Zone:Sizing object)

<div>\[WaterVolFlowRat{e_{coil,des}} = \frac{{Loa{d_{coil,des}}}}{{{\rho_w} \cdot {c_{p,w}} \cdot \Delta {T_{w,des}}}}\]</div>

where *DT<sub>w,des</sub>* is the *Loop Design Temperature Difference* user input from the *Sizing:Plant* object*.*

#### Design Air Flow Rate

##### System Coils

The design air volumetric flow rate depends on the location of the coil. If the coil is in the outside air stream the flow rate is set to *DesOutAirVolFlow<sub>sys</sub>*. If the coil is in a cooling duct the flow rate is set to *DesCoolVolFlow<sub>sys</sub>*. If the coil is in a heating duct the flow rate is set to *DesHeatVolFlow<sub>sys</sub>*. If the coil is in the main duct (or any other kind of duct) the flow rate is set to *DesMainVolFlow<sub>sys</sub>*.

##### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, the design air volumetric flow rate is set equal to the flow rate of the terminal unit. For all other zone coils it is set equal to:

Max(DesCoolMassFlow<sub>zone</sub>,DesHeatMassFlow<sub>zone</sub>) / r<sub>air</sub>

#### Design Inlet Air Temperature

##### System Coils

The inlet air temperature depends on whether the coil is in the outside air stream and if it is not, whether or not there is outside air preconditioning.

1.    Coil in outside air stream: *T<sub>air,in</sub>* = *CoolOutTemp<sub>sys</sub>* (the outside air temperature at the design cooling peak).

2.    Coil in main air stream, no preconditioning of outside air: *T<sub>air,in</sub>* = *CoolMixTemp<sub>sys</sub>* (the mixed air temperature at the design cooling peak).

3.    Coil in main air stream, outside air preconditioned. The outside air fraction is calculated as *Frac*<sub>oa</sub> = *DesOutAirVolFlow<sub>sys</sub>* / *DesVolFlow*. *DesVolFlow* is just *AirMassFlowRate<sub>coil,des</sub>* / *r<sub>air</sub>*. Then

*T<sub>air,in</sub>*=*Frac*<sub>oa</sub>*·PrecoolTemp<sub>sys</sub>* + (1.- *Frac<sub>oa</sub>*)·*CoolRetTemp<sub>sys</sub>*

##### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, the Design Inlet Air Temperature is set equal to *ZoneTempAtCoolPeak<sub>zone</sub>* (see Table 40.  Zone Sizing Data). For all other zone coils, it is set equal to *DesCoolCoilInTemp<sub>zone</sub>* (see Table 40).

#### Design Outlet Air Temperature

##### System Coils

The outlet air temperature depends on whether the coil is in the outside air stream.

1.    Coil in outside air stream: *T<sub>air,out,des</sub>* = *PrecoolTemp<sub>sys</sub>*  (the specified Precool Design Temperature from the *Sizing:System* object).

2.    Coil in main air stream: *T<sub>air,out,des</sub>* = *CoolSupTemp<sub>sys</sub>*  (the specified Central Cooling Design Supply Air Temperature from the *Sizing:System* object)

##### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, then:

<div>\[Loa{d_{coil,des}} = AirMassFlowRat{e_{coil,des}} \cdot \Delta {T_{w,des}} \cdot {c_{p,w}} \cdot {\rho_w}\]</div>

<div>\[{T_{air,out,des}} = {T_{air,in,des}} - Loa{d_{coil,des}}/({\rho_{air}} \cdot {c_{p,air}} \cdot CoolVolFlo{w_{coil,air,des}})\]</div>

where *CoolVolFlow<sub>coil,air,des</sub>* is the user input or previously autosized coil Design Air Flow Rate. For all other zone coils the Design Outlet Air Temperature is set to *CoolDesTemp<sub>zone</sub>* (see Table 40.  Zone Sizing Data).

#### Design Inlet Air Humidity Ratio

##### System Coils

The inlet air humidity ratio depends on whether the coil is in the outside air stream and if it is not, whether or not there is outside air preconditioning.

1.    Coil in outside air stream: *W<sub>air,in,des</sub>* = *CoolOutHumRat<sub>sys</sub>* (the outside humidity ratio at the design cooling peak).

2.    Coil in main air stream, no preconditioning of outside air: *W<sub>air,in,des</sub>* = *CoolMixHumRat<sub>sys</sub>* (the mixed air humidity ratio at the design cooling peak).

3.    Coil in main air stream, outside air preconditioned. The outside air fraction is calculated as *Frac*<sub>oa</sub> = *DesOutAirVolFlow<sub>sys</sub>* / *DesVolFlow*. *DesVolFlow* is just *AirMassFlowRate<sub>coil,des</sub>* / *r<sub>air</sub>*. Then

*W<sub>air,in,des</sub>*=*Frac<sub>oa</sub>·PrecoolHumRat<sub>sys</sub>* + (1.- *Frac<sub>oa</sub>*)·*CoolRetHumRat<sub>sys</sub>*

##### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction* unit, the Design Inlet Air Humidity Ratio is set equal to *ZoneHumRatAtCoolPeak<sub>zone</sub>* (see Table 40.  Zone Sizing Data). For all other zone coils, it is set equal to *DesCoolCoilInHumRat<sub>zone</sub>* (see Table 40).

#### Design Outlet Air Humidity Ratio

##### System Coils

The outlet air humidity ratio depends on whether the coil is in the outside air stream.

1.    Coil in outside air stream: *W<sub>air,out,des</sub>* = *PrecoolHumRat<sub>sys</sub>* (the specified Precool Design Humidity Ratio from the *Sizing:System* object)

2.    Coil in main air stream: *W<sub>air,out,des</sub>* = *CoolSupHumRat<sub>sys</sub>* (the specified Central Cooling Design Supply Air Humidity Ratio from the *Sizing:System* object)

##### Zone Coils

The Design Outlet Air Humidity Ratio is set equal to *CoolDesHumRat<sub>zone</sub>* (user input from *Zone:Sizing*).

#### Design Inlet Water Temperature

##### System Coils

The Design Inlet Water Temperature is set to the *Design Loop Exit Temperature* specified in the *Sizing*:*Plant* object for the water loop serving this coil.

##### Zone Coils

The Design Inlet Water Temperature is set to the *Design Loop Exit Temperature* specified in the *Sizing*:*Plant* object for the water loop serving this coil.

### Coil:Cooling:Water:DetailedGeometry Sizing

The sizing is done in subroutine *SizeWaterCoil*

#### Max Water Flow Rate of Coil

The calculation is identical to that done for *Coil:Cooling:Water*.

#### Number of Tubes per Row

<div>\[Ntube/row = {\mathop{\rm Int}\nolimits} ({\rm{13750}}\cdot \dot Vcoil,water,max)\]</div>

*N<sub>tube/row</sub>*=**Max**(*N<sub>tube/row</sub>*,3)

#### Fin Diameter

Depending on the duct type, get the coil design air flow rate.

For duct type = *main, other* or default

<div>\[\dot mair,des = \rho air\cdot DesMainVolFlowsys\]</div>

for duct type=*cooling*

<div>\[\dot mair,des = \rho air\cdot DesCoolVolFlowsys\]</div>

for duct type=*heating*

<div>\[\dot mair,des = \rho air\cdot DesHeatVolFlowsys\]</div>

<div>\[Dfin = 0.335\cdot \dot mair,des\]</div>

#### Minimum Air Flow Area

Depending on the duct type, get the coil design air flow rate.

For duct type = *main, other* or default

<div>\[\dot mair,des = \rho air\cdot DesMainVolFlowsys\]</div>

for duct type=*cooling*

<div>\[\dot mair,des = \rho air\cdot DesCoolVolFlowsys\]</div>

for duct type=*heating*

<div>\[\dot mair,des = \rho air\cdot DesHeatVolFlowsys\]</div>

<div>\[AMinAirFlow = 0.44\cdot \dot mair,des\]</div>

#### Fin Surface Area

Depending on the duct type, get the coil design air flow rate.

For duct type = *main, other* or default

<div>\[\dot mair,des = \rho air\cdot DesMainVolFlowsys\]</div>

for duct type=*cooling*

<div>\[\dot mair,des = \rho air\cdot DesCoolVolFlowsys\]</div>

for duct type=*heating*

<div>\[\dot mair,des = \rho air\cdot DesHeatVolFlowsys\]</div>

<div>\[AFinSurf = 78.5\cdot \dot mair,des\]</div>

#### Total Tube Inside Area

*A<sub>tube,total\\ inside</sub>*=4.4·*D<sub>tube,inside</sub>*·*N<sub>tube\\ rows</sub>*·*N<sub>tubes/row</sub>*

Where *D<sub>tube,inside</sub>* is the tube inside diameter.

#### Tube Outside Surf Area

*A<sub>tube,outside</sub>*=4.1·*D<sub>tube,outside</sub>*·*N<sub>tube\\ rows</sub>*·*N<sub>tubes/row</sub>*

Where *D<sub>tube,outside</sub>* is the tube outside diameter.

#### Coil Depth

*Depth<sub>coil</sub>*=*Depth<sub>tube\\ spacing</sub>*· *N<sub>tube\\ rows</sub>*

### Coil:Cooling:WaterToAirHeatPump:EquationFit Sizing

The sizing is done in subroutine *SizeHVACWaterToAir*

#### Rated Air Flow Rate

The calculation is identical to that done for *Coil:Cooling:Water*.

#### Rated Water Flow Rate

The calculation is identical to that done for *Coil:Cooling:Water*, which is the coil design load divided by the *Loop Design Temperature Difference* user input from *Sizing:Plant.* If there is a companion heating coil, the heating coil design load is used so that both modes will have the same rated water flow rate. For sizing the plant loop serving this coil, only one half of this flow rate is used since both the cooling and heating coil will save a flow rate but only one of these coils will operate at a time.

#### Rated Total Cooling Capacity

The calculation for coil operating temperatures (inlet and outlet) are identical to that done for *Coil:Cooling:Water*. The following calculations are then performed to determine the rated total cooling capacity.

<div>\[{T_{WB,ratio}} = {\raise0.7ex\hbox{${\left( {{T_{WB,air,in,des}} + 273.15\,C} \right)}$} \!\mathord{\left/ {\vphantom {{\left( {{T_{WB,air,in,des}} + 273.15\,C} \right)} {283.15\,C}}}\right.}\!\lower0.7ex\hbox{${283.15\,C}$}}\]</div>

<div>\[{T_{S,ratio}} = {\raise0.7ex\hbox{${\left( {29.44\,C + 273.15\,C} \right)}$} \!\mathord{\left/ {\vphantom {{\left( {29.44\,C + 273.15\,C} \right)} {283.15\,C}}}\right.}\!\lower0.7ex\hbox{${283.15\,C}$}}\]</div>

where:

<div img="image2072.txt">\({T_{WB,ratio}} = \)</div>ratio of load-side inlet air wet-bulb temperature in Kelvin to a reference temperature

<div img="image2073.txt">\({T_{S,ratio}} = \)</div> ratio of source-side inlet water temperature in Kelvin to a reference temperature

TCC1 = user input for Total Cooling Capacity Coefficient 1

TCC2 = user input for Total Cooling Capacity Coefficient 2

TCC3 = user input for Total Cooling Capacity Coefficient 3

TCC4 = user input for Total Cooling Capacity Coefficient 4

TCC5 = user input for Total Cooling Capacity Coefficient 5

<div>\[TotCapTempModFac = \,TCC1 + TCC2\left( {{T_{WB,ratio}}} \right) + TCC3\left( {{T_{S,ratio}}} \right) + TCC4 + TCC5\]</div>

The 4<sup>th</sup> and 5<sup>th</sup> coefficient (TCC4 and TCC5) used in the above equation are multipliers for the load-side and source-side flow ratios, respectively. For sizing, these ratios are assumed to be 1.

The enthalpy of the entering air is then compared with the enthalpy of the exiting air. The calculations for air enthalpy are identical to that done for *Coil:Cooling:Water*. If the entering air enthalpy is less than the exiting air enthalpy, a reference value of 48,000 J/kg is used as the entering air enthalpy. If the TotCapTempModFac calculation above yields 0 as the result, a value of 1 is used in the following calculation. If the design air mass flow rate is determined to be less than a very small flow value (0.001 kg/s) or the capacity calculated here is less than 0, the coil total cooling capacity is set equal to 0.

<div>\[\begin{array}{l}IF({H_{in}}\, > \,{H_{out}})THEN\\\,\,\,\,\,{\mathop Q\limits^\cdot_{coil,des,total}} = {\raise0.7ex\hbox{${{m_{air,des}}\left( {{H_{in}} - {H_{out}}} \right)}$} \!\mathord{\left/ {\vphantom {{{m_{air,des}}\left( {{H_{in}} - {H_{out}}} \right)} {TotCapTempModFac}}}\right.}\!\lower0.7ex\hbox{${TotCapTempModFac}$}}\\ELSE\\\,\,\,\,\,{\mathop Q\limits^\cdot_{coil,des,total}} = {\raise0.7ex\hbox{${{m_{air,des}}\left( {48000 - {H_{out}}} \right)}$} \!\mathord{\left/ {\vphantom {{{m_{air,des}}\left( {48000 - {H_{out}}} \right)} {TotCapTempModFac}}}\right.}\!\lower0.7ex\hbox{${TotCapTempModFac}$}}\\ENDIF\end{array}\]</div>

#### Rated Sensible Cooling Capacity

The calculation for coil operating temperatures (inlet and outlet) are identical to that done for *Coil:Cooling:Water*. The following calculations are then performed to determine the rated sensible cooling capacity.

<div>\[{T_{DB,ratio}} = {\raise0.7ex\hbox{${\left( {{T_{DB,air,in,des}} + 273.15\,C} \right)}$} \!\mathord{\left/ {\vphantom {{\left( {{T_{DB,air,in,des}} + 273.15\,C} \right)} {283.15\,C}}}\right.}\!\lower0.7ex\hbox{${283.15\,C}$}}\]</div>

<div>\[{T_{S,ratio}} = {\raise0.7ex\hbox{${\left( {29.44\,C + 273.15\,C} \right)}$} \!\mathord{\left/ {\vphantom {{\left( {29.44\,C + 273.15\,C} \right)} {283.15\,C}}}\right.}\!\lower0.7ex\hbox{${283.15\,C}$}}\]</div>

where:

<div img="image2078.txt">\({T_{DB,ratio}} = \)</div>ratio of load-side inlet air dry-bulb temperature in Kelvin to a reference temperature

SCC1 = user input for Sensible Cooling Capacity Coefficient 1

SCC2 = user input for Sensible Cooling Capacity Coefficient 2

SCC3 = user input for Sensible Cooling Capacity Coefficient 3

SCC4 = user input for Sensible Cooling Capacity Coefficient 4

SCC5 = user input for Sensible Cooling Capacity Coefficient 5

SCC6 = user input for Sensible Cooling Capacity Coefficient 6

<div>\[\begin{array}{l}SensCapTempModFac = \,SCC1 + SCC2\left( {{T_{DB,ratio}}} \right) + SCC3\left( {{T_{WB,ratio}}} \right) + SCC4\left( {{T_{S,ratio}}} \right)\\\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\,\, + SCC5 + SCC6\end{array}\]</div>

The 5<sup>th</sup> and 6<sup>th</sup> coefficient (SCC5 and SCC6) used in the above equation are multipliers for the load-side and source-side flow ratios, respectively. For sizing, these ratios are assumed to be 1.

The dry-bulb temperature of the entering air is then compared with the dry-bulb temperature of the exiting air. The calculations for air dry-bulb temperature are identical to that done for *Coil:Cooling:Water*. If the entering air dry-bulb temperature is less than the exiting air dry-bulb temperature, a reference value of 24 C is used as the entering air dry-bulb temperature. If the SensCapTempModFac calculation above yields 0 as the result, a value of 1 is used in the following calculation. If the design air mass flow rate is determined to be less than a very small flow value (0.001 kg/s) or the capacity calculated here is less than 0, the coil sensible cooling capacity is set equal to 0.

<div>\[\begin{array}{l}IF({T_{DB,in}}\, > \,{T_{DB,out}})THEN\\\,\,\,\,\,{\mathop Q\limits^\cdot_{coil,des,sensible}} = {\raise0.7ex\hbox{${{m_{air,des}}C{p_{air,des}}\left( {{T_{DB,in}} - {T_{DB,out}}} \right)}$} \!\mathord{\left/ {\vphantom {{{m_{air,des}}C{p_{air,des}}\left( {{T_{DB,in}} - {T_{DB,out}}} \right)} {SensCapTempModFac}}}\right.}\!\lower0.7ex\hbox{${SensCapTempModFac}$}}\\ELSE\\\,\,\,\,\,{\mathop Q\limits^\cdot_{coil,des,sensible}} = {\raise0.7ex\hbox{${{m_{air,des}}C{p_{air,des}}\left( {24 - {T_{DB,out}}} \right)}$} \!\mathord{\left/ {\vphantom {{{m_{air,des}}C{p_{air,des}}\left( {24 - {T_{DB,out}}} \right)} {SensCapTempModFac}}}\right.}\!\lower0.7ex\hbox{${SensCapTempModFac}$}}\\ENDIF\end{array}\]</div>

### Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit Sizing

For the cooling coil of VS WSHP, we specify a nominal speed level. During the sizing calculation, the Rated Air Volume Flow Rate, the Rated Water Volume Flow Rate and the Rated Total Cooling Capacity at the Selected Nominal Speed Level are determined in the same way as the *Coil:Cooling:WaterToAirHeatPump:EquationFit* object. The sensible heat transfer rate is not allowed for auto-sizing, instead, it is a function of the rated air and water flow rates, rated total cooling capacity and the Reference Unit SHR at the nominal speed level. The default nominal speed level is the highest speed. However, the model allows the user to select a nominal speed level rather than the highest.

#### Rated Air Flow Rate

The calculation is identical to that done for *Coil:Cooling:WaterToAirHeatPump:EquationFit*.

#### Rated Water Flow Rate

The calculation is identical to that done for *Coil:Cooling:WaterToAirHeatPump:EquationFit* , which is the coil design load divided by the *Loop Design Temperature Difference* user input from *Sizing:Plant.* If there is a companion heating coil, the heating coil design load is used so that both modes will have the same rated water flow rate. For sizing the plant loop serving this coil, only one half of this flow rate is used since both the cooling and heating coil will save a flow rate but only one of these coils will operate at a time.

#### Rated Total Cooling Capacity

The calculation for coil operating temperatures (inlet and outlet) are identical to that done for *Coil:Cooling:WaterToAirHeatPump:EquationFit*. The calculations for air enthalpy are similar to that done for *Coil:Cooling:WaterToAirHeatPump:EquationFit.* The difference is in calculating the total cooling capacity temperature modifier function at the selected nominal speed level, as below:

<div>\[TotCapTempModFra{c_{NominalSpeed}} = {\rm{a}} + {\rm{b*}}W{B_i} + c*WB_i^2 + {\rm{d*EWT}} + e*EW{T^2} + f*W{B_i}*EWT\]</div>

where

WB<sub>i</sub> = wet-bulb temperature of the air entering the heating coil, °C

EWT = entering water temperature, °C

a-f = regression curve-fit coefficients.

If the entering air enthalpy is less than the exiting air enthalpy, a reference value of 48,000 J/kg is used as the entering air enthalpy. If the *TotCapTempModFac* calculation above yields 0 as the result, a value of 1 is used in the following calculation. If the rated air mass flow rate is determined to be less than a very small flow value (0.001 kg/s) or the capacity calculated here is less than 0, the coil total cooling capacity is set equal to 0.

*If H<sub>in</sub> &gt; H<sub>out</sub> Then*

<div>\[{\dot Q_{coil,rated,total}} = {m_{air,rated}}({H_{in}} - {H_{out}})/TotCapTempModFra{c_{NominalSpeed}}\]</div>

*Else*

<div>\[{\dot Q_{coil,rated,total}} = {m_{air,rated}}(48000 - {H_{out}})/TotCapTempModFra{c_{NominalSpeed}}\]</div>

*End If*

### Coil:Heating:WaterToAirHeatPump:EquationFit Sizing

The sizing is done in subroutine *SizeHVACWaterToAir.*

#### Rated Air Flow Rate

The calculation is identical to that done for *Coil:Cooling:Water*.

#### Rated Water Flow Rate

The calculation is identical to that done for *Coil:Cooling:Water* , which is the coil design load divided by the *Loop Design Temperature Difference* user input from *Sizing:Plant.* For sizing the plant loop serving this coil, only one half of this flow rate is used since both the cooling and heating coil will save a flow rate but only one of these coils will operate at a time.

#### Rated Total Heating Capacity

The rated total heating capacity is set equal to the rated total cooling capacity.

### Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit Sizing

For the heating coil of VS WSHP, we specify a nominal speed level. During the sizing calculation, the Rated Air Volume Flow Rate and the Rated Water Volume Flow Rate are determined in the same way as the *Coil:Heating:WaterToAirHeatPump:EquationFit* object. On the other hand, the Rated Heating Capacity at the Selected Nominal Speed Level should be the same as the total cooling capacity of its corresponding cooling coil, which has to be sized first. The default nominal speed level will be the highest speed. However, the model allows the user to select a nominal speed level rather than the highest.

#### Rated Air Flow Rate

The calculation is identical to that done for Coil:Cooling:WaterToAirHeatPump:EquationFit.

#### Rated Water Flow Rate

The calculation is identical to that done for Coil:Cooling:WaterToAirHeatPump:EquationFit, which is the coil design load divided by the *Loop Design Temperature Difference* user input from *Sizing:Plant.* For sizing the plant loop serving this coil, only one half of this flow rate is used since both the cooling and heating coil will save a flow rate but only one of these coils will operate at a time.

#### Rated Total Heating Capacity

The rated total heating capacity is set equal to the rated total cooling capacity.

### Coil:Heating:Water Sizing

The sizing is done in subroutine *SizeWaterCoil*.

#### Max Water Flow Rate of Coil

##### System Coils

With the coil load from the system design data array and the user specified (in a Sizing:Plant object) design hot water temperature fall, calculate the max water flow rate:

<div>\[\dot Vcoil,water,max = HeatCapsys/(Cp,water\cdot \rho water\cdot \Delta Tplt,hw,des)\]</div>

##### Zone Coils

Using the zone design coil inlet and supply air conditions calculate the design coil load.

If the coil is not part of an induction unit then obtain the coil inlet temperature from the zone design data array;

*T<sub>in,air</sub>= DesHeatCoilInTemp<sub>zone</sub>*

If the coil is part of an induction unit take into account the induced air:

*Frac<sub>minflow</sub>*=*MinFlowFrac<sub>zone</sub>*

*T<sub>in,air</sub>*= *DesHeatCoilInTemp<sub>zone</sub>*· *Frac<sub>minflow</sub>* +

*ZoneTempAtHeatPeak<sub>­zone</sub>*·(1- *Frac<sub>minflow</sub>*)

*T<sub>out,air</sub>=HeatDesTemp<sub>zone</sub>*

W*<sub>out,air</sub>= HeatDesHumRat<sub>zone</sub>*

If the coil is part of a terminal unit the mass flow rate is determined by the volumetric flow rate of the terminal unit:

<div>\[\dot mair,des = \rho air\cdot \dot mair,des,tu\]</div>

Otherwise the design flow is obtained from the zone design data array:

<div>\[\dot mair,des = DesHeatMassFlowzone\]</div>

<div>\[Qcoil,des = cp,air\dot mair,des\cdot (Tout,air - Tin,air)\]</div>

Here *c<sub>p,air</sub>* is calculated at the outlet humidity and the average of the inlet and outlet temperatures.

With the coil load and the user specified (in a Sizing:Plant object) design hot water temperature decrease, calculate the max water flow rate:

<div>\[\dot Vcoil,water,max = Qcoil,des/(Cp,water\cdot \rho water\cdot \Delta Tplt,hw,des)\]</div>

#### UA of the Coil

To obtain the UA of the coil, we specify the model inputs (other than the UA) at design conditions and the design coil load that the coil must meet. Then we numerically invert the coil model to solve for the UA that will enable the coil to meet the design coil load given the specified inputs.

##### System Coils

The design coil load is the system design sensible cooling capacity;

*Q<sub>coil,des</sub>*= *HeatCap<sub>sys</sub>*

The required inputs for the simple coil model are:

*T<sub>in,air</sub>*= *HeatMixTemp<sub>sys</sub>*

*W<sub>in,air</sub>*= *HeatMixHumRat<sub>sys</sub>*

*T<sub>in,water</sub>*= *ExitTemp<sub>plt,hw,des</sub>*

<div>\[\dot min,water = \rho water\cdot \dot Vcoil,water,max\]</div>

Depending on the duct type, get the coil design air flow rate.

For duct type = *main, other* or default

*<div img="image2090.txt">\(\dot min,air = \rho air\cdot DesMainVolFlowsys\)</div>*

for duct type=*cooling*

*<div img="image2091.txt">\(\dot min,air = \rho air\cdot DesCoolVolFlowsys\)</div>*

for duct type=*heating*

<div>\[\dot min,air = \rho air\cdot DesHeatVolFlowsys\]</div>

We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design coil load and the coil output divided by the design coil load. The residual is calculated in the function *SimpleHeatingCoilUAResidual*.

##### Zone Coils

If the coil is not part of an induction unit then obtain the coil inlet temperature from the zone design data array;

*T<sub>in,air</sub>= DesHeatCoilInTemp<sub>zone</sub>*

If the coil is part of an induction unit take into account the induced air:

*Frac<sub>minflow</sub>*=*MinFlowFrac<sub>zone</sub>*

*T<sub>in,air</sub>*= *DesHeatCoilInTemp<sub>zone</sub>*· *Frac<sub>minflow</sub>* +

*ZoneTempAtHeatPeak<sub>­zone</sub>*·(1- *Frac<sub>minflow</sub>*)

*W<sub>in,air</sub>*= *DesHeatCoilInHumRat<sub>zone</sub>*

*T<sub>in,water</sub>*= *ExitTemp<sub>plt,hw,des</sub>*

<div>\[\dot min,water = \rho water\cdot \dot Vcoil,water,max\]</div>

*T<sub>out,air</sub>=HeatDesTemp<sub>zone</sub>*

W*<sub>out,air</sub>= HeatDesHumRat<sub>zone</sub>*

If the coil is part of a terminal unit the mass flow rate is determined by the volumetric flow rate of the terminal unit:

<div>\[\dot mair,des = \rho air\cdot \dot mair,des,tu\]</div>

Otherwise the design flow is obtained from the zone design data array:

<div>\[\dot mair,des = DesHeatMassFlowzone\]</div>

<div>\[\dot Qcoil,des = cp,air\cdot \dot mair,des\cdot (Tout,air - Tin,air)\]</div>

Here *c<sub>p,air</sub>* is calculated at the outlet humidity and the average of the inlet and outlet temperatures.

We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design coil load and the coil output divided by the design coil load. The residual is calculated in the function *SimpleHeatingCoilUAResidual*.

### Coil:Heating:Steam Sizing

The sizing is done in subroutine *SizeSteamCoil*.

#### Maximum Steam Flow Rate

##### System Coils

The maximum steam volumetric flow rate is calculated using:

<div>\[{\dot V_{coil,steam,max}}\,\,\, = \,\,\,\,\,\frac{{Loa{d_{coil,des}}}}{{{\rho_{steam}}\left( {{h_{fg}} + {c_{p,w}}\cdot \Delta {T_{sc}}} \right)}}\]</div>

The steam density (<div img="image2098.txt">\({\rho_{steam}}\)</div>) is for saturated steam at 100°C (101325.0 Pa) and *h<sub>fg</sub>* is the latent heat of vaporization of water at 100°C (101325.0 Pa). *C<sub>p,w</sub>* is the heat capacity of saturated water (condensate) at 100°C (101325.0 Pa)  and <div img="image2099.txt">\(\Delta {T_{sc}}\)</div> is the Degree of  Subcooling defined in the Coil:Heating:Steam object input. The design coil load *Load<sub>coil,des</sub>* is calculated from:

<div>\[Loa{d_{coil,des}} = {\dot m_{air,des}}({c_{p,air}})({T_{air,coil,des,out}} - {T_{air,coil,des,in}})\]</div>

The design air mass flow rate depends on the location of the coil (duct type). For duct type =  *main,* the flow rate is set to *r<sub>air</sub>*·*DesMainVolFlow<sub>sys</sub>*·*MinSysAirFlowRatio*. If the coil is in a cooling duct the flow rate is set to *r<sub>air</sub>*·*DesCoolVolFlow<sub>sys</sub>*·*MinSysAirFlowRatio*. If the coil is in a heating duct the flow rate is set to *r<sub>air</sub>*·*DesHeatVolFlow<sub>sys</sub>*. If the coil is in any other kind of duct, the flow rate is set to *r<sub>air</sub>*·*DesMainVolFlow<sub>sys</sub>*.

For sizing, the design outlet air temperature (*T<sub>air,coil,des,out</sub>*) is the Central Heating Design Supply Air Temperature specified in the Sizing:System object.

The design inlet air temperature depends on whether the coil is being sized for 100% outdoor air or minimum outdoor air flow (per 100% Outdoor Air in Heating input field in the Sizing:System object).

1)    Sizing based on 100% Outdoor Air in Heating

*T<sub>air,coil,des,in</sub>* = *HeatOutTemp<sub>sys</sub>* (the outdoor air temperature at the design heating peak)

2)    Sizing based on minimum outdoor air flow. The outdoor air fraction is calculated as *Frac*<sub>oa</sub> = *DesOutAirVolFlow<sub>sys</sub>* / *DesVolFlow*. *DesVolFlow* is <div img="image2101.txt">\({{{{\mathop m\limits^ \bullet  }_{air,des}}} \mathord{\left/ {\vphantom {{{{\mathop m\limits^ \bullet  }_{air,des}}} {{\rho_{air}}}}} \right. } {{\rho_{air}}}}\)</div>*.*

*T<sub>air,coil,des,in</sub>* =*Frac*<sub>oa</sub>*· HeatOutTemp<sub>sys</sub>* + (1.- *Frac<sub>oa</sub>*)·*HeatRetTemp<sub>sys</sub>* (see Table 41.  System Sizing Data)

##### Zone Coils

If the coil is part of an *AirTerminal:SingleDuct:\** unit (e.g., *AirTerminal:SingleDuct:ConstantVolume:Reheat, AirTerminal:SingleDuct:VAV:Reheat, AirTerminal:SingleDuct:SeriesPIU:Reheat, etc.)*, the maximum steam flow rate is set equal to the terminal unit’s maximum steam flow rate. Otherwise (e.g., the zone-level coil is part of *ZoneHVAC:PackagedTerminalAirConditioner, ZoneHVAC:UnitVentilator, ZoneHVAC:UnitHeater or ZoneHVAC:VentilatedSlab*) the calculation is similar to that at the system level. A design load is calculated:

<div>\[Loa{d_{coil,des}} = {\dot m_{air,des}}({c_{p,air}})({T_{air,coil,des,out}} - {T_{air,coil,des,in}})\]</div>

where:

<div img="image2103.txt">\({\dot m_{air,des}}\)</div>= *DesHeatMassFlow<sub>zone</sub>* (see Table 40.  Zone Sizing Data)

*T<sub>air,coil,des,in</sub>* = *DesHeatCoilInTemp<sub>zone</sub>* (see Table 40)

*T<sub>air,coil,des,out</sub>* =*HeatDesTemp<sub>zone</sub>* (user input from Sizing:Zone object)

<div img="image2104.txt">\({c_{p,air}}\)</div> = Specific heat of air (evaluated at the average of inlet and outlet air temperatures, and at the zone heating design supply air humidity ratio *HeatDesHumRat<sub>zone</sub>* [user input from Sizing:Zone object])

<div>\[{\dot V_{coil,steam,max}}\,\,\, = \,\,\,\,\,\frac{{Loa{d_{coil,des}}}}{{{\rho_{steam}}\left( {{h_{fg}} + {c_{p,w}}\cdot \Delta {T_{sc}}} \right)}}\]</div>

The terms in the denominator of this equation (*ρ<sub>steam</sub>*, *h<sub>fg</sub>*, etc.) are evaluated in the same way as described above for steam System Coils.

### Sizing of Gas and Electric Heating Coils

The sizing calculation is done in subroutine *SizeHeatingCoil* in module *HeatingCoils*.

#### Nominal Capacity of the Coil

##### System Coils

The value is obtained from the system design array.

*Cap<sub>nom</sub>*= *HeatCap<sub>sys</sub>*

##### Zone Coils

The capacity is calculated from the design coil inlet and outlet conditions.

If the coil is not part of an induction unit then obtain the coil inlet temperature from the zone design data array;

*T<sub>in,air</sub>= DesHeatCoilInTemp<sub>zone</sub>*

If the coil is part of an induction unit take into account the induced air:

*Frac<sub>minflow</sub>*=*MinFlowFrac<sub>zone</sub>*

*T<sub>in,air</sub>*= *DesHeatCoilInTemp<sub>zone</sub>*· *Frac<sub>minflow</sub>* +

*ZoneTempAtHeatPeak<sub>­zone</sub>*·(1- *Frac<sub>minflow</sub>*)

*T<sub>out,air</sub>=HeatDesTemp<sub>zone</sub>*

W*<sub>out,air</sub>= HeatDesHumRat<sub>zone</sub>*

*Q<sub>coil,des</sub>*=*C<sub>p,air</sub>*· *DesHeatMassFlow<sub>zone</sub>*·(*T<sub>out,air</sub>*-*T<sub>in,air</sub>*)

Here *c<sub>p,air</sub>* is calculated at the outlet humidity and the average of the inlet and outlet temperatures.

### DX Coil Sizing

The sizing calculations are done in subroutine *SizeDXCoil* in module *DXCoils*. This section covers the sizing of the objects

1.    Coil:Cooling:DX:SingleSpeed

2.    Coil:Heating:DX:SingleSpeed

3.    Coil:Cooling:DX:TwoSpeed

#### Rated Air Volume Flow Rate

#### System Coils

The rated air flow rate is obtained from the system design array.

<div>\[\dot Vair,rated = DesMainVolFlowsys\]</div>

#### Zone Coils

The rated air flow rate is the maximum of the heating and cooling design flow rates from the zone design array.

<div>\[\dot Vair,rated = Max(DesCoolVolFlowzone,DesHeatVolFlowzone)\]</div>

#### Rated Total Cooling Capacity

#### System Coils

The rated cooling capacity is obtained by dividing the peak cooling capacity by the *Cooling Capacity Modifier Curve* evaluated at peak mixed wetbulb and outdoor drybulb temperatures.

*T<sub>mix</sub>*= *CoolMixTemp<sub>sys</sub>*

*W<sub>mix</sub>*=*CoolMixHumRat<sub>sys</sub>*

*T<sub>sup</sub>*=*CoolSupTemp<sub>sys</sub>*

*W<sub>sup</sub>*=*CoolSupHumRat<sub>sys</sub>*

*T<sub>outside</sub>*=*CoolOutTemp<sub>sys</sub>*

*r<sub>air</sub>*=*PsyRhoAirFnPbTdbW*(*p<sub>air,std</sub>*, *T<sub>mix</sub>*,*W<sub>mix</sub>*)

*h<sub>mix</sub>*= *PsyHFnTdbW*(*T<sub>mix</sub>*,*W<sub>mix</sub>*)

*h<sub>sup</sub>*= *PsyHFnTdbW*(*T<sub>sup</sub>*,*W<sub>sup</sub>*)

*T<sub>mix,wb</sub>*= *PsyTwbFnTdbWPb*(*T<sub>mix</sub>*,*W<sub>mix</sub>*, *p<sub>air,std</sub>*)

*CapModFac*=*CurveValue*(CCapFTemp,*T<sub>mix,wb</sub>*,*T<sub>outside</sub>*)

<div>\[CCappeak = \rho air\cdot \dot Vair,rated\cdot (hmix - hsup)\]</div>

*CCap<sub>rated</sub>*=*CCap<sub>peak</sub>*/ *CapModFac*

We check that the design volume flow per total capacity is within the prescribed range:

<div>\[FlowCapRatio = \dot Vair,rated/CCaprated\]</div>

If *FlowCapRatio* &lt; *FlowCapRatio<sub>min</sub>*  then

<div>\[CCaprated = \dot Vair,rated/FlowCapRatiomin\]</div>

If *FlowCapRatio* &gt; *FlowCapRatio<sub>max</sub>*  then

<div>\[CCaprated = \dot Vair,rated/FlowCapRatiomax\]</div>

where

*FlowCapRatio<sub>min</sub>* = 0.00004027 m<sup>3</sup>/s per watt (300 cfm/ton)

And

*FlowCapRatio<sub>max</sub>*= 0.00006041 m<sup>3</sup>/s per watt (450 cfm/ton)

The sizing calculation for DX cooling coils for 100% dedicated outdor air system (DOAS) are identical to regular DX cooling coils.  However, they operate operate at different flow to capacity ratio ranges and are within the prescribed range below:

*FlowCapRatio<sub>min</sub>* = 0.00001677 m<sup>3</sup>/s per Watt (125 cfm/ton)

And

*FlowCapRatio<sub>max</sub>*= 0.00003355 m<sup>3</sup>/s per Watt (250 cfm/ton)



#### Zone Coils

The rated cooling capacity for zone coils is calculated in the same manner as for system coils.

*T<sub>mix</sub>*= *DesCoolCoilInTemp<sub>zone</sub>*

*W<sub>mix</sub>*= *DesCoolCoilInHumRat<sub>zone</sub>*

*T<sub>sup</sub>*= *CoolDesTemp<sub>zone</sub>*

*W<sub>sup</sub>*= *CoolDesHumRat<sub>zone</sub>*

*T<sub>outside</sub>*=*T<sub>outside</sub>,<sub>desday,peak</sub>*

*r<sub>air</sub>*=*PsyRhoAirFnPbTdbW*(*p<sub>air,std</sub>*, *T<sub>mix</sub>*,*W<sub>mix</sub>*)

*h<sub>mix</sub>*= *PsyHFnTdbW*(*T<sub>mix</sub>*,*W<sub>mix</sub>*)

*h<sub>sup</sub>*= *PsyHFnTdbW*(*T<sub>sup</sub>*,*W<sub>sup</sub>*)

*T<sub>mix,wb</sub>*= *PsyTwbFnTdbWPb*(*T<sub>mix</sub>*,*W<sub>mix</sub>*, *p<sub>air,std</sub>*)

*CapModFac*=*CurveValue*(CCapFTemp,*T<sub>mix,wb</sub>*,*T<sub>outside</sub>*)

<div>\[CCappeak = \rho air\cdot \dot Vair,rated\cdot (hmix - hsup)\]</div>

*CCap<sub>rated</sub>*=*CCap<sub>peak</sub>*/ *CapModFac*

We check that the design volume flow per total capacity is within the prescribed range:

<div>\[FlowCapRatio = \dot Vair,rated/CCaprated\]</div>

If *FlowCapRatio* &lt; *FlowCapRatio<sub>min</sub>*  then

<div>\[CCaprated = \dot Vair,rated/FlowCapRatiomin\]</div>

If *FlowCapRatio* &gt; *FlowCapRatio<sub>max</sub>*  then

<div>\[CCaprated = \dot Vair,rated/FlowCapRatiomax\]</div>

where

*FlowCapRatio<sub>min</sub>* = 0.00004027 m<sup>3</sup>/s per watt (300 cfm/ton)

And

*FlowCapRatio<sub>max</sub>*= 0.00006041 m<sup>3</sup>/s per watt (450 cfm/ton)

We check the design flow to the total cooling capacity rato for dedicated zone outdoor unit DX cooling coils to be within the limits prescribed below:

*FlowCapRatio<sub>min</sub>* = 0.00001677 m<sup>3</sup>/s per Watt (125 cfm/ton)

And

*FlowCapRatio<sub>max</sub>*= 0.00003355 m<sup>3</sup>/s per Watt (250 cfm/ton)

#### Rated Total Heating Capacity

For Coil:Heating:DX:SingleSpeed the rated heating capacity is set equal to the cooling capacity.

#### Rated SHR

The rated sensible heat ratio is calculated to be the sensible cooling (from rated inlet conditions to user specified supply conditions) divided by the total cooling (from rated inlet to specified supply).

*T<sub>in,rated</sub>*= 26.6667 <sup>o</sup>C (80 <sup>o</sup>F)

*W<sub>in,rated</sub>*= 0.01125 (corresponds to 80 <sup>o</sup>F drybulb, 67 <sup>o</sup>F wetbulb)

*C<sub>p,air</sub>*= *PsyCpAirFnWTdb*(*W<sub>in,rated</sub>*, *T<sub>in,rated</sub>*)

For system coils

*T<sub>sup</sub>*=*CoolSupTemp<sub>sys</sub>*

*W<sub>sup</sub>*=*CoolSupHumRat<sub>sys</sub>*

For zone coils

*T<sub>sup</sub>*= *CoolDesTemp<sub>zone</sub>*

*W<sub>sup</sub>*= *CoolDesHumRat<sub>zone</sub>*

Then

*h<sub>rated</sub>*= *PsyHFnTdbW*(*T<sub>in,rated</sub>*, *W<sub>in,rated</sub>*)

*h<sub>sup</sub>*= *PsyHFnTdbW*(*T<sub>sup</sub>*, *W<sub>sup</sub>*)

*Dh*<sub>rated,sup</sub>=*h<sub>rated</sub>*-*h<sub>sup</sub>*

*DQs<sub>rated,sup</sub>*=*C<sub>p,air</sub>*·(*T<sub>in,rated</sub>*-*T<sub>sup</sub>*)

*SHR<sub>rated</sub>*=*DQs<sub>rated,sup</sub>*/*Dh*<sub>rated,sup</sub>

#### Evaporative Condenser Air Volume Flow Rate

The evaporative condenser air volume flow rate (m<sup>3</sup>/s) is set to 0.000114 m<sup>3</sup>/s per watt (850 cfm/ton) times the total rated cooling capacity.

#### Evaporative Condenser Air Volume Flow Rate, Low Speed

The evaporative condenser air volume flow rate, low speed (m<sup>3</sup>/s) is set to 1/3 times 0.000114 m<sup>3</sup>/s per watt (850 cfm/ton) times the total rated cooling capacity.

#### Evaporative Condenser Pump Rated Power Consumption

The evaporative condenser pump rated power consumption is set equal to the total cooling capacity times 0.004266 watts pump power per watt capacity (15 W/ton).

#### Evaporative Condenser Pump Rated Power Consumption, Low Speed

The evaporative condenser pump rated power consumption, low speed, is set equal to 1/3 times the total cooling capacity times 0.004266 watts pump power per watt capacity (15 W/ton).

#### Rated Air Volume Flow Rate, low speed

The rated air volume flow rate, low speed, is set equal to 1/3 times the full rated air volume flow rate.

#### Rated Total Cooling Capacity, Low Speed

The rated total cooling capacity, low speed, is set equal to 1/3 times the full rated total cooling capacity.

#### Rated SHR, low speed

The rated sensible heat ratio, low speed, is set equal to the full speed SHR.

#### Resistive Defrost Heater Capacity

For the heat pump the resistive defrost heat capacity is set equal to the cooling capacity.

### DX MultiSpeed Coil Sizing

The sizing calculations are done in subroutine *SizeDXCoil* in module *DXCoils*. This section covers the sizing of the objects

n Coil:Heating:DX:MultiSpeed

n Coil:Cooling:DX: MultiSpeed

The rated air volume flow rate, rated total cooling capacity, rated heating capacity, rated SHR, evaporative condenser air volume flow rate, evaporative condenser pump rated power consumption at the highest speed are sized in the same ways as DX Coil Sizing.

After the sizes are determined at the highest speed, the sizes in the rest of speeds are assumed to

<div>\[Valu{e_n} = \frac{n}{{NumberOfSpeed}}*Valu{e_{NumberOfSpeed}}\]</div>

where

Value<sub>n</sub>         = Any autosizable variable at Speed n, except SHR

SHR<sub>n</sub> = SHR<sub>NumberOfSpeed</sub>

n    = Speed Index number from 1 to NumberOfSpeed-1

NumberOfSpeed     = The highest speed number

### Coil:Cooling:DX:VariableSpeed Sizing

For the variable-speed DX cooling coil, we specify a nominal speed level. During the sizing calculation, the Rated Total Cooling Capacity at the Selected Nominal Speed Level is determined in the same way as the Coil:Cooling:DX:SingleSpeed object. If the user chooses to autosize the Rated Air Volume Flow Rate, the flow rate, as compared to the Rated Total Cooling Capacity, is sized to have the same ratio as the air volume flow rate to the total cooling capacity at the nominal speed, of the Reference Unit. The sensible heat transfer rate is not allowed for auto-sizing, instead, it is a function of the rated air flow, rated total cooling capacity and the Reference Unit SHR at the nominal speed level. The default nominal speed level is the highest speed. However, the model allows the user to select a nominal speed level rather than the highest.

***Rated Total Cooling Capacity***

The calculation for coil operating temperatures (inlet and outlet) are identical to that done for Coil:Cooling:DX:SingleSpeed. The calculations for air enthalpy are similar to that done for Coil:Cooling:DX:SingleSpeed*.* The difference is in calculating the total cooling capacity temperature modifier function at the selected nominal speed level, as below:

<div>\[TotCapTempModFra{c_{NominalSpeed}} = {\rm{a}} + {\rm{b*}}W{B_i} + c*WB_i^2 + {\rm{d*}}D{B_o} + e*D{B_o}{T^2} + f*W{B_i}*D{B_o}\]</div>

where

WB<sub>i</sub> =wet-bulb temperature of the air entering thecooling coil, °C

DB<sub>o</sub> =condenser entering air temperature, °C

a-f = regression curve-fit coefficients.

If the entering air enthalpy is less than the exiting air enthalpy, a reference value of 48,000 J/kg is used as the entering air enthalpy. If the *TotCapTempModFac* calculation above yields 0 as the result, a value of 1 is used in the following calculation. If the rated air mass flow rate is determined to be less than a very small flow value (0.001 kg/s) or the capacity calculated here is less than 0, the coil total cooling capacity is set equal to 0.

*If H<sub>in</sub> &gt; H<sub>out</sub> Then*

<div>\[{\dot Q_{coil,rated,total}} = {m_{air,rated}}({H_{in}} - {H_{out}})/TotCapTempModFra{c_{NominalSpeed}}\]</div>

*Else*

<div>\[{\dot Q_{coil,rated,total}} = {m_{air,rated}}(48000 - {H_{out}})/TotCapTempModFra{c_{NominalSpeed}}\]</div>

*End If*

The other sizing procedures, e.g. evaporative condenser pump, etc., are the same as Coil:Cooling:DX:SingleSpeed.

### Coil:Heating:DX:VariableSpeed Sizing

For the variable-speed DX heating coil, we specify a nominal speed level. During the sizing calculation, the Rated Heating Capacity at the Selected Nominal Speed Level should be the same as the total cooling capacity of its corresponding cooling coil, which has to be sized first. The default nominal speed level will be the highest speed. However, the model allows the user to select a nominal speed level rather than the highest. If the user chooses to autosize the Rated Air Volume Flow Rate, the flow rate, as compared to the Rated Heating Capacity, is sized to have the same ratio as the air volume flow rate to the heating capacity at the nominal speed, of the Reference Unit. The other sizing procedures are the same as Coil:Heating:DX:SingleSpeed.

### Pump Sizing

The loop pumps’ autosizable inputs are nominal volumetric flow rate and nominal power consumption. We have

*Eff<sub>tot</sub>*=*Eff<sub>mot</sub>*·*Eff<sub>impeller</sub>*

The motor efficiency is an input. Since we need the total efficiency to calculate the nominal power consumption we assume an impeller efficiency of 0,78 for purposes of sizing.

#### Rated Volumetric Flow Rate

This is just set equal to the design loop demand obtained from summing the needs of the components on the demand side of the loop.

#### Rated Power Consumption

<div>\[\dot Qnom = Hnom\cdot \dot Vnom/Efftot\]</div>

*H<sub>nom</sub>*, the nominal head, is an input.

### Electric Chiller Sizing

Generally chillers will need nominal cooling capacity, evaporator flow rate and condenser flow rate. All 3 quantities can be straightforwardly obtained using the user specified loop sizing data and the loop design flow rates.

All chillers on a loop are sized to meet the full loop load. If there are multiple chillers on a loop that call for autosizing, they will all be assigned the same cooling capacity and evaporator flow rate.

#### Nominal Cooling Capacity

<div>\[\dot Qchiller,nom = Cp,w\cdot \rho w\cdot \Delta Tloop,des\cdot \dot Vloop,des\]</div>

where

*C<sub>p,w</sub>* is the specific heat of water at 5 <sup>o</sup>C;

*r<sub>w</sub>* is the density of water at standard conditions (5.05 <sup>o</sup>C);

*DT<sub>loop,des</sub>* is the chilled water loop design temperature rise;

<div img="image2122.txt">\(\dot Vloop,des\)</div>is the loop design volumetric flow rate.

#### Design Evaporator Volumetric Water Flow Rate

<div>\[\dot Vevap,des = \dot Vloop,des\]</div>

#### Design Condenser Volumetric Water Flow Rate

<div>\[\dot Vcond,des = \dot Qchiller,nom\cdot (1 + 1/COPchiller,nom)/(\Delta Tloop,des\cdot Cp,w\cdot \rho w)\]</div>

where

*C<sub>p,w</sub>* is the specific heat of water at design condenser inlet temperature;

*r<sub>w</sub>* is the density of water at standard conditions (5.05 <sup>o</sup>C);

*DT<sub>loop,des</sub>* is the chilled water loop design temperature rise;

*COP<sub>chiller,nom</sub>* is the chiller nominal COP.

Boiler Sizing

Generally boilers will need nominal heating capacity and rate. Both quantities can be straightforwardly obtained using the user specified loop sizing data and the loop design flow rates.

All boilers on a loop are sized to meet the full loop load. If there are multiple boilers on a loop that call for autosizing, they will all be assigned the same heating capacity and flow rate.

#### Nominal Capacity

<div>\[\dot Qboiler,nom = Cp,w\cdot \rho w\cdot \Delta Tloop,des\cdot \dot Vloop,des\]</div>

where

*C<sub>p,w</sub>* is the specific heat of water at the boiler design outlet temperature;

*r<sub>w</sub>* is the density of water at standard conditions (5.05 <sup>o</sup>C);

*DT<sub>loop,des</sub>* is the hot water loop design temperature decrease;

<div img="image2126.txt">\(\dot Vloop,des\)</div>is the loop design volumetric flow rate.

#### Design Evaporator Volumetric Water Flow Rate

<div>\[\dot Vdes = \dot Vloop,des\]</div>

### Plant Heat Exchanger Sizing

The sizing of plant heat exchanger component (object: HeatExchanger:FluidToFluid) involves determining design flow rates for both sides, a UA value, and a nominal capacity for reporting.  The component has a sizing factor for fine control and uses the design temperatures defined in the Sizing:Plant object.

The Loop Supply Side design flow rate, <div img="image2128.txt">\({\dot V_{Sup,des}}\)</div>, is set equal to the design flow rate for that loop, multiplied by the component sizing factor, <div img="image2129.txt">\({f_{comp}}\)</div>.

<div>\[{\dot V_{Sup,des}} = {\dot V_{loop,des}}*{f_{comp}}\]</div>

The Loop Demand Side design flow rate,<div img="image2131.txt">\({\dot V_{Dmd,des}}\)</div> , is set equal to the Loop Supply Side design flow rate.

<div>\[{\dot V_{Dmd,des}} = {\dot V_{Sup,des}}\]</div>

The design heat transfer capacity and UA for the heat exchanger are calculated using the design temperatures for the two plant loops.  The loop design temperature difference for the Loop Supply Side, <div img="image2133.txt">\(\Delta {T_{SupLoop,Des}}\)</div>, is used to determine a nominal capacity.

<div>\[\dot Q = {\mathop V\limits^._{Sup,des}}\rho {c_p}\Delta {T_{SupLoop,Des}}\]</div>

A loop-to-loop design temperature difference, <div img="image2135.txt">\(\Delta {T_{LoopToLoop,Des}}\)</div>, is determined depending on the nature of the plant loop connected to the Loop Supply Side.  The Sizing:Plant object includes  classifications for the type of loop that include Heating, Steam, Cooling, or Condenser. For Cooling and Condenser loop types, the loop design temperature difference is added to the design exit temperature for the Loop Supply Side, <div img="image2136.txt">\({T_{SupLoop,Exit}}\)</div>.  For Heating and Stem loop types, the loop design temperature difference is subtracted from the design exit temperature.  This adjusted supply side temperature is then compared to the design exit temperature for the Loop Demand Side,<div img="image2137.txt">\({T_{DmdLoop,Exit}}\)</div> .

<div img="image2138.txt">\(\Delta {T_{LoopToLoop,Des}} = \left( {{T_{SupLoop,Exit}} + \Delta {T_{SupLoop,Des}}} \right) - {T_{DmdLoop,Exit}}\)</div>    (Cooling, Condenser)

<div img="image2139.txt">\(\Delta {T_{LoopToLoop,Des}} = \left( {{T_{SupLoop,Exit}} - \Delta {T_{SupLoop,Des}}} \right) - {T_{DmdLoop,Exit}}\)</div>    (Heating, Steam)

<div>\[\Delta {T_{LoopToLoop,Des}} = MAX\left( {ABS\left( {\Delta {T_{LoopToLoop,Des}}} \right),2.0} \right)\]</div>

The UA (U-Factor Time Area Value) is determined by assuming that the target capacity can be delivered for the loop-to-loop temperature difference which after substituting and rearranging becomes:

<div>\[UA = \frac{{{{\mathop {V}\limits }_{Sup,des}}\rho {c_p}\Delta {T_{SupLoop,Des}}}}{{\Delta {T_{LoopToLoop,Des}}}}\]</div>

A nominal capacity for the heat exchanger is determined from the design flow rates and UA (regardless of if they were automatically sized or input by the user) and the expected operating temperatures of the two loops.  The loop operating temperatures are obtained from the input in Sizing:Plant object if it is present for that loop.  If no Sizing:Plant is present then the loop’s overall setpoint is used (if the loop’s load scheme is DualSetpointDeadband then the average of the high and low setpoints is used).  The full heat exchanger model is then calculated for the maximum loop flow rates and expected loop temperatures as inlets to the heat exchanger.  The absolute value for the model result for heat transfer rate is then used as the capacity of the heat exchanger.  This capacity is reported and may be used for controls based on operation scheme.

### Humidifier Sizing

The rated power, or nominal electric power input of an Electric Steam Humidifier (Humidifier:Steam:Electric) is calculated from user specified rated capacity (m<sup>3</sup>/s) and the enthalpy change of the water from a reference temperature (20.0°C) to saturated steam at 100.0°C. Autosizing procedure assumes that electrical heating element in the humidifier heat the water from the reference temperature and generate saturated steam at 100°C, and electric to thermal energy conversion efficiency of 100.0%.

#### Rated Power

<div>\[{P_{rated}} = {\dot V_{rated}} \cdot {\rho_w} \cdot \left( {{h_{fg}} + {C_{p,w}} \cdot \Delta {T_w}} \right)\]</div>

where

*C<sub>p,w</sub> is the specific heat of water at average temperature ((100+20)/2 = 60.0* *°C), (J/kgK);*

*r<sub>w</sub> is the density of water at standard conditions (5.05* *°C);*

*DT<sub>w</sub>  is the sensible temperature rise of water (100.0 – 20.0=80.0* *°C);*

*<div img="image2143.txt">\({\dot V_{rated}}\)</div> is the rated capacity of the humidifier in volumetric flow rate.*

*h<sub>fg</sub> is the latent heat of vaporization of water at 100.0°C, (J/kg);*

#### Rated Capacity

<div>\[{\dot m_w} = {\dot m_a}\left( {{\omega_o} - {\omega_i}} \right)\]</div>

where

<div img="image2145.txt">\({\dot m_w}\)</div>* iswater mass flow rate, kg/s;*

<div img="image2146.txt">\({\dot m_a}\)</div>* is design air mass flow rate, kg/s;*

*ω<sub>o</sub> is design outlet humidity ratio, kg-water/kg-air; *

*ω<sub>i</sub> is design inlet humidity ratio, kg-water/kg-air.*



The air mass flow rate and humidity ratios are determined based upon zone design conditions. If the unit is part of zone equipment, then:



<div>\[{\dot m_a} = Max\left( {DesCoolVolFlo{w_{zone}},DesHeatVolFlo{w_{zone}}} \right) \cdot {\rho_a}\]</div>

<div>\[{\omega_i} = Min\left( {OutHumRatAtCoolPea{k_{zone}},OutHumRatAtHeatPea{k_{zone}}} \right)\]</div>

      <div img="image2149.txt">\({\omega_o} = Max\left( {ZoneHumRatAtCoolPea{k_{zone}},ZoneHumRatAtHeatPea{k_{zone}}} \right)\)</div>



where

*r<sub>a</sub> is the density of air at design conditions, kg/s.*

* *

If the unit is part of the central air system, then check if outdoor air system is present. If outdoor air system is part of the air loop and design outdoor air flow rate is greater than zero, then:

<div>\[{\dot m_a} = DesOutAirVolFlo{w_{sys}} \cdot {\rho_a}\]</div>

<div>\[{\omega_i} = Min\left( {CoolOutHumRa{t_{sys}},HeatOutHumRa{t_{sys}}} \right)\]</div>

      <div img="image2152.txt">\({\omega_o} = Max\left( {CoolSupHumRa{t_{sys}},HeatSupHumRa{t_{sys}}} \right)\)</div>

Otherwise, air mass flow rate is determined as follows:

for duct type = *main*

<div>\[{\dot m_a} = DesMainAirVolFlo{w_{sys}} \cdot {\rho_a}\]</div>

for duct type = *cooling*

<div>\[{\dot m_a} = DesCoolVolFlo{w_{sys}} \cdot {\rho_a}\]</div>

for duct type = *heating*

<div>\[{\dot m_a} = DesHeatVolFlo{w_{sys}} \cdot {\rho_a}\]</div>

for duct type = *other*

<div img="image2156.txt">\({\dot m_a} = DesMainVolFlo{w_{sys}} \cdot {\rho_a}\)</div>,

and the humidity ratios are:

<div>\[{\omega_i} = Min\left( {CoolMixHumRa{t_{sys}},HeatMixHumRa{t_{sys}}} \right)\]</div>

      <div img="image2158.txt">\({\omega_o} = Max\left( {CoolSupHumRa{t_{sys}},HeatSupHumRa{t_{sys}}} \right)\)</div>

### Cooling Tower Sizing

The quantities needed to autosize a cooling tower include the design water flow rate, the nominal fan power and air flow rate, and the tower UA. This data may be need to be given at more than one operating point:, for instance – high speed fan, low speed fan and free convection.

EnergyPlus provides two input choices: the user can input the design water flow rate and tower UA at each operating point or the tower nominal capacity (and let the program calculate the water flow rate and UA). Choice of input method will affect the sizing calculations in ways noted below.

#### Design Water Flow Rate

If *Tower Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* then

<div>\[\dot Vtower,w,des = \dot Vloop,des\]</div>

If *Tower Performance Input Method* = *NominalCapacity* then

<div>\[\dot Vtower,w,des = 5.382E - 8\cdot \dot Qtower,nom\]</div>

where 5.382·10<sup>-08</sup> is m<sup>3</sup>/s per watt corresponds to the rule-of-thumb of sizing the tower flow rate at 3 gallons per minute per ton. For the CoolingTower:VariableSpeed:Merkel model with NominalCapacity input method, the user can input the value used to scale design water flow rate from nominal capacity and the default is 5.382·10<sup>-08</sup> m<sup>3</sup>/s/W.

#### Fan Power at Design Air Flow Rate

The nominal fan power is sized to be 0.0105 times the design load.

If *Tower Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* then

<div>\[\dot Qtower,nom = Cp,w\cdot \rho w\cdot \dot Vtower,w,des\cdot \Delta Tloop,des\]</div>

where

*C<sub>p,w</sub>* is the specific heat of water at the condenser loop design exit temperature;

*r<sub>w</sub>* is the density of water at standard conditions (5.05 <sup>o</sup>C);

*DT<sub>loop,des</sub>* is the condenser water loop design temperature rise;

Finally

<div>\[\dot Qfan,nom = 0.0105\cdot \dot Qtower,nom\]</div>

For the CoolingTower:VariableSpeed:Merkel model, the design fan power is determined using a scaling factor, in units of Watts per Watt, that can be input by the user.  The default value is 0.0105 which is the same as above.

#### Design Air Flow Rate

We assume a fan efficiency of 0.5 and a fan pressure rise of 190 Pascals. Then

<div>\[\dot Vtower,air,des = \dot Qfan,nom\cdot 0.5\cdot \rho air/190\]</div>

where

r<sub>air</sub> is the density of air at standard conditions.

For the CoolingTower:VariableSpeed:Merkel model, the design air flow rate is determined from the nominal capacity using a scaling factor, <div img="image2164.txt">\({f_{airflow/W}}\)</div>,in units of m<sup>3</sup>/s/W.  The default value is 2.76316\*10<sup>-5</sup>.  When the input field is left blank, the default is used as follows

<div>\[{\dot V_{tower,air,des}} = {\dot Q_{tower,nom}} \bullet {f_{airflow/W}} \bullet \frac{{101325}}{{{P_{std,altitude}}}}\]</div>

where, <div img="image2166.txt">\({P_{std,altitude}}\)</div> is the standard barometric pressure for the location’s elevation.

When the input field is filled with a hard value, the pressure scaling is not used

<div>\[{\dot V_{tower,air,des}} = {\dot Q_{tower,nom}} \bullet {f_{airflow/W}}\]</div>

#### Tower UA Value at Design Air Flow Rate

To obtain the UA of the tower, we specify the model inputs (other than the UA) at design conditions and the design tower load that the tower must meet. Then we numerically invert the tower model to solve for the UA that will enable the tower to meet the design tower load given the specified inputs.

The design tower load is:

for *Tower Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate*

<div>\[\dot Qtower,des = Cp,w\cdot \rho w\cdot \dot Vtower,w,des\cdot \Delta Tloop,des\]</div>

for *Tower Performance Input Method* = *NominalCapacity*

<div img="image2169.txt">\(\dot Qtower,des = 1.25\cdot \dot Qtower,nom\)</div>(to allow for compressor heat)

Where, <div img="image2170.txt">\({f_{des,heat,ratio}}\)</div> is the ratio of actual heat rejection capacity to nominal capacity.  This ratio is available as a user input with a default value of 1.25 (to allow for compressor heat).

Then we assign the inputs needed for the model.

*T<sub>in,air</sub>*=35 <sup>o</sup>C (95 <sup>o</sup>F design air inlet temperature)

*T<sub>in,air,wb</sub>*=25.6 <sup>o</sup>C (78 <sup>o</sup>F design air inlet wetbulb temperature)

*W<sub>in</sub>* is calculated from the entering air drybulb and wetbulb.

The  inlet water mass flow rate is just the design volumetric flow rate times the density of water.

The inlet water temperature is set slightly differently for the 2 input methods. For

*UFactorTimesAreaAndDesignWaterFlowRate*

*T<sub>in,water</sub>*=*T<sub>loop,exit,des</sub>*+*DT<sub>loop,des</sub>*

*NominalCapacity*

*T<sub>in,water</sub>*=35 <sup>o</sup>C (95 <sup>o</sup>F design inlet water temperature).

We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design tower load and the tower output divided by the design tower load. The residual is calculated in the function *SimpleTowerUAResidual.*

#### Air Flow Rate at Low Fan Speed

The nominal air flow rate at low fan speed is set to a fraction of the full speed air flow rate. The fraction is available for user input in the field called Low Fan Speed Air Flow Rate Sizing Factor. The default is 0.5.

#### Fan Power at Low Fan Speed

The fan power at low fan speed is set to a fraction of the fan power at full speed.  The fraction is available for user input in the field called Low Fan Speed Fan Power Sizing Factor. The default is 0.16.

#### Tower UA Value at Low Fan Speed

For *Tower Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* the low speed UA is set to a fraction of the full speed UA.  The fraction is available for user input in the field called Low Fan Speed U-Factor Times Area Sizing Factor. The default is 0.6. For *Tower Performance Input Method* = *NominalCapacity* the low speed UA is calculated in the same manner as the full speed UA using <div img="image2171.txt">\(\dot Qtower,nom,lowspeed\)</div> instead of <div img="image2172.txt">\(\dot Qtower,nom\)</div>.

#### Air Flow Rate in Free Convection Regime

The free convection air flow rate is set to a fraction of the full air flow rate. The fraction is available for user input in the field called Free Convection Regime Air Flow Rate Sizing Factor. The default is 0.1.

#### Tower UA Value in Free Convection Regime

For *Tower Performance Input Method* = *UA and Design Water Flow Rate* the low speed UA is set to a fraction of the full speed UA. The fraction is available for user input in the field called Free Convection U-Factor Times Area Value Sizing Factor. The default is 0.1. For *Tower Performance Input Method* = *NominalCapacity* the low speed UA is calculated in the same manner as the full speed UA using <div img="image2173.txt">\(\dot Qtower,nom,freeconv\)</div> instead of <div img="image2174.txt">\(\dot Qtower,nom\)</div>.

### Fluid Cooler Sizing

The quantities needed to autosize a fluid cooler include the design water flow rate, the nominal fan power, air flow rate, and the fluid cooler UA. This data may need to be given at more than one operating point:, for instance – high speed fan and low speed fan.

EnergyPlus provides two input choices: the user can input the design water flow rate and fluid cooler UA at each operating point or the fluid cooler nominal capacity and the water flow rate (and let the program calculate UA). Choice of input method will affect the sizing calculations in ways noted below.

#### Design Water Flow Rate

The design water flow rate is sized as follows

<div>\[{\dot V_{fluidcooler,w,des}} = {\dot V_{loop,des}}\]</div>

#### Fan Power at Design Air Flow Rate

The nominal fan power is sized to be 0.0105 times the design load.

If *Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* then

<div>\[{\dot Q_{fluidcooler,nom}} = {C_{p,w}} \bullet {\rho_w} \bullet {\dot V_{fluidcooler,w,des}} \bullet \Delta {T_{loop,des}}\]</div>

  where

*  C<sub>p,w</sub>* is the specific heat of water at the condenser loop design exit temperature;

* * *r<sub>w</sub>* is the density of water at standard conditions (5.05 <sup>o</sup>C);

* * *DT<sub>loop,des</sub>* is the condenser water loop design temperature rise;

  Finally

<div>\[{\dot Q_{fan,nom}} = 0.0105 \bullet {\dot Q_{fluidcooler,nom}}\]</div>

#### ElseifPerformance Input Method= NominalCapacitythen

      <div img="image2178.txt">\({\dot Q_{fan,nom}} = 0.0105 \bullet {\dot Q_{fluidcooler,nom}}\)</div>

  Where

      <div img="image2179.txt">\({\dot Q_{fluidcooler,nom}}\)</div> is provided by the user.

#### Design Air Flow Rate

n For Performance Input Method = UFactorTimesAreaAndDesignWaterFlowRate

  <div img="image2180.txt">\({\dot Q_{fluidcooler,nom}} = {C_{p,w}} \bullet {\rho_w} \bullet {\dot V_{fluidcooler,w,des}} \bullet \Delta {T_{loop,des}}\)</div>

n For Performance Input Method = NominalCapacity

  <div img="image2181.txt">\({\dot Q_{fluidcooler,nom}}\)</div> is provided by the user.

  <div img="image2182.txt">\({\dot V_{fluidcooler,air,des}} = {\dot Q_{fluidcooler,nom}}/({T_{in,water}} - {T_{in,air}})*4\)</div>

Where,

*T<sub>in,water  </sub>* = Design entering water temperature provided by the user

*T<sub>in,air       </sub>* = Design air inlet temperature provided by the user

#### Fluid cooler UA Value at Design Air Flow Rate

To obtain the UA of the fluid cooler, we specify the model inputs (other than the UA) at design conditions and the design fluid cooler load that the fluid cooler must meet. Then we numerically invert the fluid cooler model to solve for the UA that will enable the fluid cooler to meet the design fluid cooler load given the specified inputs.

The design fluid cooler load is:

n For Performance Input Method = UFactorTimesAreaAndDesignWaterFlowRate

  <div img="image2183.txt">\({\dot Q_{fluidcooler,nom}} = {C_{p,w}} \bullet {\rho_w} \bullet {\dot V_{fluidcooler,w,des}} \bullet \Delta {T_{loop,des}}\)</div>

n For Performance Input Method = NominalCapacity

  <div img="image2184.txt">\({\dot Q_{fluidcooler,nom}}\)</div> is provided by the user.

Then we assign the inputs needed for the model.

*T<sub>in,air      </sub>* = Design air inlet temperature provided by the user

*T<sub>in,air,wb</sub>* = Design air inlet wetbulb temperature provided by the user

*W<sub>in</sub>* is calculated from the entering air drybulb and wetbulb.

The  inlet water mass flow rate is just the design entering volumetric flow rate times the density of water.

The inlet water temperature is set slightly differently for the 2 input methods. For

n UFactorTimesAreaAndDesignWaterFlowRate

<div>\[{T_{in,water}} = {T_{loop,exit,des}} + \Delta {T_{loop,des}}\]</div>

n NominalCapacity

<div>\[{T_{in,water}} = Provided\,by\,the\,user\]</div>

We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design fluid cooler load and the fluid cooler output divided by the design fluid cooler load. The residual is calculated in the function *SimpleFluidCoolerUAResidual.*

#### Air Flow Rate at Low Fan Speed

The nominal air flow rate at low fan speed is set to a fraction of the full speed air flow rate. The fraction is available for user input in the field called Low Fan Speed Air Flow Rate Sizing Factor. The default is 0.5.

#### Fan Power at Low Fan Speed

The fan power at low fan speed is set to a fraction of the fan power at full speed. The fraction is available for user input in the field called Low Fan Speed Fan Power Sizing Factor. The default is 0.16.

#### Fluid cooler UA Value at Low Fan Speed

For *Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* the low speed UA is set to a fraction of the full speed UA. .  The fraction is available for user input in the field called Low Fan Speed U-Factor Times Area Sizing Factor. The default is 0.6. For *Performance Input Method* = *NominalCapacity* the low speed UA is calculated in the same manner as the full speed UA using <div img="image2187.txt">\({\dot Q_{fluidcooler,nom,lowspeed}}\)</div> instead of <div img="image2188.txt">\({\dot Q_{fluidcooler,nom}}\)</div>.

### Evaporative Fluid cooler Sizing

The quantities needed to autosize an evaporative fluid cooler include the design water flow rate, the nominal fan power, air flow rate, and the fluid cooler UA. This data may need to be given at more than one operating point:, for instance – high speed fan and low speed fan.

EnergyPlus provides three input choices: the user can input the design water flow rate and fluid cooler UA at each operating point (*UFactorTimesAreaAndDesignWaterFlowRate*) or the fluid cooler design capacity and the water flow rate and let the program calculate UA (*UserSpecifiedDesignCapacity*) or only the fluid cooler design capacity and let the program calculate UA and the water flow rate (*StandardDesignCapacity*). Choice of input method will affect the sizing calculations in ways noted below.

#### Design Water Flow Rate

If *Performance Input Method* = *StandardDesignCapacity* then

<div>\[{\dot V_{fluidcooler,w,des}} = 5.382E - 8\cdot {\dot Q_{fluidcooler,standarddesign}}\]</div>

Else

<div>\[{\dot V_{fluidcooler,w,des}} = {\dot V_{loop,des}}\]</div>

where 5.382·10<sup>-08</sup> is m<sup>3</sup>/s per watt corresponds to the rule-of-thumb of sizing the fluid cooler flow rate at 3 gallons per minute per ton.

#### Fan Power at Design Air Flow Rate

The design fan power is sized to be 0.0105 times the design load.

If *Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* then

<div>\[{\dot Q_{fluidcooler,design}} = {C_{p,w}} \bullet {\rho_w} \bullet {\dot V_{fluidcooler,w,des}} \bullet \Delta {T_{loop,des}}\]</div>

where

*C<sub>p,w</sub>* is the specific heat of water at the condenser loop design exit temperature;

*r<sub>w</sub>* is the density of water at standard conditions (5.05 <sup>o</sup>C);

*DT<sub>loop,des</sub>* is the condenser water loop design temperature rise;

Finally

<div>\[{\dot Q_{fan,design}} = 0.0105 \bullet {\dot Q_{fluidcooler,design}}\]</div>

#### Else

<div>\[{\dot Q_{fan,design}} = 0.0105 \bullet {\dot Q_{fluidcooler,design}}\]</div>

Where

<div img="image2194.txt">\({\dot Q_{fluidcooler,design}}\)</div>is the design capacity provided by the user for the other two performance input methods

#### Design Air Flow Rate

We assume a fan efficiency of 0.5 and a fan pressure rise of 190 Pascals. Then

<div>\[{\dot V_{fluidcooler,air,des}} = {\dot Q_{fan,design}} \bullet 0.5 \bullet {\rho_{air}}/190\]</div>

where

r<sub>air</sub> is the density of air at standard conditions.

#### Fluid cooler UA Value at Design Air Flow Rate

To obtain the UA of the evaporative fluid cooler, we specify the model inputs (other than the UA) at design conditions and the design fluid cooler load that the fluid cooler must meet. Then we numerically invert the fluid cooler model to solve for the UA that will enable the fluid cooler to meet the design fluid cooler load given the specified inputs.

The design fluid cooler load is:

n For Performance Input Method = UFactorTimesAreaAndDesignWaterFlowRate

<div>\[{\dot Q_{fluidcooler,design}} = {C_{p,w}} \bullet {\rho_w} \bullet {\dot V_{fluidcooler,w,des}} \bullet \Delta {T_{loop,des}}\]</div>



n For Performance Input Method = StandardDesignCapacity

<div img="image2197.txt">\({\dot Q_{fluidcooler,design}} = 1.25 \bullet {\dot Q_{fluidcooler,standarddesign}}\)</div> (to allow for compressor heat)

Then we assign the inputs needed for the model.

*T<sub>in,air      </sub>* = 35 <sup>o</sup>C (95 <sup>o</sup>F design air inlet temperature)

*T<sub>in,air,wb</sub>* = 25.6 <sup>o</sup>C (78 <sup>o</sup>F design air inlet wetbulb temperature)

*W<sub>in</sub>* is calculated from the entering air drybulb and wetbulb.



n For Performance Input Method = UserSpecifiedDesignCapacity

<div>\[{\dot Q_{fluidcooler,design}} = {\dot Q_{fluidcooler,userspecifieddesign}}\]</div>

Where, <div img="image2199.txt">\({f_{des,heat,ratio}}\)</div> is the ratio of actual heat rejection capacity to nominal capacity.  This ratio is available as a user input with a default value of 1.25 (to allow for compressor heat)

Then we assign the inputs needed for the model.

*T<sub>in,air      </sub>* = Design air inlet temperature provided by the user

*T<sub>in,air,wb</sub>* = Design air inlet wetbulb temperature provided by the user

*W<sub>in</sub>* is calculated from the entering air drybulb and wetbulb.



The inlet water mass flow rate is just the design entering volumetric flow rate times the density of water.

The inlet water temperature is set slightly differently for the 3 input methods. For

n UFactorTimesAreaAndDesignWaterFlowRate

<div>\[{T_{in,water}} = {T_{loop,exit,des}} + \Delta {T_{loop,des}}\]</div>

n StandardDesignCapacity

<div>\[{T_{in,water}} = 35^\circ C\;(95^\circ F\,design\,inlet\,water\,temperature)\]</div>

n UserSpecifiedDesignCapacity

<div>\[{T_{in,water}} = Provided\,by\,the\,user\]</div>



We now have all the data needed to obtain UA. The numerical inversion is carried out by calling subroutine *SolveRegulaFalsi*. This is a general utility routine for finding the zero of a function. In this case it finds the UA that will zero the residual function – the difference between the design fluid cooler load and the fluid cooler output divided by the design fluid cooler load. The residual is calculated in the function *SimpleEvapFluidCoolerUAResidual.*

#### Air Flow Rate at Low Fan Speed

The design air flow rate at low fan speed is set to a fraction of the full speed air flow rate. The fraction is available for user input in the field called Low Fan Speed Air Flow Rate Sizing Factor. The default is 0.5.

#### Fan Power at Low Fan Speed

The fan power at low fan speed is set to a fraction of the fan power at full speed. The fraction is available for user input in the field called Low Fan Speed Fan Power Sizing Factor. The default is 0.16.

#### Fluid cooler UA Value at Low Fan Speed

For *Performance Input Method* = *UFactorTimesAreaAndDesignWaterFlowRate* the low speed UA is set to a fraction of the full speed UA. The fraction is available for user input in the field called Low Fan Speed U-Factor Times Area Sizing Factor. The default is 0.6. For *Performance Input Method* = *StandardDesignCapacity* (and similarly for *UserSpecifiedDesignCapacity method*) the low speed UA is calculated in the same manner as the full speed UA using <div img="image2203.txt">\({\dot Q_{fluidcooler,standarddesign,lowspeed}}\)</div> instead of <div img="image2204.txt">\({\dot Q_{fluidcooler,standarddesign}}\)</div>.

### Fan Coil Unit Sizing

Fan Coil units are compound components: each unit contains a fan, hot water coil, chilled water coil and outside air mixer. The inputs that may need to be autosized are the nominal unit air flow rate, the maximum hot and chilled water flow rates, and the design outside air flow rate. The data needed for sizing the units is obtained from the zone design arrays and the user specified plant sizing input.

#### Maximum Air Flow Rate

<div>\[\dot Vair,max = Max(DesCoolVolFlowzone,DesHeatVolFlowzone)\]</div>

#### Maximum Outside Air Flow Rate

<div>\[\dot Voutsideair,max = Min(MinOAzone,\dot Vair,max)\]</div>

#### Maximum Hot Water Flow

*T<sub>coil,in</sub>*=*DesHeatCoilInTemp<sub>zone</sub>*

*T<sub>coil,out</sub>*=*HeatDesTemp<sub>zone</sub>*

<sub><div img="image2207.txt">\(\dot Qcoil,des = cp,air\cdot DesHeatMassFlowzone\cdot (Tout,coil - Tin,coil)\)</div></sub>

<div>\[\dot Vmax,hw = \dot Qcoil,des/(cp,w\cdot \rho w\cdot \Delta Tloop,des)\]</div>

where

*c<sub>p,air</sub>* is evaluated at the average of the inlet & outlet temperatures and the coil outlet humidity ratio.

#### Maximum Cold Water Flow

*T<sub>coil,in</sub>*=*DesColdCoilInTemp<sub>zone</sub>*

*T<sub>coil,out</sub>*=*ColdDesTemp<sub>zone</sub>*

*W<sub>coil,in</sub>*= *DesCoolCoilInHumRat<sub>zone</sub>*

*W<sub>coil,out</sub>*= *CoolDesHumRat<sub>zone</sub>*

*H<sub>coil,in</sub>*= *PsyHFnTdbW*(*T<sub>coil,in</sub>*, *W<sub>coil,in</sub>*)

*H<sub>coil,out</sub>*= *PsyHFnTdbW*(*T<sub>coil,out</sub>*, *W<sub>coil,out</sub>*)

<sub><div img="image2209.txt">\(\dot Qcoil,des = DesCoolMassFlowzone\cdot (hin,coil - hout,coil)\)</div></sub>

<div>\[\dot Vmax,hw = \dot Qcoil,des/(cp,w\cdot \rho w\cdot \Delta Tloop,des)\]</div>

where

*c<sub>p,air</sub>* is evaluated at the average of the inlet & outlet temperatures and the coil outlet humidity ratio.

### Window Air Conditioner Sizing

Window air conditioners are compound components: each unit contains a fan, a DX coil and an outside air mixer. The inputs that may need to be autosized are the nominal unit air flow rate and the design outside air flow rate. The data needed for sizing the units is obtained from the zone design arrays.

#### Maximum Air Flow Rate

<div>\[{\dot V_{air,max}} = DesCoolVolFlo{w_{max}}\]</div>

#### Maximum Outside Air Flow Rate

<div>\[\dot Voutsideair,max = Min(MinOAzone,\dot Vair,max)\]</div>

### Unit Ventilator Sizing

Unit ventilators are compound components: each unit contains an outdoor air mixer, a fan, a heating coil, and a cooling coil. The inputs that may need to be autosized are the maximum supply air flow rate and the maximum/minimum outside air flow rates. The data needed for sizing the units is obtained from the zone design arrays.

#### Maximum Air Flow Rate

If both the cooling and heating coil are present, then:

<div>\[{\dot V_{supplyair,max}} = MAX\left( {DesCoolVolFlo{w_{zone}},DesHeatVolFlo{w_{zone}}} \right)\]</div>

If only the heating coil is present, then:

<div>\[{\dot V_{supplyair,max}} = DesHeatVolFlo{w_{zone}}\]</div>

If only cooling coil is present, then:

<div>\[{\dot V_{supplyair,max}} = DesCoolVolFlo{w_{zone}}\]</div>

If neither heating nor cooling coil is present, then:

<div>\[{\dot V_{supplyair,max}} = MinO{A_{zone}}\]</div>

#### Maximum Outside Air Flow Rate

<div>\[{\dot V_{outsideair,max}} = {\dot V_{air,max}}\]</div>

#### Minimum Outside Air Flow Rate

<div>\[{\dot V_{outsideair,min}} = Min\left( {MinO{A_{zone}},{{\dot V}_{air,max}}} \right)\]</div>

### Packaged Terminal Heat Pump Sizing

Packaged terminal heat pumps are compound components: each unit contains a supply air fan, a DX cooling coil, a DX heating coil, a GAS or ELECTRIC supplemental heating coil, and an outside air mixer. The inputs that may need to be autosized are the supply air and outside air volumetric air flow rates during cooling operation, heating operation, and when no cooling or heating is needed. In addition, the maximum supply air temperature from the supplemental heater can also be automatically selected. The data needed for sizing the units are obtained from the zone design arrays.

#### Supply air volumetric flow rate during cooling operation

<div>\[{\mathop V\limits^ \cdot_{SA\,\,,\,cooling}} = \,\,MAX(DesCoolVolFlowzone,\,DesHeatVolFlowzone)\]</div>

#### Supply air volumetric flow rate during heating operation

<div>\[{\mathop V\limits^ \cdot_{SA\,\,,\,heating}} = \,\,MAX(DesCoolVolFlowzone,\,DesHeatVolFlowzone)\]</div>

#### Supply air volumetric flow rate when no cooling or heating is needed

<div>\[{\mathop V\limits^ \cdot_{SA\,,\,no\,cool\,or\,heat}} = \,\,MAX(DesCoolVolFlowzone,\,DesHeatVolFlowzone)\]</div>

#### Outside air volumetric flow rate during cooling operation

<div>\[{\mathop V\limits^ \cdot_{OA\,,\,\,cooling}} = {\rm{MIN}}(MinO{A_{zone}},{\mathop {\,V}\limits^ \cdot_{SA\,,\,\,cooling}})\]</div>

#### Outside air volumetric flow rate during heating operation

<div>\[{\mathop V\limits^ \cdot_{OA\,,\,\,heating}} = {\rm{MIN}}(MinO{A_{zone}},{\mathop {\,V}\limits^ \cdot_{SA\,,\,\,heating}})\]</div>

#### Outside air volumetric flow rate when no cooling or heating is needed

<div>\[{\mathop V\limits^ \cdot_{OA\,,\,no\,\,cool\,or\,heat}} = {\rm{MIN}}(MinO{A_{zone}},{\mathop {\,V}\limits^ \cdot_{SA\,,\,no\,\,cool\,or\,heat}})\]</div>

#### Maximum supply air temperature from supplemental heater

<div>\[{T_{SA,\max }} = HeatDesTem{p_{zone}}\]</div>

#### Unitary System Sizing

The AirloopHVAC:UnitarySystem object incorporates all coils types and fans as a complete packaged system. The fans and coils are optional allowing virtually any system type to be modeled. Sizing of this object depends on the coils selected. For single coil systems, the associated air flow rate is used as the operating flow rate (i.e., cooling or heating). For systems with both a cooling and heating coil, this methodology still applies except for DX systems (Heat Pumps) where the greater of the cooling or heating air flow rate is used. Heat pumps are defined as systems having both a DX cooling and DX heating coil. Other AirloopHVAC equipment models use the greater of the cooling and heating flow rates. The inputs that may need to be autosized are the supply air air volumetric air flow rates during cooling operation, heating operation, and when no cooling or heating is needed. The data needed for sizing the units are obtained from the zone design arrays.

Supply Air Flow Rate:

Supply air volumetric flow rate during cooling operation

<div>\[\dot{V}_{SA,cooling} = \rm{DesCoolVolFlow}_{zone}/\rm{ZoneFraction} \]</div>
 
Supply air volumetric flow rate during heating operation

<div>\[\dot{V}_{SA,heating} = \rm{DesHeatVolFlow}_{zone}/\rm{ZoneFraction} \]</div>
 
Supply air volumetric flow rate when DX coils are used as a system

<div>\[\dot{V}_{SA} = \max{\eft(\rm{DesCoolVolFlow},\rm{DesHeatVolFlow}\right)}/\rm{ZoneFraction} \]</div>
 
where ZoneFraction = Fraction of the total volume flow that goes through the controlling zone

The unitary system object also allows scalable sizing as follows:

Flow Per Floor Area:

<div>\[\dot{V}_{SA} = \rm{FlowPerFloorArea}\left(\rm{TotalFloorArea}\right)\]</div>

Fraction of Autosized Cooling Value:

<div>\[\dot{V}_{SA,cooling} = \dot{V}_{SA,des,cooling}\left(\rm{FractionOfCoolingValue}\right)\]</div>
 
Fraction of Autosized Heating Value:

<div>\[\dot{V}_{SA,heating} = \dot{V}_{SA,des,heating}\left(\rm{FractionOfHeatingValue}\right)\]</div>
 
Flow Per Cooling Capacity

<div>\[\dot{V}_{SA,cooling} = \dot{Q}_{coil,des,cooling}\left(\rm{FractionOfCoolingValue}\right)\]</div>

Flow Per Heating Capacity

<div>\[\dot{V}_{SA,heating} = \dot{Q}_{coil,des,heating}\left(\rm{FractionOfHeatingValue}\right)\]</div>

The maximum supply air temperature can also be automatically selected. The value is determined from the Sizing:Zone or Sizing:System object depending on where the object is used in the simulation (i.e., as zone or air loop equipment).

Maximum supply air temperature
 
<div>\[T_{SA,max}=\rm{HeatDesTemp}_{ZoneOrSystem}]</div>

### MultiSpeed Heat Pump Sizing

MultiSpeed heat pumps are compound components: each unit contains a supply air fan, a multispeed DX cooling coil, a multispeed DX heating coil, and a GAS or ELECTRIC supplemental heating coil. The inputs that may need to be autosized are the supply air volumetric air flow rates during cooling operation, heating operation, and when no cooling or heating is needed. The data needed for sizing the units are obtained from the controlled zone design arrays.

#### Supply air volumetric flow rate during cooling operation at the highest speed

<div>\[{\mathop V\limits^ \cdot_{SA\,\,,\,cooling}} = \,\,MAX(DesCoolVolFlowzone,\,DesHeatVolFlowzone)/ZoneFraction\]</div>

#### Supply air volumetric flow rate during heating operation at the highest speed

<div>\[{\mathop V\limits^ \cdot_{SA\,\,,\,heating}} = \,\,MAX(DesCoolVolFlowzone,\,DesHeatVolFlowzone)/ZoneFraction\]</div>

#### Supply air volumetric flow rate when no cooling or heating is needed

<div>\[{\mathop V\limits^ \cdot_{SA\,,\,no\,cool\,or\,heat}} = \,\,MAX(DesCoolVolFlowzone,\,DesHeatVolFlowzone)/ZoneFraction\]</div>

where

ZoneFraction           = Fraction of the total volume flow that goes through the controlling zone

#### Supply air volumetric flow rate during cooling operation at Speed n (1 to NumberOfSpeed-1)

<div>\[{\mathop V\limits^ \cdot_{SA\,\,,\,cooling,n}} = \frac{n}{{NumberOfSpeed}}{\mathop V\limits^ \cdot_{SA\,\,,\,cooling}}\]</div>

#### Supply air volumetric flow rate during heating operation at Speed n (1 to NumberOfSpeed-1)

<div>\[{\mathop V\limits^ \cdot_{SA\,\,,\,heating,n}} = \frac{n}{{NumberOfSpeed}}{\mathop V\limits^ \cdot_{SA\,\,,\,heating}}\]</div>

where

n    = Speed Index number from 1 to NumberOfSpeed-1

NumberOfSpeed     = The highest speed number

### Single Duct Terminal Units

These are all the EnergyPlus components whose names begin with "*AirTerminal:SingleDuct:*" (except for Cooled Beam units). This includes *Uncontrolled, ConstantVolume:Reheat, VAV:NoReheat, VAV:Reheat, VAV:Reheat:VariableSpeedFan, VAV:HeatAndCool:NoReheat, VAV:HeatAndCool:Reheat, SeriesPIU:Reheat, ParallelPIU:Reheat,* and *ConstantVolume:FourPipeInduction*. The inputs that may need to be autosized are the various maximum air flow rates through the unit, minimum air flow rates, and maximum hot water and/or chilled water flow rates if heating or cooling coils are present.

*Note:* all zone design flow rates and loads referenced below may have been altered by system sizing inputs. For instance, if the user specifies a *Cooling Design Air Flow Method = Flow/System*  and specifies a *Cooling Design Air Flow Rate* the zone cooling design values will be altered to match the specified system flow rate.

#### Maximum Air Flow Rate

<div>\[{\dot V_{air,\max ,{\rm{terminal}}}} = Max(DesCoolVolFlowzone,DesHeatVolFlowzone)\]</div>

#### Maximum Heating Air Flow Rate

<div>\[{\dot V_{air,max,heat,terminal}} = DesHeatVolFlowzone\]</div>

#### Maximum Primary and Secondary Air Flow Rates

For the PIU terminal units, the maximum primary and secondary air flow rates are sized to the same value as the maximum total air flow rate.

#### Minimum Air Flow Rate

Basically minimum air flow rates are sized to the ventilation air requirement. This may be more or less complicated.

For the PIU's, the minimum primary air flow fraction is set to

<div img="image2233.txt">\(\mathop {MinOA}\nolimits_{zone} /{\dot V_{air,max,primary,terminal}}\)</div>.

For other VAV terminal units

<div>\[{\dot V_{air,min,terminal}} = Fra{c_{air,\min }}*DesVolFlowzone\]</div>

where, *Fracair,min*corresponds to the minimum flow fraction of the teminal unit. This value is provided as user input, typically as the field “Zone Minimum Air Flow Fraction.” For the VAV terminals that allow scheduling minimum flow fraction (e.g., AirTerminal:SingleDuct:VAV:Reheat), there are two ways that *Fracair,min*can be determined. If a value is entered in the input field Constant Minimum Air Flow Fraction, then it is always used for *Fracair,min*. If the mimimum air flow fraction method is “Schedule” and the Constant Minimum Air Flow Fraction is left blank, then the program uses the average of the minimum and maximum values in the schedule for *Fracair,min*.

#### Fan On Flow Fraction

For the parallel PIU, this is set to the minimum primary air flow fraction.

#### Max Hot  Water Flow

<div>\[{T_{coil,in}} = DesHeatCoilInTem{p_{zone}}\]</div>

<div>\[{T_{coil,out}} = HeatDesTem{p_{zone}}\]</div>

The coil load and max hot water flow rate are then:

<sub><div img="image2237.txt">\(\dot Qcoil,des = cp,air\cdot \rho air\cdot \dot Vair,coil,heating\cdot (Tout,coil - Tin,coil)\)</div></sub>

<div>\[\dot Vmax,hw = \dot Qcoil,des/(cp,w\cdot \rho w\cdot \Delta Tloop,des)\]</div>

where

*c<sub>p,air</sub>* is evaluated at the average of the inlet & outlet temperatures and the coil outlet humidity ratio.

The four-pipe induction unit sizes the heating coil differently: to meet the zone load rather than match the design zone supply temperature. The load on the hot water coil is then the zone load minus whatever the central system does.

<div>\[{\dot Q_{coil,des}} = {\dot Q_{zone,des}} - {c_{p,air}}{\rho_{air}}{\dot V_{air,primary,des}}({T_{coil,in}} - {T_{zone,heatpeak}})\]</div>

where

<div>\[{T_{coil,in}} = DesHeatCoilInTem{p_{zone}}\]</div>

<div>\[{T_{zone,heatpeak}} = ZoneTempAtHeatPea{k_{zone}}\]</div>

<div>\[{\dot V_{air,primary,des}} = {\dot V_{air,max,terminal}}/(1 + {R_{induc}})\]</div>

#### Max Chilled Water Flow

The chilled water flow for the four-pipe induction unit is sized analogously to the hot water flow.

### Indirect Evaporative Cooler Sizing

The model for the object called EvaporativeCooler:Indirect:ResearchSpecial has a field for the secondary fan flow rate that can be autosized.

#### Secondary Fan Flow Rate

The secondary fan is not part of an airstream that is directly modeled in EnergyPlus.  Because the primary side air flows can be autosized as part of the air system, it is convenent to also scale the size of the secondary flow.   If the cooler is part of the main loop of a central air system, then the secondary fan flow rate is sized to equal to the main design flow rate.

<div>\[\dot Vfan,max = DesMainVolFlowsys\]</div>

If the cooler is part of the outdoor air path of a central air system, then the secondary fan flow rate is sized to be the maximum of either the design minimum outdoor air flow rate or one-half of the main design flow rate.

<div>\[{\dot V_{fan,\max }} = MAX({\rm{DesOutAirVolFlow, 0}}{\rm{.5*DesMainVolFlow)}}\]</div>

### Desiccant Dehumidifier Sizing

The sizing of dehumidifier with no fans is done in subroutine SizeDesiccantDehumidifier.

#### Nominal Process Air Flow Rate

If the unit is part of zone equipment, then:

<div>\[{\dot V_p} = Max\left( {DesCoolVolFlo{w_{zone}},DesHeatVolFlo{w_{zone}}} \right)\]</div>

If the unit is part of the central air system, then check if outdoor air system is present. If outdoor air system is part of the air loop and design outdoor air flow rate is greater than zero, then:

<div>\[{\dot V_p} = DesOutAirVolFlo{w_{sys}}\]</div>

Otherwise, nominal process air flow rate is determined as follows:

for duct type = *main*

<div>\[{\dot V_p} = DesMainVolFlo{w_{sys}}\]</div>

for duct type = *cooling*

<div>\[{\dot V_p} = DesCoolVolFlo{w_{sys}}\]</div>

for duct type = *heating*

<div>\[{\dot V_p} = DesHeatVolFlo{w_{sys}}\]</div>

for duct type = *other*

<div>\[{\dot V_p} = DesMainVolFlo{w_{sys}}\]</div>

### Evaporative Cooler Sizing

The sizing of evaporative cooler objects is done in subroutine SizeEvapCooler.

#### Secondary Fan Flow Rate

<div>\[\dot V = DesMainVolFlo{w_{sys}}\]</div>

Note that it is only applicable to indirect CELdek pad object.

#### Direct Pad Area

As from the continuity equation, the area of direct pad is directly determined by assuming face air velocity of 3m/s as:

<div>\[{A_{pad}} = \frac{{DesMainVolFlo{w_{sys}}}}{V}\]</div>

where

*A<sub>pad</sub>* is area of direct pad, m<sup>2</sup>

*V* is face air velocity, m/s

#### Direct Pad Depth

The solution of the following regression equation is used, assuming saturation effectiveness of 70% and face air velocity of 3m/s:

ɛ = 0.792714 + 0.958569D - 0.25193V - 1.03215D2 + 0.0262659V2 + 0.914869DV

- 1.48241VD<sup>2</sup> - 0.018992 D V<sup>3</sup> + 1.13137D<sup>3</sup>V + 0.0327622 D<sup>2</sup>V<sup>3</sup> - 0.145384D<sup>3</sup>V<sup>2</sup>

where

D is depth of pad, m

ɛ is saturation effectiveness

### Heat Recovery Sizing

The sizing of heat exchanger objects is done in subroutine SizeHeatRecovery.

#### Nominal Supply Air Flow Rate

If the unit is part of zone equipment, then:

<div>\[{\dot V_p} = Max\left( {DesCoolVolFlo{w_{zone}},DesHeatVolFlo{w_{zone}}} \right)\]</div>

If the unit is part of the central air system, then check if outdoor air system is present. If outdoor air system is part of the air loop and design outdoor air flow rate is greater than zero, then:

<div>\[{\dot V_p} = DesOutAirVolFlo{w_{sys}}\]</div>

Otherwise, nominal supply air flow rate is determined as follows:

for duct type = *main*

<div>\[{\dot V_p} = DesMainVolFlo{w_{sys}}\]</div>

for duct type = *cooling*

<div>\[{\dot V_p} = DesCoolVolFlo{w_{sys}}\]</div>

for duct type = *heating*

<div>\[{\dot V_p} = DesHeatVolFlo{w_{sys}}\]</div>

for duct type = *other*

<div>\[{\dot V_p} = DesMainVolFlo{w_{sys}}\]</div>

#### Nominal Supply Air Flow Rate

It is assumed to be equal to the nominal supply air flow rate.

### Low Temperature Radiant System Sizing

The sizing is done in subroutine *SizeLowTempRadiantSystem*.

#### ZoneHVAC:LowTemperatureRadiant:Electric:

##### Nominal Capacity

<div>\[Cap = DesHeatLoa{d_{zone}} \cdot HeatSizingFactor\]</div>

#### ZoneHVAC:LowTemperatureRadiant:VariableFlow:

##### Maximum Hot Water Flow

<div>\[{\dot V_h} = \frac{{DesHeatLoa{d_{zone}} \cdot HeatSizingFactor}}{{\Delta {T_h} \cdot {c_{p,h}} \cdot {\rho_h}}}\]</div>

where

<div img="image2261.txt">\({\dot V_h}\)</div>* is maximum hot water flow rate, m<sup>3</sup>/s*

*c<sub>p,h</sub> is specific heat of hot water at reference condition (60°C), J/kgK*

*r<sub>h</sub> is the density of water at reference condition (60°C), kg/m<sup>3</sup>*

##### Maximum Cool Water Flow

<div>\[{\dot V_c} = \frac{{DesCoolLoa{d_{zone}} \cdot CoolSizingFactor}}{{\Delta {T_c} \cdot {c_{p,c}} \cdot {\rho_c}}}\]</div>

<div img="image2263.txt">\({\dot V_c}\)</div>* is maximum chilled water flow rate, m<sup>3</sup>/s*

*c<sub>p,c</sub> is specific heat of hot water at reference condition (5°C), J/kgK*

*r<sub>c</sub> is the density of chilled water at reference condition (5°C), kg/m<sup>3</sup>*

##### Hydronic Tubing Length

<div>\[TubeLength = \frac{{TotalSurfaceArea}}{{TubeSpacing}}\]</div>

Note that tube spacing is assumed to be 0.15m.

#### ZoneHVAC:LowTemperatureRadiant:ConstantFlow:

##### Rated Flow Rate

The object provides both cooling and heating, and also operates in a single operating mode such as cooling-only or heating-only mode. Thus, the rated flow rate is determined, depending upon the operating mode of the unit. If the unit operates in a single operating mode, either design chilled water or hot water flow rate is chosen. The larger of the two is chosen if the unit provides both cooling and heating. The flow rates are determined in the same fashion to the variable flow system above.

##### Hydronic Tubing Length

The length of hydronic tube is determined as described in the variable flow radiant system above.

Zone Outdoor Air Design Data
----------------------------

Outdoor air design data may be required for many aspects of a building computer model. Sizing of HVAC equipment, infiltration and ventilation, and specific outdoor air requirements for different zone types are a few examples where required outdoor air quantities may vary. Since there would be a significant chance for data input errors if each individual aspect of the simulation model allowed for independent input of outdoor air design data, this general object is used to define outdoor air design data and this data set may be used throughout the building simulation model.

The design data is provided as a group of inputs that are recognized by designers as standard practice. This information may be used individually or used as a group to calculate summations or maximums of the entered data. These design data include values for:

·        Outdoor air per person

·        Outdoor air per zone floor area

·        Outdoor air per zone

·        Outdoor air changes per hour

This design data is entered in an outdoor air design data object and may be referenced by other objects during the simulation. A single specification for outdoor air design data may be used by all other appropriate objects within EnergyPlus, or multiple outdoor air design data objects may be specified and these design data objects may be used as necessary by other objects when outdoor air design quantaties vary for any reason.

### Design Outdoor Air Calculation

The outdoor air design data is entered as a group and referenced through name association to this group of design data (Ref. DesignSpecification:OutdoorAir). The reference name in the following example is “ZoneOAData”.

A DesignSpecification:OutdoorAirexample:

DesignSpecification:OutdoorAir,

    ZoneOAData,            !- Name

    Maximum,               !- Outdoor Air Method

    0.00944,               !- Outdoor Air Flow per Person {m3/s}

    0.00305;               !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}

    ,                      !- Outdoor Air Flow per Zone

    ,                      !- Outdoor Air Flow Air Changes per Hour

    Min OARequirements Sched; !- Outdoor Air Flow Rate Fraction Schedule Name

Given this set of data, the quantity of outdoor air is calculated based on the Outdoor Air Method specified in each outdoor air design data object. In this example, the maximum of the per person and per zone floor area is used to establish the outdoor air quantity.

As previously mentioned, this group of outdoor air design data is reference by other objects used in the simulation. The reference is by the *name* of the outdoor air design data object. Any reference to this name by other objects simply means that the object referencing this data set will use the values specified in this common object. Note that a zone name is not included in the list of data and the calculation of occupancy, zone floor area, or zone volume is implied through this named reference and *the connection to a zone via the referencing object*. For example, if a terminal unit references an outdoor air design data object, the zone served by that terminal unit is used to determine the occupancy, zone floor area, and zone volume in the following calculations.

<div>\[O{A_{people}} = Oc{c_{zone}}\left( {OAFlowperPerson} \right)\]</div>

<div>\[O{A_{floor\,area}} = {A_{zone}}\left( {OAFlowperFloorArea} \right)\]</div>

<div>\[O{A_{zone}} = \left( {OAFlowperZone} \right)\]</div>

<div>\[O{A_{ACH}} = {V_{zone}}\left( {{\raise0.7ex\hbox{${OAFlowAirChangesperHour}$} \!\mathord{\left/ {\vphantom {{OAFlowAirChangesperHour} {3600}}}\right.}\!\lower0.7ex\hbox{${3600}$}}} \right)\]</div>

where:

<div img="image2269.txt">\(O{A_{people}}\)</div>= outdoor air volume flow rate based on occupancy, [m<sup>3</sup>/s]

<div img="image2270.txt">\(Oc{c_{zone}}\)</div>= number of occupants in zone, [people]

<div img="image2271.txt">\(OAFlowperPerson\)</div>= outdoor air volume flow rate per person, [m<sup>3</sup>/s-person]

<div img="image2272.txt">\(O{A_{floor\;area}}\)</div>= outdoor air volume flow rate based on zone floor area, [m<sup>3</sup>/s]

<div img="image2273.txt">\({A_{zone}}\)</div>= zone floor area, [m<sup>2</sup>]

<div img="image2274.txt">\(OAFlowperFloorArea\)</div>= outdoor air volume flow rate per zone floor area, [m<sup>3</sup>/s-m<sup>2</sup>]

<div img="image2275.txt">\(O{A_{zone}}\)</div>= zone outdoor air volume flow rate, [m<sup>3</sup>/s]

<div img="image2276.txt">\(OAFlowperZone\)</div>= outdoor air volume flow rate per zone, [m<sup>3</sup>/s]

<div img="image2277.txt">\(O{A_{ACH}}\)</div>= outdoor air volume flow rate based on air changes per hour, [m<sup>3</sup>/s]

<div img="image2278.txt">\({V_{zone}}\)</div>= zone volume, [m<sup>3</sup>]

<div img="image2279.txt">\(OAFlowAirChangesperHour\)</div>= outdoor air volume flow in air changes per hour, [m<sup>3</sup>/s-m<sup>3</sup>]

Given the calculations for each specific type of design data, the method used to calculate the outdoor air design data is then based on a user selected method for this specific outdoor air design data object. The outdoor air methods used to calculate the outdoor air quantity and the associated value for outdoor air volume flow rate are shown here.

Flow/Person =&gt; <div img="image2280.txt">\(O{A_{people}}\)</div>

Flow/Area =&gt; <div img="image2281.txt">\(O{A_{floor\,area}}\)</div>

Flow/Zone =&gt; <div img="image2282.txt">\(O{A_{zone}}\)</div>

AirChanges/Hour =&gt; <div img="image2283.txt">\(O{A_{ACH}}\)</div>

Sum =&gt; <div img="image2284.txt">\(O{A_{people}}\, + \,O{A_{floor\,area}}\, + \,O{A_{zone}}\, + \,O{A_{ACH}}\)</div>

Maximum =&gt; <div img="image2285.txt">\(MAX\left( {O{A_{people}}\,,\,O{A_{floor\,area}}\,,\,O{A_{zone}}\,,\,O{A_{ACH}}} \right)\)</div>

If an Outdoor Air Flow Rate Fraction Schedule Name is specified, the flow rate determined above will be multiplied by the current schedule value.

Examples of objects that reference the outdoor air design data object are:

n AirTerminal:SingleDuct:VAV:NoReheat

n AirTerminal:SingleDuct:VAV:Reheat

### References

ASHRAE Fundamentals 2001. 2001 ASHRAE Fundamentals Handbook. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.

Pedersen, C.O., D.E. Fisher, and R.J. Liesen. 1997. A heat balance based cooling load calculation procedure. ASHRAE Transactions, Vol. 103(2), pp. 459-468.

Pedersen, C.O. 2001. Toolkit for Building Load Calculations. Atlanta: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc.
