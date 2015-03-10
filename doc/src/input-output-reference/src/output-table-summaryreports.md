# Output:Table:SummaryReports

The Output:Table:SummaryReports object controls which predefined tabular reports are produced.  The easiest option is to specify "AllSummary" which will produce all of the summary reports described in this section and does not include the [Zone](#zone) Component Load report. In addition, "AllMonthly" will produce all of the monthly reports described. If all predefined reports are needed. "AllSummaryAndMonthly" shows all of the summary and monthly predefined reports and does not include the [Zone](#zone) Component Load report. The "AllSummaryAndSizingPeriod" option and "AllSummaryMonthlyAndSizingPeriod" option are similar and add the [Zone](#zone) Component Load report. Including the [Zone](#zone) Component Load Summary report may increase the simulation run time.

## Inputs

#### Field: Report <#> Name

All of the fields in the Output:Table:SummaryReports are the same. A long list of predefined summary reports is available by entering one of the available key choices described below. Each one indicates the name of the predefined reports that should be output. The input AllSummary will cause all the reports described below to be created.  The Report <#> Name field can be repeated.

### Predefined Annual Summary Reports

#### Annual Building Utility Performance Summary (sometimes called ABUPS)

The Annual [Building](#building) Utility Performance Summary report often called ABUPS – (key: AnnualBuildingUtilityPerformanceSummary) produces a report that is an overall summary of the utility consumption of the building. It contains a number of subtables that are each described below.

- Site and Source Energy – Indicates the total site and source energy use. For electricity the net electricity from the utility is used for the electric contribution. The site to source conversion factors are based on those entered by the user. These are entered in the [EnvironmentalImpactFactors](#environmentalimpactfactors) object and [FuelFactors](#fuelfactors) objects.
- [Building](#building) Area – Shows the total floorspace of the building and the conditioned floorspace.
- End Uses – This shows the total use of electricity, natural gas, other fuels, purchased cooling, purchased heating and water for each major end-use category. The end-use categories are Heating, Cooling, Interior Lighting, Exterior Lighting, Interior Equipment, Exterior Equipment, Fans, Pumps, Heat Rejection, Humidification, Heat Recovery, Hot Water, Refrigeration, and Generators. Not all fuels have corresponding end uses. The values in this sub-table are from output meters. To determine which components are attached to each end-use meter, consult the meter details output file (\*.mtd). The source of the resource does not affect this table – the amount of electricity used for lights does not change if the electricity is from the utility or from an on-site generator. The Other Fuel column includes fuel oil#1, fuel oil#2, gasoline, coal, propane, diesel, otherfuel1 and otherfuel2. The district heating column also  includes steam.
- End Uses By Subcategory – Shows a breakdown of the major end uses by user-defined category. If an end-use subcategory was not input for an object, it is automatically added to the General subcategory for that major end-use category.
- Utility Use Per Floor Area – These two sub-tables show the results from the end-uses table divided by the total floor area defined for the building and for the total conditioned floor area. Only three categories for end-uses are used for these sub-tables, lighting, HVAC and other. HVAC includes fans, pumps, heating, cooling, heat rejection, humidification, and domestic hot water heating. The Other Fuel column includes fuel oil#1, fuel oil#2, gasoline, coal, propane, diesel, otherfuel1 and otherfuel2. The district heating column also  includes steam.
- Electric Loads Satisfied – Shows the different methods that electric loads are satisfied in the building. The values shown for on site power generation are: Fuel-Fired Power Generation, High Temperature Geothermal, Photovoltaic Power, and Wind Power. The flows to and from the electric utility are shown next and finally the total electricity used at the site is compared to the total generated on site plus the net amount from the electric utility. The percentages shown are based on the total electricity used by the end-uses. Note that High Temperature Geothermal and Wind Power are not yet implemented in EnergyPlus.
- On-Site Thermal Sources – Shows the on-site thermal sources of energy such as Water-Side Heat Recovery, Air to Air Heat Recovery for Cooling, Air to Air Heat Recovery for Heating, High-Temperature Geothermal, Solar Water Thermal, Solar Air Thermal. Note that High-Temperature Geothermal Solar Water Thermal, and Solar Air Thermal are not yet implemented in EnergyPlus.
- Water Loads Summary – Shows the different methods the water loads were satisfied. This table shows all zeros because water use is not yet implemented in EnergyPlus.

#### Input Verification and Results Summary (or IVRS)

The Input Verification and Results Summary report (key: InputVerificationandResultsSummary) produces a report with several tables including:

- General which includes general information like the Program [Version](#version) and Build, Weather, Latitude, Longitude, Elevation, Time [Zone](#zone), North Axis Angle, and Hours Simulated.
- Window-Wall Ratio table for envelope which includes the wall area, the window area and the ratio of the two. These are computed for all walls and for walls that are oriented generally north, south, east and west. All walls are categorized into one of these four cardinal directions. This is computed for walls that have a tilt of 60 to 120 degrees.
- Skylight-Roof Ratio table for envelope which includes the roof area and the skylight area and the ratio of the two. This includes all surfaces with a tilt of less than 60 degrees.
- [Zone](#zone) Summary includes internal load summary for each zone including area, if conditioned, volume, multipliers, gross wall area, window area, design lighting, design people, design plug and process.

#### Source Energy End Use Components Summary

The Source Energy End Use Components Summary report produces a report (key: SourceEnergyEndUseComponentsSummary) that includes three tables. These tables display source energy by fuel type that is calculated based on site to source energy factors specified by the user in the [EnvironmentalImpactFactors](#environmentalimpactfactors) and [FuelFactors](#fuelfactors) objects. The last two tables display the source energy in terms of area normalized metrics. Following is a description of each table:

- Source Energy End Use Components – This shows the total use of source electricity, source natural gas, source values of other fuels, source purchased cooling   and purchased heating for each major end-use category. The end-use categories are Heating, Cooling, Interior Lighting, Exterior Lighting, Interior Equipment, Exterior Equipment, Fans, Pumps, Heat Rejection, Humidification, Heat Recovery, Hot Water, Refrigeration, and Generators. Not all fuels have corresponding end uses. The values in this sub-table are from output meters. To determine which components are attached to each end-use meter, consult the meter details output file (\*.mtd). The source of the resource will affect this table – the amount of electricity used for lights will change if the electricity is from the utility or from an on-site generator. The Other Fuel column includes FuelOil#1, FuelOil#2, Gasoline, Coal, Propane, Diesel, OtherFuel1 and OtherFuel2. The district heating column also  includes steam.
- Source Energy End Use Components normalized by Conditioned Floor Area – This table shows the total end uses in source energy normalized by conditioned floor area.
- Source Energy End Use Components normalized by Total Floor Area – This table shows the total end uses in source energy normalized by total floor area.  

#### Climatic Data Summary

The Climate Summary or Climatic Data Summary report (key: ClimaticDataSummary) produces a report that includes some of the details on each of the design days including: maximum dry-bulb, daily temperature range, humidity value, humidity type, wind speed, and wind direction.

#### Envelope Summary

The Envelope Summary report (key: EnvelopeSummary) produces a report that includes the following tables:

- Opaque which includes all opaque surfaces and includes the name of the construction, reflectance, U-Factor, gross area, azimuth, tilt, cardinal direction.
- Fenestration which includes all non-opaque surfaces and includes the name of the construction, areas (glass, frame, divider, single opening, multiplied openings), U-Factor, SHGC (the solar heat gain coefficient based on summer conditions), visible transmittance, conductance (frame, divider), indication of shade control, the name of the parent surface, azimuth, tilt, cardinal direction.

#### Surface Shadowing Summary

The Surface Shadowing Summary report (key: SurfaceShadowingSummary) produces a report that includes two tables. Note that surfaces starting with "Mir-" are automatically generated by EnergyPlus and are the mirror images of user entered surfaces.

- Surfaces (Walls, Roofs, etc) that may be Shadowed by Other Surfaces and includes the name of the surface and a list of surfaces that may possibly cast shadows on that named surface. The list of possible shadow casters does not necessarily mean that they do cast shadows during the simulation, only that their relative position makes it possible that shadows from a surface in the list may fall on the named surface.
- Subsurfaces (Windows and Doors) that may be Shadowed by Surfaces, includes the name of the subsurface such as a window or a door and a corresponding list of surfaces that may be casting shadows on the windows and doors. 

#### Shading Summary

The Shading Summary report (key: ShadingSummary) produces a report that includes the following tables:

- Sunlit Fraction which shows a list of windows and the fraction of the window that is sunlit for nine specific times of the year. The nine specific times include 9am, noon and 3pm on March 21, June 21, and December 21. These nine times were chosen to represent the range of sun angles. The simulation must include those times for the value to be included in the report.
- [Window](#window) control includes the names of all windows that have a window shading control (see [WindowProperty:ShadingControl](#windowpropertyshadingcontrol)) and includes the name of the control, the type of shading, the shaded construction, the kind of control, and if glare control is used.

#### Lighting Summary

The Lighting Summary report (key: LightingSummary) produces a report that includes the following tables:

- Interior Lighting which includes the name of the lights object, the zone that it is used in, the lighting power density, zone area, total power, end use subcategory, schedule name, average hours per week, return air fraction, and whether the zone is conditioned.
- Daylighting which includes the names of the daylighting objects, the zone they are used in, the type of daylighting being used, the control type, the fraction of the lighting controlled, the total power of the lighting installed in the zone and the lighting power that is controlled by the daylighting object. 
- Exterior Lighting which includes the name of the ExteriorLights object, the total watts described by the object, if the ExteriorLights uses an astronomical clock or just a schedule, the name of the schedule used, and the average hours per week for the named schedule for the year. The effect of the astronomical clock does not get included in the averaged hours per week shown. 

#### Equipment Summary

The Equipment Summary report (key: EquipmentSummary) produces a report that includes some details on the major HVAC equipment present. The report has seven parts.

- Central Plant includes details on chillers, boilers, and cooling towers including the capacity and efficiency. For [Chiller:Electric:EIR](#chillerelectriceir) and [Chiller:Electric:ReformulatedEIR](#chillerelectricreformulatedeir), IPLV at AHRI standard test conditions is reported.
- Cooling Coils includes the nominal total, sensible and latent capacities, the nominal sensible heat ratio, the nominal efficiency, nominal UA value, and nominal  surface area for each cooling coil. These values are calculated by calling the cooling coil simulation routine with the rated inlet conditions: inlet air dry bulb temperature = 26.67C, inlet air wet bulb temperature = 19.44C, inlet chilled water temperature = 6.67C.
- DX Cooling Coils summarizes the Standard Rating (Net) Cooling Capacity, SEER, EER and IEER values at AHRI standard test. Currently, these values are only reported for coil type = [Coil:Cooling:DX:SingleSpeed](#coilcoolingdxsinglespeed) with condenser type = AirCooled.
- DX Heating Coils summarizes the High Temperature Heating Standard (Net) Rating Capacity, Low Temperature Heating Standard (Net) Rating Capacity and Heating Seasonal Performance Factor (HSPF) values at AHRI standard test. Currently, these values are only reported for coil type = [Coil:Heating:DX:SingleSpeed](#coilheatingdxsinglespeed).
- Heating Coils includes the nominal capacity and efficiency for each heating coil. The capacity is calculated by calling the heating coil simulation routine at the rated inlet conditions: inlet air dry bulb temperature = 16.6C, inlet relative humidity = 50%, inlet hot water temperature = 82.2C.
- Fan includes the type of fan, the total efficiency, delta pressure, max flow rate, motor heat in air fraction, and end use.
- Pumps includes the type of pump, control type, head pressure, electric power, and motor efficiency for each pump.
- Service Water Heating includes the type of water heater, the storage volume, input, thermal efficiency, recovery efficiency, and energy factor.

#### HVAC Sizing Summary

The HVAC Sizing Summary report (key: HVACSizingSummary) produces a report that includes the following tables:

- [Zone](#zone) Cooling which includes the following columns for each zone: the calculated design load, the user specified design load, the calculated design air flow, the user specified design air flow, the name of the sizing period, the time of the peak load during the sizing period, the temperature at the time of the peak load during the sizing period, and the humidity ratio at the time of the peak load during the sizing period used.
- [Zone](#zone) Heating which includes the following columns for each zone: the calculated design load, the user specified design load, the calculated design air flow, the user specified design air flow, the name of the sizing period, the time of the peak load during the sizing period, the temperature at the time of the peak load during the sizing period, and the humidity ratio at the time of the peak load during the sizing period used.
- System Design Air Flow Rates which includes the following columns for each air loop: the calculated cooling air flow rate, the user specified air flow rate for cooling, the calculated heating air flow rate, the user specified air flow rate for heating.
- *Note:* values listed as "calculated" are the unaltered result of the zone or system sizing calculations, using the design sizing period weather and schedules specified in the input. Values listed as "user specified" are either the calculated values modified by global or zone sizing factors or values specified with the *flow/zone* or *flow/system* design air flow method.

#### Component Sizing Summary

The Component Sizing Summary report (key: ComponentSizingSummary) produces a report that includes different tables depending on the kinds of HVAC components that exist in the input file. A table is shown for each type of HVAC component that is sized. The table lists the objects of that type of component that appear in the input file and one or more parameters related to that component.  For example, the [AirTerminal:SingleDuct:VAV:Reheat](#airterminalsingleductvavreheat) component creates a table showing the maximum air flow rate from the sizing calculations and the maximum reheat water flow rate. Another example is the [Fan:VariableVolume](#fanvariablevolume) object which shows a table with both the maximum and minimum flow rates for each fan based on the results from the sizing calculations.

#### Outdoor Air Summary

The Outdoor Air Summary report (key: OutdoorAirSummary) produces a report that includes the following tables:

- Average Outside Air During Occupied Hours table shows for each zone the average and nominal number of occupants, the zone volume, the average air change rate based on mechanical ventilation, infiltration and simple ventilation during occupied hours.
- Minimum Outside Air During Occupied Hours table shows for each zone the average and nominal number of occupants, the zone volume, the minimum air change rate based on mechanical ventilation, infiltration and simple ventilation during occupied hours.

#### System Summary

The System Summary Report (key: SystemSummary) produces a report that includes the following tables:

- Economizer which includes the following columns for each [Controller:OutdoorAir](#controlleroutdoorair) object: the high limit shutoff control, the minimum outdoor air flow, the maximum outdoor air flow, if the return air temperature has a control limit, if the return air has an enthalpy limit, the outdoor air temperature limit, and the outdoor air enthalpy limit.
- Demand Controlled Ventilation table is for each [Controller:MechanicalVentilation](#controllermechanicalventilation) object and shows the name, the nominal outdoor air per person and the nominal outdoor air per zone area.
- Time Not Comfortable Based on Simple ASHRAE 55-2004 table shows how many hours that the space is not comfortable for each zone under the criteria of assuming winter clothes, summer clothes or both summer and winter clothes.  See the [People](#people) object for more information about this thermal comfort calculation.
- Time Setpoint is Not Met table shows how many hours the space is more than 0.2C from the setpoint during heating and during cooling.  The last two columns indicate those hours that the setpoint is not met while the space is occupied. 

#### Adaptive Comfort Summary

The Adaptive Comfort Summary report (key: AdaptiveComfortSummary) produces a report tabulating the sum of occupied hours not meeting adaptive comfort acceptability limits for each relevant [People](#people) object ([People](#people) objects for which adaptive comfort calculations are requested). These acceptability limits include ASHRAE Std. 55 80%, ASHRAE Std. 55 90%, CEN-15251 Category I, CEN-15251 Category II, and CEN-15251 Category III.

#### Sensible Heat Gain Summary

The Sensible Heat Gain Summary (key: SensibleHeatGainSummary) provides results for each zone and the overall building for some of the major heat gain components. The first four columns show the loads satisfied by sensible air heating and cooling as well as radiant heating and cooling surfaces in the zone. The heat gains from people, lighting, equipment, windows, interzone air flow, and infiltration are shown when adding heat to the zone and separately when removing heat from the zone (for applicable components). Finally the balance is shown as "Opaque Surface Conduction and Other Heat Addition" and "Opaque Surface Conduction and Other Heat Removal" which is a term indicating the affect of the walls, floors and ceilings/roof to the zone as well as the impact of the delay between heat gains/losses and loads on the HVAC equipment serving the zone.  The following shows each output variable that is used for each column. For each timestep in the simulation, positive values are shown as additions and negative values are shown as removal for most variables.

- HVAC Input Sensible Air Heating

[Zone](#zone) Air Heat Balance System Air Transfer Rate

[Zone](#zone) Air Heat Balance System Convective Heat Gain Rate

- HVAC Input Sensible Air Cooling

[Zone](#zone) Air Heat Balance System Air Transfer Rate

[Zone](#zone) Air Heat Balance System Convective Heat Gain Rate

- HVAC Input Heated Surface Heating

[Zone](#zone) Radiant HVAC Heating Energy

[Zone](#zone) Ventilated Slab Radiant Heating Energy

- HVAC Input Cooled Surface Cooling

[Zone](#zone) Radiant HVAC Cooling Energy

[Zone](#zone) Ventilated Slab Radiant Cooling Energy

- [People](#people) Sensible Heat Addition

[Zone](#zone) [People](#people) Sensible Heating Energy

- [Lights](#lights) Sensible Heat Addition

[Zone](#zone) [Lights](#lights) Total Heating Energy

- Equipment Sensible Heat Addition & Equipment Sensible Heat Removal 

[Zone](#zone) Electric Equipment Radiant Heating Energy

[Zone](#zone) Gas Equipment Radiant Heating Energy

[Zone](#zone) Steam Equipment Radiant Heating Energy

[Zone](#zone) Hot Water Equipment Radiant Heating Energy

[Zone](#zone) Other Equipment Radiant Heating Energy

[Zone](#zone) Electric Equipment Convective Heating Energy

[Zone](#zone) Gas Equipment Convective Heating Energy

[Zone](#zone) Steam Equipment Convective Heating Energy

[Zone](#zone) Hot Water Equipment Convective Heating Energy

[Zone](#zone) Other Equipment Convective Heating Energy

- [Window](#window) Heat Addition & [Window](#window) Heat Removal

[Zone](#zone) Windows Total Heat Gain Energy

- Interzone Air Transfer Heat Addition & Interzone Air Transfer Heat Removal

[Zone](#zone) Air Heat Balance Interzone Air Transfer Rate

- Infiltration Heat Addition & Infiltration Heat Removal

[Zone](#zone) Air Heat Balance Outdoor Air Transfer Rate

The Opaque Surface Conduction and Other Heat Addition and Opaque Surface Conduction and Other Heat Removal columns are also calculated on an timestep basis as the negative value of the other removal and gain columns so that the total for the timestep sums to zero. These columns are derived strictly from the other columns.

#### Zone Component Load Summary

The [Zone](#zone) Component Loads Summary provides an estimate of the heating and cooling peak loads for each zone broken down into various components. This report may help determine which components of the load have the largest impact for the heating and cooling peak design conditions. When specified, the [Zone](#zone) Component Loads Summary report is created for each zone that is conditioned. The difference between the peak design sensible load and the estimated instant + delayed sensible load (as shown in the *Peak Conditions* subtable) is an indication of how consistent the overall total estimate may be to the computed total peak loads for the zone. When the report is called the zone sizing calculations are repeated twice so this may result in longer simulation times.  The key used to obtain this report is ZoneComponentLoadSummary. Since including this report may increase the simulation time, new key options have been added that will display all reports but the [Zone](#zone) Component Load Summary those keys used are AllSummaryButZoneComponentLoad and AllSummaryAndMonthlyButZoneComponentLoad.

The report has four parts:

- Estimated Cooling Peak Load Components
- Contains the sensible-instant, sensible-delay, sensible-return air, latent, total and %grand total for people, lights, equipment, refrigeration, water use equipment, HVAC equipment loads, power generation equipment, infiltration, zone ventilation, interzone mixing, roof, interzone ceiling, other roof, exterior wall, interzone wall, ground contact wall, other wall, exterior floor, interzone floor, ground contact floor, other floor, fenestration conduction, fenestration solar, opaque door. The values in the sensible-delay column are estimated using a procedure shown in the Engineering Reference.
- Cooling Peak Conditions
- Contains the time of the peak load and the outside dry bulb and wet bulb temperatures as well as the outside humidity ratio for that time. It also shows the zone temperature and relative humidity and humidity ratio for that time.  
- Estimated Heating Peak Load Components
- Contains the sensible-instant, sensible-delay, sensible-return air, latent, total and %grand total for people, lights, equipment, refrigeration, water use equipment, HVAC equipment loads, power generation equipment, infiltration, zone ventilation, interzone mixing, roof, interzone ceiling, other roof, exterior wall, interzone wall, ground contact wall, other wall, exterior floor, interzone floor, ground contact floor, other floor, fenestration conduction, fenestration solar, opaque door. The values in the sensible-day column are estimated using a procedure shown in the Engineering Reference.
- Heating Peak Conditions
- An intermediate calculation in the estimation of the sensible-delayed colum is the decay curves that are shown in the *Radiant to Convective Decay Curve* subtables. A decay curve is created for each surface in the zone.

#### Standard 62.1 Summary

The Standard 62.1 Summary (key: Standard62.1Summary) produces a report that is consistent with many of the outputs needed when doing calculations consistent with ASHRAE Standard 62.1-2010. The report is generated when sizing calculations are specified. The abbreviations used in the report are consistent with the abbreviations used in Appendix A4 of the Standard. The following tables are part of the report:

- System Ventilation Requirements for Cooling containing: Sum of [Zone](#zone) Primary Air Flow - Vpz-sum, System Population – Ps, Sum of [Zone](#zone) Population - Pz-sum, Occupant Diversity – D, Uncorrected Outdoor Air Intake Airflow – Vou, System Primary Airflow – Vps, Average Outdoor Air Fraction – Xs, System Ventilation Efficiency – Ev, Outdoor Air Intake Flow – Vot, Percent Outdoor Air - %OA.
- System Ventilation Requirements for Heating containing: Sum of [Zone](#zone) Primary Air Flow - Vpz-sum, System Population – Ps, Sum of [Zone](#zone) Population - Pz-sum, Occupant Diversity – D, Uncorrected Outdoor Air Intake Airflow – Vou, System Primary Airflow – Vps, Average Outdoor Air Fraction – Xs, System Ventilation Efficiency – Ev, Outdoor Air Intake Flow – Vot, Percent Outdoor Air - %OA.
- [Zone](#zone) Ventilation Parameters containing: AirLoop Name, [People](#people) Outdoor Air Rate – Rp,  [Zone](#zone) Population – Pz, Area Outdoor Air Rate – Ra, [Zone](#zone) Floor Area – Az, Breathing [Zone](#zone) Outdoor Airflow – Vbz, Cooling [Zone](#zone) Air Distribution Effectiveness - Ez-clg, Cooling [Zone](#zone) Outdoor Airflow - Voz-clg, Heating [Zone](#zone) Air Distribution Effectiveness - Ez-htg, Heating [Zone](#zone) Outdoor Airflow - Voz-htg.
- System Ventilation Parameters containing: [People](#people) Outdoor Air Rate – Rp, Sum of [Zone](#zone) Population - Pz-sum, Area Outdoor Air Rate – Ra, Sum of [Zone](#zone) Floor Area - Az-sum, Breathing [Zone](#zone) Outdoor Airflow – Vbz, Cooling [Zone](#zone) Outdoor Airflow - Voz-clg, Heating [Zone](#zone) Outdoor Airflow - Voz-htg.
- [Zone](#zone) Ventilation Calculations for Cooling Design containing: AirLoop Name, Box Type, [Zone](#zone) Primary Airflow – Vpz, [Zone](#zone) Discharge Airflow – Vdz, Minimum [Zone](#zone) Primary Airflow - Vpz-min, [Zone](#zone) Outdoor Airflow Cooling - Voz-clg, Primary Outdoor Air Fraction – Zpz, Primary Air Fraction – Ep, Secondary Recirculation Fraction- Er, Supply Air Fraction- Fa, Mixed Air Fraction – Fb, Outdoor Air Fraction – Fc, [Zone](#zone) Ventilation Efficiency – Evz.
- System Ventilation Calculations for Cooling Design containing: Sum of [Zone](#zone) Primary Airflow - Vpz-sum, System Primary Airflow – Vps, Sum of [Zone](#zone) Discharge Airflow - Vdz-sum, Minimum [Zone](#zone) Primary Airflow - Vpz-min, [Zone](#zone) Outdoor Airflow Cooling - Voz-clg, [Zone](#zone) Ventilation Efficiency - Evz-min.
- [Zone](#zone) Ventilation Calculations for Heating Design containing: AirLoop Name, Box Type, [Zone](#zone) Primary Airflow – Vpz, [Zone](#zone) Discharge Airflow – Vdz, Minimum [Zone](#zone) Primary Airflow - Vpz-min, [Zone](#zone) Outdoor Airflow Cooling - Voz-clg, Primary Outdoor Air Fraction – Zpz, Primary Air Fraction – Ep, Secondary Recirculation Fraction- Er, Supply Air Fraction- Fa, Mixed Air Fraction – Fb, Outdoor Air Fraction – Fc, [Zone](#zone) Ventilation Efficiency – Evz.
- System Ventilation Calculations for Heating Design containing: Sum of [Zone](#zone) Primary Airflow - Vpz-sum, System Primary Airflow – Vps, Sum of [Zone](#zone) Discharge Airflow - Vdz-sum, Minimum [Zone](#zone) Primary Airflow - Vpz-min, [Zone](#zone) Outdoor Airflow Cooling - Voz-clg, [Zone](#zone) Ventilation Efficiency - Evz-min.

#### Energy Meters Summary

The Energy Meters Summary (key: EnergyMeters) (which is a slight misnomer as some meters may not be strictly energy) provides the annual period (runperiod) results for each meter (reference the meter data dictionary file (.mdd) and/or the meter details file (.mtd). The results are broken out by fuel type (resource type) in this report.

### Predefined Monthly Summary Reports

The predefined monthly report options are shown below. The key name of the predefined monthly report is all that is needed to have that report appear in the tabular output file. After each report name below are the output variables and aggregation types used. These cannot be modified when using the predefined reports but if changes are desired, a Output:Table:Monthly can be used instead. The StandardReports.idf file in the DataSets directory includes a Output:Table:Monthly that exactly corresponds to the predefined monthly reports shown below. They can be copied into an IDF file and extended if additional variables are desired. A listing of each available key for predefined monthly summary reports follows with a discrption of the variables included.

#### ZoneCoolingSummaryMonthly

- [Zone](#zone) Air System Sensible Cooling Energy (SumOrAverage)
- [Zone](#zone) Air System Sensible Cooling Rate (Maximum)
- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)
- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)
- [Zone](#zone) Total Internal Latent Gain Energy (SumOrAverage)
- [Zone](#zone) Total Internal Latent Gain Energy (Maximum)
- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)
- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)

#### ZoneHeatingSummaryMonthly

- [Zone](#zone) Air System Sensible Heating Energy (SumOrAverage)
- [Zone](#zone) Air System Sensible Heating Rate (Maximum)
- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)

#### ZoneElectricSummaryMonthly

- [Zone](#zone) [Lights](#lights) Electric Energy (SumOrAverage)
- [Zone](#zone) [Lights](#lights) Electric Energy (Maximum)
- [Zone](#zone) Electric Equipment Electric Energy (SumOrAverage)
- [Zone](#zone) Electric Equipment Electric Energy (Maximum)

#### SpaceGainsMonthly

- [Zone](#zone) [People](#people) Total Heating Energy (SumOrAverage)
- [Zone](#zone) [Lights](#lights) Total Heating Energy (SumOrAverage)
- [Zone](#zone) Electric Equipment Total Heating Energy (SumOrAverage)
- [Zone](#zone) Gas Equipment Total Heating Energy (SumOrAverage)
- [Zone](#zone) Hot Water Equipment Total Heating Energy (SumOrAverage)
- [Zone](#zone) Steam Equipment Total Heating Energy (SumOrAverage)
- [Zone](#zone) Other Equipment Total Heating Energy (SumOrAverage)
- [Zone](#zone) Infiltration Sensible Heat Gain Energy (SumOrAverage)
- [Zone](#zone) Infiltration Sensible Heat Loss Energy (SumOrAverage)

#### PeakSpaceGainsMonthly

- [Zone](#zone) [People](#people) Total Heating Energy (Maximum)
- [Zone](#zone) [Lights](#lights) Total Heating Energy (Maximum)
- [Zone](#zone) Electric Equipment Total Heating Energy (Maximum)
- [Zone](#zone) Gas Equipment Total Heating Energy (Maximum)
- [Zone](#zone) Hot Water Equipment Total Heating Energy (Maximum)
- [Zone](#zone) Steam Equipment Total Heating Energy (Maximum)
- [Zone](#zone) Other Equipment Total Heating Energy (Maximum)
- [Zone](#zone) Infiltration Sensible Heat Gain Energy (Maximum)
- [Zone](#zone) Infiltration Sensible Heat Loss Energy (Maximum)

#### SpaceGainComponentsAtCoolingPeakMonthly

- [Zone](#zone) Air System Sensible Cooling Rate (Maximum)
- [Zone](#zone) [People](#people) Total Heating Energy (ValueWhenMaxMin)
- [Zone](#zone) [Lights](#lights) Total Heating Energy (ValueWhenMaxMin)
- [Zone](#zone) Electric Equipment Total Heating Energy (ValueWhenMaxMin)
- [Zone](#zone) Gas Equipment Total Heating Energy (ValueWhenMaxMin)
- [Zone](#zone) Hot Water Equipment Total Heating Energy (ValueWhenMaxMin)
- [Zone](#zone) Steam Equipment Total Heating Energy (ValueWhenMaxMin)
- [Zone](#zone) Other Equipment Total Heating Energy (ValueWhenMaxMin)
- [Zone](#zone) Infiltration Sensible Heat Gain Energy (ValueWhenMaxMin)
- [Zone](#zone) Infiltration Sensible Heat Loss Energy (ValueWhenMaxMin)

#### EnergyConsumptionElectricityNaturalGasMonthly

- Electricity:Facility (SumOrAverage)
- Electricity:Facility (Maximum)
- Gas:Facility (SumOrAverage)
- Gas:Facility (Maximum)

#### EnergyConsumptionElectricityGeneratedPropaneMonthly

- ElectricityProduced:Facility (SumOrAverage)
- ElectricityProduced:Facility (Maximum)
- Propane:Facility (SumOrAverage)
- Propane:Facility (Maximum)

#### EnergyConsumptionDieselFuel OilMonthly

- Diesel:Facility (SumOrAverage)
- Diesel:Facility (Maximum)
- FuelOil#1:Facility (SumOrAverage)
- FuelOil#1:Facility (Maximum)
- FuelOil#2:Facility (SumOrAverage)
- FuelOil#2:Facility (Maximum)

#### EnergyConsumptionDisctrictHeatingCoolingMonthly

- DistrictCooling:Facility (SumOrAverage)
- DistrictCooling:Facility (Maximum)
- DistrictHeating:Facility (SumOrAverage)
- DistrictHeating:Facility (Maximum)

#### EnergyConsumptionCoalGasolineMonthly

- Coal:Facility (SumOrAverage)
- Coal:Facility (Maximum)
- Gasoline:Facility (SumOrAverage)
- Gasoline:Facility (Maximum)

#### EnergyConsumptionOtherFuelsMonthly

- OtherFuel1:Facility (SumOrAverage)
- OtherFuel1:Facility (Maximum)
- OtherFuel2:Facility (SumOrAverage)
- OtherFuel2:Facility (Maximum)

#### EndUseEnergyConsumptionElectricityMonthly

- InteriorLights:Electricity (SumOrAverage)
- ExteriorLights:Electricity (SumOrAverage)
- InteriorEquipment:Electricity (SumOrAverage)
- ExteriorEquipment:Electricity (SumOrAverage)
- Fans:Electricity (SumOrAverage)
- Pumps:Electricity (SumOrAverage)
- Heating:Electricity (SumOrAverage)
- Cooling:Electricity (SumOrAverage)
- HeatRejection:Electricity (SumOrAverage)
- Humidifier:Electricity (SumOrAverage)
- HeatRecovery:Electricity (SumOrAverage)
- WaterSystems:Electricity (SumOrAverage)
- Cogeneration:Electricity (SumOrAverage)

#### EndUseEnergyConsumptionNaturalGasMonthly

- InteriorEquipment:Gas (SumOrAverage)
- ExteriorEquipment:Gas (SumOrAverage)
- Heating:Gas (SumOrAverage)
- Cooling:Gas (SumOrAverage)
- WaterSystems:Gas (SumOrAverage)
- Cogeneration:Gas (SumOrAverage)

#### EndUseEnergyConsumptionDieselMonthly

- ExteriorEquipment:Diesel (SumOrAverage)
- Cooling:Diesel (SumOrAverage)
- Heating:Diesel (SumOrAverage)
- WaterSystems:Diesel (SumOrAverage)
- Cogeneration:Diesel (SumOrAverage)

#### EndUseEnergyConsumptionFuelOilMonthly

- ExteriorEquipment:FuelOil#1 (SumOrAverage)
- Cooling:FuelOil#1 (SumOrAverage)
- Heating:FuelOil#1 (SumOrAverage)
- WaterSystems:FuelOil#1 (SumOrAverage)
- Cogeneration:FuelOil#1 (SumOrAverage)
- ExteriorEquipment:FuelOil#2 (SumOrAverage)
- Cooling:FuelOil#2 (SumOrAverage)
- Heating:FuelOil#2 (SumOrAverage)
- WaterSystems:FuelOil#2 (SumOrAverage)
- Cogeneration:FuelOil#2 (SumOrAverage)

#### EndUseEnergyConsumptionCoalMonthly

- ExteriorEquipment:Coal (SumOrAverage)
- Heating:Coal (SumOrAverage)
- WaterSystems:Coal (SumOrAverage)

#### EndUseEnergyConsumptionPropaneMonthly

- ExteriorEquipment:Propane (SumOrAverage)
- Cooling:Propane (SumOrAverage)
- Heating:Propane (SumOrAverage)
- WaterSystems:Propane (SumOrAverage)
- Cogeneration:Propane (SumOrAverage)

#### EndUseEnergyConsumptionGasolineMonthly

- ExteriorEquipment:Gasoline (SumOrAverage)
- Cooling:Gasoline (SumOrAverage)
- Heating:Gasoline (SumOrAverage)
- WaterSystems:Gasoline (SumOrAverage)
- Cogeneration:Gasoline (SumOrAverage)

#### EndUseEnergyConsumptionOtherFuelsMonthly

- ExteriorEquipment:OtherFuel1 (SumOrAverage)
- Cooling:OtherFuel1 (SumOrAverage)
- Heating:OtherFuel1 (SumOrAverage)
- WaterSystems:OtherFuel1 (SumOrAverage)
- Cogeneration:OtherFuel1 (SumOrAverage)
- ExteriorEquipment:OtherFuel1 (SumOrAverage)
- Cooling:OtherFuel2 (SumOrAverage)
- Heating:OtherFuel2 (SumOrAverage)
- WaterSystems:OtherFuel2 (SumOrAverage)
- Cogeneration:OtherFuel2 (SumOrAverage)

#### PeakEnergyEndUseElectricityPart1Monthly

- InteriorLights:Electricity (Maximum)
- ExteriorLights:Electricity (Maximum)
- InteriorEquipment:Electricity (Maximum)
- ExteriorEquipment:Electricity (Maximum)
- Fans:Electricity (Maximum)
- Pumps:Electricity (Maximum)
- Heating:Electricity (Maximum)

#### PeakEnergyEndUseElectricityPart2Monthly

- Cooling:Electricity (Maximum)
- HeatRejection:Electricity (Maximum)
- Humidifier:Electricity (Maximum)
- HeatRecovery:Electricity (Maximum)
- WaterSystems:Electricity (Maximum)
- Cogeneration:Electricity (Maximum)

#### ElectricComponentsOfPeakDemandMonthly

- Electricity:Facility (Maximum)
- InteriorLights:Electricity (ValueWhenMaxMin)
- InteriorEquipment:Electricity (ValueWhenMaxMin)
- ExteriorLights:Electricity (ValueWhenMaxMin)
- ExteriorEquipment:Electricity (ValueWhenMaxMin)
- Fans:Electricity (ValueWhenMaxMin)
- Pumps:Electricity (ValueWhenMaxMin)
- Heating:Electricity (ValueWhenMaxMin)
- Cooling:Electricity (ValueWhenMaxMin)
- HeatRejection:Electricity (ValueWhenMaxMin)

#### PeakEnergyEndUseNaturalGasMonthly

- InteriorEquipment:Gas (Maximum)
- ExteriorEquipment:Gas (Maximum)
- Heating:Gas (Maximum)
- Cooling:Gas (Maximum)
- WaterSystems:Gas (Maximum)
- Cogeneration:Gas (Maximum)

#### PeakEnergyEndUseDieselMonthly

- ExteriorEquipment:Diesel (Maximum)
- Cooling:Diesel (Maximum)
- Heating:Diesel (Maximum)
- WaterSystems:Diesel (Maximum)
- Cogeneration:Diesel (Maximum)

#### PeakEnergyEndUseFuelOilMonthly

- ExteriorEquipment:FuelOil#1 (Maximum)
- Cooling:FuelOil#1 (Maximum)
- Heating:FuelOil#1 (Maximum)
- WaterSystems:FuelOil#1 (Maximum)
- Cogeneration:FuelOil#1 (Maximum)
- ExteriorEquipment:FuelOil#2 (Maximum)
- Cooling:FuelOil#2 (Maximum)
- Heating:FuelOil#2 (Maximum)
- WaterSystems:FuelOil#2 (Maximum)
- Cogeneration:FuelOil#2 (Maximum)

#### PeakEnergyEndUseCoalMonthly

- ExteriorEquipment:Coal (Maximum)
- Heating:Coal (Maximum)
- WaterSystems:Coal (Maximum)

#### PeakEnergyEndUsePropaneMonthly

- ExteriorEquipment:Propane (Maximum)
- Cooling:Propane (Maximum)
- Heating:Propane (Maximum)
- WaterSystems:Propane (Maximum)
- Cogeneration:Propane (Maximum)

#### PeakEnergyEndUseGasolineMonthly

- ExteriorEquipment:Gasoline (Maximum)
- Cooling:Gasoline (Maximum)
- Heating:Gasoline (Maximum)
- WaterSystems:Gasoline (Maximum)
- Cogeneration:Gasoline (Maximum)

#### PeakEnergyEndUseOtherFuelsMonthly

- ExteriorEquipment:OtherFuel1 (Maximum)
- Cooling:OtherFuel1 (Maximum)
- Heating:OtherFuel1 (Maximum)
- WaterSystems:OtherFuel1 (Maximum)
- Cogeneration:OtherFuel1 (Maximum)
- ExteriorEquipment:OtherFuel1 (Maximum)
- Cooling:OtherFuel1 (Maximum)
- Heating:OtherFuel1 (Maximum)
- WaterSystems:OtherFuel1 (Maximum)
- Cogeneration:OtherFuel1 (Maximum)

#### SetpointsNotMetWithTemperaturesMonthly

- [Zone](#zone) Heating Setpoint Not Met Time (HoursNonZero)
- [Zone](#zone) Mean Air Temperature (SumOrAverageDuringHoursShown)
- [Zone](#zone) Heating Setpoint Not Met While Occupied Time (HoursNonZero)
- [Zone](#zone) Mean Air Temperature (SumOrAverageDuringHoursShown)
- [Zone](#zone) Cooling Setpoint Not Met Time (HoursNonZero)
- [Zone](#zone) Mean Air Temperature (SumOrAverageDuringHoursShown)
- [Zone](#zone) Cooling Setpoint Not Met While Occupied Time (HoursNonZero)
- [Zone](#zone) Mean Air Temperature (SumOrAverageDuringHoursShown)

#### ComfortReportSimple55Monthly

- [Zone](#zone) Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time (HoursNonZero)
- [Zone](#zone) Mean Air Temperature (SumOrAverageDuringHoursShown)
- [Zone](#zone) Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time (HoursNonZero)
- [Zone](#zone) Mean Air Temperature (SumOrAverageDuringHoursShown)
- [Zone](#zone) Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time (HoursNonZero)
- [Zone](#zone) Mean Air Temperature (SumOrAverageDuringHoursShown)

#### UnglazedTranspiredSolarCollectorSummaryMonthly

- Solar Collector System Efficiency (HoursNonZero)
- Solar Collector System Efficiency (SumOrAverageDuringHoursShown)
- Solar Collector Outside Face Suction Velocity (SumOrAverageDuringHoursShown)
- Solar Collector Sensible Heating Rate (SumOrAverageDuringHoursShown)

#### OccupantComfortDataSummaryMonthly

- [Zone](#zone) [People](#people) Occupant Count (HoursNonZero)
- [Zone](#zone) Air Temperature (SumOrAverageDuringHoursShown)
- [Zone](#zone) Air Relative Humidity (SumOrAverageDuringHoursShown)
- [Zone](#zone) Thermal Comfort Fanger Model PMV (SumOrAverageDuringHoursShown)
- [Zone](#zone) Thermal Comfort Fanger Model PPD (SumOrAverageDuringHoursShown)

#### Chille ReportMonthly

- Chiller Electric Energy (SumOrAverage)
- Chiller Electric Power (Maximum)
- Chiller Electric Energy (HoursNonZero)
- Chiller Evaporator Cooling Energy (SumOrAverage)
- Chiller Condenser Heat Transfer Energy (SumOrAverage)
- Chiller COP (SumOrAverage)
- Chiller COP (Maximum)

#### TowerReportMonthly

- Tower Fan Electric Consumption (SumOrAverage)
- Tower Fan Electric Consumption (HoursNonZero)
- Cooling Tower Fan Electric Power (Maximum)
- Cooling Tower Heat Transfer Rate (Maximum)
- Cooling Tower Inlet Temperature (SumOrAverage)
- Cooling Tower Outlet Temperature (SumOrAverage)
- Cooling Tower Mass Flow Rate (SumOrAverage)

#### BoilerReportMonthly

- Boiler Heating Energy (SumOrAverage)
- Boiler Gas Consumption (SumOrAverage)
- Boiler Heating Energy (HoursNonZero)
- Boiler Heating Rate (Maximum)
- Boiler Gas Consumption Rate (Maximum)
- Boiler Inlet Temperature (SumOrAverage)
- Boiler Outlet Temperature (SumOrAverage)
- Boiler Mass Flow Rate (SumOrAverage)
- Boiler Ancillary Electric Power (SumOrAverage)

#### DXReportMonthly

- Cooling Coil Total Cooling Energy (SumOrAverage)
- Cooling Coil Electric Energy (SumOrAverage)
- Cooling Coil Total Cooling Energy (HoursNonZero)
- Cooling Coil Sensible Cooling Energy (SumOrAverage)
- Cooling Coil Latent Cooling Energy (SumOrAverage)
- Cooling Coil Crankcase Heater Electric Energy (SumOrAverage)
- Cooling Coil Runtime Fraction (Maximum)
- Cooling Coil Runtime Fraction (Minimum)
- DX Coil Total Cooling Rate (Maximum)
- Cooling Coil Sensible Cooling Rate (Maximum)
- Cooling Coil Latent Cooling Rate (Maximum)
- Cooling Coil Electric Power (Maximum)
- Cooling Coil Crankcase Heater Electric Power (Maximum)

#### WindowReportMonthly

- Surface [Window](#window) Transmitted Solar Radiation Rate (SumOrAverage)
- Surface [Window](#window) Transmitted Beam Solar Radiation Rate (SumOrAverage)
- Surface [Window](#window) Transmitted Diffuse Solar Radiation Rate (SumOrAverage)
- Surface [Window](#window) Heat Gain Rate (SumOrAverage)
- Surface [Window](#window) Heat Loss Rate (SumOrAverage)
- Surface [Window](#window) Inside Face Glazing Condensation Status (HoursNonZero)
- Surface Shading Device Is On Time Fraction (HoursNonZero)
- Surface Storm [Window](#window) On Off Status (HoursNonZero)

#### WindowEnergyReportMonthly

- Surface [Window](#window) Transmitted Solar Radiation Energy (SumOrAverage)
- Surface [Window](#window) Transmitted Beam Solar Radiation Rate Energy (SumOrAverage)
- Surface [Window](#window) Transmitted Diffuse Solar Radiation Energy (SumOrAverage)
- Surface [Window](#window) Heat Gain Energy (SumOrAverage)
- Surface [Window](#window) Heat Loss Energy (SumOrAverage)

#### WindowZoneSummaryMonthly

- [Zone](#zone) Windows Total Heat Gain Rate (SumOrAverage)
- [Zone](#zone) Windows Total Heat Loss Rate (SumOrAverage)
- [Zone](#zone) Windows Total Transmitted Solar Radiation Rate (SumOrAverage)
- [Zone](#zone) Exterior Windows Total Transmitted Beam Solar Radiation Rate (SumOrAverage)
- [Zone](#zone) Exterior Windows Total Transmitted Diffuse Solar Radiation Rate (SumOrAverage)
- [Zone](#zone) Interior Windows Total Transmitted Diffuse Solar Radiation Rate (SumOrAverage)
- [Zone](#zone) Interior Windows Total Transmitted Beam Solar Radiation Rate (SumOrAverage)

#### WindowEnergyZoneSummaryMonthly

- [Zone](#zone) Windows Total Heat Gain Energy (SumOrAverage)
- [Zone](#zone) Windows Total Heat Loss Energy (SumOrAverage)
- [Zone](#zone) Windows Total Transmitted Solar Radiation Energy (SumOrAverage)
- [Zone](#zone) Exterior Windows Total Transmitted Beam Solar Radiation Energy (SumOrAverage)
- [Zone](#zone) Exterior Windows Total Transmitted Diffuse Solar Radiation Energy (SumOrAverage)
- [Zone](#zone) Interior Windows Total Transmitted Diffuse Solar Radiation Energy (SumOrAverage)
- [Zone](#zone) Interior Windows Total Transmitted Beam Solar Radiation Energy (SumOrAverage)

#### AverageOutdoorConditionsMonthly

- Site Outdoor Air Drybulb Temperature (SumOrAverage)
- Site Outdoor Air Wetbulb Temperature (SumOrAverage)
- Site Outdoor Air Dewpoint Temperature (SumOrAverage)
- Wind Speed (SumOrAverage)
- Site Sky Temperature (SumOrAverage)
- Site Diffuse Solar Radiation Rate per Area (SumOrAverage)
- Site Direct Solar Radiation Rate per Area (SumOrAverage)
- Raining (SumOrAverage)

#### OutdoorConditionsMaximumDryBulbMonthly

- Site Outdoor Air Drybulb Temperature (Maximum)
- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)
- Site Outdoor Air Dewpoint Temperature (ValueWhenMaxMin)
- Wind Speed (ValueWhenMaxMin)
- Site Sky Temperature (ValueWhenMaxMin)
- Site Diffuse Solar Radiation Rate per Area (ValueWhenMaxMin)
- Site Direct Solar Radiation Rate per Area (ValueWhenMaxMin)

#### OutdoorConditionsMinimumDryBulbMonthly

- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)
- Site Outdoor Air Dewpoint Temperature (ValueWhenMaxMin)
- Wind Speed (ValueWhenMaxMin)
- Site Sky Temperature (ValueWhenMaxMin)
- Site Diffuse Solar Radiation Rate per Area (ValueWhenMaxMin)
- Site Direct Solar Radiation Rate per Area (ValueWhenMaxMin)

#### OutdoorConditionsMaximumWetBulbMonthly

- Site Outdoor Air Wetbulb Temperature (Maximum)
- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)
- Site Outdoor Air Dewpoint Temperature (ValueWhenMaxMin)
- Wind Speed (ValueWhenMaxMin)
- Site Sky Temperature (ValueWhenMaxMin)
- Site Diffuse Solar Radiation Rate per Area (ValueWhenMaxMin)
- Site Direct Solar Radiation Rate per Area (ValueWhenMaxMin)

#### OutdoorConditionsMaximumDewPointMonthly

- Site Outdoor Air Dewpoint Temperature (Maximum)
- Site Outdoor Air Drybulb Temperature (ValueWhenMaxMin)
- Site Outdoor Air Wetbulb Temperature (ValueWhenMaxMin)
- Wind Speed (ValueWhenMaxMin)
- Site Sky Temperature (ValueWhenMaxMin)
- Site Diffuse Solar Radiation Rate per Area (ValueWhenMaxMin)
- Site Direct Solar Radiation Rate per Area (ValueWhenMaxMin)

#### OutdoorGroundConditionsMonthly

- Site Ground Temperature (SumOrAverage)
- Site Surface Ground Temperature (SumOrAverage)
- Site Deep Ground Temperature (SumOrAverage)
- Site Mains Water Temperature (SumOrAverage)
- Site Ground Reflected Solar Radiation Rate per Area (SumOrAverage)
- Snow On Ground (SumOrAverage)

#### WindowACReportMonthly

- [Zone](#zone) [Window](#window) Air Conditioner Total Cooling Energy (SumOrAverage)
- [Zone](#zone) [Window](#window) Air Conditioner Electric Energy (SumOrAverage)
- [Zone](#zone) [Window](#window) Air Conditioner Total Cooling Energy (HoursNonZero)
- [Zone](#zone) [Window](#window) Air Conditioner Sensible Cooling Energy (SumOrAverage)
- [Zone](#zone) [Window](#window) Air Conditioner Latent Cooling Energy (SumOrAverage)
- [Zone](#zone) [Window](#window) Air Conditioner Total Cooling Rate (Maximum)
- [Zone](#zone) [Window](#window) Air Conditioner Sensible Cooling Rate (ValueWhenMaxMin)
- [Zone](#zone) [Window](#window) Air Conditioner Latent Cooling Rate (ValueWhenMaxMin)
- [Zone](#zone) [Window](#window) Air Conditioner Electric Power (ValueWhenMaxMin)

#### WaterHeaterReportMonthly

- Water Heater Total Demand Energy (SumOrAverage)
- Water Heater Use Side Heat Transfer Energy (SumOrAverage)
- Water Heater Burner Heating Energy (SumOrAverage)
- Water Heater Gas Consumption (SumOrAverage)
- Water Heater Total Demand Energy (HoursNonZero)
- Water Heater Loss Demand Energy (SumOrAverage)
- Water Heater Heat Loss Energy (SumOrAverage)
- Water Heater Tank Temperature (SumOrAverage)
- Water Heater Heat Recovery Supply Energy (SumOrAverage)
- Water Heater Source Side Heat Transfer Energy (SumOrAverage)

#### GeneratorReportMonthly

- Generator Produced Electric Energy (SumOrAverage)
- Generator Diesel Energy (SumOrAverage)
- Generator Gas Consumption (SumOrAverage)
- Generator Produced Electric Energy (HoursNonZero)
- Generator Total Heat Recovery (SumOrAverage)
- Generator Jacket Heat Recovery Energy (SumOrAverage)
- Generator Lube Heat Recovery Energy (SumOrAverage)
- Generator Exhaust Heat Recovery Energy (SumOrAverage)
- Generator Exhaust Air Temperature (SumOrAverage)

#### DaylightingReportMonthly

- Site Exterior Beam Normal Illuminance (HoursNonZero)
- Daylighting Lighting Power Multiplier (SumOrAverageDuringHoursShown)
- Daylighting Lighting Power Multiplier (MinimumDuringHoursShown)
- Daylighting Reference Point 1 Illuminance (SumOrAverageDuringHoursShown)
- Daylighting Reference Point 1 Glare Index (SumOrAverageDuringHoursShown)
- Daylighting Reference Point 2 Illuminance (SumOrAverageDuringHoursShown)
- Daylighting Reference Point 2 Glare Index (SumOrAverageDuringHoursShown)
- Daylighting Reference Point 1 Glare Index Setpoint Exceeded Time
- Daylighting Reference Point 2 Glare Index Setpoint Exceeded Time
- Daylighting Reference Point 1 Daylight Illuminance Setpoint Exceeded Time
- Daylighting Reference Point 2 Daylight Illuminance Setpoint Exceeded Time

#### CoilReportMonthly

- Heating Coil Heating Energy (SumOrAverage)
- Heating Coil Heating Rate (Maximum)
- Cooling Coil Total Cooling Energy (SumOrAverage)
- Cooling Coil Sensible Cooling Energy (SumOrAverage)
- Cooling Coil Total Cooling Rate (Maximum)
- Cooling Coil Sensible Cooling Rate (ValueWhenMaxMin)
- Cooling Coil Wetted Area Fraction (SumOrAverage)

#### PlantLoopDemandReportMonthly

- Plant Supply Side Cooling Demand Rate (SumOrAverage)
- Plant Supply Side Cooling Demand Rate (Maximum)
- Plant Supply Side Heating Demand Rate (SumOrAverage)
- Plant Supply Side Heating Demand Rate (Maximum)

#### FanReportMonthly

- Fan Electric Consumption (SumOrAverage)
- Fan Rise in Air Temperature (SumOrAverage)
- Fan Electric Power (Maximum)
- Fan Rise in Air Temperature (ValueWhenMaxMin)

#### PumpReportMonthly

- Pump Electric Energy (SumOrAverage)
- Pump Fluid Heat Gain Energy (SumOrAverage)
- Pump Electric Power (Maximum)
- Pump Shaft Power (ValueWhenMaxMin)
- Pump Fluid Heat Gain Rate (ValueWhenMaxMin)
- Pump Outlet Temperature (ValueWhenMaxMin)
- Pump Mass Flow Rate (ValueWhenMaxMin)

#### CondLoopDemandReportMonthly

- Plant Supply Side Cooling Demand Rate (SumOrAverage)
- Plant Supply Side Cooling Demand Rate (Maximum)
- Plant Supply Side Inlet Temperature (ValueWhenMaxMin)
- Plant Supply Side Outlet Temperature (ValueWhenMaxMin)
- Plant Supply Side Heating Demand Rate (SumOrAverage)
- Plant Supply Side Heating Demand Rate (Maximum)

#### ZoneTemperatureOscillationReportMonthly

- [Zone](#zone) Oscillating Temperatures Time (HoursNonZero)
- [Zone](#zone) [People](#people) Occupant Count (SumOrAverageDuringHoursShown)

#### AirLoopSystemEnergyAndWaterUseMonthly

- Air System Hot Water Energy (SumOrAverage)
- Air System Steam Energy (SumOrAverage)
- Air System Chilled Water Energy (SumOrAverage)
- Air System Electric Energy (SumOrAverage)
- Air System Gas Energy (SumOrAverage)
- Air System Water Volume (SumOrAverage)

#### AirLoopSystemComponentLoadsMonthly

- Air System Fan Air Heating Energy (SumOrAverage)
- Air System Cooling Coil Total Cooling Energy (SumOrAverage)
- Air System Heating Coil Total Heating Energy (SumOrAverage)
- Air System Heat Exchanger Total Heating Energy (SumOrAverage)
- Air System Heat Exchanger Total Cooling Energy (SumOrAverage)
- Air System Humidifier Total Heating Energy (SumOrAverage)
- Air System Evaporative Cooler Total Cooling Energy (SumOrAverage)
- Air System Desiccant Dehumidifier Total Cooling Energy (SumOrAverage)

#### AirLoopSystemComponentEnergyUseMonthly

- Air System Fan Electric Energy (SumOrAverage)
- Air System Heating Coil Hot Water Energy (SumOrAverage)
- Air System Cooling Coil Chilled Water Energy (SumOrAverage)
- Air System DX Heating Coil Electric Energy (SumOrAverage)
- Air System DX Cooling Coil Electric Energy (SumOrAverage)
- Air System Heating Coil Electric Energy (SumOrAverage)
- Air System Heating Coil Gas Energy (SumOrAverage)
- Air System Heating Coil Steam Energy (SumOrAverage)
- Air System Humidifier Electric Energy (SumOrAverage)
- Air System Evaporative Cooler Electric Energy (SumOrAverage)
- Air System Desiccant Dehumidifier Electric Energy (SumOrAverage)

#### MechanicalVentilationLoadsMonthly

- [Zone](#zone) Mechanical Ventilation No Load Heat Removal Energy (SumOrAverage)
- [Zone](#zone) Mechanical Ventilation Cooling Load Increase Energy (SumOrAverage)
- [Zone](#zone) Mechanical Ventilation Cooling Load Increase Due to Overheating Energy (SumOrAverage)
- [Zone](#zone) Mechanical Ventilation Cooling Load Decrease Energy (SumOrAverage)
- [Zone](#zone) Mechanical Ventilation No Load Heat Addition Energy (SumOrAverage)
- [Zone](#zone) Mechanical Ventilation Heating Load Increase Energy (SumOrAverage)
- [Zone](#zone) Mechanical Ventilation Heating Load Increase Due to Overcooling Energy (SumOrAverage)
- [Zone](#zone) Mechanical Ventilation Heating Load Decrease Energy (SumOrAverage)
- [Zone](#zone) Mechanical Ventilation Air Changes per Hour (SumOrAverage)
    -

Sample IDF Input – Output:Table:SummaryReports

~~~~~~~~~~~~~~~~~~~~

    Output:Table:SummaryReports,
            AllSummary,  !- Report 1 Name
            AllMonthly;  !- Report 2 Name
~~~~~~~~~~~~~~~~~~~~