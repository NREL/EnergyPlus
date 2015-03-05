# eplustbl.<ext>

The eplustbl file contains the tabular output results that are created when using the following objects:

- Output:Table:SummaryReports
- Output:Table:TimeBins
- Output:Table:Monthly
- UtilityCost:Tariff
- ComponentCost:Line Item

The format and the extension for the file depends on the setting of the ColumnSeparator field of the Output:Table:Style object. The choices of HTML, tab, fixed, comma, and XML result in eplustbl.htm, eplustbl.tab, eplustbl.txt, eplustbl.csv, eplustbl.xml respectively. The HTML version of the report also includes a table of contents that allows easier navigation through the file.

By default the energy units reported in all of the eplustbl files are in Joules (J) but the UnitConversion field of the Output:Table:Style object allows for the values to be reported in MJ, GJ or in kWh. In addition, the Output:Table:Style object can specify for the tables to be in IP units for all fields.

## Output:Table:SummaryReports

Several predefined reports are available from the Output:Table:SummaryReports object including the following. (spaces are inserted in names for readability; keys are included in the Input Data Dictionary or by just removing the spaces):

- All Summary

- Lists all following applicable tables with "Summary" in the name.

- All Monthly

- Lists all following applicable tables with "Monthly" in the name.

- All Summary And Monthly

- Lists all following applicable tables with both "Summary" and "Monthly" in the name. This does not include the Zone Component Load Summary report.

- All Summary and Sizing Period
- Lists the All Summary tables as well as the Zone Component Load report (currently the only Sizing Period report). Please note that the Zone Component Load report does increase the run time because it repeats sizing periods.
- All Summary Monthly and Sizing Period
- Lists the All Summary tables, the Monthly tables as well as the Zone Component Load report (currently the only Sizing Period report). Please note that the Zone Component Load report does increase the run time because it repeats sizing periods.
- Annual Building Utility Performance Summary
- Input Verification and Results Summary
- Demand End Use Components Summary
- Source Energy End Use Components Summary
- Climatic Data Summary
- Equipment Summary
- Envelope Summary
- Surface Shadowing Summary
- Shading Summary
- Lighting Summary
- HVAC Sizing Summary
- System Summary
- Component Sizing Summary
- Outdoor Air Summary
- Object Count Summary
- Component Cost Economics Summary
- Energy Meters
- Sensible Heat Gain Summary
- Standard 62.1 Summary
- Zone Component Load Summary
- Zone Cooling Summary Monthly
- Zone Heating Summary Monthly
- Zone Electric Summary Monthly
- Space Gains Monthly
- Peak Space Gains Monthly
- Space Gain Components At Cooling Peak Monthly
- Energy Consumption Electricity Natural Gas Monthly
- Energy Consumption Electricity Generated Propane Monthly
- Energy Consumption Diesel FuelOil Monthly
- Energy Consumption District Heating Cooling Monthly
- Energy Consumption Coal Gasoline Monthly
- End Use Energy Consumption Electricity Monthly
- End Use Energy Consumption NaturalGas Monthly
- End Use Energy Consumption Diesel Monthly
- End Use Energy Consumption FuelOil Monthly
- End Use Energy Consumption Coal Monthly
- End Use Energy Consumption Propane Monthly
- End Use Energy Consumption Gasoline Monthly
- Peak Energy End Use Electricity Part1 Monthly
- Peak Energy End Use Electricity Part2 Monthly
- Electric Components Of Peak Demand Monthly
- Peak Energy End Use NaturalGas Monthly
- Peak Energy End Use Diesel Monthly
- Peak Energy End Use FuelOil Monthly
- Peak Energy End Use Coal Monthly
- Peak Energy End Use Propane Monthly
- Peak Energy End Use Gasoline Monthly
- Setpoints Not Met With Temperatures Monthly
- Comfort Report Simple55 Monthly
- Unglazed Transpired Solar Collector Summary Monthly
- Occupant Comfort Data Summary Monthly
- Chiller Report Monthly
- Tower Report Monthly
- Boiler Report Monthly
- DX Report Monthly
- Window Report Monthly
- Window Energy Report Monthly
- Window Zone Summary Monthly
- Window Energy Zone Summary Monthly
- Average Outdoor Conditions Monthly
- Outdoor Conditions Maximum DryBulb Monthly
- Outdoor Conditions Minimum DryBulb Monthly
- Outdoor Conditions Maximum WetBulb Monthly
- Outdoor Conditions Maximum DewPoint Monthly
- Outdoor Ground Conditions Monthly
- WindowAC Report Monthly
- Water Heater Report Monthly
- Generator Report Monthly
- Daylighting Report Monthly
- Coil Report Monthly
- PlantLoop Demand Report Monthly
- Fan Report Monthly
- Pump Report Monthly
- CondLoop Demand Report Monthly
- Zone Temperature Oscillation Report Monthly
- AirLoop System Energy And Water Use Monthly
- AirLoop System Component Loads Monthly
- AirLoop System Component Energy Use Monthly
- Mechanical Ventilation Loads Monthly

Each of these reports is made up of several sub-tables of information. Examples of some of the tables are shown below. To enable all of the reports the single All Summary may be specified.

## Annual Building Utility Performance Summary

The Annual Building Utility Performance Summary report provides an overview of energy consumption in the building for different end uses. The following is an example this report (some columns may be truncated due to page size). The key used to obtain this report is AnnualBuildingUtilityPerformanceSummary.

In the *Comfort and Setpoint Not Met Summary* sub-table, facility hours represents the total number of hours that any single zone did not meet the comfort or setpoint criteria. It is not weighted by number of zones or number of occupants.

The values in the *End Uses* sub-table are from report meters. To determine which components are attached to each end-use meter, consult the meter details output file (\*.mtd).

Report: Annual Building Utility Performance Summary

For: Entire Facility

Timestamp: 2009-02-10 12:39:35

Values gathered over 8760.00 hours

Site and Source Energy

|Total Energy (GJ)|Energy Per Total Building Area (MJ/m2)|Energy Per Conditioned Building Area (MJ/m2)
|-----------------|--------------------------------------|--------------------------------------------
Total Site Energy|194.80|210.09|210.09
Net Site Energy|194.80|210.09|210.09
Total Source Energy|532.25|574.04|574.04
Net Source Energy|532.25|574.04|574.04

Source to Site Energy Converstion Factors

|Source=>Site Conversion Factor
|------------------------------
Electricity|3.167
Natural Gas|1.084
District Cooling|1.056
District Heating|3.613
Steam|0.300
Gasoline|1.050
Diesel|1.050
Coal|1.050
Fuel Oil #1|1.050
Fuel Oil #2|1.050
Propane|1.050

Building Area

|Area (m2)
|---------
Total Building Area|927.20
Net Conditioned Building Area|927.20
Unconditioned Building Area|0.00

End Uses

|Electricity (GJ)|Natural Gas (GJ)|Other Fuel (GJ)|District Cooling (GJ)|District Heating (GJ)|Water (m3)
|----------------|----------------|---------------|---------------------|---------------------|----------
Heating|0.00|40.65|0.00|0.00|0.00|0.00
Cooling|18.18|0.00|0.00|0.00|0.00|0.00
Interior Lighting|81.24|0.00|0.00|0.00|0.00|0.00
Exterior Lighting|0.00|0.00|0.00|0.00|0.00|0.00
Interior Equipment|47.70|0.00|0.00|0.00|0.00|0.00
Exterior Equipment|0.00|0.00|0.00|0.00|0.00|0.00
Fans|7.03|0.00|0.00|0.00|0.00|0.00
Pumps|0.00|0.00|0.00|0.00|0.00|0.00
Heat Rejection|0.00|0.00|0.00|0.00|0.00|0.00
Humidification|0.00|0.00|0.00|0.00|0.00|0.00
Heat Recovery|0.00|0.00|0.00|0.00|0.00|0.00
Water Systems|0.00|0.00|0.00|0.00|0.00|0.00
Refrigeration|0.00|0.00|0.00|0.00|0.00|0.00
Generators|0.00|0.00|0.00|0.00|0.00|0.00
 | | | | | | 
Total End Uses|154.15|40.65|0.00|0.00|0.00|0.00

*Note: Natural gas appears to be the principal heating source based on energy usage.*

End Uses By Subcategory

|Subcategory|Electricity (GJ)|Natural Gas (GJ)|Other Fuel (GJ)|District Cooling (GJ)|District Heating (GJ)|Water (m3)
|-----------|----------------|----------------|---------------|---------------------|---------------------|----------
Heating|General|0.00|40.65|0.00|0.00|0.00|0.00
Cooling|General|18.18|0.00|0.00|0.00|0.00|0.00
Interior Lighting|GeneralLights|81.24|0.00|0.00|0.00|0.00|0.00
Exterior Lighting|General|0.00|0.00|0.00|0.00|0.00|0.00
Interior Equipment|General|47.70|0.00|0.00|0.00|0.00|0.00
Exterior Equipment|General|0.00|0.00|0.00|0.00|0.00|0.00
Fans|General|7.03|0.00|0.00|0.00|0.00|0.00
Pumps|General|0.00|0.00|0.00|0.00|0.00|0.00
Heat Rejection|General|0.00|0.00|0.00|0.00|0.00|0.00
Humidification|General|0.00|0.00|0.00|0.00|0.00|0.00
Heat Recovery|General|0.00|0.00|0.00|0.00|0.00|0.00
Water Systems|General|0.00|0.00|0.00|0.00|0.00|0.00
Refrigeration|General|0.00|0.00|0.00|0.00|0.00|0.00
Generators|General|0.00|0.00|0.00|0.00|0.00|0.00

Normalized Metrics

Utility Use Per Conditioned Floor Area

|Electricity Intensity (MJ/m2)|Natural Gas Intensity (MJ/m2)|Other Fuel Intensity (MJ/m2)|District Cooling Intensity (MJ/m2)|District Heating Intensity (MJ/m2)|Water Intensity (m3/m2)
|-----------------------------|-----------------------------|----------------------------|----------------------------------|----------------------------------|-----------------------
Lighting|87.62|0.00|0.00|0.00|0.00|0.00
HVAC|27.19|43.84|0.00|0.00|0.00|0.00
Other|51.44|0.00|0.00|0.00|0.00|0.00
Total|166.25|43.84|0.00|0.00|0.00|0.00

Utility Use Per Total Floor Area

|Electricity Intensity (MJ/m2)|Natural Gas Intensity (MJ/m2)|Other Fuel Intensity (MJ/m2)|District Cooling Intensity (MJ/m2)|District Heating Intensity (MJ/m2)|Water Intensity (m3/m2)
|-----------------------------|-----------------------------|----------------------------|----------------------------------|----------------------------------|-----------------------
Lighting|87.62|0.00|0.00|0.00|0.00|0.00
HVAC|27.19|43.84|0.00|0.00|0.00|0.00
Other|51.44|0.00|0.00|0.00|0.00|0.00
Total|166.25|43.84|0.00|0.00|0.00|0.00

Electric Loads Satisfied

|Electricity (GJ)|Percent Electricity (%)
|----------------|-----------------------
Fuel-Fired Power Generation|0.00|0.00
High Temperature Geothermal\*|0.00|0.00
Photovoltaic Power|0.00|0.00
Wind Power\*|0.00|0.00
Net Decrease in On-Site Storage|0.00|0.00
Total On-Site Electric Sources|0.00|0.00
 | | 
Electricity Coming From Utility|154.15|100.00
Surplus Electricity Going To Utility|0.00|0.00
Net Electricity From Utility|154.15|100.00
 | | 
Total On-Site and Utility Electric Sources|154.15|100.00
Total Electricity End Uses|154.15|100.00

On-Site Thermal Sources

|Heat (GJ)|Percent Heat (%)
|---------|----------------
Water-Side Heat Recovery|0.00| 
Air to Air Heat Recovery for Cooling|0.00| 
Air to Air Heat Recovery for Heating|0.00| 
High-Temperature Geothermal\*|0.00| 
Solar Water Thermal|0.00| 
Solar Air Thermal|0.00| 
Total On-Site Thermal Sources|0.00| 

Water Source Summary

|Water (m3)|Percent Water (%)
|----------|-----------------
Rainwater Collection|0.00|-
Condensate Collection|0.00|-
Groundwater Well|0.00|-
Total On Site Water Sources|0.00|-
-|-|-
Initial Storage|0.00|-
Final Storage|0.00|-
Change in Storage|0.00|-
-|-|-
Water Supplied by Utility|0.00|-
-|-|-
Total On Site, Change in Storage, and Utility Water Sources|0.00|-
Total Water End Uses|0.00|-

Comfort and Setpoint Not Met Summary

|Facility (Hours)
|----------------
Time Set Point Not Met During Occupied Heating|0.00
Time Set Point Not Met During Occupied Cooling|406.00
Time Not Comfortable Based on Simple ASHRAE 55-2004|1033.00

Note 1: An asterisk (\*) indicates that the feature is not yet implemented.

## Input Verification and Results Summary

The Input Verification and Results Summary report provides a summary of some of the most common input assumptions that are not included in any of the other predefined reports. Directly following is an example of the report. The key used to obtain this report is InputVerificationandResultsSummary.

Report: Input Verification and Results Summary

For: Entire Facility

Timestamp: 2009-02-10 12:39:35

General

|Value
|-----
Program Version and Build|EnergyPlus, Version 3.1
Weather|Chicago IL United States TMY2 94846 WMO#=725340
Latitude (deg)|41.78
Longitude (deg)|-87.8
Elevation (m)|190.00
Time Zone|-6.0
North Axis Angle (deg)|30.00
Hours Simulated (hrs)|8760.00

ENVELOPE

Window-Wall Ratio

|Total|North (315 to 45 deg)|East (45 to 135 deg)|South (135 to 225 deg)|West (225 to 315 deg)
|-----|---------------------|--------------------|----------------------|---------------------
Gross Wall Area (m2)|274.20|91.50|45.60|91.50|45.60
Window Opening Area (m2)|61.65|20.85|9.12|22.56|9.12
Window-Wall Ratio (%)|22.48|22.79|20.00|24.66|20.00

Skylight-Roof Ratio

|Total
|-----
Gross Roof Area (m2)|463.60
Skylight Area (m2)|0.00
Skylight-Roof Ratio (%)|0.00

PERFORMANCE

Zone Summary

|Area (m2)|Conditioned (Y/N)|Volume (m3)|Multipliers|Gross Wall Area (m2)|Window Glass Area (m2)|Lighting (W/m2)|People (m2/person)|Plug and Process(W/m2)
|---------|-----------------|-----------|-----------|--------------------|----------------------|---------------|------------------|----------------------
PLENUM-1|463.60|Yes|283.20|1.00|54.84|0.00|0.0000| |0.0000
SPACE1-1|99.16|Yes|239.25|1.00|73.20|22.56|15.9742|9.01|10.6495
SPACE2-1|42.73|Yes|103.31|1.00|36.48|9.12|16.0056|8.55|10.6704
SPACE3-1|96.48|Yes|239.25|1.00|73.20|20.85|16.4179|8.77|10.9453
SPACE4-1|42.73|Yes|103.31|1.00|36.48|9.12|16.0056|8.55|10.6704
SPACE5-1|182.49|Yes|447.68|1.00|0.00|0.00|16.2420|9.12|10.8280
Total|927.20| |1416.00| |274.20|61.65|8.0889|17.83|5.3926
Conditioned Total|927.20| |1416.00| |274.20|61.65|8.0889|17.83|5.3926
Unconditioned Total|0.00| |0.00| |0.00|0.00| | | 

## Demand End Use Components Summary

The Demand End Use Components Summary report shows the demand breakdown by component end use at the time that the peak demand for each source of energy is set. The time of the peak demand is shown in the first row. The contributions of each end use at the time of the peak demand of the energy source is shown in this report. The key used to obtain this report is DemandEndUseComponentsSummary.

Report: Demand End Use Components Summary

For: Entire Facility

Timestamp: 2009-02-10 12:39:35

End Uses

|Electricity (W)|Natural Gas (W)|Propane (W)|District Cooling (W)|Steam (W)|Water (m3/s)
|---------------|---------------|-----------|--------------------|---------|------------
Time of Peak|02-JUL-14:00|31-DEC-07:15|-|-|-|-
Heating|0.00|81764.44|0.00|0.00|0.00|0.00
Cooling|12342.62|0.00|0.00|0.00|0.00|0.00
Interior Lighting|7500.00|0.00|0.00|0.00|0.00|0.00
Exterior Lighting|0.00|0.00|0.00|0.00|0.00|0.00
Interior Equipment|4000.00|0.00|0.00|0.00|0.00|0.00
Exterior Equipment|0.00|0.00|0.00|0.00|0.00|0.00
Fans|1498.36|0.00|0.00|0.00|0.00|0.00
Pumps|0.00|0.00|0.00|0.00|0.00|0.00
Heat Rejection|0.00|0.00|0.00|0.00|0.00|0.00
Humidification|0.00|0.00|0.00|0.00|0.00|0.00
Heat Recovery|0.00|0.00|0.00|0.00|0.00|0.00
Water Systems|0.00|0.00|0.00|0.00|0.00|0.00
Refrigeration|0.00|0.00|0.00|0.00|0.00|0.00
Generators|0.00|0.00|0.00|0.00|0.00|0.00
 | | | | | | 
Total End Uses|25340.97|81764.44|0.00|0.00|0.00|0.00

End Uses By Subcategory

|Subcategory|Electricity (W)|Natural Gas (W)|Propane (W)|District Cooling (W)|Steam (W)|Water (m3/s)
|-----------|---------------|---------------|-----------|--------------------|---------|------------
Heating|General|0.00|81764.44|0.00|0.00|0.00|0.00
Cooling|General|12342.62|0.00|0.00|0.00|0.00|0.00
Interior Lighting|GeneralLights|7500.00|0.00|0.00|0.00|0.00|0.00
Exterior Lighting|General|0.00|0.00|0.00|0.00|0.00|0.00
Interior Equipment|General|4000.00|0.00|0.00|0.00|0.00|0.00
Exterior Equipment|General|0.00|0.00|0.00|0.00|0.00|0.00
Fans|General|1498.36|0.00|0.00|0.00|0.00|0.00
Pumps|General|0.00|0.00|0.00|0.00|0.00|0.00
Heat Rejection|General|0.00|0.00|0.00|0.00|0.00|0.00
Humidification|General|0.00|0.00|0.00|0.00|0.00|0.00
Heat Recovery|General|0.00|0.00|0.00|0.00|0.00|0.00
Water Systems|General|0.00|0.00|0.00|0.00|0.00|0.00
Refrigeration|General|0.00|0.00|0.00|0.00|0.00|0.00
Generators|General|0.00|0.00|0.00|0.00|0.00|0.00

## Source Energy End Use Components Summary

The Source Energy End Use Components Summary report produces a report that includes three tables. These tables display source energy by fuel type that is calculated based on site to source energy factors specified by the user in the EnvironmentalImpactFactors and FuelFactors objects. The last two tables display the source energy in terms of area normalized metrics. Directly following is an example of the three tables. The key used to obtain this report is SourceEnergyEndUseComponentsSummary.

Report: **Source Energy End Use Components Summary**

For: **Entire Facility**

Timestamp: **2011-10-07 20:53:43**

**Values gathered over 8760.00 hoursSource Energy End Use Components Summary**

|Source Electricity [GJ]|Source Natural Gas [GJ]|Source Other Fuel [GJ]|Source District Cooling [GJ]|Source District Heating [GJ]
|-----------------------|-----------------------|----------------------|----------------------------|----------------------------
Heating|0.00|140.03|0.00|0.00|0.00
Cooling|167.65|0.00|0.00|0.00|0.00
Interior Lighting|569.39|0.00|0.00|0.00|0.00
Exterior Lighting|0.00|0.00|0.00|0.00|0.00
Interior Equipment|325.49|0.00|0.00|0.00|0.00
Exterior Equipment|0.00|0.00|0.00|0.00|0.00
Fans|55.72|0.00|0.00|0.00|0.00
Pumps|7.57|0.00|0.00|0.00|0.00
Heat Rejection|0.00|0.00|0.00|0.00|0.00
Humidification|0.00|0.00|0.00|0.00|0.00
Heat Recovery|0.00|0.00|0.00|0.00|0.00
Water Systems|0.00|0.00|0.00|0.00|0.00
Refrigeration|0.00|0.00|0.00|0.00|0.00
Generators|0.00|0.00|0.00|0.00|0.00
|||||
Total Source Energy End Use Components|1125.82|140.03|0.00|0.00|0.00

**Normalized MetricsSource Energy End Use Components Per Conditioned Floor Area**

|Source Electricity [MJ/m2]|Source Natural Gas [MJ/m2]|Source Other Fuel [MJ/m2]|Source District Cooling [MJ/m2]|Source District Heating [MJ/m2]
|--------------------------|--------------------------|-------------------------|-------------------------------|-------------------------------
Heating|0.00|151.02|0.00|0.00|0.00
Cooling|180.82|0.00|0.00|0.00|0.00
Interior Lighting|614.09|0.00|0.00|0.00|0.00
Exterior Lighting|0.00|0.00|0.00|0.00|0.00
Interior Equipment|351.05|0.00|0.00|0.00|0.00
Exterior Equipment|0.00|0.00|0.00|0.00|0.00
Fans|60.10|0.00|0.00|0.00|0.00
Pumps|8.16|0.00|0.00|0.00|0.00
Heat Rejection|0.00|0.00|0.00|0.00|0.00
Humidification|0.00|0.00|0.00|0.00|0.00
Heat Recovery|0.00|0.00|0.00|0.00|0.00
Water Systems|0.00|0.00|0.00|0.00|0.00
Refrigeration|0.00|0.00|0.00|0.00|0.00
Generators|0.00|0.00|0.00|0.00|0.00
|||||
Total Source Energy End Use Components|1214.22|151.02|0.00|0.00|0.00

**Source Energy End Use Components Per Total Floor Area**

|Source Electricity [MJ/m2]|Source Natural Gas [MJ/m2]|Source Other Fuel [MJ/m2]|Source District Cooling [MJ/m2]|Source District Heating [MJ/m2]
|--------------------------|--------------------------|-------------------------|-------------------------------|-------------------------------
Heating|0.00|151.02|0.00|0.00|0.00
Cooling|180.82|0.00|0.00|0.00|0.00
Interior Lighting|614.09|0.00|0.00|0.00|0.00
Exterior Lighting|0.00|0.00|0.00|0.00|0.00
Interior Equipment|351.05|0.00|0.00|0.00|0.00
Exterior Equipment|0.00|0.00|0.00|0.00|0.00
Fans|60.10|0.00|0.00|0.00|0.00
Pumps|8.16|0.00|0.00|0.00|0.00
Heat Rejection|0.00|0.00|0.00|0.00|0.00
Humidification|0.00|0.00|0.00|0.00|0.00
Heat Recovery|0.00|0.00|0.00|0.00|0.00
Water Systems|0.00|0.00|0.00|0.00|0.00
Refrigeration|0.00|0.00|0.00|0.00|0.00
Generators|0.00|0.00|0.00|0.00|0.00
|||||
Total Source Energy End Use Components|1214.22|151.02|0.00|0.00|0.00

## Equipment Summary

The Equipment Summary report provides a summary of the HVAC related equipment in the building. The central chiller example shows the most common type of chillers, boilers and towers used in EnergyPlus. The remaining subtables cover DX coils, fans and water heating equipment. Not every type of HVAC equipment is represented in this report. Directly following is an example of the report. The key used to obtain this report is EquipmentSummary.

Report: Equipment Summary

For: Entire Facility

Timestamp: 2009-02-10 12:39:35

Central Plant

|Type|Nominal Capacity (W)|Nominal Efficiency (W/W)
|----|--------------------|------------------------
None| | | 

Cooling Coils

|Type|Nominal Total Capacity (W)|Nominal Sensible Capacity (W)|Nominal Latent Capacity (W)|Nominal Sensible Heat Ratio|Nominal Efficiency (W/W)
|----|--------------------------|-----------------------------|---------------------------|---------------------------|------------------------
MAIN COOLING COIL 1|Coil:Cooling:DX:TwoSpeed|37506.82|25504.64|12002.18|0.68|3.00

Heating Coils

|Type|Nominal Total Capacity (W)|Nominal Efficiency (W/W)
|----|--------------------------|------------------------
SPACE1-1 ZONE COIL|Coil:Heating:Gas|17614.83|0.80
SPACE2-1 ZONE COIL|Coil:Heating:Gas|14619.82|0.80
SPACE3-1 ZONE COIL|Coil:Heating:Gas|16093.74|0.80
SPACE4-1 ZONE COIL|Coil:Heating:Gas|18942.35|0.80
SPACE5-1 ZONE COIL|Coil:Heating:Gas|19146.73|0.80
MAIN HEATING COIL 1|Coil:Heating:Gas|19754.61|0.80

Fans

|Type|Total Efficiency (W/W)|Delta Pressure (pa)|Max Flow Rate (m3/s)|Rated Power (W)|Motor Heat In Air Fraction|End Use
|----|----------------------|-------------------|--------------------|---------------|--------------------------|-------
SUPPLY FAN 1|Fan:VariableVolume|0.70|600.00|2.27|1942.10|1.00|General

Pumps

|Type|Control|Head (pa)|Power (W)|Motor Efficiency (W/W)
|----|-------|---------|---------|----------------------
None| | | | | 

Service Water Heating

|Type|Storage Volume (m3)|Input(W)|Thermal Efficiency (W/W)|Recovery Efficiency (W/W)|Energy Factor
|----|-------------------|--------|------------------------|-------------------------|-------------
None| | | | | | 

## Envelope Summary

The Envelope Summary report provides a summary of the elements of the envelope of the building. The first table describes the exterior opaque elements and the second table describes the fenestration elements. Reflectance is defined as one minus the thermal absorptance. Directly following is an example of the report. The key used to obtain this report is EnvelopeSummary.

Report: Envelope Summary

For: Entire Facility

Timestamp: 2009-02-10 12:39:35

Opaque Exterior

|Construction|Reflectance|U-Factor with Film (W/m2-K)|U-Factor no Film (W/m2-K)|Gross Area (m2)|Azimuth (deg)|Tilt (deg)|Cardinal Direction
|------------|-----------|---------------------------|-------------------------|---------------|-------------|----------|------------------
WALL-1PF|WALL-1|0.22|0.384|0.41|18.30|210.00|90.00|S
WALL-1PR|WALL-1|0.22|0.384|0.41|9.12|120.00|90.00|E
WALL-1PB|WALL-1|0.22|0.384|0.41|18.30|30.00|90.00|N
WALL-1PL|WALL-1|0.22|0.384|0.41|9.12|300.00|90.00|W
TOP-1|ROOF-1|0.35|0.268|0.28|463.60|210.00|0.00| 
FRONT-1|WALL-1|0.22|0.384|0.41|73.20|210.00|90.00|S
F1-1|FLOOR-SLAB-1|0.35|1.454|2.25|99.16|30.00|180.00| 
RIGHT-1|WALL-1|0.22|0.384|0.41|36.48|120.00|90.00|E
F2-1|FLOOR-SLAB-1|0.35|1.454|2.25|42.73|300.00|180.00| 
BACK-1|WALL-1|0.22|0.384|0.41|73.20|30.00|90.00|N
F3-1|FLOOR-SLAB-1|0.35|1.454|2.25|96.48|74.22|180.00| 
LEFT-1|WALL-1|0.22|0.384|0.41|36.48|300.00|90.00|W
F4-1|FLOOR-SLAB-1|0.35|1.454|2.25|42.73|120.00|180.00| 
F5-1|FLOOR-SLAB-1|0.35|1.454|2.25|182.49|30.00|180.00| 

Fenestration

|Construction|Area of One Opening (m2)|Area of Openings (m2)|U-Factor|SHGC|Visible Transmittance|Shade Control|Parent Surface|Azimuth (deg)|Cardinal Direction
|------------|------------------------|---------------------|--------|----|---------------------|-------------|--------------|-------------|------------------
WF-1|DBL CLR 3MM/13MM AIR|16.56|16.56|2.72|0.763|0.812|No|FRONT-1|210.00|S
DF-1|SGL GREY 3MM|6.00|6.00|5.89|0.708|0.611|No|FRONT-1|210.00|S
WR-1|DBL CLR 3MM/13MM AIR|9.12|9.12|2.72|0.763|0.812|No|RIGHT-1|120.00|E
WB-1|DBL CLR 3MM/13MM AIR|16.44|16.44|2.72|0.763|0.812|No|BACK-1|30.00|N
DB-1|SGL GREY 3MM|4.41|4.41|5.89|0.708|0.611|No|BACK-1|30.00|N
WL-1|DBL CLR 3MM/13MM AIR|9.12|9.12|2.72|0.763|0.812|No|LEFT-1|300.00|W
Total or Average| | |61.65|3.26|0.753|0.778| | | | 
North Total or Average| | |20.85|3.39|0.751|0.769| | | | 
Non-North Total or Average| | |40.80|3.19|0.755|0.782| | | | 

## Surface Shadowing Summary

The Surface Shadowing Summary report summarizes how the surfaces may cast shadows on other surfaces. Directly following is an example of the report. The key used to obtain this report is SurfaceShadowingSummary.

Report: Surface Shadowing Summary

For: Entire Facility

Timestamp: 2007-10-17 08:54:27

Surfaces (Walls, Roofs, etc) that may be Shadowed by Other Surfaces

|Possible Shadow Casters
|-----------------------
FRONT-1|MAIN SOUTH OVERHANG |Mir-MAIN SOUTH OVERHANG |
SOUTH DOOR OVERHANG|Mir-MAIN SOUTH OVERHANG |WALL-1PF |FRONT-1 |
WALL-1PF|Mir-MAIN SOUTH OVERHANG |SOUTH DOOR OVERHANG |Mir-SOUTH DOOR OVERHANG |
MAIN SOUTH OVERHANG|FRONT-1 |

Subsurfaces (Windows and Doors) that may be Shadowed by Surfaces

|Possible Shadow Casters
|-----------------------
WF-1|FRONT-1 |
DF-1|FRONT-1 |
WR-1|RIGHT-1 |
WB-1|BACK-1 |
DB-1|BACK-1 |
WL-1|LEFT-1 |

## Shading Summary

The Shading Summary report shows how much of each window is sunlit at different times of the year and also includes a summary of the window controls. Directly following is an example of the report. The key used to obtain this report is ShadingSummary.

Report: Shading Summary

For: Entire Facility

Timestamp: 2007-10-17 08:54:27

Sunlit Fraction

|March 21 9am|March 21 noon|March 21 3pm|June 21 9am|June 21 noon|June 21 3pm|December 21 9am|December 21 noon|December 21 3pm
|------------|-------------|------------|-----------|------------|-----------|---------------|----------------|---------------
WF-1|0.00|0.00|0.43|0.00|0.00|0.00|0.30|0.52|0.86
DF-1|0.73|0.26|0.62|0.00|0.00|0.28|0.90|0.69|0.92
WR-1|1.00|1.00|0.00|1.00|1.00|0.00|1.00|1.00|0.00
WB-1|0.00|0.00|0.00|1.00|0.00|0.00|0.00|0.00|0.00
DB-1|0.00|0.00|0.00|1.00|0.00|0.00|0.00|0.00|0.00
WL-1|0.00|0.00|1.00|0.00|0.00|1.00|0.00|0.00|1.00

Window Control

|Name|Type|Shaded Construction|Control|Glare Control
|----|----|-------------------|-------|-------------
none| | | | | 

## Lighting Summary

The Lighting Summary report provides a description of the interior and exterior lighting systems being simulated. It also provides a summary of daylighting controls. The Interior Lighting table has three columns that are explained below:

Scheduled Hours/Week [hr] - In the schedule shown, this represents the average weekly sum of hourly values. It is based on a full year even if the simulation is only performed for part of the year. It is not affected by daylighting.

Hours/Week > 1% [hr] – This represents the average hours per week that are above 1% of the design value.

Full Load Hours/Week [hr] – This is based on consumption and hours that the consumption occurred. It is dependent on the run period used. When simulating only a portion of the year, the value depends on which days of the year (the number of weekdays versus weekend days, for example) are simulated. It also indicates the impact of daylighting control. Directly following is an example of the report. The key used to obtain this report is LightingSummary.

Report: Lighting Summary

For: Entire Facility

Timestamp: 2012-09-05 12:10:52

Interior Lighting

|Zone|Lighting Power Density [W/m2]|Zone Area [m2]|Total Power [W]|End Use Subcategory|Schedule Name|Scheduled Hours/Week [hr]|Hours/Week > 1% [hr]|Full Load Hours/Week [hr]|Return Air Fraction|Conditioned (Y/N)|Consumption [GJ]
|----|-----------------------------|--------------|---------------|-------------------|-------------|-------------------------|--------------------|-------------------------|-------------------|-----------------|----------------
SPACE1-1 LIGHTS 1|SPACE1-1|15.9742|99.16|1584.00|GeneralLights|LIGHTS-1|57.70|168.00|57.70|0.2000|Y|17.16
SPACE2-1 LIGHTS 1|SPACE2-1|16.0056|42.73|684.00|GeneralLights|HALFPERCENT|0.60|0.00|0.59|0.2000|Y|0.08
SPACE3-1 LIGHTS 1|SPACE3-1|16.4179|96.48|1584.00|GeneralLights|ALWAYSON|168.00|168.00|168.00|0.2000|Y|49.95
SPACE4-1 LIGHTS 1|SPACE4-1|16.0056|42.73|684.00|GeneralLights|LIGHTS-1|57.70|168.00|57.70|0.2000|Y|7.41
SPACE5-1 LIGHTS 1|SPACE5-1|16.2420|182.49|2964.00|GeneralLights|LIGHTS-1|57.70|168.00|57.70|0.2000|Y|32.11
Interior Lighting Total| |16.1777|463.60|7500.00| | | | | | | |106.70

Daylighting

|Zone|Daylighting Type|Control Type|Fraction Controlled|Lighting Installed in Zone [W]|Lighting Controlled [W]
|----|----------------|------------|-------------------|------------------------------|-----------------------
None| | | | | | 

Exterior Lighting

|Total Watts|Astronomical Clock/Schedule|Schedule Name|Scheduled Hours/Week [hr]|Hours/Week > 1% [hr]|Full Load Hours/Week [hr]|Consumption [GJ]
|-----------|---------------------------|-------------|-------------------------|--------------------|-------------------------|----------------
Exterior Lighting Total|0.00| | | | | |0.00

## HVAC Sizing Summary

The HVAC Sizing Summary report provides information on the zone cooling and heating sizing and the peak load conditions as well as information about the system air flow sizing. Directly following is an example of the report. The key used to obtain this report is HVACSizingSummary.

Report: **HVAC Sizing Summary**For: **Entire FacilityTimestamp: 2011-09-23 15:09:34Zone Cooling**

Calculated Design Load [W]
User Design Load [W]
Calculated Design Air Flow [m3/s]
User Design Air Flow [m3/s]
Design Day Name
Date/Time Of Peak
Temperature at Peak [C]
Humidity Ratio at Peak [kgWater/kgAir]

SPACE1-1
2647.33
2647.33
0.223
0.223
CHICAGO_IL_USA ANNUAL COOLING 1% DESIGN CONDITIONS DB/MCWB
7/21 15:45:00
31.02
0.01459

SPACE2-1
2234.38
2234.38
0.188
0.188
CHICAGO_IL_USA ANNUAL COOLING 1% DESIGN CONDITIONS DB/MCWB
7/21 10:00:00
27.43
0.01459

SPACE3-1
2506.34
2506.34
0.211
0.211
CHICAGO_IL_USA ANNUAL COOLING 1% DESIGN CONDITIONS DB/MCWB
7/21 15:00:00
31.50
0.01459

SPACE4-1
2464.72
2464.72
0.207
0.207
CHICAGO_IL_USA ANNUAL COOLING 1% DESIGN CONDITIONS DB/MCWB
7/21 17:30:00
29.47
0.01459

SPACE5-1
2628.69
2628.69
0.221
0.221
CHICAGO_IL_USA ANNUAL COOLING 1% DESIGN CONDITIONS DB/MCWB
7/21 15:00:00
31.50
0.01459

**Zone Heating**

Calculated Design Load [W]
User Design Load [W]
Calculated Design Air Flow [m3/s]
User Design Air Flow [m3/s]
Design Day Name
Date/Time Of Peak
Temperature at Peak [C]
Humidity Ratio at Peak [kgWater/kgAir]

SPACE1-1
3860.76
3860.76
0.117
0.117
CHICAGO_IL_USA ANNUAL HEATING 99% DESIGN CONDITIONS DB
1/21 24:00:00
-17.30
0.00084

SPACE2-1
1625.15
1625.15
0.049
0.049
CHICAGO_IL_USA ANNUAL HEATING 99% DESIGN CONDITIONS DB
1/21 24:00:00
-17.30
0.00084

SPACE3-1
3753.07
3753.07
0.113
0.113
CHICAGO_IL_USA ANNUAL HEATING 99% DESIGN CONDITIONS DB
1/21 24:00:00
-17.30
0.00084

SPACE4-1
1625.15
1625.15
0.049
0.049
CHICAGO_IL_USA ANNUAL HEATING 99% DESIGN CONDITIONS DB
1/21 24:00:00
-17.30
0.00084

SPACE5-1
2981.57
2981.57
0.090
0.103
CHICAGO_IL_USA ANNUAL HEATING 99% DESIGN CONDITIONS DB
1/21 24:00:00
-17.30
0.00084

**System Design Air Flow Rates**

Calculated cooling [m3/s]
User cooling [m3/s]
Calculated heating [m3/s]
User heating [m3/s]

VAV SYS 1
1.05
1.05
0.43
0.43

## System Summary

The System Summary report provides information about some of the primary components in the system including the economizer and demand controlled ventilation. In addition, this report describes when the zone conditions are not comfortable or when set points are not met.

In the *Time Not Comfortable Based on Simple ASHRAE 55-2004* and **Time Setpoint Not Met sub-tables, The reported time represents the total number of hours that a given zone did not meet the comfort or setpoint criteria. For the Facility row, this is the total numbers of hours that one or more zones were out of range. The values are not weighted by number of zones or number of occupants.

Directly following is an example of the report. The key used to obtain this report is SystemSummary.

Report: System Summary

For: Entire Facility

Timestamp: 2007-10-17 08:54:27

Economizer

|High Limit Shutoff Control|Minimum Outside Air (m3/s)|Maximum Outside Air (m3/s)|Return Air Temp Limit|Return Air Enthalpy Limit|Outside Air Temperature Limit (C)|Outside Air Enthalpy Limit (C)
|--------------------------|--------------------------|--------------------------|---------------------|-------------------------|---------------------------------|------------------------------
none| | | | | | | 

Demand Controlled Ventilation using VENTILATION:MECHANICAL

|Ventilation:Mechanical Name|Outside Air Per Person(m3/s-person)|Outside Air Per Area (m3/s-m2)
|---------------------------|-----------------------------------|------------------------------
none| | | 

Time Not Comfortable Based on Simple ASHRAE 55-2004

|Winter Clothes (hr)|Summer Clothes (hr)|Summer or Winter Clothes (hr)
|-------------------|-------------------|-----------------------------
SPACE1-1|429.00|1913.75|79.25
SPACE2-1|739.25|1719.75|22.50
SPACE3-1|482.00|1885.50|85.50
SPACE4-1|588.25|2039.75|59.50
SPACE5-1|14.75|2535.75|14.75
PLENUM-1|0.00|0.00|0.00
Facility|1041.75|2564.75|97.00

Time Setpoint Not Met

|During Heating (hr)|During Cooling (hr)|During Occupied Heating (hr)|During Occupied Cooling (hr)
|-------------------|-------------------|----------------------------|----------------------------
SPACE1-1|321.00|45.25|35.00|45.25
SPACE2-1|104.25|44.00|0.25|10.75
SPACE3-1|297.50|0.75|36.25|0.75
SPACE4-1|77.75|27.00|0.25|0.00
SPACE5-1|292.25|1.00|13.75|1.00
PLENUM-1|0.00|0.00|0.00|0.00
Facility|333.50|117.00|37.25|56.75

## Component Sizing Summary

The Component Sizing Summary report includes details on many of the HVAC components in the simulation. For each of the components, one or more parameters are shown. Directly following is an example of the report. The key used to obtain this report is ComponentSizingSummary.

Report: Component Sizing Summary

For: Entire Facility

Timestamp: 2007-10-17 08:54:27

SINGLE DUCT:VAV:REHEAT

|Maximum air flow rate [m3/s]|Max Reheat Water Flow [m3/s]
|----------------------------|----------------------------
SPACE1-1 VAV REHEAT|0.226630|0.000059
SPACE2-1 VAV REHEAT|0.176548|0.000046
SPACE3-1 VAV REHEAT|0.209331|0.000054
SPACE4-1 VAV REHEAT|0.222526|0.000058
SPACE5-1 VAV REHEAT|0.221695|0.000057

COIL:Water:SimpleHeating

|Max Water Flow Rate of Coil [m3/s]|Design Coil Load [W]|UA of the Coil [W/delK]
|----------------------------------|--------------------|-----------------------
SPACE1-1 ZONE COIL|0.000059|2698.50|66.13
SPACE2-1 ZONE COIL|0.000046|2102.17|51.51
SPACE3-1 ZONE COIL|0.000054|2492.52|61.08
SPACE4-1 ZONE COIL|0.000058|2649.63|64.93
SPACE5-1 ZONE COIL|0.000057|2639.74|64.69
OA HEATING COIL 1|0.000148|6812.27|84.72
MAIN HEATING COIL 1|0.000075|3458.59|55.80

BRANCH

|Maximum Branch Flow Rate [m3/s]
|-------------------------------
VAV SYS 1 MAIN BRANCH|1.06

AIR PRIMARY LOOP

|Primary air design volumetric flow rate [m3/s]
|----------------------------------------------
VAV SYS 1|1.06

CONTROLLER:OUTSIDE AIR

|maximum outside air flow rate [m3/s]|minimum outside air flow rate [m3/s]
|------------------------------------|------------------------------------
OA CONTROLLER 1|1.06|0.264118

COIL:Water:Cooling

|Max Water Flow Rate of Coil [m3/s]|Max Air Flow Rate of Coil [m3/s]|Design Air Inlet Temperature [C]|Design Air Outlet Temperature [C]|Design Water Inlet Temperature [C]|Design Air Inlet Humidity Ratio|Design Air Outlet Humidity Ratio
|----------------------------------|--------------------------------|--------------------------------|---------------------------------|----------------------------------|-------------------------------|--------------------------------
OA COOLING COIL 1|0.001145|0.264118|30.01|11.00|7.00|0.014595|0.008000
MAIN COOLING COIL 1|0.000916|1.06|21.55|12.80|7.00|0.009333|0.008000

FAN:SIMPLE:VARIABLEVOLUME

|Max Flow Rate [m3/s]|Min Flow Rate [m3/s]
|--------------------|--------------------
SUPPLY FAN 1|1.06|0.353263

CONTROLLER:SIMPLE

|Max Actuated Flow [m3/s]
|------------------------
OA CC CONTROLLER 1|0.001145
OA HC CONTROLLER 1|0.000148
CENTRAL COOLING COIL CONTROLLER 1|0.000916
CENTRAL HEATING COIL CONTROLLER 1|0.000075

PLANT LOOP

|Maximum Loop Volumetric Flow Rate [m3/s]|Volume of the plant loop [m3]
|----------------------------------------|-----------------------------
HOT WATER LOOP|0.000497|0.559158
CHILLED WATER LOOP|0.002061|2.32

BOILER:SIMPLE

|Nominal Capacity [W]|Design Boiler Water Flow Rate [m3/s]
|--------------------|------------------------------------
CENTRAL BOILER|22853.43|0.000497

CHILLER:ELECTRIC

|Nominal Capacity [W]|Design Evaporator Volumetric Water Flow Rate [m3/s]
|--------------------|---------------------------------------------------
CENTRAL CHILLER|34468.25|0.002061

PUMP:VARIABLE SPEED

|Rated Volumetric Flow Rate [m3/s]|Rated Power Consumption [W]
|---------------------------------|---------------------------
HW CIRC PUMP|0.000497|126.98
CW CIRC PUMP|0.002061|526.69

## Outdoor Air Summary

The Outdoor Air Summary provides information for each zone on the average and minimum ventilation provided.

The reports described so far in this section are displayed when specified in the Output:Table:Predefined object. They are either on or off and are not customizable. The next few types of tabular reports are customizable. You can specify the report variables being used in each one.

Directly following is an example of the report. The key used to obtain this report is OutdoorAirSummary.

Report: Outdoor Air Summary

For: Entire Facility

Timestamp: 2007-10-17 08:54:27

Average Outside Air During Occupied Hours

|Average Number of Occupants|Nominal Number of Occupants|Zone Volume (m3)|Mechanical Ventilation (ach)|Infiltration (ach)|Simple Ventilation (ach)
|---------------------------|---------------------------|----------------|----------------------------|------------------|------------------------
SPACE1-1|9.50|11.00|239.25|0.365|0.050|0.000
SPACE2-1|4.32|5.00|103.31|0.630|0.051|0.000
SPACE3-1|9.50|11.00|239.25|0.338|0.050|0.000
SPACE4-1|4.32|5.00|103.31|0.708|0.051|0.000
SPACE5-1|17.27|20.00|447.68|0.214|0.052|0.000

Minimum Outside Air During Occupied Hours

|Average Number of Occupants|Nominal Number of Occupants|Zone Volume (m3)|Mechanical Ventilation (ach)|Infiltration (ach)|Simple Ventilation (ach)
|---------------------------|---------------------------|----------------|----------------------------|------------------|------------------------
SPACE1-1|9.50|11.00|239.25|0.000|0.000|0.000
SPACE2-1|4.32|5.00|103.31|0.000|0.000|0.000
SPACE3-1|9.50|11.00|239.25|0.000|0.000|0.000
SPACE4-1|4.32|5.00|103.31|0.000|0.000|0.000
SPACE5-1|17.27|20.00|447.68|0.000|0.000|0.000

## Climatic Data Summary

The Climatic Data Summary provides some statistics from the .STAT file concerning the selected weather. The Stat file must be available (it is included with all the weather files on the EnergyPlus website) for the report to be fully produced. Directly following is an example of the report. The key used to obtain this report is ClimaticDataSummary.

Report: Climatic Data Summary

For: Entire Facility

Timestamp: 2009-02-10 13:27:45

SizingPeriod:DesignDay

|Maximum Dry Bulb (C)|Daily Temperature Range (C)|Humidity Value|Humidity Type|Wind Speed (m/s)|Wind Direction
|--------------------|---------------------------|--------------|-------------|----------------|--------------
CHICAGO_IL_USA ANNUAL HEATING 99% DESIGN CONDITIONS DB|-17.30|0.00|-17.30|WETBULB|4.90|270.00
CHICAGO_IL_USA ANNUAL COOLING 1% DESIGN CONDITIONS DB/MCWB|31.50|10.70|23.00|WETBULB|5.30|230.00

Weather Statistics File

|Value
|-----
Reference|USA_Chicago-OHare_TMY2
Site:Location|Chicago IL USA
Latitude|N 41° 58'
Longitude|W 87° 54'
Time Zone|GMT -6.0 Hours
Elevation|190m above sea level
Standard Pressure at Elevation|99063Pa
Data Source|Release Test
WMO Station|725300
Design Conditions|Climate Design Data 2005 ASHRAE Handbook
Heating Design Temperature 99.6% (C)|-20.6
Heating Design Temperature 99% (C)|-17.3
Cooling Design Temperature 0.4% (C)|33.2
Cooling Design Temperature 1% (C)|31.5
Cooling Design Temperature 2% (C)|29.9
Maximum Dry Bulb Temperature (C)|35.6°
Maximum Dry Bulb Occurs on|Jul 9
Minimum Dry Bulb Temperature (C)|-22.8°
Minimum Dry Bulb Occurs on|Jan 7
Maximum Dew Point Temperature (C)|25.6
Maximum Dew Point Occurs on|Aug 4
Minimum Dew Point Temperature (C)|-28.9
Minimum Dew Point Occurs on|Dec 31
Heating Degree-Days (base 10°C)|1745
Cooling Degree-Days (base 18°C)|464
Köppen Classification|Dfa
Köppen Description|Humid continental (hot summer, cold winter, no dry season, lat. 30-60°N)
Köppen Recommendation|Unbearably humid periods in summer, but passive cooling is possible
ASHRAE Climate Zone|5A
ASHRAE Description|Cool-Humid

## Object Count Summary 

This report provides the count on the number of specific objects in the file. Directly following is an example of the report. The key used to obtain this report is ObjectCountSummary.

Report: Object Count Summary

For: Entire Facility

Timestamp: 2009-02-10 12:39:35

Surfaces by Class

|Total|Outdoors
|-----|--------
Wall|24|8
Floor|10|5
Roof|6|1
Internal Mass|0|0
Building Detached Shading|0|0
Fixed Detached Shading|0|0
Window|6|6
Door|0|0
Glass Door|0|0
Shading|4|4
Overhang|0|0
Fin|0|0
Tubular Daylighting Device Dome|0|0
Tubular Daylighting Device Diffuser|0|0

HVAC

|Count
|-----
HVAC Air Loops|1
Conditioned Zones|6
Unconditioned Zones|0
Supply Plenums|0
Return Plenums|1

## Energy Meters

This report provides the details on all the energy meters generated by the simulation. Directly following is an example of the report. The key used to obtain this report is EnergyMeters.

Report: Energy Meters

For: Entire Facility

Timestamp: 2012-05-01 17:07:24

Annual and Peak Values - Electricity

|Electricity Annual Value [GJ]|Electricity Minimum Value [W]|Timestamp of Minimum|Electricity Maximum Value [W]|Timestamp of Maximum
|-----------------------------|-----------------------------|--------------------|-----------------------------|--------------------
Electricity:Facility|156.68|475.00|01-APR-00:15|20165.58|17-JUL-15:00
Electricity:Building|128.94|475.00|01-JAN-00:15|12000.00|01-JAN-10:15
Electricity:Zone:SPACE1-1|27.23|100.32|01-JAN-00:15|2534.40|01-JAN-10:15
InteriorLights:Electricity|81.24|375.00|01-JAN-00:15|7500.00|01-JAN-10:15
InteriorLights:Electricity:Zone:SPACE1-1|17.16|79.20|01-JAN-00:15|1584.00|01-JAN-10:15
GeneralLights:InteriorLights:Electricity|81.24|375.00|01-JAN-00:15|7500.00|01-JAN-10:15
Electricity:Zone:SPACE2-1|11.76|43.32|01-JAN-00:15|1094.40|01-JAN-10:15
InteriorLights:Electricity:Zone:SPACE2-1|7.41|34.20|01-JAN-00:15|684.00|01-JAN-10:15
Electricity:Zone:SPACE3-1|27.23|100.32|01-JAN-00:15|2534.40|01-JAN-10:15
InteriorLights:Electricity:Zone:SPACE3-1|17.16|79.20|01-JAN-00:15|1584.00|01-JAN-10:15
Electricity:Zone:SPACE4-1|11.76|43.32|01-JAN-00:15|1094.40|01-JAN-10:15
InteriorLights:Electricity:Zone:SPACE4-1|7.41|34.20|01-JAN-00:15|684.00|01-JAN-10:15
Electricity:Zone:SPACE5-1|50.96|187.72|01-JAN-00:15|4742.40|01-JAN-10:15
InteriorLights:Electricity:Zone:SPACE5-1|32.11|148.20|01-JAN-00:15|2964.00|01-JAN-10:15
InteriorEquipment:Electricity|47.70|100.00|01-JAN-00:15|4500.00|01-JAN-09:15
InteriorEquipment:Electricity:Zone:SPACE1-1|10.07|21.12|01-JAN-00:15|950.40|01-JAN-09:15
General:InteriorEquipment:Electricity|47.70|100.00|01-JAN-00:15|4500.00|01-JAN-09:15
InteriorEquipment:Electricity:Zone:SPACE2-1|4.35|9.12|01-JAN-00:15|410.40|01-JAN-09:15
InteriorEquipment:Electricity:Zone:SPACE3-1|10.07|21.12|01-JAN-00:15|950.40|01-JAN-09:15
InteriorEquipment:Electricity:Zone:SPACE4-1|4.35|9.12|01-JAN-00:15|410.40|01-JAN-09:15
InteriorEquipment:Electricity:Zone:SPACE5-1|18.85|39.52|01-JAN-00:15|1778.40|01-JAN-09:15
ElectricityPurchased:Facility|156.68|475.00|28-JUN-05:15|20165.58|17-JUL-15:00
ElectricityPurchased:Plant|156.68|475.00|28-JUN-05:15|20165.58|17-JUL-15:00
Cogeneration:ElectricityPurchased|156.68|475.00|28-JUN-05:15|20165.58|17-JUL-15:00
ElectricitySurplusSold:Facility|0.00|0.00|01-JAN-00:15|0.00|01-JAN-00:15
ElectricitySurplusSold:Plant|0.00|0.00|01-JAN-00:15|0.00|01-JAN-00:15
Cogeneration:ElectricitySurplusSold|0.00|0.00|01-JAN-00:15|0.00|01-JAN-00:15
ElectricityNet:Facility|156.68|475.00|28-JUN-05:15|20165.58|17-JUL-15:00
ElectricityNet:Plant|156.68|475.00|28-JUN-05:15|20165.58|17-JUL-15:00
Cogeneration:ElectricityNet|156.68|475.00|28-JUN-05:15|20165.58|17-JUL-15:00
Electricity:HVAC|8.87|0.00|01-APR-00:15|655.43|17-JUL-15:00
Fans:Electricity|8.87|0.00|01-APR-00:15|655.43|17-JUL-15:00
General:Fans:Electricity|8.87|0.00|01-APR-00:15|655.43|17-JUL-15:00
Electricity:Plant|18.88|0.00|01-JAN-00:15|8053.88|19-JUL-16:00
Heating:Electricity|0.00|0.00|01-JAN-00:15|0.00|01-JAN-00:15
Boiler Parasitic:Heating:Electricity|0.00|0.00|01-JAN-00:15|0.00|01-JAN-00:15
Cooling:Electricity|16.44|0.00|01-JAN-00:15|7478.31|19-JUL-16:00
Pumps:Electricity|2.44|0.00|01-JAN-00:15|575.73|24-JUL-20:00
MYGENERALLIGHTS|81.24|375.00|01-JAN-00:15|7500.00|01-JAN-10:15
MYBUILDINGELECTRIC|128.94|475.00|01-JAN-00:15|12000.00|01-JAN-10:15
MYBUILDINGOTHER|47.70|375.00|01-JAN-00:15|7500.00|01-JAN-10:15

Annual and Peak Values - Gas

|Gas Annual Value [GJ]|Gas Minimum Value [W]|Timestamp of Minimum|Gas Maximum Value [W]|Timestamp of Maximum
|---------------------|---------------------|--------------------|---------------------|--------------------
Gas:Facility|68.82|0.00|01-JAN-00:15|42092.68|01-FEB-06:15
Gas:Plant|68.82|0.00|01-JAN-00:15|42092.68|01-FEB-06:15
Heating:Gas|68.82|0.00|01-JAN-00:15|42092.68|01-FEB-06:15
Boiler:Heating:Gas|68.82|0.00|01-JAN-00:15|42092.68|01-FEB-06:15

Annual and Peak Values - Cooling

|Cooling Annual Value [GJ]|Cooling Minimum Value [W]|Timestamp of Minimum|Cooling Maximum Value [W]|Timestamp of Maximum
|-------------------------|-------------------------|--------------------|-------------------------|--------------------
PlantLoopCoolingDemand:Facility|62.41|0.00|01-JAN-00:15|27050.46|17-JUL-15:00
PlantLoopCoolingDemand:HVAC|62.41|0.00|01-JAN-00:15|27050.46|17-JUL-15:00
CoolingCoils:PlantLoopCoolingDemand|62.41|0.00|01-JAN-00:15|27050.46|17-JUL-15:00

Annual and Peak Values - Water

|Annual Value [m3]|Minimum Value [m3/s]|Timestamp of Minimum|Maximum Value [m3/s]|Timestamp of Maximum
|-----------------|--------------------|--------------------|--------------------|--------------------
None| | | | | 

Annual and Peak Values - Other by Weight/Mass

|Annual Value [kg]|Minimum Value [kg/s]|Timestamp of Minimum|Maximum Value [kg/s]|Timestamp of Maximum
|-----------------|--------------------|--------------------|--------------------|--------------------
Carbon Equivalent:Facility|0.00|0.000|01-JAN-00:15|0.000|01-JAN-00:15
CarbonEquivalentEmissions:Carbon Equivalent|0.00|0.000|01-JAN-00:15|0.000|01-JAN-00:15

Annual and Peak Values - Other Volumetric

|Annual Value [m3]|Minimum Value [m3/s]|Timestamp of Minimum|Maximum Value [m3/s]|Timestamp of Maximum
|-----------------|--------------------|--------------------|--------------------|--------------------
None| | | | | 

Annual and Peak Values - Other Liquid/Gas

|Annual Value [L]|Minimum Value [L]|Timestamp of Minimum|Maximum Value [L]|Timestamp of Maximum
|----------------|-----------------|--------------------|-----------------|--------------------
None| | | | | 

Annual and Peak Values - Other

|Annual Value [GJ]|Minimum Value [W]|Timestamp of Minimum|Maximum Value [W]|Timestamp of Maximum
|-----------------|-----------------|--------------------|-----------------|--------------------
EnergyTransfer:Facility|415.65|0.00|01-APR-00:15|102784.74|17-JUL-15:00
EnergyTransfer:Building|97.60|0.00|01-APR-00:15|21488.05|28-JAN-06:15
EnergyTransfer:Zone:PLENUM-1|22.50|0.00|01-APR-00:15|4218.21|07-JAN-08:15
Heating:EnergyTransfer|42.23|0.00|19-MAR-15:30|21488.05|28-JAN-06:15
Heating:EnergyTransfer:Zone:PLENUM-1|19.63|0.00|16-JAN-21:15|4218.21|07-JAN-08:15
Cooling:EnergyTransfer|55.38|0.00|01-JAN-06:15|13937.19|17-JUL-15:00
Cooling:EnergyTransfer:Zone:PLENUM-1|2.87|0.00|01-JAN-00:15|2539.33|18-JUL-15:00
EnergyTransfer:Zone:SPACE1-1|17.91|0.00|01-APR-00:15|4012.66|30-DEC-06:15
Heating:EnergyTransfer:Zone:SPACE1-1|6.51|0.00|01-JAN-10:15|4012.66|30-DEC-06:15
Cooling:EnergyTransfer:Zone:SPACE1-1|11.40|0.00|01-JAN-00:15|3138.67|27-SEP-16:00
EnergyTransfer:Zone:SPACE2-1|10.67|0.00|01-APR-00:15|3432.93|30-DEC-06:15
Heating:EnergyTransfer:Zone:SPACE2-1|2.42|0.00|01-JAN-01:30|3432.93|30-DEC-06:15
Cooling:EnergyTransfer:Zone:SPACE2-1|8.25|0.00|01-JAN-00:15|2460.04|06-SEP-10:15
EnergyTransfer:Zone:SPACE3-1|16.79|0.00|01-APR-00:15|3788.83|30-DEC-06:15
Heating:EnergyTransfer:Zone:SPACE3-1|6.09|0.00|01-JAN-12:00|3788.83|30-DEC-06:15
Cooling:EnergyTransfer:Zone:SPACE3-1|10.70|0.00|01-JAN-00:15|2807.57|19-JUL-11:00
EnergyTransfer:Zone:SPACE4-1|9.82|0.00|01-APR-00:15|3746.68|28-JAN-06:15
Heating:EnergyTransfer:Zone:SPACE4-1|2.98|0.00|01-JAN-01:30|3746.68|28-JAN-06:15
Cooling:EnergyTransfer:Zone:SPACE4-1|6.85|0.00|01-JAN-00:15|2281.99|25-JUN-17:15
EnergyTransfer:Zone:SPACE5-1|19.91|0.00|01-APR-00:15|4037.31|30-DEC-06:15
Heating:EnergyTransfer:Zone:SPACE5-1|4.60|0.00|01-JAN-00:15|4037.31|30-DEC-06:15
Cooling:EnergyTransfer:Zone:SPACE5-1|15.31|0.00|01-JAN-06:15|2611.53|17-JUL-15:00
EnergyTransfer:HVAC|117.82|0.00|01-JAN-00:15|31769.77|07-JAN-06:15
HeatingCoils:EnergyTransfer|55.41|0.00|01-JAN-00:15|31769.77|07-JAN-06:15
PlantLoopHeatingDemand:Facility|55.41|0.00|01-JAN-00:15|31769.77|07-JAN-06:15
PlantLoopHeatingDemand:HVAC|55.41|0.00|01-JAN-00:15|31769.77|07-JAN-06:15
HeatingCoils:PlantLoopHeatingDemand|55.41|0.00|01-JAN-00:15|31769.77|07-JAN-06:15
CoolingCoils:EnergyTransfer|62.41|0.00|01-JAN-00:15|27050.46|17-JUL-15:00
EnergyTransfer:Plant|200.23|0.00|01-JAN-00:15|62116.41|19-JUL-16:00
Boilers:EnergyTransfer|55.06|0.00|01-JAN-00:15|33674.15|28-MAR-06:15
Chillers:EnergyTransfer|64.37|0.00|01-JAN-00:15|27319.05|19-JUL-16:00
HeatRejection:EnergyTransfer|80.81|0.00|01-JAN-00:15|34797.36|19-JUL-16:00

## Sensible Heat Gain Summary

This report is more fully described in the Input Output Reference but represents the major heat gain components for each zone as well as the HVAC loads satisfied. Directly following is an example of the report. The key used to obtain this report is SensibleHeatGainSummary.

Report: Sensible Heat Gain Summary

For: Entire Facility

Timestamp: 2011-09-09 08:02:03

Annual Building Sensible Heat Gain Components

|HVAC Input Sensible Air Heating [GJ]|HVAC Input Sensible Air Cooling [GJ]|HVAC Input Heated Surface Heating [GJ]|HVAC Input Cooled Surface Cooling [GJ]|People Sensible Heat Addition [GJ]|Lights Sensible Heat Addition [GJ]|Equipment Sensible Heat Addition [GJ]|Window Heat Addition [GJ]|Interzone Air Transfer Heat Addition [GJ]|Infiltration Heat Addition [GJ]|Opaque Surface Conduction and Other Heat Addition [GJ]|Equipment Sensible Heat Removal [GJ]|Window Heat Removal [GJ]|Interzone Air Transfer Heat Removal [GJ]|Infiltration Heat Removal [GJ]|Opaque Surface Conduction and Other Heat Removal [GJ]
|------------------------------------|------------------------------------|--------------------------------------|--------------------------------------|----------------------------------|----------------------------------|-------------------------------------|-------------------------|-----------------------------------------|-------------------------------|------------------------------------------------------|------------------------------------|------------------------|----------------------------------------|------------------------------|-----------------------------------------------------
SPACE1-1|0.096|-0.070|0.000|0.000|0.035|0.131|0.065|0.220|0.000|0.000|0.001|0.000|-0.078|0.000|-0.004|-0.395
SPACE2-1|0.051|-0.038|0.000|0.000|0.014|0.056|0.028|0.173|0.000|0.000|0.001|0.000|-0.029|0.000|-0.002|-0.253
SPACE3-1|0.093|-0.030|0.000|0.000|0.034|0.131|0.065|0.157|0.000|0.000|0.000|0.000|-0.082|0.000|-0.004|-0.363
SPACE4-1|0.055|-0.015|0.000|0.000|0.015|0.056|0.028|0.105|0.000|0.000|0.000|0.000|-0.032|0.000|-0.002|-0.211
SPACE5-1|0.032|-0.072|0.000|0.000|0.073|0.244|0.121|0.000|0.000|0.000|0.000|0.000|0.000|0.000|-0.007|-0.392
PLENUM-1|0.118|-0.024|0.000|0.000|0.000|0.000|0.000|0.000|0.000|0.000|0.000|0.000|0.000|0.000|0.000|-0.094
Total Facility|0.445|-0.248|0.000|0.000|0.170|0.618|0.307|0.655|0.000|0.000|0.002|0.000|-0.221|0.000|-0.017|-1.709

Peak Cooling Sensible Heat Gain Components

|Time of Peak|HVAC Input Sensible Air Heating [W]|HVAC Input Sensible Air Cooling [W]|HVAC Input Heated Surface Heating [W]|HVAC Input Cooled Surface Cooling [W]|People Sensible Heat Addition [W]|Lights Sensible Heat Addition [W]|Equipment Sensible Heat Addition [W]|Window Heat Addition [W]|Interzone Air Transfer Heat Addition [W]|Infiltration Heat Addition [W]|Opaque Surface Conduction and Other Heat Addition [W]|Equipment Sensible Heat Removal [W]|Window Heat Removal [W]|Interzone Air Transfer Heat Removal [W]|Infiltration Heat Removal [W]|Opaque Surface Conduction and Other Heat Removal [W]
|------------|-----------------------------------|-----------------------------------|-------------------------------------|-------------------------------------|---------------------------------|---------------------------------|------------------------------------|------------------------|----------------------------------------|------------------------------|-----------------------------------------------------|-----------------------------------|-----------------------|---------------------------------------|-----------------------------|----------------------------------------------------
SPACE1-1|14-JAN-15:00|0.00|-3502.16|0.00|0.00|724.60|1584.00|739.20|9347.85|0.00|0.00|0.00|0.00|0.00|0.00|0.00|-8893.49
SPACE2-1|14-JAN-10:07|0.00|-1738.94|0.00|0.00|345.99|684.00|410.40|4684.27|0.00|0.00|0.00|0.00|0.00|0.00|0.00|-4385.71
SPACE3-1|14-JAN-14:45|0.00|-1095.49|0.00|0.00|761.13|1584.00|844.80|356.87|0.00|0.00|0.00|0.00|0.00|0.00|0.00|-2451.31
SPACE4-1|14-JAN-16:00|0.00|-736.19|0.00|0.00|345.95|684.00|228.00|1157.98|0.00|0.00|0.00|0.00|0.00|0.00|0.00|-1679.74
SPACE5-1|14-JAN-13:35|0.00|-2315.43|0.00|0.00|1135.19|2667.60|1778.40|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|-3265.77
PLENUM-1|07-JUL-19:46|0.00|-1579.40|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|1579.40|0.00|0.00|0.00|0.00|0.00
Total Facility|14-JAN-15:00|0.00|-7822.86|0.00|0.00|3561.18|7500.00|3500.00|10470.76|0.00|0.00|0.00|0.00|0.00|0.00|0.00|-17209.08

Peak Heating Sensible Heat Gain Components

|Time of Peak|HVAC Input Sensible Air Heating [W]|HVAC Input Sensible Air Cooling [W]|HVAC Input Heated Surface Heating [W]|HVAC Input Cooled Surface Cooling [W]|People Sensible Heat Addition [W]|Lights Sensible Heat Addition [W]|Equipment Sensible Heat Addition [W]|Window Heat Addition [W]|Interzone Air Transfer Heat Addition [W]|Infiltration Heat Addition [W]|Opaque Surface Conduction and Other Heat Addition [W]|Equipment Sensible Heat Removal [W]|Window Heat Removal [W]|Interzone Air Transfer Heat Removal [W]|Infiltration Heat Removal [W]|Opaque Surface Conduction and Other Heat Removal [W]
|------------|-----------------------------------|-----------------------------------|-------------------------------------|-------------------------------------|---------------------------------|---------------------------------|------------------------------------|------------------------|----------------------------------------|------------------------------|-----------------------------------------------------|-----------------------------------|-----------------------|---------------------------------------|-----------------------------|----------------------------------------------------
SPACE1-1|14-JAN-07:00|10652.50|0.00|0.00|0.00|0.00|79.20|21.12|0.00|0.00|0.00|0.00|0.00|-1551.57|0.00|0.00|-9201.25
SPACE2-1|14-JAN-07:00|7816.55|0.00|0.00|0.00|0.00|34.20|9.12|0.00|0.00|0.00|0.00|0.00|-594.65|0.00|0.00|-7265.22
SPACE3-1|14-JAN-07:00|11495.41|0.00|0.00|0.00|0.00|79.20|21.12|0.00|0.00|0.00|0.00|0.00|-1539.09|0.00|0.00|-10056.64
SPACE4-1|14-JAN-07:00|8837.62|0.00|0.00|0.00|0.00|34.20|9.12|0.00|0.00|0.00|0.00|0.00|-604.20|0.00|0.00|-8276.74
SPACE5-1|14-JAN-07:00|12359.25|0.00|0.00|0.00|0.00|148.20|39.52|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|-12546.97
PLENUM-1|14-JAN-07:18|5275.99|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|-5275.99
Total Facility|14-JAN-07:15|55720.39|0.00|0.00|0.00|0.00|375.00|100.00|0.00|0.00|0.00|0.00|0.00|-4289.51|0.00|0.00|-51905.88

## Standard 62.1 Summary

This report is more fully described in the Input Output Reference. The Standard 62.1 Summary produces a report that is consistent with many of the outputs needed when doing calculations consistent with ASHRAE Standard 62.1-2010. The report is generated when sizing calculations are specified. The abbreviations used in the report are consistent with the abbreviations used in Appendix A4 of the Standard. Directly following is an example of the report. The key used to obtain this report is Standard62.1Summary.

Report: Standard 62.1 Summary

For: Entire Facility

Timestamp: 2012-04-20 12:00:57

System Ventilation Requirements for Cooling

|Sum of Zone Primary Air Flow - Vpz-sum [m3/s]|System Population - Ps|Sum of Zone Population - Pz-sum|Occupant Diversity - D|Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]|System Primary Airflow - Vps [m3/s]|Average Outdoor Air Fraction - Xs|System Ventilation Efficiency - Ev|Outdoor Air Intake Flow - Vot [m3/s]|Percent Outdoor Air - %OA
|---------------------------------------------|----------------------|-------------------------------|----------------------|---------------------------------------------------|-----------------------------------|---------------------------------|----------------------------------|------------------------------------|-------------------------
VAV SYS 1|1.05|52.00|52.00|1.00|0.26|1.05|0.252|1.000|0.27|0.26

System Ventilation Requirements for Heating

|Sum of Zone Primary Air Flow - Vpz-sum [m3/s]|System Population - Ps|Sum of Zone Population - Pz-sum|Occupant Diversity - D|Uncorrected Outdoor Air Intake Airflow - Vou [m3/s]|System Primary Airflow - Vps [m3/s]|Average Outdoor Air Fraction - Xs|System Ventilation Efficiency - Ev|Outdoor Air Intake Flow Vot [m3/s]|Percent Outdoor Air - %OA
|---------------------------------------------|----------------------|-------------------------------|----------------------|---------------------------------------------------|-----------------------------------|---------------------------------|----------------------------------|----------------------------------|-------------------------
VAV SYS 1|1.05|0.00|0.00|1.00|0.26|0.43|0.967|1.000|0.27|0.26

Zone Ventilation Parameters

|AirLoop Name|People Outdoor Air Rate - Rp [m3/s-person]|Zone Population - Pz|Area Outdoor Air Rate - Ra [m3/s-m2]|Zone Floor Area - Az [m2]|Breathing Zone Outdoor Airflow - Vbz [m3/s]|Cooling Zone Air Distribution Effectiveness - Ez-clg|Cooling Zone Outdoor Airflow - Voz-clg [m3/s]|Heating Zone Air Distribution Effectiveness - Ez-htg|Heating Zone Outdoor Airflow - Voz-htg [m3/s]
|------------|------------------------------------------|--------------------|------------------------------------|-------------------------|-------------------------------------------|----------------------------------------------------|---------------------------------------------|----------------------------------------------------|---------------------------------------------
SPACE1-1|VAV SYS 1|0.002360|11.00|0.000305|99.16|0.0562|0.900|0.0624| | 
SPACE2-1|VAV SYS 1|0.002360|5.00|0.000305|42.73|0.0248|0.900|0.0276| | 
SPACE3-1|VAV SYS 1|0.002360|11.00|0.000305|96.48|0.0554|1.000|0.0554| | 
SPACE4-1|VAV SYS 1|0.002360|5.00|0.000305|42.73|0.0248|1.000|0.0248| | 
SPACE5-1|VAV SYS 1|0.002360|20.00|0.000305|182.49|0.1029|1.000|0.1029| | 

System Ventilation Parameters

|People Outdoor Air Rate - Rp [m3/s-person]|Sum of Zone Population - Pz-sum|Area Outdoor Air Rate - Ra [m3/s-m2]|Sum of Zone Floor Area - Az-sum [m2]|Breathing Zone Outdoor Airflow - Vbz [m3/s]|Cooling Zone Outdoor Airflow - Voz-clg [m3/s]|Heating Zone Outdoor Airflow - Voz-htg [m3/s]
|------------------------------------------|-------------------------------|------------------------------------|------------------------------------|-------------------------------------------|---------------------------------------------|---------------------------------------------
VAV SYS 1|0.002360|52.00|0.000305|463.60|0.2641|0.2731|0.0000

Zone Ventilation Calculations for Cooling Design

|AirLoop Name|Box Type|Zone Primary Airflow - Vpz [m3/s]|Zone Discharge Airflow - Vdz [m3/s]|Minimum Zone Primary Airflow - Vpz-min [m3/s]|Zone Outdoor Airflow Cooling - Voz-clg [m3/s]|Primary Outdoor Air Fraction - Zpz|Primary Air Fraction - Ep|Secondary Recirculation Fraction- Er|Supply Air Fraction- Fa|Mixed Air Fraction - Fb|Outdoor Air Fraction - Fc|Zone Ventilation Efficiency - Evz
|------------|--------|---------------------------------|-----------------------------------|---------------------------------------------|---------------------------------------------|----------------------------------|-------------------------|------------------------------------|-----------------------|-----------------------|-------------------------|---------------------------------
SPACE1-1|VAV SYS 1|AIRTERMINAL:SINGLEDUCT:VAV:REHEAT|0.22|0.22|0.08|0.0624|0.280|1.000|0.000|0.000|0.000|0.000|0.900
SPACE2-1|VAV SYS 1|AIRTERMINAL:SINGLEDUCT:VAV:REHEAT|0.19|0.19|0.03|0.0276|0.147|1.000|0.000|0.000|0.000|0.000|0.900
SPACE3-1|VAV SYS 1|AIRTERMINAL:SINGLEDUCT:VAV:REHEAT|0.21|0.21|0.07|0.0554|0.263|1.000|0.000|0.000|0.000|0.000|1.000
SPACE4-1|VAV SYS 1|AIRTERMINAL:SINGLEDUCT:VAV:REHEAT|0.21|0.21|0.03|0.0248|0.120|1.000|0.000|0.000|0.000|0.000|1.000
SPACE5-1|VAV SYS 1|AIRTERMINAL:SINGLEDUCT:VAV:REHEAT|0.22|0.22|0.14|0.1029|0.465|1.000|0.000|0.000|0.000|0.000|1.000

System Ventilation Calculations for Cooling Design

|Sum of Zone Primary Airflow - Vpz-sum [m3/s]|System Primary Airflow - Vps [m3/s]|Sum of Zone Discharge Airflow - Vdz-sum [m3/s]|Minimum Zone Primary Airflow - Vpz-min [m3/s]|Zone Outdoor Airflow Cooling - Voz-clg [m3/s]|Zone Ventilation Efficiency - Evz-min
|--------------------------------------------|-----------------------------------|----------------------------------------------|---------------------------------------------|---------------------------------------------|-------------------------------------
VAV SYS 1|1.05|1.05|1.05|0.19|0.2731|1.000

Zone Ventilation Calculations for Heating Design

|AirLoop Name|Box Type|Zone Primary Airflow - Vpz [m3/s]|Zone Discharge Airflow - Vdz [m3/s]|Minimum Zone Primary Airflow - Vpz-min [m3/s]|Zone Outdoor Airflow Heating - Voz-htg [m3/s]|Primary Outdoor Air Fraction - Zpz|Primary Air Fraction - Ep|Secondary Recirculation Fraction- Er|Supply Air Fraction- Fa|Mixed Air Fraction - Fb|Outdoor Air Fraction - Fc|Zone Ventilation Efficiency - Evz
|------------|--------|---------------------------------|-----------------------------------|---------------------------------------------|---------------------------------------------|----------------------------------|-------------------------|------------------------------------|-----------------------|-----------------------|-------------------------|---------------------------------
None| | | | | | | | | | | | | 

System Ventilation Calculations for Heating Design

|Sum of Zone Primary Airflow - Vpz-sum [m3/s]|System Primary Airflow - Vps [m3/s]|Sum of Zone Discharge Airflow - Vdz-sum [m3/s]|Minimum Zone Primary Airflow - Vpz-min [m3/s]|Zone Outdoor Airflow Heating - Voz-htg [m3/s]|Zone Ventilation Efficiency - Evz-min
|--------------------------------------------|-----------------------------------|----------------------------------------------|---------------------------------------------|---------------------------------------------|-------------------------------------
VAV SYS 1|1.05|1.05|1.05|0.18+309|0.0000|1.000

## Zone Component Loads Summary

The Zone Component Loads Summary provides an estimate of the heating and cooling peak loads for each zone broken down into various components. This report may help determine which components of the load have the largest impact for the heating and cooling peak design conditions. When specified, the Zone Component Loads Summary report is created for each zone that is conditioned. The Sensible-Delayed column of the summary is only an estimate based on a procedure described in the Engineering Reference documentation. An intermediate calculation in this procedure is the decay curves that are shown in the the EIO file when Output:Surfaces:List is set to show DecayCurvesFromZoneComponentLoads. The difference between the peak design sensible load and the estimated instant + delayed sensible load (as shown in the *Peak Conditions* subtable) is an indication of how consistent the overall total estimate may be to the computed total peak loads for the zone. When the report is called the zone sizing calculations are repeated twice so this may result in longer simulation times.  The key used to obtain this report is ZoneComponentLoadSummary.

Report: ZoneComponentLoadSummary

For: SPACE1-1

Timestamp: 2012-09-21 15:48:46

Estimated Cooling Peak Load Components

|Sensible - Instant [W]|Sensible - Delayed [W]|Sensible - Return Air [W]|Latent [W]|Total [W]|%Grand Total
|----------------------|----------------------|-------------------------|----------|---------|------------
People|533.03|272.38| |528.17|1333.58|30.78
Lights|15.84|1424.81|316.80| |1757.45|40.57
Equipment|517.44|318.36| |0.00|835.80|19.29
Refrigeration|0.00| |0.00|0.00|0.00|0.00
Water Use Equipment|0.00| | | |0.00|0.00
HVAC Equipment Losses|0.00|0.00| | |0.00|0.00
Power Generation Equipment|0.00|0.00| | |0.00|0.00
Infiltration|34.65| | |69.37|104.03|2.40
Zone Ventilation|0.00| | |0.00|0.00|0.00
Interzone Mixing|0.00| | |0.00|0.00|0.00
Roof| |0.00| | |0.00|0.00
Interzone Ceiling| |132.59| | |132.59|3.06
Other Roof| |0.00| | |0.00|0.00
Exterior Wall| |451.39| | |451.39|10.42
Interzone Wall| |-511.9| | |-511.9|-11.8
Ground Contact Wall| |0.00| | |0.00|0.00
Other Wall| |0.00| | |0.00|0.00
Exterior Floor| |0.00| | |0.00|0.00
Interzone Floor| |0.00| | |0.00|0.00
Ground Contact Floor| |-1745.4| | |-1745.4|-40.3
Other Floor| |0.00| | |0.00|0.00
Fenestration Conduction|655.17| | | |655.17|15.12
Fenestration Solar|-|1319.55| | |1319.55|30.46
Opaque Door| |0.00| | |0.00|0.00
Grand Total|1756.13|1661.76|316.80|597.54|4332.24| 

Cooling Peak Conditions

|Value
|-----
Time of Peak Load|7/21 16:00:00
Outside Dry Bulb Temperature [C]|30.86
Outside Wet Bulb Temperature [C]|23.10
Outside Humidity Ratio at Peak [kgWater/kgAir]|0.01459
Zone Dry Bulb Temperature [C]|24.00
Zone Relative Humdity [%]|47.16
Zone Humidity Ratio at Peak [kgWater/kgAir]|0.00876
Peak Design Sensible Load [W]|3187.17
Estimated Instant + Delayed Sensible Load [W]|3417.89
Difference [W]|-230.7

Estimated Heating Peak Load Components

|Sensible - Instant [W]|Sensible - Delayed [W]|Sensible - Return Air [W]|Latent [W]|Total [W]|%Grand Total
|----------------------|----------------------|-------------------------|----------|---------|------------
People|0.00|0.00| |0.00|0.00|0.00
Lights|0.79|85.74|15.84| |102.37|-3.3
Equipment|14.78|9.37| |0.00|24.15|-0.8
Refrigeration|0.00| |0.00|0.00|0.00|0.00
Water Use Equipment|0.00| | | |0.00|0.00
HVAC Equipment Losses|0.00|0.00| | |0.00|0.00
Power Generation Equipment|0.00|0.00| | |0.00|0.00
Infiltration|-222.8| | |-93.8|-316.6|10.07
Zone Ventilation|0.00| | |0.00|0.00|0.00
Interzone Mixing|0.00| | |0.00|0.00|0.00
Roof| |0.00| | |0.00|0.00
Interzone Ceiling| |-1027.9| | |-1027.9|32.70
Other Roof| |0.00| | |0.00|0.00
Exterior Wall| |-762.5| | |-762.5|24.26
Interzone Wall| |102.80| | |102.80|-3.3
Ground Contact Wall| |0.00| | |0.00|0.00
Other Wall| |0.00| | |0.00|0.00
Exterior Floor| |0.00| | |0.00|0.00
Interzone Floor| |0.00| | |0.00|0.00
Ground Contact Floor| |889.07| | |889.07|-28.3
Other Floor| |0.00| | |0.00|0.00
Fenestration Conduction|-2155.0| | | |-2155.0|68.55
Fenestration Solar|-|0.00| | |0.00|0.00
Opaque Door| |0.00| | |0.00|0.00
Grand Total|-2362.2|-703.3|15.84|-93.8|-3143.5| 

Heating Peak Conditions

|Value
|-----
Time of Peak Load|1/21 20:15:00
Outside Dry Bulb Temperature [C]|-17.3
Outside Wet Bulb Temperature [C]|-19.0
Outside Humidity Ratio at Peak [kgWater/kgAir]|0.00084
Zone Dry Bulb Temperature [C]|23.00
Zone Relative Humdity [%]|43.89
Zone Humidity Ratio at Peak [kgWater/kgAir]|0.00766
Peak Design Sensible Load [W]|-3077.1
Estimated Instant + Delayed Sensible Load [W]|-3065.5
Difference [W]|-11.6

## Output:Table:TimeBins

A TimeBins report shows a grid of the number of hours that specific report variable spend in a range of values. The first table is broken into two parts. The first part is monthly and the second part is for each hour of the day. The columns show the range of values. A second table is provided to show some statistics on the report variable for the entire year including the maximum and minimum. These statistics are helpful in setting the field values for the interval in the Output Table:TimeBins object. Due to this, it is often necessary to run two simulations to fully utilize the TimeBins report with the first run used to find the minimum and maximum of the variable, these values are then used in the TimeBins object and the second run a better table of binned results are displayed.

Report: PURCHASEDCOOLING:FACILITY J per second

For: Meter

Timestamp: 2006-08-30 07:48:55

Values in table are in hours.

||1|2|3|4|5|6|7|8|9|10||
||-|-|-|-|-|-|-|-|-|--||
Interval Start|less than|0.<=|2000.<=|4000.<=|6000.<=|8000.<=|10000.<=|12000.<=|14000.<=|16000.<=|18000.<=|equal to or more than|Row
Interval End|0.|2000.>|4000.>|6000.>|8000.>|10000.>|12000.>|14000.>|16000.>|18000.>|20000.>|20000.|Total
January|0.00|744.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|744.00
February|0.00|672.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|672.00
March|0.00|744.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|744.00
April|0.00|524.17|40.67|40.33|45.67|45.50|19.67|4.00|0.00|0.00|0.00|0.00|720.00
May|0.00|480.33|44.50|55.50|41.00|53.50|38.17|20.67|10.33|0.00|0.00|0.00|744.00
June|0.00|393.50|31.83|55.33|19.83|53.00|65.83|62.83|24.00|13.83|0.00|0.00|720.00
July|0.00|372.67|30.83|40.83|45.67|53.83|42.83|48.33|45.33|47.83|15.83|0.00|744.00
August|0.00|373.50|24.50|37.17|44.50|64.50|56.67|50.67|52.33|35.00|5.17|0.00|744.00
September|0.00|406.33|52.00|42.50|14.83|54.50|64.67|54.50|27.17|3.50|0.00|0.00|720.00
October|0.00|744.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|744.00
November|0.00|720.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|720.00
December|0.00|744.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|744.00
12:01 to 1:00 am|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
1:01 to 2:00 am|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
2:01 to 3:00 am|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
3:01 to 4:00 am|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
4:01 to 5:00 am|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
5:01 to 6:00 am|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
6:01 to 7:00 am|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
7:01 to 8:00 am|0.00|294.33|33.83|25.00|5.17|2.17|1.67|1.17|1.00|0.67|0.00|0.00|365.00
8:01 to 9:00 am|0.00|238.00|14.17|16.50|17.00|40.33|29.67|9.33|0.00|0.00|0.00|0.00|365.00
9:01 to 10:00 am|0.00|231.83|14.50|13.00|21.83|38.83|31.83|12.00|1.17|0.00|0.00|0.00|365.00
10:01 to 11:00 am|0.00|226.33|14.00|12.50|17.50|36.50|31.33|22.17|4.67|0.00|0.00|0.00|365.00
11:01 to 12:00 pm|0.00|221.33|12.17|13.00|14.50|30.67|33.50|27.83|11.00|1.00|0.00|0.00|365.00
12:01 to 1:00 pm|0.00|211.00|18.00|14.00|14.00|29.17|32.33|25.67|17.67|3.17|0.00|0.00|365.00
1:01 to 2:00 pm|0.00|208.00|15.00|15.33|11.00|22.17|27.83|27.50|26.83|10.83|0.50|0.00|365.00
2:01 to 3:00 pm|0.00|205.33|11.83|18.17|11.00|17.50|23.17|29.50|26.17|18.17|4.17|0.00|365.00
3:01 to 4:00 pm|0.00|203.00|9.50|17.67|14.00|16.17|20.67|30.17|22.00|24.83|7.00|0.00|365.00
4:01 to 5:00 pm|0.00|201.17|10.83|18.83|13.67|16.33|19.50|29.67|22.83|25.17|7.00|0.00|365.00
5:01 to 6:00 pm|0.00|200.00|13.50|21.33|14.83|22.17|22.67|26.00|25.83|16.33|2.33|0.00|365.00
6:01 to 7:00 pm|0.00|227.33|29.17|43.17|28.33|27.50|9.50|0.00|0.00|0.00|0.00|0.00|365.00
7:01 to 8:00 pm|0.00|235.83|27.83|43.17|28.67|25.33|4.17|0.00|0.00|0.00|0.00|0.00|365.00
8:01 to 9:00 pm|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
9:01 to 10:00 pm|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
10:01 to 11:00 pm|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
11:01 to 12:00 am|0.00|365.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|365.00
Total|0.00|6918.50|224.33|271.67|211.50|324.83|287.83|241.00|159.17|100.17|21.00|0.00|8760.00

Statistics

|Statistic
|---------
Minimum|0.00
Mean minus two standard deviations|-6496.35
Mean|1995.71
Mean plus two standard deviations|10487.78
Maximum|19476.00
Standard deviation|4246.03

## Output:Table:Monthly

Monthly reports are not predefined. Instead each column is defined when using the Output:Table:Monthly object. In that object each column is defined by a variable and how it should be aggregated. The StandardReports.idf file is located in DataSets directory where EnergyPlus is installed and contains a large number of examples of how to use Output:Table:Monthly to get summaries of the performance of different components in the building model. These examples include:

- Zone Cooling Summary
- Zone Heating Summary
- Zone Electric Summary
- Space Gains
- Peak Space Gains
- Space Gain Components at Cooling Peak
- Energy Consumption - Electricity & Natural Gas
- Energy Consumption - Electricity Generated & Propane
- Energy Consumption - Diesel & Fuel Oil
- Energy Consumption  - Purchased Heating & Cooling
- Energy Consumption - Coal & Gasoline
- End-Use Energy Consumption - Electricity
- End-Use Energy Consumption - Natural Gas
- End-Use Energy Consumption - Diesel
- End-Use Energy Consumption - Fuel Oil
- End-Use Energy Consumption - Coal
- End-Use Energy Consumption - Propane
- End-Use Energy Consumption - Gasoline
- Peak Energy End-Use - Electricity Part 1
- Peak Energy End-Use - Electricity Part 2
- Electric Components of Peak Demand
- Peak Energy End-Use - Natural Gas
- Peak Energy End-Use - Diesel
- Peak Energy End-Use - Fuel Oil
- Peak Energy End-Use - Coal
- Peak Energy End-Use - Propane
- Peak Energy End-Use - Gasoline
- Setpoints Not Met With Temperatures
- Comfort Report - Simple 55
- Unglazed Transpired Solar Collector Summary
- Occupant Comfort Data Summary
- Chiller Report
- Tower Report
- Boiler Report
- DX Report
- Window Report
- Window Energy Report
- Window Zone Summary
- Average Outdoor Conditions
- Outdoor Conditions Maximum Drybulb
- Outdoor Conditions Minimum Drybulb
- Outdoor Conditions Maximum Wetbulb
- Outdoor Conditions Maximum Dewpoint
- Outdoor Ground Conditions
- Window AC Report
- Water Heater Report
- Generator Report
- Daylighting Report
- Coil Report
- Plant Loop Demand Report
- Fan Report
- Pump Report
- Cond Loop Demand Report
- Zone Temperature Oscillation Report
- Air Loop System Energy and Water Use
- Air Loop System Component Loads
- Air Loop System Component Energy Use
- Mechanical Ventilation Loads

An example of the Zone Cooling Summary monthly report is shown below.

Report: ZONE COOLING SUMMARY

For: SPACE2-1

Timestamp: 2006-08-29 07:21:57

|ZONE/SYS SENSIBLE COOLING ENERGY [J]|ZONE/SYS SENSIBLE COOLING RATE {MAXIMUM}[W]|ZONE/SYS SENSIBLE COOLING RATE {TIMESTAMP}|OUTDOOR DRY BULB {AT MAX/MIN} [C]|OUTDOOR WET BULB {AT MAX/MIN} [C]|ZONE TOTAL INTERNAL LATENT GAIN [J]|ZONE TOTAL INTERNAL LATENT GAIN {MAXIMUM}[W]|ZONE TOTAL INTERNAL LATENT GAIN {TIMESTAMP}|OUTDOOR DRY BULB {AT MAX/MIN} [C]|OUTDOOR WET BULB {AT MAX/MIN} [C]
|------------------------------------|-------------------------------------------|------------------------------------------|---------------------------------|---------------------------------|-----------------------------------|--------------------------------------------|-------------------------------------------|---------------------------------|---------------------------------
January|281755456.00|1427.84|17-JAN-10:07|8.30|5.48|0.00|0.00|01-JAN-00:15|-4.32|-5.53
February|306879872.00|1440.59|28-FEB-10:46|11.27|6.67|0.00|0.00|01-FEB-00:15|-3.17|-4.91
March|419829728.00|1822.13|26-MAR-09:14|3.90|-0.20|0.00|0.00|01-MAR-00:15|-1.40|-2.84
April|752874688.00|1986.25|29-APR-10:22|21.10|11.44|0.00|0.00|01-APR-00:15|17.12|10.64
May|0.100670E+10|2163.18|22-MAY-09:45|25.17|15.42|0.00|0.00|01-MAY-00:15|3.75|1.60
June|0.100822E+10|2129.73|28-JUN-09:45|28.02|22.39|0.00|0.00|01-JUN-00:15|17.30|14.95
July|0.120671E+10|2312.36|08-JUL-09:00|31.70|23.35|0.00|0.00|01-JUL-00:15|15.60|12.95
August|0.109213E+10|2352.64|02-AUG-09:45|27.08|20.48|0.00|0.00|01-AUG-00:15|17.20|15.70
September|0.100619E+10|2453.23|06-SEP-10:15|28.35|20.53|0.00|0.00|01-SEP-00:15|16.75|15.45
October|771385600.00|2181.25|10-OCT-10:16|15.25|10.12|0.00|0.00|01-OCT-00:15|4.88|3.31
November|384831744.00|1566.35|11-NOV-10:45|14.00|8.89|0.00|0.00|01-NOV-00:15|14.65|11.75
December|246930448.00|1306.75|11-DEC-10:07|5.00|1.69|0.00|0.00|01-DEC-00:15|1.80|-0.11
 | | | | | | | | | | 
Annual Sum or Average|0.848444E+10| | | | |0.00| | | | 
Minimum of Months|246930448.00|1306.75| |3.90|-0.20|0.00|0.00| |-4.32|-5.53
Maximum of Months|0.120671E+10|2453.23| |31.70|23.35|0.00|0.00| |17.30|15.70

## UtilityCost:Tariff

The use of UtilityCost:Tariff objects automatically generates two reports related to the calculation of annual utility costs. The first report is a summary across multiple tariffs and is called the Economics Results Summary report and is shown directly below. After that example is an example of the Tariff report which is created for each of the UtilityCost:Tariff Objects defined in the IDF file  (some columns may be truncated due to page size).

Report: Economics Results Summary Report

For: Entire Facility

Timestamp: 2006-08-29 07:21:57

Annual Cost

|Facility:Electric|Facility:Gas|Other|Total
|-----------------|------------|-----|-----
Cost (\$)|2269.11|612.08|0.00|2881.19
Cost per Total Building Area (\$/m2)|2.45|0.66|0.00|3.11
Cost per Net Conditioned Building Area (\$/m2)|2.45|0.66|0.00|3.11

Tariff Summary

|Selected|Qualified|Meter|Group|Annual Cost
|--------|---------|-----|-----|-----------
EXAMPLEA|No|Yes|ELECTRICITY:FACILITY|(none)|2359.48
EXAMPLEA-GAS|Yes|Yes|GAS:FACILITY|(none)|612.08
EXAMPLEAWITHVARIABLEMONTHLYCHARGE|No|Yes|ELECTRICITY:FACILITY|(none)|2407.36
EXAMPLEB|No|Yes|ELECTRICITY:FACILITY|(none)|2668.34
EXAMPLEC|No|Yes|ELECTRICITY:FACILITY|(none)|3017.45
EXAMPLED|Yes|Yes|ELECTRICITY:FACILITY|(none)|2269.11
EXAMPLEDWITHRATCHET|No|Yes|ELECTRICITY:FACILITY|(none)|3745.90
EXAMPLEE|No|Yes|ELECTRICITY:FACILITY|(none)|3766.76
EXAMPLEF|No|Yes|ELECTRICITY:FACILITY|(none)|3512.12
EXAMPLEFMC|No|Yes|ELECTRICITY:FACILITY|(none)|3512.12
EXAMPLEG|No|Yes|ELECTRICITY:FACILITY|(none)|3866.91

The following is an example of the Tariff report which is created for each of the UtilityCost:Tariff objects defined in the file.

Report: Tariff Report

For: EXAMPLEA

Timestamp: 2006-08-29 07:21:57

|Parameter
|---------
Meter|ELECTRICITY:FACILITY
Selected|No
Group|(none)
Qualified|Yes
Disqualifier|n/a
Computation|automatic
Units|kWh

Categories

|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|Sum|Max
|---|---|---|---|---|---|---|---|---|---|---|---|---|---
EnergyCharges|181.84|159.97|170.98|178.85|207.28|207.97|249.10|234.09|202.57|191.20|169.19|176.34|2329.36|249.10
DemandCharges|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
ServiceCharges|2.51|2.51|2.51|2.51|2.51|2.51|2.51|2.51|2.51|2.51|2.51|2.51|30.12|2.51
Basis|184.35|162.48|173.49|181.36|209.79|210.48|251.61|236.60|205.08|193.71|171.70|178.85|2359.48|251.61
Adjustment|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
Surcharge|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
Subtotal|184.35|162.48|173.49|181.36|209.79|210.48|251.61|236.60|205.08|193.71|171.70|178.85|2359.48|251.61
Taxes|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
Total|184.35|162.48|173.49|181.36|209.79|210.48|251.61|236.60|205.08|193.71|171.70|178.85|2359.48|251.61

Charges

|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|Sum|Max|Category
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|--------
FLATENERGYCHARGE|181.84|159.97|170.98|178.85|207.28|207.97|249.10|234.09|202.57|191.20|169.19|176.34|2329.36|249.10|none

Ratchets

|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|Sum|Max
|---|---|---|---|---|---|---|---|---|---|---|---|---|---

Qualifies

|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|Sum|Max
|---|---|---|---|---|---|---|---|---|---|---|---|---|---

Native Variables

|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|Sum|Max
|---|---|---|---|---|---|---|---|---|---|---|---|---|---
TotalEnergy|3285.70|2890.52|3089.52|3231.79|3745.42|3757.86|4501.09|4229.79|3660.28|3454.82|3057.13|3186.33|42090.24|4501.09
TotalDemand|12.52|12.53|12.53|14.53|17.40|20.71|21.08|19.50|18.63|15.53|12.90|12.55|190.42|21.08
PeakEnergy|3285.70|2890.52|3089.52|3231.79|3745.42|3757.86|4501.09|4229.79|3660.28|3454.82|3057.13|3186.33|42090.24|4501.09
PeakDemand|12.52|12.53|12.53|14.53|17.40|20.71|21.08|19.50|18.63|15.53|12.90|12.55|190.42|21.08
ShoulderEnergy|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
ShoulderDemand|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
OffPeakEnergy|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
OffPeakDemand|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
MidPeakEnergy|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
MidPeakDemand|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
PeakExceedsOffPeak|12.52|12.53|12.53|14.53|17.40|20.71|21.08|19.50|18.63|15.53|12.90|12.55|190.42|21.08
OffPeakExceedsPeak|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
PeakExceedsMidPeak|12.52|12.53|12.53|14.53|17.40|20.71|21.08|19.50|18.63|15.53|12.90|12.55|190.42|21.08
MidPeakExceedsPeak|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
PeakExceedsShoulder|12.52|12.53|12.53|14.53|17.40|20.71|21.08|19.50|18.63|15.53|12.90|12.55|190.42|21.08
ShoulderExceedsPeak|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
IsWinter|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|12.00|1.00
IsNotWinter|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
IsSpring|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
IsNotSpring|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|12.00|1.00
IsSummer|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
IsNotSummer|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|12.00|1.00
IsAutumn|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
IsNotAutumn|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|1.00|12.00|1.00
PeakAndShoulderEnergy|3285.70|2890.52|3089.52|3231.79|3745.42|3757.86|4501.09|4229.79|3660.28|3454.82|3057.13|3186.33|42090.24|4501.09
PeakAndShoulderDemand|12.52|12.53|12.53|14.53|17.40|20.71|21.08|19.50|18.63|15.53|12.90|12.55|190.42|21.08
PeakAndMidPeakEnergy|3285.70|2890.52|3089.52|3231.79|3745.42|3757.86|4501.09|4229.79|3660.28|3454.82|3057.13|3186.33|42090.24|4501.09
PeakAndMidPeakDemand|12.52|12.53|12.53|14.53|17.40|20.71|21.08|19.50|18.63|15.53|12.90|12.55|190.42|21.08
ShoulderAndOffPeakEnergy|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
ShoulderAndOffPeakDemand|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00
PeakAndOffPeakEnergy|3285.70|2890.52|3089.52|3231.79|3745.42|3757.86|4501.09|4229.79|3660.28|3454.82|3057.13|3186.33|42090.24|4501.09
PeakAndOffPeakDemand|12.52|12.53|12.53|14.53|17.40|20.71|21.08|19.50|18.63|15.53|12.90|12.55|190.42|21.08

Other Variables

|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec|Sum|Max
|---|---|---|---|---|---|---|---|---|---|---|---|---|---
NotIncluded|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00|0.00

Computation - AutomaticFLATENERGYCHARGE FROM TotalEnergyEnergyCharges SUM FLATENERGYCHARGEBasis SUM EnergyCharges DemandCharges ServiceChargesSubtotal SUM Basis Adjustment SurchargeTotal SUM Subtotal Taxes

## Construction Cost Estimate Summary

The Construction Cost Estimate Summary report shows the details of a simplified cost estimate of the building along with a comparison with reference building costs entered by the user. An example of the Construction Cost Estimate Summary report is shown below.

Construction Cost Estimate Summary

|Reference Bldg.|Current Bldg. Model|Difference
|---------------|-------------------|----------
Line Item SubTotal (\$)|665082.04|720964.07|55882.03
Misc. Costs (\$)|928979.09|928979.09|0.00
Regional Adjustment (\$)|216792.31|224392.27|7599.96
Design Fee (\$)|108651.21|149946.83|41295.63
Contractor Fee (\$)|126759.74|131203.48|4443.74
Contigency (\$)|181085.34|187433.54|6348.20
Permits, Bonds, Insurance (\$)|0.00|0.00|0.00
Commissioning (\$)|9054.27|18743.35|9689.09
Cost Estimate Total (\$)|2236404.00|2361662.65|125258.65
Cost Per Conditioned Building Area (\$/m2)|1203.69|1271.11|67.42

Cost Line Item Details

|Line No.|Item Name|Quantity.|Units|\$ per Qty.|SubTotal \$
|--------|---------|---------|-----|-----------|-----------
--|1|EXTERIOR WALLS|627.57|m2|168.64|105832.65
--|2|INTERIOR WALLS|854.18|m2|31.16|26616.23
--|3|ROOF|1857.96|m2|104.69|194509.64
--|4|GROUND FLR SLAB|1857.96|m2|56.06|104157.14
--|5|ACOUSTICAL DROP CEILINGS|1857.96|m2|23.79|44200.83
--|6|QUAD HIGH SHGC SUPERWINDOWS|226.22|m2|687.84|155604.96
--|7|QUAD LOW SHGC SUPERWINDOWS|42.68|m2|687.84|29359.55
--|8|CENTRAL CHILLER|153.48|kW (tot cool cap.)|340.00|52183.09
--|9|CONTINUOUS AIR BARRIER|1.00|Ea.|8500.00|8500.00
Line Item SubTotal|--|--|--|--|--|720964.07

## XML Tabular Output

The tables discussed in this section can also be output in an XML format that may be easier for some other programs to extract specific results. An excerpt of the XML file format is shown below. This report presents the same output results as the other tabular report formats but using XML tags.

~~~~~~~~~~~~~~~~~~~~

    <?xml version="1.0"?>
    <EnergyPlusTabularReports>
      <BuildingName>NONE</BuildingName>
      <EnvironmentName>Chicago Ohare Intl Ap IL USA TMY3 WMO#=725300</EnvironmentName>
      <WeatherFileLocationTitle>Chicago Ohare Intl Ap IL USA TMY3 WMO#=725300</WeatherFileLocationTitle>
      <ProgramVersion>EnergyPlus, Version 8.0, YMD=2013.02.18 15:50</ProgramVersion>
      <SimulationTimestamp>
        <Date>
          2013-02-18
        </Date>
        <Time>
          15:50:33
        </Time>
      </SimulationTimestamp>

~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~

    <AnnualBuildingUtilityPerformanceSummary>
      <for>Entire Facility</for>
      <SiteAndSourceEnergy>
        <name>TotalSiteEnergy</name>
        <TotalEnergy units="kBtu">200671.97</TotalEnergy>
        <EnergyPerTotalBuildingArea units="kBtu/ft2">143.32</EnergyPerTotalBuildingArea>
        <EnergyPerConditionedBuildingArea units="kBtu/ft2">143.32</EnergyPerConditionedBuildingArea>
      </SiteAndSourceEnergy>
      <SiteAndSourceEnergy>
        <name>NetSiteEnergy</name>
        <TotalEnergy units="kBtu">200671.97</TotalEnergy>
        <EnergyPerTotalBuildingArea units="kBtu/ft2">143.32</EnergyPerTotalBuildingArea>
        <EnergyPerConditionedBuildingArea units="kBtu/ft2">143.32</EnergyPerConditionedBuildingArea>
      </SiteAndSourceEnergy>
      <SiteAndSourceEnergy>
        <name>TotalSourceEnergy</name>
        <TotalEnergy units="kBtu">442504.19</TotalEnergy>
        <EnergyPerTotalBuildingArea units="kBtu/ft2">316.04</EnergyPerTotalBuildingArea>
        <EnergyPerConditionedBuildingArea units="kBtu/ft2">316.04</EnergyPerConditionedBuildingArea>
      </SiteAndSourceEnergy>
      <SiteAndSourceEnergy>
        <name>NetSourceEnergy</name>
        <TotalEnergy units="kBtu">397810.98</TotalEnergy>
        <EnergyPerTotalBuildingArea units="kBtu/ft2">284.12</EnergyPerTotalBuildingArea>
        <EnergyPerConditionedBuildingArea units="kBtu/ft2">284.12</EnergyPerConditionedBuildingArea>
      </SiteAndSourceEnergy>
~~~~~~~~~~~~~~~~~~~~

*An actual file will be much longer than this example but follows this format.*