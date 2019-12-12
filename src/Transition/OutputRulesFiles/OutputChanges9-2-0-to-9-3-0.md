Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Additional field for Coil Sizing Details:Coils report

A new column "Peak Load Type to Size On" has been added to the `Coil Sizing Details:Coils` report

See [7397](https://github.com/NREL/EnergyPlus/pull/7397)

### Name re-ordering in the sizing labels and modified variable names for MulitSpeed DX Cooling and Heating Coils

The change impacts rated capacity, rated flow rate, rated sensible heat ratio, etc, see sample changes below


*Before:*
```
'Speed X' Design Size Design Size Rated Total Cooling Capacity
'Speed X' Design Size Gross Rated Heating Capacity
'Speed X' Design Size Rated Air Flow Rate
'Speed X' Design Size Rated Sensible Heat Ratio
'Speed X' Design Size Evaporative Condenser Air Flow Rate
'Speed X' Design Size Rated Evaporative Condenser Pump Power Consumption

'Speed X' User-Specified User-Specified Total Cooling Capacity
'Speed X' User-Specified Gross Rated Heating Capacity
'Speed X' User-Specified Rated Air Flow Rate
'Speed X' User-Specified Rated Sensible Heat Ratio
'Speed X' User-Specified Evaporative Condenser Air Flow Rate
'Speed X' User-Specified Rated Evaporative Condenser Pump Power Consumption
```

*After:*
```
Design Size 'Speed X' Gross Rated Total Cooling Capacity
Design Size 'Speed X' Gross Rated Heating Capacity
Design Size 'Speed X' Rated Air Flow Rate
Design Size 'Speed X' Rated Sensible Heat Ratio
Design Size 'Speed X' Evaporative Condenser Air Flow Rate
Design Size 'Speed X' Rated Evaporative Condenser Pump Power Consumption

User-Specified 'Speed X' Gross Rated Total Cooling Capacity
User-Specified 'Speed X' Gross Rated Heating Capacity
User-Specified 'Speed X' Rated Air Flow Rate
User-Specified 'Speed X' Rated Sensible Heat Ratio
User-Specified 'Speed X' Evaporative Condenser Air Flow Rate
User-Specified 'Speed X' Rated Evaporative Condenser Pump Power Consumption
```

### EIO output for SizingPeriod:DesignDay

In the eio and table output related to `SizingPeriod:DesignDay`, the Humidity Indicating Type and Value are now correctly reported. Example:

*Before:*
```
! <Environment:Design Day Data>, Max Dry-Bulb Temp {C}, Temp Range {dC}, Temp Range Ind Type, Hum Ind Value at Max Temp, Hum Ind Type,Pressure {Pa}, Wind Direction {deg CW from N}, Wind Speed {m/s}, Clearness, Rain, Snow
Environment:Design Day Data,33.00,6.60,DefaultMultipliers,100511,220,3.2,0.00,No,No
```

If we realign to the header, we see that two values related to Humidity Indicating Value were missing.

```
# Realign to header
Environment:Design Day Data,33.00,6.60,DefaultMultipliers,<MISSING>,<MISSING>,100511,220,3.2,0.00,No,No
```

*v9.3.0:* The `Hum Ind Type` and `Hum Ind Value at Max Temp` are swapped and actually output to the EIO, and the `Hum Ind Units` is added:

```
! <Environment:Design Day Data>, Max Dry-Bulb Temp {C}, Temp Range {dC}, Temp Range Ind Type, Hum Ind Type, Hum Ind Value at Max Temp, Hum Ind Units, Pressure {Pa}, Wind Direction {deg CW from N}, Wind Speed {m/s}, Clearness, Rain, Snow
Environment:Design Day Data,33.00,6.60,DefaultMultipliers,Enthalpy,90500.00,{J/kgDryAir},100511,220,3.2,0.00,No,No
```

See [#PR7577](https://github.com/NREL/EnergyPlus/pull/7577)

### End Use By Subcategory in SQL

In the SQL Output file, for `ReportName = "AnnualBuildingUtilityPerformanceSummary"` and `ReportName = "DemandEndUseComponentsSummary"`,
the tables `TableName = "End Uses by Subcategory"` no longer have blank `RowName` corresponding to the End Use (eg: Heating, Interior Lighting, etc) for additional End Use Subcategories.
This will allow querying a specific End Use Subcategory in the SQL file.

Example SQL Query to return all rows (one row per fuel type) for End Use "Interior Lighting", Subcategory "GeneralLights":

```
SELECT * FROM TabularDataWithStrings
  WHERE TableName = "End Uses By Subcategory"
  AND ReportName = "AnnualBuildingUtilityPerformanceSummary"
  AND RowName = "Interior Lighting"
  AND (TabularDataIndex - (SELECT TabularDataIndex FROM TabularDataWithStrings
                              WHERE TableName = "End Uses By Subcategory"
                              AND ReportName = "AnnualBuildingUtilityPerformanceSummary"
                              AND ColumnName = "Subcategory"
                              AND RowName = "Interior Lighting"
                              AND Value = "GeneralLights"))
      % (SELECT COUNT(Value) FROM TabularDataWithStrings
                              WHERE TableName = "End Uses By Subcategory"
                              AND ReportName = "AnnualBuildingUtilityPerformanceSummary"
                              AND ColumnName = "Subcategory")
```

See [PR#7584](https://github.com/NREL/EnergyPlus/pull/7584).
