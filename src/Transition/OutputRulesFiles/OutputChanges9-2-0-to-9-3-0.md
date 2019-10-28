Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Additional field for Coil Sizing Details:Coils report

A new column "Peak Load Type to Size On" has been added to the `Coil Sizing Details:Coils` report

See [7397](https://github.com/NREL/EnergyPlus/pull/7397)

### End Use By Subcategory in SQL

In the SQL Output file, for `ReportName = "AnnualBuildingUtilityPerformanceSummary"` and `ReportName = "DemandEndUseComponentsSummary"`,
the tables `TableName = "End Uses by Subcategory"` no longer have blank `RowName` corresponding to the End Use (eg: Heating, Interior Lighting, etc) for additional End Use Subcategories.
This will allow querying a specific End Use Subcategory in the SQL file.

Example SQL Query to return all rows (one row per fuel type) for End Use "Interior Lighting", Sucategory "GeneralLights":
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
