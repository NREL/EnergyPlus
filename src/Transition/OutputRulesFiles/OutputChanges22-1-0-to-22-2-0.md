Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Template Title

Change description goes here, use links to the PR such as [#9117](https://github.com/NREL/EnergyPlus/pull/9117/files).

### SurfaceProperty:GroundSurfaces New Object Output Variables

Two new output variables related to the `SurfaceProperty:GroundSurfaces` have been added along with the new feature development. These new variables are:

- Surfaces Property Ground Surfaces Average Temperature [C]
- Surfaces Property Ground Surfaces Average Reflectance []

See pull request [#9409](https://github.com/NREL/EnergyPlus/pull/9409) for more details.

### Tabular report changes in Output:SQLite and Output:JSON

#### Renames in SQLite

The report `Initialization Summary` which was the only one with a space in its name was renamed to `InitializationSummary`.

#### New reports in JSON output

`AnnualHeatEmissionsReport` and `InitializationSummary` reports were **added** to the JSON output.

#### Renames in JSON

Six reports have been renamed to remove spaces:

```diff
-Annual Building Utility Performance Summary
+AnnualBuildingUtilityPerformanceSummary

-Component Sizing Summary
+ComponentSizingSummary

-Demand End Use Components Summary
+DemandEndUseComponentsSummary

-Input Verification and Results Summary
+InputVerificationandResultsSummary

-Source Energy End Use Components Summary
+SourceEnergyEndUseComponentsSummary

-Surface Shadowing Summary
+SurfaceShadowingSummary
```

#### Renames in both SQLite and JSON

Under the report `SourceEnergyEndUseComponentsSummary`, the table `Source Energy End Use Component Per Conditioned Floor Area` was renamed to `Source Energy End Use Components Per Conditioned Floor Area` to match the HTML output and the other Source Energy End Use Component**s** tables.

The Adaptive Comfort Summary/Report was renamed to match the HTML:

```diff
-AdaptiveComfortReport,People Summary
+AdaptiveComfortSummary,Time Not Meeting the Adaptive Comfort Models during Occupied Hours
```

See pull request [#9461](https://github.com/NREL/EnergyPlus/pull/9461) for more details.

### Tabular Report Outdoor Air Summary - New Column
In both subtables of the Outdoor Air Summary report, these is a new column at the far right for *AFN Ventilation [ach]*. AirflowNetwork infiltraion (cracks) and ventilation (openings) flow rates are now reported separately. Previously, they were combined in the AFN Infiltration column. 

Also, the AFN Infiltration values were corrected to ignore zone multipliers since this report is per zone without multipliers. See issue [#8528](https://github.com/NREL/EnergyPlus/issues/8528) for more details.

See pull request [#9519](https://github.com/NREL/EnergyPlus/pull/9519) for more details.

### Tabular Report Outdoor Air Details - New Footnotes
The Outdoor Air Details report, the subtables "by Zone" have a new footnote:

*Values shown for a single zone without multipliers. Total Facility includes multipliers*

In the Outdoor Air Details, Total Outdoor Air by AirLoop subtable, Mechanical Ventilation and Total Ventilation values were corrected to properly apply density. This results in changes to the values in the three Time Below/At/Above Voz-sum-dyn columns as well. See issue [#9398](https://github.com/NREL/EnergyPlus/issues/9398) for more details.

See pull request [#9519](https://github.com/NREL/EnergyPlus/pull/9519) for more details.

