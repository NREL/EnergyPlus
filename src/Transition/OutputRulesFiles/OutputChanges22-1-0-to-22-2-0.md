Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.


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
