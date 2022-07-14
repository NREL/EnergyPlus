Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Added a few new tables in the annual thermal/visual/CO2 resilience reporting in the tabular output. Added thermal/visual/CO2 resilience reporting for user-specified reporting period.

For the annual thermal resilience reports, added the following tables
- Heat Index OccupiedHours
- Humidex OccupiedHours
- Hours of Safety for Cold Events
- Hours of Safety for Heat Events
- Unmet Degree-Hours
- Discomfort-weighted Exceedance OccupantHours
- Discomfort-weighted Exceedance OccupiedHours

For the annual CO2 resilience report, the following table is added
- CO2 Level OccupiedHours

For the annual Visual resilience report, the following table is added
- Illuminance Level OccupiedHours

For the period-specific reports, when users choose ThermalResilienceSummary, CO2ResilienceSummary, or VisualResilienceSummary, the same set of tables as the annual thermal, CO2, or visual resilience table will be generated for the specified reporting period. When the "AllResilienceSummaries" is chosen for the report name field, all of the three sets of reports will be generated for the specified reporting period.

A "Reporting Period Time and Consumption" table is created to summarize the type, time range, and total electricity consumption for all reporting periods.

Please see PR [#9156](https://github.com/NREL/EnergyPlus/pull/9156).

### SurfaceProperty:GroundSurfaces New Object Output Variables

Two new output variables related to the `SurfaceProperty:GroundSurfaces` have been added along with the new feature development. These new variables are:

- Surfaces Property Ground Surfaces Average Temperature [C]
- Surfaces Property Ground Surfaces Average Reflectance []

See pull request [#9409](https://github.com/NREL/EnergyPlus/pull/9409) for more details.
