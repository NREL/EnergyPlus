Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Additional fields for Shadowing/Sun Position Calculations Annual Simulations

The existing Shadowing/Sun Position Calculations Annual Simulations report in the eio output has additional field at the end:
External Shading Calculation Method, Output External Shading Calculation Results, Disable Self-Shading Within Shading Zone Groups, Disable Self-Shading From Shading Zone Groups to Other Zones

See [6390](https://github.com/NREL/EnergyPlus/pull/6390)

### Annual reporting frequency

Output variables and meters can now be reported yearly. The existing `annual` choice is used, which previously was a synonym for `runperiod`. This means that existing input files may report different information than previously if the simulation does not run from 1/1 to 12/31. Input files that have both `runperiod` and `annual` reporting requested will now produce two reports when previously only one report would have been made. The output files are modified as follows:

 * Text (ESO, etc.): A new yearly timestamp was added, so entry numbering has been changed. Timestamps for other frequencies is unchanged.
 * SQL: All timestamps have been modified to include a year field. This field is not filled in all situations (e.g. `runperiod` frequency), but users of this format will need to adjust accordingly. See `SQLite::initializeTimeIndicesTable` in SQLiteProcedures.cc.

See [6456](https://github.com/NREL/EnergyPlus/pull/6456)

