Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Additional fields for Shadowing/Sun Position Calculations Annual Simulations

The existing Shadowing/Sun Position Calculations Annual Simulations report in the eio output has additional field at the end:
External Shading Calculation Method, Output External Shading Calculation Results, Disable Self-Shading Within Shading Zone Groups, Disable Self-Shading From Shading Zone Groups to Other Zones

See [6390](https://github.com/NREL/EnergyPlus/pull/6390)

