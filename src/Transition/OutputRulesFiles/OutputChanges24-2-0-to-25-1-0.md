Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited. It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo. At least a PR number or whatever.

### Output:Constructions for WindowMaterial:Shade
Data for Output:Constructions for WindowMaterial:Shade were misaligned with the column headings in the eio output and the table Initialization Summary report. Data has shifted one column to the left.

See PR [#10750](https://github.com/NREL/EnergyPlus/pull/10750)
