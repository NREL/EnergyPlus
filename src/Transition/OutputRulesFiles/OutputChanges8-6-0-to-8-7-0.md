Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### New table, Hybrid Model: Internal Thermal Mass, is added to tabular report. 

The table is only shown when hybrid model: internal thermal mass is flagged.  It has two columns, the first column indicates Hybrid Modeling (Y/N) and the second column provides Temperature Capacitance Multiplier values for each zone. 

See [5882](https://github.com/NREL/EnergyPlus/pull/5882)

