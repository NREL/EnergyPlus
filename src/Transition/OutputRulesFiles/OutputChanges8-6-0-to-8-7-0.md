Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.


### Daylighting Table Fixes in Lighting Summary tabular report

The column labeled Zone used to show the daylighting control name but has been fixed to show the zone name. In addition, the control name now appears in a column with the heading control name. Also the column heading "Daylighting Type" was changed to "Daylighting Method" to be more consistent with the input field of that it is reporting. This column also was always just showing "Detailed" previously and now shows either SplitFlux or DElight as appropriate.

See [5945](https://github.com/NREL/EnergyPlus/pull/5945). 


