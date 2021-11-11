Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### EIO Changes for WindowConstruction

Two new fields were added to the EIO report for `<WindowConstruction>` entries:
`Conductance (Before Adjusted) {W/m2-K}` and `Convection Coefficient Adjustment Ratio`.
See pull request [#9117](https://github.com/NREL/EnergyPlus/pull/9117/files) for more details.
