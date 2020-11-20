Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.


### Surface reporting for Construction:AirBoundary
The reporting order has changed for surfaces which use Construction:AirBoundary. These surfaces will be listed before other 
surfaces in the same zone. This affects table outputs, eio, shd, wrl, and dxf.

See [8370](https://github.com/NREL/EnergyPlus/pull/8370)

