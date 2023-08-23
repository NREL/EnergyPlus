Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### EIO System Sizing Information User Design Capacity header
Missing unit is added to the EIO ystem Sizing Informationg table "User Design Capacity" header as shown below: 
- <System Sizing Information>, System Name, Load Type, Peak Load Kind, User Design Capacity [W], Calc Des Air Flow Rate [m3/s], User Des Air Flow Rate [m3/s], Design Day Name, Date/Time of Peak
- This change also impacts the html tabular output report file: report name "Initialization Summary" and table name "System Sizing Information". 

See pull request [#9967](https://github.com/NREL/EnergyPlus/pull/9967) for more details.

### Space Sizing Output in EIO and Tables
When `ZoneAirHeatBalanceAlgorithm` "Do Space Heat Balance for Sizing" = Yes, zone sizing is also done for all spaces. The HVAC Sizing Summary table report will include subtables for Space Sensible Cooling and Heating as well as for Zone Sensible Cooling and Heating. Space Sizing will also be reported to the eio output.

See pull request [#9982](https://github.com/NREL/EnergyPlus/pull/9982) for more details.

