Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.


### System Sizing Information in EIO Changed 

The System Sizing Information rows in the EIO file has been rearranged and expanded. Previously each row showed one value with a field description for what that value was included in each row in the format System Name, Field Description, Value. Also the outputs were just air flow rate, calculated and user defined, and heating and cooling. Now rows are like Zone Sizing Information rows and show many columns of values for each system. A row appears for each system for heating and another for cooling. The columns include the System Name, Load Type, Peak Load Kind, User Design Capacity, Calc Des Air Flow Rate [m3/s], User Des Air Flow Rate [m3/s], Design Day Name, Date/Time of Peak.

See [5923](https://github.com/NREL/EnergyPlus/pull/5923). 


