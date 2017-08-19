Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### New and Enhanced Load Component Summary Reports

The existing Zone Component Loads Summary report has been enhanced and new reports titled AirLoop Component Loads Summary and Facility Component Loads Summary reports have been added. The main "Estimated Peak Load Components" subtables has two new columns for related areas and total per area. These show values for the envelope related rows of the table and for the internal loads (which use the floor area). The "Peak Conditions" subtable has additional temperatures such as the supply air temperature. The "Peak Conditions" subtable has main fan air flow and outdoor airflow. A new "Engineering Checks" subtable that shows the percent outside air, airflow per floor area, airflow per  capacity, area per capacity and number of people. AirLoop Component Loads Summary table shows similar results that are based on the combined airloop results and shows which zones are included in the results. Similarly, the Facility Component Loads Summary report shows the results for the entire facility being modeled.

See [5923] (https://github.com/NREL/EnergyPlus/pull/5923)

### Adds additional records of User-Specified component sizing output to SQLite output file. 

In the Component Sizing table report, autosized fields may report values for "Design Size", "User-Specified", or both.  Previously, for fields with both values, only the "Design Size" value was included in the Component Sizing table in the SQLite output.  Now both values are included in the SQLite output.

See [6147](https://github.com/NREL/EnergyPlus/pull/6147)

