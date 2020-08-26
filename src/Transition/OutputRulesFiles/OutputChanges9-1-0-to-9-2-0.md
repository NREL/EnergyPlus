Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### View Factor Output
In the eio and table output related to surface view factors "Zone Name" is now "Zone/Enclosure Name". For example:

*Before:*
```
! <Surface View Factor Check Values>,Zone Name,Original Check Value,Calculated Fixed Check Value,Final Check Value,Number of Iterations,Fixed RowSum Convergence,Used RowSum Convergence
 Surface View Factor Check Values,CORE_ZN,8.881784E-016,9.093943E-004,9.093943E-004,33,9.093943E-004,9.093943E-004
```

*v9.2.0:*
```
! <Surface View Factor Check Values>,Zone/Enclosure Name,Original Check Value,Calculated Fixed Check Value,Final Check Value,Number of Iterations,Fixed RowSum Convergence,Used RowSum Convergence
 Surface View Factor Check Values,CORE_ZN,8.881784E-016,9.093943E-004,9.093943E-004,33,9.093943E-004,9.093943E-004
```

For simulations with no Construction:AirBoundary objects using the GroupedZone radiant exchange option, the
data rows will remain the same, using the zone name. For grouped zones, the enclosure name will be used. This
may be automatically generated as "Enclosure 1" or user-specified if ZoneProperty:UserViewFactors:bySurfaceName and a
corresponding ZoneList are included for the grouped zones.

### New Solar View Factor Output
In the eio and table output related to surface view factors, there is a new section for "Solar View Factors" which is similar to the radiant view factors above.
