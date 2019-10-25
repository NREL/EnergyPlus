Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Additional field for Coil Sizing Details:Coils report

A new column "Peak Load Type to Size On" has been added to the `Coil Sizing Details:Coils` report

See [7397](https://github.com/NREL/EnergyPlus/pull/7397)

### EIO output for SizingPeriod:DesignDay

In the eio and table output related to `SizingPeriod:DesignDay`, the Humidity Indicating Type and Value are now correctly reported. Example:

*Before:*
```
! <Environment:Design Day Data>, Max Dry-Bulb Temp {C}, Temp Range {dC}, Temp Range Ind Type, Hum Ind Value at Max Temp, Hum Ind Type,Pressure {Pa}, Wind Direction {deg CW from N}, Wind Speed {m/s}, Clearness, Rain, Snow
Environment:Design Day Data,33.00,6.60,DefaultMultipliers,100511,220,3.2,0.00,No,No
```

If we realign to the header, we see that two values related to Humidity Indicating Value were missing.

```
# Realign to header
Environment:Design Day Data,33.00,6.60,DefaultMultipliers,<MISSING>,<MISSING>,100511,220,3.2,0.00,No,No
```

*v9.3.0:* The `Hum Ind Type` and `Hum Ind Value at Max Temp` are swapped and actually output to the EIO:

```
! <Environment:Design Day Data>, Max Dry-Bulb Temp {C}, Temp Range {dC}, Temp Range Ind Type, Hum Ind Type, Hum Ind Value at Max Temp, Pressure {Pa}, Wind Direction {deg CW from N}, Wind Speed {m/s}, Clearness, Rain, Snow
Environment:Design Day Data,33.00,6.60,DefaultMultipliers,Enthalpy,90500.00 {J/kg},100511,220,3.2,0.00,No,No
```

See [#PR7577](https://github.com/NREL/EnergyPlus/pull/7577)
