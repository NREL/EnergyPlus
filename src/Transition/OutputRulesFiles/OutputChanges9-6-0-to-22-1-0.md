Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### EIO and Initialization Summary Report Changes for WindowConstruction

Two new fields were added to the EIO report for `<WindowConstruction>` entries:
`Conductance (Before Adjusted) {W/m2-K}` and `Convection Coefficient Adjustment Ratio`.
See pull request [#9117](https://github.com/NREL/EnergyPlus/pull/9117/files) for more details.


### EIO and Initialization Summary Report Changes for Daylighting

Daylighting outputs in the EIO and Initialization Summary Report which were by Zone are now by Enclosure or Daylighting Control. 
This changes the headers and tags. It may also change the number, naming, and order of entries for each section.

Also, for Sky Daylight Factors, "Sky Type" was missing from the header.

```
! <Zone/Window Adjacency Daylighting Counts>, Zone Name, Number of Exterior Windows, Number of Exterior Windows in Adjacent Zones
Zone/Window Adjacency Daylighting Counts, ZN_1_FLR_1_SEC_1,1,0

! <Zone/Window Adjacency Daylighting Matrix>, Zone Name, Number of Adjacent Zones with Windows,Adjacent Zone Names - 1st 100 (max)
Zone/Window Adjacency Daylighting Matrix, ZN_1_FLR_1_SEC_1,0

! <Sky Daylight Factors>, MonthAndDay, Zone Name, Window Name, Reference Point, Daylight Factor
Sky Daylight Factors,Clear Sky,01/21,ZN_1_FLR_1_SEC_1,ZN_1_FLR_1_SEC_1_WALL_1_WINDOW_1,ZN_1_FLR_1_SEC_1_DAYLREFPT1,0.1352


```

are now

```
! <Enclosure/Window Adjacency Daylighting Counts>, Enclosure Name, Number of Exterior Windows, Number of Exterior Windows in Adjacent Enclosures
Enclosure/Window Adjacency Daylighting Counts, ZN_1_FLR_1_SEC_1,1,0

! <Enclosure/Window Adjacency Daylighting Matrix>, Enclosure Name, Number of Adjacent Enclosures with Windows,Adjacent Enclosure Names - 1st 100 (max)
Enclosure/Window Adjacency Daylighting Matrix, ZN_1_FLR_1_SEC_1,0

! <Sky Daylight Factors>, Sky Type, MonthAndDay, Daylighting Control Name, Enclosure Name, Window Name, Reference Point, Daylight Factor
Sky Daylight Factors,Clear Sky,01/21,ZN_1_FLR_1_SEC_1_DAYLCTRL,ZN_1_FLR_1_SEC_1,ZN_1_FLR_1_SEC_1_WALL_1_WINDOW_1,ZN_1_FLR_1_SEC_1_DAYLREFPT1,0.1352
```

See pull request [#9102](https://github.com/NREL/EnergyPlus/pull/9102/files) for more details.


### DXF Changes for Daylighting Reference Points

The following changes were made to Daylighting reference points in dxf output:
  * Duplicate daylighting reference points are removed
  * The actual reference point names are used in the label, rather than RefPt1, 2, etc.

See pull request [#9102](https://github.com/NREL/EnergyPlus/pull/9102/files) for more details.

### DFS Changes for Daylighting

For `Output:DaylightFactors`, the daylight factors are now reported by Enclosure instead of by Zone, and Enclosure Name is added as a new field along with Zone Name.
This may alter the order and number of entries.

```
This file contains daylight factors for all exterior windows of daylight zones.
MonthAndDay,Zone Name,Window Name,Window State
Hour,Reference Point,Daylight Factor for Clear Sky,Daylight Factor for Clear Turbid Sky,Daylight Factor for Intermediate Sky,Daylight Factor for Overcast Sky
01/21,PERIMETER_BOT_ZN_1,PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW,Base Window
```

are now

```
This file contains daylight factors for all exterior windows of daylight enclosures.
MonthAndDay,Enclosure Name,Zone Name,Window Name,Window State
Hour,Reference Point,Daylight Factor for Clear Sky,Daylight Factor for Clear Turbid Sky,Daylight Factor for Intermediate Sky,Daylight Factor for Overcast Sky
01/21,PERIMETER_BOT_ZN_3,PERIMETER_BOT_ZN_3,PERIMETER_BOT_ZN_3_WALL_NORTH_WINDOW,Base Window
```

See pull request [#9102](https://github.com/NREL/EnergyPlus/pull/9102/files) for more details.

### Row Order Changes for Lighting Summary, Daylighting subtable

The row order may change in this subtable.

See pull request [#9102](https://github.com/NREL/EnergyPlus/pull/9102/files) for more details.

