Allow Different Units in SQL and HTML Tabular Reports
================

**Jason Glazer, GARD Analytics**

 - May 12, 2020
 

## Justification for New Feature ##

Some users want the EnergyPlus tabular reports in IP units but user interfaces for EnergyPlus may want
the SQL in SI units for the same simulation. OpenStudio for example, does post processing and depends 
on the tabular reports in the SQL file being in SI units. The mixture of both units in output has been 
requested by user interface developers. 

See:

https://github.com/NREL/EnergyPlus/issues/5435

and

https://github.com/NREL/OpenStudio/issues/2039


## E-mail and  Conference Call Conclusions ##

None

## Overview ##

Continue to allow EnergyPlus HTML tabular reports in various SI and IP units while adding the ability to keep
the SQL output file in native SI units. 

## Approach ##

Many different input objects control the production of tabular reports in EnergyPlus including:

- OutputControl:Table:Style
- Output:JSON
- Output:SQLite
- OutputControl:IlluminanceMap:Style

And additional input objects control the production of timeseries data. A current effort is underway
to directly provide timeseries data in CSV format instead of ESO/MTR format:

https://github.com/NREL/EnergyPlus/pull/7904

While some refactoring of the input objects that control reporting have been considered and perhaps
should be considered in the future, no specific plan is recommended at this point. 
In order to satisfy the original need identified of producing SQL files for tabular reports in SI 
units even if the HTML file is in IP units, a small change to the input structure is required.

A second field will be added to the Output:SQLite object that will control unit conversion for the 
SQL file.

```
Output:SQLite,
       \memo Output from EnergyPlus can be written to an SQLite format file.
       \unique-object
  A1 , \field Option Type
       \type choice
       \key Simple
       \key SimpleAndTabular
  A2;  \field Tabular Unit Conversion
       \type choice
       \key UseOutputControlTableStyle
       \key None
       \key JtoKWH
       \key JtoMJ
       \key JtoGJ
       \key InchPound
       \default UseOutputControlTableStyle

```

The default of UseOutputControlTableStyle will let the tables in the SQL file follow the unit conversion 
specified in the OutputControl:Table:Style just as it currently does but the other options would allow different 
unit conversions or no unit conversions at all to apply to the tables in the SQL file. The timeseries data
in the SQL file does not have, and will not have, unit conversions performed.


## Testing/Validation/Data Sources ##

Will test that the unit conversion in the SQL file for tables is correct.

## Input Output Reference Documentation ##

Add a description of the proposed field to the Output:SQLite input object.

## Engineering Reference ##

No changes

## Example File and Transition Changes ##

No transition changes expected. An example file will be modified to include this option.

## References ##

None



