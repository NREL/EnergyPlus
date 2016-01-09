Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Timestamp tag added to tabular headers

With Pull Request [#5295](https://github.com/NREL/EnergyPlus/pull/5295), some tabular report headers were changed to include the tag ```{TIMESTAMP}``` to make them more consistent with the use of the that tag with monthly and annual reports when the minimum or maximum was shown. This impacts the HVAC Sizing Summary tables, the Plant Loop Coincident Design Fluid Flow Rate Adjustments table,  the Energy Meters tables, and the Sensible Heat Gain Summary tables.

### TimeBins value rounding

The general algorithm for choosing when an output variable is in various time bins was altered slightly with this change. Now rounding occurs to the same number of significant digits as shown in the headings of the TimeBin reports. For example, the value of 4.997 would have been put in the 4.00 to 5.00 to  bin previously since less than 5 but now would be put in the 5.00 to 5.99 bin since the rounded value would be 5.00.  This stabilizes the total hours in particular bins when comparing cases that vary slightly. See Pull Request [#5295](https://github.com/NREL/EnergyPlus/pull/5295) for the actual code change.

### Add units and change output variable name for Availability Manager Optimum Start

The output variable which was previously "Availability Manager Optimum Start Hours Before Occupancy []" has been changed to be more consistent with other length of time output variable names, and units have been added:
"Availability Manager Optimum Start Time Before Occupancy [hr]".

### Plant Pump Models

The plant pump models have had some wording changes.  Use of the word *Rated* has been changed to *Design*. The reason is that these values are not really ratings in the sense that they correspond to the results of a standard rating system or a specific rating point that might appear in product catalog data.  Rather they are just the design values used in the pump model calculations.  This affects not only the input fields in the pump objects, but also the strings reported for sizing outcomes.

For version 8.5, the strings used to report pump sizes have changed In the EIO file and table summary reports in the following ways:

    Rated Flow Rate [m3/s] => Design Flow Rate [m3/s]
    Initial Rated Flow Rate [m3/s] => Initial Design Flow Rate [m3/s]
    Rated Power Consumption [W] => Design Power Consumption [W]
    Initial Rated Power Consumption [W] => Initial Design Power Consumption [W]
