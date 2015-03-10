# Weather File Data Reporting (errors) during Simulation

Missing data on the weather file used will be summarized on the **eplusout.err** file. In EnergyPlus, "missing data" is shown only for fields that EnergyPlus will use. For the "WeatherCodes", an invalid field count (where the number of items in the field does not = 9) will be shown. The number of items count refers to the number of records on the weather file that are in error or missing – for an hourly weather file, this is the number of hours. Likewise out of range values (see specific fields in the previous definition) will be counted for each occurance and summarized. Note that the out of range values will not be changed by EnergyPlus and could affect your simulation results.

For example:

~~~~~~~~~~~~~~~~~~~~

       ** Warning ** Missing Data Found on Weather Data File
       ************* Missing Atmospheric Pressure, Number of items=   48
       ************* Missing Dry Bulb Temperatures, Number of items=   4
       ** Warning ** Out of Range Data Found on Weather Data File
       ************* Out of Range Dry Bulb Temperature [>-70,<70], Number of items=   1
~~~~~~~~~~~~~~~~~~~~