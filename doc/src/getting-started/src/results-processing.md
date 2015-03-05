# Results Processing

Results from EnergyPlus (using EP-Launch) appear in several possible formats.  The most basic are the csv files for the time oriented output and the meter output.  These will appear as <filename>.csv and <filename>Meter.csv.  These can be quite detailed files (ref: Output:Variable, Output:Meter commands).  Other formats (such as Tabular outputs) can yield more summarized results.  These files and contents are described in more detail in the "Output Details and Examples" document.

As an example, here is what the normal "csv" file might look like in Excel™:

![Results in Spreadsheet format](media/results-in-spreadsheet-format.png)


Likewise, a tabular output (usually in HTML format – which can be read by any web browser) might look like:

**End Uses**

Electricity (GJ)
Natural Gas (GJ)
Other Fuel (GJ)
Purchased Cooling (GJ)
Purchased Heating (GJ)
Water (m3)

Heating
0.00
95.17
0.00
0.00
0.00
0.00

Cooling
56.78
0.00
0.00
0.00
0.00
0.00

Interior Lighting
124.39
0.00
0.00
0.00
0.00
0.00

Exterior Lighting
0.00
0.00
0.00
0.00
0.00
0.00

Interior Equipment
28.27
0.00
0.00
0.00
0.00
0.00

Exterior Equipment
0.00
0.00
0.00
0.00
0.00
0.00

Fans
73.52
0.00
0.00
0.00
0.00
0.00

Pumps
0.00
0.00
0.00
0.00
0.00
0.00

Heat Rejection
0.00
0.00
0.00
0.00
0.00
0.00

Humidification
0.00
0.00
0.00
0.00
0.00
0.00

Heat Recovery
0.00
0.00
0.00
0.00
0.00
0.00

Water Systems
0.08
85.39
0.00
0.00
0.00
363.07

Refrigeration
0.00
0.00
0.00
0.00
0.00
0.00

Generators
0.00
0.00
0.00
0.00
0.00
0.00

 
 
 
 
 
 
 

Total End Uses
283.03
180.56
0.00
0.00
0.00
363.07