Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.


### New Underground Gross Wall Area Added to Zone Summary Table in Input Verification and Results Summary Report

The Zone Summary table in the Input Verification and Results Summary Report always had a column labeled "Gross Wall Area" which was the above ground wall area for each zone. This change relabeled that column to be called "Above Ground Gross Wall Area" and added an entirely new column called "Underground Gross Wall Area" that shows the amount of underground wall area for each zone.

See [5588](https://github.com/NREL/EnergyPlus/pull/5588 "5588"). 


### Life-Cycle Cost Report Updated for Water

1. The "Energy Cost Cash Flows (Without Escalation)" table has been renamed "Energy and Water Cost Cash Flows (Without Escalation)". 

2. The "Operating Cash Flow by Category (Without Escalation)" table used to combine energy and water costs in the Energy column and have zeros in the Water column. Now energy costs are in the Energy column and water costs are in the Water column. 

3. The "Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)" table used to show water costs with the Energy in the Category column and Energy Cost in the Kind column but now show Water and Water Cost in those respective columns. 

4. The Present Value by Category table used to include the sum of energy and water present values in the Energy row and zero in the Water row but now the present values of energy costs and water costs are in the appropriate row. 

See [5594](https://github.com/NREL/EnergyPlus/pull/5594 "5594"). 


### JtoKWH Unit Conversion Impacts LEED Summary Report

When the JtoKHW unit conversion option is used in the OutputControl:Table:Style object it now changes the units used in the LEED Summary report. Columns that were previously left as GJ are now expressed as kWh and columns that were previously MJW/m2 are now expressed as kWh/m2.

See [5616](https://github.com/NREL/EnergyPlus/pull/5616).
