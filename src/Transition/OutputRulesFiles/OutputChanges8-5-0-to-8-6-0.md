Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.


### New Underground Gross Wall Area Added to Zone Summary Table in Input Verification and Results Summary Report

The Zone Summary table in the Input Verification and Results Summary Report always had a column labeled "Gross Wall Area" which was the above ground wall area for each zone. This change relabeled that column to be called "Above Ground Gross Wall Area" and added an entirely new column called "Underground Gross Wall Area" that shows the amount of underground wall area for each zone.


### Life-Cycle Cost Report Updated for Water

The "Energy Cost Cash Flows (Without Escalation" table has been renamed "Energy and Water Cost Cash Flows (Without Escalation)". The "Operating Cash Flow by Category (Without Escalation)" table used to combine energy and water costs in the Energy column and have zeros in the Water column and now energy costs are the Energy column and water costs are in the Water column. The "Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)" table used to show water costs with the Energy in the Category column and Energy Cost in the Kind column but now show Water and Water Cost in those respective columns. The Present Value by Category table used to include the sum of energy and water present values in the Energy row and zero in the Water row but now the present values of energy costs and water costs are in the appropriate row. See [5594](https://github.com/NREL/EnergyPlus/pull/5594 "5594"). 



