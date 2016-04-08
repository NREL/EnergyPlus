Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.


### Economics, Tariff, and Life Cycle Cost Reports Explicitly Requested

The Economics Results Summary Report, Tariff Report, and Life-Cycle Cost Report now need to be specifically requested rather than being automatically generated if specific objects are present in the file. To get the reports individually use EconomicResultSummary, TariffReport, and LifeCycleCostReport respectively in the Output:Table:SummaryReports object. These are also available when using any of the AllSummary options. The reports are unchanged just they way of generating them has changed.

