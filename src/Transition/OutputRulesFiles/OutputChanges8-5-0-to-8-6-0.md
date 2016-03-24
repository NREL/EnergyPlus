Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.


### Degree days in tabular reports correspond to ASHRAE Standard 90.1 degree day bases

Impacts the LEED Summary report table SEc1.1A-General Information and the Climatic Data Summary report. The #5083 issue was resolved and indicated that the temperature bases used for reporting the heating and cooling degree days were not consistent with ASHRAE Standard 90.1 which uses CDD50 and HDD65 (Fahrenheit). These correspond to CDD10 and HDD18 (Celsius). Previously these were reversed and using CDD65 and HDD50 instead. In addition made changes to use the terms "standard" to be "ASHRAE Handbook 2009" for the reporting of the degree day values in the Climatic Data Summary report.


