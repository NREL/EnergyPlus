Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited. It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo. At least a PR number or whatever.

### Table Output, Sizing Output, and Output:Variable Spelling Corrections
* "Equipment Summary" table report, sub-table "VAV DX Cooling Standard Rating Details", column heading, "Assocated Fan" --> "Associated Fan".

* "Equipment Summary" table report, sub-table "Air Heat Recovery", column "Input object type", "Dessicant Balanced" --> "Desiccant Balanced" (for object type HeatExchanger:Desiccant:BalancedFlow).

* Output:Variable "Zone Hybrid Unitary HVAC DehumidificationLoad to Humidistat Setpoint Heat Tansfer Energy" --> "Zone Hybrid Unitary HVAC Dehumidification Load to Humidistat Setpoint Heat Transfer Energy".

* Output:Variable "Zone Hybrid Unitary HVAC Humidification Load to Humidistat Setpoint Heat Tansfer Energy" --> "Zone Hybrid Unitary HVAC Humidification Load to Humidistat Setpoint Heat Transfer Energy".

* Sizing output for ZoneHVAC:Baseboard:RadiantConvective:Steam, "User-Speicified Maximum Steam Flow Rate [m3/s]" --> "User-Specified Maximum Steam Flow Rate [m3/s]".

* Sizing output for Chiller:Absorption, "Iniital Design Size Design Generator Fluid Flow Rate [m3/s]" --> "Initial Design Size Design Generator Fluid Flow Rate [m3/s]"

* eio output header for "\<ShadingProperty Reflectance\>", "Contruction" --> "Construction". Also appears in the "Initialization Summary" table output, "ShadingProperty Reflectance" sub-table, column heading.

See Pull Request [#10760](https://github.com/NREL/EnergyPlus/pull/10760).
