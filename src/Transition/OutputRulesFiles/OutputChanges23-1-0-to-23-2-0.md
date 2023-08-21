Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### EIO System Sizing Information User Design Capacity header
Missing unit is added to the EIO ystem Sizing Informationg table "User Design Capacity" header as shown below: 
- <System Sizing Information>, System Name, Load Type, Peak Load Kind, User Design Capacity [W], Calc Des Air Flow Rate [m3/s], User Des Air Flow Rate [m3/s], Design Day Name, Date/Time of Peak
- This change also impacts the html tabular output report file: report name "Initialization Summary" and table name "System Sizing Information". 

See pull request [#9967](https://github.com/NREL/EnergyPlus/pull/9967) for more details.

### Std 229 Ruleset Description Model (Phase 2) tabular output changes

This work started since 2022 aims to develop a script that creates a JSON file consistent with the ASHRAE Standard 229 Ruleset Model Description schema to show the feasibility of the schema, uncover problems with implementation, and provide an implementation for early adopters. The full title for ASHRAE Standard 229 is "Protocols for Evaluating Ruleset Application in Building Performance Models." This standard establishes tests and acceptance criteria for application of rulesets and related reporting for building performance models; and applies to evaluation of the implementation of rulesets associated with new or existing buildings, their systems, controls, sites, and other aspects described by the ruleset. It establishes requirements for: (2.1) building performance modeling software; and (2.2) software that evaluates building performance models and associated information to check the application of a ruleset. Currently, ASHRAE Standard 229 has not been published or even gone through public review and is under development by the ASHRAE SPC 229P committee. However, the intention of the standard is to provide code officials and rating authorities with files that they can use with a Ruleset Checking Tool (currently, an example is under development at PNNL) to automatically check if a ruleset (such as 90.1 Appendix G, RESNET, California Title 24 performance paths, or Canada National Energy Code for Buildings performance path) has been implemented correctly.

Since each EnergyPlus IDF file could generate an RMD file, the Ruleset Checking Tool will be able to see if the changes between the RMD files correspond to rules in the ruleset by looking at both the baseline and proposed RMD file. The work in the current stage prioritize the support on the selection of baseline HVAC system in Appendix G which includes changes to or new EnergyPlus reports:

#### Adding reporting entries to existing tabular reports
- Equipment Summary - Fans
- HVAC Topology
- Equipment Summary - PlantLoop or CondenserLoop
- Equipment Summary - AirTerminals

#### Adding additional central plant and equipment reports

- Equipment Summary - Pumps
- Component Sizing Summary - PlantLoop
- Equipment Summary Coils and Connections
- Equipment Summary - Chillers
- Equipment Summary - Boiler
- Equipment Summary - fluid coolers

See Pull Request [#10051] (https://github.com/NREL/EnergyPlus/pull/10051) for more details about the NFP; and [#10143] (https://github.com/NREL/EnergyPlus/pull/10143) for details about current implementations.
