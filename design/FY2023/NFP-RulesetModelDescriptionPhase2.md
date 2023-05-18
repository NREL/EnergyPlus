Ruleset Model Description Phase 2 JSON Output
================

**Jason Glazer, GARD Analytics**

 - May 18, 2023

## Justification for New Feature ##

This continues the work from 2022 to develop a script that creates a JSON file consistent with the ASHRAE Standard 229
Ruleset Model Description schema  to show the feasibilty of the schema and uncover problems with implementation. 

The title, purpose, and scope of ASHRAE Standard 229 are:

Title: 

- Protocols for Evaluating Ruleset Application in Building Performance Models

Purpose: 

- This standard establishes tests and acceptance criteria for application of rulesets and related reporting for building 
performance models.

Scope:

- This standard applies to evaluation of the implementation of rulesets associated with new or existing buildings, their 
systems, controls, sites, and other aspects described by the ruleset. It establishes requirements for:  
- 2.1 building performance modeling software    
- 2.2 software that evaluates building performance models and associated information to check the application of a ruleset

ASHRAE Standard 229 has not been published or gone through public review yet and is being developed
by the ASHRAE SPC 229P committee. The intention of the standard is to provide code officials and rating
authorities with files that they can use with a Ruleset Checking Tool (currently under development at 
PNNL) to automatically check if a ruleset (such as 90.1 Appendix G, RESNET, California Title 24 performance 
paths, or Canada National Energy Code for Buildings performance path) has been implemented correctly. 
Since each EnergyPlus IDF file could generate a RMD file, the Ruleset Checking Tool will be able to see if the 
changes between the RMD files correspond to rules in the ruleset by looking at both the baseline
and proposed RMD file. 

The original NFP is here:

https://github.com/NREL/EnergyPlus/blob/develop/design/FY2022/NFP-InitialRulesetModelDescription.md

The repo for the script development is here:

https://github.com/JasonGlazer/createRulesetModelDescription

Some lessons learned from the initial effort in 2022 is described here:

https://github.com/JasonGlazer/createRulesetModelDescription/blob/main/docs/lessons_learned.rst

The schema is described here:

https://github.com/open229/ruleset-model-description-schema/blob/main/schema-source/ASHRAE229_extra.schema.yaml

## E-mail and  Conference Call Conclusions ##


## Overview ##

The initial version does not support the full schema and needs polishing up before being used in widespread workflows.  
For this phase, the focus is on methods to merge compliance parameters, implementing additional output reports in EnergyPlus, 
filling in gaps in existing data groups, and supporting more data groups.

Targeted data groups from the 229 RMD Schema for this effort include the HVAC portion of the schema:

- HeatingVentilatingAirConditioningSystem*
- HeatingSystem*
- CoolingSystem*
- FanSystem*
- AirEconomizer
- AirEnergyRecovery
- Fan*
- FanOutputValidationPoint
- Terminal*
- FluidLoop*
- FluidLoopDesignAndControl*
- Pump*
- Boiler*
- BoilerOutputValidationPoint
- Chiller*
- ChillerCapacityValidationPoint
- ChillerPowerValidationPoint
- HeatRejection*

This effort will focus on trying to implement as many data groups as possible. It data groups with asterisks
have the highest priority. It is unlikely that full support for all data groups and data elements for these 
will be achieved in this phase so the focus will again be on implementing the more direct data elements within 
each data group so that as many data elements can be implemented as possible.


## Approach ##

The effort will continue the approach used in 2022 during the initial development of the createRMD but 
will add more reporting to EnergyPlus to be consistent with the goal of being driven by EnergyPlus 
outputs. The approach has two focuses:

- Enhance EnergyPlus Tabular Output Reports
- Support New Data Groups in createRMD Python script

In addition some fixes of the createRMD Python script will be incorporated from the current list of issues:

- Ensure that all IDs are unique and consistent between instances (issue #4)
- Add Infiltration for each Zone even if not present in EnergyPlus (issue #5)
- Utilize additional Space tags in E+ for other enumerations of Space data group (issue #6)
- Add docstrings (issue #7)
- Fix grouping of HVAC related datagroups and use Construction:FfactorGroundFloor (issue #8)
- Ground Surfaces cannot be identified (issue #12)

To understand how often data elements would be populated by either EnergyPlus input or 
EnergyPlus output as the source of the RMD data elements, a special tagged version of the 
schema file was created here:

https://github.com/open229/ruleset-model-description-schema/blob/EPtags/schema-source/ASHRAE229_extra.schema.yaml

Tags added are:

- Used by RCT Test
- EPin Object
- EPin Field
- EPout File
- EPout Report
- EPout Table
- EPout Column
- EPstatus
- EP Notes

Most of these were used previously during the 2022 effort.

### Enhance Existing EnergyPlus Tabular Output Reports ###




### Add New EnergyPlus Tabular Reports ###






## Testing/Validation/Data Sources ##

Since the output JSON file needs to comply with a JSON schema, it should be easy to confirm that it is valid. 
The Python jsonschema library can be used to confirm that the RMD file is consistent with the schema.

## Input Output Reference Documentation ##

No specific changes for input are expected.

Additional tabular outputs will be considered on a case-by-case basis as described above.

## Outputs Description ##

The new output file is a JSON file following the schema described here:

https://github.com/open229/ruleset-model-description-schema

## Engineering Reference ##

No changes

## Example File and Transition Changes ##

None required.

## Design Document ##

The following describes the design:

 - A new Python utility will be created separate from EnergyPlus that can
 eventually be packaged with the EnergyPlus installer. It will be developed in
 its own repository but eventually this may be merged or linked from the
 EnergyPlus repository.

 - The new Python utility will read the JSON files that EnergyPlus produces when
 the output:JSON input object is used as the primary source of information. As a
 secondary source of information, the epJSON input file will be read. It is
 possible that the EIO file will be read as part of this effort. 

 - The Ruleset Model Description (RMD) format will be produced by the utility and
 is also a JSON format.

 - Verification that the RMD output produced by the new utility is consistent
 with the RMD schema will be performed by using the jsonschema Python library
 "validate" method.

 - The PathLib library is expected to be used for accessing files.

 - The unittest library is expected to be used for providing unit testing. The
 goal is to have tests for almost all of the methods.

 - At this point no changes to EnergyPlus are expected as part of this but
 issues may be added for features that are not working or are needed. For
 example, #9419 was added due to the initialization summary not being produced
 in the output:JSON file. 

 - Only a subset of data groups from the RMD schema will be generated and only
 data elements that are most direct will be implemented. This is expected to be
 the first step in an ongoing effort to fully implement the RMD schema as an
 output format.

## References ##

https://github.com/open229/ruleset-model-description-schema



