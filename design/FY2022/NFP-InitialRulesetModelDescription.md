Initial Ruleset Model Description JSON Output
================

**Jason Glazer, GARD Analytics**

 - March 24, 2022
 

## Justification for New Feature ##

Demostrate the initial implementation of the ASHRAE Standard 229 Ruleset Model Description Schema as an output of 
EnergyPlus to show the feasibilty of the schema and uncover problems with implementation. The title, purpose, and 
scope of ASHRAE Standard 229 are:

Title: 

- Protocols for Evaluating Ruleset Implementation in Building Performance Modeling Software

Purpose: 

-  This standard establishes tests and acceptance criteria for implementation of rulesets (e.g., modeling 
rules) and related reporting in building performance modeling software.

Scope:

-   This standard applies to building performance modeling software that implements rulesets.
-   This standard applies to rulesets associated with new or existing buildings and their systems, system controls, 
their sites, and other aspects of buildings described by the ruleset implementation being evaluated.

Revisions to the title, purpose and scope are currently being considered.

## E-mail and  Conference Call Conclusions ##

None so far.

## Overview ##

Initial implementation of the draft ASHRAE Standard 229 Ruleset Model Description (RMD) in EnergyPlus as a new JSON 
output file as described in Issue #8970. 

https://github.com/NREL/EnergyPlus/issues/8970

The RMR schema is currently under development by the ASHRAE committee SPC 229 and has not gone through 
public review yet, so it is likely to change in the future. The RMD schema is being developed at:

https://github.com/open229/ruleset-model-report-schema

and specifically at:

https://github.com/open229/ruleset-model-description-schema/blob/master/docs229/ASHRAE229.schema.json

Examples of the schema can be found here:

https://github.com/open229/ruleset-model-description-schema/tree/master/examples/0.0.5

Some more stable aspects of the schema will be implemented first such as data groups for:

- Space
- Surface
- Construction
- Material
- InteriorLighting
- MiscellanousEquipment 

The initial effort will focus on trying to implement as many stable data groups as possible.  The new Space input object will facilitate 
implementation separating space and zone related data elements. Additional "compliance:" input objects will need to be added to cover 
inputs that don't appear anywhere else and these inputs would pass through into the RMD JSON file. This would be implemented as a new 
optional RMR JSON output file controlled by a new field added to the OutputControl:Files input object. The new RMR JSON file will be 
populated primarily by user input in the IDF or epJSON file and by sizing results so that most of it can be created without an annual simulation.


## Approach ##

Most of the schema data groups and data elements are based on inputs to
simulation programs and those will be taken from EnergyPlus data structures soon
after the GetInput phase of processing. Some other data element values will use
output from EnergyPlus ususally from tabular reports. Finally some data elements
that are more closeAddly alligned with compliance parameters, these will be will 
require additional user input using new "compliance:" input objects.

In addition, some data elements in data groups may be difficult to implement and in 
those cases, they may be skipped in this initial implementation which is trying to 
cover as many data elements as quickly as possible.

It is unlikely that the entire schema will be implemented during this initial effort
so the focus will be on data groups shown in the Overview section. If addition
data groups can be implented under this effort they would be other data groups that
have been approved by the SPC 229P committee including:

- Zone
- Infiltration
- SurfaceOpticalProperties
- Subsurface
- Transformer
- Schedule
- Calendar
- Weather
- Elevator
- ExteriorLighting

Elements in the Zone data group related to HVAC are unlikely to be implemetned since 
many other HVAC portions of the schema have not been approved by SPC 229P yet.

For each RMD data group added an analysis of the values in the field need to be performed. 
The following is an example for the Surface data group.

| RMD Data Element             | Data Type                   | EnergyPlus Input Object      | EnergyPlus Field                   | Output Table     | Output Row/Column| Notes                                                                  |
| ---------------------------- | ----------------------------| ---------------------------- | ---------------------------------- | ---------------- | -----------------| ---------------------------------------------------------------------- |
| id                           | ID                          | BuildingSurface:Detailed     | Name                               |                  |                  | Many other input objects could contain this (Wall:Exterior, Roof, etc… |
| reporting\_name              | String                      | BuildingSurface:Detailed     | Name                               |                  |                  |                                                                        |
| notes                        | String                      | Compliance:Surface           | Notes                              |                  |                  | New object and field                                                   |
| subsurfaces                  | \[{Subsurface}\]            | FenestrationSurface:Detailed | Building Surface Name              |                  |                  | Many other input objects could contain this (Window, Door, GlazedDoor, etc.) |
| classification               | enumeration                 | BuildingSurface:Detailed     | Surface Type                       |                  |                  |                                                                        |
| area                         | Numeric                     |                              |                                    | Envelope Summary | Gross Area       |                                                                        |
| tilt                         | Numeric                     |                              |                                    | Envelope Summary | Tilt             |                                                                        |
| azimuth                      | Numeric                     |                              |                                    | Envelope Summary | Azimuth          |                                                                        |
| adjacent\_to                 | enumeration                 |                              |                                    |                  |                  |This would be complicated to implement                                  |
| adjacent\_zone               | $ID                         | BuildingSurface:Detailed     | Outside Boundary Condition Object  |                  |                  |                                                                        |
| does\_cast\_shade            | Boolean                     | ShadowCalculation            | Shading Zone Group 1 ZoneList Name |                  |                  |                                                                        |
| construction                 | {Construction}              | BuildingSurface:Detailed     | Construction Name                  |                  |                  |                                                                        |
| surface\_optical\_properties | {SurfaceOpticalProperties}  | Construction                 | Outside Layer                      |                  |                  |                                                                        |
| status\_type                 | enumeration                 | Compliance:Surface           | Status Type                        |                  |                  | New object and field                                                   |

## Testing/Validation/Data Sources ##

Since the output JSON file needs to comply with a JSON schema, it should be easy to confirm that it is valid.

## Input Output Reference Documentation ##

For OutputControl:Files, add a field

```
Field: RMD
Conditionally turn on or off the Ruleset Model Description file that is consistent with the schema defined in
ASHRAE Standard 229P Protocols for Evaluating Ruleset Implementation in Building Performance Modeling Software
and documented here:

https://github.com/open229/ruleset-model-description-schema

```

The current Group of Compliance input objects only has one input object Compliance:Building which has one field called 
Building Rotation for Appendix G. Additional Compliance input objects will be added as necessary to allow for user 
inputs to be included in the RMD file.


## Outputs Description ##

The new output file is a JSON file following the schema described here:

https://github.com/open229/ruleset-model-description-schema


## Engineering Reference ##

No changes

## Example File and Transition Changes ##

None required.

## References ##

https://github.com/open229/ruleset-model-description-schema



