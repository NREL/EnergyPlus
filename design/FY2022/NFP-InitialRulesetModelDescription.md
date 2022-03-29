Initial Ruleset Model Description JSON Output
================

**Jason Glazer, GARD Analytics**

 - March 29, 2022

## Justification for New Feature ##

Demostrate the initial implementation of a new JSON based output file that conforms to the draft ASHRAE Standard 229 
Ruleset Model Description Schema to show the feasibilty of the schema and uncover problems with implementation. 

The title, purpose, and scope of ASHRAE Standard 229 are:

Title: 

- Protocols for Evaluating Ruleset Implementation in Building Performance Modeling Software

Purpose: 

-  This standard establishes tests and acceptance criteria for implementation of rulesets (e.g., modeling 
rules) and related reporting in building performance modeling software.

Scope:

-   This standard applies to building performance modeling software that implements rulesets.
-   This standard applies to rulesets associated with new or existing buildings and their systems, system controls, 
their sites, and other aspects of buildings described by the ruleset implementation being evaluated.

ASHRAE Standard 229 has not been published or gone through public review yet and is being developed
by the ASHRAE SPC 229P committee. The intention of the standard is to provide code officials and rating
authorities with files that they can use with a Ruleset Checking Tool (currently under development at 
PNNL) to automatically check if a ruleset (such as 90.1 Appendix G, RESNET, California Title 24 performance 
paths, or Canada National Energy Code for Buildings performance path) has been implemented correctly. 
Since each EnergyPlus IDF file could generate a RMD file, the Ruleset Checking Tool will be able to see if the 
changes between the RMD files correspond to rules in the ruleset by looking at both the baseline
and proposed RMD file. 

Revisions to the title, purpose and scope are currently being considered but are not expected to 
substantially change the Ruleset Model Description Schema.

## E-mail and  Conference Call Conclusions ##

None so far.

## Overview ##

Initial implementation of the draft ASHRAE Standard 229 Ruleset Model Description (RMD) in EnergyPlus as a new JSON 
output file as described in Issue #8970. 

https://github.com/NREL/EnergyPlus/issues/8970

The RMD schema is being developed at:

https://github.com/open229/ruleset-model-report-schema

and specifically, the JSON schema file is available here:

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

The initial effort will focus on trying to implement as many stable data groups as possible. The new 
EnergyPlus Space input object will facilitate implementation separating space and zone related data 
elements. 

The RMD file would be an optional output file controlled by a new field added to the OutputControl:Files 
input object. 


## Approach ##

Most of the schema data groups and data elements are based on inputs to
simulation programs and those will be taken from EnergyPlus data structures soon
after the GetInput phase of processing. Some other data element values will use
output from EnergyPlus ususally from tabular reports. The new RMD JSON file will 
be populated primarily by user input in the IDF or epJSON file and by sizing results 
so that most of it can be created without an annual simulation. Some data elements
that are more closely aligned with compliance parameters do not correspond with 
EnergyPlus inputs or outputs, possible approaches for handling these are shown in the 
Compliance Parameters section below.

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
many other HVAC portions of the schema have not been approved by the ASHRAE Standard 229P 
committee yet.

For each RMD data group added an analysis of the values in the field need to be performed. 
The following is an example for the Surface data group.

| RMD Data Element             | Data Type                   | EnergyPlus Input Object      | EnergyPlus Field                   | Output Table     | Output Row/Column| Notes                                                                  |
| ---------------------------- | ----------------------------| ---------------------------- | ---------------------------------- | ---------------- | -----------------| ---------------------------------------------------------------------- |
| id                           | ID                          | BuildingSurface:Detailed     | Name                               |                  |                  | Many other input objects could contain this (Wall:Exterior, Roof, etc… |
| reporting\_name              | String                      | BuildingSurface:Detailed     | Name                               |                  |                  |                                                                        |
| notes                        | String                      |                              |                                    |                  |                  | Compliance Parameter                                                   |
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
| status\_type                 | enumeration                 |                              |                                    |                  |                  | Compliance Parameter                                                   |

### Compliance Parameters ###

The Ruleset Model Description schema includes some data elements that do not correspond to 
EnergyPlus (or probably any simulation program) inputs or outputs. These data elements most 
often have to do with compliance. In the example above for Surface, the status\_type data 
element has possible enumeration values of:

- NEW
- ALTERED
- EXISTING
- EXISTING_PLUS_NEW

It is unlikely that most simulation programs would know such a thing about a surface but it is
very likely that the modelers would know. There are quite a few data elements that would fall into 
this category of Compliance Parameters including Notes that occur in almost every data group in
the RMD. There are multiple ways this type of data element populated in the RMD file:

- The user could add those data elements and values by hand after EnergyPlus generates the RMD file
- The IDF file could have new compliance:surface (and other) input objects that populate the compliance 
data elements in the RMD file
- A JSON file could be created by user that gets merged each time that EnergyPlus runs to populate those 
data elements in the RMD file

Each option has advantages and disadvantages. We need to decide which approach or additional alternative
should be used for this implementation.

## Testing/Validation/Data Sources ##

Since the output JSON file needs to comply with a JSON schema, it should be easy to confirm that it is valid. 
The valijson library can be used to confirm that the RMD file is consistent with the schema.

## Input Output Reference Documentation ##

For OutputControl:Files, add a field

```
Field: RMD
Conditionally turn on or off the Ruleset Model Description file that is consistent with the schema defined in
ASHRAE Standard 229P Protocols for Evaluating Ruleset Implementation in Building Performance Modeling Software
and documented here:

https://github.com/open229/ruleset-model-description-schema

```

Depending on the decision related to Compliance Parameters, additional documentation related to Compliance: input 
object may be necessary. 

## Outputs Description ##

The new output file is a JSON file following the schema described here:

https://github.com/open229/ruleset-model-description-schema

## Engineering Reference ##

No changes

## Example File and Transition Changes ##

None required.

## Design Document ##

Two JSON libraries are currently used in EnergyPlus:

- https://github.com/nlohmann/json

- https://github.com/tristanpenman/valijson

Which are expected to be the only libraries needed for this work.

Some challenges of this task that we would like feedback on:

- what data structure would make the most sense for ultimately outputing a JSON file?
- how can we reduce the footprint of adding to the data structure since it will appear
 in many places in the code?
- are there any recommendations since we know we will be making many changes in the 
future since the 229 schema is still undergoing revisions?

## References ##

https://github.com/open229/ruleset-model-description-schema



