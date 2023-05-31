Ruleset Model Description Phase 2 JSON Output
================

**Jason Glazer, GARD Analytics**

 - May 31, 2023

## Justification for New Feature ##

This continues the work from 2022 to develop a script that creates a JSON file consistent with the ASHRAE Standard 229
Ruleset Model Description schema to show the feasibility of the schema, uncover problems with implementation, and provide
an implementation for early adopters. 

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

ASHRAE Standard 229 has not been published or even gone through public review and is under development
by the ASHRAE SPC 229P committee. The intention of the standard is to provide code officials and rating
authorities with files that they can use with a Ruleset Checking Tool (currently, an example is under development at 
PNNL) to automatically check if a ruleset (such as 90.1 Appendix G, RESNET, California Title 24 performance 
paths, or Canada National Energy Code for Buildings performance path) has been implemented correctly. 
Since each EnergyPlus IDF file could generate an RMD file, the Ruleset Checking Tool will be able to see if the 
changes between the RMD files correspond to rules in the ruleset by looking at both the baseline
and proposed RMD file. 

The original NFP is here:

https://github.com/NREL/EnergyPlus/blob/develop/design/FY2022/NFP-InitialRulesetModelDescription.md

The repo for the script development is here:

https://github.com/JasonGlazer/createRulesetModelDescription

Some lessons learned from the initial effort in 2022 are described here:

https://github.com/JasonGlazer/createRulesetModelDescription/blob/main/docs/lessons_learned.rst

The schema is described here:

https://github.com/open229/ruleset-model-description-schema/blob/main/schema-source/ASHRAE229_extra.schema.yaml

## E-mail and  Conference Call Conclusions ##

None so far.

## Overview ##

The initial version did not support the full schema and needed polishing up before being used in widespread workflows.  
For this phase, the focus is on methods to merge compliance parameters, implementing additional output reports in EnergyPlus, 
filling in gaps in existing data groups, and supporting more data groups.

Targeted data groups from the 229 RMD Schema for this effort include the HVAC portion of the schema:

- HeatingVentilatingAirConditioningSystem
- HeatingSystem
- CoolingSystem
- FanSystem
- AirEconomizer
- AirEnergyRecovery
- Fan
- Terminal
- FluidLoop
- FluidLoopDesignAndControl
- Pump
- Boiler
- Chiller
- HeatRejection

This effort will focus on trying to implement as many data groups as possible. It is unlikely that full support 
for all data groups and data elements for these will be achieved in this phase, so the focus will again be on 
implementing the more direct data elements within each data group so that as many data elements can be implemented 
as possible.

In future efforts, related data groups will be supported:
- FanOutputValidationPoint
- BoilerOutputValidationPoint 
- ChillerCapacityValidationPoint
- ChillerPowerValidationPoint

## Approach ##

The effort will continue the approach used in 2022 during the initial development of the createRMD but 
will add more reporting to EnergyPlus to be consistent with the goal of being driven by EnergyPlus 
outputs and not needing to read the IDF or epJSON files. The approach has two focuses:

- Enhance EnergyPlus Tabular Output Reports
- Support New Data Groups in createRMD Python script

In addition, some fixes of the createRMD Python script will be incorporated from the current list of issues:

- Ensure that all IDs are unique and consistent between instances (issue #4)
- Add Infiltration for each Zone even if not present in EnergyPlus (issue #5)
- Utilize additional Space tags in E+ for other enumerations of Space data group (issue #6)
- Add docstrings (issue #7)
- Fix grouping of HVAC related datagroups and use Construction:FfactorGroundFloor (issue #8)
- Ground Surfaces cannot be identified (issue #12)
- Add method to merge compliance parameters (issue #14)

To understand how when data elements can be populated by either EnergyPlus input or 
EnergyPlus output as the source of the RMD data elements, a specially tagged version of the 
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

## Prioritization ##

It is unlikely that all table enhancements and RMD data groups described in this NFP will be able to be supported
with the budget for this task, so some prioritization of what should be in this effort versus in following efforts
will be needed. The prioritization will link the support of the enhanced tables to the support of data elements in 
the RMD data elements. Feedback on the priorities is welcome, either overall (air side vs. water side) or detailed, 
such as specific new columns and data elements that are most important.

### Enhance Existing EnergyPlus Tabular Output Reports ###

#### Equipment Summary - Central Plant ####

The current columns are:
- Type
- Reference Capacity [W]
- Reference Efficiency [W/W]
- Rated Capacity [W]
- Rated Efficiency [W/W]
- IPLV in SI Units [W/W]
- IPLV in IP Units [Btu/W-h]

The new columns would be:
- Plantloop name
- Plantloop branch name
- Minimum part load ratio 
- Fuel type
- Parasitic electric load
- Rated entering condenser temperature
- Rated leaving evaporator temperature
- Reference entering condenser temperature
- Reference leaving evaporator temperature
- Design water flow rate
- Chiller condenser design flow rate
- Heat recovery Plantloop name
- Heat recovery Plantloop branch name
- Recovery Relative Capacity Fraction
- Fluid type
- Range
- Approach
- Design Fan Power
- Design inlet air wet-bulb temperature
- Leaving water setpoint temperature
- Chiller Condenser type

We may also want to consider breaking up the "Central Plant" table into separate tables for chillers, boilers, and heat 
rejection since more columns are unique to just one of those.

#### Equipment Summary - Heating Coils ####

The current columns are:
- Type
- Design Coil Load [W]
- Nominal Total Capacity [W]
- Nominal Efficiency [W/W]

The new columns would be:
- Used as Supplementary Heat
- Airloop name
- Airloop branch name
- Plantloop name
- Plantloop branch name
- Supplemental heat high shutoff temperature [C]

#### Equipment Summary - DX Heating Coils ####

The current columns are:
- DX Heating Coil Type
- High Temperature Heating (net) Rating Capacity [W]
- Low Temperature Heating (net) Rating Capacity [W]
- HSPF [Btu/W-h]
- Region Number

The new columns would be:
- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
- Airloop name
- Airloop branch name

#### Equipment Summary - Fans ####

The current columns are:
- Type
- Total Efficiency [W/W]
- Delta Pressure [pa]
- Max Air Flow Rate [m3/s]
- Rated Electricity Rate [W]
- Rated Power Per Max Air Flow Rate [W-s/m3]
- Motor Heat In Air Fraction
- Fan Energy Index
- End Use Subcategory
- Design Day Name for Fan Sizing Peak
- Date/Time for Fan Sizing Peak

The new columns would be:
- Purpose (supply, return, exhaust, relief)
- Occupied operation (cycling, continuous, off)
- Unoccupied operation (cycling, continuous, off)
- Locked out during central heating (yes, no, N/A)
- Is autosized
- Motor efficiency
- Motor heat to zone fraction
- Motor loss zone name
- Airloop name
- Airloop branch name

#### Equipment Summary - Pumps ####

The current columns are:
- Type
- Control
- Head [pa]
- Water Flow [m3/s]
- Electricity Rate [W]
- Power Per Water Flow Rate [W-s/m3]
- Motor Efficiency [W/W]
- End Use Subcategory

The new columns would be:
- Is autosized
- Plantloop name
- Plantloop branch name

#### System Summary - Demand Controlled Ventilation using Controller:MechanicalVentilation ####

The current columns are:
- Controller:MechanicalVentilation Name 
- Outdoor Air Per Person [m3/s-person] 
- Outdoor Air Per Area [m3/s-m2] 
- Outdoor Air Per Zone [m3/s] 
- Outdoor Air ACH [ach] 
- Outdoor Air Method 
- Outdoor Air Schedule Name 
- Air Distribution Effectiveness in Cooling Mode 
- Air Distribution Effectiveness in Heating Mode 
- Air Distribution Effectiveness Schedule Name

The new columns would be:
- Type (CO2 Zone/Other)

#### Component Sizing Summary - PlantLoop ####

The current columns are:
- Maximum Loop Flow Rate [m3/s]
- Plant Loop Volume [m3]

The new columns would be:
- Design supply temperature
- Design return temperature
- Sizing option (Coincident/NonCoincident)

### Add New EnergyPlus Tabular Reports and Tables ###

#### HVAC Topology ####

Consists of tables for air and plant loops

For each Plant loop and AirLoopHVAC a table is created that has:
- Component Type (normal components including Connector:Splitter and Connector:Mixer)
- Component Name
- Zone Name (for demand side)
- Loop side (supply side/demand side)
- Order (2nd of 5 items on branch, items in parallel after split would have same number)
- Branch Name
- Split (either y/n or “split 3 of 5”)
- node name in (multiple names for mixer)
- node name out (multiple names for splitter)
- other control objects that reference node in as sensor (or node out?)
- other control objects that reference node in as actuator (or node out?)

Will this completely describe the zone equipment?

Probably also need a table to describe the components on an outdoor air branch.

The table would list where controllers, setpoint managers, and other items that reference nodes.

Additionally, the BND file may also be reviewed for what can easily be added to the topology report with minimal effort.

#### Coil Sizing Details - Coil Connections ####

A new table under the current Coil Sizing Details report called "Coil Connections". Each row would be a coil:

- Coil Name
- Coil Type*
- Coil Location*
- HVAC Type*
- HVAC Name*
- Zone Name(s)*
- Supply Fan Name for Coil*
- Supply Fan Type for Coil*
- Plant Name for Coil*
- Airloop Name
- Airloop Branch Name
- Location count on Airloop Branch
- Plant Loop Name
- Plant Branch Name
- Location count on Plantloop Branch

Some of these columns (shown with asterisks) are currently in the "Coils" table and could be removed from 
that table since it is very wide already, or they could be kept for compatibility.

#### Controls ####

A new report that includes Setpoint Managers, Controllers, Availability Managers, PlantEquipmentOperation, and components
with control options and identifies the type, where they apply, sense, and control parameters from the input. Need: 

- minimum and maximum setpoint temperatures
- minimum turndown ratio
- schedules
- load ranges
- Setpoint at Outdoor Low Temperature
- Outdoor Low Temperature
- Setpoint at Outdoor High Temperature
- Outdoor High Temperature
   
This is probably a series of tables, one for each input object. The report would include the pertinent information,
including the controls, related temperatures, and what they apply to but would generally be limited to what is already
available within the data structure that would be useful to report. Include specifically:

- Heat Recovery - Operation With Economizer
- Heat Recovery - Supply Air Temperature Control



#### Equipment Summary - PlantLoop or CondenserLoop ####

A new report for PlantLoop objects. One row for each Plant or CondenserLoop
- name
- type (PlantLoop or CondenserLoop)
- provides heating
- provides cooling
- Maximum Loop Flow Rate
- Minimum Loop Flow Rate
- Total pump power on loop
- Branch name

#### Equipment Summary - AirTerminals ####

A new report for AirTerminals.
- name
- type of input object
- Heat/Reheat Coil Object Type
- Zone Name
- Hot Water Plant Loop Name
- Hot Water Plant Branch Name
- Chilled Water Coil Object Type
- Chilled Water Plant Loop Name
- Chilled Water Plant Branch Name
- Fan Object Type
- Fan Name
- Primary Air Flow Rate
- Secondary Air Flow Rate
- Minimum Flow
- Minimum Flow Schedule Name
- Maximum Flow During Reheat
- Minimum Outdoor Flow
- Minimum Outdoor Flow Schedule Name
- Supply cooling setpoint
- Supply heating setpoint
- Temperature control
- Heating capacity
- Cooling capacity

#### Equipment Summary - Air Heat Recovery ####

A new report for each HeatExchanger:AirToAir:SensibleAndLatent and HeatExchanger:AirToAir:FlatPlate:
- name
- input object type
- plate/rotary
- Sensible Effectiveness at 100% Heating Air Flow
- Sensible Effectiveness at 100% Cooling Air Flow
- Latent Effectiveness at 100% Heating Air Flow
- Latent Effectiveness at 100% Cooling Air Flow
- Exhaust airflow
- Outdoor airflow

We might want to add the other effectivenesses at different air flow rates to make the report more comprehensive.

## Testing/Validation/Data Sources ##

Since the output JSON file needs to comply with a JSON schema, it should be easy to confirm that it is valid. 
The Python jsonschema library can be used to confirm that the RMD file is consistent with the schema. Comparison
of input and related outputs should provide the final check if the reporting is being done correctly.

## Input Output Reference Documentation ##

No specific changes for input are expected.

Additional tabular outputs will be described in the IOref.

## Outputs Description ##

The new output file is a JSON file following the schema described here:

https://github.com/open229/ruleset-model-description-schema

## Engineering Reference ##

No changes

## Example File and Transition Changes ##

None required.

## Design Document ##

Not yet developed

## References ##

The original NFP is here:

https://github.com/NREL/EnergyPlus/blob/develop/design/FY2022/NFP-InitialRulesetModelDescription.md

The repo for the script development is here:

https://github.com/JasonGlazer/createRulesetModelDescription

Some lessons learned from the initial effort in 2022 are described here:

https://github.com/JasonGlazer/createRulesetModelDescription/blob/main/docs/lessons_learned.rst

The schema is described here:

https://github.com/open229/ruleset-model-description-schema/blob/main/schema-source/ASHRAE229_extra.schema.yaml


