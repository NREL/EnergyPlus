Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited. It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo. At least a PR number or whatever.

### EIO Internal Gains and Initialization report

For the EIO lines for:

- People Internal Gains Nominal
- Lights Internal Gains Nominal
- ElectricEquipment Internal Gains Nominal
- GasEquipment Internal Gains Nominal
- HotWaterEquipment Internal Gains Nominal
- SteamEquipment Internal Gains Nominal
- OtherEquipment Internal Gains Nominal

replaced the colums:

- Nominal Minimum 
- Nominal Maximum

with 

- Minimum for All Day Types
- Maximum for All Day Types
- Minimum for Weekdays
- Maximum for Weekdays
- Minimum for Weekends/Holidays
- Maximum for Weekends/Holidays
- Minimum for Summer Design Days
- Maximum for Summer Design Days
- Minimum for Winter Design Days
- Maximum for Winter Design Days

These changes will also make corresponding changes in the HTML Intialization Summary report

### Adding an Output:Variable, Zone/Space Wetbulb Globe Temperature

An output variable will be added at both zone and space level: Zone Wetbulb Globe Temperature, and Space Wetbulb Globe
Temperature

### EnvelopeSummary in Tabular Reports

In the Opaque Exterior table added “Zone” column

Add an entirely new table called Opaque Construction Layers which shows the layers of materials for each construction


###	EquipmentSummary in Tabular Reports

In the DX Heating Coils table added "Supplemental Heat High Shutoff Temperature”

In the Fans table added "Motor Loss Zone Name"

Added an entirely new table called Air Terminals

###	SystemSummary in Tabular Reports

In the Demand Controlled Ventilation table added the "type"

Added an entirely new table called Thermostat Schedules

## New HVAC Topology report in Tabular Reports

The HVAC Topology report provides information about the arrangement of HVAC components in the supply and demand side of the airloop, zone equipment, and plant loop. Each row shows the additional component, sub-component, or sub-sub-component being added to the arrangement. 


