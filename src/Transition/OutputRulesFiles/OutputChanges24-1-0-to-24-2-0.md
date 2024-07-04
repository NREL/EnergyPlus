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

### EIO DX Heating Coil Standard Rating Information 

The EIO and html tabular output files now have a seprate heading and data stream for DX Heating Coils with the AHRI 2023 and prior versions.

! <DX Heating Coil Standard Rating Information>, Component Type, Component Name, High Temperature Heating (net) Rating Capacity {W}, Low Temperature Heating (net) Rating Capacity {W}, HSPF {Btu/W-h}, Region Number
! <DX Heating Coil AHRI 2023 Standard Rating Information>, Component Type, Component Name, High Temperature Heating (net) Rating Capacity {W}, Low Temperature Heating (net) Rating Capacity {W}, HSPF2 {Btu/W-h}, Region Number


### Euipment Summary Report 

Renamed a table name in the Equipment Summary report:

- from "DX Heating Coils [ HSPF2 ]" 
- to   "DX Heating Coils AHRI 2023"

Added two new columnuns to the named `DX Heating Coils AHRI 2023` table to make it equivalent to an exiting `DX Heating Coils` table. 

New columns header added:

- "Minimum Outdoor Dry-Bulb Temperature for Compressor Operation [C]"
- "AirLoop Name"

Renamed a column header by adding units to the `DX Heating Coils` table:

- from "Minimum Outdoor Dry-Bulb Temperature for Compressor Operation"
- to   "Minimum Outdoor Dry-Bulb Temperature for Compressor Operation [C]"

