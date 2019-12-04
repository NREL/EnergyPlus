Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### Additional field for Coil Sizing Details:Coils report

A new column "Peak Load Type to Size On" has been added to the `Coil Sizing Details:Coils` report

See [7397](https://github.com/NREL/EnergyPlus/pull/7397)

### Name re-ordering in the sizing labels and modified variable names for MulitSpeed DX Cooling and Heating Coils

The change impacts rated capacity, rated flow rate, rated sensible heat ratio, etc, see sample changes below 

```
Before: 
'Speed X' Design Size Design Size Rated Total Cooling Capacity
'Speed X' Design Size Gross Rated Heating Capacity
'Speed X' Design Size Rated Air Flow Rate 
'Speed X' Design Size Rated Sensible Heat Ratio 
'Speed X' Design Size Evaporative Condenser Air Flow Rate
'Speed X' Design Size Rated Evaporative Condenser Pump Power Consumption
          
'Speed X' User-Specified User-Specified Total Cooling Capacity
'Speed X' User-Specified Gross Rated Heating Capacity
'Speed X' User-Specified Rated Air Flow Rate 
'Speed X' User-Specified Rated Sensible Heat Ratio 
'Speed X' User-Specified Evaporative Condenser Air Flow Rate
'Speed X' User-Specified Rated Evaporative Condenser Pump Power Consumption

After:
Design Size 'Speed X' Gross Rated Total Cooling Capacity
Design Size 'Speed X' Gross Rated Heating Capacity
Design Size 'Speed X' Rated Air Flow Rate 
Design Size 'Speed X' Rated Sensible Heat Ratio 
Design Size 'Speed X' Evaporative Condenser Air Flow Rate
Design Size 'Speed X' Rated Evaporative Condenser Pump Power Consumption

User-Specified 'Speed X' Gross Rated Total Cooling Capacity
User-Specified 'Speed X' Gross Rated Heating Capacity
User-Specified 'Speed X' Rated Air Flow Rate 
User-Specified 'Speed X' Rated Sensible Heat Ratio 
User-Specified 'Speed X' Evaporative Condenser Air Flow Rate
User-Specified 'Speed X' Rated Evaporative Condenser Pump Power Consumption


```