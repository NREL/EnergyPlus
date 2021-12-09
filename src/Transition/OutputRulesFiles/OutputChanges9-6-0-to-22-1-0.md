Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited.  It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo.  At least a PR number or whatever.

### EIO Changes for WindowConstruction

Two new fields were added to the EIO report for `<WindowConstruction>` entries:
`Conductance (Before Adjusted) {W/m2-K}` and `Convection Coefficient Adjustment Ratio`.
See pull request [#9117](https://github.com/NREL/EnergyPlus/pull/9117/files) for more details.


### Emission Meter Renaming

A number of meters in the emission system were renamed to use "Electricity" instead of "Electric".  The meters that were renamed are:

- ElectricEmissions:CO2 -> ElectricityEmissions:CO2
- ElectricEmissions:CO -> ElectricityEmissions:CO
- ElectricEmissions:CH4 -> ElectricityEmissions:CH4
- ElectricEmissions:NOx -> ElectricityEmissions:NOx
- ElectricEmissions:N2O -> ElectricityEmissions:N2O
- ElectricEmissions:SO2 -> ElectricityEmissions:SO2
- ElectricEmissions:PM -> ElectricityEmissions:PM
- ElectricEmissions:PM10 -> ElectricityEmissions:PM10
- ElectricEmissions:PM2.5 -> ElectricityEmissions:PM2.5
- ElectricEmissions:NH3 -> ElectricityEmissions:NH3
- ElectricEmissions:NMVOC -> ElectricityEmissions:NMVOC
- ElectricEmissions:Hg -> ElectricityEmissions:Hg
- ElectricEmissions:Pb -> ElectricityEmissions:Pb
- ElectricEmissions:WaterEnvironmentalFactors -> ElectricityEmissions:WaterEnvironmentalFactors
- ElectricEmissions:Nuclear High -> ElectricityEmissions:Nuclear High
- ElectricEmissions:Nuclear Low -> ElectricityEmissions:Nuclear Low
- PurchasedElectricEmissions:Source -> PurchasedElectricityEmissions:Source
- SoldElectricEmissions:Source -> SoldElectricityEmissions:Source

See pull request [#9101](https://github.com/NREL/EnergyPlus/pull/9101) for more details.


### Emission Variable Renaming

Two variables in the emission system were renamed to use "OtherFuel" instead of "CO2".  The variables that were renamed are:

- Environmental Impact OtherFuel1 CO2 Water Consumption Volume -> Environmental Impact OtherFuel1 Water Consumption Volume
- Environmental Impact OtherFuel2 CO2 Water Consumption Volume -> Environmental Impact OtherFuel2 Water Consumption Volume

See pull request [#9089](https://github.com/NREL/EnergyPlus/pull/9089) for more details.
