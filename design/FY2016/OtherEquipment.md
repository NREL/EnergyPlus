Add Fuel Use to `OtherEquipment`
================================

**Noel Merket, NREL**

 - 14 June 2016

## Justification for New Feature ##

See [#5656](https://github.com/NREL/EnergyPlus/issues/5656). EnergyPlus currently has `ElectricEquipment` and `GasEquipment` objects that use electricity or natural gas and contribute to internal gains. There is also a `OtherEquipment` which uses no fuel and contributes to internal gains. Additionally, there is a `Exterior:FuelEquipment` that uses a fuel specified in a field (Electricity, NaturalGas, PropaneGas, FuelOil, etc) but does not contribute to internal gains. 

For residential use there exists equipment that uses fuels other than electricity or natural gas and contributes to internal gains (i.e. a propane oven/range or a propane clothes dryer). Therefore there is a need for a generic object that allows the user to specify a fuel and contributes to internal gains.


## E-mail and  Conference Call Conclusions ##

We have not had any emails or conference calls, but there was a lot of discussion on the [GitHub issue](https://github.com/NREL/EnergyPlus/issues/5656). Several options were proposed. Including:

1. Create a `FuelEquipment` to replace (or deprecate) `GasEquipment` and `ElectricEquipment`.
2. Create a `FuelEquipment` that has all of the fuel types except electricity or natural gas.
3. Create separate `PropaneEquipment`, `FuelOilEquipment`, etc objects.
4. Equipment that replaces all interior/exterior, gas/electric/other. From [below](https://github.com/NREL/EnergyPlus/issues/5656#issuecomment-220482638)
5. Create a `FuelEquipment` to replace `GasEquipment`. From [below](https://github.com/NREL/EnergyPlus/issues/5656#issuecomment-219066932)
6. Add "Fuel Type" field to `OtherEquipment`. From [below](https://github.com/NREL/EnergyPlus/issues/5656#issuecomment-221892458)

| Object                 | Generates Heat in Zone | Exterior (no zone heat gain) | Generates CO2 | Consumes Electricity | Consumes Natural Gas | Uses District Heat | Consumes any Fuel Type | Consumes no Energy Resource | Net Number of Objects |
|------------------------|:----------------------:|:----------------------------:|:-------------:|:--------------------:|:--------------------:|:------------------:|:----------------------:|:---------------------------:|:---------------------:|
| ElectricEquipment      |            x           |                              |               |           x          |                      |                    |                        |                             |           NA          |
| GasEquipment           |            x           |                              |       x       |                      |           x          |                    |                        |                             |           NA          |
| HotWaterEquipment      |            x           |                              |               |                      |                      |          x         |                        |                             |           NA          |
| SteamEquipment         |            x           |                              |               |                      |                      |          x         |                        |                             |           NA          |
| OtherEquipment         |            x           |                              |               |                      |                      |                    |                        |              x              |           NA          |
| Exterior:FuelEquipment |                        |               x              |               |           x          |           x          |          x         |            x           |                             |           NA          |
| Option 1               |            x           |                              |       x       |           x          |           x          |          x         |            x           |                             |           -1          |
| Option 2               |            x           |                              |       x       |                      |                      |          x         |            x           |                             |           +1          |
| Option 3               |            x           |                              |       x       |                      |                      |                    |                        |                             |          +11          |
| Option 4               |            x           |               x              |       x       |           x          |           x          |          x         |            x           |              x              |           -5          |
| Option 5               |            x           |                              |       x       |                      |           x          |          x         |            x           |                             |           0           |
| Option 6               |            x           |                              |               |           x          |           x          |          x         |            x           |              x              |           0           |

After much discussion, the least intrusive was Option 6, which is adding a fuel type to `OtherEquipment`. 

## Overview ##

In order to meet this need, `OtherEquipment` will be augmented with the following additional fields:

- Fuel Type
- End-use subcategory
- Carbon Dioxide Generation Rate

When Fuel Type is omitted, `OtherEquipment` will behave as it does now. When a Fuel Type is specified it will behave as the `ElectricEquipment` or `GasEquipment` objects do where energy will be used and the gains will go to the zone accordingly. 

## Approach ##

The approach will be to add inputs to `OtherEquipment` to make it work more like `GasEquipment` but with a user specified fuel as described above. Then add the calculations into `OtherEquipment` to make it use fuel and possibly generate emissions when a fuel type is specified. 

## Testing/Validation/Data Sources ##

Testing will include:

- Unit tests of specific functionality.
- Full simulation test of a simple building with an `OtherEquipment` with and without a fuel type to determine it behaves as expected.
- Regression tests comparing an `ElectricEquipment` and `GasEquipment` to equivalent `OtherEquipment` objects. 

## Input Output Reference Documentation ##

I will update this in the LATEX documentation for your review.

## Input Description ##

Inputs would look much the same as `GasEquipment` but with a Fuel Type right after the Object name.

## Outputs Description ##

Outputs would generally mirror what's in `GasEquipment` as well.

## Engineering Reference ##

It will be updated to reflect the changes accordingly.

## Example File and Transition Changes ##

### Old Object

```
OtherEquipment,	BASE-1 OthEq 1, !- Name	BASE-1, !- Zone Name ALWAYSON, !- SCHEDULE Name	EquipmentLevel, !- Design Level calculation method 6766., !- Design Level {W}	, !- Power per Zone Floor Area {watts/m2}	, !- Power per Person {watts/person}	0, !- Fraction Latent 0.3, !- Fraction Radiant	0; !- Fraction Lost
```

### New Object

```
OtherEquipment,	BASE-1 OthEq 1, !- Name	PropaneGas,  !- Fuel Use Type	BASE-1, !- Zone Name 
	ALWAYSON, !- SCHEDULE Name
	EquipmentLevel, !- Design Level calculation method
	6766., !- Design Level {W}	, !- Power per Zone Floor Area {watts/m2}	, !- Power per Person {watts/person}	0, !- Fraction Latent
	0.3, !- Fraction Radiant	0, !- Fraction Lost
	1.2E-7, !- Carbon Dioxide Generation Rate
	SubCategory1; !- End-Use Subcategory
```

### Transition

Transition will insert the Fuel Use Type field and leave it blank, which will cause `OtherEquipment` to behave as it currently does. Carbon Dioxide Generation Rate and End-Use Subcategory will be omitted from the end of the list as they will be optional. 

## References ##

n/a


